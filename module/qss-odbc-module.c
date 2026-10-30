/* qss-odbc-module.c --- ODBC for Emacs Lisp, as a dynamic module

   Copyright (C) 2026 Friedrich Dominicus
   SPDX-License-Identifier: MIT

   This file is part of qss.  See qss.el for the package headers and
   LICENSE for the license text.

   The C side does what needs memory and pointers: handles, buffers,
   UTF-16, parameters, reading columns, collecting diagnostics.  It
   returns raw values; qss-odbc.el interprets them (dates, scaled
   decimals, booleans) and defines the error `qss-odbc-error'.

   The rules (types, lengths, units, driver quirks) follow plain-odbc,
   QSS version 2026-10, which was tested against Access, SQLite,
   PostgreSQL and MariaDB.

   Raw values from `qss-odbc--fetch':
     NULL                      nil
     TINYINT..BIGINT, BIT      integer
     REAL, FLOAT, DOUBLE       float
     DATE                      [date Y M D]
     TIME                      [time H MI S]
     TIMESTAMP                 [timestamp Y M D H MI S NANOSECONDS]
     BINARY, VARBINARY, ...    unibyte string
     everything else           string (read as UTF-16; NUMERIC/DECIMAL
                               come as text, qss-odbc.el scales them)

   Parameters for `qss-odbc--exec' (a vector):
     nil                       NULL
     integer (64 bit)          INTEGER, or NUMERIC text beyond 32 bit
     float                     DOUBLE
     string                    WVARCHAR, WLONGVARCHAR beyond 4000 units
     [decimal TEXT P S]        NUMERIC(P,S) as text
     [binary UNIBYTE]          VARBINARY, LONGVARBINARY beyond 8000 bytes
     [bit 0|1]                 BIT
     [date Y M D]              TYPE_DATE
     [timestamp Y M D H MI S NS]  TYPE_TIMESTAMP  */

#include <emacs-module.h>
#include <sql.h>
#include <sqlext.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int plugin_is_GPL_compatible;

/* ------------------------------------------------------------------ */
/* Handles                                                             */

struct qss_stmt;

struct qss_conn
{
  SQLHDBC dbc;                  /* NULL after disconnect */
  int refs;                     /* living statement objects */
  int finalized;                /* the Lisp object is gone */
  struct qss_stmt *stmts;       /* open statements, for disconnect */
  int sql_utf8;                 /* ANSI driver that expects UTF-8: SQL
                                   text, text parameters and character
                                   columns go as UTF-8 bytes (SQL_C_CHAR) */
};

struct qss_stmt
{
  SQLHSTMT h;                   /* NULL after close */
  struct qss_conn *conn;
  struct qss_stmt *next;        /* in conn->stmts */
  SQLSMALLINT ncols;            /* of the current result set */
  SQLSMALLINT *types;           /* SQL type of each column */
};

static SQLHENV henv;

/* The connection now in use is an ANSI driver that expects UTF-8: its
   diagnostics are read as UTF-8 too.  Set by get_conn and get_stmt at
   the start of every call (module calls run in the main thread).  */
static int current_utf8;

static void
conn_free_if_done (struct qss_conn *c)
{
  if (c->finalized && c->refs == 0)
    {
      if (c->dbc)
        {
          SQLDisconnect (c->dbc);
          SQLFreeHandle (SQL_HANDLE_DBC, c->dbc);
        }
      free (c);
    }
}

static void
stmt_unlink (struct qss_stmt *s)
{
  struct qss_stmt **p;
  for (p = &s->conn->stmts; *p; p = &(*p)->next)
    if (*p == s)
      {
        *p = s->next;
        break;
      }
  s->next = NULL;
}

/* Free the ODBC handle; the struct stays until its finalizer.  */
static void
stmt_close (struct qss_stmt *s)
{
  if (s->h)
    {
      SQLFreeHandle (SQL_HANDLE_STMT, s->h);
      s->h = NULL;
      stmt_unlink (s);
    }
  free (s->types);
  s->types = NULL;
  s->ncols = 0;
}

static void
stmt_finalize (void *p)
{
  struct qss_stmt *s = p;
  struct qss_conn *c = s->conn;
  stmt_close (s);
  free (s);
  c->refs--;
  conn_free_if_done (c);
}

static void
conn_finalize (void *p)
{
  struct qss_conn *c = p;
  c->finalized = 1;
  conn_free_if_done (c);
}

/* ------------------------------------------------------------------ */
/* Emacs helpers                                                       */

static emacs_value Qnil, Qt;

static emacs_value
intern (emacs_env *env, const char *name)
{
  return env->intern (env, name);
}

static emacs_value
list (emacs_env *env, ptrdiff_t n, emacs_value *args)
{
  return env->funcall (env, intern (env, "list"), n, args);
}

static int
exited (emacs_env *env)
{
  return env->non_local_exit_check (env) != emacs_funcall_exit_return;
}

static void
signal_message (emacs_env *env, const char *msg)
{
  emacs_value m = env->make_string (env, msg, strlen (msg));
  env->non_local_exit_signal (env, intern (env, "qss-odbc-error"),
                              list (env, 1, &m));
}

/* UTF-8 (Emacs) <-> UTF-16 (ODBC W functions).  Invalid input becomes
   U+FFFD; Emacs raw bytes (U+3FFF80..) do the same.  */

static SQLWCHAR *
utf8_to_utf16 (const unsigned char *s, ptrdiff_t n, ptrdiff_t *units)
{
  SQLWCHAR *out = malloc ((n + 1) * sizeof (SQLWCHAR));
  ptrdiff_t i = 0, j = 0;
  if (!out)
    return NULL;
  while (i < n)
    {
      uint32_t c = s[i];
      int len = 1;
      if (c >= 0xF0 && c < 0xF8 && i + 3 < n)
        c = ((c & 0x07) << 18) | ((s[i + 1] & 0x3F) << 12)
          | ((s[i + 2] & 0x3F) << 6) | (s[i + 3] & 0x3F), len = 4;
      else if (c >= 0xE0 && c < 0xF0 && i + 2 < n)
        c = ((c & 0x0F) << 12) | ((s[i + 1] & 0x3F) << 6)
          | (s[i + 2] & 0x3F), len = 3;
      else if (c >= 0xC0 && c < 0xE0 && i + 1 < n)
        c = ((c & 0x1F) << 6) | (s[i + 1] & 0x3F), len = 2;
      else if (c >= 0x80)
        c = 0xFFFD;
      if (c > 0x10FFFF || (c >= 0xD800 && c < 0xE000))
        c = 0xFFFD;
      i += len;
      if (c >= 0x10000)
        {
          c -= 0x10000;
          out[j++] = 0xD800 | (c >> 10);
          out[j++] = 0xDC00 | (c & 0x3FF);
        }
      else
        out[j++] = c;
    }
  out[j] = 0;
  *units = j;
  return out;
}

static emacs_value
utf16_to_string (emacs_env *env, const SQLWCHAR *w, ptrdiff_t units)
{
  unsigned char *out = malloc (units * 3 + 1);
  ptrdiff_t i, j = 0;
  emacs_value v;
  if (!out)
    {
      signal_message (env, "out of memory");
      return Qnil;
    }
  for (i = 0; i < units; i++)
    {
      uint32_t c = w[i];
      if (c >= 0xD800 && c < 0xDC00 && i + 1 < units
          && w[i + 1] >= 0xDC00 && w[i + 1] < 0xE000)
        c = 0x10000 + ((c - 0xD800) << 10) + (w[++i] - 0xDC00);
      else if (c >= 0xD800 && c < 0xE000)
        c = 0xFFFD;
      if (c < 0x80)
        out[j++] = c;
      else if (c < 0x800)
        out[j++] = 0xC0 | (c >> 6), out[j++] = 0x80 | (c & 0x3F);
      else if (c < 0x10000)
        out[j++] = 0xE0 | (c >> 12), out[j++] = 0x80 | ((c >> 6) & 0x3F),
          out[j++] = 0x80 | (c & 0x3F);
      else
        out[j++] = 0xF0 | (c >> 18), out[j++] = 0x80 | ((c >> 12) & 0x3F),
          out[j++] = 0x80 | ((c >> 6) & 0x3F), out[j++] = 0x80 | (c & 0x3F);
    }
  v = env->make_string (env, (char *) out, j);
  free (out);
  return v;
}

/* UTF-8 bytes from an ANSI driver as a Lisp string; invalid bytes
   become U+FFFD (make_string wants valid UTF-8).  */
static emacs_value
utf8_bytes_to_string (emacs_env *env, const char *b, ptrdiff_t n)
{
  ptrdiff_t units;
  SQLWCHAR *w = utf8_to_utf16 ((const unsigned char *) b, n, &units);
  emacs_value v;
  if (!w)
    {
      signal_message (env, "out of memory");
      return Qnil;
    }
  v = utf16_to_string (env, w, units);
  free (w);
  return v;
}

/* Copy a Lisp string as UTF-8; caller frees.  NULL after a signal.  */
static char *
copy_string (emacs_env *env, emacs_value v, ptrdiff_t *len)
{
  ptrdiff_t size = 0;
  char *buf;
  if (!env->copy_string_contents (env, v, NULL, &size))
    return NULL;
  buf = malloc (size);
  if (!buf)
    {
      signal_message (env, "out of memory");
      return NULL;
    }
  if (!env->copy_string_contents (env, v, buf, &size))
    {
      free (buf);
      return NULL;
    }
  *len = size - 1;
  return buf;
}

static SQLWCHAR *
lisp_to_utf16 (emacs_env *env, emacs_value v, ptrdiff_t *units)
{
  ptrdiff_t len;
  char *s = copy_string (env, v, &len);
  SQLWCHAR *w;
  if (!s)
    return NULL;
  w = utf8_to_utf16 ((unsigned char *) s, len, units);
  free (s);
  if (!w)
    signal_message (env, "out of memory");
  return w;
}

/* ------------------------------------------------------------------ */
/* Diagnostics                                                         */

/* Collect all diagnostic records of HANDLE and signal qss-odbc-error
   with data (MESSAGE SQLSTATE NATIVE-ERROR DIAGNOSTICS), where
   DIAGNOSTICS is a list of (SQLSTATE NATIVE-ERROR MESSAGE).  The
   message buffer grows: psqlODBC splits long messages at the size of
   the first request and reports SQL_SUCCESS (see plain-odbc, part 12),
   so the first buffer is large.  */
static void
signal_odbc (emacs_env *env, SQLSMALLINT type, SQLHANDLE handle,
             const char *where)
{
  emacs_value diags = Qnil, message = Qnil, state0 = Qnil, native0 = Qnil;
  emacs_value items[64];
  int n = 0;
  SQLSMALLINT rec;
  for (rec = 1; handle && rec <= 64; rec++)
    {
      SQLWCHAR state[6];
      SQLINTEGER native = 0;
      SQLSMALLINT cap = 8192, len = 0;
      SQLWCHAR *msg = NULL;
      SQLRETURN rc;
      if (current_utf8)
        {
          /* An ANSI driver writes UTF-8; through SQLGetDiagRecW unixODBC
             would read it as Latin-1.  */
          SQLCHAR st[6];
          SQLCHAR *m = NULL;
          for (;;)
            {
              free (m);
              m = malloc (cap + 1);
              if (!m)
                break;
              rc = SQLGetDiagRec (type, handle, rec, st, &native, m, cap, &len);
              if (rc == SQL_SUCCESS_WITH_INFO && len >= cap && cap < 32000)
                {
                  cap = len + 1 > 32000 ? 32000 : len + 1;
                  continue;
                }
              break;
            }
          if (!m || !SQL_SUCCEEDED (rc))
            {
              free (m);
              break;
            }
          if (len > cap)
            len = cap;
          {
            emacs_value e[3];
            e[0] = env->make_string (env, (char *) st, strnlen ((char *) st, 5));
            e[1] = env->make_integer (env, native);
            e[2] = utf8_bytes_to_string (env, (char *) m, len);
            items[n++] = list (env, 3, e);
            if (n == 1)
              state0 = e[0], native0 = e[1], message = e[2];
          }
          free (m);
          continue;
        }
      for (;;)
        {
          free (msg);
          msg = malloc ((cap + 1) * sizeof (SQLWCHAR));
          if (!msg)
            break;
          rc = SQLGetDiagRecW (type, handle, rec, state, &native, msg,
                               cap, &len);
          if (rc == SQL_SUCCESS_WITH_INFO && len >= cap && cap < 32000)
            {
              cap = len + 1 > 32000 ? 32000 : len + 1;
              continue;
            }
          break;
        }
      if (!msg || !SQL_SUCCEEDED (rc))
        {
          free (msg);
          break;
        }
      if (len > cap)
        len = cap;
      {
        emacs_value e[3];
        e[0] = utf16_to_string (env, state, 5);
        e[1] = env->make_integer (env, native);
        e[2] = utf16_to_string (env, msg, len);
        items[n++] = list (env, 3, e);
        if (n == 1)
          state0 = e[0], native0 = e[1], message = e[2];
      }
      free (msg);
    }
  diags = list (env, n, items);
  if (n == 0)
    {
      char buf[200];
      snprintf (buf, sizeof buf, "%s failed without a diagnostic record",
                where);
      message = env->make_string (env, buf, strlen (buf));
    }
  else
    {
      /* All records, joined with " | ", as plain-odbc does.  */
      emacs_value args[3];
      int i;
      for (i = 1; i < n; i++)
        {
          args[0] = message;
          args[1] = env->make_string (env, " | ", 3);
          args[2] = env->funcall (env, intern (env, "nth"), 2,
                                  (emacs_value[]) {
                                    env->make_integer (env, 2), items[i]});
          message = env->funcall (env, intern (env, "concat"), 3, args);
        }
    }
  {
    emacs_value data[4] = { message, state0, native0, diags };
    env->non_local_exit_signal (env, intern (env, "qss-odbc-error"),
                                list (env, 4, data));
  }
}

#define CHECK(env, rc, type, handle, where)                     \
  do {                                                          \
    if (!SQL_SUCCEEDED (rc))                                    \
      {                                                         \
        signal_odbc ((env), (type), (handle), (where));         \
        goto fail;                                              \
      }                                                         \
  } while (0)

/* ------------------------------------------------------------------ */
/* Argument access                                                     */

static struct qss_conn *
get_conn (emacs_env *env, emacs_value v)
{
  struct qss_conn *c;
  if (env->get_user_finalizer (env, v) != conn_finalize)
    {
      if (!exited (env))
        signal_message (env, "not an ODBC connection");
      else
        {
          env->non_local_exit_clear (env);
          signal_message (env, "not an ODBC connection");
        }
      return NULL;
    }
  c = env->get_user_ptr (env, v);
  current_utf8 = c->sql_utf8;
  if (!c->dbc)
    {
      signal_message (env, "connection is closed");
      return NULL;
    }
  return c;
}

static struct qss_stmt *
get_stmt (emacs_env *env, emacs_value v, int must_be_open)
{
  struct qss_stmt *s;
  if (env->get_user_finalizer (env, v) != stmt_finalize)
    {
      if (exited (env))
        env->non_local_exit_clear (env);
      signal_message (env, "not an ODBC statement");
      return NULL;
    }
  s = env->get_user_ptr (env, v);
  current_utf8 = s->conn->sql_utf8;
  if (must_be_open && !s->h)
    {
      signal_message (env, "statement is closed");
      return NULL;
    }
  return s;
}

/* ------------------------------------------------------------------ */
/* Connect                                                             */

static emacs_value
Fconnect (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c;
  SQLWCHAR *cs = NULL;
  ptrdiff_t units;
  SQLRETURN rc;
  SQLHDBC dbc = NULL;
  int autocommit = nargs > 1 ? env->is_not_nil (env, args[1]) : 1;
  (void) data;

  current_utf8 = 0;
  cs = lisp_to_utf16 (env, args[0], &units);
  if (!cs)
    return Qnil;
  rc = SQLAllocHandle (SQL_HANDLE_DBC, henv, &dbc);
  CHECK (env, rc, SQL_HANDLE_ENV, henv, "SQLAllocHandle");
  rc = SQLSetConnectAttrW (dbc, SQL_ATTR_AUTOCOMMIT,
                           (SQLPOINTER) (intptr_t) (autocommit
                                                    ? SQL_AUTOCOMMIT_ON
                                                    : SQL_AUTOCOMMIT_OFF),
                           SQL_IS_UINTEGER);
  CHECK (env, rc, SQL_HANDLE_DBC, dbc, "SQLSetConnectAttr");
  rc = SQLDriverConnectW (dbc, NULL, cs, (SQLSMALLINT) units, NULL, 0, NULL,
                          SQL_DRIVER_NOPROMPT);
  CHECK (env, rc, SQL_HANDLE_DBC, dbc, "SQLDriverConnect");
  free (cs);
  c = calloc (1, sizeof *c);
  if (!c)
    {
      SQLDisconnect (dbc);
      SQLFreeHandle (SQL_HANDLE_DBC, dbc);
      signal_message (env, "out of memory");
      return Qnil;
    }
  c->dbc = dbc;
  return env->make_user_ptr (env, conn_finalize, c);

fail:
  free (cs);
  if (dbc)
    SQLFreeHandle (SQL_HANDLE_DBC, dbc);
  return Qnil;
}

static emacs_value
Fdisconnect (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  (void) nargs; (void) data;
  if (!c)
    return Qnil;
  while (c->stmts)
    stmt_close (c->stmts);
  SQLDisconnect (c->dbc);
  SQLFreeHandle (SQL_HANDLE_DBC, c->dbc);
  c->dbc = NULL;
  return Qnil;
}

static emacs_value
Fconnected_p (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c;
  (void) nargs; (void) data;
  if (env->get_user_finalizer (env, args[0]) != conn_finalize)
    {
      if (exited (env))
        env->non_local_exit_clear (env);
      return Qnil;
    }
  c = env->get_user_ptr (env, args[0]);
  return c->dbc ? Qt : Qnil;
}

static emacs_value
Fend_transaction (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  SQLRETURN rc;
  (void) nargs;
  if (!c)
    return Qnil;
  rc = SQLEndTran (SQL_HANDLE_DBC, c->dbc,
                   data ? SQL_COMMIT : SQL_ROLLBACK);
  CHECK (env, rc, SQL_HANDLE_DBC, c->dbc, "SQLEndTran");
  return Qnil;
fail:
  return Qnil;
}

static emacs_value
Fset_autocommit (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                 void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  SQLRETURN rc;
  (void) nargs; (void) data;
  if (!c)
    return Qnil;
  rc = SQLSetConnectAttrW (c->dbc, SQL_ATTR_AUTOCOMMIT,
                           (SQLPOINTER) (intptr_t)
                           (env->is_not_nil (env, args[1])
                            ? SQL_AUTOCOMMIT_ON : SQL_AUTOCOMMIT_OFF),
                           SQL_IS_UINTEGER);
  CHECK (env, rc, SQL_HANDLE_DBC, c->dbc, "SQLSetConnectAttr");
  return Qnil;
fail:
  return Qnil;
}

static emacs_value
Fset_sql_utf8 (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
               void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  (void) nargs; (void) data;
  if (c)
    c->sql_utf8 = env->is_not_nil (env, args[1]);
  return Qnil;
}

/* String-valued SQLGetInfo: dbms-name, dbms-version, driver-name,
   identifier-quote, catalog-separator.  */
static emacs_value
Fget_info (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  static const struct { const char *name; SQLUSMALLINT code; } infos[] = {
    { "dbms-name", SQL_DBMS_NAME },
    { "dbms-version", SQL_DBMS_VER },
    { "driver-name", SQL_DRIVER_NAME },
    { "driver-version", SQL_DRIVER_VER },
    { "identifier-quote", SQL_IDENTIFIER_QUOTE_CHAR },
    { "catalog-separator", SQL_CATALOG_NAME_SEPARATOR },
  };
  SQLWCHAR buf[512];
  SQLSMALLINT len = 0;
  SQLRETURN rc;
  size_t i;
  (void) nargs; (void) data;
  if (!c)
    return Qnil;
  for (i = 0; i < sizeof infos / sizeof infos[0]; i++)
    if (env->eq (env, args[1], intern (env, infos[i].name)))
      {
        rc = SQLGetInfoW (c->dbc, infos[i].code, buf, sizeof buf, &len);
        CHECK (env, rc, SQL_HANDLE_DBC, c->dbc, "SQLGetInfo");
        if (len > (SQLSMALLINT) sizeof buf - 2)
          len = sizeof buf - 2;
        return utf16_to_string (env, buf, len / sizeof (SQLWCHAR));
      }
  signal_message (env, "unknown info key");
fail:
  return Qnil;
}

/* ------------------------------------------------------------------ */
/* Statements                                                          */

/* Describe the current result set into S->types.  */
static int
describe (emacs_env *env, struct qss_stmt *s)
{
  SQLSMALLINT n = 0, i;
  SQLRETURN rc;
  free (s->types);
  s->types = NULL;
  s->ncols = 0;
  rc = SQLNumResultCols (s->h, &n);
  CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLNumResultCols");
  if (n > 0)
    {
      s->types = calloc (n, sizeof (SQLSMALLINT));
      if (!s->types)
        {
          signal_message (env, "out of memory");
          return 0;
        }
      for (i = 0; i < n; i++)
        {
          /* The type from SQLDescribeCol, not SQLColAttribute: the
             SQLite driver reports VARCHAR there for BIGINT and BLOB
             columns, and SQLGetData follows SQLDescribeCol.  */
          SQLWCHAR name[2];
          SQLSMALLINT len, type = 0, digits, nullable;
          SQLULEN size;
          rc = SQLDescribeColW (s->h, i + 1, name, 2, &len, &type, &size,
                                &digits, &nullable);
          CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLDescribeCol");
          s->types[i] = type;
        }
    }
  s->ncols = n;
  return 1;
fail:
  return 0;
}

static struct qss_stmt *
new_stmt (emacs_env *env, struct qss_conn *c)
{
  struct qss_stmt *s = calloc (1, sizeof *s);
  SQLRETURN rc;
  if (!s)
    {
      signal_message (env, "out of memory");
      return NULL;
    }
  rc = SQLAllocHandle (SQL_HANDLE_STMT, c->dbc, &s->h);
  if (!SQL_SUCCEEDED (rc))
    {
      signal_odbc (env, SQL_HANDLE_DBC, c->dbc, "SQLAllocHandle");
      free (s);
      return NULL;
    }
  s->conn = c;
  s->next = c->stmts;
  c->stmts = s;
  c->refs++;
  return s;
}

/* Wrap S as a Lisp object; on failure free it.  */
static emacs_value
wrap_stmt (emacs_env *env, struct qss_stmt *s)
{
  emacs_value v = env->make_user_ptr (env, stmt_finalize, s);
  if (exited (env))
    stmt_finalize (s);
  return v;
}

/* Parameters.  */

struct param
{
  void *buf;
  SQLLEN ind;
  /* small values live here instead of in BUF */
  union
  {
    SQLINTEGER i;
    double d;
    unsigned char bit;
    SQL_DATE_STRUCT date;
    SQL_TIMESTAMP_STRUCT ts;
  } v;
};

static int
vec_int (emacs_env *env, emacs_value vec, ptrdiff_t i, intmax_t *out)
{
  emacs_value x = env->vec_get (env, vec, i);
  if (exited (env))
    return 0;
  *out = env->extract_integer (env, x);
  return !exited (env);
}

static int
tag_is (emacs_env *env, emacs_value vec, const char *tag)
{
  return env->eq (env, env->vec_get (env, vec, 0), intern (env, tag));
}

/* Bind text: UTF8 (LEN bytes) as parameter I.  For a Unicode driver as
   UTF-16 (SQL_C_WCHAR, lengths in UTF-16 units), for an ANSI driver
   that expects UTF-8 as bytes (SQL_C_CHAR, lengths in bytes): an ANSI
   driver rejects SQL_C_WCHAR (psqlODBC: HYC00 "Unrecognized
   C_parameter type").  WTYPE/ATYPE: the SQL type for each; SIZE: the
   column size, or -1 for the length of the text.  */
static SQLRETURN
bind_text (emacs_env *env, struct qss_stmt *s, SQLUSMALLINT i,
           struct param *p, const char *utf8, ptrdiff_t len,
           SQLSMALLINT wtype, SQLSMALLINT atype, SQLSMALLINT long_wtype,
           SQLSMALLINT long_atype, SQLULEN size, SQLSMALLINT digits)
{
  if (s->conn->sql_utf8)
    {
      p->buf = malloc (len + 1);
      if (!p->buf)
        {
          signal_message (env, "out of memory");
          return SQL_ERROR - 100;
        }
      memcpy (p->buf, utf8, len);
      ((char *) p->buf)[len] = 0;
      p->ind = len;
      return SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_CHAR,
                               len > 4000 ? long_atype : atype,
                               size != (SQLULEN) -1 ? size
                               : (SQLULEN) (len > 0 ? len : 1),
                               digits, p->buf, p->ind, &p->ind);
    }
  else
    {
      ptrdiff_t units;
      p->buf = utf8_to_utf16 ((const unsigned char *) utf8, len, &units);
      if (!p->buf)
        {
          signal_message (env, "out of memory");
          return SQL_ERROR - 100;
        }
      p->ind = units * sizeof (SQLWCHAR);
      /* Column size in characters (UTF-16 units), at least 1: never 0
         (HY104), see plain-odbc changes 4 and 9.  */
      return SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_WCHAR,
                               units > 4000 ? long_wtype : wtype,
                               size != (SQLULEN) -1 ? size
                               : (SQLULEN) (units > 0 ? units : 1),
                               digits, p->buf, p->ind, &p->ind);
    }
}

/* bind_text with a Lisp string.  */
static SQLRETURN
bind_lisp_text (emacs_env *env, struct qss_stmt *s, SQLUSMALLINT i,
                struct param *p, emacs_value v, SQLSMALLINT wtype,
                SQLSMALLINT atype, SQLSMALLINT long_wtype,
                SQLSMALLINT long_atype, SQLULEN size, SQLSMALLINT digits)
{
  ptrdiff_t len;
  char *u = copy_string (env, v, &len);
  SQLRETURN rc;
  if (!u)
    return SQL_ERROR - 100;
  rc = bind_text (env, s, i, p, u, len, wtype, atype, long_wtype, long_atype,
                  size, digits);
  free (u);
  return rc;
}

/* Bind parameter number I (from 1) with value V.  */
static int
bind_param (emacs_env *env, struct qss_stmt *s, SQLUSMALLINT i,
            emacs_value v, struct param *p)
{
  emacs_value type = env->type_of (env, v);
  SQLRETURN rc;
  if (!env->is_not_nil (env, v))
    {
      p->ind = SQL_NULL_DATA;
      rc = s->conn->sql_utf8
        ? SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_CHAR,
                            SQL_VARCHAR, 1, 0, NULL, 0, &p->ind)
        : SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_WCHAR,
                            SQL_WVARCHAR, 1, 0, NULL, 0, &p->ind);
    }
  else if (env->eq (env, type, intern (env, "integer")))
    {
      intmax_t n = env->extract_integer (env, v);
      if (exited (env))
        return 0;
      if (n >= INT32_MIN && n <= INT32_MAX)
        {
          p->v.i = (SQLINTEGER) n;
          p->ind = 0;
          rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_SLONG,
                                 SQL_INTEGER, 10, 0, &p->v.i, 0, &p->ind);
        }
      else
        {
          /* As in plain-odbc: beyond 32 bit as exact NUMERIC text.  */
          char txt[32];
          int len = snprintf (txt, sizeof txt, "%jd", n);
          int digits = len - (n < 0);
          rc = bind_text (env, s, i, p, txt, len, SQL_NUMERIC, SQL_NUMERIC,
                          SQL_NUMERIC, SQL_NUMERIC, digits, 0);
        }
    }
  else if (env->eq (env, type, intern (env, "float")))
    {
      p->v.d = env->extract_float (env, v);
      p->ind = 0;
      rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_DOUBLE,
                             SQL_DOUBLE, 15, 0, &p->v.d, 0, &p->ind);
    }
  else if (env->eq (env, type, intern (env, "string")))
    {
      rc = bind_lisp_text (env, s, i, p, v, SQL_WVARCHAR, SQL_VARCHAR,
                           SQL_WLONGVARCHAR, SQL_LONGVARCHAR, (SQLULEN) -1, 0);
    }
  else if (env->eq (env, type, intern (env, "vector"))
           && env->vec_size (env, v) >= 2)
    {
      if (tag_is (env, v, "decimal") && env->vec_size (env, v) == 4)
        {
          intmax_t prec, scale;
          if (!vec_int (env, v, 2, &prec) || !vec_int (env, v, 3, &scale))
            return 0;
          /* SQL_NUMERIC, not SQL_DECIMAL: the Access driver rejects
             DECIMAL (HYC00), plain-odbc 2026-10-14.  */
          rc = bind_lisp_text (env, s, i, p, env->vec_get (env, v, 1),
                               SQL_NUMERIC, SQL_NUMERIC, SQL_NUMERIC,
                               SQL_NUMERIC, prec, scale);
        }
      else if (tag_is (env, v, "binary"))
        {
          ptrdiff_t len;
          char *b = copy_string (env, env->vec_get (env, v, 1), &len);
          if (!b)
            return 0;
          p->buf = b;
          p->ind = len;
          rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_BINARY,
                                 len > 8000 ? SQL_LONGVARBINARY
                                 : SQL_VARBINARY,
                                 len > 0 ? len : 1, 0, p->buf, len, &p->ind);
        }
      else if (tag_is (env, v, "bit"))
        {
          intmax_t b;
          if (!vec_int (env, v, 1, &b))
            return 0;
          p->v.bit = b != 0;
          p->ind = 0;
          rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_BIT,
                                 SQL_BIT, 1, 0, &p->v.bit, 0, &p->ind);
        }
      else if (tag_is (env, v, "date") && env->vec_size (env, v) == 4)
        {
          intmax_t y, m, d;
          if (!vec_int (env, v, 1, &y) || !vec_int (env, v, 2, &m)
              || !vec_int (env, v, 3, &d))
            return 0;
          p->v.date.year = y, p->v.date.month = m, p->v.date.day = d;
          p->ind = 0;
          rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT, SQL_C_TYPE_DATE,
                                 SQL_TYPE_DATE, 10, 0, &p->v.date, 0,
                                 &p->ind);
        }
      else if (tag_is (env, v, "timestamp") && env->vec_size (env, v) == 8)
        {
          intmax_t f[7];
          int k;
          for (k = 0; k < 7; k++)
            if (!vec_int (env, v, k + 1, &f[k]))
              return 0;
          p->v.ts.year = f[0], p->v.ts.month = f[1], p->v.ts.day = f[2];
          p->v.ts.hour = f[3], p->v.ts.minute = f[4], p->v.ts.second = f[5];
          p->v.ts.fraction = f[6];
          p->ind = 0;
          /* Column size 19 without fraction (plain-odbc), else
             microseconds.  */
          rc = SQLBindParameter (s->h, i, SQL_PARAM_INPUT,
                                 SQL_C_TYPE_TIMESTAMP, SQL_TYPE_TIMESTAMP,
                                 f[6] ? 26 : 19, f[6] ? 6 : 0, &p->v.ts, 0,
                                 &p->ind);
        }
      else
        {
          signal_message (env, "unknown tagged parameter");
          return 0;
        }
    }
  else
    {
      emacs_value e[2];
      e[0] = env->make_string (env, "unsupported parameter", 21);
      e[1] = v;
      env->non_local_exit_signal (env, intern (env, "qss-odbc-error"),
                                  list (env, 2, e));
      return 0;
    }
  if (exited (env) || rc == SQL_ERROR - 100)
    return 0;
  if (!SQL_SUCCEEDED (rc))
    {
      signal_odbc (env, SQL_HANDLE_STMT, s->h, "SQLBindParameter");
      return 0;
    }
  return 1;
}

/* (qss-odbc--exec CONN SQL &optional PARAMS) -> statement.  */
static emacs_value
Fexec (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  struct qss_stmt *s = NULL;
  struct param *ps = NULL;
  ptrdiff_t np = 0, i, units;
  SQLWCHAR *sql = NULL;
  char *sql8 = NULL;
  SQLRETURN rc;
  emacs_value result = Qnil;
  (void) data;
  if (!c)
    return Qnil;
  if (nargs > 2 && env->is_not_nil (env, args[2]))
    {
      np = env->vec_size (env, args[2]);
      if (exited (env))
        return Qnil;
    }
  if (c->sql_utf8)
    sql8 = copy_string (env, args[1], &units);
  else
    sql = lisp_to_utf16 (env, args[1], &units);
  if (!sql && !sql8)
    return Qnil;
  s = new_stmt (env, c);
  if (!s)
    goto done;
  if (np > 0)
    {
      ps = calloc (np, sizeof *ps);
      if (!ps)
        {
          signal_message (env, "out of memory");
          goto done;
        }
      for (i = 0; i < np; i++)
        if (!bind_param (env, s, i + 1, env->vec_get (env, args[2], i),
                         &ps[i]))
          goto done;
    }
  /* An ANSI driver behind unixODBC gets W text converted to Latin-1;
     SQLite expects UTF-8 (plain-odbc part 8: encoding per connection,
     not per system).  */
  rc = sql8 ? SQLExecDirect (s->h, (SQLCHAR *) sql8, (SQLINTEGER) units)
    : SQLExecDirectW (s->h, sql, (SQLINTEGER) units);
  if (rc != SQL_NO_DATA && !SQL_SUCCEEDED (rc))
    {
      signal_odbc (env, SQL_HANDLE_STMT, s->h, "SQLExecDirect");
      goto done;
    }
  /* Parameters are no longer needed: no data-at-execution, no output
     parameters.  Unbind before freeing.  */
  SQLFreeStmt (s->h, SQL_RESET_PARAMS);
  if (!describe (env, s))
    goto done;
  result = wrap_stmt (env, s);
  s = NULL;

done:
  if (s)
    {
      /* not wrapped: free it here */
      struct qss_conn *cc = s->conn;
      stmt_close (s);
      free (s);
      cc->refs--;
    }
  if (ps)
    {
      for (i = 0; i < np; i++)
        free (ps[i].buf);
      free (ps);
    }
  free (sql);
  free (sql8);
  return result;
}

/* Catalog functions -> statement: (qss-odbc--catalog CONN KIND ARGS).
   KIND tables: ARGS [catalog schema table type]
        columns: [catalog schema table column]
        primary-keys: [catalog schema table]
   nil in ARGS means "any" (NULL).  */
static emacs_value
Fcatalog (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_conn *c = get_conn (env, args[0]);
  struct qss_stmt *s = NULL;
  SQLWCHAR *a[4] = { NULL, NULL, NULL, NULL };
  SQLSMALLINT l[4] = { 0, 0, 0, 0 };
  ptrdiff_t n, i;
  SQLRETURN rc = SQL_ERROR;
  emacs_value result = Qnil;
  (void) nargs; (void) data;
  if (!c)
    return Qnil;
  n = env->vec_size (env, args[2]);
  if (exited (env))
    return Qnil;
  if (n > 4)
    n = 4;
  for (i = 0; i < n; i++)
    {
      emacs_value x = env->vec_get (env, args[2], i);
      if (env->is_not_nil (env, x))
        {
          ptrdiff_t u;
          a[i] = lisp_to_utf16 (env, x, &u);
          if (!a[i])
            goto done;
          l[i] = (SQLSMALLINT) u;
        }
    }
  s = new_stmt (env, c);
  if (!s)
    goto done;
  if (env->eq (env, args[1], intern (env, "tables")))
    rc = SQLTablesW (s->h, a[0], l[0], a[1], l[1], a[2], l[2], a[3], l[3]);
  else if (env->eq (env, args[1], intern (env, "columns")))
    rc = SQLColumnsW (s->h, a[0], l[0], a[1], l[1], a[2], l[2], a[3], l[3]);
  else if (env->eq (env, args[1], intern (env, "primary-keys")))
    rc = SQLPrimaryKeysW (s->h, a[0], l[0], a[1], l[1], a[2], l[2]);
  else
    {
      signal_message (env, "unknown catalog function");
      goto done;
    }
  if (!SQL_SUCCEEDED (rc))
    {
      signal_odbc (env, SQL_HANDLE_STMT, s->h, "catalog function");
      goto done;
    }
  if (!describe (env, s))
    goto done;
  result = wrap_stmt (env, s);
  s = NULL;
done:
  if (s)
    {
      struct qss_conn *cc = s->conn;
      stmt_close (s);
      free (s);
      cc->refs--;
    }
  for (i = 0; i < 4; i++)
    free (a[i]);
  return result;
}

/* (qss-odbc--columns STMT) -> vector of (NAME SQL-TYPE SIZE DIGITS
   NULLABLE TYPE-NAME), NULLABLE t, nil or unknown.  */
static emacs_value
Fcolumns (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_stmt *s = get_stmt (env, args[0], 1);
  emacs_value vec, *items;
  SQLSMALLINT i;
  (void) nargs; (void) data;
  if (!s)
    return Qnil;
  items = calloc (s->ncols ? s->ncols : 1, sizeof (emacs_value));
  if (!items)
    {
      signal_message (env, "out of memory");
      return Qnil;
    }
  for (i = 0; i < s->ncols; i++)
    {
      SQLSMALLINT cap = 256, len = 0, type, digits, nullable;
      SQLULEN size;
      SQLWCHAR *name = NULL;
      SQLRETURN rc;
      emacs_value e[6];
      SQLWCHAR tname[128];
      SQLSMALLINT tlen = 0;
      /* The name buffer grows; a full buffer may be truncated
         (plain-odbc part 10).  */
      for (;;)
        {
          free (name);
          name = malloc ((cap + 1) * sizeof (SQLWCHAR));
          if (!name)
            break;
          rc = SQLDescribeColW (s->h, i + 1, name, cap, &len, &type, &size,
                                &digits, &nullable);
          if (SQL_SUCCEEDED (rc) && len >= cap - 1 && cap < 8192)
            {
              cap *= 4;
              continue;
            }
          break;
        }
      if (!name)
        {
          free (items);
          signal_message (env, "out of memory");
          return Qnil;
        }
      if (!SQL_SUCCEEDED (rc))
        {
          free (name);
          free (items);
          signal_odbc (env, SQL_HANDLE_STMT, s->h, "SQLDescribeCol");
          return Qnil;
        }
      if (len > cap)
        len = cap;
      e[0] = utf16_to_string (env, name, len);
      e[1] = env->make_integer (env, type);
      e[2] = env->make_integer (env, (intmax_t) size);
      e[3] = env->make_integer (env, digits);
      e[4] = nullable == SQL_NULLABLE ? Qt
        : nullable == SQL_NO_NULLS ? Qnil : intern (env, "unknown");
      /* The database's own name of the type ("bool", "int4", "TEXT"):
         psqlODBC reports booleans as VARCHAR.  */
      if (SQL_SUCCEEDED (SQLColAttributeW (s->h, i + 1, SQL_DESC_TYPE_NAME,
                                           tname, sizeof tname, &tlen, NULL)))
        {
          if (tlen > (SQLSMALLINT) sizeof tname - 2)
            tlen = sizeof tname - 2;
          e[5] = utf16_to_string (env, tname, tlen / sizeof (SQLWCHAR));
        }
      else
        e[5] = Qnil;
      items[i] = list (env, 6, e);
      free (name);
    }
  vec = env->funcall (env, intern (env, "vector"), s->ncols, items);
  free (items);
  return vec;
}

/* Read column COL (from 1) as character data (SQL_C_WCHAR) or binary
   (SQL_C_BINARY) with SQLGetData, piece by piece.  The pieces are
   collected as bytes and converted at the end, so a surrogate pair on
   a piece boundary stays whole (plain-odbc part 2).  */
static emacs_value
get_chunked (emacs_env *env, struct qss_stmt *s, SQLUSMALLINT col,
             int binary)
{
  /* An ANSI driver that expects UTF-8 gives characters as UTF-8 bytes:
     SQL_C_CHAR, one terminating byte.  */
  int narrow = !binary && s->conn->sql_utf8;
  SQLLEN cap = 32768, total = 0, ind = 0;
  char *acc = NULL;
  char *buf = malloc (cap);
  int null = 0;
  emacs_value v = Qnil;
  if (!buf)
    goto oom;
  for (;;)
    {
      SQLRETURN rc = SQLGetData (s->h, col, binary ? SQL_C_BINARY
                                 : narrow ? SQL_C_CHAR : SQL_C_WCHAR,
                                 buf, cap, &ind);
      SQLLEN got, room = binary ? cap
        : narrow ? cap - 1 : cap - (SQLLEN) sizeof (SQLWCHAR);
      char *n;
      if (rc == SQL_NO_DATA)
        break;
      if (!SQL_SUCCEEDED (rc))
        {
          free (buf);
          free (acc);
          signal_odbc (env, SQL_HANDLE_STMT, s->h, "SQLGetData");
          return Qnil;
        }
      if (ind == SQL_NULL_DATA)
        {
          null = 1;
          break;
        }
      got = (ind == SQL_NO_TOTAL || ind > room) ? room : ind;
      n = realloc (acc, total + got + 1);
      if (!n)
        goto oom;
      acc = n;
      memcpy (acc + total, buf, got);
      total += got;
      if (rc == SQL_SUCCESS || (ind != SQL_NO_TOTAL && ind <= room))
        break;
    }
  if (null)
    v = Qnil;
  else if (binary)
    v = env->make_unibyte_string (env, acc ? acc : "", total);
  else if (narrow)
    v = utf8_bytes_to_string (env, acc ? acc : "", total);
  else
    v = utf16_to_string (env, (SQLWCHAR *) (acc ? acc : buf),
                         acc ? total / (SQLLEN) sizeof (SQLWCHAR) : 0);
  free (buf);
  free (acc);
  return v;
oom:
  free (buf);
  free (acc);
  signal_message (env, "out of memory");
  return Qnil;
}

static emacs_value
get_value (emacs_env *env, struct qss_stmt *s, SQLUSMALLINT col)
{
  SQLLEN ind = 0;
  SQLRETURN rc;
  switch (s->types[col - 1])
    {
    case SQL_TINYINT:
    case SQL_SMALLINT:
    case SQL_INTEGER:
    case SQL_BIGINT:
    case SQL_BIT:
      {
        SQLBIGINT n = 0;
        rc = SQLGetData (s->h, col, SQL_C_SBIGINT, &n, 0, &ind);
        CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLGetData");
        return ind == SQL_NULL_DATA ? Qnil : env->make_integer (env, n);
      }
    case SQL_REAL:
    case SQL_FLOAT:
    case SQL_DOUBLE:
      {
        double d = 0;
        rc = SQLGetData (s->h, col, SQL_C_DOUBLE, &d, 0, &ind);
        CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLGetData");
        return ind == SQL_NULL_DATA ? Qnil : env->make_float (env, d);
      }
    case SQL_TYPE_DATE:
    case SQL_DATE:
      {
        SQL_DATE_STRUCT d;
        emacs_value e[4];
        rc = SQLGetData (s->h, col, SQL_C_TYPE_DATE, &d, 0, &ind);
        CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLGetData");
        if (ind == SQL_NULL_DATA)
          return Qnil;
        e[0] = intern (env, "date");
        e[1] = env->make_integer (env, d.year);
        e[2] = env->make_integer (env, d.month);
        e[3] = env->make_integer (env, d.day);
        return env->funcall (env, intern (env, "vector"), 4, e);
      }
    case SQL_TYPE_TIME:
    case SQL_TIME:
      {
        SQL_TIME_STRUCT t;
        emacs_value e[4];
        rc = SQLGetData (s->h, col, SQL_C_TYPE_TIME, &t, 0, &ind);
        CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLGetData");
        if (ind == SQL_NULL_DATA)
          return Qnil;
        e[0] = intern (env, "time");
        e[1] = env->make_integer (env, t.hour);
        e[2] = env->make_integer (env, t.minute);
        e[3] = env->make_integer (env, t.second);
        return env->funcall (env, intern (env, "vector"), 4, e);
      }
    case SQL_TYPE_TIMESTAMP:
    case SQL_TIMESTAMP:
      {
        SQL_TIMESTAMP_STRUCT t;
        emacs_value e[8];
        rc = SQLGetData (s->h, col, SQL_C_TYPE_TIMESTAMP, &t, 0, &ind);
        CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLGetData");
        if (ind == SQL_NULL_DATA)
          return Qnil;
        e[0] = intern (env, "timestamp");
        e[1] = env->make_integer (env, t.year);
        e[2] = env->make_integer (env, t.month);
        e[3] = env->make_integer (env, t.day);
        e[4] = env->make_integer (env, t.hour);
        e[5] = env->make_integer (env, t.minute);
        e[6] = env->make_integer (env, t.second);
        e[7] = env->make_integer (env, t.fraction);
        return env->funcall (env, intern (env, "vector"), 8, e);
      }
    case SQL_BINARY:
    case SQL_VARBINARY:
    case SQL_LONGVARBINARY:
      return get_chunked (env, s, col, 1);
    default:
      return get_chunked (env, s, col, 0);
    }
fail:
  return Qnil;
}

/* (qss-odbc--fetch STMT) -> vector of raw values, nil after the last
   row.  */
static emacs_value
Ffetch (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_stmt *s = get_stmt (env, args[0], 1);
  emacs_value *vals, vec;
  SQLRETURN rc;
  SQLSMALLINT i;
  (void) nargs; (void) data;
  if (!s)
    return Qnil;
  if (s->ncols == 0)
    return Qnil;
  rc = SQLFetch (s->h);
  if (rc == SQL_NO_DATA)
    return Qnil;
  if (!SQL_SUCCEEDED (rc))
    {
      signal_odbc (env, SQL_HANDLE_STMT, s->h, "SQLFetch");
      return Qnil;
    }
  vals = calloc (s->ncols, sizeof (emacs_value));
  if (!vals)
    {
      signal_message (env, "out of memory");
      return Qnil;
    }
  for (i = 0; i < s->ncols; i++)
    {
      vals[i] = get_value (env, s, i + 1);
      if (exited (env))
        {
          free (vals);
          return Qnil;
        }
    }
  vec = env->funcall (env, intern (env, "vector"), s->ncols, vals);
  free (vals);
  return vec;
}

static emacs_value
Frow_count (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_stmt *s = get_stmt (env, args[0], 1);
  SQLLEN n = -1;
  SQLRETURN rc;
  (void) nargs; (void) data;
  if (!s)
    return Qnil;
  rc = SQLRowCount (s->h, &n);
  CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLRowCount");
  return env->make_integer (env, n);
fail:
  return Qnil;
}

/* Advance to the next result set: t, or nil if there is none.  Errors
   of the next set are signalled (plain-odbc part 10).  */
static emacs_value
Fmore_results (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
               void *data)
{
  struct qss_stmt *s = get_stmt (env, args[0], 1);
  SQLRETURN rc;
  (void) nargs; (void) data;
  if (!s)
    return Qnil;
  rc = SQLMoreResults (s->h);
  if (rc == SQL_NO_DATA)
    return Qnil;
  CHECK (env, rc, SQL_HANDLE_STMT, s->h, "SQLMoreResults");
  return describe (env, s) ? Qt : Qnil;
fail:
  return Qnil;
}

static emacs_value
Fclose (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  struct qss_stmt *s = get_stmt (env, args[0], 0);
  (void) nargs; (void) data;
  if (s)
    stmt_close (s);
  return Qnil;
}

/* ------------------------------------------------------------------ */
/* Init                                                                */

static void
defun (emacs_env *env, const char *name, ptrdiff_t min, ptrdiff_t max,
       emacs_value (*f) (emacs_env *, ptrdiff_t, emacs_value *, void *),
       const char *doc, void *data)
{
  emacs_value fn = env->make_function (env, min, max, f, doc, data);
  emacs_value args[2] = { intern (env, name), fn };
  env->funcall (env, intern (env, "defalias"), 2, args);
}

int
emacs_module_init (struct emacs_runtime *rt)
{
  emacs_env *env;
  SQLRETURN rc;
  if (rt->size < (ptrdiff_t) sizeof *rt)
    return 1;
  env = rt->get_environment (rt);
  if (env->size < (ptrdiff_t) sizeof (struct emacs_env_28))
    return 2;                   /* make_unibyte_string: Emacs 28 */
  Qnil = env->make_global_ref (env, intern (env, "nil"));
  Qt = env->make_global_ref (env, intern (env, "t"));

  rc = SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv);
  if (!SQL_SUCCEEDED (rc))
    return 3;
  rc = SQLSetEnvAttr (henv, SQL_ATTR_ODBC_VERSION,
                      (SQLPOINTER) (intptr_t) SQL_OV_ODBC3, 0);
  if (!SQL_SUCCEEDED (rc))
    return 4;

  defun (env, "qss-odbc--connect", 1, 2, Fconnect,
         "Connect with CONNECTION-STRING; AUTOCOMMIT (default t).\n\n"
         "(fn CONNECTION-STRING &optional AUTOCOMMIT)", NULL);
  defun (env, "qss-odbc--disconnect", 1, 1, Fdisconnect,
         "Close CONNECTION and all its statements.\n\n(fn CONNECTION)",
         NULL);
  defun (env, "qss-odbc--connected-p", 1, 1, Fconnected_p,
         "Return t if OBJECT is an open ODBC connection.\n\n(fn OBJECT)",
         NULL);
  defun (env, "qss-odbc--commit", 1, 1, Fend_transaction,
         "Commit the transaction of CONNECTION.\n\n(fn CONNECTION)",
         (void *) 1);
  defun (env, "qss-odbc--rollback", 1, 1, Fend_transaction,
         "Roll back the transaction of CONNECTION.\n\n(fn CONNECTION)",
         NULL);
  defun (env, "qss-odbc--set-autocommit", 2, 2, Fset_autocommit,
         "Switch autocommit of CONNECTION on or off.\n\n(fn CONNECTION ON)",
         NULL);
  defun (env, "qss-odbc--set-sql-utf8", 2, 2, Fset_sql_utf8,
         "Treat CONNECTION as an ANSI driver that expects UTF-8 (SQLite,\n"
         "psqlODBC ANSI): SQL text, text parameters and character columns\n"
         "go as UTF-8 bytes instead of UTF-16.\n\n"
         "(fn CONNECTION ON)", NULL);
  defun (env, "qss-odbc--get-info", 2, 2, Fget_info,
         "String information about CONNECTION: KEY is dbms-name,\n"
         "dbms-version, driver-name, driver-version, identifier-quote\n"
         "or catalog-separator.\n\n(fn CONNECTION KEY)", NULL);
  defun (env, "qss-odbc--exec", 2, 3, Fexec,
         "Execute SQL on CONNECTION with PARAMS (a vector); a statement.\n\n"
         "(fn CONNECTION SQL &optional PARAMS)", NULL);
  defun (env, "qss-odbc--catalog", 3, 3, Fcatalog,
         "Run catalog function KIND (tables, columns, primary-keys)\n"
         "with ARGS (a vector, nil = any); a statement.\n\n"
         "(fn CONNECTION KIND ARGS)", NULL);
  defun (env, "qss-odbc--columns", 1, 1, Fcolumns,
         "Columns of STATEMENT: vector of\n(NAME TYPE SIZE DIGITS NULLABLE TYPE-NAME)."
         "\n\n(fn STATEMENT)", NULL);
  defun (env, "qss-odbc--fetch", 1, 1, Ffetch,
         "Next row of STATEMENT as a vector of raw values, or nil.\n\n"
         "(fn STATEMENT)", NULL);
  defun (env, "qss-odbc--row-count", 1, 1, Frow_count,
         "Rows affected by STATEMENT.\n\n(fn STATEMENT)", NULL);
  defun (env, "qss-odbc--more-results", 1, 1, Fmore_results,
         "Advance STATEMENT to its next result set; nil if none.\n\n"
         "(fn STATEMENT)", NULL);
  defun (env, "qss-odbc--close", 1, 1, Fclose,
         "Free STATEMENT now instead of at garbage collection.\n\n"
         "(fn STATEMENT)", NULL);

  {
    emacs_value feature = intern (env, "qss-odbc-module");
    env->funcall (env, intern (env, "provide"), 1, &feature);
  }
  return 0;
}
