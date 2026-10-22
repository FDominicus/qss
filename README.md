# qss — tables and one vocabulary for all collections in Emacs

qss brings the idea of [Nushell](https://www.nushell.sh/) to Emacs: data
as tables, and one small vocabulary that works on all of them — files,
processes, property lists, rows of an SQLite table.

```
M-x qss
qss> (files "~") (where (> :size 100000)) (sort-by size :desc) (first 5)
5 rows  [buffer]  files ~/ | where (> :size 100000) | sort-by size desc | first 5
name                     type  size modified         mode       user
big-archive.tar          file  2.1G 2026-10-20 09:12 -rw-r--r-- frido
...
qss> (select name size)
```

`M-x qss` opens a REPL: type a pipe, the table appears right below.
The rows stay what they are: `RET` on a row opens the file, `a` offers
its actions, `S` sorts by the column at point. A line that starts with
a word, like `(select name size)` above, continues the table before it.
`M-p`/`M-n` walk the history, `TAB` completes words, sources and column
names, `[buffer]` opens a table in a buffer of its own, where `g` reads
the source again.

`M-x qss-eval` does the same from the minibuffer, with the result in a
table buffer.

### The REPL as a shell

```
~/ qss> cd ~/src/project
~/src/project/ qss> !git log --oneline -5
~/src/project/ qss> !grep -n TODO main.el
~/src/project/ qss> (where (like :line "%urgent%"))
~/src/project/ qss> (files) (where (= :type "dir"))
```

- `!PROGRAM ARGS` runs a program directly, **without a shell**. The output
  appears as text, and it is the last table too, one row per line in the
  column `line`, so the next input can filter it. Quotes `"..."` and
  `'...'` and `~` work as in a shell; `;`, `|`, `>` and globs are plain
  characters. For those, say so: `!sh -c "make && make test"`, or use
  Nushell: `(nu "ls *.el | where size > 10kb")`.
- `cd DIR` changes the directory (`cd ..`, `cd` for home, spaces in names
  need no quotes). The prompt shows it; `files`, `nu`, `run` and `!` all
  run there — on the remote host in a TRAMP directory.
- `RET` on a directory in a table does `cd` there and lists its files.
- `TAB` completes file names after `!` and `cd` and inside strings.
- `(run PROGRAM ARGS...)` is the same as a source for pipes.

Only what is built into Emacs 29 is used: `vtable`, `sqlite` and
`cl-generic`. No other packages.

## Install

Emacs 29.1 or later.

```elisp
(package-vc-install "https://github.com/FDominicus/qss")
(require 'qss)
```

Or clone the repository, put the directory on your `load-path` and
`(require 'qss)`.

## Sources

| Source | Rows |
|---|---|
| `(files DIR)` | files of DIR (TRAMP works) |
| `(processes)` | running processes |
| `(sqlite FILE TABLE)` | rows of an SQLite table, as a query |
| `(tables FILE)` | the tables of an SQLite file; `RET` shows the rows |
| `(nu COMMAND)` | the result of a Nushell command |
| `(run PROGRAM ARGS...)` | the output lines of a program (column `line`), no shell |
| any list | its elements; property lists are rows, other values have the column `value` |

### Nushell

Everything Nushell produces becomes a table, and the qss words go on
from there:

```
qss> (nu "ps | where cpu > 5")
qss> (nu "open export.csv") (where (> :amount 100)) (sort-by date :desc)
qss> (nu "sys disks")
qss> (nu "ls **/*.el") (sort-by size :desc) (first 10)
```

`(nu COMMAND)` runs `nu -c "(COMMAND) | to json --raw"` in
`default-directory` (on the remote host in a TRAMP buffer) and reads
the JSON: a list of records becomes the rows, a single record rows of
`column` and `value`. `RET` on a row from `ls` opens the file.
`qss-nu-program` names the program, `qss-nu-arguments` its arguments
(by default your config files are not loaded).

## Words

| Word | Meaning |
|---|---|
| `(where COND)` | rows for which COND holds |
| `(select COL...)` / `(reject COL...)` | keep / drop columns |
| `(sort-by COL [:desc])` | sort; missing values last |
| `(first [N])` / `(last [N])` | the first / last N rows |
| `(get COL)` | the values of one column |
| `(group-by COL)` | one row per value with a count; `RET` shows the group |
| `(each FORM)` | FORM with `it` bound to each row; results are the new rows |
| `columns` | the column names |
| `count` | the number of rows |
| `collect` | the rows as a Lisp list |
| `sql` | the SQL of a query (SQLite only) |

`collect` after `get` returns the plain values.

### Conditions

Columns are keywords; everything else is Lisp, variables included:

```elisp
(where (> :size 10000))
(where (and (>= :date since) (in :kind kinds)))
(where (like :name "%.el"))
(where (is-null :invoice-no))
```

Comparisons mean what they mean in SQL: `=`, `<` … work on numbers,
strings and times (`(> :modified "2026-10-20")` works), `like` ignores
case, and a missing value is NULL — comparing with it is unknown, so the
same condition returns the same rows in memory and in SQLite.

If `it` occurs, the condition is ordinary Lisp with `it` bound to the row:

```elisp
(where (string-prefix-p "." (qss-field it 'name)))
```

### SQLite

To try it without a database of your own, `(qss-demo-database)`
creates a small one with the tables `Customers` and `Orders`; all
SQLite examples here use it.

```
qss: (tables (qss-demo-database))
```

On an SQLite source the pipe stays a query until the rows are needed.
`where`, `sort-by`, `first`, `count`, `select`, `reject` and `columns`
become SQL with `?` parameters; a word that cannot (a condition with
`it`, `each`, `group-by`, `last`, `get`) fetches the rows and goes on in
memory.

```
qss: (sqlite (qss-demo-database) "Orders") (where (> :amount 100)) (sort-by order-date :desc) (first 20) sql
SELECT *
FROM "Orders"
WHERE ("Amount" > ?)
ORDER BY "OrderDate" IS NULL, "OrderDate" DESC
LIMIT ?
Parameters: (100 20)
```

Column and table names become keys mechanically: `OrderDate` →
`order-date`, `HT_Orders` → `ht-orders`, `Straße` → `strasse`.
Display fetches at most `qss-sql-display-limit` rows (1000).

## In Lisp

```elisp
(qss-> (sqlite (qss-demo-database) "Orders")
       (where (> :amount 100))
       (select order-no amount)
       collect)
;; => ((:order-no 4711 :amount 120.0) ...)

(qss-display (qss-> (files user-emacs-directory) (where (like :name "%.el"))))
```

`qss->` returns a table, or a query on SQLite; `collect` turns either
into a list, `qss-display` shows it.

## Own row types

A row type is a few methods:

```elisp
(cl-defmethod qss-columns ((_ my-thing)) '(name (size :type size) (date :type time)))
(cl-defmethod qss-field ((x my-thing) name) ...)
(cl-defmethod qss-default-action ((x my-thing)) ...)   ; RET
(cl-defmethod qss-actions ((x my-thing)) (list (cons "Do it" #'my-do-it)))  ; a
```

## Examples

```
M-x qss-examples
```

shows all examples of `qss-examples.el` with their results; `[show]`
displays a result as a table. The SQLite examples use a small demo
database that is created in `temporary-file-directory`. The file is
also meant to be read: every example is one form you can evaluate with
`C-x C-e`.

## Tests

From inside Emacs:

```
M-x qss-run-tests
```

From a shell, from any directory:

```
emacs --batch -L /path/to/qss -l qss -f qss-run-tests-batch
```

The SQLite tests are skipped when Emacs has no SQLite.

## License

MIT, see [LICENSE](LICENSE).
