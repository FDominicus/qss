# qss — tables and one vocabulary for all collections in Emacs

qss brings the idea of [Nushell](https://www.nushell.sh/) to Emacs: data
as tables, and one small vocabulary that works on all of them — files,
processes, property lists, rows of an SQLite table, and whatever Nushell
itself produces.

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
The rows stay what they are: `RET` on a row opens the file, `a` (or
the right mouse button) offers its actions.

A line that starts with a word, like `(select name size)` above, or
with `|`, continues the last table; a line that starts with a source,
like `(files)` or `(nu "...")`, starts a new one:

```
qss> (files "~/Downloads")
qss> | (nu "where size > 100mb")
qss> | (sort-by size :desc)
```

| Key | In the REPL |
|---|---|
| `RET` | on the input line: run it; on a table row: open the row |
| `a`, right mouse button | on a table row: the actions of the row, words with the value at point, copying the table |
| `w` | on a table row: copy the whole table as text (`C-u w` asks for the format) |
| `S` | on a table row: sort by the column at point |
| `M-p` / `M-n` | previous / next input |
| `TAB` | complete words, sources, column names and file names |
| `C-c M-o` | clear the buffer |
| `[buffer]` | show the table in a buffer of its own |

In a table buffer `RET`, `a`, `w` and `S` work the same, and `g` reads
the source again.

### Actions at point

`a` or the right mouse button on a cell offers, in three groups:

- the actions of the row object: open the file, Dired, kill the process …
- words with the value under point: `(where (= :status "paid"))`,
  `(where (/= ...))`, `(where (> :amount 120.0))` for numbers,
  `(where (is-null :city))` for an empty cell, for a time the day:
  `(where (and (>= :modified "2026-10-22") (< :modified "2026-10-23")))`;
  then `(sort-by COL)`, `(sort-by COL :desc)`, `(group-by COL)`,
  `(select COL)`, `(reject COL)` and copying the value;
- copying the whole table as text, Org, Markdown, CSV or TSV.

A word continues the *whole* table, not only the rows the REPL shows,
and on SQLite it continues the query, so the condition becomes SQL. In
the REPL the word is submitted as the next input — visible and in the
history — and continues the table that was clicked, even an older one;
what you had typed at the prompt stays there. In a table buffer the
result opens in a buffer of its own.

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

`(nu COMMAND)` runs `nu -c "(COMMAND) | to nuon"` in
`default-directory` (on the remote host in a TRAMP buffer) and reads
the result: a list of records becomes the rows, a single record rows of
`column` and `value`. `RET` on a row from `ls` opens the file.
`qss-nu-program` names the program, `qss-nu-arguments` its arguments
(by default your config files are not loaded).

NUON is Nushell's own format. Unlike JSON it keeps the types: sizes
(`10kb`), durations (`3hr`) and datetimes become columns of the types
`size`, `duration` and `time`, shown as `9.8k`, `3hr 20min` and
`2026-10-22 14:40`, sorted and compared as numbers and times. qss reads
NUON with its own reader (qss-nuon.el; `qss-nuon-read` and
`qss-nuon-encode` can be used on their own). If a Nushell writes NUON
that qss cannot read, set `qss-nu-format` to `json`; then sizes are only
recognized by the column name (`qss-nu-size-columns`) and durations are
plain numbers of nanoseconds.

In the middle of a pipe, `nu` works the other way round too: the table
so far goes to Nushell as `$in`, and what comes back is a table again.

```
qss> (sqlite (qss-demo-database) "Orders") (where (is-null :status))
       (nu "to csv | save -f open-orders.csv")
qss> (files) (nu "where size > 10kb | get size | math sum")
qss> (files) (where (> :size 1000)) (nu "sort-by modified -r") (first 5)
```

The rows go over as NUON with their types: time columns arrive as
datetimes, size columns as file sizes and duration columns as
durations, so `where size > 10kb` and `where modified > 2026-01-01`
work. A single value (like a sum) comes back as it is; nothing (like
from `save`) comes back as `nil`. `g` in the table buffer runs the whole
pipe again, the qss part and the Nushell part.

Nushell must be installed (tested with 0.116); without it everything
else works.

## Words

| Word | Meaning |
|---|---|
| `(where COND)` | rows for which COND holds; `filter` is the same |
| `(select COL...)` / `(reject COL...)` | keep / drop columns |
| `(sort-by COL [:desc])` | sort; missing values last |
| `(first [N])` / `(last [N])` | the first / last N rows |
| `(get COL)` | the values of one column |
| `(group-by COL)` | one row per value with a count; `RET` shows the group |
| `(each FORM)` | FORM with `it` bound to each row; results are the new rows |
| `(nu COMMAND)` | the table through a Nushell command (see above) |
| `(to FORMAT)` | the table as text: `text`, `org`, `md`, `csv`, `tsv` |
| `columns` | the column names |
| `count` | the number of rows |
| `collect` | the rows as a Lisp list |
| `sql` | the SQL of a query (SQLite only) |

`collect` after `get` returns the plain values.

`(to text)`, `(to org)` and `(to md)` are aligned and show the cells as
the table does (`9.8k`, `2026-10-22 14:40`); `(to csv)` and `(to tsv)`
carry the plain values for other programs: sizes in bytes, durations in
nanoseconds, times with seconds. `w` on a table uses the same, in the
format of `qss-copy-format`.

```
qss> (sqlite (qss-demo-database) "Customers") (select name city) (to org)
| name        | city     |
|-------------+----------|
| Anna Meier  | Bruchsal |
...
```

### Conditions

Columns are keywords; everything else is Lisp, variables included:

```elisp
(where (> :size 10000))
(where (and (>= :date since) (in :kind kinds)))
(where (like :name "%.el"))
(where (is-null :invoice-no))
```

Comparisons mean what they mean in SQL: `=`, `<` … work on numbers,
strings and times (`(> :modified "2026-10-20")` works), sizes and
durations can be written as in Nushell (`(> :size "10kb")`,
`(< :took "1hr")`), `like` ignores
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
qss> (tables (qss-demo-database))
```

On an SQLite source the pipe stays a query until the rows are needed.
`where`, `sort-by`, `first`, `count`, `select`, `reject` and `columns`
become SQL with `?` parameters; a word that cannot (a condition with
`it`, `each`, `group-by`, `last`, `get`) fetches the rows and goes on in
memory.

```
qss> (sqlite (qss-demo-database) "Orders") (where (> :amount 100)) (sort-by order-date :desc) (first 20) sql
SELECT *
FROM "Orders"
WHERE ("Amount" > ?)
ORDER BY "OrderDate" IS NULL, "OrderDate" DESC
LIMIT ?
Parameters: (100 20)
```

Sizes and durations written as in Nushell work in SQL too:
`(where (> :size "10kb"))` sends the parameter 10000. For `<`, `>`, `<=`
and `>=` this holds for every column without a text type; for `=`, `/=`
and `in` only for columns of a numeric type, so `"10kb"` in a text
column stays text.

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

(qss-> (files) (nu "get size | math sum"))
;; => 1234567

(qss-nuon-read "[[name, size]; [foo, 10kb]]")
;; => (((name . "foo") (size . #s(qss-nuon-size 10000))))
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

Column types are `number`, `size` (bytes), `duration` (nanoseconds) and
`time` (a Lisp time); they decide alignment, display and sorting.

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

The SQLite tests are skipped when Emacs has no SQLite, the Nushell tests
when `nu` is not installed.

## License

MIT, see [LICENSE](LICENSE).
