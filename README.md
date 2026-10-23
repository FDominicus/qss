# qss — tables and one vocabulary for all collections in Emacs

qss brings the idea of [Nushell](https://www.nushell.sh/) to Emacs: data
as tables, and one small vocabulary that works on all of them — files,
processes, property lists, rows of an SQLite table, and whatever Nushell
itself produces.

Around it are a few more old ideas, each with its own section: the
words become SQL as in dbplyr; SQLite is source and store; what is
shown stays the object it shows and can be pointed at, as in CLIM
(*presentations* and *accept*); and the interface is checked by
contracts with blame, as in Racket. [Where the ideas come
from](#where-the-ideas-come-from) says more.

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
| `C-c C-v` | while typing: take a value from a table into the input, by a click |
| `C-c C-f` | while typing: take the name of a file row into the input |
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

What is shown in the REPL stays the object it shows, and can be
pointed at later: `C-c C-v` while typing highlights the cells above, and
a click puts the value into the input. See
[Presentations and accept](#presentations-and-accept).

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

Only what is built into Emacs 29 is used: `vtable`, `sqlite`,
`cl-generic` and advice. No other packages.

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
| `(sqlite TABLE)` | the same in the central file `qss-database` |
| `(tables [FILE])` | the tables of an SQLite file (default `qss-database`); `RET` shows the rows |
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
| `(into sqlite NAME ...)` | store the table in SQLite; go on with it as a query (see below) |
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

### Storing in SQLite

`into` is the way back: the table of a pipe goes into SQLite and comes
out as a query, so the words go on there and become SQL.

```
qss> (files "~") (where (> :size "10mb")) (into sqlite "big-files")
qss> (sqlite "big-files") (where (> :modified "2026-01-01")) (sort-by size :desc)
qss> (nu "sys disks") (into sqlite "disks" :append)
qss> (sqlite (qss-demo-database) "Orders") (where (is-null :status))
       (into sqlite "open" :file "~/work/orders.sqlite3" :replace)
```

- Without `:file` the table goes into the central file `qss-database`
  (`~/.emacs.d/qss.sqlite3`); `(sqlite "big-files")` and `(tables)`
  read from there.
- If the table exists, `into` refuses unless told: `:replace` drops it
  first, `:append` adds the rows. All in one transaction.
- Sizes are stored as INTEGER bytes, durations as INTEGER nanoseconds,
  times as ISO text `2026-10-22 07:11:05` (local time, the form SQLite
  itself uses; `qss-time-format`). So `(> :size "10kb")` and
  `(> :modified "2026-01-01")` work on the stored table too. The qss
  types are recorded in a table `_qss_columns`, so sizes and durations
  show as `9.8k` and `3hr` again; `tables` does not list it.
- A file row also stores its `path`, so `RET` on the stored row opens
  the file (or enters the directory in the REPL). Other row types can
  name fields to store with the generic `qss-stored-fields`.

Column and table names become keys mechanically: `OrderDate` →
`order-date`, `HT_Orders` → `ht-orders`, `Straße` → `strasse`.
Display fetches at most `qss-sql-display-limit` rows (1000).

## Presentations and accept

The idea comes from CLIM, the Common Lisp Interface Manager (and
before it Dynamic Windows on the Lisp machines): output remembers what
it shows, so it can be used as input later. In a CLIM listener you type
a command, and where it wants a file you click a file that some earlier
output printed — the program gets the file object, not its name as
text.

qss does the same in Emacs buffers. A buffer is the store: whatever
qss (or your own code) has shown in a buffer that is visible can be
chosen.

### What a presentation is

A *presentation* is a piece of text in a buffer together with an object
and a *presentation type*:

- every **row** of a table is a presentation of its row object: a file
  row of a `qss-file` (type `file` or `directory`), a process row of a
  process, a row of `tables` of a `db-table`, a row of data of a `row`
  — or of a `file`, if it has a `path` (a file row stored with `into`);
- every **cell** is a presentation of its value: `size`, `duration` and
  `time` from the type of the column, otherwise from the value itself
  (`integer`, `number`, `string`, `time`, `symbol`); empty cells (NULL)
  are none;
- what the **REPL prints** — a number, a line of text, a time — is one
  too;
- **any text** in any buffer, made with `qss-present`:

```elisp
;; A buffer of notes with some files in it.  Every file name is a
;; presentation of that file, as good as a file row in a table.
(with-current-buffer (get-buffer-create "*to read*")
  (erase-buffer)
  (insert "Files to look at:\n")
  (dolist (f (directory-files "~/notes" t "\\.org\\'"))
    (insert "  ")
    (qss-present (list :path f) 'file (file-name-nondirectory f))
    (insert "\n"))
  (display-buffer (current-buffer)))
```

The text stays plain text; the object rides along in the text property
`qss-presentation`.

### What accept does

`(qss-accept TYPE PROMPT)` asks for an object of TYPE:

1. It looks at **every visible window** (not at hidden buffers: what you
   see is what you can choose) and collects the presentations of TYPE —
   of TYPE itself or any type below it.
2. It **highlights** them. The prompt says how many there are:
   `with file: (3 shown, click one or type)`.
3. You answer in one of three ways:
   - **click** one (or move point onto it in its window and press
     `RET`): the answer is the object itself — the `qss-file`, the
     process, the number 3;
   - **type** its label, with completion over the highlighted ones (a
     file by its path, a process by PID and command ...);
   - **type** something new: the text is parsed for the type — a path
     for `file`, `"10kb"` for `size`, `"3hr"` for `duration`,
     `"2026-10-22"` for `time`, a PID for `process`.
4. `C-g` gives up; the highlighting is gone either way.

A row whose object fits the type is offered as a whole row; otherwise
its cells are looked at. So `file` offers file rows, `value` the cells.

### Types

```
t ─┬─ object ─┬─ file ─ directory
   │          ├─ process, db-table, row
   └─ value ──┬─ number ─ integer ─┬─ size
              │                    └─ duration
              └─ string, time, symbol
```

A directory is a file too, a size a number: accepting `file` offers
directories as well, accepting `directory` only directories. Every type
can parse typed text, or take it from the nearest type above that can
(`qss-presentation-parse`).

### Examples

**A value into the input.** In the REPL, while typing, `C-c C-v`
accepts a `value`:

```
qss> (sqlite (qss-demo-database) "Orders")
6 rows  ...
    4714           3 2026-10-02   45.0 paid
qss> (where (= :customer-no ▮          C-c C-v, all cells light up,
                                      click the 3
qss> (where (= :customer-no 3▮
```

A number goes in as a number, text as a string, a time as
`"2026-10-22 07:11:05"`. `C-c C-f` accepts a `file` and inserts its
name: `!wc -l ` `C-c C-f`, click a file row.

**Two files from two places.** Show two directories, each in a buffer
of its own (`[buffer]` after the table), side by side:

```
qss> (files "~/project/v1")      [buffer]
qss> (files "~/project/v2")      [buffer]
```

On `main.el` in the left window, `a` → *Compare with file...*: the left
row is the first file; every file row in both windows lights up; a click
on `main.el` on the right, and `diff` shows the differences.

**Only what fits.** On a file, *Copy into directory...* lights up only
the directories — rows of type `directory` — in every visible window,
the notes buffer above included if it presents some.

**A value as the start.** On a size cell, `a` offers *Files larger than
this, here*: the size is the argument, and the files of the current
directory larger than it come up.

**From Lisp.** Any code can ask:

```elisp
(let ((dir (qss-accept 'directory "Back up into: ")))
  (copy-directory "~/notes" (qss-file-name dir)))
```

### Commands with typed arguments

`qss-define-command` defines a command whose arguments are accepted by
type, after `define-command` in CLIM:

```elisp
(qss-define-command my-open-both "Open side by side"
    ((a file "Open: ") (b file "next to: "))
  "Show the files A and B side by side."
  (find-file (qss-file-name a))
  (find-file-other-window (qss-file-name b)))
```

- `M-x my-open-both` accepts both files, each by a click or by typing.
- On every presentation of the type of the first argument — every file
  row, every presented file in any buffer — `a` and the right mouse
  button offer *Open side by side...*; that presentation is the first
  argument, the others are accepted.
- `qss-file-name` turns what was chosen (a file row, a stored row, a
  typed path) into the file name.

qss brings three: *Compare with file* (`qss-compare-files`), *Copy into
directory* (`qss-copy-file-into`) and *Files larger than this, here*
(`qss-files-larger-than`).

### Own types

```elisp
(qss-define-presentation-type 'invoice 'object
  (lambda (text) (my-invoice-by-number text))   ; typed text -> invoice
  "An invoice.")
(cl-defmethod qss-presentation-type ((_ my-invoice)) 'invoice)
(cl-defmethod qss-presentation-label ((x my-invoice)) (my-invoice-number x))
```

Rows of `my-invoice` are then invoices for `qss-accept` and for
commands with an argument of type `invoice`.

### Compared with CLIM

qss takes the core — presentations with types, accept with
highlighting, commands with typed arguments offered on the objects —
and leaves out much: there are no presentation translators besides the
first argument of a command, no parsing of whole command lines in an
input editor, no presentation views, and only visible windows are
searched. What it adds is that every buffer can take part.

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
(cl-defmethod qss-stored-fields ((x my-thing)) '(id))                         ; into
(qss-define-presentation-type 'my-thing 'object)
(cl-defmethod qss-presentation-type ((x my-thing)) 'my-thing)                 ; accept
```

Column types are `number`, `size` (bytes), `duration` (nanoseconds) and
`time` (a Lisp time); they decide alignment, display and sorting.

The contracts of the protocol check these methods: a `qss-columns`
that returns something that is not a list of columns is reported as
the fault of that method, with its type (see [Contracts](#contracts)).

## Contracts

The public interface of qss is checked by contracts, after Racket: every
function says what it takes and what it returns, and when that is
broken, the message says **who** broke it.

```
qss> (processes) (first -1)
Contract violation: qss-first: argument 2 must satisfy natnump, got -1
  blame: the caller (outside the checked functions)
  contract: (-> qss-tabular (opt natnump) qss-result)
```

```
Contract violation: qss-columns: the result must satisfy (listof qss-column-spec),
got (name 42) (element 1 is 42, not qss-column-spec)
  blame: qss-columns itself, the method for my-thing
  contract: (-> any (listof qss-column-spec))
```

- An argument that breaks the contract is the fault of the **caller**;
  if that is a checked function itself, it is named
  (`blame: the caller, qss-continue`).
- A result that breaks it is the fault of the **function** — for a
  generic function, of the method for the type of the first argument.
  This is where own row types are caught: the methods for them must
  keep the protocol.

The whole interface is in one file, **qss-interface.el**, nearly fifty
functions, each with its contract — readable as the reference of qss:

```elisp
(qss-define-contract qss-sqlite  (-> stringp (opt (or/c stringp symbolp)) qss-relation-p))
(qss-define-contract qss-columns (-> any (listof qss-column-spec)))
(qss-define-contract qss-accept  (-> qss-presentation-type-name (opt stringp) any))
```

`C-h f` shows the contract of a function, and the contracts are a source
like any other:

```
qss> (contracts) (where (like :function "%sql%"))
```

The contracts are `:around` advice. They stand beside the code, not in
it; they hold for every method of a generic function, also methods
added later, and survive redefinition. They are not put on functions
that run once per row or per cell (`qss-field`, the formats of values):
at the boundaries they cost nothing noticeable — a pipe over 20000
rows is a few percent slower — inside the loops they would.
`qss-contract-enabled` turns all checks off.

### The language

| Contract | Means |
|---|---|
| `stringp`, `natnump`, `qss-table-p` … | a predicate |
| `any` | anything |
| `(or/c C ...)`, `(and/c C ...)`, `(not/c C)` | one of, all of, not |
| `(one-of V ...)` | one of these values |
| `(listof C)`, `(list/c C ...)`, `(cons/c A D)` | lists and conses |
| `(lambda (x) ...)` | a predicate written in place |
| `(-> ARG ... RESULT)` | a function; `(opt C)` an optional argument (nil always allowed), `(rest C)` the rest |
| `qss-column-spec` … | a contract named with `qss-define-contract-type` |

A function passed as an argument is only checked to be a function, not
wrapped (Racket does that; qss does not, yet).

### For your own code

The library, **qss-contract.el**, depends on nothing else of qss and can
be used for any Emacs Lisp:

```elisp
(require 'qss-contract)
(qss-define-contract-type 'my-weight '(and/c numberp (lambda (w) (>= w 0))))
(qss-define-contract my-tare (-> my-weight my-weight my-weight))
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

The SQLite tests are skipped when Emacs has no SQLite, the Nushell tests
when `nu` is not installed. The tests run with the contracts on, so
every test is a check of the contracts as well — and of the code
against them.

## Where the ideas come from

Little in qss is new; it puts old ideas together in Emacs.

- **Nushell** — data as tables, not as text; one vocabulary (`where`,
  `select`, `sort-by`, `get`, `first` …) for every source; the pipe.
  The names of the words, the units `10kb` and `3hr`, and NUON come
  from there. <https://www.nushell.sh/>
- **dbplyr** (R) — a pipe over a database table stays a query and is
  translated to SQL only when the rows are needed; the same words work
  in memory and in the database. <https://dbplyr.tidyverse.org/>
- **SQL** — the meaning of the comparisons: `like`, `in`, and NULL as
  unknown (three-valued logic), so that a condition returns the same
  rows in memory and in SQLite.
- **SQLite** — one file as the store of everything, source and target
  alike (`sqlite`, `into`).
- **CLIM** (Common Lisp Interface Manager), and Dynamic Windows of the
  Symbolics Lisp machines before it — presentations, presentation
  types, `accept`, commands with typed arguments offered on the
  objects shown. <https://mcclim.common-lisp.dev/> (McCLIM, a free
  implementation; [CLIM paper](https://mcclim.common-lisp.dev/static/documents/clim-paper.pdf))
- **CLOS** — generic functions with methods per type (in Emacs:
  `cl-generic`): the protocol of row types, so a file stays a file and
  a process a process, and own types fit in with a few methods.
- **Racket contracts** — contracts on functions at the boundaries, and
  blame: the caller or the function. After R. B. Findler and
  M. Felleisen, [*Contracts for Higher-Order Functions*](https://www.cs.northwestern.edu/~robby/pubs/papers/ho-contracts-techreport.pdf)
  (ICFP 2002; the link is the technical report).
  <https://docs.racket-lang.org/guide/contracts.html>
- **Emacs** itself — `vtable` for the tables, the built-in `sqlite`,
  advice for the contracts, text properties for the presentations,
  IELM for the shape of the REPL; and the conviction that the editor is
  the place to work in.

## License

MIT, see [LICENSE](LICENSE).
