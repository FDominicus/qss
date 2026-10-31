# Changes to the Chinook scripts

The scripts are those of Chinook 1.4.5
(https://github.com/lerocha/chinook-database/releases/tag/v1.4.5, MIT, see
LICENSE.md), with one change:

- `Chinook_PostgreSql.sql`: the string literals are written `'...'`
  instead of `N'...'`.  In PostgreSQL `N'...'` is of type `character`,
  whose trailing spaces do not count and are dropped when the value goes
  into a `VARCHAR` column: the city `'Edinburgh '` (customer 54 and his
  invoices) became `'Edinburgh'`, while SQLite and MySQL keep the space.
  Without the `N` all three hold the same text.
