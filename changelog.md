# Changelog

All notable user-facing changes to this project.


## 2026-05-26 - 0.2

- Improved WebDAV compliance based on Litmus findings.
- Deleting a file by setting its cell to `NULL`.
- First-class support for SQLite Archive Files (sqlar).
- Binary file extension detection via libmagic.
- `LOCK` and `UNLOCK` method support.
- Support for the `Prefer` header values `depth-noroot` and `return=minimal`.
- `PROPFIND` no longer loads every row's BLOB content into memory,
    fixing OOMs on multi-gigabyte databases.
- Editing a cell via `PUT` preserves the column's declared type
    (TEXT, INTEGER, REAL, BLOB) instead of forcing every write to BLOB.
- `--rowname` CLI flag controls how plain-table row directories are
    named: `rowid` (default), `pk` (primary-key value), or `combined`
    (`<rowid> - <pk-value>`). Tables without a single-column PK keep
    the rowid naming. (#1)


## 2023-10-30 - 0.1.1.0

- Initial release of SQLiteDAV, a lightweight WebDAV server backed by SQLite.
- `PROPFIND`, `GET`, `PUT`, `MOVE`, `COPY`, and `DELETE` method handlers.
- Listing of database columns as files.
- `OPTIONS` request support with both `application/xml` and `text/xml`.
- CLI with `--db-path` flag.
- Loading of actual content length, modification date, and file extension
  from the database.
- CI builds of binaries for Linux and macOS.
