# SQLiteDAV conventions

## Feature parity: sqlar AND plain SQLite

Both sqlar archives and plain SQLite databases are first-class deployment
targets. Any WebDAV method handler must work for both modes, with these
mappings:

- **Plain DB**: tables → folders, rows → folders (keyed by rowid),
  columns → files named `<col>.<ext>` where the extension is derived
  from the cell's type.
- **sqlar**: paths inside the archive table are filesystem-like.

When adding or changing a handler, implement and exercise both code paths
in unit tests before shipping. Returning a no-op (`traceM` / `NoContent`
without DB effects) for plain mode silently makes the server look
healthier than it is — Litmus only stresses the URL it's pointed at, so
gaps in the other mode go unnoticed.

### Per-method mapping in plain mode

| URL shape                  | meaning              |
|----------------------------|----------------------|
| `/`                        | database root        |
| `/<table>/`                | a table (folder)     |
| `/<table>/<rowid>/`        | a row (folder)       |
| `/<table>/<rowid>/<col>.x` | a cell (file)        |

- `GET /table/rowid/col.x` → SELECT the cell value.
- `PUT /table/rowid/col.x` → UPDATE that cell. Parent row must exist
  (409 otherwise); column must exist (404 otherwise). Returns 201 if
  the cell was previously NULL, 204 otherwise.
- `DELETE /table/rowid/col.x` → set cell to NULL.
- `DELETE /table/rowid/` → DELETE the row.
- `DELETE /table/` → DROP the table.
- `MKCOL /new-table/` → CREATE a new table (with the sqlar schema, so
  later writes under it work). 405 if it already exists.
- `MKCOL /table/<rowid>/` → INSERT a row with that rowid (other columns
  NULL). 405 if the row already exists, 409 if a NOT NULL constraint
  without a default blocks the insert.
- `COPY` / `MOVE` are supported within a table:
  - cell → cell (read source cell, write to destination)
  - row → row (clone all column values, optionally delete source)
  Cross-table operations are refused with 502 (mirrors sqlar's
  cross-archive policy).
