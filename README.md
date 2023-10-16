# SQLiteDAV

WebDAV server that maps an SQLite database to directories/files.

| | |
---|---
Database Schema | ![SQL Schema Code](./images/2023-10-16t1031_sql.png)
File View | ![Screenshot of macOS Finder](./images/2023-10-15t1454_finder.png)


## Installation

Prerequisite:
[Install Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)

```sh
git clone https://github.com/Airsequel/SQLiteDAV
cd SQLiteDAV
stack install
```


## Usage

1. Start WebDAV server:
    ```sh
    sqlitedav --dbpath path/to/database.sqlite
    ```
2. Connect your WebDAV client to `http://localhost:3543` \
    (E.g. with macOS Finder by executing `cmd + k`)
