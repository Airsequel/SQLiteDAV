# SQLiteDAV

WebDAV server that maps an SQLite database to directories/files.

| | |
---|---
Database Schema | ![SQL Schema Code](./images/2023-10-16t1031_sql.png)
File View | ![Screenshot of macOS Finder](./images/2023-10-17t2031_finder.png)


## Installation

### From Binaries

1. Go to https://github.com/Airsequel/SQLiteDAV/releases
1. Download the latest release for your platform
1. Unzip the archive:
    ```sh
    unzip sqlitedav_*.zip
    ```
1. Make the binary executable:
    ```sh
    chmod +x sqlitedav
    ```


### From Source

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
2. Connect your WebDAV client to `http://localhost:1234` \
    (E.g. with macOS Finder by executing `cmd + k`)


## Roadmap

The next features are implemented based on popular demand.
So please upvote any [issues](https://github.com/Airsequel/SQLiteDAV/issues)
you would like to see implemented!


## Related

- [github.com/adamobeng/wddbfs][wddbfs] - [webdavfs] provider that can read sqlite databases.
- [sqlite.org/cloudsqlite][cloudsqlite] - Cloud backed SQLite system.
- [sqlite-fs] - Mount a SQLite database as a normal filesystem on Linux and macOS.

[sqlite-fs]: https://github.com/narumatt/sqlitefs
[cloudsqlite]: https://sqlite.org/cloudsqlite/doc/trunk/www/index.wiki
[wddbfs]: https://github.com/adamobeng/wddbfs
[webdavfs]: https://github.com/miquels/webdavfs
