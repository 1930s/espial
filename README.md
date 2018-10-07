# Espial

Espial is an open-source, web-based bookmarking server.

It allows mutiple accounts, but currently intended for self-host scenarios.

The bookmarks are stored in a sqlite3 database, for ease of deployment & maintenence.

The easist way for logged-in users to add bookmarks, is with the "bookmarklet", found on the Settings page.

## demo server

log in — username: demo  password: demo

https://esp.ae8.org/u:demo

![jpg](https://i.imgur.com/y8PE0Ul.png)



## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`

## Espial Setup

1. Build executables
 
    ```
    stack build
    ```

2. Create the database

    ```
    stack exec migration -- createdb --conn espial.sqlite3
    ```

3. Create a user

    ```
    stack exec migration -- createuser --conn espial.sqlite3 --userName myusername --userPassword myuserpassword
    ```

4. Import a bookmark file for a user (optional)

    ```
     stack exec migration -- importbookmarks --conn espial.sqlite3 --userName myusername --bookmarkFile sample-bookmarks.json
    ```

5. Start a development server:

    ```
    stack exec -- yesod devel
    ```

6. Start a production server:

    ```
    stack exec espial
    ```

## Import Bookmark file format

see `sample-bookmarks.json`, which contains a JSON array, each line containing a `Post` object. 

example:

```
[ {"href":"http:\/\/mqtt.org\/","description":"MQTT","extended":"long description","time":"2018-02-25T21:22:42Z","shared":"yes","toread":"no","tags":"mqtt"}
, {"href":"http:\/\/mqtt.com\/","description":"MQTT","extended":"big  description","time":"2019-02-25T21:22:42Z","shared":"yes","toread":"no","tags":"mqtt"}
]

```
