# slackat

> slack message scheduler

## Build/Installation

Build from source or see [releases](https://github.com/jaemk/slackat/releases)
for pre-built executables (jre is still required)

```
# generate a standalone jar wrapped in an executable script
$ lein bin
```

## Database

[`migratus`](https://github.com/yogthos/migratus) is used for migration management.

```
# create db
$ sudo -u postgres psql -c "create database slackat"

# create user
$ sudo -u postgres psql -c "create user slackat"

# set password
$ sudo -u postgres psql -c "alter user slackat with password 'slackat'"

# allow user to create schemas in database
$ sudo -u postgres psql -c "grant create on database slackat to slackat"

# allow user to create new databases
$ sudo -u postgres psql -c "alter role slackat createdb"

# apply migrations from repl
$ lein with-profile +dev repl
user=> (cmd/migrate!)

# running the app from "main" will also apply migrations
# lein run
```

## Usage

```
# start the server
$ export PORT=3003        # default
$ export REPL_PORT=3999   # default
$ bin/slackat

# connect to running application
$ lein repl :connect 3999
user=> (initenv)  ; loads a bunch of namespaes
user=> (cmd/migrate!)
```

## Testing

```
# run test
$ lein midje

# or interactively in the repl
$ lein with-profile +dev repl
user=> (autotest)
```

## Docker

```
# build
$ docker build -t slackat:latest .
# run
$ docker run --rm -p 3003:3003 -p 3999:3999 --env-file .env.values slackat:latest
```

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
