# linux.usu.edu

This is the source of the [linux.usu.edu](linux.usu.edu) website, a rewrite of the old [usufslc.com](https://github.com/USUFSLC/usufslc-elixir)! Written in Common Lisp, using [Mito](https://github.com/fukamachi/mito), [Hunchentoot](https://edicl.github.io/hunchentoot/), (some parts of) [Caveman](https://github.com/fukamachi/caveman), and [LSX](https://github.com/fukamachi/lsx).

Current features include:

- Arguably the most important, a Dark _and_ Light theme based on Gruvbox!
- A custom "shell" in JavaScript.
- Discord OAuth2 login for club members.
- Concurrent multi-user RTMP live streams via some NGINX hacking.

## Are you a member? Want to add your own page?

1. Fork this repo.
2. Use the [`make_user_page.sh`](./make_user_page.sh) script: `./make_user_page.sh <your_username>`.
3. Optionally, edit and add to the generated static files in `public/pages/<your_username>`.
4. Push your changes to your fork.
5. Make a pull request to this repo!

## Development Setup

You'll need:

- [PostgreSQL](https://www.postgresql.org/)
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Node.js](https://nodejs.org/en/) and [npm](https://www.npmjs.com/) to build the frontend.

Additionally I _highly recommend_ Emacs with [Swank](https://quickref.common-lisp.net/swank.html) for development due to the ease of dropping into a REPL and making changes on the fly. It's 100% worth learning if you're seriously interested in working on this project, or any Lisp project for that matter.

Then go ahead and create a database in PostgreSQL, which you'll need to configure below.

## Configuration

The configuration system is written in a custom format parsed by [config.lisp](src/config/parser.lisp) that I stole from my [Speedrun Timer](https://github.com/Simponic/speedrun-timer) project.

When the application is run, it will look for a file named `config/APP_ENV.conf` where `APP_ENV` is the value of the environment variable `APP_ENV`. If `APP_ENV` is not set, it will default to `development`. Therefore, if you plan on doing some development, copy `config/development.conf.example` to `config/development.conf` and edit it to your liking!

Note the `db` section! You'll need to set the `:username`, `:password`, and `:name` properties to the values you set up for a new PostgreSQL database in the previous step.

## Frontend Shtuff

The frontend is bundled via ESBuild, and to do so, cd into `frontend/` and run `npm install` to install the dependencies then `npm run build`. You can also `npm run watch` to have the frontend rebuild on changes.

If you want to add a new program to the "shell" presented on the home page, all that stuff is in `frontend/js/shell`. You can add a new command by creating a new file in that `frontend/js/shell/commands`, and then by importing it in `frontend/src/js/shell/main.js` and adding it in the `FILES` object, in some nested index specified by the `PATH` env var - by default, `/usr/bin` and `/usr/local/bin`.

## Running

Finally, you can run the application (and auto-migrate the database schema if `:auto-migrate`'s set in the configuration) with:

```bash
$ sbcl --load usufslc.asd
* (ql:quickload :usufslc)
* (usufslc:start)
```

## Seeding

With the database schema migrated, you can seed the database with some initial data by running:

```bash
$ sbcl --load usufslc.asd
* (ql:quickload :usufslc)
* (usufslc.db.seed::seed-streaming-context)
```

## Testing

Some unit and integration tests do have the property of existing. They can be run via:

```
sbcl --load usufslc.asd
(ql:quickload 'usufslc)
(asdf:test-system 'usufslc)
```

There are a few integration tests that change the database. If they run correctly they will clean themselves up, but it's a good idea to create a configuration file `testing.conf` in `config` equivalent to `development.conf` with a new database and running with `APP_ENV=testing`.

The rendering tests also change the root template to a non-existant file path, which is global, so you'll want to restart your SLIME session before re-running the actual server if you're using a SLIME session.

## Copyright

There is no copyright, and this is free software! Do whatever you want with (in accordance with the MIT License)!
