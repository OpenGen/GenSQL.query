# GenSQL.query

![tests](https://github.com/OpenGen/GenSQL.query/workflows/tests/badge.svg)
![linter](https://github.com/OpenGen/GenSQL.query/workflows/linter/badge.svg)
[![Codecov branch][codecov]][codecov-url]

## Usage

### Dependencies and Nix

Many of the usages described below are powered by the Nix package manager, which
works on most Unix systems. We recommend installing Nix with the
[Determinate Systems installer](https://determinate.systems/posts/determinate-nix-installer/).
If you already have Nix installed, note that the precise minimum version of Nix
required is not known, but we have tested with Nix 2.19.3:

```shell
$ nix --version
nix (Nix) 2.19.3
```

Using Nix ensures you can get reliable version sets of all dependencies even when the project uses multiple languages and tools.

### Command-line interface

`GenSQL.query` provides a simple command-line application that allows the user to manually enter and evaluate GenSQL queries. Usage information can be printed with the following command.

To run the latest version of `GenSQL.query` REPL without installing Clojure or source code, run:

```shell
nix run github:OpenGen/GenSQL.query -- --help  # pass parameters after a double dash
nix run github:OpenGen/GenSQL.query            # to get an interactive REPL
```

The first time you invoke this, Nix will build some dependencies, but these will cache for future runs.

To run the code in this repository with your native environment, see (Development)[#Development].

The command-line application currently supports GenSQL-strict and GenSQL-permissive queries, with strict as the default.

#### SPPL

If you would like to use `GenSQL.query` to query SPPL models you will need to ensure that SPPL is on the classpath, and that [libpython-clj](https://github.com/clj-python/libpython-clj) can find a Python where SPPL is installed. The easiest way to accomplish this is to use [Nix](https://nixos.org/):

```shell
nix develop github:OpenGen/GenSQL.gpm.sppl -c clj -Sdeps '{:deps {io.github.OpenGen/GenSQL.gpm.sppl {:git/sha "718de40878766bb8d08acc2b429a76ed662a1352"}}}' -M -m gensql.query.main --help
```

### Clojure interface

`gensql.query.strict` and `gensql.query.permissive` each expose a function, `q`, which can be used to evaluate queries. `q` accepts two positional arguments:

1. a query to be evaluate, a string
2. a database

Databases can be constructed using the functions in `gensql.query.db`.

### JavaScript interface

The library can be built into a stand-alone `.js` bundle with the following command:

```shell
clojure -M:js-build
```

After loading the JavaScript bundle a query can be issued by calling the function `gensql.query.js.query`:

```javascript
gensql.query.js.query("SELECT *", [{x: 0}, {x: 1}, {x: 2}], {model: ...})
```

The JavaScript interface currently only supports GenSQL-strict queries.

### Development

#### Clojurescript / Shadow-cljs

To develop with a cljs environment, use shadow-cljs. The simplest way is to use a bare Node-based REPL. There are two primary methods:

##### Terminal
From the command line, run:

```shell
npx shadow-cljs node-repl # start a CLI node repl
```

##### Editor nREPL
First, from the command line, run:

```shell
npx shadow-cljs server # start the shadow-cljs server
```

Then, connect to the nREPL server in your editor with the standard .nrepl-port file. (This process will be editor-specific.)

This will get you a **Clojure**-based REPL in your editor. To convert it to **Clojurescript**, in the REPL, run:

```clojure
(shadow/node-repl) ; starts a bare node repl
```

To exit Clojurescript mode, enter `:cljs/quit`.

#### Running with native Clojure

To run the local source code without pinned dependency versions:

```shell
clj -M -m gensql.query.main --help
```

#### Testing

Make sure [babashka](https://github.com/babashka/babashka) is installed. Then
run the tests via `bb test`. Dialect-specific tests can be run with
`bb test:clj` and `bb test:cljs`.

#### Dependency upgrades

We use `clj-nix` to support tamper-proof reproducible builds of the dependencies for all environments.
When you upgrade a package in `deps.edn`, it is necessary to update `deps-lock.json` as well, so that
the nix build universe has knowledge of the hash fingerprints of the new deps version tarballs.
It can be done without any setup like so:

```shell
nix develop '.#depsLock' --command bash -c "nix run .#deps-lock"
```

This script can take a minute or two as it needs to build local dependencies of the `clj-nix` library,
though this should only need to happen the first time you run it.
You will see the changes to `deps.edn` reflected in `deps-lock.json`; you should commit these; and the
release build will work again.

#### Style

##### Git

This repo follows the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary) 
specification for commit messages. We use the defaults, except our subject lines 
are in sentence case (e.g., "This is in sentence case") and do not end in a 
stop/period.

To check your most recent commit message, run `npx commitlint --from HEAD~1 
--to HEAD --verbose`. If you need to change the last message, run `git commit 
--amend`.

If you forget, a CI job will check it for you when you make a pull request.

#### Building a jar (portable Java application)

```shell
nix build '.#uber' -o gensql.jar
```

[codecov-url]: https://codecov.io/github/OpenGen/GenSQL.query
[codecov]: https://img.shields.io/codecov/c/github/OpenGen/GenSQL.query/main.svg?maxAge=3600
