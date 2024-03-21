# inferenceql.query
![tests](https://github.com/OpenIQL/inferenceql.query/workflows/tests/badge.svg)
![linter](https://github.com/OpenIQL/inferenceql.query/workflows/linter/badge.svg)

## Usage

### Dependencies and Nix

Many of the usages described below are powered by the Nix package manager, which works on most Unix systems. We recommend installing Nix with the
[Determinate Systems installer](https://determinate.systems/posts/determinate-nix-installer/). If you already have Nix installed, note that the
precise minimum version of Nix required is not known, but we have tested with Nix 2.19.3:

```
$ nix --version
nix (Nix) 2.19.3
```

Using Nix ensures you can get reliable version sets of all dependencies even when the project uses multiple languages and tools.

### Command-line interface

`inferenceql.query` provides a simple command-line application that allows the user to manually enter and evaluate InferenceQL queries. Usage information can be printed with the following command.

To run the latest version of `inferenceql.query` REPL without installing clojure or source code, run:

```
nix run github:inferenceql/inferenceql.query -- --help  # pass parameters after a double dash
nix run github:inferenceql/inferenceql.query            # to get an interactive REPL
```

The first time you invoke this, Nix will build some dependencies, but these will cache for future runs.

To run the code in this repository with your native environment, see (Development)[#Development].

The command-line application currently supports IQL-strict and IQL-permissive queries, with strict as the default.

#### SPPL

If you would like to use `inferenceql.query` to query SPPL models you will need to ensure that SPPL is on the classpath, and that [libpython-clj](https://github.com/clj-python/libpython-clj) can find a Python where SPPL is installed. The easiest way to accomplish this is to use [Nix](https://nixos.org/):

``` shell
nix develop github:inferenceql/inferenceql.gpm.sppl -c clj -Sdeps '{:deps {io.github.inferenceql/inferenceql.gpm.sppl {:git/sha "52f8316e094b3644709dccde8f0a935f9b55f187"}}}' -M -m inferenceql.query.main --help
```

### Clojure interface

`inferenceql.query.strict` and `inferenceql.query.permissive` each expose a function, `q`, which can be used to evaluate queries. `q` accepts two positional arguments:

1. a query to be evaluate, a string
2. a database

Databases can be constructed using the functions in `inferenceql.query.db`.

### JavaScript interface

The library can be built into a stand-alone `.js` bundle with the following command:

``` shell
clojure -M:js-build
```

After loading the JavaScript bundle a query can be issued by calling the function `inferenceql.query.js.query`:

``` javascript
inferenceql.query.js.query("SELECT *", [{x: 0}, {x: 1}, {x: 2}], {model: ...})
```

The JavaScript interface currently only supports IQL-strict queries.

### Development

#### Running with native clojure

To run the local source code without pinned dependency versions:

``` bash
clj -M -m inferenceql.query.main --help
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

```
nix develop --command bash -c "nix run github:jlesquembre/clj-nix#deps-lock"
```

This script can take a minute or two as it needs to build local dependencies of the `clj-nix` library,
though this should only need to happen the first time you run it.
You will see the changes to `deps.edn` reflected in `deps-lock.json`; you should commit these; and the
release build will work again.

#### Building a JAR (portable java application)

```
nix build '.#uber' -o iql.jar
```
