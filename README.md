# inferenceql.query
![tests](https://github.com/OpenIQL/inferenceql.query/workflows/tests/badge.svg)
![linter](https://github.com/OpenIQL/inferenceql.query/workflows/linter/badge.svg)

## Usage

### Command-line interface

`inferenceql.query` provides a simple command-line application that allows the user to manually enter and evaluate InferenceQL queries. Usage information can be printed with the following command.

``` bash
clj -M -m inferenceql.query.main --help
```

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

#### Testing

Make sure babashka is installed. Then run the tests via `bb test`. Dialect-specific tests can be run with `bb test:clj` and `bb test:cljs`. 
