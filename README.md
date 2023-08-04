# inferenceql.query
![tests](https://github.com/OpenIQL/inferenceql.query/workflows/tests/badge.svg)
![linter](https://github.com/OpenIQL/inferenceql.query/workflows/linter/badge.svg)

## Usage

### Command-line interface

`inferenceql.query` provides a simple command-line application that allows the user to manually enter and evaluate InferenceQL queries. Usage information can be printed with the following command.

``` bash
% clj -M -m inferenceql.query.main --help
```

The command-line application currently supports IQL-strict and IQL-permissive queries, with strict the default.

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
