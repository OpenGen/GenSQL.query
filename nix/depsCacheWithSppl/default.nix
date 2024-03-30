{ mk-deps-cache,
}: mk-deps-cache {
  lockfile = builtins.path {
    path = ./deps-lock.json;
    name = "inferenceql.query.deps-lock.json";
  };
}
