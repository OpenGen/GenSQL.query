{ stdenv,
  pkgs,
  mk-deps-cache,
}: mk-deps-cache {
    lockfile = builtins.path {
      path = ./../../deps-lock.json;
      name = "gensql.query.deps-lock.json";
    };
  }
