{ stdenv,
  pkgs,
  mk-deps-cache,
}: let
  depsCache = mk-deps-cache {
    lockfile = builtins.path {
      path = ./../../deps-lock.json;
      name = "inferenceql.query.deps-lock.json";
    };
  };
in stdenv.mkDerivation {
  name = "inferenceql.query-uberjar";
  version = "unstable";
  src = builtins.path {
    path = ./../..;
    name = "inferenceql.query";
  };

  env = {
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ;
    CLJ_CONFIG = "${depsCache}/.clojure" ;
    GITLIBS = "${depsCache}/.gitlibs" ;
    JAVA_TOOL_OPTIONS = "-Duser.home=${depsCache}" ;
  };

  nativeBuildInputs = with pkgs; [ clojure git ];
  buildPhase = ''
    cp -R $src .
    clojure -T:build uber
  '';
  installPhase = ''
    cp -R target/*.jar $out
  '';
}
