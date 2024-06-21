{ stdenv,
  pkgs,
  depsCache,
  buildTool
}: stdenv.mkDerivation {
  name = "gensql.query-uberjar";
  version = "unstable";
  src = builtins.path {
    path = ./../..;
    name = "gensql.query";
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
    clojure -T:${buildTool} uber
  '';
  installPhase = ''
    cp -R target/*.jar $out
  '';
}
