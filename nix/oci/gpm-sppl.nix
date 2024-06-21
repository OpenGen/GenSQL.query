{ pkgs,
  nixpkgs,
  system,
  opengen,
  uber,
  pname,
  basicToolsFn,
  depsCache,
}: let
    # in OCI context, whatever our host platform we want to build same arch but linux
    systemWithLinux = builtins.replaceStrings [ "darwin" ] [ "linux" ] system;

    crossPkgsLinux = nixpkgs.legacyPackages.${systemWithLinux};

    # TODO: This can be factored out into an gensql/nix
    # shared package.
    baseImg = opengen.packages.${system}.ociImgBase;

    sppl =  opengen.packages.${systemWithLinux}.sppl;
    python =  opengen.packages.${systemWithLinux}.sppl.runtimePython;

    ociBin = crossPkgsLinux.callPackage ./../bin/gpm-sppl.nix {
      inherit
        uber
        opengen
        pname
      ;
    };
in pkgs.dockerTools.buildLayeredImage {
  name = "probcomp/gensql.query";
  tag = systemWithLinux;
  fromImage = baseImg;
  # architecture
  contents = [ ociBin depsCache crossPkgsLinux.clojure sppl python];
  config = {
    Cmd = [ "${ociBin}/bin/${pname}" ];
    Env = [
      "CLJ_CONFIG=${depsCache}/.clojure"
      "GITLIBS=${depsCache}/.gitlibs"
      "JAVA_TOOL_OPTIONS=-Duser.home=${depsCache}"
      "JDK_JAVA_OPTIONS=--add-modules jdk.incubator.foreign,jdk.incubator.vector --enable-native-access=ALL-UNNAMED"
      "PYTHONPATH=${python.sitePackages}"
    ];
  };
}



