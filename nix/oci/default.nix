{ pkgs,
  nixpkgs,
  system,
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
    baseImg = pkgs.dockerTools.buildLayeredImage {
      name = "gensql.base";
      contents =
        (basicToolsFn crossPkgsLinux) ++ (with crossPkgsLinux; [
          bashInteractive
      ]);
      config = {
        Cmd = [ "${crossPkgsLinux.bashInteractive}/bin/bash" ];
      };
    };

    ociBin =  crossPkgsLinux.callPackage ./../bin {inherit uber pname;};
in pkgs.dockerTools.buildImage {
  name = "probcomp/gensql.query";
  tag = systemWithLinux;
  fromImage = baseImg;
  # architecture
  copyToRoot = [ ociBin depsCache crossPkgsLinux.clojure ];
  config = {
    Cmd = [ "${ociBin}/bin/${pname}" ];
    Env = [
      "CLJ_CONFIG=${depsCache}/.clojure"
      "GITLIBS=${depsCache}/.gitlibs"
      "JAVA_TOOL_OPTIONS=-Duser.home=${depsCache}"
    ];
  };
}



