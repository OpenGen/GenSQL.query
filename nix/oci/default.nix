{ pkgs,
  nixpkgs,
  system,
  uber,
  pname,
  basicToolsFn,
}: let
    # in OCI context, whatever our host platform we want to build same arch but linux
    systemWithLinux = builtins.replaceStrings [ "darwin" ] [ "linux" ] system;

    crossPkgsLinux = nixpkgs.legacyPackages.${systemWithLinux};

    # TODO: This can be factored out into an inferenceql/nix
    # shared package.
    baseImg = pkgs.dockerTools.buildLayeredImage {
      name = "inferenceql.base";
      contents =
        (basicToolsFn crossPkgsLinux) ++ (with crossPkgsLinux; [
          bashInteractive
          busybox # NOTE: might be unnecessary
      ]);
      config = {
        Cmd = [ "${crossPkgsLinux.bashInteractive}/bin/bash" ];
      };
    };

    ociBin =  crossPkgsLinux.callPackage ./../bin {inherit uber pname;};
in pkgs.dockerTools.buildImage {
  name = "probcomp/inferenceql.query";
  tag = systemWithLinux;
  fromImage = baseImg;
  # architecture
  copyToRoot = [ ociBin ];
  config = {
    Cmd = [ "${ociBin}/bin/${pname}" ];
  };
}



