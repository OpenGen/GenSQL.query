{ pkgs,
  nixpkgs,
  inferenceql-gpm-sppl,
  system,
  uber,
  pname,
  basicToolsFn,
}: let
    # in OCI context, whatever our host platform we want to build same arch but linux
    systemWithLinux = builtins.replaceStrings [ "darwin" ] [ "linux" ] system;

    crossPkgsLinux = nixpkgs.legacyPackages.${systemWithLinux};


    nixpkgs-sppl = import (pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "35a74aa665a681b60d68e3d3400fcaa11690ee50";
      sha256 = "sha256-6qjgWliwYtFsEUl4/SjlUaUF9hSSfVoaRZzN6MCgslg=";
    }) {system=systemWithLinux;};

    sppl = inferenceql-gpm-sppl.packages.${systemWithLinux}.sppl;
    python = nixpkgs-sppl.python39.withPackages (p: [ p.callPackage sppl ]);

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
  name = "probcomp/inferenceql.query-sppl";
  tag = systemWithLinux;
  fromImage = baseImg;
  # architecture

  copyToRoot = [ ociBin python sppl ];

  config = {
    Cmd = [ "${ociBin}/bin/${pname}" ];
    Env = [
      "PYTHONPATH=${python}/${python.sitePackages}"
    ];
  };
}



