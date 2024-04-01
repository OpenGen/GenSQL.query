{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    systems.url = "github:nix-systems/default";
    clj-nix.url = "github:jlesquembre/clj-nix";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
  };

  outputs = inputs@{ flake-parts, systems , ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;

      perSystem = { system, pkgs, ... }: let

        pkgsWithCljNixOverlay = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.clj-nix.overlays.default
          ];
        };

        depsCache = pkgsWithCljNixOverlay.callPackage ./nix/depsCache {};
        uber = pkgs.callPackage ./nix/uber {inherit depsCache;};

        pname = "iql";
        bin = pkgs.callPackage ./nix/bin { inherit uber pname; };

        basicToolsFn = pkgs: with pkgs; [
          coreutils
          curl
          file
          gawk
          git
          gnugrep
          gnused
          which
        ];

        # This invocation is a bit odd on the eyes:
        # callPackage is available only on the system-namespaced
        #   packages (derived from inputs.nixpkgs automatically by
        #   flake-parts), but ...
        ociImg = pkgs.callPackage ./nix/oci {
          inherit uber pname basicToolsFn depsCache;
          # ... we still must pass in the original nixpkgs because
          # we need access to a different system's set of packages
          # when compiling for linux while remaining agnostic of
          # the workstation platform we are running this on.
          nixpkgs = inputs.nixpkgs;
        };

      in {
        # development shell
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.openjdk21 pkgs.clojure pkgs.babashka depsCache ] ++ (basicToolsFn pkgs);

          shellHook = ''
            echo "Setting up default dev shell..."
            export CLJ_CONFIG="${depsCache}/.clojure"
            export GITLIBS="${depsCache}/.gitlibs"
            export JAVA_TOOL_OPTIONS="-Duser.home=${depsCache}"
          '';
        };

        # a minimal shell for doing a depsLock, that doesn't require an existing deps cache
        devShells.depsLock = pkgs.mkShell {
          buildInputs = [ pkgs.openjdk21 pkgs.clojure pkgs.babashka ] ++ (basicToolsFn pkgs);

          shellHook = ''
            echo "Setting up minimal dev shell..."
          '';
        };

        packages = {
          inherit uber bin ociImg;
          default = bin;
        };
      };
    };
}
