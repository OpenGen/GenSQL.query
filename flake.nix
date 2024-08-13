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

        pname = "gensql";
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
          buildInputs = [ pkgs.openjdk21 pkgs.clojure pkgs.babashka pkgs.nodejs_20 depsCache ] ++ (basicToolsFn pkgs);

          shellHook = ''
            echo "Setting up default dev shell..."

            DEPS_CACHE_TMP="/tmp/clojure_cache_gensql_query"
            echo "Using read-write clojure cache at:"
            echo "$DEPS_CACHE_TMP"

            cache_signature_file="/tmp/clojure_cache_gensql_query_signature"
            cache_signature="$(echo ${depsCache} | tr -d '\n')"
            last_cache_signature="$(cat $cache_signature_file | tr -d '\n')"

            if [[ $cache_signature != $last_cache_signature ]]; then
              echo -n $cache_signature > $cache_signature_file

              mkdir -p $DEPS_CACHE_TMP
              echo "Copying readonly clojure cache to read-write location"
              rsync -LrltgoD --chmod=ug+w ${depsCache}/ $DEPS_CACHE_TMP/
            fi

            export CLJ_CONFIG="$DEPS_CACHE_TMP/.clojure"
            export GITLIBS="$DEPS_CACHE_TMP/.gitlibs"
            export JAVA_TOOL_OPTIONS="-Duser.home=$DEPS_CACHE_TMP"
          '';
        };

        # development shell with readonly dependencies
        devShells.strict = pkgs.mkShell {
          buildInputs = [ pkgs.openjdk21 pkgs.clojure pkgs.babashka depsCache ] ++ (basicToolsFn pkgs);

          shellHook = ''
            echo "Setting up strict dev shell with readonly clojure deps..."
            export CLJ_CONFIG="${depsCache}/.clojure"
            export GITLIBS="${depsCache}/.gitlibs"
            export JAVA_TOOL_OPTIONS="-Duser.home=${depsCache}"
          '';
        };

        # a minimal shell for doing a depsLock, that doesn't require an existing deps cache
        devShells.depsLock = pkgs.mkShell {
          buildInputs = [
            pkgs.openjdk21
            pkgs.clojure
            pkgs.babashka
            pkgsWithCljNixOverlay.deps-lock
          ] ++ (basicToolsFn pkgs);

          shellHook = ''
            echo "Setting up minimal dev shell..."
            unset CLJ_CONFIG
            unset GITLIBS
            unset JAVA_TOOL_OPTIONS
          '';
        };

        packages = {
          inherit uber bin ociImg;
          default = bin;
        };
      };
    };
}
