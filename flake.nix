{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  };

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.aarch64-darwin;

    jdk = pkgs.openjdk17;
    clojure = pkgs.clojure.override { jdk = jdk; };

    build-clojure = pkgs.runCommand "build-clojure" {
      __noChroot = true;
      src = ./deps.edn;
      nativeBuildInputs = [ clojure pkgs.git pkgs.makeWrapper ];
    } ''
      mkdir -p $out

      makeWrapper ${pkgs.clojure}/bin/clojure ./build-clojure \
        --set GIT_SSL_CAINFO ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt \
        --set CLJ_CONFIG $out/.clojure \
        --set GITLIBS $out/.gitlibs \
        --set JAVA_TOOL_OPTIONS "-Duser.home=$out"

      cp $src ./deps.edn

      ./build-clojure -A:build -P
      ./build-clojure -P

      mkdir -p $out/bin
      cp ./build-clojure $out/bin/build-clojure
    '';
  in {
    devShells.aarch64-darwin.default = pkgs.mkShell {
      buildInputs = [ jdk clojure ];
    };

    packages.aarch64-darwin = rec {
      uber = pkgs.stdenv.mkDerivation {
        name = "inferenceql.query uberjar";
        src = ./.;
        nativeBuildInputs = [ build-clojure pkgs.git ];
        buildPhase = ''
          cp -R $src .
          build-clojure -T:build uber
        '';
        installPhase = ''
          cp -R target/*.jar $out
        '';
      };

      default = pkgs.stdenv.mkDerivation rec {
        name = "inferenceql.query";
        pname = "iql";
        src = ./.;
        nativeBuildInputs = with pkgs; [ makeWrapper ];
        buildInputs = [ jdk ];
        installPhase = ''
          makeWrapper ${jdk}/bin/java $out/bin/${pname} \
            --add-flags "-jar ${uber}"
        '';
      };
    };
  };
}
