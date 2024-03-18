{ pkgs,
  uber,
  pname
}: pkgs.stdenv.mkDerivation {
  name = "inferenceql.query";
  inherit pname;
  src = ./.;
  nativeBuildInputs = [ pkgs.makeWrapper ];
  buildInputs = [ pkgs.openjdk17 ];
  installPhase = ''
    makeWrapper ${pkgs.openjdk17}/bin/java $out/bin/${pname} \
    --add-flags "-jar ${uber}"
  '';
}
