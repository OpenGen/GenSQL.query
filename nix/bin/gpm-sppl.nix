{ pkgs,
  system,
  uber,
  opengen,
  pname
}: pkgs.stdenv.mkDerivation {
  name = "gensql.query";
  inherit pname;
  src = ./.;
  nativeBuildInputs = [ pkgs.makeWrapper ];
  buildInputs = [ pkgs.openjdk17 ];
  propagatedBuildInputs = with opengen; [
    opengen.packages.${system}.sppl
    opengen.packages.${system}.sppl.runtimePython
  ];
  installPhase = ''
    makeWrapper ${pkgs.openjdk17}/bin/java $out/bin/${pname} \
    --add-flags "-jar ${uber}"
  '';
}
