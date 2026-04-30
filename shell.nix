{ pkgs ? import <nixpkgs> {} }:

let
  eldev = pkgs.stdenv.mkDerivation {
    pname = "eldev";
    version = "1.10";

    src = pkgs.fetchFromGitHub {
      owner = "doublep";
      repo = "eldev";
      rev = "1.10";
      sha256 = "sha256-9x9KaeMCf3Zyf/fTq/2HwMCM1uDrnvQsWUUcXq3R0ws=";
    };

    buildInputs = [ pkgs.emacs ];

    installPhase = ''
      mkdir -p $out/bin
      cp bin/eldev $out/bin/
      chmod +x $out/bin/eldev
    '';
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.emacs
    eldev
  ];

  shellHook = ''
    echo "ox-zola development shell"
    echo "Run 'eldev test' to run tests"
  '';
}
