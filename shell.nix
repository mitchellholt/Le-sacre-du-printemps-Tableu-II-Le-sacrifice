{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.git
  ];

  shellHook = ''
    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"
    export GHC_COLORS="always"
  '';
}