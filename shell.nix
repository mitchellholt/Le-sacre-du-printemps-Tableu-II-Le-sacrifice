{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc927
    pkgs.haskellPackages.cabal-install
    pkgs.git
  ];
}
