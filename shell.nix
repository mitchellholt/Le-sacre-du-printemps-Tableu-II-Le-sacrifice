{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc927
    haskellPackages.cabal-install
    pkgs.git
  ];
}
