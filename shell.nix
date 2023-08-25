{ pkgs ? import <nixpkgs> {} }:

let
  hask = pkgs.haskell.packages.ghc927;
in
pkgs.mkShell {
  buildInputs = [
    hask.ghc
    hask.cabal-install
    pkgs.git
  ];
}
