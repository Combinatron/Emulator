{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./Combinatron-Emulator.nix {
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install];
}
