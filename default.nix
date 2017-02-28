{ nixpkgsFn ? import ./nixpkgs.nix
, compiler ? "ghc801"
, system ? null }:
let haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
    callPackage = haskellPackages.callPackage;
    nixpkgs = nixpkgsFn ({
      # extra config goes here
    } // ( if system == null then {} else { inherit system; } ));
    packageOverrides = rec {};
in
callPackage ./Combinatron-Emulator.nix ({
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install ];
} // packageOverrides)
