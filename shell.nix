{ nixpkgsFn ? import ./nixpkgs.nix, compiler ? "ghc801" }:
(import ./default.nix { inherit nixpkgsFn compiler; }).env
