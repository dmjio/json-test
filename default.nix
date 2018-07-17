{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskell.lib;
  doBenchmark (dontCheck (pkgs.haskellPackages.callPackage ./json-test.nix {}))
