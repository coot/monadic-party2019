{ pkgs ? import <nixpkgs> {} }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
