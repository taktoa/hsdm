let
  pkgs = import <nixpkgs> {};
  callPackage = pkgs.newScope self;
  self = rec {
    hsdm = callPackage ./hsdm.nix {};
    hsdm_config = callPackage ./hsdm_config.nix {};
  };
in self
