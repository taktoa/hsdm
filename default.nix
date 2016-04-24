let
  pkgs = import <nixpkgs> {};
  callPackage = pkgs.newScope self;
  self = rec {
    hsdm = callPackage ./hsdm.nix {};
    hsdm_config = callPackage ./hsdm_config.nix {};
    guest = (import <nixpkgs/nixos> {
      configuration = { ... }:
      {
        imports = [ <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix> ];
        environment.systemPackages = with pkgs; [ hsdm_config xorg.xwininfo xorg.xauth xorg.xprop ];
        users.users = {
          root.initialPassword = "root";
          test = {
            initialPassword = "test";
            isNormalUser = true;
          };
        };
      };
    }).config.system.build.vm;
  };
in self
