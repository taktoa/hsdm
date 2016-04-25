let
  pkgs = import <nixpkgs> {};
  callPackage = pkgs.newScope self;
  self = rec {
    tightvnc = pkgs.tightvnc.overrideDerivation (oldAttrs: { src = /home/clever/x/vnc_unixsrc; });
    hsdm = callPackage ./hsdm.nix {};
    xorg2 = {
      inherit (pkgs.xorg) xwininfo xauth xprop xkeyboardconfig;
      xorgserver = pkgs.lib.overrideDerivation pkgs.xorg.xorgserver (oldAttrs: {
        src = /home/clever/apps/xorg-server-1.17.4;
        patches = [];
        configureFlags = oldAttrs.configureFlags ++ [
          "--with-xkb-path=${pkgs.xkeyboard_config}/etc/X11/xkb"
          "--with-xkb-bin-directory=${pkgs.xorg.xkbcomp}/bin"
          "--with-xkb-output=$(out)/share/X11/xkb/compiled"
        ];
      });
    };
    hsdm_config = callPackage ./hsdm_config.nix {};
    guest = (import <nixpkgs/nixos> {
      configuration = { lib, pkgs, ... }:
      with lib;
      {
        imports = [ <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix> ./hsdm_module.nix ];
        environment.systemPackages = with pkgs; [ hsdm_config xorg.xwininfo xorg.xauth xorg.xprop xterm ];
        users.users = {
          root.initialPassword = "root";
          test = {
            initialPassword = "test";
            isNormalUser = true;
          };
        };
        services.xserver = {
          enable = true;
          displayManager.hsdm.enable = true;
          #displayManager.xserverBin = mkForce "{pkgs.coreutils}/bin/env";
          useGlamor = false;
          deviceSection = ''Option "NoAccel" "true"'';
        };
        nixpkgs.config.packageOverrides = pkgs: {
          hsdm = pkgs.callPackage ./hsdm.nix {};
          hsdm_config = pkgs.callPackage ./hsdm_config.nix {};
        };
      };
    }).config.system.build.vm;
  };
in self
