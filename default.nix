let
  pkgs = import <nixpkgs> {};
  callPackage = pkgs.newScope self;
  self = rec {
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
        environment.systemPackages = with pkgs; [ file gdb stdenv chromium ];
        users.users = {
          root.initialPassword = "root";
          test = {
            initialPassword = "test";
            isNormalUser = true;
          };
        };
        services.openssh.enable = true;
        services.openssh.permitRootLogin = "yes";
        services.xserver = {
          enable = true;
          displayManager.hsdm.enable = true;
          desktopManager.xfce.enable = true;
          useGlamor = false;
          #serverLayoutSection = ''Option "NoTrapSignals"'';
        };
        nixpkgs.config.packageOverrides = pkgs: {
          hsdm = pkgs.callPackage ./hsdm.nix {};
          hsdm_config = pkgs.callPackage ./hsdm_config.nix {};
        };
      };
    }).config.system.build.vm;
  };
in self
