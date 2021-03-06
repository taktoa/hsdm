let
  pkgs = import <nixpkgs> {};
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      FontyFruity = overrideCabal super.FontyFruity (old: {
        src = pkgs.fetchFromGitHub {
          owner  = "taktoa";
          repo   = "FontyFruity";
          rev    = "18bdf2ca36e724c128adc7fd178efc86a9fd9959";
          sha256 = "0940frxngr89rk8mqaa1nizrxcgm3y8d4rp880gwjn81hpm0zk80";
        };
        libraryHaskellDepends = old.libraryHaskellDepends ++ [ super.xml ];
      });
    };
  };
  selfCP = pkgs.newScope self;
  self = rec {
    hsdm = haskellPackages.callPackage ./hsdm.nix {
             inherit (pkgs) pam;
             inherit (haskellPackages) FontyFruity;
           };
    xorg2 = {
      inherit (pkgs.xorg) xwininfo xauth xprop xkeyboardconfig;
      xorgserver = pkgs.lib.overrideDerivation pkgs.xorg.xorgserver (old: {
        src = /home/clever/apps/xorg-server-1.17.4;
        patches = [];
        configureFlags = old.configureFlags ++ [
          "--with-xkb-path=${pkgs.xkeyboard_config}/etc/X11/xkb"
          "--with-xkb-bin-directory=${pkgs.xorg.xkbcomp}/bin"
          "--with-xkb-output=$(out)/share/X11/xkb/compiled"
        ];
      });
    };
    hsdm-config = selfCP ./hsdm-config.nix {};
    guest = (import <nixpkgs/nixos> {
      configuration = { lib, pkgs, ... }:
      with lib; {
        imports = [
           <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
          ./hsdm-module.nix
        ];
        # FIXME: for debug, delete later
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
          #serverLayoutSection = ''Option "NoTrapSignals"'';
        };
        nixpkgs.config.packageOverrides = pkgs: with pkgs; {
          hsdm        = callPackage ./hsdm.nix {};
          hsdm-config = callPackage ./hsdm-config.nix {};
        };
      };
    }).config.system.build.vm;
  };
in self
