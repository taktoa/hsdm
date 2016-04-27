let
  pkgs = import <nixpkgs> {};
  selfCP = pkgs.newScope self;
  self = rec {
    hsdm = pkgs.haskellPackages.callPackage ./hsdm.nix { inherit (pkgs) pam; };
    hsdm-shell = pkgs.haskell.lib.overrideCabal hsdm (old: {
                   librarySystemDepends = old.librarySystemDepends
                                          ++ [ pkgs.pkgconfig ];
                 });
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
