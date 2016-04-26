{ config, lib, pkgs, ... }:

with lib;

let
  dmcfg = config.services.xserver.displayManager;
  cfg = dmcfg.hsdm;
  configObj = {
    default_xserver = dmcfg.xserverBin;
    xserver_arguments = splitString " " dmcfg.xserverArgs;
    sessiondir = dmcfg.session.desktops;
    # FIXME: hsdm must pass the chosen DE to the script
    login_cmd = "exec ${pkgs.stdenv.shell} ${dmcfg.session.script} xfce";
  };
  configFile = pkgs.writeText "hsdm.conf" (builtins.toJSON configObj);
in
{
  options = {
    services.xserver.displayManager.hsdm = {
      enable = mkEnableOption "HSDM is a display-manager written in haskell";
    };
  };
  config = mkIf cfg.enable {
    # FIXME: this shouldn't really be necessary? really a NixOS bug though.
    services.xserver.displayManager.slim.enable = false;
    services.xserver.displayManager.job = {
      environment = {
      };
      execCmd = "exec ${pkgs.hsdm}/bin/hsdm -c ${configFile}";
    };
    security.pam.services.hsdm = {
      allowNullPassword = true;
      startSession = true;
    };
  };
}
