{ config, lib, pkgs, ... }:

with lib;

let
  dmcfg = config.services.xserver.displayManager;
  cfg = config.services.xserver.displayManager.hsdm;
  configObj = {
    default_xserver = dmcfg.xserverBin;
    xserver_arguments = splitString " " dmcfg.xserverArgs;
    sessiondir = dmcfg.session.desktops;
    login_cmd = "exec ${pkgs.stdenv.shell} ${dmcfg.session.script} \"%session\"";
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
    services.xserver.displayManager.slim.enable = false;
    services.xserver.displayManager.job = {
      environment = {
      };
      execCmd = "exec ${pkgs.hsdm}/bin/hsdm -c ${configFile}";
    };
    security.pam.services.hsdm = { allowNullPassword = true; startSession = true; };
  };
}
