{ writeText, writeScript, hsdm, tightvnc }:

let
  config = writeText "hsdm.conf" (builtins.toJSON {
    default_xserver = "${tightvnc}/bin/Xvnc";
    xserver_arguments = "-nolisten tcp :1";
    sessiondir = "/nix/store/55y4angy2kl7lfk026yhw2ccfislb1dm-desktops";
    login_cmd = "exec /usr/bin/env bash /nix/store/cbsaaam06nj3v42zkw3gfk4zfz3p3vxy-xsession \"%session\"";
  });
in
writeScript
  "hsdm_config"
  ''
    #!/bin/sh
    ${hsdm}/bin/hsdm -c ${config}
  ''
