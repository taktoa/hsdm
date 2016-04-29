{ stdenv, enableDebugging, writeText, writeScriptBin, hsdm, tightvnc, xorg, xkeyboard_config }:

let
  config = writeText "hsdm.conf" (builtins.toJSON rec {
    #default_xserver = "${tightvnc}/bin/Xvnc";
    default_xserver = "${xorg.xorgserver}/bin/Xnest";
    xserver_arguments = [
      "-display" ":0" display
      "-xkbdir" "${xorg.xkeyboardconfig}/etc/X11/xkb"
      "-geometry" "1024x768"
      "-fp" "${xorg.fontmiscmisc}/lib/X11/fonts/misc,${xorg.fontcursormisc}/lib/X11/fonts/misc"
      #"-nolisten" "tcp"
      "-depth" "24"
      "-auth" "/tmp/hsdm.auth" ];
    sessiondir = "/nix/store/55y4angy2kl7lfk026yhw2ccfislb1dm-desktops";
    #login_cmd = "xterm";
    #login_cmd = "env;xfce4-session";
    login_cmd = "exec bash /nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession xfce";
    display = ":1";
  });
in
writeScriptBin
  "hsdm-config"
  ''
    #!/bin/sh
    export XKB_BINDIR=${xorg.xkbcomp}/bin
    ${hsdm}/bin/hsdm -c ${config}
  ''
