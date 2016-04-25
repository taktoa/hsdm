{ writeText, writeScriptBin, hsdm, tightvnc, xorg }:

let
  config = writeText "hsdm.conf" (builtins.toJSON {
    #default_xserver = "${tightvnc}/bin/Xvnc";
    default_xserver = "${xorg.xorgserver}/bin/Xorg";
    xserver_arguments = [
      "-nolisten" "tcp"
      ":1"
      "-fp" "/nix/store/cidl7bc0npkpjq1rv0mvbc6bf1yhsk00-font-misc-misc-1.1.2/lib/X11/fonts/misc/,/nix/store/9jx0cdw81f3alg6lbqzgxkjxxlci1709-font-cursor-misc-1.0.3/lib/X11/fonts/misc/"
      "-logfile" "/var/log/X.1.log"
      "-xkbdir" "${xorg.xkeyboardconfig}/etc/X11/xkb"
      "-config" "${./xserver.conf}"
      #"-xkbmap" "basic"
      "-logverbose" "10"
      #"-allowMouseOpenFail"
      "-depth" "24" ];
    sessiondir = "/nix/store/55y4angy2kl7lfk026yhw2ccfislb1dm-desktops";
    #login_cmd = "exec /usr/bin/env bash /nix/store/cbsaaam06nj3v42zkw3gfk4zfz3p3vxy-xsession \"%session\"";
    #login_cmd = "xlsclients ; xfconf-query -c xsettings -lv; sleep 5; xfconf-query -c xsettings -lv;xterm";
    #login_cmd = "xterm";
    #login_cmd = "env;xfce4-session";
    #login_cmd = "env; exec strace -ff -o /tmp/logfiles bash /nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession xfce4";
    login_cmd = "env; exec bash /nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession xfce";
  });
in
writeScriptBin
  "hsdm_config"
  ''
    #!/bin/sh
    ${hsdm}/bin/hsdm -c ${config}
  ''
