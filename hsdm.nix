{ stdenv, haskellPackages }:

with stdenv.lib;
let
  hsEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [ X11 pam ]);
  pathPartMatches = name: path: any (x: x == name) (splitString "/" path);
in stdenv.mkDerivation {
  name = "hsdm";
  src = builtins.filterSource (path: type: !(pathPartMatches ".git" path || type == "symlink" || hasSuffix ".nix" path || hasSuffix ".swp" path)) ./.;

  buildInputs = [ hsEnv ];
}
