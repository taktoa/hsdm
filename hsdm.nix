{ mkDerivation, aeson, array, base, bytestring, c2hs, colour
, containers, data-default, diagrams-lib, diagrams-rasterific
, JuicyPixels, lens, mtl, pam, process, random, rasterific-svg
, semigroups, stdenv, unix, X11
}:
mkDerivation {
  pname = "hsdm";
  version = "0.0.1";
  src = ./haskell;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring colour containers data-default
    diagrams-lib diagrams-rasterific JuicyPixels lens mtl process
    random rasterific-svg semigroups unix X11
  ];
  librarySystemDepends = [ pam ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/taktoa/hsdm";
  description = "A display manager, written in Haskell";
  license = stdenv.lib.licenses.mit;
}
