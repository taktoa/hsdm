{ mkDerivation, aeson, array, async, base, bytestring, c2hs, cairo
, colour, containers, data-default, diagrams-lib
, diagrams-rasterific, exceptions, FontyFruity, frpnow, glib, gtk3
, JuicyPixels, lens, linear, mtl, pam, pipes, pipes-concurrency
, process, random, rasterific-svg, semigroups, split, stdenv
, svg-tree, text, transformers, unix, varying, X11
}:
mkDerivation {
  pname = "hsdm";
  version = "0.0.1";
  src = ./haskell;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring cairo colour containers
    data-default diagrams-lib diagrams-rasterific exceptions
    FontyFruity frpnow glib gtk3 JuicyPixels lens linear mtl pipes
    pipes-concurrency process random rasterific-svg semigroups split
    svg-tree text transformers unix varying X11
  ];
  librarySystemDepends = [ pam ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/taktoa/hsdm";
  description = "A display manager, written in Haskell";
  license = stdenv.lib.licenses.mit;
}
