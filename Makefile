.PHONY: all clean

all: hsdm.nix

clean:
	-rm -f hsdm.nix &>/dev/null || true

hsdm.nix: haskell/hsdm.cabal
	nix-shell -p cabal2nix --run "cabal2nix ./haskell > hsdm.nix"
