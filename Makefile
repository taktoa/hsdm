.PHONY: all clean

all: hsdm.nix

repl: hsdm.nix
	nix-shell -A hsdm --run "cd haskell; ./run-repl"

clean:
	-rm -f hsdm.nix &>/dev/null || true

hsdm.nix: haskell/hsdm.cabal
	nix-shell -p cabal2nix --run "cabal2nix ./haskell > hsdm.nix"
