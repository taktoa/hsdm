hsdm: hsdm.hs
	ghc hsdm.hs

install: hsdm
	mkdir -pv ${out}/bin/
	cp -vi hsdm ${out}/bin/
