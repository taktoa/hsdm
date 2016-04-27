hsdm: hsdm.hs PAM.hs
	ghc hsdm.hs

pam_test: pam_test.hs PAM.hs
	ghc pam_test.hs -lpam

install: hsdm
	mkdir -pv ${out}/bin/
	cp -vi hsdm ${out}/bin/
