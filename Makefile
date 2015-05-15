clean:
	(cd hs; make clean)
	cabal clean

push-sp:
	darcs push -a rd@slavepianos.org:sw/hsc3-utils

build-setup:
	(cd hs; make build-setup)
