all:
	(cd cmd ; make all)

install:
	cabal v1-install --allow-newer

clean:
	rm -fR dist dist-newstyle
	(cd cmd ; make clean)

mk-cmd:
	(cd cmd ; make install)

push-all:
	r.gitlab-push.sh hsc3-util
	r.github-push.sh hsc3-util

indent:
	fourmolu -i md

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns cmd/grid.hs
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns cmd/manta.hs
