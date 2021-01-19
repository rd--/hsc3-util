all:
	(cd cmd ; make all)

clean:
	(cd cmd ; make clean)

install:
	(cd cmd ; make install)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3-util

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3-util
