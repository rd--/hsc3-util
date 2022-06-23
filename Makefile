all:
	(cd cmd ; make all)

clean:
	rm -fR dist dist-newstyle
	(cd cmd ; make clean)

install:
	(cd cmd ; make install)

push-all:
	r.gitlab-push.sh hsc3-util
	r.github-push.sh hsc3-util
