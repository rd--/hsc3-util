GL_GIT=git@gitlab.com:rd--/hsc3-util.git
GL_HTTP=https://gitlab.com/rd--/hsc3-util.git

all:
	(cd cmd ; make all)

clean:
	(cd cmd ; make clean)

install:
	(cd cmd ; make install)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3-util; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd
