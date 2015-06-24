prefix = ~/.cabal
cmd =	default-param \
	hash-paren id-clear id-rewrite psynth uparam sexp \
	scsyndef-stat scsyndef-to-dot scsyndef-to-fs scsyndef-to-hs \
	setup
cmd_i = $(addprefix hsc3-, $(cmd))

% : %.hs
	ghc -Wall -fwarn-tabs --make $<

hsc3-% : %
	cp $< hsc3-$<

all: $(cmd)

install: $(cmd_i)
	cp hsc3-* $(prefix)/bin

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
	rm -f $(cmd_i)

build-setup:
	make setup
	cp setup $(prefix)/bin/hsc3-setup
