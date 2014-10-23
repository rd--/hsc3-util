prefix = ~/.cabal
cmd = hash-paren id-clear id-rewrite psynth uparam sexp

% : %.hs
	ghc -Wall -fwarn-tabs --make $<

all: $(cmd)

install:
	cp hash-paren $(prefix)/bin/hsc3-hash-paren
	cp id-clear $(prefix)/bin/hsc3-id-clear
	cp id-rewrite $(prefix)/bin/hsc3-id-rewrite
	cp psynth $(prefix)/bin/hsc3-psynth
	cp uparam $(prefix)/bin/hsc3-uparam
	cp sexp $(prefix)/bin/hsc3-sexp

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
