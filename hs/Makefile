prefix = ~/.cabal
cmd = hash-paren id-clear id-rewrite

% : %.hs
	ghc -Wall -fwarn-tabs --make $<

all: $(cmd)

install:
	cp hash-paren $(prefix)/bin/hsc3-hash-paren
	cp id-clear $(prefix)/bin/hsc3-id-clear
	cp id-rewrite $(prefix)/bin/hsc3-id-rewrite

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
