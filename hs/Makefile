prefix = ~/opt

% : %.hs
	ghc -Wall -fwarn-tabs --make $<

all: hash-paren

install:
	cp hash-paren $(prefix)/bin/hsc3-hash-paren

clean:
	rm -f *.hi *.o
	rm -f hash-paren
