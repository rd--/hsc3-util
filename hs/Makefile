prefix = ~/opt

% : %.hs
	ghc -Wall -fwarn-tabs --make $<

all: hash-paren

install:
	cp hash-paren $(prefix)/bin

clean:
	rm -f *.hi *.o
	rm -f hash-paren
