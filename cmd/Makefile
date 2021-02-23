cmd = sensel-ll

include ~/sw/hsc3/mk/cmd.mk

CC = gcc

CPPFLAGS += -Wall -pedantic -O3 -g
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
LDLIBS = c-common/lib-c-common.a -ljack -lpthread -lm

sensel-ll: sensel-ll.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o sensel-ll sensel-ll.c $(LDLIBS) -lsensel

ln-local-c-common:
	rm -f c-common
	ln -s $(HOME)/sw/c-common c-common

mk-local-c-common:
	(cd c-common ; make)

# sensel-ll-np is linked against the sensel-api library built without the non-free pressure library.
# this library can be compiled on arm64, where sensel don't provide a pre-built library.
# the distinct naming here is for testing on systems that have the standard -lsensel installed.
# building libsenselnp.so requires editing sensel-api/sensel-lib/Makefile.
sensel-np: sensel.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o sensel-ll-np sensel-ll.c $(LDLIBS) -lsenselnp
