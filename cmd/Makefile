cmd = sensel

include ~/sw/hsc3/mk/cmd.mk

CPPFLAGS += -Wall -pedantic -O3 -g
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
LDLIBS = c-common/lib-c-common.a -ljack -lpthread -lm

sensel: sensel.c
	gcc $(CPPFLAGS) $(CFLAGS) -o sensel sensel.c $(LDLIBS) -lsensel

ln-local-c-common:
	rm -f c-common
	ln -s $(HOME)/sw/c-common c-common

mk-local-c-common:
	(cd c-common ; make)

# sensel-np is linked against the sensel-api library built without the non-free pressure library.
# this library can be compiled on arm64, where sensel don't provide a pre-built library.
# the distinct naming here is for testing on systems that have the standard -lsensel installed.
# building libsenselnp.so requires editing sensel-api/sensel-lib/Makefile.
sensel-np: sensel.c
	gcc $(CPPFLAGS) $(CFLAGS) -o sensel-np sensel.c $(LDLIBS) -lsenselnp
