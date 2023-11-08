cmd = sensel-ll
ext = scsynth

include ~/sw/hsc3-meta/mk/cmd.mk

CC = gcc # gcc ; clang

CPPFLAGS += -Wall -pedantic -O3 -g
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
LDLIBS = r-common/c/lib-r-common.a -lpthread -lm

sensel-ll: sensel-ll.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o sensel-ll sensel-ll.c $(LDLIBS) -lsensel

mk-r-common:
	git submodule update
	(cd r-common/c ; make)

# np is the sensel-api library built without the non-free pressure library (np = no-pressure)
# sensel-ll-np is linked dynamically and sensel-ll-np-static is linked statically.
# this library can be compiled on arm64, where sensel don't provide a pre-built library.
# the distinct naming here is for testing on systems that have the standard -lsensel installed.
# building libsenselnp.so requires editing sensel-api/sensel-lib/Makefile.

SENSEL_DIR = $(HOME)/opt/src/sensel/sensel-api/sensel-lib/build/release/nopressure/obj
SENSEL_OBJ = $(SENSEL_DIR)/sensel.o $(SENSEL_DIR)/sensel_register.o $(SENSEL_DIR)/sensel_serial_linux.o

sensel-ll-np-static: sensel-ll.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o sensel-ll-np-static sensel-ll.c $(LDLIBS) $(SENSEL_OBJ)

sensel-ll-np: sensel-ll.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o sensel-ll-np sensel-ll.c $(LDLIBS) -lsenselnp

clean-all:
	rm -f sensel-ll sensel-ll-np-static sensel-ll-np
	(cd r-common/c ; make clean)
