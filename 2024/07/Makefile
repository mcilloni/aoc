CC ?= clang
CFLAGS = -Wno-implicit-function-declaration -Wno-incompatible-pointer-types -w -O3

ifndef FORKPATH
	$(error FORKPATH is not set)
endif

TRNS = $(FORKPATH)/transmod

LDFLAGS = -L$(FORKPATH) -lfork ${FORKPATH}/rt.o

.PHONY: all clean
all: 07

07: main.o
	$(CC) $(LDFLAGS) -o $@ $^
	strip $@

main.o: main.c
	$(CC) $(CFLAGS) -c -o $@ $<

main.c: main/main.fork
	FORDPATHS=$(FORKPATH)/ford $(TRNS) main

clean:
	rm -f 07 main.c main.s main.o main.ford
