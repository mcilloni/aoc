COBC ?= cobc

#disable all warnings from the generated C
export COB_CFLAGS = -w 

.PHONY: all clean
all: 02.1 02.2

02.1: part1.cbl
	$(COBC) -o 02.1 --Wall --free -x part1.cbl -g
	rm -f part1.c part1.c.* part1.i

02.2: main.cbl
	$(COBC) -o 02.2 --Wall --free -x main.cbl -g
	rm -f main.c main.c.* main.i

clean:
	rm -vf 02.1 02.2 *.c *.c.* main main.i part1 part1.i
