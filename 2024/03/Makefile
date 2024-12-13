ERLC ?= erlc

.PHONY: all clean
all: main.beam

main.beam: main.erl
	$(ERLC) $<

clean:
	rm -f main.beam
