PAS ?= fpc

.PHONY: all clean
all: 09

09: main.pas
	$(PAS) -o$@ $< -g

clean:
	rm -f 09 *.o link*.res
