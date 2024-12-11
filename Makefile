CC_AS ?= clang -target aarch64-pc-linux-gnu -fuse-ld=lld

.PHONY: all clean
all: 01

01: main.s
	$(CC_AS) -nostartfiles -nostdlib -static -o 01 main.s -g

clean:
	rm -f 01
