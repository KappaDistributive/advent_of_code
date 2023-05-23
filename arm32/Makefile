AS = as
LD = ld

SOURCES = $(wildcard src/*.s)
OBJECTS = $(patsubst src/%.s, build/%.o, $(SOURCES))

%: build/%.o
	$(LD) build/$@.o -o build/$@

build/%.o: src/%.s
	mkdir -p build
	$(AS) -o $@ $<

