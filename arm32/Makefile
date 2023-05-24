AS = as
LD = ld
CC = gcc

SOURCES = $(wildcard src/*.s)
OBJECTS = $(patsubst src/%.s, build/%.o, $(SOURCES))

2015_01:
	$(AS) -o build/2015_01.o src/2015/day_01.s src/2015/input_01.s
	$(CC) -o build/2015_01 build/2015_01.o

2015_02:
	$(AS) -o build/2015_02.o src/2015/day_02.s src/2015/input_02.s
	$(CC) -o build/2015_02 build/2015_02.o

