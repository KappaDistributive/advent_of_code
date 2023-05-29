AS = as
ASFLAGS = -march=armv8-a
CC = gcc

2015_01:
	$(AS) $(ASFLAGS) -o build/2015_01.o src/2015/day_01.s
	$(CC) -o build/2015_01 build/2015_01.o
	./build/2015_01 "`cat ../data/2015/input_01.txt`"

2015_02:
	$(AS) $(ASFLAGS) -o build/2015_02.o src/2015/day_02.s
	$(CC) -o build/2015_02 build/2015_02.o
	./build/2015_02 "`cat ../data/2015/input_02.txt`"

