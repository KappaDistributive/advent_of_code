AS := as
ASFLAGS := -march=armv8-a
CC := gcc
INCLUDES := "build/text.o"

SOURCE_PATH := src
TESTS_PATH := tests
BIN_PATH := build
DATA_PATH := ../data

text.o:
	mkdir -p $(BIN_PATH)
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/text.o $(SOURCE_PATH)/utils/text.s

tests: text.o
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/tests.o $(TESTS_PATH)/text.s
	$(CC) -o $(BIN_PATH)/tests $(BIN_PATH)/tests.o $(INCLUDES)
	# execute
	$(BIN_PATH)/tests

2%: text.o
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/$@.o $(SOURCE_PATH)/$(word 1,$(subst _, ,$@))/day_$(word 2,$(subst _, ,$@)).s
	$(CC) -o $(BIN_PATH)/$@ $(BIN_PATH)/$@.o $(INCLUDES)
	$(BIN_PATH)/$@ "`cat $(DATA_PATH)/$(word 1,$(subst _, ,$@))/input_$(word 2,$(subst _, ,$@)).txt`"

