AS := as
ASFLAGS := -march=armv8-a
CC := gcc

SOURCE_PATH := src
BIN_PATH := build
DATA_PATH := ../data

%:
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/$@.o $(SOURCE_PATH)/$(word 1,$(subst _, ,$@))/day_$(word 2,$(subst _, ,$@)).s
	$(CC) -o $(BIN_PATH)/$@ $(BIN_PATH)/$@.o
	$(BIN_PATH)/$@ "`cat $(DATA_PATH)/$(word 1,$(subst _, ,$@))/input_$(word 2,$(subst _, ,$@)).txt`"


