AS := as
ASFLAGS := -march=armv8-a
CC := gcc
INCLUDES := "build/text.o"

SOURCE_PATH := src
BIN_PATH := build
DATA_PATH := ../data

%:
	# assemble
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/text.o $(SOURCE_PATH)/utils/text.s
	$(AS) $(ASFLAGS) -o $(BIN_PATH)/$@.o $(SOURCE_PATH)/$(word 1,$(subst _, ,$@))/day_$(word 2,$(subst _, ,$@)).s
	# link
	$(CC) -o $(BIN_PATH)/$@ $(BIN_PATH)/$@.o $(INCLUDES)
	# execute
	$(BIN_PATH)/$@ "`cat $(DATA_PATH)/$(word 1,$(subst _, ,$@))/input_$(word 2,$(subst _, ,$@)).txt`"
