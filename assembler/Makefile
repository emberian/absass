SOURCES ?= hello.asm blink.asm test.asm
SIZES ?= 64 16
LUA ?= lua5.3
MOONC ?= moonc

RESULTS = $(foreach size,$(SIZES),$(patsubst %.asm,%$(size).bin,$(SOURCES)))

all: $(RESULTS)
.PHONY: all

clean:
	rm *.bin
.PHONY: clean

%.lua: %.moon
	moonc -o $@ $^

define newline


endef

define rule
%$(1).bin: %.asm | assembler.lua
	cat <(echo ".WS $(1)") $$^ | $(LUA) assembler.lua $$@


endef

$(foreach size,$(SIZES),$(eval $(call rule,$(size))))
