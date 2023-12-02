# compiler
CA := ca65
CAFLAGS := -t nes

# linker
MAPPERS := mappers
LD := ld65
LDFLAGS := -v --cfg-path $(MAPPERS)

# output
BUILDDIR := build

# projects
PROJECTS := template

CHILDREN := $(addsuffix /game.mk, $(PROJECTS))
CHILDREN := $(addprefix src/, $(CHILDREN))

.PHONY: all clean

all: $(PROJECTS)

clean: $(addsuffix -clean, $(PROJECTS))
	@echo "Done cleaning"

-include $(CHILDREN)