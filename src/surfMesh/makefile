#!/usr/bin/make

### auxiliary variables --------------------------------------------------------

COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

DIR_SUB_SRC:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

### Compiling Rules ------------------------------------------------------------

createObj: $(DIR_OBJ)modSurfMesh.o

$(DIR_OBJ)modSurfMesh.o: $(DIR_SUB_SRC)modSurfMesh.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@
