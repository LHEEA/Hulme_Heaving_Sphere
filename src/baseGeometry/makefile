#!/usr/bin/make

### auxiliary variables --------------------------------------------------------

COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

DIR_SUB_SRC:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

### Compiling Rules ------------------------------------------------------------

createObj: $(DIR_OBJ)modBaseGeometry.o

$(DIR_OBJ)modBaseGeometry.o: $(DIR_SUB_SRC)modBaseGeometry.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@
