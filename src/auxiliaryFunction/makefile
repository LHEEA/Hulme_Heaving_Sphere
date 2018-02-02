#!/usr/bin/make

### auxiliary variables --------------------------------------------------------

COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

DIR_SUB_SRC:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

### Compiling Rules ------------------------------------------------------------

createObj: $(DIR_OBJ)modAuxiliaryFunction.o

$(DIR_OBJ)modAuxiliaryFunction.o: $(DIR_SUB_SRC)modAuxiliaryFunction.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@
