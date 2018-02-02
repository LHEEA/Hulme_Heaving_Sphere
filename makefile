#!/usr/bin/make

### Path -----------------------------------------------------------------------

export PROJECT_DIR:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

export DIR_SRC=$(PROJECT_DIR)src/
export DIR_OBJ=$(PROJECT_DIR)obj/
export DIR_EXE=$(PROJECT_DIR)

export DIR_SRCTEST=$(DIR_SRC)testCode/

export EXES=$(PROJECT_DIR)main

### compiler (gfortran) --------------------------------------------------------

export FC=gfortran
#export FC=ifort

# C flags
export CFLAGS = -fPIC -O2 -g

# linking flags
export LDFLAGS = -shared

#Flag for writing modules in $(OBJ)
export FLAGMOD1= -J $(DIR_OBJ)
#Flag for reading modules in $(OBJ)
export FLAGMOD2= -I $(DIR_OBJ)
export OPTSC0  = -c $(FLAGMOD1)
export OPTSL0  = $(FLAGMOD2)
export MKDIRS  = $(DIR_OBJ)

export OPTSC = $(OPTSC0)
export OPTSL = $(OPTSL0)

export COMPILE_OBJECT_RULE=@$(FC) $(CFLAGS) $(OPTSC) -o $@

### Targets for compilation ----------------------------------------------------

Release: cleanall
Release: $(MKDIRS)
Release: compileObject
Release: main

### auxiliary variables --------------------------------------------------------
COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

main: $(DIR_OBJ)main.o
	@echo $(COTEXT)
	@$(FC) -o $@ $(DIR_OBJ)*.o
EXES := $(DIR_EXE)main

compileObject: compileObj

### compiling rules ------------------------------------------------------------

$(DIR_OBJ)main.o: $(DIR_SRC)main.f90 \
	$(DIR_OBJ)modGlobal.o
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@

compileObj: \
	$(DIR_OBJ)modGlobal.o
	$(MAKE) createObj -f $(DIR_SRC)auxiliaryFunction/makefile
	$(MAKE) createObj -f $(DIR_SRC)quadraturePack/makefile
	$(MAKE) createObj -f $(DIR_SRC)heavingHemiSphere/makefile
	$(MAKE) createObj -f $(DIR_SRC)baseGeometry/makefile
	$(MAKE) createObj -f $(DIR_SRC)surfMesh/makefile
	$(MAKE) createObj -f $(DIR_SRC)hSpherePostProcessing/makefile

$(DIR_OBJ)modGlobal.o: $(DIR_SRC)modGlobal.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@

# $(DIR_OBJ)modTestModule.o: $(DIR_SRC)testModule/modTestModule.f90
# 	@echo $(COTEXT)
# 	$(COMPILE_OBJECT_RULE) $< -o $@

### phony auxiliary rules ------------------------------------------------------
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DIR_OBJ) @rm *.so *.o
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DIR_OBJ)*.mod *.mod
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(EXES)
cleanresult:
	@echo deleting result files
	@rm -fr radiationForce.dat VTK
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe cleanmod cleanresult
