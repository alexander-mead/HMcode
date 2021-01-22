ifeq (${FC},)
FC = gfortran
endif
ifeq (${FC},f77)
FC = gfortran
endif

UNAME = $(shell uname)
ifeq (${UNAME}, Darwin)
  LIBTOOL = libtool -static -o
else
  LIBTOOL = ar src
endif

# Source-code directory
LIB_SRC_DIR = library/src

PYTHON_MOD = pyhmcode
F90WRAP_SRC_DIR = f90wrap_helpers


# Build directory
BUILD_DIR = build
F90WRAP_BUILD_DIR = f90wrap_pyhmcode

LIB_DIR = lib
INCLUDE_DIR = include

# Debug build directory
DEBUG_BUILD_DIR = debug_build

ifeq (,$(findstring gfortran,${FC}))
# Not gfortran
FFLAGS = -fpic -fpp -qopenmp -fp-model precise -W0 -WB -O3 -ipo -axCORE-AVX2 -fdefault-real-8 -fdefault-double-8 -ffpe-trap=invalid,zero,overflow
DEBUGFLAGS = -fpp -qopenmp -g -check all -check noarg_temp_created -traceback -fpe0
MODOUT = -module $(INCLUDE_DIR)
DEBUG_MODOUT = -module $(INCLUDE_DIR)
else
FFLAGS =  -fPIC -cpp -fopenmp -O3 -ffree-line-length-none -std=gnu -fdefault-real-8 -fdefault-double-8
DEBUGFLAGS = -g -O0 -fcheck=all -fbacktrace -cpp -fdec -fopenmp -ffree-line-length-none -Wall -fbounds-check -ffpe-trap=invalid,zero,overflow
#add -fpe0 to check for floating point errors (think lowLike also throws these harmlessly)
MODOUT = -J$(INCLUDE_DIR)
DEBUG_MODOUT = -J$(INCLUDE_DIR)
endif

# Objects
_OBJ = \
	precision.o \
	constants.o \
	physics.o \
	sorting.o \
	basic_operations.o \
	special_functions.o \
	array_operations.o \
	file_info.o \
	random_numbers.o \
	table_integer.o \
	interpolate.o \
	solve_equations.o \
	string_operations.o \
	calculus_table.o \
	statistics.o \
	calculus.o \
	minimization.o \
	camb_stuff.o \
	multidark_stuff.o \
	cosmology_functions.o \
	hmx.o \
	
WRAP_SRC = cosmology_functions.f90 hmx.f90
F90WRAP_SRC_FROM_BUILD_DIR = $(addprefix f90wrap_,$(WRAP_SRC))
F90WRAP_SRC = $(addprefix $(F90WRAP_BUILD_DIR)/f90wrap_,$(WRAP_SRC))
WRAP_SRC_FROM_BUILD_DIR = $(addprefix ../$(LIB_SRC_DIR)/,$(WRAP_SRC))

# Add prefixes of build directory to objects
OBJ = $(addprefix $(BUILD_DIR)/,$(_OBJ))
DEBUG_OBJ = $(addprefix $(DEBUG_BUILD_DIR)/,$(_OBJ))

# ?
make_dirs = @mkdir -p $(@D)

all: wrapper

# Standard rules
lib: $(LIB_DIR)/libhmx.a
wrapper: $(LIB_DIR)/libhmx_wrapper.so

# Debugging rules
debug: FFLAGS += $(DEBUGFLAGS)
debug: $(LIB_DIR)/libhmx_wrapper.so

lib_debug: FFLAGS += $(DEBUGFLAGS)
lib_debug: $(LIB_DIR)/libhmx.a

# Rule to make object files
$(BUILD_DIR)/%.o: $(LIB_SRC_DIR)/%.f90 | $(INCLUDE_DIR)
	$(make_dirs)
	$(FC) -c -o $@ $< $(MODOUT) $(LDFLAGS) $(FFLAGS)

# Rule to make debugging objects
$(DEBUG_BUILD_DIR)/%.o: $(LIB_SRC_DIR)/%.f90 | $(INCLUDE_DIR)
	$(FC) -c -o $@ $< $(DEBUG_MODOUT) $(LDFLAGS) $(FFLAGS)

# Create directories
$(INCLUDE_DIR):
	-mkdir $(INCLUDE_DIR)

$(F90WRAP_BUILD_DIR):
	-mkdir $(F90WRAP_BUILD_DIR)
	
# Rule to make HMx static library
$(LIB_DIR)/libhmx.a: $(OBJ)
	$(make_dirs)
	$(LIBTOOL) $@ $^

$(LIB_DIR)/libhmx_wrapper.so: HMx_wrapper.f90 $(LIB_DIR)/libhmx.a
	$(make_dirs)
	$(FC) -o $@ $^ -I$(INCLUDE_DIR) $(LDFLAGS) -shared $(FFLAGS)


f90wrap_python: $(F90WRAP_SRC)

$(F90WRAP_SRC): $(F90WRAP_SRC_DIR)/classes.py | $(F90WRAP_BUILD_DIR)
	cd $(F90WRAP_BUILD_DIR) && f90wrap \
		--only print_cosmology assign_cosmology init_cosmology init_external_linear_power_tables print_halomod assign_halomod init_halomod calculate_HMx_old \
		--shorten-routine-names \
		--skip-types interpolator1D interpolator2D interpolator3D \
		--kind-map ../$(F90WRAP_SRC_DIR)/kind_map \
		-m $(PYTHON_MOD) --package \
		--init-file ../$(F90WRAP_SRC_DIR)/classes.py \
		$(WRAP_SRC_FROM_BUILD_DIR)

f90wrap: lib f90wrap_python
	cd $(F90WRAP_BUILD_DIR) && f2py-f90wrap \
		--build-dir . \
		-c -m _$(PYTHON_MOD) \
		--f90flags="-fdefault-real-8 -fdefault-double-8" \
		-L../$(LIB_DIR) -lhmx -I../$(INCLUDE_DIR) $(F90WRAP_SRC_FROM_BUILD_DIR)

# Clean up
.PHONY: clean
clean:
	rm -f $(LIB_DIR)/libhmx.a
	rm -f $(LIB_DIR)/libhmx_wrapper.so
	test -n "$(LIB_DIR)" && rm -rf $(LIB_DIR)/libhmx_wrapper.so.dSYM/
	rm -f $(BUILD_DIR)/*.o
	rm -f $(INCLUDE_DIR)/*.mod
	rm -f $(SRC_DIR)/*.mod
	rm -f *.mod
	rm -f $(DEBUG_BUILD_DIR)/*.o
	rm -f $(F90WRAP_BUILD_DIR)/*.f90
	rm -f $(F90WRAP_BUILD_DIR)/*.o
	rm -f $(F90WRAP_BUILD_DIR)/*.so
	rm -f $(F90WRAP_BUILD_DIR)/.f2py_f2cmap
	test -n "$(F90WRAP_BUILD_DIR)" && rm -rf $(F90WRAP_BUILD_DIR)/*/
	test -n "$(F90WRAP_BUILD_DIR)" && rm -rf $(F90WRAP_BUILD_DIR)/.libs/
	test -n "$(BUILD_DIR)" && rm -rf $(BUILD_DIR)/*/

