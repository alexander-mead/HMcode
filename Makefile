# Makefile to compile HMcode

# Compiler
FC = gfortran 

# Standard flags
FFLAGS = \
	-fcheck=all \
	-fmax-errors=4 \
	-ffpe-trap=invalid,zero,overflow \
	-fimplicit-none \
	-O3 \
	-std=gnu \
	-ffree-line-length-none \
	-fdefault-real-8 \
	-fdefault-double-8 \
	-lgfortran \
	-lm

# Extra debugging flags
DEBUG_FLAGS = \
	-Wall \
	-fcheck=all \
	-fbounds-check \
	-fbacktrace \
	-Og

# Binary
BIN = HMcode
BIN_DEBUG = $(BIN)_debug

# Source-code directory
SRC_DIR = src

# Build directory
BUILD_DIR = build

# Fortran library directory
MOD_DIR = library/src
#MOD_DIR = /Users/Mead/Physics/library/src

# Debug build directory
DEBUG_BUILD_DIR = debug_build

# Executable directory
BIN_DIR = bin

# Objects
_OBJ = \
	precision.o \
	constants.o \
	physics.o \
	basic_operations.o \
	array_operations.o \
	random_numbers.o \
	file_info.o \
	table_integer.o \
	special_functions.o \
	interpolate.o \
	solve_equations.o \
	string_operations.o \
	calculus_table.o \
	camb_stuff.o \
	sorting.o \
	statistics.o \
	calculus.o \
	minimization.o \
	multidark_stuff.o \
	cosmology_functions.o \
	hmx.o 

# Add prefixes of build directory to objects
OBJ = $(addprefix $(BUILD_DIR)/,$(_OBJ))
DEBUG_OBJ = $(addprefix $(DEBUG_BUILD_DIR)/,$(_OBJ))

# This is a rule to make directories
make_dirs = @mkdir -p $(@D)

# Standard rules
all: bin
bin: $(BIN_DIR)/$(BIN)

# Debugging rules
debug: FFLAGS += $(DEBUG_FLAGS)
debug: $(BIN_DIR)/$(BIN_DEBUG)

# Rule to make object files
$(BUILD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(make_dirs)
	$(FC) -c -o $@ $< -J$(BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make executable
$(BIN_DIR)/$(BIN): $(OBJ) $(SRC_DIR)/$(BIN).f90
	@echo "\nBuilding executable.\n"
	$(make_dirs)
	$(FC) -o $@ $^ -J$(BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make debugging objects
$(DEBUG_BUILD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(make_dirs)
	$(FC) -c -o $@ $< -J$(DEBUG_BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make debugging executable
$(BIN_DIR)/$(BIN_DEBUG): $(DEBUG_OBJ) $(SRC_DIR)/$(BIN).f90
	@echo "\nBuilding debugging executable.\n"
	$(FC) -o $@ $^ -J$(DEBUG_BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Clean up
.PHONY: clean
clean:
	rm -f $(BIN_DIR)/$(BIN)
	rm -f $(BIN_DIR)/$(BIN_DEBUG)
	rm -f $(BUILD_DIR)/*.o
	rm -f $(BUILD_DIR)/*.mod
	rm -f $(DEBUG_BUILD_DIR)/*.o
	rm -f $(DEBUG_BUILD_DIR)/*.mod
