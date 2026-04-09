# libtess2 Pascal port - Simple unit test Makefile

ifeq ($(wildcard ../../src),)
    SRCDIR := ../..
else
    SRCDIR := ../../src
endif
PROG := test_simple

# Output directories for different targets (avoid interference)
OUTDIR_DEFAULT := out
OUTDIR_WIN64 := out-win64

# Default target: build and run with default platform
test: build-default
	@echo "Running tests (default platform)..."
	@./$(OUTDIR_DEFAULT)/$(PROG)

# Win64 target: build and run for win64
test-win64: build-win64
	@echo "Running tests (win64)..."
	@./$(OUTDIR_WIN64)/$(PROG)

# Build for default platform
build-default: $(OUTDIR_DEFAULT)
	fpc -Fu$(SRCDIR) -FE$(OUTDIR_DEFAULT) -FU$(OUTDIR_DEFAULT) $(PROG).pas

# Build for win64
build-win64: $(OUTDIR_WIN64)
	fpc -Fu$(SRCDIR) -FE$(OUTDIR_WIN64) -FU$(OUTDIR_WIN64) -Twin64 -Px86_64 $(PROG).pas

$(OUTDIR_DEFAULT):
ifeq ($(OS),Windows_NT)
	-mkdir $(OUTDIR_DEFAULT)
else
	mkdir -p $(OUTDIR_DEFAULT)
endif

$(OUTDIR_WIN64):
ifeq ($(OS),Windows_NT)
	-mkdir $(OUTDIR_WIN64)
else
	mkdir -p $(OUTDIR_WIN64)
endif

clean:
ifeq ($(OS),Windows_NT)
	-rmdir /S /Q $(OUTDIR_DEFAULT) 2>nul
	-rmdir /S /Q $(OUTDIR_WIN64) 2>nul
else
	rm -rf $(OUTDIR_DEFAULT) $(OUTDIR_WIN64)
endif

.PHONY: test test-win64 build-default build-win64 clean
