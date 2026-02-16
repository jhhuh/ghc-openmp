CC ?= gcc
CFLAGS = -Wall -Wextra -O2 -g

.PHONY: all clean test test-native test-ghcomp

all: build/libghcomp.so build/test_omp_basic_ghcomp build/test_omp_basic_native

# Our replacement runtime
build/libghcomp.so: src/ghc_omp_runtime.c
	@mkdir -p build
	$(CC) -shared -fPIC $(CFLAGS) -o $@ $< -lpthread

# Test program linked against our runtime
build/test_omp_basic_ghcomp: src/test_omp_basic.c build/libghcomp.so
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< \
		-Lbuild -Wl,-rpath,'$$ORIGIN' \
		-lghcomp \
		-nostdlib \
		$(shell $(CC) -print-file-name=crtbeginS.o 2>/dev/null || true) \
		-lc -lgcc -lgcc_s \
		2>/dev/null || \
	$(CC) -fopenmp $(CFLAGS) -o $@ $< \
		-Lbuild -Wl,-rpath,'$$ORIGIN' -lghcomp

# Test program with native libgomp for comparison
build/test_omp_basic_native: src/test_omp_basic.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $<

test: test-ghcomp

test-ghcomp: build/test_omp_basic_ghcomp
	@echo "=== Running with GHC-OMP runtime ==="
	build/test_omp_basic_ghcomp
	@echo ""

test-native: build/test_omp_basic_native
	@echo "=== Running with native libgomp ==="
	build/test_omp_basic_native
	@echo ""

test-both: test-native test-ghcomp
	@echo "=== Both tests passed ==="

# Verify our library exports the right symbols
check-symbols: build/libghcomp.so
	@echo "=== Exported GOMP/omp symbols ==="
	@nm -D build/libghcomp.so | grep -E " T (GOMP_|omp_)" | awk '{print $$3}' | sort

# Show what symbols the test program needs
check-deps: build/test_omp_basic_native
	@echo "=== Required GOMP/omp symbols ==="
	@objdump -T build/test_omp_basic_native | grep -i "UND.*\(GOMP\|omp_\)" | awk '{print $$NF}' | sort

clean:
	rm -rf build/
