CC ?= gcc
GHC ?= ghc
CFLAGS = -Wall -Wextra -O2 -g

# GHC RTS paths (set by nix develop or manually)
GHC_LIBDIR := $(shell $(GHC) --print-libdir 2>/dev/null)
GHC_PLAT := x86_64-linux-ghc-9.10.3
RTS_INCDIR := $(GHC_LIBDIR)/$(GHC_PLAT)/rts-1.0.2/include
RTS_LIBDIR := $(GHC_LIBDIR)/$(GHC_PLAT)

.PHONY: all clean test test-native test-ghcomp test-rts-embed test-both

all: build/libghcomp.so build/test_omp_basic_ghcomp build/test_omp_basic_native

# ---- Phase 1: pthread-based stub runtime ----

# Our replacement runtime (pthread-based)
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

# ---- Phase 2: GHC RTS embedding ----

# Compile Haskell stub (pulls in base library closures)
build/HsStub.o: src/HsStub.hs
	@mkdir -p build
	$(GHC) -threaded -c $< -o $@

# GHC RTS embedding test
build/test_rts_embed: src/test_rts_embed.c build/HsStub.o
	@mkdir -p build
	$(CC) -DTHREADED_RTS -I$(RTS_INCDIR) $(CFLAGS) \
		-c $< -o build/test_rts_embed.o
	$(GHC) -threaded -no-hs-main \
		build/test_rts_embed.o build/HsStub.o \
		-o $@ -lpthread -lm

# Compile RTS-backed runtime as object file (statically linked into test)
build/ghc_omp_runtime_rts.o: src/ghc_omp_runtime_rts.c
	@mkdir -p build
	$(CC) -DTHREADED_RTS -I$(RTS_INCDIR) $(CFLAGS) \
		-c $< -o $@

# Test program with RTS-backed runtime (statically linked, like test_rts_embed)
# Uses ghc as linker to resolve all Haskell RTS symbols
build/test_omp_rts: src/test_omp_basic.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/test_omp_basic_rts.o
	$(GHC) -threaded -no-hs-main \
		build/test_omp_basic_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

# ---- Test targets ----

test: test-ghcomp

test-ghcomp: build/test_omp_basic_ghcomp
	@echo "=== Running with GHC-OMP runtime ==="
	OMP_NUM_THREADS=4 build/test_omp_basic_ghcomp
	@echo ""

test-native: build/test_omp_basic_native
	@echo "=== Running with native libgomp ==="
	OMP_NUM_THREADS=4 build/test_omp_basic_native
	@echo ""

test-rts-embed: build/test_rts_embed
	@echo "=== Running GHC RTS embedding test ==="
	build/test_rts_embed
	@echo ""

test-both: test-native test-ghcomp
	@echo "=== Both tests passed ==="

test-rts: build/test_omp_rts
	@echo "=== Running with GHC RTS-backed runtime ==="
	OMP_NUM_THREADS=4 build/test_omp_rts
	@echo ""

test-all: test-native test-ghcomp test-rts-embed test-rts

# ---- Benchmarks ----

build/bench_native: src/bench_overhead.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/bench_rts: src/bench_overhead.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/bench_overhead_rts.o
	$(GHC) -threaded -no-hs-main \
		build/bench_overhead_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

bench: build/bench_native build/bench_rts
	@echo "=== Native libgomp ==="
	@OMP_NUM_THREADS=4 build/bench_native
	@echo ""
	@echo "=== GHC RTS-backed ==="
	@OMP_NUM_THREADS=4 build/bench_rts

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
