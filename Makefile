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

test-all: test-native test-ghcomp test-rts-embed test-rts demo

# ---- Phase 4: Haskell ↔ OpenMP Interop ----

# Compile OpenMP compute library
build/omp_compute.o: src/omp_compute.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o $@

# Haskell driver that calls OpenMP C via FFI
# ghc provides main, links our runtime + the C compute library
build/hs_omp_demo: src/HsMain.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsMain.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_omp

# Phase 5: Concurrent Haskell + OpenMP
build/hs_concurrent: src/HsConcurrent.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsConcurrent.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_conc_out

demo: build/hs_omp_demo
	@echo "=== Haskell ↔ OpenMP Interop Demo ==="
	build/hs_omp_demo +RTS -N4

demo-concurrent: build/hs_concurrent
	@echo "=== Concurrent Haskell + OpenMP Demo ==="
	build/hs_concurrent +RTS -N4

# Phase 6: GC interaction test
build/gc_stress: src/HsGCStress.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsGCStress.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_gc_out

demo-gc: build/gc_stress
	@echo "=== GC Interaction Stress Test ==="
	build/gc_stress +RTS -N4 -s

# Phase 7: Dense matrix multiply
build/matmul_demo: src/HsMatMul.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsMatMul.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_matmul_out

demo-matmul: build/matmul_demo
	@echo "=== Dense Matrix Multiply Demo ==="
	build/matmul_demo +RTS -N4

# Phase 9: Bidirectional interop (OpenMP -> Haskell callbacks)
build/callback_demo: src/HsCallback.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsCallback.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_callback_out

demo-callback: build/callback_demo
	@echo "=== Bidirectional Interop Demo ==="
	build/callback_demo +RTS -N4

# Phase 10: Cmm primitives
build/omp_prims.o: src/omp_prims.cmm
	@mkdir -p build
	$(GHC) -c -x cmm $< -o $@

build/cmm_demo: src/HsCmmDemo.hs build/omp_prims.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsCmmDemo.hs build/omp_prims.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_cmm_out

demo-cmm: build/cmm_demo
	@echo "=== Cmm Primitives Demo ==="
	build/cmm_demo +RTS -N4

# Phase 12: Batched safe calls via Cmm
build/omp_batch.o: src/omp_batch.cmm
	@mkdir -p build
	$(GHC) -c -x cmm $< -o $@

build/cmm_batch: src/HsCmmBatch.hs build/omp_batch.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsCmmBatch.hs build/omp_batch.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_batch_out

# Phase 13: Parallelism crossover analysis
build/crossover: src/HsCrossover.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsCrossover.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_crossover_out

# Phase 15: Deferred task execution
build/task_demo: src/HsTaskDemo.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsTaskDemo.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_task_out

# Test task execution with native libgomp for comparison
build/test_tasks_native: src/test_omp_tasks.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/test_tasks_rts: src/test_omp_tasks.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/test_omp_tasks_rts.o
	$(GHC) -threaded -no-hs-main \
		build/test_omp_tasks_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

demo-tasks: build/task_demo
	@echo "=== Deferred Task Execution Demo ==="
	build/task_demo +RTS -N4

test-tasks: build/test_tasks_native build/test_tasks_rts
	@echo "=== Tasks: Native libgomp ==="
	@OMP_NUM_THREADS=4 build/test_tasks_native
	@echo ""
	@echo "=== Tasks: GHC RTS-backed ==="
	@OMP_NUM_THREADS=4 build/test_tasks_rts

# Phase 14: GHC native parallelism vs OpenMP
build/par_compare: src/HsParCompare.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsParCompare.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_parcomp_out

demo-parcompare: build/par_compare
	@echo "=== GHC vs OpenMP Parallelism Comparison ==="
	build/par_compare +RTS -N4

demo-crossover: build/crossover
	@echo "=== Parallelism Crossover Analysis ==="
	build/crossover +RTS -N4

demo-batch: build/cmm_batch
	@echo "=== Batched Safe Calls Demo ==="
	build/cmm_batch +RTS -N4

# Phase 16: Zero-copy FFI with pinned ByteArray
build/zerocopy_demo: src/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_zerocopy_out

demo-zerocopy: build/zerocopy_demo
	@echo "=== Zero-Copy FFI Demo ==="
	build/zerocopy_demo +RTS -N4

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

# DGEMM benchmark — native vs RTS
build/bench_dgemm_native: src/bench_dgemm.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/bench_dgemm_rts: src/bench_dgemm.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/bench_dgemm_rts.o
	$(GHC) -threaded -no-hs-main \
		build/bench_dgemm_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

bench-dgemm: build/bench_dgemm_native build/bench_dgemm_rts
	@for t in 1 2 4 8; do \
		echo "=== $$t threads: Native libgomp ===" ; \
		OMP_NUM_THREADS=$$t build/bench_dgemm_native ; \
		echo "" ; \
		echo "=== $$t threads: GHC RTS-backed ===" ; \
		OMP_NUM_THREADS=$$t build/bench_dgemm_rts ; \
		echo "" ; \
	done

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
