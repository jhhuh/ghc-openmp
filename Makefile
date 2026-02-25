CC ?= gcc
GHC ?= ghc
CFLAGS = -Wall -Wextra -O2 -g

# GHC RTS paths (auto-discovered from ghc --print-libdir)
GHC_LIBDIR := $(shell $(GHC) --print-libdir 2>/dev/null)
GHC_VER := $(shell $(GHC) --numeric-version 2>/dev/null)
RTS_INCDIR := $(shell find $(GHC_LIBDIR) -name 'Rts.h' -path '*/include/*' -print -quit 2>/dev/null | xargs dirname)
RTS_SODIR := $(shell find $(GHC_LIBDIR) -maxdepth 2 -name 'libHSrts*_thr-ghc*.so' -print -quit 2>/dev/null | xargs dirname)
RTS_LIBNAME := $(shell find $(GHC_LIBDIR) -maxdepth 2 -name 'libHSrts*_thr-ghc*.so' -print -quit 2>/dev/null | xargs basename | sed 's/^lib//; s/\.so$$//')

.PHONY: all clean test test-native test-ghcomp test-rts-embed test-both

all: build/libghcomp.so build/test_omp_basic_ghcomp build/test_omp_basic_native

# ---- Shared library: RTS-backed OpenMP runtime ----

build/ghc_omp_runtime_rts_pic.o: cbits/ghc_omp_runtime_rts.c
	@mkdir -p build
	$(CC) -DTHREADED_RTS -I$(RTS_INCDIR) $(CFLAGS) -fPIC \
		-c $< -o $@

build/HsStub_pic.o: cbits/HsStub.hs
	@mkdir -p build
	$(GHC) -threaded -dynamic -c $< -o $@

build/libghcomp.so: build/ghc_omp_runtime_rts_pic.o build/HsStub_pic.o
	@mkdir -p build
	$(GHC) -shared -dynamic -threaded -no-hs-main \
		build/ghc_omp_runtime_rts_pic.o build/HsStub_pic.o \
		-o $@ -lpthread -lm \
		-optl-L$(RTS_SODIR) -optl-l$(RTS_LIBNAME) \
		-optl-Wl,-rpath,$(RTS_SODIR) \
		-optl-Wl,-soname,libghcomp.so

# ---- Phase 1: pthread-based stub runtime (reference) ----

build/libghcomp_stub.so: cbits/ghc_omp_runtime.c
	@mkdir -p build
	$(CC) -shared -fPIC $(CFLAGS) -o $@ $< -lpthread

# Test program linked against our shared library (no GHC knowledge needed)
build/test_omp_basic_ghcomp: cbits/test_omp_basic.c build/libghcomp.so
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< \
		-Lbuild -Wl,-rpath,'$$ORIGIN' -lghcomp

# Test program with native libgomp for comparison
build/test_omp_basic_native: cbits/test_omp_basic.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $<

# ---- Phase 2: GHC RTS embedding ----

# Compile Haskell stub (pulls in base library closures)
build/HsStub.o: cbits/HsStub.hs
	@mkdir -p build
	$(GHC) -threaded -c $< -o $@

# GHC RTS embedding test
build/test_rts_embed: cbits/test_rts_embed.c build/HsStub.o
	@mkdir -p build
	$(CC) -DTHREADED_RTS -I$(RTS_INCDIR) $(CFLAGS) \
		-c $< -o build/test_rts_embed.o
	$(GHC) -threaded -no-hs-main \
		build/test_rts_embed.o build/HsStub.o \
		-o $@ -lpthread -lm

# Compile RTS-backed runtime as object file (statically linked into test)
build/ghc_omp_runtime_rts.o: cbits/ghc_omp_runtime_rts.c
	@mkdir -p build
	$(CC) -DTHREADED_RTS -I$(RTS_INCDIR) $(CFLAGS) \
		-c $< -o $@

# Test program with RTS-backed runtime (statically linked, like test_rts_embed)
# Uses ghc as linker to resolve all Haskell RTS symbols
build/test_omp_rts: cbits/test_omp_basic.c build/ghc_omp_runtime_rts.o build/HsStub.o
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

# Phase 18: Guided scheduling test
build/test_guided_native: cbits/test_guided.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/test_guided_rts: cbits/test_guided.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/test_guided_rts.o
	$(GHC) -threaded -no-hs-main \
		build/test_guided_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

test-guided: build/test_guided_native build/test_guided_rts
	@echo "=== Guided: Native libgomp ==="
	@OMP_NUM_THREADS=4 build/test_guided_native
	@echo ""
	@echo "=== Guided: GHC RTS-backed ==="
	@OMP_NUM_THREADS=4 build/test_guided_rts

# Phase 18: Nested parallelism test
build/test_nested_native: cbits/test_nested.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $<

build/test_nested_rts: cbits/test_nested.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/test_nested_rts.o
	$(GHC) -threaded -no-hs-main \
		build/test_nested_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

test-nested: build/test_nested_native build/test_nested_rts
	@echo "=== Nested: Native libgomp ==="
	@OMP_NUM_THREADS=4 build/test_nested_native
	@echo ""
	@echo "=== Nested: GHC RTS-backed ==="
	@OMP_NUM_THREADS=4 build/test_nested_rts

test-all: test-native test-ghcomp test-rts-embed test-rts demo test-guided test-nested

# ---- Phase 4: Haskell ↔ OpenMP Interop ----

# Compile OpenMP compute library
build/omp_compute.o: cbits/omp_compute.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o $@

# Haskell driver that calls OpenMP C via FFI
# ghc provides main, links our runtime + the C compute library
build/hs_omp_demo: demos/HsMain.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsMain.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_omp

# Phase 5: Concurrent Haskell + OpenMP
build/hs_concurrent: demos/HsConcurrent.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsConcurrent.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_conc_out

demo: build/hs_omp_demo
	@echo "=== Haskell ↔ OpenMP Interop Demo ==="
	build/hs_omp_demo +RTS -N4

demo-concurrent: build/hs_concurrent
	@echo "=== Concurrent Haskell + OpenMP Demo ==="
	build/hs_concurrent +RTS -N4

# Phase 6: GC interaction test
build/gc_stress: demos/HsGCStress.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsGCStress.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_gc_out

demo-gc: build/gc_stress
	@echo "=== GC Interaction Stress Test ==="
	build/gc_stress +RTS -N4 -s

# Phase 7: Dense matrix multiply
build/matmul_demo: demos/HsMatMul.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsMatMul.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_matmul_out

demo-matmul: build/matmul_demo
	@echo "=== Dense Matrix Multiply Demo ==="
	build/matmul_demo +RTS -N4

# Phase 9: Bidirectional interop (OpenMP -> Haskell callbacks)
build/callback_demo: demos/HsCallback.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsCallback.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_callback_out

demo-callback: build/callback_demo
	@echo "=== Bidirectional Interop Demo ==="
	build/callback_demo +RTS -N4

# Phase 10: Cmm primitives
build/omp_prims.o: cbits/omp_prims.cmm
	@mkdir -p build
	$(GHC) -c -x cmm $< -o $@

build/cmm_demo: demos/HsCmmDemo.hs build/omp_prims.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsCmmDemo.hs build/omp_prims.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_cmm_out

demo-cmm: build/cmm_demo
	@echo "=== Cmm Primitives Demo ==="
	build/cmm_demo +RTS -N4

# Phase 12: Batched safe calls via Cmm
build/omp_batch.o: cbits/omp_batch.cmm
	@mkdir -p build
	$(GHC) -c -x cmm $< -o $@

build/cmm_batch: demos/HsCmmBatch.hs build/omp_batch.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsCmmBatch.hs build/omp_batch.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_batch_out

# Phase 13: Parallelism crossover analysis
build/crossover: demos/HsCrossover.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsCrossover.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_crossover_out

# Phase 15: Deferred task execution
build/task_demo: demos/HsTaskDemo.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsTaskDemo.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_task_out

# Test task execution with native libgomp for comparison
build/test_tasks_native: cbits/test_omp_tasks.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/test_tasks_rts: cbits/test_omp_tasks.c build/ghc_omp_runtime_rts.o build/HsStub.o
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
build/par_compare: demos/HsParCompare.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsParCompare.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
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
build/zerocopy_demo: demos/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_zerocopy_out

demo-zerocopy: build/zerocopy_demo
	@echo "=== Zero-Copy FFI Demo ==="
	build/zerocopy_demo +RTS -N4

# Phase 17: Linear typed arrays
build/linear_demo: demos/HsLinearDemo.hs demos/Data/Array/Linear.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		demos/HsLinearDemo.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-idemos \
		-outputdir build/hs_linear_out

demo-linear: build/linear_demo
	@echo "=== Linear Typed Arrays Demo ==="
	build/linear_demo +RTS -N4

# ---- Benchmarks ----

build/bench_native: cbits/bench_overhead.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/bench_rts: cbits/bench_overhead.c build/ghc_omp_runtime_rts.o build/HsStub.o
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -c $< -o build/bench_overhead_rts.o
	$(GHC) -threaded -no-hs-main \
		build/bench_overhead_rts.o build/ghc_omp_runtime_rts.o build/HsStub.o \
		-o $@ -lpthread -lm

# DGEMM benchmark — native vs RTS
build/bench_dgemm_native: cbits/bench_dgemm.c
	@mkdir -p build
	$(CC) -fopenmp $(CFLAGS) -o $@ $< -lm

build/bench_dgemm_rts: cbits/bench_dgemm.c build/ghc_omp_runtime_rts.o build/HsStub.o
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

# ---- Build all binaries ----

BUILD_ALL_BINS = \
	build/libghcomp.so \
	build/test_omp_basic_ghcomp build/test_omp_basic_native \
	build/test_rts_embed build/test_omp_rts \
	build/test_guided_native build/test_guided_rts \
	build/test_nested_native build/test_nested_rts \
	build/test_tasks_native build/test_tasks_rts \
	build/hs_omp_demo build/hs_concurrent build/gc_stress \
	build/matmul_demo build/callback_demo \
	build/cmm_demo build/cmm_batch \
	build/crossover build/par_compare build/task_demo \
	build/zerocopy_demo build/linear_demo \
	build/bench_native build/bench_rts \
	build/bench_dgemm_native build/bench_dgemm_rts

build-all: $(BUILD_ALL_BINS)

PREFIX ?= /usr/local

install: build-all
	install -d $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/lib
	install -d $(DESTDIR)$(PREFIX)/lib/pkgconfig
	install -m 755 build/libghcomp.so $(DESTDIR)$(PREFIX)/lib/
	sed 's|@PREFIX@|$(PREFIX)|g; s|@VERSION@|0.18.0|g' \
		ghcomp.pc.in > $(DESTDIR)$(PREFIX)/lib/pkgconfig/ghcomp.pc
	for f in $(filter-out build/libghcomp.so build/%.o,$(BUILD_ALL_BINS)); do \
		install -m 755 $$f $(DESTDIR)$(PREFIX)/bin/$$(basename $$f); \
	done

clean:
	rm -rf build/
