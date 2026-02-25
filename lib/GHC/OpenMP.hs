{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : GHC.OpenMP
-- License     : BSD-3-Clause
-- Stability   : experimental
--
-- = Overview
--
-- An OpenMP runtime that uses GHC's Runtime System (RTS) as its thread pool
-- and scheduler infrastructure.  Standard C code compiled with
-- @gcc -fopenmp@ runs on GHC Capabilities instead of libgomp's pthreads,
-- enabling seamless interoperation between Haskell and OpenMP-parallelized C.
--
-- Full documentation with benchmarks:
-- <https://jhhuh.github.io/ghc-openmp/>
--
-- = How it works
--
-- The runtime implements the GCC @GOMP_*@ ABI and the @omp_*@ user API in a
-- single C file (@cbits\/ghc_omp_runtime_rts.c@, ~1300 lines).  On the first
-- @GOMP_parallel@ call, it initializes the GHC RTS (or increments its ref
-- count if already running from Haskell) and creates N-1 OS worker threads
-- pinned to GHC Capabilities via @rts_setInCallCapability()@.
--
-- Workers are /not/ Haskell threads — they are plain OS threads that spin on
-- atomic variables, invisible to GHC's garbage collector.  This means:
--
--   * GC does not pause OpenMP workers (they don't hold Capabilities)
--   * Haskell green threads and OpenMP parallel regions can run simultaneously
--   * @foreign import ccall safe@ releases the Capability, so other Haskell
--     threads run while OpenMP executes
--
-- = Usage from Haskell
--
-- Add @ghc-openmp@ to your @build-depends@.  The C runtime source is compiled
-- directly into your package using your own GHC, so there are no ABI conflicts
-- with your RTS version.
--
-- @
-- -- In your .cabal file:
-- build-depends: ghc-openmp
-- ghc-options:   -threaded
--
-- -- In your Haskell code:
-- foreign import ccall safe "omp_parallel_sinsum"
--   c_sinsum :: CInt -> IO CDouble
-- @
--
-- = Usage from C
--
-- Build @libghcomp.so@ via @make@ or @nix build@, then link with @-lghcomp@:
--
-- @
-- gcc -fopenmp my_program.c -lghcomp -o my_program
-- @
--
-- The shared library embeds the GHC RTS via rpath — C consumers don't need
-- to know about GHC at all.  A pkg-config template is shipped in the
-- package's @data\/@ directory (see 'pkgConfigTemplatePath').
--
-- = Performance
--
-- After lock-free optimization (sense-reversing barriers, generation-counter
-- dispatch), the runtime achieves performance parity with native libgomp:
--
-- +----------------------+----------------+------------+-------+
-- | Metric               | Native libgomp | RTS-backed | Ratio |
-- +======================+================+============+=======+
-- | Fork/join            | 0.97 us        | 0.81 us    | 0.83x |
-- +----------------------+----------------+------------+-------+
-- | Barrier              | 0.51 us        | 0.25 us    | 0.50x |
-- +----------------------+----------------+------------+-------+
-- | Parallel for (1M)    | 3.85 ms        | 3.91 ms    | 1.01x |
-- +----------------------+----------------+------------+-------+
-- | DGEMM 1024           | 748.8 ms       | 663.4 ms   | 0.89x |
-- +----------------------+----------------+------------+-------+
--
-- = Calling convention overhead
--
-- When calling C from Haskell, the FFI calling convention matters:
--
-- +----------------------------------+---------+----------------------------------+
-- | Convention                       | ns/call | Notes                            |
-- +==================================+=========+==================================+
-- | @foreign import prim@ (Cmm)      | ~0      | GHC can optimize away (LICM)     |
-- +----------------------------------+---------+----------------------------------+
-- | @foreign import ccall unsafe@    | ~2      | STG register save/restore        |
-- +----------------------------------+---------+----------------------------------+
-- | @foreign import ccall safe@      | ~68     | + Capability release/reacquire   |
-- +----------------------------------+---------+----------------------------------+
--
-- Use @unsafe@ for fast, non-blocking C functions.  Use @safe@ for functions
-- that may block or call back into Haskell.

module GHC.OpenMP
    ( -- * Thread queries
      -- | Query and control the OpenMP thread team.  These correspond to the
      -- standard @omp_*@ API from the OpenMP specification.
      --
      -- All thread query functions use @ccall unsafe@ since they are simple
      -- reads of thread-local variables (~2 ns overhead).
      ompGetNumThreads
    , ompGetThreadNum
    , ompGetMaxThreads
    , ompGetNumProcs
    , ompSetNumThreads
    , ompInParallel
      -- * Timing
      -- | Wall-clock timing functions for benchmarking parallel regions.
      -- Uses @clock_gettime(CLOCK_MONOTONIC)@ internally.
    , ompGetWtime
    , ompGetWtick
      -- * Nesting
      -- | Query nesting state.  Our runtime serializes nested parallel regions
      -- (inner regions execute with 1 thread) but tracks nesting level up to
      -- 8 levels deep for @omp_get_level()@ and @omp_get_active_level()@.
    , ompGetLevel
    , ompGetActiveLevel
      -- * Pkg-config
      -- | Access the pkg-config template for building @libghcomp.so@.
    , pkgConfigTemplatePath
    ) where

import Paths_ghc_openmp (getDataFileName)

-- | Returns the number of threads in the current parallel team.
-- Outside a parallel region, returns 1.
--
-- Corresponds to @omp_get_num_threads()@ in the OpenMP specification.
-- Reads from a thread-local variable — essentially free.
foreign import ccall unsafe "omp_get_num_threads" ompGetNumThreads :: IO Int

-- | Returns the thread number of the calling thread within the current
-- parallel team (0 = master, 1..N-1 = workers).
-- Outside a parallel region, returns 0.
--
-- Corresponds to @omp_get_thread_num()@.  Reads from a thread-local variable.
foreign import ccall unsafe "omp_get_thread_num"  ompGetThreadNum  :: IO Int

-- | Returns the maximum number of threads that could form a parallel team.
-- Equivalent to the value of @OMP_NUM_THREADS@ or the number of GHC
-- Capabilities (whichever the runtime was initialized with).
--
-- Corresponds to @omp_get_max_threads()@.
foreign import ccall unsafe "omp_get_max_threads" ompGetMaxThreads :: IO Int

-- | Returns the number of processors available to the program.
-- Uses @sysconf(_SC_NPROCESSORS_ONLN)@ on Linux.
--
-- Corresponds to @omp_get_num_procs()@.
foreign import ccall unsafe "omp_get_num_procs"   ompGetNumProcs   :: IO Int

-- | Sets the number of threads for subsequent parallel regions.
-- Takes effect at the next @GOMP_parallel@ call.
--
-- Corresponds to @omp_set_num_threads()@.
--
-- Note: cannot exceed @GHC_OMP_MAX_THREADS@ (default 64).
foreign import ccall unsafe "omp_set_num_threads" ompSetNumThreads :: Int -> IO ()

-- | Returns 1 if called from within an active parallel region, 0 otherwise.
--
-- Corresponds to @omp_in_parallel()@.
foreign import ccall unsafe "omp_in_parallel"     ompInParallel    :: IO Int

-- | Returns the elapsed wall-clock time in seconds since an arbitrary
-- reference point (monotonic clock).  Useful for timing parallel regions:
--
-- @
-- t0 <- ompGetWtime
-- -- ... parallel work ...
-- t1 <- ompGetWtime
-- putStrLn $ "Elapsed: " ++ show (t1 - t0) ++ " s"
-- @
--
-- Corresponds to @omp_get_wtime()@.  Uses @clock_gettime(CLOCK_MONOTONIC)@.
foreign import ccall unsafe "omp_get_wtime"       ompGetWtime      :: IO Double

-- | Returns the precision of the timer used by 'ompGetWtime', in seconds.
-- Typically @1e-9@ (nanosecond resolution) on modern Linux.
--
-- Corresponds to @omp_get_wtick()@.
foreign import ccall unsafe "omp_get_wtick"       ompGetWtick      :: IO Double

-- | Returns the current nesting level of parallel regions.
-- The outermost parallel region is level 1; outside any parallel region,
-- returns 0.
--
-- Our runtime supports serialized nesting up to 8 levels deep.
--
-- Corresponds to @omp_get_level()@.
foreign import ccall unsafe "omp_get_level"       ompGetLevel      :: IO Int

-- | Returns the number of /active/ (non-serialized) nesting levels.
-- Since our runtime serializes inner parallel regions, this is at most 1.
--
-- Corresponds to @omp_get_active_level()@.
foreign import ccall unsafe "omp_get_active_level" ompGetActiveLevel :: IO Int

-- | Returns the file path to the @ghcomp.pc.in@ pkg-config template
-- shipped with this package.  This template can be used to generate a
-- @ghcomp.pc@ file for @pkg-config@ integration:
--
-- @
-- template <- pkgConfigTemplatePath
-- -- template points to: \<package-data-dir\>/ghcomp.pc.in
-- -- Substitute \@PREFIX\@ and \@VERSION\@ to generate ghcomp.pc
-- @
--
-- C programs can then use:
--
-- @
-- gcc -fopenmp my_code.c $(pkg-config --cflags --libs ghcomp) -o my_code
-- @
pkgConfigTemplatePath :: IO FilePath
pkgConfigTemplatePath = getDataFileName "ghcomp.pc.in"
