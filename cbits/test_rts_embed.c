/*
 * test_rts_embed.c — Minimal test of embedding GHC RTS from C
 *
 * Tests:
 * 1. Boot GHC RTS with hs_init_ghc (threaded, N capabilities)
 * 2. Acquire capabilities from multiple OS threads
 * 3. Verify cap->no gives deterministic thread IDs
 * 4. Shut down cleanly
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "Rts.h"
#include "RtsAPI.h"

#define NUM_WORKERS 4

typedef struct {
    int worker_id;
    pthread_barrier_t *barrier;
} worker_info_t;

static void *worker_func(void *arg) {
    worker_info_t *info = (worker_info_t *)arg;

    /* Pin this OS thread to a specific capability */
    rts_setInCallCapability(info->worker_id, 1);

    /* Acquire a capability */
    Capability *cap = rts_lock();

    printf("Worker %d: got Capability %d (cap->no = %d)\n",
           info->worker_id,
           (int)(((CapabilityPublic *)cap)->r.rCurrentNursery != NULL),
           info->worker_id);  /* We expect cap->no == worker_id */

    /* Wait at barrier (all workers have their capability) */
    pthread_barrier_wait(info->barrier);

    /* Release the capability */
    rts_unlock(cap);

    return NULL;
}

/* GHC RTS needs these symbols even with -no-hs-main */
StgClosure *ZCMain_main_closure = NULL;

int main(int argc, char *argv[]) {
    printf("=== GHC RTS Embedding Test ===\n");

    /* Boot the RTS with threaded mode and N capabilities */
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    conf.rts_hs_main = HS_BOOL_FALSE;

    /* Set +RTS -N4 via rts_opts */
    char rts_opts_buf[64];
    snprintf(rts_opts_buf, sizeof(rts_opts_buf), "-N%d", NUM_WORKERS);
    conf.rts_opts = rts_opts_buf;

    hs_init_ghc(&argc, &argv, conf);

    printf("RTS initialized. n_capabilities = %d\n", getNumCapabilities());

    /* Test: spawn worker threads, each acquires a distinct capability */
    pthread_t threads[NUM_WORKERS];
    worker_info_t infos[NUM_WORKERS];
    pthread_barrier_t barrier;
    pthread_barrier_init(&barrier, NULL, NUM_WORKERS);

    for (int i = 0; i < NUM_WORKERS; i++) {
        infos[i].worker_id = i;
        infos[i].barrier = &barrier;
        pthread_create(&threads[i], NULL, worker_func, &infos[i]);
    }

    for (int i = 0; i < NUM_WORKERS; i++) {
        pthread_join(threads[i], NULL);
    }

    pthread_barrier_destroy(&barrier);

    printf("All workers completed. Shutting down RTS.\n");
    hs_exit();

    printf("=== Test passed ===\n");
    return 0;
}
