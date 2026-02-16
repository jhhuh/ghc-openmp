#include <stdio.h>
#include <omp.h>

int main(void) {
    int sum = 0;

    // Test 1: parallel region
    #pragma omp parallel
    {
        printf("Hello from thread %d of %d\n",
               omp_get_thread_num(), omp_get_num_threads());
    }

    // Test 2: parallel for with reduction
    #pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < 100; i++) {
        sum += i;
    }
    printf("Sum = %d\n", sum);

    // Test 3: critical section
    #pragma omp parallel
    {
        #pragma omp critical
        {
            sum++;
        }
    }

    // Test 4: single
    #pragma omp parallel
    {
        #pragma omp single
        {
            printf("Only once\n");
        }
        #pragma omp barrier
    }

    // Test 5: task
    #pragma omp parallel
    {
        #pragma omp single
        {
            #pragma omp task
            printf("Task A\n");
            #pragma omp task
            printf("Task B\n");
            #pragma omp taskwait
        }
    }

    return 0;
}
