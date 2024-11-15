// All threads are created using wrapper function

//# thread-graph:
//#   - main -> thread1
//#   - main -> thread2

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

void *thread1(void *v)
{
    return NULL;
}

void *thread2(void *v)
{
    return NULL;
}

void thread_create_wrapper(pthread_t *thread, void * (*fn)(void *))
{
    pthread_create(thread, NULL, fn, NULL);
}

int main()
{
    pthread_t threads[2];

    thread_create_wrapper(&threads[0], thread1);
    thread_create_wrapper(&threads[1], thread2);

    pthread_join(threads[0], NULL);
    pthread_join(threads[1], NULL);

    return 0;
}
