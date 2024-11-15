// Thread-graph construction test

//# thread-graph:
//#   - main -> thread1
//#   - main -> thread2

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

pthread_mutex_t lock1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock2 = PTHREAD_MUTEX_INITIALIZER;

void *thread1(void *v)
{
    return NULL;
}

void *thread2(void *v)
{
    return NULL;
}

void t1(pthread_t *t)
{
    pthread_create(t, NULL, thread1, NULL);
}

void t2(pthread_t *t)
{
    pthread_create(t, NULL, thread2, NULL);
}

void f()
{
    pthread_t threads[2];
    t1(&threads[0]);
    t2(&threads[1]);

    pthread_join(threads[0], NULL);
    pthread_join(threads[1], NULL);
}

int main()
{
    f();
    return 0;
}
