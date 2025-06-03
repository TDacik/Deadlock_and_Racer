//# deadlocks: []

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

pthread_mutex_t lock1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock2 = PTHREAD_MUTEX_INITIALIZER;

void lock_wrapper(pthread_mutex_t *lock)
{
    pthread_mutex_lock(lock);
}

void unlock_wrapper(pthread_mutex_t *lock)
{
    pthread_mutex_unlock(lock);
}

void *thread1(void *v)
{
    lock_wrapper(&lock1);
    lock_wrapper(&lock2);
    unlock_wrapper(&lock2);
    unlock_wrapper(&lock1);

    return NULL;
}

void *thread2(void *v)
{
    lock_wrapper(&lock1);
    lock_wrapper(&lock2);
    unlock_wrapper(&lock2);
    unlock_wrapper(&lock1);

    return NULL;
}

int main()
{
    pthread_t threads[2];

    pthread_create(&threads[0], NULL, thread1, NULL);
    pthread_create(&threads[1], NULL, thread2, NULL);

    pthread_join(threads[0], NULL);
    pthread_join(threads[1], NULL);

    return 0;
}
