//# deadlocks: []
//# params:
//#   - -cc-lockset-context-depth=3

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

pthread_mutex_t lock1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock2 = PTHREAD_MUTEX_INITIALIZER;

void lock_wrapper1(pthread_mutex_t *lockA)
{
    pthread_mutex_lock(lockA);
}

void lock_wrapper2(pthread_mutex_t *lockB)
{
    lock_wrapper1(lockB);
}

void lock_wrapper3(pthread_mutex_t *lockC)
{
    lock_wrapper2(lockC);
}

void lock_wrapper4(pthread_mutex_t *lockD)
{
    lock_wrapper3(lockD);
}

void unlock_wrapper(pthread_mutex_t *lock)
{
    pthread_mutex_unlock(lock);
}

void *thread1(void *v)
{
    lock_wrapper4(&lock1);
    lock_wrapper4(&lock2);
    unlock_wrapper(&lock2);
    unlock_wrapper(&lock1);

    return NULL;
}

void *thread2(void *v)
{
    lock_wrapper4(&lock1);
    lock_wrapper4(&lock2);
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
