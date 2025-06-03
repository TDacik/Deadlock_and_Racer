// Two independent deadlocks

//# deadlocks:
//#   - - thread1 : lock1 -> lock2 @ 24
//#     - thread2 : lock2 -> lock1 @ 34
//#
//#   - - thread3 : lock3 -> lock4 @ 44
//#     - thread4 : lock4 -> lock5 @ 54
//#     - thread5 : lock5 -> lock3 @ 64

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

pthread_mutex_t lock1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock3 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock4 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t lock5 = PTHREAD_MUTEX_INITIALIZER;

void *thread1(void *v)
{
    pthread_mutex_lock(&lock1);
    pthread_mutex_lock(&lock2);
    pthread_mutex_unlock(&lock2);
    pthread_mutex_unlock(&lock1);

    return NULL;
}

void *thread2(void *v)
{
    pthread_mutex_lock(&lock2);
    pthread_mutex_lock(&lock1);
    pthread_mutex_unlock(&lock1);
    pthread_mutex_unlock(&lock2);

    return NULL;
}

void *thread3(void *v)
{
    pthread_mutex_lock(&lock3);
    pthread_mutex_lock(&lock4);
    pthread_mutex_unlock(&lock4);
    pthread_mutex_unlock(&lock3);

    return NULL;
}

void *thread4(void *v)
{
    pthread_mutex_lock(&lock4);
    pthread_mutex_lock(&lock5);
    pthread_mutex_unlock(&lock5);
    pthread_mutex_unlock(&lock4);

    return NULL;
}

void *thread5(void *v)
{
    pthread_mutex_lock(&lock5);
    pthread_mutex_lock(&lock3);
    pthread_mutex_unlock(&lock3);
    pthread_mutex_unlock(&lock5);

    return NULL;
}

int main(int argc, char **argv)
{
    pthread_t threads[5];

    pthread_create(&threads[0], NULL, thread1, NULL);
    pthread_create(&threads[1], NULL, thread2, NULL);
    pthread_create(&threads[2], NULL, thread3, NULL);
    pthread_create(&threads[3], NULL, thread4, NULL);
    pthread_create(&threads[4], NULL, thread5, NULL);

    pthread_join(threads[0], NULL);
    pthread_join(threads[1], NULL);
    pthread_join(threads[2], NULL);
    pthread_join(threads[3], NULL);
    pthread_join(threads[4], NULL);

    return 0;
}
