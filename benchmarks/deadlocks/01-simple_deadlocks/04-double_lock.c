//# deadlocks:
//#   - - thread1: lock -> lock @ 16
//#
//# params:
//#   - -dl-double-locks

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *v)
{
    pthread_mutex_lock(&lock);
    pthread_mutex_lock(&lock);

    return NULL;
}

int main(int argc, char **argv)
{
    pthread_t tid;

    pthread_create(&tid, NULL, thread, NULL);
    pthread_join(tid , NULL);

    return 0;
}
