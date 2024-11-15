// Thread-graph construction test

//# thread-graph:
//#   - main -> thread1
//#   - main -> thread2
//#   - thread1 -> thread3
//#   - thread2 -> thread3

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

void *thread3(void *v)
{
    return NULL;
}


void *thread1(void *v)
{
    pthread_t t;
    pthread_create(&t, NULL, thread3, NULL);
    pthread_join(t, NULL);

    return NULL;
}

void *thread2(void *v)
{
    pthread_t t;
    pthread_create(&t, NULL, thread3, NULL);
    pthread_join(t, NULL);

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
