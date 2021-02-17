==============================================================================
C Code for fork() creation test
==============================================================================
#include <stdio.h>
#include <stdlib.h>
#define NFORKS 50000

void do_nothing() {
int i;
i= 0;
}

int main(int argc, char *argv[]) {
int pid, j, status;

for (j=0; j<NFORKS; j++) {

  /*** error handling ***/
  if ((pid = fork()) < 0 ) {
    printf ("fork failed with error code= %d\n", pid);
    exit(0);
    }

  /*** this is the child of the fork ***/
  else if (pid ==0) {
    do_nothing();
    exit(0);
    }

  /*** this is the parent of the fork ***/
  else {
    waitpid(pid, status, 0);
    }
  }
}  

==============================================================================
C Code for pthread_create() test
==============================================================================
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NTHREADS 50000

void *do_nothing(void *null) {
int i;
i=0;
pthread_exit(NULL);
}                      

int main(int argc, char *argv[]) {
int rc, i, j, detachstate;
pthread_t tid;
pthread_attr_t attr;

pthread_attr_init(&attr);
pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

for (j=0; j<NTHREADS; j++) {
  rc = pthread_create(&tid, &attr, do_nothing, NULL);
  if (rc) {              
    printf("ERROR; return code from pthread_create() is %d\n", rc);
    exit(-1);
    }

  /* Wait for the thread */
  rc = pthread_join(tid, NULL);
  if (rc) {
    printf("ERROR; return code from pthread_join() is %d\n", rc);
    exit(-1);
    }
  }

pthread_attr_destroy(&attr);
pthread_exit(NULL);

}
