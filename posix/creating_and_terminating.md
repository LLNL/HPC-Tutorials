---
layout: tutorial_page
title: "Creating and Terminating Threads"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
## Thread Management: Creating and Terminating Threads

### Routines:

[`pthread_create(thread, attr, start_routine, arg)`](man/pthread_create.txt)

[`pthread_exit(status)`](man/pthread_exit.txt)

[`pthread_cancel(thread)`](man/pthread_cancel.txt)

[`pthread_attr_init(attr)`](man/pthread_attr_init.txt)

[`pthread_attr_destroy(attr)`](man/pthread_attr_destroy.txt)

### Creating Threads:

Initially, your `main()` program comprises a single, default thread. All other threads must be explicitly created by the programmer. `pthread_create` creates a new thread and makes it executable. This routine can be called any number of times from anywhere within your code.

`pthread_create` arguments:
* `thread`: An opaque, unique identifier for the new thread returned by the subroutine.
* `attr`:  An opaque attribute object that may be used to set thread attributes. You can specify a thread attributes object, or NULL for the default values.
* `start_routine`: the C routine that the thread will execute once it is created.
* `arg`: A single argument that may be passed to start_routine. It must be passed by reference as `(void *)`. `NULL` may be used if no argument is to be passed.

The maximum number of threads that may be created by a process is implementation dependent. Programs that attempt to exceed the limit can fail or produce wrong results.

### Thread limits

The next examples show how to query and set your implementation's thread limit on Linux. First we query the default (soft) limits and then set the maximum number of processes (including threads) to the hard limit. Then we verify that the limit has been overridden.

#### bash / ksh / sh example

```sh
$ ulimit -a
core file size          (blocks, -c) 16
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 0
file size               (blocks, -f) unlimited
pending signals                 (-i) 255956
max locked memory       (kbytes, -l) 64
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) unlimited
cpu time               (seconds, -t) unlimited
max user processes              (-u) 1024
virtual memory          (kbytes, -v) unlimited
file locks                      (-x) unlimited

$ ulimit -Hu
7168

$ ulimit -u 7168

$ ulimit -a
core file size          (blocks, -c) 16
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 0
file size               (blocks, -f) unlimited
pending signals                 (-i) 255956
max locked memory       (kbytes, -l) 64
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) unlimited
cpu time               (seconds, -t) unlimited
max user processes              (-u) 7168
virtual memory          (kbytes, -v) unlimited
file locks                      (-x) unlimited
```

#### tcsh/csh example

```sh
% limit
cputime      unlimited
filesize     unlimited
datasize     unlimited
stacksize    unlimited
coredumpsize 16 kbytes
memoryuse    unlimited
vmemoryuse   unlimited
descriptors  1024
memorylocked 64 kbytes
maxproc      1024

% limit maxproc unlimited

% limit
cputime      unlimited
filesize     unlimited
datasize     unlimited
stacksize    unlimited
coredumpsize 16 kbytes
memoryuse    unlimited
vmemoryuse   unlimited
descriptors  1024
memorylocked 64 kbytes
maxproc      7168
```

Once created, threads are peers, and may create other threads. There is no implied hierarchy or dependency between threads.

![peer_threads](images/peerThreads.gif)

### Thread Attributes:

By default, a thread is created with certain attributes. Some of these attributes can be changed by the programmer via the thread attribute object.

`pthread_attr_init` and `pthread_attr_destroy` are used to initialize/destroy the thread attribute object.

Other routines are then used to query/set specific attributes in the thread attribute object. Attributes include:

* Detached or joinable state
* Scheduling inheritance
* Scheduling policy
* Scheduling parameters
* Scheduling contention scope
* Stack size
* Stack address
* Stack guard (overflow) size
* Some of these attributes will be discussed later.

### Thread Binding and Scheduling:

Question: After a thread has been created, how do you know a) when it will be scheduled to run by the operating system, and b) which processor/core it will run on?

<details>
  <summary>Click for answer</summary>


*Unless you are using the Pthreads scheduling mechanism, it is up to the implementation and/or operating system to decide where and when threads will execute. Robust programs should not depend upon threads executing in a specific order or on a specific processor/core.*

</details>

The Pthreads API provides several routines that may be used to specify how threads are scheduled for execution. For example, threads can be scheduled to run FIFO (first-in first-out), RR (round-robin) or OTHER (operating system determines). It also provides the ability to set a thread's scheduling priority value.

These topics are not covered here, however a good overview of "how things work" under Linux can be found in the [sched_setscheduler](man/sched_setscheduler.txt) man page.

The Pthreads API does not provide routines for binding threads to specific cpus/cores. However, local implementations may include this functionality - such as providing the non-standard [pthread_setaffinity_np](man/pthread_setaffinity_np.txt) routine. Note that "_np" in the name stands for "non-portable".

Also, the local operating system may provide a way to do this. For example, Linux provides the [sched_setaffinity](man/sched_setaffinity.txt) routine.

### Terminating Threads & `pthread_exit()`

There are several ways in which a thread may be terminated:

* The thread returns normally from its starting routine. Its work is done.
* The thread makes a call to the `pthread_exit` subroutine - whether its work is done or not.
* The thread is canceled by another thread via the `pthread_cancel` routine.
* The entire process is terminated due to making a call to either the `exec()` or `exit()`
* If `main()` finishes first, without calling `pthread_exit` explicitly itself

The `pthread_exit()` routine allows the programmer to specify an optional termination status parameter. This optional parameter is typically returned to threads "joining" the terminated thread (covered later).

In subroutines that execute to completion normally, you can often dispense with calling `pthread_exit()` - unless, of course, you want to get the routine's return value.

Cleanup: the `pthread_exit()` routine does not close files; any files opened inside the thread will remain open after the thread is terminated.

Discussion on calling `pthread_exit()` from `main()`:

* There is a definite problem if `main()` finishes before the threads it spawned if you don't call `pthread_exit()` explicitly. All of the threads it created will terminate because `main()` is done and no longer exists to support the threads.

* By having `main()` explicitly call `pthread_exit()` as the last thing it does, `main()` will block and be kept alive to support the threads it created until they are done.

### Example: Pthread Creation and Termination

This simple example code creates 5 threads with the `pthread_create()` routine. Each thread prints a "Hello World!" message. Then, it terminates with a call to `pthread_exit()`.

```C
#include <pthread.h>
#include <stdio.h>
#define NUM_THREADS     5

void *PrintHello(void *threadid)
{
   long tid;
   tid = (long)threadid;
   printf("Hello World! It's me, thread #%ld!\n", tid);
   pthread_exit(NULL);
}

int main (int argc, char *argv[])
{
   pthread_t threads[NUM_THREADS];
   int rc;
   long t;
   for(t = 0; t < NUM_THREADS; t++) {
      printf("In main: creating thread %ld\n", t);
      rc = pthread_create(&threads[t], NULL, PrintHello, (void *)t);
      if (rc) {
         printf("ERROR; return code from pthread_create() is %d\n", rc);
         exit(-1);
      }
   }

   /* Last thing that main() should do */
   pthread_exit(NULL);
}
```

Output:

```raw
In main: creating thread 0
In main: creating thread 1
Hello World! It's me, thread #0!
In main: creating thread 2
Hello World! It's me, thread #1!
Hello World! It's me, thread #2!
In main: creating thread 3
In main: creating thread 4
Hello World! It's me, thread #3!
Hello World! It's me, thread #4!
```
