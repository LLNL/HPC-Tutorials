---
layout: default
title: "Passing Arguments to Threads"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

The `pthread_create()` routine permits the programmer to pass one argument to the thread start routine. For cases where multiple arguments must be passed, this limitation is easily overcome by creating a structure which contains all of the arguments, and then passing a pointer to that structure in the `pthread_create()` routine.

All arguments must be passed by reference and cast to (void *).

Question: How can you safely pass data to newly created threads, given their non-deterministic start-up and scheduling? 


<detail>
  <summary>Answer (Click to view.)</summary>
  
  *Make sure that all passed data is thread safe - that it can not be changed by other threads.  The three examples that follow demonstrate what not and what to do.*
  
</detail>

####  Example 1 - Thread Argument Passing

This code fragment demonstrates how to pass a simple integer to each thread. The calling thread uses a unique data structure for each thread, insuring that each thread's argument remains intact throughout the program.

```
long taskids[NUM_THREADS];

for(t=0; t<NUM_THREADS; t++)
{
   taskids[t] = t;
   printf("Creating thread %ld\n", t);
   rc = pthread_create(&threads[t], NULL, PrintHello, (void *) taskids[t]);
   ...
}
```

####  Example 2 - Thread Argument Passing

This example shows how to setup/pass multiple arguments via a structure. Each thread receives a unique instance of the structure.

```
struct thread_data{
   int  thread_id;
   int  sum;
   char *message;
};

struct thread_data thread_data_array[NUM_THREADS];

void *PrintHello(void *threadarg)
{
   struct thread_data *my_data;
   ...
   my_data = (struct thread_data *) threadarg;
   taskid = my_data->thread_id;
   sum = my_data->sum;
   hello_msg = my_data->message;
   ...
}

int main (int argc, char *argv[])
{
   ...
   thread_data_array[t].thread_id = t;
   thread_data_array[t].sum = sum;
   thread_data_array[t].message = messages[t];
   rc = pthread_create(&threads[t], NULL, PrintHello, 
        (void *) &thread_data_array[t]);
   ...
}
```

#### Example 3 - Thread Argument Passing (Incorrect)

This example performs argument passing incorrectly. It passes the address of variable `t`, which is shared memory space and visible to all threads. As the loop iterates, the value of this memory location changes, possibly before the created threads can access it.

```
int rc;
long t;

for(t=0; t<NUM_THREADS; t++) 
{
   printf("Creating thread %ld\n", t);
   rc = pthread_create(&threads[t], NULL, PrintHello, (void *) &t);
   ...
}
```
