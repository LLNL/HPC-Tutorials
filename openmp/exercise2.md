---
layout: tutorial_page
title: "Exercise 2"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview:

* Login to the LC workshop cluster, if you are not already logged in.
* Work-Sharing DO/for construct examples: review, compile and run.
* Work-Sharing SECTIONS construct example: review, compile and run.

## 1. Still logged into the workshop cluster?

If so, then continue to the next step. If not, then login as you did previously for Exercise 1.

## 2. Review / Compile / Run the `workshare1` example code

This example demonstrates use of the OpenMP loop work-sharing construct. Notice that it specifies dynamic scheduling of threads and assigns a specific number of iterations to be done by each thread.

1. First, set the number of threads to 4:
```
setenv OMP_NUM_THREADS 4
```
2. After reviewing the source code, use your preferred compiler to compile and run the executable. For example:

**C:**
```
    icc -openmp omp_workshare1.c -o workshare1 
    workshare1 | sort
```

**Fortran:**
```
    ifort -openmp omp_workshare1.f -o workshare1
    workshare1 | sort
```

3. Review the output. Note that it is piped through the sort utility. This will make it easier to view how loop iterations were actually scheduled across the team of threads.

4. Run the program a couple more times and review the output. What do you see? Typically, dynamic scheduling is not deterministic. Everytime you run the program, different threads can run different chunks of work. It is even possible that a thread might not do any work because another thread is quicker and takes more work. In fact, it might be possible for one thread to do all of the work.

5. Edit the `workshare1.*` source file and change the dynamic scheduling to static scheduling.

6. Recompile and run the modified program. Notice the difference in output compared to dynamic scheduling. Specifically, notice that thread 0 gets the first chunk, thread 1 the second chunk, and so on.

7. Run the program a couple more times. Does the output change? With static scheduling, the allocation of work is deterministic and should not change between runs, and every thread gets work to do.

8. Reflect on possible performance differences between dynamic and static scheduling.

## 3.

## 4. 