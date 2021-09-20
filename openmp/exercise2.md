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
#### C:
```
    icc -openmp omp_workshare1.c -o workshare1 
    workshare1 | sort
```
#### Fortran:
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

## 3. Review / Compile / Run the matrix multiply example code

This example performs a matrix multiple by distributing the iterations of the operation between available threads.

1. After reviewing the source code, compile and run the program. For example:
#### C:	
```
	icc -openmp omp_mm.c -o matmult 
	matmult
```
#### Fortran:
```
	ifort -openmp omp_mm.f -o matmult
	matmult
```

2. Review the output. It shows which thread did each iteration and the final result matrix.

3. Run the program again, however this time sort the output to clearly see which threads execute which iterations:
```
matmult | sort | grep Thread
```
Do the loop iterations match the SCHEDULE(STATIC,CHUNK) directive for the matrix multiple loop in the code?

## 4. Review / Compile / Run the workshare2 example code

This example demonstrates use of the OpenMP SECTIONS work-sharing construct Note how the PARALLEL region is divided into separate sections, each of which will be executed by one thread.

1. As before, compile and execute the program after reviewing it. For example:
#### C:	
```
    icc -openmp omp_workshare2.c -o workshare2 
    workshare2
```
#### Fortran:
```
    ifort -openmp omp_workshare2.f -o workshare2
    workshare2
```

2. Run the program several times and observe any differences in output. Because there are only two sections, you should notice that some threads do not do any work. You may/may not notice that the threads doing work can vary. For example, the first time thread 0 and thread 1 may do the work, and the next time it may be thread 0 and thread 3. It is even possible for one thread to do all of the work. Which thread does work is non-deterministic in this case.


