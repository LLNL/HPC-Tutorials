---
layout: tutorial_page
title: "Exercise 3"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview:

* Login to the workshop cluster, if you are not already logged in

* Orphaned directive example: review, compile, run

* Get OpenMP implementation environment information

* Check out the "bug" programs

## 1. Still logged into the workshop cluster?
1. If so, then continue to the next step. If not, then login as you did previously for Exercise 1.

## 2. Review / Compile / Run the orphan example code

This example computes a dot product in parallel, however it differs from previous examples because the parallel loop construct is orphaned - it is contained in a subroutine outside the lexical extent of the main program's parallel region.

1. After reviewing the source code, compile and run the program. For example:

### C:	
```
icc -openmp omp_orphan.c -o orphan 
orphan | sort
```
### Fortran:	
```
ifort -openmp omp_orphan.f -o orphan
orphan | sort
```

2. Note the result...and the fact that this example will come back to haunt as `omp_bug6` later.

## 3. Get environment information

1. Starting from scratch, write a simple program that obtains information about your openMP environment. Alternately, you can modify the "hello" program to do this.

2. Using the appropriate openMP routines/functions, have the master thread query and print the following:
  * The number of processors available
  * The number of threads being used
  * The maximum number of threads available
  * If you are in a parallel region
  * If dynamic threads are enabled
  * If nested parallelism is supported
  
  NOTE: Some compilers (IBM, GNU) implement some of the necessary Fortran functions as integer instead of logical as the standard specifies.

3. If you need help, you can consult the [omp_getEnvInfo.c](code_examples/C/omp_getEnvInfo.c) or [omp_getEnvInfo.f](code_examples/Fortran/omp_getEnvInfo.f) example file.

## 4. When things go wrong...

There are many things that can go wrong when developing OpenMP programs. The `omp_bugX.X` series of programs demonstrate just a few. See if you can figure out what the problem is with each case and then fix it.

**Note:** Please use the Intel (icc, ifort) compile commands for these exercises.

The buggy behavior will differ for each example. Some hints are provided below.

<details>
  <summary> Hints (Click to expand!)</summary>
	

</details>

*If you're just finishing the tutorial and haven't filled out our [evaluation form](https://hpc.llnl.gov/training/tutorials/evaluation-form) yet, please do!*


	
