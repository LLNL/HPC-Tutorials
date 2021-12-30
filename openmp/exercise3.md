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

1. This example computes a dot product in parallel, however it differs from previous examples because the parallel loop construct is orphaned - it is contained in a subroutine outside the lexical extent of the main program's parallel region.

2. After reviewing the source code, compile and run the program. For example:

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

Note the result...and the fact that this example will come back to haunt as `omp_bug6` later.

## 3. Get environment information

Starting from scratch, write a simple program that obtains information about your openMP environment. Alternately, you can modify the "hello" program to do this.
Using the appropriate openMP routines/functions, have the master thread query and print the following:
The number of processors available
The number of threads being used
The maximum number of threads available
If you are in a parallel region
If dynamic threads are enabled
If nested parallelism is supported
NOTE: Some compilers (IBM, GNU) implement some of the necessary Fortran functions as integer instead of logical as the standard specifies.

If you need help, you can consult the  or  example file.

## 4. When things go wrong...
There are many things that can go wrong when developing OpenMP programs. The omp_bugX.X series of programs demonstrate just a few. See if you can figure out what the problem is with each case and then fix it.

The buggy behavior will differ for each example. Some hints are provided below.

Note: Please use the Intel (icc, ifort) compile commands for these exercises.

Code	Behavior	Hints/Notes
omp_bug1 
omp_bug1fix	Fails compilation. Solution provided - must compile solution file.	
omp_bug2	Thread identifiers are wrong. Wrong answers.	
omp_bug3	Run-time error, hang.	
omp_bug4 
omp_bug4fix	Causes a segmentation fault. Solution provided - note that it is a script and will need to be "sourced". For example: "source omp_bug4fix". Be sure to examine the solution file to see what's going on - especially the last line, where you may need to change the name of the executable to match yours.	
omp_bug5 
omp_bug5fix	Program hangs. Solution provided - must compile solution file.	
omp_bug6	Failed compilation	
