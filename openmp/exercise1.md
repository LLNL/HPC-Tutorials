---
layout: tutorial_page
title: "Exercise 1"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview

* Login to the workshop cluster using your workshop username and OTP token
* Copy the exercise files to your home directory
* Familiarize yourself with LC's OpenMP environment
* Write a simple "Hello World" OpenMP program
* Successfully compile your program
* Successfully run your program
* Modify the number of threads used to run your program

## 1. Login to the workshop machine

Workshops differ in how this is done. The instructor will go over this beforehand.

## 2. Copy the example files

In your home directory, create a subdirectory for the example codes and then cd to it.

```
mkdir openMP
cd  openMP 
```

Then, copy either the Fortran or the C version of the parallel OpenMP exercise files to your openMP subdirectory:

**C:**
```	
cp  /usr/global/docs/training/blaise/openMP/C/*  ~/openMP
```
**Fortran:**
```
cp  /usr/global/docs/training/blaise/openMP/Fortran/*  ~/openMP
```

## 3. List the contents of your OpenMP subdirectory

You should notice the following files. ***Note:*** Most of these are simple example files. Their primary purpose is to demonstrate the basics of how to parallelize a code with OpenMP. Most execute in a second or two.

# TABLE
Test: [omp_bug1.f](code_examples/Fortran/omp_bug1.f)

## 4. Compilers - What's Available?

Visit the [Compilers at LC webpage](https://hpc.llnl.gov/software/development-environment-software/compilers).

You can also view the available compilers in the [Compilers section](https://hpc.llnl.gov/training/tutorials/livermore-computing-linux-commodity-clusters-overview-part-one#Compilers) of the Linux Clusters Overview tutorial.

Now, in your cluster login window, try the `use -l compilers` command to display available compilers. You should see GNU, Intel and PGI compilers - several versions of each.
	* Question: Which version is the default version?
	* Answer: Use the `dpkg-defaults` command and look for the asterisk.

## 5. Create, compile and run an OpenMP "Hello world" program
Using your favorite text editor (vi/vim, emacs, nedit, gedit, nano...) open a new file - call it whatever you'd like.

Create a simple OpenMP program that does the following:
* Creates a parallel region
* Has each thread in the parallel region obtain its thread id
* Has each thread print "Hello World" along with its unique thread id
* Has the master thread only, obtain and then print the total number of threads
* If you need help, see the provided []() or []() file.

Using your choice of compiler (see above section 4), compile your hello world OpenMP program. This may take several attempts if there are any code errors. For example:

# TABLE

When you get a clean compile, proceed.

Run your hello executable and notice its output.
	* Is it what you expected? As a comparison, you can compile and run the provided omp_hello.c or omp_hello.f example program.
	* How many threads were created? By default, the Intel and GNU compilers will create 1 thread for each core. The PGI compiler will create only 1 thread total.
Notes:
	* For the remainder of this exercise, you can use the compiler command of your choice unless indicated otherwise.
	* Compilers will differ in which warnings they issue, but all can be ignored for this exercise. Errors are different, of course.

## 6. Vary the number of threads and re-run Hello World

Explicitly set the number of threads to use by means of the `OMP_NUM_THREADS` environment variable:

```
setenv OMP_NUM_THREADS 8
```

Your output should look something like below.

```
Hello World from thread = 0
Hello World from thread = 3
Hello World from thread = 2
Number of threads = 8
Hello World from thread = 6
Hello World from thread = 1
Hello World from thread = 4
Hello World from thread = 7
Hello World from thread = 5
```

Run your program several times and observe the order of print statements. Notice that the order of output is more or less random.



