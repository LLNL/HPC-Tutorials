---
layout: page
title: "Exercise 1"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

### Overview

* Login to an LC cluster using your workshop username and OTP token
* Copy the exercise files to your home directory
* Familiarize yourself with LC's MPI compilers
* Write a simple "Hello World" MPI program using several MPI Environment Management routines
* Successfully compile your program
* Successfully run your program - several different ways

### 1. Login to the workshop machine
Workshops differ in how this is done. The instructor will go over this beforehand.

### 2. Copy the example files

In your home directory, create a subdirectory for the MPI test codes and cd to it.

```
mkdir ~/mpi
cd  ~/mpi
```

Copy either the Fortran or the C version of the parallel MPI exercise files to your mpi subdirectory:


**C:**
```
cp  /usr/global/docs/training/blaise/mpi/C/*   ~/mpi
```

**Fortran:**
```	
cp  /usr/global/docs/training/blaise/mpi/Fortran/*   ~/mpi
```

Some of the example codes have serial versions for comparison. Use the appropriate command below to copy those files to your mpi subdirectory also.

**C:**
```
cp  /usr/global/docs/training/blaise/mpi/Serial/C/*   ~/mpi
```
**Fortran:**	
```
cp  /usr/global/docs/training/blaise/mpi/Serial/Fortran/*   ~/mpi 
```

### 3. List the contents of your MPI subdirectory

You should notice quite a few files. The parallel MPI versions have names which begin with or include `mpi_`. The serial versions have names which begin with or include `ser_`. Makefiles are also included.

Note: These are example files, and as such, are intended to demonstrate the basics of how to parallelize a code using MPI. Most execute in just a second or two.

# Include table here

### 4. MPI Libraries and Compilers - What's Available?

Recall from the [LLNL MPI Implementations and Compilers](implementations.md) section of the MPI tutorial, that LC has three different MPI libraries on its Linux clusters: **MVAPICH**, **Open MPI** and **Intel MPI**. There are multiple versions for each.

The default MPI library on LC's TOSS3 Linux clusters is MVAPICH 2.

To view available MPI libraries, try the following commands:

```
module avail mvapich
module avail openmpi
module avail impi
```

Additionally, there are multiple compilers (and versions). Try the following commands to view them:

```
module avail intel
module avail gcc
module avail pgi
module avail clang
```

*Moral of the Story*: if you want to use a specific version of an MPI library and/or compiler other than the default, you will need to load the selected package.

### 5. Create, compile and run an MPI "Hello world" program

**Create:** Using your favorite text editor (vi/vim, emacs, nedit, gedit, nano...) open a new file - call it whatever you'd like. 
It should do the following:
* Use the appropriate MPI include file
* Identify task 0 as the "master" task
* Initialize the MPI environment
* Get the total number of tasks
* Get the task's rank (who it is)
* Get the name of the processor it is executing on
* Print a hello message that includes its task rank and processor name
* Have the master task alone print the total number of tasks
* Terminate the MPI environment

If you need help, see the provided example files [mpi_hello.c](examples/mpi_hello.c) or [mpi_hello.f](examples/mpi_hello.f)

**Compile:** Use a C or Fortran MPI compiler command. For example:

```
mpicc -w -o hello myhello.c
mpif77 -w -o hello myhello.f
mpif90 -w -o hello myhello.f
```

`myhello.c`, `myhello.f`  represent your source file - use your actual source file name 
The `-o` compiler flag specifies the name for your executable 
The `-w` compiler flag is simply being used to suppress annoying warning messages.

When you get a clean compile, proceed.

**Run:** Use the `srun` command to run your MPI executable. Be sure to use the `pReserved` partition with 8 tasks on two different nodes. For example:
```
srun -N2 -n8 -ppReserved hello
```

You may see a message like below while the job gets ready to run:

```
srun: Job is in held state, pending scheduler release
srun: job 1139098 queued and waiting for resources
```

Did your job run successfully? Based on the output, did it behave as expected? If not, figure out any problems and fix them before proceeding.

Run your program a few more times, but vary the number of nodes and total tasks. Observe the task output statements to confirm.
