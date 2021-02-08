---
layout: page
title: "Exercise 3"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

### Overview:

- Login to the LC workshop cluster, if you are not already logged in.
- Following the Exercise 3 instructions will take you through all sorts of MPI programs - pick any/all that are of interest.
- The intention is review the codes and see what's happening - not just compile and run.
- Several codes provide serial examples for a comparison with the parallel MPI versions.
- Check out the "bug" programs.

#### 1. Still logged into the workshop cluster?
If so, then continue to the next step. If not, then login as you did previously for Exercises 1 and 2.

#### 2. Review the array decomposition example code.

First, review the serial version of this example code, either [ser_array.c](examples/ser_array.c) or [ser_array.f](examples/ser_array.c).

After you understand what's going on, review the parallel MPI version, either [mpi_array.c](examples/mpi_array.c) or [mpi_array.f](examples/mpi_array.f). The comments explain how MPI is used to implement a parallel data decomposition on an array.

#### 3. Compile the parallel MPI and serial versions of the array decomposition example code
As with Exercises 1 & 2, use the compiler command of your choice to compile the mpi_array example code. 

Use the appropriate serial compiler command for the serial version. For example:

`C`:

```
icc ser_array.c  -o ser_array
```
```
mpicc mpi_array.c  -o mpi_array
```

`Fortran`:
```
ifort ser_array.f -o ser_array
```
```
mpif77 mpi_array.f -o mpi_array
```

#### 4. Run the executables interactively

For the MPI executable, use the special workshop pool and 8 tasks. For example:

`Serial`:	
```
ser_array
```
`MPI:`
```	
srun -n8 -ppReserved mpi_array
```

**Note**: The `srun` command is covered in detail in the "Starting Jobs" section of the Linux Clusters Overview tutorial, located at: computing.llnl.gov/tutorials/linux_clusters/index.html#Starting. There is also a man page.
