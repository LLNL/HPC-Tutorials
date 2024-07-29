---
layout: tutorial_page
title: "LLNL MPI Implementations and Compilers"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

## Multiple Implementations

Although the MPI programming interface has been standardized, actual library implementations will differ.

For example, just a few considerations of many:

- Which version of the MPI standard is supported?
- Are all of the features in a particular MPI version supported?
- Have any new features been added?
- What network interfaces are supported?
- How are MPI applications compiled?
- How are MPI jobs launched?
- Runtime environment variable controls?

MPI library implementations on LC systems vary, as do the compilers they are built for. These are summarized in the table below:

<table class="table table-bordered table-striped">
<tbody>
<tr valign="top">
    <th>MPI Library</th>
    <th>Where?</th>
    <th>Compilers</th>
</tr>
<tr valign="top">
    <td><b>MVAPICH</b></td>
    <td>Linux clusters</td>
    <td>GNU, Intel, Clang</td>
</tr>
<tr valign="top">
    <td><b>Open MPI</b></td>
    <td>Linux clusters</td>
    <td>GNU, Intel, Clang</td>
</tr>
    <tr valign="top">
    <td><b>Intel MPI</b></td>
    <td>Linux clusters</td>
    <td>Intel, GNU</td>
</tr>
    <tr valign="top">
    <td><b>IBM Spectrum MPI</b></td>
    <td>Coral Early Access and Sierra clusters</td>
    <td>IBM, GNU, PGI, Clang</td>
</tr>
</tbody>
</table>

Each MPI library is briefly discussed in the following sections, including links to additional detailed information.

## Selecting Your MPI Library and Compiler

LC provides a default MPI library for each cluster.

LC also provides default compilers for each cluster.

Typically, there are multiple versions of MPI libraries and compilers on each cluster.

Modules are used to select a specific MPI library or compiler: More info [HERE](https://hpc.llnl.gov/software/modules-and-software-packaging).

For example, using modules (June 2024):

- List which modules are currently loaded
- Show all available modules
- Load a different MPI module
- Load a different compiler module
- Confirm newly loaded modules

```sh
### List currently loaded modules
% module list

Currently Loaded Modules:
  1) intel-classic/2021.6.0-magic   3) jobutils/1.0       5) StdEnv (S)
  2) mvapich2/2.3.7                 4) texlive/20220321

module avail mvapich

----- /usr/tce/modulefiles/MPI/intel-classic/2021.6.0-magic/mvapich2/2.3.7 -----
   mvapich2-tce/2.3.7

---------- /usr/tce/modulefiles/Compiler/intel-classic/2021.6.0-magic ----------
   mvapich2/2.3.7 (L)

module avail openmpi

---------- /usr/tce/modulefiles/Compiler/intel-classic/2021.6.0-magic ----------
   openmpi/4.1.2



### Load a different MPI module
% mmodule load openmpi

Lmod is automatically replacing "mvapich2/2.3.7" with "openmpi/4.1.2".

module list

Currently Loaded Modules:
  1) intel-classic/2021.6.0-magic   3) texlive/20220321       5) openmpi/4.1.2
  2) jobutils/1.0                   4) StdEnv           (S)

  Where:
   S:  Module is Sticky, requires --force to unload or purge

```

## MVAPICH

### General Info

MVAPICH MPI is developed and supported by the Network-Based Computing Lab at Ohio State University.

Available on all of LC's Linux clusters.

MVAPICH

- Default MPI implementation
- Multiple versions available
- MPI-2 and MPI-3 implementations based on MPICH MPI library from Argonne National Laboratory. Versions 1.9 and later implement MPI-3 according to the developer's documentation. Version 2.3.7 - integrated and ABI compatible with MPICH-3.2.1. Complete user information available at:
  https://mvapich.cse.ohio-state.edu/static/media/mvapich/mvapich2-userguide.html
- Thread-safe

To see what versions are available, and/or to select an alternate version, use Modules commands. For example:

```sh
module avail mvapich         (list available modules)
module load mvapich2/2.3.7     (use the module of interest)
```
This ensures that LC's MPI wrapper scripts point to the desired version of MVAPICH.

### Compiling

See the MPI Build Scripts table below.

### Running

MPI executables are launched using the SLURM `srun` command with the appropriate options. For example, to launch an 8-process MPI job split across two different nodes in the `pdebug` pool:

```sh
srun -N2 -n8 -ppdebug a.out
```

The `srun` command is discussed in detail in the Running Jobs section of the Linux Clusters Overview tutorial.

### Documentation

- MVAPICH home page: mvapich.cse.ohio-state.edu/
- MVAPICH2 User Guides: http://mvapich.cse.ohio-state.edu/userguide/
- MVAPICH 2.3.7 User Guide - https://mvapich.cse.ohio-state.edu/static/media/mvapich/mvapich2-userguide.html

## Open MPI

### General Information

Open MPI is a thread-safe, open source MPI implementation developed and supported by a consortium of academic, research, and industry partners.

Available on all LC Linux clusters. However, you'll need to load the desired [module](https://hpc.llnl.gov/training/tutorials/livermore-computing-resources-and-environment#modules) first. For example:

```sh
module avail                 (list available modules)
module load openmpi/4.1.2    (use the module of interest)
```

This ensures that LC's MPI wrapper scripts point to the desired version of Open MPI.

### Compiling

See the MPI Build Scripts table below.

### Running

Be sure to load the same Open MPI module that you used to build your executable. If you are running a batch job, you will need to load the module in your batch script.

Launching an Open MPI job can be done using the following commands. For example, to run a 48 process MPI job:

```sh
mpirun -np 48 a.out
mpiexec -np 48 a.out
srun -n 48 a.out
```

### Documentation

Open MPI home page: http://www.open-mpi.org/

## Intel MPI

- Available on LC's Linux clusters.
- Based on MPICH3. Supports MPI-3 functionality.
- Thread-safe
- Compiling and running Intel MPI programs: see the LC documentation at: https://lc.llnl.gov/confluence/pages/viewpage.action?pageId=137725526

## CORAL 1 Early Access and Sierra Clusters:

- The IBM Spectrum MPI library is the only supported implementation on these clusters.
- Based on Open MPI. Includes MPI-3 functionality.
- Thread-safe
- NVIDIA GPU support
- Compiling and running IBM Spectrum MPI programs: see the Sierra Tutorial at https://hpc.llnl.gov/training/tutorials/using-lcs-sierra-system

## CORAL 2 Early Access and El Capitan Clusters:

### MPI Build Scripts

LC developed MPI compiler wrapper scripts are used to compile MPI programs on all LC systems.

Automatically perform some error checks, include the appropriate MPI #include files, link to the necessary MPI libraries, and pass options to the underlying compiler.

- Note: you may need to load a module for the desired MPI implementation, as discussed previously. Failing to do this will result in getting the default implementation.

The table below lists the primary MPI compiler wrapper scripts for LC's Linux clusters. For CORAL EA / Sierra systems, see the links provided above.

<table border="1" cellspacing="0" cellpadding="3">
<tbody>
<tr>
    <th colspan="4">MPI Build Scripts - Linux Clusters</th>
</tr>
<tr>
    <th>Implementation</th>
    <th>Language</th>
    <th>Script Name</th>
    <th>Underlying Compiler</th>
</tr>
<tr>
    <td rowspan="5"><b>MVAPICH2</b></td>
    <td><b>C</b></td>
    <td><tt><b>mpicc</b></tt></td>
    <td>C compiler for loaded compiler package</td>
</tr>
<tr>
    <td><b>C++</b></td>
    <td><tt><b>mpicxx
    <br>mpic++</b></tt></td>
    <td>C++ compiler for loaded compiler package</td>
</tr>
<tr>
    <td rowspan="3"><b>Fortran</b></td>
    <td><tt><b>mpif77</b></tt></td>
    <td>Fortran77 compiler for loaded compiler package. Points to mpifort.</td>
</tr>
<tr>
    <td><tt><b>mpif90</b></tt></td>
    <td>Fortran90 compiler for loaded compiler package. Points to mpifort.</td>
</tr>
<tr>
    <td><tt><b>mpifort</b></tt></td>
    <td>Fortran 77/90 compiler for loaded compiler package.</td>
</tr>
<tr>
    <td rowspan="5"><b>Open MPI</b></td>
    <td><b>C</b></td>
    <td><tt><b>mpicc</b></tt></td>
    <td>C compiler for loaded compiler package</td>
</tr>
<tr>
    <td><b>C++</b></td>
    <td><tt><b>mpiCC<br>mpic++<br>mpicxx</b></tt></td>
    <td>C++ compiler for loaded compiler package</td>
</tr>
<tr>
    <td rowspan="3"><b>Fortran</b></td>
    <td><tt><b>mpif77</b></tt></td>
    <td>Fortran77 compiler for loaded compiler package. Points to mpifort.</td>
</tr>
<tr>
    <td><tt><b>mpif90</b></tt></td>
    <td>Fortran90 compiler for loaded compiler package. Points to mpifort.</td>
</tr>
<tr>
    <td><tt><b>mpifort</b></tt></td>
    <td>Fortran 77/90 compiler for loaded compiler package.</td>
</tr>
</tbody>
</table>

For additional information:

- See the man page (if it exists)
- Issue the script name with the -help option
- View the script yourself directly

### Level of Thread Support

MPI libraries vary in their level of thread support:

- **MPI_THREAD_SINGLE** - Level 0: Only one thread will execute.
- **MPI_THREAD_FUNNELED** - Level 1: The process may be multi-threaded, but only the main thread will make MPI calls - all MPI calls are funneled to the main thread.
- **MPI_THREAD_SERIALIZED** - Level 2: The process may be multi-threaded, and multiple threads may make MPI calls, but only one at a time. That is, calls are not made concurrently from two distinct threads as all MPI calls are serialized.
- **MPI_THREAD_MULTIPLE** - Level 3: Multiple threads may call MPI with no restrictions.

Consult the [MPI_Init_thread() man page](MPI_appendix/MPI_Init_thread.txt) for details.

A simple C language example for determining thread level support is shown below.

```c
#include "mpi.h"
#include <stdio.h>

int main( int argc, char *argv[] )
{
    int provided, claimed;

/*** Select one of the following
    MPI_Init_thread( 0, 0, MPI_THREAD_SINGLE, &provided );
    MPI_Init_thread( 0, 0, MPI_THREAD_FUNNELED, &provided );
    MPI_Init_thread( 0, 0, MPI_THREAD_SERIALIZED, &provided );
    MPI_Init_thread( 0, 0, MPI_THREAD_MULTIPLE, &provided );
***/

    MPI_Init_thread(0, 0, MPI_THREAD_MULTIPLE, &provided );
    MPI_Query_thread( &claimed );
        printf( "Query thread level= %d  Init_thread level= %d\n", claimed, provided );

    MPI_Finalize();
}
```

```sh
# Sample output:
Query thread level= 3  Init_thread level= 3
```
