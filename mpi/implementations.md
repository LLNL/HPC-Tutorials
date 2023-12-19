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
    <td>GNU, Intel, PGI, Clang</td>
</tr>
<tr valign="top">
    <td><b>Open MPI</b></td>
    <td>Linux clusters</td>
    <td>GNU, Intel, PGI, Clang</td>
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

For example, using modules:

- List which modules are currently loaded
- Show all available modules
- Load a different MPI module
- Load a different compiler module
- Confirm newly loaded modules

```sh
### List currently loaded modules
% module list

Currently Loaded Modules:
  1) intel/18.0.1   2) mvapich2/2.2   3) texlive/2016   4) StdEnv

### Show all available modules
% module avail

------------------------ /usr/tce/modulefiles/MPI/mvapich2/2.2 -------------------------
   openss/2.3

------------------ /usr/tce/modulefiles/MPI/intel/18.0.1/mvapich2/2.2 ------------------
   MUST/1.5        fftw/3.3.7              mpip/3.4.1
   boost/1.66.0    hdf5-parallel/1.8.18    pnetcdf/1.9.0

---------------------- /usr/tce/modulefiles/Compiler/intel/18.0.1 ----------------------
   hdf5-serial/1.8.18     impi/2018.0     (D)      netcdf-fortran/4.4.4
   hpctoolkit/10102016    mvapich2/2.2    (L,D)    openmpi/2.0.0        (D)
   impi/5.1.3             mvapich2/2.3             openmpi/2.1.0
   impi/2017.0            ncl_ncarg/6.3.0          openmpi/3.0.1
   impi/2017.1            netcdf/4.4.1.1

------------------------------ /usr/tce/modulefiles/Core -------------------------------
   StdEnv             (L)    inspector/2018.0    (D)      pgi/16.3
   advisor/2016.4            intel/14.0.3                 pgi/16.7
   advisor/2017.1            intel/15.0.6                 pgi/16.9
   advisor/2017.2            intel/16.0.2                 pgi/17.10
   advisor/2018.0            intel/16.0.3                 pgi/18.1              (D)
   advisor/2018.1     (D)    intel/16.0.4                 python/2.7.11
   allineaforge/6.0.5        intel/17.0.0                 python/2.7.13
   allineaforge/6.1.1        intel/17.0.2                 python/2.7.14         (D)
   allineaforge/7.0.3 (D)    intel/18.0-beta              python/3.5.1
   atom/1.13.1               intel/18.0.0                 python/3.6.0
   cbflib/0.9.2              intel/18.0.1        (L,D)    python/3.6.4
   clang/3.9.0               intel/18.0.2                 rasmol/2.7.5.2
   clang/3.9.1               intel/19.0-beta              spindle/0.10
   clang/4.0.0        (D)    iorun                        sqlcipher/3.7.9
   cmake/3.5.2        (D)    itac/2017.1                  stat/2.2.0
   cmake/3.8.2               itac/2018.0         (D)      stat/3.0.0            (D)
   cmake/3.9.2               launchmon/1.0.2              sublime_text/3.1.1
   cqrlib/1.0.5              ld-auto-rpath                svn/1.6.23
   cvector/1.0.3             license-llnl-ocf             svn/1.7.14            (D)
   dyninst/9.1.0             make/4.2.1                   tclap/1.2.0
   dyninst/9.3.1      (D)    mathematica/10.3.1           tecplot/2016.1
   emacs/24.3-redhat         matlab/8.1                   texlive/2016          (L)
   emacs/25.3         (D)    memcheckview/3.11.0          totalview/2016.01.06
   ensight/10.1.6            memcheckview/3.12.0          totalview/2016.04.08
   fgfs/1.1                  memcheckview/3.13.0 (D)      totalview/2016.06.21
   gcc/4.8-redhat            mesa3d/17.0.5                totalview/2016.07.22
   gcc/4.9.3          (D)    mkl/11.3.3                   totalview/2017X.01.07
   gcc/6.1.0                 mkl/2017.1                   totalview/2017.1.21
   gcc/7.1.0                 mkl/2018.0          (D)      totalview/2017.3.8    (D)
   gdal/1.9.0                mpa/1.1                      valgrind/3.11.0
   git/1.8.3.1               mpifileutils/0.6             valgrind/3.12.0
   git/2.8.3          (D)    mpifileutils/0.7    (D)      valgrind/3.13.0       (D)
   git-lfs/1.4.1             mrnet/5.0.1                  vampir/9.1.0
   glxgears/1.2              neartree/5.1.1               vampir/9.2            (D)
   gmt/5.1.2                 nvidia/375                   vmd/1.9.3
   gnuplot/5.0.0             opt                          vtune/2016.3
   grace/5.1.25              papi/5.4.3                   vtune/2017.1
   graphlib/2.0.0            paraview/5.0                 vtune/2018.0          (D)
   graphlib/3.0.0     (D)    paraview/5.4-server          xalt
   idl/8.5                   paraview/5.4        (D)      xforms/1.0.91
   inspector/2016.3          patchelf/0.8                 xsu
   inspector/2017.0          perfreports/7.0.3

-------------------------------- /usr/apps/modulefiles ---------------------------------
   Spheral/exp        Spheral/old       pact/current        pact/new
   Spheral/new (D)    pact/current_s    pact/new_s   (D)

------------------------ /usr/share/lmod/lmod/modulefiles/Core -------------------------
   lmod/6.5.1    settarg/6.5.1

  Where:
   L:  Module is loaded
   D:  Default Module

Use "module spider" to find all possible modules.
Use "module keyword key1 key2 ..." to search for all possible modules matching any of
the "keys".

### Load a different MPI module
% module load openmpi/3.0.1

Lmod is automatically replacing "mvapich2/2.2" with "openmpi/3.0.1"

### Load a different compiler module
% module load pgi/18.1

Lmod is automatically replacing "intel/18.0.1" with "pgi/18.1"

Due to MODULEPATH changes the following have been reloaded:
  1) openmpi/3.0.1

### Confirm newly loaded modules
% module list

Currently Loaded Modules:
  1) texlive/2016   2) StdEnv   3) pgi/18.1   4) openmpi/3.0.1
```

## MVAPICH

### General Info

MVAPICH MPI is developed and supported by the Network-Based Computing Lab at Ohio State University.

Available on all of LC's Linux clusters.

MVAPICH

- Default MPI implementation
- Multiple versions available
- MPI-2 and MPI-3 implementations based on MPICH MPI library from Argonne National Laboratory. Versions 1.9 and later implement MPI-3 according to the developer's documentation.
- Thread-safe

To see what versions are available, and/or to select an alternate version, use Modules commands. For example:

```sh
module avail mvapich         (list available modules)
module load mvapich2/2.3     (use the module of interest)
```

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

## CORAL Early Access and Sierra Clusters:

- The IBM Spectrum MPI library is the only supported implementation on these clusters.
- Based on Open MPI. Includes MPI-3 functionality.
- Thread-safe
- NVIDIA GPU support
- Compiling and running IBM Spectrum MPI programs: see the Sierra Tutorial at https://hpc.llnl.gov/training/tutorials/using-lcs-sierra-system

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
