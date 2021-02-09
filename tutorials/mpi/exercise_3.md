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
mpicc mpi_array.c  -o mpi_array
```

`Fortran`:
```
ifort ser_array.f -o ser_array
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

#### 5. Compare other serial codes to their parallel version
If we had more time, you might even be able to start with a serial code or two and create your own parallel version. Feel free to try if you'd like.

#### 6. Try any/all of the other MPI example codes

- First, review the code(s) so that you understand how MPI is being used.
- Then, using the MPI compiler command(s) of your choice, compile the codes of interest.
- For convenience, the included Makefiles can be used to compile any or all of the exercise codes. For example:

`C:`
```	
make -f Makefile.MPI.c
make -f Makefile.MPI.c  mpi_mm
make -f Makefile.Ser.c 
```
`Fortran:`
```
make -f Makefile.MPI.f
make -f Makefile.MPI.f  mpi_mm
make -f Makefile.Ser.f 
```

**Note**: you can change the compiler being used by editing the Makefile.

- Run the executables interactively in the special workshop pool. Use the `srun` command for this as shown previously. 
Most of the executables only need 4 MPI tasks or less. Some exceptions and notes:

<table style="border-collapse:collapse;border-spacing:0" class="tg"><thead><tr><th style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Code</th><th style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Requirement(s)</th></tr></thead><tbody><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:bold">mpi_bandwidth</span></td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal">Requires an even number of tasks.</span></td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:bold">mpi_cartesian</span></td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Requires 16 MPI tasks</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:bold">mpi_group</span></td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Requires 8 MPI tasks</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:bold">mpi_heat2D </span><br><span style="font-weight:bold">mpi_wave</span></td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">These examples attempt to generate an Xwindows display for results. You will need to make sure that your Xwindows environment and software is setup correctly if you want to see the graphic. Ask the instructor if you have any questions.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:bold">mpi_latency</span></td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Requires only 2 MPI tasks that should be on DIFFERENT nodes</td></tr></tbody></table>


- Some things to try:
    * Different compilers
    * Experiment with compiler flags (see respective man pages).
    * Vary the number of tasks and nodes used.

