---
layout: tutorial_page
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

{% raw %}

<table class="table table-striped table-bordered">
        <thead><tr><th>C Files</th><th>Fortran Files</th><th>Description</th></tr></thead>
        <tbody><tr><td><a href="/mpi/examples/mpi_hello.c">mpi_hello.c</a></td><td><a href="/mpi/examples/mpi_hello.f">mpi_hello.f</a></td><td>Hello World</td></tr>
        <tr><td ><a href="/mpi/examples/mpi_helloBsend.c">mpi_helloBsend.c</a></td><td><a href="/mpi/examples/mpi_helloBsend.f">mpi_helloBsend.f</a></td><td><a href="/mpi/examples/..."></a>Hello World modified to include blocking send/receive routines</td></tr>
        <tr><td ><a href="/mpi/examples/mpi_helloNBsend.c">mpi_helloNBsend.c</a></td><td ><a href="/mpi/examples/mpi_helloNBsend.f">mpi_helloNBsend.f</a></td><td>Hello World modified to include nonblocking send/receive routines</td></tr>
        <tr><td><a href="/mpi/examples/mpi_array.c">mpi_array.c</a><br><a href="/mpi/examples/ser_array.c">ser_array.c</a></td><td><a href="/mpi/examples/mpi_array.f">mpi_array.f</a><br><a href="/mpi/examples/ser_array.f">ser_array.f</a></td><td>Array Decomposition</td></tr>
        <tr><td ><a href="/mpi/examples/mpi_mm.c">mpi_mm.c</a><br><a href="/mpi/examples/ser_mm.c">ser_mm.c</a></td><td ><a href="/mpi/examples/mpi_mm.f">mpi_mm.f</a><br><a href="/mpi/examples/ser_mm.f">ser_mm.f</a></td><td>Matrix Multiply</td></tr>
        <tr><td><a href="/mpi/examples/mpi_pi_send.c">mpi_pi_send.c</a><br><a href="/mpi/examples/ser_pi_calc.c">ser_pi_calc.c</a></td><td><a href="/mpi/examples/mpi_pi_send.f">mpi_pi_send.f</a><br><a href="/mpi/examples/ser_pi_calc.f">ser_pi_calc.f</a></td><td><a href="/mpi/examples/..."></a>pi Calculation - point-to-point communications</td></tr>
        <tr><td><a href="/mpi/examples/mpi_pi_reduce.c">mpi_pi_reduce.c</a><br><a href="/mpi/examples/ser_pi_calc.c">ser_pi_calc.c</a></td><td ><a href="/mpi/examples/mpi_pi_reduce.f">mpi_pi_reduce.f</a><br><a href="/mpi/examples/ser_pi_calc.f">ser_pi_calc.f</a></td><td>pi Calculation - collective communications</td></tr>
        <tr><td><a href="/mpi/examples/mpi_wave.c">mpi_wave.c</a><br><a href="/mpi/examples/draw_wave.c">draw_wave.c</a><br><a href="/mpi/examples/ser_wave.c">ser_wave.c</a></td><td ><a href="/mpi/examples/mpi_wave.f">mpi_wave.f</a><br><a href="/mpi/examples/mpi_wave.h">mpi_wave.h</a><br><a href="/mpi/examples/draw_wavef.c">draw_wavef.c</a><br><a href="/mpi/examples/ser_wave.f">ser_wave.f</a></td><td>Concurrent Wave Equation</td></tr>
        <tr><td><a href="/mpi/examples/mpi_heat2D.c">mpi_heat2D.c</a><br><a href="/mpi/examples/draw_heat.c">draw_heat.c</a><br><a href="/mpi/examples/ser_heat2D.c">ser_heat2D.c</a></td><td><a href="/mpi/examples/mpi_heat2D.f">mpi_heat2D.f</a><br><a href="/mpi/examples/mpi_heat2D.h">mpi_heat2D.h</a><br><a href="/mpi/examples/draw_heatf.c">draw_heatf.c</a><br><a href="/mpi/examples/ser_heat2D.f">ser_heat2D.f</a></td><td>2D Heat Equation</td></tr>
        <tr><td><a href="/mpi/examples/mpi_latency.c">mpi_latency.c</a></td><td ><a href="/mpi/examples/mpi_latency.f">mpi_latency.f</a></td><td>Round Trip Latency Timing Test</td></tr>
        <tr><td><a href="/mpi/examples/mpi_bandwidth.c">mpi_bandwidth.c</a><br><a href="/mpi/examples/mpi_bandwidth_nonblock.c">mpi_bandwidth_nonblock.c</a></td><td ><a href="/mpi/examples/mpi_bandwidth.f">mpi_bandwidth.f</a><br><a href="/mpi/examples/mpi_bandwidth_nonblock.f">mpi_bandwidth_nonblock.f</a></td><td>Bandwidth Timing Tests</td></tr>
        <tr><td><a href="/mpi/examples/mpi_prime.c">mpi_prime.c</a><br><a href="/mpi/examples/ser_prime.c">ser_prime.c</a></td><td><a href="/mpi/examples/mpi_prime.f">mpi_prime.f</a><br><a href="/mpi/examples/ser_prime.f">ser_prime.f</a></td><td>Prime Number Generation</td></tr>
        <tr><td><a href="/mpi/examples/mpi_ping.c">mpi_ping.c</a><br><a href="/mpi/examples/mpi_ringtopo.c">mpi_ringtopo.c</a><br><a href="/mpi/examples/mpi_scatter.c">mpi_scatter.c</a><br><a href="/mpi/examples/mpi_contig.c">mpi_contig.c</a><br><a href="/mpi/examples/mpi_vector.c">mpi_vector.c</a><br><a href="/mpi/examples/mpi_struct.c">mpi_struct.c</a><br><a href="/mpi/examples/mpi_group.c">mpi_group.c</a><br><a href="/mpi/examples/mpi_cartesian.c">mpi_cartesian.c</a></td><td><a href="/mpi/examples/mpi_ping.f">mpi_ping.f</a><br>
        <a href="/mpi/examples/mpi_ringtopo.f">mpi_ringtopo.f</a><br>
        <a href="/mpi/examples/mpi_scatter.f">mpi_scatter.f</a><br>
        <a href="/mpi/examples/mpi_contig.f">mpi_contig.f</a><br>        
        <a href="/mpi/examples/mpi_vector.f">mpi_vector.f</a><br>
        <a href="/mpi/examples/mpi_indexed.f">mpi_indexed.f</a><br>
        <a href="/mpi/examples/mpi_struct.f">mpi_struct.f</a><br>
        <a href="/mpi/examples/mpi_group.f">mpi_group.f</a><br>
        <a href="/mpi/examples/mpi_cartesian.f">mpi_cartesian.f</a></td>
      <td>From the tutorial...<br>Non-blocking send-receive<br>Collective communications<br>Contiguous derived datatype<br>Vector derived datatype<br>Indexed derived datatype <br>Structure derived datatype<br>Groups/Communicators<br>CartesianVirtual Topology</td></tr>
      <tr>
        <td><a href="/mpi/examples/Makefile.MPI.c">Makefile.MPI.c</a><br>
        <a href="/mpi/examples/Makefile.Ser.c">Makefile.Ser.c</a></td>
        <td><a href="/mpi/examples/Makefile.MPI.f">Makefile.MPI.f</a><br>
        <a href="/mpi/examples/Makefile.Ser.f">Makefile.Ser.f</a></td>
        <td><a href="/mpi/examples/..."></a>Makefiles</td></tr><tr>
          <td><a href="/mpi/examples/batchscript.c">batchscript.c/</a></td>
          <td><a href="/mpi/examples/batchscript.f">batchscript.f</a></td><td>Batch job scripts</td></tr><tr>
            <td><p><a href="/mpi/examples/mpi_bug1.c">mpi_bug1.c</a><br>
              <a href="/mpi/examples/mpi_bug2.c">mpi_bug2.c</a><br>
              <a href="/mpi/examples/mpi_bug3.c">mpi_bug3.c</a><br>
              <a href="/mpi/examples/mpi_bug4.c">mpi_bug4.c</a><br>
              <a href="/mpi/examples/mpi_bug5.c">mpi_bug5.c <br>
              </a><a href="/mpi/examples/mpi_bug6.c">mpi_bug6.c</a><br>
              <a href="/mpi/examples/mpi_bug7.c">mpi_bug7.c</a></p></td>
            <td><a href="/mpi/examples/mpi_bug1.f">mpi_bug1.f</a><br>
            <a href="/mpi/examples/mpi_bug2.f">mpi_bug2.f</a><br>
            <a href="/mpi/examples/mpi_bug3.f">mpi_bug3.f</a><br>
            <a href="/mpi/examples/mpi_bug4.f">mpi_bug4.f</a><br>
            <a href="/mpi/examples/mpi_bug5.f">mpi_bug5.f</a><br>
            <a href="/mpi/examples/mpi_bug6.f">mpi_bug6.f</a><br>
            <a href="/mpi/examples/mpi_bug7.f">mpi_bug7.f</a></td>
        <td>Programs with bugs</td></tr>
        </tbody>
        </table>
{% endraw %}

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
