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
<table style="border-collapse:collapse;border-spacing:0" class="tg"><thead><tr><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal">C Files</th><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Fortran Files</th><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Description</th></tr></thead><tbody><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_hello.c](examples/mpi_hello.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_hello.f](examples/mpi_hello.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_helloBsend.c](examples/mpi_helloBsend.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_helloBsend.f](examples/mpi_helloBsend.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include blocking send/receive routines</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_helloNBsend.c](examples/mpi_helloNBsend.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_helloNBsend.f](examples/mpi_helloNBsend.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include nonblocking send/receive routines</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_array.f](examples/mpi_array.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_array.c](examples/ser_array.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_array.f](examples/mpi_array.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_array.f](examples/ser_array.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Array Decomposition</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_mm.c](examples/mpi_mm.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_mm.c](examples/ser_mm.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_mm.f](examples/mpi_mm.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_mm.f](examples/ser_mm.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Matrix Multiply</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_pi_send.c](examples/mpi_pi_send.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_pi_calc.c](examples/ser_pi_calc.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_pi_send.f](examples/mpi_pi_send.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_pi_calc.f](examples/ser_pi_calc.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - point-to-point communications</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_pi_reduce.c](examples/mpi_pi_reduce.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_pi_calc.c](examples/ser_pi_calc.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_pi_reduce.f](examples/mpi_pi_reduce.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_pi_calc.f](examples/ser_pi_calc.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - collective communications</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_wave.c](examples/mpi_wave.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[draw_wave.c](examples/draw_wave.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_wave.c](examples/ser_wave.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_wave.f](examples/mpi_wave.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_wave.h](examples/mpi_wave.h)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[draw_wavef.c](examples/draw_wavef.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_wave.f](examples/ser_wave.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Concurrent Wave Equation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_heat2D.c](examples/mpi_heat2D.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[draw_heat.c](examples/draw_heat.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_heat2D.c](examples/ser_heat2D.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_heat2D.f](examples/mpi_heat2D.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_heat2D.h](examples/mpi_heat2D.h)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[draw_heatf.c](examples/draw_heatf.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_heat2D.f](examples/ser_heat2D.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">2D Heat Equation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_latency.c](examples/mpi_latency.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_latency.f](examples/mpi_latency.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Round Trip Latency Timing Test</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bandwidth.c](examples/mpi_bandwidth.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bandwidth_nonblock.c](examples/mpi_bandwidth_nonblock.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bandwidth.f](examples/mpi_bandwidth.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bandwidth_nonblock.f](examples/mpi_bandwidth_nonblock.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Bandwidth Timing Tests</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_prime.c](examples/mpi_prime.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_prime.c](examples/ser_prime.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_prime.f](examples/mpi_prime.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[ser_prime.f](examples/ser_prime.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Prime Number Generation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_ping.c](examples/mpi_ping.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_ringtopo.c](examples/mpi_ringtopo.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_scatter.c](examples/mpi_scatter.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_contig.c](examples/mpi_contig.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_vector.c](examples/mpi_vector.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_vector.c](examples/mpi_vector.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_struct.c](examples/mpi_struct.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_group.c](examples/mpi_group.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_cartesian.c](examples/mpi_cartesian.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_ping.f](examples/mpi_ping.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_ringtopo.f](examples/mpi_ringtopo.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_scatter.f](examples/mpi_scatter.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_contig.f](examples/mpi_contig.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_vector.f](examples/mpi_vector.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_indexed.f](examples/mpi_indexed.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_struct.f](examples/mpi_struct.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_group.f](examples/mpi_group.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_group.f](examples/mpi_group.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">From the tutorial...</span><br><span style="font-weight:normal;font-style:italic;color:#000">Blocking send-receive </span><br><span style="font-weight:normal;font-style:italic;color:#000">Non-blocking send-receive</span><span style="font-weight:normal;font-style:normal;color:#000"> </span><br><span style="font-weight:normal;font-style:italic;color:#000">Collective communications </span><br><span style="font-weight:normal;font-style:italic;color:#000">Contiguous derived datatype </span><br><span style="font-weight:normal;font-style:italic;color:#000">Vector derived datatype </span><br><span style="font-weight:normal;font-style:italic;color:#000">Indexed derived datatype </span><br><span style="font-weight:normal;font-style:italic;color:#000">Structure derived datatype </span><br><span style="font-weight:normal;font-style:italic;color:#000">Groups/Communicators </span><br><span style="font-weight:normal;font-style:italic;color:#000">Cartesian </span><span style="font-style:italic;color:#000">Virtual Topology</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[Makefile.MPI.c](examples/Makefile.MPI.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[Makefile.Ser.c](examples/Makefile.Ser.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[Makefile.MPI.f](examples/Makefile.MPI.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[Makefile.Ser.f](examples/Makefile.Ser.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Makefiles</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[batchscript.c](examples/batchscript.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[batchscript.f](examples/batchscript.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Batch job scripts</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug1.c](examples/mpi_bug1.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug2.c](examples/mpi_bug2.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug3.c](examples/mpi_bug3.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug4.c](examples/mpi_bug4.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug5.c](examples/mpi_bug5.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug6.c](examples/mpi_bug6.c)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug7.c](examples/mpi_bug7.c)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug1.f](examples/mpi_bug1.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug2.f](examples/mpi_bug2.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug3.f](examples/mpi_bug3.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug4.f](examples/mpi_bug4.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug5.f](examples/mpi_bug5.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug6.f](examples/mpi_bug6.f)</span><br><span style="font-weight:normal;font-style:normal;color:#000">[mpi_bug6.f](examples/mpi_bug6.f)</span></td><td style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Programs with bugs</span></td></tr></tbody></table>
{% endraw %}

<br> </br>

{% raw %}
<table style="border-collapse:collapse;border-spacing:0" class="tg"><thead><tr><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"><span style="background-color:#869AC3">C Files</span></th><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"><span style="background-color:#869AC3">Fortran Files</span></th><th style="background-color:#869AC3;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Description</th></tr></thead><tbody><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_hello.c'&gt;mpi_hello.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_hello.f'&gt;mpi_hello.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_helloBsend.c'&gt;mpi_helloBsend.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_helloBsend.f'&gt;mpi_helloBsend.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include blocking send/receive routines</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_helloNBsend.c'&gt;mpi_helloNBsend.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_helloNBsend.f'&gt;mpi_helloNBsend.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include nonblocking send/receive routines</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_array.c'&gt;mpi_array.c&lt;/a&gt;<br>&lt;a href='examples/ser_array.c'&gt;ser_array.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_array.f'&gt;mpi_array.f&lt;/a&gt;<br>&lt;a href='examples/ser_array.f'&gt;ser_array.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Array Decomposition</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_mm.c'&gt;mpi_mm.c&lt;/a&gt;<br>&lt;a href='examples/ser_mm.c'&gt;ser_mm.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_mm.f'&gt;mpi_mm.f&lt;/a&gt;<br>&lt;a href='examples/ser_mm.f'&gt;ser_mm.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Matrix Multiply</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_pi_send.c'&gt;mpi_pi_send.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_pi_send.f'&gt;mpi_pi_send.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - point-to-point communications</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_pi_reduce.c'&gt;mpi_pi_reduce.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_pi_reduce.f'&gt;mpi_pi_reduce.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - collective communications</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_wave.c'&gt;mpi_wave.c&lt;/a&gt;<br>&lt;a href='examples/draw_wave.c'&gt;draw_wave.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.c'&gt;ser_wave.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_wave.f'&gt;mpi_wave.f&lt;/a&gt;<br>&lt;a href='examples/mpi_wave.h'&gt;mpi_wave.h&lt;/a&gt;<br>&lt;a href='examples/draw_wavef.c'&gt;draw_wavef.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.f'&gt;ser_wave.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Concurrent Wave Equation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_heat2D.c'&gt;mpi_heat2D.c&lt;/a&gt;<br>&lt;a href='examples/draw_heat.c'&gt;draw_heat.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.c'&gt;ser_heat2D.c&lt;/a&gt;</td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_heat2D.f'&gt;mpi_heat2D.f&lt;/a&gt;<br>&lt;a href='examples/mpi_heat2D.h'&gt;mpi_heat2D.h&lt;/a&gt;<br>&lt;a href='examples/draw_heatf.c'&gt;draw_heatf.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.f'&gt;ser_heat2D.f&lt;/a&gt;</td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">2D Heat Equation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_latency.c'&gt;mpi_latency.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_latency.f'&gt;mpi_latency.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Round Trip Latency Timing Test</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_bandwidth.c'&gt;mpi_bandwidth.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.c'&gt;mpi_bandwidth_nonblock.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_bandwidth.f'&gt;mpi_bandwidth.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.f'&gt;mpi_bandwidth_nonblock.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Bandwidth Timing Tests</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_prime.c'&gt;mpi_prime.c&lt;/a&gt;<br>&lt;a href='examples/ser_prime.c'&gt;ser_prime.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_prime.f'&gt;mpi_prime.f&lt;/a&gt;<br>&lt;a href='examples/ser_prime.f'&gt;ser_prime.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Prime Number Generation</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_ping.c'&gt;mpi_ping.c&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.c'&gt;mpi_ringtopo.c&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.c'&gt;mpi_scatter.c&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.c'&gt;mpi_contig.c&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.c'&gt;mpi_vector.c&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.c'&gt;mpi_indexed.c&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.c'&gt;mpi_struct.c&lt;/a&gt;<br>&lt;a href='examples/mpi_group.c'&gt;mpi_group.c&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.c'&gt;mpi_cartesian.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_ping.f'&gt;mpi_ping.f&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.f'&gt;mpi_ringtopo.f&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.f'&gt;mpi_scatter.f&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.f'&gt;mpi_contig.f&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.f'&gt;mpi_vector.f&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.f'&gt;mpi_indexed.f&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.f'&gt;mpi_struct.f&lt;/a&gt;<br>&lt;a href='examples/mpi_group.f'&gt;mpi_group.f&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.f'&gt;mpi_cartesian.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">From the tutorial...</span><br>Blocking send-receive <br>Non-blocki<span style="font-weight:bold">ng send-rece</span>ive <br>Collective communications <br>Contiguous derived datatype <br>Vector derived datatype <br>Indexed derived datatype <br>Structure derived datatype <br>Groups/Communicators <br>Cartesian Virtual Topology</td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/Makefile.MPI.c'&gt;Makefile.MPI.c&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.c'&gt;Makefile.Ser.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/Makefile.MPI.f'&gt;Makefile.MPI.f&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.f'&gt;Makefile.Ser.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Makefiles</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/batchscript.c'&gt;batchscript.c&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/batchscript.f'&gt;batchscript.f&lt;/a&gt;<br></td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;font-style:normal;color:#000">Batch job scripts</span></td></tr><tr><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_bug1.c'&gt;mpi_bug1.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.c'&gt;mpi_bug2.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.c'&gt;mpi_bug3.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.c'&gt;mpi_bug4.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.c'&gt;mpi_bug5.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.c'&gt;mpi_bug6.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.c'&gt;mpi_bug7.c&lt;/a&gt;</td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#262087;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">&lt;a href='examples/mpi_bug1.f'&gt;mpi_bug1.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.f'&gt;mpi_bug2.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.f'&gt;mpi_bug3.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.f'&gt;mpi_bug4.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.f'&gt;mpi_bug5.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.f'&gt;mpi_bug6.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.f'&gt;mpi_bug7.f&lt;/a&gt;</td><td style="border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal"><span style="font-weight:normal;color:#000">Programs with bugs</span></td></tr></tbody></table>
{% endraw %}

<br></br>

{% raw %}
<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-nj5a{background-color:#869AC3;border-color:inherit;color:#333;font-weight:bold;position:-webkit-sticky;position:sticky;
  text-align:left;top:-1px;vertical-align:top;will-change:transform}
.tg .tg-mxcs{border-color:inherit;color:#333;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-j2ao{background-color:#869AC3;border-color:inherit;color:#333;font-weight:bold;position:-webkit-sticky;position:sticky;
  text-align:center;top:-1px;vertical-align:top;will-change:transform}
.tg .tg-eof1{border-color:inherit;color:#262087;text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-j2ao"><span style="background-color:#869AC3">C Files</span></th>
    <th class="tg-j2ao"><span style="background-color:#869AC3">Fortran Files</span></th>
    <th class="tg-nj5a">Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_hello.c'&gt;mpi_hello.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_hello.f'&gt;mpi_hello.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Hello World</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_helloBsend.c'&gt;mpi_helloBsend.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_helloBsend.f'&gt;mpi_helloBsend.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include blocking send/receive routines</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_helloNBsend.c'&gt;mpi_helloNBsend.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_helloNBsend.f'&gt;mpi_helloNBsend.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Hello World modified to include nonblocking send/receive routines</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_array.c'&gt;mpi_array.c&lt;/a&gt;<br>&lt;a href='examples/ser_array.c'&gt;ser_array.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_array.f'&gt;mpi_array.f&lt;/a&gt;<br>&lt;a href='examples/ser_array.f'&gt;ser_array.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Array Decomposition</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_mm.c'&gt;mpi_mm.c&lt;/a&gt;<br>&lt;a href='examples/ser_mm.c'&gt;ser_mm.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_mm.f'&gt;mpi_mm.f&lt;/a&gt;<br>&lt;a href='examples/ser_mm.f'&gt;ser_mm.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Matrix Multiply</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_pi_send.c'&gt;mpi_pi_send.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_pi_send.f'&gt;mpi_pi_send.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - point-to-point communications</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_pi_reduce.c'&gt;mpi_pi_reduce.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_pi_reduce.f'&gt;mpi_pi_reduce.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">pi Calculation - collective communications</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_wave.c'&gt;mpi_wave.c&lt;/a&gt;<br>&lt;a href='examples/draw_wave.c'&gt;draw_wave.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.c'&gt;ser_wave.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_wave.f'&gt;mpi_wave.f&lt;/a&gt;<br>&lt;a href='examples/mpi_wave.h'&gt;mpi_wave.h&lt;/a&gt;<br>&lt;a href='examples/draw_wavef.c'&gt;draw_wavef.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.f'&gt;ser_wave.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Concurrent Wave Equation</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_heat2D.c'&gt;mpi_heat2D.c&lt;/a&gt;<br>&lt;a href='examples/draw_heat.c'&gt;draw_heat.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.c'&gt;ser_heat2D.c&lt;/a&gt;</td>
    <td class="tg-eof1">&lt;a href='examples/mpi_heat2D.f'&gt;mpi_heat2D.f&lt;/a&gt;<br>&lt;a href='examples/mpi_heat2D.h'&gt;mpi_heat2D.h&lt;/a&gt;<br>&lt;a href='examples/draw_heatf.c'&gt;draw_heatf.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.f'&gt;ser_heat2D.f&lt;/a&gt;</td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">2D Heat Equation</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_latency.c'&gt;mpi_latency.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_latency.f'&gt;mpi_latency.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Round Trip Latency Timing Test</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_bandwidth.c'&gt;mpi_bandwidth.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.c'&gt;mpi_bandwidth_nonblock.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_bandwidth.f'&gt;mpi_bandwidth.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.f'&gt;mpi_bandwidth_nonblock.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Bandwidth Timing Tests</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_prime.c'&gt;mpi_prime.c&lt;/a&gt;<br>&lt;a href='examples/ser_prime.c'&gt;ser_prime.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_prime.f'&gt;mpi_prime.f&lt;/a&gt;<br>&lt;a href='examples/ser_prime.f'&gt;ser_prime.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Prime Number Generation</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_ping.c'&gt;mpi_ping.c&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.c'&gt;mpi_ringtopo.c&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.c'&gt;mpi_scatter.c&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.c'&gt;mpi_contig.c&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.c'&gt;mpi_vector.c&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.c'&gt;mpi_indexed.c&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.c'&gt;mpi_struct.c&lt;/a&gt;<br>&lt;a href='examples/mpi_group.c'&gt;mpi_group.c&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.c'&gt;mpi_cartesian.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/mpi_ping.f'&gt;mpi_ping.f&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.f'&gt;mpi_ringtopo.f&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.f'&gt;mpi_scatter.f&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.f'&gt;mpi_contig.f&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.f'&gt;mpi_vector.f&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.f'&gt;mpi_indexed.f&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.f'&gt;mpi_struct.f&lt;/a&gt;<br>&lt;a href='examples/mpi_group.f'&gt;mpi_group.f&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.f'&gt;mpi_cartesian.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">From the tutorial...</span><br>Blocking send-receive <br>Non-blocki<span style="font-weight:bold">ng send-rece</span>ive <br>Collective communications <br>Contiguous derived datatype <br>Vector derived datatype <br>Indexed derived datatype <br>Structure derived datatype <br>Groups/Communicators <br>Cartesian Virtual Topology</td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/Makefile.MPI.c'&gt;Makefile.MPI.c&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.c'&gt;Makefile.Ser.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/Makefile.MPI.f'&gt;Makefile.MPI.f&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.f'&gt;Makefile.Ser.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Makefiles</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/batchscript.c'&gt;batchscript.c&lt;/a&gt;<br></td>
    <td class="tg-eof1">&lt;a href='examples/batchscript.f'&gt;batchscript.f&lt;/a&gt;<br></td>
    <td class="tg-mxcs"><span style="font-weight:normal;font-style:normal;color:#000">Batch job scripts</span></td>
  </tr>
  <tr>
    <td class="tg-eof1">&lt;a href='examples/mpi_bug1.c'&gt;mpi_bug1.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.c'&gt;mpi_bug2.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.c'&gt;mpi_bug3.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.c'&gt;mpi_bug4.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.c'&gt;mpi_bug5.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.c'&gt;mpi_bug6.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.c'&gt;mpi_bug7.c&lt;/a&gt;</td>
    <td class="tg-eof1">&lt;a href='examples/mpi_bug1.f'&gt;mpi_bug1.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.f'&gt;mpi_bug2.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.f'&gt;mpi_bug3.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.f'&gt;mpi_bug4.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.f'&gt;mpi_bug5.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.f'&gt;mpi_bug6.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.f'&gt;mpi_bug7.f&lt;/a&gt;</td>
    <td class="tg-mxcs"><span style="font-weight:normal;color:#000">Programs with bugs</span></td>
  </tr>
</tbody>
</table>
{% endraw %}

Non-CSS attempt attempt:

{% raw %}
<table>
<thead>
  <tr>
    <th>C Files</th>
    <th>Fortran Files</th>
    <th>Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>&lt;a href='examples/mpi_hello.c'&gt;mpi_hello.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_hello.f'&gt;mpi_hello.f&lt;/a&gt;<br></td>
    <td>Hello World</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_helloBsend.c'&gt;mpi_helloBsend.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_helloBsend.f'&gt;mpi_helloBsend.f&lt;/a&gt;<br></td>
    <td>Hello World modified to include blocking send/receive routines</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_helloNBsend.c'&gt;mpi_helloNBsend.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_helloNBsend.f'&gt;mpi_helloNBsend.f&lt;/a&gt;<br></td>
    <td>Hello World modified to include nonblocking send/receive routines</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_array.c'&gt;mpi_array.c&lt;/a&gt;<br>&lt;a href='examples/ser_array.c'&gt;ser_array.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_array.f'&gt;mpi_array.f&lt;/a&gt;<br>&lt;a href='examples/ser_array.f'&gt;ser_array.f&lt;/a&gt;<br></td>
    <td>Array Decomposition</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_mm.c'&gt;mpi_mm.c&lt;/a&gt;<br>&lt;a href='examples/ser_mm.c'&gt;ser_mm.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_mm.f'&gt;mpi_mm.f&lt;/a&gt;<br>&lt;a href='examples/ser_mm.f'&gt;ser_mm.f&lt;/a&gt;<br></td>
    <td>Matrix Multiply</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_pi_send.c'&gt;mpi_pi_send.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_pi_send.f'&gt;mpi_pi_send.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td>
    <td>pi Calculation - point-to-point communications</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_pi_reduce.c'&gt;mpi_pi_reduce.c&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.c'&gt;ser_pi_calc.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_pi_reduce.f'&gt;mpi_pi_reduce.f&lt;/a&gt;<br>&lt;a href='examples/ser_pi_calc.f'&gt;ser_pi_calc.f&lt;/a&gt;<br></td>
    <td>pi Calculation - collective communications</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_wave.c'&gt;mpi_wave.c&lt;/a&gt;<br>&lt;a href='examples/draw_wave.c'&gt;draw_wave.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.c'&gt;ser_wave.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_wave.f'&gt;mpi_wave.f&lt;/a&gt;<br>&lt;a href='examples/mpi_wave.h'&gt;mpi_wave.h&lt;/a&gt;<br>&lt;a href='examples/draw_wavef.c'&gt;draw_wavef.c&lt;/a&gt;<br>&lt;a href='examples/ser_wave.f'&gt;ser_wave.f&lt;/a&gt;<br></td>
    <td>Concurrent Wave Equation</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_heat2D.c'&gt;mpi_heat2D.c&lt;/a&gt;<br>&lt;a href='examples/draw_heat.c'&gt;draw_heat.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.c'&gt;ser_heat2D.c&lt;/a&gt;</td>
    <td>&lt;a href='examples/mpi_heat2D.f'&gt;mpi_heat2D.f&lt;/a&gt;<br>&lt;a href='examples/mpi_heat2D.h'&gt;mpi_heat2D.h&lt;/a&gt;<br>&lt;a href='examples/draw_heatf.c'&gt;draw_heatf.c&lt;/a&gt;<br>&lt;a href='examples/ser_heat2D.f'&gt;ser_heat2D.f&lt;/a&gt;</td>
    <td>2D Heat Equation</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_latency.c'&gt;mpi_latency.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_latency.f'&gt;mpi_latency.f&lt;/a&gt;<br></td>
    <td>Round Trip Latency Timing Test</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_bandwidth.c'&gt;mpi_bandwidth.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.c'&gt;mpi_bandwidth_nonblock.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_bandwidth.f'&gt;mpi_bandwidth.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bandwidth_nonblock.f'&gt;mpi_bandwidth_nonblock.f&lt;/a&gt;<br></td>
    <td>Bandwidth Timing Tests</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_prime.c'&gt;mpi_prime.c&lt;/a&gt;<br>&lt;a href='examples/ser_prime.c'&gt;ser_prime.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_prime.f'&gt;mpi_prime.f&lt;/a&gt;<br>&lt;a href='examples/ser_prime.f'&gt;ser_prime.f&lt;/a&gt;<br></td>
    <td>Prime Number Generation</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_ping.c'&gt;mpi_ping.c&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.c'&gt;mpi_ringtopo.c&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.c'&gt;mpi_scatter.c&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.c'&gt;mpi_contig.c&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.c'&gt;mpi_vector.c&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.c'&gt;mpi_indexed.c&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.c'&gt;mpi_struct.c&lt;/a&gt;<br>&lt;a href='examples/mpi_group.c'&gt;mpi_group.c&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.c'&gt;mpi_cartesian.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/mpi_ping.f'&gt;mpi_ping.f&lt;/a&gt;<br>&lt;a href='examples/mpi_ringtopo.f'&gt;mpi_ringtopo.f&lt;/a&gt;<br>&lt;a href='examples/mpi_scatter.f'&gt;mpi_scatter.f&lt;/a&gt;<br>&lt;a href='examples/mpi_contig.f'&gt;mpi_contig.f&lt;/a&gt;<br>&lt;a href='examples/mpi_vector.f'&gt;mpi_vector.f&lt;/a&gt;<br>&lt;a href='examples/mpi_indexed.f'&gt;mpi_indexed.f&lt;/a&gt;<br>&lt;a href='examples/mpi_struct.f'&gt;mpi_struct.f&lt;/a&gt;<br>&lt;a href='examples/mpi_group.f'&gt;mpi_group.f&lt;/a&gt;<br>&lt;a href='examples/mpi_cartesian.f'&gt;mpi_cartesian.f&lt;/a&gt;<br></td>
    <td>From the tutorial...<br>Blocking send-receive <br>Non-blocking send-receive <br>Collective communications <br>Contiguous derived datatype <br>Vector derived datatype <br>Indexed derived datatype <br>Structure derived datatype <br>Groups/Communicators <br>Cartesian Virtual Topology</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/Makefile.MPI.c'&gt;Makefile.MPI.c&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.c'&gt;Makefile.Ser.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/Makefile.MPI.f'&gt;Makefile.MPI.f&lt;/a&gt;<br>&lt;a href='examples/Makefile.Ser.f'&gt;Makefile.Ser.f&lt;/a&gt;<br></td>
    <td>Makefiles</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/batchscript.c'&gt;batchscript.c&lt;/a&gt;<br></td>
    <td>&lt;a href='examples/batchscript.f'&gt;batchscript.f&lt;/a&gt;<br></td>
    <td>Batch job scripts</td>
  </tr>
  <tr>
    <td>&lt;a href='examples/mpi_bug1.c'&gt;mpi_bug1.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.c'&gt;mpi_bug2.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.c'&gt;mpi_bug3.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.c'&gt;mpi_bug4.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.c'&gt;mpi_bug5.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.c'&gt;mpi_bug6.c&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.c'&gt;mpi_bug7.c&lt;/a&gt;</td>
    <td>&lt;a href='examples/mpi_bug1.f'&gt;mpi_bug1.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug2.f'&gt;mpi_bug2.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug3.f'&gt;mpi_bug3.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug4.f'&gt;mpi_bug4.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug5.f'&gt;mpi_bug5.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug6.f'&gt;mpi_bug6.f&lt;/a&gt;<br>&lt;a href='examples/mpi_bug7.f'&gt;mpi_bug7.f&lt;/a&gt;</td>
    <td>Programs with bugs</td>
  </tr>
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
