---
layout: tutorial_page
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

First, review the serial version of this example code, either [ser_array.c](examples/ser_array.c) or [ser_array.f](examples/ser_array.f).

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

**Note**: The `srun` command is covered in detail in the "Starting Jobs" section of the Linux Clusters Overview tutorial, located [here](https://hpc.llnl.gov/training/tutorials/livermore-computing-linux-commodity-clusters-overview-part-two#Starting). There is also a man page.

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

#### 7. Compare per task and aggregate communications bandwidths
- Compile the `mpi_bandwidth` code if you haven't already.
- Run the code interactively with 2 tasks on two different nodes:
```
srun -N2 -n2 -ppReserved mpi_bandwidth
```
- Note the overall average bandwidth for the largest message size of 1,000,000 bytes.
- Now run the code interactively with 4, 8, 16, 32 and 64 tasks on two different nodes:

```
srun -N2 -n4 -ppReserved mpi_bandwidth
srun -N2 -n8 -ppReserved mpi_bandwidth
srun -N2 -n16 -ppReserved mpi_bandwidth
srun -N2 -n32 -ppReserved mpi_bandwidth
srun -N2 -n64 -ppReserved mpi_bandwidth
```

- Note the average bandwidths as before.
- What are your observations? Why?
<details>
  <summary> Explanation (Click to expand!)</summary>

As the number of tasks increase, the per task bandwidth decreases because they must compete for use of the network adapter. Aggregate bandwidth will increase until it plateaus.

</details>

#### 8. Compare blocking send/receive with non-blocking send/receive
- Copy your `mpi_bandwidth` source file to another file called `mpi_bandwidthNB`. Modify your new file so that it performs non-blocking sends/receives instead of blocking. An example `mpi_bandwidth_nonblock` file has been provided in case you need it.
- After you're satisfied with your new non-blocking version of the bandwidth code, compile both.
- Run each code using two tasks on two different nodes in the special workshop pool:
```
srun -N2 -ppReserved mpi_bandwidth
srun -N2 -ppReserved mpi_bandwidthNB
```
- Compare the results. Which one performs best?

<details>
  <summary> Explanation (Click to expand!)</summary>

Non-blocking send/receive operations are often significantly faster than blocking send/receive operations.

</details>

#### 9. When things go wrong...
There are many things that can go wrong when developing MPI programs. The mpi_bug series of programs demonstrate just a few. See if you can figure out what the problem is with each case and then fix it.

Compile with the compile command(s) of your choice and run interactively using 4 tasks in the special workshop pool.

The buggy behavior will differ for each example. Some hints are provided below.

<details>
  <summary> Hints (Click to expand!)</summary>

<table style="border-collapse:collapse;border-spacing:0" class="tg"><thead><tr><th style="background-color:#98ABCE;border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"><span style="background-color:#98ABCE">Code</span></th><th style="background-color:#98ABCE;border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"><span style="background-color:#98ABCE">Behavior</span></th><th style="background-color:#98ABCE;border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"><span style="background-color:#98ABCE">Hints/Notes</span></th></tr></thead><tbody><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug1</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Hangs</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">mpi_bug1 demonstrates how miscoding even a simple parameter like a message tag can lead to a hung program. Verify that the message sent from task 0 is not exactly what task 1 is expecting and vice versa. Matching the send tags with the receive tags solves the problem.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug2</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Wrong results or abnormal termination</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">mpi_bug2 shows another type of miscoding. The data type of the message sent by task 0 is not what task 1 expects. Nevertheless, the message is received, resulting in wrong results or abnormal termination - depending upon the MPI library and platform. Matching the send data type with the receive data type solves the problem.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug3</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Error message and/or abnormal termination</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">mpi_bug3 shows what happens when the MPI environment is not initialized or terminated properly. Inserting the MPI init and finalize calls in the right locations will solve the problem.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug4</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Gives the wrong result for "Final sum". Compare to mpi_array</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"><span style="font-weight:bold"> Number of MPI tasks must be divisible by 4; </span>mpi_bug4 shows what happens when a task does not participate in a collective communication call. In this case, task 0 needs to call MPI_Reduce as the other tasks do.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug5</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Dies or hangs - depends upon platform and MPI library </td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">mpi_bug5 demonstrates an unsafe program, because sometimes it will execute fine, and other times it will fail. The reason why the program fails or hangs is due to buffer exhaustion on the receiving task side, as a consequence of the way an MPI library has implemented an eager protocol for messages of a certain size.  One possible solution is to include an MPI_Barrier call in  the both the send and receive loops.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug6</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Terminates or is ignored (depends on platform/language)</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"><span style="font-weight:bold"> Requires 4 MPI tasks; </span>mpi_bug6 has a bug that will terminate the program in some cases but be ignored in other cases. The problem is that task 2 performs a blocking operation, but then hits the MPI_Wait call near the end of the program. Only the tasks that make non-blocking calls should hit the MPI_Wait. The coding error in this case is easy to fix - simply make sure task 2 does not encounter the MPI_Wait call.</td></tr><tr><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">mpi_bug7</td><td style="border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">Hangs</td><td style="background-color:#C0C0C0;border-color:black;border-style:solid;border-width:1px;color:rgba(0, 0, 0, 0.847);font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">mpi_bug7 performs a collective communication broadcast but erroneously codes the count argument incorrectly resulting in a hang condition.</td></tr></tbody></table>

</details>

*If you're just finishing the tutorial and haven't filled out our [evaluation form](https://hpc.llnl.gov/training/tutorials/evaluation-form) yet, please do!*
