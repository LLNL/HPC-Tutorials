---
layout: tutorial_page
title: "Exercise 2"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

### Overview:
* Login to the LC workshop cluster, if you are not already logged in
* Using your "Hello World" MPI program from Exercise 1, add MPI blocking point-to-point routines to send and receive messages
* Successfully compile your program
* Successfully run your program - several different ways
* Try the same thing with nonblocking send/receive routines

**Approx. 20 minutes**

#### 1. Still logged into the workshop cluster?
If so, then continue to the next step. If not, then login as you did previously for Exercise 1.

#### 2. Create, compile and run an MPI blocking send/receive program

Assuming you were able to create a successful "hello world" MPI program in Exercise 1, copy your source file to a new file and call it something like `helloBsend.c` or `helloBsend.f`. If you were **not** successful, you can use the provided `mpi_hello.c` or `mpi_hello.f`. as your starting point.

Edit your new `helloBsend` source file and modify it to do the following - after the master task has printed the number of tasks, but before MPI_Finalize:
Have each task determine a unique partner task to send/receive with. One easy way to do this:

`C:`
```
if (taskid < numtasks/2) then partner = numtasks/2 + taskid
else if (taskid >= numtasks/2) then partner = taskid - numtasks/2
```
`Fortran:`
```
if (taskid .lt. numtasks/2) then partner = numtasks/2 + taskid
else if (taskid .ge. numtasks/2) then partner = taskid - numtasks/2
```

Each task sends its partner a single integer message: its `taskid`.

Each task receives from its partner a single integer message: the partner's `taskid`.

For confirmation, after the send/receive, each task prints something like "Task ## is partner with ##" where ## is the taskid of the task and its partner.

If you need help, see the provided example files [mpi_helloBsend.c](examples/mpi_helloBsend.c) or [mpi_helloBsend.f](examples/mpi_helloBsend.f)

#### 3. Compile your `helloBsend` program

Using your choice of compiler as in Exercise 1, compile your `helloBsend` program until you get a clean compile.

#### 4. Run your executable

Using the `srun` command as in Exercise 1 to run your executable. For example:
```
srun -N2 -n8 -ppReserved helloBsend
```

Did your job run successfully? Based on the output, did it behave as expected? If not, figure out any problems and fix them before proceeding.

#### 5. Vary numbers of nodes and total tasks

Run your program a few more times, but vary the number of nodes and total tasks. Observe the task output statements to confirm.

#### 6. Convert to non-blocking

If time permits, copy your helloBsend source file to a new helloNBsend source file. Then convert the blocking routines to non-blocking routines. See the provided or source files if you need assistance.

#### 7. Compile and run as before.
