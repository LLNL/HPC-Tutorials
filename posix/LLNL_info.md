---
layout: tutorial_page
title: "LLNL Specific Information and Recommendations"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
This section describes details specific to Livermore Computing's systems.

## Implementations:

All LC production systems include a Pthreads implementation that follows draft 10 (final) of the POSIX standard. This is the preferred implementation.
Implementations differ in the maximum number of threads that a process may create. They also differ in the default amount of thread stack space.

## Compiling:

LC maintains a number of compilers, and usually several different versions of each - see the LC's Supported Compilers web page.
The compiler commands described in the Compiling Threaded Programs section apply to LC systems.

## Mixing MPI with Pthreads:

This is the primary motivation for using Pthreads at LC.

## Design:

Each MPI process typically creates and then manages N threads, where N makes the best use of the available cores/node.
Finding the best value for N will vary with the platform and your application's characteristics.
In general, there may be problems if multiple threads make MPI calls. The program may fail or behave unexpectedly. If MPI calls must be made from within a thread, they should be made only by one thread.

## Compiling:

Use the appropriate MPI compile command for the platform and language of choice
Be sure to include the required Pthreads flag as shown in the Compiling Threaded Programs section.
An example code that uses both MPI and Pthreads is available below. The serial, threads-only, MPI-only and MPI-with-threads versions demonstrate one possible progression.

* Serial
* Pthreads only
* MPI only
* MPI with pthreads
* makefile
