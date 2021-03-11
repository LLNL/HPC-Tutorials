---
layout: tutorial_page 
title: "Monitoring, Debugging, and Performance Analysis for Pthreads"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
## Monitoring and Debugging Pthreads:

- Debuggers vary in their ability to handle Pthreads. The TotalView debugger is LC's recommended debugger for parallel programs. It is well suited for both monitoring and debugging threaded programs.
- An example screenshot from a TotalView session using a threaded code is shown below.
    1. Stack Trace Pane: Displays the call stack of routines that the selected thread is executing.
    2. Status Bars: Show status information for the selected thread and its associated process.
    3. Stack Frame Pane: Shows a selected thread's stack variables, registers, etc.
    4. Source Pane: Shows the source code for the selected thread.
    5. Root Window showing all threads
    6. Threads Pane: Shows threads associated with the selected process

![Windows Workflows](/posix/images/pthreadWindows.gif)

- See the [TotalView Debugger tutorial](https://hpc.llnl.gov/training/tutorials/totalview) for details.
- The Linux ps command provides several flags for viewing thread information. Some examples are shown below. See the man page for details.
