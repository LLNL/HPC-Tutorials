---
layout: tutorial_page
title: "Monitoring, Debugging and Performance Analysis Tools for OpenMP"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Monitoring and Debugging Threads:

* Debuggers vary in their ability to handle threads. The TotalView debugger is LC's recommended debugger for parallel programs. It is well suited for both monitoring and debugging threaded programs.

* An example screenshot from a TotalView session using an OpenMP code is shown below.
  1. Master thread Stack Trace Pane showing original routine
  2. Process/thread status bars differentiating threads
  3. Master thread Stack Frame Pane showing shared variables
  4. Worker thread Stack Trace Pane showing outlined routine.
  5. Worker thread Stack Frame Pane
  6. Root Window showing all threads
  7. Threads Pane showing all threads plus selected thread

![Totalview_image](images/totalview.gif)

* See the [TotalView Debugger tutorial](https://hpc.llnl.gov/training/tutorials/totalview-tutorial) for details.

* The Linux `ps` command provides several flags for viewing thread information. Some examples are shown below. See the [man page](https://www.freebsd.org/cgi/man.cgi?query=ps&manpath=FreeBSD+13.0-RELEASE+and+Ports) for details.

<pre>
% <b>ps -Lf </b>
UID        PID  PPID   LWP  C NLWP STIME TTY          TIME CMD
blaise   22529 28240 22529  0    5 11:31 pts/53   00:00:00 a.out
blaise   22529 28240 22530 99    5 11:31 pts/53   00:01:24 a.out
blaise   22529 28240 22531 99    5 11:31 pts/53   00:01:24 a.out
blaise   22529 28240 22532 99    5 11:31 pts/53   00:01:24 a.out
blaise   22529 28240 22533 99    5 11:31 pts/53   00:01:24 a.out

% <b>ps -T </b>
  PID  SPID TTY          TIME CMD
22529 22529 pts/53   00:00:00 a.out
22529 22530 pts/53   00:01:49 a.out
22529 22531 pts/53   00:01:49 a.out
22529 22532 pts/53   00:01:49 a.out
22529 22533 pts/53   00:01:49 a.out

% <b>ps -Lm</b> 
  PID   LWP TTY          TIME CMD
22529     - pts/53   00:18:56 a.out
    - 22529 -        00:00:00 -
    - 22530 -        00:04:44 -
    - 22531 -        00:04:44 -
    - 22532 -        00:04:44 -
    - 22533 -        00:04:44 -
</pre>

LC's Linux clusters also provide the `top` command to monitor processes on a node. If used with the `-H` flag, the threads contained within a process will be visible. An example of the `top -H` command is shown below. The parent process is PID 18010 which spawned three threads, shown as PIDs 18012, 18013 and 18014.

![terminal window screenshot](/posix/images/topH.gif)


## Performance Analysis Tools:

* There are a variety of performance analysis tools that can be used with OpenMP programs. Searching the web will turn up a wealth of information.

* At LC, the list of supported computing tools can be found at: [https://hpc.llnl.gov/software](https://hpc.llnl.gov/software).

* These tools vary significantly in their complexity, functionality and learning curve. Covering them in detail is beyond the scope of this tutorial.

* Some tools worth investigating, specifically for OpenMP codes, include:
 
  * Open\|SpeedShop

  * TAU
 
  * PAPI
  
  * Intel VTune Amplifier
  
  * ThreadSpotter
