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
