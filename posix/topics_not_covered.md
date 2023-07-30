---
layout: tutorial_page
title: "Topics Not Covered"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
Several features of the Pthreads API are not covered in this tutorial. These are listed below. See the Pthread Library Routines Reference section for more information.

- Thread Scheduling
    - Implementations will differ on how threads are scheduled to run. In most cases, the default mechanism is adequate.
    - The Pthreads API provides routines to explicitly set thread scheduling policies and priorities which may override the default mechanisms.
    - The API does not require implementations to support these features.
- Keys: Thread-Specific Data
    - As threads call and return from different routines, the local data on a thread's stack comes and goes.
    - To preserve stack data you can usually pass it as an argument from one routine to the next, or else store the data in a global variable associated with a thread.
    - Pthreads provides another, possibly more convenient and versatile, way of accomplishing this through keys.
- Mutex Protocol Attributes and Mutex Priority Management for the handling of "priority inversion" problems.
    - Condition Variable Sharing—across processes
    - Thread Cancellation
    - Threads and Signals
    - Sychronization constructs—barriers and locks
