---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: FLUSH Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The FLUSH directive identifies a synchronization point at which the implementation must provide a consistent view of memory. Thread-visible variables are written back to memory at this point.

There is a fair amount of discussion on this directive within OpenMP circles that you may wish to consult for more information. Some of it is hard to understand. Per the API:
```
If the intersection of the flush-sets of two flushes performed by two different threads is non-empty, then the two flushes must be completed as if in some sequential order, seen by all threads.
```
*Say what?*

To quote from the openmp.org FAQ:

<pre>
<b>Q17:</b> Is the !$omp flush directive necessary on a cache coherent system?

<i><b>A17:</b> Yes the flush directive is necessary. Look in the OpenMP specifications for examples of it's uses. The directive is necessary to instruct the compiler that the variable must be written to/read from the memory system, i.e. that the variable can not be kept in a local CPU register over the flush "statement" in your code.


Cache coherency makes certain that if one CPU executes a read or write instruction from/to memory, then all other CPUs in the system will get the same value from that memory address when they access it. All caches will show a coherent value. However, in the OpenMP standard there must be a way to instruct the compiler to actually insert the read/write machine instruction and not postpone it. Keeping a variable in a register in a loop is very common when producing efficient machine language code for a loop.
</i></pre>

Also see the most recent OpenMP specs for details.