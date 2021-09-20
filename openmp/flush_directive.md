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

> If the intersection of the flush-sets of two flushes performed by two different threads is non-empty, then the two flushes must be completed as if in some sequential order, seen by all threads.

*Say what?*

To quote from the openmp.org FAQ:


><b>Q17:</b> Is the !$omp flush directive necessary on a cache coherent system?

><i><b>A17:</b> Yes the flush directive is necessary. Look in the OpenMP specifications for examples of it's uses. The directive is necessary to instruct the compiler that the variable must be written to/read from the memory system, i.e. that the variable can not be kept in a local CPU register over the flush "statement" in your code.


>Cache coherency makes certain that if one CPU executes a read or write instruction from/to memory, then all other CPUs in the system will get the same value from that memory address when they access it. All caches will show a coherent value. However, in the OpenMP standard there must be a way to instruct the compiler to actually insert the read/write machine instruction and not postpone it. Keeping a variable in a register in a loop is very common when producing efficient machine language code for a loop.
</i>

Also see the most recent OpenMP specs for details.

## Format:

### Fortran
<pre>
!$OMP FLUSH  <i>(list)</i>
</pre>

### C/C++	
<pre>
#pragma omp flush <i>(list)  newline</i>
</pre>

## Notes:

The optional list contains a list of named variables that will be flushed in order to avoid flushing all variables. For pointers in the list, note that the pointer itself is flushed, not the object it points to.

Implementations must ensure any prior modifications to thread-visible variables are visible to all threads after this point; ie. compilers must restore values from registers to memory, hardware might need to flush write buffers, etc.

The FLUSH directive is implied for the directives shown in the table below. The directive is not implied if a NOWAIT clause is present.

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Fortran</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">C / C++</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">BARRIER <br>END PARALLEL <br>CRITICAL and END CRITICAL <br>END DO <br>END SECTIONS <br>END SINGLE <br>ORDERED and END ORDERED</td>
    <td class="tg-0lax">barrier <br>parallel - upon entry and exit <br>critical - upon entry and exit <br>ordered - upon entry and exit <br>for - upon exit <br>sections - upon exit <br>single - upon exit </td>
  </tr>
</tbody>
</table>