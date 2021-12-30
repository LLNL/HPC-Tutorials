---
layout: tutorial_page
title: "Thread Stack Size and Thread Binding"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---
## Thread Stack Size:

* The OpenMP standard does not specify how much stack space a thread should have. Consequently, implementations will differ in the default thread stack size.

* Default thread stack size can be easy to exhaust. It can also be non-portable between compilers. Using past versions of LC compilers as an example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-cly1{text-align:left;vertical-align:middle}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Compiler</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Approx. Stack Limit</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Approx. Array Size (doubles)</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-cly1">Linux icc, ifort</td>
    <td class="tg-cly1">4 MB</td>
    <td class="tg-cly1">700 x 700</td>
  </tr>
  <tr>
    <td class="tg-cly1">Linux pgcc, pgf90</td>
    <td class="tg-cly1">8 MB</td>
    <td class="tg-cly1">1000 x 1000</td>
  </tr>
  <tr>
    <td class="tg-cly1">Linux gcc, gfortran</td>
    <td class="tg-cly1">2 MB</td>
    <td class="tg-cly1">500 x 500</td>
  </tr>
</tbody>
</table>

* Threads that exceed their stack allocation may or may not seg fault. An application may continue to run while data is being corrupted.

* Statically linked codes may be subject to further stack restrictions.

* A user's login shell may also restrict stack size.

* If your OpenMP environment supports the OpenMP 3.0 OMP_STACKSIZE environment variable (covered in previous section), you can use it to set the thread stack size prior to program execution. For example:

```
setenv OMP_STACKSIZE 2000500B
setenv OMP_STACKSIZE "3000 k "
setenv OMP_STACKSIZE 10M
setenv OMP_STACKSIZE " 10 M "
setenv OMP_STACKSIZE "20 m "
setenv OMP_STACKSIZE " 1G"
setenv OMP_STACKSIZE 20000
```

* Otherwise, at LC, you should be able to use the method below for Linux clusters. The example shows setting the thread stack size to 12 MB, and as a precaution, setting the shell stack size to unlimited.

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">csh/tcsh</span></th>
    <th class="tg-0lax">setenv KMP_STACKSIZE 12000000 <br>limit stacksize unlimited </th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-fdm5"><span style="background-color:#98ABCE">ksh/sh/bash</span></td>
    <td class="tg-0lax">export KMP_STACKSIZE=12000000 <br>ulimit -s unlimited </td>
  </tr>
</tbody>
</table>


## Thread Binding:

* In some cases, a program will perform better if its threads are bound to processors/cores.

* "Binding" a thread to a processor means that a thread will be scheduled by the operating system to always run on a the same processor. 

* Otherwise, threads can be scheduled to execute on any processor and "bounce" back and forth between processors with each time slice.

* Also called "thread affinity" or "processor affinity"

* Binding threads to processors can result in better cache utilization, thereby reducing costly memory accesses. This is the primary motivation for binding threads to processors.

* Depending upon your platform, operating system, compiler and OpenMP implementation, binding threads to processors can be done several different ways.

* The OpenMP version 3.1 API provides an environment variable to turn processor binding "on" or "off". For example:
```
setenv OMP_PROC_BIND  TRUE
setenv OMP_PROC_BIND  FALSE
```

* At a higher level, processes can also be bound to processors.

* Detailed information about process and thread binding to processors on LC Linux clusters can be found [HERE](ProcessThreadAffinity.pdf).
