---
layout: tutorial_page
title: "Exercise 3"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview:

* Login to the workshop cluster, if you are not already logged in

* Orphaned directive example: review, compile, run

* Get OpenMP implementation environment information

* Check out the "bug" programs

## 1. Still logged into the workshop cluster?
1. If so, then continue to the next step. If not, then login as you did previously for Exercise 1.

## 2. Review / Compile / Run the orphan example code

This example computes a dot product in parallel, however it differs from previous examples because the parallel loop construct is orphaned - it is contained in a subroutine outside the lexical extent of the main program's parallel region.

1. After reviewing the source code, compile and run the program. For example:
#### C:	
```
icc -qopenmp omp_orphan.c -o orphan 
orphan | sort
```
#### Fortran:	
```
ifort -qopenmp omp_orphan.f -o orphan
orphan | sort
```

2. Note the result...and the fact that this example will come back to haunt as `omp_bug6` later.

## 3. Get environment information

1. Starting from scratch, write a simple program that obtains information about your openMP environment. Alternately, you can modify the "hello" program to do this.

2. Using the appropriate openMP routines/functions, have the master thread query and print the following:
  * The number of processors available
  * The number of threads being used
  * The maximum number of threads available
  * If you are in a parallel region
  * If dynamic threads are enabled
  * If nested parallelism is supported
  NOTE: Some compilers (IBM, GNU) implement some of the necessary Fortran functions as integer instead of logical as the standard specifies.

3. If you need help, you can consult the [omp_getEnvInfo.c](code_examples/C/omp_getEnvInfo.c) or [omp_getEnvInfo.f](code_examples/Fortran/omp_getEnvInfo.f) example file.

## 4. When things go wrong...

There are many things that can go wrong when developing OpenMP programs. The `omp_bugX.X` series of programs demonstrate just a few. See if you can figure out what the problem is with each case and then fix it.

**Note:** Please use the Intel (icc, ifort) compile commands for these exercises.

The buggy behavior will differ for each example. Some hints are provided below.

<details>
  <summary> Hints (Click to expand!)</summary>

	<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-1wig{font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-ycr8{background-color:#ffffff;text-align:left;vertical-align:top}
.tg .tg-hsb7{background-color:#ffffff;color:rgba(0, 0, 0, 0.847);text-align:center;vertical-align:top}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Code</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Behavior</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Hints/Notes</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-1wig">omp_bug1 <br>omp_bug1fix</td>
    <td class="tg-0lax">Fails compilation. Solution provided - must compile solution file.</td>
    <td class="tg-hsb7">EXPLANATION: This example attempts to show the use of the combined PARALLEL for-DO directive. It fails because the loop does not come immediately after the directive. Corrections include removing all statements between the PARALLEL for-DO directive and the actual loop. Also, logic is added to preserve the ability to query the thread id and print it from inside the loop. Notice the use of the FIRSTPRIVATE clause to intialize the flag.</td>
  </tr>
  <tr>
    <td class="tg-1wig">omp_bug2</td>
    <td class="tg-0lax">Thread identifiers are wrong. Wrong answers.</td>
    <td class="tg-hsb7">EXPLANATION: The bugs in this case are caused by neglecting to scope the TID and TOTAL variables as PRIVATE. By default, most OpenMP variables are scoped as SHARED. These variables need to be unique for each thread.</td>
  </tr>
  <tr>
    <td class="tg-1wig">omp_bug3</td>
    <td class="tg-0lax">Run-time error, hang.</td>
    <td class="tg-hsb7">EXPLANATION: The run time error is caused by by the OMP BARRIER directive in the PRINT_RESULTS subroutine. By definition, an OMP BARRIER can not be nested outside the static extent of a SECTIONS directive. In this case it is orphaned outside the calling SECTIONS block.</td>
  </tr>
  <tr>
    <td class="tg-1wig">omp_bug4 <br>omp_bug4fix</td>
    <td class="tg-0lax">Causes a segmentation fault. Solution provided - note that it is a script and will need to be "sourced". For example: "source omp_bug4fix". Be sure to examine the solution file to see what's going on - especially the last line, where you may need to change the name of the executable to match yours.</td>
    <td class="tg-hsb7">EXPLANATION: OpenMP thread stack size is an implementation dependent resource. In this case, the array is too large to fit into the thread stack space and causes the segmentation fault.</td>
  </tr>
  <tr>
    <td class="tg-1wig">omp_bug5 <br>omp_bug5fix</td>
    <td class="tg-0lax">Program hangs. Solution provided - must compile solution file.</td>
    <td class="tg-hsb7">EXPLANATION: The problem in omp_bug5 is that the first thread acquires locka and then tries to get lockb before releasing locka. Meanwhile, the second thread has acquired lockb and then tries to get locka before releasing lockb. The solution overcomes the deadlock by using locks correctly.</td>
  </tr>
  <tr>
    <td class="tg-1wig">omp_bug6</td>
    <td class="tg-0lax">Failed compilation</td>
    <td class="tg-ycr8">EXPLANATION: With orphaned directives, the correct scoping of variables is critical. The error occurs because the sum variable is scoped incorrectly. See the omp_orphan routine for one example of correct scoping. Note that there are other ways.</td>
  </tr>
</tbody>
</table>

</details>

*If you're just finishing the tutorial and haven't filled out our [evaluation form](https://hpc.llnl.gov/training/tutorials/evaluation-form) yet, please do!*


	
