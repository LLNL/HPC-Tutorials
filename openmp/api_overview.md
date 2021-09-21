---
layout: tutorial_page
title: "OpenMP API Overview"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Three Components:

The OpenMP API is comprised of three distinct components. As of version 4.0:
* Compiler Directives (44)
* Runtime Library Routines (35)
* Environment Variables (13) 

The application developer decides how to employ these components. In the simplest case, only a few of them are needed.

Implementations differ in their support of all API components. For example, an implementation may state that it supports nested parallelism, but the API makes it clear that may be limited to a single thread - the master thread. Not exactly what the developer might expect? 

## Compiler Directives:

Compiler directives appear as comments in your source code and are ignored by compilers unless you tell them otherwise - usually by specifying the appropriate compiler flag, as discussed in the [Compiling](compiling.md) section later.

OpenMP compiler directives are used for various purposes:
* Spawning a parallel region
* Dividing blocks of code among threads
* Distributing loop iterations between threads
* Serializing sections of code
* Synchronization of work among threads 

Compiler directives have the following syntax:

<pre><i>sentinel       directive-name      [clause, ...]</i></pre>

For example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-1wig{font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Fortran</span></th>
    <th class="tg-1wig">!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(BETA,PI) </th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-xq0d"><span style="background-color:#98ABCE">C/C++</span></td>
    <td class="tg-1wig">#pragma omp parallel default(shared) private(beta,pi)</td>
  </tr>
</tbody>
</table>

Compiler directives are covered in detail later. 

## Run-time Library Routines:

The OpenMP API includes an ever-growing number of run-time library routines.

These routines are used for a variety of purposes:
* Setting and querying the number of threads
* Querying a thread's unique identifier (thread ID), a thread's ancestor's identifier, the thread team size
* Setting and querying the dynamic threads feature
* Querying if in a parallel region, and at what level
* Setting and querying nested parallelism
* Setting, initializing and terminating locks and nested locks
* Querying wall clock time and resolution 

For C/C++, all of the run-time library routines are actual subroutines. For Fortran, some are actually functions, and some are subroutines. For example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-1wig{font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Fortran</span></th>
    <th class="tg-1wig">INTEGER FUNCTION OMP_GET_NUM_THREADS() </th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-xq0d"><span style="background-color:#98ABCE">C/C++</span></td>
    <td class="tg-1wig">#include &lt;omp.h&gt; <br>int omp_get_num_threads(void)</td>
  </tr>
</tbody>
</table>

Note that for C/C++, you usually need to include the `<omp.h>` header file.

Fortran routines are not case sensitive, but C/C++ routines are.

The run-time library routines are briefly discussed as an overview in the [Run-Time Library Routines](run_time_routines.md) section, and in more detail in [Appendix A](appendix_a.md). 

## Environment Variables:

OpenMP provides several environment variables for controlling the execution of parallel code at run-time.

These environment variables can be used to control such things as:
* Setting the number of threads
* Specifying how loop interations are divided
* Binding threads to processors
* Enabling/disabling nested parallelism; setting the maximum levels of nested parallelism
* Enabling/disabling dynamic threads
* Setting thread stack size
* Setting thread wait policy 

Setting OpenMP environment variables is done the same way you set any other environment variables, and depends upon which shell you use. For example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-1wig{font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">csh/tcsh</span></th>
    <th class="tg-1wig">setenv OMP_NUM_THREADS 8 </th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-xq0d"><span style="background-color:#98ABCE">sh/bash</span></td>
    <td class="tg-1wig">export OMP_NUM_THREADS=8</td>
  </tr>
</tbody>
</table>

OpenMP environment variables are discussed in the [Environment Variables](env_vars.md) section later. 

## Example OpenMP Code Structure:

### Fortran - General Code Structure

<pre>
       PROGRAM HELLO

       INTEGER VAR1, VAR2, VAR3

<i>       Serial code 
             .
             .
             .

       Beginning of parallel section. Fork a team of threads. 
       Specify variable scoping</i> 

<b>!$OMP PARALLEL PRIVATE(VAR1, VAR2) SHARED(VAR3)

       <i>Parallel section executed by all threads 
                 .
       Other OpenMP directives
                 .
       Run-time Library calls
                 .
       All threads join master thread and disband </i>

!$OMP END PARALLEL</b>
<i>
       Resume serial code 
             .
             .
             .
</i>
       END
</pre>

### C / C++ - General Code Structure

<pre><b>#include &lt;omp.h&gt;</b>

main ()  {

int var1, var2, var3;

<i>Serial code 
      .
      .
      .

Beginning of parallel section. Fork a team of threads.
Specify variable scoping</i> 

<b>#pragma omp parallel private(var1, var2) shared(var3)
{

<i>Parallel section executed by all threads 
                 .
Other OpenMP directives
                 .
Run-time Library calls
                 .
All threads join master thread and disband</i></b>

 }  

<i>Resume serial code 
      .
      .
      .
</i>
 }
</pre>