---
layout: tutorial_page
title: "Exercise 1"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview

* Login to the workshop cluster using your workshop username and OTP token
* Copy the exercise files to your home directory
* Familiarize yourself with LC's OpenMP environment
* Write a simple "Hello World" OpenMP program
* Successfully compile your program
* Successfully run your program
* Modify the number of threads used to run your program

## 1. Login to the workshop machine

Workshops differ in how this is done. The instructor will go over this beforehand.

## 2. Copy the example files

1. In your home directory, create a subdirectory for the example codes and then cd to it.
```
    mkdir openMP
    cd  openMP 
```

2. Then, copy either the Fortran or the C version of the parallel OpenMP exercise files to your openMP subdirectory:
#### C:
```	
    cp  /usr/global/docs/training/blaise/openMP/C/*  ~/openMP
```
#### Fortran:
```
    cp  /usr/global/docs/training/blaise/openMP/Fortran/*  ~/openMP
```

## 3. List the contents of your OpenMP subdirectory

You should notice the following files. ***Note:*** Most of these are simple example files. Their primary purpose is to demonstrate the basics of how to parallelize a code with OpenMP. Most execute in a second or two.

{% raw %}

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-oqcz{background-color:#869AC3;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-oqcz">C Files</th>
    <th class="tg-oqcz">Fortran Files</th>
    <th class="tg-oqcz" colspan="2">Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_hello.c">omp_hello.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_hello.f">omp_hello.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Hello world</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_workshare1.c">omp_workshare1.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_workshare1.f">omp_workshare1.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Loop work-sharing</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_workshare2.c">omp_workshare2.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_workshare2.f">omp_workshare2.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Sections work-sharing</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_reduction.c">omp_reduction.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_reduction.f">omp_reduction.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Combined parallel loop reduction</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_orphan.c">omp_orphan.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_orphan.f">omp_orphan.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Orphaned parallel loop reduction</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_mm.c">omp_mm.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_mm.f">omp_mm.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Matrix multiply</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_getEnvInfo.c">omp_getEnvInfo.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_getEnvInfo.f">omp_getEnvInfo.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Get and print environment information</span></td>
  </tr>
	<tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/ser_pi_calc.c">ser_pi_calc.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_serPi_calc.f">ser_pi_calc.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Serial Pi calculation</span></td>
  </tr>
  <tr>
    <td class="tg-0lax"><a href="/openmp/code_examples/C/omp_bug1.c">omp_bug1.c </a><br><a href="/openmp/code_examples/C/omp_bug1fix.c">omp_bug1fix.c </a><br><a href="/openmp/code_examples/C/omp_bug2.c">omp_bug2.c </a><br><a href="/openmp/code_examples/C/omp_bug3.c">omp_bug3.c </a><br><a href="/openmp/code_examples/C/omp_bug4.c">omp_bug4.c </a><br><a href="/openmp/code_examples/C/omp_bug4fix">omp_bug4fix </a><br><a href="/openmp/code_examples/C/omp_bug5.c">omp_bug5.c </a><br><a href="/openmp/code_examples/C/omp_bug5fix.c">omp_bug5fix.c </a><br><a href="/openmp/code_examples/C/omp_bug6.c">omp_bug6.c</a></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug1.f">omp_bug1.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug1fix.f">omp_bug1fix.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug2.f">omp_bug2.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug3.f">omp_bug3.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug4.f">omp_bug4.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug4fix">omp_bug4fix </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug5.f">omp_bug5.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug5fix.f">omp_bug5fix.f </a></span><br><span style="font-weight:normal;font-style:normal;color:#000"><a href="/openmp/code_examples/Fortran/omp_bug6.f">omp_bug6.f</a></span></td>
    <td class="tg-0lax" colspan="2"><span style="font-weight:normal;font-style:normal;color:#000">Programs with bugs</span></td>
  </tr>
</tbody>
</table>

{% endraw %}


## 4. Compilers - What's Available?

1. Visit the [Compilers at LC webpage](https://hpc.llnl.gov/software/development-environment-software/compilers).

2. You can also view the available compilers in the [Compilers section](https://hpc.llnl.gov/training/tutorials/livermore-computing-linux-commodity-clusters-overview-part-one#Compilers) of the Linux Clusters Overview tutorial.

3. Now, in your cluster login window, try the `module avail` command to display available compilers. You should see GNU, Intel and PGI compilers - several versions of each.
	* Question: Which version is the default version?
	* Answer: Look for the "(D)".

## 5. Create, compile and run an OpenMP "Hello world" program
1\. Using your favorite text editor (vi/vim, emacs, nedit, gedit, nano...) open a new file - call it whatever you'd like.

2\. Create a simple OpenMP program that does the following:
* Creates a parallel region
* Has each thread in the parallel region obtain its thread id
* Has each thread print "Hello World" along with its unique thread id
* Has the master thread only, obtain and then print the total number of threads

If you need help, see the provided [omp_hello.c](code_examples/C/omp_hello.c) or [omp_hello.f](code_examples/Fortran/omp_hello.f) file.

3\. Using your choice of compiler (see above section 4), compile your hello world OpenMP program. This may take several attempts if there are any code errors. For example:
<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-oqcz{background-color:#869AC3;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-oqcz">C:</th>
    <th class="tg-oqcz">Fortran:</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax"><span style="font-weight:bold">icc -openmp omp_hello.c -o hello</span><br><span style="font-weight:bold">pgcc -mp omp_hello.c -o hello</span><br><span style="font-weight:bold">gcc -fopenmp omp_hello.c -o hello</span><br></td>
    <td class="tg-0lax"><span style="font-weight:bold">ifort -openmp omp_hello.f -o hello</span><br><span style="font-weight:bold">pgf90 -mp omp_hello.f -o hello</span><br><span style="font-weight:bold">gfortran -fopenmp omp_hello.f -o hello</span><br></td>
  </tr>
</tbody>
</table>



When you get a clean compile, proceed.

4\. Run your hello executable and notice its output.
* Is it what you expected? As a comparison, you can compile and run the provided omp_hello.c or omp_hello.f example program.
* How many threads were created? By default, the Intel and GNU compilers will create 1 thread for each core. The PGI compiler will create only 1 thread total.

5\. Notes:
* For the remainder of this exercise, you can use the compiler command of your choice unless indicated otherwise.
* Compilers will differ in which warnings they issue, but all can be ignored for this exercise. Errors are different, of course.


## 6. Vary the number of threads and re-run Hello World

Explicitly set the number of threads to use by means of the `OMP_NUM_THREADS` environment variable:

```
setenv OMP_NUM_THREADS 8
```

Your output should look something like below.

```
Hello World from thread = 0
Hello World from thread = 3
Hello World from thread = 2
Number of threads = 8
Hello World from thread = 6
Hello World from thread = 1
Hello World from thread = 4
Hello World from thread = 7
Hello World from thread = 5
```

Run your program several times and observe the order of print statements. Notice that the order of output is more or less random.



