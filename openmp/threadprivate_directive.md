---
layout: tutorial_page
title: "OpenMP Directives: THREADPRIVATE Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The THREADPRIVATE directive is used to make global file scope variables (C/C++) or common blocks (Fortran) local and persistent to a thread through the execution of multiple parallel regions.

## Format:

### Fortran
<pre>!$OMP THREADPRIVATE (/cb/, ...) <i> cb is the name of a common block </i></pre>

### C/C++	
<pre>
#pragma omp threadprivate <i> (list) </i>
</pre>

## Notes:

The directive must appear after the declaration of listed variables/common blocks. Each thread then gets its own copy of the variable/common block, so data written by one thread is not visible to other threads. For example:

## Example:

### Fortran - THREADPRIVATE Directive Example
<pre>
      PROGRAM THREADPRIV
 
      INTEGER A, B, I, TID, OMP_GET_THREAD_NUM
      REAL*4 X
      COMMON /C1/ A
<b> 
!$OMP THREADPRIVATE(/C1/, X) 
</b> 
C     Explicitly turn off dynamic threads
      CALL OMP_SET_DYNAMIC(.FALSE.)
 
      PRINT *, '1st Parallel Region:'
<b>!$OMP PARALLEL PRIVATE(B, TID) </b>
      TID = OMP_GET_THREAD_NUM()
      A = TID
      B = TID
      X = 1.1 * TID + 1.0
      PRINT *, 'Thread',TID,':   A,B,X=',A,B,X
<b>!$OMP END PARALLEL </b>
 
      PRINT *, '************************************'
      PRINT *, 'Master thread doing serial work here'
      PRINT *, '************************************'
 
      PRINT *, '2nd Parallel Region: '
<b>!$OMP PARALLEL PRIVATE(TID) </b>
      TID = OMP_GET_THREAD_NUM()
      PRINT *, 'Thread',TID,':   A,B,X=',A,B,X
<b>!$OMP END PARALLEL </b>
 
      END
</pre>

#### Output:

```
 1st Parallel Region:
 Thread 0 :   A,B,X= 0 0 1.000000000
 Thread 1 :   A,B,X= 1 1 2.099999905
 Thread 3 :   A,B,X= 3 3 4.300000191
 Thread 2 :   A,B,X= 2 2 3.200000048
 ************************************
 Master thread doing serial work here
 ************************************
 2nd Parallel Region: 
 Thread 0 :   A,B,X= 0 0 1.000000000
 Thread 2 :   A,B,X= 2 0 3.200000048
 Thread 3 :   A,B,X= 3 0 4.300000191
 Thread 1 :   A,B,X= 1 0 2.099999905
 ```

### C/C++ - threadprivate Directive Example

<pre><b>#include &lt;omp.h&gt;</b>
 
int  a, b, i, tid;
float x;

<b>#pragma omp threadprivate(a, x)
</b> 
main ()  {
 
/* Explicitly turn off dynamic threads */
<b>  omp_set_dynamic(0);</b> 

  printf("1st Parallel Region:\n");
<b>#pragma omp parallel private(b,tid)</b>
  {
  tid = omp_get_thread_num();
  a = tid;
  b = tid;
  x = 1.1 * tid +1.0;
  printf("Thread %d:   a,b,x= %d %d %f\n",tid,a,b,x);
  }  /* end of parallel section */
 
  printf("************************************\n");
  printf("Master thread doing serial work here\n");
  printf("************************************\n");
 
  printf("2nd Parallel Region:\n");
<b>#pragma omp parallel private(tid)</b>
  {
  tid = omp_get_thread_num();
  printf("Thread %d:   a,b,x= %d %d %f\n",tid,a,b,x);
  }  /* end of parallel section */

}
</pre>

#### Output:

```
1st Parallel Region:
Thread 0:   a,b,x= 0 0 1.000000
Thread 2:   a,b,x= 2 2 3.200000
Thread 3:   a,b,x= 3 3 4.300000
Thread 1:   a,b,x= 1 1 2.100000
************************************
Master thread doing serial work here
************************************
2nd Parallel Region:
Thread 0:   a,b,x= 0 0 1.000000
Thread 3:   a,b,x= 3 0 4.300000
Thread 1:   a,b,x= 1 0 2.100000
Thread 2:   a,b,x= 2 0 3.200000
```

## Restrictions:

* Data in THREADPRIVATE objects is guaranteed to persist only if the dynamic threads mechanism is "turned off" and the number of threads in different parallel regions remains constant. The default setting of dynamic threads is undefined.

* The THREADPRIVATE directive must appear after every declaration of a thread private variable/common block.

* Fortran: only named common blocks can be made THREADPRIVATE.
