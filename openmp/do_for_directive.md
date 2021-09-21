---
layout: tutorial_page
title: "OpenMP Directives: Work-Sharing Constructs: DO / for Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose

The DO / for directive specifies that the iterations of the loop immediately following it must be executed in parallel by the team. This assumes a parallel region has already been initiated, otherwise it executes in serial on a single processor.

## Format:

### Fortran:

<pre>
!$OMP DO <i>[clause ...] </i>
         SCHEDULE <i>(type [,chunk]) </i>
         ORDERED 
         PRIVATE <i>(list) </i>
         FIRSTPRIVATE <i>(list) </i>
         LASTPRIVATE <i>(list) </i>
         SHARED <i>(list) </i>
         REDUCTION <i>(operator | intrinsic : list) </i>
         COLLAPSE <i>(n) 

   do_loop</i>

!$OMP END DO  [ NOWAIT ]
</pre>

### C/C++:

<pre>
#pragma omp for <i>[clause ...]  newline </i>
                schedule <i>(type [,chunk]) </i>
                ordered
                private <i>(list) </i>
                firstprivate <i>(list) </i>
                lastprivate <i>(list) </i>
                shared <i>(list) </i>
                reduction <i>(operator: list)</i> 
                collapse <i>(n) </i>
                nowait 

   <i>for_loop</i>
</pre>

## Clauses:

### SCHEDULE: 

Describes how iterations of the loop are divided among the threads in the team. The default schedule is implementation dependent. For a discussion on how one type of scheduling may be more optimal than others, see https://forum.openmp.org/viewtopic.php?t=83.

#### STATIC
Loop iterations are divided into pieces of size chunk and then statically assigned to threads. If chunk is not specified, the iterations are evenly (if possible) divided contiguously among the threads.

#### DYNAMIC
Loop iterations are divided into pieces of size chunk, and dynamically scheduled among the threads; when a thread finishes one chunk, it is dynamically assigned another. The default chunk size is 1.

#### GUIDED

Iterations are dynamically assigned to threads in blocks as threads request them until no blocks remain to be assigned. Similar to DYNAMIC except that the block size decreases each time a parcel of work is given to a thread. The size of the initial block is proportional to:

`number_of_iterations / number_of_threads`

Subsequent blocks are proportional to

`number_of_iterations_remaining / number_of_threads`

The chunk parameter defines the minimum block size. The default chunk size is 1.

#### RUNTIME
The scheduling decision is deferred until runtime by the environment variable OMP_SCHEDULE. It is illegal to specify a chunk size for this clause.

#### AUTO
The scheduling decision is delegated to the compiler and/or runtime system.

### NO WAIT / nowait: 

If specified, then threads do not synchronize at the end of the parallel loop.

### ORDERED: 

Specifies that the iterations of the loop must be executed as they would be in a serial program.

### COLLAPSE: 

Specifies how many loops in a nested loop should be collapsed into one large iteration space and divided according to the schedule clause. The sequential execution of the iterations in all associated loops determines the order of the iterations in the collapsed iteration space.


Other clauses are described in detail later, in the [Data Scope Attribute Clauses section](data_scope.md).

## Restrictions:

The DO loop can not be a DO WHILE loop, or a loop without loop control. Also, the loop iteration variable must be an integer and the loop control parameters must be the same for all threads.

Program correctness must not depend upon which thread executes a particular iteration.

It is illegal to branch (goto) out of a loop associated with a DO/for directive.

The chunk size must be specified as a loop invarient integer expression, as there is no synchronization during its evaluation by different threads.

ORDERED, COLLAPSE and SCHEDULE clauses may appear once each.

See the OpenMP specification document for additional restrictions.

## Example: DO / for Directive

Simple vector-add program
* Arrays A, B, C, and variable N will be shared by all threads. 
* Variable I will be private to each thread; each thread will have its own unique copy.
* The iterations of the loop will be distributed dynamically in CHUNK sized pieces.
* Threads will not synchronize upon completing their individual pieces of work (NOWAIT).

### Fortran - DO Directive Example

<pre>
      PROGRAM VEC_ADD_DO

      INTEGER N, CHUNKSIZE, CHUNK, I
      PARAMETER (N=1000) 
      PARAMETER (CHUNKSIZE=100) 
      REAL A(N), B(N), C(N)

!     Some initializations
      DO I = 1, N
        A(I) = I * 1.0
        B(I) = A(I)
      ENDDO
      CHUNK = CHUNKSIZE
        
<b>!$OMP PARALLEL SHARED(A,B,C,CHUNK) PRIVATE(I)</b>

<b>!$OMP DO SCHEDULE(DYNAMIC,CHUNK)</b>
      DO I = 1, N
         C(I) = A(I) + B(I)
      ENDDO
<b>!$OMP END DO NOWAIT</b>

<b>!$OMP END PARALLEL</b>

      END
</pre>

### C/C++ - for Directive Example

<pre>
<b>#include &lt;omp.h&gt;</b>
#define CHUNKSIZE 100
#define N     1000

main ()  
{

int i, chunk;
float a[N], b[N], c[N];

/* Some initializations */
for (i=0; i < N; i++)
  a[i] = b[i] = i * 1.0;
chunk = CHUNKSIZE;

<b>#pragma omp parallel shared(a,b,c,chunk) private(i)</b>
  {

<b>  #pragma omp for schedule(dynamic,chunk) nowait</b>
  for (i=0; i < N; i++)
    c[i] = a[i] + b[i];

  }  /* end of parallel section */

}
</pre>