---
layout: tutorial_page
title: "OpenMP Directives: Work-Sharing Constructs: SECTIONS Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The SECTIONS directive is a non-iterative work-sharing construct. It specifies that the enclosed section(s) of code are to be divided among the threads in the team.

Independent SECTION directives are nested within a SECTIONS directive. Each SECTION is executed once by a thread in the team. Different sections may be executed by different threads. It is possible for a thread to execute more than one section if it is quick enough and the implementation permits such.

## Format:

### Fortran:

<pre>
!$OMP SECTIONS <i>[clause ...] </i>
               PRIVATE <i>(list) </i>
               FIRSTPRIVATE <i>(list)</i> 
               LASTPRIVATE <i>(list)</i> 
               REDUCTION <i>(operator | intrinsic : list)</i> 

!$OMP  SECTION 

   <i>block</i>

!$OMP  SECTION 

    <i>block</i> 

!$OMP END SECTIONS  [ NOWAIT ]
</pre>

### C/C++:

<pre>
#pragma omp sections <i>[clause ...]  newline </i>
                     private <i>(list) </i>
                     firstprivate <i>(list)</i> 
                     lastprivate <i>(list) </i>
                     reduction <i>(operator: list)</i> 
                     nowait
  {

  #pragma omp section   <i>newline 

     structured_block</i>

  #pragma omp section   <i>newline 

     structured_block</i>

  }
</pre>

## Clauses:

There is an implied barrier at the end of a SECTIONS directive, unless the NOWAIT/nowait clause is used.
Clauses are described in detail later, in the [Data Scope Attribute Clauses section](data_scope.md).

## Questions:

<details>
  <summary>	What happens if the number of threads and the number of SECTIONs are different? More threads than SECTIONs? Less threads than SECTIONs?</summary>
  
#### ANSWER: 

If there are more threads than sections, some threads will not execute a section and some will.  If there are more sections than threads, the implementation defines how the extra sections are executed.
</details>

 <details>
  <summary>	Which thread executes which SECTION? </summary>
  
#### ANSWER: 

It is up to the implementation to decide which threads will execute a section and which threads will not, and it can vary from execution to execution.
</details>

## Restrictions:

It is illegal to branch (goto) into or out of section blocks.

SECTION directives must occur within the lexical extent of an enclosing SECTIONS directive (no orphan SECTIONs).

## Example: SECTIONS Directive

Simple program demonstrating that different blocks of work will be done by different threads.

### Fortran - SECTIONS Directive Example

<pre>
      PROGRAM VEC_ADD_SECTIONS

      INTEGER N, I
      PARAMETER (N=1000)
      REAL A(N), B(N), C(N), D(N)

!     Some initializations
      DO I = 1, N
        A(I) = I * 1.5
        B(I) = I + 22.35
      ENDDO

<b>!$OMP PARALLEL SHARED(A,B,C,D), PRIVATE(I)

!$OMP SECTIONS

!$OMP SECTION</b>
      DO I = 1, N
         C(I) = A(I) + B(I)
      ENDDO

<b>!$OMP SECTION</b>
      DO I = 1, N
         D(I) = A(I) * B(I)
      ENDDO

<b>!$OMP END SECTIONS NOWAIT

!$OMP END PARALLEL</b>

      END
</pre>

### C/C++ - sections Directive Example

<pre>
<b>#include &lt;omp.h&gt;</b>
#define N     1000

main ()
{

int i;
float a[N], b[N], c[N], d[N];

/* Some initializations */
for (i=0; i < N; i++) {
  a[i] = i * 1.5;
  b[i] = i + 22.35;
  }

<b>#pragma omp parallel shared(a,b,c,d) private(i)</b>
  {

  <b>#pragma omp sections nowait</b>
    {

<b>    #pragma omp section</b>
    for (i=0; i < N; i++)
      c[i] = a[i] + b[i];

<b>    #pragma omp section</b>
    for (i=0; i < N; i++)
      d[i] = a[i] * b[i];

    }  /* end of sections */

  }  /* end of parallel section */

}
</pre>

