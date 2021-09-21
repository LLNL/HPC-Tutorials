---
layout: tutorial_page
title: "OpenMP Directives: Work-Sharing Constructs: WORKSHARE Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

* **Fortran only**
* The WORKSHARE directive divides the execution of the enclosed structured block into separate units of work, each of which is executed only once.
* The structured block must consist of only the following:
    * array assignments
    * scalar assignments
    * FORALL statements
    * FORALL constructs
    * WHERE statements
    * WHERE constructs
    * atomic constructs
    * critical constructs
    * parallel constructs

See the OpenMP API documentation for additional information, particularly for what comprises a "unit of work".

## Format:

### Fortran:

<pre>
!$OMP WORKSHARE

   <i>structured block</i>

!$OMP END WORKSHARE [ NOWAIT ]
</pre>

## Restrictions:

The construct must not contain any user defined function calls unless the function is ELEMENTAL.

## Example: WORKSHARE Directive

Simple array and scalar assigments shared by the team of threads. A unit of work would include:
    * Any scalar assignment
    * For array assignment statements, the assignment of each element is a unit of work

### Fortran:

<pre>
      PROGRAM WORKSHARE
 
      INTEGER N, I, J
      PARAMETER (N=100)
      REAL AA(N,N), BB(N,N), CC(N,N), DD(N,N), FIRST, LAST
 
!     Some initializations
      DO I = 1, N
        DO J = 1, N
          AA(J,I) = I * 1.0
          BB(J,I) = J + 1.0
        ENDDO
      ENDDO
 <b>!$OMP PARALLEL SHARED(AA,BB,CC,DD,FIRST,LAST)

!$OMP WORKSHARE</b>
      CC = AA * BB
      DD = AA + BB
      FIRST = CC(1,1) + DD(1,1)
      LAST = CC(N,N) + DD(N,N)
<b>!$OMP END WORKSHARE NOWAIT

!$OMP END PARALLEL</b>
 
      END
</pre>
