---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: CRITICAL Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:
The CRITICAL directive specifies a region of code that must be executed by only one thread at a time.

## Format:

### Fortran	
<pre>
!$OMP CRITICAL <i>[ name ]</i>

   <i>block</i>

!$OMP END CRITICAL <i>[ name ]</i>
</pre>

### C/C++	
<pre>
#pragma omp critical <i>[ name ]  newline</i>

   <i>structured_block</i>
</pre>

## Notes:

If a thread is currently executing inside a CRITICAL region and another thread reaches that CRITICAL region and attempts to execute it, it will block until the first thread exits that CRITICAL region.

The optional name enables multiple different CRITICAL regions to exist:

Names act as global identifiers. Different CRITICAL regions with the same name are treated as the same region.
All CRITICAL sections which are unnamed, are treated as the same section.

## Restrictions:

It is illegal to branch into or out of a CRITICAL block.

**Fortran only:** The names of critical constructs are global entities of the program. If a name conflicts with any other entity, the behavior of the program is unspecified.

## Example: CRITICAL Construct

All threads in the team will attempt to execute in parallel, however, because of the CRITICAL construct surrounding the increment of x, only one thread will be able to read/increment/write x at any time.

### Fortran - CRITICAL Directive Example
<pre>
      PROGRAM CRITICAL

      INTEGER X
      X = 0

<b>!$OMP PARALLEL SHARED(X)</b>

<b>!$OMP CRITICAL </b>
      X = X + 1
<b>!$OMP END CRITICAL </b>

<b>!$OMP END PARALLEL </b>

      END

</pre>

### C / C++ - critical Directive Example

<pre>
#include <omp.h>

main()
{

int x;
x = 0;

<b>#pragma omp parallel shared(x) </b>
  {

  <b>#pragma omp critical </b>
  x = x + 1;

  }  /* end of parallel section */

}
</pre>