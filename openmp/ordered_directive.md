---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: ORDERED Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

* The ORDERED directive specifies that iterations of the enclosed loop will be executed in the same order as if they were executed on a serial processor.

* Threads will need to wait before executing their chunk of iterations if previous iterations haven't completed yet.

* Used within a DO / for loop with an ORDERED clause.

* The ORDERED directive provides a way to "fine tune" where ordering is to be applied within a loop. Otherwise, it is not required.

## Format:

### Fortran	
<pre>
!$OMP DO ORDERED <i>[clauses...]
   (loop region)</i>

!$OMP ORDERED

   <i>(block)</i>

!$OMP END ORDERED

   <i>(end of loop region)</i>
!$OMP END DO
</pre>

### C/C++	
<pre>
#pragma omp for ordered <i>[clauses...]
   (loop region)</i>

#pragma omp ordered  <i>newline

   structured_block

   (endo of loop region)</i>
</pre>

## Restrictions:

* An ORDERED directive can only appear in the dynamic extent of the following directives:
    * DO or PARALLEL DO (Fortran)
    * for or parallel for (C/C++)

* Only one thread is allowed in an ordered section at any time.

* It is illegal to branch into or out of an ORDERED block.

* An iteration of a loop must not execute the same ORDERED directive more than once, and it must not execute more than one ORDERED directive.

* A loop which contains an ORDERED directive, must be a loop with an ORDERED clause.