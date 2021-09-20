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