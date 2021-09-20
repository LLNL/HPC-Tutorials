---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: MASTER Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The MASTER directive specifies a region that is to be executed only by the master thread of the team. All other threads on the team skip this section of code

There is no implied barrier associated with this directive

## Format:

### Fortran

<pre>
!$OMP MASTER

   <i>block</i>

!$OMP END MASTER
</pre>

### C/C++	
<pre>
#pragma omp master <i> newline </i>

 <i>  structured_block </i>
</pre>

## Restrictions:

It is illegal to branch into or out of MASTER block.