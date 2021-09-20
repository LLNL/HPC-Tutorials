---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: BARRIER Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The BARRIER directive synchronizes all threads in the team.

When a BARRIER directive is reached, a thread will wait at that point until all other threads have reached that barrier. All threads then resume executing in parallel the code that follows the barrier.

## Format:

### Fortran	
```
!$OMP BARRIER
```

###C/C++	
<pre>
#pragma omp barrier  <i>newline</i>
</pre>

## Restrictions:

All threads in a team (or none) must execute the BARRIER region.

The sequence of work-sharing regions and barrier regions encountered must be the same for every thread in a team.