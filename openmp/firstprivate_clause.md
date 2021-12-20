---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: FIRSTPRIVATE Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The FIRSTPRIVATE clause combines the behavior of the PRIVATE clause with automatic initialization of the variables in its list.

## Format:

### Fortran	
<pre>
FIRSTPRIVATE <i>(list)</i>
</pre>

### C/C++	
<pre>
firstprivate <i>(list)</i>
</pre>

## Notes:

Listed variables are initialized according to the value of their original objects prior to entry into the parallel or work-sharing construct.
