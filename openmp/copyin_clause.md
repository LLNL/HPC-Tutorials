---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: COPYIN Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:
The COPYIN clause provides a means for assigning the same value to THREADPRIVATE variables for all threads in the team.

## Format:

### Fortran	
<pre>
COPYIN <i>(list)</i>
</pre>

### C/C++	
<pre>
copyin  <i>(list)</i>
</pre>

## Notes:

List contains the names of variables to copy. In Fortran, the list can contain both the names of common blocks and named variables.
The master thread variable is used as the copy source. The team threads are initialized with its value upon entry into the parallel construct.
