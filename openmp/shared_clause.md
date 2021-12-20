---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: SHARED Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The SHARED clause declares variables in its list to be shared among all threads in the team.

## Format:

### Fortran	
<pre>
SHARED <i>(list)</i>
</pre>

### C/C++	
<pre>
shared <i>(list)</i>
</pre>

## Notes:

A shared variable exists in only one memory location and all threads can read or write to that address.

It is the programmer's responsibility to ensure that multiple threads properly access SHARED variables (such as via CRITICAL sections).
