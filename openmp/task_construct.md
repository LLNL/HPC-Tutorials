---
layout: tutorial_page
title: "OpenMP Directives: TASK Construct"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The TASK construct defines an explicit task, which may be executed by the encountering thread, or deferred for execution by any other thread in the team.

The data environment of the task is determined by the data sharing attribute clauses.

Task execution is subject to task scheduling - see the OpenMP 3.1 specification document for details.

Also see the OpenMP 3.1 documentation for the associated taskyield and taskwait directives.

## Format:

### Fortran:

<pre>
!$OMP TASK <i>[clause ...] </i>
             IF <i>(scalar logical expression)</i> 
             FINAL <i>(scalar logical expression) </i>
             UNTIED
             DEFAULT (PRIVATE | FIRSTPRIVATE | SHARED | NONE)
             MERGEABLE
             PRIVATE <i>(list)</i> 
             FIRSTPRIVATE <i>(list)</i> 
             SHARED <i>(list)</i> 

   <i>block</i>

!$OMP END TASK
</pre>

### C/C++:

<pre>
#pragma omp task <i>[clause ...]  newline </i>
                   if <i>(scalar expression) </i>
                   final <i>(scalar expression) </i>
                   untied
                   default (shared | none)
                   mergeable
                   private <i>(list) </i>
                   firstprivate <i>(list) </i>
                   shared <i>(list) 

     structured_block</i>
</pre>

## Clauses and Restrictions:

Please consult the OpenMP 3.1 specifications document for details.