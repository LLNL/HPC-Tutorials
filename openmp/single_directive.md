---
layout: tutorial_page
title: "OpenMP Directives: Work-Sharing Constructs: SINGLE Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:
 
The SINGLE directive specifies that the enclosed code is to be executed by only one thread in the team.

May be useful when dealing with sections of code that are not thread safe (such as I/O)

## Format:

### Fortran:

<pre>
!$OMP SINGLE <i>[clause ...]</i> 
             PRIVATE <i>(list)</i> 
             FIRSTPRIVATE <i>(list) 

   block</i>

!$OMP END SINGLE [ NOWAIT ]
</pre>

### C/C++:

<pre>
#pragma omp single <i>[clause ...]  newline </i>
                   private <i>(list) </i>
                   firstprivate <i>(list)</i> 
                   nowait

     <i>structured_block</i>
</pre>

## Clauses:

Threads in the team that do not execute the SINGLE directive, wait at the end of the enclosed code block, unless a NOWAIT/nowait clause is specified.

Clauses are described in detail later, in the [Data Scope Attribute Clauses section](data_scope.md).

## Restrictions:

It is illegal to branch into or out of a SINGLE block.