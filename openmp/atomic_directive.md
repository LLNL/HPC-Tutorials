---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: ATOMIC Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The ATOMIC directive specifies that a specific memory location must be updated atomically, rather than letting multiple threads attempt to write to it. In essence, this directive provides a mini-CRITICAL section.

## Format:

### Fortran	
<pre>
!$OMP ATOMIC

   <i>statement_expression</i>
</pre>

### C/C++	
<pre>
#pragma omp atomic  <i>newline

   statement_expression</i>
</pre>

## Restrictions:

The directive applies only to a single, immediately following statement.

An atomic statement must follow a specific syntax. See the most recent OpenMP specs for this.