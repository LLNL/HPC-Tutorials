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

```
!$OMP TASK [clause ...] 
             IF (scalar logical expression) 
             FINAL (scalar logical expression) 
             UNTIED
             DEFAULT (PRIVATE | FIRSTPRIVATE | SHARED | NONE)
             MERGEABLE
             PRIVATE (list) 
             FIRSTPRIVATE (list) 
             SHARED (list) 

   block

!$OMP END TASK
```

### C/C++:

```
#pragma omp task [clause ...]  newline 
                   if (scalar expression) 
                   final (scalar expression) 
                   untied
                   default (shared | none)
                   mergeable
                   private (list) 
                   firstprivate (list) 
                   shared (list) 

     structured_block
```

## Clauses and Restrictions:

Please consult the OpenMP 3.1 specifications document for details.