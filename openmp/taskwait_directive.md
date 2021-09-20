---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs: TASKWAIT Directive"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

* New with OpenMP 3.1

* The TASKWAIT construct specifies a wait on the completion of child tasks generated since the beginning of the current task.

## Format:

### Fortran	
```
!$OMP TASKWAIT
```

### C/C++	
<pre>
#pragma omp taskwait  <i>newline</i>
</pre>

## Restrictions:

Because the taskwait construct does not have a C language statement as part of its syntax, there are some restrictions on its placement within a program. The taskwait directive may be placed only at a point where a base language statement is allowed. The taskwait directive may not be used in place of the statement following an if, while, do, switch, or label. See the OpenMP 3.1 specifications document for details.