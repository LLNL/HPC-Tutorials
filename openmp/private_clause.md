---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: PRIVATE Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The PRIVATE clause declares variables in its list to be private to each thread.

## Format:

### Fortran	
```
PRIVATE (list)
```

### C/C++	
```
private (list)
```

## Notes:

PRIVATE variables behave as follows:
    * A new object of the same type is declared once for each thread in the team
    * All references to the original object are replaced with references to the new object
    * Variables declared PRIVATE should be assumed to be uninitialized for each thread

Comparison between PRIVATE and THREADPRIVATE:


