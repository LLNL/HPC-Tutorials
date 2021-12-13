---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: DEFAULT Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The DEFAULT clause allows the user to specify a default scope for all variables in the lexical extent of any parallel region.

## Format:

### Fortran	
```
DEFAULT (PRIVATE | FIRSTPRIVATE | SHARED | NONE)
```

### C/C++	
```
default (shared | none)
```

## Notes:

* Specific variables can be exempted from the default using the PRIVATE, SHARED, FIRSTPRIVATE, LASTPRIVATE, and REDUCTION clauses.

* The C/C++ OpenMP specification does not include private or firstprivate as a possible default. However, actual implementations may provide this option.

* Using NONE as a default requires that the programmer explicitly scope all variables.

## Restrictions:

Only one DEFAULT clause can be specified on a PARALLEL directive.
