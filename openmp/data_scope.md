---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

* Also called Data-sharing Attribute Clauses

* An important consideration for OpenMP programming is the understanding and use of data scoping.

* Because OpenMP is based upon the shared memory programming model, most variables are shared by default.

* Global variables include:
    * Fortran: COMMON blocks, SAVE variables, MODULE variables
    * C: File scope variables, static

* Private variables include:
    * Loop index variables
    * Stack variables in subroutines called from parallel regions
    * Fortran: Automatic variables within a statement block

The OpenMP Data Scope Attribute Clauses are used to explicitly define how variables should be scoped. They include:
    * PRIVATE
    * FIRSTPRIVATE
    * LASTPRIVATE
    * SHARED
    * DEFAULT
    * REDUCTION
    * COPYIN

* Data Scope Attribute Clauses are used in conjunction with several directives (PARALLEL, DO/for, and SECTIONS) to control the scoping of enclosed variables.

* These constructs provide the ability to control the data environment during execution of parallel constructs.
    * They define how and which data variables in the serial section of the program are transferred to the parallel sections of the program (and back)
    * They define which variables will be visible to all threads in the parallel sections and which variables will be privately allocated to all threads.

* Data Scope Attribute Clauses are effective only within their lexical/static extent.

* **Important**: Please consult the latest OpenMP specs for important details and discussion on this topic.

* A [Clauses / Directives Summary Table](clauses_directives_summary.md) is provided for convenience.
