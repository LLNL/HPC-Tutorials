---
layout: tutorial_homepage
title: "OpenMP"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

### Table of Contents

1. [Abstract](abstract)
2. [Introduction](introduction)
3. [OpenMP Programming Model](programming_model)
4. [OpenMP API Overview](api_overview)
5. [Compiling OpenMP Programs](compiling)
6. OpenMP Directives
    1. [Fortran Directive Format](fortran_directive_format)
    2. [C/C++ Directive Format](c_directive_format)
    3. [Directive Scoping](directive_scoping)
    4. [PARALLEL Construct](parallel_construct)
    5. [Exercise 1](exercise1)
    6. [Work-Sharing Constructs](work_sharing_constructs)
        1. [DO / for Directive](do_for_directive)
        2. [SECTIONS Directive](sections_directive)
        3. [WORKSHARE Directive](workshare_directive)
        4. [SINGLE Directive](single_directive)
    7. [Combined Parallel Work-Sharing Constructs](combined-parallel-work-sharing)
    8. [TASK Construct](task_construct)
    9. [Exercise 2](exercise2)
    10. [Synchronization Constructs](synchronization_constructs)
        1. [MASTER Directive](master_directive)
        1. [CRITICAL Directive](critical_directive)
        1. [BARRIER Directive](barrier_directive)
        1. [TASKWAIT Directive](taskwait_directive)
        1. [ATOMIC Directive](atomic_directive)
        1. [FLUSH Directive](flush_directive)
        1. [ORDERED Directive](ordered_directive)
    11. [THREADPRIVATE Directive](threadprivate_directive)
    12. [Data Scope Attribute Clauses](data_scope)
        1. [PRIVATE Clause](private_clause)

[Clauses/Directives Summary](clauses_directives_summary)
[Environment Variables](env_vars)
[Run-Time Library Routines](run_time_routines)
[Appendix A: Run-Time Library Routines](appendix_a)
[Directive Binding and Nesting Rules](directive_binding_and_nesting)
Once you have finished the tutorial, please complete our [evaluation form](https://hpc.llnl.gov/training/tutorials/evaluation-form)!
