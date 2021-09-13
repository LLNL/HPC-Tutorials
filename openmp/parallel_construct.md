---
layout: tutorial_page
title: "OpenMP Directives: Parallel Region Construct"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose

A parallel region is a block of code that will be executed by multiple threads. This is the fundamental OpenMP parallel construct.

## Format

### Fortran

```
!$OMP PARALLEL [clause ...] 
               IF (scalar_logical_expression) 
               PRIVATE (list) 
               SHARED (list) 
               DEFAULT (PRIVATE | FIRSTPRIVATE | SHARED | NONE) 
               FIRSTPRIVATE (list) 
               REDUCTION (operator: list) 
               COPYIN (list) 
               NUM_THREADS (scalar-integer-expression)

   block

!$OMP END PARALLEL
```

### C/C++


```
#pragma omp parallel [clause ...]  newline 
                     if (scalar_expression) 
                     private (list) 
                     shared (list) 
                     default (shared | none) 
                     firstprivate (list) 
                     reduction (operator: list) 
                     copyin (list) 
                     num_threads (integer-expression)

 
   structured_block
```

## Notes

When a thread reaches a PARALLEL directive, it creates a team of threads and becomes the master of the team. The master is a member of that team and has thread number 0 within that team.

Starting from the beginning of this parallel region, the code is duplicated and all threads will execute that code.

There is an implied barrier at the end of a parallel section. Only the master thread continues execution past this point.

If any thread terminates within a parallel region, all threads in the team will terminate, and the work done up until that point is undefined.

## How many threads?

The number of threads in a parallel region is determined by the following factors, in order of precedence:

1. Evaluation of the `IF` clause
2. Setting of the `NUM_THREADS` clause
3. Use of the `omp_set_num_threads()` library function
4. Setting of the `OMP_NUM_THREADS` environment variable
5. Implementation default - usually the number of CPUs on a node, though it could be dynamic.

Threads are numbered from `0` (master thread) to `N-1`.

## Dynamic Threads

Use the `omp_get_dynamic()` library function to determine if dynamic threads are enabled.

If supported, the two methods available for enabling dynamic threads are:

1. The `omp_set_dynamic()` library routine
2. Setting of the `OMP_DYNAMIC` environment variable to TRUE

If not supported, a parallel region nested within another parallel region results in the creation of a new team, consisting of one thread, by default.

## Clauses:

**IF** clause: If present, it must evaluate to `.TRUE.` (Fortran) or non-zero (C/C++) in order for a team of threads to be created. Otherwise, the region is executed serially by the master thread.

The remaining clauses are described in detail later, in the [Data Scope Attribute Clauses](data_scope.md) section.

## Restrictions:

* A parallel region must be a structured block that does not span multiple routines or code files.

* It is illegal to branch (goto) into or out of a parallel region.

* Only a single `IF` clause is permitted.

* Only a single `NUM_THREADS` clause is permitted.

* A program must not depend upon the ordering of the clauses.

## Example: Parallel Region

### Fortran

```
       PROGRAM HELLO

       INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM

      # Fork a team of threads with each thread having a private TID variable
!$OMP PARALLEL PRIVATE(TID)

      # Obtain and print thread id
      TID = OMP_GET_THREAD_NUM()
      PRINT *, 'Hello World from thread = ', TID

      # Only master thread does this
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
      END IF

      # All threads join master thread and disband
!$OMP END PARALLEL

       END
```

### C/C++

```
#include <omp.h>

main ()  {

int nthreads, tid;

/* Fork a team of threads with each thread having a private tid variable */
#pragma omp parallel private(tid)
  {

  /* Obtain and print thread id */
  tid = omp_get_thread_num();
  printf("Hello World from thread = %d\n", tid);

  /* Only master thread does this */
  if (tid == 0) 
    {
    nthreads = omp_get_num_threads();
    printf("Number of threads = %d\n", nthreads);
    }

  }  /* All threads join master thread and terminate */

}
```

























#





