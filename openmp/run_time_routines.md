---
layout: tutorial_page
title: "Run-Time Library Routines"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Overview:

* The OpenMP API includes an ever-growing number of run-time library routines.

* These routines are used for a variety of purposes as shown in the table below:

{% raw %}<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-875e{color:#339;text-align:left;text-decoration:underline;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Routine</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Purpose</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_num_threads/">OMP_SET_NUM_THREADS</a></td>
    <td class="tg-0lax">Sets the number of threads that will be used in the next parallel region</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_num_threads/" target="_blank" rel="noopener noreferrer">OMP_GET_NUM_THREADS</a></td>
    <td class="tg-0lax">Returns the number of threads that are currently in the team executing the parallel region from which it is called</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_max_threads/" target="_blank" rel="noopener noreferrer">OMP_GET_MAX_THREADS</a></td>
    <td class="tg-0lax">Returns the maximum value that can be returned by a call to the OMP_GET_NUM_THREADS function</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_thread_num/" target="_blank" rel="noopener noreferrer">OMP_GET_THREAD_NUM</a></td>
    <td class="tg-0lax">Returns the thread number of the thread, within the team, making this call.</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_thread_limit/" target="_blank" rel="noopener noreferrer">OMP_GET_THREAD_LIMIT</a></td>
    <td class="tg-0lax">Returns the maximum number of OpenMP threads available to a program</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_num_procs/" target="_blank" rel="noopener noreferrer">OMP_GET_NUM_PROCS</a></td>
    <td class="tg-0lax">Returns the number of processors that are available to the program</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_in_parallel/" target="_blank" rel="noopener noreferrer">OMP_IN_PARALLEL</a></td>
    <td class="tg-0lax">Used to determine if the section of code which is executing is parallel or not</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_dynamic/" target="_blank" rel="noopener noreferrer">OMP_SET_DYNAMIC</a></td>
    <td class="tg-0lax">Enables or disables dynamic adjustment (by the run time system) of the number of threads available for execution of parallel regions</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_dynamic/" target="_blank" rel="noopener noreferrer">OMP_GET_DYNAMIC</a></td>
    <td class="tg-0lax">Used to determine if dynamic thread adjustment is enabled or not</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_nested/" target="_blank" rel="noopener noreferrer">OMP_SET_NESTED</a></td>
    <td class="tg-0lax">Used to enable or disable nested parallelism</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_nested/" target="_blank" rel="noopener noreferrer">OMP_GET_NESTED</a></td>
    <td class="tg-0lax">Used to determine if nested parallelism is enabled or not</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_schedule/" target="_blank" rel="noopener noreferrer">OMP_SET_SCHEDULE</a></td>
    <td class="tg-0lax">Sets the loop scheduling policy when "runtime" is used as the schedule kind in the OpenMP directive</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_schedule/" target="_blank" rel="noopener noreferrer">OMP_GET_SCHEDULE</a></td>
    <td class="tg-0lax">Returns the loop scheduling policy when "runtime" is used as the schedule kind in the OpenMP directive</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_max_active_levels/" target="_blank" rel="noopener noreferrer">OMP_SET_MAX_ACTIVE_LEVELS</a></td>
    <td class="tg-0lax">Sets the maximum number of nested parallel regions</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_max_active_levels/" target="_blank" rel="noopener noreferrer">OMP_GET_MAX_ACTIVE_LEVELS</a></td>
    <td class="tg-0lax">Returns the maximum number of nested parallel regions</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_level/" target="_blank" rel="noopener noreferrer">OMP_GET_LEVEL</a></td>
    <td class="tg-0lax">Returns the current level of nested parallel regions</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_ancestor_thread_num/" target="_blank" rel="noopener noreferrer">OMP_GET_ANCESTOR_THREAD_NUM</a></td>
    <td class="tg-0lax">Returns, for a given nested level of the current thread, the thread number of ancestor thread</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_team_size/" target="_blank" rel="noopener noreferrer">OMP_GET_TEAM_SIZE</a></td>
    <td class="tg-0lax">Returns, for a given nested level of the current thread, the size of the thread team</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_active_level/" target="_blank" rel="noopener noreferrer">OMP_GET_ACTIVE_LEVEL</a></td>
    <td class="tg-0lax">Returns the number of nested, active parallel regions enclosing the task that contains the call</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_in_final/" target="_blank" rel="noopener noreferrer">OMP_IN_FINAL</a></td>
    <td class="tg-0lax">Returns true if the routine is executed in the final task region; otherwise it returns false</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_init_lock/" target="_blank" rel="noopener noreferrer">OMP_INIT_LOCK</a></td>
    <td class="tg-0lax">Initializes a lock associated with the lock variable</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_destroy_lock/" target="_blank" rel="noopener noreferrer">OMP_DESTROY_LOCK</a></td>
    <td class="tg-0lax">Disassociates the given lock variable from any locks</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_lock/" target="_blank" rel="noopener noreferrer">OMP_SET_LOCK</a></td>
    <td class="tg-0lax">Acquires ownership of a lock</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_unset_lock/" target="_blank" rel="noopener noreferrer">OMP_UNSET_LOCK</a></td>
    <td class="tg-0lax">Releases a lock</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_test_lock/" target="_blank" rel="noopener noreferrer">OMP_TEST_LOCK</a></td>
    <td class="tg-0lax">Attempts to set a lock, but does not block if the lock is unavailable</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_init_lock/" target="_blank" rel="noopener noreferrer">OMP_INIT_NEST_LOCK</a></td>
    <td class="tg-0lax">Initializes a nested lock associated with the lock variable</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_destroy_lock/" target="_blank" rel="noopener noreferrer">OMP_DESTROY_NEST_LOCK</a></td>
    <td class="tg-0lax">Disassociates the given nested lock variable from any locks</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_set_lock/" target="_blank" rel="noopener noreferrer">OMP_SET_NEST_LOCK</a></td>
    <td class="tg-0lax">Acquires ownership of a nested lock</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_unset_lock/" target="_blank" rel="noopener noreferrer">OMP_UNSET_NEST_LOCK</a></td>
    <td class="tg-0lax">Releases a nested lock</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_test_lock/" target="_blank" rel="noopener noreferrer">OMP_TEST_NEST_LOCK</a></td>
    <td class="tg-0lax">Attempts to set a nested lock, but does not block if the lock is unavailable</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_wtime/" target="_blank" rel="noopener noreferrer">OMP_GET_WTIME</a></td>
    <td class="tg-0lax">Provides a portable wall clock timing routine</td>
  </tr>
  <tr>
    <td class="tg-875e"><a href="https://hpc-tutorials.llnl.gov/openmp/omp_get_wtick/" target="_blank" rel="noopener noreferrer">OMP_GET_WTICK</a></td>
    <td class="tg-0lax">Returns a double-precision floating point value equal to the number of seconds between successive clock ticks</td>
  </tr>
</tbody>
</table>
{% endraw %}


* For C/C++, all of the run-time library routines are actual subroutines. For Fortran, some are actually functions, and some are subroutines. For example:

**Fortran**
```
INTEGER FUNCTION OMP_GET_NUM_THREADS()
```

**C/C++**
```
#include <omp.h>
int omp_get_num_threads(void)
```

* Note that for C/C++, you usually need to include the **<omp.h>** header file.

* Fortran routines are not case sensitive, but C/C++ routines are.

* For the Lock routines/functions:
  * The lock variable must be accessed only through the locking routines
  * For Fortran, the lock variable should be of type integer and of a kind large enough to hold an address.
  * For C/C++, the lock variable must have type `omp_lock_t` or type `omp_nest_lock_t`, depending on the function being used.

* Implementation notes:
  * Implementations may or may not support all OpenMP API features. For example, if nested parallelism is supported, it may be only nominal, in that a nested parallel region may only have one thread.
  * Consult your implementation's documentation for details - or experiment and find out for yourself if you can't find it in the documentation.

* The run-time library routines are discussed in more detail in [Appendix A](appendix_a.md).
