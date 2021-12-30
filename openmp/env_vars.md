---
layout: tutorial_page
title: "Environment Variables"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

OpenMP provides the following environment variables for controlling the execution of parallel code.

All environment variable names are uppercase. The values assigned to them are not case sensitive.

## OMP_SCHEDULE
Applies only to DO, PARALLEL DO (Fortran) and `for`, `parallel for` (C/C++) directives which have their schedule clause set to RUNTIME. The value of this variable determines how iterations of the loop are scheduled on processors. For example:
```
setenv OMP_SCHEDULE "guided, 4" 
setenv OMP_SCHEDULE "dynamic"
```

## OMP_NUM_THREADS
Sets the maximum number of threads to use during execution. For example:
```
setenv OMP_NUM_THREADS 8
```

## OMP_DYNAMIC
Enables or disables dynamic adjustment of the number of threads available for execution of parallel regions. Valid values are TRUE or FALSE. For example:

```
setenv OMP_DYNAMIC TRUE
```

Implementation notes:
* Your implementation may or may not support this feature.

## OMP_PROC_BIND
Enables or disables threads binding to processors. Valid values are TRUE or FALSE. For example:
```
setenv OMP_PROC_BIND TRUE
```

Implementation notes:
* Your implementation may or may not support this feature.

## OMP_NESTED
Enables or disables nested parallelism. Valid values are TRUE or FALSE. For example:
```
setenv OMP_NESTED TRUE
```

Implementation notes:
* Your implementation may or may not support this feature. If nested parallelism is supported, it is often only nominal, in that a nested parallel region may only have one thread.

## OMP_STACKSIZE
Controls the size of the stack for created (non-Master) threads. Examples:
```
setenv OMP_STACKSIZE 2000500B 
setenv OMP_STACKSIZE "3000 k " 
setenv OMP_STACKSIZE 10M 
setenv OMP_STACKSIZE " 10 M " 
setenv OMP_STACKSIZE "20 m " 
setenv OMP_STACKSIZE " 1G" 
setenv OMP_STACKSIZE 20000
```
Implementation notes:
* Your implementation may or may not support this feature.

## OMP_WAIT_POLICY
Provides a hint to an OpenMP implementation about the desired behavior of waiting threads. A compliant OpenMP implementation may or may not abide by the setting of the environment variable. Valid values are ACTIVE and PASSIVE. ACTIVE specifies that waiting threads should mostly be active, i.e., consume processor cycles, while waiting. PASSIVE specifies that waiting threads should mostly be passive, i.e., not consume processor cycles, while waiting. The details of the ACTIVE and PASSIVE behaviors are implementation defined. Examples:
```
setenv OMP_WAIT_POLICY ACTIVE 
setenv OMP_WAIT_POLICY active 
setenv OMP_WAIT_POLICY PASSIVE 
setenv OMP_WAIT_POLICY passive
```

Implementation notes:
* Your implementation may or may not support this feature.

## OMP_MAX_ACTIVE_LEVELS
Controls the maximum number of nested active parallel regions. The value of this environment variable must be a non-negative integer. The behavior of the program is implementation defined if the requested value of OMP_MAX_ACTIVE_LEVELS is greater than the maximum number of nested active parallel levels an implementation can support, or if the value is not a non-negative integer. Example:
```
setenv OMP_MAX_ACTIVE_LEVELS 2
```

Implementation notes:
* Your implementation may or may not support this feature.
 
## OMP_THREAD_LIMIT
Sets the number of OpenMP threads to use for the whole OpenMP program. The value of this environment variable must be a positive integer. The behavior of the program is implementation defined if the requested value of OMP_THREAD_LIMIT is greater than the number of threads an implementation can support, or if the value is not a positive integer. Example:
```
setenv OMP_THREAD_LIMIT 8
```
Implementation notes:
* Your implementation may or may not support this feature.
