# OMP_SET_DYNAMIC

## Purpose:

Enables or disables dynamic adjustment (by the run time system) of the number of threads available for execution of parallel regions.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_DYNAMIC(scalar_logical_expression)
```

### C/C++	
```
#include <omp.h>
void omp_set_dynamic(int dynamic_threads)
```

## Notes & Restrictions:

* For Fortran, if called with .TRUE. then the number of threads available for subsequent parallel regions can be adjusted automatically by the run-time environment. If called with .FALSE., dynamic adjustment is disabled.

* For C/C++, if dynamic_threads evaluates to non-zero, then the mechanism is enabled, otherwise it is disabled.

* The OMP_SET_DYNAMIC subroutine has precedence over the OMP_DYNAMIC environment variable.

* The default setting is implementation dependent.

* Must be called from a serial section of the program.
