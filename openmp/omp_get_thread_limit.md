# OMP_GET_THREAD_LIMIT

## Purpose:
Returns the maximum number of OpenMP threads available to a program.
## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_THREAD_LIMIT
```

### C/C++	
```
#include <omp.h>
int omp_get_thread_limit (void)
```

## Notes:

Also see the OMP_THREAD_LIMIT environment variable.
