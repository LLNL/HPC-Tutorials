# OMP_GET_MAX_THREADS

## Purpose:
Returns the maximum value that can be returned by a call to the OMP_GET_NUM_THREADS function.

### Fortran	
```
INTEGER FUNCTION OMP_GET_MAX_THREADS()
```

### C/C++	
```
#include <omp.h>
int omp_get_max_threads(void)
```
## Notes & Restrictions:

* Generally reflects the number of threads as set by the OMP_NUM_THREADS environment variable or the OMP_SET_NUM_THREADS() library routine.

* May be called from both serial and parallel regions of code.
