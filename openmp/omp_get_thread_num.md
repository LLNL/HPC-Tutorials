# OMP_GET_THREAD_NUM

## Purpose:
<br>
Returns the thread number of the thread, within the team, making this call. This number will be between 0 and OMP_GET_NUM_THREADS-1. The master thread of the team is thread 0

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_THREAD_NUM()
```

### C/C++	
```
#include <omp.h>
int omp_get_thread_num(void)
```

## Notes & Restrictions:

If called from a nested parallel region, or a serial region, this function will return 0.

## Examples:

* Example 1 is the correct way to determine the number of threads in a parallel region.
* Example 2 is incorrect - the TID variable must be PRIVATE
* Example 3 is incorrect - the OMP_GET_THREAD_NUM call is outside the parallel region
