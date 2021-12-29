# OMP_GET_ANCESTOR_THREAD_NUM

## Purpose:

This routine returns, for a given nested level ofthe current thread, the thread number of the ancestor or the current thread.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_ANCESTOR_THREAD_NUM(LEVEL)
INTEGER LEVEL
```

### C/C++	
```
#include <omp.h>
int omp_get_ancestor_thread_num(int level) 
```
## Notes & Restrictions:

If the requested nest level is outside the range of 0 and the nest level of the current thread, as returned by the omp_get_level routine, the routine returns -1.
