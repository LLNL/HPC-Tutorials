# OMP_GET_TEAM_SIZE

## Purpose:

This routine returns, for a given nested level of the current thread, the size of the thread team to which the ancestor or the current thread belongs.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_TEAM_SIZE(LEVEL)
INTEGER LEVEL
```

### C/C++	
```
#include <omp.h>
int omp_get_team_size(int level); 
```
## Notes & Restrictions:

If the requested nested level is outside the range of 0 and the nested level of the current thread, as returned by the omp_get_level routine, the routine returns -1. Inactive parallel regions are regarded like active parallel regions executed with one thread.
