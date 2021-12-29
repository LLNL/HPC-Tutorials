# OMP_IN_PARALLEL

## Purpose:

May be called to determine if the section of code which is executing is parallel or not.

## Format:

### Fortran	
```
LOGICAL FUNCTION OMP_IN_PARALLEL()
```

### C/C++
```
#include <omp.h>
int omp_in_parallel(void)
```

## Notes & Restrictions:

For Fortran, this function returns .TRUE. if it is called from the dynamic extent of a region executing in parallel, and .FALSE. otherwise. 

For C/C++, it will return a non-zero integer if parallel, and zero otherwise.
