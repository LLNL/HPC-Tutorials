# OMP_GET_NESTED

## Purpose:

Used to determine if nested parallelism is enabled or not.

## Format:

### Fortran	
```
LOGICAL FUNCTION OMP_GET_NESTED
```

### C/C++	
```
#include <omp.h>
int omp_get_nested (void)
```

## Notes & Restrictions:

* For Fortran, this function returns .TRUE. if nested parallelism is enabled, and .FALSE. otherwise.

* For C/C++, non-zero will be returned if nested parallelism is enabled, and zero otherwise.
