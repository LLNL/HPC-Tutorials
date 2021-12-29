# OMP_GET_DYNAMIC

## Purpose:

Used to determine if dynamic thread adjustment is enabled or not.

## Format:

### Fortran	
```
LOGICAL FUNCTION OMP_GET_DYNAMIC()
```

### C/C++	
```
#include <omp.h>
int omp_get_dynamic(void)
```

## Notes & Restrictions:

* For Fortran, this function returns .TRUE. if dynamic thread adjustment is enabled, and .FALSE. otherwise.

* For C/C++, non-zero will be returned if dynamic thread adjustment is enabled, and zero otherwise.
