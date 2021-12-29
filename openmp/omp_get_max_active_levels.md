# OMP_GET_MAX_ACTIVE_LEVELS

## Purpose:

This routine returns the maximum number of nested active parallel regions.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_MAX_ACTIVE_LEVELS()
```

### C/C++	
```
#include <omp.h>
int omp_get_max_active_levels(void) 
```
