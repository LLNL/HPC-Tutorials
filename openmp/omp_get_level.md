# OMP_GET_LEVEL

## Purpose:

This routine returns the number of nested parallel regions enclosing the task that contains the call.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_LEVEL()
```

### C/C++	
```
#include <omp.h>
int omp_get_level(void) 
```

## Notes & Restrictions:

The omp_get_level routine returns the number of nested parallel regions (whether active or inactive) enclosing the task that contains the call, not including the implicit parallel region. The routine always returns a non-negative integer, and returns 0 if it is called from the sequential part of the program.
