# OMP_GET_ACTIVE_LEVEL

## Purpose:

The omp_get_active_level routine returns the number of nested, active parallel regions enclosing the task that contains the call.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_ACTIVE_LEVEL()
```
### C/C++	
```
#include <omp.h>
int omp_get_active_level(void);
```
## Notes & Restrictions:

The routine always returns a nonnegative integer, and returns 0 if it is called from the sequential part of the program.
