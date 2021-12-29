# OMP_INIT_LOCK 
# OMP_INIT_NEST_LOCK

## Purpose:
This subroutine initializes a lock associated with the lock variable.

## Format:

### Fortran	
```
SUBROUTINE OMP_INIT_LOCK(var)
SUBROUTINE OMP_INIT_NEST_LOCK(var)
```

### C/C++	
```
#include <omp.h>
void omp_init_lock(omp_lock_t *lock)
void omp_init_nest_lock(omp_nest_lock_t *lock)
```
## Notes & Restrictions:

* The initial state is unlocked.

* For Fortran, ***var*** must be an integer large enough to hold an address, such as INTEGER*8 on 64-bit systems.
