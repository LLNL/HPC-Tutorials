# OMP_UNSET_LOCK 
# OMP_UNSET_NEST_LOCK

## Purpose:

This subroutine releases the lock from the executing subroutine.

## Format:

### Fortran	
```
SUBROUTINE OMP_UNSET_LOCK(var)
SUBROUTINE OMP_UNSET_NEST_LOCK(var)
```

### C/C++	
```
#include <omp.h>
void omp_unset_lock(omp_lock_t *lock)
void omp_unset_nest__lock(omp_nest_lock_t *lock)
```

## Notes & Restrictions:

* It is illegal to call this routine with a lock variable that is not initialized.

* For Fortran, ***var*** must be an integer large enough to hold an address, such as INTEGER*8 on 64-bit systems.
