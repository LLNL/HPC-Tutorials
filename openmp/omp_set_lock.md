# OMP_SET_LOCK 
# OMP_SET_NEST_LOCK

## Purpose:
This subroutine forces the executing thread to wait until the specified lock is available. A thread is granted ownership of a lock when it becomes available.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_LOCK(var)
SUBROUTINE OMP_SET_NEST_LOCK(var)
```

### C/C++	
```
#include <omp.h>
void omp_set_lock(omp_lock_t *lock)
void omp_set_nest__lock(omp_nest_lock_t *lock)
```
## Notes & Restrictions:

* It is illegal to call this routine with a lock variable that is not initialized.

* For Fortran, ***var*** must be an integer large enough to hold an address, such as INTEGER*8 on 64-bit systems.
