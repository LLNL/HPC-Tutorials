# OMP_DESTROY_LOCK 
# OMP_DESTROY_NEST_LOCK

## Purpose:
This subroutine disassociates the given lock variable from any locks.

## Format:

### Fortran	
```
SUBROUTINE OMP_DESTROY_LOCK(var)
SUBROUTINE OMP_DESTROY_NEST_LOCK(var)
```
### C/C++	
```
#include <omp.h>
void omp_destroy_lock(omp_lock_t *lock)
void omp_destroy_nest_lock(omp_nest_lock_t *lock)
```
## Notes & Restrictions:

* It is illegal to call this routine with a lock variable that is not initialized.

* For Fortran, ***var*** must be an integer large enough to hold an address, such as INTEGER*8 on 64-bit systems.
