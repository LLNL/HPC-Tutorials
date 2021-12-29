# OMP_TEST_LOCK 
# OMP_TEST_NEST_LOCK

## Purpose:

This subroutine attempts to set a lock, but does not block if the lock is unavailable.

## Format:

### Fortran	
```
SUBROUTINE OMP_TEST_LOCK(var)
SUBROUTINE OMP_TEST_NEST_LOCK(var)
```

### C/C++	
```
#include <omp.h>
int omp_test_lock(omp_lock_t *lock)
int omp_test_nest__lock(omp_nest_lock_t *lock)
```
## Notes & Restrictions:

* For Fortran, .TRUE. is returned if the lock was set successfully, otherwise .FALSE. is returned.

* For Fortran, ***var*** must be an integer large enough to hold an address, such as INTEGER*8 on 64-bit systems.

* For C/C++, non-zero is returned if the lock was set successfully, otherwise zero is returned.

* It is illegal to call this routine with a lock variable that is not initialized.
