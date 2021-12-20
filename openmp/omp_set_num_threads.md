# OMP_SET_NUM_THREADS

## Purpose:
Sets the number of threads that will be used in the next parallel region. Must be a postive integer.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_NUM_THREADS(scalar_integer_expression)
```
### C/C++	
```
#include <omp.h>
void omp_set_num_threads(int num_threads)
```
## Notes & Restrictions:

* The dynamic threads mechanism modifies the effect of this routine.
  * Enabled: specifies the maximum number of threads that can be used for any parallel region by the dynamic threads mechanism.
  * Disabled: specifies exact number of threads to use until next call to this routine.
* This routine can only be called from the serial portions of the code
* This call has precedence over the OMP_NUM_THREADS environment variable
