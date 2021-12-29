# OMP_IN_FINAL

## Purpose:
This routine returns true if the routine is executed in a final task region; otherwise, it returns false.

## Format:
### Fortran
```
LOGICAL FUNCTION OMP_IN_FINAL()
```
### C/C++
```
#include <omp.h>
int omp_in_final(void) 
```
