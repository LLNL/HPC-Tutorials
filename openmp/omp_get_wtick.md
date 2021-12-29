# OMP_GET_WTICK

## Purpose:

* Provides a portable wall clock timing routine

* Returns a double-precision floating point value equal to the number of seconds between successive clock ticks.

## Format:

### Fortran	
```
DOUBLE PRECISION FUNCTION OMP_GET_WTICK() 
```

## C/C++	
```
#include <omp.h>
double omp_get_wtick(void) 
```
