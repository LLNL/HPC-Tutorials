# OMP_GET_WTIME

## Purpose:
* Provides a portable wall clock timing routine

* Returns a double-precision floating point value equal to the number of elapsed seconds since some point in the past. Usually used in "pairs" with the value of the first call subtracted from the value of the second call to obtain the elapsed time for a block of code.

* Designed to be "per thread" times, and therefore may not be globally consistent across all threads in a team - depends upon what a thread is doing compared to other threads.

## Format:

### Fortran	
```
DOUBLE PRECISION FUNCTION OMP_GET_WTIME() 
```

### C/C++	
```
#include <omp.h>
double omp_get_wtime(void)
```
