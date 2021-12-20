# OMP_GET_NUM_THREADS

## Purpose:
Returns the number of threads that are currently in the team executing the parallel region from which it is called.

## Format:

### Fortran	
```
INTEGER FUNCTION OMP_GET_NUM_THREADS()
```

### C/C++	
```
#include <omp.h>
int omp_get_num_threads(void)
```

## Notes & Restrictions:

If this call is made from a serial portion of the program, or a nested parallel region that is serialized, it will return 1.

The default number of threads is implementation dependent.
