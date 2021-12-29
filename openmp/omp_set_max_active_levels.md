# OMP_SET_MAX_ACTIVE_LEVELS

##  Purpose:

This routine limits the number of nested active parallel regions.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_MAX_ACTIVE_LEVELS (MAX_LEVELS)
INTEGER MAX_LEVELS
```

### C/C++	
```
#include <omp.h>
void omp_set_max_active_levels (int max_levels) 
```

## Notes & Restrictions:

* If the number of parallel levels requested exceeds the number of levels of parallelism supported by the implementation, the value will be set to the number of parallel levels supported by the implementation.

* This routine has the described effect only when called from the sequential part of the program. When called from within an explicit parallel region, the effect of this routine is implementation defined.
