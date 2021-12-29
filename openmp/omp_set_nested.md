# OMP_SET_NESTED

## Purpose:

Used to enable or disable nested parallelism.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_NESTED(scalar_logical_expression)
```

### C/C++	
```
#include <omp.h>
void omp_set_nested(int nested)
```

## Notes & Restrictions:

* For Fortran, calling this function with .FALSE. will disable nested parallelism, and calling with .TRUE. will enable it.

* For C/C++, if nested evaluates to non-zero, nested parallelism is enabled; otherwise it is disabled.

* The default is for nested parallelism to be disabled.

* This call has precedence over the OMP_NESTED environment variable
