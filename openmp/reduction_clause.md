

## Purpose:

The REDUCTION clause performs a reduction on the variables that appear in its list.

A private copy for each list variable is created for each thread. At the end of the reduction, the reduction variable is applied to all private copies of the shared variable, and the final result is written to the global shared variable.

## Format:

### Fortran	
```
REDUCTION (operator|intrinsic: list)
```

### C/C++	
```
reduction (operator: list)
```
## Example: REDUCTION - Vector Dot Product:

Iterations of the parallel loop will be distributed in equal sized blocks to each thread in the team (SCHEDULE STATIC)

At the end of the parallel loop construct, all threads will add their values of "result" to update the master thread's global copy.

### Fortran - REDUCTION Clause Example
 ```
       PROGRAM DOT_PRODUCT

       INTEGER N, CHUNKSIZE, CHUNK, I
       PARAMETER (N=100)
       PARAMETER (CHUNKSIZE=10)
       REAL A(N), B(N), RESULT

!      Some initializations
       DO I = 1, N
         A(I) = I * 1.0
         B(I) = I * 2.0
       ENDDO
       RESULT= 0.0
       CHUNK = CHUNKSIZE

!$OMP  PARALLEL DO
!$OMP& DEFAULT(SHARED) PRIVATE(I)
!$OMP& SCHEDULE(STATIC,CHUNK)
!$OMP& REDUCTION(+:RESULT)

       DO I = 1, N
         RESULT = RESULT + (A(I) * B(I))
       ENDDO

!$OMP  END PARALLEL DO

       PRINT *, 'Final Result= ', RESULT
       END
```

### C / C++ - reduction Clause Example
```
#include <omp.h>

main ()  {

int   i, n, chunk;
float a[100], b[100], result;

/* Some initializations */
n = 100;
chunk = 10;
result = 0.0;
for (i=0; i < n; i++)
  {
  a[i] = i * 1.0;
  b[i] = i * 2.0;
  }

#pragma omp parallel for      \  
  default(shared) private(i)  \  
  schedule(static,chunk)      \  
  reduction(+:result)  

  for (i=0; i < n; i++)
    result = result + (a[i] * b[i]);

printf("Final result= %f\n",result);

}
```
