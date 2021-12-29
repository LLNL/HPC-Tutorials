# OMP_SET_SCHEDULE

## Purpose:

This routine sets the schedule type that is applied when the loop directive specifies a runtime schedule.

## Format:

### Fortran	
```
SUBROUTINE OMP_SET_SCHEDULE(KIND, MODIFIER)
INTEGER (KIND=OMP_SCHED_KIND) KIND
INTEGER MODIFIER
```

### C/C++	
```
#include <omp.h>
void omp_set_schedule(omp_sched_t kind, int modifier)
```
