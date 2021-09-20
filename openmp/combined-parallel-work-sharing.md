---
layout: tutorial_page
title: "OpenMP Directives: Combined Parallel Work-Sharing Constructs"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

OpenMP provides three directives that are merely conveniences:
* PARALLEL DO / parallel for
* PARALLEL SECTIONS
* PARALLEL WORKSHARE (fortran only)

For the most part, these directives behave identically to an individual PARALLEL directive being immediately followed by a separate work-sharing directive.

Most of the rules, clauses and restrictions that apply to both directives are in effect. See the OpenMP API for details.

An example using the PARALLEL DO / parallel for combined directive is shown below.

## Fortran - PARALLEL DO Directive Example

```
      PROGRAM VECTOR_ADD

      INTEGER N, I, CHUNKSIZE, CHUNK
      PARAMETER (N=1000) 
      PARAMETER (CHUNKSIZE=100) 
      REAL A(N), B(N), C(N)

!     Some initializations
      DO I = 1, N
        A(I) = I * 1.0
        B(I) = A(I)
      ENDDO
      CHUNK = CHUNKSIZE
             
!$OMP PARALLEL DO
!$OMP& SHARED(A,B,C,CHUNK) PRIVATE(I) 
!$OMP& SCHEDULE(STATIC,CHUNK)

      DO I = 1, N
         C(I) = A(I) + B(I)
      ENDDO

!$OMP END PARALLEL DO

      END
```

## C/C++ - parallel for Directive Example

```
#include <omp.h>
#define N       1000
#define CHUNKSIZE   100

main ()  {

int i, chunk;
float a[N], b[N], c[N];

/* Some initializations */
for (i=0; i < N; i++)
  a[i] = b[i] = i * 1.0;
chunk = CHUNKSIZE;

#pragma omp parallel for \
   shared(a,b,c,chunk) private(i) \
   schedule(static,chunk)
  for (i=0; i < n; i++)
    c[i] = a[i] + b[i];
}
```