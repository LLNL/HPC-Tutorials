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

<pre>
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
             
<b>!$OMP PARALLEL DO
!$OMP& SHARED(A,B,C,CHUNK) PRIVATE(I) 
!$OMP& SCHEDULE(STATIC,CHUNK)</b>

      DO I = 1, N
         C(I) = A(I) + B(I)
      ENDDO

<b>!$OMP END PARALLEL DO</b>

      END
</pre>

## C/C++ - parallel for Directive Example

<pre>
<b>#include &lt;omp.h&gt;</b>
#define N       1000
#define CHUNKSIZE   100

main ()  {

int i, chunk;
float a[N], b[N], c[N];

/* Some initializations */
for (i=0; i < N; i++)
  a[i] = b[i] = i * 1.0;
chunk = CHUNKSIZE;

<b>#pragma omp parallel for \
   shared(a,b,c,chunk) private(i) \
   schedule(static,chunk)</b>
  for (i=0; i < n; i++)
    c[i] = a[i] + b[i];
}
</pre>