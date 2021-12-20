---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: REDUCTION Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The REDUCTION clause performs a reduction on the variables that appear in its list.

A private copy for each list variable is created for each thread. At the end of the reduction, the reduction variable is applied to all private copies of the shared variable, and the final result is written to the global shared variable.

## Format:

### Fortran	
<pre>
REDUCTION <i>(operator|intrinsic: list)</i>
</pre>

### C/C++	
<pre>
reduction <i>(operator: list)</i>
</pre>
## Example: REDUCTION - Vector Dot Product:

Iterations of the parallel loop will be distributed in equal sized blocks to each thread in the team (SCHEDULE STATIC)

At the end of the parallel loop construct, all threads will add their values of "result" to update the master thread's global copy.

### Fortran - REDUCTION clause example
<pre>
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

<b>
!$OMP  PARALLEL DO
!$OMP& DEFAULT(SHARED) PRIVATE(I)
!$OMP& SCHEDULE(STATIC,CHUNK)
!$OMP& REDUCTION(+:RESULT)
</b>

       DO I = 1, N
         RESULT = RESULT + (A(I) * B(I))
       ENDDO

<b>!$OMP  END PARALLEL DO</b>

       PRINT *, 'Final Result= ', RESULT
       END
</pre>

### C / C++ - reduction clause example

<pre>
#include &ltomp.h&gt

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

<b>
#pragma omp parallel for      \  
  default(shared) private(i)  \  
  schedule(static,chunk)      \  
  reduction(+:result)  
</b>

  for (i=0; i < n; i++)
    result = result + (a[i] * b[i]);

printf("Final result= %f\n",result);

}

</pre>

## Restrictions:

Variables in the list must be named scalar variables. They can not be array or structure type variables. They must also be declared SHARED in the enclosing context.

Reduction operations may not be associative for real numbers.

The REDUCTION clause is intended to be used on a region or work-sharing construct in which the reduction variable is used only in statements which have one of following forms:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-4erg{border-color:inherit;font-style:italic;text-align:left;vertical-align:top}
.tg .tg-vi1i{background-color:#98ABCE;border-color:inherit;text-align:center;vertical-align:middle}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-vi1i"><span style="background-color:#98ABCE">Fortran</span></th>
    <th class="tg-vi1i"><span style="background-color:#98ABCE">C / C++</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-4erg"><b><br>x = x operator expr <br>x = expr operator x </b>(except subtraction) <b><br>x = intrinsic(x, expr) <br>x = intrinsic(expr, x)</b></td>
    <td class="tg-4erg"><br><b>x = x op expr <br>x = expr op x </b>(except subtraction) <br><b>x binop = expr <br>x++ <br>++x <br>x-- <br>--x</b></td>
  </tr>
  <tr>
    <td class="tg-4erg"><b>x</b> is a scalar variable in the list <br><b>expr</b> is a scalar expression that does not reference <b>x</b> <br><b>intrinsic</b> is one of MAX, MIN, IAND, IOR, IEOR <br><b>operator</b> is one of +, *, -, .AND., .OR., .EQV., .NEQV.</td>
    <td class="tg-4erg"><b>x</b> is a scalar variable in the list <br><b>expr</b> is a scalar expression that does not reference <b>x</b> <br><b>op</b> is not overloaded, and is one of +, *, -, /, &amp;, ^, |, &amp;&amp;, || <br><b>binop</b> is not overloaded, and is one of +, *, -, /, &amp;, ^, |</td>
  </tr>
</tbody>
</table>
