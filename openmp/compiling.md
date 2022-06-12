---
layout: tutorial_page
title: "Compiling OpenMP Programs"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## LC OpenMP Implementations:

As of June 2022, the documentation for LC's default compilers claims the following OpenMP support:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-5iie{background-color:#98ABCE;border-color:inherit;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-5iie"><span style="background-color:#98ABCE">Compiler</span></th>
    <th class="tg-5iie"><span style="background-color:#98ABCE">Version</span></th>
    <th class="tg-5iie"><span style="background-color:#98ABCE">Supports</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0pky">Intel C/C++, Fortran</td>
    <td class="tg-0pky">intel/19.0.4 </td>
    <td class="tg-0pky">OpenMP 5.0</td>
  </tr>
  <tr>
    <td class="tg-0pky">GNU C/C++, Fortran</td>
    <td class="tg-0pky">4.9</td>
    <td class="tg-0pky">OpenMP 4.5</td>
  </tr>
  <tr>
    <td class="tg-0pky">PGI C/C++, Fortran</td>
    <td class="tg-0pky">pgi/22.1</td>
    <td class="tg-0pky">OpenMP 5.0</td>
  </tr>
  <tr>
    <td class="tg-0pky">IBM Coral Systems C/C++</td>
    <td class="tg-0pky">xl/2021.09.22</td>
    <td class="tg-0pky">OpenMP 4.5</td>
  </tr>
  <tr>
    <td class="tg-0pky">IBM Coral Systems Fortran</td>
    <td class="tg-0pky">xl/2021.09.22</td>
    <td class="tg-0pky">OpenMP 4.5</td>
  </tr>
  <tr>
    <td class="tg-0pky">IBM Coral Systems  GNU C/C++, Fortran</td>
    <td class="tg-0pky">xl/2021.09.22</td>
    <td class="tg-0pky">OpenMP 4.5</td>
  </tr>
</tbody>
</table>

### OpenMP 4.0 Support: 

According to vendor documentation, OpenMP 4.0 is supported beginning with the following compiler versions:
* GNU: 4.9 for C/C++ and 4.9.1 for Fortran
* Intel: 14.0 has "some" support; 15.0 supports "most features"
* PGI: not currently available
* IBM Coral Systems: 4.0, 4.5 - depending upon compiler

Use the command `use -l compilers` to view compiler packages by version.
You can also view compiler version information at: https://computing.llnl.gov/code/compilers.html

## Compiling:

All of LC's compilers require you to use the appropriate compiler flag to "turn on" OpenMP compilations. The table below shows what to use for each compiler.

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Compiler / Platform</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Compiler</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Flag</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">Intel<br>Linux Opteron/Xeon</td>
    <td class="tg-0lax">icc<br>icpc<br>ifort</td>
    <td class="tg-0lax">-qopenmp</td>
  </tr>
  <tr>
    <td class="tg-0lax">PGI<br>Linux Opteron/Xeon</td>
    <td class="tg-0lax">pgcc<br>pgCC<br>pgf77<br>pgf90</td>
    <td class="tg-0lax">-mp</td>
  </tr>
  <tr>
    <td class="tg-0lax">GNU<br>Linux Opteron/Xeon<br>IBM Blue Gene</td>
    <td class="tg-0lax">gcc<br>g++<br>g77<br>gfortran</td>
    <td class="tg-0lax">-fopenmp</td>
  </tr>
  <tr>
    <td class="tg-0lax">IBM<br>Coral Systems </td>
    <td class="tg-0lax">xlc_r, cc_r xlC_r, xlc++_r xlc89_r xlc99_r xlf_r xlf90_r xlf95_r xlf2003_r *Be sure to use a thread-safe compiler - its name ends with _r</td>
    <td class="tg-0lax">-qsmp=omp</td>
  </tr>
</tbody>
</table>

### Compiler Documentation:

* IBM Coral Systems: https://hpc.llnl.gov/documentation/tutorials/using-lc-s-sierra-systems
* Intel: www.intel.com/software/products/compilers/
* PGI: www.pgroup.com
* GNU: www.gnu.org
* All: See the relevant man pages and any files that might relate in `/usr/local/docs`

[//]: # "Old/broken links for IBM GlueGene compiler docs:  www-01.ibm.com/software/awdtools/fortran/ and www-01.ibm.com/software/awdtools/xlcpp "
