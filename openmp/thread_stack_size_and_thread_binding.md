---
layout: tutorial_page
title: "Thread Stack Size and Thread Binding"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

The OpenMP standard does not specify how much stack space a thread should have. Consequently, implementations will differ in the default thread stack size.

Default thread stack size can be easy to exhaust. It can also be non-portable between compilers. Using past versions of LC compilers as an example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-cly1{text-align:left;vertical-align:middle}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Compiler</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Approx. Stack Limit</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">Approx. Array Size (doubles)</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-cly1">Linux icc, ifort</td>
    <td class="tg-cly1">4 MB</td>
    <td class="tg-cly1">700 x 700</td>
  </tr>
  <tr>
    <td class="tg-cly1">Linux pgcc, pgf90</td>
    <td class="tg-cly1">8 MB</td>
    <td class="tg-cly1">1000 x 1000</td>
  </tr>
  <tr>
    <td class="tg-cly1">Linux gcc, gfortran</td>
    <td class="tg-cly1">2 MB</td>
    <td class="tg-cly1">500 x 500</td>
  </tr>
</tbody>
</table>
