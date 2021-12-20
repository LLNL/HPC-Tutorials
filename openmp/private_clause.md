---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: PRIVATE Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The PRIVATE clause declares variables in its list to be private to each thread.

## Format:

### Fortran	
<pre>
PRIVATE <i>(list)</i>
</pre>

### C/C++	
<pre>
private <i>(list)</i>
</pre>

## Notes:

* PRIVATE variables behave as follows:
    * A new object of the same type is declared once for each thread in the team
    * All references to the original object are replaced with references to the new object
    * Variables declared PRIVATE should be assumed to be uninitialized for each thread

* Comparison between PRIVATE and THREADPRIVATE:

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
    <th class="tg-xq0d"></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">PRIVATE</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">THREADPRIVATE</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">Data Item</td>
    <td class="tg-0lax">C/C++: variable<br>Fortran: variable or common block</td>
    <td class="tg-0lax">C/C++: variable<br>Fortran: common block</td>
  </tr>
  <tr>
    <td class="tg-0lax">Where Declared</td>
    <td class="tg-0lax">At start of region or work-sharing group</td>
    <td class="tg-0lax">In declarations of each routine using block or global file scope</td>
  </tr>
  <tr>
    <td class="tg-0lax">Persistent?</td>
    <td class="tg-0lax">No</td>
    <td class="tg-0lax">Yes</td>
  </tr>
  <tr>
    <td class="tg-0lax">Extent</td>
    <td class="tg-0lax">Lexical only - unless passed as an argument to subroutine</td>
    <td class="tg-0lax">Dynamic</td>
  </tr>
  <tr>
    <td class="tg-0lax">Initialized</td>
    <td class="tg-0lax">Use FIRSTPRIVATE</td>
    <td class="tg-0lax">Use COPYIN</td>
  </tr>
</tbody>
</table>
