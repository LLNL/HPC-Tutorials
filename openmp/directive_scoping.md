---
layout: tutorial_page
title: "OpenMP Directives: Directive Scoping"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

Do we do this now...or do it later? Oh well, let's get it over with early...

## Static (Lexical) Extent:

The code textually enclosed between the beginning and the end of a structured block following a directive.

The static extent of a directives does not span multiple routines or code files.

## Orphaned Directive:

An OpenMP directive that appears independently from another enclosing directive is said to be an orphaned directive. It exists outside of another directive's static (lexical) extent.

An orphaned directive can span routines and possibly code files.

## Dynamic Extent:

The dynamic extent of a directive includes both its static (lexical) extent and the extents of its orphaned directives.

## Example:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-dydl{background-color:#DDD;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-liws{background-color:#F0F5FE;text-align:left;vertical-align:top}
.tg .tg-b6y0{background-color:#F0F5FE;color:#000000;text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-b6y0">     PROGRAM TEST       <br>&nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP PARALLEL</b>       <br>&nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP DO</b>       <br>&nbsp;&nbsp;&nbsp;&nbsp;DO I=...       <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...       <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CALL SUB1       <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...       <br>    &nbsp;&nbsp;&nbsp;&nbsp;ENDDO       <br>&nbsp;&nbsp;&nbsp;&nbsp;...       <br>&nbsp;&nbsp;&nbsp;&nbsp;CALL SUB2       <br>&nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP END PARALLEL</b> </th>
    <th class="tg-liws">&nbsp;&nbsp;&nbsp;&nbsp;SUBROUTINE SUB1<br>&nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP CRITICAL</b><br>&nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP END CRITICAL</b><br>&nbsp;&nbsp;&nbsp;&nbsp;END<br><br>&nbsp;&nbsp;&nbsp;&nbsp;SUBROUTINE SUB2<br>  &nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP SECTIONS</b><br> &nbsp;&nbsp;&nbsp;&nbsp;... <br><b>!$OMP END SECTIONS</b><br> &nbsp;&nbsp;&nbsp;&nbsp;... <br>&nbsp;&nbsp;&nbsp;&nbsp;END <br></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-dydl"><span style="font-weight:bold;font-style:normal">STATIC EXTENT</span> <br>The DO directive occurs within an enclosing parallel region</td>
    <td class="tg-dydl"><span style="font-weight:bold;font-style:normal">ORPHANED DIRECTIVES</span> <br>The CRITICAL and SECTIONS directives occur outside an enclosing parallel region</td>
  </tr>
  <tr>
    <td class="tg-dydl" colspan="2"><span style="font-weight:bold;font-style:normal">DYNAMIC EXTENT</span> <br>The CRITICAL and SECTIONS directives occur within the dynamic extent of the DO and PARALLEL directives.</td>
  </tr>
</tbody>
</table>

## Why Is This Important?

OpenMP specifies a number of scoping rules on how directives may associate (bind) and nest within each other.

Illegal and/or incorrect programs may result if the OpenMP binding and nesting rules are ignored.

See [Directive Binding and Nesting Rules](directive_binding_and_nesting.md) for specific details.