---
layout: tutorial_page
title: "OpenMP Directives: C/C++ Directive Format"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Format:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">#pragma omp</span> </th>
    <th class="tg-xq0d">directive-name</th>
    <th class="tg-xq0d">[clause, ...]</th>
    <th class="tg-xq0d">newline</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">Required for all OpenMP C/C++ directives.</td>
    <td class="tg-0lax">A valid OpenMP directive must appear after the pragma and before any clauses.</td>
    <td class="tg-0lax">Optional. Clauses can be in any order, and repeated as necessary unless otherwise restricted.</td>
    <td class="tg-0lax">Required. Precedes the structured block which is enclosed by this directive.</td>
  </tr>
</tbody>
</table>

## Example:

```
#pragma omp parallel default(shared) private(beta,pi)
```

## General Rules:

* Case sensitive.

* Directives follow conventions of the C/C++ standards for compiler directives.

* Only one directive-name may be specified per directive.

* Each directive applies to at most one succeeding statement, which must be a structured block.

* Long directive lines can be "continued" on succeeding lines by escaping the newline character with a backslash ("\") at the end of a directive line.

```
#pragma omp <directive>

    [ structured block of code ]
```