---
layout: tutorial_page
title: "OpenMP Directives: Fortran Directive Format"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Format: (case insensitive)

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">sentinel</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">directive-name</span></th>
    <th class="tg-fdm5"><span style="background-color:#98ABCE">[clause ...]</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">All Fortran OpenMP directives must begin with a sentinel. The accepted sentinels depend upon the type of Fortran source. Possible sentinels are:<br><span style="font-weight:bold">    !$OMP  </span><br><span style="font-weight:bold">    C$OMP     </span><br><span style="font-weight:bold">    *$OMP </span></td>
    <td class="tg-0lax">A valid OpenMP directive must appear after the sentinel and before any clauses.</td>
    <td class="tg-0lax">Optional. Clauses can be in any order, and repeated as necessary unless otherwise restricted.</td>
  </tr>
</tbody>
</table>


## Example:

```
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(BETA,PI)
```

## Fixed Form Source:

* `!$OMP` `C$OMP` `*$OMP` are accepted sentinels and must start in column 1.

* All Fortran fixed form rules for line length, white space, continuation and comment columns apply for the entire directive line.

* Initial directive lines must have a space/zero in column 6.

* Continuation lines must have a non-space/zero in column 6.

## Free Form Source:

* `!$OMP` is the only accepted sentinel. Can appear in any column, but must be preceded by white space only.

* All Fortran free form rules for line length, white space, continuation and comment columns apply for the entire directive line.

* Initial directive lines must have a space after the sentinel.

* Continuation lines must have an ampersand as the last non-blank character in a line. The following line must begin with a sentinel and then the continuation directives.

## General Rules:

* Comments can not appear on the same line as a directive.

* Only one directive-name may be specified per directive.

* Fortran compilers which are OpenMP enabled generally include a command line option which instructs the compiler to activate and interpret all OpenMP directives.

* Several Fortran OpenMP directives come in pairs and have the form shown below. The "end" directive is optional but advised for readability.

<pre>
!$OMP  <i> &lt;directive &gt; 

    [ structured block of code ]</i> 

!$OMP end  <i> &lt;directive &gt; </i>
</pre>