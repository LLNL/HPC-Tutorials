---
layout: tutorial_page
title: "OpenMP Directives: Clauses/Directives Summary"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---


<br>
The table below summarizes which clauses are accepted by which OpenMP directives.
<br>


<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-1wig{font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-hjji{font-size:24px;text-align:center;vertical-align:top}
.tg .tg-fdm5{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:middle}
.tg .tg-ygmq{font-size:24px;text-align:center;vertical-align:middle}
.tg .tg-zffl{font-size:100%;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-xq0d{background-color:#98ABCE;font-weight:bold;text-align:center;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-fdm5" rowspan="2"><span style="background-color:#98ABCE">Clause</span></th>
    <th class="tg-fdm5" colspan="6"><span style="background-color:#98ABCE">Directive</span></th>
  </tr>
  <tr>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">PARALLEL</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">DO/for</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">SECTIONS</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">SINGLE</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">PARALLEL</span> <br><span style="background-color:#98ABCE">DO/for</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">PARALLEL</span> <br><span style="background-color:#98ABCE">SECTIONS</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-1wig">IF</td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
  </tr>
  <tr>
    <td class="tg-1wig">PRIVATE</td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
  </tr>
  <tr>
    <td class="tg-zffl">SHARED</td>
    <td class="tg-hjji"><span style="font-weight:normal;font-style:normal;text-decoration:none">♦</span></td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">DEFAULT</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">FIRSTPRIVATE</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">LASTPRIVATE</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">REDUCTION</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">COPYIN</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
  </tr>
  <tr>
    <td class="tg-zffl">COPYPRIVATE</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
  </tr>
  <tr>
    <td class="tg-zffl">SCHEDULE</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
  </tr>
  <tr>
    <td class="tg-zffl">ORDERED</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
  </tr>
  <tr>
    <td class="tg-zffl">NOWAIT</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-hjji">♦</td>
    <td class="tg-ygmq"> </td>
    <td class="tg-ygmq"> </td>
  </tr>
</tbody>
</table>

<br>

The following OpenMP directives do not accept clauses:
* MASTER
* CRITICAL
* BARRIER
* ATOMIC
* FLUSH
* ORDERED
* THREADPRIVATE

Implementations may (and do) differ from the standard in which clauses are supported by each directive.
