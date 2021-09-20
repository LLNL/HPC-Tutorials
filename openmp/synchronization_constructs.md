---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

<br>

Consider a simple example where two threads on two different processors are both trying to increment a variable x at the same time (assume x is initially 0):

<br>

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-6t3r{font-style:italic;font-weight:bold;text-align:left;vertical-align:top}
.tg .tg-amwm{font-weight:bold;text-align:center;vertical-align:top}
.tg .tg-0lax{text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-6t3r"></th>
    <th class="tg-amwm">THREAD 1:</th>
    <th class="tg-amwm">THREAD 2:</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-6t3r">High level code</td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;text-decoration:none">increment(x)</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">{</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">    x = x + 1;</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">}</span></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;text-decoration:none">increment(x)</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">{</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">    x = x + 1;</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">}</span></td>
  </tr>
  <tr>
    <td class="tg-6t3r">Assembly</td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;text-decoration:none">10   LOAD A, (x address)</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">20   ADD A, 1</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">30   STORE A, (x address)</span></td>
    <td class="tg-0lax"><span style="font-weight:normal;font-style:normal;text-decoration:none">10   LOAD A, (x address)</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">20   ADD A, 1</span><br><span style="font-weight:normal;font-style:normal;text-decoration:none">30   STORE A, (x address)</span></td>
  </tr>
</tbody>
</table>

<br>

One possible execution sequence:
1. Thread 1 loads the value of x into register A.
1. Thread 2 loads the value of x into register A.
1. Thread 1 adds 1 to register A
1. Thread 2 adds 1 to register A
1. Thread 1 stores register A at location x
1. Thread 2 stores register A at location x

The resultant value of x will be 1, not 2 as it should be.

To avoid a situation like this, the incrementing of x must be synchronized between the two threads to ensure that the correct result is produced.

OpenMP provides a variety of Synchronization Constructs that control how the execution of each thread proceeds relative to other team threads.