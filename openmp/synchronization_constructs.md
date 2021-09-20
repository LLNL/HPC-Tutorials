---
layout: tutorial_page
title: "OpenMP Directives: Synchronization Constructs"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

Consider a simple example where two threads on two different processors are both trying to increment a variable x at the same time (assume x is initially 0):

<table>
<thead>
  <tr>
    <th></th>
    <th>THREAD 1:</th>
    <th>THREAD 2:</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>High level code</td>
    <td>increment(x)<br>{<br>    x = x + 1;<br>}</td>
    <td>increment(x)<br>{<br>    x = x + 1;<br>}</td>
  </tr>
  <tr>
    <td>Assembly</td>
    <td>10  LOAD A, (x address)<br>20  ADD A, 1<br>30  STORE A, (x address)</td>
    <td>10  LOAD A, (x address)<br>20  ADD A, 1<br>30  STORE A, (x address)</td>
  </tr>
</tbody>
</table>

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