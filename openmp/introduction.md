---
layout: tutorial_page
title: "Introduction"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---



![logo](images/openmp_logo.gif)


## OpenMP is:
* An Application Program Interface (API) that may be used to explicitly direct ***multi-threaded, shared memory parallelism***
* Comprised of three primary API components:
    * Compiler Directives
    * Runtime Library Routines
    * Environment Variables
* An abbreviation for:
    * Short version: **Open Multi-Processing**
    * Long version: **Open** specifications for **Multi-Processing** via collaborative work between interested parties from the hardware and software industry, government and academia.

## OpenMP is not:

* Necessarily implemented identically by all vendors
* Guaranteed to make the most efficient use of shared memory
* Required to check for data dependencies, data conflicts, race conditions, or deadlocks
* Required to check for code sequences that cause a program to be classified as non-conforming
* Designed to guarantee that input or output to the same file is synchronous when executed in parallel. The programmer is responsible for synchronizing input and output.

## Goals of OpenMP:

* **Standardization**:
    * Provide a standard among a variety of shared memory architectures/platforms
    * Jointly defined and endorsed by a group of major computer hardware and software vendors
* **Lean and Mean**:
    * Establish a simple and limited set of directives for programming shared memory machines.
    * Significant parallelism can be implemented by using just 3 or 4 directives.
    * This goal is becoming less meaningful with each new release, apparently.
* **Ease of Use**:
    * Provide capability to incrementally parallelize a serial program, unlike message-passing libraries which typically require an all or nothing approach
    * Provide the capability to implement both coarse-grain and fine-grain parallelism
* **Portability**:
	* The API is specified for C/C++ and Fortran
	* Public forum for API and membership
	* Most major platforms have been implemented including Unix/Linux platforms and Windows

## History:

In the early 90's, vendors of shared-memory machines supplied similar, directive-based, Fortran programming extensions:
    * The user would augment a serial Fortran program with directives specifying which loops were to be parallelized
    * The compiler would be responsible for automatically parallelizing such loops across the SMP processors

Implementations were all functionally similar, but were diverging (as usual)

First attempt at a standard was the draft for ANSI X3H5 in 1994. It was never adopted, largely due to waning interest as distributed memory machines became popular.

However, not long after this, newer shared memory machine architectures started to become prevalent, and interest resumed.

The OpenMP standard specification started in the spring of 1997, taking over where ANSI X3H5 had left off.

Led by the OpenMP Architecture Review Board (ARB). Original ARB members and contributors are shown below. (Disclaimer: all partner names derived from the [OpenMP web site](https://www.openmp.org/))

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
    <th class="tg-5iie"><span style="background-color:#98ABCE">APR Members</span></th>
    <th class="tg-5iie"><span style="background-color:#98ABCE">Endorsing Application Developers</span></th>
    <th class="tg-5iie"><span style="background-color:#98ABCE">Endorsing Software Vendors</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0pky">Compaq / Digital <br>Hewlett-Packard Company<br>Intel Corporation<br>International Business Machines (IBM) <br>Kuck &amp; Associates, Inc. (KAI)<br>Silicon Graphics, Inc.<br>Sun Microsystems, Inc.<br>U.S. Department of Energy ASCI program </td>
    <td class="tg-0pky">ADINA R&amp;D, Inc.<br>ANSYS, Inc.<br>Dash Associates<br>Fluent, Inc.<br>ILOG CPLEX Division<br>Livermore Software Technology Corporation (LSTC)<br>MECALOG SARL<br>Oxford Molecular Group PLC<br>The Numerical Algorithms Group Ltd.(NAG) </td>
    <td class="tg-0pky">Absoft Corporation<br>Edinburgh Portable Compilers<br>GENIAS Software GmBH<br>Myrias Computer Technologies, Inc. <br>The Portland Group, Inc. (PGI)<br><br></td>
  </tr>
</tbody>
</table>

For more news and membership information about the OpenMP ARB, visit: [openmp.org/wp/about-openmp](https://openmp.org/wp/about-openmp/).

## Release History

OpenMP continues to evolve, with new constructs and features being added over time.

Initially, the API specifications were released separately for C and Fortran. Since 2005, they have been released together.

The table below chronicles the OpenMP API release history:

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
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Month/Year</span></th>
    <th class="tg-xq0d"><span style="background-color:#98ABCE">Version</span></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0lax">Oct 1997</td>
    <td class="tg-0lax">Fortran 1.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Oct 1998</td>
    <td class="tg-0lax">C/C++ 1.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Nov 1999</td>
    <td class="tg-0lax">Fortran 1.1</td>
  </tr>
  <tr>
    <td class="tg-0lax">Nov 2000</td>
    <td class="tg-0lax">Fortran 2.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Mar 2002</td>
    <td class="tg-0lax">C/C++ 2.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">May 2005</td>
    <td class="tg-0lax">OpenMP 2.5</td>
  </tr>
  <tr>
    <td class="tg-0lax">May 2008</td>
    <td class="tg-0lax">OpenMP 3.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Jul 2011</td>
    <td class="tg-0lax">OpenMP 3.1</td>
  </tr>
  <tr>
    <td class="tg-0lax">Jul 2013</td>
    <td class="tg-0lax">OpenMP 4.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Nov 2018</td>
    <td class="tg-0lax">OpenMP 5.0</td>
  </tr>
  <tr>
    <td class="tg-0lax">Nov 2020</td>
    <td class="tg-0lax">OpenMP 5.1</td>
  </tr><tr>
    <td class="tg-0lax">Nov 2021</td>
    <td class="tg-0lax">OpenMP 5.2</td>
  </tr>

</tbody>
</table>

**Note**: The remainder of this tutorial, unless otherwise indicated, refers to OpenMP version 3.1. Syntax and features new with OpenMP 4.x and OpenMP 5.x are not currently covered.

## References:

* OpenMP website: [openmp.org](https://openmp.org) 
	API specifications, FAQ, presentations, discussions, media releases, calendar, membership application and more...
* Wikipedia: en.wikipedia.org/wiki/OpenMP


