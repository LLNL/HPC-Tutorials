---
layout: tutorial_page
title: "Introduction"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---



![logo](images/openmp_logo.gif)


**OpenMP is:**
* An Application Program Interface (API) that may be used to explicitly direct ***multi-threaded, shared memory parallelism***
* Comprised of three primary API components:
    * Compiler Directives
    * Runtime Library Routines
    * Environment Variables
* An abbreviation for:
    * Short version: **Open Multi-Processing**
    * Long version: **Open** specifications for **Multi-Processing** via collaborative work between interested parties from the hardware and software industry, government and academia.

**OpenMP is not:**

* Meant for distributed memory parallel systems (by itself)
* Necessarily implemented identically by all vendors
* Guaranteed to make the most efficient use of shared memory
* Required to check for data dependencies, data conflicts, race conditions, or deadlocks
* Required to check for code sequences that cause a program to be classified as non-conforming
* Meant to cover compiler-generated automatic parallelization and directives to the compiler to assist such parallelization
* Designed to guarantee that input or output to the same file is synchronous when executed in parallel. The programmer is responsible for synchronizing input and output.

**Goals of OpenMP:**

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

**History:**

In the early 90's, vendors of shared-memory machines supplied similar, directive-based, Fortran programming extensions:
    * The user would augment a serial Fortran program with directives specifying which loops were to be parallelized
    * The compiler would be responsible for automatically parallelizing such loops across the SMP processors

Implementations were all functionally similar, but were diverging (as usual)

First attempt at a standard was the draft for ANSI X3H5 in 1994. It was never adopted, largely due to waning interest as distributed memory machines became popular.

However, not long after this, newer shared memory machine architectures started to become prevalent, and interest resumed.

The OpenMP standard specification started in the spring of 1997, taking over where ANSI X3H5 had left off.

Led by the OpenMP Architecture Review Board (ARB). Original ARB members and contributors are shown below. (Disclaimer: all partner names derived from the [OpenMP web site](https://www.openmp.org/))

|   APR Members     |     Endorsing Application Developers      |     Endorsing Software Vendors    |
|:--------------------:|:----------------------------------:|:-----------------------------------------:|
| Compaq / Digital <br>Hewlett-Packard Company <br>Intel Corporation <br>International Business Machines (IBM)  <br>Kuck & Associates, Inc. (KAI) <br>Silicon Graphics, Inc. <br>Sun Microsystems, Inc. <br>U.S. Department of Energy ASCI program  | ADINA R&D, Inc. <br>ANSYS, Inc. <br>Dash Associates <br>Fluent, Inc. <br>ILOG CPLEX Division <br>Livermore Software Technology Corporation (LSTC) <br>MECALOG SARL <br>Oxford Molecular Group PLC <br>The Numerical Algorithms Group Ltd.(NAG)  | Absoft Corporation <br>Edinburgh Portable Compilers <br>GENIAS Software GmBH <br>Myrias Computer Technologies, Inc.  <br>The Portland Group, Inc. (PGI)<br><br> |

