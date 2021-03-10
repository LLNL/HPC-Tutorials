---
layout: tutorial_page
title: "A Brief Word on MPI-2 and MPI-3"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

### MPI-2:

Intentionally, the MPI-1 specification did not address several "difficult" issues. For reasons of expediency, these issues were deferred to a second specification, called MPI-2 in 1998. MPI-2 was a major revision to MPI-1 adding new functionality and corrections.

**Key areas of new functionality in MPI-2:**

- *Dynamic Processes* - extensions that remove the static process model of MPI. Provides routines to create new processes after job startup.
- *One-Sided Communications* - provides routines for one directional communications. Include shared memory operations (put/get) and remote accumulate operations.
- *Extended Collective Operations* - allows for the application of collective operations to inter-communicators
- *External Interfaces* - defines routines that allow developers to layer on top of MPI, such as for debuggers and profilers.
- *Additional Language Bindings* - describes C++ bindings and discusses Fortran-90 issues.
- *Parallel I/O* - describes MPI support for parallel I/O.


### MPI-3:

The MPI-3 standard was adopted in 2012, and contains significant extensions to MPI-1 and MPI-2 functionality including:

- *Nonblocking Collective Operations* - permits tasks in a collective to perform operations without blocking, possibly offering performance improvements.
- *New One-sided Communication Operations* - to better handle different memory models.
- *Neighborhood Collectives* - extends the distributed graph and Cartesian process topologies with additional communication power.
- *Fortran 2008 Bindings* - expanded from Fortran90 bindings
- *MPIT Tool Interface* - allows the MPI implementation to expose certain internal variables, counters, and other states to the user (most likely performance tools).
- *Matched Probe* - fixes an old bug in MPI-2 where one could not probe for messages in a multi-threaded environment.


#### More Information on MPI-2 and MPI-3:

MPI Standard documents: http://www.mpi-forum.org/docs/
