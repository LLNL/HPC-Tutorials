---
layout: page
title: "Collective Communication Routines"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

![collective_comm](images/collective_comm.gif)

#### Types of Collective Operations: 

* **Synchronization** - processes wait until all members of the group have reached the synchronization point.
* **Data Movement** - broadcast, scatter/gather, all to all.
* **Collective Computation** (reductions) - one member of the group collects data from the other members and performs an operation (min, max, add, multiply, etc.) on that data.

#### Scope:

Collective communication routines must involve all processes within the scope of a communicator. All processes are by default, members in the communicator MPI_COMM_WORLD. Additional communicators can be defined by the programmer. See the [Group and Communicator Management Routines](management_routines) section for details.

Unexpected behavior, including program failure, can occur if even one task in the communicator doesn't participate.

It is the programmer's responsibility to ensure that all processes within a communicator participate in any collective operations.

#### Programming Considerations and Restrictions:

Collective communication routines do not take message tag arguments.

Collective operations within subsets of processes are accomplished by first partitioning the subsets into new groups and then attaching the new groups to new communicators (discussed in the [Group and Communicator Management Routines](management_routines) section).

Can only be used with MPI predefined datatypes - not with MPI [Derived Data Types](derived_data_types).

MPI-2 extended most collective operations to allow data movement between intercommunicators (not covered here).

With MPI-3, collective operations can be blocking or non-blocking. Only blocking operations are covered in this tutorial.