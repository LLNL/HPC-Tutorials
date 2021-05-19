---
layout: tutorial_page
title: "Introduction"
release_number:
author: Ryan Day, Lawrence Livermore National Laboratory
---

# Introduction
The functions of a batch system can be thought of in terms of workload management and resource management. The batch system software has to manage the programs, scripts, etc that the users want to run (workload) on the nodes, cores, gpus, etc of a cluster (resource). Batch system software does this by allowing users to submit units of work (jobs) to a queue, assigning priorities to those jobs, and then allocating resources to the highest priority jobs as the resources become available. In traditional batch systems, the user can then manage their allocated resources by assigning nodes, cores, etc to individual job steps, but all of this is managed by one parent control daemon. Flux extends on this by making each allocation a full featured Flux instance. This hierarchical model means that users can easily create allocations within their allocation(s), allowing for [highly complex workflows](https://dl.acm.org/doi/10.1145/3295500.3356197) that take full advantage of the complex hardware in modern HPC clusters, or just [launching a whole lot of job steps really quickly](https://lc.llnl.gov/uqp/docs/themis/index.html).

The first part of this tutorial will introduce you to the basic commands that will allow you use Flux like a traditional resource manager. You will learn how to either get an allocation in a running multi-user Flux instance or start a Flux instance in an allocation that you get in another batch system. You will then learn how to launch work as job steps within that instance, combine units of work in a batch script, and monitor your running or submitted work.

The second part of this tutorial will give you the tools to run more complex workflows in your Flux jobs. You will dig deeper into Flux's hierarchical nature and create allocations within your allocation. You will also get an introduction to Flux's Python bindings so that you can write job scripts with all of the power and flexibility of Python.

---
Introduction | [Section 1](/flux/section1)  
Back to [index](/flux/index)
