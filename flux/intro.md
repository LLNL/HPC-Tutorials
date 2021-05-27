---
layout: tutorial_page
title: "Introduction"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The functions of a batch system can be thought of in terms of workload management and resource management. The batch system software has to manage the programs, scripts, etc that the users want to run (workload) on the nodes, cores, gpus, etc of a cluster (resource). Batch system software does this by allowing users to submit units of work (jobs) to a queue, assigning priorities to those jobs, and then allocating resources to the highest priority jobs as the resources become available. In traditional batch systems, the user can then manage their allocated resources by assigning nodes, cores, etc to individual job steps, but all of this is managed by one parent control daemon. Flux extends on this by making each allocation a full featured Flux instance. This hierarchical model means that users can easily create allocations within their allocation(s), allowing for [highly complex workflows](https://dl.acm.org/doi/10.1145/3295500.3356197) that take full advantage of the complex hardware in modern HPC clusters, or just [launching a whole lot of jobs really quickly](https://lc.llnl.gov/uqp/docs/themis/index.html).

Flux's hierarchical design necessitates a change in how we think about jobs and the language that we use to describe them. In a traditional resource manager like Slurm or LSF, a job consists of an allocation, which describes the hardware resources assigned to the job, and one or more job steps, which describe the work to be done on that hardware. While there are options to try to assign job steps to specific resources in an allocation, a Slurm or LSF allocation cannot be divided into sub-allocations. In Flux, the Flux instance keeps track of the hardware resources available and a job is a unit of work to be done on a subset of that hardware. The key difference between Flux and other resource managers is that it is trivial to start new Flux instances on a subset of the resources managed by a parent instance. So, rather than getting an allocation and launching job steps in that allocation, users get a new Flux instance and can launch jobs or other Flux instances on subsets of the resources in that instance.

The first part of this tutorial will introduce you to the basic commands that will allow you use Flux like a traditional resource manager. For this section, you can think of a Flux instance as being analogous to a Slurm or LSF allocation, and a Flux job as being analogous to a job step. You will learn how to either run a job in a running multi-user Flux instance or start a Flux instance in an allocation that you get in another batch system and run jobs in that. You will then learn how to run a job or jobs in a batch script and monitor your running or submitted work.

The second part of this tutorial will give you the tools to run more complex workflows in your Flux jobs. Here, the differences between Flux instances and Slurm or LSF allocations will become clearer. You will dig deeper into Flux's hierarchical nature and create instances within your instance. You will also get an introduction to Flux's Python bindings so that you can write job scripts with all of the power and flexibility of Python.

---
Introduction | [Section 1](/flux/section1)  
Back to [index](/flux/index)
