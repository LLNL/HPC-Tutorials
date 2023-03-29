---
layout: tutorial_page
title: "Usage and job priority"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The top level Flux system instance on LC systems is configured to store information about completed jobs in a database. This allows us to track usage by users and banks and assign job priorities based on that usage and the bank's allocation. In this section, we will discuss the commands for querying information about completed jobs, usage, and job priority.
### Viewing completed jobs with `flux jobs -a`
By default, the `flux jobs` command only lists your running and pending jobs. You can see all of your jobs, including completed jobs, by adding a `-a` flag. You can also show more information about any jobs with a `-o "FORMAT"` flag. See `man flux-jobs` for a detailed description of how to construct a `"FORMAT"` string and what information may be included in it.
### Submitting jobs to a non-default bank
Some LC clusters have Flux's accounting modules enabled. This allows us to use a mutli-factor priority system that includes a hierarchical fairshare algorithm. Users of these systems may have access to multiple banks. One bank will be set as your default bank, but you can choose to submit jobs using an alternate bank by adding `-o setattr=system.bank=BANK` to your `flux run|submit|alloc|batch` command.  

##### *More on accounting and usage coming soon*

---
[Section 3](/flux/section3) | Section 4 | [Exercise 4](/flux/exercises/exercise4) | [Section 5](/flux/section5)  
Back to [index](/flux/index)
