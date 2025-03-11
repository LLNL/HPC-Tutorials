---
layout: tutorial_page
title: "Completed jobs, usage and job priority"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The top level Flux system instance on LC systems is configured to store information about completed jobs in a database. This allows us to track usage by users and banks and assign job priorities based on that usage and the bank's allocation. In this section, we will discuss the commands for querying information about completed jobs, usage, and job priority.

### Viewing completed jobs with `flux jobs -a`
By default, the `flux jobs` command only lists your running and pending jobs. You can see all of your jobs, including completed jobs, by adding a `-a` flag. You can also show more information about any jobs with a `-o "FORMAT"` flag. See `man flux-jobs` for a detailed description of how to construct a `"FORMAT"` string and what information may be included in it. The pre-defined `endreason` format string can be useful for a quick summary of your completed jobs (e.g. `flux jobs -a -o endreason`). You can also get detailed information like the Flux event log and submitted jobspec with the `flux job info` command.

For older completed jobs, accounting information (job size, duration, user, bank, etc) is available from the Flux accounting database via the `flux account view-jobs` command.

### Submitting jobs to a non-default bank
Some LC clusters have Flux's accounting modules enabled. This allows us to use a mutli-factor priority system that includes a hierarchical fairshare algorithm. Users of these systems may have access to multiple banks. One bank will be set as your default bank, but you can choose to submit jobs using an alternate bank by adding `--bank=BANK` to your `flux run|submit|alloc|batch` command.

### Viewing job and bank priorities
You can view the priorities of all jobs in the queue, along with information about dependencies and job urgency (hold, normal, expedite) with `flux jobs -o deps` or, for jobs from all users, `flux jobs -A -o deps`. Note that the highest priority job will also have an estimated start time listed as `eta: TIME` in the `INFO` column of the `flux jobs -A` output.

You can check on which banks you have access to and your current priority in those banks with `flux account view-user USERNAME`. The full bank tree with all banks, users, and priorities can be viewed with `flux account view-bank -t root`. LC also provides a `bankinfo` command that provides more options for viewing smaller parts of the bank tree.

---
[Section 3](/flux/section3) | Section 4 | [Exercise 4](/flux/exercises/exercise4) | [Section 5](/flux/section5)  
Back to [index](/flux/index)
