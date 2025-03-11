---
layout: tutorial_page
title: "Launching and tracking Flux jobs"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

In the previous section, we learned how to find Flux, get an allocation, and query the compute resources in that allocation. Now, we are ready to launch work on those compute resources and get some work done. When you launch work in Flux, that work takes the form of jobs that can be either blocking or non-blocking. Blocking jobs will run to completion before control is returned to the terminal, whereas non-blocking jobs are enqueued, allowing you to immediately submit more work in the allocation.

Before we get into submitting and managing Flux jobs, we should also discuss Flux's jobids as they're a bit different than what you'll find in other resource management software. Remember that an allocation in Flux is a fully featured Flux instance. Rather than create sequential numeric ids within each instance, Flux combines the submit time, an instance id, and sequence number to create identifiers that are unique for each job in a Flux instance. This avoids requiring a central allocator for jobids which improves the scalability of job submission within instances. There are options to display these identifiers in a number of ways, but the default is an 8 character string prepended by an `f`, e.g. `fBsFXaow5` for the job submitted in the example below. For more details on Flux's identifiers, see the [FLUID documentation](https://flux-framework.readthedocs.io/projects/flux-rfc/en/latest/spec_19.html).

### Submit blocking Flux jobs with `flux run`
If you want your work to block until it completes, the `flux run` command will submit a job and then wait until it is complete before returning. For example, in a two node allocation, we can launch an mpi program with 4 tasks:
```
sh-4.2$ flux run -n4 ./mpi_hellosleep
task 2 on rzalastor6 going to sleep
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor5 going to sleep
task 3 on rzalastor6 going to sleep
task 1 on rzalastor5 going to sleep
task 2 on rzalastor6 woke up
task 0 on rzalastor5 woke up
task 3 on rzalastor6 woke up
task 1 on rzalastor5 woke up
sh-4.2$
```

### Submit non-blocking Flux jobs with `flux submit`
If you just want to queue up work in a Flux instance, the `flux submit` command will submit the job and return immediately. As in the example above, here we will submit a 4 task mpi program in our two node allocation:
```
sh-4.2$ flux submit -n4 --output=job_{{id}}.out ./mpi_hellosleep
fBsFXaow5
sh-4.2$ tail -f job_fBsFXaow5.out
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor5 going to sleep
task 1 on rzalastor5 going to sleep
task 2 on rzalastor6 going to sleep
task 3 on rzalastor6 going to sleep
task 0 on rzalastor5 woke up
task 1 on rzalastor5 woke up
task 2 on rzalastor6 woke up
task 3 on rzalastor6 woke up
^C
sh-4.2$
```

### Submit dependent jobs with `--dependency=`
If you want to submit a Flux job that won't start until another job has completed or reached some other state, you can add a `--dependency=` flag to any `flux run` or other job submission command. Flux currently supports five dependency conditions:

  `--dependency=after:JOBID`  
  &emsp;job will not start until JOBID has started  
  `--dependency=afterany:JOBID`  
  &emsp;job will not start until JOBID has completed  
  `--dependency=afterok:JOBID`  
  &emsp;job will not start until JOBID has completed successfully  
  `--dependency=afternotok:JOBID`  
  &emsp;job will not start until JOBID has completed unsuccessfully  
  `--dependency=begin-time:TIMESTAMP`  
  &emsp;job will not start until TIMESTAMP  

These dependency conditions can be used with `flux run`, `flux submit`, and the `flux batch` and `flux alloc` commands described in the [Section 3](/flux/section3).

### Managing Flux jobs with `flux jobs` and `flux job`
If you have multiple Flux jobs running and queued you can list those jobs with the `flux jobs` command, and manage them with `flux job`. For example, in two node instance with 20 cores per node, we can see the states of the job steps that we've submitted as:
```
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7AC3114K
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7As1t7pB
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7BK65VFu
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7Bgjnzwy
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7WUMj9qH
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7WsybiDu
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7XUqnpcF
sh-4.2$ flux submit -N1 -n10 ./mpi_hellosleep
f7Y14ABhq
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   f7WUMj9qH day36    mpi_hellos PD     10      -        - -
   f7WsybiDu day36    mpi_hellos PD     10      -        - -
   f7XUqnpcF day36    mpi_hellos PD     10      -        - -
   f7Y14ABhq day36    mpi_hellos PD     10      -        - -
   f7Bgjnzwy day36    mpi_hellos  R     10      1   48.41s rzalastor5
   f7BK65VFu day36    mpi_hellos  R     10      1   49.25s rzalastor5
   f7As1t7pB day36    mpi_hellos  R     10      1   50.27s rzalastor4
   f7AC3114K day36    mpi_hellos  R     10      1    51.8s rzalastor4
sh-4.2$
```
We can also see any output from a given job with `flux job attach` and kill a given job with `flux job cancel`:
```
sh-4.2$ flux job attach f7XUqnpcF
task 9 on rzalastor5 going to sleep
task 8 on rzalastor5 going to sleep
task 7 on rzalastor5 going to sleep
task 6 on rzalastor5 going to sleep
task 5 on rzalastor5 going to sleep
task 4 on rzalastor5 going to sleep
task 3 on rzalastor5 going to sleep
task 2 on rzalastor5 going to sleep
task 1 on rzalastor5 going to sleep
MASTER: Number of MPI tasks is: 10
task 0 on rzalastor5 going to sleep
^Z
[1]+  Stopped(SIGTSTP)        flux job attach f7XUqnpcF
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   f7Y14ABhq day36    mpi_hellos  R     10      1   47.18s rzalastor5
   f7XUqnpcF day36    mpi_hellos  R     10      1   47.99s rzalastor5
   f7WsybiDu day36    mpi_hellos  R     10      1   49.05s rzalastor4
   f7WUMj9qH day36    mpi_hellos  R     10      1   50.44s rzalastor4
sh-4.2$ flux job cancel f7XUqnpcF
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   f7Y14ABhq day36    mpi_hellos  R     10      1   59.17s rzalastor5
   f7WsybiDu day36    mpi_hellos  R     10      1   1.017m rzalastor4
   f7WUMj9qH day36    mpi_hellos  R     10      1    1.04m rzalastor4
sh-4.2$
```

---
[Section 1](/flux/section1) | Section 2 | [Exercise 2](/flux/exercises/exercise2) | [Section 3](/flux/section3)  
Back to [index](/flux/index)
