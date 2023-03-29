---
layout: tutorial_page
title: "Hierarchical job submission"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

One of the key innovations of Flux is the ability to easily start Flux instances within a parent Flux instances. This allows users to create separate allocations on different subsets of their allocated resources and assign different portions of their workflow to those resources. The basic command line interface for Flux has two commands that create new Flux instances, and you've already been using one of them. The `flux batch` command described in [section 3](/flux/section3) is actually creating a flux instance that the `flux run` commands are running in. Similarly, `flux alloc` can be used to create a new instance, but blocks until its work is complete.
### Creating allocations inside of an allocation
We can use the `flux resource list` and `flux jobs` commands discussed in [section 1](/flux/section1) and [section 2](/flux/section2) to demonstrate the differences between running in an allocation (`flux run` or `flux submit`) and creating a new allocation (`flux batch` or `flux alloc`). We will start with a two node allocation:
```
sh-4.2$ flux resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free      2       40        0 rzalastor[6-7]
 allocated      0        0        0
      down      0        0        0
sh-4.2$
```
We can submit work directly to this allocation as discussed previously and see that work with `flux jobs`. Note that the two `sleep` processes ended up on different nodes in the allocation:
```
sh-4.2$ flux submit -n1 sleep 10m
f4M6c3TKd
sh-4.2$ flux submit -n1 sleep 10m
f4NGSibEo
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   f4NGSibEo day36    sleep       R      1      1   4.608s rzalastor7
   f4M6c3TKd day36    sleep       R      1      1   7.261s rzalastor6
sh-4.2$
```
We can also submit batch scripts with `flux batch`. These will create new flux instances with different hardware resources available. We will demonstrate this with two batch scripts. `script1.sh` creates an allocation with eight tasks spread across the two nodes of the parent allocation:
```
sh-4.2$ cat script1.sh
#!/bin/sh

flux batch \
-N 2 \
-n 8 \
<<- 'END_OF_SCRIPT'
    #!/bin/sh

    date
    flux resource list
    flux run -n4 ./mpi_hellosleep &
    flux run -n4 ./mpi_hellosleep &
    sleep 3
    flux jobs
    wait
    echo 'job complete'

END_OF_SCRIPT
sh-4.2$
```
In this script, the four tasks of each `flux run` will be spread across both nodes. In contrast, `script2.sh` creates an allocation with eight tasks on just one of the nodes of the parent allocation, so all of the tasks from both `flux run` commands will be on the same node:
```
sh-4.2$ cat script2.sh
#!/bin/sh

flux batch \
-N 1 \
-n 8 \
<<- 'END_OF_SCRIPT'
    #!/bin/sh

    date
    flux resource list
    flux run -n4 ./mpi_hellosleep &
    flux run -n4 ./mpi_hellosleep &
    sleep 3
    flux jobs
    wait
    echo 'job complete'

END_OF_SCRIPT
sh-4.2$
```
If we submit both of these scripts, running `flux jobs` in the parent allocation will tell us that both are running alongside the `sleep` processes that we submitted earlier, but will not tell us any information about the `mpi_hellosleep` processes that we submitted in the scripts:
```
sh-4.2$ ./script1.sh
f4SLQ6Hbu
sh-4.2$ ./script2.sh
f4TgsWGkX
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   f4TgsWGkX day36    batchscrip  R      8      1   3.506s rzalastor6
   f4SLQ6Hbu day36    batchscrip  R      8      2   6.576s rzalastor[6-7]
   f4NGSibEo day36    sleep       R      1      1   15.81s rzalastor7
   f4M6c3TKd day36    sleep       R      1      1   18.46s rzalastor6
sh-4.2$
```
In order to get information about the work submitted in `script1.sh` and `script2.sh` we must attach to those jobs (recall the `flux job attach` command from [section 2](/flux/section2)) or look at their log files. For the allocation created by `script1.sh`, we can see that flux knows about 8 cores on two nodes and that the `mpi_hellosleep` tasks are running across both nodes:
```
sh-4.2$ cat flux-f4SLQ6Hbu.out
Mon Mar 29 15:55:33 PDT 2021
     STATE NNODES   NCORES    NGPUS NODELIST
      free      2        8        0 rzalastor[6-7]
 allocated      0        0        0
      down      0        0        0
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor6 going to sleep
task 2 on rzalastor7 going to sleep
task 3 on rzalastor7 going to sleep
task 1 on rzalastor6 going to sleep
task 2 on rzalastor7 going to sleep
task 3 on rzalastor7 going to sleep
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor6 going to sleep
task 1 on rzalastor6 going to sleep
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
     fQ2mquh day36    mpi_hellos  R      4      2   2.841s rzalastor[6-7]
     fQ1HrdM day36    mpi_hellos  R      4      2   2.845s rzalastor[6-7]
sh-4.2$
```
Looking at the job log for the allocation created by `script2.sh`, we can see that flux knows about 8 cores on one node and that all of the mpi_hellosleep tasks are running on this one node:
```
sh-4.2$ cat flux-f4TgsWGkX.out
Mon Mar 29 15:55:35 PDT 2021
     STATE NNODES   NCORES    NGPUS NODELIST
      free      1        8        0 rzalastor6
 allocated      0        0        0
      down      0        0        0
task 3 on rzalastor6 going to sleep
task 2 on rzalastor6 going to sleep
task 1 on rzalastor6 going to sleep
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor6 going to sleep
task 3 on rzalastor6 going to sleep
task 1 on rzalastor6 going to sleep
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor6 going to sleep
task 2 on rzalastor6 going to sleep
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
     fGcqSBi day36    mpi_hellos  R      4      1   2.870s rzalastor6
     fGcqSBh day36    mpi_hellos  R      4      1   2.871s rzalastor6
sh-4.2$
```
While this is an admittedly contrived example, it begins to show how Flux's hierarchical model allows fine grained control of the hardware resources in a job allocation. This is key to the more complex workflows that are enabled by Flux. It is also worth pointing out the difference between the behavior of Flux and what would happen if you were to run a similar set of commands under a traditional resource manager such as Slurm or LSF. If you run a batch command (e.g. `sbatch` or `bsub`) in a Slurm or LSF job script, that command will cause a new job to be enqueued. No immediate work will be performed and it is unlikely that the job will end up running on the same hardware resources as the allocation that you were in when you ran the command.

---
[Section 4](/flux/section4) | Section 5 | [Exercise 5](/flux/exercises/exercise5) | [Section 6](/flux/section6)  
Back to [index](/flux/index)
