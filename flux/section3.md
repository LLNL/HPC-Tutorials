---
layout: tutorial_page
title: "Batch jobs"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The reality of computing on shared resources is that nodes are rarely available when you're in front of the keyboard and you need to put your work into a script that can be run by the scheduler when resources become available. Your batch script may mix basic shell commands and functions that will be run serially on the first compute node in your allocation with parallel programs that are run as jobs as described in [Section 2](/flux/section2).

### Submitting a basic job script with `flux batch`
The `flux  batch` command allows you to submit batch scripts to a queue for later execution once resources are available. In the `simplescript.sh` example below, we mix shell commands to log when the job starts and that it has completed with a `flux run` command to launch an MPI program.
```
sh-4.2$ cat simplescript.sh
#!/bin/sh

date
flux run -n4 ./mpi_hellosleep
echo 'job complete'
sh-4.2$ flux batch -N2 -n4 ./simplescript.sh
fDkrou8xo
sh-4.2$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
   fDkrou8xo day36    simplescri  R      4      2   9.366s rzalastor[5-6]
sh-4.2$ ls
flux-fDkrou8xo.out  mpi_hellosleep  mpi_hellosleep.c  simplescript.sh
sh-4.2$ tail -f flux-fDkrou8xo.out
Thu Mar 11 10:36:16 PST 2021
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor5 going to sleep
task 2 on rzalastor6 going to sleep
task 1 on rzalastor5 going to sleep
task 3 on rzalastor6 going to sleep
task 0 on rzalastor5 woke up
task 2 on rzalastor6 woke up
task 1 on rzalastor5 woke up
task 3 on rzalastor6 woke up
job complete
^C
sh-4.2$
```

### Adding job submission directives to your batch script
Many resource managers allow you to put batch submission flags in your script as comments. In Flux, you can do this by prepending the flags with `#flux:` in your script. For example, the job script below will run 4 tasks on two nodes.
```
sh-4.2$ cat simplescript.sh
#!/bin/sh

#flux: -N 2
#flux: -n 4

date
flux run -n4 ./mpi_hellosleep
echo 'job complete'

sh-4.2$ flux batch ./simplescript.sh
f3YDA4qqR
sh-4.2$ tail -f flux-f3YDA4qqR.out
Mon Mar 15 13:45:01 PDT 2021
task 2 on rzalastor5 going to sleep
MASTER: Number of MPI tasks is: 4
task 0 on rzalastor4 going to sleep
task 3 on rzalastor5 going to sleep
task 1 on rzalastor4 going to sleep
task 2 on rzalastor5 woke up
task 0 on rzalastor4 woke up
task 3 on rzalastor5 woke up
task 1 on rzalastor4 woke up
job complete
^C
sh-4.2$
```

### Starting an interactive Flux instance with `flux alloc`
When you submit a job script with `flux batch`, you are actually starting a new Flux instance with its own resources and running your script in that instance. The `flux alloc` command will create a new Flux instance and start an interactive shell in it.  
```
[day36@corona211:~]$ flux alloc -N2 -n2 -t 1h
[day36@corona177:~]$ flux resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free      2       96       16 corona[177-178]
 allocated      0        0        0
      down      0        0        0
[day36@corona177:~]$ flux run -N2 -n2 hostname
corona177
corona178
[day36@corona177:~]$
```
Alternatively, you can supply `flux alloc` with a command or script and it will run that in a new Flux instance. Unlike `flux batch`, `flux alloc` will block until the command or script returns and send the standard output and error to the terminal.
```
[day36@corona211:flux_test]$ cat test_batch.sh
#!/bin/bash

echo "resources"
flux resource list

echo "hosts"
flux run -N 2 -n 2 hostname
[day36@corona211:flux_test]$ flux alloc -N2 -n2 ./test_batch.sh
resources
     STATE NNODES   NCORES    NGPUS NODELIST
      free      2       96       16 corona[177-178]
 allocated      0        0        0
      down      0        0        0
hosts
corona177
corona178
[detached: session exiting]
[day36@corona211:flux_test]
```

### Submitting jobs to an existing instance with `flux proxy`
Some user workflows involve getting an allocation (Flux instance) and submitting work to it from outside of that allocation. Flux can accomodate these types of workflows using the `flux proxy` command. You could, for example, create a two node Flux instance with `flux alloc -N2 -n96 -t 1d --bg`, then use a `flux proxy` command to submit work in that instance:
```
[day36@corona212:~]$ flux jobs
       JOBID USER     NAME       ST NTASKS NNODES  RUNTIME NODELIST
 ƒ8RmSm8mYW3 day36    flux        R      2      2   53.01s corona[177-178]
[day36@corona212:~]$ flux proxy ƒ8RmSm8mYW3 flux mini run -N2 -n2 hostname
corona177
corona178
[day36@corona212:~]$
```

### Running commands in the system instance with `flux --parent`
Since each Flux job is a fully featured Flux instance, running a Flux command inside of a job will run that command inside of that instance. For example, if you run `flux resource list` on a corona login node, it will list all of the resources in that cluster:
```
[day36@corona82:~]$ flux resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free     45     2160      360 corona[173,194,238,253-259,261-283,285-296]
 allocated     68     3264      544 corona[171,174-193,196-201,203-207,213-216,218-237,239-247,250-252]
      down      8      384       64 corona[172,195,202,217,248-249,260,284]
[day36@corona82:~]$
```
If, however, you get an interactive allocation with `flux alloc` and run `flux resource list` in that, you will only see the resources allocated to your job:
```
[day36@corona82:~]$ flux alloc -N1
flux-job: fpyF8fye25m started                                                                                                                                                                     00:00:06
[day36@corona189:~]$ flux resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free      1       48        8 corona189
 allocated      0        0        0
      down      0        0        0
[day36@corona189:~]$
```
If you want to know information about the resources available in the main system instance (the parent instance), you can add a `--parent` flag to your `flux` command:
```
[day36@corona189:~]$ flux --parent resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free     48     2304      384 corona[194,198,204,206,238,250,253-259,261-283,285-296]
 allocated     65     3120      520 corona[171,173-193,196-197,199-201,203,205,207,213-216,218-237,239-247,251-252]
      down      8      384       64 corona[172,195,202,217,248-249,260,284]
[day36@corona189:~]$
```
This becomes particularly important if, for example, you want to include a command in your batch script that will submit the next job in a series of jobs. As we will discuss further in [Section 5](/flux/section5), if you put a `flux batch myscript` command in your batch script, that command will get run as a subjob of the current job. If you want that batch job to submitted to the main system queue, you will need to use `flux --parent batch myscript`.

### Queues
Flux supports dividing compute resources into separate queues. A `-q QUEUENAME` argument to your `flux alloc|batch|run|submit` command will direct your job to a specific queue.

You can list the queues on a system, their status, and limits with the `flux queue list` command. E.g.
```
[day36@corona211:~]$ flux queue list
QUEUE    EN ST TDEFAULT   TLIMIT     NNODES     NCORES      NGPUS
pbatch*   ✔  ✔      30m       1d      0-inf      0-inf      0-inf
pdebug    ✔  ✔      30m       1h      0-inf      0-inf      0-inf
pnvmeof   ✔  ✔      30m       1d      0-inf      0-inf      0-inf
[day36@corona211:~]$
```
The `*` next to the queue name indicates which queue is the default queue for the system. `EN` is short for `enabled` and indicates whether or not users can submit jobs to the queue. `ST` is short for `started` and indicates whether or not jobs from a queue will be started by the scheduler. For example, a long timelimit queue may be accepting jobs (`enabled`) at all times, but administrators might only `start` the queue off hours or for a specific DAT.

You can also see how many nodes there are in a given queue and their status with `flux resource list -q QUEUENAME`. This command will display detailed information about how many and which nodes in a given queue are `free`, `allocated`, and `down`. Nodes listed as `free` may still be reserved for the high priority job in the queue. That job will be identifiable by the `eta: TIME` listed in the `INFO` column of `flux jobs -A -q QUEUENAME`.

### More user facing batch options
The Flux job submission commands have many more options for doing things like running on specific nodes or queues, modifying your job environment, specifying task mappings, and more. See, for example, `man flux-run` for details on all of the options available. We have also put together a [table of equivalent resource manager flags](https://hpc.llnl.gov/banks-jobs/running-jobs/batch-system-cross-reference-guides).

### Modifying submitted jobs
You can currently modify the duration, queue, and urgency of a submitted job. If you want to shorten the expected duration of your job to try to get it start via backfill (or any other reason), you can do so with `flux update JOBID duration=TIME` where `TIME` is a Flux Standard Duration (e.g. `6h`). Similarly, you can change the queue that a job is submitted to with `flux update JOBID queue=QUEUENAME`.

Modifying a job's `urgency` will hold or release the job. `flux job urgency JOBID hold` will set `JOBID`'s urgency to `0` and prevent it from being considered for scheduling. `flux job urgency JOBID default` will return the job's urgency to `16` and allow it to be started.

Privileged users (i.e. Hotline and other LC staff) can also use these commands to extend a job's duration beyond the configured queue limits or move a job to the top of the queue (`flux job urgency JOBID expedite`).

---
[Section 2](/flux/section2) | Section 3 | [Exercise 3](/flux/exercises/exercise3) | [Section 4](/flux/section4)  
Back to [index](/flux/index)
