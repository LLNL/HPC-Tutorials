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
flux mini run -n4 ./mpi_hellosleep
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
### More user facing batch options
The Flux job submission commands have many more options for doing things like running on specific nodes or queues, modifying your job environment, specifying task mappings, and more. See, for example, `man flux-run` for details on all of the options available.

---
[Section 2](/flux/section2) | Section 3 | [Exercise 3](/flux/exercises/exercise3) | [Section 4](/flux/section4)  
Back to [index](/flux/index)
