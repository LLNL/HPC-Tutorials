---
layout: tutorial_page
title: "Batch jobs"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The reality of computing on shared resources is that nodes are rarely available when you're in front of the keyboard and you need to put your work into a script that can be run by the scheduler when resources become available. Your batch script may mix basic shell commands and functions that will be run serially on the first compute node in your allocation with parallel programs that are run as job steps as described in the [previous section](/flux/section2).
### Submitting a basic job script with `flux mini batch`
The `flux mini batch` command allows you to submit batch scripts to a queue for later execution once resources are available. In the `simplescript.sh` example below, we mix shell commands to log when the job starts and that it has completed with a `flux mini run` command to launch an MPI program.
```
sh-4.2$ cat simplescript.sh
#!/bin/sh

date
flux mini run -n4 ./mpi_hellosleep
echo 'job complete'
sh-4.2$ flux mini batch -N2 -n4 ./simplescript.sh
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
### Making your job script submission self documenting
Many resource managers allow you to put batch submission scripts in your script as comments. For example, in a Slurm sbatch script, you can use `#SBATCH -N 2` in your script to request two nodes in your allocation. Flux does not have directly analogous functionality. If you want to include your job submission flags in your script, you can use a [heredoc](https://en.wikipedia.org/wiki/Here_document) to include the `flux mini batch` command in your script and run it directly.
```
sh-4.2$ cat simplescript.sh
#!/bin/sh

flux mini batch \
-N 2 \
-n 4 \
<<- 'END_OF_SCRIPT'
    #!/bin/sh

    date
    flux mini run -n4 ./mpi_hellosleep
    echo 'job complete'

END_OF_SCRIPT
sh-4.2$ ./simplescript.sh
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
### More user facing batch options
There are a bunch of things like job dependencies, queues, qos, modifying jobs, holding jobs, etc that aren't in flux yet, but will be described here once they are.

---
[Section 2](/flux/section2) | Section 3 | [Exercise 3](/flux/exercises/exercise3) | [Section 4](/flux/section4)  
Back to [index](/flux/index)
