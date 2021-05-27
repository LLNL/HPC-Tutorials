---
layout: tutorial_page
title: "Starting Flux and getting an allocation"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

Regardless of what resource management software a cluster is running, the first step in running in a multi-user environment is to get an allocation of hardware resources. In Flux, your allocation will take the form of a Flux instance where you can use Flux commands to manage your workload on those resources. This section will tell you where to find Flux and how to start it in an allocation even if it is not the main resource manager on the cluster that you are running on.
### Finding Flux
Flux is included in the TOSS operating system on LC systems, so should be available in your standard `PATH`. You can check on this with:
```console
[day36@rzalastor1:~]$ which flux
/usr/bin/flux
[day36@rzalastor1:~]$ flux --version
commands:    		0.26.0
libflux-core:		0.26.0
libflux-security:	0.4.0
build-options:		+hwloc==1.11.0
[day36@rzalastor1:~]$
```
Flux is under heavy development. At times you may want a version that is newer than the TOSS version, or just ensure that you stay on a consistent version. Builds of Flux are also installed in `/usr/global/tools/flux/` on LC clusters. You can use one of these versions by adding it to your PATH:
```console
[day36@rzalastor2:~]$ export PATH=/usr/global/tools/flux/$SYS_TYPE/default/bin:$PATH
[day36@rzalastor2:~]$ which flux
/usr/global/tools/flux/toss_3_x86_64_ib/default/bin/flux
[day36@rzalastor2:~]$ flux --version
commands:    		0.18.0
libflux-core:		0.18.0
build-options:		+hwloc==1.11.0
[day36@rzalastor2:~]$
```
Note that the `default` and `new` links can change as new versions of Flux are released.

If you are not on an LC cluster, and flux is not already installed, or if you're just into that sort of thing, you can also install Flux using `spack` or build it from source. See [Appendix I](/flux/appendices/appendixI) for more details on those options.
### Starting Flux
Even if you are on a cluster that is running another resource manager, such as Slurm or LSF, you can still use Flux to run your workload. You will need to get an allocation, then start Flux on all of the nodes in that allocation with the `flux start` command. This will start `flux-broker` processes on all of the nodes that will gather information about the hardware resources available and communicate between each other to assign your workload to those resources. On a cluster running Slurm, this will look like:
```console
[day36@rzalastor2:~]$ salloc -N2 --exclusive
salloc: Granted job allocation 234174
sh-4.2$ srun -N2 -n2 --pty --mpibind=off flux start
sh-4.2$ flux mini run -n 2 hostname
rzalastor6
rzalastor5
sh-4.2$
```
The `--mpibind=off` flag affects an LC-specific plugin, and should not be used on non-LC clusters.

If you're on a cluster that is running a multi-user Flux instance, getting an allocation with `flux-broker` processes running is even easier. You can just use the `flux mini alloc` command to get an interactive allocation or any of the batch commands described in [Section 3](/flux/section3).
### Showing the resources in your Flux instance
Flux uses [hwloc](http://manpages.org/hwloc/7) to build an internal model of the hardware available in a Flux instance. You can query this model with `flux hwloc`, or see a view of what resources are allocated and available with `flux resource list`. For example, in the Flux instance started in the previous section, we have two nodes with 20 cores each:
```
sh-4.2$ flux hwloc info   
2 Machines, 40 Cores, 40 PUs
sh-4.2$ flux resource list
     STATE NNODES   NCORES    NGPUS NODELIST
      free      2       40        0 rzalastor[5-6]
 allocated      0        0        0
      down      0        0        0
sh-4.2$ flux resource list -v
     STATE NNODES   NCORES    NGPUS LIST
      free      2       40        0 rank[0-1]/core[0-19]
 allocated      0        0        0
      down      0        0        0
sh-4.2$
```
### Getting help
Most Flux commands will give you a brief summary of their options if you add a `--help` flag. For more detailed help, you can easily access the man page for a given command with `flux help COMMAND` or `man flux-COMMAND`.

---
[Introduction](/flux/intro) | Section 1 | [Exercise 1](/flux/exercises/exercise1) | [Section 2](/flux/section2)  
Back to [index](/flux/index)
