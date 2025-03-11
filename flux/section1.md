---
layout: tutorial_page
title: "Starting Flux"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

Regardless of what resource management software a cluster is running, the first step in running in a multi-user environment is to get an allocation of hardware resources. In Flux, your allocation will take the form of a Flux instance where you can use Flux commands to manage your workload on those resources. This section will tell you where to find Flux and how to start an instance of it in an allocation even if it is not the main resource manager on the cluster that you are running on.
### Finding Flux
Flux is included in the TOSS operating system on LC systems, so should be available in your standard `PATH`. You can check on this with:
```console
[day36@corona211:~]$ which flux
/usr/bin/flux
[day36@corona211:~]$ flux --version
commands:    		0.48.0
libflux-core:		0.48.0
libflux-security:	0.8.0
build-options:		+ascii-only+hwloc==2.8.0+zmq==4.3.4
[day36@corona211:~]$
```
LC clusters running TOSS 4 are tracking Flux releases closely, but on TOSS 3 or non-LC clusters you may want a newer version. You can install a local build of Flux using `spack` or build it from source. See [Appendix I](/flux/appendices/appendixI) for more details on those options.

### Starting Flux
If you're on an LC cluster such as elcap or tuolumne where Flux is running as the system level scheduler, you can skip this step. You can just use the `flux alloc` command to get an interactive allocation or any of the batch commands described in [Section 3](/flux/section3).

If you are on a cluster that is running another resource manager, such as Slurm or LSF, you can still use Flux to run your workload.  You will need to get an allocation using the native resource managers commands (e.g. `salloc`), then start Flux on all of the nodes in that allocation with the `flux start` command. This will start `flux-broker` processes on all of the nodes that will gather information about the hardware resources available and communicate between each other to assign your workload to those resources. On a cluster running Slurm, this will look like:
```console
[day36@rzalastor2:~]$ salloc -N2 --exclusive
salloc: Granted job allocation 234174
sh-4.2$ srun -N2 -n2 --pty --mpibind=off flux start
sh-4.2$ flux run -n 2 hostname
rzalastor6
rzalastor5
sh-4.2$
```
The `--mpibind=off` flag affects an LC-specific plugin, and should not be used on non-LC clusters.

### Showing the resources in your Flux instance
When started as a job in another resource manager, Flux uses [hwloc](http://manpages.org/hwloc/7) to build an internal model of the hardware available in a Flux instance. You can see a view of what resources are allocated and available with `flux resource list`. For example, in the Flux instance started in the previous section, we have two nodes with 20 cores each:
```
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
