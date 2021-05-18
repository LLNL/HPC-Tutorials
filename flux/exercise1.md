---
layout: tutorial_page
title: "Exercise 1"
release_number:
author: Ryan Day, Lawrence Livermore National Laboratory
---

## Exercise 1: getting an allocation and starting Flux
1. Run `flux resource list` to determine if Flux is already running on your system.
2. If flux is running on the system, use `flux mini alloc` to get a two node allocation. If flux is not running on the system, use allocation commands appropriate to that system to get a two node allocation and start Flux with `flux start`.
3. Use `flux resource list` to query the state of the resources in your allocation.
4. Use `flux help hwloc` to determine the command to print the detailed hardware topology for the resources in your allocation and run that command.

### Notes / Solutions
1. If Flux is not running `flux resource list` will error out with:
```
flux-resource: ERROR: [Errno 2] Unable to connect to Flux: ENOENT: No such file or directory
```
2. See "Starting Flux" in [Section 1](/flux/section1).
3. See "Showing the resources in your Flux allocation" in [Section 1](/flux/section1).
4. The flux-hwloc man page gives the helpful command `flux hwloc topology | lstopo-no-graphics --if xml -i -` for displaying a detailed view of the hardware topology.
