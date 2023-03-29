---
layout: tutorial_page
title: "Exercise 1: Starting Flux"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

1. Run `flux resource list` to determine if Flux is already running on your system.
2. If flux is running on the system, use `flux alloc` to get a two node allocation. If flux is not running on the system, use allocation commands appropriate to that system to get a two node allocation and start Flux with `flux start`.
3. Use `flux resource list` to query the state of the resources in your allocation.

### Notes / Solutions
1. If Flux is not running `flux resource list` will error out with:
```
flux-resource: ERROR: [Errno 2] Unable to connect to Flux: ENOENT: No such file or directory
```
2. See "Starting Flux" in [Section 1](/flux/section1).
3. See "Showing the resources in your Flux allocation" in [Section 1](/flux/section1).

---
[Introduction](/flux/intro) | [Section 1](/flux/section1) | Exercise 1 | [Section 2](/flux/section2)  
Back to [index](/flux/index)
