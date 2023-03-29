---
layout: tutorial_page
title: "Exercise 5: Hierarchical job submission"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

1. Clone the workflow examples from the [flux github](https://github.com/flux-framework/flux-workflow-examples.git):
```
$ git clone https://github.com/flux-framework/flux-workflow-examples.git
$ cd flux-workflow-examples/hierarchical-launching
```
2. Run the [hierarchical launching](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/hierarchical-launching/README.html) example. Review the launcher scripts to understand which `flux` commands are launching Flux instances and which are not.

### Notes / Solutions
2. This workflow example explicitly includes instructions for getting a Slurm allocation and starting Flux. See [Section 1](/flux/section1) for general instructions on getting an allocation in Flux or starting Flux under Slurm.

---
[Section 4](/flux/section4) | [Section 5](/flux/section5) | Exercise 5 | [Section 6](/flux/section6)  
Back to [index](/flux/index)
