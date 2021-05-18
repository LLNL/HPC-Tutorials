---
layout: default
title: "Exercise 5"
release_number:
author: Ryan Day, Lawrence Livermore National Laboratory
---

## Exercise 5: Hierarchical job submission
1. Clone the workflow examples from the [flux github](https://github.com/flux-framework/flux-workflow-examples.git):
```
$ git clone https://github.com/flux-framework/flux-workflow-examples.git
$ cd flux-workflow-examples/hierarchical-launching
```
2. Run the [hierarchical launching](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/hierarchical-launching/README.html) example. Review the launcher scripts to understand which `flux mini` commands are launching Flux instances and which are not.
### Notes / Solutions
2. This workflow example explicitly includes instructions for getting a Slurm allocation and starting flux. See [Section 1](section1.md) for general instructions on getting an allocation in flux or starting flux under Slurm.
