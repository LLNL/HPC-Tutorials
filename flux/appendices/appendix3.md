---
layout: tutorial_page
title: "Workflow management software"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

The Flux Python bindings provide tools for managing complex workflows, but using them requires software development skills that domain scientists and other end users may not possess. There are several projects that leverage these bindings to build general, user-friendly workflow managers.

[Maestro](https://github.com/LLNL/maestrowf) is a workflow manager developed at LLNL that uses Flux to orchestrate complex or simple workloads based on straightforward workflow descriptions. Most notably, it has enabled the Gordon Bell finalist and SC19 Best Paper [MuMMI project](https://dl.acm.org/doi/10.1145/3295500.3356197).

[Merlin](https://github.com/LLNL/merlin) is an LLNL developed workflow manager specifically for running machine learning workflows. It uses Maestro for describing tasks and Flux for task launch.

[UQPipeline](https://lc.llnl.gov/uqp/docs/index.html) is an LLNL internal tool used for [uncertainty quantification](https://wci.llnl.gov/simulation/computer-codes/uncertainty-quantification) studies. Its [Themis](https://lc.llnl.gov/uqp/docs/themis/index.html) component can use Flux to launch large ensembles of jobs quickly.

---
[Appendix 2](/flux/appendices/appendix2) | Appendix 3  
Back to [index](/flux/index)
