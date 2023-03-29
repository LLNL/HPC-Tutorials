---
layout: tutorial_page
title: "Exercise 6: Flux Python API basics"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

1. Clone the workflow examples from the [Flux github](https://github.com/flux-framework/flux-workflow-examples.git):
```
$ git clone https://github.com/flux-framework/flux-workflow-examples.git
$ cd flux-workflow-examples/job-submit-api
```
2. Run the [job submit api](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/job-submit-api/README.html) and [job cancellation](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/job-cancel/README.html) workflow examples. Review the python scripts included in those examples to see how `flux` methods are being used.
3. Run the [Python Job Submit/Wait](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/job-submit-wait/README.html), [Python Asynchronous Bulk Job Submission](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/async-bulk-job-submit/README.html), and [Using Flux job status and control API](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/job-status-control/README.html) work examples. Review the scripts included in those examples to see how the `FluxExecutor` and `concurrent.futures` are being used.

### Notes / Solutions
2. Most of the examples explicitly include instructions for getting a Slurm allocation and starting Flux. See [Section 1](/flux/section1) for general instructions on getting an allocation in Flux or starting Flux under Slurm.
3. See note 2.

---
[Section 5](/flux/section5) | [Section 6](/flux/section6) | Exercise 6 | [Appendix 1](/flux/appendices/appendix1)  
Back to [index](/flux/index)
