---
layout: tutorial_page
title: "Flux Python API Basics"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

Flux's command line interface allows users to quickly and easily schedule relatively simple workloads. Users with more complex workflows will most likely find that Flux's Python API provides a better, more powerful interface. This section will introduce the Flux Python API and demonstrate simple job submission via Python. The [exercises](/flux/exercises/exercise6) will explore some more interesting examples on the [Flux readthedocs](https://flux-framework.readthedocs.io/projects/flux-workflow-examples/en/latest/index.html).
### Importing Flux and the Flux Jobspec
In order to access the Flux API from your python scripts, you'll need to import the `flux` module and the Flux Jobspec class. The `flux` module will allow you to get a handle for the current Flux instance and functions to submit and interact with jobs in that instance. The Jobspec class will generate jobs that you can submit. You can import Flux and the Flux Jobspec as:
```
import flux
from flux.job import JobspecV1
```
You can then create a handle for the currently running Flux instance with:
```
f = flux.Flux()
```
Note that in order to `import flux` you need to have the Flux python bindings in your `PYTHONPATH`. If you are in a Flux allocation this will have been done automatically. If you're not already in an allocation you may need to add an appropriate path to your `PYTHONPATH` (e.g. `PYTHONPATH=/usr/lib64/flux/python3.6:$PYTHONPATH`). Alternatively, you can call the Python that Flux was built against with `#!/bin/flux python`.
### Using the Flux API to submit a simple job
Once you've imported Flux and the Jobspec and created a Flux handle, you can construct a job request using the Jobspec class and submit it using the `flux.job.submit` function. For example, to submit a `sleep 60` command:
```
sleep_jobreq = JobspecV1.from_command(
  ["sleep","60"], num_tasks=1, cores_per_task=1
  )
sleep_jobid = flux.job.submit(
  f, sleep_jobreq
  )
```
The `sleep_jobid` returned by `flux.job.submit()` can be passed to other `flux` functions such as `flux.job.cancel()` and `flux.job.wait_async()` to manage the job.
### Using the Flux Executor to launch jobs asynchronously
While `flux.job.submit()` allows you to launch jobs from a Python script and manage them by jobid, cancelling and waiting on individual jobs can become unwieldy in workloads with multiple jobs. The Flux Executor uses Python's [`concurrent.futures` module](https://docs.python.org/3/library/concurrent.futures.html) to provide tools for managing a submitted workload. In order to make use of the Flux Executor, import the `concurrent.futures` module and `FluxExecutor` class, along with the `JobspecV1` class to build your job submissions, as:
```
import concurrent.futures
from flux.job import JobspecV1, FluxExecutor
```
Once you have all of that, you can build a job as before, but when you submit it with `FluxExecutor.submit()`, the return value with be a `futures` object that can be managed with `concurrent.futures` methods rather than a jobid. In the simple example below, the `concurrent.futures.wait()` method is used to wait for a `sleep` task to complete:
```
with FluxExecutor() as executor:
  sleep_jobreq = JobspecV1.from_command(
    ["sleep","60"], num_tasks=1, cores_per_task=1
    )
  sleep_future = executor.submit(sleep_jobreq)
  done, not_done = concurrent.futures.wait(
    [sleep_future], return_when=concurrent.futures.FIRST_COMPLETED
    )
```
As you'll see in the [exercises](/flux/exercises/exercise6) the `FluxExecutor` would generally be used to submit many jobs and store their futures in a list for `concurrent.futures` methods to manage.

---
[Section 5](/flux/section5) | Section 6 | [Exercise 6](/flux/exercises/exercise6) | [Appendix 1](/flux/appendices/appendix1)  
Back to [index](/flux/index)
