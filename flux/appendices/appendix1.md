---
layout: tutorial_page
title: "Getting Flux"
release_number: LLNL-WEB-822959
author: Ryan Day, Lawrence Livermore National Laboratory
---

If you are running on a non-LC cluster or system and Flux is not already installed, there are a couple of ways to easily install a local copy that you can use to launch work in an allocation from whatever resource management software is running on your system. These are described in detail on the [Flux readthedocs page](https://flux-framework.readthedocs.io/en/latest/quickstart.html#building-the-code). Briefly, they are:
1. [Spack](https://github.com/spack/spack): Flux is available as a Spack package. Once Spack is installed on your system, you can run `spack install flux-sched` to install a local copy of Flux.
2. Docker: For single-node deployments of Flux, you can download and run a flux-sched Docker image from [DockerHub](https://hub.docker.com/u/fluxrm) with `docker run -ti fluxrm/flux-sched:latest`.
3. Manual: If you are looking to contribute to Flux, or just like building things from source, you can clone Flux directly from [GitHub](https://github.com/flux-framework/). You'll need to clone and build both the [flux-core](https://github.com/flux-framework/flux-core) and [flux-sched](https://github.com/flux-framework/flux-sched) components with `git clone https://github.com/flux-framework/flux-core.git` and `git clone https://github.com/flux-framework/flux-sched.git`. You can then `cd` into each directory and build with:
```
$ ./autogen.sh && ./configure --prefix=$HOME/local
...
$ make -j 8
...
```
You may then check on your build with `make check` and install it in whatever directory you specified with `--prefix` by running `make install`.

---
[Section 6](/flux/section6) | Appendix 1 | [Appendix 2](/flux/appendices/appendix2)  
Back to [index](/flux/index)
