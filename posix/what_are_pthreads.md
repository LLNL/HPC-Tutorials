---
layout: tutorial_page
title: "What are Pthreads?"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

## Pthreads Overview: What are Pthreads?

Historically, hardware vendors have implemented their own proprietary versions of threads. These implementations differed substantially from each other making it difficult for programmers to develop portable threaded applications.

In order to take full advantage of the capabilities provided by threads, a standardized programming interface was required.
* For UNIX systems, this interface has been specified by the IEEE POSIX 1003.1c standard (1995).
* Implementations adhering to this standard are referred to as POSIX threads, or Pthreads.
* Most hardware vendors now offer Pthreads in addition to their proprietary API's.

The POSIX standard has continued to evolve and undergo revisions, including the Pthreads specification.

Some useful links:
* [standards.ieee.org/ieee/1003.1/7101/](https://standards.ieee.org/ieee/1003.1/7101/)
* [www.opengroup.org/austin/papers/posix_faq.html](http://www.opengroup.org/austin/papers/posix_faq.html)

Pthreads are defined as a set of C language programming types and procedure calls, implemented with a `pthread.h` header/include file and a thread library - though this library may be part of another library, such as `libc`, in some implementations.
