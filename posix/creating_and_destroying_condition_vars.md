---
layout: tutorial_page
title: "Creating and Destroying Condition Variables"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

## Routines:
[`pthread_cond_init(condition, attr)`](man/pthread_cond_init.txt)

[`pthread_cond_destroy(condition)`](man/pthread_cond_destroy.txt)

[`pthread_condattr_init(attr)`](man/pthread_condattr_init.txt)

[`pthread_condattr_destroy(attr)`](man/pthread_condattr_destroy.txt)

## Usage:

Condition variables must be declared with type `pthread_cond_t`, and must be initialized before they can be used. There are two ways to initialize a condition variable:

1. Statically, when it is declared. For example:
    `pthread_cond_t myconvar = PTHREAD_COND_INITIALIZER;`

2. Dynamically, with the `pthread_cond_init()` routine.

    The ID of the created condition variable is returned to the calling thread through the condition parameter. This method permits setting condition variable object attributes (`attr`).

The optional `attr` object is used to set condition variable attributes. There is only one attribute defined for condition variables: process-shared, which allows the condition variable to be seen by threads in other processes. The attribute object, if used, must be of type `pthread_condattr_t` (may be specified as NULL to accept defaults). Note that not all implementations may provide the process-shared attribute.

The `pthread_condattr_init()` and `pthread_condattr_destroy()` routines are used to create and destroy condition variable attribute objects.

`pthread_cond_destroy()` should be used to free a condition variable that is no longer needed.
