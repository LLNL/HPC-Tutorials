---
layout: tutorial_page 
title: "Creating and Destroying Mutexes"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

## Routines:

[pthread_mutex_init](man/pthread_mutex_init.txt) (mutex,attr)

[pthread_mutex_destroy](man/pthread_mutex_destroy.txt) (mutex)

[pthread_mutexattr_init](man/pthread_mutexattr_init.txt) (attr)

[pthread_mutexattr_destroy](man/pthread_mutexattr_destroy.txt) (attr)

## Usage:

Mutex variables must be declared with type `pthread_mutex_t`, and must be initialized before they can be used. There are two ways to initialize a mutex variable:

  1. Statically, when it is declared. For example: 
        `pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;`
        
  2. Dynamically, with the `pthread_mutex_init()` routine. This method permits setting mutex object attributes, *attr*.
    
The mutex is initially unlocked.

The *attr* object is used to establish properties for the mutex object, and must be of `type pthread_mutexattr_t` if used (may be specified as NULL to accept defaults). The Pthreads standard defines three optional mutex attributes:
* Protocol: Specifies the protocol used to prevent priority inversions for a mutex.
* Prioceiling: Specifies the priority ceiling of a mutex.
* Process-shared: Specifies the process sharing of a mutex.

Note that not all implementations may provide the three optional mutex attributes.

The `pthread_mutexattr_init()` and `pthread_mutexattr_destroy()` routines are used to create and destroy mutex attribute objects respectively.

`pthread_mutex_destroy()` should be used to free a mutex object which is no longer needed.
