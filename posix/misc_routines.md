---
layout: tutorial_page
title: "Miscellaneous Routines"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

[`pthread_self()`](man/pthread_self.txt)

[`pthread_equal(thread1, thread2)`](man/pthread_equal.txt)

`pthread_self` returns the unique, system assigned thread ID of the calling thread.

`pthread_equal` compares two thread IDs. If the two IDs are different 0 is returned, otherwise a non-zero value is returned.

Note that for both of these routines, the thread identifier objects are opaque and can not be easily inspected. Because thread IDs are opaque objects, the C language equivalence operator (`==`) should not be used to compare two thread IDs against each other, or to compare a single thread ID against another value.

[`pthread_once(once_control, init_routine)`](man/pthread_once.txt)

`pthread_once` executes the `init_routine` exactly once in a process. The first call to this routine by any thread in the process executes the given `init_routine`, without parameters. Any subsequent calls will have no effect.

The `init_routine` routine is typically an initialization routine.

The `once_control` parameter is a synchronization control structure that requires initialization prior to calling `pthread_once`. For example:

```C
pthread_once_t once_control = PTHREAD_ONCE_INIT;`
```
