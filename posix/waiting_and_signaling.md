---
layout: tutorial_page
title: "Waiting and Signaling on Condition Variables"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
## Routines:

[`pthread_cond_wait(condition,mutex)`](man/pthread_cond_wait.txt)

[`pthread_cond_signal(condition)`](man/pthread_cond_signal.txt)

[`pthread_cond_broadcast(condition)`](man/pthread_cond_broadcast.txt)

## Usage:

- `pthread_cond_wait()` blocks the calling thread until the specified condition is signalled. This routine should be called while mutex is locked, and it will automatically release the mutex while it waits. After signal is received and thread is awakened, mutex will be automatically locked for use by the thread. The programmer is then responsible for unlocking mutex when the thread is finished with it.
- Recommendation: Using a WHILE loop instead of an IF statement (see watch_count routine in example below) to check the waited for condition can help deal with several potential problems, such as:

  - If several threads are waiting for the same wake up signal, they will take turns acquiring the mutex, and any one of them can then modify the condition they all waited for.
  - If the thread received the signal in error due to a program bug
  - The Pthreads library is permitted to issue spurious wake ups to a waiting thread without violating the standard.

- The `pthread_cond_signal()` routine is used to signal (or wake up) another thread which is waiting on the condition variable. It should be called after mutex is locked, and must unlock mutex in order for `pthread_cond_wait()` routine to complete.
- The `pthread_cond_broadcast()` routine should be used instead of `pthread_cond_signal()` if more than one thread is in a blocking wait state.
- It is a logical error to call `pthread_cond_signal()` before calling `pthread_cond_wait()`.
- Proper locking and unlocking of the associated mutex variable is essential when using these routines. For example:
  - Failing to lock the mutex before calling `pthread_cond_wait()` may cause it NOT to block.
  - Failing to unlock the mutex after calling `pthread_cond_signal()` may not allow a matching `pthread_cond_wait()` routine to complete (it will remain blocked).
