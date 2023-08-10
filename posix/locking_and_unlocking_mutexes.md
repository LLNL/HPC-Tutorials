---
layout: tutorial_page
title: "Locking and Unlocking Mutexes"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

## Routines:

[`pthread_mutex_lock(mutex)`](man/pthread_mutex_lock.txt)

[`pthread_mutex_trylock(mutex)`](man/pthread_mutex_trylock.txt)

[`pthread_mutex_unlock(mutex)`](man/pthread_mutex_unlock.txt)

## Usage:

The `pthread_mutex_lock()` routine is used by a thread to acquire a lock on the specified mutex variable. If the mutex is already locked by another thread, this call will block the calling thread until the mutex is unlocked.

`pthread_mutex_trylock()` will attempt to lock a mutex. However, if the mutex is already locked, the routine will return immediately with a "busy" error code. This routine may be useful in preventing deadlock conditions, as in a priority-inversion situation.

`pthread_mutex_unlock()` will unlock a mutex if called by the owning thread. Calling this routine is required after a thread has completed its use of protected data if other threads are to acquire the mutex for their work with the protected data. An error will be returned if:

* If the mutex was already unlocked
* If the mutex is owned by another thread

There is nothing "magical" about mutexes...in fact they are akin to a "gentlemen's agreement" between participating threads. It is up to the programmer to ensure that all threads make lock and unlock mutexes appropriately. The following scenario demonstrates a logical error:

<table style="border-collapse:collapse;border-spacing:0" class="tg"><thead><tr><th style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Thread 1<br><span style="font-weight:bold;font-style:normal;text-decoration:none">Lock</span><br><span style="font-weight:bold;font-style:normal;text-decoration:none">A = 2</span><br><span style="font-weight:bold;font-style:normal;text-decoration:none">Unlock</span></th><th style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Thread 2<br><span style="font-weight:bold;font-style:normal;text-decoration:none">Lock</span><br><span style="font-weight:bold;font-style:normal;text-decoration:none">A = A+1</span><br><span style="font-weight:bold;font-style:normal;text-decoration:none">Unlock       </span><br></th><th style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:left;top:-1px;vertical-align:top;will-change:transform;word-break:normal">Thread 3<br><span style="font-weight:bold;font-style:normal;text-decoration:none">        </span><br><span style="font-weight:bold;font-style:normal;text-decoration:none">A = A*B</span><br></th></tr></thead></table>

**Question**: When more than one thread is waiting for a locked mutex, which thread will be granted the lock first after it is released?

<details>
  <summary>Click for answer.</summary>
Unless thread priority scheduling (not covered) is used, the assignment will be left to the native system scheduler and may appear to be more or less random.
</details>
