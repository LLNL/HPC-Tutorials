---
layout: tutorial_page 
title: "Exercise 2"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
<ol><li><b>Mutexes </b>
    
    <ol><li>Review, compile and run the <a href="../samples/dotprod_serial.c">dotprod_serial.c program</a>. As its name implies, it is serial - no threads are created.
    </li><li>Now review, compile and run the <a href="../samples/dotprod_mutex.c">dotprod_mutex.c</a> program. This version of the dotprod program uses threads and requires a mutex to protect the global sum as each thread updates it with their partial sums.
        </li><li>Execute the dotprod_mutex program several times and notice that the order in which threads update the global sum varies.
        </li><li>Review, compile and run the <a href="../samples/bug6.c">bug6.c</a> program.
        </li><li>Run it several times and notice what the global sum is each time? See if you can figure out why and fix it. The explanation is provided in the bug examples table above, and an example solution is provided by the <a href="../samples/bug6fix.c">bug6fix.c</a> program.
        </li><li>The <a href="../samples/arrayloops.c">arrayloops.c</a> program is another example of using a mutex to protect updates to a global sum. Feel free to review, compile and run this example code as well.</li></ol>
    </li><li><b>Condition Variables </b>
    
    <ol><li>Review, compile and run the <a href="../samples/condvar.c">condvar.c</a>program. This example is essentially the same as the shown in the tutorial. Observe the output of the three threads.
    </li><li>Now, review, compile and run the <a href="../samples/bug1fix.c">bug1.c</a> program. Observe the output of the five threads. What happens? See if you can determine why and fix the problem. The explanation is provided in the bug examples table above, and an example solution is provided by the <a href="../samples/bug1fix.c">bug1fix.c</a> program.
    </li><li>The <a href="../samples/bug1fix.c">bug4.c</a> program is yet another example of what can go wrong when using condition variables. Review, compile (for gcc include the <span class="cmd"> -lm flag) and run the code. Observe the output and then see if you can fix the problem. The explanation is provided in the bug examples table above, and an example solution is provided by the bug4fix.c program.</li></ol>
    </li><li><b>Hybrid MPI with Pthreads </b>
    
    <ol><li>Your pthreads directory should contain the following 5 codes:
        <ul><li><a href="../samples/mpithreads_serial.c">mpithreads_serial.c</a></li>
            <li><a href="../samples/mpithreads_threads.c">mpithreads_threads.c</a></li>
            <li><a href="../samples/mpithreads_mpi.c">mpithreads_mpi.c</a></li>
            <li><a href="../samples/mpithreads_both.c">mpithreads_both.c</a></li>
            <li><a href="../samples/mpithreads.makefile">mpithreads.makefile</a></li></ul></ul>
        <p>These codes implement a dot product calculation and are designed to show the progression of developing a hybrid MPI / Pthreads program from a a serial code. The problem size increases as the examples go from serial, to threads/mpi to mpi with threads.</p>
        <p><u>Suggestion:</u> simply making and running this series of codes is rather unremarkable. Using the available lab time to understand what is actually happening is the intent. The instructor is available for your questions.</p>
        </li><li>Review each of the codes. The order of the listing above shows the "progression".</li><li>Use the provided makefile to compile all of the codes at once. The makefile uses the gcc compiler - feel free to modify it and use a different compiler.<p><span class="cmd">make -f mpithreads.makefile </p>
        </li><li>Run each of the codes and observe their output:
        <table><tbody><tr valign="top"><th>Execution command</th><th>Description</th></tr><tr valign="top"><td>
            <pre><b>mpithreads_serial</b></pre>
        </td><td>Serial version - no threads or MPI</td></tr><tr valign="top"><td>
            <pre><b>mpithreads_threads</b></pre>
        </td><td>Threads only version of the code using 8 threads</td></tr><tr valign="top"><td>
            <pre><b>srun -n8 -ppReserved mpithreads_mpi</b></pre>
        </td><td>MPI only version with 8 tasks running on a single node in the special workshop pool</td></tr><tr valign="top"><td>
            <pre><b>srun -N4 -ppReserved mpithreads_both</b></pre>
        </td><td>MPI with threads using 4 tasks running on 4 different nodes, each of which spawns 8 threads, running in special workshop pool</td></tr></tbody></table></li></ol></li></ol>

<hr><p><b>This completes the exercise.</b></p>
