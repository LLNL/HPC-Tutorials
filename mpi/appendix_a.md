---
layout: tutorial_page
title: "Appendix A"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

- These man pages were derived from the MVAPICH 0.9 implementation of MPI and may differ from the man pages of other implementations.
- Not all MPI routines are shown
- \* = deprecated in MPI-2.0, replaced in MPI-3.0
- The complete MPI-3 standard (2012) defines over 430 routines.

{% raw %}
<table style="border-collapse:collapse;border-spacing:0" class="tg">
    <thead>
        <tr>
            <th style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;position:-webkit-sticky;position:sticky;text-align:center;top:-1px;vertical-align:top;will-change:transform;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Environment Management Routines</span></th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Abort.txt'>MPI_Abort</a></span></td>
            <td
                style="background-color:#DDD;border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Errhandler_create.txt'>MPI_Errhandler_create*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Errhandler_free.txt'>MPI_Errhandler_free</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Errhandler_get.txt'>MPI_Errhandler_get*</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Errhandler_set.txt'>MPI_Errhandler_set*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Error_class.txt'>MPI_Error_class</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Error_string.txt'>MPI_Error_string</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Finalize.txt'>MPI_Finalize</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Get_processor_name.txt'>MPI_Get_processor_name</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Get_version.txt'>MPI_Get_version</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Init.txt'>MPI_Init</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Initialized.txt'>MPI_Initialized</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Wtick.txt'>MPI_Wtick</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Wtime.txt'>MPI_Wtime</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Point-to-Point Communication Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Bsend.txt'>MPI_Bsend</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Bsend_init.txt'>MPI_Bsend_init</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Buffer_attach.txt'>MPI_Buffer_attach</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Buffer_detach.txt'>MPI_Buffer_detach</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cancel.txt'>MPI_Cancel</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Get_count.txt'>MPI_Get_count</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Get_elements.txt'>MPI_Get_elements</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Ibsend.txt'>MPI_Ibsend</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Iprobe.txt'>MPI_Iprobe</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Irecv.txt'>MPI_Irecv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Irsend.txt'>MPI_Irsend</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Isend.txt'>MPI_Isend</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Issend.txt'>MPI_Issend</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Probe.txt'>MPI_Probe</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Recv.txt'>MPI_Recv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Recv_init.txt'>MPI_Recv_init</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Request_free.txt'>MPI_Request_free</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Rsend.txt'>MPI_Rsend</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Rsend_init.txt'>MPI_Rsend_init</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Send.txt'>MPI_Send</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Send_init.txt'>MPI_Send_init</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Sendrecv.txt'>MPI_Sendrecv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Sendrecv_replace.txt'>MPI_Sendrecv_replace</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Ssend.txt'>MPI_Ssend</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Ssend_init.txt'>MPI_Ssend_init</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Start.txt'>MPI_Start</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Startall.txt'>MPI_Startall</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Test.txt'>MPI_Test</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Test_cancelled.txt'>MPI_Test_cancelled</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Testall.txt'>MPI_Testall</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Testany.txt'>MPI_Testany</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Testsome.txt'>MPI_Testsome</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Wait.txt'>MPI_Wait</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Waitall.txt'>MPI_Waitall</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Waitany.txt'>MPI_Waitany</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Waitsome.txt'>MPI_Waitsome</a></span></td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Collective Communication Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Allgather.txt'>MPI_Allgather</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Allgatherv.txt'>MPI_Allgatherv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Allreduce.txt'>MPI_Allreduce</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Alltoall.txt'>MPI_Alltoall</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Alltoallv.txt'>MPI_Alltoallv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Barrier.txt'>MPI_Barrier</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Bcast.txt'>MPI_Bcast</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Gather.txt'>MPI_Gather</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Gatherv.txt'>MPI_Gatherv</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Op_create.txt'>MPI_Op_create</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Op_free.txt'>MPI_Op_free</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Reduce.txt'>MPI_Reduce</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Reduce_scatter.txt'>MPI_Reduce_scatter</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Scan.txt'>MPI_Scan</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Scatter.txt'>MPI_Scatter</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Scatterv.txt'>MPI_Scatterv</a></span></td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Process Group Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_compare.txt'>MPI_Group_compare</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_difference.txt'>MPI_Group_difference</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_excl.txt'>MPI_Group_excl</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_free.txt'>MPI_Group_free</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_incl.txt'>MPI_Group_incl</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_intersection.txt'>MPI_Group_intersection</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_range_excl.txt'>MPI_Group_range_excl</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_range_incl.txt'>MPI_Group_range_incl</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_rank.txt'>MPI_Group_rank</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_size.txt'>MPI_Group_size</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_translate_ranks.txt'>MPI_Group_translate_ranks</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Group_union.txt'>MPI_Group_union</a></span></td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Communicators Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_compare.txt'>MPI_Comm_compare</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_create.txt'>MPI_Comm_create</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_dup.txt'>MPI_Comm_dup</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_free.txt'>MPI_Comm_free</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_group.txt'>MPI_Comm_group</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_rank.txt'>MPI_Comm_rank</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_remote_group.txt'>MPI_Comm_remote_group</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_remote_size.txt'>MPI_Comm_remote_size</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_size.txt'>MPI_Comm_size</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_split.txt'>MPI_Comm_split</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Comm_test_inter.txt'>MPI_Comm_test_inter</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Intercomm_create.txt'>MPI_Intercomm_create</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Intercomm_merge.txt'>MPI_Intercomm_merge</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Derived Types Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_commit.txt'>MPI_Type_commit</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_contiguous.txt'>MPI_Type_contiguous</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_extent.txt'>MPI_Type_extent*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_free.txt'>MPI_Type_free</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_hindexed.txt'>MPI_Type_hindexed*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_hvector.txt'>MPI_Type_hvector*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_indexed.txt'>MPI_Type_indexed</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_lb.txt'>MPI_Type_lb</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_size.txt'>MPI_Type_size</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_struct.txt'>MPI_Type_struct*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_ub.txt'>MPI_Type_ub*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Type_vector.txt'>MPI_Type_vector</a></span></td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Virtual Topology Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_coords.txt'>MPI_Cart_coords</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_create.txt'>MPI_Cart_create</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_get.txt'>MPI_Cart_get</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_map.txt'>MPI_Cart_map</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_rank.txt'>MPI_Cart_rank</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_shift.txt'>MPI_Cart_shift</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cart_sub.txt'>MPI_Cart_sub</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Cartdim_get.txt'>MPI_Cartdim_get</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Dims_create.txt'>MPI_Dims_create</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graph_create.txt'>MPI_Graph_create</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graph_get.txt'>MPI_Graph_get</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graph_map.txt'>MPI_Graph_map</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graph_neighbors.txt'>MPI_Graph_neighbors</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graph_neighbors_count.txt'>MPI_Graph_neighbors_count</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Graphdims_get.txt'>MPI_Graphdims_get</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Topo_test.txt'>MPI_Topo_test</a></span></td>
        </tr>
        <tr>
            <td style="background-color:#98ABCE;border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;font-weight:bold;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal"
                colspan="4"><span style="background-color:#98ABCE">Miscellaneous Routines</span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Address.txt'>MPI_Address*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Attr_delete.txt'>MPI_Attr_delete*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Attr_get.txt'>MPI_Attr_get*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Attr_put.txt'>MPI_Attr_put*</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Keyval_create.txt'>MPI_Keyval_create*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Keyval_free.txt'>MPI_Keyval_free*</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Pack.txt'>MPI_Pack</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Pack_size.txt'>MPI_Pack_size</a></span></td>
        </tr>
        <tr>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Pcontrol.txt'>MPI_Pcontrol</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;color:#339;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;text-decoration:underline;vertical-align:top;word-break:normal">
                <span style="font-weight:normal;font-style:normal;color:#000"><a
                    href='/mpi/MPI_appendix/MPI_Unpack.txt'>MPI_Unpack</a></span></td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
            <td
                style="border-color:inherit;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal">
            </td>
        </tr>
    </tbody>
</table>
{% endraw %}
