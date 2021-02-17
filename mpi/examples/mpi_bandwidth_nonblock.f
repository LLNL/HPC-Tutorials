C****************************************************************************
C FILE: mpi_bandwidth_nonblock.f
C DESCRIPTION:
C   Provides non-blocking point-to-point communications timings for any even 
C   number of MPI tasks.
C AUTHOR: Blaise Barney
C LAST REVISED: 01/23/09
C***************************************************************************/ 

      program smp_bandwidth

      include 'mpif.h'
   
      integer MAXTASKS, STARTSIZE, ENDSIZE, INCREMENT, ROUNDTRIPS
      parameter (MAXTASKS=8192)
C     Change the next four parameters to suit your case
      parameter (STARTSIZE= 100000)
      parameter (ENDSIZE= 1000000)
      parameter (INCREMENT=100000)
      parameter (ROUNDTRIPS=100)

      integer numtasks, rank, n, i, j, rndtrps, nbytes, start, 
     &        end, incr, src, dest, tag, taskpairs(0:MAXTASKS),
     &        ierr, stats(MPI_STATUS_SIZE,2), reqs(2), namelength
      double precision  thistime, bw, bestbw, worstbw, totalbw, 
     &        bestall, avgall, worstall,
     &        avgbw, timings(3,0:MAXTASKS/2), tmptimes(3), 
     &        resolution, t1, t2
      character  msgbuf(ENDSIZE)
      character(MPI_MAX_PROCESSOR_NAME)   host, hostmap(0:MAXTASKS)

C     Some initializations and error checking 
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      i = MOD(numtasks, 2)
      if (i .ne. 0) then
        print *, 'ERROR: Must be an even number of tasks!'
        print *, 'Quitting...'
        call MPI_Abort(MPI_COMM_WORLD,ierr)
        stop
      endif
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      start = STARTSIZE;
      end = ENDSIZE;
      incr = INCREMENT;
      rndtrps = ROUNDTRIPS;
      tag = 1
      do i=1,end
        msgbuf(i) = 'x'
      enddo

C     All tasks send their host name to task 0 
      call MPI_GET_PROCESSOR_NAME(host, namelength, ierr)
      call MPI_GATHER(host, MPI_MAX_PROCESSOR_NAME, MPI_CHARACTER,
     &                hostmap, MPI_MAX_PROCESSOR_NAME,
     &                MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

C     Determine who my send/receive partner is and tell task 0 */
      if (rank .lt. numtasks/2) then
        dest = numtasks/2 + rank
        src = dest
      endif
      if (rank .ge. numtasks/2) then
        dest = rank - numtasks/2
        src = dest
      endif
      call MPI_GATHER(dest, 1, MPI_INTEGER, taskpairs, 1, MPI_INTEGER,
     &                0, MPI_COMM_WORLD, ierr)

      if (rank .eq. 0) then
      resolution = MPI_WTICK()
      print *,' '
      print *,'******************** MPI Bandwidth Test ',
     &        '********************'
      print *,'Message start size=',start,'bytes'
      print *,'Message finish size=',end,'bytes'
      print *,'Incremented by',incr,'bytes per iteration'
      print *,'Roundtrips per iteration=',rndtrps
      print *,'MPI_Wtick resolution = ',resolution
      print *,'***************************************************',
     &        '*********'
      do i=0,numtasks-1
        write(*,100) i, hostmap(i), taskpairs(i)
 100    format(' task ',I4,' is on ', A24, ' partner=',I4)
      enddo
      print *,'***************************************************',
     &        '*********'
      endif


C*************************** first half of tasks *****************************
C  These tasks send/receive messages with their partner task, and then do a 
C  few bandwidth calculations based upon message size and timings.           

      if (rank .lt. numtasks/2) then
        do n=start, end, incr
          bestbw = 0.0
          worstbw = .99E+30
          totalbw = 0.0
          nbytes =  n
          do i=1, rndtrps
            t1 = MPI_WTIME()
            call MPI_IRECV(msgbuf, n, MPI_CHARACTER, src, tag, 
     &                    MPI_COMM_WORLD, reqs(1), ierr)
            call MPI_ISEND(msgbuf, n, MPI_CHARACTER, dest, tag, 
     &                    MPI_COMM_WORLD, reqs(2), ierr)
            call MPI_WAITALL(2, reqs, stats, ierr)
            t2 = MPI_WTIME()
            thistime = t2 - t1
            bw = (nbytes*2) / thistime
            totalbw = totalbw + bw
            bestbw = MAX(bw, bestbw)
            worstbw = MIN(bw, worstbw)
          enddo
C         Convert to megabytes per second
          bestbw = bestbw/1000000.0
          avgbw = (totalbw/1000000.0)/(1.0 * rndtrps)
          worstbw = worstbw/1000000.0

C         Task 0 collects timings from all relevant tasks 
          if (rank .eq. 0) then
C           Keep track of my own timings first
            timings(1,0) = bestbw
            timings(2,0) = avgbw
            timings(3,0) = worstbw
C           Initialize overall averages
            bestall = 0.0
            avgall = 0.0
            worstall = 0.0
C           Now receive timings from other tasks and print results. Note
C           that this loop will be appropriately skipped if there are 
C           only two tasks.
            do j=1, (numtasks/2) - 1
              call MPI_RECV(timings(1,j), 3, MPI_DOUBLE_PRECISION, 
     &                      j, tag, MPI_COMM_WORLD, status, ierr)
            enddo
            print *,'***Message size: ',n,' *** best  /  avg  / worst ',
     &              '(MB/sec)'
            do j=0, (numtasks/2)-1
              write(*,200) j, taskpairs(j), timings(1,j),
     &               timings(2,j), timings(3,j)
 200          format('    task pair: ', I4, '-', I4, '     ', 3F8.2)
              bestall = bestall + timings(1,j)
              avgall = avgall + timings(2,j)
              worstall = worstall + timings(3,j)
            enddo
            write(*,201) bestall/(numtasks/2), avgall/(numtasks/2), 
     &           worstall/(numtasks/2)
 201          format('    OVERALL AVERAGES:        ', 3F8.2)
     
          else 
C           Other tasks send their timings to task 0
            tmptimes(1) = bestbw
            tmptimes(2) = avgbw
            tmptimes(3) = worstbw
            call MPI_SEND(tmptimes, 3, MPI_DOUBLE_PRECISION, 0, 
     &                    tag, MPI_COMM_WORLD, ierr)
          endif
        enddo
      endif


C**************************** second half of tasks ***************************
C These tasks do nothing more than send and receive with their partner task 

      if (rank .ge. numtasks/2) then
        do n=start, end, incr
          do i=1, rndtrps
            call MPI_IRECV(msgbuf, n, MPI_CHARACTER, src, tag,
     &                    MPI_COMM_WORLD, reqs(1), ierr)
            call MPI_ISEND(msgbuf, n, MPI_CHARACTER, dest, tag,
     &                    MPI_COMM_WORLD, reqs(2), ierr)
            call MPI_WAITALL(2, reqs, stats, ierr)
          enddo
        enddo
      endif


      call MPI_FINALIZE(ierr)
      end
