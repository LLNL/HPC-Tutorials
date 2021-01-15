C ****************************************************************************
C FILE: mpi_struct.f
C DESCRIPTION:
C   MPI tutorial example code: Structure Derived Datatype
C AUTHOR: Blaise Barney
C LAST REVISED: 04/02/05
C ****************************************************************************

      program struct
      include 'mpif.h'

      integer NELEM
      parameter(NELEM=25)
      integer numtasks, rank, source, dest, tag, i,  ierr
      integer stat(MPI_STATUS_SIZE)

      type Particle
      sequence
      real*4 x, y, z, velocity
      integer n, type
      end type Particle

      type (Particle) p(NELEM), particles(NELEM)
      integer particletype, oldtypes(0:1), blockcounts(0:1), 
     &        offsets(0:1), extent

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

C     Setup description of the 4 MPI_REAL fields x, y, z, velocity 
      offsets(0) = 0
      oldtypes(0) = MPI_REAL
      blockcounts(0) = 4

C     Setup description of the 2 MPI_INTEGER fields n, type 
C     Need to first figure offset by getting size of MPI_REAL
      call MPI_TYPE_EXTENT(MPI_REAL, extent, ierr)
      offsets(1) = 4 * extent
      oldtypes(1) = MPI_INTEGER
      blockcounts(1) = 2

C     Now define structured type and commit it 
      call MPI_TYPE_STRUCT(2, blockcounts, offsets, oldtypes, 
     &                     particletype, ierr)
      call MPI_TYPE_COMMIT(particletype, ierr)
  
C     Initialize the particle array and then send it to each task
      tag = 1
      if (rank .eq. 0) then
         do 10 i=0, NELEM-1
         particles(i) = Particle ( 1.0*i, -1.0*i, 1.0*i, 
     &                  0.25, i, mod(i,2) )
  10     continue

         do 20 i=0, numtasks-1
         call MPI_SEND(particles, NELEM, particletype, i, tag, 
     &                 MPI_COMM_WORLD, ierr)
  20     continue
      endif

      source = 0
      call MPI_RECV(p, NELEM, particletype, source, tag, 
     &              MPI_COMM_WORLD, stat, ierr)

      print *, 'rank= ',rank,' p(3)= ',p(3)
      call MPI_TYPE_FREE(particletype, ierr)
      call MPI_FINALIZE(ierr)
      end
