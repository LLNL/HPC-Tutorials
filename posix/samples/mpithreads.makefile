###############################################################################
# FILE: mpithreads.makefile
# DESCRIPTION:
#   Makefile for MPI with threads example codes.
# LAST REVISED: 0712/129  Blaise Barney
###############################################################################

all: mpithreads_serial mpithreads_threads mpithreads_mpi mpithreads_both

mpithreads_serial: mpithreads_serial.c
	gcc -o mpithreads_serial mpithreads_serial.c 

mpithreads_threads: mpithreads_threads.c
	gcc -pthread -o mpithreads_threads mpithreads_threads.c

mpithreads_mpi: mpithreads_mpi.c
	mpicc -o mpithreads_mpi mpithreads_mpi.c

mpithreads_both: mpithreads_both.c
	mpicc -pthread -o mpithreads_both mpithreads_both.c

