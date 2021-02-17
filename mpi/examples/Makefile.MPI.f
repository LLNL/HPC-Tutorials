##############################################################################
# FILE: Makefile.MPI.f
# DESCRIPTION:
#   Makefile for all IBM MPI Fortran example codes
# AUTHOR: Blaise Barney
# LAST REVISED:  05/25/17
##############################################################################

#Uncomment your choice of compiler
F90    =    mpif90
#F90    =    mpiifort
#F90    =    mpigfortran
#F90    =    mpipgf90
CC 	=   icc

#Add your choice of flags
FLAGS   =   -O2 -w

.PHONY:	cleanup

all:    mpi_array       \
        mpi_mm          \
        mpi_pi_send     \
        mpi_pi_reduce   \
        mpi_wave        \
        mpi_heat2D      \
        mpi_latency     \
        mpi_bandwidth   \
        mpi_prime       \
        mpi_ping        \
        mpi_ringtopo    \
        mpi_scatter     \
        mpi_contig      \
        mpi_vector      \
        mpi_indexed     \
        mpi_struct      \
        mpi_group       \
        mpi_cartesian	\
	cleanup   

cleanup:  
	/bin/rm -rf *.o     

clean:  
	/bin/rm -rf     \
	mpi_array       \
	mpi_mm          \
	mpi_pi_send     \
	mpi_pi_reduce   \
	mpi_wave        \
	mpi_heat2D      \
	mpi_latency     \
	mpi_bandwidth   \
	mpi_prime       \
	mpi_ping        \
	mpi_ringtopo    \
	mpi_scatter     \
	mpi_contig      \
	mpi_vector      \
	mpi_indexed     \
	mpi_struct      \
	mpi_group       \
	mpi_cartesian   \
	initial.dat     \
	final.dat       \
        *.o


mpi_array:  mpi_array.f
	$(F90) $(FLAGS) mpi_array.f -o mpi_array

mpi_mm:  mpi_mm.f
	$(F90) $(FLAGS) mpi_mm.f -o mpi_mm

mpi_pi_send:  mpi_pi_send.f
	$(F90) $(FLAGS) mpi_pi_send.f -o mpi_pi_send

mpi_pi_reduce:  mpi_pi_reduce.f 
	$(F90) $(FLAGS) mpi_pi_reduce.f -o mpi_pi_reduce

mpi_wave:  mpi_wave.f draw_wavef.o
	$(F90) $(FLAGS) mpi_wave.f draw_wavef.o -o mpi_wave -lX11
draw_wavef.o:  draw_wavef.c
	$(CC) -c draw_wavef.c 

mpi_heat2D:  mpi_heat2D.f draw_heatf.o
	$(F90) $(FLAGS) mpi_heat2D.f draw_heatf.o -o mpi_heat2D -lX11
draw_heatf.o:  draw_heatf.c
	$(CC) -c $(FLAGS) draw_heatf.c 

mpi_latency:  mpi_latency.f
	$(F90) $(FLAGS) mpi_latency.f -o mpi_latency

mpi_bandwidth:  mpi_bandwidth.f 
	$(F90) $(FLAGS) mpi_bandwidth.f -o mpi_bandwidth

mpi_prime:  mpi_prime.f 
	$(F90) $(FLAGS) mpi_prime.f -o mpi_prime

mpi_ping:  mpi_ping.f 
	$(F90) $(FLAGS) mpi_ping.f -o mpi_ping

mpi_ringtopo:  mpi_ringtopo.f 
	$(F90) $(FLAGS) mpi_ringtopo.f -o mpi_ringtopo

mpi_scatter:  mpi_scatter.f 
	$(F90) $(FLAGS) mpi_scatter.f -o mpi_scatter

mpi_contig:  mpi_contig.f 
	$(F90) $(FLAGS) mpi_contig.f -o mpi_contig

mpi_vector:  mpi_vector.f 
	$(F90) $(FLAGS) mpi_vector.f -o mpi_vector

mpi_indexed:  mpi_indexed.f 
	$(F90) $(FLAGS) mpi_indexed.f -o mpi_indexed

mpi_struct:  mpi_struct.f 
	$(F90) $(FLAGS) mpi_struct.f -o mpi_struct

mpi_group:  mpi_group.f 
	$(F90) $(FLAGS) mpi_group.f -o mpi_group

mpi_cartesian:  mpi_cartesian.f 
	$(F90) $(FLAGS) mpi_cartesian.f -o mpi_cartesian


