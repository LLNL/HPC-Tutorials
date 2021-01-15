##############################################################################
# FILE: Makefile.MPI.c
# DESCRIPTION:
#   Makefile for all MPI C Language example codes
# AUTHOR: Blaise Barney
# LAST REVISED:  05/25/17
###############################################################################

#Uncomment your choice of compiler
CC    =   mpicc
#CC    =   mpiicc
#CC    =   mpigcc
#CC    =   mpipgcc

#Add your choice of flags
FLAGS   =   -O2 -w

all:    mpi_array       \
        mpi_bandwidth   \
        mpi_cartesian   \
        mpi_contig      \
        mpi_group       \
        mpi_heat2D      \
        mpi_indexed     \
        mpi_mm          \
        mpi_pi_reduce   \
        mpi_pi_send     \
        mpi_ping        \
        mpi_prime       \
        mpi_ringtopo    \
        mpi_scatter     \
        mpi_struct      \
        mpi_latency     \
        mpi_vector      \
        mpi_wave        


clean:  
	/bin/rm -rf     \
	initial.dat     \
	final.dat       \
	mpi_array       \
	mpi_bandwidth   \
	mpi_cartesian   \
	mpi_contig      \
	mpi_group       \
	mpi_heat2D      \
	mpi_indexed     \
	mpi_mm          \
	mpi_pi_reduce   \
	mpi_pi_send     \
	mpi_ping        \
	mpi_prime       \
	mpi_ringtopo    \
	mpi_scatter     \
	mpi_struct      \
	mpi_latency     \
	mpi_vector      \
	mpi_wave        \
        *.o
        


mpi_array:  mpi_array.c
	$(CC) $(FLAGS) mpi_array.c -o mpi_array

mpi_bandwidth:  mpi_bandwidth.c 
	$(CC) $(FLAGS) mpi_bandwidth.c -o mpi_bandwidth

mpi_cartesian:  mpi_cartesian.c 
	$(CC) $(FLAGS) mpi_cartesian.c -o mpi_cartesian

mpi_contig:  mpi_contig.c 
	$(CC) $(FLAGS) mpi_contig.c -o mpi_contig

mpi_group:  mpi_group.c 
	$(CC) $(FLAGS) mpi_group.c -o mpi_group

mpi_heat2D:  mpi_heat2D.c draw_heat.c
	$(CC) $(FLAGS) mpi_heat2D.c draw_heat.c -o mpi_heat2D -lX11

mpi_indexed:  mpi_indexed.c 
	$(CC) $(FLAGS) mpi_indexed.c -o mpi_indexed

mpi_mm:  mpi_mm.c
	$(CC) $(FLAGS) mpi_mm.c -o mpi_mm

mpi_pi_reduce:  mpi_pi_reduce.c
	$(CC) $(FLAGS) mpi_pi_reduce.c -o mpi_pi_reduce

mpi_pi_send:  mpi_pi_send.c
	$(CC) $(FLAGS) mpi_pi_send.c -o mpi_pi_send

mpi_ping:  mpi_ping.c 
	$(CC) $(FLAGS) mpi_ping.c -o mpi_ping

mpi_prime:  mpi_prime.c 
	$(CC) $(FLAGS) mpi_prime.c -o mpi_prime -lm

mpi_ringtopo:  mpi_ringtopo.c 
	$(CC) $(FLAGS) mpi_ringtopo.c -o mpi_ringtopo

mpi_scatter:  mpi_scatter.c 
	$(CC) $(FLAGS) mpi_scatter.c -o mpi_scatter

mpi_struct:  mpi_struct.c 
	$(CC) $(FLAGS) mpi_struct.c -o mpi_struct

mpi_latency:  mpi_latency.c 
	$(CC) $(FLAGS) mpi_latency.c -o mpi_latency

mpi_wave:  mpi_wave.c draw_wave.c
	$(CC) $(FLAGS) mpi_wave.c draw_wave.c -o mpi_wave -lX11 -lm

mpi_vector:  mpi_vector.c 
	$(CC) $(FLAGS) mpi_vector.c -o mpi_vector




