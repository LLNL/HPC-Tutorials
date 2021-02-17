##############################################################################
# FILE: Makefile.Ser.c
# DESCRIPTION: 
#   Makefile for all Serial C Language example codes
# AUTHOR: Blaise Barney
# LAST REVISED:  01/20/09
###############################################################################

#Uncomment your choice of compiler
CC    =   icc
#CC    =   gcc
#CC    =   pathcc
#CC    =   pgcc

#Add your choice of flags
FLAGS   =   -O1

all:	ser_2dfft       \
	ser_array       \
	ser_heat2D      \
	ser_mm          \
	ser_pi_calc     \
	ser_prime       \
	ser_wave 

clean:	
	/bin/rm  -rf    \
	initial.dat     \
	final.dat       \
	ser_2dfft       \
	ser_array       \
	ser_heat2D      \
	ser_mm          \
	ser_pi_calc     \
	ser_prime       \
	ser_wave        \
        *.o


ser_2dfft:  ser_2dfft.c ser_fft.h
	$(CC) $(FLAGS) ser_2dfft.c -o ser_2dfft -lm 

ser_array: ser_array.c
	${CC} ${FLAGS} ser_array.c -o ser_array 

ser_heat2D: ser_heat2D.c
	${CC} ${FLAGS} ser_heat2D.c -o ser_heat2D 

ser_mm: ser_mm.c
	${CC} ${FLAGS} ser_mm.c -o ser_mm 

ser_pi_calc: ser_pi_calc.c
	${CC} ${FLAGS} ser_pi_calc.c -o ser_pi_calc 

ser_prime: ser_prime.c
	${CC} ${FLAGS} ser_prime.c -o ser_prime  -lm

ser_wave: ser_wave.c
	${CC} ${FLAGS} ser_wave.c -o ser_wave -lm

