##############################################################################
# FILE Makefile.Ser.f
# DESCRIPTION:
#   Makefile for all Serial Fortran example codes
# AUTHOR: Blaise Barney
# LAST REVISED:  01/21/09
##############################################################################

#Uncomment your choice of compiler
F90    =   ifort
#F90    =   gfortran
#F90    =   pathf90
#F90    =   pgf90
 
#Add your choice of flags
FLAGS   =   -O1

all:    ser_array       \
        ser_heat2D      \
        ser_mm          \
        ser_pi_calc     \
        ser_prime       \
        ser_wave 

clean: 
	/bin/rm -rf     \
	initial.dat     \
	final.dat       \
	ser_array       \
	ser_heat2D      \
	ser_mm          \
	ser_pi_calc     \
	ser_prime       \
	ser_wave        \
	*.o


ser_array: ser_array.f
	${F90} ${FLAGS} ser_array.f -o ser_array 

ser_heat2D: ser_heat2D.f
	${F90} ${FLAGS} ser_heat2D.f -o ser_heat2D 

ser_mm: ser_mm.f
	${F90} ${FLAGS} ser_mm.f -o ser_mm 

ser_pi_calc: ser_pi_calc.f
	${F90} ${FLAGS} ser_pi_calc.f -o ser_pi_calc 

ser_prime: ser_prime.f
	${F90} ${FLAGS} ser_prime.f -o ser_prime  

ser_wave: ser_wave.f
	${F90} ${FLAGS} ser_wave.f -o ser_wave 
