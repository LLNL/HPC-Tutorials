#!/bin/csh

#******************************************************************************
# FILE: omp_bug4fix
# DESCRIPTION:
#   This script is used to set the thread stack size limit to accomodate
#   the omp_bug4 example. The example code requires @16MB per thread. For
#   safety, this script sets the stack limit to 20MB. Note that the way
#   to do this differs between architectures. 
# AUTHOR: Blaise Barney  01/12/04
# LAST REVISED: 
#*****************************************************************************/

# This is for all systems
limit stacksize unlimited

# This is for IBM AIX systems
setenv XLSMPOPTS "stack=20000000"

# This is for Linux systems 
setenv KMP_STACKSIZE 20000000

# This is for HP/Compaq Tru64 systems
setenv MP_STACK_SIZE 20000000

# Now call the executable - change the name to match yours
omp_bug4
