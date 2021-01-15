/******************************************************************************
* FILE: ser_prime.c
* DESCRIPTION: 
*   This program generates primes. The approach taken is a "brute force"
*   method which requires increasingly greater amounts of cpu as the problem
*   size increases. It should lend itself well to an embarrassingly parallel
*   solution since each prime can be computed independently of all other
*   primes.
* AUTHOR: Blaise Barney 11/25/95 - adapted from version contributed by 
*   Richard Ng & Wong Sze Cheong during MHPCC Singapore Workshop (8/22/95).
* LAST REVISED: 04/12/05
****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define LIMIT     2500000     /* Increase this to find more primes */
#define PRINT     100000      /* Print a line after this many numbers */


int isprime(int n) {
int i,squareroot;
if (n>10) {
   squareroot = (int) sqrt(n);
   for (i=3; i<=squareroot; i=i+2)
      if ((n%i)==0)
         return 0;
   return 1;
   }
/* Assume first four primes are counted elsewhere. Forget everything else */
else
   return 0;
}


int main(int argc, char *argv[])
{
int n,                           /* loop variables */
    pc,                          /* prime counter */
    foundone;                    /* most recent prime found */

printf("Starting. Numbers to be scanned= %d\n",LIMIT);

pc=4;     /* Assume the primes less than 10 (2,3,5,7) are counted here */

for (n=11; n<=LIMIT; n=n+2) {
   if (isprime(n)) {
      pc++;
      foundone = n;
      /***** Optional: print each prime as it is found 
      printf("%d\n",foundone);
      *****/
      }			
   if ( (n-1)%PRINT == 0 ) 
      printf("Numbers scanned= %d   Primes found= %d\n",n-1,pc);
   }
printf("Done. Largest prime is %d Total primes %d\n",foundone,pc);
} 
