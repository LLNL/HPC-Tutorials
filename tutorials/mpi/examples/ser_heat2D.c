/****************************************************************************
 * FILE: ser_heat2D.c
 * DESCRIPTION:  
 *   Serial HEAT2D Example - C Version
 *   This example is based on a simplified 
 *   two-dimensional heat equation domain decomposition.  The initial 
 *   temperature is computed to be high in the middle of the domain and 
 *   zero at the boundaries.  The boundaries are held at zero throughout 
 *   the simulation.  During the time-stepping, an array containing two 
 *   domains is used; these domains alternate between old data and new data.
 * AUTHOR: D. Turner
 * Last Revised: 04/15/05 Blaise Barney
 ****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#define NXPROB 100
#define NYPROB 100

struct Parms
{ 
  float cx;
  float cy;
  int nts;
} parms = {0.1, 0.1, 100};

int main(int argc, char *argv[])
{
float u[2][NXPROB][NYPROB];
int ix, iy, iz, it;
void inidat(), prtdat(), update();

printf("Starting serial version of 2D heat example...\n");
printf("Using [%d][%d] grid.\n",NXPROB, NYPROB);

/* Initialize grid and create input file */
printf("Initializing grid and creating input file:");
inidat(NXPROB, NYPROB, u);
prtdat(NXPROB, NYPROB, u, "initial.dat");
for (ix = 0; ix <= NXPROB-1; ix++) {
   u[1][ix][0] = u[0][ix][0];
   u[1][ix][NYPROB-1] = u[0][ix][NYPROB-1];
   }
for (iy = 0; iy <= NYPROB-1; iy++) {
   u[1][0][iy] = u[0][0][iy];
   u[1][NXPROB-1][iy] = u[0][NXPROB-1][iy];
   }

/* Iterate over all timesteps and create output file */
printf("Iterating over %d time steps...\n",parms.nts);
iz = 0;
for (it = 1; it <= parms.nts; it++) {
   update(NXPROB, NYPROB, &u[iz][0][0], &u[1-iz][0][0]);
   iz = 1 - iz;
   }
printf("Done. Created output file: ");
prtdat(NXPROB, NYPROB, &u[iz][0][0], "final.dat");
}


/****************************************************************************
 *  subroutine update
 ****************************************************************************/
void
update(nx, ny, u1, u2)
int nx, ny;
float *u1, *u2;
{
   int ix, iy;

   for (ix = 1; ix <= nx-2; ix++) {
      for (iy = 1; iy <= ny-2; iy++) {
         *(u2+ix*ny+iy) = *(u1+ix*ny+iy)  + 
         parms.cx * (*(u1+(ix+1)*ny+iy) + *(u1+(ix-1)*ny+iy) - 
         2.0 * *(u1+ix*ny+iy)) +
         parms.cy * (*(u1+ix*ny+iy+1) + *(u1+ix*ny+iy-1) - 
         2.0 * *(u1+ix*ny+iy));
         }
      }
}

/*****************************************************************************
 *  subroutine inidat
 *****************************************************************************/
void
inidat(nx, ny, u1)
int nx, ny;
/*float u1[nx][ny];*/
float *u1;
{
   int ix, iy;

   for (ix = 0; ix <= nx-1; ix++) 
      for (iy = 0; iy <= ny-1; iy++) 
         *(u1+ix*ny+iy) = (float)(ix * (nx - ix - 1) * iy * (ny - iy - 1));
}

/**************************************************************************
 * subroutine prtdat
 **************************************************************************/
void
prtdat(nx, ny, u1, fnam)
int nx, ny;
float *u1;
char *fnam;
{
   int ix, iy;
   FILE *fp;

   fp = fopen(fnam, "w");
   for (iy = ny-1; iy >= 0; iy--) {
      for (ix = 0; ix <= nx-1; ix++) {
        fprintf(fp, "%8.3f", *(u1+ix*ny+iy));
        if (ix != nx-1) {
           fprintf(fp, " ");
           }
       else {
          fprintf(fp, "\n");
          }
       }
    }
   fclose(fp);
   printf(" %s\n",fnam);
}
