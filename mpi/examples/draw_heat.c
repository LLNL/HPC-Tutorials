/***************************************************************************
*  FILENAME: draw_heat.c   C VERSION
*  DESCRIPTION:  
*    Called by heat2D example files.  No attempt is made to 
*    document this code.  It is simply meant to provide a graphical interface
*    for the heat2D sample code's results.
*  AUTHOR: Blaise Barney
*  LAST REVISED: 01/26/09
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define HEIGHT         750 
#define WIDTH          750
#define NCOLORS        255

typedef struct {
   Window window;
   XSizeHints hints;
   XColor backcolor;
   XColor bordcolor;
   int    bordwidth;
} MYWINDOW;

char title[] = {"draw_heat"},
     moreword[] = {"More"},
     exitword[] = {"Exit"},
     text[10];

/* To call from Fortran: uncomment the routine below with the extra underscore in the name
void draw_heat_(int nx, int ny) {
*/
/* To call from C, use the routine below - without the extra underscore */
void draw_heat(int nx, int ny) {

float 	scale, point, high, *a1, *a2, coloratio = 65535.0 / 255.0;
int 	i,j,k,x,y,done, myscreen, rectheight, rectwidth,toggle;
FILE	*initial, *final;

MYWINDOW base,more,quit;
Font 	font,font2;   
GC 		itemgc,textgc,rectgc[NCOLORS],linegc; 
XColor 	rectcolor,red,yellow,blue,green,black,white;
XEvent 	myevent;
Colormap cmap;
KeySym 	mykey;
Display *mydisp;


/* Set rgb values for colors */
yellow.red= (int) (255 * coloratio);
yellow.green= (int) (255 * coloratio);
yellow.blue= (int) (0 * coloratio);

green.red= (int) (0 * coloratio);
green.green= (int) (255 * coloratio);
green.blue= (int) (0 * coloratio);

black.red= (int) (0 * coloratio);
black.green= (int) (0 * coloratio);
black.blue= (int) (0 * coloratio);

white.red= (int) (255 * coloratio);
white.green= (int) (255 * coloratio);
white.blue= (int) (255 * coloratio);

mydisp = XOpenDisplay("");
if (!mydisp) {
   fprintf (stderr, "Hey! Either you don't have X or something's not right.\n");
   fprintf (stderr, "Guess I won't be showing the graph.  No big deal.\n");
   exit(1);
   }
myscreen = DefaultScreen(mydisp);
cmap = DefaultColormap (mydisp, myscreen);
XAllocColor (mydisp, cmap, &yellow);
XAllocColor (mydisp, cmap, &black); 
XAllocColor (mydisp, cmap, &green); 
XAllocColor (mydisp, cmap, &white); 

/* initial window position and size */
base.hints.x = 50;
base.hints.y = 50;
base.hints.width = WIDTH;
base.hints.height = HEIGHT;
base.hints.flags = PPosition | PSize;
base.bordwidth = 5;

/* window Creation */
base.window = XCreateSimpleWindow (mydisp, DefaultRootWindow (mydisp), 
            base.hints.x, base.hints.y, base.hints.width, base.hints.height, 
            base.bordwidth, black.pixel, black.pixel);
XSetStandardProperties (mydisp, base.window, title, title, None,
                        NULL, 0, &base.hints);

/* quit window position and size (subwindow of base) */
quit.hints.x = 5;
quit.hints.y = HEIGHT-75;
quit.hints.width = 70;
quit.hints.height = 30;
quit.hints.flags = PPosition | PSize;
quit.bordwidth = 5;

quit.window = XCreateSimpleWindow (mydisp, base.window, quit.hints.x, 
              quit.hints.y, quit.hints.width, quit.hints.height,
              quit.bordwidth, green.pixel, yellow.pixel);
XSetStandardProperties (mydisp, quit.window, exitword, exitword, None,
             NULL, 0, &quit.hints);
/* more window position and size (subwindow of base) */
more.hints.x = WIDTH-85;
more.hints.y = HEIGHT-75;
more.hints.width = 70;
more.hints.height = 30;
more.hints.flags = PPosition | PSize;
more.bordwidth = 5;

more.window = XCreateSimpleWindow (mydisp, base.window, more.hints.x, 
              more.hints.y, more.hints.width, more.hints.height,
              more.bordwidth, green.pixel, yellow.pixel);
XSetStandardProperties (mydisp, more.window, moreword, moreword, None,
             NULL, 0, &more.hints);

/* Load fonts */
/*
font = XLoadFont (mydisp, "Rom28");
font2 = XLoadFont (mydisp, "Rom17.500");
*/
font = XLoadFont (mydisp, "fixed");
font2 = XLoadFont (mydisp, "fixed");

/* GC creation and initialization */
textgc = XCreateGC (mydisp, base.window, 0,0);
XSetFont (mydisp, textgc, font);
XSetForeground (mydisp, textgc, green.pixel);

linegc = XCreateGC (mydisp, base.window, 0,0);
XSetForeground (mydisp, linegc, white.pixel);

itemgc = XCreateGC (mydisp, quit.window, 0,0);
XSetFont (mydisp, itemgc, font2);
XSetForeground (mydisp, itemgc, black.pixel);

/* create shades of red color scale */
for (k=0;k<NCOLORS;k++) {
  rectgc[k] = XCreateGC (mydisp, base.window, 0,0);
  rectcolor.red= (int) (k * coloratio);
  rectcolor.green= (int) (0 * coloratio);
  rectcolor.blue= (int) (0 * coloratio);
  XAllocColor (mydisp, cmap, &rectcolor);
  XSetForeground (mydisp, rectgc[k], rectcolor.pixel);
  }

/* The program is event driven; the XSelectInput call sets which kinds of */
/* interrupts are desired for each window. These aren't all used. */
XSelectInput (mydisp, base.window,
              ButtonPressMask | KeyPressMask | ExposureMask);
XSelectInput (mydisp, quit.window,
              ButtonPressMask | KeyPressMask | ExposureMask);
XSelectInput (mydisp, more.window,
              ButtonPressMask | KeyPressMask | ExposureMask);

/* window mapping -- this lets windows be displayed */
XMapRaised (mydisp, base.window);
XMapSubwindows (mydisp, base.window);

/* create space for data to be read */
a1 = (float *) malloc(nx*ny*sizeof(float));
a2 = (float *) malloc(nx*ny*sizeof(float));

/* Read the two data files created by heat2D program execution */
/* Also determine the highest value read */
high = 0.0;
initial = fopen("initial.dat","r");
for (k=0;k<nx*ny;k++) {
  fscanf(initial,"%f",a1+k);
  if ( *(a1+k) > high) high = *(a1+k);
  }
fclose(initial);

final = fopen("final.dat","r");
for (k=0;k<nx*ny;k++) 
  fscanf(final,"%f",a2+k);
fclose(final);

/* Scale rectangle size and color */
rectwidth = WIDTH/ny;
rectheight = HEIGHT/nx;
scale = ((float)NCOLORS) / high;

/* Main event loop  --  exits when user clicks on "exit" */
done = 0;
toggle = 0;
while (! done) {
   XNextEvent (mydisp, &myevent);   /* Read next event */
   switch (myevent.type) {
   case Expose:
      if (myevent.xexpose.count == 0) {
         if (myevent.xexpose.window == base.window) {
            for (x=0;x<nx;x++) {
              for (y=0;y<ny;y++) {
                if (toggle == 0)
                  point = scale * *(a1+x*ny+y);
                else
                  point = scale * *(a2+x*ny+y);
                k = (int) point;
                if (k >= NCOLORS) k = NCOLORS-1;
                XFillRectangle (mydisp, base.window, rectgc[k], y*rectwidth, 
                                x*rectheight, rectwidth, rectheight);
                }
              }
            for (x=0;x<nx;x++) 
              XDrawLine (mydisp, base.window, linegc, 1,x*rectheight,WIDTH, 
                         x*rectheight);
            for (y=0;y<ny;y++) 
              XDrawLine (mydisp, base.window, linegc, y*rectwidth,1, 
                         y*rectwidth,HEIGHT);
            XDrawString (mydisp, base.window, textgc,325,30,"Heat2D",6);
            if (toggle == 0)
              XDrawString (mydisp, base.window, textgc,325,735,"INITIAL",7);
            else
              XDrawString (mydisp,base.window,textgc,330,735,"FINAL",5);
            }

         else if (myevent.xexpose.window == quit.window) 
            XDrawString (mydisp, quit.window, itemgc, 12,20, exitword, 
                         strlen(exitword));
         else if (myevent.xexpose.window == more.window) 
            XDrawString (mydisp, more.window, itemgc, 12,20, moreword, 
                         strlen(moreword));
      }   /* case Expose */
      break;

   case ButtonPress:
      if (myevent.xbutton.window == quit.window)
         done = 1;
      else if (myevent.xbutton.window == more.window) {
         if (toggle==0) 
           toggle = 1;
         else if (toggle==1)
           toggle = 0;
         for (x=0;x<nx;x++) {
           for (y=0;y<ny;y++) {
             if (toggle==0) 
               point = scale * *(a1+x*ny+y);
             else 
               point = scale * *(a2+x*ny+y);
             k = (int) point;
             if (k >= NCOLORS) k = NCOLORS-1;
             XFillRectangle (mydisp, base.window, rectgc[k], y*rectwidth,
                            x*rectheight, rectwidth, rectheight);
             }
          }
          for (x=0;x<nx;x++)
            XDrawLine (mydisp, base.window, linegc, 1,x*rectheight,WIDTH,
                      x*rectheight);
          for (y=0;y<ny;y++)
            XDrawLine (mydisp, base.window, linegc, y*rectwidth,1,
                       y*rectwidth,HEIGHT);
          XDrawString (mydisp, base.window, textgc,325,30,"heat2D",6);
          if (toggle==0) 
            XDrawString (mydisp, base.window, textgc,325,735,"INITIAL",7);
          else
            XDrawString (mydisp,base.window,textgc,330,735,"FINAL",5);
       }

      break;

   case MappingNotify:
/*
      XRefreshKeyboardMapping (&myevent);
*/
      break;

      }  /* switch (myevent.type) */

   }   /* while (! done) */

XDestroyWindow (mydisp, base.window);
XCloseDisplay (mydisp);
}
