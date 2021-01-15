/***************************************************************************
*  FILENAME: draw_wave.c   C VERSION
*  DESCRIPTION:  
*    Called by mpi_wave routines to draw a graph of results
*  AUTHOR: Blaise Barney
*  LAST REVISED: 01/26/09
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define HEIGHT         500 
#define WIDTH          1000

typedef struct {
   Window window;
   XSizeHints hints;
   XColor backcolor;
   XColor bordcolor;
   int    bordwidth;
} MYWINDOW;

char baseword[] = {"draw_wave"},
     exitword[] = {"Exit"},
     text[10];

/* To call from Fortran: uncomment the routine below with the extra underscore in the name 
void draw_wave_(double * results) {
*/
/* To call from C, use the routine below - without the extra underscore */
void draw_wave(double * results) {

/* Note extra underscore in draw_wave routine - needed for Fortran */

float 	scale, point, coloratio = 65535.0 / 255.0;
int 	i,j,k,y, zeroaxis, done, myscreen, points[WIDTH];

MYWINDOW base, quit;
Font 	font,font2;   
GC 		itemgc,textgc,pointgc,linegc; 
XColor 	red,yellow,blue,green,black,white;
XEvent 	myevent;
Colormap cmap;
KeySym 	mykey;
Display *mydisp;


/* Set rgb values for colors */
red.red= (int) (255 * coloratio);
red.green= (int) (0 * coloratio);
red.blue = (int) (0 * coloratio);

yellow.red= (int) (255 * coloratio);
yellow.green= (int) (255 * coloratio);
yellow.blue= (int) (0 * coloratio);

blue.red= (int) (0 * coloratio);
blue.green= (int) (0 * coloratio);
blue.blue= (int) (255 * coloratio);

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
XAllocColor (mydisp, cmap, &red); 
XAllocColor (mydisp, cmap, &yellow);
XAllocColor (mydisp, cmap, &blue);
XAllocColor (mydisp, cmap, &black); 
XAllocColor (mydisp, cmap, &green); 
XAllocColor (mydisp, cmap, &white); 

/* Set up for creating the windows */
/* XCreateSimpleWindow uses defaults for many attributes,   */
/* thereby simplifying the programmer's work in many cases. */

/* base window position and size */
base.hints.x = 50;
base.hints.y = 50;
base.hints.width = WIDTH;
base.hints.height = HEIGHT;
base.hints.flags = PPosition | PSize;
base.bordwidth = 5;

/* window Creation */
/* base window */
base.window = XCreateSimpleWindow (mydisp, DefaultRootWindow (mydisp), 
              base.hints.x, base.hints.y, base.hints.width, 
              base.hints.height, base.bordwidth, black.pixel,
              black.pixel);
XSetStandardProperties (mydisp, base.window, baseword, baseword, None,
                        NULL, 0, &base.hints);

/* quit window position and size (subwindow of base) */
quit.hints.x = 5;
quit.hints.y = 450;
quit.hints.width = 70;
quit.hints.height = 30;
quit.hints.flags = PPosition | PSize;
quit.bordwidth = 5;

quit.window = XCreateSimpleWindow (mydisp, base.window, quit.hints.x, 
              quit.hints.y, quit.hints.width, quit.hints.height,
              quit.bordwidth, green.pixel, yellow.pixel);
XSetStandardProperties (mydisp, quit.window, exitword, exitword, None,
             NULL, 0, &quit.hints);

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
XSetForeground (mydisp, textgc, white.pixel);

linegc = XCreateGC (mydisp, base.window, 0,0);
XSetForeground (mydisp, linegc, white.pixel);

itemgc = XCreateGC (mydisp, quit.window, 0,0);
XSetFont (mydisp, itemgc, font2);
XSetForeground (mydisp, itemgc, black.pixel);

pointgc = XCreateGC (mydisp, base.window, 0,0);
XSetForeground (mydisp, pointgc, green.pixel);

/* The program is event driven; the XSelectInput call sets which */
/* kinds of interrupts are desired for each window.              */
/* These aren't all used. */
XSelectInput (mydisp, base.window,
              ButtonPressMask | KeyPressMask | ExposureMask);
XSelectInput (mydisp, quit.window,
              ButtonPressMask | KeyPressMask | ExposureMask);

/* window mapping -- this lets windows be displayed */
XMapRaised (mydisp, base.window);
XMapSubwindows (mydisp, base.window);

/* Scale each data point  */
zeroaxis = HEIGHT/2;
scale = (float)zeroaxis;
for(j=0;j<WIDTH;j++) 
   points[j]  = zeroaxis - (int)(results[j] * scale);

/* Main event loop  --  exits when user clicks on "exit" */
done = 0;
while (! done) {
   XNextEvent (mydisp, &myevent);   /* Read next event */
   switch (myevent.type) {
   case Expose:
      if (myevent.xexpose.count == 0) {
         if (myevent.xexpose.window == base.window) {
            XDrawString (mydisp, base.window, textgc, 775, 30, "Wave",4);
            XDrawLine (mydisp, base.window, linegc, 1,zeroaxis,WIDTH,
                       zeroaxis);
            for (j=1; j<WIDTH; j++) 
               XDrawPoint (mydisp, base.window, pointgc, j, points[j-1]);
            }

         else if (myevent.xexpose.window == quit.window) {
            XDrawString (mydisp, quit.window, itemgc, 12,20, exitword, 
                         strlen(exitword));
            }
      }   /* case Expose */
      break;

   case ButtonPress:
      if (myevent.xbutton.window == quit.window)
         done = 1;
      break;

   case KeyPress:
/*
      i = XLookupString (&myevent, text, 10, &mykey, 0);
      if (i == 1 && text[0] == 'q')
         done = 1;
*/
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
