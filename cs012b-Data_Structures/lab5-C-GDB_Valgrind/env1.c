/*
 * $Id: env1.c,v 1.1 2009-11-04 21:04:22-08 - - $
 * Derek Frank, dmfrank@ucsc.edu 
 */

#include <stdlib.h>
#include <stdio.h>

extern char **environ;

int main (int argc, char **argv) {
   printf ("%p\n", (void*) environ);
   putenv ("FOO=BAR\n");
   printf ("%p\n", (void*) environ);
}
