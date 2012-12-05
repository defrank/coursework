/*******************************************************************************
 * $Id: initaliens.c,v 1.1 2012-05-12 18:06:03-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   initaliens.c - implementation file
 *
 * DESCRIPTION
 *   Initializes the aliens usage.  It is at the users discretion to correctly
 *   use the semaphores and prevent freeing a semaphore with a non-empty process
 *   list.
 *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <lib.h>
#include <assert.h>
#include <unistd.h>
#include <time.h>
#include <string.h>


///
// SHARED VARIABLES
// Initializations
//
// Semaphores:
//   line_1 (1)
//   line_2 (1)
//   line_3 (1)
//   mutex (1)
//   depends (0)
//   reproducing (2)
//   raising (0)
//   group (3)
//
// Integers:
//   gathered = 0
///


////
// main
//
int main (int argc, char **argv) {
   // set the execution name
   char *exec = argv[0];

   // print a usage area for insufficient arguments
   if (argc != 1) {
      fprintf (stderr, "%s: %s: incorrect usage\n", exec, "ERROR");
      fprintf (stderr, "%s: %s\n", "USAGE", exec);
      return -1;
   }

   // increase random
   srandom (time (NULL));

   // hide misleading index numbers
   int one = 0;
   int two = 1;
   int three = 2;
   
   // semaphore variables
   int line[3];
   line[one] = seminit(0, 1);
   line[two] = seminit (0, 1);
   line[three] = seminit (0, 1);
   int mutex = seminit (0, 1);
   int depends = seminit (0, 0);
   int reproducing = seminit (0, 2);
   int raising = seminit (0, 0);
   int group = seminit (0, 3);

   // shared variable
   int gathered = 0;

   // file pointer
   FILE *fp; // file pointers for shared files

   // write the shared semaphores from "aliensemvars"
   fp = fopen ("aliensemvars", "w");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliensemvars\n", exec, "ERROR");
      return -1;
   }
   fprintf (fp, "%d ", line[one]);
   fprintf (fp, "%d ", line[two]);
   fprintf (fp, "%d ", line[three]);
   fprintf (fp, "%d ", mutex);
   fprintf (fp, "%d ", depends);
   fprintf (fp, "%d ", group);
   fprintf (fp, "%d ", reproducing);
   fprintf (fp, "%d\n", raising);
   fclose (fp);

   // write the shared variable gathered
   fp = fopen ("aliengathvar", "w");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      return -1;
   }
   fprintf (fp, "%d\n", gathered);
   fclose (fp);

   // return successful
   return 1;
}
