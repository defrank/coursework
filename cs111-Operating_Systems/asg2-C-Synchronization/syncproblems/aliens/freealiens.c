/*******************************************************************************
 * $Id: freealiens.c,v 1.1 2012-05-12 18:06:03-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   freealiens.c - implementation file
 *
 * DESCRIPTION
 *   Cleans up after the aliens usage.  Be sure there are no sleeping processes
 *   on any semaphore's process list or else this will not work.
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

   // hide misleading index numbers
   int one = 0;
   int two = 1;
   int three = 2;
   
   // semaphore variables
   int line[3];
   int mutex;
   int depends;
   int reproducing;
   int raising;
   int group;


   // check if the files exist
   //if (fstat ("aliensemvars") < 0) {
   //   fprintf (stderr, "%s: %s: things seem to have been freed\n", exec, "ERROR");
   //   return -1;
   //}
   //if (fstat ("aliengathvar") < 0) {
   //   fprintf (stderr, "%s: %s: things seem to have been freed\n", exec, "ERROR");
   //   return -1;
   //}
   
   // file pointer
   FILE *fp; // file pointers for shared files

   // get the shared semaphores from "aliensemvars"
   fp = fopen ("aliensemvars", "r");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliensemvars\n", exec, "ERROR");
      return -1;
   }
   fscanf (fp, "%d", &line[one]);
   fscanf (fp, "%d", &line[two]);
   fscanf (fp, "%d", &line[three]);
   fscanf (fp, "%d", &mutex);
   fscanf (fp, "%d", &depends);
   fscanf (fp, "%d", &group);
   fscanf (fp, "%d", &reproducing);
   fscanf (fp, "%d", &raising);
   fclose (fp);

   // create and truncate files
   fp = fopen ("aliensemvars", "w");
   fprintf (fp, "\n");
   fclose (fp);
   fp = fopen ("aliengathvar", "w");
   fprintf (fp, "\n");
   fclose (fp);

   // delete/unlink the shared files
   unlink ("aliensemvars");
   unlink ("aliengathvar");


   // free/deallocate the semaphores
   int result;
   result = semfree (line[one]);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (line[two]);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (line[three]);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (mutex);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (depends);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (group);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (reproducing);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }
   result = semfree (raising);
   if (result <= 0) {
      fprintf (stderr, "%s: %s: in %s\n", exec, "ERROR", "semfree");
      fprintf (stderr, "%s: %s: %s, %s\n", exec, "ERROR",
               "if you left processes sleeping on a semaphore process list",
               "then it is your fault things are so messed up!");
      return result;
   }

   
   // return successful
   return result;
}
