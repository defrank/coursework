/*******************************************************************************
 * $Id: alien.c,v 1.1 2012-05-12 18:06:03-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   alien.c - implementation file
 *
 * DESCRIPTION
 *   Runs an alien based on a given gender for a number of times specified by
 *   the user.  Must run initaliens.c to set up usage of this program.  If all
 *   goes well, i.e., there are no remaining sleeping processes on any
 *   semaphore's process list, then running freealiens.c should clean up nicely
 *   after.
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
// wait
//
//   Sleep for a random number of microseconds ranging from 20000 to 200000.
//
void dowait (int choice) {
   // choice must be 0 or something else
   if (choice == 0) {
      int msec = random() % 200001;
      if (msec < 20000) msec = 200000 - msec;
      usleep (msec);
   }else {
      int msec = random() % 400001;
      if (msec < 1000) msec = 400000 - msec;
      usleep (msec);
   }
}


////
// gender_one
//
//
//
void gender_one (char *exec, int id, int *line, int mutex, int depends,
                 int group, int reproducing, int raising) {
   int one = 0; // hide misleading index number
   int gathered; // shared variable
   FILE *fp; // file pointer for gathered shared variable file
   // print entering waiting state
   printf ("alien %d of gender 1 beginning to wait in line\n", id);
   // begin sleeping for random time
   dowait (1);
   //
   semdown (line[one]); // line_1.down()
   semdown (group); // group.down()
   semdown (mutex); // mutex.down()
   // get the shared gathered variable from "aliengathvar"
   fp = fopen ("aliengathvar", "r");
   // print error if could not open file successfully
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fscanf (fp, "%d", &gathered); // scan shared variable
   fclose (fp); // close read file
   // increment gathered
   gathered += 1;
   // check if the last of three gender residents necessary to reproduce
   if (gathered == 3) {
      gathered = 0; // attained group size so reset
      semdown (reproducing); // one of two groups to begin reproducing
      // all three have "reached" barrier
      semup (depends);
      semup (depends);
      semup (depends);
   }
   // write the shared variable gathered to "aliengathvar"
   fp = fopen ("aliengathvar", "w");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fprintf (fp, "%d\n", gathered); // write shared variable
   fclose (fp); // close write file
   //
   semup (mutex); // mutex.up()
   semdown (depends); // depends.down()
   semup (group); // group.up()
   // print entering reproducing state
   printf ("alien %d of gender 1 beginning to reproduce\n", id);
   // hit sleeping barrier
   semdown (raising); // raising.down()
   printf ("%s %d of gender 1: leaving nursery\n", "alien", id); // leaving nursery
}


////
// gender_two
//
//
//
void gender_two (char *exec, int id, int *line, int mutex, int depends,
                 int group, int reproducing, int raising) {
   int two = 1; // hide misleading index number
   int gathered; // shared variable
   FILE *fp; // file pointer for gathered shared variable file
   // print entering waiting state
   printf ("alien %d of gender 2 beginning to wait in line\n", id);
   // begin sleeping for random time
   dowait (1);
   //
   semdown (line[two]); // line_2.down()
   semdown (group); // group.down()
   semdown (mutex); // mutex.down()
   // get the shared gathered variable from "aliengathvar"
   fp = fopen ("aliengathvar", "r");
   // print error if could not open file successfully
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fscanf (fp, "%d", &gathered); // scan shared variable
   fclose (fp); // close read file
   // increment gathered
   gathered += 1;
   // check if the last of three gender residents necessary to reproduce
   if (gathered == 3) {
      gathered = 0; // attained group size so reset
      semdown (reproducing); // one of two groups to begin reproducing
      // all three have "reached" barrier
      semup (depends);
      semup (depends);
      semup (depends);
   }
   // write the shared variable gathered to "aliengathvar"
   fp = fopen ("aliengathvar", "w");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fprintf (fp, "%d\n", gathered); // write shared variable
   fclose (fp); // close write file
   //
   semup (mutex); // mutex.up()
   semdown (depends); // depends.down()
   semup (group); // group.up()
   // print entering reproducing state
   printf ("alien %d of gender 2 beginning to reproduce\n", id);
   // hit sleeping barrier
   semdown (raising); // raising.down()
   printf ("%s %d of gender 2: leaving nursery\n", "alien", id); // leaving nursery
}


////
// gender_three
//
//
//
void gender_three (char *exec, int id, int *line, int mutex, int depends,
                 int group, int reproducing, int raising) {
   int one = 0; // hide misleading index number
   int two = 1; // hide misleading index number
   int three = 2; // hide misleading index number
   int gathered; // shared variable
   FILE *fp; // file pointer for gathered shared variable file
   // print entering waiting state
   printf ("alien %d of gender 3 beginning to wait in line\n", id);
   // begin sleeping for random time
   dowait (1);
   //
   semdown (line[three]); // line_3.down()
   semdown (group); // group.down()
   semdown (mutex); // mutex.down()
   // get the shared gathered variable from "aliengathvar"
   fp = fopen ("aliengathvar", "r");
   // print error if could not open file successfully
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fscanf (fp, "%d", &gathered); // scan shared variable
   fclose (fp); // close read file
   // increment gathered
   gathered += 1;
   // check if the last of three gender residents necessary to reproduce
   if (gathered == 3) {
      gathered = 0; // attained group size so reset
      semdown (reproducing); // one of two groups to begin reproducing
      // all three have "reached" barrier
      semup (depends);
      semup (depends);
      semup (depends);
   }
   // write the shared variable gathered to "aliengathvar"
   fp = fopen ("aliengathvar", "w");
   if (fp < 0 || fp == NULL) {
      fprintf (stderr, "%s: %s: failed to open aliengathvar\n", exec, "ERROR");
      exit (-1);
   }
   fprintf (fp, "%d\n", gathered); // write shared variable
   fclose (fp); // close write file
   //
   semup (mutex); // mutex.up()
   semdown (depends); // depends.down()
   // free others in lines
   semup (line[one]);
   semup (line[two]);
   semup (line[three]);
   semup (group); // group.up()
   // print entering reproducing state
   printf ("alien %d of gender 3 beginning to reproduce\n", id);
   // sleep while reproducing
   wait (0);
   printf ("%s %d of gender 3: leaving nursery\n", "alien", id); // leaving nursery
   // release sleeping barriers on other two genders
   semup (raising); // raising.up()
   semup (raising); // raising.up()
   // finished reproducing
   semup (reproducing); // reproducing.up()
}


////
// main
//
int main (int argc, char **argv) {
   // set the execution name
   char *exec = argv[0];

   // print a usage area for insufficient arguments
   if (argc != 4) {
      fprintf (stderr, "%s: %s: incorrect usage\n", exec, "ERROR");
      fprintf (stderr, "%s: %s %s %s %s\n", "USAGE", exec, 
               "[alienID]", "[genderNumber]", "[numberReproductions]"); 
      return -1;
   }

   // increase random
   srandom (time (NULL));

   // hide misleading index numbers
   int one = 0;
   int two = 1;
   int three = 2;
   
   // set the specified gender
   int id = atoi (argv[1]);
   int gender = atoi (argv[2]);
   int repros = atoi (argv[3]);

   if (repros < 0) {
      fprintf (stderr, "%s: %s: number of reproductions must be non-negative\n", exec, "ERROR");
      return -1;
   }
   
   // get global/shared variables
   int line[3]; // semaphores for individual gender lines
   int mutex, depends, group; // semaphores
   int reproducing, raising; // semaphores
   int gathered; // count of genders gathered

   // file pointers for shared files
   FILE *fp;

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

   // gender is selected by the user
   // enters an infinite loop
   int i;
   for (i = 0; i < repros; ++i) {
      switch (gender) {
         case 1:
            gender_one (exec, id, line, mutex, depends, group, reproducing, raising);
            break;
         case 2:
            gender_two (exec, id, line, mutex, depends, group, reproducing, raising);
            break;
         case 3:
            gender_three (exec, id, line, mutex, depends, group, reproducing, raising);
            break;
         default:
            fprintf (stderr, "%s: %s: must specify gender of [1 2 3]\n", exec, "ERROR");
            exit (-1);
            break;
      }
   }

   // will technically never return
   return 1;
}
