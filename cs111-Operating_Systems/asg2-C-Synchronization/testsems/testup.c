/*******************************************************************************
 * $Id: testup.c,v 1.1 2012-05-12 18:48:09-07 dmfrank - $
 * Derek Frank
 * dmfrank@ucsc.edu
 *
 * NAME
 *   testup.c
 *
 * DESCRIPTION
 *   A test file to test the system calls of semaphore.
 ******************************************************************************/


#include <lib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char **argv) {
   char *exec = argv[0];
   if (argc < 3) {
      fprintf (stderr, "%s: %s: need a test id and semaphore id\n", "ERROR", exec);
      fprintf (stderr, "%s: ./testup [testID] [semaphoreID]\n", "USAGE");
      return -1;
   }
   int testid = atoi (argv[1]);
   int semid = atoi (argv[2]);

   
   // test semup
   printf ("%s %d: %s: %d\n", exec, testid, "beginning semup", semid);
   int result = semup (semid);
   if (result <= 0) {
      fprintf (stderr, "%s %d: %s: in %s\n", exec, testid, "ERROR", "semup");
      return result;
   }
   printf ("%s %d: %s: %d\n", exec, testid, "finished semup", semid);

   // finished all
   printf ("%s %d: %s\n", exec, testid, "finished all");
   
   return result;
}
