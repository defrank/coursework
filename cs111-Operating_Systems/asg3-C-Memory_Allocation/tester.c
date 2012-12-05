/*******************************************************************************
 * $Id: libmem.c,v 1.1 2012-05-20 23:03:30-07 dmfrank - $
 * Justin Chen (juychen@ucsc.edu)
 * Derek Frank (dmfrank@ucsc.edu)
 * Benjamin Chow (bchow@ucsc.edu)
 *
 * NAME
 *   tester.c - implementation file
 *
 * DESCRIPTION
 *   A tester program to test the function of the libmem.c file and
 *   implementation.
 *
 ******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <time.h>

// flags
#define BUDDY_FLAG     0x01
#define SLAB_FLAG      0x02
#define FREELIST_FLAG  0x04
#define FIRSTFIT_FLAG  0x00
#define NEXTFIT_FLAG   0x08
#define BESTFIT_FLAG   0x10
#define WORSTFIT_FLAG  0x18

int main () {

   srand(time(NULL));

   int handle[3];
   int *myInts[30];
   int bytesRequested[30];
   int totalBytesRequested;
   
   long n_bytes = 1048576; // 1 MB
   
   int parm2[10];
   parm2[0] = 16;
   parm2[1] = 32;
   parm2[2] = 64;
   parm2[3] = 128;
   parm2[4] = 12432;
   parm2[5] = 1223;
   parm2[6] = 0;
   
   handle[0] = meminit (n_bytes, BUDDY_FLAG, 4, parm2);  
   handle[1] = meminit (n_bytes, SLAB_FLAG, 8, parm2);
   handle[2] = meminit (n_bytes, FREELIST_FLAG, 0, parm2);
   
   int handleCount;
   for (handleCount = 0; handleCount < 3; handleCount++) {
      totalBytesRequested = 0;
      if (handle[handleCount] < 0)
         printf ("%s: %s %d %s\n", "ERROR", "handle", handleCount, "did not initialize");
      int counter;
      for (counter = 0; counter < 30; counter++) {
         int randBytes = sizeof(char)*((rand()%99)+1); //1 - 100 bytes
         myInts[counter] = (int*)memalloc (handle[handleCount], randBytes);
         bytesRequested[counter] = randBytes;
         totalBytesRequested += randBytes;
      }
      // Free half
      for (counter = 0; counter < 30; counter+=2) {
         memfree(myInts[counter]);
         totalBytesRequested -= bytesRequested[counter];
         bytesRequested[counter] = 0;
      }
      // Reallocate
      for (counter = 0; counter < 30; counter+=2) {
         int randBytes = sizeof(char)*((rand()%99)+1); //1 - 100 bytes
         myInts[counter] = (int*)memalloc (handle[handleCount], randBytes);
         bytesRequested[counter] = randBytes;
         totalBytesRequested += randBytes;
      }
      int memoryUsed = memUsed(handle[handleCount]);
      printf("Allocator %d:\n", handleCount);
      printf("Total bytes: %d\n", memSize(handle[handleCount]));
      printf("Total bytes requested: %d\n", totalBytesRequested);
      printf("Total bytes used: %d\n", memoryUsed);
      printf("Total waste: %f%%\n", (1- (float)totalBytesRequested/memoryUsed)*100);
      if (handleCount == 0) handle[0] = handle[1];
      else if (handleCount == 1) handle[0] = handle[2];
   }
   
   printf("Tester complete.\n");
}
