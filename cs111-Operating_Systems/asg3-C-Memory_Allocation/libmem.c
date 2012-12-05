/*******************************************************************************
 * $Id: libmem.c,v 1.1 2012-05-20 23:03:30-07 dmfrank - $
 * Justin Chen (juychen@ucsc.edu)
 * Derek Frank (dmfrank@ucsc.edu)
 * Benjamin Chow (bchow@ucsc.edu)
 *
 * NAME
 *   libmem.c - implementation file
 *
 * DESCRIPTION
 *   Implementation for buddy, slab, and free-list allocators.  Created as a
 *   static library.
 *
 ******************************************************************************/

#include <stdlib.h>
//#include <lib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#define public
#define private static

#define PAGE_SIZE      4096 // 4 KB
#define MAX_ALLOCATORS 512
#define MAX_PARM2      256
// flags
#define BUDDY_FLAG     0x01
#define SLAB_FLAG      0x02
#define FREELIST_FLAG  0x04
#define FIRSTFIT_FLAG  0x00
#define NEXTFIT_FLAG   0x08
#define BESTFIT_FLAG   0x10
#define WORSTFIT_FLAG  0x18
// used to make switch statements more readable
#define FIRSTFIT   1
#define NEXTFIT    2
#define BESTFIT    3
#define WORSTFIT   4


/************************** Structure Definition ******************************/

typedef struct allocator *allocator_ref;
struct allocator {
   int handle;           // handle of the current allocator
   long n_bytes;         // size of the available allocation space, not including the struct
   unsigned int flags;   // flags specifying the type of allocator (i.e., buddy, slab, or free-list)
   int parm1;            // buddy: minimum page size in address bits
                         // slab: number of pages from which a batch of objects is to be allocated
   int *parm2;           // slab: an array of object sizes that allocator should track

   //// BUDDY
   int totalBytesUsed;
   int buddyMaxLevel;
   unsigned int *buddyMap;
   /////

   //// FREELIST
   // some variables
   ////

   void *memory;         // points to the beginning of the available memory managed by allocator
};
allocator_ref allocators[MAX_ALLOCATORS]; // Array to hold allocators
int allocSize = 0;
void *nextfit = NULL;


/***************************** Helper Functions *******************************/

////
// eprintf
//
//   Prints an error message to stderr.
//
private void eprintf (const char *funcname, const char *msg) {
   fprintf (stderr, "%s: %s: %s\n", "ERROR", funcname, msg);
}


////
// memStats
//
public void memStats (int handle) {
   char *funcname = "memStats()"; // define function name for error usage
   allocator_ref myAlloc = allocators[handle];
   // check that the handle does not reference a null allocator
   if (myAlloc == NULL) {
      // return error
      eprintf (funcname, "could not locate handle");
   }
   
   printf("Allocator %d:\n", handle);
   printf("Total Memory: %d bytes\n", myAlloc->n_bytes);
   printf("Memory Used: %d bytes\n", myAlloc->totalBytesUsed);
   
}


////
// memUsed
//
public int memUsed (int handle) {
   char *funcname = "memUsed()"; // define function name for error usage
   allocator_ref myAlloc = allocators[handle];
   // check that the handle does not reference a null allocator
   if (myAlloc == NULL) {
      // return error
      eprintf (funcname, "could not locate handle");
   }
   return myAlloc->totalBytesUsed;
}


////
// memSize
//
public int memSize (int handle) {
   char *funcname = "memSize()"; // define function name for error usage
   allocator_ref myAlloc = allocators[handle];
   // check that the handle does not reference a null allocator
   if (myAlloc == NULL) {
      // return error
      eprintf (funcname, "could not locate handle");
   }
   return myAlloc->n_bytes;
}



/****************************** Init Functions ********************************/

////
// slab_init
//
//
//
private int buddy_init (long n_bytes, unsigned int flags, int parm1, int *parm2) {
   char *funcname = "buddy_init()"; // function name for error handling
   int handle; // return possible error or handle
   
   ///// Buddy allocator stuff
   double n_bytesLog = (log(n_bytes)/log(2));
   if ((round(n_bytesLog) - n_bytesLog) != 0) {
      printf ("Error: n_bytes is not a power of two.\n");
      return -1;
   }
   int minPageLength = (int)pow(2, parm1);  // Number of byte in smallest page
   int smallestPageNumber = n_bytes/minPageLength; // Number of smallest pages
   int totalPositions = smallestPageNumber * 2; // Number of positions
   int buddyMapBytes = ceil((float)totalPositions / 32) * 4; // Total bytes must be multiple of 4, since using ints
   int maximumLevel = log(smallestPageNumber)/log(2); // 2^x = # of pages of highest level
   
   // now allocate and initialize
   allocator_ref newAlloc = malloc(sizeof(struct allocator) + buddyMapBytes + n_bytes);
   //printf("newAlloc: %p\n", newAlloc);
   handle = allocSize;
   newAlloc->handle = handle;
   newAlloc->n_bytes = n_bytes;
   newAlloc->flags = flags;
   newAlloc->parm1 = parm1;
   newAlloc->parm2 = parm2;
   newAlloc->totalBytesUsed = sizeof(struct allocator);
   newAlloc->buddyMaxLevel = maximumLevel;
   newAlloc->buddyMap = (void*)((newAlloc) + sizeof(struct allocator));
   //printf("buddyMap: %p\n", newAlloc->buddyMap);
   memset (newAlloc->buddyMap, 0, buddyMapBytes);  // Set buddyMap to 0
   newAlloc->memory = newAlloc + sizeof(struct allocator) + (buddyMapBytes/8); // point to the beginning of managed memory
   //printf("memory: %p\n", newAlloc->memory);
   /////

   // set the new allocator reference into the global array
   allocators[allocSize] = newAlloc;

   return handle;
}


////
// slab_init
//
//
//
private int slab_init (long n_bytes, unsigned int flags, int parm1, int *parm2) {
   char *funcname = "slab_init()"; // function name for error handling
   // userful variables
   int handle = -1; // return possible error or handle
   int slabsize = parm1 * PAGE_SIZE;
   int structsize = sizeof (struct allocator);
   int numslabs = (n_bytes - structsize) / slabsize;
   int reservedspace = sizeof (void *) + sizeof (int);
   // Error checking
   if (parm1 <= 0) {
      eprintf (funcname, "use of non-positive parm1");
      return handle; // error
   }else if (n_bytes < slabsize + structsize) {
      eprintf (funcname, "requested memory is too small to manage");
      return handle; // error
   }else if (parm2 == NULL) {
      eprintf (funcname, "parm2 is a null pointer");
      return handle; // error
   }else if (parm2[0] == 0) {
      eprintf (funcname, "parm2 contains no object sizes");
      return handle; // error
   }
   int i; // index
   for (i = 0; i < MAX_PARM2 && parm2[i] != 0; ++i) {
      if (parm2[i] < 0) {
         eprintf (funcname, "use of negative object size in parm2");
         return handle; // error
      }else if (parm2[i] > slabsize - reservedspace) {
         eprintf (funcname, "parm2 contains an object size exceeding the slab size");
         return handle; // error
      }else if (parm2[i] < sizeof (void *)) {
         eprintf (funcname, "parm2 contains an object size of insufficient size");
         return handle; // error
      }
   }
   
   // allocate requested space for memory management
   allocator_ref newAlloc = malloc(n_bytes);
   // initialize newAlloc and set handle
   handle = allocSize;
   newAlloc->handle = handle;
   newAlloc->n_bytes = n_bytes;
   newAlloc->flags = flags;
   newAlloc->parm1 = parm1;
   newAlloc->parm2 = parm2;
   newAlloc->memory = &(newAlloc->memory) + sizeof (void *); // point to the beginning of managed memory

   // useful pointers to alter slab status
   void *slabptr = newAlloc->memory;  // pointer to a slab
   int *objsize = slabptr;            // pointer to the first 4 bytes of a slab
   void **freeptr = slabptr + sizeof (int); // pointer to the next 4 bytes after objsize in a slab
   // initialize all slabs to free
   for (i = 0; i < numslabs; ++i) {
      *objsize = 0;
      *freeptr = NULL;
      // increment slab
      slabptr += slabsize;
      objsize = slabptr;
      freeptr = slabptr + sizeof (int);
   }
   // set the allocator
   allocators[handle] = newAlloc;
   // return the handle
   return handle;
}


////
// freelist_init
//
//
//
private int freelist_init (long n_bytes, unsigned int flags, int parm1, int *parm2) {
   char *funcname = "freelist_init()"; // function name for error handling

   printf("firstfit_init called for: %d n_bytes\n", n_bytes);

   int handle = allocSize;     // Get current value of handle

   // Check to make sure it is a valid number
   if (n_bytes <= 0){
      printf("Need to allocate more than 0 bytes\n");
      return -1;
   }

   // initialize newAlloc
   allocator_ref newAlloc = malloc(n_bytes);
   newAlloc->handle = handle;
   newAlloc->n_bytes = n_bytes;
   newAlloc->flags = flags;
   newAlloc->parm1 = parm1;
   newAlloc->parm2 = parm2;
   newAlloc->memory = (void*)&(newAlloc->memory) + sizeof (void *); // pointer to head of freelist


   // Set the first segment of freespace 
   long initoverhead = newAlloc->memory - (void*)newAlloc;
   long space_left = n_bytes - initoverhead;  

   // Set the remaining space
   void *sizeptr;
   sizeptr = newAlloc->memory;
   *(long*)sizeptr = space_left-sizeof(long);

   // Set the head of the list to the free space
   newAlloc->memory = sizeptr+4;

   // Indicates that this is the end of the list
   *(int *)newAlloc->memory = NULL;

   // set the allocator
   allocators[handle] = newAlloc;
   // return the handle
   return handle;
}


//////////////////
/// INITIALIZE ///
//////////////////

////
// meminit
//
//   The goal is to initialize the memory allocator.  The n_bytes parameter is
//   the number of bytes that this allocator should manage.  flags describe the
//   behavior of the allocator.  parm1 and parm2 are parameters that can be used
//   to customize a memory allocator.  Returns a handle that can be used to
//   identify the allocator; a single process might choose to use more than one
//   allocator.  This handle must be at least 0; a negative number is returned
//   to indicate an error.
//
public int meminit(long n_bytes, unsigned int flags, int parm1, int *parm2) {
   char *funcname = "meminit()"; // define function name for error usage
   int handle = -1; // return handle or error
   // check if overhead fits within requested user memory
   if (n_bytes <= sizeof (struct allocator)) {
      eprintf (funcname, "requested memory is too small to manage");
      return -1; // Error, requested memory is too small
   }

   // make all allocator references null if this is the first call to meminit()
   if (allocSize == 0) {
      int i;
      for (i = 0; i < MAX_ALLOCATORS; ++i) allocators[i] = NULL;
   }
   
   // check if there is allocation space
   if (allocSize >= MAX_ALLOCATORS) {
      // return error
      eprintf (funcname, "no more allocation space");
      return -1; // Error, couldn't find handle
   }

   // Determine flag specifics and free
   // now allocate and initialize
   if (flags & BUDDY_FLAG) {
      // buddy
      handle = buddy_init (n_bytes, flags, parm1, parm2);
   } else if (flags & SLAB_FLAG) {
      // slab
      handle = slab_init (n_bytes, flags, parm1, parm2);
   } else if (flags & FREELIST_FLAG) {
      // freelist
      handle = freelist_init (n_bytes, flags, parm1, parm2);
   } else {
      // print and return error
      eprintf (funcname, "must specify allocator flags");
      return handle;
   }

   if (handle < 0) {
      // if there was an error
      eprintf (funcname, "could not initialize");
      return handle;
   }
   // increment number of allocators initialized
   allocSize++;
   
   return (allocSize - 1);
}


/***************************** Alloc Functions ********************************/

////
// buddy_alloc
//
//   A buddy allocator takes parm1 as the minimum page size in address bits.  In
//   other words, if the minimum address bits is specified as 12, the minimum
//   page size is actually 2^12 bytes, or 4 kilobytes.  If the region to divide
//   up isn't a power of 2, return an error of -1.  flags should be set to 0x01
//   to create this allocator.
//
//   Should start with a memory region whose size is passed (return -1 if the
//   region size isn't a power of 2), and successively split it.  The typical
//   approach is to use one bitmap per page size, with a single bit for each
//   page of that size.  In other words, if there are 128 pages of 4 kilobytes,
//   the 4 kilobyte page bitmap would have 128 entries.  For the bitmap for the
//   smallest-size page, a 1 means the page is in use, and a 0 means the page is
//   free or unallocated because the parent page hasn't been split.  The same is
//   true for all levels above the smallest page size.  For example, a 1 entry
//   at the 8 kilobyte level means that at least one of its (two) children must
//   be allocated, and hence set to 1.
//
//   To allocate a block of a particular size 2^k, check the bitmap for that
//   size block.  If there are any blocks marked free whose buddy is marked in
//   use, allocate that block.  If no such block is found, there are no free
//   blocks of size 2^k, so check blocks of size 2^k+1 in the same way.  If one
//   is found, mark it in use (set the bitmap entry to 1) and split it, marking
//   one child allocated (return this one) and the other one free.  This
//   continues recursively up the bitmaps until the root is reached.
//

// Get bit value from specified position
private int getBMapBit(allocator_ref myAlloc, int level, int position) {
   assert (myAlloc != NULL);
   unsigned int *buddyMap = myAlloc->buddyMap;
   int offset = pow(2, level-1) + position;  // Which bit inside buddyMap
   int targetInt = floor(offset / 32);  // Point to int containing bit
   int targetOffset = offset - (targetInt * 32); // Point to position of bit inside int
   
   if (buddyMap[targetInt] & (1 << targetOffset)) { // Not 0
      return 1;
   } else {
      return 0;
   }
}

// Get bit value from buddy of specified position
private int getBMapBuddyBit(allocator_ref myAlloc, int level, int position) {
   int buddyPosition;
   if (level == 0) return 1;
   if (position % 2 == 0) {   // Even number
      buddyPosition = position + 1;
   } else { // Odd number
      buddyPosition = position - 1;
   }
   return getBMapBit(myAlloc, level, buddyPosition);
}

// Get bit value from buddy of specified position
private void setBMapBit(allocator_ref myAlloc, int level, int position, int value) {
   assert (myAlloc != NULL);
   if (value != 0 && value != 1) {
      fprintf(stderr, "Error: setBMapBit value must be 0 or 1.\n");
      return;
   }
   unsigned int *buddyMap = myAlloc->buddyMap;
   
   int offset = pow(2, level-1) + position;  // Which bit inside buddyMap
   
   int targetInt = floor(offset / 32);  // Point to int containing bit
   int targetOffset = offset - (targetInt * 32); // Point to position of bit inside int
   
   if (value == 0) {
      buddyMap[targetInt] &= ~(1 << targetOffset);
   } else {
      buddyMap[targetInt] |= 1 << targetOffset;
   }
}

// Get memory address of specified position
private void *getMemoryAddress (allocator_ref myAlloc, int level, int position) {
   assert (myAlloc != NULL);
   void *memory = myAlloc->memory;
   long n_bytes = myAlloc->n_bytes;
   long offset = ((float)n_bytes/pow(2, level)) * position;  // Which fraction inside buddyMap
   return memory+offset;
}

// Divide block into two
private void divide (allocator_ref myAlloc, int level, int position) {
   //printf("Divided: Level %d\n", level);
   setBMapBit(myAlloc, level, position, 1); // Mark as used
   setBMapBit(myAlloc, level+1, position*2, 0);   // Mark resulting buddies as unused
   setBMapBit(myAlloc, level+1, position*2+1, 0);
}

// Allocate a new memory block
private void *buddy_alloc (allocator_ref myAlloc, long n_bytes) {
   //printf("buddy_alloc called for %d bytes.\n", n_bytes);
   assert (myAlloc != NULL);
   if (n_bytes <= 0) {
      fprintf(stderr, "Error: n_bytes must be > 0.\n"); 
      return NULL; // Error: n_bytes must be > 0
   }
   if (n_bytes > myAlloc->n_bytes) {
      fprintf(stderr, "Error: n_bytes can't be greater than memory size.\n"); 
      return NULL; // Error: n_bytes can't be greater than memory size
   }
   // 
   void *memptr = NULL;
   
   ///// Calculate closest fitting page index
   double numFit = myAlloc->n_bytes / n_bytes;  // how many objects would fit in memory
   int targetLevel = floor(log(numFit)/log(2));  // how many powers of two is that
   /////
   //printf("TargetLevel: %d\n", targetLevel);
   ///// Check for first existing page, starting from targetLevel
   int level = targetLevel;
   if (level > myAlloc->buddyMaxLevel) {
      level = myAlloc->buddyMaxLevel;
   }
   int piecePosition = -1;
   int levelFound = 0;
   while (levelFound == 0) {
      // Check each bit in index for open value
      int counter;
      for (counter = 0; counter < pow(2,level); counter++) {
         if (getBMapBit(myAlloc, level, counter) == 0
            && getBMapBuddyBit(myAlloc, level, counter) == 1) { // Check for free block with used buddy
            levelFound = 1;
            piecePosition = counter;
            break;
         }
      }
      if (levelFound == 0) {
         level--;
         if (level < 0) {
            fprintf(stderr, "Error: No empty slots available.\n"); 
            return NULL; // Error: No empty slots available
         }
      }
   }
   /////
   
   ///// Divide until target level is reached, and return the piece
   while (level != targetLevel) {
      if (level >= myAlloc->buddyMaxLevel) {
         break;
      }
      divide (myAlloc, level, piecePosition);
      level++;
      piecePosition *= 2;  // Double, since piecePosition differs based on level
   }
   /////
   
   // Mark piece we are returning as used
   setBMapBit(myAlloc, level, piecePosition, 1);
   
   double bytesUsed = myAlloc->n_bytes / pow(2,level);
   //printf("Used %f bytes to allocate %d bytes of memory for L%d P%d.\n", bytesUsed, n_bytes, level, piecePosition);
   
   myAlloc->totalBytesUsed += bytesUsed;
   
   memptr = getMemoryAddress (myAlloc, level, piecePosition);
   
   //printf("Returning memptr: %p\n", memptr);
   
   return memptr;
}


////
// slab_alloc
//
//   A slab allocator takes parm1 to indicate the number of pages from which a
//   batch of objects is to be allocated, and parm2 points to an array of object
//   sizes that the allocator should track (the array is terminated by a zero -
//   in other words, keep scanning until parm2[i] == 0).  flags should be set to
//   0x02 to create this allocator.
//
//   When allocating an object, allocate the smallest object that will wold the
//   memory region whose size is passed.  Note that, when freeing an object, no
//   size is provided.  To address this issue,  store the size in the 4 bytes
//   before the start of the region passed back to the user.
//
private void *slab_alloc (allocator_ref myAlloc, long n_bytes) {
   assert (myAlloc != NULL);
   char *funcname = "slab_alloc()"; // function name used for error output
   // variables
   void *objptr = NULL; // return null in error or region in success
   int structsize = sizeof (struct allocator);
   int reservedspace = sizeof (void *) + sizeof (int);
   int slabsize = myAlloc->parm1 * PAGE_SIZE;
   int numslabs = (myAlloc->n_bytes - structsize) / slabsize;
   int curobjsize = 0; // smallest object size that will fit n_bytes, none found yet
   int *parm2 = myAlloc->parm2;
   // Error checking
   if (n_bytes == 0) {
      eprintf (funcname, "cannot allocate nothing zero bytes");
      return objptr;
   }else if (n_bytes < 0) {
      eprintf (funcname, "cannot allocate negative bytes");
      return objptr;
   }
   int i; // index
   // find curobjsize
   for (i = 0; i < MAX_PARM2 && parm2[i] != 0; ++i) {
      if (curobjsize == 0 && n_bytes <= parm2[i]) curobjsize = parm2[i];
      else if (n_bytes <= parm2[i] && parm2[i] < curobjsize) curobjsize = parm2[i];
   }
   // no fitting object size was found for 
   if (curobjsize == 0) {
      eprintf (funcname, "no fitting object size for allocation");
      return objptr; // error
   }

   // useful pointer variables for slabs
   void *slabptr = myAlloc->memory;
   int *objsize = slabptr;
   void **freeptr = slabptr + sizeof (int);
   void **nextfreeptr = NULL;
   // use an object from a previously created slab
   for (i = 0; i < numslabs && objptr == NULL; ++i) {
      if (*objsize == curobjsize && *freeptr != NULL) {
         nextfreeptr = *freeptr;
         objptr = *freeptr;
         *freeptr = *nextfreeptr;
      }
      // increment
      slabptr += slabsize;
      objsize = slabptr;
      freeptr = slabptr + sizeof (int);
   }
   // if there was no room in another slab, then allocate a new one
   if (objptr == NULL) {
      slabptr = myAlloc->memory;
      objsize = slabptr;
      freeptr = slabptr + sizeof (int);
      int numobjects = (slabsize - reservedspace) / curobjsize;
      // find a free slab
      for (i = 0; i < numslabs && objptr == NULL; ++i) {
         if (*objsize == 0) {
            *objsize = curobjsize;
            *freeptr = slabptr + reservedspace;
            // set up free-list by stringing linking all free objects
            int j; // another index
            nextfreeptr = *freeptr;
            for (j = 0; j < numobjects; ++j) {
               if (j == numobjects - 1) *nextfreeptr = NULL;
               else {
                  *nextfreeptr = nextfreeptr + (*objsize);
                  nextfreeptr = *nextfreeptr;
               }
            }
            nextfreeptr = *freeptr;
            objptr = *freeptr;
            *freeptr = *nextfreeptr;
         }
         // increment
         slabptr += slabsize;
         objsize = slabptr;
         freeptr = slabptr + sizeof (int);
      }
      // there is no place to put the object
      if (objptr == NULL) {
         eprintf (funcname, "no place to allocate object");
         return objptr; // error
      }
   }

   //printf ("objptr: %p, memory: %p, n_bytes: %d, mem+bytes-struct: %p\n",
   //       objptr, myAlloc->memory, n_bytes, myAlloc->memory + myAlloc->n_bytes - sizeof(struct allocator));
   return objptr;
}



////
// firstfit_alloc
//
//   The first suitable hole on the list.
//
private void *firstfit_alloc (allocator_ref myAlloc, long n_bytes) {
   assert (myAlloc != NULL);
   void *memptr = NULL;
   void *current;
   void *next = NULL;
   void *prev = NULL; 
   void *nextfree = NULL;
   int current_head = 0;

   current = myAlloc->memory;

   while(current != NULL){
      long curr_size = *(long*)(current-4);
      next = *(int*)current;
      
      // A segment large enough was found
      if (n_bytes < curr_size){

         if (current == myAlloc->memory)
            current_head = 1;

         nextfree = *(int*)current;

         *(long*)(current-4) = n_bytes;
         memptr = current;
         current = current + n_bytes;
         *(long*)(current) = curr_size - n_bytes - 4;
         current += 4;

         // check if current is the head of list
         if (current_head == 1){
            myAlloc->memory = current;
            *(int*)current = nextfree;
            current_head = 0;
         }
         else{
            *(int*)prev = current;  
            *(int*)current = nextfree;      
         }

      }
      prev = current;
      current = next;

   } // end while

   return memptr;
}


////
// nextfit_alloc
//
//   The first suitable hole afte the previously allocated hole.
//
private void *nextfit_alloc (allocator_ref myAlloc, long n_bytes) {
   assert (myAlloc != NULL);
   void *memptr = NULL;
   void *current;
   void *next = NULL;
   void *prev = NULL; 
   void *nextfree = NULL;
   int current_head = 0;

   // If nextfit is NULL start at the beginning
   if (nextfit == NULL)
      nextfit = myAlloc->memory;

   // set the current as the nextfit (last time it worked)
   current = nextfit;

   while(current != NULL){
      long curr_size = *(long*)(current-4);
      next = *(int*)current;

      // A segment large enough was found
      if (n_bytes < curr_size){

         if (current == myAlloc->memory)
            current_head = 1;

         nextfree = *(int*)current;

         *(long*)(current-4) = n_bytes;
         memptr = current;
         current = current + n_bytes;
         *(long*)(current) = curr_size - n_bytes - 4;
         current += 4;

         // check if current is the head of list
         if (current_head == 1){
            myAlloc->memory = current;
            *(int*)current = nextfree;
            nextfit = current;
            current_head = 0;
         }
         else{
            *(int*)prev = current;  
            *(int*)current = nextfree;    
            nextfit = current;  
         }

      }
      prev = current;
      current = next;

   } // end while

   if (current == NULL)
      nextfit = NULL;

   return memptr;
}


////
// bestfit_alloc
//
//   The smallest hole that is larger than the desired region.
//
private void *bestfit_alloc (allocator_ref myAlloc, long n_bytes) {
   assert (myAlloc != NULL);
   void *memptr = NULL;
   void *current;
   void *next = NULL;
   void *prev = NULL;
   void *nextfree = NULL;
   int current_head = 0;
   void *bestfit = NULL;
   void *bestfitprev = NULL;
   long smallest_diff = 9999999999999999;
   long size_diff = 0;

   current = myAlloc->memory;

   // try to find the best fitting segment
   while(current != NULL){
      long curr_size = *(long*)(current-4);
      size_diff = curr_size - (n_bytes+4);

      next = *(int*)current;

      if (size_diff >= 0 && size_diff < smallest_diff){
         smallest_diff = size_diff;
         bestfit = current;
         bestfitprev = prev;
      }
      prev = current;
      current = next;

   } // end while


   // allocate the space
   if (bestfit == NULL)
      return NULL;
   else{
      if (bestfit == myAlloc->memory)
            current_head = 1;
      nextfree = *(int*)bestfit;
      long best_size = *(long*)(bestfit-4);
      *(long*)(bestfit-4) = n_bytes;
      memptr = bestfit;
      bestfit += n_bytes;
      *(long*)(bestfit) = best_size - n_bytes - 4;
      bestfit += 4;
   

      // check if current is the head of list
      if (current_head == 1){
         myAlloc->memory = bestfit;
         *(int*)bestfit = nextfree;
         current_head = 0;
      }
      else{
         *(int*)bestfitprev = bestfit;  
         *(int*)bestfit = nextfree;      
      }

   } // end else

   return memptr;
}


////
// worstfit_alloc
//
//   The larges available hole, which will leave largest fragment.
//
private void *worstfit_alloc (allocator_ref myAlloc, long n_bytes) {
   assert (myAlloc != NULL);
   void *memptr = NULL;
   void *current;
   void *next = NULL;
   void *prev = NULL;
   void *nextfree = NULL;
   int current_head = 0;
   long largest = 0;
   void *worstfit = NULL;
   void *worstfitprev = NULL;

   current = myAlloc->memory;

   // Try to find the biggest segment
   while(current != NULL){
      long curr_size = *(long*)(current-4);

      next = *(int*)current;

      if (largest < curr_size){
         largest = curr_size;
         worstfit = current;
         worstfitprev = prev;
      }
      prev = current;
      current = next;

   } // end while


   if (worstfit == NULL)
      return NULL;
   else{
      if (worstfit == myAlloc->memory)
            current_head = 1;
      nextfree = *(int*)worstfit;
      long largest_size = *(long*)(worstfit-4);
      *(long*)(worstfit-4) = n_bytes;
      memptr = worstfit;
      worstfit += n_bytes;
      *(long*)(worstfit) = largest_size - n_bytes - 4;
      worstfit += 4;
   

      // check if current is the head of list
      if (current_head == 1){
         myAlloc->memory = worstfit;
         *(int*)worstfit = nextfree;
         current_head = 0;
      }
      else{
         *(int*)worstfitprev = worstfit;  
         *(int*)worstfit = nextfree;      
      }

   } // end else

   return memptr;
}


////
// freelist_alloc
//
//   A free-list based allocator allocates objects of whatever size is passed.
//   Need to store the size before the start of an allocated region.  This is
//   typically done by allocating a region of size n+4, setting the first four
//   bytes of the region to the size, and return a pointer to the region after
//   where the size is stored.
//
//   Implemented using first-fit, next-fit, best-fit, and worst-fit algorithms.
//   flags should be set to 0x04 to use this allocator.  flags should be set to
//   0x04 for first-fit, 0x0C for next-fit, 0x14 for best-fit, and 0x1C for
//   worst-fit.
//
private void *freelist_alloc (allocator_ref myAlloc, long n_bytes, int fittype) {
   assert (myAlloc != NULL);

   // do general free list stuff
   //
   
   // do specific fit-type stuff
   void *memptr = NULL;
   switch (fittype) {
      case FIRSTFIT:
         // first fit
         memptr = firstfit_alloc (myAlloc, n_bytes);   break;
      case NEXTFIT:
         // next fit
         memptr = nextfit_alloc (myAlloc, n_bytes);    break;
      case BESTFIT:
         // best fit
         memptr = bestfit_alloc (myAlloc, n_bytes);    break;
      case WORSTFIT:
         // worst fit
         memptr = worstfit_alloc (myAlloc, n_bytes);   break;
      default:
         // no fit
         memptr = NULL;                                break;
   }
   // do more general free list stuff
   //
   
   return memptr;
}


////////////////
/// ALLOCATE ///
////////////////

////
// memalloc
//
//   Takes a handle and returns a pointer to a region of n_bytes of data.  If no
//   memory is available, or the handle is invalid, it returns NULL (void *0).
//
public void *memalloc (int handle, long n_bytes) {
   char *funcname = "memalloc()"; // define function name for error usage
   // check the handle is positive and in bounds
   if (handle < 0 || handle >= MAX_ALLOCATORS) {
      eprintf (funcname, "handle is not in bounds");
      return NULL;
   }
   allocator_ref myAlloc = allocators[handle];
   // check that the handle does not reference a null allocator
   if (myAlloc == NULL) {
      // return error
      eprintf (funcname, "could not locate handle");
      return NULL; // Error, couldn't find handle
   }
   // check that the number of bytes is non-negative
   long zero = 0;
   if (n_bytes < zero) {
      // return error
      eprintf (funcname, "allocation size must be at least zero bytes");
      return NULL; // Error, negative allocation size specified
   }
   
   // Determine flag specifics and allocate
   if (myAlloc->flags & BUDDY_FLAG) {
      // buddy
      return buddy_alloc (myAlloc, n_bytes);
   } else if (myAlloc->flags & SLAB_FLAG) {
      // slab
      return slab_alloc (myAlloc, n_bytes);
   } else if (myAlloc->flags & FREELIST_FLAG) {
      // freelist
      if (myAlloc->flags & (FREELIST_FLAG | FIRSTFIT_FLAG)) {
         // first fit
         return freelist_alloc (myAlloc, n_bytes, FIRSTFIT);
      } else if (myAlloc->flags & (FREELIST_FLAG | NEXTFIT_FLAG)) {
         // next fit
         return freelist_alloc (myAlloc, n_bytes, NEXTFIT);
      } else if (myAlloc->flags & (FREELIST_FLAG | BESTFIT_FLAG)) {
         // best fit
         return freelist_alloc (myAlloc, n_bytes, BESTFIT);
      } else if (myAlloc->flags & (FREELIST_FLAG | WORSTFIT_FLAG)) {
         // worst fit
         return freelist_alloc (myAlloc, n_bytes, WORSTFIT);
      }
   }
   
   // print an error and return a null pointer if no flags matched
   eprintf (funcname, "no allocation specified");
   return NULL;
}



/****************************** Free Functions ********************************/

////
// buddy_free
//
//   When a block si freed, check its buddy to see if it's free as well.  If so,
//   merge the two blocks and mark the parent free.  This goes on recursively
//   until you reach the top level bitmap.  Note that space must be allocated on
//   2^x byte boundaries.  this fact can be used to free a block when its size
//   is unknown.  simply start at the lowest level (smallest page size) of
//   bitmap, and see if the corresponding entry is in use.  if it is, mark it
//   free and merge upwards.  If not, try the next higher bitmap, and so on.
//
private void buddy_free (allocator_ref myAlloc, void *region) {
   //printf("Called buddy_free\n");
   assert (myAlloc != NULL && region != NULL);
   long memoryOffset = region - myAlloc->memory;
   
   int level = myAlloc->buddyMaxLevel;
   int position = 0;
   
   int bytesFreed = 0;
   
   while (level >= 0) {
      float position2 = memoryOffset / ((float)(myAlloc->n_bytes)/pow(2,level));
      if (floor(position2) == position2) {
         position = (int)position2;
         if (getBMapBit(myAlloc, level, position) == 1) {
            setBMapBit(myAlloc, level, position, 0);
            bytesFreed = myAlloc->n_bytes / pow(2,level);
            break;
         }
      }
      level --;
      if (level == -1) {
         fprintf(stderr, "No variable to free.\n");
         return;
      }
   }
   
   //printf("Freed %d bytes at %p.\n", bytesFreed, region);
   myAlloc->totalBytesUsed -= bytesFreed;
   
   int merging = 1;
   while (merging == 1) {
      if (getBMapBuddyBit(myAlloc, level, position) == 0) {
         //printf("Merged level %d position %d.\n", level, position);
         level--;
         position /= 2;
         setBMapBit(myAlloc, level, position, 0);
      } else merging = 0;
      if (level < 0) merging = 0;
   }
   
   
   
}


////
// slab_free
//
//   Free a set of pages when all of the objects in them are freed.  Also,
//   recycle freed objects.
//
//   The size of the regions is stored in the 4 bytes before the start of the
//   region.
//
private void slab_free (allocator_ref myAlloc, void *region) {
   //printf("Called slab_free\n");
   assert (myAlloc != NULL && region != NULL);
   char *funcname = "slab_free()"; // function name used for error handling
   // variables
   int slabsize = myAlloc->parm1 * PAGE_SIZE;
   int reservedspace = sizeof (void *) + sizeof (int);
   // locate slab and object information with respect to the region
   int slabnumber = (region - myAlloc->memory) / slabsize;
   void *slabptr = myAlloc->memory + (slabsize * slabnumber);
   int *objsize = (int *) slabptr;
   // check the slab is being used
   if (*objsize <= 0) {
      eprintf (funcname, "cannot free in a slab marked free");
      return; // error
   }
   void **freeptr = (void **) slabptr + sizeof (int);
   void **nextfreeptr;
   int objectnumber =  (region - slabptr - reservedspace) / (*objsize);
   void *objptr = slabptr + reservedspace + (*objsize * objectnumber);
   int numobjects = (slabsize - reservedspace) / (*objsize);

   // Error checking
   // region must point to the beginning of an object
   if (objptr != region) {
      eprintf (funcname, "must reference the beginning of the object");
   }else {
      // free object and add it to the free-list
      nextfreeptr = objptr;
      *nextfreeptr = *freeptr;
      *freeptr = objptr;
      // check to free the slab
      int count = 0;
      nextfreeptr = freeptr;
      int i; // index
      for (i = 0; i < numobjects && (*nextfreeptr) != NULL; ++i) {
         ++count;
         nextfreeptr = *nextfreeptr;
      }
      // free the slab
      if (count == numobjects) {
         *objsize = 0; // the slab does not contain objects any more
         *freeptr = slabptr + reservedspace;
      }
   }
}


////
// freelist_free
//
//   Free the specified region.  The size of the region is stored in the 4 bytes
//   directly preceding the region.
//
private void freelist_free (allocator_ref myAlloc, void *region) {
   //printf("Called freelist_free\n");
   assert (myAlloc != NULL && region != NULL);

   void *current;
   void *next;
   void *prev;
   next = NULL;
   prev = NULL;


   long user_size = *(long*)(region-4);
   printf("the user wants to free: %d\n", user_size);
   printf("starting at: %p\n", region);

   
   // Find the proper place to insert into the list
   current = myAlloc->memory;

   while(current != NULL){
      long curr_size = *(long*)(current-4);
      next = *(int*)current;

      // Find proper place to insert into list
      if (region < current){
         current = next;
         continue;
      }

      // Check if left region can merge with this free
      if (current + curr_size == region){
         *(long*)(current-4) += (user_size + 4);
         // Check if right region can merge with this free
         if ((current + *(long*)(current-4) + 4) == next){
            *(int*)current = *(int*)next;
            *(long*)(current-4) += *(long*)(next-4);
         }
      }
      else{
         *(int*)region = *(int*)current;
         *(int*)(current) = region;
         // Check if the right region can merge with this free
         if ((region + user_size + 4) == next){
            *(int*)region = *(int*)next;
            *(long*)(region-4) += *(long*)next;
         }
      }

   } // end while
}


////////////
/// FREE ///
////////////

////
// memfree
//
//   Frees a previously-allocated region of memory, returning it to the pool of
//   available memory for the appropriate handle.  Note that the handle itself
//   is not passed; this function must figure out how to associate the region
//   with the appropriate handle.  There is no return value; if the region isn't
//   valid, the call has no effect.
//
public void memfree (void *region) {
   char *funcname = "memfree()"; // define function name for error usage
   // cannot free a null pointer
   if (region == NULL) {
      eprintf (funcname, "cannot free a null reference");
      return;
   }
   
   int handle = -1;
   // iterate through each allocators until the region is found
   int i;
   for (i = 0; i < MAX_ALLOCATORS && handle == -1; ++i) {
      // search each allocator for the one that contains region
      if (allocators[i] != NULL && (allocators[i]->flags & (SLAB_FLAG | FREELIST_FLAG))
          && (region >= allocators[i]->memory)
          && region < (allocators[i]->memory + allocators[i]->n_bytes - sizeof (struct allocator))) {
         handle = i;
      }
   }
   // buddy gets its own loop because of issues with allocation
   for (i = 0; i < MAX_ALLOCATORS && handle == -1; ++i) {
      // search each allocator for the one that contains region
      if (allocators[i] != NULL && (allocators[i]->flags & BUDDY_FLAG)
          && (region >= allocators[i]->memory)) {
         int minPageLength = (int)pow(2, allocators[i]->parm1);  // Number of byte in smallest page
         int smallestPageNumber = (allocators[i]->n_bytes)/minPageLength; // Number of smallest pages
         int totalPositions = smallestPageNumber * 2; // Number of positions
         int buddyMapBytes = ceil((float)totalPositions / 32) * 4; // Total bytes must be multiple of 4, since using ints
         int structsize = sizeof (struct allocator);
         if (region < (allocators[i]->memory - structsize - buddyMapBytes + allocators[i]->n_bytes)) {
//            if (region < (allocators[i] + (buddyMapBytes/8) + allocators[i]->n_bytes - sizeof (struct allocator))) {
            handle = i;
         }
      }
   }
   // region not found
   if (handle == -1) {
      eprintf (funcname, "region is not allocated");
      return;
   }
   
   // deal with that particular region
   allocator_ref myAlloc = allocators[handle];
   
   // Determine flag specifics and free
   if (myAlloc->flags & BUDDY_FLAG) {
      // buddy
      buddy_free (myAlloc, region);
   } else if (myAlloc->flags & SLAB_FLAG) {
      // slab
      slab_free (myAlloc, region);
   } else if (myAlloc->flags & FREELIST_FLAG) {
      // freelist
      freelist_free (myAlloc, region);
   }
}
