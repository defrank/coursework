/*******************************************************************************
 * $Id: semaphore.c,v 1.1 2012-05-12 19:48:02-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   do_semaphore.c - implementation file
 *
 * DESCRIPTION
 *   An implementation file for implementing semaphore system calls.  These
 *   calls are handled by the process manager server.  Semaphores are used to
 *   both effectively allow mutual exclusion on resources and prevent race
 *   conditions.
 *
 *   Semaphores are identified by a positive (non-zero) integer that's provided
 *   when the semaphore is created; if the call to create a semaphore is passed
 *   the value 0, the kernel chooses an identifier for the semaphore.  When the
 *   user is done with a sempahore, the code must explicitly release the
 *   semaphore, allowing the process manager to reuse it.
 *
 *   The process manager only has to handle up to 100 semaphores at any given
 *   time, so you can simply create an array that holds the semaphores the
 *   kernel may allocate to use.  However
 *
 ******************************************************************************/

#include "pm.h"
#include "param.h"
#include "glo.h"
#include "mproc.h"
#include <sys/wait.h>
#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// constants
#define NR_SEMS      100          // number of semaphores available in kernel
#define SZ_PLIST     1000         // size of each semaphore's process list
#define NO_SEM      -1            // a semaphore id set to -1 is unallocated
#define INIT_SEMVAL  1000         // a bound for the maximum initial semaphore value
#define MAX_SEMVAL   1000000      // a bound for the maximum semaphore value
#define INT_LIMIT    0x7fffffff   // the maximum value of an int primitive value
#define ESEMVAL      0x8000000    // the return value for an error in do_semvalue
#define NO_PROC_NR   0            // a proc number cannot be 0
#define RETERR       0            // functions return 0 on error, except do_semvalue


// A private function within semaphore.c
FORWARD _PROTOTYPE( int sem_exists, (int sem)    			);
FORWARD _PROTOTYPE( void init_sems, (void)      			);


/***************************** Type Definitions *******************************/
/********************************** Globals ***********************************/

////
// semaphore - struct
//
//   A global array of semaphore structures.  Statically allocated.
//
struct semaphore {
   int id, value; // semaphore id and value
   int plist[SZ_PLIST]; // "queue" of sleeping processes
   int begin; // index to the next to wake process, highest priority
   int end; // index to the most recently put to sleep process, low priority
   int pcount; // number of sleeping processes
} semarray[NR_SEMS];
int semcount = 0; // number of allocated semaphores in semarray

PRIVATE void init_sems (void) {
   // Initialize all the semaphores to NO_SEM to know they are free to use.
   int i;
   for (i = 0; i < NR_SEMS; ++i) {
      // "mark" the semaphores as free
      semarray[i].id = NO_SEM;
      semarray[i].value = 0;
      // initialize the process list
      semarray[i].pcount = 0; // no processes in process list
      semarray[i].plist[0] = NO_PROC_NR; // an invalid proc number 
      semarray[i].begin = 0;
      semarray[i].end = 0;
   }
   semcount = 0;
}



/***************************** Access Functions *******************************/

////
// sem_exists
//
//   Returns 1 if the semaphore id exists in the global semaphore array.
//   Otherwise, returns 0.
//
PRIVATE int sem_exists (sem)
   int sem;
{
   // ERROR CHECKING //
   // errors: EINVAL
   //   check if id is negative
   if (sem <= 0 || sem > INT_LIMIT - 1) return 0;

   // semaphore id cannot exist if there are no allocated semaphores
   if (semcount < 1) return 0;
   // look for the semaphore id
   int i;
   for (i = 0; i < NR_SEMS; ++i) {
      if (semarray[i].id == sem) return 1; // found the id
   }
   // did not find the id
   return 0;
}


////
// do_semvalue
//
//   This call returns the current value of the semaphore whose identifier is
//   passed.  If there are n processes waiting on the semaphore, the call should
//   return -n.  If the semaphore has a value of 0, the next process to call
//   semdown() would wait, but no process is currently waiting.  If an error
//   occurs, return 0x8000000.
//
PUBLIC int do_semvalue (void) {
   int exitstatus = ESEMVAL; // not yet successful, assume error
   // get message
   int sem = m_in.m1_i1;

   // ERROR CHECKING //
   //   check if semaphore id is within bounds
   //   semaphore cannot exist if there are none allocated
   if (sem <= 0 || sem > INT_LIMIT - 1 || semcount < 1) return ESEMVAL;
   
   // find index around sem modulo NR_SEMS
   int index;
   index = (sem % NR_SEMS) - 1;
   if (index < 0) index = NR_SEMS - 1;
   int i;
   for (i = 0; i < NR_SEMS && exitstatus == ESEMVAL; ++i) {
      if (semarray[index].id == sem) exitstatus = semarray[index].value;
      else if (index == (NR_SEMS - 1)) index = 0;
      else ++index;
   }

   // MORE ERROR CHECKING //
   // semaphore id could not be found
   if (exitstatus == ESEMVAL) return ESEMVAL;

   return exitstatus;
}



/************************** Manipulation Functions ****************************/

////
// do_seminit
//
//   The call initializes a new semaphore.  The semaphore is identified by the
//   integer passed in sem; if sem is 0, the kernel chooses an identifier.  Note
//   that an identifier can be any positive integer between 1 and 2^31; it need
//   not be in the range 1-100.  The call returns the identifier for the
//   semaphore--either the value passed or the one the kernel chose--or 0 if an
//   error ocurred.  The initial value for the semaphore is passed in value.
//   On an error, the semaphore is not initialized.
//
//   Error conditions (with error codes) include:
//     EAGAIN
//       No free semaphores in the kernel (there is a limit of 100 active
//       semaphores in the kernel).
//
//     EINVAL
//       Semaphore identifier is negative.
//
//     EINVAL
//       Semaphore initial value is not in the range
//       -INIT_SEMVAL<=value<=INIT_SEMVAL.
//
//     EEXIST
//       Semaphore identifier is already in use for an active semaphore (one
//       that was freed with semfree() is not active).
//
PUBLIC int do_seminit (void) {
   // initialize semaphore array if not yet initialized
   if (semcount == 0) init_sems ();

   // return value: should be set to the sem id or 0 if an error occurred
   int exitstatus = RETERR; // not yet successful
   // get message
   int sem = m_in.m1_i1;
   int value = m_in.m1_i2;

   // ERROR CHECKING //
   // errors: EAGAIN
   //   check if free semaphores are available
   if (semcount >= NR_SEMS) {
      m_in.m_type = EAGAIN;
      return RETERR;
   }
   // errors: EINVAL
   //   check if id is negative
   //   check if id exceeds 2^31
   //   check if value is less than -INIT_SEMVAL
   //   check if value is greater than INIT_SEMVAL
   if (sem < 0 || sem > INT_LIMIT - 1 || value > INIT_SEMVAL || value < -INIT_SEMVAL) {
      m_in.m_type = EINVAL;
      return RETERR;
   }
   // errors: EEXIST
   //   check if id is in use for active semaphore
   if (sem != 0 && sem_exists (sem)) {
      m_in.m_type = EEXIST;
      return RETERR;
   }

   // allocate the semaphore
   // two cases:
   //   choose semaphore id, sem == 0
   //   given semaphore id, sem != 0
   int index;
   if (sem == 0) {
      // look for an empty/unused semaphore
      int i;
      // only NR_SEMS available semaphores
      for (i = 0; i < NR_SEMS && exitstatus == RETERR; ++i) {
         if (semarray[i].id == NO_SEM) {
            index = i;
            sem = i + 1;
            // the sem id already exists, alter sem id chosen
            while (sem_exists (sem)) sem += 100;
            ++semcount; // increment the number of allocated semaphores
            exitstatus = sem; // will return the id to the user, but cannot be 0 or 101
            // initialize the semaphore
            semarray[i].id = sem; // set id
            semarray[i].value = value; // set value
            // initialize the process list to empty
            semarray[i].pcount = 0; // no processes in the process list
            semarray[i].plist[0] = NO_PROC_NR; // an invalid pid
            semarray[i].begin = 0;
            semarray[i].end = 0;
         }
      }
   }else {
      // Only have NR_SEMS indices to index, so a large id number will exist
      // around id modulo NR_SEMS.
      index = ((sem % NR_SEMS) - 1);
      if (index < 0) index = NR_SEMS - 1;
      // find an unused semaphore
      int j;
      for (j = 0; j < NR_SEMS && exitstatus == RETERR; ++j) {
         if (semarray[index].id == NO_SEM) {
            ++semcount; // increment the number of allocated semaphores
            exitstatus = sem; // will return success by sem id
            // initialize the semaphore
            semarray[index].id = sem; // set id
            semarray[index].value = value; // set value
            // initialize the process list
            semarray[index].pcount = 0; // no processes in the process list
            semarray[index].plist[0] = NO_PROC_NR; // an invalid pid
            semarray[index].begin = 0;
            semarray[index].end = 0;
         }else if (index == (NR_SEMS - 1)) index = 0;
         else ++index; // increment the index
      }
   }
   
   return exitstatus;
}


////
// semup
//
//   This call does UP on the semaphore whose identifier is passed.  This call
//   never blocks.  If there's at least one process waiting on this semaphore,
//   semup() causes one waiting process to be awakened.  The call returns 1 if
//   successful, 0 otherwise.
//
//   NOTE: while a semaphore can't be initialized outside the range
//   -1000<=value<=1000, it may be incremented (or decremented) to a value
//   outside this range, up to +/-10^6.  If the semup() call would result in a
//   value above 10^6, return 0 and set the error to EOVERFLOW.
//
PUBLIC int do_semup (void) {
   // return 1 if successful, 0 otherwise
   int exitstatus = RETERR; // not yet successful
   // get message
   int sem = m_in.m1_i1;

   // ERROR CHECKING //
   //   check if semaphore id is within bounds
   //   semaphore cannot exist if there are none allocated
   if (sem <= 0 || sem > INT_LIMIT - 1 || semcount < 1) {
      m_in.m_type = EINVAL;
      return RETERR;
   }
   
   // find index around sem modulo NR_SEMS
   int index;
   index = (sem % NR_SEMS) - 1;
   if (index < 0) index = NR_SEMS - 1;
   int i;
   for (i = 0; i < NR_SEMS && exitstatus == RETERR; ++i) {
      if (semarray[index].id == sem) exitstatus = 1;
      else if (index == (NR_SEMS - 1)) index = 0;
      else ++index;
   }

   // MORE ERROR CHECKING //
   // semaphore id could not be found
   if (exitstatus == RETERR) {
      //m_in.m_type = EEXIST;
      return RETERR;
   }
   //   check if value will exceed bounds of 10^6
   if (semarray[index].value == MAX_SEMVAL) {
      m_in.m_type = EOVERFLOW;
      return RETERR;
   }

   // increment value
   ++(semarray[index].value);
   // wake up a sleeping process if there is one
   if (semarray[index].pcount != 0) {
      int next = semarray[index].begin;
      int proc_nr = semarray[index].plist[next];
      // check if a valid process id number
      if (proc_nr <= NO_PROC_NR || proc_nr >= NR_PROCS) return RETERR;
      
      // increment to the next sleeping process, unless there are no more
      if (next == semarray[index].end) {
         semarray[index].plist[next] = NO_PROC_NR; // an invalid pid
         semarray[index].begin = 0; // make process list start at beginning of list
         semarray[index].end = 0; // make process list start at beginning of list
      }else if (next == (SZ_PLIST - 1)) {
         semarray[index].plist[SZ_PLIST - 1] = NO_PROC_NR;
         semarray[index].begin = 0; // loop back around to index 0
      }else {
         semarray[index].plist[next] = NO_PROC_NR;
         ++next; // increment next index
         semarray[index].begin = next;
      }
      // decrement process list count
      --(semarray[index].pcount); // one less sleeping process

      // now wake the process with the proc_nr, check for error
      register struct mproc *rmp = &mproc[proc_nr];
      setreply (proc_nr, exitstatus);
   }
   
   return exitstatus;
}


////
// do_semdown
//
//   This call does DOWN on the semaphore whose identifier is passed.  If the
//   semaphore value would go below zero, the call blocks until the value goes
//   above zero again.  The call returns 1 if successful, 0 otherwise.
//
//   NOTE: while a semaphore can't be initialized outside the range
//   -1000<=value<=1000, it may be incremented (or decremented) to a value
//   outside this range, up to +/-10^6.  If the semdown() call would result in
//   a value below -10^6, return 0 and set the erro to EOVERFLOW.
//
PUBLIC int do_semdown (void) {
   // return 1 if successful, 0 otherwise
   int exitstatus = RETERR; // not yet successful
   // get message
   int sem = m_in.m1_i1;

   // ERROR CHECKING //
   //   check if semaphore id is within bounds
   //   semaphore cannot exist if there are none allocated
   if (sem <= 0 || sem > INT_LIMIT - 1 || semcount < 1) {
      m_in.m_type = EINVAL;
      return RETERR;
   }
   
   // find index around sem modulo NR_SEMS
   int index;
   index = (sem % NR_SEMS) - 1;
   if (index < 0) index = NR_SEMS - 1;
   int i;
   for (i = 0; i < NR_SEMS && exitstatus == RETERR; ++i) {
      if (semarray[index].id == sem) exitstatus = 1;
      else if (index == (NR_SEMS - 1)) index = 0;
      else ++index;
   }

   // MORE ERROR CHECKING //
   // semaphore id could not be found
   if (exitstatus == RETERR) {
      //m_in.m_type = EEXIST;
      return RETERR;
   }
   // check if value will exceed bounds of -10^6
   if (semarray[index].value == -MAX_SEMVAL) {
      m_in.m_type = EOVERFLOW;
      return RETERR;
   }
   // check if process will fit on the process list
   if (semarray[index].pcount == SZ_PLIST) {
      m_in.m_type = EOVERFLOW;
      return RETERR;
   }
   
   // decrement sem value
   --(semarray[index].value);
   // go to sleep if value is less than 0 after decrementing
    if (semarray[index].value < 0) {
      int last = semarray[index].end;
      int proc_nr = who_p;

      // add the process to the process list
      if (semarray[index].pcount == 0) {
         semarray[index].plist[last] = proc_nr;
      }else if (last == (SZ_PLIST - 1)) {
         last = 0;
         semarray[index].end = last;
         semarray[index].plist[last] = proc_nr;
      }else {
         ++last;
         semarray[index].end = last;
         semarray[index].plist[last] = proc_nr;
      }
      // increment process list count
      ++(semarray[index].pcount); // one more sleeping process
      
      // suspend this process, i.e., put to sleep
      return (SUSPEND);
   }

   return exitstatus;
}


////
// do_semfree
//
//   This call frees a semaphore that's currently allocated, making the slot in
//   the kernel available for reuse.  A semaphore may not be released if there
//   are processes waiting on it; the process that wants to free the semaphore
//   must call semup() if necessary to ensure that there are no waiting
//   processes before freeing it.  The call returns 1 if successful, 0
//   otherwise.
//
//   Error codes include:
//     EBUSY
//       The semaphore has process(es) waiting on it.
//
//     EEXIST
//       The semaphore doesn't currently exist.
//
PUBLIC int do_semfree (void) {
   // return 1 if successful, 0 otherwise
   int exitstatus = RETERR; // not yet successful
   // get message
   int sem = m_in.m1_i1;
   // ERROR CHECKING //
   // errors: EEXIST
   //   check if id is negative or 0
   //   check if id exceeds 2^31
   if (sem <= 0 || sem > INT_LIMIT - 1) {
      m_in.m_type = EINVAL;
      return RETERR;
   }
   
   // find the index for the semaphore array if it exists
   int index;
   index = (sem % NR_SEMS) - 1;
   if (index < 0) index = NR_SEMS - 1;
   int i;
   for (i = 0; i < NR_SEMS && exitstatus == RETERR; ++i) {
      if (semarray[index].id == sem) exitstatus = 1; // could successfully free
      else if (index == (NR_SEMS - 1)) index = 0;
      else ++index; // increment the index
   }

   // MORE ERROR CHECKING //
   // errors: EEXIST
   //   could not find the semaphore
   if (exitstatus == RETERR) {
      m_in.m_type = EEXIST;
      return RETERR;
   }
   // errors: EBUSY
   //   check if the semaphore has processes waiting on it
   if (semarray[index].pcount != 0) {
      m_in.m_type = EBUSY;
      return RETERR;
   }

   // go ahead and free the semaphore
   --semcount;
   // "mark" the semaphore as free
   semarray[index].id = NO_SEM;
   semarray[index].value = 0;
   // "empty" the process list
   semarray[index].pcount = 0; // there are no processes in the process list
   semarray[index].plist[0] = NO_PROC_NR; // 
   semarray[index].begin = 0;
   semarray[index].pcount = 0;
   
   return exitstatus;
}
