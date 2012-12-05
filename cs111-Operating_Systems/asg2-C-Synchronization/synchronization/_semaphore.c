/*******************************************************************************
 * $Id: _semaphore.c,v 1.1 2012-05-12 19:48:02-07 dmfrank - $
 * Derek Frank (dmfrank@ucsc.edu)
 * Harrison Vuong (hvuong@ucsc.edu)
 * David Zou (dzou@ucsc.edu)
 *
 * NAME
 *   _semaphore.c - implementation file
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
 *
 ******************************************************************************/

#include <sys/cdefs.h>
#include "namespace.h"
#include <lib.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <minix/callnr.h>



/***************************** Access Functions *******************************/

////
// semvalue
//
//   This call returns the current value of the semaphore whose identifier is
//   passed.  If there are n processes waiting on the semaphore, the call should
//   return n.  If the semaphore has a value of 0, the next process to call
//   semdown() would wait, but no process is currently waiting.  If an error
//   occurs, return 0x8000000.
//
PUBLIC int semvalue (int sem) {
   message m;
   m.m1_i1 = sem;
   return (_syscall (PM_PROC_NR, SEMVALUE, &m));
}



/************************** Manipulation Functions ****************************/

////
// seminit
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
//       Semaphore initial value is not in the range -1000<=value<=1000.
//
//     EEXIST
//       Semaphore identifier is already in use for an active semaphore (one
//       that was freed with semfree() is not active).
//
PUBLIC int seminit (int sem, int value) {
   message m;
   m.m1_i1 = sem;
   m.m1_i2 = value;
   return (_syscall (PM_PROC_NR, SEMINIT, &m));
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
//   -1000<=value<=1000, it may be inremented (or decremented) to a value
//   outside this range, up to +/-10^6.
//
PUBLIC int semup (int sem) {
   message m;
   m.m1_i1 = sem;
   return (_syscall (PM_PROC_NR, SEMUP, &m));
}


////
// semdown
//
//   This call does DOWN on the semaphore whose identifier is passed.  If the
//   semaphore value would go below zero, the call blocks until the value goes
//   above zero again.  The call returns 1 if successful, 0 otherwise.
//
//   NOTE: while a semaphore can't be initialized outside the range
//   -1000<=value<=1000, it may be inremented (or decremented) to a value
//   outside this range, up to +/-10^6.
//
PUBLIC int semdown (int sem) {
   message m;
   m.m1_i1 = sem;
   return (_syscall (PM_PROC_NR, SEMDOWN, &m));
}


////
// semfree
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
PUBLIC int semfree (int sem) {
   message m;
   m.m1_i1 = sem;
   return (_syscall (PM_PROC_NR, SEMFREE, &m));
}
