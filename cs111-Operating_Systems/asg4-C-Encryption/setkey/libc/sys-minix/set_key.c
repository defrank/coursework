/*******************************************************************************
 * $Id: setkey.c,v 1.1 2012-06-04 21:36:44-07 dmfrank - $
 * Derek Frank (dmfrank@ucsc.edu)
 * Alexander McCaleb (amccaleb@ucsc.edu)
 *
 * NAME
 *   set_key.c - implementation file
 *
 * DESCRIPTION
 *   A system call that sets the key for the current user.  The two most
 *   significant integers (half the AES key) are zero, with k0 and k1 occupying
 *   the other positions.
 *
  ******************************************************************************/

#include <lib.h>
#include <unistd.h>
#include <set_key.h>

PUBLIC int set_key (unsigned int k0, unsigned int k1) {
   message m;

   m.m1_i1 = k0;
   m.m1_i2 = k1;
   
   printf("Before invoking the system call\n");
   return (_syscall (VFS_PROC_NR, DO_SETKEY, &m));
}
