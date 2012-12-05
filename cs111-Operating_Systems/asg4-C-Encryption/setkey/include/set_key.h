/*******************************************************************************
 * $Id: setkey.h,v 1.1 2012-06-04 21:36:44-07 dmfrank - $
 * Derek Frank (dmfrank@ucsc.edu)
 * Alexander McCaleb (amccaleb@ucsc.edu)
 *
 * NAME
 *   setkey.h - definition file
 *
 * DESCRIPTION
 *   A system call that sets the key for the current user.  The two most
 *   significant integers (half the AES key) are zero, with k0 and k1 occupying
 *   the other positions.
 *
  ******************************************************************************/

#include <lib.h>
#include <unistd.h>

PUBLIC int set_key (unsigned int k0, unsigned int k1);