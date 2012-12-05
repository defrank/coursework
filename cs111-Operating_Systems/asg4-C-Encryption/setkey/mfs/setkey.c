/*******************************************************************************
 * $Id: setkey.c,v 1.1 2012-06-04 21:36:44-07 dmfrank - $
 * Derek Frank (dmfrank@ucsc.edu)
 * Alexander McCaleb (amccaleb@ucsc.edu)
 * Alexander Kerr (ahkerr@ucsc.edu)
 *
 * NAME
 *   setkey.c - implementation file
 *
 * DESCRIPTION
 *   A system call that sets the key for the current user.  The two most
 *   significant integers (half the AES key) are zero, with k0 and k1 occupying
 *   the other positions.
 *
  ******************************************************************************/

#include <stdio.h>
#include <unistd.h>

typedef struct key {
	//uid_t user_id;
	unsigned int e_key;
	unsigned int d_key;
} key;

key keys[8];
int next_slot = 0;

void fs_setkey (unsigned int k0, unsigned int k1) {
	//printf("Hello from fs_setkey\n");
	//keys[next_slot].user_id = geteuid();
	keys[next_slot].e_key = k0;
	keys[next_slot].d_key = k1;
	//printf("ENcryption key: %d\n", keys[next_slot].e_key);
	//printf("DEcryption key: %d\n", keys[next_slot].d_key);
	next_slot += next_slot % 8;
}