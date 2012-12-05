/*******************************************************************************
 * $Id: lockcond.c,v 1.1 2012-05-12 19:48:02-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   lockcond.c - implementation file
 *
 ******************************************************************************/

#include <lib.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <minix/lockcond.h>

void lock_init (struct lock *l)
{
   assert (l != NULL);

   l->mutex = seminit(0,1);
   l->next = seminit(0,0);
   l->nextCount = 0;
}

void lock_acquire (struct lock *l)
{
   assert (l != NULL);
   semdown(l->mutex);
}

void lock_release (struct lock *l)
{
   assert (l != NULL);
   if(l->nextCount > 0)
   {
      // l->nextCount -= 1;  SHOULD NOT BE HERE!!!
      semup (l->next); //// MISSING!!!!!!!!!
   }
   else
   {
      semup(l->mutex);
   }
}

void cond_init(struct cond *cnd, struct lock *l)
{
   assert (cnd != NULL && l != NULL);
   cnd->lock = l;
   cnd->condSem = seminit(0,0);
   cnd->semCount = 0;

}

void cond_wait(struct cond *cnd)
{	
   assert (cnd != NULL);
   cnd->semCount += 1;
   lock_release(cnd->lock);
   semdown(cnd->condSem);
   if(cnd->lock->nextCount > 0)
   {
      semdown(cnd->lock->next);
      cnd->lock->nextCount -= 1;
   }
   cnd->semCount -= 1;
}

void cond_signal (struct cond *cnd)
{
   assert (cnd != NULL);
   if(cnd->semCount > 0)
   {
      cnd->lock->nextCount += 1;
      semup(cnd->condSem);
   }
}

