/*******************************************************************************
 * $Id: lockcond.h,v 1.1 2012-05-12 19:48:02-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * Harrison Vuong, hvuong@ucsc.edu
 * David Zou, dzou@ucsc.edu
 *
 * NAME
 *   lockcond.h - interface file
 *
 ******************************************************************************/


#ifndef __LOCKCOND_H__
#define __LOCKCOND_H__

struct lock
{
   int nextCount;
   int mutex; //semaphore
   int next; //semaphore
};

struct cond
{
   struct lock *lock;
   int condSem;
   int semCount;

};

void lock_init (struct lock *l);

void lock_acquire (struct lock *l);

void lock_release (struct lock *l);

void cond_init(struct cond *cnd, struct lock *l);

void cond_wait(struct cond *cnd);

void cond_signal (struct cond *cnd);

#endif

