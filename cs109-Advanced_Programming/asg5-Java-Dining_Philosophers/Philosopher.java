/* $Id: Philosopher.java,v 1.2 2012/12/05 04:49:23 dmf Exp $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    Philosophers
 *
 * DESCRIPTION
 *    A class that implements Runnable.  Used to create a Thread.  An
 *    abstract object describing a Philosopher sitting at a table with
 *    a Fork object to his left and to his right.  This philosopher has
 *    two specified amounts of time in milliseconds to eat and think.
 *    When the Philosopher is eating or thinking, the sleep() method is
 *    called.  The Philosopher can only think when it is not holding
 *    either of its Fork objects.  The Philosopher can only eat when it
 *    is holding both its Fork objects.  It cannot eat or think when
 *    holding only a single Fork object.  The Philosopher also has a
 *    handedness specification to determine which Fork object it tries
 *    to pick up first.
 */

import static java.lang.System.*;

class Philosopher implements Runnable {
   // Fields
   private int id;
   private int eatTime;
   private int thinkTime;
   private Fork left;
   private Fork right;
   private String handedness;
   
   // Constructor
   Philosopher (int id, int eT, int tT, Fork l, Fork r, String hand) {
      this.id = id;
      eatTime = eT;
      thinkTime = tT;
      left = l;
      right = r;
      handedness = hand;
   }

   // Functions
   public void run () {
      // Notify to stdout that this Philosopher has started
      out.printf ("Philosopher %d: started%n", id);
      out.flush ();

      // Put Philosopher to sleep to allow others to "wake up"
      try {
         Thread.currentThread().sleep (100);
      } catch (InterruptedException e) {
         out.printf ("sleep interrupted: %s%n", e);
      }
      
      // First start thinking
      try {
         out.printf ("Philosopher %d: thinking%n",id);
         Thread.currentThread().sleep (thinkTime);
         out.printf ("Philosopher %d: finished thinking%n", id);
      } catch (InterruptedException e) {
         out.printf ("thinking interrupted: %s%n", e);
      }

      // Now try eating, but can only eat when holding both forks.
      out.flush ();
      try {
         if ( handedness.equals("left") ) {
            left.pickup ();
            right.pickup ();
         } else {
            right.pickup ();
            left.pickup ();
         }
         out.printf ("Philosopher %d: eating%n", id);
         Thread.currentThread().sleep (eatTime);
         out.printf ("Philosopher %d: finished eating%n", id);
         left.putdown ();
         right.putdown ();
      } catch (InterruptedException e) {
         out.printf ("eating interrupted: %s%n", e);
      } catch (RuntimeException re) {
         out.printf ("%s%n", re);
      }

      out.flush ();
   }
}
