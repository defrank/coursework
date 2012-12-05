/* $Id: Fork.java,v 1.1 2011-03-11 22:30:08-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    Fork
 *
 * DESCRIPTION
 *    A Fork class describes an obect that can be picked up and put down
 *    by only one other object at a time.
 */

class Fork {
   // Fields
   private Boolean held;

   // Constructor
   Fork () {
      held = false;
   }

   // Functions
   public Boolean isHeld () {
      return held;
   }

   public synchronized void pickup () {
      while (held) {
         try {
            wait ();
         } catch (InterruptedException e) {
            System.out.printf ("wait interrupted: %s", e);
         }
      }
      
      held = true;
   }

   public synchronized void putdown () {
      if ( held == false )
         throw new RuntimeException
            ("Fork: calling putdown() on unheld fork");

      held = false;
      notify ();
   }
}
