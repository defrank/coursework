/* $Id: ListTest.java,v 1.1 2010-10-10 04:36:14-07 - - $
 * Derek Frank, dmfrank@ucsc.edu
 * 
 * NAME
 *   ListTest -- used to test List.java.
 * 
 * DESCRIPTION
 *   Tests List.java to check if it functions properly and according to specifications.
 */

class ListTest {
  public static void main (String[] args) {
    List A = new List();
    List B = new List();
    
    for ( int i=1; i<=10; ++i) {
      A.insertAfterLast(i);
      B.insertAfterLast(11-i);
    }
    System.out.println("A = " + A);
    System.out.println("B = " + B);

    for(int i=1; i<=6; ++i){
      A.insertAfterLast(B.getFirst());
      B.deleteFirst();
    }
    System.out.println("A = " + A);
    System.out.println("B = " + B);
    List C = A.copy();
    System.out.println("C = " + C);
    System.out.println("A " + (A.equals(B)?"equals":"does not equal") + " B");
    System.out.println("A " + (A.equals(C)?"equals":"does not equal") + " C");
    
    List D = A.cat(B);
    System.out.println("D = " + D);
    System.out.println("A " + (A.equals(D)?"equals":"does not equal") + " D");
    System.out.println("D " + (D.equals(A)?"equals":"does not equal") + " A");
    boolean lengthCompare = ( A.getLength() == D.getLength() );
    System.out.println("Length A = " + A.getLength() + " and Length D = " + D.getLength());
    System.out.println("Length Compare = " + lengthCompare);
    
    
  }
}