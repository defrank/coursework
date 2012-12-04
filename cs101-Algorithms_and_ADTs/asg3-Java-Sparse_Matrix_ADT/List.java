/* $Id: List.java,v 1.2 2010-10-31 07:26:38-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * 
 * NAME
 *   List -- a double ended Object queue with a current-position 
 * marker.
 * 
 * DESCRIPTION
 *   A linked list for Objects with markers to keep track of the
 * front and rear of the double ended queue.  Includes a current-
 * position marker that can move to previous and next positions.
 */

class List {
  
  // Inner Classes //////////////////////////////////////////////
  
  /* class Node
   * Private inner class
   */
  private class Node{
    
    // Fields //
    Object data;
    Node next;
    Node prev;
    
    // Constructor //
    
    /* Node()
     * Create new node.
     */
    Node(Object data) {
      this.data = data;
      next = prev = null;
    }
    
    // Access Functions //
    
    /* equals()
     * Overrides Object's equals() method.
     * Pre: (obj instanceof Node)
     */
    public boolean equals(Object obj) {
      if ( !(obj instanceof Node) ) return false;
      if (obj == this) return true;
      Node N = (Node)obj;
      
      Node M = this;
      return N.data.equals(M.data);
    }
    
    // Other Functions //
    
    /* toString()
     * Overides Object's toString method.
     */
    public String toString() {
      String str = "";
      str += this.data.toString();
      return str;
    }
    
  }
  
  // Fields /////////////////////////////////////////////////////
  private Node front;
  private Node back;
  private Node curr;
  private int length;
  
  // Constructors ///////////////////////////////////////////////
  
  /* List()
   * Create new empty List.
   */
  List() {
    front = back = curr = null;
    length = 0;
  }
  
  // Access functions ///////////////////////////////////////////
  
  /* isEmpty()
   * Returns true is this List is empty, false otherwise.
   */
  boolean isEmpty() {
    return (length == 0);
  }
  
  /* offEnd()
   * Returns true if current is undefined.
   */
  boolean offEnd() {
    return (this.curr == null);
  }
  
  /* atFirst()
   * Returns true if first element is current.
   * Precondition: !isEmpty().
   */
  boolean atFirst() { 
    if ( this.isEmpty() || this.offEnd() ) return false;
    return (this.curr.prev == null);
  }
  
  /* atLast()
   * Returns true if last element is current.
   * Pre: !isEmpty().
   */
  boolean atLast() {
    if ( this.isEmpty() || this.offEnd() ) return false;
    return (curr.next == null);
  }
  
  /* getFirst()
   * Returns first element.
   * Pre: !isEmpty().
   */
  Object getFirst() {
    if( this.isEmpty() ) {
      throw new RuntimeException("List Error: getFirst() called on empty List");
    }
    return front.data;
  }
  
  /* getLast()
   * Returns last element.
   * Pre: !isEmpty().
   */
  Object getLast() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: getLast() called on empty List");
    }
    return back.data;
  }
  
  /* getCurrent()
   * Returns current element.
   * Pre: !isEmpty(), !offEnd().
   */
  Object getCurrent() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: getCurrent() called on empty List");
    }else if ( this.offEnd() ) {
      throw new RuntimeException("List Error: getCurrent() called on Null Pointer");
    }
    return curr.data;
  }
  
  /* getLength()
   * Returns length of this List.
   */
  int getLength() { return length; }
  
  /* equals()
   * Overrides Object's equals() method.
   * Pre: (obj instanceof List)
   */
  public boolean equals(Object obj) {
    if ( !(obj instanceof List) ) return false;
    if (obj == this) return true;
    List L = (List)obj;
    
    boolean flag  = true;
    Node N = this.front;
    Node M = L.front;
    
    if ( this.length == L.length ) {
      while ( flag && N != null ) {
        flag = ( N.equals(M) );
        N = N.next;
        M = M.next;
      }
      return flag;
    }else {
      return false;
    }
  }
  
  // Manipulation procedures ////////////////////////////////////
  
  /* makeEmpty()
   * Sets this List to the empty state.
   * Post: isEmpty().
   */
  void makeEmpty() {
    this.back = null;
    while ( !this.isEmpty() ) {
      this.curr = this.front;
      this.curr = this.curr.next;
      if ( this.curr != null ) this.curr.prev = null;
      this.front.next = null;
      --length;
    }
    this.front = null;
  }
  
  /* moveFirst()
   * Sets current marker to first element.
   * Pre: !isEmpty(); Post: !offEnd().
   */
  void moveFirst() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: moveFirst() called on empty List");
    }else if ( this.offEnd() ) { this.curr = this.front; }
    else {
      while ( this.curr.prev != null ) {
        this.curr = this.curr.prev;
      }
    }
  }
  
  /* moveLast()
   * Sets current marker to last element
   * Pre: !isEmpty(); Post: !offEnd().
   */
  void moveLast() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: moveLast() called on empty List");
    }else if ( this.offEnd() ) { this.curr = this.back; }
    else {
      while ( this.curr.next != null ) {
        this.curr = this.curr.next;
      }
    }
  }
  
  /*movePrev()
   * Moves current marker one step toward first element.
   * Pre: !isEmpty(), !offEnd().
   */
  void movePrev() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: movePrev() called on empty List");
    }else if ( this.offEnd() ) {
      throw new RuntimeException("List Error: movePrev() called on Null Pointer");
    }else { this.curr = this.curr.prev; }
  }
  
  /* moveNext()
   * Moves current marker one step toward last element.
   * Pre: !isEmpty(), !offEnd().
   */
  void moveNext() {
    if ( this.isEmpty() ) {
      throw new RuntimeException("List Error: moveNext() called on empty List");
    }else if ( this.offEnd() ) {
      throw new RuntimeException("List Error: moveNext() called on Null Pointer");
    }else { this.curr = this.curr.next; }
  }
  
  /* insertBeforeFirst()
   * Inserts new element before first element.
   * Post: !isEmpty().
   */
  void insertBeforeFirst(Object data) {
    Node N = new Node(data);
    if ( this.isEmpty() ) { front = back = N; }
    else { N.next = front; front.prev = N; front = N; }
    ++length;
  }
  
  /* insertAfterLast()
   * Inserts new element after last element.
   * Post: !isEmpty().
   */
  void insertAfterLast(Object data) {
    Node N = new Node(data);
    if ( this.isEmpty() ) { front = back = N; }
    else { N.prev = back; back.next = N; back = N; }
    ++length;
  }
  
  /* insertBeforeCurrent()
   * Inserts new element before current element.
   * Pre: !isEmpty(), !offEnd().
   */
  void insertBeforeCurrent(Object data){
    if ( this.isEmpty() ) {
      throw new RuntimeException("ListError: insertBeforeCurrent() called on empty List");
    }else if ( this.offEnd() ) {
      throw new RuntimeException("ListError: insertBeforeCurrent() called on a Null Pointer");
    }
    Node N = new Node(data);
    if ( curr.prev == null) {
      N.next = front;
      front.prev = N;
      front = N;
    }else {
      N.prev = curr.prev;
      N.next = curr;
      curr.prev.next = N;
      curr.prev = N;
    }
    ++length;
  }
  
  /* insertAfterCurrent()
   * Inserts new element after current element.
   * Pre: !isEmpty(), !offEnd().
   */
  void insertAfterCurrent(Object data) {
    if ( this.isEmpty() ) {
      throw new RuntimeException("ListError: insertAfterCurrent() called on empty List");
    }else if ( this.offEnd() ) {
      throw new RuntimeException("ListError: insertAfterCurrent() called on a Null Pointer");
    }
    Node N = new Node(data);
    if ( curr.next == null) {
      back.next = N;
      back = N;
      back.prev = curr;
    }else {
      N.prev = curr;
      N.next = curr.next;
      curr.next.prev = N;
      curr.next = N;
    }
    ++length;
  }
  
  /* deleteFirst()
   * Deletes first element.
   * Pre: !isEmpty().
   */
  void deleteFirst() {
    if( this.isEmpty() ){
      throw new RuntimeException("List Error: deleteFirst() called on empty List");
    }
    if ( this.length > 1 ) {
      front = front.next;
      front.prev = null;
      if ( !this.offEnd() ) {
        if ( curr.prev == null && curr.next == null ) { curr = null; }
      }
    }
    else { front = back = curr = null; }
    --length;
  }
  
  /* deleteLast()
   * Deletes last element.
   * Pre: !isEmpty().
   */
  void deleteLast() {
    if( this.isEmpty() ){
      throw new RuntimeException("List Error: deleteLast() called on empty List");
    }
    if ( this.length > 1 ) {
      back = back.prev;
      back.next = null;
      if ( !this.offEnd() ) {
        if ( curr.prev == null && curr.next == null ) { curr = null; }
      }
    }
    else { front = back = curr = null; }
    --length;
  }
  
  /* deleteCurrent()
   * Deletes current element.
   * Pre: !isEmpty(), !offEnd(); Post: offEnd().
   */
  void deleteCurrent() {
    if( this.isEmpty() ) {
      throw new RuntimeException("List Error: deleteCurrent() called on empty List");
    }else if ( this.offEnd() ){
      throw new RuntimeException("List Error: deleteCurrent() called on a Null Pointer");
    }
    if ( this.length > 1 ) {
      if ( curr.prev == null ) {
        front = curr.next;
        curr.next = null;
      }else if ( curr.next == null ) {
        back = curr.prev;
        curr.prev = null;
      }else {
        curr.prev.next = curr.next;
        curr.next.prev = curr.prev;
        curr.prev = curr.next = null;
      }
      curr = null;
    }
    else { front = back = curr = null; }
    --length;
  }
  
  // Other methods //////////////////////////////////////////////
  
  /* toString()
   * Overrides Object's toString method and returns a string
   * representation of this List consisting of a space
   * separated list of integers.
   */
  public String toString() {
    String str = "";
    for (Node N = front; N != null; N = N.next){
      str += N.toString() + " ";
    }
    return str;
  }
  
}