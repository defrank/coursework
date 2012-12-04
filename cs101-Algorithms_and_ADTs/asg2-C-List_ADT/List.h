/*********************************************************************************
 *  $Id: List.h,v 1.3 2010-10-17 08:24:36-07 - - $
 *	Derek Frank, dmfrank@ucsc.edu
 *
 *	NAME
 *	  List.h
 *
 *	DESCRIPTION
 *    Header file for List ADT
 *********************************************************************************/

#if !defined(_LIST_H_INCLUDE_)
#define _LIST_H_INCLUDE_

/***************************** Exported Types *************************************/
typedef struct List * ListRef;


/************** Constructors-Destructors ******************************************/

/*
 *  newNode
 *  Returns pointer to new Node struct. Initializes next field to NULL, and sets
 *  data field to input parameter node_data. Private.
 *
NodeRef newNode(int node_data); */

/*
 *  freeNode
 *  Frees heap memory pointed to by pN. Private.
 *
void freeNode(NodeRef* pN); */

/*
 *  newList()
 *  Returns ListRef pointing to new ListStruct which represents an empty List.
 *  Initializes front, current, and back fields to NULL, sets length field to 0.
 *	Exported.
 */
ListRef newList(void);

/*
 *  freeList()
 *  Frees all heap memory associated with the ListRef *pL, including all memory
 *  in existing Nodes.  Sets *pL to NULL.  Exported.
 *	Post: isEmpty(pL)
 */
void freeList(ListRef* pL);


/***************** Access functions ***********************************************/

/*
 *  isEmpty()
 *  Returns True if L is empty, otherwise returns false
 */
int isEmpty(ListRef L);

/*
 *  offEnd()
 *  Returns True if curr is NULL in L, otherwise returns false
 */
int offEnd(ListRef L);

/*
 *  atFirst()
 *  Returns True if curr is pointing at the front of L, otherwise returns false
 *	Pre: !isEmpty(L); !offEnd(L).
 */
int atFront(ListRef L);

/*
 *  atLast()
 *  Returns True if curr is pointing at the back of L, otherwise returns false
 *	Pre: !isEmpty(L); !offEnd(L).
 */
int atBack(ListRef L);

/*
 *  getFront()
 *  Returns the value at the front of L.
 *  Pre: !isEmpty(L)
 */
int getFront(ListRef L);

/*
 *  getBack()
 *  Returns the value at the back of L.
 *  Pre: !isEmpty(L)
 */
int getBack(ListRef L);

/*
 *  getCurrent()
 *  Returns the value pointed at by curr in L.
 *  Pre: !isEmpty(L); !offEnd(L).
 */
int getCurrent(ListRef L);

/*
 *  getLength()
 *  Returns the length of L
 */
int getLength(ListRef L);

/*
 *  equals()
 *  Returns true if A is identical to B, false otherwise.
 *	Ignores current marker position.
 *  Pre: !isEmpty(A,B)
 */
int equals(ListRef A, ListRef B);


/****************************** Manipulation procedures ***************************/

/*
 *  makeEmpty()
 *  Sets L to the empty state.
 *	Post: isEmpty(L).
 */
void makeEmpty(ListRef L);

/*
 *  moveFront()
 *  Sets current marker to front.
 *	Pre: !isEmpty(L).
 */
void moveFront(ListRef L);

/*
 *  moveLast()
 *  Sets current marker to back.
 *	Pre: !isEmpty(L).
 */
void moveLast(ListRef L);

/*
 *  movePrev()
 *  Moves current marker one step toward front.
 *	Pre: !isEmpty(L); !offEnd()L.
 */
void movePrev(ListRef L);

/*
 *  moveNext()
 *  Moves current marker one step toward last element.
 *	Pre: !isEmpty(L); !offEnd(L).
 */
void moveNext(ListRef L);

/*
 *  insertBeforeFront()
 *  Places new data element at the front of L
 *  Post: !isEmpty(L)
 */
void insertBeforeFront(ListRef L, int data);

/*
 *  insertAfterBack()
 *  Places new data element at the back of L
 *  Post: !isEmpty(L)
 */
void insertAfterBack(ListRef L, int data);

/*
 *  insertBeforeCurrent()
 *  Places new data element before curr in L
 *  Pre: !isEmpty(L); !offEnd()
 */
void insertBeforeCurrent(ListRef L, int data);

/*
 *  insertAfterCurrent()
 *  Places new data element after curr in L
 *  Pre: !isEmpty(L); !offEnd()
 */
void insertAfterCurrent(ListRef L, int data);

/*
 *  deleteFront()
 *  Deletes element at front of L
 *  Pre: !isEmpty(L)
 */
void deleteFront(ListRef L);

/*
 *  deleteBack()
 *  Deletes element at back of L
 *  Pre: !isEmpty(L)
 */
void deleteBack(ListRef L);

/*
 *  deleteCurrent()
 *  Deletes element at curr in L
 *  Pre: !isEmpty(L); !offEnd(L)
 */
void deleteCurrent(ListRef L);


/*************** Other Functions *************************************************/

/*
 *  printList()
 *  Prints data elements in L on a single line to specified output file.
 */
void printList( FILE* out, ListRef L);
/*
 *  copyList()
 *  Returns a ListRef to a new list equivalent to L
 */
ListRef copyList(ListRef L);

/*
 *	catList()
 *	Returns a new List that is the concatenation of List A
 *	followed by List B.  The current marker in the new list is
 *	undefined regardless of the states of the current markers in
 *	the two lists.  The states of the two Lists are unchanged.
 */
ListRef catList(ListRef A, ListRef B);

#endif
