/*******************************************************************************
 *  $Id: List.c,v 1.2 2010-11-14 19:30:00-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  List.c
 *  
 *	DESCRIPTION
 *	  Implementation file for the List ADT.  A linked list for integers with
 *	markers to keep track of the front and rear of the double ended queue.
 *	Includes a current-position marker that can move between previous and next
 *	positions.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "List.h"

/************** Private Structs: not exported *********************************/

typedef struct Node {
	int data;
	struct Node* next;
	struct Node* prev;
} Node;

typedef Node* NodeRef;

typedef struct List {
	NodeRef front;
	NodeRef back;
	NodeRef curr;
	int length;
} List;


/************** Constructors-Destructors **************************************/

/*
 *  newNode
 *  Returns pointer to new Node struct. Initializes next field to NULL, and sets
 *  data field to input parameter node_data.
 *	Private.
 */
NodeRef newNode(int node_data) {
	NodeRef N = malloc(sizeof(Node));
	N->data = node_data;
	N->next = NULL;
	N->prev = NULL;
	return (N);
}

/*
 *  freeNode
 *  Frees heap memory pointed to by pN.
 *	Private.
 */
void freeNode(NodeRef* pN) {
	if ( pN != NULL && *pN != NULL ) {
		free(*pN);
		*pN = NULL;
	}
}


/*
 *  newList()
 *  Returns ListRef pointing to new List Struct which represents an empty List.
 *  Initializes front, current, and back fields to NULL, sets length field to 0.
 *	Exported.
 */
ListRef newList(void) {
	ListRef L;
	L = malloc(sizeof(List));
	L->front = L->back = L->curr = NULL;
	L->length = 0;
	return (L);
}

/*
 *  freeList()
 *  Frees all heap memory associated with the ListRef *pL, including all memory
 *  in existing Nodes.  Sets *pL to NULL.
 *	Exported.
 *	Post: isEmpty(pL)
 */
void freeList(ListRef* pL) {
	if ( pL == NULL || *pL == NULL ) {
		return;
	}
	while ( !isEmpty(*pL) ) {
		deleteFront(*pL);
	}
	free(*pL);
	*pL = NULL;
}



/***************** Access functions *******************************************/

/*
 *  isEmpty()
 *  Returns True if L is empty, otherwise returns false.
 */
int isEmpty(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling isEmpty() on NULL ListRef\n");
		exit(1);
	}
	return (L->length == 0);
}

/*
 *  offEnd()
 *  Returns True if curr is NULL in L, otherwise returns false.
 */
int offEnd(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling offEnd() on NULL ListRef\n");
		exit(1);
	}
	return (L->curr == NULL);
}

/*
 *  atFront()
 *  Returns True if curr is pointing at the front of L, otherwise returns false.
 *	Pre: !isEmpty(L); !offEnd(L).
 */
int atFront(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling atFront() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling atFront() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling atFront() on NULL pointer\n");
		exit(1);
	}
	return (L->curr->prev == NULL);
}

/*
 *  atBack()
 *  Returns True if curr is pointing at the back of L, otherwise returns false.
 *	Pre: !isEmpty(L); !offEnd(L).
 */
int atBack(ListRef L) {
	if ( L == NULL ){
		printf("List Error: calling atBack() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling atBack() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling atBack() on NULL pointer\n");
		exit(1);
	}
	return (L->curr->next == NULL);
}

/*
 *  getFront()
 *  Returns the value at the front of L.
 *  Pre: !isEmpty(L)
 */
int getFront(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling getFront() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling getFront() on an empty List\n");
		exit(1);
	}
	return (L->front->data);
}

/*
 *  getBack()
 *  Returns the value at the back of L.
 *  Pre: !isEmpty(L)
 */
int getBack(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling getBack() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling getBack() on an empty List\n");
		exit(1);
	}
	return (L->back->data);
}

/*
 *  getCurrent()
 *  Returns the value pointed at by curr in L.
 *  Pre: !isEmpty(L); !offEnd(L).
 */
int getCurrent(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling getCurrent() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling getCurrent() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling getCurrent() on NULL pointer\n");
		exit(1);
	}
	return (L->curr->data);
}

/*
 *  getLength()
 *  Returns the length of L.
 */
int getLength(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling getLength() on NULL ListRef\n");
		exit(1);
	}
	return (L->length);
}

/*
 *  equals()
 *  Returns true if A is identical to B, false otherwise.  Ignores current
 *	marker position.
 *  Pre: !isEmpty(A,B)
 */
int equals(ListRef A, ListRef B) {
	int flag = 1;
	NodeRef N = NULL;
	NodeRef M = NULL;
	
	if ( A==NULL || B==NULL ) {
		printf("List Error: calling equals() on NULL ListRef\n");
		exit(1);
	}
	N = A->front;
	M = B->front;
	if ( getLength(A) == getLength(B) ) {
		while ( flag && N != NULL ) {
			flag = (N->data == M->data);
			N = N->next;
			M = M->next;
		}
		return flag;
	}
	return (0);
}


/**************** Manipulation procedures *************************************/

/*
 *  makeEmpty()
 *  Sets L to the empty state.
 *	Post: isEmpty(L).
 */
void makeEmpty(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling makeEmpty() on NULL ListRef\n");
		exit(1);
	}
	if ( L->front == NULL) {
		return;
	}
	while( !isEmpty(L) ) {
		deleteFront(L);
	}
}

/*
 *  moveFront()
 *  Sets current marker to front.
 *	Pre: !isEmpty(L).
 */
void moveFront(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling moveFront() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling moveFront() on an empty List\n");
		exit(1);
	}
	L->curr = L->front;
}

/*
 *  moveLast()
 *  Sets current marker to back.
 *	Pre: !isEmpty(L).
 */
void moveLast(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling moveLast() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling moveBack() on an empty List\n");
		exit(1);
	}
	L->curr = L->back;
}

/*
 *  movePrev()
 *  Moves current marker one step toward front.
 *	Pre: !isEmpty(L); !offEnd(L).
 */
void movePrev(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling movePrev() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling movePrev() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling movePrev() on a NULL pointer\n");
		exit(1);
	}
	L->curr = L->curr->prev;
}

/*
 *  moveNext()
 *  Moves current marker one step toward last element.
 *	Pre: !isEmpty(L); !offEnd(L).
 */
void moveNext(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling moveNext() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling moveNext() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling moveNext() on a NULL pointer\n");
		exit(1);
	}
	L->curr = L->curr->next;
}

/*
 *  insertBeforeFront()
 *  Places new data element at the front of L.
 *  Post: !isEmpty(L)
 */
void insertBeforeFront(ListRef L, int data) {
	if ( L == NULL ) {
		printf("List Error: calling insertBeforeFront() on NULL ListRef\n");
		exit(1);
	}
	
	NodeRef N = newNode(data);
	if ( isEmpty(L) ) {
		L->front = L->back = N;
	}
	else {
		N->next = L->front;
		L->front->prev = N;
		L->front = N;
	}
	++L->length;
}

/*
 *  insertAfterBack()
 *  Places new data element at the back of L.
 *  Post: !isEmpty(L)
 */
void insertAfterBack(ListRef L, int data) {
	if ( L == NULL ) {
		printf("List Error: calling insertAfterBack() on NULL ListRef\n");
		exit(1);
	}
	
	NodeRef N = newNode(data);
	if ( isEmpty(L) ) {
		L->front = L->back = N;
	}
	else {
		L->back->next = N;
		N->prev = L->back;
		L->back = N;
	}
	++L->length;
}

/*
 *  insertBeforeCurrent()
 *  Places new data element before curr in L.
 *	Pre: !isEmpty(L); !offEnd(L)
 */
void insertBeforeCurrent(ListRef L, int data) {
	if ( L == NULL ) {
		printf("List Error: calling insertBeforeCurrent() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling insertBeforeCurrent() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling insertBeforeCurrent() on a NULL pointer\n");
		exit(1);
	}
	
	NodeRef N = newNode(data);
	if ( L->curr->prev == NULL ) {
		N->next = L->front;
		L->front->prev = N;
		L->front = N;
	}else {
		N->prev = L->curr->prev;
		N->next = L->curr;
		L->curr->prev->next = N;
		L->curr->prev = N;
	}
	++L->length;
}

/*
 *  insertAfterCurrent()
 *  Places new data element after curr in L.
 *  Pre: !isEmpty(L); !offEnd()
 */
void insertAfterCurrent(ListRef L, int data) {
	if ( L == NULL ) {
		printf("List Error: calling insertAfterCurrent() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling insertAfterCurrent() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling insertAfterCurrent() on a NULL pointer\n");
		exit(1);
	}
	
	NodeRef N = newNode(data);
	if ( L->curr->next == NULL ) {
		L->back->next = N;
		N->prev = L->back;
		L->back = N;
	}else {
		N->prev = L->curr;
		N->next = L->curr->next;
		L->curr->next->prev = N;
		L->curr->next = N;
	}
	++L->length;
}

/*
 *  deleteFront()
 *  Deletes element at front of L.
 *  Pre: !isEmpty(L)
 */
void deleteFront(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling deleteFront() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling deleteFront() on an empty List\n");
		exit(1);
	}
	
	NodeRef N = NULL;
	N = L->front;
	if ( getLength(L) > 1 ) {
		L->front = L->front->next;
		L->front->prev = NULL;
	}else {
		L->front = L->back = L->curr = NULL;
	}
	--L->length;
	freeNode(&N);
}

/*
 *  deleteBack()
 *  Deletes element at back of L.
 *  Pre: !isEmpty(L)
 */
void deleteBack(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling deleteBack() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling deleteBack() on an empty List\n");
		exit(1);
	}
	
	NodeRef N = NULL;
	N = L->back;
	if ( getLength(L) > 1 ) {
		L->back = L->back->prev;
		L->back->next = NULL;
	}else {
		L->front = L->back = L->curr = NULL;
	}
	--L->length;
	freeNode(&N);
}

/*
 *  deleteCurrent()
 *  Deletes element at curr in L.
 *  Pre: !isEmpty(L); !offEnd(L)
 */
void deleteCurrent(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling deleteCurrent() on NULL ListRef\n");
		exit(1);
	}
	if ( isEmpty(L) ) {
		printf("List Error: calling deleteCurrent() on an empty List\n");
		exit(1);
	}
	if ( offEnd(L) ) {
		printf("List Error: calling deleteCurrent() on a NULL pointer\n");
		exit(1);
	}
	
	NodeRef N = NULL;
	N = L->curr;
	if ( getLength(L) > 1 ) {
		if ( L->curr->prev == NULL ) {
			L->front = L->curr->next;
			L->curr->next = NULL;
		}else if ( L->curr->next == NULL ) {
			L->back = L->curr->prev;
			L->curr->prev = NULL;
		}else {
			L->curr->prev->next = L->curr->next;
			L->curr->next->prev = L->curr->prev;
			L->curr->prev = L->curr->next = NULL;
		}
		L->curr = NULL;
	}else {
		L->front = L->back = L->curr = NULL;
	}
	--L->length;
	freeNode(&N);
}


/*************** Other Functions **********************************************/
/*
 *  printList()
 *  Prints data elements in L on a single line to specified output file.
 */
void printList(FILE* out, ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling printList() on NULL ListRef\n");
		exit(1);
	}

	NodeRef N = NULL;
	for ( N = L->front; N != NULL; N = N->next ) {
		fprintf(out, "%d ", N->data);
	}
	fprintf(out, "\n");
}

/*
 *  copyList()
 *  Returns a ListRef to a new list equivalent to L.
 */
ListRef copyList(ListRef L) {
	if ( L == NULL ) {
		printf("List Error: calling copyList() on NULL ListRef\n");
		exit(1);
	}
	
	ListRef copy = newList();
	NodeRef N = L->front;
	while ( N != NULL) {
		insertAfterBack(copy, N->data);
		N = N->next;
	}
	return (copy);
}

/*
 *	catList()
 *	Returns a new List that is the concatenation of List A followed by List B.
 *	The current marker in the new list is undefined regardless of the states of
 *	the current markers in the two lists.  The states of the two Lists are
 *	unchanged.
 */
ListRef catList(ListRef A, ListRef B) {
	if ( A == NULL || B == NULL) {
		printf("List Error: calling catList() on NULL ListRef\n");
		exit(1);
	}

	ListRef mainList = copyList(A);
    ListRef catList = copyList(B);
    mainList->back->next = catList->front;
    catList->front->prev = mainList->back;
    mainList->back = catList->back;
    mainList->length += catList->length;
	catList = NULL;
    return (mainList);
}
