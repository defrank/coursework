/*******************************************************************************
 *  $Id: List.h,v 1.1 2010-11-22 13:18:09-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *
 *	NAME
 *	  List.h
 *
 *	DESCRIPTION
 *    Header file for List ADT
 ******************************************************************************/

#if !defined(_LIST_H_INCLUDE_)
#define _LIST_H_INCLUDE_

/***************************** Exported Types *********************************/

typedef struct List * ListRef;


/************** Constructors-Destructors **************************************/

ListRef newList(void);
void freeList(ListRef* pL);


/***************** Access functions *******************************************/

int isEmpty(ListRef L);
int offEnd(ListRef L);
int atFront(ListRef L);
int atBack(ListRef L);
int getFront(ListRef L);
int getBack(ListRef L);
int getCurrent(ListRef L);
int getLength(ListRef L);
int equals(ListRef A, ListRef B);


/****************************** Manipulation procedures ***********************/

void makeEmpty(ListRef L);
void moveFront(ListRef L);
void moveLast(ListRef L);
void movePrev(ListRef L);
void moveNext(ListRef L);
void insertBeforeFront(ListRef L, int data);
void insertAfterBack(ListRef L, int data);
void insertBeforeCurrent(ListRef L, int data);
void insertAfterCurrent(ListRef L, int data);
void deleteFront(ListRef L);
void deleteBack(ListRef L);
void deleteCurrent(ListRef L);


/*************** Other Functions **********************************************/

void printList( FILE* out, ListRef L);
ListRef copyList(ListRef L);
ListRef catList(ListRef A, ListRef B);

#endif
