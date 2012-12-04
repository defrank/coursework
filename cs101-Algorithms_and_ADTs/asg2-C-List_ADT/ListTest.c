/* $Id: ListTest.c,v 1.2 2010-10-17 08:48:47-07 - - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   ListTest -- used to test List.c
 * 
 * DESCRIPTION
 *   Tests List.c to check if it functions properly and according to specifications.
 */

#include <stdio.h>
#include <stdlib.h>
#include "List.h"

int main (int argc, const char * argv[]) {
	ListRef A = newList();
	ListRef B = newList();
	int i;
	
	for ( i=1; i<=10; ++i) {
		insertAfterBack(A, i);
		insertAfterBack(B, 11-i);
	}
	printf("A = ");
	printList(A);
	printf("\nB = ");
	printList(B);
	
	for( i=1; i<=6; ++i ) {
		insertAfterBack(A, getFront(B));
		deleteFront(B);
	}
	printf("\nA = ");
	printList(A);
	printf("\nB = ");
	printList(B);
	ListRef C = copyList(A);
	printf("\nC = ");
	printList(C);
	
	if ( equals(A,B) ) { printf("\nA equals B"); }
	else { printf("\nA does not equal B"); }
	if ( equals(A,C) ) { printf("\nA equals C"); }
	else { printf("\nA does not equal C"); }
	
	ListRef D = catList(A,B);
	printf("\nD = ");
	printList(D);
	if ( equals(A,D) ) {
		printf("\nA equals D");
	}else {
		printf("\nA does not equal D");
	}
	if ( equals(D,A) ) { printf("\nD equals A"); }
	else { printf("\nD does not equal A"); }
	
	int lengthCompare = ( getLength(A) == getLength(D) );
	printf("\nLength A = %d and Length D = %d and Length C = %d", getLength(A), getLength(D),getLength(C));
	printf("\nLength Compare = %d\n",lengthCompare);
	
	makeEmpty(D);
	printList(D);
	freeList(&D);
	printList(D);
	return (0);
}
