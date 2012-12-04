/*******************************************************************************
 *  $Id: ListTest.c,v 1.5 2010-11-14 19:42:28-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  ListTest.c
 *  
 *	DESCRIPTION
 *	  A class to run and test the List.c ADT.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "List.h"

int main (int argc, char * argv[]) {
	FILE *out;
	
	/* check command line for correct number of arguments */
	if( argc != 2 ) {
		printf("Usage: %s outfile\n", argv[0]);
		exit(1);
	}
	
	/* open file for writing */
	out = fopen(argv[1], "w");
	if( out==NULL ) {
		printf("Unable to open file %s for writing\n", argv[1]);
		exit(1);
	}
	
	ListRef A = newList();
	ListRef B = newList();
	int i;
	
	for ( i=1; i<=10; ++i) {
		insertAfterBack(A, i);
		insertAfterBack(B, 11-i);
	}
	fprintf(out, "A = ");
	printList(out, A);
	fprintf(out, "\nB = ");
	printList(out, B);
	
	for( i=1; i<=6; ++i ) {
		insertAfterBack(A, getFront(B));
		deleteFront(B);
	}
	fprintf(out, "\nA = ");
	printList(out, A);
	fprintf(out, "\nB = ");
	printList(out, B);
	ListRef C = copyList(A);
	fprintf(out, "\nC = ");
	printList(out, C);
	
	if ( equals(A,B) ) { fprintf(out, "\nA equals B"); }
	else { fprintf(out, "\nA does not equal B"); }
	if ( equals(A,C) ) { fprintf(out, "\nA equals C"); }
	else { fprintf(out, "\nA does not equal C"); }
	
	ListRef D = catList(A,B);
	fprintf(out, "\nD = ");
	printList(out, D);
	if ( equals(A,D) ) {
		fprintf(out, "\nA equals D");
	}else {
		fprintf(out, "\nA does not equal D");
	}
	if ( equals(D,A) ) { fprintf(out, "\nD equals A"); }
	else { fprintf(out, "\nD does not equal A"); }
	
	int lengthCompare = ( getLength(A) == getLength(D) );
	fprintf(out, "\nLength A = %d and Length D = %d and Length C = %d",
			getLength(A),getLength(D),getLength(C));
	fprintf(out, "\nLength Compare = %d\n",lengthCompare);
	
	makeEmpty(D);
	printList(out, D);
	freeList(&D);
	printList(out, D);
	
	/* close files */
	fclose(out);
	
	return (0);
}
