/*	$Id: Shuffle.c,v 1.7 2010-11-11 19:35:04-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *
 *	NAME
 *		Shuffle.c
 *
 *	DESCRIPTION
 *		Reads permutations line by line from a file and shuffles a list
 *	the length of the permutation according to the operators found in
 *	the permutations.  The list is shuffled until it returns to its
 *	original state and the number of shuffles is counted.  The first
 *	shuffle along with the number of shuffles needed to return the
 *	list to its original state are written to a file specified by the
 *	user line by line with its corresponding permutation.  The first
 *	line read from the file by Shuffle is assumed to be the number of
 *	permutations in the file.	
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "List.h"

#define MAX_LEN 250

void shuffle(ListRef L, ListRef P) {
	int temp, operator, i, j, jLimit;
	int prevOperator = 1;
	moveFront(P);
	moveLast(L);
	
	for ( i=1; i<=getLength(P); ++i ) {
		operator = getCurrent(P);
		temp = getFront(L);
		jLimit = (operator - prevOperator);
		
		/* either move current marker right or left */
		if ( jLimit < 0 ) {
			jLimit = (-1*jLimit);
			for ( j=jLimit; j>0; --j ) {
				movePrev(L);
			}
		}else if ( jLimit > 0 ) {
			for ( j=0; j<jLimit; ++j ) {
				moveNext(L);
			}
		}

		insertAfterCurrent(L, temp);
		moveNext(P); /* move operator current-marker right */
		prevOperator = operator; /* keep track of previous current marker */
		deleteFront(L);
    }
}

int main (int argc, const char * argv[]) {
	int i, n, temp, order, numberOfP;
	int count=0;
	FILE *in, *out;
	char line[MAX_LEN];
	char* token;
	ListRef permutation = newList();
	ListRef list = newList();
	ListRef copy = NULL;
	
	/* check command line for correct number of arguments */
	if( argc != 3 ){
		printf("Usage: %s infile outfile\n", argv[0]);
		exit(1);
	}
	
	/* open files for reading and writing */
	in = fopen(argv[1], "r");
	out = fopen(argv[2], "w");
	if( in==NULL ){
		printf("Unable to open file %s for reading\n", argv[1]);
		exit(1);
	}
	if( out==NULL ){
		printf("Unable to open file %s for writing\n", argv[2]);
		exit(1);
	}
	
	/* read each line of input file, then place tokens in list
	 * and shuffle until the list is back to its original state */
	while( fgets(line, MAX_LEN, in) != NULL)  {
		count++;
		order = 0;
		n = 0;
		token = strtok(line, " \n");
		while( token!=NULL ){
			temp = atoi(token);
			n++;
			insertAfterBack(permutation, temp);
			token = strtok(NULL, " \n");
		}
		if (count == 1) {
			numberOfP = temp;
		}else {
			for ( i=1; i<=getLength(permutation); ++i) {
				insertAfterBack(list, i);
			}

			copy = copyList(list);
			shuffle(list, permutation);
			++order;
			printList(out, list);
			
			while ( !equals(list, copy) ) {
				shuffle(list, permutation);
				++order;
			}
			
			fprintf(out," order= %d\n", order);
			makeEmpty(list);
			freeList(&copy);
			copy = NULL;
		}
		makeEmpty(permutation);
	}
	
	freeList(&permutation);
	freeList(&list);
	
	/* close files */
	fclose(in);
	fclose(out);
	
	return(0);
	
}
