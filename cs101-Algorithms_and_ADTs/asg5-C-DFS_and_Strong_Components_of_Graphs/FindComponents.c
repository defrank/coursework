/*	$Id: FindComponents.c,v 1.9 2010-11-22 18:35:36-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *
 *	NAME
 *	  FindComponents.c
 *
 *	DESCRIPTION
 *	  Program "FindComponents" main.  Implements the Graph and List ADTs. Reads
 *	from a specified file a directed graph and uses the Depth First Search
 *	algorithm to to find the strongly connected components in the given graph.
 *	Prints output to a specified outfile.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Graph.h"

#define MAX_LEN 250

/* Method to print to outfile the strongly connected components */
void printCC(FILE* out, GraphRef G, ListRef S){
	fprintf(out, "G contains %d strongly connected components:\n", getCC(G));
	int i, x;
	if ( !isEmpty(S) ) { moveLast(S); }
	i = 1;
	while ( !offEnd(S) && i <= getCC(G) ) {
		x = getCurrent(S);
		fprintf(out, "Component %d:", i);
		while ( getParent(G, x) != NIL ) {
			fprintf(out, " %d", x);
			movePrev(S);
			x = getCurrent(S);
		}
		fprintf(out, " %d", x);
		movePrev(S);
		++i;
		fprintf(out,"\n");
	}
}

/* Main method */
int main (int argc, char * argv[]) {
	
	/* check command line for correct number of arguments */
	if( argc != 3 ){
		printf("Usage: %s infile outfile\n", argv[0]);
		exit(1);
	}
	
	/* open files for reading and writing */
	FILE *in, *out;
	in = fopen(argv[1], "r");
	out = fopen(argv[2], "w");
	if( in==NULL ) {
		printf("Unable to open file %s for reading\n", argv[1]);
		exit(1);
	}
	if( out==NULL ) {
		printf("Unable to open file %s for writing\n", argv[2]);
		exit(1);
	}
	
	/* declare and initialize variables */
	char line[MAX_LEN];
	char* token;
	int linenum = 0;
	int  n, x, y;
	GraphRef G, T;
	ListRef stack = newList();
	
	/* read and store the directed graph and print out its adjacency list */
	while( fgets(line, MAX_LEN, in) != NULL ) {
		++linenum;
		token = strtok(line, " \n");
		if ( linenum == 1 ) {
			n = atoi(token);
			G = newGraph(n);
		}else {
			x = atoi(token);
			token = strtok(NULL, " \n");
			y = atoi(token);
			if( x != 0 ) {
				addArc(G, x, y);
			}
		}
	}
	printGraph(out, G);
	fprintf(out, "\n");
	
	/* Initialize stack list to a permutation from 1 to n */
	int i;
	for ( i = 1; i <= getOrder(G); ++i ) {
		insertAfterBack(stack, i);
	}
	
	/* Run DFS() on G, altering the stack list */
	DFS(G, stack);
	
	/* Get the transpose of G and run DFS() on it attaining the strongly
	 * connected components within the stack list */
	T = transpose(G);
	DFS(T, stack);
	
	/* Print the strongly connected components of G */
	printCC(out, T, stack);
	
	/* Free memory */
	freeList(&stack);
	freeGraph(&G);
	freeGraph(&T);
	
	/* close files */
	fclose(in);
	fclose(out);
	
	return 0;
}
