/*******************************************************************************
 *  $Id: GraphTest.c,v 1.3 2010-11-22 16:31:39-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  GraphTest.c
 *  
 *	DESCRIPTION
 *	  A class to run and test the Graph.c ADT.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "Graph.h"

/* Print to file the stack list of strongly connected components */
void printCC(FILE* out, GraphRef G, ListRef S){
	fprintf(out,"\n");
	fprintf(out, "Stack of G's %d strongly connected components:\n", getCC(G));
	printList(out, S);
	fprintf(out,"\n");
}

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
	
	/* Create graph on n vertices */
	int n = 8;
	GraphRef G = newGraph(n);
	ListRef L = newList();
	
	/* Initialize stack list to length n */
	int i;
	for ( i = 1; i <= getOrder(G); ++i ) {
		insertAfterBack(L, i);
	}
	
	/* Add directed edges to graph and print */
	addArc(G, 1, 2);
	addArc(G, 2, 3);
	addArc(G, 2, 5);
	addArc(G, 2, 6);
	addArc(G, 3, 4);
	addArc(G, 3, 7);
	addArc(G, 4, 3);
	addArc(G, 4, 8);
	addArc(G, 5, 1);
	addArc(G, 5, 6);
	addArc(G, 6, 7);
	addArc(G, 7, 6);
	addArc(G, 7, 8);
	addArc(G, 8, 8);
	printGraph(out, G);
	
	/* Test and print DFS(), transpose(), and copyGraph() */
	DFS(G, L);
	fprintf(out, "\nCopy:\n");
	GraphRef C = copyGraph(G);
	printGraph(out, C);
	fprintf(out, "\nTranspose:\n");
	GraphRef T = transpose(G);
	printGraph(out, T);
	DFS(T, L);
	
	/* Print the stack list of strongly connected components */
	printCC(out, T, L);
	
	/* Free memory */
	freeList(&L);
	freeGraph(&G);
	freeGraph(&C);
	freeGraph(&T);
	
	/* close files */
	fclose(out);
	
	return 0;
}
