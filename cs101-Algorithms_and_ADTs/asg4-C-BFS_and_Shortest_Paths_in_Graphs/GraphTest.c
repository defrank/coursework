/*******************************************************************************
 *  $Id: GraphTest.c,v 1.5 2010-11-14 19:42:28-08 dmfrank - $
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

void printPath(FILE* out, GraphRef G, ListRef path, int dest){
	fprintf(out,"\n");
	fprintf(out, "The distance from %d to %d is ", getSource(G), dest);
	if ( getFront(path) == NIL ) {
		fprintf(out, "infinity\n");
		fprintf(out, "No %d-%d path exists", getSource(G), dest);
	}else {
		fprintf(out, "%d\n", getDist(G, dest));
		fprintf(out, "A shortest %d-%d path is: ", getSource(G), dest);
		printList(out, path);
	}
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
	
	int n = 7;
	int dest;
	GraphRef G = newGraph(n);
	ListRef path = newList();
	
	addEdge(G, 1, 4);
	addEdge(G, 1, 5);
	addEdge(G, 4, 5);
	addEdge(G, 2, 3);
	addEdge(G, 2, 6);
	addEdge(G, 3, 7);
	addEdge(G, 6, 7);
	
	printGraph(out, G);
	
	BFS(G, 2);
	dest = 7;
	getPath(path, G, dest);
	printPath(out, G, path, dest);
	
	
	BFS(G, 3);
	makeEmpty(path);
	dest = 6;
	getPath(path, G, dest);
	printPath(out, G, path, dest);
	
	BFS(G, 1);
	makeEmpty(path);
	dest = 7;
	getPath(path, G, dest);
	printPath(out, G, path, dest);
	
	freeList(&path);
	freeGraph(&G);
	
	/* close files */
	fclose(out);
	
	return 0;
}
