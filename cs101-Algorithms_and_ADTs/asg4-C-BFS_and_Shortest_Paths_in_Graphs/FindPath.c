/*******************************************************************************
 *  $Id: FindPath.c,v 1.10 2010-11-14 19:42:28-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  FindPath.c
 *  
 *	DESCRIPTION
 *	  The main class of the program FindPath that implements both the Graph and
 *	List ADTs.  First reads an input file containing a graph on n vertices with
 *	specified edges.  It then prints the graph's adjacency list.  Next it goes
 *	on a loop to read in a source and destination from the input file, then uses
 *	the Breadth First Search algorithm to make a tree of shortest paths given
 *	the source from the input file and prints out the distance and shortest path
 *	from the source to the given destination.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Graph.h"

#define MAX_LEN 250

/* boolean type definition */
typedef enum {FALSE = 0, TRUE = 1} bool;

void printDestandPath(FILE* out, GraphRef G, ListRef path, int dest) {
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
	bool flag = FALSE;
	int  n, x, y, source, dest;
	GraphRef G;
	ListRef path = newList();
	
	/* read and store the graph and print out its adjacency list */
	while( !flag && fgets(line, MAX_LEN, in) != NULL ) {
		++linenum;
		token = strtok(line, " \n");
		if( linenum == 1 ) {
			n = atoi(token);
			G = newGraph(n);
		}else {
			x = atoi(token);
			token = strtok(NULL, " \n");
			y = atoi(token);
			if( x != 0 ) {
				addEdge(G, x, y);
			}
			else { flag = TRUE; }
		}
	}
	printGraph(out, G);
	fprintf(out, "\n");
	
	/* read in pair of vertices, run BFS on source vertex, print the distance
	 * to the destination vertex, then find and print the resulting shortest
	 * path, it if exists, or print a message if no path from source to
	 * destination exists */
	while( fgets(line, MAX_LEN, in) != NULL ) {
		token = strtok(line, " \n");
		source = atoi(token);
		token = strtok(NULL, " \n");
		dest = atoi(token);
		if( source != NIL ){
			BFS(G, source);
			makeEmpty(path);
			getPath(path, G, dest);
			printDestandPath(out, G, path, dest);
		}
	}
	
	/* free memory */
	freeList(&path);
	freeGraph(&G);
	
	/* close files */
	fclose(in);
	fclose(out);

	return(0);
}
