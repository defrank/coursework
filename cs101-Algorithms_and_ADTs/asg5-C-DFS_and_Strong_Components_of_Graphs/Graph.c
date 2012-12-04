/*******************************************************************************
 *	$Id: Graph.c,v 1.5 2010-11-22 18:35:36-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *
 *	NAME
 *	  Graph.c
 *
 *	DESCRIPTION
 *	  Implementation file for the Graph ADT.  Contains a Directed Graph on n
 *	vertices.  Uses the List ADT to keep track of adjacent vertices.  Includes a
 *	Depth First Search algorithm used to find the strongly connected components
 *	of a Directed Graph.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "Graph.h"


/************** Private Structs: not exported *********************************/

typedef struct Graph {
	ListRef* adj; /* array of Lists of ints corresponding to vertice number */
	int* color; /* array of color values corresponding to vertice number */
	int* d; /* array of DFS discover values corresponding to vertice number */
	int* f; /* array of DFS finsih values corresponding to vertice number */
	int* P; /*array of parent values corresponding to vertice number */
	int order; /* number of vertices */
	int size; /* number of edges */
	int cc; /* number of connected components found after running DFS */
} Graph;


/************** Constructors-Destructors **************************************/

/*	newGraph()
 *	Returns GraphRef pointing to new Graph Struct which represents a Graph
 *	having n vertices and no edges.  Initializes arrays adj, color, d, and P to
 *	be size n+1.  Sets order to n, size to NULL, and source to NIL.
 *	Exported.
 */
GraphRef newGraph(int n) {
	GraphRef G = malloc(sizeof(Graph));

	G->adj = calloc(n+1, sizeof(ListRef));
	G->color = calloc(n+1, sizeof(int));
	G->d = calloc(n+1, sizeof(int));
	G->f = calloc(n+1, sizeof(int));
	G->P = calloc(n+1, sizeof(int));
	G->order = n;
	G->size = 0;
	G->cc = 0;

	int i;
	for ( i = 1; i <= n; ++i ) {
		G->adj[i] = newList();
		G->color[i] = WHITE;
		G->d[i] = UNDEF;
		G->f[i] = UNDEF;
		G->P[i] = NIL;
	}
	return (G);
}

/*	freeGraph()
 *  Frees all dynamic memory associated with the GraphRef *pG, then sets *pG to
 *	NULL.
 *	Exported.
 */
void freeGraph(GraphRef* pG) {
	if ( pG == NULL || (*pG) == NULL ) {
		return;
	}

	int i;
	for ( i = 1; i <= getOrder(*pG); ++i ) {
		freeList(&((*pG)->adj[i]));
	}
	free((*pG)->adj);
	free((*pG)->color);
	free((*pG)->d);
	free((*pG)->f);
	free((*pG)->P);
	free(*pG);
	*pG = NULL;
}


/*************** Access Functions *********************************************/

/*	getOrder()
 *	Returns field value order, i.e., number of vertices.
 */
int getOrder(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getOrder() on NULL GraphRef\n");
		exit(1);
	}

	return (G->order);
}

/*	getSize()
 *	Returns field value size, i.e., number of edges.
 */
int getSize(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getSize() on NULL GraphRef\n");
		exit(1);
	}

	return (G->size);
}

/*	getCC()
 *	Returns the DFS found number of strongly connected components.
 */
int getCC(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getFinish() on NULL GraphRef\n");
		exit(1);
	}

	return (G->cc);
}

/*	getColor()
 *	Returns the current color of vertex u in the Depth-First Tree created by
 *	DFS().
 *	Pre: 1 <= u <= getOrder(G)
 */
int getColor(GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getParent() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getParent() on out of index array P\n");
		exit(1);
	}

	return (G->color[u]);
}

/*	getParent()
 *	Return the parent of vertex u in the Depth-First Tree created by DFS(), or
 *	NIL if DFS() has not yet been called.
 *	Pre: 1 <= u <= getOrder(G)
 */
int getParent(GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getParent() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getParent() on out of index array P\n");
		exit(1);
	}

	return (G->P[u]);
}

/*	getDiscover()
 *	Returns the DFS discover time of the vertex u, or UNDEF if DFS() has not yet
 *	been called.
 *	Pre: 1 <= u <= getOrder(G)
 */
int getDiscover(GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getDiscover() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getDiscover() on out of index array d\n");
		exit(1);
	}

	return (G->d[u]);
}

/*	getFinish()
 *	Returns the DFS finish time of the vertex u, or UNDEF if DFS() has not yet
 *	been called.
 *	Pre: 1 <= u <= getOrder(G)
 */
int getFinish(GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getFinish() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getFinish() on out of index array d\n");
		exit(1);
	}

	return (G->f[u]);
}


/************** Manipulation Procedures ***************************************/

/*	makeNull()
 *	Deletes all edges of G, restoring it to its original state, i.e. a NULL
 *	Graph on n vetices.
 */
void makeNull(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling makeNull() on NULL GraphRef\n");
		exit(1);
	}

	int i;
	for ( i = 1; i <= getOrder(G); ++i ) {
		makeEmpty(G->adj[i]); /* Empty the adjacency lists */
		G->color[i] = NIL;
		G->d[i] = UNDEF;
		G->f[i] = UNDEF;
		G->P[i] = NIL;
	}
	G->size = 0;
	G->cc = 0;
}

/*	addArc()
 *	Inserts a new directed edge from u to v, i.e. v is added to the adjacency
 *	List of u (but not u to the adjacency List of v).
 *	Pre: 1 <= u <= getOrder(G); 1 <= v <= getOrder(G)
 */
void addArc(GraphRef G, int u, int v) {
	if ( G == NULL ) {
		printf("Graph Error: calling addArc() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling addArc() on u non-existent within bounds of order\n");
		exit(1);
	}
	if ( v < 1 || v > getOrder(G) ) {
		printf("Graph Error: calling addArc() on v non-existent within bounds of order\n");
		exit(1);
	}

	insertAfterBack(G->adj[u], v);
	++(G->size);
}

/*	Visit()
 *	Recursive function paired with DFS().  Visits a vertex and all of its
 *	descendants.
 *	Not exported.
 */
void Visit(GraphRef G, ListRef L, int x, int* time){
	G->color[x] = GRAY;
	G->d[x] = (++(*time));
	if ( !isEmpty(G->adj[x]) ) { moveFront(G->adj[x]); }
	int y;
	while ( !offEnd(G->adj[x]) ) {
		y = getCurrent(G->adj[x]);
		if ( G->color[y] == WHITE ) {
			G->P[y] = x;
			Visit(G, L, y, &(*time));
		}
		moveNext(G->adj[x]);

	}
	G->color[x] = BLACK;
	G->f[x] = (++(*time));
	insertBeforeFront(L, x);
}

/*	DFS()
 *	Runs the depth first search algorithm on the Graph G.  The List S has two
 *	purposes in this function.  First, it defines the order in which vertices
 *	will be processed in the main loop of DFS.  Second, when DFS is complete, it
 *	will store the vertices in order of decreasing finish times.  The List S can
 *	therefore be classified as both input and an output parameter to function
 *	DFS().
 *	Pre: getOrder(G) = getLength(S)
 */
void DFS(GraphRef G, ListRef S) {
	if ( G == NULL ) {
		printf("Graph Error: calling DFS() on NULL GraphRef\n");
		exit(1);
	}
	if ( getOrder(G) != getLength(S) ) {
		printf("Graph Error: calling DFS() on unequal sized order and length\n");
		exit(1);
	}

	int i, x, time;
	ListRef L = newList();
	for ( i = 1; i <= getOrder(G); ++i) {
		G->color[i] = WHITE;
		G->d[i] = UNDEF;
		G->f[i] = UNDEF;
		G->P[i] = NIL;
	}
	time = 0;
	G->cc = 0;
	if ( !isEmpty(S) ) { moveFront(S); }
	while ( !offEnd(S) ) {
		x = getCurrent(S);
		if ( G->color[x] == WHITE ) {
			Visit(G, L, x, &time);
			++(G->cc);
		}
		moveNext(S);
	}

	makeEmpty(S);
	if ( !isEmpty(L) ) { moveFront(L); }
	while ( !offEnd(L) ) {
		insertAfterBack(S, getCurrent(L));
		moveNext(L);
	}
	freeList(&L);
}


/***************** Other Operations *******************************************/

/*	transpose()
 *	Returns a handle to a new Graph object representing the transpose of G.
 */
GraphRef transpose(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling transpose() on NULL GraphRef\n");
		exit(1);
	}

	GraphRef T = newGraph(getOrder(G));
	int i, x;
	for ( i = 1; i <= getOrder(G); ++i) {
		if ( !isEmpty(G->adj[i]) ) { moveFront(G->adj[i]); }
		while ( !offEnd(G->adj[i]) ) {
			x = getCurrent(G->adj[i]);
			addArc(T, x, i);
			moveNext(G->adj[i]);
		}
	}
	return (T);
}

/*	copyGraph()
 *	Returns a handle to a new Graph which is a copy of G.
 */
GraphRef copyGraph(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling copyGraph() on NULL GraphRef\n");
		exit(1);
	}

	GraphRef N = newGraph(getOrder(G));
	int i;
	for ( i = 1; i <= getOrder(N); ++i ) {
		if ( !isEmpty(G->adj[i]) ) { moveFront(G->adj[i]); }
		while ( !offEnd(G->adj[i]) ) {
			insertAfterBack(N->adj[i], getCurrent(G->adj[i]));
			moveNext(G->adj[i]);
		}
		N->color[i] = getColor(G, i);
		N->d[i] = getDiscover(G, i);
		N->f[i] = getFinish(G, i);
		N->P[i] = getParent(G, i);
	}
	N->size = getSize(G);
	N->cc = getCC(G);
	return (N);
}

/*	printGraph()
 *	Prints the adjacency list representation of G to the file pointed to by out.
 */
void printGraph(FILE* out, GraphRef G){
	if ( G == NULL ) {
		printf("Graph Error: calling printGraph() on NULL GraphRef\n");
		exit(1);
	}

	fprintf(out, "Adjacency list representation of G:\n");
	int i, temp;
	for ( i = 1; i <= getOrder(G); ++i ) {
		fprintf(out, "%d:", i);
		if ( !isEmpty(G->adj[i]) ) {
			moveFront(G->adj[i]);
			while ( !offEnd(G->adj[i]) ) {
				temp = getCurrent(G->adj[i]);
				moveNext(G->adj[i]);
				fprintf(out, " %d", temp);
			}
		}
		fprintf(out, "\n");
	}
}
