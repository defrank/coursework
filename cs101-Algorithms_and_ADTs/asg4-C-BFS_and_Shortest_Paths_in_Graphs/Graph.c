/*******************************************************************************
 *	$Id: Graph.c,v 1.3 2010-11-14 19:30:00-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  Graph.c
 *  
 *	DESCRIPTION
 *	  Implementation file for the Graph ADT.  Contains a Graph on n vertices.
 *	Uses the List ADT to keep track of adjacent vertices.  Includes a Breadth
 *	First Search algorithm that will find the shortest paths tree given a
 *	source.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "Graph.h"

/************** Private Structs: not exported *********************************/

typedef struct Graph {
	ListRef* adj; /* array of Lists of ints corresponding to vertice number */
	int* color; /* array of color values corresponding to vertice number */
	int* d; /* array of distance values corresponding to vertice number */
	int* P; /*array of parent values corresponding to vertice number */
	int order; /* number of vertices */
	int size; /* number of edges */
	int source; /* vertex most recently used as source for BFS */
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
	G->P = calloc(n+1, sizeof(int));
	G->order = n;
	G->size = 0;
	G->source = NIL;

	int i;
	for ( i = 1; i <= n; ++i ) {
		G->adj[i] = newList();
		G->color[i] = WHITE;
		G->d[i] = INF;
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
	free((*pG)->P);
	free(*pG);
	*pG = NULL;
}

/*************** Access Functions *********************************************/

/*	getOrder()
 *	Returns field value order.
 */
int getOrder(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getOrder() on NULL GraphRef\n");
		exit(1);
	}
	
	return G->order;
}

/*	getSize()
 *	Returns field value size.
 */
int getSize(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getSize() on NULL GraphRef\n");
		exit(1);
	}
	
	return G->size;
}

/*	getSource()
 *	Returns the source vertex most recently used in the function BFS(), or NIL
 *	if BFS() has not yet been called.
 */
int getSource(GraphRef G) {
	if ( G == NULL ) {
		printf("Graph Error: calling getSource() on NULL GraphRef\n");
		exit(1);
	}
	
	return G->source;
}

/*	getParent()
 *	Return the parent of vertex u in the Breadth-First Tree created by BFS(), or
 *	NIL if BFS() has not yet been called.
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
	
	return G->P[u];
}

/*	getDist()
 *	Returns the distance from the most recent BFS source to vertex u, or INF if
 *	BFS() has not yet been called.
 *	Pre: 1 <= u <= getOrder(G)
 */
int getDist(GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getDist() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getDist() on out of index array d\n");
		exit(1);
	}
	
	return G->d[u];
}

/*	getPath()
 *	Appends to the List L the vertices of a shortest path in G from the source
 *	to u, or appends to L the value NIL if no such path exists.
 *	Pre: 1 <= u <= getOrder(G); getSource(G) != NIL
 */
void getPath(ListRef L, GraphRef G, int u) {
	if ( G == NULL ) {
		printf("Graph Error: calling getPath() on NULL GraphRef\n");
		exit(1);
	}
	if ( L == NULL ) {
		printf("Graph Error: calling getPath() on NULL ListRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling getPath() on out of index array d\n");
		exit(1);
	}
	if ( getSource(G) == NIL ) {
		printf("Graph Error: calling getpath() on NIL source\n");
		exit(1);
	}

	if ( u == getSource(G) ) { insertAfterBack(L, u); }
	else if ( getParent(G, u) == NIL ) { insertAfterBack(L, NIL); }
	else {
		insertBeforeFront(L, u);
		int x = getParent(G, u);
		while ( x != getSource(G) && x != NIL ) {
			insertBeforeFront(L, x);
			x = getParent(G, x);
		}
		if ( x == NIL ) { insertBeforeFront(L, NIL); }
		else { insertBeforeFront(L, x); }
	}
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
		G->d[i] = NIL;
		G->P[i] = NIL;
	}
	G->size = 0;
	G->source = NIL;
}

/*	addEdge()
 *	Inserts a new edge joining u to v, i.e. u is added to the adjacency List of
 *	v, and v to the adjacency List of u.
 *	Pre: 1 <= u <= getOrder(G); 1 <= v <= getOrder(G)
 */
void addEdge(GraphRef G, int u, int v) {
	if ( G == NULL ) {
		printf("Graph Error: calling addEdge() on NULL GraphRef\n");
		exit(1);
	}
	if ( u < 1 || u > getOrder(G) ) {
		printf("Graph Error: calling addEdge() on u non-existent within bounds of order\n");
		exit(1);
	}
	if ( v < 1 || v > getOrder(G) ) {
		printf("Graph Error: calling addEdge() on v non-existent within bounds of order\n");
		exit(1);
	}
	
	insertAfterBack(G->adj[u], v);
	insertAfterBack(G->adj[v], u);
	++G->size;
}

/*	addArc()
 *	Inserts a new directed edge from u to v, i.e. v is added to the adjacency List of
 *	u (but not u to the adjacency List of v).
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
	++G->size;
}

/*	BFS()
 *	Runs the Breadth First Search algorithm on the Graph G with source s,
 *	setting the color, distance, and paret fields of G accordingly.
 *	Pre: 1 <= s <= getOrder(G)
 */
void BFS(GraphRef G, int s) {
	if ( G == NULL ) {
		printf("Graph Error: calling BFS() on NULL GraphRef\n");
		exit(1);
	}
	if ( s < 1 || s > getOrder(G) ) {
		printf("Graph Error: calling BFS() on s non-existent within bounds of order\n");
		exit(1);
	}
	
	G->source = s;
	int i, x, y;
	for ( i = 1; i <= getOrder(G); ++i) {
		G->color[i] = WHITE;
		G->d[i] = INF;
		G->P[i] = NIL;
	}
	G->color[s] = GREY;
	G->d[s] = 0;
	G->P[s] = NIL;
	ListRef L = newList();
	insertAfterBack(L, s);
	while ( !isEmpty(L) ) {
		x = getFront(L);
		deleteFront(L);
		if ( !isEmpty(G->adj[x]) ) moveFront(G->adj[x]);
		while ( !offEnd(G->adj[x]) ) {
			y = getCurrent(G->adj[x]);
			if ( G->color[y] == WHITE ) {
				G->color[y] = GREY;
				G->d[y] = (getDist(G, x) + 1);
				G->P[y] = x;
				insertAfterBack(L, y);
			}
			moveNext(G->adj[x]);
		}
		G->color[x] = BLACK;
	}
	freeList(&L);
}

/***************** Other Operations *******************************************/

/*	printGraph()
 *	Prints the adjacency list representation of G to the file pointed to by out.
 */
void printGraph(FILE* out, GraphRef G){
	if ( G == NULL ) {
		printf("Graph Error: calling printGraph() on NULL GraphRef\n");
		exit(1);
	}
	
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
