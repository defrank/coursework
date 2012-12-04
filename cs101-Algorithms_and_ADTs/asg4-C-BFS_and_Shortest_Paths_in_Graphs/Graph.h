/*******************************************************************************
 *  $Id: Graph.h,v 1.3 2010-11-14 18:13:56-08 dmfrank - $
 *	Derek Frank, dmfrank@ucsc.edu
 *	
 *	NAME
 *	  Graph.h
 *  
 *	DESCRIPTION
 *	  Header file for the Graph.c ADT.
 ******************************************************************************/

#if !defined(_GRAPH_H_INCLUDE_)
#define _GRAPH_H_INCLUDE_
#define INF -1
#define NIL 0
#define WHITE 0
#define GREY 1
#define BLACK 2

#include "List.h"

/************* Exported Types *************************************************/

typedef struct Graph* GraphRef;


/************** Constructors-Destructors **************************************/

GraphRef newGraph(int n);
void freeGraph(GraphRef* pG);


/*************** Access Functions *********************************************/

int getOrder(GraphRef G);
int getSize(GraphRef G);
int getSource(GraphRef G);
int getParent(GraphRef G, int u);
int getDist(GraphRef G, int u);
void getPath(ListRef L, GraphRef G, int u);


/************** Manipulation Procedures ***************************************/

void makeNull(GraphRef G);
void addEdge(GraphRef G, int u, int v);
void addArc(GraphRef G, int u, int v);
void BFS(GraphRef G, int s);


/***************** Other Operations *******************************************/

void printGraph(FILE* out, GraphRef G);

#endif
