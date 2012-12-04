# $Id: Makefile,v 1.1 2010-11-14 15:34:14-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Makefile for Programming Assignment 4

FindPath: FindPath.o Graph.o List.o
	gcc -o FindPath FindPath.o Graph.o List.o

FindPath.o: FindPath.c Graph.h List.h
	gcc -c -ansi -Wall FindPath.c

GraphTest: GraphTest.o Graph.o List.o
	gcc -o GraphTest GraphTest.o Graph.o List.o

GraphTest.o : GraphTest.c Graph.h List.h
	gcc -c -ansi -Wall GraphTest.c

Graph.o : Graph.c Graph.h List.h
	gcc -c -ansi -Wall Graph.c

ListTest: ListTest.o List.o
	gcc -o ListTest ListTest.o List.o

ListTest.o : ListTest.c List.h
	gcc -c -ansi -Wall ListTest.c

List.o : List.c List.h
	gcc -c -ansi -Wall List.c

clean :
	rm -f FindPath GraphTest ListTest FindPath.o \
	      GraphTest.o ListTest.o Graph.o List.o
