# $Id: Makefile,v 1.1 2010-11-22 13:18:09-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Makefile for Programming Assignment 5

FindComponents: FindComponents.o Graph.o List.o
	gcc -o FindComponents FindComponents.o Graph.o List.o

FindComponents.o: FindComponents.c Graph.h List.h
	gcc -c -ansi -Wall FindComponents.c

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
	rm -f FindComponents GraphTest ListTest FindComponents.o \
              GraphTest.o ListTest.o Graph.o List.o
