# $Id: Makefile,v 1.3 2010-10-17 10:03:14-07 - - $
# Derek Frank, dmfrank@ucsc.edu
# Makefile for Programming Assignment 2

Shuffle : List.o Shuffle.o
	gcc -o Shuffle Shuffle.o List.o
	
Shuffle.o : List.h Shuffle.c
	gcc -c -ansi -Wall Shuffle.c
	
ListTest : List.o ListTest.o
	gcc -o ListTest ListTest.o List.o
	
ListTest.o : List.h ListTest.c
	gcc -c -anse -Wall ListTest.c

List.o: List.h List.c
	gcc -c -ansi -Wall List.c
	
clean :
	rm -f Shuffle ListTest Shuffle.o ListTest.o List.o
	
Target : Dependency_List
	Operation