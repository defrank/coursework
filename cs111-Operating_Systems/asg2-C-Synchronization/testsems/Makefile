# $Id: Makefile,v 1.1 2012-05-12 18:48:09-07 dmfrank - $
# Names: David S. Zou, Derek M. Frank, Harrison Vuong 
# CS111 Spring 2012 - Ethan Miller (Operating Systems)
# Makefile for Programming Project #2's Alien Program

CSOURCE1 = testinit.c
CSOURCE2 = testval.c
CSOURCE3 = testup.c
CSOURCE4 = testdown.c
CSOURCE5 = testfree.c
CSOURCES = ${CSOURCE1} ${CSOURCE2} ${CSOURCE3} ${CSOURCE4} ${CSOURCE5}
TARGET1  = init
TARGET2  = val
TARGET3  = up
TARGET4  = down
TARGET5  = free
EXECBIN  = init val free up down
CC = gcc

all :
	${CC} -o ${TARGET1} ${CSOURCE1}
	${CC} -o ${TARGET2} ${CSOURCE2}
	${CC} -o ${TARGET3} ${CSOURCE3}
	${CC} -o ${TARGET4} ${CSOURCE4}
	${CC} -o ${TARGET5} ${CSOURCE5}

clean :
	- rm -rf ${EXECBIN}

spotless : clean
	- rm ${EXECBIN}

