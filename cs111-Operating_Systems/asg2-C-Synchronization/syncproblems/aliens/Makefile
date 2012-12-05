# $Id: Makefile,v 1.1 2012-05-12 18:06:03-07 dmfrank - $
# Names: David S. Zou, Derek M. Frank, Harrison Vuong 
# CS111 Spring 2012 - Ethan Miller (Operating Systems)
# Makefile for Programming Project #2's Alien Program

CSOURCE1 = alien.c
CSOURCE2 = freealiens.c
CSOURCE3 = initaliens.c
CSOURCES = ${CSOURCE1} ${CSOURCE2} ${CSOURCE3}
TARGET1  = alien
TARGET2  = freealiens
TARGET3  = initaliens
EXECBIN  = alien freealiens initaliens
CC = gcc

all :
	${CC} -o ${TARGET1} ${CSOURCE1}
	${CC} -o ${TARGET2} ${CSOURCE2}
	${CC} -o ${TARGET3} ${CSOURCE3}

clean :
	- rm -rf ${EXECBIN}

spotless : clean
	- rm ${EXECBIN}

