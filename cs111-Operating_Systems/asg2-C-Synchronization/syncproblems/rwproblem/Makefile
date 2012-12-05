# Makefile for Programming Project #2's Reader/Writer Program
# Names: David S. Zou, Derek M. Frank, Harrison Vuong 
# CS111 Spring 2012 - Ethan Miller (Operating Systems)
#
# A Makefile for the readers/writers problem.

CSOURCE1 = rwinit.c
CSOURCE2 = reader.c
CSOURCE3 = writer.c
CSOURCE4 = rwfree.c
CSOURCES = ${CSOURCE1} ${CSOURCE2} ${CSOURCE3} ${CSOURCE4} 
TARGET1  = initrw
TARGET2  = reader
TARGET3  = writer
TARGET4  = freerw
EXECBIN  = initrw reader writer freerw
CC = gcc

all :	
	${CC} -o ${TARGET1} ${CSOURCE1} 
	${CC} -o ${TARGET2} ${CSOURCE2} 
	${CC} -o ${TARGET3} ${CSOURCE3}
	${CC} -o ${TARGET4} ${CSOURCE4} 

clean :
	- rm -rf ${EXECBIN}

spotless : clean
	- rm ${EXECBIN}

