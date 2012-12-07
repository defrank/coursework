#$Id: Makefile,v 1.3 2009-10-29 23:12:50-07 - - $
# Derek Frank, dmfrank@ucsc.edu

GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
CCOPT     = -D__EXTENSIONS__ -Xc -v -g -xO0
LINTOPT   = -D__EXTENSIONS__ -Xa -fd -m -u -x -errchk=%all,no%longptr64

CSOURCE   = bcat.c
EXECBIN   = bcat
SOURCES   = ${CSOURCE} Makefile README
LISTING   = ../lab4c-stdio.bcat.ps
SUBMITDIR = cmps012b-wm.f09 lab4

all : ${EXECBIN}

${EXECBIN} : ${CSOURCE}
	${GCC} ${CSOURCE} -o ${EXECBIN}
#cc ${CCOPT} ${CSOURCE} -o ${EXECBIN}

lint : ${CSOURCE}
	checksource ${SOURCES}
	lint ${LINTOPT} ${CSOURCE}

ci : ${SOURCES}
	cid + ${SOURCES}

clean :
	- rm core

spotless : clean
	- rm ${EXECBIN}

lis : ${SOURCES}
	mkpspdf ${LISTING} ${SOURCES}

submit : ${SOURCES}
	submit ${SUBMITDIR} ${SOURCES}
	- testsubmit ${SUBMITDIR} ${SOURCES}
