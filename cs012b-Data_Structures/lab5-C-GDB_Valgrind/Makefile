# $Id: Makefile,v 1.2 2009-11-04 00:34:33-08 - - $
# Derek Frank, dmfrank@ucsc.edu

GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
CCOPT   = -D__EXTENSIONS__ -Xc -v -g -xO0
LINTOPT = -D__EXTENSIONS__ -Xa -fd -m -u -x -errchk=%all,no%longptr64

CSOURCE = list1.c
EXECBIN = list1
SOURCES = ${CSOURCE} Makefile
LISTING = ../lab5c-dbx-bcheck.ps

all : ${EXECBIN}

${EXECBIN} : ${CSOURCE}
	${GCC} ${CSOURCE} -o ${EXECBIN}
#cc ${CCOPT} ${CSOURCE} -o ${EXECBIN}

lint : ${CSOURCE}
	checksource ${SOURCES}
	lint ${LINTOPT} ${CSOURCE}

