# $Id: Makefile,v 1.9 2011-01-19 10:47:32-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 1

MKFILE      = Makefile
DEPSFILE    = ${MKFILE}.deps
NOINCL      = ci clean spotless
NEEDINCL    = ${filter ${NOINCL}, ${MAKECMDGOALS}}
GMAKE       = ${MAKE} --no-print-directory
UNAME      ?= ${shell uname -s}

ifeq (${UNAME},SunOS)
COMPILECCC  = CC -g -features=extensions
MAKEDEPSCCC = CC -xM1
endif
ifeq (${UNAME},Linux)
COMPILECCC  = g++ -g -Wall -Wextra -Werror
MAKEDEPSCCC = g++ -MM
endif
ifeq (${UNAME},Darwin)
COMPILECCC  = g++ -g -Wall -Wextra -Werror
MAKEDEPSCCC = g++ -MM
endif

CCSOURCE    = commands.cpp inode.cpp trace.cpp util.cpp yshell.cpp
CCHEADER    = commands.h  inode.h  trace.h  util.h
EXECBIN     = yshell
OBJECTS     = ${CCSOURCE:.cpp=.o}
OTHERS      = ${MKFILE} README
ALLSOURCES  = ${CCHEADER} ${CCSOURCE} ${OTHERS}
LISTING     = ../asg1-shell.code.ps
CLASS       = cmps109-wm.w11
PROJECT     = asg1

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	${COMPILECCC} -o $@ ${OBJECTS}
	- checksource ${CCSOURCE}

%.o : %.cpp
#cil + $<
	${COMPILECCC} -c $<

ci : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	ci + ${ALLSOURCES}

lis : ${ALLSOURCES}
	mkpspdf ${LISTING} ${ALLSOURCES} ${DEPSFILE}

clean :
	- rm ${OBJECTS} ${DEPSFILE} core ${EXECBIN}.errs

spotless : clean
	- rm ${EXECBIN} 

submit : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	submit ${CLASS} ${PROJECT} ${ALLSOURCES}
	testsubmit ${CLASS} ${PROJECT} ${ALLSOURCES}

deps : ${CCSOURCE} ${CCHEADER}
	@ echo "# ${DEPSFILE} created `LC_TIME=C date`" >${DEPSFILE}
	${MAKEDEPSCCC} ${CCSOURCE} | sort | uniq >>${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${MAKE} deps

again :
	gmake --no-print-directory spotless deps ci all lis

ifeq (${NEEDINCL}, )
include ${DEPSFILE}
endif

