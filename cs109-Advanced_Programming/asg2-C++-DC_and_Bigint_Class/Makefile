# $Id: Makefile,v 1.2 2011-01-29 15:35:40-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu

MKFILE      = Makefile
DEPSFILE    = ${MKFILE}.deps
NOINCL      = ci clean spotless
NEEDINCL    = ${filter ${NOINCL}, ${MAKECMDGOALS}}
GMAKE       = ${MAKE} --no-print-directory
UNAME      ?= ${shell uname -s}
WHOAMI     ?= ${shell whoami}

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
CICOMMAND   = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif

CCHEADER    = bigint.h iterstack.h scanner.h trace.h util.h
CCSOURCE    = main.cpp bigint.cpp scanner.cpp trace.cpp util.cpp
EXECBIN     = ydc
OBJECTS     = ${CCSOURCE:.cpp=.o}
OTHERS      = ${MKFILE} ${DEPSFILE} README
ALLSOURCES  = ${CCHEADER} ${CCSOURCE} ${OTHERS}
LISTING     = ../asg2-ydc.code.ps
CLASS       = cmps109-wm.w11
PROJECT     = asg2

all : ${EXECBIN}
	- checksource ${ALLSOURCES}

${EXECBIN} : ${OBJECTS}
	${COMPILECCC} -o $@ ${OBJECTS}

%.o : %.cpp
#${CICOMMAND} + $<
	${COMPILECCC} -c $<

ci : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	${CICOMMAND} + ${ALLSOURCES}

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
	${GMAKE} deps

again :
	${GMAKE} spotless deps ci all lis

ifeq (${NEEDINCL}, )
include ${DEPSFILE}
endif

