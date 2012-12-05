# $Id: Makefile,v 1.3 2011-02-02 08:39:52-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 3

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

CICOMMAND   = cid
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif


CCHEADER    = interp.h numbers.h object.h trace.h util.h util-inst.h
CCSOURCE    = main.cpp interp.cpp numbers.cpp object.cpp trace.cpp \
              util.cpp
EXECBIN     = draw
OBJECTS     = ${CCSOURCE:.cpp=.o}
OTHERS      = ${MKFILE} README
ALLSOURCES  = ${CCHEADER} ${CCSOURCE} ${OTHERS}
LISTFILES   = ${ALLSOURCES} ${DEPSFILE}

LISTING     = ../asg3-draw.code.ps
CLASS       = cmps109-wm.w10
PROJECT     = asg3

all : ${EXECBIN}
	- checksource ${ALLSOURCES}

${EXECBIN} : ${OBJECTS}
	${COMPILECCC} -o $@ ${OBJECTS}

%.o : %.cpp
	${CICOMMAND} + $<
	${COMPILECCC} -c $<

ci : ${ALLSOURCES}
	@ - checksource ${ALLSOURCES}
	${CICOMMAND} + ${ALLSOURCES}

lis : ${ALLSOURCES}
	mkpspdf ${LISTING} ${LISTFILES}

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

