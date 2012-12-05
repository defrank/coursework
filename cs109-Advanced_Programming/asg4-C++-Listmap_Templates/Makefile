# $Id: Makefile,v 1.3 2011-03-01 18:12:10-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 4

MKFILE      = Makefile
DEPSFILE    = ${MKFILE}.deps
NOINCL      = ci clean spotless
NEEDINCL    = ${filter ${NOINCL}, ${MAKECMDGOALS}}
GMAKE       = ${MAKE} --no-print-directory
UNAME      ?= ${shell uname -s}
WHOAMI     ?= ${shell whoami}


# Set compiler depending on operating system.
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

# Set "ci" command depending on user.
CICOMMAND   = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif


CCHEADER    = list.h util.h
CCSOURCE    = main.cpp list.cpp util.cpp
EXECBIN     = mylist
OBJECTS     = main.o
OTHERS      = ${MKFILE} README
ALLSOURCES  = ${CCHEADER} ${CCSOURCE} ${OTHERS}
LISTFILES   = ${ALLSOURCES} ${DEPSFILE}

CLASS       = cmps109-wm.w11
PROJECT     = asg4

all : ${EXECBIN}
	- checksource ${ALLSOURCES}

${EXECBIN} : ${OBJECTS}
	${COMPILECCC} -o $@ ${OBJECTS}

%.o : %.cpp %.h
#${CICOMMAND} + $<
	${COMPILECCC} -c $<

#${EXECBIN} : main.o
#${COMPILECCC} -o mylist main.o

#main.o : main.cpp list.cpp list.h util.cpp util.h
#${COMPILECCC} -c main.cpp

ci : ${ALLSOURCES}
	@ - checksource ${ALLSOURCES}
	${CICOMMAND} + ${ALLSOURCES}

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

