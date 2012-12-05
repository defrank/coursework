# $Id: Makefile,v 1.1 2011-05-26 15:19:13-07 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 5

MKFILE    = Makefile
NOINCLUDE = ci clean spotless
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
SUBMAKE   = ${MAKE} --no-print-directory
WHOAMI    ?= ${shell whoami}

SOURCE    = functions.pl
ALLSRC    = ${SOURCE} README
ALLSOURCE = ${ALLSRC} ${MKFILE} database.pl
CLASS     = cmps112-wm.s11
PROJECT   = asg5

# Set "ci" command depending on user.
CICOMMAND   = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},fain)
CICOMMAND   = cil
endif

all :
#${CICOMMAND} + ${ALLSOURCE}
	checksource ${ALLSOURCE}

clean :
	- rm group*.tests.output checksource.log

spotless : clean
	- rm

ci : ${RCSFILES}
	${CICOMMAND} + ${ALLSOURCE}
	checksource ${ALLSOURCE}

submit : ${ALLSRC}
	- checksource ${ALLSOURCE}
	submit ${CLASS} ${PROJECT} ${ALLSRC}
	testsubmit ${CLASS} ${PROJECT} ${ALLSRC}

again :
	${SUBMAKE} spotless ci
	${SUBMAKE} all
