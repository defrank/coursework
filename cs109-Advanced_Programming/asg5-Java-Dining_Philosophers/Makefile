# $Id: Makefile,v 1.1 2011-03-11 22:30:08-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 3

MKFILE      = Makefile
NOINCL      = ci clean spotless
NEEDINCL    = ${filter ${NOINCL}, ${MAKECMDGOALS}}
GMAKE       = ${MAKE} --no-print-directory
WHOAMI     ?= ${shell whoami}


CICOMMAND   = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif


JAVASRC     = DiningPhilosophers.java Philosopher.java Fork.java
ALLSOURCES  = ${JAVASRC} ${MKFILE} README
MAINCLASS   = DiningPhilosophers
CLASSES     = ${JAVASRC:.java=.class}
JARCLASSES  = ${CLASSES}
JARFILE     = DiningPhilosophers

SUBMITDIR   = cmps109-wm.w11 asg5


all : ${JARFILE}
	- checksource ${ALLSOURCES}
#${CICOMMAND} + ${ALLSOURCES}

${JARFILE} : ${CLASSES}
	echo Main-class: ${MAINCLASS} >Manifest
	jar cvfm ${JARFILE} Manifest ${JARCLASSES}
	- rm Manifest
	chmod +x ${JARFILE}

%.class : %.java
	- checksource $<
#${CICOMMAND} + $<
	javac $<

ci : ${ALLSOURCES}
	@ - checksource ${ALLSOURCES}
	${CICOMMAND} + ${ALLSOURCES}

clean :
	- rm ${JARCLASSES} Manifest

spotless : clean
	- rm ${JARFILE} 

submit : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	submit ${SUBMITDIR} ${ALLSOURCES}
	testsubmit ${SUBMITDIR} ${ALLSOURCES}

again :
	${GMAKE} spotless ci all
