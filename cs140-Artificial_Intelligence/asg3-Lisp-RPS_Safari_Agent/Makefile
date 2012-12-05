# $Id: Makefile,v 1.1 2012-02-13 00:44:44-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# CMPS 140 Winter 2012
# Program 3

MKFILE    = Makefile
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
WHOAMI   ?= ${shell whoami}

#
# Define the "ci" command with respect to the current user
#
CICOMMAND = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND = cil
endif
ifeq (${WHOAMI},fain)
CICOMMAND = cil
endif

#
# Define checksource.  Set line length to 80 for common lisp in cs140.
#
CHKSRC   = checksource -l 80

#
# Definition of files:
#
SOURCES  = dmfrank.lisp agent.lisp dmfrank-p3.lisp formatted-dmfrank-p3.lisp
HWFILE   = hw3.txt
MISC     = ${MKFILE}
PFILES   = ${SOURCES} ${MISC}
ALLSRCS  = ${PFILES} ${HWFILE}

#
# Class and submit definitions:
#
CLASS    = cmps140-ral.w12
PROG     = P3
HW       = HW3

#
# The first target is always ''all'', and hence the default.
#
all : ci

#
# Check sources into an RCS subdirectory
#
ci : ${ALLSRC}
	${CICOMMAND} + ${ALLSRCS}

#
# Run checksource on the files.
#
check : ${PFILES}
	- ${CHKSRC} ${PFILES}

#
# Clean and spotless remove generated files.
#
clean :
	- rm core

spotless : clean
	- rm

#
# Submit commands:
#
submit : ${SOURCES}
	- ${CHKSRC} ${SOURCES}
	submit ${CLASS} ${PROG} ${SOURCES}
	testsubmit ${CLASS} ${PROG} ${SOURCES}

submithw : ${HWFILE}
	submit ${CLASS} ${HW} ${HWFILE}
	testsubmit ${CLASS} ${HW} ${HWFILE}
