# $Id: Makefile,v 1.2 2011-12-04 07:45:10-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# CMPE 150L Winter 2012
# Project eVote - Server

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
WHOAMI   ?= ${shell whoami}

#
# Define the "ci" command with respect to the current user.
#
CICOMMAND  = ci
ifeq (${WHOAMI},dmfrank)
CICOMMAND  = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND  = cil
endif
ifeq (${WHOAMI},fain)
CICOMMAND  = cil
endif

#
# Definitions of the compiler and compilation options:
#
GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
LINT      = lint -Xa -fd -m -u -x -errchk=%all
SPLINT    = splint +posixlib -incondefs
MKDEPS    = gcc -MM

#
# Defintions of list of files:
#
CSOURCES  = evs.c list.c auxlib.c # evc.c
# CSOURCES  = evc.c auxlib.c # evs.c
HSOURCES  = list.h auxlib.h
MISCSRCS  = README ${MKFILE}
OBJECTS   = ${CSOURCES:.c=.o}
EXECBIN   = evs
ALLSRCS   = ${HSOURCES} ${CSOURCES}
CHKSRC    = ${CSOURCES} ${HSOURCES} ${MISCSRCS}

#
# The first target is always ``all'', and hence the default,
# and builds the executable images.
#
all : ${EXECBIN}

#
# Build the executable image from the object files.
#
${EXECBIN} : ${OBJECTS}
	${GCC} -o${EXECBIN} ${OBJECTS}

#
# Build an object file from a C source file.
#
%.o : %.c
	${GCC} -c $<

#
# Run lint to check the source files.
#
lint : ${CSOURCES}
	${LINT} ${CSOURCES}
	checksource ${CHKSRC}

#
# Run splint to check the source files.
#
splint : ${CSOURCES}
	${SPLINT} ${CSOURCES}
	checksource ${CHKSRC}

#
# Check sources into an RCS subdirectory.
#
ci : ${CHKSRC}
	${CICOMMAND} + ${CHKSRC}
	checksource ${CHKSRC}

#
# Run checksource on the sources.
#
check : ${CHKSRC}
	checksource ${CHKSRC}

#
# Clean and spotless remove generated files.
#
clean :
	- rm ${OBJECTS} ${ALLGENS} ${REPORTS} ${DEPSFILE} core

spotless : clean
	- rm ${EXECBIN}

#
# Build the dependencies file using the C preprocessor.
#
deps : ${ALLCSRCS}
	@ echo "# ${DEPSFILE} created `date` by ${MAKE}" >${DEPSFILE}
	${MKDEPS} ${CSOURCES} >>${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${MAKE} --no-print-directory deps

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif
