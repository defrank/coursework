###########################################################################
# $Id: Makefile,v 1.9 2012-04-30 20:19:29-07 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# CMPS 111 Spring 2012
# Project 1
#
# NAME
#   Makefile
#
# DESCRIPTION
#   A Makefile specific to Linux
#
###########################################################################

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless git lint splint check
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
WHOAMI   ?= ${shell whoami}

#
# Define the "ci" command with respect to the current user.
# dmfrank, derekmfrank, fain, and ghoti are all aliases of
# myself Derek Michael Frank.
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
ifeq (${WHOAMI},ghoti)
CICOMMAND = cil
endif

#
# Define checksource.
# Set line length of .c/.h files to 95.
# Set line length of text files to 75.
#
CHK95   = checksource -l 95
CHK75   = checksource -l 75

#
# Definitions of the compiler and compilation options:
#
GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
LINT      = lint -Xa -fd -m -u -x -errchk=%all
SPLINT    = splint +posixlib -incondefs
MKDEPS    = gcc -MM

#
# Definition of files:
#
CSRCS    = auxlib.c shell.c
HSRCS    = auxlib.h
SRCS     = ${CSRCS} ${HSRCS}
EXECBIN  = myshell
OBJECTS  = ${CSRCS:.c=.o}
MISC     = ${MKFILE}
TXT      = design.txt README
ALLSRCS  = ${SRCS} ${MISC} ${TXT}
CHKTXT   = ${TXT}
CHKSRC   = ${SRCS} ${MISC}

#
# The first target is always ''all'', and hence the default.
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
lint : ${CSRCS}
	${LINT} ${CSRCS}
	${CHK95} ${CHKSRC}

#
# Run splint to check the source files.
#
splint : ${CSRCS}
	${SPLINT} ${CSRCS}
	${CHK95} ${CHKSRC}


#
# Update the git index
#
git :
	git status
	git commit -a
	git log

#
# Check sources into an RCS subdirectory
#
ci : ${ALLSRCS}
	${CICOMMAND} + ${ALLSRCS}

#
# Run checksource on the files.
#
check : ${CHKSRC}
	- ${CHK75} ${CHKTXT}
	- ${CHK95} ${CHKSRC}

#
# Clean and spotless remove generated files.
#
clean :
	- rm ${OBJECTS} ${DEPSFILE} 

spotless : clean
	- rm ${EXECBIN}

#
# Build the dependencies file using the C preprocessor.
#
deps : ${CSRCS}
	@ echo "# ${DEPSFILE} created `date` by ${MAKE}" >${DEPSFILE}
	${MKDEPS} ${CSRCS} >>${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${MAKE} --no-print-directory deps


ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

