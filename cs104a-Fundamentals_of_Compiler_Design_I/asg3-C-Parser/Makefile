# $Id: Makefile,v 1.21 2011-11-07 12:32:52-08 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 3

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless submit
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
CSOURCES  = stringtable.c astree.c lyutils.c main.c auxlib.c
HSOURCES  = stringtable.h astree.h astree.rep.h lyutils.h auxlib.h
LSOURCE   = scanner.l
YSOURCE   = parser.y
MISCSRCS  = README ${MKFILE}
ETCSRCS   = ${MISCSRCS} ${DEPSFILE}
CLGEN     = yylex.c
HYGEN     = yyparse.h
CYGEN     = yyparse.c
CGENS     = ${CLGEN} ${CYGEN}
ALLGENS   = ${HYGEN} ${CGENS}
ALLCSRCS  = ${CSOURCES} ${CGENS}
OBJECTS   = ${ALLCSRCS:.c=.o}
EXECBIN   = oc
LREPORT   = yylex.output
YREPORT   = yyparse.output
IREPORT   = ident.output
REPORTS   = ${LREPORT} ${YREPORT} ${IREPORT}
ALLSRCS   = ${ETCSRCS} ${YSOURCE} ${LSOURCE} ${HSOURCES} ${CSOURCES}
CHKSRC    = ${CSOURCES} ${HSOURCES} ${YSOURCE} ${LSOURCE} ${MISCSRCS}

#
# Class and submit definitions:
#
CLASS     = cmps104a-wm.f11
PROJECT   = asg3

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
	ident ${OBJECTS} ${EXECBIN} >${IREPORT}

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
# Build the scanner.
#
${CLGEN} : ${LSOURCE}
	flex -o${CLGEN} ${LSOURCE} 2>${LREPORT}
	- grep -v '^ ' ${LREPORT}
	- (perl -e 'print "="x65,"\n"'; cat lex.backup) >>${LREPORT}
	- rm lex.backup

#
# Build the parser.
#
${CYGEN} ${HYGEN} : ${YSOURCE}
	bison -dtv -o${CYGEN} ${YSOURCE}

#
# Check sources into an RCS subdirectory.
#
ci : ${CHKSRC}
	${CICOMMAND} + ${CHKSRC}
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
	${MKDEPS} ${ALLCSRCS} >>${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${MAKE} --no-print-directory deps

submit : ${CHKSRC}
	- checksource ${CHKSRC}
	submit ${CLASS} ${PROJECT} ${CHKSRC}
	testsubmit ${CLASS} ${PROJECT} ${CHKSRC}

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif
