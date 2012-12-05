# $Id: Makefile,v 1.15 2011-10-06 00:01:48-07 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 1

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless submit
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
WHOAMI   ?= ${shell whoami}

#
# Definitions of the compiler and compilation options:
#
GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
LINT      = lint -Xa -fd -m -u -x -errchk=%all
MKDEPS    = gcc -MM

#
# Defintions of list of files:
#
CSOURCES  = stringtable.c auxlib.c main.c
HSOURCES  = stringtable.h auxlib.h
LSOURCES  = scanner.l
YSOURCES  = parser.y
ETCSRCS   = README ${MKFILE} ${DEPSFILE}
CLGEN     = yylex.c
HYGEN     = yyparse.h
CYGEN     = yyparse.c
CGENS     = ${CLGEN} ${CYGEN}
ALLGENS   = ${HYGEN} ${CGENS}
ALLCSRCS  = ${CSOURCES}
OBJECTS   = ${ALLCSRCS:.c=.o}
EXECBIN   = oc
LREPORT   = yylex.output
YREPORT   = yyparse.output
IREPORT   = ident.output
REPORTS   = ${LREPORT} ${YREPORT} ${IREPORT}
ALLSRCS   = ${ETCSRCS} ${HSOURCES} ${CSOURCES}
ALLSOURCE = ${CSOURCES} ${HSOURCES} README ${MKFILE}
ALLSAVE   = ${ALLSOURCE}

#
# Class and submit definitions:
#
CLASS     = cmps104a-wm.f11
PROJECT   = asg1

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
CICOMMAND  = cid
endif

#
# The first target is always ``all'', and hence the default,
# and builds the executable images.
#
all : ${EXECBIN}

#
# Build the executable image from the object files.
#
${EXECBIN} : ${OBJECTS}
#${CICOMMAND} + ${ALLSOURCE}
	checksource ${ALLSOURCE}
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
	checksource ${CSOURCES}

#
# Build the scanner.
#
#${CLGEN} : ${LSOURCES}
#flex -o${CLGEN} ${LSOURCES} 2>${LREPORT}
#- grep -v '^ ' ${LREPORT}
#- (perl -e 'print "="x65,"\n"'; cat lex.backup) >>${LREPORT}
#- rm lex.backup

#
# Build the parser.
#
#${CYGEN} ${HYGEN} : ${YSOURCES}
#bison -dtv -o${CYGEN} ${YSOURCES}

#
# Check sources into an RCS subdirectory.
#
ci : ${ALLSAVE}
	${CICOMMAND} + ${ALLSAVE}
	checksource ${ALLSAVE}

#
# Clean and spotless remove generated files.
#
clean :
	- rm ${OBJECTS} ${DEPSFILE} core

spotless : clean
	- rm ${EXECBIN}

#
# Build the dependencies file using the C preprocessor.
#
deps : ${ALLCSRCS}
	@ echo "# ${DEPSFILE} created `date`" >${DEPSFILE}
	${MKDEPS} ${ALLCSRCS} | sort | uniq >>${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${MAKE} --no-print-directory deps

submit : ${ALLSOURCE}
	- checksource ${ALLSOURCE}
	submit ${CLASS} ${PROJECT} ${ALLSOURCE}
	testsubmit ${CLASS} ${PROJECT} ${ALLSOURCE}

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif
