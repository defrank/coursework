# $Id: Makefile,v 1.5 2011-05-10 08:58:23-07 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Assignment 3

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
SUBMAKE   = ${MAKE} --no-print-directory
WHOAMI    ?= ${shell whoami}

SOURCE    = bigint.mli bigint.ml maindc.ml scanner.mll
ALLSRC    = ${SOURCE} dc.ml ${MKFILE}
ALLSOURCES = ${SOURCE} ${MKFILE} README
OBJCMO    = bigint.cmo scanner.cmo maindc.cmo
OBJCMI    = ${patsubst %.cmo, %.cmi, ${OBJCMO}}
CAMLRUN   = ocamldc
LISTING   = ../asg4-ocaml.code.ps
CLASS     = cmps112-wm.s11
PROJECT   = asg3

# Set "ci" command depending on user.
CICOMMAND   = cid
ifeq (${WHOAMI},dmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},derekmfrank)
CICOMMAND   = cil
endif
ifeq (${WHOAMI},fain)
CICOMMAND   = cil
endif

all : ${CAMLRUN}

${CAMLRUN} : ${OBJCMO} ${OBJCMI}
	ocamlc ${OBJCMO} -o ${CAMLRUN}

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

%.ml : %.mll
	ocamllex $<

clean :
	- rm ${OBJCMO} ${OBJCMI} ${DEPSFILE} scanner.ml

spotless : clean
	- rm ${CAMLRUN}

ci : ${RCSFILES}
	${CICOMMAND} + ${ALLSRC}
	checksource ${ALLSRC}

submit : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	submit ${CLASS} ${PROJECT} ${ALLSOURCES}
	testsubmit ${CLASS} ${PROJECT} ${ALLSOURCES}

deps : ${SOURCE}
	ocamldep ${SOURCE} >${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${SUBMAKE} deps

lis : ${ALLSRC}
	mkpspdf ${LISTING} ${ALLSRC} ${DEPSFILE}

again :
	${SUBMAKE} spotless ci
	${SUBMAKE} all lis

ifeq (${NEEDINCL}, )
include ${DEPSFILE}
endif

.PRECIOUS : scanner.ml

