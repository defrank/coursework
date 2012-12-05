###########################################################################
# $Id: Makefile,v 1.1 2012-05-18 02:13:55-07 dmfrank - $
# Derek Frank (dmfrank@ucsc.edu)
# Justin Chen (juychen@ucsc.edu)
# Benjamin Chow (bchow@ucsc.edu)
#
# CMPS 111 Spring 2012
# Project 3
#
# NAME
#   Makefile
#
# DESCRIPTION
#   A Makefile specific to Minix
#
###########################################################################

MKFILE    = Makefile

#
# Definitions of the compiler and compilation options:
#
#GCC       = gcc -g -O0 -Wall -Wextra -std=gnu99
GCC       = gcc
AR        = ar -rvs

#
# Definition of files:
#
#DIR      = /root/p3/test
DIR      = /root
CSRCS    = tester.c
LIBSRC   = libmem.c
OSRC     = libmem.o
ASRC     = libmem.a
SRCS     = ${CSRCS} ${HSRCS}
EXECBIN  = tester
OBJECTS  = ${CSRCS:.c=.o}
MISC     = ${MKFILE}
ALLSRCS  = ${SRCS} ${MISC}

#
# The first target is always ''all'', and hence the default.
#
all : ${EXECBIN}

#
# Build the executable image from the object files.
#
${EXECBIN} :
	${GCC} -c ${LIBSRC}
	${AR} ${ASRC} ${OSRC}
	${GCC} -o${EXECBIN} ${CSRCS} -L${DIR} -lmem -lm
#	${GCC} -o${EXECBIN} ${CSRCS} ${ASRC}

#
# Clean and spotless remove generated files.
#
clean :
	- rm ${OSRC} ${OBJECTS} core

spotless : clean
	- rm ${EXECBIN} ${ASRC}

