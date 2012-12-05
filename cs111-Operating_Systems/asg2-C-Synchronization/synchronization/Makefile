################################################################################
# $Id: Makefile,v 1.1 2012-05-12 19:48:02-07 dmfrank - $
# Derek Frank, dmfrank@ucsc.edu
# Harrison Vuong, hvuong@ucsc.edu
# David Zou, dzou@ucsc.edu
#
# NAME
#   Makefile - /usr/src/servers/pm/
#
################################################################################


.include <bsd.own.mk>

# Makefile for Process Manager (PM)
PROG=	pm
SRCS=	main.c forkexit.c break.c exec.c time.c alarm.c \
	signal.c utility.c table.c getset.c misc.c \
	profile.c schedule.c semaphore.c

.if ${USE_MCONTEXT} != "no"
SRCS+= mcontext.c
CPPFLAGS+= -DUSE_MCONTEXT
.endif

.if ${USE_TRACE} != "no"
SRCS+= trace.c
CPPFLAGS+= -DUSE_TRACE
.endif

DPADD+=	${LIBSYS} ${LIBTIMERS}
LDADD+=	-lsys -ltimers

MAN=

BINDIR?= /usr/sbin

CPPFLAGS.main.c+=	-I${MINIXSRCDIR}
CPPFLAGS.misc.c+=	-I${MINIXSRCDIR}
CPPFLAGS.schedule.c+=	-I${MINIXSRCDIR}
CPPFLAGS.utility.c+=	-I${MINIXSRCDIR}

.include <minix.bootprog.mk>
