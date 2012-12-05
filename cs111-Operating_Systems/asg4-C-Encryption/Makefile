# $Id: Makefile,v 1.1 2012-06-09 04:09:01-07 dmfrank - $
# Derek Frank (dmfrank@ucsc.edu)
# Alexander McCaleb (amccaleb@ucsc.edu)
#
# Makefile to move all the files to the correct location and set eveything
# up.  Assumes user has root permissions!
#
# NOTE: has GNU make specific stuff

# Working directory
DIR       = /root/project4

# Set up setkey system call.
# /usr/src/include/
SKDIR     = $(DIR)/setkey
SKINCL    = $(SKDIR)/include/Makefile $(SKDIR)/include/set_key.h
SKINCLMIN = $(SKDIR)/include/minix/callnr.h $(SKDIR)/include/minix/vfsif.h
# /usr/src/lib/libc/sys-minix/
SKLIBC    = $(SKDIR)/libc/sys-minix/Makefile.inc \
            $(SKDIR)/libc/sys-minix/set_key.c
# /usr/src/lib/libvtreefs/
SKLIBV    = $(SKDIR)/libvtreefs/table.c
# /usr/src/servers/
SKEXT2    = $(SKDIR)/ext2/table.c
SKHGFS    = $(SKDIR)/hgfs/table.c
SKVFS     = $(SKDIR)/vfs/table.c $(SKDIR)/vfs/open.c $(SKDIR)/vfs/proto.h \
            $(SKDIR)/vfs/protect.c $(SKDIR)/vfs/request.c
SKMFS     = $(SKDIR)/mfs/Makefile $(SKDIR)/mfs/open.c $(SKDIR)/mfs/proto.h \
            $(SKDIR)/mfs/protect.c $(SKDIR)/mfs/table.c $(SKDIR)/mfs/setkey.c

# Set up protectfile user program
PFDIR     = protectfile


all : sksetup


#
# Move all the necessary file to the correct place for the system call
# setkey.
#
sksetup :
	cp $(SKINCL) /usr/src/include
	cp $(SKINCL) /usr/include
	cp $(SKINCLMIN) /usr/src/include/minix
	cp $(SKINCLMIN) /usr/include/minix
	cp $(SKLIBC) /usr/src/lib/libc/sys-minix
	cp $(SKLIBV) /usr/src/lib/libvtreefs
	cp $(SKEXT2) /usr/src/servers/ext2
	cp $(SKHGFS) /usr/src/servers/hgfs
	cp $(SKVFS) /usr/src/servers/vfs
	cp $(SKMFS) /usr/src/servers/mfs
