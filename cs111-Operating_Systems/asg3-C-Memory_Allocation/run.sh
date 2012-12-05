#!/bin/sh
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
#   run.sh
#
# DESCRIPTION
#   A script to compile and run libmem and the tester.
#
# USAGE 1
#   ./run.sh
#
# USAGE 2
#   sh run.sh
#
###########################################################################

# build the files
make spotless
make all
# run the tester
./tester
