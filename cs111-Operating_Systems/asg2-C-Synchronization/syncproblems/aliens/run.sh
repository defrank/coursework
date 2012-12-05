#!/bin/sh
# $Id: run.sh,v 1.1 2012-05-12 18:06:03-07 dmfrank - $
# Derek Frank (dmfrank@ucsc.edu)
# Harrison Vuong (dmfrank@ucsc.edu)
# David Zou (dzou@ucsc.edu)
#
# run.sh
#   A script file that may be altered to test the aliens as background
#   processes.  Be sure to leave initaliens and freealiens.  Best to
#   set the total number of reproductions by each gender to the same
#   total or else freealiens will attempt to free semaphores with
#   sleeping processes, which is not possible.

./initaliens
./alien 1 3 24 &
./alien 2 1 26 &
./alien 3 2 26 &
./alien 4 3 2  &
./alien 5 1 51 &
./alien 6 2 25 &
./alien 7 1 4 &       # 1 => 26 + 51 + 4 = 81
./alien 8 3 55 &      # 3 => 24 + 2 + 55 = 81
./alien 9 2 25 &
./alien 10 2 5 &      # 2 => 26 + 25 + 25 + 5 = 81

# freealiens should be called separately to prevent freeing on running
# processes
