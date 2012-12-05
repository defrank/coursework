#!/bin/sh
# $Id: run.sh,v 1.1 2012-05-12 18:48:09-07 dmfrank - $
# Derek Frank (dmfrank@ucsc.edu)
# Harrison Vuong (dmfrank@ucsc.edu)
# David Zou (dzou@ucsc.edu)
#
# run.sh
#   A script file that may be altered to test the semaphores as background
#   processes.

./init 0 &          # random sem id
./init 2029 &       # sem id 2029
./down 1 2029 &     # down on sem 2029
./down 2 2029 &     # down on sem 2029
./down 3 2029 &     # down on sem 2029
./up 4 2029 &       # up on sem 2029
./down 5 2029 &     # down on sem 2029
./up 6 2029 &       # up on sem 2029
./val 2029 &        # up on sem 2029
./up 7 2029 &       # up on sem 2029
./up 8 2029 &       # up on sem 2029
./up 9 2029 &       # up on sem 2029
./up 10 2029 &      # up on sem 2029
./free 2029 &       # free sem 2029

# cannot free randomly chosed sem id, must change manually by reading
# its value from stdout
