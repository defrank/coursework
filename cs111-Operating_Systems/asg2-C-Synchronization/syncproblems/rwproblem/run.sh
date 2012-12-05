#!/bin/sh
# Names: David S. Zou, Derek M. Frank, Harrsion Vuong
# Project #2
#
# Script to run the reader/writer problem program
# tempfile is used to store the semaphore ids and
# use them as command line arguments for the
# reader programs. readcount keeps track of nreaders.


./initrw 
./reader 1 &
./reader 2 &
./writer 1 &
./reader 3 &
./writer 2 &
./reader 4 &
./reader 5 &
./writer 3 &
./reader 6 &
