# $Id: Makefile,v 1.1 2012-06-10 00:22:58-07 dmfrank - $
# Derek Frank (dmfrank@ucsc.edu)
# Alexander McCaleb (amccaleb@ucsc.edu)
# Alexander Kerr (ahkerr@ucsc.edu)
#
# Makefile for protectfile and Rijndael (AES) cipher.
#
# $Id: Makefile,v 1.1 2012-06-10 00:22:58-07 dmfrank - $
# NOTE: has GNU make specific stuff
#

DIR = $(shell pwd)
SRCS = protectfile.c rijndael.c
INCS = rijndael.h
OBJS = $(SRCS:.c=.o)
EXECBIN = protectfile
OTHERS = Makefile

# Testing section
TESTKEY = 123456789abcdef0
TESTFILE = testfile.txt
TESTTXT = This is a test file for encryption and decryption.


GCC = gcc -Wall

all: $(EXECBIN)

$(EXECBIN): $(OBJS) $(INCS)
	$(GCC) -o$(EXECBIN) $(OBJS)
	rm *.o

%.o: %.c
	$(GCC) -c $<

test: all
	@echo '$(TESTTXT)' >$(TESTFILE)
	@chmod -t $(TESTFILE)
	@printf "\\n"
	@printf "\\n"
	@echo "TEST FILE PROTECTION BITS BEFORE ENCRYPTION:"
	@ls -la $(TESTFILE)
	@printf "\\n"
	@echo "TEST FILE BEFORE ENCRYPTION:"
	@cat $(TESTFILE)
	@printf "\\n"
	@echo "PROTECT FILE:"
	./protectfile e $(TESTKEY) $(TESTFILE)
	@printf "\\n"
	@echo "TEST FILE PROTECTION BITS AFTER ENCRYPTION:"
	@ls -la $(TESTFILE)
	@printf "\\n"
	@echo "TEST FILE AFTER ENCRYPTION:"
	@cat $(TESTFILE)
	@printf "\\n"
	@printf "\\n"
	@echo "UNPROTECT FILE:"
	./protectfile d $(TESTKEY) $(TESTFILE)
	@printf "\\n"
	@echo "TEST FILE PROTECTION BITS AFTER DECRYPTION:"
	@ls -la $(TESTFILE)
	@printf "\\n"
	@echo "TEST FILE AFTER DECRYPTION:"
	@cat $(TESTFILE)
	@printf "\\n"

teste:
#	@echo '$(TESTTXT)' >$(TESTFILE)
	@printf "\\n"
	@ls -la $(TESTFILE)
	@printf "\\n"
	@cat $(TESTFILE)
	@printf "\\n"
	./protectfile e $(TESTKEY) $(TESTFILE)
	@printf "\\n"
	@ls -la $(TESTFILE)
	@cat $(TESTFILE)
	@printf "\\n"
	@printf "\\n"

testd:
	@printf "\\n"
	@ls -la $(TESTFILE)
	@printf "\\n"
	@cat $(TESTFILE)
	@printf "\\n"
	./protectfile d $(TESTKEY) $(TESTFILE)
	@printf "\\n"
	@ls -la $(TESTFILE)
	@cat $(TESTFILE)
	@printf "\\n"
	@printf "\\n"

clean:
	rm *.o

spotless: 
	rm $(EXECBIN) $(TESTFILE) *.o
