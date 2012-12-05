/***********************************************************************
 * $Id: auxlib.c,v 1.8 2011-10-05 18:49:09-07 dmfrank - $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   auxlib.c - implementation file for auxiliary library
 *
 * DESCRIPTION
 *   Auxiliary library containing miscellaneous useful things.
 *
 **********************************************************************/


#define _GNU_SOURCE
#define __USE_GNU

#include <assert.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wait.h>

#include "auxlib.h"

static int exitstatus = EXIT_SUCCESS;
static char *execname = NULL;
static char *debugflags = "";
static bool alldebugflags = FALSE;

void set_execname (char *argv0) {
   execname = basename (argv0);
}

char *get_execname (void) {
   assert (execname != NULL);
   return execname;
}

void veprintf (char *format, va_list args) {
   assert (execname != NULL);
   assert (format != NULL);
   xfflush (NULL);
   if (strstr (format, "%:") == format) {
      xfprintf (stderr, "%s: ", get_execname ());
      format += 2;
   }
   xvfprintf (stderr, format, args);
   xfflush (NULL);
}

void eprintf (char *format, ...) {
   va_list args;
   va_start (args, format);
   veprintf (format, args);
   va_end (args);
}

void errprintf (char *format, ...) {
   va_list args;
   va_start (args, format);
   veprintf (format, args);
   va_end (args);
   exitstatus = EXIT_FAILURE;
}

void syserrprintf (char *object) {
   errprintf ("%: %s: %s\n", object, strerror (errno) );
}

static void eprint_signal (char *kind, int signal) {
   eprintf (", %s %d", kind, signal);
   char *sigstr = strsignal (signal);
   if (sigstr != NULL) xfprintf (stderr, " %s", sigstr);
}

void eprint_status (char *command, int status) {
   if (status == 0) return;
   eprintf ("%s: status 0x%04X", command, status);
   if (WIFEXITED (status)) {
      eprintf (", exit %d", WEXITSTATUS (status));
   }
   // LINTED (warning: cast from 32-bit integer to 8-bit integer)
   if (WIFSIGNALED (status)) {
      eprint_signal ("Terminated", WTERMSIG (status));
      #ifdef WCOREDUMP
      if (WCOREDUMP (status)) eprintf (", core dumped");
      #endif
   }
   if (WIFSTOPPED (status)) {
      eprint_signal ("Stopped", WSTOPSIG (status));
   }
   if (WIFCONTINUED (status)) {
      eprintf (", Continued");
   }
   eprintf ("\n");
}

int get_exitstatus (void) {
   return exitstatus;
}

void set_exitstatus (int newexitstatus) {
   newexitstatus &= 0xFF;
   if (exitstatus < newexitstatus) exitstatus = newexitstatus;
   DEBUGF ('x', "exitstatus = %d\n", exitstatus);
}

void __stubprintf (char *file, int line, const char *func,
                   char *format, ...) {
   va_list args;
   xfflush (NULL);
   xprintf ("%s: %s[%d] %s: ", execname, file, line, func);
   va_start (args, format);
   xvprintf (format, args);
   va_end (args);
   xfflush (NULL);
}

void set_debugflags (char *flags) {
   debugflags = flags;
   if (strchr (debugflags, '@') != NULL) alldebugflags = TRUE;
   DEBUGF ('x', "Debugflags = \"%s\", all = %d\n",
           debugflags, alldebugflags);
}

bool is_debugflag (char flag) {
   return alldebugflags || strchr (debugflags, flag) != NULL;
}

void __debugprintf (char flag, char *file, int line, const char *func,
                    char *format, ...) {
   va_list args;
   if (! is_debugflag (flag)) return;
   xfflush (NULL);
   va_start (args, format);
   xfprintf (stderr, "DEBUGF(%c): %s[%d] %s():\n",
             flag, file, line, func);
   xvfprintf (stderr, format, args);
   va_end (args);
   xfflush (NULL);
}
