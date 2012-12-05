/***********************************************************************
 * $Id: auxlib.h,v 1.1 2011-12-03 16:56:00-08 dmfrank - $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   auxlib.h - interface file for auxiliary library
 *
 * DESCRIPTION
 *   Auxiliary library containing miscellaneous useful things.
 *
 * SOURCE
 *   Code written by Wesley Mackey.
 *
 **********************************************************************/


#ifndef __AUXLIB_H__
#define __AUXLIB_H__

#include <stdarg.h>

// Silence lint from complaining about unused returned values
#define xfflush   (void) fflush
#define xfprintf  (void) fprintf
#define xprintf   (void) printf
#define xvprintf  (void) vprintf
#define xvfprintf (void) vfprintf
#define xstrcpy   (void) strcpy
#define xstrlen   (void) strlen
#define xstrcat   (void) strcat
#define xstrcmp   (void) strcmp
#define xmemset   (void) memset

//
// Miscellaneous useful typedefs.
//

typedef enum {FALSE = 0, TRUE = 1} bool;

//
// Error message and exit status utility.
//

void set_execname (char *argv0);
   //
   // Sets the program name for use by auxlib messages.
   // Must called from main before anything else is done,
   // passing in argv[0].
   //

char *get_execname (void);
   //
   // Returns a read-only value previously stored by set_progname.
   //

void veprintf (char *format, va_list args);
   //
   // Prints a message to stderr using the vector form of
   // argument list.
   //

void eprintf (char *format, ...);
   //
   // Print a message to stderr according to the printf format
   // specified.  Usually called for debug output.
   // Precedes the message by the program name if the format
   // begins with the characters `%:'.
   //

void errprintf (char *format, ...);
   //
   // Print an error message according to the printf format
   // specified, using eprintf.  Sets the exitstatus to EXIT_FAILURE.
   //

void syserrprintf (char *object);
   //
   // Print a message resulting from a bad system call.  The
   // object is the name of the object causing the problem and
   // the reason is taken from the external variable errno.
   //

void eprint_status (char *command, int status);
   //
   // Print the status returned by wait(2) from a subprocess.
   //

int get_exitstatus (void);
   //
   // Returns the exit status.  Default is EXIT_SUCCESS unless
   // set_exitstatus (int) is called.  The last statement in main
   // should be:  ``return get_exitstatus();''.
   //

void set_exitstatus (int);
   //
   // Sets the exit status.  Remebers only the largest value passed in.
   //

//
// Support for stub messages.
//
#define STUBPRINTF(...) \
        __stubprintf (__FILE__, __LINE__, __func__, __VA_ARGS__)
void __stubprintf (char *file, int line, const char *func,
                   char *format, ...);

//
// Debugging utility.
//

void set_debugflags (char *flags);
   //
   // Sets a string of debug flags to be used by DEBUGF statements.
   // Uses the address of the string, and does not copy it, so it
   // must not be dangling.  If a particular debug flag has been set,
   // messages are printed.  The format is identical to printf format.
   // The flag "@" turns on all flags.
   //

bool is_debugflag (char flag);
   //
   // Checks to see if a debugflag is set.
   //

#ifdef NDEBUG
// Do not generate any code.
#define DEBUGF(FLAG,...)   /**/
#define DEBUGSTMT(FLAG,STMTS) /**/
#else
// Generate debugging code.
void __debugprintf (char flag, char *file, int line, const char *func,
                    char *format, ...);
#define DEBUGF(FLAG,...) \
        __debugprintf (FLAG, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DEBUGSTMT(FLAG,STMTS) \
        if (is_debugflag (FLAG)) { DEBUGF (FLAG, "\n"); STMTS }
#endif

//
// Definition of RCSID macro to include RCS info in objs and execbin.
//
#define RCSH(NAME,ID) \
static const char __RCS_##NAME[] = "\0" ID;
#define RCSC(NAME,ID) \
static const char __RCS_C_##NAME[] = "\0" ID \
"\0$Compiled: " __FILE__ " " __DATE__ " " __TIME__ " $";

#endif
