/***********************************************************************
 * $Id: main.c,v 1.75 2011-11-07 23:49:22-08 dmfrank - $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   main.c - main program
 *
 * DESCRIPTION
 *   Will analyze the argv array as appropriate and set up the various
 *   option flags.  Writes a string table to program.str, each scanned
 *   token to program.tok, and the abstract syntax tree to program.ast.
 *   Created files are always in the current directory, regardless of
 *   where the input files are found.  Uses getopt(3) to analyze the
 *   options and arguments.
 *
 * SYNOPSIS
 *   oc [-ly] [-@ flag ...] [-D string] program.oc
 *
 * OPTIONS
 *   -@flags   Call set_debugflags and use DEBUGF and DEBUGSTMT for
 *             debugging.  The details of the flags are at the
 *             implementor's discretion, and are not documented here.
 *
 *   -Dstring  Pass this option and its argument to cpp.  This is
 *             mostly useful as -D__OCLIB_OH__ to suppress inclusion
 *             of the code from oclib.oh when testing a program.
 *
 *
 *   -l        Debug yylex() with yy_flex_debug = 1
 *
 *   -y        Debug yyparse() with yydebug = 1
 *
 **********************************************************************/


#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "astree.h"
#include "lyutils.h"
#include "auxlib.h"

#define CPP "/usr/bin/cpp"

////
// options - struct
//
//   Stores the results from scanning the command line options.
//
struct options{
   bool badopt;
   bool suppresscode;
   char *d_arg;
};

////
// remove_ext
//
//   Removes the extension of a given character string if it has one.
//   Calls a function from "lyutils.c" to set the basefilename character
//   string in that file, since all opening and closing of files related
//   to this filename are done there.  This prevents the use of another
//   external variable.  This function reports an error if no extension
//   is found or if the extension is not ".oc".  Program will continue
//   with an error.
//
void remove_ext (char *string) {
   if (string == NULL) {
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "main.c", "remove_ext()", "null character string");
      return;
   }
   char *filename = strdup (string);
   char *period = strrchr (filename, '.');
   // block extension with null plug if it exists, then check if file
   // ends in ".oc"
   if (period == NULL) {
      errprintf ("%s: %s\n", get_execname(),
                 "missing file extension");
   }else if (strcmp (period, ".oc") != 0) {
      errprintf ("%s: %s: %s\n", get_execname(),
                 "invalid file extension", period);
      *period = '\0';
   }else {
      *period = '\0';
   }
   set_basefilename (filename);
   free (filename);
}

////
// yyin_cpp_popen
//
//   Open a pipe from the C preprocessor.
//   Exit failure if can't.
//   Assign opened pipe to FILE *yyin.
//
void yyin_cpp_popen (char *command, char *filename,
                     struct options *options) {
   assert (command != NULL);
   xstrcpy (command, CPP);
   xstrcat (command, " ");
   // suppress inclusion of code
   if (options->suppresscode) {
      xstrcat (command, options->d_arg);
      xstrcat (command, " ");
   }
   xstrcat (command, filename);
   yyin = popen (command, "r");
   if (yyin == NULL) {
      syserrprintf (command);
      exit (get_exitstatus());
   }
}

////
// strfilewrite
//
//   If the CPP pipe is not null, then calls on yyparse() to parse the
//   pipe.  yyparse() calls on yylex() when necessary to tokenize the
//   pipe.  The first call to yylex() will create the "program.tok" file
//   and store the tokens in the string table as specified by functions
//   in "lyutils.c".  This function will create files "program.str"
//   and "program.ast" by calling on functions in "lyutils.c".  The
//   string table and ast will then be dumped to their respective
//   files.  This function will also close the CPP pipe,
//   yyin.
//
void strfilewrite (char *command) {
   if (yyin == NULL) {
      syserrprintf (command);
   }else {
      // yyparse() also creates "program.tok", but does not close it
      int parsestatus = yyparse();
      int pclose_rc = pclose (yyin);
      eprint_status (command, pclose_rc);
      // "lyutils.c" function creates the remaining files, dumps
      // output to them, and closes all the files
      file_dumps ();
      if (pclose_rc != 0 || parsestatus != 0) {
         // if CPP or yyparse() returns an error then exit with status 1
         set_exitstatus (1);
      }
   }
}

////
// scan_opts
//
//   Uses getopt() to scan options from the command line.  Sets
//   necessary flags.  Prints an error message for incorrect options,
//   but program will still continue.
//
void scan_opts (int argc, char **argv, struct options *options) {
   int option;
   opterr = 0;
   yy_flex_debug = 0;
   yydebug = 0;
   for(;;) {
      option = getopt (argc, argv, "@:D:ly");
      if (option == EOF) break;
      switch (option) {
         case '@': set_debugflags (optarg);                     break;
         case 'D': options->suppresscode = TRUE;
                   options->d_arg = optarg;                     break;
         case 'l': yy_flex_debug = 1;                           break;
         case 'y': yydebug = 1;                                 break;
         default:  errprintf ("%:bad option (%c)\n", optopt);
                   options->badopt = TRUE;                      break;
      }
   }
   if (optind > argc || options->badopt) {
      errprintf ("%s: %s %s %s %s %s\n", "Usage", get_execname(),
                 "-ely", "-@[flag ...]", "-D[string]", "program.oc");
   }
   char *filename = optind == argc ? "-" : argv[optind];
   DEBUGF ('m', "filename = %s\n", filename);
}

////
// main - main
//
int main (int argc, char **argv) {
   struct options options = {FALSE, FALSE, NULL};
   set_execname (argv[0]);
   DEBUGSTMT ('m',
      for (int argi = 0; argi < argc; ++argi) {
         eprintf ("%s%c", argv[argi], argi < argc - 1 ? ' ' : '\n');
      }
   );
   scan_opts (argc, argv, &options);
   char *filename = optind == argc ? "-" : argv[optind];
   DEBUGF ('m', "filename = %s\n", filename);
   if (filename == NULL || strcmp (filename, "-") == 0
       || strcmp (filename, "") == 0) {
      errprintf ("%s: %s\n", get_execname(), "no input file");
      return get_exitstatus();
   }
   // strip extension and file directory
   char *extname = basename (filename);
   remove_ext (extname); // uses malloc() for extern basenamefile
   // create CPP command for pipe
   char *yyin_cpp_command = NULL;
   if (options.suppresscode) {
      yyin_cpp_command = malloc (strlen (CPP) + strlen (options.d_arg)
                        + strlen (filename) + 3);
   }else {
      yyin_cpp_command = malloc (strlen (CPP) + strlen (filename) + 2);
   }
   yyin_cpp_popen (yyin_cpp_command, filename, &options);
   strfilewrite (yyin_cpp_command);
   free (yyin_cpp_command);
   return get_exitstatus();
}
