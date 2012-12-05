/***********************************************************************
 * $Id: main.c,v 1.14 2011-10-22 22:57:21-07 dmfrank - $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   main.c - main program
 *
 * DESCRIPTION
 *   Will analyze the argv array as appropriate and set up the various
 *   option flags.  program.str, depending on the name of the program
 *   source file.  Created files are always in the current directory,
 *   regardless of where the input files are found.  Uses getopt(3) to
 *   analyze the options and arguments.
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

#include "stringtable.h"
#include "astree.h"
#include "lyutils.h"
#include "auxlib.h"

#define CPP "/usr/bin/cpp"

char *basefilename;

////
// options - struct
//
//   Stores the results from scanning the command line options.
//
struct options{
   bool badopt;
   bool suppresscode;
   cstring d_arg;
};

////
// remove_ext
//
//   Removes the extension of a given character string if it has one.
//   Copies this new string in the external character string
//   basefilename located in "lyutils.h".
//   Reports an error if no extension is found or if the extension is
//   not ".oc".  Program will continue with an error.
//
void remove_ext (cstring string) {
   if (string == NULL) {
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "main.c", "remove_ext()", "null character string");
      return;
   }
   basefilename = strdup (string);
   char *period = strrchr (basefilename, '.');
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
}

////
// yyin_cpp_popen
//
//   Open a pipe from the C preprocessor.
//   Exit failure if can't.
//   Assign opened pipe to FILE *yyin.
//
void yyin_cpp_popen (cstring command, cstring filename,
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
// yytokenize
//
//   Call yylex() repeatedly to scan through the cpp pipe and attain
//   each token and token code.  Stores the tokens in the string table.
//   yylex() calls on functions from "lyutils.c" that correctly print
//   output to "program.tok".
//
void yytokenize (stringtable_ref table) {
   int tokenct;
   for (tokenct = 1; ; ++tokenct) {
      int tokencode = yylex();
      if (tokencode == YYEOF) break;
      // xprintf ("token code: %d, token: %s\n", tokencode, yytext);
      intern_stringtable (table, yytext);
   }
   // tokfile opened in lyutils.c: scanner_userinit()
   (void) fclose (tokfile);
}

////
// strfilewrite
//
//   If the CPP pipe is not null, then calls on yytokenize() to tokenize
//   the pipe and store the tokens in the string table.  This function
//   then dumps the string table to "program.str".  Will also close the
//   CPP pipe, yyin.
//
void strfilewrite (stringtable_ref table, cstring command) {
   if (yyin == NULL) {
      syserrprintf (command);
   }else {
      yytokenize (table);
      int pclose_rc = pclose (yyin);
      eprint_status (command, pclose_rc);
      // if CPP returns error then do none of this
      if (pclose_rc == 0) {
         cstring strname = malloc (strlen (basefilename)
                                   + strlen (".str") + 1);
         xstrcpy (strname, basefilename);
         xstrcat (strname, ".str");
         FILE *strfile = fopen (strname, "w+");
         free (strname);
         debugdump_stringtable (table, strfile);
         (void) fclose (strfile);
      }else {
         // if CPP returns an error then so shall this program
         set_exitstatus (pclose_rc);
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
   cstring filename = optind == argc ? "-" : argv[optind];
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
   cstring filename = optind == argc ? "-" : argv[optind];
   DEBUGF ('m', "filename = %s\n", filename);
   if (filename == NULL || strcmp (filename, "-") == 0
       || strcmp (filename, "") == 0) {
      errprintf ("%s: %s\n", get_execname(), "no input file");
      return get_exitstatus();
   }
   // strip extension and file directory
   cstring extname = basename (filename);
   remove_ext (extname); // uses malloc() for extern basenamefile
   // create CPP command for pipe
   cstring yyin_cpp_command = NULL;
   if (options.suppresscode) {
      yyin_cpp_command = malloc (strlen (CPP) + strlen (options.d_arg)
                        + strlen (filename) + 3);
   }else {
      yyin_cpp_command = malloc (strlen (CPP) + strlen (filename) + 2);
   }
   yyin_cpp_popen (yyin_cpp_command, filename, &options);
   stringtable_ref hashtable = new_stringtable (); // create the table
   strfilewrite (hashtable, yyin_cpp_command);
   free (basefilename); // strdup() uses malloc()
   delete_stringtable (hashtable);
   return get_exitstatus();
}
