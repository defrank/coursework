/***********************************************************************
 * $Id: main.c,v 1.80 2011-10-14 22:11:24-07 dmfrank - $
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
//#include "astree.h"
//#include "lyutils.h"
#include "auxlib.h"

#define CPP "/usr/bin/cpp"
#define LINESIZE 1024


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
//   Reports an error if no extension is found or if the extension is
//   not ".oc".  Program will continue with an error.
//
char *remove_ext (cstring string) {
   if (string == NULL) {
      errprintf ("%s: %s: %s: %s: %s\n", "Error", get_execname(),
                 "main.c", "remove_ext()", "null character string");
      return NULL;
   }
   char *copystr;
   if ( (copystr = malloc (strlen (string) + 1) ) == NULL) {
      errprintf ("%s: %s: %s: %s: %s\n", "Error", get_execname(),
                 "main.c", "remove_ext()", "allocation error");
      return NULL;
   }
   xstrcpy (copystr, string);
   char *period = strrchr (copystr, '.');
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
   return copystr;
}


////
// chomp
//
//   Chomp the last character from a buffer if it is delim.
//
void chomp (cstring string, char delim) {
   size_t len = strlen (string);
   if (len == 0) return;
   cstring nlpos = string + len - 1;
   if (*nlpos == delim) *nlpos = '\0';
}

////
// cpplines
//
//   Run cpp against the lines of the file.
//
void cpplines (FILE *pipe, cstring filename, stringtable_ref table) {
   int linenr = 1;
   char inputname[LINESIZE];
   xstrcpy (inputname, filename);
   for (;;) {
      char buffer[LINESIZE];
      cstring fgets_rc = fgets (buffer, LINESIZE, pipe);
      if (fgets_rc == NULL) break;
      chomp (buffer, '\n');
      /* xprintf ("%s:line %d: [%s]\n", filename, linenr, buffer); */
      char flags[LINESIZE];
      // http://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
      int sscanf_rc = sscanf (buffer, "# %d \"%[^\"]\"%[^\n]",
                              &linenr, filename, flags);
      if (sscanf_rc == 3) {
         /* xprintf ("Directive: line %d, file \"%s\", flags \"%s\"\n",
            linenr, filename, flags); */
         continue;
      }
      cstring savepos = NULL;
      cstring bufptr = buffer;
      for (int tokenct = 1; ; ++tokenct) {
         cstring token = strtok_r (bufptr, " \t\n", &savepos);
         bufptr = NULL;
         if (token == NULL) break;
         // lines such as "# 1 <command line>" do not get tokenized
         if (strcmp ("#", token) == 0) { --linenr; break; }
         /* xprintf ("token %d.%d: [%s]\n", linenr, tokenct, token); */
         (void) intern_stringtable (table, token);
      }
   ++linenr;
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
   for(;;) {
      option = getopt (argc, argv, "@:D:ly");
      if (option == EOF) break;
      switch (option) {
         case '@': set_debugflags (optarg);                     break;
         case 'D': options->suppresscode = TRUE;
            options->d_arg = optarg;                            break;
         case 'l':                                              break;
         case 'y':                                              break;
         default:  errprintf ("%:bad option (%c)\n", optopt);
            options->badopt = TRUE;                             break;
      }
   }
   if (optind > argc || options->badopt) {
      errprintf ("%s: %s %s %s %s %s\n", "Usage", get_execname(),
                 "-{ly}", "-@[flag ...]", "-D[string]", "program.oc");
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
   char command[strlen (CPP) + 1 + strlen (filename) + 1];
   xstrcpy (command, CPP);
   xstrcat (command, " ");
   // suppress inclusion of code
   if (options.suppresscode) {
      xstrcat (command, options.d_arg);
      xstrcat (command, " ");
   }
   xstrcat (command, filename);
   FILE *pipe = popen (command, "r");
   stringtable_ref hashtable = new_stringtable (); // create the table
   if (pipe == NULL) {
      syserrprintf (command);
   }else {
      cpplines (pipe, filename, hashtable);
      int pclose_rc = pclose (pipe);
      eprint_status (command, pclose_rc);
      // if CPP returns error then do none of this
      if (pclose_rc == 0) {
         // strip extension and file directory
         cstring basefile = basename (filename);
         cstring stripped = remove_ext (basefile); // uses malloc()
         stripped = strcat (stripped, ".str");
         FILE* writefile = fopen (stripped, "w+");
         free (stripped);
         debugdump_stringtable (hashtable, writefile);
         (void) fclose (writefile);
      }
   }
   delete_stringtable (hashtable);
   return get_exitstatus();
}
