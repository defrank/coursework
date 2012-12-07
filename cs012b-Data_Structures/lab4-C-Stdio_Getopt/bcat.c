/* $Id: bcat.c,v 1.7 2009-10-30 07:02:34-07 - - $
 * Derek Frank, dmfrank@ucsc.edu
 * 
 * NAME
 *   bcat -- concatenate and display files
 *
 * SYNOPSIS
 *   bcat [-mnsv] [filename...]
 *
 * DESCRIPTION
 *   The bcat utility reads files in sequence and copies each file to
 *   stdout, with options that control display attributes.
 *
 * OPTIONS
 *   All options precede all operands and are scanned via getopt(3c).
 *   The following options are supported:
 *
 *   -m   In the style of more(1), a title is printed in front of each
 *        file.  A title consists of exactly 5 lines:  an empty line,
 *        a line of 64 colons, a line with the name of the file (a
 *        minus sign (-) is used for stdin), a line of 64 colons, an
 *        empty line.
 * 
 *   -n   Line numbers are displayed to the left of each line in a
 *        field of width 6 followed by 2 spaces.
 * 
 *   -s   Multiple empty lines are squeezed into a single empty line.
 *        That is, if three or more consecutive newline characters
 *        (\n) appear on input, only the first two are copied.  They
 *        are still counted, though, for the purposes of printing line
 *        numbers if the -n option is specified.
 * 
 *   -v   Unprintable characters are displayed in visible format.
 *        ASCII characters are displayed in control format, e.g., ^M
 *        for the carriage return character.  and high-bit characters
 *        are displayed in hexadecimal, e.g., \x8F.  The function
 *        isprint(3c) determines if they are printable.  Newline
 *        characters are not printed specially in this notation and
 *        still terminate lines.
 * 
 * OPERANDS
 *   Each operand is the name of an input file.  If no filenames are
 *   specified, bcat reads from stdin.  If a filename is given as a
 *   minus sign (-), stdin will be read at that point.  The file stdin
 *   is never closed and multiple occurrences are accepted without
 *   complaint.
 * 
 * EXIT STATUS
 * 
 *   0    Normal successful completion.
 * 
 *   1    An error has occurred.  Program execution continues if
 *        possible in the presence of an error.
 * 
 * SEE ALSO
 *   cat(1), more(1), getopt(3c), isprint(3c).
 */

#include <errno.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <ctype.h>
#include <unistd.h>

/*
 * New types needed for this program.
 */
typedef enum bool { FALSE = 0, TRUE = 1 } bool;

struct options {
  bool moretitles;    // -m print file titles à la `more'
  bool numberlines;   // -n print line numbers in left column
  bool squeeze;       // -s squeeze multiple blank lines into one
  bool visiblechars;  // -v display unprintables visibly
};

/*
 * eprintf -
 * Print messages to stderr instead of stdout, but otherwise has
 * arguments like printf.  Also flushes to ensure that output is
 * not lost or interleaved.
 */

void eprintf (const char *format, ...) {
  va_list args;
  va_start (args, format);
  fflush (NULL);
  vfprintf (stderr, format, args);
  va_end (args);
  fflush (NULL);
}

/*
 * cat -
 * Copy the contents of an already-opened file to stdout.
 * Changes the format printed to stdout based on options.
 */
void catfile (FILE *input, struct options *options_ref, 
              char *filename) {
  bool nflag = FALSE; //flag for numberlines
  bool sflag = FALSE; //flag for squeeze
  bool vflag = FALSE; //flag for visiblechars
  int linenr = 1; //variable for line number
  int count = 0; //variable for newline count
  bool newline = FALSE; //variable for byte and newline compare
  //prints out a title for option -m
  if (options_ref->moretitles == TRUE) {
    char bdr[65] = "::::::::::::::::::::::::::::::::";
    printf("\n%s%s\n", bdr, bdr);
    printf("%s", filename);
    printf("\n%s%s\n", bdr, bdr);
  };
  //sets flags based on options
  if (options_ref->numberlines == TRUE) nflag = TRUE;
  if (options_ref->squeeze == TRUE) sflag = TRUE;
  if (options_ref->visiblechars == TRUE) vflag = TRUE;
  for (;;) {
    int byte = getc (input);
    if (byte == '\n') newline = TRUE; // repeated bool test
    else newline = FALSE;
    if (byte == EOF) break; //loop reacheds end of file
    //count will always be less than two even if option -v
    //is not called
    else if (nflag == TRUE && count <= 2) {
      printf("%6d  ", linenr); //prints line number
      nflag = FALSE;
    };
    //increments count only for a new line
    if ((newline == TRUE) && (sflag == TRUE)) ++count;
    else count = 0;
    //count will always be less than two even if option -v
    //is not called
    if (count <= 2) {
      if ((vflag == FALSE) || (newline == TRUE)) putchar (byte);
      else {
        //print characters normally
        if (isprint(byte)) putchar (byte);
        //print characters in control fashion
        else if (isascii(byte)) putchar (byte);
        //print characters in hexadecimal
        //else {};
      };
      if ((newline == TRUE) && (options_ref->numberlines == TRUE)) {
        ++linenr;
        nflag = TRUE;
      };
    };
  };
}

/*
 * main -
 * Loop over files, if any, and cat each of them to stdout.
 * Print error messages if appropriate.
 */
int main (int argc, char **argv) {
  int exit_status = EXIT_SUCCESS;
  struct options options;
  int argi;
  char *progname = basename (argv[0]);
  // Scan arguments and set flags.
  opterr = FALSE;
  bzero (&options, sizeof options); // set all opts FALSE
  for (;;) {
    int option = getopt (argc, argv, "mnsv");
    if (option == EOF) break;
    switch (option) {
      case 'm': options.moretitles   = TRUE; break;
      case 'n': options.numberlines  = TRUE; break;
      case 's': options.squeeze      = TRUE; break;
      case 'v': options.visiblechars = TRUE; break;
      default : eprintf ("%s: -%c: invalid option\n",
                         progname, optopt);
      exit_status = EXIT_FAILURE;
    };
  };
  if (argc == 1) {
    catfile (stdin, &options, "null");
  }else{
    for (argi = 1; argi < argc; ++argi) {
      if (strcmp (argv[argi], "-") == 0) {
        catfile (stdin, &options, "null");
      }else{
        FILE *input = fopen (argv[argi], "r");
        if (input == NULL) {
          //keeps from sending to stderr that options are not files
          if ((options.moretitles == TRUE
                 || options.numberlines == TRUE
                 || options.squeeze == TRUE
                 || options.visiblechars == TRUE)
                 && (strcmp (argv[argi], "-m")
                 || strcmp (argv[argi], "-n")
                 || strcmp (argv[argi], "-s")
                 || strcmp (argv[argi], "-v")));
          else {
            eprintf ("%s: %s: %s\n", progname,
                     argv[argi], strerror (errno));
            exit_status = EXIT_FAILURE;
          };
        }else{
          catfile (input, &options, argv[argi]);
          fclose (input);
        };
      };
    };
  };
  return exit_status;
}

/*
 *****************************************************************
 * 
 As with lab3, just ignore how `eprintf' works.  Just use it.
 
 Whenever a man page is referenced, read it online.  For example,
 when we refer to `stdio(3c)', you can read it with ``man -s 3C
 stdio''.
 
 As described in stdio(3c), there are three FILE* handles that
 are always opened when a program starts:  `stdin', `stdout', and
 `stderr'.  These are, respectively, the standard input, standard
 output, and standard error.  Normal output is written to stdout,
 while error messages are written to stderr.
 
 The usual format of an error message is something like:
 .   progname: object_or_function: reason
 The reason a system call has failed is given in the external
 variable `errno'.  This can be translated into English via
 strerror(3c).
 
 `fopen(3c)' opens a file and returns a pointer to a `FILE',
 given a filename.  `fclose(3c)' closes that file, given a
 FILE*.  `putchar(3c)' writes one byte to stdout.  `getc(3c)'
 reads one byte from the FILE* given as an argument and returns
 an int containing the character, if one exists.  If not, returns
 EOF (-1).  Note that end of line is signalled by '\n'.  To
 signal EOF from a Unix terminal, type Control/D as the first
 character on a line.
 
 Strings are represented as arrays of characters.  Each string
 ends with a null plug ('\0').  `strcmp(3c)' compares two such
 character strings and returns a number that is <, =, or > 0,
 depending on the relationship.  See Java's compareTo function.
 
 Some functions return values instead of void, but we often don't
 care what these values are, so we use the function in a
 statement context.  This causes lint(1) to complain:  ``function
 returns value which is always ignored''.  So we explicitly cast
 the results of these functions to (void) in order to suppress
 this error.  Alternately we could have use a drop-in macro to
 replace them.
 
 The call fflush (NULL) causes all opened FILE* handles to be
 flushed.  When a program writes data, it is buffered in memory
 before being written to the disk.  This causes immediate writing
 instead of waiting until the buffer is full.  We do this so that
 anything written to stdout and stderr are properly interleaved.
 
 *****************************************************************
 */

