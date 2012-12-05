/* $Id: yshell.cpp,v 1.5 2011-01-19 09:26:15-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   yshell - in memory simulated tree shell
 *
 * SYNOPSIS
 *   yshell [-@ flags]
 *
 * DESCRIPTION
 *   This shell reads commands from stdin and write output to stdout,
 *   with errors being set to cerr. Each line read by the shell is
 *   parsed into words by splitting using space characters, with any
 *   number of spaces between words. There may also be leading and
 *   trailing spaces. The first word on any line is a command to be
 *   simulated, and the rest are operands to that command. If either
 *   stdin or stdout is not a tty, each line from stdin is echoed
 *   to stdout.
 *
 *   The commands modify an inode tree, where each inode is either a
 *   file or a directory. Files contain data and directories contain
 *   inodes. An inode is specified by means of a pathname. A pathname
 *   consists of a sequence of characters separated by slash (/)
 *   characters.
 *
 *   The inode tree has a root, which is a special node, and also a
 *   current inode as well. Whenever a pathname is decoded, if the first
 *   character is a slash, decoding begins at the root, otherwise it
 *   begins with the current directory. Whenever a pathname component is
 *   a dot (.), it refers to the current directory. If a component is a
 *   double dot (..) it refers to the parent of the current directory.
 *   Every directory has both of these entries, with the root being its
 *   own parent.
 *
 *   Every inode has three attributes : an inode number, which is
 *   uniquely assigned, starting from 1 for the root ; contents, which
 *   is a vector of inodes for a directory, and text for a file ; and a
 *   size, which is the byte count for text, and the number of
 *   sub-inodes for a directory.
 *
 * OPERANDS
 *   None. All input comes from stdin.
 *
 * OPTIONS
 *   The -@ option is followed by a sequence of flags to enable
 *   debugging output, which is written to cerr.
 *
 * COMMANDS
 *     The following commands are interpreted. Error messages are
 *     printed and nothing is done in the case of invalid operands.
 *
 *     # string
 *           If the first non-space character on a line is a hash, the
 *           line is a comment and is ignored.
 *
 *     cat pathname . . .
 *           The contents of each file is copied to stdout. An error is
 *           reported if no files are specified, a file does not exist,
 *           or is a directory.
 *
 *     cd [pathname]
 *           The current directory is set the the pathname given. If no
 *           pathname is specified, the root directory (/) is used. It
 *           is an error if the pathname does not exist or is a plain
 *           file, or if more than one operand is given.
 *
 *     echo [words . . .]
 *           The string, which may be empty, is echoed to stdout on a
 *           line by itself.
 *
 *     exit [status]
 *           Exit the program with the given status. If the status is
 *           missing exit with status 0. If a non-numeric argument is
 *           given, exit with status 255.
 *
 *     ls [pathname . . .]
 *           A description of the files or directories are printed to
 *           stdout. It is an error if any of the file or directory does
 *           not exist. If no pathname is specified, the current working
 *           directory is used. If a pathname specified is a directory,
 *           then the contents of the directory are listed. A directory
 *           listed within a directory is shown by a terminating slash.
 *           Elements of a directory are listed lexicographically.
 *
 *           For each file listed, output consists of the inode number,
 *           then the size, then the filename. Output is lined up into
 *           columns and each column is separated from the next by two
 *           spaces. The numberic fields are exactly 6 characters wide
 *           and the units position in a column must be aligned.
 *
 *     lsr [pathname . . .]
 *           As for ls, but a recursive depth-first preorder traversal
 *           is done for subdirectories.
 *
 *     make filename [words . . .]
 *           The file specified is created and the rest of the words are
 *           put in that file. If the file already exists, a new one is
 *           not created, but its contents are replaced. It is an error
 *           to specify a directory. If there are no words, the file is
 *           empty.
 *
 *     mkdir pathname
 *           A new directory is created. It is an error if a file or
 *           directory of the same name already exists, or if the
 *           complete pathname to the parent of this new directory does
 *           not already exit. Two entries are added to the directory,
 *           namely dot (.) and dotdot (..). Directory entries are
 *           always kept in sorted lexicographic order.
 *
 *     prompt string
 *           Set the prompt to the words specified on the command line.
 *           Each word is separated from the next by one space and the
 *           prompt itself is terminated by an extra space. The default
 *           prompt is a single percent sign and a space (% ).
 *
 *     pwd
 *           Prints the current working directory.
 *
 *     rm pathname
 *           The specified file or directory is deleted (removed from
 *           its parent's list of files and subdirectories). It is an
 *           error for the pathname not to exist. If the pathname is a
 *           directory, it must be empty.
 *
 *     rmr pathname
 *           A recursive removal is done, using a depth-first postorder
 *           traversal.
 *
 * EXIT STATUS
 *
 *   0  No errors were detected.
 *
 *   1  Error messages were printed to cerr.
 */

#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
#include <utility>
#include <unistd.h>
#include <cstdio>

using namespace std;

#include "commands.h"
#include "trace.h"
#include "inode.h"
#include "util.h"

//
// scan_options
//    Options analysis:  The only option is -Dflags. 
//

void scan_options (int argc, char **argv) {
   opterr = 0;
   for (;;) {
      int option = getopt (argc, argv, "@:");
      if (option == EOF) break;
      switch (option) {
         case '@':
            traceflags::setflags (optarg);
            break;
         default:
            complain() << "-" << (char) option << ": invalid option"
                       << endl;
            break;
      }
   }
   if (optind < argc) {
      complain() << "operands not permitted" << endl;
   }
}

//
// main -
//    Main program which loops reading commands until end of file.
//

int main (int argc, char **argv) {
   setexecname (argv[0]);
   cout << boolalpha; // Print false or true instead of 0 or 1.
   cerr << boolalpha;
   scan_options (argc, argv);
   bool need_echo = want_echo();
   commands cmdmap;
   string prompt;
   inode_state state;
   inode root (DIR_INODE);
   root.alter_dirname ("/");
   root.insert_dirents (".", &root);
   root.insert_dirents ("..", &root);
   state.alter_root (&root);
   state.alter_cwd (&root);
   state.push_cwd ("/");
   try {
      for (;;) {
         try {
            // reload prompt since it can be changed by user
            prompt = state.get_prompt();
            // Read a line, break at EOF, and echo print the prompt
            // if one is needed.
            cout << prompt << " ";
            string line;
            getline (cin, line);
            if (cin.eof()) {
               if (need_echo) cout << "^D";
               cout << endl;
               TRACE ('y', "EOF");
               break;
            }
            if (need_echo) cout << line << endl;
   
            // Split the line into words and lookup the appropriate
            // function.  Complain or call it.
            wordvec words = split (line, " \t");
            TRACE ('y', "words = " << words);
            // Ignore all lines beginning with a hash (#).
            if (words[0][0] != '#') {
               function fn = cmdmap[words[0]];
               if (fn == NULL) {
                  throw yshell_exn (words[0] + ": no such function");
               }
               fn (state, words);
            }
         }catch (yshell_exn exn) {
            // If there is a problem discovered in any function, an
            // exn is thrown and printed here.
            complain() << exn.what() << endl;
         }
      }
   } catch (ysh_exit_exn) {
   }

   return exit_status_message();
}

