/**************************************************************************
 * $Id: shell.c,v 1.1 2012-04-28 06:32:21-07 dmfrank - $
 * Derek Frank
 * dmfrank@ucsc.edu
 *
 *
 * NAME
 *   main.c - main program
 *
 * DESCRIPTION
 *   A basic shell program capable of handling up to 1024 character lines.
 *   Is able to run commands found in the user defined directories
 *   (e.g., /bin).  The commands accept as many arguments as necessary that
 *   fit within the 1024 character line limit.  The shell also handles
 *   input redirection (<), output redirection (>), and pipe (|) commands.
 *
 * SYNOPSIS
 *   fish
 *
 *************************************************************************/

#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "auxlib.h"


////
// checktopipe
//
//   Scans the tokens within an index boundary to see if the current command
//   will be sending output through a pipe.  Returns true it this command will
//   pipe and false otherwise.
//
bool checktopipe (char **tokens, int curstart, int nextstart) {
   int i;
   for (i = curstart; i < nextstart; ++i) {
      if (tokens[i][0] == '4') return TRUE;
   }
   return FALSE;
}

////
// get_redirectarg
//
//   Scan the tokens within an index boundary for the last instance of the
//   given redirection symbol.  Return its argument.
//
char *get_redirectarg (char **tokens, int curstart, int nextstart, char sym) {
   char *arg = NULL;
   int i;
   for (i = curstart; i < nextstart; ++i) {
      if (tokens[i][0] == '2' && tokens[i][1] == sym && tokens[i+1][0] == '3') {
         arg = &tokens[i+1][1];
         // do not allow filenames to exceed 255 characters
         if (strlen (arg) > 255) {
            eprintf ("%s: %s\n", "ERROR", "filenames cannot exceed 255 characters");
            arg = NULL;
         }
      }
   }
   return arg;
}


////
// get_newargc
//
//   Take the set of tokens and scan within two boundary indexes to determine
//   a new argc value.  Return the new argc.
//
int get_newargc (char **tokens, int curstart, int nextstart) {
   assert (tokens != NULL);
   int count = 0;
   int i;
   for (i = curstart; i < nextstart; ++i) {
      if (tokens[i][0] == '1' || tokens[i][0] == '0') ++count;
   }
   return count;
}

////
// set_command
//
//   Takes the set of tokens, a start index, and the total number of tokens.
//   Returns a pointer to the beginning of the first command string. 
//
char *set_command (char **tokens, int toknum, int *nextstart) {
   char *command = NULL;
   int i;
   for (i = *nextstart; i < toknum; ++i) {
      if (tokens[i][0] == '0') {
         // found the command so set it
         command = &tokens[i][1];
      }else if (tokens[i][0] == '4') {
         // found the next pipe, set next start and return
         *nextstart = i + 1;
         return command;
      }
   }
   // there are no more pipes so set next start to end
   *nextstart = toknum;
   
   return command;
}

////
// set_numofcmds
//
//   Scan the given array tokens and count the number of tokens that begin
//   with a '0'.  Return the number of commands counted.
//
int set_numofcmds (char **tokens, int n) {
   int count = 0;
   int i;
   for (i = 0; i < n; ++i) {
      if (tokens[i][0] == '0') ++count;
   }
   return count;
}

////
// check_exit
//
//   Checks if the user wishes to exit the shell.
//
bool check_exit (char **tokens, int n) {
   bool notfound = TRUE;
   int i;
   for (i = 0; i < n && notfound; ++i) {
      if (tokens[i][0] == '0') {
         notfound = FALSE;
         if (strcmp (tokens[i], "0exit") == 0) return TRUE;
         return FALSE;
      }
   }
   return FALSE;
}

////
// ismetachar
//
//   Takes a character and returns true if the character is a whitespace
//   character.  Whitespace characters include spaces (' '), tabs ('\t'),
//   newlines ('\n'), and carriage returns ('\r').
//
bool ismetachar (char test) {
   if (test == '|' || test == '<' || test == '>')
      return TRUE;
   return FALSE;
}

////
// isnewline
//
//   Takes a character and returns true if the character is a newline
//   character.  Newline characters include newlines ('\n') and carriage
//   returns ('\r').
//
bool isnewline (char test) {
   if (test == '\r' || test == '\n')
      return TRUE;
   return FALSE;
}

////
// iswhitespace
//
//   Takes a character and returns true if the character is a whitespace
//   character.  Whitespace characters include spaces (' '), tabs ('\t'),
//   newlines ('\n'), and carriage returns ('\r').
//
bool iswhitespace (char test) {
   if (test == ' ' || test == '\t' || isnewline (test))
      return TRUE;
   return FALSE;
}

////
// case_whitespace
//
//   Returns true if any error is produced and false otherwise.  Handles
//   the case where the current character is a whitespace.
//     A following null plug ends parsing.
//     A following non-whitepsace begins a token preceded by 0, 1, or 3.
//     A following whitespace is ignored.
//
bool case_whitespace (char *line, char *buffer, char **tokens,
                      int *ii, int *jj, int *kk, int *iscmd, int *isarg) {
   // make the passed indices more accessible
   int i = (*ii);
   int j = (*jj);
   int k = (*kk);

   // a following null plug is ignored
   // a following meta-char is ignored
   // a following whitespace is ignored
   // a following non-whitespace begins a token (including a backslash)
   if (!iswhitespace (line[i+1]) && line[i+1] != '\0' &&
       !ismetachar (line[i+1])) {
      if (*isarg) {
         buffer[j] = '3'; // a redirection argument token
      }else if (*iscmd) {
         buffer[j] = '0'; // a command token
         *iscmd = 0; // no longer looking for a command
         *isarg = 0;
      }else {
         buffer[j] = '1'; // a command argument token
      }
      tokens[k] = &buffer[j];
      ++(*jj); // increment buffer index
      ++(*kk); // increment tokens index
   }
   return FALSE;
}

////
// case_redirection
//
//   Returns true if any error is produced and false otherwise.  Handles
//   the case where the current character is a redirection meta-character.
//     A redirection meta-character is a token by itself preceded by a '2'.
//     A following null plug will case a parsing error.
//     A following meta-character will cause a parsing error.
//     A following backslash begins a token preceded by a '3'.
//     A following non-whitespace begins a token preceded by a '3'.
//     A following whitespace is ignored.
//
bool case_redirection (char *line, char *buffer, char **tokens,
                       int *ii, int *jj, int *kk, int *isarg) {
   // make the passed indices more accessible
   int i = (*ii);
   int j = (*jj);
   int k = (*kk);

   // if we are looking for a redirection arg then throw a parsing error
   if (*isarg) return TRUE;

   // '2' represents a meta-char
   buffer[j] = '2'; // a meta-char
   *isarg = 1; // next token should be a redirection arg
   tokens[k] = &buffer[j]; // begin a new token
   // place redirection symbol into buffer
   buffer[j+1] = line[i]; // meta-char placement
   buffer[j+2] = '\0'; // null plug
   ++(*jj); ++(*jj); ++(*jj); j = (*jj); // increment buffer index
   ++(*kk); k = (*kk); // increment tokens index
      
   // a following null plug will cause a parsing error
   // a following meta-char will cause a parsing error
   // a following non-whitespace begins a token (including a backslash)
   // a following whitespace is ignored
   if (line[i+1] == '\0' || ismetachar (line[i+1]) ||
       isnewline (line[i+1])) {
      return TRUE; // parsing error
   }else if (!iswhitespace (line[i+1])) {
      // non-whitespace begins a redirection argument
      buffer[j] = '3';
      tokens[k] = &buffer[j];
      ++(*jj);
      ++(*kk);
   }
   return FALSE;
}

////
// case_pipe
//
//   Returns true if any error is produced and false otherwise.  Handles
//   the case where the current character is a pipe meta-character.
//     A pipe meta-character is a token by itself preceded by a '4'.
//     A following null plug will cause a parsing error.
//     A following pipe meta-character will cause a parsing error.
//     A following redirection meta-character is ignored.
//     A following backslash will begin a new command token.
//     A following non-whitespace begins a new command token.
//     A following whitespace is ignored.
//
bool case_pipe (char *line, char *buffer, char **tokens,
                int *ii, int *jj, int *kk, int *iscmd, int *isarg) {
   // make the passed indices more accessible
   int i = (*ii);
   int j = (*jj);
   int k = (*kk);

   // if we are looking for a redirection arg or command
   // then throw a parsing error
   if ((*isarg) || (*iscmd)) return TRUE;

   // '4' represents a pipe meta-char
   buffer[j] = '4'; // a pipe meta-char
   *iscmd = 1; // next token should be a command
   tokens[k] = &buffer[j]; // begin a new token
   // place  symbol into buffer
   buffer[j+1] = line[i]; // meta-char placement
   buffer[j+2] = '\0'; // null plug
   ++(*jj); ++(*jj); ++(*jj); j = (*jj); // increment buffer index
   ++(*kk); k = (*kk); // increment tokens index
      
   // a following null plug will cause a parsing error
   // a following pipe will cause a parsing error
   // a following redirection symbol is ignored
   // a following whitespace is ignored
   // a following non-whitespace begins a token (including a backslash)
   if (line[i+1] == '\0' || line[i+1] == '|') {
      return TRUE; // parsing error
   }else if (!iswhitespace (line[i+1]) && !ismetachar (line[i+1])) {
      // non-whitespace begins a command
      buffer[j] = '0';
      *iscmd = 0; // no longer looking for command
      tokens[k] = &buffer[j];
      ++(*jj);
      ++(*kk);
   }
   return FALSE;
}

////
// case_backslash
//
//   Returns true if any error is produced and false otherwise.  Handles
//   the case where the current character is a backslash.
//     A following null plug will cause a parsing error.
//     Anything else following is added to the current token.
//     Examine the next following char
//       A null plug ends the current token.
//       A meta-character ends the current token.
//       A whitespace ends the current token.
//       Anything else is ignored.
//
bool case_backslash (char *line, char *buffer, char **tokens,
                     int *ii, int *jj, int *kk, int *iscmd, int *isarg) {
   // make the passed indices more accessible
   int i = (*ii);
   int j = (*jj);
   int k = (*kk);

   // a following null plug will cause a parsing error
   // any following char is taken as a literal (including whitespace)
   if (line[i+1] == '\0' || line[i+1] == '\n' || line[i+1] == '\r')
      return TRUE; // parsing error
   if (*iscmd && !(*isarg)) {
      // token is a command
      buffer[j] = '0';
      tokens[k] = &buffer[j];
      ++(*jj); j = (*jj);
      ++(*kk); k = (*kk);
      *iscmd = 0;
      *isarg = 0;
   }
   // place char that follows the backslash
   buffer[j] = line[i+1];
   ++(*ii);
   ++(*jj);

   // check if this char ends the token
   if (ismetachar (line[i+2]) || iswhitespace (line[i+2]) ||
       line[i+2] == '\0') {
      // place a null plug
      buffer[j+1] = '\0';
      ++(*jj); j = (*jj);
      tokens[k] = &buffer[j];
      *isarg = 0;
   }
   return FALSE;
}

////
// case_word
//
//   Returns true if any error is produced and false otherwise.  Handles
//   the case where the current character belongs to a word.
//     A word character is added to the current token.
//     A following null plug ends the current token and parsing.
//     A following meta-character ends the current token.
//     A following whitespace ends the current token.
//     A following backslash is ignored.
//     A following word character is ignored.
//
bool case_word (char *line, char *buffer, char **tokens,
                  int *ii, int *jj, int *kk, int *iscmd, int *isarg) {
   // make the passed indices more accessible
   int i = (*ii);
   int j = (*jj);
   int k = (*kk);

   // token is a command
   if (*iscmd && !(*isarg)) {
      buffer[j] = '0';
      tokens[k] = &buffer[j];
      ++(*jj); j = (*jj);
      ++(*kk); k = (*kk);
      *iscmd = 0;
      *isarg = 0;
   }
   // place the char
   buffer[j] = line[i];
   ++(*jj); j = (*jj);

   // a following null plug will end the token
   // a following meta-char will end the token
   // a following whitespace will end the token
   // a following backslash is ignored
   // a following non-whitespace is ignored
   if (line[i+1] == '\0' || ismetachar (line[i+1]) ||
       iswhitespace (line[i+1])) {
      buffer[j] = '\0';
      ++(*jj); j = (*jj);
      tokens[k] = &buffer[j];
      *isarg = 0;
   }
   return FALSE;
}

////
// parseline
//
//   Takes a buffer and returns another buffer containing tokens separated
//   by null ('\0').  Also returns a character array pointing to each token
//   in order.  Each token will also be altered so as to have a 0, 1, 2, or
//   3 placed before it to illustrate whether the token is a command (0),
//   argument (1), redirection meta-character (2), redirection argument
//   (3), or pipe meta-character (4).  In addition, anything following
//   backslash ('\') will be taken as a literal.  If backslash or meta-
//   character ends the line, then it will cause an error for the current
//   parse and the whole command line to be disregarded.
//
bool parseline (char *line, char *buffer, char **tokens, int *toknum) {
   assert (line != NULL && buffer != NULL &&
           tokens != NULL && toknum != NULL);
   bool parse_err = FALSE;
   int iscmd = 1; // looking for a command token
   int isarg = 0; // looking for a redirection argument token
                  // isarg takes priority over iscmd
   
   // loop through each character in the line
   int i = 0; // index for line
   int j = 0; // index for buffer
   int k = 0; // index for tokens array
   
   // first token begins at the head of the buffer (even if null)
   buffer[0] = '\0';
   tokens[0] = &buffer[0];
   while (line[i] != '\0') {
      // error parsing if values exceeded
      // should never happen
      if (i > 1024 || j > 2048 || k > 512) {
         eprintf ("%s: %s\n     %s: %d\n     %s: %d\n     %s: %d\n",
                  "FATAL ERROR",
                  "indices attempting to exceed buffer or array limits",
                  "line buffer", i, "parse buffer", j, "tokens array", k);
         return TRUE;
      }
      switch (line[i]) {
         // whitespace
         case '\t':
            parse_err = case_whitespace (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // whitespace
         case ' ':
            parse_err = case_whitespace (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // whitespace
         case '\n':
            parse_err = case_whitespace (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // whitespace
         case '\r':
            parse_err = case_whitespace (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // redirection meta-character
         case '<':
            parse_err = case_redirection (line, buffer, tokens, &i, &j, &k, &isarg);
            break;
         // redirection meta-character
         case '>':
            parse_err = case_redirection (line, buffer, tokens, &i, &j, &k, &isarg);
            break;
         // pipe meta-character
         case '|':
            parse_err = case_pipe (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // backslash
         case '\\':
            parse_err = case_backslash (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
         // word
         default:
            parse_err = case_word (line, buffer, tokens, &i, &j, &k, &iscmd, &isarg);
            break;
      }
      if (parse_err) return TRUE;
      ++i; // increment line index
   }
   *toknum = k;
   buffer[j] = '\0';
   // user should give a command
   // user should also give an argument for a redirection
   if (k != 0 && (isarg || iscmd)) return TRUE;
   return FALSE;
}



////
// get_cmdline
//
//   Uses fgets() to get the command line from the user ended by a newline.
//   The line cannot exceed 1024 characters.  fgets() will grab 1025
//   characters.  The function will then test if 1024 characters are
//   exceeded.  Returns true if the line contains 1024 or fewer characters
//   and false otherwise.
//
bool get_cmdline (char *line) {
   fgets (line, 1025, stdin);
   if (strlen (line) >= 1024) {
      eprintf ("%s: %s\n", "Error", "Cannot exceed 1024 characters!");
      return FALSE;
   }
   return TRUE;
}

////
// print_prompt
//
//   Prints a prompt to the command line for the user.
//   The prompt is the username contained in brackets followed by a "$".
//   Uses getenv("USER") to get the username.
//   Prompt: [username]$
//
void print_prompt () {
   char *username = getenv("USER");
   printf ("[%s]$ ", username);
}

////
// main
//
//   The main function to the shell program.
//
int main (int argc, char **argv) {
   (void) argc;
   
   // variables
   set_execname (argv[0]);
   
   // loop until user exits
   while (TRUE) {   // repeat until user exits the shell
      char cmdline[1025]; // will only accept 1024
      bzero (cmdline, 1025);
      char parsebuffer[2024];
      bzero (parsebuffer, 2024);
      char *tokens[512];
      int toknum = 0;
//      bool pipein = FALSE;
//      bool pipeout = FALSE;
      
      print_prompt();    // print the prompt
      // get up to 1024 chars from the user
      if (get_cmdline (cmdline) == FALSE) continue;
      // break the line into tokens
      if (parseline (cmdline, parsebuffer, tokens, &toknum)) {
         // parseline returned an error
         eprintf ("%s: %s\n", "Error",
                  "parsing error caused by incorrect usage");
         continue;
      }

      // Check if the user wishes to exit the shell
      if (check_exit (tokens, toknum)) exit (get_exitstatus());
      

      // set the number of commands to be executed
      int numcmds = set_numofcmds (tokens, toknum);
      // the number of commands should not exceed 20
      if (numcmds > 20) {
         eprintf ("%s: %s\n", "ERROR", "cannot handle more than 20 commands");
         continue;
      }

      // initialize the current start index of the tokens
      int curstart;
      int nextstart = 0;
      
      int fd[2];
//      int fdin[2];
//      int fdout[2];
      // loop fork-exec for each command
      int i;
      for (i = 0; i < numcmds && nextstart < toknum; ++i) {
         // setup current command, update current start, and determine pipe
         curstart = nextstart;
         char *command = set_command (tokens, toknum, &nextstart);
         // a null command will cause an error
         if (command == NULL || command[0] == '\0') {
            eprintf ("%s: %s\n", "FATAL ERROR", "encountered a null command");
            break;
         }
         // make a new argv
         int newargc = get_newargc (tokens, curstart, nextstart);
         char *newargv[newargc + 1];
         newargv[0] = command;
         newargv[newargc] = NULL;
         int j = 1;
         int k;
         for (k = curstart; k < nextstart && j < newargc; ++k) {
            if (tokens[k][0] == '1') {
               newargv[j] = &tokens[k][1];
               ++j;
            }
         }

/*         // set pipe
         pipeout = checktopipe (tokens, curstart, nextstart);
         if (pipein) {
            fdin[0] = fdout[0];
            fdin[1] = fdout[1];
         }
         if (pipeout) {
            pipe (fdout);
         }
*/       
         // set redirection
         char *input_redirect = get_redirectarg (tokens, curstart, nextstart, '<');
         char *output_redirect = get_redirectarg (tokens, curstart, nextstart, '>');
         if (input_redirect != NULL) {
            // open file and set file descriptor
            fd[0] = open (input_redirect, O_RDONLY, 0755);
         }
         if (output_redirect != NULL) {
            // open file and set file descriptor
            fd[1] = open (output_redirect, O_CREAT | O_WRONLY | O_TRUNC, 0644);
         }


         // fork child process
         int status = 0;
         int pid = fork();
         if (pid != 0) {
            /* this is the PARENT PROCESS */
/*            // write to pipe from parent to child
            if (pipein) {
               close (fdin[0]);
               if (dup2 (fdin[1], STDOUT_FILENO) == -1)
                  eprintf ("%s: %s\n", "ERROR",
                           "could not open and write to pipe from parent to child");
               close (fdin[1]);
               pipein = FALSE;
               
            }
*/
            // wait for any child process to finish
            int waitstatus = waitpid (-1, &status, 0);

/*            // read pipe from child to parent
            if (pipeout) {
               close (fdout[1]);
               if (dup2 (fdout[0], STDIN_FILENO) == -1)
                  eprintf ("%s: %s\n", "ERROR",
                           "could not open and read pipe from child to parent");
               pipein = TRUE;
            }
*/          // close the file descriptors caused by redirection
            // close stdin
            if (input_redirect != NULL) close (fd[0]);
            // close stdout
            if (output_redirect != NULL) close (fd[1]);
            // determine errors
            if (waitstatus == 0) {
               eprintf ("%s: %s\n", "ERROR", "error waiting on child process");
               break;
            }else if (waitstatus == -1) {
               eprintf ("%s: %s\n", "ERROR", "error executing child process");
               break;
            }
            if (status != 0) {
               eprintf ("%s: %s: %s %d\n", "ERROR", command, "exit status of", status);
               break;
            }
         }else {
            /* this is the CHILD PROCESS */
            // set up pipe in from a previous command
/*            if (pipein) {
               close (fdin[1]);
               if (dup2 (fdin[0], STDIN_FILENO) == -1)
                  eprintf ("%s: %s\n", "ERROR",
                           "could not open and write to pipe from child to parent");
               close (fdin[0]);
            }
            // set up pipe out to the next command
            if (pipeout) {
               close (fdout[0]);
               if (dup2 (fdout[1], STDOUT_FILENO) == -1)
                  eprintf ("%s: %s\n", "ERROR",
                           "could not open and write to pipe from child to parent");
            }
*/            
            // set up redirection
            if (input_redirect != NULL) {
               // open file and set file descriptor
               // set file to stdin
               if (dup2 (fd[0], STDIN_FILENO) == -1)
                  eprintf ("%s: %s \"%s\" %s\n", "ERROR", "could not open",
                           input_redirect, "for input redirection");
            }
            if (output_redirect != NULL) {
               // open file and set file descriptor
               // set stdout to file
               if (dup2 (fd[1], STDOUT_FILENO) == -1)
                  eprintf ("%s: %s \"%s\" %s\n", "ERROR", "could not open",
                           output_redirect, "for output redirection");
            }

            // execute the command
            set_exitstatus (execvp (command, newargv));
            // if there was an error executing the command
            eprintf ("%s: %s: %s\n", "ERROR", command, "could not execute");
            return (get_exitstatus());
         }
         
      }

   }
   return get_exitstatus();
}
