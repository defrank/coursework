/* $Id: commands.h,v 1.5 2011-01-19 09:26:15-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * commands
 */

#ifndef __COMMANDS_H__
#define __COMMANDS_H__

#include <map>

using namespace std;

#include "inode.h"
#include "trace.h"
#include "util.h"

//
// A couple of convenient typedefs to avoid verbosity.
//

typedef void (*function) (inode_state &state, const wordvec &words);
typedef map<string, function> commandmap;

//
// commands -
//    A class to hold and dispatch each of the command functions.
//    Each command "foo" is interpreted by a function fn_foo.
// ctor -
//    The default ctor initializes the map.
// operator[] -
//    Given a string, returns a function associated with it,
//    or 0 if not found.
//

class commands {
   private:
      commands (const inode &); // disable copy ctor
      commands &operator= (const inode &); // disable operator=
      commandmap map;
   public:
      commands();
      function operator[] (const string &cmd);
      static string prompt;
};

//
// execution functions -
//    See the man page for a description of each of these functions.
//

void fn_cat    (inode_state &state, const wordvec &words);
void fn_cd     (inode_state &state, const wordvec &words);
void fn_echo   (inode_state &state, const wordvec &words);
void fn_exit   (inode_state &state, const wordvec &words);
void fn_ls     (inode_state &state, const wordvec &words);
void fn_lsr    (inode_state &state, const wordvec &words);
void fn_make   (inode_state &state, const wordvec &words);
void fn_mkdir  (inode_state &state, const wordvec &words);
void fn_prompt (inode_state &state, const wordvec &words);
void fn_pwd    (inode_state &state, const wordvec &words);
void fn_rm     (inode_state &state, const wordvec &words);
void fn_rmr    (inode_state &state, const wordvec &words);

//
// exit_status_message -
//    Prints an exit message and returns the exit status, as recorded
//    by any of the functions.
//

int exit_status_message();
class ysh_exit_exn: public exception {};

#endif

