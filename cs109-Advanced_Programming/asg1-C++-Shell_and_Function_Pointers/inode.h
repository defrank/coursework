/* $Id: inode.h,v 1.5 2011-01-19 09:26:15-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * inode
 */

#ifndef __INODE_H__
#define __INODE_H__

#include <exception>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

#include "trace.h"
#include "util.h"

//
// inode_t -
//    An inode is either a directory or a plain file.
//

enum inode_t {DIR_INODE, FILE_INODE};

//
// directory -
//    A directory is a list of paired strings (filenames) and inodes.
//    An inode in a directory may be a directory or a file.
//

class inode;
typedef map<string, inode *> directory;

//
// class inode -
//
// inode ctor -
//    Create a new inode of the given type, using a union.
//
// get_inode_nr -
//    Retrieves the serial number of the inode.  Inode numbers are
//    allocated in sequence by small integer.
//
// size -
//    Returns the size of an inode.  For a directory, this is the
//    number of dirents.  For a text file, the number of characters
//    when printed (the sum of the lengths of each word, plus the
//    number of words.
//
// readfile -
//    Returns a copy of the contents of the wordvec in the file.
//    Throws an yshell_exn for a directory.
//
// writefile -
//    Replaces the contents of a file with new contents.
//    Throws an yshell_exn for a directory.
//
// remove -
//    Removes the file or subdirectory from the current inode.
//    Throws an yshell_exn if this is not a directory, the file
//    does not exist, or the subdirectory is not empty.
//    Here empty means the only entries are dot (.) and dotdot (..).
//
// mkdir -
//    Creates a new directory under the current directory and 
//    immediately adds the directories dot (.) and dotdot (..) to it.
//    Note that the parent (..) of / is / itself.  It is an error
//    if the entry already exists.
//
// mkfile -
//    Create a new empty text file with the given name.  Error if
//    a dirent with that name exists.
//  
//    

class inode {
   private:
      /** Private Fields **/
      int inode_nr;
      inode_t type;
      union {
         directory *dirents;
         wordvec *data;
      } contents;
      static int next_inode_nr;
      string dirname;
   public:
      /** Constructors **/
      inode (inode_t init_type);
      inode (const inode &source);
      inode &operator= (const inode &from);
      /** Access Functions **/
      int get_inode_nr();
      int size();
      const wordvec &readfile() const;
      string get_dirname();
      inode_t get_type();
      directory* get_dirents();
      /** Manipulation Functions **/
      void writefile (const wordvec &newdata);
      void remove (const string &filename);
      void alter_dirname (string newname); // change the name of inode
      void insert_dirents (string newname, inode *newinode);
      void print_map (directory *entries);
};

//
// inode_state -
//    A small convenient class to maintain the state of the simulated
//    process:  the root (/), the current directory (.), and the
//    prompt.
//

class inode_state {
   friend class inode;
   friend ostream &operator<< (ostream &out, const inode_state &);
   private:
      /** Constructors **/
      inode_state (const inode_state &); // disable copy ctor
      inode_state &operator= (const inode_state &); // disable op=
      /** Private Fields **/
      inode *root;
      inode *cwd;
      string prompt;
      vector<string> cwd_names; // push or pop current directory name
   public:
      inode_state();
      /** Access Functions **/
      inode* get_root(); // return pointer to root inode
      inode* get_cwd(); // return pointer to cwd inode
      string get_prompt(); // return prompt string
      string get_cwd_names(); // return cwd string from cwd_names
      /** Manipulation Functions **/
      void alter_root (inode* newroot); // change the root directory
      void alter_cwd (inode* newcwd); // change the current directory
      void alter_prompt (string newprompt); // change the prompt
      void push_cwd (string cdirname); // push onto cwd_names
      void pop_cwd(); // pop from cwd_names
};

ostream &operator<< (ostream &out, const inode_state &);

#endif

