/* $Id: inode.cpp,v 1.4 2011-01-19 09:26:15-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   inode
 *
 * DESCRIPTION
 *   
 */

#include <cassert>
#include <iostream>

using namespace std;

#include "trace.h"
#include "inode.h"

int inode::next_inode_nr = 1;

/********** Constructors - inode **********/

inode::inode (inode_t init_type):
   inode_nr (next_inode_nr++), type (init_type)
{
   switch (type) {
      case DIR_INODE:
           contents.dirents = new directory();
           break;
      case FILE_INODE:
           contents.data = new wordvec();
           break;
   }
   TRACE ('i', "inode " << inode_nr << ", type = " << type);
}

//
// copy ctor -
//    Make a copy of a given inode.  This should not be used in
//    your program if you can avoid it, since it is expensive.
//    Here, we can leverage operator=.
//
inode::inode (const inode &that) {
   *this = that;
}

//
// operator= -
//    Assignment operator.  Copy an inode.  Make a copy of a
//    given inode.  This should not be used in your program if
//    you can avoid it, since it is expensive.
//
inode &inode::operator= (const inode &that) {
   if (this != &that) {
      inode_nr = that.inode_nr;
      type = that.type;
      contents = that.contents;
   }
   TRACE ('i', "inode " << inode_nr << ", type = " << type);
   return *this;
}

/********** Access Functions - inode **********/

int inode::get_inode_nr() {
   TRACE ('i', "inode = " << inode_nr);
   return inode_nr;
}

int inode::size() {
   int size = 0;
   TRACE ('i', "size = " << size);
   
   if (type == DIR_INODE) {
      size = contents.dirents->size();
   }else if (type == FILE_INODE) {
      
   }
   return size;
}

const wordvec &inode::readfile() const {
   TRACE ('i', *contents.data);
   assert (type == FILE_INODE);
   return *contents.data;
}

string inode::get_dirname() {
   TRACE ('i', dirname);
   return dirname;
}

inode_t inode::get_type() {
   return type;
}

directory* inode::get_dirents() {
   return contents.dirents;
}

/********** Manipulation Functions - inode **********/

void inode::writefile (const wordvec &words) {
   TRACE ('i', words);
   assert (type == FILE_INODE);
}

void inode::remove (const string &filename) {
   TRACE ('i', filename);
   assert (type == DIR_INODE);
}

void inode::alter_dirname (string newname) {
   dirname.erase();
   dirname.append(newname);
}

void inode::insert_dirents (string newname, inode *newinode) {
   assert (type == DIR_INODE);
   contents.dirents->insert (pair<string, inode *>(newname, newinode));
}

/********** Other Functions - inode **********/

//
// print_map
//    Print the contents of a map.
//
// * Partially written by Wesley Mackey in mapitor.cpp.
//
void inode::print_map (directory *entries) {
   directory::const_iterator itor = entries->begin();
   for (; itor != entries->end(); ++itor) {
      cout << "     " << itor->second->get_inode_nr() << "     "
      << itor->second->size() << "  " << itor->first << endl;
   }
}

/********** Constructors - inode_state **********/

inode_state::inode_state(): root (NULL), cwd (NULL), prompt ("%") {
   TRACE ('i', "root = " << (void*) root << ", cwd = " << (void*) cwd
          << ", prompt = " << prompt);
}

/********** Access Functions  - inode_state **********/

inode* inode_state::get_root() {
   return root;
}

inode* inode_state::get_cwd() {
   return cwd;
}

string inode_state::get_prompt() {
   return prompt;
}

string inode_state::get_cwd_names() {
   string name = cwd_names[cwd_names.size()-1];
   return name;
}

/********** Manipulation Functions - inode_state **********/

void inode_state::alter_root (inode* newroot) {
   root = newroot;
}

void inode_state::alter_cwd (inode* newcwd) {
   cwd = newcwd;
}

void inode_state::alter_prompt (string newprompt) {
   prompt.erase();
   prompt.append(newprompt);
}

void inode_state::push_cwd (string cdirname) {
   cwd_names.push_back(cdirname);
}

void inode_state::pop_cwd() {
   // The parent of root is root so do not remove it
   if (cwd_names.size() > 1)
      cwd_names.pop_back();
}

/********** Other Functions - inode_state **********/

ostream &operator<< (ostream &out, const inode_state &state) {
   out << "inode_state: root = " << state.root
       << ", cwd = " << state.cwd;
   return out;
}

