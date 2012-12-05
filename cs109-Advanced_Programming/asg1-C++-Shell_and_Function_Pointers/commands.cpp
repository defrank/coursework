/* $Id: commands.cpp,v 1.8 2011-01-27 20:24:24-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   commands
 *
 * DESCRIPTION
 *   
 */

#include "commands.h"
#include "trace.h"

commands::commands(): map (commandmap()) {
   map["cat"    ] = fn_cat    ;
   map["cd"     ] = fn_cd     ;
   map["echo"   ] = fn_echo   ;
   map["exit"   ] = fn_exit   ;
   map["ls"     ] = fn_ls     ;
   map["lsr"    ] = fn_lsr    ;
   map["make"   ] = fn_make   ;
   map["mkdir"  ] = fn_mkdir  ;
   map["prompt" ] = fn_prompt ;
   map["pwd"    ] = fn_pwd    ;
   map["rm"     ] = fn_rm     ;
}

function commands::operator[] (const string& cmd) {
   return map[cmd];
}

void fn_cat (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

void fn_cd (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

void fn_echo (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
   if (words.size() > 1) {
      for (unsigned int i = 1; i < words.size(); ++i) {
         cout << words[i] << " ";
      }
   }
   cout << endl;
}

void fn_exit (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
   
   if (words.size() > 2) {
      // set exit status to 255 if more than one argument is present
      exit_status::set(255);
   }else if (words.size() == 1){
      // no status given, set to 0
      // exit_status::set(0);
   }else {
      // set exit status to 255 if status is non-numeric
      // or convert status to int and set as exit status
      for (unsigned int i = 0; i < words[1].length(); ++i) {
         if (!std::isdigit(words[1][i])) {
            // status can be negative
            if (i == 0 && words[1][0] == '-') {
            }else {
               exit_status::set(255);
               throw ysh_exit_exn ();
            }
         }
      }
      // convert status from string to int then set as status
      int status = str2int(words[1]);
      exit_status::set(status);
   }
   // throw exception of given status
   throw ysh_exit_exn ();
}

void fn_ls (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
   
   inode current = *(state.get_cwd());
   if (current.get_type() == DIR_INODE) {
      cout << state.get_cwd_names() << ":" << endl;
      current.print_map(current.get_dirents());
   }else if (current.get_type() == FILE_INODE) {
      
   }
}

void fn_lsr (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

void fn_make (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

void fn_mkdir (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
   
   inode newdir (DIR_INODE);
   
   if (words.size() < 2) {
      throw yshell_exn (words[0] + ": no pathname specified");
   }
   if (words[1][0] == '/') {
      // pathname begins at root
      string root = (*(state.get_root())).get_dirname();
      
   }else if (words[1][0] == '.' && words[1][1] != '.') {
      // pathname begins with current directory
      
   }else if (words[1][0] == '.' && words[1][1] == '.') {
      //pathname begins with parent directory
      
   }else {
      size_t found;
      string path;
      inode current = *(state.get_cwd());
      unsigned int count = 1;
      // check if the pathname already exists, if so, print to cerr.
      // otherwise create directory in specified pathname
      if (words[1].length() > 0) {
         found= words[1].find_first_of ("/");
         path = words[1].substr(0,found);
      }
      while (count <= words[1].length()) {
         directory *entries = current.get_dirents();
         directory::const_iterator itor = entries->begin();
         for (; itor != entries->end(); ++itor) {
            if (path.compare (itor->first) == 0) {
               if (found == words[1].length()) {
                  throw yshell_exn (words[0]
                                    + ": directory already exists");
               }else {
                  current = *(itor->second);
                  break;
               }
            }
         }
         // search for next "/" in pathname
         size_t temp = found;
         found =  words[1].find_first_of ("/", found+1);
         path = words[1].substr(temp+1,found);
         ++count;
      }
   }
   
   // newdir.alter_dirname (words[1]);
   newdir.insert_dirents (".", &newdir);
   newdir.insert_dirents ("..", &newdir);
}

void fn_prompt (inode_state &state, const wordvec &words){ 
   TRACE ('c', state);
   TRACE ('c', words);
   
   string newprompt = "";
   if ( words.size() > 1 ) {
      for (unsigned int i = 1; i < words.size(); ++i) {
         if (i != words.size()-1) {
            newprompt.append (words[i]+" ");
         }else {
            newprompt.append (words[i]);
         }
      }
   }
   state.alter_prompt (newprompt);
}

void fn_pwd (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
   
   cout << state.get_cwd_names() << endl;
}

void fn_rm (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

void fn_rmr (inode_state &state, const wordvec &words) {
   TRACE ('c', state);
   TRACE ('c', words);
}

int exit_status_message() {
   int exit_status = exit_status::get();
   cout << execname() << ": exit(" << exit_status << ")" << endl;
   return exit_status;
}
