/* $Id: util.h,v 1.2 2011-03-01 18:12:10-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * util -
 *   A utility class to provide various services not conveniently
 *   included in other modules. 
 */

#ifndef __UTIL_H__
#define __UTIL_H__

#include <iostream>

#ifdef __GNUC__
#include <stdexcept>
#endif

using namespace std;

//
// list_exn -
//    Indicate a problem where processing should be abandoned and
//    the main function should take control.
//

class list_exn: public runtime_error {
   public:
      explicit list_exn (const string &what);
};

class list_exit_exn: public exception {};

//
// octal -
//    Convert integer to octal string.
//

const string octal (int decimal);

//
// sys_info -
//    Keep track of execname and exit status.  Must be initialized
//    as the first thing done inside main.  Main should call:
//       sys_info::set_execname (argv[0]);
//    before anything else.
//

class sys_info {
   private:
      static string execname;
      static int exit_status;
   public:
      static void set_execname (const string &argv0);
      static const string &get_execname () {return execname; }
      static void set_status (int status) {exit_status = status; }
      static int get_status () {return exit_status; }
};

//
// complain -
//    Used for starting error messages.  Sets the exit status to
//    EXIT_FAILURE, writes the program name to cerr, and then
//    returns the cerr ostream.  Example:
//       complain() << filename << ": some problem" << endl;
//

ostream &complain();

//
// string to_string (thing) -
//    Convert anything into a string if it has an ostream<< operator.
//

template <typename T>
string to_string (const T &);

//
// thing from_string (cons string &) -
//    Scan a string for something if it has an istream>> operator.
//

template <typename T>
T from_string (const string &);


#include "util.cpp"


#endif
