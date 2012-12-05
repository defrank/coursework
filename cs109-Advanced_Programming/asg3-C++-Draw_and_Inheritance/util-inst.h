/* $Id: util-inst.h,v 1.3 2011-02-14 23:57:05-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    util-inst - header file
 *
 * DESCRITION
 *    Placed at the end of util.cpp since explicit instantiations
 *    must appear in the implementation file after the body of the
 *    instantiated entity.
 */

// 
// Explicit template instantiation.
//
// G++ is not always able to automatically instantiate certain
// templates, and so they must be instantiated explicitly.
// The errors that indicate that explicit instantiation is
// necessary occur at link time, not at run time, and produce
// messages like the following:
//
// object.cc:43: undefined reference to `std::basic_ostream<char,
// std::char_traits<char> >& operator<< <std::pair<inches,
// inches> >(std::basic_ostream<char, std::char_traits<char>
// >&, std::list<std::pair<inches, inches>,
// std::allocator<std::pair<inches, inches> > > const&)'
// collect2: ld returned 1 exit status
//
// Explicit instantiations must appear in the implementation file
// after the body of the instantiated entity.  In order to separate
// concerns, we put this information in a header file which is
// included at the *end* of the implementation file.
//

#include "numbers.h"

template ostream &operator<< (ostream &, const list<xycoords> &);
template double from_string <double> (const string &);

