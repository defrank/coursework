/* $Id: numbers.cpp,v 1.3 2011-02-14 00:25:25-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    numbers - implemention file
 *
 * DESCRIPTION
 *    Implementation of classes degrees, points, and inches.
 */

#include <cstdlib>

using namespace std;

#include "numbers.h"
#include "util.h"

ostream &operator<< (ostream &out, const degrees &that) {
   out << that.angle << "deg";
   return out;
}

ostream &operator<< (ostream &out, const points &that) {
   out << that.pointvalue << "pt";
   return out;
}

ostream &operator<< (ostream &out, const inches &that) {
   out << that.pointvalue / PTS_PER_INCH << "in";
   return out;
}

ostream &operator<< (ostream &out, const xycoords &coords) {
   out << "(" << coords.first << "," << coords.second << ")";
   return out;
}

