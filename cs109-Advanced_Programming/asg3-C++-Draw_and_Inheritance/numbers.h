/* $Id: numbers.h,v 1.4 2011-02-14 23:57:05-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    numbers - header file
 *
 * DESCRIPTION
 *    Classes defining degrees, points, and inches as doubles and in
 *    format that is readible by Postscript.
 */

#ifndef __NUMBERS_H__
#define __NUMBERS_H__

#include <iostream>
#include <utility>

using namespace std;

#include "trace.h"

const double PTS_PER_INCH = 72;

class degrees {
   friend ostream &operator<< (ostream &, const degrees &);
   public:
      explicit degrees (double init): angle(init) {}
      operator double() const {return angle; }
   private:
      degrees (); // Disable.
      double angle;
};

class points {
   friend ostream &operator<< (ostream &, const points &);
   public:
      explicit points (double init): pointvalue(init) {}
      operator double() const {return pointvalue; }
   private:
      points (); // Disable.
      double pointvalue;
};

class inches {
   friend ostream &operator<< (ostream &, const inches &);
   public:
      explicit inches (double init): pointvalue(init * PTS_PER_INCH) {}
      operator double() const {return pointvalue; }
   private:
      inches (); // Disable.
      double pointvalue;
};

typedef pair <inches, inches> xycoords;

ostream &operator<< (ostream &, const xycoords &);

#endif

