/* $Id: object.h,v 1.4 2011-02-14 22:47:07-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    object - header file
 *
 * DESCRIPTION
 *    An inheritance hierarchy with base object.
 */

#ifndef __OBJECT_H__
#define __OBJECT_H__

#include <iomanip>
#include <iostream>
#include <list>
#include <utility>

using namespace std;

#include "numbers.h"

//
// Objects constitute a single-inheritance hierarchy, summarized
// here, with the superclass listed first, and subclasses indented
// under their immediate superclass.
//
// object
//    text
//    shape
//       ellipse
//          circle
//       polygon
//          rectangle
//             square
//          line
//

typedef list<xycoords> coordlist;

//
// Class object
//    Abstract base class for all shapes in this system.
//
class object {
   public:
      virtual ~object ();
      virtual void draw (ostream &, const xycoords &,
                         const degrees &angle) = 0;
   protected:
      object () {}
};


//
// Class text
//
class text: public object {
   public:
      text (const string &fontname, const points &fontsize,
            const string &textdata);
      virtual void draw (ostream &, const xycoords &,
                         const degrees &angle);
   protected:
      string fontname;
      points fontsize;
      string textdata;
};

//
// Class shape
//
class shape: public object {
   protected:
      shape (const points &thick): thick(thick) {}
      points thick;
   private:
      shape (); // Disable.
};

//
// Class ellipse
//
class ellipse: public shape {
   public:
      ellipse (const inches &height, const inches &width,
               const points &thick);
      virtual void draw (ostream &, const xycoords &,
                         const degrees &angle);
   protected:
      inches height;
      inches width;
};

//
// Class circle
//
class circle: public ellipse {
   public:
      circle (const inches &diameter, const points &thick);
};

//
// Class polygon
//
class polygon: public shape {
   public:
      polygon (const coordlist &coords, const points &thick);
      virtual void draw (ostream &, const xycoords &,
                         const degrees &angle);
   protected:
      const coordlist coordinates;
};

//
// Class rectangle
//
class rectangle: public polygon {
   public:
      rectangle (const inches &height, const inches &width,
                 const points &thick);
   private:
      static coordlist make_list (
             const inches &height, const inches &width);
};

//
// Class square
//
class square: public rectangle {
   public:
      square (const inches &width, const points &thick);
};

//
// Class line
//
class line: public polygon {
   public:
      line (const inches &length, const points &thick);
   private:
      static coordlist make_list (const inches &length);
};

#endif

