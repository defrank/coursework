/* $Id: interp.h,v 1.3 2011-02-14 05:13:02-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    interp - header file for interp
 *
 * DESCRIPTION
 *    Takes a command and processes it, throwing a runtime error if it
 *    recognizes that something goes wrong.  Takes care of writing the
 *    Postscript prolog, epilog, and page boundaries.
 */

#ifndef __INTERP_H__
#define __INTERP_H__

#include <iostream>
#include <map>

using namespace std;

#include "object.h"
#include "trace.h"

typedef map <string, object *> objectmap;

class interpreter {
   public:
      typedef list<string> parameters;
      interpreter (const string &, ostream &, objectmap &);
      ~interpreter ();
      void interpret (parameters &);
   private:
      interpreter (); // Disable
      interpreter (const interpreter &); // Disable
      interpreter &operator= (const interpreter &); // Disable

      // Data fields.
      typedef void (interpreter::*interpreterfn) (parameters &);
      typedef object *(interpreter::*factoryfn) (parameters &);
      static map <string, interpreterfn> interpmap;
      static map <string, factoryfn> factorymap;
      ostream &outfile;
      int pagenr;
      objectmap objmap; // Maps names of defined objects onto the shapes
      string infilename;
      double page_xoffset;
      double page_yoffset;

      // Service functions.
      void do_define (parameters &);
      void do_draw (parameters &);
      void do_newpage (parameters &);
      void prolog ();
      void startpage ();
      void endpage ();
      void epilog ();

      // Factory functions.
      object *make_object (parameters &);
      object *make_text (parameters &);
      object *make_ellipse (parameters &);
      object *make_circle (parameters &);
      object *make_polygon (parameters &);
      object *make_rectangle (parameters &);
      object *make_square (parameters &);
      object *make_line (parameters &);
};

#endif

