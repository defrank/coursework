/* $Id: interp.cpp,v 1.29 2011-02-14 23:34:10-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    interp
 *
 * DESCRIPTION
 *    Implementation for interp class.
*/

#include <list>
#include <map>
#include <string>

using namespace std;

#include "interp.h"
#include "object.h"
#include "util.h"

//
// ctor
//    Constructor for the interpreter class.
//
interpreter::interpreter(const string &filename, ostream &outfile,
                         objectmap &objmap):
   outfile(outfile), pagenr(1), objmap(objmap), infilename(filename),
   page_xoffset (inches (.25)), page_yoffset (inches (.25)) {
   if (interpmap.size() == 0) {
      interpmap["define" ] = &interpreter::do_define ;
      interpmap["draw"   ] = &interpreter::do_draw   ;
      interpmap["newpage"] = &interpreter::do_newpage;
   }
   if (factorymap.size() == 0) {
      factorymap["text"     ] = &interpreter::make_text     ;
      factorymap["ellipse"  ] = &interpreter::make_ellipse  ;
      factorymap["circle"   ] = &interpreter::make_circle   ;
      factorymap["polygon"  ] = &interpreter::make_polygon  ;
      factorymap["rectangle"] = &interpreter::make_rectangle;
      factorymap["square"   ] = &interpreter::make_square   ;
      factorymap["line"     ] = &interpreter::make_line     ;
   }
   prolog ();
   startpage ();
}

//
// dtor
//    Destructor for the interpreter class.
//
interpreter::~interpreter () {
   endpage ();
   epilog ();

   objectmap::iterator itor = objmap.begin ();
   for (; itor != objmap.end (); ++itor) {
      delete itor->second;
   }
}

//
// interpmap
//    Map that selects functions to carry out the various commands.
//
map <string, interpreter::interpreterfn> interpreter::interpmap;

//
// factorymap
//    Map that contains pointers to the carious factory functions.
//
map <string, interpreter::factoryfn> interpreter::factorymap;

//
// shift
//    Function that pops the front of a list of strings.  Returns the
//    front of the list as a string.
//
string shift (list<string> &words) {
   if (words.size() == 0) throw runtime_error ("syntax error");
   string front = words.front();
   words.pop_front();
   return front;
}

//
// interpret
//    Looks up a function based on a command and call
//
void interpreter::interpret (parameters &params) {
   TRACE ('i', params);
   string command = shift (params);
   interpreterfn function = interpmap[command];
   if (function == NULL) throw runtime_error ("syntax error");
   (this->*function) (params);
}

//
// do_define
//    Takes care of accessing the factory to make new objects.
//
void interpreter::do_define (parameters &params) {
   TRACE ('i', params);
   string name = shift (params);
   objmap[name] = make_object (params);
}

//
// do_draw
//    Takes care of accessing the factory and telling objects when to
//    draw themselves.
//
void interpreter::do_draw (parameters &params) {
   TRACE ('i', params);
   string name = shift (params);
   object *thing = objmap[name];
   if (thing == NULL) throw runtime_error (name + ": no such object");
   degrees angle = degrees (0);

   // If there exists three parameters, try to convert the last to a
   // double and store it as the angle.  Catch and throw any syntax
   // errors.
   if (params.size() == 3) {
      try {
         angle = degrees (from_string<double> (params.back()));
         params.pop_back();
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }

   // Unable to use try-catch statement to catch any syntax errors.
   if (params.size() != 2) { throw runtime_error ("syntax error"); }
   xycoords coords (inches (from_string<double> (params.front () )),
                    inches (from_string<double> (params.back () )));
   
   thing->draw (outfile, coords, angle);
}

//
// do_newpage
//    Ends and starts a new page for drawing.  Throws a syntax error if
//    the command list is not empty.
//
void interpreter::do_newpage (parameters &params) {
   if (params.size() != 0) throw runtime_error ("syntax error");
   endpage ();
   ++pagenr;
   startpage ();
}

//
// prolog
//    Writes the prolog comments for a new Postscript program.
//
void interpreter::prolog () {
   outfile << "%!PS-Adobe-3.0" << endl;
   outfile << "%%Creator: " << sys_info::get_execname () << endl;
   outfile << "%%CreationDate: " << datestring() << endl;
   outfile << "%%PageOrder: Ascend" << endl;
   outfile << "%%Orientation: Portrait" << endl;
   outfile << "%%SourceFile: " << infilename << endl;
   outfile << "%%EndComments" << endl;
}

//
// startpage
//    Starts a new Postscript page.
//
void interpreter::startpage () {
   outfile << endl
           << "%%Page: " << pagenr << " " << pagenr << endl
           << page_xoffset << " " << page_yoffset
                           << " translate" << endl
           << "/Courier findfont 10 scalefont setfont" << endl
           << "0 0 " << "moveto" << endl
           << "(" << infilename << ":" << pagenr << ") show" << endl;
}

//
// endpage
//    Ends a Postscript page.
//
void interpreter::endpage () {
   outfile << endl;
   outfile << "showpage" << endl;
   outfile << "grestoreall" << endl;
}

//
// epilog
//    Writes the epilog to a Postscript program.
//
void interpreter::epilog () {
   outfile << endl;
   outfile << "%%Trailer" << endl;
   outfile << "%%Pages: " << pagenr << endl;
   outfile << "%%EOF" << endl;

}

//
// make_object
//    Determines what kind of object needs to be created, then calles an
//    appropriate factory function.
//
object *interpreter::make_object (parameters &command) {
   TRACE ('f', command);
   string type = shift (command);
   factoryfn func = factorymap[type];
   if (func == NULL) throw runtime_error (type + ": no such object");
   return (this->*func) (command);
}

//
// make_text
//    Create and return a pointer to a text object initialized to the
//    given list of parameters.  Scan the parameter list for validity
//    and throw a syntax error if the number of parameters is incorrect,
//    or if a non-number is given for any argument other than the font
//    name and words of a text.
//
object *interpreter::make_text (parameters &command) {
   TRACE ('f', command);
   if (command.size () < 2)
      throw runtime_error ("syntax error");

   // Pop the next string argument from the command list.  Try to
   // convert it to a double and set it as the font size.  If it
   // contains non-numbers, from_string() will throw an error.  Catch
   // the error and set the string as the font style.  Set the font size
   // to the default of 12.0 points.
   double size;
   string temp;
   try {
      temp = shift (command);
      size = from_string<double> (temp);
      temp = shift (command);
   } catch (domain_error error) {
      size = 12.0;
   }
    string font = temp;
   
   // Throw a syntax error if the font style contains any numbers.
   string::iterator itor = font.begin ();
   for (; itor != font.end (); ++itor) {
      unsigned char ctemp = *itor;
      int dtemp = ctemp -'0';
      if (0 <= dtemp && dtemp <= 9)
         throw runtime_error ("syntax error"); 
   }
 
   // Loop through the rest of the command list.  Pop and append each
   // string to the text string.
   string txt = shift (command);
   while (!command.empty ()) {
      string temp = shift (command);
      txt.append (" " + temp);
   }
   
   // Iterate throw the text string and put a '\' in front of all '\',
   // '(', or ')'.
   string::iterator itor2 = txt.begin ();
   for (; itor2 != txt.end (); ++itor2) {
      if ((*itor2 == '\\') || (*itor2 == '(') || (*itor2 == ')')) {
         itor2 = txt.insert (itor2, '\\');
         ++itor2;
      }
   }

   return new text (font, points (size), txt);
}

//
// make_ellipse
//    Create and return a pointer to an ellipse object initialized to
//    the given list of parameters.  Scan the parameter list for
//    validity and throw a syntax error if the number of parameters is
//    incorrect, or if a non-number is given for any argument.
//
object *interpreter::make_ellipse (parameters &command) {
   TRACE ('f', command);
   if (command.size () < 2 || command.size () > 3)
      throw runtime_error ("syntax error");

   // Pop the front two strings from the command list.  Try to convert
   // them to a double and set them as height and width respectively.
   // If an error is caught, throw a syntax error.
   double height, width;
   try {
      string temp1 = shift (command);
      string temp2 = shift (command);
      height = from_string<double> (temp1);
      width = from_string<double> (temp2);
   } catch (domain_error error) {
      throw runtime_error ("syntax error");
   }
   
   // Check if the command list is empty.  If so, set the thickness to
   // the default size of 2.0 points.  If not, pop the thickness
   // string.  Try to convert it to a double.  If any errors are caught,
   // throw a syntax error.
   double thick;
   if (command.empty ()) { thick = 2.0; }
   else {
      try {
         string temp = shift (command);
         thick = from_string<double> (temp);
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }
   
   return new ellipse (inches (height), inches (width), points (thick));
}

//
// make_circle
//    Create and return a pointer to a circle object initialized to the
//    given list of parameters.  Scan the parameter list for validity
//    and throw a syntax error if the number of parameters is incorrect,
//    or if a non-number is given for any argument.
//
object *interpreter::make_circle (parameters &command) {
   TRACE ('f', command);
   if (command.empty () || command.size () > 2)
      throw runtime_error ("syntax error");

   // Pop the diameter string from the command list.  Try to convert the
   // string to a double.  If any errors are caught, throw a syntax
   // error.
   double diameter;
   try {
      string temp = shift (command);
      diameter = from_string<double> (temp);
   } catch (domain_error error) {
      throw runtime_error ("syntax error");
   }
   
   // Check if the command list is empty.  If so, set the thickness to
   // the default size of 2.0 points.  If not, pop the thickness string
   // and try to convert it to a double.  If any errors are caught,
   // throw a syntax error.
   double thick;
   if (command.empty ()) { thick = 2.0; }
   else {
      try {
         string temp = shift (command);
         thick = from_string<double> (temp);
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }
   
   return new circle (inches (diameter), points (thick));
}

//
// make_polygon
//    Create and return a pointer to a polygon object initialized to the
//    given list of parameters.  Scan the parameter list for validity
//    and throw a syntax error if the number of parameters is incorrect,
//    or if a non-number is given for any argument.
//
object *interpreter::make_polygon (parameters &command) {
   TRACE ('f', command);
   if (command.size () <= 1) throw runtime_error ("syntax error");

   // Iterate through the command list.  Try to convert a pair of
   // strings to doubles.  Push the pair of doubles onto a coordinate
   // list.  If any errors are caught, throw a syntax error. A single
   // left over command is considered the thickness.  If no thickness
   // is given, set to the default of 2.0 points.  Try to convert to a
   // double as well and throw a syntax error for any errors caught.
   coordlist coords;
   double thick;
   while (!command.empty ()) {
      if (command.size () >= 2) {
         try {
            string tempx = shift (command);
            string tempy = shift (command);
            double x = from_string<double> (tempx);
            double y = from_string<double> (tempy);
            coords.push_back (xycoords (inches (y), inches (x)));
         } catch (domain_error error) {
            throw runtime_error ("syntax error");
         }
      }else if (command.size () == 1) {
         try {
            string temp = shift (command);
            thick = from_string<double> (temp);
         } catch (domain_error error) {
            throw runtime_error ("syntax error");
         }
      }else { thick = 2.0; }
   }
   
   return new polygon (coords, points (thick));
}

//
// make_rectangle
//    Create and return a pointer to a rectangle object initialized to
//    the  given list of parameters.  Scan the parameter list for
//    validity and throw a syntax error if the number of parameters is
//    incorrect, or if a non-number is given for any argument.
//
object *interpreter::make_rectangle (parameters &command) {
   TRACE ('f', command);
   if (command.size () < 2 || command.size () > 3)
      throw runtime_error ("syntax error");

   // Pop the first two string arguments from the command list.  Try to
   // convert them to double.  If any errors are caught, throw a syntax
   // error.
   double height, width;
   try {
      string temp1 = shift (command);
      string temp2 = shift (command);
      height = from_string<double> (temp1);
      width = from_string<double> (temp2);
   } catch (domain_error error) {
      throw runtime_error ("syntax error");
   }

   // Check if the command list is empty.  If so, set the thickness to
   // the default of 2.0 points.  If not, pop the thickness string from
   // the command list.  Try to convert the string to a double.  If any
   // errors are caught, throw a syntax error.
   double thick;
   if (command.empty ()) { thick = 2.0; }
   else {
      try {
         string temp = shift (command);
         thick = from_string<double> (temp);
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }
   
   return new rectangle (inches(height), inches(width), points(thick));
}

//
// make_square
//    Create and return a pointer to a square object initialized to the
//    given list of parameters.  Scan the parameter list for validity
//    and throw a syntax error if the number of parameters is incorrect,
//    or if a non-number is given for any argument.
//
object *interpreter::make_square (parameters &command) {
   TRACE ('f', command);
   if (command.empty () || command.size () > 2)
      throw runtime_error ("syntax error");

   // Pop the width string from the command list.  Try to convert the
   // string to a double.  If any errors are caught, throw a syntax
   // error.
   double width;
   try {
      string temp = shift (command);
      width = from_string<double> (temp);
   } catch (domain_error error) {
      throw runtime_error ("syntax error");
   }
   
   // Check if the command list is empty.  If so, set the thickness to
   // the default of 2.0 points.  If not, pop the thickness string from
   // the command list.  Try to convert the string to a double.  If any
   // errors are caught, throw a syntax error.
   double thick;
   if (command.empty ()) { thick = 2.0; }
   else {
      try {
         string temp = shift (command);
         thick = from_string<double> (temp);
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }
   
   return new square (inches (width), points (thick));
}

//
// make_line
//    Create and return a pointer to a line object initialized to the
//    given list of parameters.  Scan the parameter list for validity
//    and throw a syntax error if the number of parameters is incorrect,
//    or if a non-number is given for any argument.
//
object *interpreter::make_line (parameters &command) {
   TRACE ('f', command);
   if (command.empty () || command.size () > 2)
      throw runtime_error ("syntax error");

   // Pop the length string from the command list.  Try to convert the
   // string to a double.  If any errors are caught, throw a syntax
   // error.
   double length;
   try {
      string temp = shift (command);
      length = from_string<double> (temp);
   } catch (domain_error error) {
      throw runtime_error ("syntax error");
   }
   
   // Check if the command list is empty.  If so, set the thickness to
   // the default of 2.0 points.  If not, pop the thickness string from
   // the command list.  Try to convert the string to a double.  If any
   // errors are caught, throw a syntax error.
   double thick;
   if (command.empty ()) { thick = 2.0; }
   else {
      try {
         string temp = shift (command);
         thick = from_string<double> (temp);
      } catch (domain_error error) {
         throw runtime_error ("syntax error");
      }
   }
   
   return new line (inches (length), points (thick));
}
