/* $Id: bigint.cpp,v 1.10 2012-12-04 19:40:04-08 - - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   bigint
 *
 * DESCRIPTION
 *
 */

#include <cstdlib>
#include <exception>
#include <limits>
#include <climits>
#include <stack>
#include <stdexcept>

using namespace std;

#include "bigint.h"
#include "trace.h"

//
// ctor
//    initializes and empty bigint
//
bigint::bigint (): negative (false), big_value (new bigvalue_t ()) {
}

//
// ctor
//    copies bigint parameter values and sets this equal to them
//
bigint::bigint (const bigint &that): negative (false), 
                  big_value (new bigvalue_t (*that.big_value)) {
                     
   this->negative = that.negative;
}

//
// operator=
//    first checks if this is equal to the bigint parameter.  if not,
//    then deletes the current values for this.  next copies the bigint
//    parameter values and sets this equal to them
//
bigint &bigint::operator= (const bigint &that) {
   if (this == &that) return *this;
   
   delete this->big_value;
   this->negative = that.negative;
   this->big_value = new bigvalue_t (*that.big_value);
   return *this;
}

//
// dtor
//    uses delete on any fields that used new
//
bigint::~bigint() {
   TRACE ('~', cout << *this);
   delete this->big_value;
}

//
// ctor
//    creates a bigint by converting an int to a bigvalue_t
//
bigint::bigint (int that): negative (false),
                  big_value (new bigvalue_t ()) {

   unsigned char currc;
   if (that == 0) {
      currc = '0';
      this->big_value->push_back (currc);
   }else {
      if (that < 0) {
         this->negative = true;
         that *= -1;
      }
      int curr = 0;
      int num = that;
      while (num != 0) {
         curr = num % 10;
         num -= curr;
         num /= 10;
         currc = curr + '0';
         this->big_value->push_back (currc);
      }
   }
}

//
// ctor
//    inserts a string as a bigint value by first checking the sign of
//    the string, then pushes each char starting from the back of the
//    string onto the bigvalue_t
//
bigint::bigint (const string &that): negative (false),
                  big_value (new bigvalue_t ()) {
   TRACE ('b', that);

   string::const_iterator itor = that.end ();
   string::const_iterator begin = that.begin ();
   if (*begin == '_') {
      this->negative = true;
      ++begin;
   }
   char currc;
   for (; itor != begin; --itor) {
      currc = *itor;
      this->big_value->push_back (currc);
   }
   currc = *begin;
   this->big_value->push_back (currc);
}

//
// operator+
//    the addition of bigints operator.  if both bigints have the same
//    sign, then calls do_bigadd() to add the vectors.  if the signs are
//    different, then calls abs_compare() to determine the larger number
//    and lastly calls do_bigsub() to subtract the larger minus the
//    smaller.  returns a bigint
//
bigint bigint::operator+ (const bigint &that) const {
   bigint bigsum;
   TRACE ('p', bigsum);
   
   if (this->negative == that.negative) {
      bigsum = (*this).do_bigadd (that);
   }else {
      if ((*this).abscompare (that) == 1) {
         bigsum = (*this).do_bigsub (that);
      }else if ((*this).abscompare (that) == -1) {
         bigsum = that.do_bigsub (*this);
      }else {
         bigsum.big_value->push_back ('0');
      }
   }
   return bigsum;
}

//
// operator-
//    the subtraction of that from this operator for bigints.  if this
//    and that have opposite signs, then calls do_bigadd() to add the
//    vectors since the subtraction turns this operation into addition.
//    if the signs are the same, then calls abs_compare() to determine
//    the dominating number and lastly calls do_bigsub() to subtract
//    the larger minus the smaller.  returns a bigint
//
bigint bigint::operator- (const bigint &that) const {
   bigint bigdiff;
   TRACE ('m', bigdiff);

   if (this->negative != that.negative) {
      bigdiff = (*this).do_bigadd (that);
   }else {
      if ((*this).abscompare (that) == 1) {
         bigdiff = (*this).do_bigsub (that);
      }else if ((*this).abscompare (that) == -1) {
         bigdiff = that.do_bigsub (*this);
         bigdiff.negative = !that.negative;
      }else {
         bigdiff.big_value->push_back ('0');
      }
   }
   return bigdiff;
}

//
// operator-
//    changes the sign of this bigint to its opposite
//
bigint bigint::operator- () const {
   bigint opp_val = (*this);
   opp_val.negative = !opp_val.negative;
   return opp_val;
}

//
// compare
//    compares two vectors.  first compares signs, then compares size of
//    vectors, and finally compares the individual digits starting with
//    the most significant.  returns -1 if this is smaller than that, 1
//    if this is bigger than that, and 0 if this equals that
//
int bigint::compare (const bigint &that) const {
   if (this->negative == true && that.negative == false) {
      return -1;
   }else if (this->negative == false && that.negative == true) {
      return 1;
   }else if ((this->negative == that.negative) == true) {
      if (this->big_value->size () < that.big_value->size ()) {
         return 1;
      }else if (this->big_value->size () > that.big_value->size ()) {
         return -1;
      }else {
         vector<bigint::digit_t>::const_iterator thisitor, thatitor;
         thisitor = this->big_value->begin ();
         thatitor = that.big_value->begin ();
         unsigned char thisc, thatc;
         for (int i = this->big_value->size () - 1; i >= 0; --i) {
            thisc = *thisitor;
            thatc = *thatitor;
            if (thisc < thatc) {
               return 1;
            }else if (thisc > thatc) {
               return -1;
            }
            ++thisitor;
            ++thatitor;
         }
      }
   }else {
      if (this->big_value->size () < that.big_value->size ()) {
         return -1;
      }else if (this->big_value->size () > that.big_value->size ()) {
         return 1;
      }else {
         vector<bigint::digit_t>::const_iterator thisitor, thatitor;
         thisitor = this->big_value->begin ();
         thatitor = that.big_value->begin ();
         unsigned char thisc, thatc;
         for (int i = this->big_value->size () - 1; i >= 0; --i) {
            thisc = *thisitor;
            thatc = *thatitor;
            if (thisc < thatc) {
               return -1;
            }else if (thisc > thatc) {
               return 1;
            }
            ++thisitor;
            ++thatitor;
         }
      }
   }
   return 0;
}

//
// abscompare
//    compares the absolute value of two vectors.  first compares size
//    of the vectors, then compares the absolute value of individual
//    digits starting with the most significant.  returns -1 if this is
//    smaller than that, 1 if this is bigger than that, and 0 if this
//    equals that.
//
int bigint::abscompare (const bigint &that) const {
   if (this->big_value->size () < that.big_value->size ()) {
      return -1;
   }else if (this->big_value->size () > that.big_value->size ()) {  
      return 1;
   }else {
      vector<bigint::digit_t>::const_iterator thisitor, thatitor;
      thisitor = this->big_value->begin ();
      thatitor = that.big_value->begin ();
      unsigned char thisc, thatc;
      for (int i = this->big_value->size () - 1; i >= 0; --i) {
         thisc = *thisitor;
         thatc = *thatitor;
         if (thisc < thatc) {
            return -1;
         }else if (thisc > thatc) {
            return 1;
         }
         --thisitor;
         --thatitor;
      }
   }
   return 0;
}

//
// smallint
//    converts a bigint to an int.  returns an int
//
int bigint::smallint () const {
   assert (((*this).compare (INT_MIN) == -1)
           || ((*this).compare (INT_MAX) == 1));
   if (((*this).compare (INT_MIN) == 1)
       || ((*this).compare (INT_MAX) == -1))
      throw range_error ("smallint: out of range");
   
   int result = 0;
   int curr, multiplier;
   unsigned char currc;
   vector<bigint::digit_t>::const_iterator itor;
   itor = this->big_value->end ();
   for (int i = this->big_value->size () - 1; i >= 0; --i) {
      currc = *itor;
      curr = currc -'0';
      if (curr != 0) {
         multiplier = 1;
         for (int j = 1; j <= i; ++j) {
            multiplier *= 10;
         }
         curr *= multiplier;
      }
      result += curr;
      --itor;
   }
   if (this->negative == true) {
      result = (-1)*(result);
   }
   return result;
}

//
// mul_by_2
//    returns a bigint of this times two.  calls on do_bigadd() to
//    double the size of this and keeps the sign unaltered
//
bigint bigint::mul_by_2 () {
   bigint double_this = (*this);
   double_this = double_this.do_bigadd (*this);
   return double_this;
}

//
// do_bigadd
//    returns a bigint sum of this and that
//
bigint bigint::do_bigadd (const bigint &that) const {
   int maxsize_x = this->big_value->size ();
   int maxsize_y = that.big_value->size ();
   int maxsize;
   if (maxsize_x < maxsize_y) { maxsize = maxsize_y; }
   else { maxsize = maxsize_x; }
   int x, y, sum;
   int carry = 0;
   unsigned char currc;
   bigint bigsum;
   if (this->abscompare (that) == 1) {
      bigsum.negative = this->negative;
   }else if (this->abscompare (that) == -1) {
      bigsum.negative = that.negative;
   }else {
      if (this->negative == that.negative) {
         bigsum.negative = this->negative;
      }else {
         bigsum.negative = false;
      }
   }
   vector<bigint::digit_t>::const_iterator thisitor, thatitor;
   
   thisitor = this->big_value->begin ();
   thatitor = that.big_value->begin ();
   for (int i = 0; i < maxsize; ++i) {
      if (i < maxsize_x) {
         currc = *thisitor;
         x = currc - '0';
         ++thisitor;
      }else { x = 0; }
      if (i < maxsize_y) {
         currc = *thatitor;
         y = currc - '0';
         ++thatitor;
      }else { y = 0; }
      sum = x + y + carry;
      if (sum > 9) {
         carry = 1;
         sum -= 10;
         currc = sum + '0';
      }
      else {
         carry = 0;
         currc = sum + '0';
      }
      bigsum.big_value->push_back (currc);
   }
   
   if (carry == 1) {
      currc = carry + '0';
      bigsum.big_value->push_back (currc);
   }
   return bigsum;
}

//
// do_bigsub
//    returns a bigint difference of that from this
//
bigint bigint::do_bigsub (const bigint &that) const {
   int maxsize_x = this->big_value->size ();
   int maxsize_y = that.big_value->size ();
   int maxsize;
   if (maxsize_x < maxsize_y) { maxsize = maxsize_y; }
   else { maxsize = maxsize_x; }
   int x, y, diff;
   int borrow = 0;
   unsigned char currc;
   bigint bigdiff;
   if (this->abscompare (that) == 1) {
      bigdiff.negative = this->negative;
   }else if (this->abscompare (that) == -1) {
      bigdiff.negative = that.negative;
   }else {
      if (this->negative == that.negative) {
         bigdiff.negative = false;
      }else {
         bigdiff.negative = true;
      }
   }

   vector<bigint::digit_t>::const_iterator thisitor, thatitor;
   
   thisitor = this->big_value->begin ();
   thatitor = that.big_value->begin ();
   for (int i = 0; i < maxsize; ++i) {
      if (i < maxsize_x) {
         currc = *thisitor;
         x = currc - '0';
         ++thisitor;
      }else { x = 0; }
      if (i < maxsize_y) {
         currc = *thatitor;
         y = currc - '0';
         ++thatitor;
      }else { y = 0; }
      x += borrow;
      if (x < y) { x += 10; borrow = -1; }
      else { borrow = 0; }
      diff = x - y;
      currc = diff + '0';
      bigdiff.big_value->push_back (currc);

   }
   
   while (bigdiff.big_value->back () == '0') {
      bigdiff.big_value->pop_back ();
   }
   return bigdiff;
}

//
// popstack
//    static function that pops and returns the top of a bigpair stack.
//    called by operator*()
//
static bigpair popstack (stack <bigpair> &egyptstack) {
   bigpair result = egyptstack.top ();
   egyptstack.pop();
   return result;
}

//
// operator*
//    Ancient Egyptian multiplication algorithm.
//    returns the bigint result of this times that
//
bigint bigint::operator* (const bigint &that) const {
   bigint big, small;
   if (that.abscompare (*this) > -1) {
      big = that;
      small = *this;
   }else {
      big = *this;
      small = that;
   }
   bigint count = (1);
   TRACE ('*', *this << " * " << that);
   stack <bigpair> egyptstack;
   
   while (count.abscompare (small) <= 0) {
      egyptstack.push (bigpair (count, big));
      count = count.mul_by_2 ();
      big = big.mul_by_2 ();
   }
   
   bigint result = 0;
   bigpair top;
   while (!egyptstack.empty ()) {
      top = popstack (egyptstack);
      count = top.first;
      big = top.second;
      if (count.abscompare (small) <= 0) {
         small = (small) - (count);
         result = (result) + (big);
      }
   }

   if (this->negative != that.negative) result = -(result);
   return result;
}

//
// div_rem
//    Ancient Egyptian division algorithm.
//    returns the bigint quotient and remainder of this divided by that
//
bigpair bigint::div_rem (const bigint &that) const {
   if (that == 0) throw range_error ("divide by 0");
   bigint numer = *this;
   bigint denom = that;
   bigint count = (1);
   TRACE ('/', *this << " /% " << that);
   stack <bigpair> egyptstack;
   
   while (denom.abscompare (numer) <= 0) {
      egyptstack.push (bigpair (count, denom));
      count = count.mul_by_2 ();
      denom = denom.mul_by_2 ();
   }
   
   bigint quotient = 0;
   bigint remainder = numer;
   while (!egyptstack.empty ()) {
      bigpair top = popstack (egyptstack);
      count = top.first;
      denom = top.second;
      if (denom.abscompare (remainder) <= 0) {
         remainder = remainder - denom;
         quotient = (quotient) + count;
      }
   }
   
   return bigpair (quotient, remainder);
}

//
// operator/
//    returns the quotient from a division.  calls on div_rem()
//
bigint bigint::operator/ (const bigint &that) const {
   return div_rem (that).first;
}

//
// operator%
//    returns the remainder (modulous) from a division.  calls on
//    div_rem()
//
bigint bigint::operator% (const bigint &that) const {
   return div_rem (that).second;
}

#define TRACE_POW \
   TRACE ('^', "result: " << result << ", base: " << base \
            << ", expt: " << expt);
bigint bigint::pow (const bigint &that) const {
   bigint base = *this;
   if (that.compare (999) == -1) throw range_error ("exp too big");
   int expt = that.smallint();
   bigint result = 1;
   TRACE_POW;
   if (expt < 0) {
      base = 1 / base;
      expt = - expt;
   }
   while (expt > 0) {
      TRACE_POW;
      if (expt & 1) { //odd
         result = result * base;
         --expt;
      }else { //even
         base = base * base;
         expt /= 2;
      }
   }
   TRACE_POW;
   return result;
}

//
// Macros can make repetitive code easier.
//

#define COMPARE(OPER) \
   bool bigint::operator OPER (const bigint &that) const { \
      return compare (that) OPER 0; \
   }
COMPARE (==)
COMPARE (!=)
COMPARE (< )
COMPARE (<=)
COMPARE (> )
COMPARE (>=)

#define INT_LEFT(RESULT,OPER) \
   RESULT operator OPER (int left, const bigint &that) { \
      return bigint (left) OPER that; \
   }
INT_LEFT (bigint, +)
INT_LEFT (bigint, -)
INT_LEFT (bigint, *)
INT_LEFT (bigint, /)
INT_LEFT (bigint, %)
INT_LEFT (bool, ==)
INT_LEFT (bool, !=)
INT_LEFT (bool, < )
INT_LEFT (bool, <=)
INT_LEFT (bool, > )
INT_LEFT (bool, >=)

ostream &operator<< (ostream &out, const bigint &that) {
   if (that.negative == true) {
      out << "-";
   }
   int count = 0;
   unsigned char currc;
   vector<bigint::digit_t>::const_iterator itor = that.big_value->end();
   for (int i = that.big_value->size () - 1; i >= 0; --i) {
      ++count;
      if (count == 70) {
         out << "\\" << endl;
         count = 1;
      }
      currc = *itor;
      out << currc;
      --itor;
   }
   return out;
}

