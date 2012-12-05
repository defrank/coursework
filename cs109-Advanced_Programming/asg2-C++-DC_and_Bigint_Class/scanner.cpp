/* $Id: scanner.cpp,v 1.2 2011-01-29 15:35:40-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   scanner
 *
 * DESCRIPTION
 *   Reads input and handles tokens.
 */

#include <iostream>
#include <locale>

using namespace std;

#include "scanner.h"
#include "trace.h"

scanner::scanner () {
   seen_eof = false;
   advance();
}

void scanner::advance () {
   if (! seen_eof) {
      cin.get (lookahead);
      if (cin.eof()) seen_eof = true;
   }
}

token_t scanner::scan() {
   token_t result;
   while (!seen_eof && isspace (lookahead)) advance();
   if (seen_eof) {
      result.symbol = SCANEOF;
   }else if (lookahead == '_' || isdigit (lookahead)) {
      result.symbol = NUMBER;
      do {
         result.lexinfo += lookahead;
         advance();
      }while (!seen_eof && isdigit (lookahead));
   }else {
      result.symbol = OPERATOR;
      result.lexinfo += lookahead;
      advance();
   }
   TRACE ('S', result);
   return result;
}

ostream &operator<< (ostream &out, const terminal_symbol &symbol) {
   switch (symbol) {
      #define CASE_SYMBOL(SYMBOL) case SYMBOL: out << #SYMBOL; break;
      CASE_SYMBOL (NUMBER);
      CASE_SYMBOL (OPERATOR);
      CASE_SYMBOL (SCANEOF);
   }
   return out;
}

ostream &operator<< (ostream &out, const token_t &token) {
   out << token.symbol << ": \"" << token.lexinfo << "\"";
   return out;
}

