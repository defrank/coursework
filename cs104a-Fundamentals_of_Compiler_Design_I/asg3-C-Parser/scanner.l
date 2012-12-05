%{
/***********************************************************************
 * $Id: scanner.l,v 1.32 2011-11-02 20:59:45-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *  scanner.l - scanner definition for flex scanner
 *
 * DESCRIPTION
 *   Definitions for the flex scanner.
 *
 **********************************************************************/

#include <stdlib.h>

#include "auxlib.h"
#include "lyutils.h"

#define YY_USER_INIT    { scanner_userinit (); }
#define YY_USER_ACTION  { scanner_useraction (); }
#define IGNORE(THING)   { }

%}

%option 8bit
%option backup
%option debug
%option ecs
%option nodefault
%option nounput
%option noyywrap
%option perf-report
%option verbose

TOK_VOID            (void)    
TOK_BOOL            (bool)
TOK_CHAR            (char)
TOK_INT             (int)
TOK_STRING          (string)
TOK_IF              (if)
TOK_ELSE            (else)
TOK_WHILE           (while)
TOK_RETURN          (return)
TOK_STRUCT          (struct)
TOK_FALSE           (false)
TOK_TRUE            (true)
TOK_NULL            (null)
TOK_NEW             (new)

TOK_ARRAY           ("[]")

TOK_ORD             (ord)
TOK_CHR             (chr)

TOK_EQ              (==)
TOK_NE              (!=)
TOK_LE              (<=)
TOK_GE              (>=)
TOK_LT              (<)
TOK_GT              (>)

LETTER              [A-Za-z_]
DIGIT               [0-9]
MANTISSA            ({DIGIT}+\.?{DIGIT}*|\.{DIGIT}+)
EXPONENT            ([Ee][+-]?{DIGIT}+)
NUMBER              ({MANTISSA}{EXPONENT}?)
NOTNUMBER           ({MANTISSA}[Ee][+-]?)

TOK_IDENT           ({LETTER}({LETTER}|{DIGIT})*)
INVAL_IDENT         ({DIGIT}+{LETTER}({LETTER}|{DIGIT})*)
TOK_INTCON          ({DIGIT}+)
INVAL_INTCON        ({MANTISSA}|{EXPONENT}|{NUMBER}|{NOTNUMBER})
TOK_CHARCON         ('([^\\'\n]|\\[\\'"0nt])')
INVAL_CHARCON       ('([^\\'\n]|\\[\\'"0nt]?)[^']?)
TOK_STRINGCON       (\"([^\\"\n]|\\[\\'"0nt])*\")
INVAL_STRINGCON     (\"([^\\"\n]|\\[\\'"0nt])*[^\"]?)

%%

"#".*               { scanner_include(); }
[ \t]+              { IGNORE (white space); }
\n                  { scanner_newline(); }

{TOK_VOID}          { return yylval_token (TOK_VOID); }
{TOK_BOOL}          { return yylval_token (TOK_BOOL); }
{TOK_CHAR}          { return yylval_token (TOK_CHAR); }
{TOK_INT}           { return yylval_token (TOK_INT); }
{TOK_STRING}        { return yylval_token (TOK_STRING); }
{TOK_IF}            { return yylval_token (TOK_IF); }
{TOK_ELSE}          { return yylval_token (TOK_ELSE); }
{TOK_WHILE}         { return yylval_token (TOK_WHILE); }
{TOK_RETURN}        { return yylval_token (TOK_RETURN); }
{TOK_STRUCT}        { return yylval_token (TOK_STRUCT); }
{TOK_FALSE}         { return yylval_token (TOK_FALSE); }
{TOK_TRUE}          { return yylval_token (TOK_TRUE); }
{TOK_NULL}          { return yylval_token (TOK_NULL); }
{TOK_NEW}           { return yylval_token (TOK_NEW); }

{TOK_ORD}           { return yylval_token (TOK_ORD); }
{TOK_CHR}           { return yylval_token (TOK_CHR); }

{TOK_IDENT}         { return yylval_token (TOK_IDENT); }
{TOK_INTCON}        { return yylval_token (TOK_INTCON); }
{TOK_CHARCON}       { return yylval_token (TOK_CHARCON); }
{TOK_STRINGCON}     { return yylval_token (TOK_STRINGCON); }

{TOK_ARRAY}         { return yylval_token (TOK_ARRAY); }

{TOK_EQ}            { return yylval_token (TOK_EQ); }
{TOK_NE}            { return yylval_token (TOK_NE); }
{TOK_LE}            { return yylval_token (TOK_LE); }
{TOK_GE}            { return yylval_token (TOK_GE); }

{TOK_LT}            { return yylval_token (TOK_LT); }
{TOK_GT}            { return yylval_token (TOK_GT); }
"("                 { return yylval_token ('('); }
")"                 { return yylval_token (')'); }
"["                 { return yylval_token ('['); }
"]"                 { return yylval_token (']'); }
"{"                 { return yylval_token ('{'); }
"}"                 { return yylval_token ('}'); }
";"                 { return yylval_token (';'); }
","                 { return yylval_token (','); }
"."                 { return yylval_token ('.'); }
"="                 { return yylval_token ('='); }
"+"                 { return yylval_token ('+'); }
"-"                 { return yylval_token ('-'); }
"*"                 { return yylval_token ('*'); }
"/"                 { return yylval_token ('/'); }
"%"                 { return yylval_token ('%'); }
"!"                 { return yylval_token ('!'); }

{INVAL_IDENT}       { scanner_badtoken (yytext);
                      return yylval_token (TOK_IDENT); }
{INVAL_CHARCON}     { scanner_badtoken (yytext);
                      return yylval_token (TOK_CHARCON); }
{INVAL_STRINGCON}   { scanner_badtoken (yytext);
                      return yylval_token (TOK_STRINGCON); }

.                   { scanner_badchar (*yytext); }

%%

// set: %option noyywrap
//int yywrap (void){
//   yy_delete_buffer (YY_CURRENT_BUFFER);
//   if (yy_flex_debug) eprintf ("yywrap() return %d\n", TRUE);
//   return TRUE;
//}
