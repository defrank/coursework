%{
/***********************************************************************
 * $Id: parser.y,v 1.48 2011-11-08 00:04:30-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   parser.y - parser
 *
 * DESCRIPTION
 *   Definitions for the bison parser.
 *
 **********************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "lyutils.h"
#include "astree.h"
#include "astree.rep.h"

%}

%debug
%defines
%error-verbose
%token-table

%token TOK_VOID TOK_BOOL TOK_CHAR TOK_INT TOK_STRING
%token TOK_IF TOK_ELSE TOK_WHILE TOK_RETURN TOK_STRUCT
%token TOK_FALSE TOK_TRUE TOK_NULL TOK_NEW TOK_ARRAY
%token TOK_ORD TOK_CHR
%token TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON

%token TOK_BLOCK TOK_CALL TOK_IFELSE TOK_INITDECL
%token TOK_POS TOK_NEG TOK_TYPEID TOK_FIELD TOK_INDEX
%token TOK_FUNCTION TOK_PARAMLIST TOK_PROTOTYPE
%token TOK_NEWARRAY TOK_NEWSTRING TOK_RETURNVOID
%token TOK_ROOT

%right     TOK_IF TOK_ELSE
%right     '='
%left      TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE
%left      '+' '-'
%left      '*' '/' '%'
%right     TOK_POS TOK_NEG '!' TOK_ORD TOK_CHR
%left      '[' '.' TOK_CALL
%nonassoc  TOK_NEW
%nonassoc  '('

%start  program

%%

program   : program structdef      { $$ = adopt1 ($1, $2); }
          | program function       { $$ = adopt1 ($1, $2); }
          | program stmt           { $$ = adopt1 ($1, $2); }
          | program error '}'      { $$ = $1; }
          | program error ';'      { $$ = $1; }
          |                        { $$ = new_parseroot(); }
          ;
structdef : TOK_STRUCT TOK_IDENT '{' '}'
          { $$ = adopt1 ($1, adopt1sym ($2, NULL, TOK_TYPEID));
            freeast2 ($3, $4); }
          | ideclist '}'           { $$ = $1; freeast ($2); }
          ;
ideclist  : ideclist identdecl ';' { $$ = adopt1 ($1, $2); freeast($3) }
          | TOK_STRUCT TOK_IDENT '{' identdecl ';'
          { $$ = adopt2 ($1, adopt1sym ($2, NULL, TOK_TYPEID), $4);
            freeast2 ($3, $5); }
          ;
identdecl : basetype TOK_ARRAY TOK_IDENT
          { $$ = adopt2 ($2, $1, $3); }
          | basetype TOK_IDENT     { $$ = adopt1 ($1, $2); }
          ;
basetype  : TOK_VOID               { $$ = $1; }
          | TOK_BOOL               { $$ = $1; }
          | TOK_CHAR               { $$ = $1; }
          | TOK_INT                { $$ = $1; }
          | TOK_STRING             { $$ = $1; }
          | TOK_IDENT
          { $$ = adopt1sym ($1, NULL, TOK_TYPEID); }
          ;
function  : identdecl '(' ')' ';'
          { $$ = adopt2 (new_protonode ($1), $1,
                         adopt1sym ($2, NULL, TOK_PARAMLIST) );
            freeast2 ($3, $4); }
          | identdecl paramlist ')' ';'
          { $$ = adopt2 (new_protonode($1), $1, $2); freeast2($3, $4); }
          | subfunc '}'            { $$ = $1; freeast ($2); }
          ;
subfunc   : identdecl '(' ')' '{'
          { $$ = adopt3 (new_funcnode ($1), $1,
                         adopt1sym ($2, NULL, TOK_PARAMLIST),
                         adopt1sym ($4, NULL, TOK_BLOCK) );
            freeast ($3); }
          | identdecl '(' ')' blocklist
          { $$ = adopt3 (new_funcnode ($1), $1,
                         adopt1sym ($2, NULL, TOK_PARAMLIST), $4);
            freeast ($3); }
          | identdecl paramlist ')' '{'
          { $$ = adopt3 (new_funcnode ($1), $1, $2,
                         adopt1sym ($4, NULL, TOK_BLOCK) );
            freeast ($3); }
          | identdecl paramlist ')' blocklist
          { $$ = adopt3 (new_funcnode ($1), $1, $2, $4);
            freeast ($3); }
          ;
paramlist : paramlist ',' identdecl
          { $$ = adopt1 ($1, $3); freeast ($2); }
          | '(' identdecl
          { $$ = adopt1sym ($1, $2, TOK_PARAMLIST); }
          ;
block     : '{' '}'
          { $$ = adopt1sym ($1, NULL, TOK_BLOCK); freeast ($2); }
          | blocklist '}'          { $$ = $1; freeast ($2); }
          | ';'                    { $$ = $1; }
          ;
blocklist : blocklist stmt         { $$ = adopt1 ($1, $2); }
          | '{' stmt
          { $$ = adopt1 (adopt1sym ($1, $2, TOK_BLOCK), NULL);}
          ;
stmt      : block                  { $$ = $1; }
          | initdecl               { $$ = $1; }
          | while                  { $$ = $1; }
          | ifelse                 { $$ = $1; }
          | return                 { $$ = $1; }
          | expr ';'               { $$ = $1; freeast ($2); }
          ;
initdecl  : identdecl '=' expr ';'
          { $$ = adopt1 (adopt1sym ($2, $1, TOK_INITDECL), $3);
            freeast ($4); }
          ;
while     : TOK_WHILE '(' expr ')' stmt
          { $$ = adopt2 ($1, $3, $5); freeast2 ($2, $4); }
          ;
ifelse    : TOK_IF '(' expr ')' stmt %prec TOK_ELSE
          { $$ = adopt2 ($1, $3, $5); freeast2 ($2, $4); }
          | TOK_IF '(' expr ')' stmt TOK_ELSE stmt
          { $$ = adopt2 (adopt1sym ($1, $3, TOK_IFELSE), $5, $7);
            freeast3 ($2, $4, $6); }
          ;
return    : TOK_RETURN expr ';'
          { $$ = adopt1 ($1, $2); freeast ($3); }
          | TOK_RETURN ';'
          { $$ = adopt1sym ($1, NULL, TOK_RETURNVOID); freeast ($2); }
          ;
expr      : expr '=' expr          { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_EQ expr       { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_NE expr       { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_LT expr       { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_LE expr       { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_GT expr       { $$ = adopt2 ($2, $1, $3); }
          | expr TOK_GE expr       { $$ = adopt2 ($2, $1, $3); }
          | expr '+' expr          { $$ = adopt2 ($2, $1, $3); }
          | expr '-' expr          { $$ = adopt2 ($2, $1, $3); }
          | expr '*' expr          { $$ = adopt2 ($2, $1, $3); }
          | expr '/' expr          { $$ = adopt2 ($2, $1, $3); }
          | expr '%' expr          { $$ = adopt2 ($2, $1, $3); }
          | '+' expr %prec TOK_POS { $$ = adopt1sym ($1, $2, TOK_POS); }
          | '-' expr %prec TOK_NEG { $$ = adopt1sym ($1, $2, TOK_NEG); }
          | '!' expr               { $$ = adopt1 ($1, $2); }
          | TOK_ORD expr           { $$ = adopt1 ($1, $2); }
          | TOK_CHR expr           { $$ = adopt1 ($1, $2); }
          | allocator              { $$ = $1; }
          | call                   { $$ = $1; }
          | variable               { $$ = $1; }
          | constant               { $$ = $1; }
          | '(' expr ')'           { $$ = $2; freeast2 ($1, $3); }
          ;
allocator : TOK_NEW TOK_IDENT '(' ')'
          { $$ = adopt1 ($1, adopt1sym ($2, NULL, TOK_TYPEID) );
            freeast2 ($3, $4); }
          | TOK_NEW TOK_STRING '(' expr ')'
          { $$ = adopt1sym ($1, $4, TOK_NEWSTRING);
            freeast3 ($2, $3, $5); }
          | TOK_NEW basetype '[' expr ']'
          { $$ = adopt1 (adopt1sym ($1, $2, TOK_NEWARRAY), $4);
            freeast2 ($3, $5); }
          ;
call      : TOK_IDENT '(' ')'
          { $$ = adopt1sym ($2, $1, TOK_CALL); freeast ($3); }
          | callque ')'            { $$ = $1; freeast ($2); }
          ;
callque   : callque ',' expr
          { $$ = adopt1 ($1, $3); freeast ($2); }
          | TOK_IDENT '(' expr
          { $$ = adopt1 (adopt1sym ($2, $1, TOK_CALL), $3); }
          ;
variable  : TOK_IDENT              { $$ = $1; }
          | expr '[' expr ']'
          { $$ = adopt1 (adopt1sym ($2, $1, TOK_INDEX), $3);
            freeast ($4); }
          | expr '.' TOK_IDENT
          { $$ = adopt2 ($2, $1, adopt1sym ($3, NULL, TOK_FIELD) ); }
          ;
constant  : TOK_INTCON             { $$ = $1; }
          | TOK_CHARCON            { $$ = $1; }
          | TOK_STRINGCON          { $$ = $1; }
          | TOK_FALSE              { $$ = $1; }
          | TOK_TRUE               { $$ = $1; }
          | TOK_NULL               { $$ = $1; }
          ;

%%

const char *get_yytname (int symbol) {
   return yytname [YYTRANSLATE (symbol)];
}
