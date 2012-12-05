/***********************************************************************
 * $Id: lyutils.h,v 1.4 2011-10-22 20:36:19-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   lyutils.h - interface file for Lex and Yacc utility
 *
 * DESCRIPTION
 *   Lex and Yacc utility.  Contains all of the external definitions
 *   and global usages necessary to connect yylex() and yyparse() to
 *   the rest of the program.
 *
 **********************************************************************/


#ifndef __LYUTILS_H__
#define __LYUTILS_H__

#include <stdio.h>

#include "astree.h"
#include "auxlib.h"

#define YYEOF 0

extern FILE *yyin;
extern astree yyparse_astree;
extern int yyin_linenr;
extern char *yytext;
extern int yy_flex_debug;
extern int yydebug;
extern int yyleng;
extern char *basefilename;
extern FILE *tokfile;

int yylex (void);
int yyparse (void);
void yyerror (char *message);
const char *get_yytname (int symbol);

char *scanner_filename (int filenr);
void scanner_newfilename (char *filename);
void scanner_badchar (unsigned char bad);
void scanner_badtoken (char *lexeme);
void scanner_newline (void);
void scanner_setecho (bool echoflag);
void scanner_userinit (void);
void scanner_useraction (void);

astree new_parseroot (void);
int yylval_token (int symbol);

void scanner_include (void);

#define YYSTYPE astree
#include "yyparse.h"

#endif
