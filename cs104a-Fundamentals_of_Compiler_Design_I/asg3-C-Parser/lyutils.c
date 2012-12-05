/***********************************************************************
 * $Id: lyutils.c,v 1.1 2012/12/05 07:59:55 dmf Exp $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   lyutils.c - implementation file for Lex and Yacc utility
 *
 * DESCRIPTION
 *   Lex and Yacc utility.  Contains all of the external definitions
 *   and global usages necessary to connect yylex() and yyparse() to
 *   the rest of the program.
 *
 **********************************************************************/


#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stringtable.h"
#include "astree.rep.h"
#include "lyutils.h"
#include "auxlib.h"

char *basefilename;
FILE *strfile;
FILE *tokfile;
FILE *astfile;

stringtable_ref hashtable = NULL;
astree yyparse_astree = NULL;
int scan_linenr = 1;
int scan_offset = 0;
bool scan_echo = FALSE;

struct {
   char **filenames;
   int size;
   int last_filenr;
} filename_stack = {NULL, 0, -1};

char *scanner_filename (int filenr) {
   assert (filename_stack.filenames != NULL);
   return filename_stack.filenames[filenr];
}

void scanner_newfilename (char *filename) {
   assert (filename != NULL);
   if (filename_stack.filenames == NULL) {
      filename_stack.size = 16;
      filename_stack.last_filenr = -1;
      filename_stack.filenames
            = malloc (filename_stack.size * sizeof (char*));
      assert (filename_stack.filenames != NULL);
   }else if (filename_stack.last_filenr == filename_stack.size - 1) {
      filename_stack.size *= 2;
      filename_stack.filenames
            = realloc (filename_stack.filenames,
                       filename_stack.size * sizeof (char*));
      assert (filename_stack.filenames != NULL);
   }
   char *newfilename = strdup (filename);
   assert (newfilename != NULL);
   filename_stack.filenames[++filename_stack.last_filenr]
         = newfilename;
}

void scanner_newline (void) {
   ++scan_linenr;
   scan_offset = 0;
}

void scanner_setecho (bool echoflag) {
   scan_echo = echoflag;
}

void scanner_userinit (void) {
   if (basefilename == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "lyutils.c", "scanner_userinit()",
                 "null file pointer");
   char *tokname = malloc (strlen (basefilename) + strlen (".tok") + 1);
   xstrcpy (tokname, basefilename);
   xstrcat (tokname, ".tok");
   tokfile = fopen (tokname, "w+");
   free (tokname);
}

void scanner_useraction (void) {
   if (scan_echo) {
      if (scan_offset == 0) xprintf (";%5d: ", scan_linenr);
      xprintf ("%s", yytext);
   }
   scan_offset += yyleng;
}

void yyerror (char *message) {
   assert (filename_stack.filenames != NULL);
   errprintf ("%:%s: %d: %s\n",
              filename_stack.filenames[filename_stack.last_filenr],
              scan_linenr, message);
}

void scanner_badchar (unsigned char bad) {
   char char_rep[16];
   (void) sprintf (char_rep, isgraph((int) bad) ? "%c" : "\\%03o", bad);
   errprintf ("%:%s: %d: invalid source character (%s)\n",
              filename_stack.filenames[filename_stack.last_filenr],
              scan_linenr, char_rep);
}

void scanner_badtoken (char *lexeme) {
   errprintf ("%:%s: %d: invalid token (%s)\n",
              filename_stack.filenames[filename_stack.last_filenr],
              scan_linenr, lexeme);
}

int yylval_token (int symbol) {
   int offset = scan_offset - yyleng;
   yylval = new_astree (symbol, filename_stack.last_filenr,
                        scan_linenr, offset, yytext);
   // print token info to "program.tok"
   xfprintf (tokfile, "  %2d  %4d.%.3d  %3d  %-16s  (%s)\n",
             filename_stack.last_filenr, scan_linenr, offset,
             symbol, get_yytname (symbol), yytext);
   (void) intern_stringtable (hashtable, yytext);
   return symbol;
}

void set_basefilename (char *filename) {
   basefilename = strdup (filename); // uses malloc()
   hashtable = new_stringtable();
}

void file_dumps (void) {
   (void) fclose (tokfile);
   // perform writing to "program.str"
   cstring strname = malloc (strlen (basefilename)
                             + strlen (".str") + 1);
   xstrcpy (strname, basefilename);
   xstrcat (strname, ".str");
   FILE *strfile = fopen (strname, "w+");
   free (strname);
   debugdump_stringtable (hashtable, strfile);
   delete_stringtable (hashtable);
   (void) fclose (strfile);
   // perform writing to "program.ast"
   cstring astname = malloc (strlen (basefilename)
                             + strlen (".ast") + 1);
   xstrcpy (astname, basefilename);
   xstrcat (astname, ".ast");
   FILE *astfile = fopen (astname, "w+");
   free (astname);
   dump_astree (astfile, yyparse_astree);
   (void) fclose (astfile);
   freeast (yyparse_astree);
   free (basefilename); // strdup() uses malloc()
}

astree new_parseroot (void) {
   yyparse_astree = new_astree (TOK_ROOT, 0, 0, 0, "");
   (void) intern_stringtable (hashtable, "");   
   return yyparse_astree;
}

////
// new_protonode
//
//   Creates a tree for a symbol type TOK_PROTOTYPE with astree
//   information taken from the provided node.
//
astree new_protonode (astree node) {
   astree prototree = new_astree (TOK_PROTOTYPE, node->filenr,
                                node->linenr, node->offset, "");
   (void) intern_stringtable (hashtable, "");   
   return prototree;
}

////
// new_funcnode
//
//   Creates a tree for a symbol type TOK_FUNCTION with astree
//   information taken from the provided node.
//
astree new_funcnode (astree node) {
   astree functree = new_astree (TOK_FUNCTION, node->filenr,
                                node->linenr, node->offset, "");
   (void) intern_stringtable (hashtable, "");   
   return functree;
}

void scanner_include (void) {
   scanner_newline();
   char *filename = alloca (strlen (yytext) + 1);
   int linenr;
   int scan_rc = sscanf (yytext, "# %d \"%[^\"]\"", &linenr, filename);
   if (scan_rc != 2) {
      errprintf ("%: %d: [%s]: invalid directive, ignored\n",
                 scan_rc, yytext);
   }else {
      char *newfilename = strdup (filename);
      assert (newfilename != NULL);
      scanner_newfilename (newfilename);
      scan_linenr = linenr;
      // print directive to "program.tok"
      xfprintf (tokfile, "%s\n", yytext);
      DEBUGF ('m', "filename=%s, scan_linenr=%d\n",
              filename_stack.filenames[filename_stack.last_filenr],
              scan_linenr);
      free (newfilename);
   }
}
