/***********************************************************************
 * $Id: astree.c,v 1.1 2012/12/05 07:59:55 dmf Exp $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   astree.c - implementaion file for the abstract syntax tree (AST)
 *
 * DESCRIPTIONS
 *   Abstract syntax tree.
 *
 **********************************************************************/

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "astree.h"
#include "astree.rep.h"
#include "lyutils.h"
#include "auxlib.h"

static char *astree_tag = "struct astree_rep";

bool is_astree (void *object) {
   astree tree = (astree) object;
   return tree != NULL && tree->tag == astree_tag;
}

astree new_astree (int symbol, int filenr, int linenr, int offset,
                   char *lexinfo) {
   size_t size = sizeof (astree_rep);
   astree tree = malloc (size);
   assert (tree != NULL);
   tree->tag = astree_tag;
   tree->symbol = symbol;
   tree->filenr = filenr;
   tree->linenr = linenr;
   tree->offset = offset;
   tree->lexinfo = strdup (lexinfo);
   assert (tree->lexinfo != NULL);
   tree->first = NULL;
   tree->last = NULL;
   tree->next = NULL;
   DEBUGF ('f', "malloc (%d) = %p-> %d:%d.%d: %s: %p->\"%s\"\n",
           size, tree, tree->filenr, tree->linenr, tree->offset,
           get_yytname (tree->symbol), tree->lexinfo, tree->lexinfo);
   return tree;
}

astree adopt (astree root, ...) {
   va_list children;
   assert (is_astree (root));
   va_start (children, root);
   for(;;) {
      astree child = va_arg (children, astree);
      if (child == NULL) break;
      assert (is_astree (child));
      if (root->last == NULL) root->first = child;
      else root->last->next = child;
      root->last = child;
      DEBUGF ('a', "%p (%s) adopting %p (%s)\n",
              root, root->lexinfo,
              child, child->lexinfo);
   }
   va_end (children);
   return root;
}

astree adopt3 (astree root, astree left, astree middle, astree right) {
   return adopt (root, left, middle, right, NULL);
}

astree adopt2 (astree root, astree left, astree right) {
   return adopt (root, left, right, NULL);
}

astree adopt1 (astree root, astree child) {
   return adopt (root, child, NULL);
}

astree adopt1sym (astree root, astree child, int symbol) {
   root = adopt1 (root, child);
   root->symbol = symbol;
   return root;
}

static void dump_node (FILE *outfile, astree node) {
   assert (is_astree (node));
   const char *tname = get_yytname (node->symbol);
   if (strstr (tname, "TOK_") == tname) tname += 4;
   xfprintf (outfile, "%s \"%s\" %d.%d.%d", tname,
             node->lexinfo, node->filenr, node->linenr, node->offset);
}

static void dump_astree_rec (FILE *outfile, astree root, int depth) {
   astree child = NULL;
   if (root == NULL) return;
   assert (is_astree (root));
   int i;
   for (i = 0; i < depth; ++i) {
      xfprintf (outfile, "|%*s", 3, "");
   }
   dump_node (outfile, root);
   xfprintf (outfile, "\n");
   for (child = root->first; child != NULL; child = child->next) {
      dump_astree_rec (outfile, child, depth + 1);
   }
}

void dump_astree (FILE *outfile, astree root) {
   dump_astree_rec (outfile, root, 0);
   xfflush (NULL);
}

void yyprint (FILE *outfile, unsigned short toknum, astree yyvaluep) {
   xfprintf (outfile, "%d=%s)\n%*s(",
             toknum, get_yytname (toknum), 9, "");
   if (is_astree (yyvaluep)) {
      dump_node (outfile, yyvaluep);
   }else{
      xfprintf (outfile, "yyvaluep = %p", (void*) yyvaluep);
   }
   xfflush (NULL);
}

void freeast (astree root) {
   astree child = NULL;
   if (root == NULL) return;
   assert (is_astree (root));
   for (child = root->first; child != NULL;) {
      astree asttofree = child;
      assert (is_astree (asttofree));
      child = child->next;
      freeast (asttofree);
   }
   DEBUGF ('f', "free [%X]-> %d:%d.%d: %s: %p->\"%s\")\n",
           (uintptr_t) root, root->filenr, root->linenr, root->offset,
            get_yytname (root->symbol), root->lexinfo, root->lexinfo);
   free (root->lexinfo);
   xmemset (root, 0, sizeof (astree_rep));
   free (root);
}
