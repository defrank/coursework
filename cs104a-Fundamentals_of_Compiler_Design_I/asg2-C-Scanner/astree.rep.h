/***********************************************************************
 * $Id: astree.rep.h,v 1.2 2011-10-22 22:57:21-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *   astree.rep.h - representation interface file for the AST
 *
 * DESCRIPTIONS
 *   Representation of the abstract syntax tree.  Usually this would
 *   be private in the implementation file, but it is made public here
 *   in order to ease access to the fields.  All fields should be
 *   considered immutable except in the implementation file.
 *
 **********************************************************************/


#ifndef __ASTREEREP_H__
#define __ASTREEREP_H__

#include "astree.h"

struct astree_rep {
   char *tag;           // tag field to verify class membership
   int symbol;          // token code
   int filenr;          // index into filename stack
   int linenr;          // line number from source code
   int offset;          // offset of token with current line
   char *lexinfo;       // pointer to lexical information
   astree first;        // first child node of this node
   astree last;         // last child node of this node
   astree next;         // next younger sibling of this node
} astree_rep;

#endif
