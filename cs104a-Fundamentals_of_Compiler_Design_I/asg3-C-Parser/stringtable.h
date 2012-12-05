/***********************************************************************
 * $Id: stringtable.h,v 1.1 2011-10-27 18:24:24-07 dmfrank - $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   stringtable.h - header file for the string table
 *
 * DESCRIPTION
 *   Interface for the string table ADT.  The string table ADT will
 *   operate as a hash table.
 *
 **********************************************************************/

#ifndef __STRINGTABLE_H__
#define __STRINGTABLE_H__

#include <inttypes.h>

#include "auxlib.h"

/******************** Exported Types **********************************/

////
// cstring - pointer
//
//   Character string that is an array of characters.
//
typedef char *cstring;

////
// hashcode_t
//
//   Type uint32_t is defined in the library file <inttypes.h>
//
typedef uint32_t hashcode_t;

////
// stringtable_ref - reference
//
//   A handle pointing at the entire hash table.  Returned by the
//   constructor and freed by the destructor.  The structure is in the
//   implementation file and consists of a pointer to the array of hash
//   headers and a dimension.  Also, a pointer to an internal static
//   string identifier so that an assertion can be used to check the
//   validity of the node.  Also, the number of elements in the table is
//   cached in this node in order to determine when the table needs
//   doubling.  Other fields if needed.
//
typedef struct stringtable *stringtable_ref;

////
// stringnode_ref - reference
//
//   A handle pointing at an individual string node.  The node itself
//   contains a pointer to a string on the heap, along with a collision
//   resolution pointer and a cached hash number.  Also a pointer to an
//   internal static string identifier.
//
typedef struct stringnode *stringnode_ref;


/********************** Type Checking-Assertion Functions *************/

////
// is_stringnode
//
//   Safety check for the RTTI (run time type identification).
//   Usually called within an assert() of a function using a string
//   node.
//
bool is_stringnode (void *object);

////
// is_stringtable
//
//   Safety check for the RTTI (run time type identification).
//   Usually called within an assert() of a function using a string
//   node.
//
bool is_stringtable (void *object);

/******************* Constructors-Destructors *************************/

////
// new_stringtable - Constructor
//
//   Creates a new string table and returns its handle.  Uses as its
//   initial capacity some arbitrary odd number, such as 31.
//
stringtable_ref new_stringtable (void);

////
// delete_stringtable - Destructor
//
//   Dismantles the entire hash table and use free(3) to delete all
//   internal nodes.  Implementation of the function is optional.
//
void delete_stringtable (stringtable_ref);

/***************** Access Functions ***********************************/

////
// debugdump_stringtable
//
//   Dumps out the hash table in debug format:
//        24     348883689  "hello"
//              4294967295  "there"
//        92     338729983  "67"
//
//   In other words, print the hash header number in %8d format followed
//   by spaces, then the hash number (%12u), and then the strings in %s
//   format inside quotes in a column.  In the above example, the two
//   strings in bucket 24 have collided.  It will always be true that
//   the second number modulo the size of the hash table will be equal
//   to the first number.
//
void debugdump_stringtable (stringtable_ref, FILE*);

////
// peek_stringtable
//
//   Returns a pointer to a character string, such as may be used for
//   printing.
//
cstring peek_stringtable (stringnode_ref);

////
//  hashcode_stringtable
//
//   Returns the cached hash number associated with this string node.
//   Does not recompute the number.
//
hashcode_t hashcode_stringtable (stringnode_ref);


/*********************** Manipulation Functions ***********************/
////
// intern_stringtable
//
//   Interns the argument string into the string table and returns a
//   pointer to the internal node.  The client is honor-bound not to
//   preserve this pointer beyond the life of the hash table.  An
//   assertion is used in other funtions to verify membership.  There is
//   no difference between insert and lookup.  If the key is not found,
//   it is inserted then returned.  If it is found, the existing node
//   reference is returned.
//
//   The argument string is copied onto the heap with strdup(3), since
//   the second argument is a loan argument, not a transfer of ownership
//   argument.  You may of course use malloc(3) and strcpy(3) instead.
//   The hash number is also cached in the node to avoid having to
//   recompute it.
//
stringnode_ref intern_stringtable (stringtable_ref, cstring);

#endif
