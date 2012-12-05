/***********************************************************************
 * $Id: stringtable.c,v 1.1 2012/12/05 07:59:55 dmf Exp $
 * Derek Frank, dmfrank@usc.edu
 *
 * NAME
 *   stringtable.c - implementation file for the string table ADT
 *
 * DESCRIPTION
 *   Implementation for the string table ADT.  The string table ADT will
 *   operate as a hash table.  Uses separate chaining for collision
 *   resolution.
 *
 **********************************************************************/


#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "stringtable.h"


/******************* Private Structs: not exported ********************/

////
// stringnode - struct
//
//   A node that contains a pointer to a string on the heap, along
//   with a collision resolution pointer and a cached hash number.
//   Also a pointer to an internal static string identifier.
//
typedef struct stringnode {
   cstring str; // pointer to a string on the heap
   struct stringnode* next; // collision resolution pointer to next node
   hashcode_t hashnum; // cached hash number
   cstring tag; // internal static string
                // - tag field to verify class membership
} stringnode;

////
// stringtable - struct
//
//   Structure that consists of a pointer to the array of hash headers
//   and a dimension.  Also, a pointer to an internal static string
//   identifier so that an assertion can be used to check the validity
//   of the node.  Also, the number of elements in the table is cached
//   in this node in order to determine when the table needs
//   doubling.  Other fields if needed.
//
typedef struct stringtable {
   stringnode_ref *headers; // array of hash headers
   int dim; // dimension of header array
   int size; // number of elements in the hash table
   cstring tag; // internal static string
                // - tag field to verify class membership
} stringtable;

/******************** Internal Static Strings - Tags ******************/

static char* stringnode_tag = "struct stringnode";
static char* stringtable_tag = "struct stringtable";


/****************** Type Checking-Assertion Functions *****************/

////
// is_stringnode
//
//   Safety check for the RTTI (run time type identification).  Usually
//   called within an assert() of a function using a string node.
//
bool is_stringnode (void *object) {
   stringnode_ref node = (stringnode_ref) object;
   return node != NULL && node->tag == stringnode_tag;
}

////
// is_stringtable
//
//   Safety check for the RTTI (run time type identification).  Usually
//   called within an assert() of a function using a string table.
//
bool is_stringtable (void *object) {
   stringtable_ref table = (stringtable_ref) object;
   return table != NULL && table->tag == stringtable_tag;
}


/******************** Constructors-Destructors ************************/

////
// new_stringnode - Constructor
//
//   Creates a new string node and returns its handle.
//
stringnode_ref new_stringnode (hashcode_t num, cstring string) {
   assert (string != NULL);
   size_t sn_size = sizeof (stringnode);
   stringnode_ref node = malloc (sn_size);
   node->str = strdup (string); // contains a call to malloc()
   assert (node->str != NULL); // null check
   node->next = NULL;
   node->hashnum = num;
   node->tag = stringnode_tag; // internal static string
   DEBUGF ('f', "malloc (%d) = %p-> hash number: %d: %p->\"%s\"\n",
           sn_size, node, node->hashnum, node->str, node->str);
   return node;
}

////
// new_stringtable - Constructor
//
//   Creates a new string table and returns its handle.  Uses as its
//   initial capacity some arbitrary odd number, such as 31.
//
stringtable_ref new_stringtable (void) {
   size_t st_size = sizeof (stringtable);
   stringtable_ref hashtable = malloc (st_size);
   int n = 31; // specified to be one less than a power of two
   hashtable->headers = calloc (n, sizeof (stringnode_ref) );
   hashtable->dim = n;
   hashtable->size = 0; // empty
   hashtable->tag = stringtable_tag; // internal static string
   DEBUGF ('f', "malloc (%d) = %p-> size: %d  %p-> dim: %d\n",
           st_size, hashtable, hashtable->size, hashtable->headers,
           hashtable->dim);
   return hashtable;
}

////
// delete_stringnode - Destructor
//
//   Uses free(3) to delete the string node.  Implementation of the
//   function is optional as it is a helper function to
//   delete_stringtable().
//
void delete_stringnode (stringnode_ref node) {
   if (node == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "delete_stringnode()",
                 "null string node reference");
   assert (is_stringnode(node) );
   free (node->str);
   node->str = NULL;
   node->next = NULL;
   free (node);
   node = NULL;
}

////
// delete_stringtable - Destructor
//
//   Dismantles the entire hash table and uses free(3) to delete all
//   internal nodes.  Implementation of the function is optional.
//
void delete_stringtable (stringtable_ref table) {
   if (table == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "delete_stringtable()",
                 "null string table reference");
   assert (is_stringtable(table) );
   stringnode_ref curr;
   stringnode_ref temp;
   int i;
   for (i = 0; i < table->dim; ++i) {
      curr = table->headers[i];
      while (curr != NULL) {
         temp = curr->next;
         delete_stringnode (curr);
         curr = temp;
      }
      table->headers[i] = NULL;
   }
   free (table->headers);
   table->headers = NULL;
   free (table);
   table = NULL;
}


/******************** Miscellaneous-Helper Functions ******************/

////
// strhash
//
//   Returns a hashcode for a given string.  Uses a bit shift.
//
hashcode_t strhash (cstring string) {
   hashcode_t hashcode = 0;
   assert (string != NULL);
   for (;;) {
      hashcode_t byte = (unsigned char) *string++;
      if (byte == '\0') break;
      hashcode = (hashcode << 5) - hashcode + byte;
   };
   return hashcode;
}

////
// find
//
//   A helper function to find the handle of a string node containing
//   the given string.
//
stringnode_ref find (stringtable_ref table, cstring string) {
   assert (is_stringtable (table) );
   // print error message if the string table reference is null
   if (table == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "find()",
                 "null string table reference");
   // print error message if the character pointer (cstring) is null
   if (string == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "find()", "null pointer");
   stringnode_ref curr;
   int i;
   for (i = 0; i < table->dim; ++i) {
      curr = table->headers[i];
      if (curr != NULL) {
         if (0 == strcmp (string, curr->str) ) break;
         curr = curr->next;
         while (curr != NULL) {
            if (0 == strcmp (string, curr->str) ) break;
            curr = curr->next;
         }
      }
   }
   return curr;
}


/********************* Access Functions *******************************/

////
// debugdump_stringtable
//
//   Dumps out the hash table in debug format:
//         24     348883689  "hello"
//               4294967295  "there"
//         92     338729983  "67"
//
//   In other words, print the hash header number in %8d format
//   followed by spaces, then the hash number (%12u), and then the
//   strings in %s format inside quotes in a column.  In the above
//   example, the two strings in bucket 24 have collided.  It will
//   always be true that the second number modulo the size of the hash
//   table will be equal to the first number.
//
void debugdump_stringtable (stringtable_ref table, FILE* outfile) {
   assert (is_stringtable (table) );
   // print error message if the string table reference is null
   if (table == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "debugdump_stringtable()",
                 "null string table reference");
   // print error message if the FILE reference is null
   if (outfile == NULL)
      errprintf ("%s: %s: %s: %s\n", "Error", "stringtable.c",
                 "debugdump_stringtable()",
                 "null FILE reference");
   int length = table->dim;
   int i;
   for (i = 0; i < length; ++i) {
      stringnode_ref curr = table->headers[i];
      if (curr != NULL) {
         xfprintf (outfile, "%8d  %12u  \"%s\"\n",
                   i, curr->hashnum, curr->str);
         curr = curr->next;
         while (curr != NULL) {
            xfprintf (outfile, "          %12u  \"%s\"\n",
                      curr->hashnum, curr->str);
            curr = curr->next;
         }
      }
   }
}

////
// peek_stringtable
//
//   Returns a pointer to a character string, such as may be used for
//   printing.
//
cstring peek_stringtable (stringnode_ref node) {
   assert (is_stringnode (node) );
   // print error message if the string node reference is null
   if (node == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "peek_stringtable()",
                 "null string node reference");
   return node->str;
}

////
// hashcode_stringtable
//
//   Returns the cached hash number associated with this string node.
//   Does not recompute the number.
//
hashcode_t hashcode_stringtable (stringnode_ref node) {
   assert (is_stringnode (node) );
   // print error message if the string node reference is null
   if (node == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "hashcode_stringtable()",
                 "null string node reference");
   return node->hashnum;
}


/********************** Manipulation Functions ************************/

////
// double_capacity
//
//   Doubles the capacity of the header array in the string table.
//   Also recomputes the modulo and moves the pointers to their
//   correct locations with respect to the new size of the array.
//   Array size should have the form of one minus a power of two.
//   Since array size starts at 31 = 2^5 - 1, the doubled array should
//   be no less than 63 = 2^6 - 1.
//
void double_capacity (stringtable_ref table) {
   assert (is_stringtable (table) );
   // print error message if the string table reference is null
   if (table == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "double_capacity()",
                 "null string table reference");
   int length = table->dim; // original hash header array dimension
   int n = ( (length + 1) * 2) - 1; // new hash header array dimension
   stringnode_ref *arr = calloc (n, sizeof (stringnode_ref) );
   stringnode_ref curr;
   stringnode_ref temp;
   int i;
   for (i = 0; i < length; ++i) {
      curr = table->headers[i];
      while (curr != NULL) {
         hashcode_t place = curr->hashnum % n; // array placement
         if (arr[place] == NULL) { arr[place] = curr; }
         else {
            temp = arr[place];
            while (temp->next != NULL) temp = temp->next;
            temp->next = curr;
         }
         temp = curr;
         curr = curr->next;
         temp->next = NULL;
      }
      table->headers[i] = NULL;
   }
   free (table->headers);
   table->headers = arr;
   table->dim = n;
   curr = NULL;
   temp = NULL;
   arr = NULL;
}

////
// intern_stringtable
//
//   Interns the argument string into the string table and returns a
//   pointer to the internal node.  The client is honor-bound not to
//   preserve this pointer beyond the life of the hash table.  An
//   assertion is used in other funtions to verify membership.  There
//   is no difference between insert and lookup.  If the key is not
//   found, it is inserted then returned.  If it is found, the
//   existing node reference is returned.
//
//   The argument string is copied onto the heap with strdup(3), since
//   the second argument is a loan argument, not a transfer of
//   ownership argument.  You may of course use malloc(3) and
//   strcpy(3) instead.  The hash number is also cached in the node to
//   avoid having to recompute it.
//
//   Also takes care of collision resolution by separate chaining.
//   Doubling  the array header size when thestring table's loading
//   factor exceeds 0.75 (number of nodes / number of headers).
//
stringnode_ref intern_stringtable (stringtable_ref table,
                                   cstring string) {
   assert (is_stringtable (table) );
   // print error message if the string table reference is null
   if (table == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "intern_stringtable()",
                 "null string table reference");
   // print error message if the character pointer (cstring) is null
   if (string == NULL)
      errprintf ("%s: %s: %s: %s: %s\n", get_execname(), "Error",
                 "stringtable.c", "intern_stringtable()",
                 "null cstring");
   stringnode_ref node = new_stringnode (strhash (string), string);
   hashcode_t place = node->hashnum % table->dim; // array placement
   stringnode_ref curr = table->headers[place];
   if (curr != NULL) {
      // already exists so free new node and return existing node
      if (strcmp (curr->str, node->str) == 0) {
         delete_stringnode (node);
         node = NULL;
         return curr;
      }
      while (curr->next != NULL) {
         curr = curr->next;
         // already exists so free new node a nd return existing node
         if (strcmp (curr->str, node->str) == 0) {
            delete_stringnode (node);
            node = NULL;
            return curr;
         }
      }
      // insert node
      curr->next = node;
      ++table->size;
   }
   // insert node
   else {
      table->headers[place] = node; ++table->size; }
   curr = NULL;
   // maintain collision resolution
   double numNodes = table->size;
   double numHeads = table->dim;
   if ( (numNodes / numHeads) > 0.75) double_capacity (table);
   return node;
}
