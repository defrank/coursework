/***********************************************************************
 * $Id: $
 * Derek Frank, dmfrank@ucsc.edu
 * CMPE 150L Winter 2012
 * Project - eVote
 *
 * NAME
 *   list.c - list implementation file
 *
 * DESCRIPTION
 *   A list data structure implementation file specifically designed
 *   for use with the eVote Server.  Handles an ambiguous queue filled
 *   with either candidate names with their corresponding votes or
 *   voter IDs that have already voted.
 *
 **********************************************************************/

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "auxlib.h"
#include "list.h"

/*************************** Structures *******************************/

////
// node - struct
//
//   A node structure.
//
typedef struct node {
   char *name;
   char *voterID;
   int points;
   node_ref next;
} node;

////
// list - struct
//
//   A list structure.
//
typedef struct list {
   node_ref head;
   node_ref tail;
   int length;
} list;


/************************ Constructors-Destructors ********************/

////
// new_name - constructor
//
//   Constructs a new node for a candidate name and points.
//
node_ref new_name (char *n) {
   assert (n != NULL);
   node_ref newnode = malloc (sizeof (node));
   newnode->voterID = NULL;
   newnode->name = strdup (n);
   newnode->points = 1;
   newnode->next = NULL;
   return newnode;   
}

////
// new_id - constructor
//
//   Constructs a new node for a voter ID number.
//
node_ref new_id (char *v) {
   assert (v != NULL);
   node_ref newnode = malloc (sizeof (node));
   newnode->name = NULL;
   newnode->voterID = strdup (v);
   newnode->next = NULL;
   newnode->points = 0;
   return newnode;   
}

////
// new_list
//
//   Constructs a new list that is initially empty.
//
list_ref new_list (void) {
   list_ref newlist = malloc (sizeof (list));
   newlist->head = NULL;
   newlist->tail = NULL;
   newlist->length = 0;
   return newlist;
}

////
// delete_node
//
//   Delete a node using free.
//
void delete_node (node_ref n) {
   assert (n != NULL);
   if (n->name == NULL) {
      free (n->voterID);
      n->voterID = NULL;
   }else if (n->voterID == NULL) {
      free (n->name);
      n->name = NULL;
   }
   n->next = NULL;
   free (n);
   n = NULL;
}


////
// delete_list
//
//   Frees a list and all of its nodes.
//
void delete_list (list_ref l) {
   assert (l != NULL);
   node_ref cur = l->head;
   node_ref next;
   while (cur != NULL) {
      next = cur->next;
      delete_node (cur);
      cur = next;
   }
   l->head = NULL;
   l->tail = NULL;
   free (l);
   l = NULL;
}


/******************************* Functions ****************************/

////
// push_vote
//
//   Determines if a name already exists in a list.  If it does not,
//   then adds it with a point of 1.  Otherwise, adds a point to it.
//
void push_vote (char *n, list_ref l) {
   assert (n != NULL);
   assert (l != NULL);
   node_ref cur = l->head;
   node_ref end = l->tail;
   if (cur == NULL) {
      l->head = new_name (n);
      l->tail = l->head;
      l->length = 1;
   } else {
      bool hit = FALSE;
      while (cur != NULL && !hit) {
         if (strcmp (cur->name, n) == 0) {
            cur->points = cur->points + 1;
            hit = TRUE;
         }
         cur = cur->next;
      }
      if (cur == NULL && !hit) {
         end->next = new_name (n);
         l->tail = end->next;
         l->length = l->length + 1;
      }
   }
}

////
// push_id
//
//   Determines if a voter ID number already exists in a list.  If it
//   does not, then adds it and returns FALSE.  Otherwise, returns TRUE.
//
bool push_id (char *v, list_ref l) {
   assert (v != NULL);
   assert (l != NULL);
   bool hit = FALSE;
   node_ref cur = l->head;
   node_ref end = l->tail;
   if (cur == NULL) {
      l->head = new_id (v);
      l->tail = l->head;
      l->length = 1;
   } else {
      while (cur != NULL && !hit) {
         if (strcmp (cur->voterID, v) == 0) hit = TRUE;
         cur = cur->next;
      }
      if (cur == NULL && !hit) {
         l->tail->next = new_id (v);
         l->tail = end->next;
         l->length = l->length + 1;
      }
   }
   return hit;
}

////
// get_length
//
//   Returns the number of nodes in a list.
//
int get_length (list_ref l) {
   assert (l != NULL);
   return l->length;
}

////
// name_array
//
//   Creates and returns an array of candidate names.
//
char **names_array (list_ref l) {
   assert (l != NULL);
   char **names = calloc (l->length, sizeof (char *));
   node_ref cur = l->head;
   int i;
   for (i = 0; i < l->length; ++i) {
      names[i] = strdup (cur->name);
      cur = cur->next;
   }
   return names;
}

////
// delete_names
//
//   Free an array of candidate names previously created.
//
void delete_names (char **names, int n) {
   assert (names != NULL);
   int i;
   for (i = 0; i < n; ++i) {
      free (names[i]);
      names[i] = NULL;
   }
   free (names);
   names = NULL;
}

////
// point_array
//
//   Creates and returns an array of points.
//
int *points_array (list_ref l) {
   assert (l != NULL);
   int *points = calloc (l->length, sizeof (int));
   node_ref cur = l->head;
   int i;
   for (i = 0; i < l->length; ++i) {
      points[i] = cur->points;
      cur = cur->next;
   }
   return points;
}

////
// delete_points
//
//   Free an array of points previously created.
//
void delete_points (int *points) {
   assert (points != NULL);
   free (points);
   points = NULL;
}
