/***********************************************************************
 * $Id: $
 * Derek Frank, dmfrank@ucsc.edu
 * CMPE 150L Winter 2012
 * Project - eVote
 *
 * NAME
 *   list.h - list header file
 *
 * DESCRIPTION
 *   A list data structure header file.
 *
 **********************************************************************/

#ifndef __LIST_H__
#define __LIST_H__

#include "auxlib.h"

/*************************** Typedefs *********************************/

////
// node_ref - reference
//
typedef struct node *node_ref;

////
// list_ref - reference
//
typedef struct list *list_ref;


/************************ Constructors-Destructors ********************/

////
// new_list - constructor
//
//   Construct a new list.
//
list_ref new_list (void);

////
// delete_list - destructor
//
//   Deletes a list.
//
void delete_list (list_ref);


/******************************* Functions ****************************/

////
// push_vote
//
//   If the candidate does not exist, then create a new node and set
//   the points to 1.  Otherwise, just add 1 to the candidates points.
//
void push_vote (char *, list_ref);

////
// push_id
//
//   If the voter ID does not exist, then add it to the list and return
//   FALSE.  Otherwise, return TRUE.
bool push_id (char *, list_ref);

////
// get_length
//
//   Get the number of nodes in the list.  Could be number of candidates
//   or number of voter IDs.
//
int get_length (list_ref);

////
// name_array
//
//   Creates and returns an array of candidate names.
//
char **names_array (list_ref);

////
// delete_names
//
//   Frees and array of candidate names previously created.
//
void delete_names (char **, int);

////
// point_array
//
//   Creates and returns an array of candidate points.
//
int *points_array (list_ref);

////
// delete_points
//
//   Frees and array of points previously created.
//
void delete_points (int *);


#endif
