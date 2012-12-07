/*
 * $Id: list5.c,v 1.1 2009-11-05 00:24:39-08 - - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#define xprintf (void) printf

typedef struct node *node_ref;
struct node {
   char *word;
   node_ref link;
};

int main (int argc, char **argv) {
   node_ref head = NULL;
   for (int argi = 0; argi < argc; ++argi) {
      node_ref node = malloc (sizeof (node_ref));
      assert (node != NULL);
      node->word = argv[argi];
      node->link = head;
      head = node;
   }
   for (node_ref curr = head; curr->link != NULL; curr = curr->link) {
      xprintf ("%p->node {word=%p->[%s], link=%p}\n",
               (void*) curr, (void*) curr->word,
               curr->word, (void*) curr->link);
   }
   while (head != NULL) {
      node_ref old = head;
      head = head->link;
      free (old);
   }
   return EXIT_SUCCESS;
}

