/* $Id: list.h,v 1.4 2011-03-01 18:24:31-08 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 *
 * NAME
 *    list - header file
 *
 * DESCRIPTION
 *    A templated doubly linked list.  Includes a sorting algorithm,
 *    which uses the Heapsort algorithm.
 */

#ifndef __LIST_H__
#define __LIST_H__

#include <iostream>

using namespace std;

template<typename T> class list;

template<typename T>
ostream& operator<< (ostream &, const list<T> &);

template<typename T>
class list {
   friend ostream &operator<< <> (ostream &, const list<T> &);

   private:
      //
      // struct node
      //    Nested class definition of a node of specified template
      //    type.
      //
      struct node {
         private:
            /** Disable some implicit functions. **/
            node ();
            node (const node &that);
            node &operator= (const node &that);
            /** Node Fields. **/
            node* prev;
            node* next;
            T elem;

         public:
            /** Override implicit dtor **/
            ~node () {
               prev = NULL;
               next = NULL;
            }
            /** Other Ctors **/
            node (T that): elem (that) {
               prev = NULL;
               next = NULL;
            }
            /** Access Functions. **/
            T getElem () {
               return elem;
            }

            node* getPrev () {
               if (prev == NULL) return NULL;
               return prev;
            }

            node* getNext () {
               if (next == NULL) return NULL;
               return next;
            }

            /** Manipulation Functions. **/
            void setPrev (node* that) {
               prev = that;
            }

            void setNext (node* that) {
               next = that;
            }       

            void setElem (T that) {
               elem = that;
            }
      };

      /** List Fields. **/
      node* front;
      node* back;
      int length;
      int heapSize;
      
   public:
      /** Override implicit members. **/
      list<T> ();
      list<T> (const list<T> &that);
      list<T> &operator= (const list<T> &that);
      ~list<T> ();
      /** Access and Manipulation Functions. **/
      int size () const ;
      bool isempty () const ;
      bool contains_elem (const T &that) const ;
      void push (T elem);
      T pop ();
      /** Heapsort **/
      int parent (int i) const ;
      int left (int i) const ;
      int right (int i) const ;
      int getHeapSize () const ;
      void heapify (int i);
      void buildHeap ();
      void sort ();
};

#include "list.cpp"

#endif
