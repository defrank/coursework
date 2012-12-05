/***********************************************************************
 * $Id: $
 * Derek Frank, dmfrank@ucsc.edu
 * CMPE 150L Winter 2012
 * Project - eVote
 *
 * NAME
 *   evs.c - main evs server program
 *
 * DESCRIPTION
 *   The server side of the evs program.  Listens to port 1337.
 *
 * SYNOPSIS
 *   evs
 *
 **********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>      // standard symbolic types and constants
#include <errno.h>       // names for "errno" values (error numbers)
#include <sys/types.h>   // prerequisite typedefs
#include <sys/socket.h>  // struct sockaddr; sys. prototypes and consts
#include <netinet/in.h>  // struct sockaddr_in; byte ordering macros
#include <arpa/inet.h>   // utility function prototypes

#include "list.h"        // user-defined data structure
#include "auxlib.h"      // user-defined utility functions

////
// options - struct
//
//   Stores the results from scanning the command line options.
//
struct options {
   bool badopt;
};

////
// error
//
// Prints and error to stdout and exits the program.
//
void error (const char *msg) {
   errprintf ("%s: %s\n", "ERROR", msg);
   exit (get_exitstatus());
}

////
// scan_opts
//
//   Uses getopt() to scan options from the command line.  Sets
//   necessary flags.  Prints an error message for incorrect options,
//   but continues running the program.
//
void scan_opts (int argc, char **argv, struct options *options) {
   int option;
   opterr = 0;
   for(;;) {
      option = getopt (argc, argv, "");
      if (option == EOF) break;
      switch (option) {
         default:  errprintf ("%:bad option (%c)\n", optopt);
                   options->badopt = TRUE;                    break;
      }
   }
   // Print error for bad options.
   if (optind > argc || options->badopt) {
      errprintf ("%s: %s\n", "Usage", get_execname());
   }
}

void vote_func (int sockfd, list_ref voted, list_ref poll) {
   int n;
   char buffer[256];
   bzero (buffer,256);
   // tell client ready
   n = write (sockfd, "okay", 4);
   if (n < 0) error ("writing to socket");
   // receive voter ID number
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   // give feedback on successful or unsuccessful vote
   bool hasvoted = push_id (buffer, voted);
   if (hasvoted) {
      n = write (sockfd, "voted", 5);
      if (n < 0) error ("writing to socket");
      return;
   }
   n = write (sockfd, "okay", 4);
   if (n < 0) error ("writing to socket");
   bzero (buffer, 256);
   // receive candidate name
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   // add a vote for the candidate
   push_vote (buffer, poll);
   // send "okay" to verify
   n = write (sockfd, "okay", 4);
   if (n < 0) error ("writing to socket");
}

void results_func (int sockfd, list_ref poll) {
   int n;
   char buffer[256];
   bzero (buffer, 256);
   // send length of results
   int len = get_length (poll);
   sprintf (buffer, "%d", len);
   n = write (sockfd, buffer, 255);
   if (n < 0) error ("writing to socket");
   bzero (buffer, 256);
   // prepare results to send
   char **names = names_array (poll);
   int *points = points_array (poll);
   // check "okay" to continue
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   if (strcmp (buffer, "okay") != 0) error ("sending results");
   bzero (buffer, 256);
   // send results one by one
   int i;
   for (i = 0; i < len; ++i) {
      // send a candidate name
      n = write (sockfd, names[i], strlen (names[i]));
      if (n < 0) error ("writing to socket");
      // verify received
      n = read (sockfd, buffer, 255);
      if (n < 0) error ("reading from socket");
      if (strcmp (buffer, "okay") != 0) error ("sending results");
      bzero (buffer, 256);
      // send the votes of the candidate
      sprintf (buffer, "%d", points[i]);
      n = write (sockfd, buffer, 255);
      if (n < 0) error ("writing to socket");
      bzero (buffer, 256);
      // verify received
      n = read (sockfd, buffer, 255);
      if (n < 0) error ("reading from socket");
      if (strcmp (buffer, "okay") != 0) error ("sending results");
      bzero (buffer, 256);
   }
   delete_names (names, len);
   delete_points (points);
}

////
// main
//
int main (int argc, char **argv) {
   // Creat a struct for the options.
   struct options options = {FALSE};
   set_execname (argv[0]); // Set the execution name.
   // Get the options from the command line.
   scan_opts (argc, argv, &options);

   // prepare to record votes and voters
   list_ref voted = new_list ();
   list_ref poll = new_list ();

   //
   int sockfd, newsockfd;
   int portno = 1337; // server's listening port number
   socklen_t clilen;
   char buffer[256];
   struct sockaddr_in serv_addr, cli_addr;
   int n;
   // SOCKET
   sockfd = socket (AF_INET, SOCK_STREAM, 0);
   if (sockfd < 0) error ("opening socket");
   bzero ((char *) &serv_addr, sizeof (serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_addr.s_addr = INADDR_ANY;
   serv_addr.sin_port = htons (portno);
   // BIND
   if (bind (sockfd, (struct sockaddr *) &serv_addr,
             sizeof (serv_addr)) < 0) error ("on bind");
   // LISTEN
   listen (sockfd, 5);

   // Loop for socket connections
   for (;;) {
      // ACCEPT
      clilen = sizeof (cli_addr);
      newsockfd = accept (sockfd, (struct sockaddr *) &cli_addr, &clilen);
      if (newsockfd < 0) error ("on accept");
      bzero (buffer, 256);
      
      // READ/RECV
      n = read (newsockfd, buffer, 255);
      if (n < 0) error ("reading from socket");

      // Loop for protocols/tasts
      while (strcmp (buffer, "done") != 0) {
         // vote protocol
         if (strcmp (buffer, "vote") == 0)
            vote_func (newsockfd, voted, poll);
         // results protocol
         if (strcmp (buffer, "results") == 0)
            results_func (newsockfd, poll);
         // read next protocol/task
         bzero (buffer, 256);
         n = read (newsockfd, buffer, 255);
         if (n < 0) error ("reading from socket");
      }

      // send verify "done"
      n = write (newsockfd, "done", 4);
      if (n < 0) error ("writing to socket");

      // CLOSE CONNECTION
      close (newsockfd);
   }

   // CLOSE SOCKET
   close (sockfd);

   // free lists
   delete_list (poll);
   delete_list (voted);
   return get_exitstatus();
}
