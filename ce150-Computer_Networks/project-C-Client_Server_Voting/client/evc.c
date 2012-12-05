/***********************************************************************
 * $Id: $
 * Derek Frank, dmfrank@ucsc.edu
 * CMPE 150L Winter 2012
 * Project - eVote
 *
 * NAME
 *   evc.c - main evc client program
 *
 * DESCRIPTION
 *   The client side of the evs program.
 *   Default server port number: 1337.
 *
 * SYNOPSIS
 *   evc [-s serverIP] [-r] [-v candidateName voterIDnumber] 
 *
 * OPTIONS
 *   -s serverIP
 *      Server - required - specifies the IP address of the server.
 *
 *   -v candidateName
 *      Vote - the client will send a voter ID number and a candidate
 *      name to the server.  the client should display whether the
 *      vote was successfully recorded or not.
 *
 *   -r
 *      Results - the client will send a request for the current poll
 *      results.  If used in combination with the vote option, it will
 *      ask for the results after the vote is recorded.  The client
 *      should display the list of candidates returned by the server
 *      along with the number of votes each candidate has received.
 *
 **********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>      // standard symbolic types and constants
#include <errno.h>       // names for "errno" values (error numbers)
#include <sys/types.h>   // prerequisite typedefs
#include <sys/socket.h>  // struct sockaddr; sys. prototypes and consts
#include <netinet/in.h>  // struct sockaddr_in; byte ordering macros
#include <arpa/inet.h>   // utility function prototypes
#include <netdb.h>       // nerwork info lookup prototypes and structs

#include "auxlib.h"      // user-defined utility functions


////
// options - struct
//
//   Stores the results from scanning the command line options.
//
struct options{
   bool badopt;
   bool server;
   char *serverIP;
   bool vote;
   char *candidateName;
   bool results;
   char *voterID;
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
//   necessary flags.  Prints an error message for incorrect options.
//
void scan_opts (int argc, char **argv, struct options *options) {
   int option;
   opterr = 0;
   for(;;) {
      option = getopt (argc, argv, "s:v:r");
      if (option == EOF) break;
      switch (option) {
         case 's': options->server = TRUE;
                   options->serverIP = optarg;                break;
         case 'v': options->vote = TRUE;
                   options->candidateName = optarg;
                   options->voterID = argv[argc - 1];         break;
         case 'r': options->results = TRUE;                   break;
         default:  errprintf ("%:bad option (%c)\n", optopt);
                   options->badopt = TRUE;                    break;
      }
   }
   // Print error for bad options.
   if (optind > argc || options->badopt || (argc > optind + 1)) {
      errprintf ("%s: %s %s %s %s %s\n", "Usage", get_execname(),
                 "-s[serverIP]", "-v[candidateName]", "-r",
                 "voterIDnumber");
      error ("bad usage");
   }
   // there must be a voter ID number with a vote
   if (options->vote && (argc != (optind + 1))) {
      errprintf ("%s: %s %s %s %s %s\n", "Usage", get_execname(),
                 "-s[serverIP]", "-v[candidateName]", "-r",
                 "voterIDnumber");
      error ("need voter ID number to vote");
   }
   // If required option -s is not present, then end program.
   if (!options->server) {
      errprintf ("%s: %s %s %s %s %s\n", "Usage", get_execname(),
                 "-s[serverIP]", "-v[candidateName]", "-r",
                 "voterIDnumber");
      error ("no server IP");
   }
   // If any argument exceeds a length of 255
   if ((options->serverIP != NULL && strlen (options->serverIP) >  255)
       || (options->candidateName != NULL &&
           strlen (options->candidateName) >  255) ||
       (options->voterID != NULL && strlen (options->voterID) >  255 ))
      error ("no argument should exceed a length of 255 characters");
}

////
// vote_func
//
void vote_func (int sockfd, char *cand, char *voter) {
   int n;
   char buffer[256];
   bzero (buffer, 256);
   // tell server about to vote
   n = write (sockfd, "vote", 4);
   if (n < 0) error ("writing to socket");
   // verify protocol received
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   if (strcmp(buffer,"okay") != 0) error ("FIRST voting");
   bzero (buffer, 256);
   // give server voter ID number
   n = write (sockfd, voter, strlen (voter));
   if (n < 0) error ("writing to socket");
   // if unsuccesful vote, then cannot give candidate
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   if (strcmp (buffer, "voted") == 0) {
      printf ("Unsuccesful voting!\n");
      return;
   }else if (strcmp (buffer,"okay") != 0) { error ("SECOND voting"); }
   // if vote is successful
   printf ("Succesfully voted!\n");
   bzero (buffer, 256);
   // give server candidate name
   n = write (sockfd, cand, strlen (cand));
   if (n < 0) error ("writing to socket");
   // verify candidate received
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   if (strcmp(buffer,"okay") != 0) error ("THIRD voting");
}

////
// results_func
//
void results_func (int sockfd) {
   int n;
   char buffer[256];
   bzero (buffer, 256);
   // tell server to send results
   n = write (sockfd, "results", 7);
   if (n < 0) error ("writing to socket");
   // receive number of candidates
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading from socket");
   int num = atoi (buffer);
   bzero (buffer, 256);
   // send "okay" to continue protocol
   n = write (sockfd, "okay", 4);
   if (n < 0) error ("writing to socket");
   // prepare to receive results
   char **names = calloc (num, sizeof (char *));
   int *points = calloc (num, sizeof (int *));
   // record each candidate and score
   int j;
   for (j = 0; j < num; ++j) {
      // read candidate name
      n = read (sockfd, buffer, 255);
      if (n < 0) error ("reading from socket");
      names[j] = strdup (buffer);
      bzero (buffer, 256);
      // send "okay" to continue protocol
      n = write (sockfd, "okay", 4);
      if (n < 0) error ("writing to socket");
      // read candidate points
      n = read (sockfd, buffer, 255);
      if (n < 0) error ("reading from socket");
      points[j] = atoi (buffer);
      bzero (buffer, 256);
      // send "okay" to continue protocol
      n = write (sockfd, "okay", 4);
      if (n < 0) error ("writing to socket");
   }
   // print results
   printf ("Current poll results:\n");
   if (j == 0) printf ("   There have been no votes.\n");
   for (j = 0; j < num; ++j) {
      printf ("   %20s      %3d\n", names[j], points[j]);
      free (names[j]); // strdup uses malloc
   }
   free (names);
   free (points);
}


////
// main
//
int main (int argc, char **argv) {
   // Creat a struct for the options.
   struct options options =
      {FALSE, FALSE, NULL, FALSE, NULL, FALSE, NULL};
   set_execname (argv[0]); // Set the execution name.
   // Get the options from the command line.
   scan_opts (argc, argv, &options);

   //
   char buffer[256];
   bzero (buffer, 256);
   int sockfd, n;
   int portno = 1337; // Default server port number 1337
   struct sockaddr_in serv_addr;
   struct hostent *server;

   // SOCKET
   sockfd = socket (AF_INET, SOCK_STREAM, 0);
   if (sockfd < 0) error ("opening socket");
   // GETHOSTBYNAME
   server = gethostbyname (options.serverIP);
   if (server == NULL) error ("no such host");
   bzero ((char *) &serv_addr, sizeof (serv_addr));
   serv_addr.sin_family = AF_INET;
   bcopy ((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr,
          server->h_length);
   serv_addr.sin_port = htons (portno);
   // CONNECT
   if (connect (sockfd, (struct sockaddr *) &serv_addr,
                sizeof (serv_addr)) < 0) error ("connecting");

   // vote protocol
   if (options.vote) {
      // convert candidate name to all lower case
      char *cand = strdup (options.candidateName);
      char *voter = strdup (options.voterID);
      int i;
      for (i = 0; cand[i]; ++i) cand[i] = tolower (cand[i]);
      vote_func (sockfd, cand, voter);
      free (cand); // strdup uses malloc
      free (voter);
   }
   // results protocol
   if (options.results) results_func (sockfd);

   // finish protocol
   n = write (sockfd, "done", 4);
   if (n < 0) error ("writing to socket");   
   bzero (buffer, 256);
   // verify finish
   n = read (sockfd, buffer, 255);
   if (n < 0) error ("reading socket");
   if (strcmp (buffer, "done") != 0) error ("finishing");

   // CLOSE
  close (sockfd);

   return get_exitstatus();
}
