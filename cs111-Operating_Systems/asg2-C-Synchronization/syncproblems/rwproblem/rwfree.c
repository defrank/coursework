/* Names: David S. Zou, Derek M. Frank, Harrison Vuong
 * Class: CS111 Spring 2012 (Operating Systems)
 * Instructor: Ethan Miller
 * Project #2 - Semaphore Freeing Program for Reader/Writer Problem
 */

#include <stdio.h>
#include <lib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main (int argc, char **argv)
{
    int mutex, writing, priority;

    // grab semaphores from file
    FILE *fp;
    fp = fopen("semfile", "r");
    if (fp == NULL) 
    {
        fprintf(stderr, "rwfree: Cannot open semfile\n");
        return -1;
    }
    fscanf(fp, "%d", &mutex);
    fscanf(fp, "%d", &writing);
    fscanf(fp, "%d", &priority);
    fclose(fp);

    if (semfree(mutex) <= 0)
    {
        fprintf(stderr, "%s: Cannot free mutex semaphore\n", argv[0]);
        return -1;
    }
    if (semfree(writing) <= 0)
    {
        fprintf(stderr, "%s: Cannot free writing semaphore\n", argv[0]);
        return -1;
    }
    if (semfree(priority) <= 0)
    {
        fprintf(stderr, "%s: Cannot free priority semaphore\n", argv[0]);
        return -1;
    }

    // delete the files
    unlink("numreaders");
    unlink("rtimefile");
    // unlink("wtimefile");
    unlink("semfile");

    return 0;
}