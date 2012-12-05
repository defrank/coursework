/* Names: David S. Zou, Derek M. Frank, Harrison Vuong
 * Class: CS111 Spring 2012 (Operating Systems)
 * Instructor: Ethan Miller
 * Project #2 - Writer Program for Reader/Writer Problem
 */

#include <stdio.h>
#include <lib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>

void writer (int mutex, int writing, int priority, int writerid)
{
    FILE *wtimefile;
    long randnum, compare;
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, NULL);

    // wait for a certain amount of time...
    // wtimefile = fopen("wtimefile", "r");
    // if (wtimefile == NULL)
    // {
    //     // create the file then open it again
    //     fclose(wtimefile);
    //     wtimefile = fopen("wtimefile", "w");
    //     fprintf(wtimefile, "0\n");
    //     fclose(wtimefile);
    //     wtimefile = fopen("wtimefile", "r");
    // }
    srandom((tv.tv_usec / 100) + (tv.tv_sec * 100));
    randnum = random() % 200000;
    if (randnum < 20000)
    { 
        randnum += 20000;
    }
    // fscanf(wtimefile, "%ld", &compare);
    // fclose(wtimefile);
    // if (compare == randnum)
    // {
    //     randnum = random() % 200000;
    //     if (randnum < 20000)
    //     { 
    //         randnum += 20000;
    //     }       
    // }
    // // write new random number to file
    // rtimefile = fopen("rtimefile", "w");
    // fprintf(rtimefile, "%ld\n", randnum);
    // fclose(rtimefile);

    printf("writer %d entering waiting state for %ld microseconds\n", writerid, randnum);
    usleep(randnum);

	semdown(priority);
	semdown(writing);
	// write some stuff
    printf ("writer %d is currently writing\n", writerid);
	semup(writing);
	semup(priority);
    printf ("writer %d is finished writing\n", writerid);
}

int main (int argc, char **argv) 
{
    if (argc < 2) {
        fprintf(stderr, "<syntax>: %s <writerid>\n", argv[0]);
        return -1;
    }

    char *exec = argv[0];
    int wid = atoi(argv[1]);
    int mutex = 0;
    int writing = 0;
    int priority = 0;

    // grab semaphores from file
    FILE *fp;
    fp = fopen("semfile", "r");
    if (fp == NULL) 
    {
        fprintf(stderr, "writer: Cannot open semfile\n");
        return -1;
    }
    fscanf(fp, "%d", &mutex);
    fscanf(fp, "%d", &writing);
    fscanf(fp, "%d", &priority);
    fclose(fp);

    writer(mutex, writing, priority, wid);
    return 0;
}
