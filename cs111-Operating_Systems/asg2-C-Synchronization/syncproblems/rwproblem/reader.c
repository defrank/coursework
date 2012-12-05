/* Names: David S. Zou, Derek M. Frank, Harrison Vuong
 * Class: CS111 Spring 2012 (Operating Systems)
 * Instructor: Ethan Miller
 * Project #2 - Reader Program for Reader/Writer Problem
 */

#include <stdio.h>
#include <lib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>

void reader (int mutex, int writing, int priority, int readerid)
{
	FILE *nreadfile;
    FILE *rtimefile;
	int nreaders; 
    long randnum, compare;
    struct timeval tv;
    gettimeofday(&tv, NULL);

    // wait for a certain amount of time...
    rtimefile = fopen("rtimefile", "w");
    srandom((tv.tv_usec / 100) + (tv.tv_sec * 100));
    randnum = random() % 200000;
    fprintf(rtimefile, "%ld\n", randnum);
    fclose(rtimefile);
    if (randnum < 20000)
    {
        randnum += 20000;
    }
    printf("reader %d entering 1st waiting state for %ld microseconds\n", readerid, randnum);
    usleep(randnum);

	semdown(priority);
	semdown(mutex);

    nreadfile = fopen ("numreaders", "r");     
	fscanf(nreadfile, "%d", &nreaders);
    fclose(nreadfile);
	nreaders += 1;
    nreadfile = fopen ("numreaders", "w"); 
    fprintf(nreadfile, "%d\n", nreaders);
    fclose(nreadfile);

	if (nreaders == 1)
	{
		semdown(writing);
	}
	semup(mutex);
	semup(priority);
	// Read some stuff

    printf ("reader %d is currently reading\n", readerid);

    // wait for a certain amount of time...
    rtimefile = fopen("rtimefile", "r");
    gettimeofday(&tv, NULL);
    srandom((tv.tv_usec / 100) + (tv.tv_sec * 100));
    randnum = random() % 200000;
    if (randnum < 20000)
    {
        randnum += 20000;
    }
    fscanf(rtimefile, "%ld", &compare);
    fclose(rtimefile);
    // do the randomizing again if same as previous random number
    if (compare == randnum)
    {
        randnum = random() % 200000;
        if (randnum < 20000)
        {
            randnum += 20000;
        }
        printf("randnum: %ld\n", randnum);
    }
    // save the different random number
    rtimefile = fopen("rtimefile", "w");
    fprintf(rtimefile, "%ld\n", randnum);
    fclose(rtimefile);

    printf("reader %d entering 2nd waiting state for %ld microseconds\n", readerid, randnum);
    usleep(randnum);
    printf("reader %d exits waiting state.\n", readerid);
	semdown(mutex);

	nreadfile = fopen ("numreaders", "r");
	fscanf(nreadfile, "%d", &nreaders);
    fclose(nreadfile);
	nreaders -= 1;
    nreadfile = fopen ("numreaders", "w"); 
    fprintf(nreadfile, "%d\n", nreaders);
    fclose(nreadfile);

	if (nreaders == 0)
	{
		semup(writing);
	}
	semup(mutex);
    printf ("reader %d is finished reading\n", readerid);
}

int main (int argc, char **argv) 
{
    if (argc < 2) {
        fprintf(stderr, "<syntax>: %s <readerid>\n", argv[0]);
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
        fprintf(stderr, "reader: Cannot open semfile\n");
        return -1;
    }
    fscanf(fp, "%d", &mutex);
    fscanf(fp, "%d", &writing);
    fscanf(fp, "%d", &priority);
    fclose(fp);

    reader(mutex, writing, priority, wid);
    return 0;
}