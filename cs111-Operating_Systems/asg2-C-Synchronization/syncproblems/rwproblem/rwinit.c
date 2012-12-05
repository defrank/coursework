/* Names: David S. Zou, Derek M. Frank, Harrison Vuong
 * Class: CS111 Spring 2012 (Operating Systems)
 * Instructor: Ethan Miller
 * Project #2 - Semaphore Init Program for Reader/Writer Problem
 */

#include <stdio.h>
#include <lib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

int main (int argc, char **argv) 
{
    FILE *fp, *fp2;
    int mutex = 0;
    int writing = 0;
    int priority = 0;

    // check if any semaphore intializations returned an error
    mutex = seminit(0,1);
    if (mutex < 0)
    {
    switch (errno) {
        case EEXIST:
            fprintf(stderr, "%s: mutex semaphore already exists\n", argv[0]);
            return -1;
            break;
        case EAGAIN:
            fprintf(stderr, "%s: cannot create mutex semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case EINVAL:
            fprintf(stderr, "%s: mutex id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        case -EEXIST:
            fprintf(stderr, "%s: mutex semaphore already exists\n", argv[0]);
            return -1;
            break;
        case -EAGAIN:
            fprintf(stderr, "%s: cannot create mutex semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case -EINVAL:
            fprintf(stderr, "%s: mutex id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        default:
            fprintf(stderr, "%s: Unknown error returned on creation of mutex semaphore\n", argv[0]);
            return -1;
            break;
        }
    }
    writing = seminit(0,1);
    if (writing < 0)
    {
    switch (errno) {
        case EEXIST:
            fprintf(stderr, "%s: writing semaphore already exists\n", argv[0]);
            return -1;
            break;
        case EAGAIN:
            fprintf(stderr, "%s: cannot create writing semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case EINVAL:
            fprintf(stderr, "%s: writing id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        case -EEXIST:
            fprintf(stderr, "%s: writing semaphore already exists\n", argv[0]);
            return -1;
            break;
        case -EAGAIN:
            fprintf(stderr, "%s: cannot create writing semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case -EINVAL:
            fprintf(stderr, "%s: writing id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        default:
            fprintf(stderr, "%s: Unknown error returned on creation of writing semaphore\n", argv[0]);
            return -1;
            break;
        }
    }
    priority = seminit(0,1);
    if (priority < 0)
    {
    switch (errno) {
        case EEXIST:
            fprintf(stderr, "%s: priority semaphore already exists\n", argv[0]);
            return -1;
            break;
        case EAGAIN:
            fprintf(stderr, "%s: cannot create priority semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case EINVAL:
            fprintf(stderr, "%s: priority id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        case -EEXIST:
            fprintf(stderr, "%s: priority semaphore already exists\n", argv[0]);
            return -1;
            break;
        case -EAGAIN:
            fprintf(stderr, "%s: cannot create priority semaphore due to max amount\n", argv[0]);
            return -1;
            break;
        case -EINVAL:
            fprintf(stderr, "%s: priority id is negative or value not in range (should not reach here!)\n", argv[0]);
            return -1;
            break;
        default:
            fprintf(stderr, "%s: Unknown error returned on creation of priority semaphore\n", argv[0]);
            return -1;
            break;
        }
    }
    printf("%s mutex id    : %d\n", argv[0], mutex);
    printf("%s writing id  : %d\n", argv[0], writing);
    printf("%s priority id : %d\n", argv[0], priority);

    // write semaphore ids to a file named 'tempfile'
    fp = fopen("semfile", "w");
    fprintf(fp, "%d ", mutex);
    fprintf(fp, "%d ", writing);
    fprintf(fp, "%d\n", priority);
    fclose(fp);

    fp2 = fopen("numreaders", "w");
    fprintf(fp2, "0\n");
    fclose(fp2);

    return 1;
}