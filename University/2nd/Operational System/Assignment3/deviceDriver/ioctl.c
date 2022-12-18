#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include "ioctl.h"

int main (int argc, char **argv) {
    
    char *filename; /* the name of the device */
    int fd; /* device file descriptor */
    int result;
    char *errorMessage;
    
    if (argc != 2) {
	fprintf (stderr, "Exactly one argument required, exiting!\n");
	exit (1);
    }

    /* ioctl  can be performed only on opened device */
    filename = argv[1];
    fd = open (filename, O_RDONLY);
    if (fd < 0) {
	errorMessage = malloc(strlen(filename) + 40);
	sprintf(errorMessage, "Could not open file %s: ", filename);
	perror(errorMessage);
	exit (1);
    }

    result = ioctl (fd, RESET_COUNTER, 0);
    printf ("The result of the ioctl is %d\n", result);
    
    close (fd);
    return 0;
}

    
    

	
