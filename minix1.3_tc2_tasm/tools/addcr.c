/*	addcr.c
 *
 *	Author: Deborah Mullen
 *	Date:   March 4, 1989
 *
 *      Compile with Turbo C, small model, linked with wildargs.obj
 *
 *	Function: This program is used to convert text files from
 *	Minix format to Dos format.  It replaces 0x0a (minix end of
 *	line) with 0x0d0a (Dos end of line).  This comes in handy
 *      when decompressing a library of files compressed under Minix.
 *
 *	Usage: addcr filenames.  Dos regular expressions accepted.
 *
 */
#include <stdio.h>
#include <fcntl.h>
#include <sys\stat.h>


#define BLOCKSIZE 4096	/* 4k block size for reading files */

char buf[BLOCKSIZE]; 
char tname[] = "ad000000.xxx";

main(argc, argv)
int argc;
char *argv[];
{

int infd, outfd;	/* file descriptors */
int i, cnt;

   if (argc < 2) {
	printf("Usage: addcr filename\n");
        exit(2);
   }

   for (i=1; i < argc; i++) {

	fprintf(stdout, "%s\n", argv[i]);
	if (( infd =  open(argv[i], O_RDONLY | O_BINARY )) < 0) {
	    printf("Cannot open input file %s\n",argv[i]);
	    exit(2);
	}


	if ((outfd = open(tname, O_RDWR |O_BINARY| O_CREAT |O_EXCL,
			S_IREAD | S_IWRITE) ) < 0) {
	    printf("Cannot open tmp file %s\n",tname);
	    exit(2);
	}

	while ( (cnt = read(infd, buf, BLOCKSIZE)) > 0) {
	    wrtcvt(outfd, buf, cnt);
	    if ( cnt < BLOCKSIZE )
		break;
	}

	close(infd);
	close(outfd);	
	if (unlink(argv[i]) < 0) {
	    printf("Can not remove original file %s\n",argv[i]);
	    exit(2);
	}
	
	if ( rename(tname, argv[i]) < 0) {
	    printf("Can not rename file %s tp %s\n",tname, argv[i]);
	    exit(2);
	}
    }
}

/* Write out buffer adding DOS eol , ie replacing 0x0a bu 0x0d0a */
wrtcvt(fd, buf, cnt) 
int fd;
char *buf;
unsigned cnt;
{
char *pt1, *pt2;
unsigned i;
static char eol[2] = {0x0d, 0x0a};

    pt1 = pt2 = buf;
    while (cnt) {
	i = 0;

	/* find Minix eol, if any */
	while ((*pt2 != 0x0a) && (i < cnt) ) { 
	    pt2++;
	    i++;
	}

	if ( *pt2 == 0x0a  ) {
	    /* add Dos eol, after writing chars checked so far */
	    if (i)
		mwrite(fd, pt1, i);
	    mwrite(fd, eol, 2);
	    pt2++;
	    pt1 = pt2;
	    cnt -= i + 1;   /* Account for eol char */
	}
	else {
	    /* just write out chars checked */
	    /* Must be at end of buffer */
	    mwrite(fd, pt1, i);
	    break;
	}
    }
}


/* Write out a buffer, error checking the return value */
mwrite(fd, ptr, cnt)
int fd;
char *ptr;
int cnt;
{
    if ( (write(fd, ptr, cnt)) != cnt) {
	printf("Error in writing to file\n");
	exit(2);
    }
}
