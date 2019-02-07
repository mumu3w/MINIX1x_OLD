/* ar - archiver		Author: Michiel Huisjes */
/* V7 upgrade			Author:	Monty Walls */

/* Modified 3/3/89 by Deborah Mullen.
 * Now called arm.c ( so as not to get confused with the real thing
 *
 * This version is a quick mod of the current ar.c to
 * produce a version that will word under DOS. The idea
 * in mind, was to build an archieve of files under MINIX and
 * then transfer this archieve via doswrite to DOS, then unpack
 * it with this version.  Also wanted to be able to build archieve
 * under DOS and unpack it under MINIX.  
 *
 * The fact that DOS has newline as crlf, and MINIX has newline as lf
 * in text files had to be taken into account.  This version adds a
 * new option 'a' for ascii.  When given, it assumes that input must
 * be modified to strip the extra cr from the end of lines, and for
 * output, a CR must be added.  Since this is only used to pack files
 * together, the  position options have been eliminated.
 *
 * Usage: ar 'key' archive [file] ...
 *
 *	where 'key' is one of: qtpx
 *
 *	  q: quickly append to the end of the archive file
 *	  t: print contents of archive
 *	  p: print named files
 *	  x: extract
 *
 *	concatencated with one or more of: vuac
 *  
 *	  v: verbose
 *	  a: assume ascii input/output
 *	  c: create  (suppresses creation message)
 *        n: convert to dosnames, if more than one period in name
 *           used when extracting, convert input names to all lowercase
 *           when adding
 */ 

/* include files */
#include <stdio.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <io.h>

extern char *strchr();
extern char *strlwr();
long dostounix();

/* ar.c header file V7 */

#define ARMAG	0177545l
struct ar_hdr {
	char ar_name[14];
	long ar_date;		/* not Intel format */
	char ar_uid;
	char ar_gid;
	int ar_mode;
	long ar_size;		/* not Intel format */
};

/* macro functions */
#define odd(nr)		(nr & 1)
#define even(nr)	(odd(nr) ? nr + 1 : nr)
#define tell(f)	(lseek(f, 0l, 1))

/* option switches */
/* major options */
#define EXTRACT		0x01
#define PRINT		0x04
#define TABLE		0x08
#define APPEND		0x20

/* minor options */
#define ASCII		0x02
#define VERBOSE		0x01
#define CREATE		0x01

/* mode bits maps */
#define EXEC_OWNER	00100
#define EXEC_GROUP	00010
#define EXEC_ALL	00001
#define READ_OWNER	00400
#define READ_GROUP	00040
#define READ_ALL	00004
#define WRITE_OWNER	00200
#define WRITE_GROUP	00020
#define WRITE_ALL	00002
#define SET_UID		04000
#define SET_GID		02000

#define DEFMODE         0x1ff

/* global defines */
#define BUFFERSIZE	4096
#define WRITE		2		/* both read & write */
#define READ		0
#define MAGICSIZE	sizeof(short)	/* size of magic number in file */

/* option switches */
char verbose = 0;
char create = 0;
char asciiflg = 0;
char major = 0;
char dosnames = 0;

/* global variables */
char *tmp1;
char *tmp2;
char *progname;
char *afile;
char buffer[BUFFERSIZE];
long pos_offset = -1;

/* keep track of member moves using this struct */
struct mov_list {
	long pos;
	struct mov_list *next;
} *moves = NULL;

/* forward declarations and external references */
extern char *malloc();
extern char *mktemp(), *rindex();
extern int strncmp();
extern print_date();
extern usage();
extern long lseek();
extern char *basename();

int
main(argc, argv)
int argc;
char **argv;
{
	int ac, opts_seen = 0, rc;
	char *av;
	
	progname = argv[0];		
	if (argc < 3) 
		usage();

	for (av = argv[1]; *av; ++av) {	
		switch (*av) {		/* major option */
			case 'q':
				major |= APPEND;
				++opts_seen;
				break;
			case 'x':
				major |= EXTRACT;
				++opts_seen;
				break;
			case 'p':
				major |= PRINT;
				++opts_seen;
				break;
			case 't':
				major |= TABLE;
				++opts_seen;
				break;
			case 'a':
				asciiflg |= ASCII;
				break;
			case 'v':
				verbose |= VERBOSE;
				break;
			case 'c':
				create |= CREATE;
				break;
			case 'n': dosnames = 1;
				  break;
			default:
				usage();
		}
	}

	if (opts_seen != 1) 
		usage();
	
			
	afile = argv[2];
	ac = 3;
	
	switch (major) {
		case EXTRACT:
		case TABLE:
		case PRINT:
			ar_common(ac, argc, argv);
			break;
		case APPEND:
			append_members(ac, argc, argv);
			break;
		default:
			usage();
	}

	for (rc = 0; ac < argc; ++ac) {
		if (*argv[ac] != '\0') {
			/* no processing done on this name */
			fprintf(stderr,"Error %s: %s not found in %s\n", progname, argv[ac], afile);
			rc = 1;
		}
	}
	fflush(stdout);
	exit(rc);
}

usage()
{
	fprintf(stderr,"Usage for Dos version: %s [qtpx][avcn] afile name ... \n",progname);
	exit(1);
}

mwrite(fd, address, bytes)
int fd;
register char *address;
register int bytes;
{
  if (write(fd, address, bytes) != bytes) {
	fprintf(stderr," Error: %s - Write error\n",progname);
	exit(1);
  }
}

long
swap(l)
long l;
{
	union {
		struct {
			int word1, word2;
		} words;
		long n;
	} u_in, u_out;
	
	u_in.n = l;
	u_out.words.word1 = u_in.words.word2;
	u_out.words.word2 = u_in.words.word1;
	return (u_out.n);
	
}

struct ar_hdr *
get_member(fd)
int fd;
{
	int ret;
	static struct ar_hdr member;
	char *dot;	

	if ((ret = read(fd,  &member, sizeof(struct ar_hdr))) <= 0) 
		return ((struct ar_hdr *)NULL);
	if (ret != sizeof(struct ar_hdr)) {
		fprintf(stderr,"Error: ar corrupted archive %s\n",afile);
		exit(1);
	}

	/* the archive long format is pdp11 not intel
	 * therefore we must reformat them for our internal use
	 */

	member.ar_date = swap(member.ar_date);
	member.ar_size = swap(member.ar_size);

	if (dosnames) {
	   /* Dos only allows 1 period in filename */
	   dot = strchr(member.ar_name, '.');
	   if (dot) {
		/* replace the other dots with x */
		dot++;   /* Skip first dot */
		while (dot = strchr(dot, '.')) 
		   *dot = 'x';
	   }
	}

	return (&member);
}

int
open_archive(filename, opt, to_create)
char *filename;
int opt;
{
	static unsigned short magic;
	int fd;
	int openmode;
	
	/* to_create can have values of 0,1,2 */
	/* 0 - don't create a file. */
	/* 1 - create file but use create switch message mode */
	/* 2 - create file but don't talk about it */
	
	if (to_create) {
 
		if ((fd = open(filename,O_CREAT|O_RDWR|O_TRUNC|O_BINARY, 
			S_IREAD|S_IWRITE  )) < 0) {
			fprintf(stderr, "Error: %s can not create %s\n",progname, filename);
			exit(1);
		}
		if (!create && to_create == 1) fprintf(stderr, "%s:%s created\n", progname, filename);
		magic = ARMAG;
		mwrite(fd, &magic, MAGICSIZE);
		return (fd);
	}
	else {
		openmode = (opt == READ) ? O_RDONLY : O_RDWR;
		if ((fd = open(filename, O_BINARY | openmode)) < 0) {
			if (opt == WRITE) 
				return (open_archive(filename, opt, 1));
			else {
				fprintf(stderr, "Error: %s can not open %s\n",progname, filename);
				exit(1);
			}
		}
		/* now check the magic number for ar V7 file */
		lseek(fd, 0l, 0);
		read(fd, &magic, MAGICSIZE);
		if (magic != ARMAG) {
			fprintf(stderr, "Error: not %s V7 format - %s\n",progname, filename);
			exit(1);
		}
		if (major & APPEND)
			lseek(fd, 0l, 2);	/* seek eof position */
			
		return (fd);
	}		
}


int
print_mode(mode)
short mode;
{
	char g_ex, o_ex, all_ex;
	char g_rd, o_rd, all_rd;
	char g_wr, o_wr, all_wr;
	
	g_ex = EXEC_GROUP & mode ? 'x' : '-';
	o_ex = EXEC_OWNER & mode ? 'x' : '-';
	all_ex = EXEC_ALL & mode ? 'x' : '-';
	
	g_ex = SET_GID & mode ? 's' : g_ex;
	o_ex = SET_UID & mode ? 's' : o_ex;
	
	g_rd = READ_GROUP & mode ? 'r' : '-';
	o_rd = READ_OWNER & mode ? 'r' : '-';
	all_rd = READ_ALL & mode ? 'r' : '-';
	
	g_wr = WRITE_GROUP & mode ? 'w' : '-';
	o_wr = WRITE_OWNER & mode ? 'w' : '-';
	all_wr = WRITE_ALL & mode ? 'w' : '-';

	fprintf(stdout,"%c%c%c",o_rd, o_wr, o_ex);
	fprintf(stdout,"%c%c%c",g_rd, g_wr, g_ex);
	fprintf(stdout,"%c%c%c",all_rd, all_wr, all_ex);
}

print_header(member)
struct ar_hdr *member;
{
	if (verbose) {
		print_mode(member->ar_mode);
		fprintf(stdout,"%3.3d",member->ar_uid);
		fprintf(stdout,"/%-3.3d ",member->ar_gid);
		fprintf(stdout,"%5ld",member->ar_size);	/* oops is long - mrw */
		print_date(member->ar_date);
	}
	fprintf(stdout,"%-14.14s\n",member->ar_name);
}

/* Write out member, stripping out the DOS eol 
 */
/* Returns # of chars dropped */

wrtstrip(fd, buf, cnt, gotod)
int fd;		/* file to write to */
char *buf;	/* what to write */
unsigned cnt;	/* how many to write */
char *gotod;    /* If last char in last buffer was 0x0d set to 0d */
{

char *ptr, *endptr, *src, *dest;
unsigned newcnt, delcnt= 0;

   /* First check if need to do special stuff from last buffer */
    if (*gotod) {
	if (*buf != 0x0a)
	   mwrite(fd, gotod, 1);  /* Not eol sequence, write it out now */
	else
	   delcnt++;    
	*gotod = '\0';
    }

    newcnt = cnt;   /* newcnt will be # of bytes to write */
    ptr = buf;
    endptr = buf + newcnt;

    while (cnt)  {   /* check each char */
	if (*ptr == 0x0d) {

	    /* make sure not last char in buffer */
	    if ( ptr + 1 != endptr) {
		
		/* Check if have true dos eol */
		if ( *(ptr + 1) == 0x0a) {

		    /* we do, so copy rest of chars in buffer over the 0d */
		    src = ptr + 1;
		    dest = ptr;
		    while (src != endptr)
			*dest++ = *src++;

		    /* Adjust counters and ptrs */
		    delcnt++;
		    newcnt--;
		    cnt--;    /* Checked two chars, so dec twice */
		    endptr = buf + newcnt;
		}
	    }
	    else {
		/* Last char in buffer is 0x0d */
		*gotod = 0x0d; 
		newcnt--;
		break;
	   }
	}
	else {
	    if (*ptr == 0x1a && (ptr+1 == endptr) ) {
	       /* Skip the end of file mark for Dos ascii files */
                
		delcnt++;
		newcnt--;
		break;
	    }
	}
	ptr++;
	cnt--;
    }

    /* Now write the buffer */
    if (newcnt)
	mwrite(fd, buf, newcnt);

    return(delcnt);
}
  

/* Write out buffer adding DOS eol */
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
		write(fd, pt1, i);
	    write(fd, eol, 2);
	    pt2++;
	    pt1 = pt2;
	    cnt -= i + 1;   /* Account for eol char */
	}
	else {
	    /* just write out chars checked */
	    /* Must be at end of buffer */
	    write(fd, pt1, i);
	    break;
	}
    }
}	

print(fd,member)
int fd;
struct ar_hdr *member;
{
int outfd;
long size;
register int cnt, ret;
int do_align;

    if (major & EXTRACT) {
        if ((outfd = open(member->ar_name,O_CREAT|O_RDWR|O_TRUNC|O_BINARY, 
	        S_IREAD|S_IWRITE  )) < 0) {
	    fprintf(stderr,"Error: %s could not creat %-14.14s\n",progname, member->ar_name);
	    exit(1);
	}
	if (verbose)
		fprintf(stdout,"x - %-14.14s\n",member->ar_name);
    }
    else {
    	if (verbose) {
    	    fprintf(stdout,"p - %-14.14s\n",member->ar_name);
    	    fflush(stdout);
	}
	outfd = fileno(stdout);
    }
    
    /* changed loop to use long size for correct extracts */	
    for (size = member->ar_size; size > 0; size -= ret) {
    	cnt = (size < BUFFERSIZE ? size : BUFFERSIZE);
    	ret = read(fd, buffer, cnt);
    	if (ret > 0) {
	    if (asciiflg)
		wrtcvt(outfd, buffer,ret);
	    else
                write(outfd,buffer, ret);
	}
    }
    if (odd(member->ar_size))
    	lseek(fd,1l,1);		/* realign ourselves */
		
    if (major & EXTRACT) {
    	close(outfd);
    	chmod(member->ar_name, member->ar_mode); 
    }		
}

/* copy a given member from fd1 to fd2 */
/* Special processing if asciiflg set */

copy_member(infd, outfd, member)
int infd, outfd;
struct ar_hdr *member;
{
int n, cnt;
long m, size, startpos, endpos;
char lastchar = '\0';
unsigned dropped = 0;     /* # of 0x0d chars from dos ascii input */

	/* save copies for our use */
	m = size = member->ar_size;

	/* format for disk usage */
	member->ar_size = swap(member->ar_size);
	member->ar_date = swap(member->ar_date);
      
        if (asciiflg)
	   startpos = tell(outfd);

	mwrite(outfd, member, sizeof(struct ar_hdr));
	for (; m > 0; m -= n) {
		cnt = (m < BUFFERSIZE ? m : BUFFERSIZE);
		if ((n = read(infd, buffer, cnt)) != cnt) {
			fprintf(stderr,"Error: %s - read error on %-14.14s\n",progname, member->ar_name);
			exit(1);
		}
		if (asciiflg)
		   dropped += wrtstrip(outfd, buffer, n, &lastchar);
		else
		   mwrite(outfd, buffer, n);
	}	

	/* adjust if last char is 0x0d */
	if (asciiflg) {
	    if (lastchar)
	       mwrite(outfd, lastchar, 1);

		/* adjust the count in header */
	
		endpos = tell(outfd);
		member->ar_size = swap(member->ar_size);
		member->ar_size -= dropped;
		size = member->ar_size;
		member->ar_size = swap(member->ar_size);
		lseek(outfd, startpos, 0);
		/* write out header again */
		mwrite(outfd, member,sizeof(struct ar_hdr));  
		lseek(outfd, endpos, 0);
	}

	if (odd(size)) {		/* pad to word boundary */
		mwrite(outfd, buffer, 1);
		lseek(infd,1l,1);		/* realign reading fd */ 
	}
}

/* insert at current offset - name file */
insert(fd, name, mess)
int fd;
char *name, *mess;
{
	static struct ar_hdr member;
	static struct stat status;
	static struct ftime ft;
	int in_fd;

	name = strlwr(name);    /* convert to lowercase */

	if (stat(name, &status) < 0) {
		fprintf(stderr,"Error: %s cannot find file %s\n",progname,name);
		exit(1);
	}
	else if ((in_fd = open(name, O_BINARY|O_RDONLY)) < 0) {
		fprintf(stderr,"Error: %s cannot open file %s\n",progname,name);
		exit(2);
	}
        getftime(in_fd, &ft);

	strncpy(member.ar_name, basename(name),14);
	member.ar_uid = status.st_uid;
	member.ar_gid = status.st_gid;
	member.ar_mode = DEFMODE;
	member.ar_date = dostounix(&ft);
	member.ar_size = status.st_size;
	
	copy_member(in_fd, fd, &member); 
	if (verbose) 
		fprintf(stdout, "%s - %-14.14s\n",mess, name);
	close(in_fd);
	return (1);
}

ar_common(ac, argc, argv)
int ac, argc;
char **argv;
{
	int a, fd, did_print;
	struct ar_hdr *member;

	fd = open_archive(afile, READ, 0);
	while ((member = get_member(fd)) != NULL) {
		did_print = 0;
		if (ac < argc) {
			for (a = ac+1; a <= argc; ++a) {
				if (strncmp(basename(argv[a-1]),member->ar_name,14) == 0) {
					if (major & TABLE)
						print_header(member);
					else if (major & (PRINT | EXTRACT)) {
						print(fd, member);
						did_print = 1;
					}
					*argv[a-1] = '\0';
					break;
				}
			}
		}
		else {
			if (major & TABLE)
				print_header(member);
			else if (major & (PRINT | EXTRACT)) {
				print(fd, member);
				did_print = 1;
			}
		}
		/* if we didn't print the member or didn't use it we will
		 * have to seek over its contents
		 */
		if (!did_print)
			lseek(fd, (long)even(member->ar_size), 1);
	}
}

append_members(ac, argc, argv)
int ac, argc;
char **argv;
{
	int a, fd;
	struct ar_hdr *member;
	
	/* quickly append members don't worry about dups in ar */
	fd = open_archive(afile, WRITE, 0);
	if (ac < argc) {
		if (odd(lseek(fd, 0l, 2)))
			mwrite(fd, buffer, 1);
		/* while not end of member list insert member at end */
		for (a = ac+1; a <= argc; ++a) {
			insert(fd, argv[a-1], "a");
			*argv[a-1] = '\0';
		}
	}
	close(fd);
}


char *basename(path)
char *path;
{
  register char *ptr = path;
  register char *last = (char *)NULL;

  while (*ptr != '\0') {
	if (*ptr == '/')
		last = ptr;
	ptr++;
  }
  if (last == (char *)NULL)
	return path;
  if (*(last + 1) == '\0') {
	*last = '\0';
	return basename(path);
  }
  return last + 1;
}


#define MINUTE	60L
#define HOUR	(60L * MINUTE)
#define DAY	(24L * HOUR)
#define YEAR	(365L * DAY)
#define LYEAR	(366L * DAY)

int mo[] = {
  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

char *moname[] = {
  " Jan ", " Feb ", " Mar ", " Apr ", " May ", " Jun ",
  " Jul ", " Aug ", " Sep ", " Oct ", " Nov ", " Dec "
};

/* Print the date.  This only works from 1970 to 2099. */
print_date(t)
long t;
{
  int i, year, day, month, hour, minute;
  long length, time(), original;

  year = 1970;
  original = t;
  while (t > 0) {
	length = (year % 4 == 0 ? LYEAR : YEAR);
	if (t < length)
		break;
	t -= length;
	year++;
  }

 /* Year has now been determined.  Now the rest. */
  day = (int) (t / DAY);
  t -= (long) day * DAY;
  hour = (int) (t / HOUR);
  t -= (long) hour * HOUR;
  minute = (int) (t / MINUTE);

 /* Determine the month and day of the month. */
  mo[1] = (year % 4 == 0 ? 29 : 28);
  month = 0;
  i = 0;
  while (day >= mo[i]) {
	month++;
	day -= mo[i];
	i++;
  }

  /* At this point, 'year', 'month', 'day', 'hour', 'minute'  ok */
  fprintf(stdout, "%s%2.2d ",moname[month],++day);
  if (time((long *)NULL) - original >= YEAR / 2L)
	fprintf(stdout,"%4.ld ",(long)year);
  else 
	fprintf(stdout,"%02.2d:%02.2d ",hour, minute);
}

/* Convrt time of a dos file to unix time */
long dostounix(ft)
struct ftime *ft;
{
int i,j,  year;
long time;

    time = 0L;
    year = 10 + (int) ft->ft_year;
    i = 0;
    while (i < year) {
	time += (i % 4 == 0) ? LYEAR : YEAR;
	i++;
    }

    mo[1] = (year % 4 == 0) ? 29 : 28;
    i = 0;
    j = (int) ft->ft_month;
    j--;

    while ( i < j) {
       time += ((long) mo[i]) * DAY;
	i++;
    }
    time += ((long) ft->ft_day - 1) * DAY;
    time += ( (long) ft->ft_hour) * HOUR;
    time += ( (long) ft->ft_min) * MINUTE;
    time += ((long) ft->ft_tsec) * 2L;
    return(time);
}

