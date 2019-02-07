/*
 *  Program name: mcc.c
 *  Author	: Deborah Mullen
 *  Date	: March 5, 1989
 *
 *  This program is a front end preprocessor for tcc.c.  It sets up
 *  default options for compiling and linking a C source file to be
 *  used under Minix.  It calls tcc, tlink and then dos2out.  The
 *  .exe and .obj files are removed. The final minix executable has
 *  the name of the input program. A new option -o is added so a 
 *  directory name can be specified to put the final executables in.
 *
 *  This program compiled with Turbo C (small) and linked with wildargs.obj
 *
 */

#include <stdio.h>
#include <fcntl.h>
#include <process.h>

#define MAXARGS 30		/* Max # of arguments to child progs */

/* argv to pass to tcc */
char *argsc[MAXARGS+1];

/* argv to pass to tlink */
char *argsl[MAXARGS+1];

/* argv to pass to dos2out */
char *argsd[5];

/* Default arguments */
char *cprog = "tcc.exe";
char *tprog = "tlink.exe";
char *dprog = "dos2out.exe";

/* Default options for compiling */
#define DOPTSCNR     8			/* # of default C options */
#define INCOPTIX     7			/* Include option number */

char *defcopts[] = { 	"tcc.exe",
			"-c",
			"-mt",
			"-k-",		/* no standard stack frame */
			"-f-",		/* no floating point */
			"-Di8088",	/* this symbol */
			"-G",		/* optimize for speed */
			"-ID:\\minix1.3\\include"
		   };

/* options for linkng */
char *lopt1 = "/n"; 		/* No default libraries */
char *lopt2 = "/c";		/* lower case significant */
char *lopt3 = "/x";		/* no map file */
char *lopt4 = "/m";		/* map with publics */
char *lopt5 = "D:\\minix1.3\\lib\\crtsot.obj";
char *lopt6 = "D:\\minix1.3\\lib\\crtsos.obj";
char *lopt7 = "D:\\minix1.3\\lib\\tmodel.lib";
char *lopt8 = "D:\\minix1.3\\lib\\smodel.lib";
char *lopt9 = "D:\\minix1.3\\lib\\minix.lib";


int sepflag;	/* Set if -ms on command line, indicates sep I&D */
int mapflag;	/* Set if want link map */
int nolink;

char oldname[50];
char newname[50];
char linkbuf[200];

char *outdir;   /* output directory for final minix executable */


/* This routine sets up the argv for the linker program */
char **dolinkargs(sep, map, fname)
int sep, map;
char *fname;
{

   /* Had to do the linkers command line special */
    sprintf(linkbuf,"%s, %s, %s, %s %s",fname, fname, fname,
		(sep) ? lopt8 : lopt7, lopt9);

    argsl[0] = tprog;
    argsl[1] = lopt1;  		/* /n -- no default libraries */
    argsl[2] = lopt2;	        /* /c -- lower case significant */
    argsl[3] = (map) ? lopt4 : lopt3;
    argsl[4] = (sep) ? lopt6 : lopt5;
    argsl[5] = linkbuf;
    argsl[6] = (char *) 0;
    return (argsl);

}


main(argc, argv)
int argc;
char *argv[];
{

int cindex, lindex, mindex;	/* used to index  argsc, argsl, argv */
char c;
int childret;
int i;
char bname[50];
char *ptr, *ptr2;

    if (argc < 2 ) {
	printf("Usage: mcc [options] filename\n");
	exit(2);
    }

    /* fill in the the args with the default options */
    for ( i = 0; i < DOPTSCNR; i++ )
	argsc[i] = defcopts[i];
    cindex = DOPTSCNR;

    /* fill in other defaults */
    nolink = sepflag = mapflag = 0;
    outdir = (char *) 0;
    mindex = 1;

    /* Now process any other options on command line */
    while (argv[mindex]) {
	if (argv[mindex][0] != '-')
	    break;    		/* all done with options */

	c = argv[mindex][1];

	/* First check for options that not passing to tcc */
	switch(c) {
	    case 'o': 	/* output directory */ 
	    		outdir = &(argv[mindex][2]);
			break;
	    case 'm':   /* model */
		        if (argv[mindex][2] == 's')
			    sepflag = 1;
			else {
			    if (argv[mindex][2] != 't') {
				printf("Illegal model \n");
				exit(2);
			    }
			}
			break;
	    case 'M':   /* link map */
			mapflag = 1;
			break;

	    case 'c':   /* compile only */
			nolink = 1;
			break;
	    default:   /* pass option on to tcc */
		        argsc[cindex++] = argv[mindex];
			break;
   	}
	mindex++;
    }
	
    /* Now for each file name listed, compile it, linkit, convert it */
    argsc[cindex+1] = (char *) 0;

    while (argv[mindex]) {

	/* Get the basename of thee argument */
	ptr = bname; 
	ptr2 = argv[mindex];
	while ((*ptr2 != '.') && *ptr2)
	   *ptr++ = *ptr2++;
        *ptr = '\0';

	/* compile it */
	argsc[cindex] = bname;
	childret = spawnvp(P_WAIT, cprog, argsc);
        if (childret < 0) {
	    printf("Could not create child tcc\n");
	    exit(2);
	}
	if (childret != 0) {
	    printf("Error in compiling %s\n",bname);
	    mindex++;
	    continue;
	}
	if (nolink ) {
	    mindex++;
	    continue;
	}
	
	/* linkit */
	childret = spawnvp(P_WAIT, tprog, dolinkargs(sepflag, mapflag, bname));
	if (childret < 0) {
	    printf("Could not creat child tlink\n");
	    exit(2);
	}
	if (childret != 0) {
	    printf("Error in linking %s\n",bname);
	    mindex++;
	    continue;
	}

	/* Now convert it */
	childret = (sepflag) ? spawnlp(P_WAIT, dprog, dprog, "-id", bname, NULL) :
			       spawnlp(P_WAIT, dprog, dprog, "-d", bname, NULL);
	if (childret < 0) {
	    printf("Could not create child dos2out\n");
	    exit(2);
	}
	if (childret != 0) {
		    printf("Error in converting  %s \n",bname);
	    mindex++;
	    continue;
	}

	/* Now move final name */
	if (outdir)
	   sprintf(newname,"%s\\%s",outdir,bname);
	else
	   strcpy(newname,bname);

	sprintf(oldname,"%s.%s",bname, (sepflag) ? "sep" : "out");

	/* Check if file exists, if so remove it */
	if (open(newname, O_RDWR) > 0)
	    unlink(newname);

	if (rename(oldname,newname) < 0) {
	   printf("Could not rename the dos2out file\n");
           mindex++;
	   continue;
	}

	/* unlink .obj files */
	sprintf(oldname,"%s.obj", bname);
	unlink(oldname);

	mindex++;
    }

}
