/* Routine to strip exe header from a file and write back the rest
 *
 * Used for bootblok, since not have dd command in DOS
 *   Usage: dc infile outfile bytes to skip at start of file
 *
 * Deborah Mullen  2/14/89
 */


#include <fcntl.h>
#include <sys\stat.h>

main(argc, argv)
int argc;
char *argv[];
{
int inf, ouf;
char *buf;
int wrcnt, incnt;
int ct, readnum;


   if (argc != 4) {
	printf("usage: dcinfile outfile count\n");
	exit(2);
   }

   ct = atoi(argv[3]);

   buf = (char *) calloc(512 + 1,1);

  
   if (!buf) {
	printf("can not get memory\n");
	exit(2);
   }

   inf = open(argv[1],O_RDONLY | O_BINARY);

   if (inf < 0) {
	printf(" Could not open %s file\n",argv[1]);
	exit(2);
   }

   ouf = open(argv[2],O_RDWR | O_BINARY | O_CREAT, S_IREAD | S_IWRITE);

   if (ouf < 0) {
	printf(" Could not open %s file\n",argv[2]);
	exit(2);
   }

   while (ct) {
	/* read part to skip */
	incnt = read(inf, buf, 512 );
	if (incnt > ct ) {
	      write(ouf, &(buf[ct]), incnt - ct);
	      break;
	}
	ct -= incnt;
   }

   /* write out the rest of the file */
   while ( (incnt = read(inf, buf, 512)) > 0)
	write(ouf, buf, incnt);

   close(inf);
   close(ouf);
}

   

