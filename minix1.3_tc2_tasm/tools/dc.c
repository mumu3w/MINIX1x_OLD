/* Routine to strip exe header from a file and write back the rest
 *
 * Used for bootblok, since not have dd command in DOS
 *   Usage: dc infile outfile bytes to skip at start of file
 *
 * Deborah Mullen  2/14/89
 */


#include <stdio.h>
#include <stdlib.h>

main(argc, argv)
int argc;
char *argv[];
{
    FILE *inf, *ouf;
    char *buf;
    int wrcnt, incnt;
    int ct, readnum;


    if (argc != 4) {
        printf("usage: dcinfile outfile count\n");
        exit(2);
    }

    ct = atoi(argv[3]);

    buf = (char *)calloc(512 + 1, 1);


    if (!buf) {
        printf("can not get memory\n");
        exit(2);
    }

    inf = fopen(argv[1], "rb");

    if (inf < 0) {
        printf(" Could not open %s file\n", argv[1]);
        exit(2);
    }

    ouf = fopen(argv[2], "wb+");

    if (ouf < 0) {
        printf(" Could not open %s file\n", argv[2]);
        exit(2);
    }

    while (ct) {
        /* read part to skip */
        incnt = fread(buf, 1, 512, inf);
        if (incnt > ct) {
            fwrite(&buf[ct], 1, incnt - ct, ouf);
            break;
        }
        ct -= incnt;
    }

    /* write out the rest of the file */
    /*while ((incnt = read(inf, buf, 512)) > 0)
        write(ouf, buf, incnt);*/
    while ((incnt = fread(buf, 1, 512, inf)) > 0)
        fwrite(buf, 1, incnt, ouf);

    fclose(inf);
    fclose(ouf);
}



