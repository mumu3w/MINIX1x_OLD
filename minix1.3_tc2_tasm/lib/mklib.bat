Rem     Batch file  to make minix.lib  using tlib
Rem
Rem     Minix.lib just consists of the compiled version of the .c files
Rem
Rem     The files were compiled with mcc -c.  All the options are built
Rem     into mcc.  The command mcc is used for all the compiling done
Rem     for minix
Rem
tcc -c -k- -f- -G -mt -Di8088 -I\usr\minix1.3\include *.c
tlib minix /C /E +abort +abs +access +alarm +atoi
tlib minix /C /E +atol  +bcmp +bcopy +brk +brk2
tlib minix /C /E +bsearch +bzero +call +chdir +chmod
tlib minix /C /E +chown +chroot	+cleanup +close +creat
tlib minix /C /E +crypt +ctermid +ctime +ctype +cuserid
tlib minix /C /E +doprintf +dup +dup2 +exec +execlp
tlib minix /C /E +exit +fclose +fdopen +fflush +ffs
tlib minix /C /E +fgetc +fgets +fopen +fork +fprintf
tlib minix /C /E +fputc +fputs +fread +freopen +fseek
tlib minix /C /E +fstat +ftell +fwrite +getcwd
tlib minix /C /E +getegid +getenv +geteuid +getgid +getgrent
tlib minix /C /E +getlogin +getpass +getpid +getpwent +gets
tlib minix /C /E +getuid +gtty +index +ioctl +isatty
tlib minix /C /E +itoa +kill +link +lsearch +lseek
tlib minix /C /E +malloc +memccpy +memchr +memcmp +memcpy
tlib minix /C /E +memset +message +mknod +mktemp +mount
tlib minix /C /E +open +pause +perror +pipe +popen
tlib minix /C /E +printdat +printk +prints +puts +qsort
tlib minix /C /E +rand +read +regexp +regsub +rindex
tlib minix /C /E +scanf +setbuf +setgid +setuid +signal
tlib minix /C /E +sleep +sprintf +stat +stb +stderr
tlib minix /C /E +stime +strcat +strchr +strcmp +strcpy
tlib minix /C /E +strcspn +strlen +strncat +strncmp +strncpy
tlib minix /C /E +strpbrk +strrchr +strspn +strstr +strtok
tlib minix /C /E +stty +swab +sync +syslib +system
tlib minix /C /E +termcap +time +times +ttyname +umask
tlib minix /C /E +umount +ungetc +unlink +utime +wait +write

