#
# Makefile for Minix Library
# Microsoft C 4.0 Compiler
# 1987 Eric Roskos
#

OBJ1 = \
abort.obj abs.obj access.obj alarm.obj atoi.obj atol.obj bcopy.obj \
brk.obj brk2.obj brksize.obj call.obj catchsig.obj chdir.obj chmod.obj \
chown.obj chroot.obj cleanup.obj close.obj creat.obj crtso.obj \
csv.obj ctype.obj doprintf.obj dup.obj dup2.obj end.obj exec.obj 

OBJ2 = \
exit.obj fclose.obj fflush.obj fgets.obj fopen.obj fork.obj fprintf.obj \
fputs.obj fread.obj freopen.obj fseek.obj fstat.obj ftell.obj fwrite.obj \
getc.obj getegid.obj getenv.obj geteuid.obj getgid.obj getgrent.obj  \
getpass.obj getpid.obj gets.obj getuid.obj getutil.obj 

OBJ3 = \
index.obj ioctl.obj isatty.obj itoa.obj kill.obj \
link.obj lseek.obj malloc.obj message.obj mknod.obj \
mktemp.obj mount.obj open.obj pause.obj perror.obj pipe.obj printdat.obj \
printk.obj prints.obj putc.obj rand.obj read.obj 

OBJ4 = \
regexp.obj regsub.obj rindex.obj scanf.obj sendrec.obj setbuf.obj \
setgid.obj setjmp.obj setuid.obj signal.obj sleep.obj sprintf.obj \
stat.obj stb.obj stderr.obj stime.obj strcat.obj strcmp.obj \
strcpy.obj strlen.obj strncat.obj strncmp.obj strncpy.obj sync.obj 

OBJ5 = \
syslib.obj time.obj times.obj umask.obj umount.obj ungetc.obj \
unlink.obj utime.obj wait.obj write.obj 

# OBJ6 should be head.obj for building the OS, crtso.obj for building
# Minix executables

OBJ6 = head.obj

OBJ7 = crypt.obj getpwent.obj 

.c.obj:
	cl -AS -Od -Di8088 -X -I..\include -Osal -Gs -c $*.c

.asm.obj:
	ml -Zm -c $*.asm

abort.obj: abort.c

abs.obj: abs.c

access.obj: access.c

alarm.obj: alarm.c

atoi.obj: atoi.c

atol.obj: atol.c

bcopy.obj: bcopy.c

brk.obj: brk.c

brk2.obj: brk2.c

brksize.obj: brksize.asm

call.obj: call.c

catchsig.obj: catchsig.asm

chdir.obj: chdir.c

chmod.obj: chmod.c

chown.obj: chown.c

chroot.obj: chroot.c

cleanup.obj: cleanup.c

close.obj: close.c

creat.obj: creat.c

crtso.obj: crtso.asm

crypt.obj: crypt.c

csv.obj: csv.asm

ctype.obj: ctype.c

doprintf.obj: doprintf.c

dup.obj: dup.c

dup2.obj: dup2.c

end.obj: end.asm

exec.obj: exec.c

exit.obj: exit.c

fclose.obj: fclose.c

fflush.obj: fflush.c

fgets.obj: fgets.c

fopen.obj: fopen.c

fork.obj: fork.c

fprintf.obj: fprintf.c

fputs.obj: fputs.c

fread.obj: fread.c

freopen.obj: freopen.c

fseek.obj: fseek.c

fstat.obj: fstat.c

ftell.obj: ftell.c

fwrite.obj: fwrite.c

getc.obj: getc.c

getegid.obj: getegid.c

getenv.obj: getenv.c

geteuid.obj: geteuid.c

getgid.obj: getgid.c

getgrent.obj: getgrent.c

getpass.obj: getpass.c

getpid.obj: getpid.c

getpwent.obj: getpwent.c

gets.obj: gets.c

getuid.obj: getuid.c

getutil.obj: getutil.asm

head.obj: head.asm

index.obj: index.c

ioctl.obj: ioctl.c

isatty.obj: isatty.c

itoa.obj: itoa.c

kill.obj: kill.c

link.obj: link.c

lseek.obj: lseek.c

malloc.obj: malloc.c

message.obj: message.c

mknod.obj: mknod.c

mktemp.obj: mktemp.c

mount.obj: mount.c

open.obj: open.c

pause.obj: pause.c

perror.obj: perror.c

pipe.obj: pipe.c

printdat.obj: printdat.c

printk.obj: printk.c

prints.obj: prints.c

putc.obj: putc.c

rand.obj: rand.c

read.obj: read.c

regexp.obj: regexp.c

regsub.obj: regsub.c

rindex.obj: rindex.c

scanf.obj: scanf.c

sendrec.obj: sendrec.asm

setbuf.obj: setbuf.c

setgid.obj: setgid.c

setjmp.obj: setjmp.asm

setuid.obj: setuid.c

signal.obj: signal.c

sleep.obj: sleep.c

sprintf.obj: sprintf.c

stat.obj: stat.c

stb.obj: stb.c

stderr.obj: stderr.c

stime.obj: stime.c

strcat.obj: strcat.c

strcmp.obj: strcmp.c

strcpy.obj: strcpy.c

strlen.obj: strlen.c

strncat.obj: strncat.c

strncmp.obj: strncmp.c

strncpy.obj: strncpy.c

sync.obj: sync.c

syslib.obj: syslib.c

time.obj: time.c

times.obj: times.c

umask.obj: umask.c

umount.obj: umount.c

ungetc.obj: ungetc.c

unlink.obj: unlink.c

utime.obj: utime.c

wait.obj: wait.c

write.obj: write.c

minix.lib: $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6)
	del minix.lib
	lib @liblist
