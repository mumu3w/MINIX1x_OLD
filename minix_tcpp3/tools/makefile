#
# Makefile for tools directory. (C) 1987 Eelco van Asperen.
#
TCINCLUDE=c:\tcpp\include
INCLUDE=..\include
CC=tcc
CCOPTS=-Di8088 -DTURBO -DSTANDALONE
#
# these are to make mkfs under DOS:
#
MKFSOPTS=-Di8088 -DTURBO -DSTANDALONE -DDOS
TCLIB=c:\tcpp\lib
LN=tlink
LNOPTS=/x/d/c
FORMATTER=c:\etc\format

.c.obj:
	$(CC) -A -mt -I$(INCLUDE) $(CCOPTS) -c $<

.asm.obj:
	masm /mx $<,,nul,nul

#.obj.exe:
#	$(LN) $(LNOPTS) ..\mlib\head $<,$*,$*,..\mlib\minix

.exe.out:
	dos2out -d $*

#
# create a boot-disk;
#
minix:	bootblok.bin ..\kernel\kernel.out ..\mm\mm.out ..\fs\fs.out init.out fsck.out build.exe
	c:\bin\build bootblok.bin ..\kernel\kernel.out ..\mm\mm.out ..\fs\fs.out init.out fsck.out image

#
# idem, but format the disk first;
#
# (the @*#*&!!?@#$!% DOS-formatter doesn't return a proper error-code !!)
#
image:	bootblok.bin ..\kernel\kernel.out ..\mm\mm.out ..\fs\fs.out init.out fsck.out build.exe
	- $(FORMATTER) a:
	build bootblok.bin ..\kernel\kernel.out ..\mm\mm.out ..\fs\fs.out init.out fsck.out a:

#
# create the bootblok;
#
bootblok.bin:	bootblok.asm
	masm /mx bootblok,,nul,nul,
	link bootblok,,nul,,
	exe2bin -q bootblok.exe bootblok.bin
#	del bootblok.exe
#	del bootblok.obj

#
# make 'fsck'; does not use crtso.obj nor head.obj.
#
fsck.out:	fsck1.obj fsck.obj
	$(LN) $(LNOPTS) fsck1 fsck,fsck,fsck,..\mlib\minix
	dos2out -d fsck.exe

#
# make 'init'; uses special startup module head.obj.
#
init.out:	init.obj
	$(LN) $(LNOPTS) ..\mlib\head init,init,init,..\mlib\minix
	dos2out -d init.exe

#
# build 'build' for use under MS-DOS;
#
build.exe:	build.obj
	$(LN) $(LNOPTS) $(TCLIB)\c0s build,build,build,$(TCLIB)\cs

build.obj:	build.c
	$(CC) -ms -I$(TCINCLUDE) -Di8088 -DTURBO -DSTANDALONE -DDOS -c build.c
	
#
# build 'dos2out' for use under MS-DOS;
#
dos2out.exe:	dos2out.obj
	$(LN) $(LNOPTS) $(TCLIB)\c0s dos2out,dos2out,dos2out,$(TCLIB)\cs

dos2out.obj:	dos2out.c
	$(CC) -ms -I$(TCINCLUDE) -Di8088 -DTURBO -DSTANDALONE -DDOS -c dos2out.c

#
# make for version of 'mkfs' for use under MS-DOS;
# (a complicated 'minixwrite')
#
dos_mkfs.exe:	..\commands\mkfs.c
	$(CC) $(MKFSOPTS) ..\commands\mkfs.c
	$(LN) $(LNOPTS) $(TCLIB)\c0s mkfs,mkfs,mkfs,$(TCLIB)\cs
	
dos_chmem.exe:	..\commands\chmem.c
	$(CC) $(MKFSOPTS) ..\commands\chmem.c
	$(LN) $(LNOPTS) $(TCLIB)\c0s chmem,chmem,chmem,$(TCLIB)\cs

