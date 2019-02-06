CC=tcc
CCOPTS=-A -mt -I..\include -Di8088 -DTURBO -G -O -c
LN=tlink /x/n/c/d		#replace /x by /s to generate a map-file
MXLN=$(LN) ..\mlib\crtso

.c.obj:
	$(CC) $(CCOPTS) $<

.obj.out:
	$(MXLN) $*,$*,$*,..\mlib\minix
	..\tools\dos2out -dp $*

.c.out:
	$(CC) $(CCOPTS) $<
	$(MXLN) $*,$*,$*,..\mlib\minix
	..\tools\dos2out -dp $*

#
# readfs is a special case; it's wants a different path for it's include files.
#
readfs.out:	readfs.obj

readfs.obj:	readfs.c
	$(CC) -I.. $(CCOPTS) readfs.c

#
#  All Commands;
#
all:	AR.out BASENAME.out cal.out CAT.out CC.out CHMEM.out \
	CHMOD.out CHOWN.out CLR.out CMP.out COMM.out \
	CP.out cpdir.out DATE.out DD.out DF.out diff.out DOSREAD.out \
	ECHO.out find.out fix.out fdisk.out GETLF.out GREP.out GRES.out \
	HEAD.out KILL.out LIBPACK.out LIBUPACK.out LN.out login.out LPR.out \
	LS.out MAKE.out MKDIR.out \
	MKFS.out MKNOD.out more.out MOUNT.out MV.out OD.out  \
	PASSWD.out PR.out printenv.out PWD.out readfs.out \
	REV.out RM.out RMDIR.out ROFF.out \
	SHAR.out SIZE.out SLEEP.out \
	SORT.out SPLIT.out STTY.out SU.out SUM.out \
	SYNC.out TAIL.out TAR.out TEE.out test.out TIME.out \
	TOUCH.out TR.out UMOUNT.out UNIQ.out UPDATE.out \
	WC.out sh.out changemem

#
#  MINED
#
mined.out:	mined\mined1.obj mined\mined2.obj
	$(MXLN) mined\mined1 mined\mined2,mined,mined,..\mlib\minix
	..\tools\dos2out -dp mined

mined\mined1.obj mined\mined2.obj:	mined\mined.h
	$(CC) $(CCOPTS) -nmined $*

#
#  SH
#
sh.out:	sh\sh1.obj sh\sh2.obj sh\sh3.obj sh\sh4.obj sh\sh5.obj sh\sh6.obj
	$(LN) ..\mlib\crtso sh\sh1 sh\sh2 sh\sh3 sh\sh4 sh\sh5 sh\sh6,sh,sh,..\mlib\minix
	..\tools\dos2out -dp sh

#
# sh uses setjmp() so we can't use register var's with Turbo C;
# the -r- option forces Turbo to ignore any register declarations
#
sh\sh1.obj:	sh\sh1.c
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh1.c
sh\sh2.obj:	sh\sh2.c        
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh2.c
sh\sh3.obj:	sh\sh3.c          
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh3.c
sh\sh4.obj:	sh\sh4.c           
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh4.c
sh\sh5.obj:	sh\sh5.c          
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh5.c
sh\sh6.obj:	sh\sh6.c          
	$(CC) $(CCOPTS) -I.\sh -r- -nsh sh\sh6.c

changemem:	cc.out cpdir.out dd.out diff.out df.out dosread.out find.out \
	fix.out grep.out gres.out libpack.out libupack.out make.out \
	mkdir.out mkfs.out mv.out pr.out readfs.out rm.out rmdir.out \
	sh.out sort.out tail.out time.out
	..\tools\chmem =2000  cc.out
	..\tools\chmem =32000 cpdir.out
	..\tools\chmem =40000 dd.out
	..\tools\chmem =50000 diff.out
	..\tools\chmem =8000  df.out
	..\tools\chmem =32000 dosread.out
	..\tools\chmem =50000 find.out
	..\tools\chmem =50000 fix.out
	..\tools\chmem =8000  grep.out
	..\tools\chmem =8000  gres.out
	..\tools\chmem =64000 libpack.out
	..\tools\chmem =64000 libupack.out
	..\tools\chmem =20000 make.out
#	..\tools\chmem =64000 mined.out
	..\tools\chmem =8000  mkdir.out
	..\tools\chmem =16000 mkfs.out
	..\tools\chmem =8000  mv.out
	..\tools\chmem =8000  pr.out
	..\tools\chmem =50000 readfs.out
	..\tools\chmem =8000  rm.out
	..\tools\chmem =8000  rmdir.out
	..\tools\chmem =8000  sh.out
	..\tools\chmem =30000 sort.out
	..\tools\chmem =2000  tail.out
	..\tools\chmem =2000  time.out

clean:
	- del *.obj
	- del *.map
	- del *.lst

fast:	*.c
	$(CC) $(CCOPTS) *.c

