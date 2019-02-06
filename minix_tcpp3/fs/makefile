CC=tcc
CCOPTS=-A -mt -I..\include -Di8088 -DTURBO -G -O -c
LN=tlink
LNOPTS=/m/d/c           # no map file; warn for duplicates; case is significant

OBJS=	write.obj cache.obj device.obj filedes.obj \
	inode.obj main.obj link.obj misc.obj mount.obj open.obj \
	path.obj pipe.obj protect.obj putc.obj read.obj stadir.obj \
	super.obj table.obj time.obj utility.obj

.c.obj:
	$(CC) $(CCOPTS) $<

all:	fs.out

fs.out:	$(OBJS) ..\mlib\head.obj
	$(LN) $(LNOPTS) @linklist
	..\tools\dos2out -dp fs

