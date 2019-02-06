CC=tcc
CCOPTS=-A -mt -I..\include -Di8088 -DTURBO -G -O -c
LN=tlink
LNOPTS=/m/d/c           # no map file; warn for duplicates; case is significant

OBJS=	exec.obj alloc.obj break.obj forkexit.obj \
	getset.obj main.obj putc.obj signal.obj table.obj utility.obj

.c.obj:
	$(CC) $(CCOPTS) $<

mm.out:	$(OBJS) ..\mlib\head.obj
	$(LN) $(LNOPTS) @linklist
	..\tools\dos2out -d mm

all:	mm.out

