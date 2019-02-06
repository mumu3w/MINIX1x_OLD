CC=tcc
CCOPTS=-A -mt -I..\include -Di8088 -DTURBO -G -O -c
LN=tlink
LNOPTS=/m/d/c           # no map file; warn for duplicates; case is significant

.c.obj:
	$(CC) $(CCOPTS) $<

.asm.obj:
	masm /mx $<,,nul,nul

OBJS=	CLOCK.OBJ DMP.OBJ FLOPPY.OBJ MAIN.OBJ MEMORY.OBJ \
	PRINTER.OBJ PROC.OBJ SYSTEM.OBJ TABLE.OBJ TTY.OBJ WINI.OBJ mpx88.obj klib88.obj
	
all:	kernel.out

# linklist中asm编译后的obj文件要靠前
# 否则得到的out文件中bss的尺寸似乎不正确
kernel.out:	$(OBJS)
	$(LN) $(LNOPTS) @linklist
#	link @linklist
	..\tools\dos2out -dp kernel
	
mpx88.obj:	mpx88.asm

klib88.obj:	klib88.asm

