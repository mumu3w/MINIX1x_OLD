CC	= cl -nologo -AS -Od -Di8088 -X -I..\include -Osal -Gs -c
AS	= ml -nologo -Zm -c
AR	= lib -nologo
RM	= del
LD	= link /nologo /noe /m /nod 
D2A	= dos2out
E2B	= exe2bin -q

Image : bootblok.bin fsck.out init.out
	build bootblok.bin \
			..\kernel\kernel.out \
			..\mm\mm.out \
			..\fs\fs.out \
			init.out \
			fsck.out \
			Image

build.exe : build.c
	cl -nologo -AS build.c

dos2out.exe : dos2out.c
	cl -nologo -AS dos2out.c

bootblok.obj : bootblok.asm
	-$(AS) bootblok.asm

bootblok.exe : bootblok.obj
	-$(LD) bootblok;

bootblok.bin : bootblok.exe
	-$(E2B) bootblok.exe bootblok.bin

fsck.obj : fsck.c
	-$(CC) fsck.c

fsck1.obj : fsck1.asm
	-$(AS) fsck1.asm

fsck.exe : fsck.obj fsck1.obj
	-$(LD) fsck1+fsck,fsck,,..\slib\minix; >fsck.lst

fsck.out : fsck.exe
	-$(D2A) fsck.exe
	
init.obj : init.c
	-$(CC) init.c

init.exe : init.obj
	-$(LD) ..\slib\head+init,init,,..\slib\minix; >init.lst

init.out : init.exe
	-$(D2A) init.exe
	
clean :
	-$(RM) *.obj *.lst *.exe *.out *.map *.bin Image
	