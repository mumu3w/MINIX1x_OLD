CC	= cl -nologo -AS -Od -Di8088 -X -I..\include -Osal -Gs -c
AS	= ml -nologo -Zm -c
AR	= lib -nologo
RM	= del
LD	= link /nologo /noe /m /nod 
D2A	= dos2out 


kernel.out : kernel.exe
	$(D2A) kernel.exe

kernel.exe :
	-$(CC) -DGENERIC_FDISK *.c
	-$(AS) -DGENERIC_FDISK *.asm
	-$(LD) mpx88+ <linklist >kernel.lst

clean :
	-$(RM) *.obj *.lst *.exe *.out *.map