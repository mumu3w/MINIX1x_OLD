CC	= cl -nologo -AS -Od -Di8088 -X -I..\include -Osal -Gs -c
AS	= ml -nologo -Zm -c
AR	= lib -nologo
RM	= del
LD	= link /nologo /noe /m /nod 
D2A	= dos2out 


mm.out : mm.exe
	$(D2A) mm.exe

mm.exe :
	-$(CC) *.c
	-$(LD) ..\slib\head+ <linklist >mm.lst

clean :
	-$(RM) *.obj *.lst *.exe *.out *.map
