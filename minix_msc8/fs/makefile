CC	= cl -nologo -AS -Od -Di8088 -X -I..\include -Osal -Gs -c
AS	= ml -nologo -Zm -c
AR	= lib -nologo
RM	= del
LD	= link /nologo /noe /m /nod 
D2A	= dos2out 


fs.out : fs.exe
	$(D2A) fs.exe

fs.exe :
	-$(CC) *.c
	-$(LD) ..\slib\head+ <linklist >fs.lst

clean :
	-$(RM) *.obj *.lst *.exe *.out *.map
