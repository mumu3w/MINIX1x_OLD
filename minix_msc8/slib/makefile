
CC	= cl -nologo -AS -Od -Di8088 -X -I..\include -Osal -Gs -c
AS	= ml -nologo -Zm -c
AR	= lib /nologo
RM	= del

minix.lib :
	-$(CC) ..\lib\*.c
	-$(AS) ..\lib\*.asm
	-$(AR) %LIB%\slibc7.lib *anldiv *anlmul *anlrem *anlshl *anlshr *annalshl *aNNalrem *aNNalmul,
	-$(RM) minix.lib
	-$(AR) @liblist

clean :
	-$(RM) *.obj *.lib *.lst
	