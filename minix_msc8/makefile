

all : slib_dir kernel_dir fs_dir mm_dir tools_dir

slib_dir :
	cd slib
	nmake /nologo
	cd ..
	
kernel_dir :
	cd kernel
	nmake /nologo
	cd ..
	
fs_dir :
	cd fs
	nmake /nologo
	cd ..
	
mm_dir :
	cd mm
	nmake /nologo
	cd ..
	
tools_dir :
	cd tools
	nmake /nologo
	cd ..
	
clean :
	cd slib
	nmake /nologo clean
	cd ..\kernel
	nmake /nologo clean
	cd ..\fs
	nmake /nologo clean
	cd ..\mm
	nmake /nologo clean
	cd ..\tools
	nmake /nologo clean
	cd ..
	