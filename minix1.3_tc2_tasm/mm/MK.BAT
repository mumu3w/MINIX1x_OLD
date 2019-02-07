echo off
Rem  Batch file that makes mm, fs, or kernel of Minix OS
Rem  Usage:  mk module_name [sep]
Rem          The last paramenter, if not null, generates Separate I&D
if "%1" == "" goto err
echo    Making %1 module
Rem
Rem Compiler options used:
Rem 	-k- : Do not use standard stack frame
Rem     -f- : Code contains no floating point
Rem     -G  : Optimize for speed
Rem     -mt : Tiny model
Rem     -D  : Define symbol i8088
Rem     -I  : Directory to look for include files
Rem
Rem Assembly Options used:
Rem	/ml : Case sensitivity on all symbols
Rem     /D  : Define symbol ( used for separate I&D)
Rem
Rem Linker options:
Rem	/m : generate map file with publics
Rem     /n : no default libraries
Rem     /c : lower case significant in symbols
Rem    Names is a file listing the .objs to link
Rem
Rem    All C code is compiled with the same options
REM 
tcc -c -k- -f- -G -mt -Di8088 -ID:\MINIX1.3\include *.c
if errorlevel 1 goto err2
if not "%2" == "" goto sep
echo Making %1 module -- combined I&D
rem
rem    Check if in kernel directory 
rem
if not exist mpx88.asm goto nxt
rem
rem    For Kernel, first file is mpx88 not crtso
tasm /ml /iD:\MINIX1.3\include *.asm
if errorlevel 1 goto err2
tlink /ml /n /c @names, %1.exe, %1.map, D:\MINIX1.3\lib\tmodel D:\MINIX1.3\lib\minix
if errorlevel 1 goto err2
goto cnvt
:nxt
tlink /ml /n /c D:\MINIX1.3\lib\headt @names, %1.exe, %1.map,D:\MINIX1.3\lib\tmodel D:\MINIX1.3\lib\minix
if errorlevel 1 goto err2
:cnvt
Rem  Convert to Minix format
dos2out %1
goto done
:sep
echo Making %1 module -- Separate I&D
rem
rem    Check if in kernel directory 
rem
if not exist mpx88.asm goto nxt2
rem
rem    For Kernel, first file is mpx88 not crtso
tasm /ml /iD:\MINIX1.3\include /D_SID *.asm
if errorlevel 1 goto err2
tlink /ml /n /c @names, %1.exe, %1.map, D:\MINIX1.3\lib\smodel D:\MINIX1.3\lib\minix
if errorlevel 1 goto err2
goto cnvt2
:nxt2
tlink /ml /n /c D:\MINIX1.3\lib\heads @names, %1.exe, %1.map, D:\MINIX1.3\lib\smodel D:\MINIX1.3\lib\minix
if errorlevel 1 goto err2
:cnvt2
dos2out -i %1
goto done
:err
echo Error: Must give a module name, either mm, fs, or kernel
goto done
:err2
echo Got an error somewhere
:done
echo on
