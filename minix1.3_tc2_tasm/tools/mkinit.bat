echo off
Rem  Batch file that makes init module of Minix OS
Rem  Usage:  mkinit [sep]
Rem          The last paramenter, if not null, generates Separate I&D
echo    Making INIT module
Rem
Rem Compiler options used:
Rem 	-k- : Do not use standard stack frame
Rem     -f- : Code contains no floating point
Rem     -G  : Optimize for speed
Rem     -mt : Tiny model
Rem     -D  : Define symbol i8088
Rem     -I  : Directory to look for include files
Rem
Rem Linker options:
Rem	/m : generate map file with publics
Rem     /n : no default libraries
Rem     /c : lower case significant in symbols
Rem
Rem    All C code is compiled with the same options
tcc -c -k- -f- -G -mt -Di8088 -ID:\MINIX1.3\include init.c
if not "%1" == "" goto sep
echo Making INIT module -- combined I&D
tlink /ml /n /c D:\MINIX1.3\lib\headt init, init.exe, init.map, D:\MINIX1.3\lib\tmodel D:\MINIX1.3\lib\minix
Rem  Convert to Minix format
dos2out init
goto done
:sep
echo Making INIT module -- Separate I&D
rem
tlink /ml /n /c D:\MINIX1.3\lib\heads init, init.exe, init.map, D:\MINIX1.3\lib\smodel D:\MINIX1.3\lib\minix
dos2out -i init
goto done
:done
del init.obj
echo on
