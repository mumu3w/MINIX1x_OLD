echo off
Rem  Batch file that makes fsck module of Minix OS
Rem  Usage:  mkfsck [sep]
Rem          The last paramenter, if not null, generates Separate I&D
echo    Making FSCK module
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
Rem
Rem    All C code is compiled with the same options
tcc -c -k- -f- -G -mt -Di8088 -DSTANDALONE -DTURBO -I\usr\minix1.3\include fsck.c
if not "%1" == "" goto sep
echo Making FSCK module -- combined I&D
tasm /ml /i\usr\minix1.3\include fsck1.asm
tlink /ml /n /c fsck1.obj fsck.obj, fsck.exe, fsck.map, \usr\minix1.3\lib\tmodel \usr\minix1.3\lib\minix
Rem  Convert to Minix format
dos2out fsck
goto done
:sep
echo Making FSCK module -- Separate I&D
rem
tasm /ml /i\usr\minix1.3\include /D_SID fsck1.asm
tlink /ml /n /c fsck1.obj fsck.obj,fsck.exe,fsck.map, \usr\minix1.3\lib\smodel \usr\minix1.3\lib\minix
dos2out -i fsck
goto done
:done
rem del fsck1.obj
echo on
