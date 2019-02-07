Rem     Batch file  to make the model sensitive  MINIX libraries
Rem     and the startup object files for each model, tiny, small.
Rem
Rem     There are 4 .asm Minix files for the libraries, the other .obj
Rem     files were taken from the TURBO C cs.lib.  These same routines
Rem     are used in both minix libraries, tmodel.lib and smodel.lib.
Rem
Rem     Before using this batch file, copy all the lib .asm files and
Rem     the lib turbo files into same directory with this batch file.
Rem	The result will be in lib directory in current directory
Rem
Rem    Delete any existing .lib files
del *.lib
rem mkdir lib
tlib C:\TC201\LIB\CS.LIB *ldiv *llsh *lrsh *lursh *lxmul *oldlrsh *overflow *pada
tlib C:\TC201\LIB\CS.LIB *padd *pcmp *pina *psbp * scopy *spush *setjmp
tasm /ml /iD:\MINIX1.3\include *.asm
tlib tmodel /C /E +portio +sendrec +getutil +catchsig
Rem
Rem    Files from Turbo C library
Rem
tlib tmodel /C /E +ldiv +llsh +lrsh +lursh +lxmul +oldlrsh +overflow +pada
tlib tmodel /C /E  +padd +pcmp +pina +psbp + scopy +spush +setjmp

rem copy tmodel.lib lib
copy crtso.obj crtsot.obj
copy head.obj headt.obj

Rem Make smodel.lib
tasm /ml /D_SID /iD:\MINIX1.3\include *.asm
tlib smodel /C /E +portio +sendrec +getutil +catchsig
tlib smodel /C /E +ldiv +llsh +lrsh +lursh +lxmul +oldlrsh +overflow +pada
tlib smodel /C /E  +padd +pcmp +pina +psbp + scopy +spush +setjmp

rem copy smodel.lib lib
copy crtso.obj crtsos.obj
copy head.obj heads.obj




