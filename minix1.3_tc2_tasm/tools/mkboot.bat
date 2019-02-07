echo off
echo Making Boot Module
tasm /ml /iD:\MINIX1.3\include bootblok
tlink /ml /n /c bootblok
dc bootblok.exe bootblok 512
del bootblok.exe
del bootblok.obj
del bootblok.map
echo on
