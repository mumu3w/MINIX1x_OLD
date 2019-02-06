masm bootblok,,nul,nul > bootblok.lst
rem link bootblok,,nul,nul >>bootblok.lst
link bootblok,,,, >>bootblok.lst
exe2bin bootblok.exe bootblok.bin >>bootblok.lst
