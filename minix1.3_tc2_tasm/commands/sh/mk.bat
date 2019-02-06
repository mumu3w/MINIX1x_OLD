mcc -c sh1.c
mcc -c sh2.c
mcc -c sh3.c
mcc -c sh4.c
mcc -c sh5.c
mcc -c sh6.c
tlink /ml /n /c ..\..\lib\crtsot sh1 sh2 sh3 sh4 sh5 sh6, sh.exe, sh.map, ..\..\lib\tmodel ..\..\lib\minix
dos2out sh.exe
mcc -c -ms sh1.c
mcc -c -ms sh2.c
mcc -c -ms sh3.c
mcc -c -ms sh4.c
mcc -c -ms sh5.c
mcc -c -ms sh6.c
tlink /ml /n /c ..\..\lib\crtsos sh1 sh2 sh3 sh4 sh5 sh6, sh.exe, sh.map, ..\..\lib\smodel ..\..\lib\minix
dos2out -i sh.exe