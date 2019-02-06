mcc -c mined1.c
mcc -c mined2.c
tlink /ml /n /c ..\..\lib\crtsot mined1 mined2, mined.exe, mined.map, ..\..\lib\tmodel ..\..\lib\minix
dos2out mined.exe
mcc -ms -c mined1.c
mcc -ms -c mined2.c
tlink /ml /n /c ..\..\lib\crtsos mined1 mined2, mined.exe, mined.map, ..\..\lib\smodel ..\..\lib\minix
dos2out -i mined.exe