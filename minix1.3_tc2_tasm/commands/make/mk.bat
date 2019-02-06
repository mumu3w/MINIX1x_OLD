mcc -c check.c
mcc -c input.c
mcc -c macro.c
mcc -c main.c
mcc -c make.c
mcc -c reader.c
mcc -c rules.c
tlink /ml /n /c ..\..\lib\crtsot make check input macro main reader rules, make.exe, make.map, ..\..\lib\tmodel ..\..\lib\minix
dos2out make.exe

