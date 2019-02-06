mcc -c disfp.c 
mcc -c dishand.c
mcc -c dismain.c
mcc -c disrel.c 
mcc -c distabs.c
tlink /ml /n /c ..\..\lib\crtsot disfp dishand dismain disrel distabs, dis88.exe, dis88.map, ..\..\lib\tmodel ..\..\lib\minix
dos2out dis88.exe
mcc -ms -c disfp.c
mcc -ms -c dishand.c
mcc -ms -c dismain.c
mcc -ms -c disrel.c
mcc -ms -c distabs.c
tlink /ml /n /c ..\..\lib\crtsos disfp dishand dismain disrel distabs, dis88.exe, dis88.map, ..\..\lib\smodel ..\..\lib\minix
dos2out -i dis88.exe