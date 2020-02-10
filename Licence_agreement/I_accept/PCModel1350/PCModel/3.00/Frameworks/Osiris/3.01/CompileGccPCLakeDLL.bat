set compilerpath=%~1
path %compilerpath%\bin;%path%
cd ..\..\..\frameworks\osiris\3.01
"%compilerpath%\bin\g++" -w -c -Iinclude pclake\pclake.cpp pcmodel\pcmodel_lake.cpp pclake\pl61316c.cpp pclake\pl61316cd.cpp pclake\pl61316ci.cpp pclake\pl61316cc.cpp pclake\pl61316db.cpp
"%compilerpath%\bin\g++" -o bin\pclake.exe pclake.o pcmodel_lake.o pl61316c.o pl61316cd.o pl61316ci.o pl61316cc.o pl61316db.o -L./ -l bin\osiris -static-libgcc -static-libstdc++
del *.o
pause