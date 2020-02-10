set compilerpath=%~1
path %compilerpath%\bin;%path%
cd ..\..\..\frameworks\osiris\3.01
"%compilerpath%\bin\g++" -w -c -Iinclude pclake_plus\pclake_plus.cpp pcmodel\pcmodel_lakeplus.cpp pclake_plus\pl61316c.cpp pclake_plus\pl61316cd.cpp pclake_plus\pl61316ci.cpp pclake_plus\pl61316cc.cpp pclake_plus\pl61316db.cpp
"%compilerpath%\bin\g++" -o bin\pclake_plus.exe pclake_plus.o pcmodel_lakeplus.o pl61316c.o pl61316cd.o pl61316ci.o pl61316cc.o pl61316db.o -L./ -l bin\osiris -static-libgcc -static-libstdc++
del *.o
pause