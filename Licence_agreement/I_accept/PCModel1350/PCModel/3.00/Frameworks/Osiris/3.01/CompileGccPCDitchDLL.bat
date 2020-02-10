set compilerpath=%~1
path %compilerpath%\bin;%path%
cd ..\..\..\frameworks\osiris\3.01
"%compilerpath%\bin\g++" -w -c -Iinclude pcditch\pcditch.cpp pcmodel\pcmodel_ditch.cpp pcditch\pd21316c.cpp pcditch\pd21316cd.cpp pcditch\pd21316ci.cpp pcditch\pd21316cc.cpp pcditch\pd21316db.cpp
"%compilerpath%\bin\g++" -o bin\pcditch.exe pcditch.o pcmodel_ditch.o pd21316c.o pd21316cd.o pd21316ci.o pd21316cc.o pd21316db.o -L./ -l bin\osiris -static-libgcc -static-libstdc++
del *.o
pause