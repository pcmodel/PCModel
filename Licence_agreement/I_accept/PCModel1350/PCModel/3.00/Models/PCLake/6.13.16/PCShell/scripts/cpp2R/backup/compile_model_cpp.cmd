rem # this batchfile compiles 'model.cpp' with the GCC compiler (which can be downloaded by downloading RTools at http://cran.r-project.org/bin/windows/Rtools/ )
set PATH=c:\Rtools\bin;c:\Rtools\gcc-4.6.3\bin;c:\PROGRA~1\R\R-3.1.2\bin
del model.o            
del model.dll
R CMD SHLIB model.cpp
pause