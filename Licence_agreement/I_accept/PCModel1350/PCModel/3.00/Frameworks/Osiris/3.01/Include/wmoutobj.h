/*------------------------------------------------------------------------*/
/*  Wmoutobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMOUTOBJ_H
#define __WMOUTOBJ_H

#define __WMM_NRC
//#define __WMM_BOR
#ifdef __BORLANDC__
    //#define __WMM_MPP
#endif

#include "stdlib.h"   //  1 -> "_defs.h", "_null.h"
#include "math.h"     //  2 -> "stddef.h"
#include "limits.h"   //  3
#include "string.h"   //  4
#include "ctype.h"    //  5
#include "time.h"     //  6
#ifdef __GNUG__
    #include "new"      //  7
    #include "iostream" //  8
    #include "iomanip"  //  9
    #include "fstream"  // 10
    #include "sstream"  // 11
#else
    #include "new.h"      //  7
    #include "iostream.h" //  8
    #include "iomanip.h"  //  9
    #include "fstream.h"  // 10
    #ifdef __BORLANDC__
        #include "strstrea.h" // 11
        #include "conio.h"    // 12
    #else
        #include "strstream.h"// 11
    #endif
#endif

#ifdef __GNUG__
    using namespace std;
#endif

#ifdef __GNUG__
class istrstream : public istringstream
{
};

class ostrstream : public ostringstream
{
};
#endif

#define CHAR_BUFFER_SIZE 4096
#define FALSE 0
#define TRUE  1

class OutObject
{ public :
                     OutObject();
                    ~OutObject();
    static void      closeOutFile()      {theOutFile.close();}
           char     *getBuffer()         {return theCharBuffer;}
           int       getBufferSize()     {return CHAR_BUFFER_SIZE * 4 - 1;}
           char     *getBuffer0()        {return theCharBuffer0;}
           int       getBufferSize0()    {return CHAR_BUFFER_SIZE - 1;}
           char     *getBuffer1()        {return theCharBuffer1;}
           int       getBufferSize1()    {return CHAR_BUFFER_SIZE - 1;}
           char     *getBuffer2()        {return theCharBuffer2;}
           int       getBufferSize2()    {return CHAR_BUFFER_SIZE - 1;}
           char     *getBuffer3()        {return theCharBuffer3;}
           int       getBufferSize3()    {return CHAR_BUFFER_SIZE - 1;}
    static ostream  &getConsole()        {return cout;}
    static ostream  &getConsole(int w)   {getConsole() << setw(w); return getConsole();}
    static ostream  &getConsole(int w, int p) {getConsole() << setw(w) << setprecision(p); return getConsole();}
    static ostream  &getOutFile()        {return theOutFile;}
    static void      openOutFile(const char *f) {theOutFile.open(f);}
#ifdef __BORLANDC__
    static void      pressAnyKey()       {printString("Press any key to continue ..."); int c = getch(); if (!c) getch();}
#else
    static void      pressAnyKey()       {printString("Enter a character to continue ... "); char c; cin >> c;}
#endif
           void      printBuffer()       {printString(theCharBuffer);}
           void      printBuffer0()      {printString(theCharBuffer0);}
           void      printBuffer1()      {printString(theCharBuffer1);}
           void      printBuffer2()      {printString(theCharBuffer2);}
           void      printBuffer3()      {printString(theCharBuffer3);}
    static void      printError(const char *a, const char *b = "", const char *c =
                       "", const char *d = "", const char *e = "")
                       {printString("Error:   ", a, b, c, d, e); pressAnyKey(); exit(1);}
    static void      printMessage(const char *a, const char *b = "", const char *c =
                       "", const char *d = "", const char *e = "")
                       {printString("Message: ", a, b, c, d, e);}
    static void      printString(const char *a, const char *b = "", const char *c =
                       "", const char *d = "", const char *e = "", const char *f = "")
                       {getConsole() << a << b << c << d << e << f << endl;
                        //getOutFile() << a << b << c << d << e << f << endl;
                       }
    static void      printWarning(const char *a, const char *b = "", const char *c =
                       "", const char *d = "", const char *e = "", const char *f = "")
                       {printString("Warning: ", a, b, c, d, e);}
#ifdef __BORLANDC__
    static void      setClrScr()          {clrscr();}
#else
    static void      setClrScr()          {}
#endif
#ifdef __BORLANDC__
    static void      setPos(int x, int y) {gotoxy(x, y);}
#else
    static void      setPos(int x, int y) {}
#endif
  private :
    static ofstream  theOutFile;
           char     *theCharBuffer;
           char     *theCharBuffer0;
           char     *theCharBuffer1;
           char     *theCharBuffer2;
           char     *theCharBuffer3;
};

extern OutObject theOutObject;

#endif







