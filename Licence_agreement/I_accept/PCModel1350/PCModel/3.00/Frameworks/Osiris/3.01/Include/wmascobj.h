/*------------------------------------------------------------------------*/
/*  Wmascobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluiskenslaan 21                                                      */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMASCOBJ_H
#define __WMASCOBJ_H

#include "wmwmmobj.h"
#include "wmstrobj.h"

// Class definitions

class AscObject
{ public :
    // Member functions
                       AscObject();
                      ~AscObject();

            int        checkFile(int l, int i, int d, int f, int s, int n) const;

            void       readFile(StrObject &s);
            void       writeFile(StrObject &s) const;

            StrObject  getFileName() const;
            int        getRecordNumber() const;
            double     getDbl(int n, int i) const;
            double    *getDblPtr(int n, int i) const;
            double    &getDblRef(int n, int i) const;
            int        getDblNumber() const;
            StrObject  getDblTitle(int i) const;
            StrObject *getDblTitleArrayPtr() const;
            float      getFlt(int n, int i) const;
            float     *getFltPtr(int n, int i) const;
            float     &getFltRef(int n, int i) const;
            int        getFltNumber() const;
            StrObject  getFltTitle(int i) const;
            StrObject *getFltTitleArrayPtr() const;
            int        getInt(int n, int i) const;
            int       *getIntPtr(int n, int i) const;
            int       &getIntRef(int n, int i) const;
            int        getIntNumber() const;
            StrObject  getIntTitle(int i) const;
            StrObject *getIntTitleArrayPtr() const;
            long       getLng(int n, int i) const;
            long      *getLngPtr(int n, int i) const;
            long      &getLngRef(int n, int i) const;
            int        getLngNumber() const;
            StrObject  getLngTitle(int i) const;
            StrObject *getLngTitleArrayPtr() const;
            StrObject &getStr(int n, int i) const;
            StrObject *getStrPtr(int n, int i) const;
            StrObject &getStrRef(int n, int i) const;
            int        getStrNumber() const;
            StrObject  getStrTitle(int i) const;
            StrObject *getStrTitleArrayPtr() const;
  private :
    // Member functions
                       AscObject(const AscObject &f);
            AscObject &operator =(const AscObject &f);

            void       readColumnNames();
            void       readColumnNumber();
            void       readRecord(int n);
            void       readRecordNumber();

            void       writeColumnNames(ofstream &o) const;
            void       writeRecord(ofstream &o, int n) const;

    // Data members
            ifstream   ascFile;
            StrObject  ascFileName;

            int        recordNumber;
            int        dblNumber;
            int        fltNumber;
            int        intNumber;
            int        lngNumber;
            int        strNumber;
            StrObject *dblTitleArrayPtr;
            StrObject *fltTitleArrayPtr;
            StrObject *intTitleArrayPtr;
            StrObject *lngTitleArrayPtr;
            StrObject *strTitleArrayPtr;
            double    *dblArrayPtr;
            float     *fltArrayPtr;
            int       *intArrayPtr;
            long      *lngArrayPtr;
            StrObject *strArrayPtr;

};

// Inline member functions AscObject

inline AscObject::AscObject() :
   recordNumber(0),
   dblNumber(0),
   fltNumber(0),
   intNumber(0),
   lngNumber(0),
   strNumber(0),
   dblTitleArrayPtr(0),
   fltTitleArrayPtr(0),
   intTitleArrayPtr(0),
   lngTitleArrayPtr(0),
   strTitleArrayPtr(0),
   dblArrayPtr(0),
   fltArrayPtr(0),
   intArrayPtr(0),
   lngArrayPtr(0),
   strArrayPtr(0)
{
}

inline AscObject::AscObject(const AscObject &f)
{   theOutObject.printError("Copy constructor AscObject not implemented");
    f;
}

inline AscObject::~AscObject()
{   if (dblTitleArrayPtr)
    {   delete[] dblTitleArrayPtr;
        dblTitleArrayPtr = 0;
    }
    if (fltTitleArrayPtr)
    {   delete[] fltTitleArrayPtr;
        fltTitleArrayPtr = 0;
    }
    if (intTitleArrayPtr)
    {   delete[] intTitleArrayPtr;
        intTitleArrayPtr = 0;
    }
    if (lngTitleArrayPtr)
    {   delete[] lngTitleArrayPtr;
        lngTitleArrayPtr = 0;
    }
    if (strTitleArrayPtr)
    {   delete[] strTitleArrayPtr;
        strTitleArrayPtr = 0;
    }
    if (dblArrayPtr)
    {   delete[] dblArrayPtr;
        dblArrayPtr = 0;
    }
    if (fltArrayPtr)
    {   delete[] fltArrayPtr;
        fltArrayPtr = 0;
    }
    if (intArrayPtr)
    {   delete[] intArrayPtr;
        intArrayPtr = 0;
    }
    if (lngArrayPtr)
    {   delete[] lngArrayPtr;
        lngArrayPtr = 0;
    }
    if (strArrayPtr)
    {   delete[] strArrayPtr;
        strArrayPtr = 0;
    }
}

inline AscObject &AscObject::operator =(const AscObject &f)
{   theOutObject.printError("Assignment operator AscObject not implemented");
    f;
    return *this;
}

inline int AscObject::checkFile(int l, int i, int d, int f, int s, int n) const
{   return (l < 0 || lngNumber == l) &&
           (i < 0 || intNumber == i) &&
           (d < 0 || dblNumber == d) &&
           (f < 0 || fltNumber == f) &&
           (s < 0 || strNumber == s) &&
           (n < 0 || recordNumber == n);
}

inline StrObject AscObject::getFileName() const
{   return ascFileName;
}

inline int AscObject::getRecordNumber() const
{   return recordNumber;
}

inline double AscObject::getDbl(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr[i + n * dblNumber];
}

inline double *AscObject::getDblPtr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr + i + n * dblNumber;
}

inline double &AscObject::getDblRef(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr[i + n * dblNumber];
}

inline int AscObject::getDblNumber() const
{   return dblNumber;
}

inline StrObject AscObject::getDblTitle(int i) const
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double title array limits");
    }
    return dblTitleArrayPtr[i];
}

inline StrObject *AscObject::getDblTitleArrayPtr() const
{   return dblTitleArrayPtr;
}

inline float AscObject::getFlt(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr[i + n * fltNumber];
}

inline float *AscObject::getFltPtr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr + i + n * fltNumber;
}

inline float &AscObject::getFltRef(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr[i + n * fltNumber];
}

inline int AscObject::getFltNumber() const
{   return fltNumber;
}

inline StrObject AscObject::getFltTitle(int i) const
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float title array limits");
    }
    return fltTitleArrayPtr[i];
}

inline StrObject *AscObject::getFltTitleArrayPtr() const
{   return fltTitleArrayPtr;
}

inline int AscObject::getInt(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr[i + n * intNumber];
}

inline int *AscObject::getIntPtr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr + i + n * intNumber;
}

inline int &AscObject::getIntRef(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr[i + n * intNumber];
}

inline int AscObject::getIntNumber() const
{   return intNumber;
}

inline StrObject AscObject::getIntTitle(int i) const
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int title array limits");
    }
    return intTitleArrayPtr[i];
}

inline StrObject *AscObject::getIntTitleArrayPtr() const
{   return intTitleArrayPtr;
}

inline long AscObject::getLng(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr[i + n * lngNumber];
}

inline long *AscObject::getLngPtr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr + i + n * lngNumber;
}

inline long &AscObject::getLngRef(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr[i + n * lngNumber];
}

inline int AscObject::getLngNumber() const
{   return lngNumber;
}

inline StrObject AscObject::getLngTitle(int i) const
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long title array limits");
    }
    return lngTitleArrayPtr[i];
}

inline StrObject *AscObject::getLngTitleArrayPtr() const
{   return lngTitleArrayPtr;
}

inline StrObject &AscObject::getStr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string array limits");
    }
    return strArrayPtr[i + n * strNumber];
}

inline StrObject *AscObject::getStrPtr(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string array limits");
    }
    return &strArrayPtr[i + n * strNumber];
}

inline StrObject &AscObject::getStrRef(int n, int i) const
{   if (n < 0 || n >= recordNumber)
    {   theOutObject.printError("Out of record limits");
    }
    if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string array limits");
    }
    return strArrayPtr[i + n * strNumber];
}

inline int AscObject::getStrNumber() const
{   return strNumber;
}

inline StrObject AscObject::getStrTitle(int i) const
{   if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string title array limits");
    }
    return strTitleArrayPtr[i];
}

inline StrObject *AscObject::getStrTitleArrayPtr() const
{   return strTitleArrayPtr;
}

#endif


