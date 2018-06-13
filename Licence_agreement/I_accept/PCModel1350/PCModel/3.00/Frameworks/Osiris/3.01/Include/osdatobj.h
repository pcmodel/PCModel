/*------------------------------------------------------------------------*/
/*  Osdatobj.h                                                            */
/*  Copyright (c) 1993, 1999                                              */
/*  W.M. Mooij                                                            */
/*  Netherlands Institute of Ecology                                      */
/*  Centre for Limnology                                                  */
/*  Rijksstraatweg 6                                                      */
/*  3631 AC  Nieuwersluis                                                 */
/*  The Netherlands                                                       */
/*  tel: +31 294 239300                                                   */
/*  fax: +31 294 232224                                                   */
/*  e-mail: mooij@cl.nioo.knaw.nl                                         */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __OSDATOBJ_H
#define __OSDATOBJ_H

#include "wmascobj.h"
#include "wmconobj.h"
#include "wmmomobj.h"
#include "wmnumobj.h"
#include "wmodeobj.h"
#include "wmoptobj.h"
#include "wmcrsobj.h"
#include "wmrndobj.h"
#include "wmstrobj.h"
#include "wmsumobj.h"

// Constant definitions

#define CLOSED -1
#define OPEN 0

#define ZERO_OR_MORE 0
#define EXACTLY_ONE 1
#define AT_LEAST_ONE 2

#define NO_EOF_CHECK 0
#define EOF_CHECK 1

class DatObject;
class DatFile;

// Class definitions

class DatObject : public SrtObject
{ public :
    // Member functions
            DatObject    &addDbl(int i, double a);
            DatObject    &addFlt(int i, float a);
            DatObject    &addInt(int i, int a);
            DatObject    &addLng(int i, long a);
            long          getDatId() const;
            double        getDbl(int i) const;
            double       *getDblPtr(int i) const;
            double       &getDblRef(int i) const;
            float         getFlt(int i) const;
            float        *getFltPtr(int i) const;
            float        &getFltRef(int i) const;
            int           getInt(int i) const;
            int          *getIntPtr(int i) const;
            int          &getIntRef(int i) const;
            long          getLng(int i) const;
            long         *getLngPtr(int i) const;
            long         &getLngRef(int i) const;
            StrObject    &getStr(int i) const;
            SumObject    &getSum(int i) const;
            DatObject    &mulDbl(int i, double m);
            DatObject    &mulFlt(int i, float m);
            DatObject    &mulInt(int i, int m);
            DatObject    &mulLng(int i, long m);
            DatObject    &powDbl(int i, double e);
            DatObject    &powFlt(int i, float e);
            DatObject    &powInt(int i, int e);
            DatObject    &powLng(int i, long e);
            void          printToStream(ostream &os, int endsWithMinusOne) const;
            DatObject    &setDbl(int i, double s);
            DatObject    &setFlt(int i, float s);
            DatObject    &setInt(int i, int s);
            DatObject    &setLng(int i, long s);
            DatObject    &setStr(int i, StrObject &s);
  protected :
    // Friends
    friend  class         System;
    friend  class         LocalCondition;
    friend  class         LocalResource;
    friend  class         Individual;
    // Member functions
                          DatObject();
                          DatObject(const DatObject &d);
                          DatObject(DatFile &f);
    virtual              ~DatObject();
            void          allocateMemory();
            void          copyData(const DatObject &d);
            void          condCopyData(const DatObject &d);
            void          freeMemory();
    virtual int           getClassId() const;
    virtual char         *getClassName() const;
    static  long          getDatInstanceCounter();
    virtual long          getHashId() const;
    virtual int           isEqual(const WmmObject &o) const;
    virtual int           isSmaller(const WmmObject &o) const;
                          operator const char *();
            DatObject    &operator =(const DatObject &d);
    virtual void          setTo(ostream &os) const;
            void          printToLogFile(DatFile &f) const;
            void          printToOutFile(DatFile &f) const;
            void          readData(DatFile &f);
            void          readFromStream(istream &is, int endsWithMinusOne);
    // Data members
            long          datId;
    static  long          datInstanceCounter;
            double       *dblArrayPtr;
            int           dblNumber;
            float        *fltArrayPtr;
            int           fltNumber;
            int          *intArrayPtr;
            int           intNumber;
            long         *lngArrayPtr;
            int           lngNumber;
            StrObject    *strArrayPtr;
            int           strNumber;
            SumObject    *sumArrayPtr;
            int           sumNumber;
};

// Class definitions

class DatFile
{ public :
    // Member functions
                         ~DatFile();
            DatFile      &setDblNumber(int i);
            DatFile      &setFltNumber(int i);
            DatFile      &setIntNumber(int i);
            DatFile      &setLngNumber(int i);
            DatFile      &setStrNumber(int i);
            DatFile      &setSumNumber(int i);
  private :
    // Friends
    friend  class         DatObject;
    friend  class         Analyser;
    friend  class         System;
    friend  class         GlobalResource;
    friend  class         GlobalCondition;
    friend  class         Population;
    friend  class         Habitat;
    friend  class         LocalCondition;
    friend  class         LocalResource;
    friend  class         Individual;
    // Member functions
                          DatFile();
                          DatFile(const DatFile &f);
            void          allocateMemory();
            DatFile      &closeInpFile(int i);
            DatFile      &closeDmpFile();
            DatFile      &closeLogFile();
            DatFile      &closeOutFile();
            void          freeMemory();
            int           getDblNumber() const;
            StrObject     getDblTitle(int i) const;
            StrObject    *getDblTitleArrayPtr() const;
            ofstream     &getDmpFile();
            int           getFltNumber() const;
            StrObject     getFltTitle(int i) const;
            StrObject    *getFltTitleArrayPtr() const;
            istream      &getInputFileToRead();
            long          getInputRecordNumber() const;
            int           getIntNumber() const;
            StrObject     getIntTitle(int i) const;
            StrObject    *getIntTitleArrayPtr() const;
            int           getLngNumber() const;
            StrObject     getLngTitle(int i) const;
            StrObject    *getLngTitleArrayPtr() const;
            ostream      &getLogFileToWrite();
            ostream      &getOutputFileToWrite();
            long          getOutputRecordNumber() const;
            int           getStrNumber() const;
            StrObject     getStrTitle(int i) const;
            StrObject    *getStrTitleArrayPtr() const;
            int           getSumNumber() const;
            StrObject     getTitle() const;
            int           isMoreInput() const;
            DatFile      &openDmpFile(StrObject f, long l);
            DatFile      &openInpFile(StrObject f, long l);
            DatFile      &openLogFile(StrObject f, long r);
            DatFile      &openOutFile(StrObject f, long r);
            DatFile      &operator =(const DatFile &f);
            void          readColumnNames();
            void          readFormatFromColumnNames();
            void          readRecordNumberFromData(long l);
            DatFile      &setOutputRecordNumber(long n);
            void          writeColumnNames(ostream &o);
    // Data members
            int           dblNumber;
            StrObject    *dblTitleArrayPtr;
            ofstream      dmpFile;
            int           endsWithMinusOne;
            int           fltNumber;
            StrObject    *fltTitleArrayPtr;
            ifstream      inputFile;
            StrObject     inputFileName;
            long          inputRecordCounter;
            long          inputRecordNumber;
            int           intNumber;
            StrObject    *intTitleArrayPtr;
            int           lngNumber;
            StrObject    *lngTitleArrayPtr;
            ofstream      logFile;
            StrObject     logFileName;
            long          logRecordCounter;
            long          logRecordNumber;
            ofstream      outputFile;
            StrObject     outputFileName;
            long          outputRecordCounter;
            long          outputRecordNumber;
            int           strNumber;
            StrObject    *strTitleArrayPtr;
            int           sumNumber;
            StrObject     title;
            int           version;
};

// Inline member functions DatObject

inline DatObject::DatObject() :
  datId(datInstanceCounter++),
  lngNumber(0), lngArrayPtr(0),
  intNumber(0), intArrayPtr(0),
  dblNumber(0), dblArrayPtr(0),
  fltNumber(0), fltArrayPtr(0),
  strNumber(0), strArrayPtr(0),
  sumNumber(0), sumArrayPtr(0)
{
}

inline istream &DatFile::getInputFileToRead()
{   if (inputRecordCounter == CLOSED)
    {   theOutObject.printError("Input file ", inputFileName, " not yet open");
    }
    else
    {   inputRecordCounter++;
    }
    return inputFile;
}

inline int DatFile::getDblNumber() const
{   return dblNumber;
}

inline int DatFile::getFltNumber() const
{   return fltNumber;
}

inline int DatFile::getIntNumber() const
{   return intNumber;
}

inline int DatFile::getLngNumber() const
{   return lngNumber;
}

inline int DatFile::getStrNumber() const
{   return strNumber;
}

inline int DatFile::getSumNumber() const
{   return sumNumber;
}

inline DatObject::DatObject(DatFile &f) :
  datId(datInstanceCounter++),
  lngNumber(f.getLngNumber()), lngArrayPtr(0),
  intNumber(f.getIntNumber()), intArrayPtr(0),
  dblNumber(f.getDblNumber()), dblArrayPtr(0),
  fltNumber(f.getFltNumber()), fltArrayPtr(0),
  strNumber(f.getStrNumber()), strArrayPtr(0),
  sumNumber(f.getSumNumber()), sumArrayPtr(0)
{   allocateMemory();
    readFromStream(f.getInputFileToRead(), f.endsWithMinusOne);
}

inline DatObject::DatObject(const DatObject &d) :
  datId(datInstanceCounter++),
  lngNumber(d.lngNumber), lngArrayPtr(0),
  intNumber(d.intNumber), intArrayPtr(0),
  dblNumber(d.dblNumber), dblArrayPtr(0),
  fltNumber(d.fltNumber), fltArrayPtr(0),
  strNumber(d.strNumber), strArrayPtr(0),
  sumNumber(d.sumNumber), sumArrayPtr(0)
{   allocateMemory();
    copyData(d);
}

inline DatObject::~DatObject()
{   freeMemory();
}

inline DatObject &DatObject::addDbl(int i, double a)
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    dblArrayPtr[i] += a;
    return *this;
}

inline DatObject &DatObject::addFlt(int i, float a)
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    fltArrayPtr[i] += a;
    return *this;
}

inline DatObject &DatObject::addInt(int i, int a)
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    intArrayPtr[i] += a;
    return *this;
}

inline DatObject &DatObject::addLng(int i, long a)
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    lngArrayPtr[i] += a;
    return *this;
}

inline int DatObject::getClassId() const
{   return datObjectClass;
}

inline char *DatObject::getClassName() const
{   return "DatObject";
}

inline long DatObject::getDatId() const
{   return datId;
}

inline long DatObject::getDatInstanceCounter()
{   return datInstanceCounter;
}

inline double DatObject::getDbl(int i) const
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr[i];
}

inline double *DatObject::getDblPtr(int i) const
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr + i;
}

inline double &DatObject::getDblRef(int i) const
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    return dblArrayPtr[i];
}

inline float DatObject::getFlt(int i) const
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr[i];
}

inline float *DatObject::getFltPtr(int i) const
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr + i;
}

inline float &DatObject::getFltRef(int i) const
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    return fltArrayPtr[i];
}

inline long DatObject::getHashId() const
{   return (long)datId;
}

inline int DatObject::getInt(int i) const
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr[i];
}

inline int *DatObject::getIntPtr(int i) const
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr + i;
}

inline int &DatObject::getIntRef(int i) const
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    return intArrayPtr[i];
}

inline long DatObject::getLng(int i) const
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr[i];
}

inline long *DatObject::getLngPtr(int i) const
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr + i;
}

inline long &DatObject::getLngRef(int i) const
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    return lngArrayPtr[i];
}

inline StrObject &DatObject::getStr(int i) const
{   if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string array limits");
    }
    return strArrayPtr[i];
}

inline SumObject &DatObject::getSum(int i) const
{   if (i < 0 || i >= sumNumber)
    {    theOutObject.printError("Out of SumObject array limits");
    }
    return sumArrayPtr[i];
}

inline int DatObject::isEqual(const WmmObject &o) const
{   return datId == ((DatObject &)o).datId;
}

inline int DatObject::isSmaller(const WmmObject &o) const
{   return datId < ((DatObject &)o).datId;
}

inline DatObject &DatObject::mulDbl(int i, double m)
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    dblArrayPtr[i] *= m;
    return *this;
}

inline DatObject &DatObject::mulFlt(int i, float m)
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    fltArrayPtr[i] *= m;
    return *this;
}

inline DatObject &DatObject::mulInt(int i, int m)
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    intArrayPtr[i] *= m;
    return *this;
}

inline DatObject &DatObject::mulLng(int i, long m)
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    lngArrayPtr[i] *= m;
    return *this;
}

inline DatObject &DatObject::operator =(const DatObject &d)
{   if (*this != d)
    {   freeMemory();
        lngNumber = d.lngNumber;
        intNumber = d.intNumber;
        dblNumber = d.dblNumber;
        fltNumber = d.fltNumber;
        strNumber = d.strNumber;
        sumNumber = d.sumNumber;
        allocateMemory();
        copyData(d);
    }
    return *this;
}

inline void DatObject::setTo(ostream &os) const
{   printToStream(os, TRUE);
}

inline DatObject &DatObject::powDbl(int i, double e)
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    dblArrayPtr[i] = pow(dblArrayPtr[i], e);
    return *this;
}

inline DatObject &DatObject::powFlt(int i, float e)
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    fltArrayPtr[i] = (float)pow((double)fltArrayPtr[i], (double)e);
    return *this;
}

inline DatObject &DatObject::powInt(int i, int e)
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    intArrayPtr[i] = (int)pow((double)intArrayPtr[i], (double)e);
    return *this;
}

inline DatObject &DatObject::powLng(int i, long e)
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    lngArrayPtr[i] = (long)pow((double)lngArrayPtr[i], (double)e);
    return *this;
}

inline void DatObject::readData(DatFile &f)
{   readFromStream(f.getInputFileToRead(), f.endsWithMinusOne);
}

inline DatObject &DatObject::setDbl(int i, double s)
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double array limits");
    }
    dblArrayPtr[i] = s;
    return *this;
}

inline DatObject &DatObject::setFlt(int i, float s)
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float array limits");
    }
    fltArrayPtr[i] = s;
    return *this;
}

inline DatObject &DatObject::setInt(int i, int s)
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int array limits");
    }
    intArrayPtr[i] = s;
    return *this;
}

inline DatObject &DatObject::setLng(int i, long s)
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long array limits");
    }
    lngArrayPtr[i] = s;
    return *this;
}

inline DatObject &DatObject::setStr(int i, StrObject &s)
{   if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string array limits");
    }
    strArrayPtr[i] = s;
    return *this;
}

// Inline member functions DatFile

inline DatFile::DatFile() :
  version(0),
  dblNumber(0),
  fltNumber(0),
  intNumber(0),
  lngNumber(0),
  strNumber(0),
  sumNumber(0),
  dblTitleArrayPtr(0),
  fltTitleArrayPtr(0),
  intTitleArrayPtr(0),
  lngTitleArrayPtr(0),
  strTitleArrayPtr(0),
  endsWithMinusOne(FALSE),
  inputRecordCounter(CLOSED),
  inputRecordNumber(0),
  logRecordCounter(CLOSED),
  logRecordNumber(0),
  outputRecordCounter(CLOSED),
  outputRecordNumber(0)
{
}

inline DatFile::DatFile(const DatFile &f)
{   theOutObject.printError("Copy constructor DatFile not yet implemented");
    f;
}

inline DatFile::~DatFile()
{   freeMemory();
}

inline StrObject DatFile::getDblTitle(int i) const
{   if (i < 0 || i >= dblNumber)
    {   theOutObject.printError("Out of double title array limits");
    }
    return dblTitleArrayPtr[i];
}

inline StrObject *DatFile::getDblTitleArrayPtr() const
{   return dblTitleArrayPtr;
}

inline ofstream &DatFile::getDmpFile()
{   return dmpFile;
}

inline StrObject DatFile::getFltTitle(int i) const
{   if (i < 0 || i >= fltNumber)
    {   theOutObject.printError("Out of float title array limits");
    }
    return fltTitleArrayPtr[i];
}

inline StrObject *DatFile::getFltTitleArrayPtr() const
{   return fltTitleArrayPtr;
}

inline long DatFile::getInputRecordNumber() const
{   return inputRecordNumber;
}

inline StrObject DatFile::getIntTitle(int i) const
{   if (i < 0 || i >= intNumber)
    {   theOutObject.printError("Out of int title array limits");
    }
    return intTitleArrayPtr[i];
}

inline StrObject *DatFile::getIntTitleArrayPtr() const
{   return intTitleArrayPtr;
}

inline StrObject DatFile::getLngTitle(int i) const
{   if (i < 0 || i >= lngNumber)
    {   theOutObject.printError("Out of long title array limits");
    }
    return lngTitleArrayPtr[i];
}

inline StrObject *DatFile::getLngTitleArrayPtr() const
{   return lngTitleArrayPtr;
}

inline ostream &DatFile::getLogFileToWrite()
{   if (logRecordCounter == CLOSED)
    {   theOutObject.printError("Log file ", logFileName, " not yet open");
    }
    else
    {   logRecordCounter++;
    }
    return logFile;
}

inline ostream &DatFile::getOutputFileToWrite()
{   if (outputRecordCounter == CLOSED)
    {   theOutObject.printError("Output file ", outputFileName, " not yet open");
    }
    else
    {   outputRecordCounter++;
    }
    return outputFile;
}

inline long DatFile::getOutputRecordNumber() const
{   return outputRecordNumber;
}

inline StrObject DatFile::getStrTitle(int i) const
{   if (i < 0 || i >= strNumber)
    {   theOutObject.printError("Out of string title array limits");
    }
    return strTitleArrayPtr[i];
}

inline StrObject *DatFile::getStrTitleArrayPtr() const
{   return strTitleArrayPtr;
}

inline StrObject DatFile::getTitle() const
{   return title;
}

inline int DatFile::isMoreInput() const
{   return inputRecordCounter < inputRecordNumber;
}

inline DatFile &DatFile::operator =(const DatFile &f)
{   theOutObject.printError("Assignment operator DatFile not yet implemented");
    f;
    return *this;
}

inline DatFile &DatFile::setDblNumber(int i)
{   dblNumber = i;
    return *this;
}

inline DatFile &DatFile::setFltNumber(int i)
{   fltNumber = i;
    return *this;
}

inline DatFile &DatFile::setIntNumber(int i)
{   intNumber = i;
    return *this;
}

inline DatFile &DatFile::setLngNumber(int i)
{   lngNumber = i;
    return *this;
}

inline DatFile &DatFile::setOutputRecordNumber(long n)
{   outputRecordNumber = n;
    return *this;
}

inline DatFile &DatFile::setStrNumber(int i)
{   strNumber = i;
    return *this;
}

inline DatFile &DatFile::setSumNumber(int i)
{   sumNumber = i;
    return *this;
}

#endif


