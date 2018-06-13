/*------------------------------------------------------------------------*/
/*  Wmnumobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMNUMOBJ_H
#define __WMNUMOBJ_H

#include "wmwmmobj.h"
#include "wmrndobj.h"
#include "wmconobj.h"

// Class definitions TmpNumObject

template <class T> class TmpNumObject: public SrtObject
{ public:
                                  TmpNumObject(const T i = 0);
                                  TmpNumObject(const TmpNumObject<T> &v);
    virtual                      ~TmpNumObject();
            TmpNumObject<T>      &addData(const T v);
            TmpNumObject<T>      &addData(const TmpNumObject<T> &v);
    virtual int                   getClassId() const;
    virtual char                 *getClassName() const;
    virtual long                  getHashId() const;
            T                     getData() const;
    virtual int                   isEqual(const WmmObject &o) const;
    virtual int                   isSmaller(const WmmObject &o) const;
                                  operator T() const;
            TmpNumObject<T>      &operator =(const T v);
            TmpNumObject<T>      &operator =(const TmpNumObject<T> &v);
    virtual void                  setTo(ostream &o) const;
            TmpNumObject<T>      &setData(const T v);
            TmpNumObject<T>      &setData(const TmpNumObject<T> &v);
  protected:
            T                     data;
};

// Type definitions TmpNumObject

typedef TmpNumObject<unsigned char>            UnsChrObject;
typedef TmpNumObject<unsigned short int>       UnsShtObject;
typedef TmpNumObject<unsigned int>             UnsIntObject;
typedef TmpNumObject<unsigned long int>        UnsLngObject;
typedef TmpNumObject<signed char>              ChrObject;
typedef TmpNumObject<signed short int>         ShtObject;
typedef TmpNumObject<signed int>               IntObject;
typedef TmpNumObject<signed long int>          LngObject;
typedef TmpNumObject<float>                    FltObject;
typedef TmpNumObject<double>                   DblObject;
typedef TmpNumObject<long double>              LngDblObject;

// Class definitions TmpNumArrayObject

template <class T> class TmpNumArrayObject: public SrtObject
{ public:
                                  TmpNumArrayObject(const int i = 0);
                                  TmpNumArrayObject(const TmpNumArrayObject<T> &v);
    virtual                      ~TmpNumArrayObject();
            TmpNumArrayObject<T> &addData(int i, const T v);
            T                     getData(int i) const;
            T                    *getDataPtr(){return dataPtr;}
    virtual long                  getHashId() const;
    virtual int                   getClassId() const;
    virtual char                 *getClassName() const;
            int                   getSize() const;
    virtual int                   isEqual(const WmmObject &o) const;
    virtual int                   isSmaller(const WmmObject &o) const;
            int                   isWithinLimits(int i) const;
            T                    &operator [](int i);
            TmpNumArrayObject<T> &operator =(const TmpNumArrayObject<T> &v);
            TmpNumArrayObject<T> &setData(int i, const T v);
            TmpNumArrayObject<T> &setRndOrder();
    virtual void                  setTo(ostream &o) const;
  protected:
            int                   dataNumber;
            T                    *dataPtr;
};

// Type definitions TmpNumArrayObject

typedef TmpNumArrayObject<unsigned char>       UnsChrArrayObject;
typedef TmpNumArrayObject<unsigned short int>  UnsShtArrayObject;
typedef TmpNumArrayObject<unsigned int>        UnsIntArrayObject;
typedef TmpNumArrayObject<unsigned long int>   UnsLngArrayObject;
typedef TmpNumArrayObject<signed char>         ChrArrayObject;
typedef TmpNumArrayObject<signed short int>    ShtArrayObject;
typedef TmpNumArrayObject<signed int>          IntArrayObject;
typedef TmpNumArrayObject<signed long int>     LngArrayObject;
typedef TmpNumArrayObject<float>               FltArrayObject;
typedef TmpNumArrayObject<double>              DblArrayObject;
typedef TmpNumArrayObject<long double>         LngDblArrayObject;

// Class definitions TmpNumVectorObject

template <class T> class TmpNumVectorObject: public TmpNumArrayObject<T>
{ public :
                                  TmpNumVectorObject(const int i = 0);
    virtual                      ~TmpNumVectorObject();
            T                    &operator ()(int i);
};

// Type definitions TmpNumVectorObject

typedef TmpNumVectorObject<unsigned char>      UnsChrVectorObject;
typedef TmpNumVectorObject<unsigned short int> UnsShtVectorObject;
typedef TmpNumVectorObject<unsigned int>       UnsIntVectorObject;
typedef TmpNumVectorObject<unsigned long int>  UnsLngVectorObject;
typedef TmpNumVectorObject<signed char>        ChrVectorObject;
typedef TmpNumVectorObject<signed short int>   ShtVectorObject;
typedef TmpNumVectorObject<signed int>         IntVectorObject;
typedef TmpNumVectorObject<signed long int>    LngVectorObject;
typedef TmpNumVectorObject<float>              FltVectorObject;
typedef TmpNumVectorObject<double>             DblVectorObject;
typedef TmpNumVectorObject<long double>        LngDblVectorObject;

// Class definitions TmpNumMatrixObject

template <class T> class TmpNumMatrixObject: public TmpNumArrayObject<T>
{ public :
                                  TmpNumMatrixObject(const int i = 0, const int j = 0);
    virtual                      ~TmpNumMatrixObject();
            T                    &operator ()(int i, int j);
    virtual void                  setTo(ostream &o) const;
  private :
            int                   dataNumberRow;
            int                   dataNumberColumn;
};

// Type definitions TmpNumMatrixObject

typedef TmpNumMatrixObject<unsigned char>      UnsChrMatrixObject;
typedef TmpNumMatrixObject<unsigned short int> UnsShtMatrixObject;
typedef TmpNumMatrixObject<unsigned int>       UnsIntMatrixObject;
typedef TmpNumMatrixObject<unsigned long int>  UnsLngMatrixObject;
typedef TmpNumMatrixObject<signed char>        ChrMatrixObject;
typedef TmpNumMatrixObject<signed short int>   ShtMatrixObject;
typedef TmpNumMatrixObject<signed int>         IntMatrixObject;
typedef TmpNumMatrixObject<signed long int>    LngMatrixObject;
typedef TmpNumMatrixObject<float>              FltMatrixObject;
typedef TmpNumMatrixObject<double>             DblMatrixObject;
typedef TmpNumMatrixObject<long double>        LngDblMatrixObject;

// Inline member functions TmpNumObject

//template <class T> inline TmpNumObject<T>::TmpNumObject(const T i = 0)
template <class T> inline TmpNumObject<T>::TmpNumObject(const T i) // to solve incompatibility with bcc55 compiler
{   data = i;
}

template <class T> inline TmpNumObject<T>::TmpNumObject(const TmpNumObject<T> &v)
{   data = v.data;
}

template <class T> inline TmpNumObject<T>::~TmpNumObject()
{
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::addData(const T v)
{   data += v;
    return *this;
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::addData(const TmpNumObject<T> &v)
{   data += v.data;
    return *this;
}

template <class T> inline T TmpNumObject<T>::getData() const
{   return data;
}

template <class T> inline long TmpNumObject<T>::getHashId() const
{   return (long)data;
}

template <class T> inline int TmpNumObject<T>::getClassId() const
{   return numObjectClass;
}

template <class T> inline char *TmpNumObject<T>::getClassName() const
{   return "NumObject";
}

template <class T> inline int TmpNumObject<T>::isEqual(const WmmObject &o) const
{   return (data == ((TmpNumObject<T> &)o).data);
}

template <class T> inline int TmpNumObject<T>::isSmaller(const WmmObject &o) const
{   return (data < ((TmpNumObject<T> &)o).data);
}

template <class T> inline TmpNumObject<T>::operator T() const
{   return data;
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::operator =(const T v)
{   data = v;
    return *this;
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::operator =(const TmpNumObject<T> &v)
{   data = v.data;
    return *this;
}

template <class T> inline void TmpNumObject<T>::setTo(ostream &o) const
{   o << data;
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::setData(const T v)
{   data = v;
    return *this;
}

template <class T> inline TmpNumObject<T> &TmpNumObject<T>::setData(const TmpNumObject<T> &v)
{   data = v.data;
    return *this;
}

// Inline member functions TmpNumArrayObject

template <class T> inline TmpNumArrayObject<T>::~TmpNumArrayObject()
{   delete[] dataPtr;
}

template <class T> inline TmpNumArrayObject<T> &TmpNumArrayObject<T>::addData(int i, const T v)
{   if (isWithinLimits(i))
    {   dataPtr[i] += v;
    }
    return *this;
}

template <class T> inline T TmpNumArrayObject<T>::getData(int i) const
{   if (!isWithinLimits(i))
    {   theOutObject.printError("Out of array limits");
    }
    return dataPtr[i];
}

template <class T> inline long TmpNumArrayObject<T>::getHashId() const
{   return (long)dataNumber;
}

template <class T> inline int TmpNumArrayObject<T>::getClassId() const
{   return intArrayObjectClass;
}

template <class T> inline char *TmpNumArrayObject<T>::getClassName() const
{   return "NumArrayObject";
}

template <class T> inline int TmpNumArrayObject<T>::getSize() const
{   return dataNumber;
}

template <class T> inline int TmpNumArrayObject<T>::isEqual(const WmmObject &o) const
{   return (dataNumber == ((TmpNumArrayObject<T> &)o).dataNumber);
}

template <class T> inline int TmpNumArrayObject<T>::isSmaller(const WmmObject &o) const
{   return (dataNumber < ((TmpNumArrayObject<T> &)o).dataNumber);
}

template <class T> inline int TmpNumArrayObject<T>::isWithinLimits(int i) const
{   return (i >= 0 && i < dataNumber) ? 1 : 0;
}

template <class T> inline T &TmpNumArrayObject<T>::operator [](int i)
{   if (!isWithinLimits(i))
    {   theOutObject.printError("Out of array limits");
    }
    return dataPtr[i];
}

template <class T> inline TmpNumArrayObject<T> &TmpNumArrayObject<T>::setData
  (int i, const T v)
{   if (isWithinLimits(i))
    {   dataPtr[i] = v;
    }
    return *this;
}

template <class T> TmpNumArrayObject<T>::TmpNumArrayObject(const int n)
{   dataNumber = n;
    dataPtr = new T[dataNumber];
    int i;
    for (i = 0; i < dataNumber; i++)
    {   dataPtr[i] = 0;
    }
}

template <class T> TmpNumArrayObject<T>::TmpNumArrayObject (const TmpNumArrayObject<T> &v)
{   dataNumber = v.dataNumber;
    dataPtr = new T[dataNumber];
    int i;
    for (i = 0; i < dataNumber; i++)
    {   dataPtr[i] = v.dataPtr[i];
    }
}

template <class T> TmpNumArrayObject<T> &TmpNumArrayObject<T>::operator =
  (const TmpNumArrayObject<T> &v)
{   if (*this != v)
    {   if (dataNumber != v.dataNumber)
        {   delete[] dataPtr;
            dataNumber = v.dataNumber;
            dataPtr = new T[dataNumber];
        }
        int i;
        for (i = 0; i < dataNumber; i++)
        {   dataPtr[i] = v.dataPtr[i];
        }
    }
    return *this;
}

template <class T> void TmpNumArrayObject<T>::setTo(ostream &o) const
{   int l;
    for (l = 0; l < dataNumber; l++)
    {   o << ((l == 0) ? "{" : "");
        o << dataPtr[l];
        o << ((l == dataNumber - 1) ? "}" : ",");
    }
}

template <class T> TmpNumArrayObject<T> &TmpNumArrayObject<T>::setRndOrder()
{   BtrConObject sBtree;
    sBtree.setOwner(0);
    int l;
    for (l = 0; l < dataNumber; l++)
    {   UnsLngObject *lPtr = new UnsLngObject(l);
        sBtree.set(*lPtr);
    }
    for (l = 0; l < dataNumber; l++)
    {   UnsLngObject *lPtr = &(UnsLngObject &)(sBtree[wmmRndLng(sBtree.getNumber())]);
        sBtree.reset(*lPtr);
        dataPtr[l] = T(lPtr->getData());
        delete lPtr;
    }
    return *this;
}

// Inline member functions TmpNumVectorObject

template <class T> inline TmpNumVectorObject<T>::TmpNumVectorObject(const int i) :
  TmpNumArrayObject<T>(i)
{
}

template <class T> inline TmpNumVectorObject<T>::~TmpNumVectorObject()
{
}

template <class T> inline T &TmpNumVectorObject<T>::operator ()(const int i)
{   return (*this)[i];
}

// Inline member functions TmpNumMatrixObject

template <class T> inline TmpNumMatrixObject<T>::TmpNumMatrixObject(const int i,
  const int j) : TmpNumArrayObject<T>(i * j), dataNumberRow(i), dataNumberColumn(j)
{
}

template <class T> inline TmpNumMatrixObject<T>::~TmpNumMatrixObject()
{
}

template <class T> inline T &TmpNumMatrixObject<T>::operator ()(const int i, const int j)
{   return (*this)[i * dataNumberColumn + j];
}

template <class T> void TmpNumMatrixObject<T>::setTo(ostream &o) const
{   int r, c;
    for (r = 0; r < dataNumberRow; r++)
    {   o << ((r == 0) ? "{" : "");
        for (c = 0; c < dataNumberColumn; c++)
        {   o << ((c == 0) ? "{" : "");
            o << TmpNumArrayObject<T>::dataPtr[r * dataNumberColumn + c];
            o << ((c == dataNumberColumn - 1) ? "}" : ",");
        }
        o << ((r == dataNumberRow - 1) ? "}" : ",");
    }
}

inline double max(double a, double b) {double m = a; return b > m ? b : m;}
inline double max(double a, double b, double c) {double m = a; m = b > m ? b : m; return c > m ? c : m;}
inline double max(double a, double b, double c, double d) {double m = a; m = b > m ? b : m; m = c > m ? c : m; return d > m ? d : m;}
inline double max(double a, double b, double c, double d, double e) {double m = a; m = b > m ? b : m; m = c > m ? c : m; m = d > m ? d : m; return e > m ? e : m;}
inline double max(double a, double b, double c, double d, double e, double f) {double m = a; m = b > m ? b : m; m = c > m ? c : m; m = d > m ? d : m; m = e > m ? e : m; return f > m ? f : m;}
inline double max(double a, double b, double c, double d, double e, double f, double g) {double m = a; m = b > m ? b : m; m = c > m ? c : m; m = d > m ? d : m; m = e > m ? e : m; m = f > m ? f : m; return g > m ? g : m;}
inline double max(double a, double b, double c, double d, double e, double f, double g, double h) {double m = a; m = b > m ? b : m; m = c > m ? c : m; m = d > m ? d : m; m = e > m ? e : m; m = f > m ? f : m; m = g > m ? g : m; return h > m ? h : m;}
inline double min(double a, double b) {double m = a; return b < m ? b : m;}
inline double min(double a, double b, double c) {double m = a; m = b < m ? b : m; return c < m ? c : m;}
inline double min(double a, double b, double c, double d) {double m = a; m = b < m ? b : m; m = c < m ? c : m; return d < m ? d : m;}
inline double min(double a, double b, double c, double d, double e) {double m = a; m = b < m ? b : m; m = c < m ? c : m; m = d < m ? d : m; return e < m ? e : m;}
inline double min(double a, double b, double c, double d, double e, double f) {double m = a; m = b < m ? b : m; m = c < m ? c : m; m = d < m ? d : m; m = e < m ? e : m; return f < m ? f : m;}
inline double min(double a, double b, double c, double d, double e, double f, double g) {double m = a; m = b < m ? b : m; m = c < m ? c : m; m = d < m ? d : m; m = e < m ? e : m; m = f < m ? f : m; return g < m ? g : m;}
inline double min(double a, double b, double c, double d, double e, double f, double g, double h) {double m = a; m = b < m ? b : m; m = c < m ? c : m; m = d < m ? d : m; m = e < m ? e : m; m = f < m ? f : m; m = g < m ? g : m; return h < m ? h : m;}

//inline float linIntPol(float tx, float t0, float t1, float x0, float x1)
//{   if (t0 == t1) theOutObject.printError("Division by zero in linIntPol");
//    return x0 + (x1 - x0)*(tx - t0)/(t1 - t0);
//}

inline double linIntPol(double tx, double t0, double t1, double x0, double x1)
{   if (t0 == t1) theOutObject.printError("Division by zero in linIntPol");
    return x0 + (x1 - x0)*(tx - t0)/(t1 - t0);
}

//inline void linSolve(float &x1, float &x2, float a11, float a12, float a21, float a22, float c1, float c2)
//{   float det = a11 * a22 - a12 * a21;
//    if (det == 0) theOutObject.printError("Determinant zero in linSolve");
//    x1 = (c1 * a22 - c2 * a12) / det;
//    x2 = (c2 * a11 - c1 * a21) / det;
//}

inline void linSolve(double &x1, double &x2, double a11, double a12, double a21, double a22, double c1, double c2)
{   double det = a11 * a22 - a12 * a21;
    if (det == 0) theOutObject.printError("Determinant zero in linSolve");
    x1 = (c1 * a22 - c2 * a12) / det;
    x2 = (c2 * a11 - c1 * a21) / det;
}

#endif


