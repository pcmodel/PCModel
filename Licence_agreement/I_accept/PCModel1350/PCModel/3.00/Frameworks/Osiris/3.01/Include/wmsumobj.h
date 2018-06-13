/*------------------------------------------------------------------------*/
/*  Wmsumobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluiskenslaan 21                                                      */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMSUMOBJ_H
#define __WMSUMOBJ_H

#include "wmwmmobj.h"

// Class definitions

class SumObject : public SrtObject
{ public :
                           SumObject();
                           SumObject(const SumObject &s);
                          ~SumObject();
            SumObject     &addX(double x = 0.0, long f = 1);
            SumObject     &addXY(double x = 0.0, double y = 0.0, long f = 1);
            SumObject     &flush();
            double         getAvgX() const;
            double         getAvgY() const;
            double         geta() const;
            double         getaInv() const;
            double         getb() const;
            double         getbInv() const;
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
            double         getCnfLim() const;
            double         getCV() const;
    virtual long           getHashId() const;
            double         getMinX() const;
            double         getMaxX() const;
            double         getMinY() const;
            double         getMaxY() const;
            long           getn() const;
            double         getp() const;
            double         getr() const;
            double         getr2() const;
            double         getSgmX() const;
            double         getSgmY() const;
            double         getSgm2X() const;
            double         getSgm2Y() const;
            double         getSprXY() const;
            double         getSsqExp() const;
            double         getSsqUnexp() const;
            double         getSsqX() const;
            double         getSsqY() const;
            double         getStdX() const;
            double         getStdY() const;
            double         getSteX() const;
            double         getSteY() const;
            double         getSumX() const;
            double         getSumX2() const;
            double         getSumXY() const;
            double         getSumY() const;
            double         getSumY2() const;
            double         getVarX() const;
            double         getVarY() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
            SumObject     &operator =(const SumObject &s);
    virtual void           setTo(ostream &os) const;
    static  float          student0_050[34];
    static  float          student0_010[34];
    static  float          student0_001[34];
  private :
            long           n;
            double         minX;
            double         maxX;
            double         sumX;
            double         sumX2;
            double         minY;
            double         maxY;
            double         sumY;
            double         sumY2;
            double         sumXY;
            int            XY;
};

inline SumObject::SumObject() : n(0), minX(1.0e100), maxX(-1.0e100), sumX(0.0),
  minY(1.0e100), maxY(-1.0e100), sumY(0.0), sumX2(0.0), sumY2(0.0), sumXY(0.0), XY(0)
{
}

inline SumObject::SumObject(const SumObject &s) : n(s.n), minX(s.minX),
  maxX(s.maxX), sumX(s.sumX), minY(s.minY), maxY(s.maxY), sumY(s.sumY), sumX2(s.sumX2), sumY2(s.sumY2),
  sumXY(s.sumXY)
{
}

inline SumObject::~SumObject()
{
}

inline double SumObject::getSsqX() const
{   return (n ? (sumX2 - sumX * sumX / n >= 0.0 ? sumX2 - sumX * sumX / n : 0.0) : -999.0);
}

inline double SumObject::getSsqY() const
{   return (n ? (sumY2 - sumY * sumY / n >= 0.0 ? sumY2 - sumY * sumY / n : 0.0) : -999.0);
}

inline double SumObject::getSprXY() const
{   return (n ? sumXY - sumX * sumY / n : -999.0);
}

inline SumObject &SumObject::addX(double x, long f)
{   if (!XY)
    {   XY = 1;
    }
    if (XY != 1)
    {   theOutObject.printError("Use this SumObject for single data");
    }
    if (f < 0)
    {   theOutObject.printError("Negative number of observations in SumObject");
    }
    n += f;
    sumX  += f * x;
    sumX2 += f * x * x;
    if (x < minX)
    {   minX = x;
    }
    if (x > maxX)
    {   maxX = x;
    }
    return *this;
}

inline SumObject &SumObject::addXY(double x, double y, long f)
{   if (!XY)
    {   XY = 2;
    }
    if (XY != 2)
    {   theOutObject.printError("Use this SumObject for paired data");
    }
    if (f < 0)
    {   theOutObject.printError("Negative number of observations in SumObject");
    }
    n += f;
    sumX += f * x;
    sumX2 += f * x * x;
    if (x < minX)
    {   minX = x;
    }
    if (x > maxX)
    {   maxX = x;
    }
    if (y < minY)
    {   minY = y;
    }
    if (y > maxY)
    {   maxY = y;
    }
    sumXY += f * x * y;
    sumY += f * y;
    sumY2 += f * y * y;
    return *this;
}

inline SumObject &SumObject::flush()
{   n = 0;
    minX = 1.0e100;
    maxX = -1.0e100;
    sumX = 0.0;
    minY = 1.0e100;
    maxY = -1.0e100;
    sumY = 0.0;
    sumX2 = 0.0;
    sumY2 = 0.0;
    sumXY = 0.0;
    XY = 0;
    return *this;
}

inline double SumObject::getAvgX() const
{   return (n ? sumX / (double)n : -999.0);
}

inline double SumObject::getAvgY() const
{   return (n ? sumY / (double)n : -999.0);
}

inline double SumObject::geta() const
{   return (n > 1 && getSsqX() ? getAvgY() - getAvgX() * getSprXY() / getSsqX() : -999.0);
}

inline double SumObject::getaInv() const
{   return (n > 1 && getSsqY() ? getAvgX() - getAvgY() * getSprXY() / getSsqY() : -999.0);
}

inline double SumObject::getb() const
{   return (n > 1 && getSsqX() ? getSprXY() / getSsqX() : -999.0);
}

inline double SumObject::getbInv() const
{   return (n > 1 && getSsqY() ? getSprXY() / getSsqY() : -999.0);
}

inline int SumObject::getClassId() const
{   return sumObjectClass;
}

inline char *SumObject::getClassName() const
{   return "SumObject";
}

inline long SumObject::getHashId() const
{   return 0;
}

inline double SumObject::getMinX() const
{   return minX;
}

inline double SumObject::getMinY() const
{   return minY;
}

inline double SumObject::getMaxX() const
{   return maxX;
}

inline double SumObject::getMaxY() const
{   return maxY;
}

inline long SumObject::getn() const
{   return n;
}

inline double SumObject::getp() const
{   return -1.0;
}

inline double SumObject::getr() const
{   return (n > 1 && getSsqX() * getSsqY() > 0 ? getSprXY() / sqrt(getSsqX() * getSsqY()) : -999.0);
}

inline double SumObject::getr2() const
{   return (n > 1 && getSsqX() * getSsqY() > 0 ? getSprXY() * getSprXY() / (getSsqX() * getSsqY()) : -999.0);
}

inline double SumObject::getSgmX() const
{   return (n > 1 && getSsqX() / n >= 0.0 ? sqrt(getSsqX() / n) : -999.0);
}

inline double SumObject::getSgmY() const
{   return (n > 1 && getSsqY() / n >= 0.0 ? sqrt(getSsqY() / n) : -999.0);
}

inline double SumObject::getSgm2X() const
{   return (n > 1 ? getSsqX() / n : -999.0);
}

inline double SumObject::getSgm2Y() const
{   return (n > 1 ? getSsqY() / n : -999.0);
}

inline double SumObject::getSsqExp() const
{   return (n > 1 && getSsqX() > 0.0 ? getSprXY() * getSprXY() / getSsqX() : -999.0);
}

inline double SumObject::getSsqUnexp() const
{   return (n > 1 && getSsqX() > 0.0 ? getSsqY() - getSprXY() * getSprXY() / getSsqX() : -999.0);
}

inline double SumObject::getStdX() const
{   return (n > 1 && getSsqX() / (n - 1) >= 0.0 ? sqrt(getSsqX() / (n - 1)) : -999.0);
}

inline double SumObject::getStdY() const
{   return (n > 1 && getSsqY() / (n - 1) >= 0.0 ? sqrt(getSsqY() / (n - 1)) : -999.0);
}

inline double SumObject::getSteX() const
{   return (n > 1 && getSsqX() / ((n - 1) * n) >= 0.0 ? sqrt(getSsqX() / ((n - 1) * n)) : -999.0);
}

inline double SumObject::getSteY() const
{   return (n > 1 && getSsqY() / ((n - 1) * n) >= 0.0 ? sqrt(getSsqY() / ((n - 1) * n)) : -999.0);
}

inline double SumObject::getSumX() const
{   return sumX;
}

inline double SumObject::getSumX2() const
{   return sumX2;
}

inline double SumObject::getSumXY() const
{   return sumXY;
}

inline double SumObject::getSumY() const
{   return sumY;
}

inline double SumObject::getSumY2() const
{   return sumY2;
}

inline double SumObject::getVarX() const
{   return (n > 1 ? getSsqX() / (n - 1) : -999.0);
}

inline double SumObject::getVarY() const
{   return (n > 1 ? getSsqY() / (n - 1) : -999.0);
}

inline int SumObject::isEqual(const WmmObject &o) const
{   return (int)n;
}

inline int SumObject::isSmaller(const WmmObject &o) const
{   return n < ((SumObject &)o).n;
}

inline SumObject &SumObject::operator =(const SumObject &s)
{   n = s.n;
    minX = s.minX;
    maxX = s.maxX;
    sumX = s.sumX;
    minY = s.minY;
    maxY = s.maxY;
    sumY = s.sumY;
    sumX2 = s.sumX2;
    sumY2 = s.sumY2;
    sumXY = s.sumXY;
    return *this;
}

inline void SumObject::setTo(ostream &o) const
{   o << getClassName();
}

#endif


