/*------------------------------------------------------------------------*/
/*  Wmmomobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMMOMOBJ_H
#define __WMMOMOBJ_H

#include "wmwmmobj.h"

#define DAY_ZERO         0.0
#define DAYS_PER_YEAR    365.25
#define DAYS_PER_MONTH   30.4375
#define DAYS_PER_WEEK   (365.25/52.0)
#define DAYS_PER_DAY     1.0
#define DAYS_PER_HOUR   (1.0/24.0)
#define DAYS_PER_MINUTE (1.0/1440.0)
#define DAYS_PER_SECOND (1.0/86400.0)

#define PERIOD_02 0.5

#define FST_HALF 0 // Apr 1 0.00-0.50
#define SND_HALF 1 // Okt 1 0.50-1.00

#define PERIOD_04 0.25

#define FST_QUARTER 0 // Feb 14 0.00-0.25
#define SND_QUARTER 1 // May 16 0.25-0.50
#define TRD_QUARTER 2 // Aug 16 0.50-0.75
#define FTH_QUARTER 3 // Nov 15 0.75-1.00

#define PERIOD_12 (1.0/12.0)

#define JANUARI__  0 // Jan 15 0.0000-0.0833
#define FEBRUARI_  1 // Feb 14 0.0833-0.1667
#define MARCH____  2 // Mar 17 0.1667-0.2500
#define APRIL____  3 // Apr 16 0.2500-0.3333
#define MAY______  4 // May 16 0.3333-0.4167
#define JUNE_____  5 // Jun 16 0.4167-0.5000
#define JULY_____  6 // Jul 16 0.5000-0.5833
#define AUGUST___  7 // Aug 16 0.5833-0.6667
#define SEPTEMBER  8 // Sep 15 0.6667-0.7500
#define OCTOBER__  9 // Oct 15 0.7500-0.8333
#define NOVEMBER_ 10 // Nov 15 0.8333-0.9167
#define DECEMBER_ 11 // Dec 15 0.9167-1.0000

#define PERIOD_25 0.04

#define EARLY_JAN  0 // Jan  6 0.00-0.04
#define LATE__JAN  1 // Jan 20 0.04-0.08
#define EARLY_FEB  2 // Feb  4 0.08-0.12
#define LATE__FEB  3 // FEB 19 0.12-0.16
#define EARLY_MAR  4 // Mar  5 0.16-0.20
#define LATE__MAR  5 // Mar 20 0.20-0.24
#define EARLY_APR  6 // Apr  4 0.24-0.28
#define LATE__APR  7 // Apr 18 0.28-0.32
#define EARLY_MAY  8 // May  3 0.32-0.36
#define LATE__MAY  9 // May 18 0.36-0.40
#define EARLY_JUN 10 // Jun  1 0.40-0.44
#define LATE__JUN 11 // Jun 16 0.44-0.48
#define MIDSUMMER 12 // Jun 30 0.48-0.52
#define EARLY_JUL 13 // Jul 15 0.52-0.56
#define LATE__JUL 14 // Jul 30 0.56-0.60
#define EARLY_AUG 15 // Aug 13 0.60-0.64
#define LATE__AUG 16 // Aug 28 0.64-0.68
#define EARLY_SEP 17 // Sep 12 0.68-0.72
#define LATE__SEP 18 // Sep 26 0.72-0.76
#define EARLY_OCT 19 // Okt 11 0.76-0.80
#define LATE__OCT 20 // Okt 20 0.80-0.84
#define EARLY_NOV 21 // Nov  9 0.84-0.88
#define LATE__NOV 22 // Nov 24 0.88-0.92
#define EARLY_DEC 23 // Dec  9 0.92-0.96
#define LATE__DEC 24 // Dec 23 0.96-1.00

#define PERIOD_50 0.02

#define FST_WK_OF_JAN  0 // Jan  2 0.00-0.02
#define SND_WK_OF_JAN  1 // Jan  9 0.02-0.04
#define TRD_WK_OF_JAN  2 // Jan 17 0.04-0.06
#define FTH_WK_OF_JAN  3 // Jan 24 0.06-0.08
#define FST_WK_OF_FEB  4 // Jan 31 0.08-0.10
#define SND_WK_OF_FEB  5 // Feb  8 0.10-0.12
#define TRD_WK_OF_FEB  6 // Feb 15 0.12-0.14
#define FTH_WK_OF_FEB  7 // Feb 22 0.14-0.16
#define FST_WK_OF_MAR  8 // Mar  2 0.16-0.18
#define SND_WK_OF_MAR  9 // Mar  9 0.18-0.20
#define TRD_WK_OF_MAR 10 // Mar 16 0.20-0.22
#define FTH_WK_OF_MAR 11 // Mar 24 0.22-0.24
#define MID_OF_FT_HLF 12 // Mar 31 0.24-0.26
#define FST_WK_OF_APR 13 // Apr  7 0.26-0.28
#define SND_WK_OF_APR 14 // Apr 15 0.28-0.30
#define TRD_WK_OF_APR 15 // Apr 22 0.30-0.32
#define FTH_WK_OF_APR 16 // Apr 29 0.32-0.34
#define FST_WK_OF_MAY 17 // May  7 0.34-0.36
#define SND_WK_OF_MAY 18 // May 14 0.36-0.38
#define TRD_WK_OF_MAY 19 // May 21 0.38-0.40
#define FTH_WK_OF_MAY 20 // May 29 0.40-0.42
#define FST_WK_OF_JUN 21 // Jun  5 0.42-0.44
#define SND_WK_OF_JUN 22 // Jun 12 0.44-0.46
#define TRD_WK_OF_JUN 23 // Jun 20 0.46-0.48
#define FTH_WK_OF_JUN 24 // Jun 27 0.48-0.50
#define FST_WK_OF_JUL 25 // Jul  4 0.50-0.52
#define SND_WK_OF_JUL 26 // Jul 11 0.52-0.54
#define TRD_WK_OF_JUL 27 // Jul 19 0.54-0.56
#define FTH_WK_OF_JUL 28 // Jul 26 0.56-0.58
#define FST_WK_OF_AUG 29 // Aug  2 0.58-0.60
#define SND_WK_OF_AUG 30 // Aug 10 0.60-0.62
#define TRD_WK_OF_AUG 31 // Aug 17 0.62-0.64
#define FTH_WK_OF_AUG 32 // Aug 24 0.64-0.66
#define FST_WK_OF_SEP 33 // Sep  1 0.66-0.68
#define SND_WK_OF_SEP 34 // Sep  8 0.68-0.70
#define TRD_WK_OF_SEP 35 // Sep 15 0.70-0.72
#define FTH_WK_OF_SEP 36 // Sep 23 0.72-0.74
#define MID_OF_SD_HLF 37 // Sep 30 0.74-0.76
#define FST_WK_OF_OCT 38 // Okt  7 0.76-0.78
#define SND_WK_OF_OCT 39 // Okt 15 0.78-0.80
#define TRD_WK_OF_OCT 40 // Okt 22 0.80-0.82
#define FTH_WK_OF_OCT 41 // Okt 29 0.82-0.84
#define FST_WK_OF_NOV 42 // Nov  6 0.84-0.86
#define SND_WK_OF_NOV 43 // Nov 13 0.86-0.88
#define TRD_WK_OF_NOV 44 // Nov 20 0.88-0.90
#define FTH_WK_OF_NOV 45 // Nov 28 0.90-0.92
#define FST_WK_OF_DEC 46 // Dec  5 0.92-0.94
#define SND_WK_OF_DEC 47 // Dec 12 0.94-0.96
#define TRD_WK_OF_DEC 48 // Dec 20 0.96-0.98
#define FTH_WK_OF_DEC 49 // Dec 27 0.98-1.00

// Class definitions

class BasicDate : public SrtObject
{ public :
                           BasicDate(long d = 1);
                           BasicDate(int y, int m = 0, int d = 1);
                           BasicDate(const char *dPtr);
                           BasicDate(const BasicDate &d);
    virtual               ~BasicDate();
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
            int            getDay() const;
            char          *getDayNamePtr() const;
    virtual long           getHashId() const;
            long           getLotusDate() const;
            int            getMonth() const;
            char          *getMonthNamePtr() const;
            int            getYear() const;
            int            getYearDay() const;
            int            isBissextile() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator long() const;
            BasicDate     &operator =(BasicDate &d);
            void           printDateDD_MM_YY(ostream &o) const;
            void           printDateDD_MM_YYYY(ostream &o) const;
            void           printDateDD_MMM_YY(ostream &o) const;
            void           printDateDD_MMM_YYYY(ostream &o) const;
            void           printDateFull(ostream &o) const;
    virtual void           setTo(ostream &o) const;
            BasicDate     &setDate(int y, int m, int d);
            BasicDate     &setDay(int d);
            BasicDate     &setLotusDate(long d);
            BasicDate     &setMonth(int m);
            BasicDate     &setYear(int y);
            BasicDate     &setYearDay(int d);
  protected :
            long           date;
};

class BasicTime : public SrtObject
{ public :
                           BasicTime(float t = 0.0);
                           BasicTime(int ho, int m = 0, int s = 0, int hu = 0);
                           BasicTime(const char *tPtr);
                           BasicTime(const BasicTime &t);
    virtual               ~BasicTime();
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
            int            getHour() const;
            int            getHundreths() const;
            float          getLotusTime() const;
            int            getMinute() const;
            int            getSecond() const;
    virtual long           getHashId() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator float() const;
            BasicTime     &operator=(BasicTime &t);
    virtual void           setTo(ostream &o) const;
            void           printTimeHH_MM(ostream &o) const;
            void           printTimeHH_MM_SS(ostream &o) const;
            void           printTimeFull(ostream &o) const;
            BasicTime     &setHour(int h);
            BasicTime     &setHundreths(int h);
            BasicTime     &setLotusTime(float t);
            BasicTime     &setMinute(int m);
            BasicTime     &setSecond(int s);
            BasicTime     &setTime(int ho, int m = 0, int s = 0, int hu = 0);
  protected :
            float          time;
};

class BasicMoment : public BasicDate, public BasicTime
{ public :
                           BasicMoment(double m = 1.0);
                           BasicMoment(long d, float t = 0.0);
                           BasicMoment(int y     , int mo = 0, int d = 1,
                                    int ho = 0, int mi = 0, int s = 0,
                                    int hu = 0);
                           BasicMoment(const char *dPtr, const char *tPtr);
                           BasicMoment(const BasicMoment &c);
    virtual               ~BasicMoment();
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
    virtual long           getHashId() const;
            double         getMoment() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator double() const;
            BasicMoment   &operator=(BasicMoment &m);
    virtual void           setTo(ostream &o) const;
            void           printMomentFull(ostream &o) const;
            BasicMoment   &setLotusMoment(double m);
            BasicMoment   &setLotusMoment(long d, float t);
            BasicMoment   &setMoment(int y     , int mo = 0, int d = 1,
                                     int ho = 0, int mi = 0, int s = 0,
                                     int hu = 0);
};

// Inline member functions BasicDate

inline BasicDate::BasicDate(long d)
{   date = d;
}

inline BasicDate::BasicDate(int y, int mo, int d)
{   theOutObject.printError("BasicDate::BasicDate(int y, int mo, int d) not yet implemented");
}

inline BasicDate::BasicDate(const BasicDate &d)
{   date = d.date;
}

inline BasicDate::~BasicDate()
{
}

inline int BasicDate::getClassId() const
{   return basicDateClass;
}

inline char *BasicDate::getClassName() const
{   return "BasicDate";
}

inline long BasicDate::getHashId() const
{   return (long)date;
}

inline long BasicDate::getLotusDate() const
{   return date;
}

inline int BasicDate::getYear() const
{   return (int)((getLotusDate() - 1) / 365.25) + 1900;
}

inline int BasicDate::getYearDay() const
{   return (int)date - (int)(365.25 * (getYear() - 1900) + 0.75);
}

inline int BasicDate::isBissextile() const
{   //if (year() % 4 != 0 || (year() % 100 == 0 && year() % 400 != 0))
    if (getYear() % 4 != 0) // Making lotus mistake about 1900
    {  return 0;
    }
    else
    {  return 1;
    }
}

inline int BasicDate::isEqual(const WmmObject &o) const
{   return date == ((BasicDate &)o).date;
}

inline int BasicDate::isSmaller(const WmmObject &o) const
{   return date < ((BasicDate &)o).date;
}

inline BasicDate::operator long() const
{   return date;
}

inline BasicDate &BasicDate::operator =(BasicDate &d)
{   date = d.date;
    return d;
}

inline void BasicDate::printDateDD_MM_YY(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getDay()           << "/"
      << setw(2)      << getMonth()         << "/"
      << setw(2)      << (getYear() - 1900) << setfill(' ');
}

inline void BasicDate::printDateDD_MM_YYYY(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getDay()          << "/"
      << setw(2)      << getMonth()        << "/"
      << setw(4)      << getYear()         << setfill(' ');
}

inline void BasicDate::printDateDD_MMM_YY(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getDay()           << "-"
                      << getMonthNamePtr()  << "-"
      << setw(2)      << (getYear() - 1900) << setfill(' ');
}

inline void BasicDate::printDateDD_MMM_YYYY(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getDay()          << "-"
                      << getMonthNamePtr() << "-"
      << setw(4)      << getYear()         << setfill(' ');
}

inline void BasicDate::printDateFull(ostream &o) const
{   o << setfill('0') << getDayNamePtr()   << " "
      << setw(2)      << getDay()          << "-"
                      << getMonthNamePtr() << "-"
      << setw(4)      << getYear()         << setfill(' ');
}

inline void BasicDate::setTo(ostream &o) const
{   printDateDD_MMM_YYYY(o);
}

inline BasicDate &BasicDate::setDate(int y, int m, int d)
{   theOutObject.printError("BasicDate::setDate(int y, int mo, int d) not yet implemented");
    return *this;
}

inline BasicDate &BasicDate::setDay(int d)
{   theOutObject.printError("BasicDate::setDay(int d) not yet implemented");
    return *this;
}

inline BasicDate &BasicDate::setLotusDate(long d)
{   date = d;
    return *this;
}

inline BasicDate &BasicDate::setMonth(int m)
{   theOutObject.printError("BasicDate::setMonth(int m) not yet implemented");
    return *this;
}

inline BasicDate &BasicDate::setYear(int y)
{   theOutObject.printError("BasicDate::setYear(int y) not yet implemented");
    return *this;
}

inline BasicDate &BasicDate::setYearDay(int d)
{   theOutObject.printError("BasicDate::setYearDay(int d) not yet implemented");
    return *this;
}

// Inline member functions BasicTime

inline BasicTime::BasicTime(float t)
{   time = t;
}

inline BasicTime::BasicTime(int ho, int m, int s, int hu)
{   time = ho/24.0 + m/1440.0 + s/86400.0 + hu/8640000.0;
}

inline BasicTime::BasicTime(const char *tPtr)
{   theOutObject.printError("BasicTime::BasicTime(const char *tPtr) not yet implemented");
}

inline BasicTime::BasicTime(const BasicTime &d)
{   time = d.time;
}

inline BasicTime::~BasicTime()
{
}

inline int BasicTime::getClassId() const
{   return basicTimeClass;
}

inline char *BasicTime::getClassName() const
{   return "BasicTime";
}

inline long BasicTime::getHashId() const
{   return (long)time;
}

inline int BasicTime::getHour() const
{   return (int)(time * 24);
}

inline int BasicTime::getHundreths() const
{   theOutObject.printError("BasicTime::getHundreths() not yet implemented");
    return 0;
}

inline float BasicTime::getLotusTime() const
{   return time;
}

inline int BasicTime::getMinute() const
{   return (int)((time * 24 - getHour()) * 60);
}

inline int BasicTime::getSecond() const
{   return (int)(((time * 24 - getHour()) * 60 - getMinute()) * 60);
}

inline int BasicTime::isEqual(const WmmObject &o) const
{   return time == ((BasicTime &)o).time;
}

inline int BasicTime::isSmaller(const WmmObject &o) const
{   return time < ((BasicTime &)o).time;
}

inline BasicTime::operator float() const
{   return time;
}

inline BasicTime &BasicTime::operator =(BasicTime &t)
{   time = t.time;
    return t;
}

inline void BasicTime::printTimeHH_MM(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getHour()         << ":"
      << setw(2)      << getMinute()       << setfill(' ');
}

inline void BasicTime::setTo(ostream &o) const
{   printTimeHH_MM(o);
}

inline void BasicTime::printTimeHH_MM_SS(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getHour()         << ":"
      << setw(2)      << getMinute()       << ":"
      << setw(2)      << getSecond()       << setfill(' ');
}

inline void BasicTime::printTimeFull(ostream &o) const
{   o << setfill('0')
      << setw(2)      << getHour()         << ":"
      << setw(2)      << getMinute()       << ":"
      << setw(2)      << getSecond()       << "."
      << setw(2)      << getHundreths()    << setfill(' ');
}

inline BasicTime &BasicTime::setHour(int h)
{   theOutObject.printError("BasicTime::setHour(int h) not yet implemented");
    return *this;
}

inline BasicTime &BasicTime::setHundreths(int h)
{   theOutObject.printError("BasicTime::setHundreths(int h) not yet implemented");
    return *this;
}

inline BasicTime &BasicTime::setLotusTime(float t)
{   time;
    return *this;
}

inline BasicTime &BasicTime::setMinute(int m)
{   theOutObject.printError("BasicTime::setMinute(int m) not yet implemented");
    return *this;
}

inline BasicTime &BasicTime::setSecond(int s)
{   theOutObject.printError("BasicTime::setSecond(int s) not yet implemented");
    return *this;
}

inline BasicTime &BasicTime::setTime(int ho, int m, int s, int hu)
{   time = ho/24.0 + m/1440.0 + s/86400.0 + hu/8640000.0;
    return *this;
}

// Inline member functions BasicMoment

inline BasicMoment::BasicMoment(double m) : BasicDate((long)m), BasicTime((float)m)
{
}

inline BasicMoment::BasicMoment(long d, float t) : BasicDate(d), BasicTime(t)
{
}

inline BasicMoment::BasicMoment(int y , int mo, int d,
                          int ho, int mi, int s, int hu) :
                          BasicDate(y, mo, d), BasicTime(ho, mi, s, hu)
{
}
inline BasicMoment::BasicMoment(const char *dPtr, const char *tPtr) :
                          BasicDate(dPtr), BasicTime(tPtr)
{
}

inline BasicMoment::BasicMoment(const BasicMoment &m) :
                          BasicDate(m), BasicTime(m)
{
}

inline BasicMoment::~BasicMoment()
{
}

inline int BasicMoment::getClassId() const
{   return basicMomentClass;
}

inline char *BasicMoment::getClassName() const
{   return "BasicMoment";
}

inline long BasicMoment::getHashId() const
{   return (long)date;
}

inline double BasicMoment::getMoment() const
{   return (double)date + (double)time;
}

inline int BasicMoment::isEqual(const WmmObject &o) const
{   return date == ((BasicMoment &)(BasicDate &)o).date && time == ((BasicMoment &)(BasicDate &)o).time;
}

inline int BasicMoment::isSmaller(const WmmObject &o) const
{   if (date != ((BasicMoment &)(BasicDate &)o).date)
    {   return date < ((BasicMoment &)(BasicDate &)o).date;
    }
    else
    {   return time < ((BasicMoment &)(BasicDate &)o).time;
    }
}

inline BasicMoment::operator double() const
{   return (double)date + (double)time;
}

inline BasicMoment &BasicMoment::operator =(BasicMoment &m)
{   date = m.date;
    time = m.time;
    return m;
}

inline void BasicMoment::setTo(ostream &o) const
{   printDateDD_MMM_YYYY(o);
    o << " ";
    printTimeHH_MM(o);
}

inline void BasicMoment::printMomentFull(ostream &o) const
{   printDateFull(o);
    o << " ";
    printTimeFull(o);
}


inline BasicMoment &BasicMoment::setLotusMoment(double m)
{   date = (long)m;
    time = (float)m;
    return *this;
}

inline BasicMoment &BasicMoment::setLotusMoment(long d, float t)
{   date = d;
    time = t;
    return *this;
}

#endif


