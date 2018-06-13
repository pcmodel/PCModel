/*------------------------------------------------------------------------*/
/*  Wmranobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMRANOBJ_H
#define __WMRANOBJ_H

#include "wmwmmobj.h"

// Inline global functions

inline long   cppSetSeed(long s);
inline double cppRnd();
inline double cppRndNrm (double avg, double std);
#ifdef __WMM_MPP
       long   mppSetSeed(long s);
       double mppRnd();
       double mppRndNrm(double avg, double std);
#endif
#ifdef __WMM_NRC
       long   nrcSetSeed1(long s);
       long   nrcSetSeed2(long s);
       float  nrcRnd1();
       float  nrcRnd2();
       float  nrcRnd1Nrm(float avg, float std);
       float  nrcGammLn(float xx);
       float  nrcFacrl(int n);
       float  nrcFactLn(int n);
       float  nrcBico(int n, int k);
       float  nrcBeta(float z, float w);
       float  nrcGamDev(int ia);
       float  nrcPoiDev(float xm);
       float  nrcBnlDev(float pp, int n);
#endif
inline long   wmmSetSeed(long s);
inline double wmmRnd();
inline double wmmRndDbl(double max);
inline double wmmRndDbl(double min, double max);
inline int    wmmRndInt(int max);
inline int    wmmRndInt(int min, int max);
inline long   wmmRndLng(long max);
inline long   wmmRndLng(long min, long max);
inline double wmmRndNrm (double avg, double std);
inline double wmmRndLogNrm (double avg, double std);
       void   wmmRndCor (double &x, double &y, double mx, double sx, double my, double sy, double r);
inline double wmmRndExpDbl(double f);
inline long   wmmRndExpLng(double f, long max);

inline long cppSetSeed(long s)
{   return s;
}

inline double cppRnd()
{   return ((double)rand() + 0.5)/((double)RAND_MAX + 1.0);
}

inline double cppRndNrm (double avg = 0.0, double std = 0.0)
{   return avg + std * sqrt(-2 * log(cppRnd())) * cos(2 * M_PI * cppRnd());
}

inline long wmmSetSeed(long s)
#ifdef __WMM_NRC
{   return nrcSetSeed1(s);
#else
{   return cppSetSeed(s);
#endif
}

inline double wmmRnd()
#ifdef __WMM_NRC
{   return nrcRnd1();
#else
{   return cppRnd();
#endif
}

inline double wmmRndDbl(double max)
{   return max * wmmRnd();
}

inline double wmmRndDbl(double min, double max)
{   return min + (max - min) * wmmRnd();
}

inline int wmmRndInt(int max)
{   int rnd = (int)(max * wmmRnd());
    if (rnd >= max || rnd < 0)
    {   OutObject::printError("wmmRndInt: This should never happen!");
    }
    return rnd;
}

inline long wmmRndInt(long min, long max)
{   return min + wmmRndLng(max - min);
}

inline long wmmRndLng(long max)
{   long rnd = (long)(max * wmmRnd());
    if (rnd >= max || rnd < 0)
    {   OutObject::printError("wmmRndLng: This should never happen!");
    }
    return rnd;
}

inline long wmmRndLng(long min, long max)
{   return min + wmmRndLng(max - min);
}

inline double wmmRndNrm (double avg = 0.0, double std = 1.0)
#ifdef __WMM_NRC
{   return nrcRnd1Nrm(avg, std);
#else
{   return cppRndNrm(avg, std);
#endif
}

inline double wmmRndLogNrm (double avg = 0.0, double std = 1.0)
{   return exp(wmmRndNrm(avg, std));
}

inline double wmmRndExpDbl(double f)
{   if (fabs(f) > 0.001)
    {   return log(wmmRnd() * (exp(f) - 1) + 1) / f;
    }
    else
    {   return wmmRnd();
    }
}

inline long wmmRndExpLng(double f, long max)
{   return long(wmmRndExpDbl(f) * max);
}

class WmmRandom
{ public :
  private :
};

class Coin : public WmmRandom
{ public :
    const char *getString() {return (wmmRnd() < 0.5 ? "Head!" : "Tail!");}
    int getValue() {return (wmmRnd() < 0.5 ? 1 : 0);}
  private :
};

class Dice : public WmmRandom
{ public :
    int getValue() {return (int)wmmRndLng(6) + 1;}
  private :
};

class Normal : public WmmRandom
{ public :
    Normal(float a = 0.0, float s = 1.0) {avg = a; std = s;}
    Normal(Normal &n) {avg = n.avg; std = n.std;}
    float getValue() {return wmmRndNrm(avg, std);}
  private :
    float avg;
    float std;
};

class LogNormal : public WmmRandom
{ public :
    LogNormal(float a = 0.0, float s = 1.0) {avg = a; std = s;}
    LogNormal(LogNormal &n) {avg = n.avg; std = n.std;}
    float getValue() {return wmmRndLogNrm(avg, std);}
  private :
    float avg;
    float std;
};

class UniformFlt : public WmmRandom
{ public :
    UniformFlt(float m = 0.0, float max = 1.0) {min = m; rng = max - m;}
    UniformFlt(UniformFlt &u) {min = u.min; rng = u.rng;}
    float getValue() {return min + wmmRndDbl(rng);}
  private :
    float min;
    float rng;
};

class UniformLng : public WmmRandom
{ public :
    UniformLng(long m = 0, long max = 1) {min = m; rng = max - m;}
    UniformLng(UniformLng &u) {min = u.min; rng = u.rng;}
    long getValue() {return min + wmmRndLng(rng);}
  private :
    long min;
    long rng;
};

#ifdef __WMM_NRC
class Poison : public WmmRandom
{ public :
    Poison(float a = 1.0) {avg = a;}
    Poison(Poison &p) {avg = p.avg;}
    float getValue() {return nrcPoiDev(avg);}
  private :
    float avg;
};

class Binomial : public WmmRandom
{ public :
    Binomial(float a = 0.0, int k = 1) {p = a / (float)k; n = k;}
    Binomial(Binomial &b) {p = b.p; n = b.n;}
    float getValue() {return nrcBnlDev(p, n);}
  private :
    float p;
    int n;
};
#endif

#endif


