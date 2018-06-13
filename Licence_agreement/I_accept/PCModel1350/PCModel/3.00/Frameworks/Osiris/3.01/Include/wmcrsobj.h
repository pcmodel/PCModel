/*------------------------------------------------------------------------*/
/*  Wmcrsobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluiskenslaan 21                                                      */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMCRSOBJ_H
#define __WMCRSOBJ_H

#include "wmwmmobj.h"
#include "wmrndobj.h"
#include "wmsumobj.h"

// Class definitions

class CrsObject
{ public :
    CrsObject(int d, double *l, double *u, double *b, double (*fp)(double *p), ostream &o);
    ~CrsObject();
    int optimize(double maxRelativeFit, long maxIteration, long printInterval);
    int confidence(double maxRelativeFit, long maxIteration, long printInterval);
  private :
    int maxDim;
    int maxSet;
    int maxTrial;
    double *initialLower;
    double *initialUpper;
    double *ultimateLower;
    double *ultimateUpper;
    double *set;
    double *fitSet;
    double *trial;
    double fitTrial;
    double *best;
    int indexBest;
    int indexWorst;
    double *min;
    double *max;
    double (*fitPtr)(double *p);
    ostream &outFile;
    void setBest();
    void setSet();
    void setTrial();
    void setTrialToWorst();
    void setTrialToRandom();
    void printIterationHeader();
    void printIteration(long i, int c);
    void printIterationFooter();
    void printSet();
    void printCorrelationMatrix();
};

inline CrsObject::CrsObject(int d, double *l, double *u, double *b, double (*fp)(double *p), ostream &o) :
  maxDim(d), maxSet(d * 100), maxTrial(d), initialLower(l), initialUpper(u), ultimateLower(l),
  ultimateUpper(u), fitTrial(0.0), best(b), indexBest(0), indexWorst(0), fitPtr(fp), outFile(o)
{   set = new double[maxDim*maxSet];
    fitSet = new double[maxSet];
    trial = new double[maxDim];
    min = new double[maxDim];
    max = new double[maxDim];
    setSet();
}

inline CrsObject::~CrsObject()
{   if (set) delete[] set;
    if (fitSet) delete[] fitSet;
    if (trial) delete[] trial;
    if (min) delete[] min;
    if (max) delete[] max;
}

#endif
