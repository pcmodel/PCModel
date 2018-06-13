/*------------------------------------------------------------------------*/
/*  Wmodeobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMODEOBJ_H
#define __WMODEOBJ_H

#include "wmwmmobj.h"

void eulerFlt(float &x, float s, float y[], int n, float p[],
  void (*derivatives)(float x, float y[], float p[], float dydx[]));

void eulerFlt(float &x, float s, float *yPtr[], int n, float p[],
  void (*derivatives)(float x, float y[], float p[], float dydx[]));

void eulerDbl(double &x, double s, double y[], int n, double p[],
  void (*derivatives)(double x, double y[], double p[], double dydx[]));

void eulerDbl(double &x, double s, double *yPtr[], int n, double p[],
  void (*derivatives)(double x, double y[], double p[], double dydx[]));

void eulerFlt(float &x, float s, float y[], int n, float p[], float e[], float d[],
  void (*derivatives)(float x, float y[], float p[], float e[], float dydx[]));

void eulerDbl(double &x, double s, double y[], int n, double p[], double e[], double d[],
  void (*derivatives)(double x, double y[], double p[], double e[], double dydx[]));

void rk4Flt(float &x, float s, float y[], int n, float p[],
  void (*derivatives)(float x, float y[], float p[], float dydx[]));

void rk4Flt(float &x, float s, float *yPtr[], int n, float p[],
  void (*derivatives)(float x, float y[], float p[], float dydx[]));

void rk4Dbl(double &x, double s, double y[], int n, double p[],
  void (*derivatives)(double x, double y[], double p[], double dydx[]));

void rk4Dbl(double &x, double s, double *yPtr[], int n, double p[],
  void (*derivatives)(double x, double y[], double p[], double dydx[]));

void rk4Flt(float &x, float s, float y[], int n, float p[], float e[], float d[],
  void (*derivatives)(float x, float y[], float p[], float e[], float dydx[]));

void rk4Dbl(double &x, double s, double y[], int n, double p[], double e[], double d[],
  void (*derivatives)(double x, double y[], double p[], double e[], double dydx[]));

void odeintFlt(float dxsav, float *x, float **y, int &kmax, int &kount,
    float ystart[], int nvar, float p[], float a[], float x1, float x2, float eps, float h1,
	float hmin, int &nok, int &nbad,
	void (*derivs)(float x, float y[], float p[], float a[], float dydx[]));
// function that takes the integration from x1 to x2

void odeintDbl(double dxsav, double *x, double **y, int &kmax, int &kount,
    double ystart[], int nvar, double p[], double a[], double x1, double x2, double eps, double h1,
	double hmin, int &nok, int &nbad,
	void (*derivs)(double x, double y[], double p[], double a[], double dydx[]));
// function that takes the integration from x1 to x2

void rkqsFlt(float y[], float p[], float a[], float dydx[], int n, float &x, float htry, float eps,
	float yscal[], float &hdid, float &hnext,
    void (*derivs)(float x, float y[], float p[], float a[], float dydx[]));
// function that takes the integration over one time step

void rkqsDbl(double y[], double p[], double a[], double dydx[], int n, double &x, double htry, double eps,
	double yscal[], double &hdid, double &hnext,
    void (*derivs)(double x, double y[], double p[], double a[], double dydx[]));
// function that takes the integration over one time step

void rkckFlt(float y[], float p[], float a[], float dydx[], int n, float x, float h, float yout[],
    float yerr[], void (*derivs)(float x, float y[], float p[], float a[], float dydx[]));
// function that calculates the derivatives

void rkckDbl(double y[], double p[], double a[], double dydx[], int n, double x, double h, double yout[],
    double yerr[], void (*derivs)(double x, double y[], double p[], double a[], double dydx[]));
// function that calculates the derivatives

void derivsFlt(float x, float y[], float p[], float a[], float dydx[]);
// function to calculate the derivatives

void derivsDbl(double x, double y[], double p[], double a[], double dydx[]);
// function to calculate the derivatives

inline float maxFlt(float a, float b) {float m = a; return b > m ? b : m;}
// return the largest value of a or b
inline float minFlt(float a, float b) {float m = a; return b < m ? b : m;}
// return the smallest value of a or b

inline double maxDbl(double a, double b) {double m = a; return b > m ? b : m;}
// return the largest value of a or b
inline double minDbl(double a, double b) {double m = a; return b < m ? b : m;}
// return the smallest value of a or b

#endif


