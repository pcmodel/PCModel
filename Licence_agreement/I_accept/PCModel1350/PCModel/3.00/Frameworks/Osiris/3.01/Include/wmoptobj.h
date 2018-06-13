/*------------------------------------------------------------------------*/
/*  Wmoptobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluiskenslaan 21                                                      */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMOPTOBJ_H
#define __WMOPTOBJ_H

#include "wmoutobj.h"

// Class definitions

// defines an optimization object
class OptObject
{ // public data members and member functions
  public :
    // constructor of OptObject
    OptObject(ostream &o, double (*f)(double *p), int d, int t = 0, double *s = 0, double *l = 0, double *u = 0, double ft = 1.0e-10, int nm = 1000);
    // constructor of OptObject
    OptObject(ostream &o, double (*f)(double *p), int d, int *t, double *s = 0, double *l = 0, double *u = 0, double ft = 1.0e-10, int nm = 1000);
    // destructor of OptObject
    ~OptObject();
    // function for function minimization
    double minimize(double *b);
    // function for function maximization
    double maximize(double *b);
    // function for optimization to a target value
    double setEqualTo(double *b, double t);
  // private data members and member functions
  protected :
    // number of dimensions
    int ndim;
    // boolean that is set if vector type is owned by the object
    int ownsType;
    // vector that contains the type for each dimension
    int *type;
    // vector that contains the lower boundary for each dimension
    double *low;
    // vector that contains the upper boundary for each dimension
    double *up;
    // vector that contains the scale for each dimension
    double *scale;
    // desired relative tolerance
    double ftol;
    // maximum number of function evaluations
    int nmax;
    // pointer to the function that is to be minimized
    double (*func)(double *p);
    // stream for output
    ostream &out;


    // function that performs the optimization given an vector of initial values
    double optimize(double *b);
    // pointer to the function that is to be minimized
    double (OptObject::*minFunc)(double *p);
    // function that is used in case of minimization
    double minimizeFunction(double *p);
    // function that is used in case of maximization
    double maximizeFunction(double *p);
    // function that is used in case of a target value
    double setEqualToFunction(double *p);
    // target value
    double target;

    // array that will hold the coordinates of the simplex
    double *v;
    // array that holds pointers to the coordinates of each of the corners of the simplex
    double **p;
    // array that holds the function values for each of the corners of the simplex
    double *y;
    // array that holds the sum of the coordinates of all corners for a given dimension
    double *psum;
    // array that holds a trial corner
    double *ptry;
    // array that holds untransformed values to be tested
    double *eval;
    // array that holds transformed initial values
    double *init;

    // number of function evaluations
    int nfunc;
    // index of the corner with the lowest function value
    int ilo;
    // index of the corner with the one but highest function value
    int inhi;
    // index of the corner with the highest function value
    int ihi;
    // relative difference between the lowest and the highest function value
    double rtol;
    // function value during the latest trial
    double ytry;

    // funtion that allocates memory
    void allocateMemory();
    // function that tries to replace the highest point with a better one, given the direction and step defined by fac
    void trial(double fac);
    // function that transforms the parameters to allow constraints on the parameters
    double transform(double *d, int i);
    // function that untransforms the parameters
    double untransform(double *d, int i);
    // function that untransforms the parameter vector that is to be evaluated
    double *untransform(double *d);
    // function that prints the header
    void printHeader();
    // function that prints the simplex and associated data
    void print();
    // function that initializes the simplex
    void initializeSimplex();
    // function that set the ilo, inhi and ihi indices
    void getIndices();
    // function that shrinks the simplex towards the corner with the lowest function value
    void shrink();
    // function that transforms the initial values
    void transformInitialValues(double *b);
    // function that untransforms the final values
    void untransformFinalValues(double *b);

};

#endif


