/*------------------------------------------------------------------------*/
/*  File pd61316.cpp                                                      */
/*  Model PCDitch, version 2.13.16 with changes as in M code              */
/*  Implemented in framework Osiris, version 3.01                         */
/*  Copyright Osiris (c) 1993, 2000                                       */
/*  W.M. Mooij                                                            */
/*  Netherlands Institute of Ecology                                      */
/*  Centre for Limnology                                                  */
/*  Rijksstraatweg 6                                                      */
/*  3631 AC  Nieuwersluis                                                 */
/*  The Netherlands                                                       */
/*  tel: +31 294 239352                                                   */
/*  fax: +31 294 232224                                                   */
/*  e-mail: mooij@cl.nioo.knaw.nl                                         */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/
//
#pragma warn -aus // assigned but not used
#pragma warn +def // used before defined
#pragma warn -use // declared but not used
//
#include "pcmodel.h"
#include "math.h"
#ifdef __GNUG__
    #include "iostream"
    using namespace std;
#else
    #include "iostream.h"
#endif
//
#define _TRUE_ 1.0
#define _FALSE_ 0.0
#define _EQ_ ==
#define _LT_ <
#define _LE_ <=
#define _GT_ >
#define _GE_ >=
#define _AND_ &&
#define _OR_ ||
#define _FLOOR_ floor
#define _COS_ cos
#define _EXP_ exp
#define _MIN_ min
#define _MAX_ max
#define _IF_
#define _THEN_ ?
#define _ELSEIF_ :
#define _ELSE_ :
#define _ENDIF_

#define DEBUG_MATH

#ifndef DEBUG_MATH

#define _ACOS_ acos
#define _POW_ pow
#define _LN_ log
#define _SQRT_ sqrt

#else

inline double _LN_(double x)
{   if (x > 0)
    {   return log(x);
    }
    else
    {   cout << "invalid argument in function log " << x << endl;
        return 0;
    }
}

inline double _SQRT_(double x)
{   if (x >= 0)
    {   return sqrt(x);
    }
    else
    {   cout << "invalid argument in function sqrt " << x << endl;
        return 0;
    }
}

inline double _POW_(double x, double y)
{   if (x >= 0)
    {   return pow(x, y);
    }
    else
    {   cout << "invalid argument in function pow " << x << endl;
        return 0;
    }
}

inline double _ACOS_(double x)
{   if (x >= -1.0 && x <= 1.0)
    {   return acos(x);
    }
    else
    {   cout << "invalid argument in function acos " << x << endl;
        return 0;
    }
}

inline double _LN_(int i, double x)
{   if (x > 0)
    {   return log(x);
    }
    else
    {   cout << "invalid argument in function log " << i << " " << x << endl;
        return 0;
    }
}

inline double _SQRT_(int i, double x)
{   if (x >= 0)
    {   return sqrt(x);
    }
    else
    {   cout << "invalid argument in function sqrt " << i << " " << x << endl;
        return 0;
    }
}

inline double _POW_(int i, double x, double y)
{   if (x >= 0)
    {   return pow(x, y);
    }
    else
    {   cout << "invalid argument in function pow " << i << " " << x << endl;
        return 0;
    }
}

inline double _ACOS_(int i, double x)
{   if (x >= -1.0 && x <= 1.0)
    {   return acos(x);
    }
    else
    {   cout << "invalid argument in function acos " << i << " " << x << endl;
        return 0;
    }
}

#endif

//
inline double max(double a, double b) {double m = a; return b > m ? b : m;}
inline double max(double a, double b, double c) {double m = a; m = b > m ? b : m; return c > m ? c : m;}
inline double max(double a, double b, double c, double d) {double m = a; m = b > m ? b : m; m = c > m ? c : m; return d > m ? d : m;}
inline double min(double a, double b) {double m = a; return b < m ? b : m;}
inline double min(double a, double b, double c) {double m = a; m = b < m ? b : m; return c < m ? c : m;}
inline double min(double a, double b, double c, double d) {double m = a; m = b < m ? b : m; m = c < m ? c : m; return d < m ? d : m;}

//
void PCModel::calculateAuxilDef(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316ra.cpp" // declare auxiliaries
    #include "../pcditch/pd21316rd.cpp" // declare derivatives
    #include "../pcditch/pd21316sa.cpp" // set auxiliaries
    #include "../pcditch/pd21316sd.cpp" // set derivatives
}

#pragma warn -aus // assigned but not used
#pragma warn -def // used before defined
#pragma warn -use // declared but not used

void PCModel::calculateAuxilSet0(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316ra.cpp" // declare auxiliaries
    #include "../pcditch/pd21316rd.cpp" // declare derivatives
    #include "../pcditch/pd2131600sa.cpp" // set auxiliaries
    #include "../pcditch/pd2131600sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet1(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316ra.cpp" // declare auxiliaries
    #include "../pcditch/pd21316rd.cpp" // declare derivatives
    #include "../pcditch/pd2131601sa.cpp" // set auxiliaries
    #include "../pcditch/pd2131601sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet2(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316ra.cpp" // declare auxiliaries
    #include "../pcditch/pd21316rd.cpp" // declare derivatives
    #include "../pcditch/pd2131602sa.cpp" // set auxiliaries
    #include "../pcditch/pd2131602sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet3(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316ra.cpp" // declare auxiliaries
    #include "../pcditch/pd21316rd.cpp" // declare derivatives
    #include "../pcditch/pd2131603sa.cpp" // set auxiliaries
    #include "../pcditch/pd2131603sd.cpp" // set derivatives
}
