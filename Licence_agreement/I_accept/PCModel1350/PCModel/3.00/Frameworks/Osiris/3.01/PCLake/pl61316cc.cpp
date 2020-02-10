/*------------------------------------------------------------------------*/
/*  File Pclk509c.cpp                                                     */
/*  Model PCLake, version 5.08 with changes as in M code                  */
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
#define _SIN_ sin
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
    #include "../pclake/pl61316rp.cpp" // declare parameters
    #include "../pclake/pl61316rs.cpp" // declare states
    #include "../pclake/pl61316ra.cpp" // declare auxiliaries
    #include "../pclake/pl61316rd.cpp" // declare derivatives
    #include "../pclake/pl61316sa.cpp" // set auxiliaries
    #include "../pclake/pl61316sd.cpp" // set derivatives
}

#pragma warn -aus // assigned but not used
#pragma warn -def // used before defined
#pragma warn -use // declared but not used

void PCModel::calculateAuxilSet0(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pclake/pl61316rp.cpp" // declare parameters
    #include "../pclake/pl61316rs.cpp" // declare states
    #include "../pclake/pl61316ra.cpp" // declare auxiliaries
    #include "../pclake/pl61316rd.cpp" // declare derivatives
    #include "../pclake/pl6131600sa.cpp" // set auxiliaries
    #include "../pclake/pl6131600sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet1(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pclake/pl61316rp.cpp" // declare parameters
    #include "../pclake/pl61316rs.cpp" // declare states
    #include "../pclake/pl61316ra.cpp" // declare auxiliaries
    #include "../pclake/pl61316rd.cpp" // declare derivatives
    #include "../pclake/pl6131601sa.cpp" // set auxiliaries
    #include "../pclake/pl6131601sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet2(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pclake/pl61316rp.cpp" // declare parameters
    #include "../pclake/pl61316rs.cpp" // declare states
    #include "../pclake/pl61316ra.cpp" // declare auxiliaries
    #include "../pclake/pl61316rd.cpp" // declare derivatives
    #include "../pclake/pl6131602sa.cpp" // set auxiliaries
    #include "../pclake/pl6131602sd.cpp" // set derivatives
}

void PCModel::calculateAuxilSet3(double time, double *state, double *param, double *auxil, double *deriv)
{
    #include "../pclake/pl61316rp.cpp" // declare parameters
    #include "../pclake/pl61316rs.cpp" // declare states
    #include "../pclake/pl61316ra.cpp" // declare auxiliaries
    #include "../pclake/pl61316rd.cpp" // declare derivatives
    #include "../pclake/pl6131603sa.cpp" // set auxiliaries
    #include "../pclake/pl6131603sd.cpp" // set derivatives
}
