/*------------------------------------------------------------------------*/
/*  File pl61316db.cpp                                                     */
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
#pragma warn +def // used before defined <-- moet altijd aanstaan!!!
#pragma warn +use // declared but not used
//
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
#define _ACOS_ acos
#define _POW_ pow
#define _LN_ log
#define _SQRT_ sqrt
#define _SIN_ sin
#define _TAN_ tan
#define _ASIN_ asin
#define _ATAN_ atan
#define _ACOS_ acos
#define _SINH_ sinh
#define _COSH_ cosh
#define _TANH_ tanh

//
inline double max(double a, double b) {double m = a; return b > m ? b : m;}
inline double max(double a, double b, double c) {double m = a; m = b > m ? b : m; return c > m ? c : m;}
inline double max(double a, double b, double c, double d) {double m = a; m = b > m ? b : m; m = c > m ? c : m; return d > m ? d : m;}
inline double min(double a, double b) {double m = a; return b < m ? b : m;}
inline double min(double a, double b, double c) {double m = a; m = b < m ? b : m; return c < m ? c : m;}
inline double min(double a, double b, double c, double d) {double m = a; m = b < m ? b : m; m = c < m ? c : m; return d < m ? d : m;}
//

#pragma warn -aus // assigned but not used

void PCLake61316Debug(double time)
{
#include "../pclake_plus/pl61316rp_.cpp" // declare parameters
#include "../pclake_plus/pl61316rc_.cpp" // declare initial states
#include "../pclake_plus/pl61316ri_.cpp" // declare initial auxiliaries
#include "../pclake_plus/pl61316rs_.cpp" // declare states
#include "../pclake_plus/pl61316ra_.cpp" // declare auxiliaries
#include "../pclake_plus/pl61316rd_.cpp" // declare derivatives
#include "../pclake_plus/pl61316sp.cpp" // set parameters
#include "../pclake_plus/pl61316sc.cpp" // set initial states
#include "../pclake_plus/pl61316si.cpp" // set initial auxiliaries
#include "../pclake_plus/pl61316ss.cpp" // set states
#include "../pclake_plus/pl61316sa.cpp" // set auxiliaries
#include "../pclake_plus/pl61316sd.cpp" // set derivatives
#include "../pclake_plus/pl61316rdd.cpp" // set derivatives
}

#pragma warn -aus // assigned but not used

void PCLake6131600Debug00(double time)
{
#include "../pclake_plus/pl61316rp_.cpp" // declare parameters
#include "../pclake_plus/pl61316rc_.cpp" // declare initial states
#include "../pclake_plus/pl61316ri_.cpp" // declare initial auxiliaries
#include "../pclake_plus/pl61316rs_.cpp" // declare states
#include "../pclake_plus/pl61316ra_.cpp" // declare auxiliaries
#include "../pclake_plus/pl61316rd_.cpp" // declare derivatives
#include "../pclake_plus/pl61316sp.cpp" // set parameters
#include "../pclake_plus/pl61316sc.cpp" // set initial states
#include "../pclake_plus/pl6131600si.cpp" // set initial auxiliaries
#include "../pclake_plus/pl6131600ss.cpp" // set states
#include "../pclake_plus/pl6131600sa.cpp" // set auxiliaries
#include "../pclake_plus/pl6131600sd.cpp" // set derivatives
#include "../pclake_plus/pl61316rdd.cpp" // set derivatives
}

void PCLake6131601Debug(double time)
{
#include "../pclake_plus/pl61316rp_.cpp" // declare parameters
#include "../pclake_plus/pl61316rc_.cpp" // declare initial states
#include "../pclake_plus/pl61316ri_.cpp" // declare initial auxiliaries
#include "../pclake_plus/pl61316rs_.cpp" // declare states
#include "../pclake_plus/pl61316ra_.cpp" // declare auxiliaries
#include "../pclake_plus/pl61316rd_.cpp" // declare derivatives
#include "../pclake_plus/pl61316sp.cpp" // set parameters
#include "../pclake_plus/pl61316sc.cpp" // set initial states
#include "../pclake_plus/pl6131601si.cpp" // set initial auxiliaries
#include "../pclake_plus/pl6131601ss.cpp" // set states
#include "../pclake_plus/pl6131601sa.cpp" // set auxiliaries
#include "../pclake_plus/pl6131601sd.cpp" // set derivatives
#include "../pclake_plus/pl61316rdd.cpp" // set derivatives
}

void PCLake6131602Debug(double time)
{
#include "../pclake_plus/pl61316rp_.cpp" // declare parameters
#include "../pclake_plus/pl61316rc_.cpp" // declare initial states
#include "../pclake_plus/pl61316ri_.cpp" // declare initial auxiliaries
#include "../pclake_plus/pl61316rs_.cpp" // declare states
#include "../pclake_plus/pl61316ra_.cpp" // declare auxiliaries
#include "../pclake_plus/pl61316rd_.cpp" // declare derivatives
#include "../pclake_plus/pl61316sp.cpp" // set parameters
#include "../pclake_plus/pl61316sc.cpp" // set initial states
#include "../pclake_plus/pl6131602si.cpp" // set initial auxiliaries
#include "../pclake_plus/pl6131602ss.cpp" // set states
#include "../pclake_plus/pl6131602sa.cpp" // set auxiliaries
#include "../pclake_plus/pl6131602sd.cpp" // set derivatives
#include "../pclake_plus/pl61316rdd.cpp" // set derivatives
}

void PCLake6131603Debug(double time)
{
#include "../pclake_plus/pl61316rp_.cpp" // declare parameters
#include "../pclake_plus/pl61316rc_.cpp" // declare initial states
#include "../pclake_plus/pl61316ri_.cpp" // declare initial auxiliaries
#include "../pclake_plus/pl61316rs_.cpp" // declare states
#include "../pclake_plus/pl61316ra_.cpp" // declare auxiliaries
#include "../pclake_plus/pl61316rd_.cpp" // declare derivatives
#include "../pclake_plus/pl61316sp.cpp" // set parameters
#include "../pclake_plus/pl61316sc.cpp" // set initial states
#include "../pclake_plus/pl6131603si.cpp" // set initial auxiliaries
#include "../pclake_plus/pl6131603ss.cpp" // set states
#include "../pclake_plus/pl6131603sa.cpp" // set auxiliaries
#include "../pclake_plus/pl6131603sd.cpp" // set derivatives
#include "../pclake_plus/pl61316rdd.cpp" // set derivatives
}

