/*------------------------------------------------------------------------*/
/*  File pd21316db.cpp                                                    */
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

//
inline double max(double a, double b) {double m = a; return b > m ? b : m;}
inline double max(double a, double b, double c) {double m = a; m = b > m ? b : m; return c > m ? c : m;}
inline double max(double a, double b, double c, double d) {double m = a; m = b > m ? b : m; m = c > m ? c : m; return d > m ? d : m;}
inline double min(double a, double b) {double m = a; return b < m ? b : m;}
inline double min(double a, double b, double c) {double m = a; m = b < m ? b : m; return c < m ? c : m;}
inline double min(double a, double b, double c, double d) {double m = a; m = b < m ? b : m; m = c < m ? c : m; return d < m ? d : m;}
//

#pragma warn -aus // assigned but not used

void PCDitch21316Debug(double time)
{
#include "../pcditch/pd21316rp_.cpp" // declare parameters
#include "../pcditch/pd21316rc_.cpp" // declare initial states
#include "../pcditch/pd21316ri_.cpp" // declare initial auxiliaries
#include "../pcditch/pd21316rs_.cpp" // declare states
#include "../pcditch/pd21316ra_.cpp" // declare auxiliaries
#include "../pcditch/pd21316rd_.cpp" // declare derivatives
#include "../pcditch/pd21316sp.cpp" // set parameters
#include "../pcditch/pd21316sc.cpp" // set initial states
#include "../pcditch/pd21316si.cpp" // set initial auxiliaries
#include "../pcditch/pd21316ss.cpp" // set states
#include "../pcditch/pd21316sa.cpp" // set auxiliaries
#include "../pcditch/pd21316sd.cpp" // set derivatives
#include "../pcditch/pd21316rdd.cpp" // set derivatives
}

#pragma warn -aus // assigned but not used

void PCDitch2131600Debug00(double time)
{
#include "../pcditch/pd21316rp_.cpp" // declare parameters
#include "../pcditch/pd21316rc_.cpp" // declare initial states
#include "../pcditch/pd21316ri_.cpp" // declare initial auxiliaries
#include "../pcditch/pd21316rs_.cpp" // declare states
#include "../pcditch/pd21316ra_.cpp" // declare auxiliaries
#include "../pcditch/pd21316rd_.cpp" // declare derivatives
#include "../pcditch/pd21316sp.cpp" // set parameters
#include "../pcditch/pd21316sc.cpp" // set initial states
#include "../pcditch/pd2131600si.cpp" // set initial auxiliaries
#include "../pcditch/pd2131600ss.cpp" // set states
#include "../pcditch/pd2131600sa.cpp" // set auxiliaries
#include "../pcditch/pd2131600sd.cpp" // set derivatives
#include "../pcditch/pd21316rdd.cpp" // set derivatives
}

void PCDitch2131601Debug(double time)
{
#include "../pcditch/pd21316rp_.cpp" // declare parameters
#include "../pcditch/pd21316rc_.cpp" // declare initial states
#include "../pcditch/pd21316ri_.cpp" // declare initial auxiliaries
#include "../pcditch/pd21316rs_.cpp" // declare states
#include "../pcditch/pd21316ra_.cpp" // declare auxiliaries
#include "../pcditch/pd21316rd_.cpp" // declare derivatives
#include "../pcditch/pd21316sp.cpp" // set parameters
#include "../pcditch/pd21316sc.cpp" // set initial states
#include "../pcditch/pd2131601si.cpp" // set initial auxiliaries
#include "../pcditch/pd2131601ss.cpp" // set states
#include "../pcditch/pd2131601sa.cpp" // set auxiliaries
#include "../pcditch/pd2131601sd.cpp" // set derivatives
#include "../pcditch/pd21316rdd.cpp" // set derivatives
}

void PCDitch2131602Debug(double time)
{
#include "../pcditch/pd21316rp_.cpp" // declare parameters
#include "../pcditch/pd21316rc_.cpp" // declare initial states
#include "../pcditch/pd21316ri_.cpp" // declare initial auxiliaries
#include "../pcditch/pd21316rs_.cpp" // declare states
#include "../pcditch/pd21316ra_.cpp" // declare auxiliaries
#include "../pcditch/pd21316rd_.cpp" // declare derivatives
#include "../pcditch/pd21316sp.cpp" // set parameters
#include "../pcditch/pd21316sc.cpp" // set initial states
#include "../pcditch/pd2131602si.cpp" // set initial auxiliaries
#include "../pcditch/pd2131602ss.cpp" // set states
#include "../pcditch/pd2131602sa.cpp" // set auxiliaries
#include "../pcditch/pd2131602sd.cpp" // set derivatives
#include "../pcditch/pd21316rdd.cpp" // set derivatives
}

void PCDitch2131603Debug(double time)
{
#include "../pcditch/pd21316rp_.cpp" // declare parameters
#include "../pcditch/pd21316rc_.cpp" // declare initial states
#include "../pcditch/pd21316ri_.cpp" // declare initial auxiliaries
#include "../pcditch/pd21316rs_.cpp" // declare states
#include "../pcditch/pd21316ra_.cpp" // declare auxiliaries
#include "../pcditch/pd21316rd_.cpp" // declare derivatives
#include "../pcditch/pd21316sp.cpp" // set parameters
#include "../pcditch/pd21316sc.cpp" // set initial states
#include "../pcditch/pd2131603si.cpp" // set initial auxiliaries
#include "../pcditch/pd2131603ss.cpp" // set states
#include "../pcditch/pd2131603sa.cpp" // set auxiliaries
#include "../pcditch/pd2131603sd.cpp" // set derivatives
#include "../pcditch/pd21316rdd.cpp" // set derivatives
}

