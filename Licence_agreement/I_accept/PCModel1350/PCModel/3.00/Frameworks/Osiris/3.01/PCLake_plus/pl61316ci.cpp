/*------------------------------------------------------------------------*/
/*  File Pclk509i.cpp                                                     */
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
#pragma warn -def // used before defined
#pragma warn -use // declared but not used

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
void PCModel::calculateInitAuxilDef()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316ri.cpp" // declare initial auxilaries
    #include "../pclake_plus/pl61316rs.cpp" // declare states
    #include "../pclake_plus/pl61316si.cpp" // set initial auxilaries
    #include "../pclake_plus/pl61316ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet0()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::initAuxil.initAuxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316ri.cpp" // declare initial auxilaries
    #include "../pclake_plus/pl61316rs.cpp" // declare states
    #include "../pclake_plus/pl6131600si.cpp" // set initial auxilaries
    #include "../pclake_plus/pl6131600ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet1()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::initAuxil.initAuxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316ri.cpp" // declare initial auxilaries
    #include "../pclake_plus/pl61316rs.cpp" // declare states
    #include "../pclake_plus/pl6131601si.cpp" // set initial auxilaries
    #include "../pclake_plus/pl6131601ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet2()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::initAuxil.initAuxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316ri.cpp" // declare initial auxilaries
    #include "../pclake_plus/pl61316rs.cpp" // declare states
    #include "../pclake_plus/pl6131602si.cpp" // set initial auxilaries
    #include "../pclake_plus/pl6131602ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet3()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::initAuxil.initAuxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316ri.cpp" // declare initial auxilaries
    #include "../pclake_plus/pl61316rs.cpp" // declare states
    #include "../pclake_plus/pl6131603si.cpp" // set initial auxilaries
    #include "../pclake_plus/pl6131603ss.cpp" // set initial states
}
