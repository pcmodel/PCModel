/*------------------------------------------------------------------------*/
/*  File pd21316ci.cpp                                                    */
/*  Model pcditch, version 2.13.16 with changes as in M code              */
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
#define _IF_
#define _THEN_ ?
#define _ELSE_ :
#define _ENDIF_

//
void PCModel::calculateInitAuxilDef()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316ri.cpp" // declare initial auxilaries
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd21316si.cpp" // set initial auxilaries
    #include "../pcditch/pd21316ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet0()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316ri.cpp" // declare initial auxilaries
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd2131600si.cpp" // set initial auxilaries
    #include "../pcditch/pd2131600ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet1()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316ri.cpp" // declare initial auxilaries
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd2131601si.cpp" // set initial auxilaries
    #include "../pcditch/pd2131601ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet2()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316ri.cpp" // declare initial auxilaries
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd2131602si.cpp" // set initial auxilaries
    #include "../pcditch/pd2131602ss.cpp" // set initial states
}

void PCModel::calculateInitAuxilSet3()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    double *initAuxil = PCModel::auxil.auxilValue;
    double *state = PCModel::state.stateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316ri.cpp" // declare initial auxilaries
    #include "../pcditch/pd21316rs.cpp" // declare states
    #include "../pcditch/pd2131603si.cpp" // set initial auxilaries
    #include "../pcditch/pd2131603ss.cpp" // set initial states
}
