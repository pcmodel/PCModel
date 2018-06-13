/*------------------------------------------------------------------------*/
/*  File pd21316cd.cpp                                                    */
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

#include "pcmodel.h"
#include "math.h"
#ifdef __GNUG__
    #include "iostream"
    using namespace std;
#else
    #include "iostream.h"
#endif
//
void PCModel::setDefaultValues()
{   double *param = PCModel::param.paramValue;
    double *initState = PCModel::state.initStateValue;
    #include "../pcditch/pd21316rp.cpp" // declare parameters
    #include "../pcditch/pd21316rc.cpp" // declare initial states
    #include "../pcditch/pd21316sp.cpp" // set parameters
    #include "../pcditch/pd21316sc.cpp" // set initial states
}
