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
    #include "../pclake_plus/pl61316rp.cpp" // declare parameters
    #include "../pclake_plus/pl61316rc.cpp" // declare initial states
    #include "../pclake_plus/pl61316sp.cpp" // set parameters
    #include "../pclake_plus/pl61316sc.cpp" // set initial states
}
