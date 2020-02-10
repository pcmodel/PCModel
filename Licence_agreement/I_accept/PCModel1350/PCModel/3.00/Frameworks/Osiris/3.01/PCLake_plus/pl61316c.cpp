/*----------------------------------------------------------------------*/
/*  File Pclk61316.cpp                                                    */
/*  Model PCLake v5.09 with changes M version                             */
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

#include "osbioobj.h"
// header file containing specification of the osiris functions
#include "pcmodel.h"
// header file containing specification of the pcmodel class

void PCLake61316Define();
// function to define the analyser and system object
void PCLake61316Data();
// function to specify the data structure of the analyser and system input table
int PCLake61316Main(Analyser &a);
// function that specifies the main function of the analyser object
void PCLake61316ConstructAna(Analyser &a);
// function to construct the analyser object
void PCLake61316DestructAna(Analyser &a);
// function to destruct the analyser object
void PCLake61316ConstructSys(System &s);
// function to construct the system object
void PCLake61316DestructSys(System &s);
// function to destruct the system object

double PCLake61316StartSys(SystemTask &t);
// task to start the system
double PCLake61316RepSys(SystemTask &t);
// task to generate time dependent output
double PCLake61316ColAvgSys(SystemTask &t);
// task to collect averages during the simulation
double PCLake61316RepAvgSys(SystemTask &t);
// task to print averages during the simulation
double PCLake61316PerformOneIntegrationStep(SystemTask &t);
// task to perform the simulation
double PCLake61316StopSys(SystemTask &t);
// task to end the simulation
double PCLake61316CompData(SystemTask &t);
// task to handle the forcing functions, including interpolation
double PCLake61316ReadParam(SystemTask &t);
// task to handle the forcing functions, including interpolation

/*------------------------------------------------------------------------*/

void PCLake61316Define()
// function to define the analyser and system object
{   cout << "Analyser define" << endl;
    // show output
    Analyser::setDataPtr(PCLake61316Data);
    // define the data structure of the analyser object, this defines the batch control information
    Analyser::setConstructPtr(PCLake61316ConstructAna);
    // define the constructor of the analyser object, this allows for initial operations before the batch is started
    Analyser::setMainPtr(PCLake61316Main);
    // define the main function of the analyser object, this starts the simulations for each line of the analyser input table
    Analyser::setDestructPtr(PCLake61316DestructAna);
    // define the destructor of the analyser object, this allows for final operations after the batch is terminated
    System::setConstructPtr(PCLake61316ConstructSys);
    // define the constructor of the system object, this allows for initial operations before the simulation is started
    System::setDestructPtr(PCLake61316DestructSys);
    // define the destructor of the system object, this allows for final operations after the simulation is terminated
}

/*------------------------------------------------------------------------*/

void PCLake61316Data()
// function to specify the data structure of the analyser and system input table
{   cout << "Analyser data" << endl;
    // show output
    //  Analyser
    //  SIM_LNG_ID                    0
    //  ANA_LNG_READY                 1
        Analyser::setLngNumber       (2);
    //  SIM_INT_STATUS                0
#define ANA_INT_RUN_TYPE              0
#define ANA_INT_INT_METHOD            1
#define ANA_INT_STATE_SET             2
#define ANA_INT_PARAM_SET             3
#define ANA_INT_INIT_AUXIL_SET        4
#define ANA_INT_AUXIL_SET             5
#define ANA_INT_CALIB_TYPE            6
#define ANA_INT_CALIB_MAX_ITERATIONS  7
        Analyser::setIntNumber       (8);
    //  SIM_DBL_TIME                  0
    //  ANA_DBL_READY                 1
#define ANA_DBL_INT_STEP              2
#define ANA_DBL_INT_ACCURACY          3
#define ANA_DBL_REP_START             4
#define ANA_DBL_REP_STEP              5
#define ANA_DBL_AVG_STEP              6
#define ANA_DBL_AVG_START             7
#define ANA_DBL_AVG_START_WITHIN_YEAR 8
#define ANA_DBL_AVG_END_WITHIN_YEAR   9
#define ANA_DBL_FACTOR_SENS          10
#define ANA_DBL_CALIB_SCALE          11
#define ANA_DBL_CALIB_ACCURACY       12
        Analyser::setDblNumber      (13);
    //  SIM_FLT_VALUE                 0
        Analyser::setFltNumber       (1);
    //  SIM_STR_NAME                  0
    //  ANA_STR_IND_DMP              35
#define ANA_STR_STATE_INP            36
#define ANA_STR_PARAM_INP            37
#define ANA_STR_INIT_AUXIL_INP       38
#define ANA_STR_AUXIL_INP            39
#define ANA_STR_SET_REP              40
#define ANA_STR_INIT_REP             41
#define ANA_STR_DYN_REP              42
#define ANA_STR_AVG_REP              43
#define ANA_STR_SENS_REP             44
#define ANA_STR_CALIB_REP            45
#define ANA_STR_OPTIM_REP            46
#define ANA_STR_BIFUR_REP            47
        Analyser::setStrNumber      (48);
    //  SIM_SUM_STAT                  0
        Analyser::setSumNumber       (1);

    //  System
    //  SIM_LNG_ID                    0
        System::setLngNumber         (1);
    //  SIM_INT_STATUS                0
        System::setIntNumber         (1);
    //  SIM_DBL_TIME                  0
        System::setDblNumber         (1);
    //  SIM_FLT_VALUE                 0
        System::setFltNumber         (1);
    //  SIM_STR_NAME                  0
        System::setStrNumber         (1);
    //  SIM_SUM_STAT                  0
        System::setSumNumber         (1);
}

/*----------------------------------------------------------------------*/

void PCLake61316ConstructAna(Analyser &a)
{   cout << "Construct analyser" << endl;
    //
//*************************SWITCH TO PCMODEL**********************************//
    PCModel::constructAnalyser();
    //
//*************************SWITCH TO PCMODEL**********************************//
    a;
    //
}

/*------------------------------------------------------------------------*/

PCModel *pclake;

int PCLake61316Main(Analyser &a)
{   cout << endl << "Analyser main" << endl;
    //
    int returnValue;
    pclake = new PCModel(a.getSimLngId(),
                         a.getInt(ANA_INT_RUN_TYPE),
                         a.getInt(ANA_INT_INT_METHOD),
                         a.getInt(ANA_INT_STATE_SET),
                         a.getInt(ANA_INT_PARAM_SET),
                         a.getInt(ANA_INT_INIT_AUXIL_SET),
                         a.getInt(ANA_INT_AUXIL_SET),
                         a.getInt(ANA_INT_CALIB_TYPE),
                         a.getInt(ANA_INT_CALIB_MAX_ITERATIONS),
                         a.getDbl(ANA_DBL_INT_STEP),
                         a.getDbl(ANA_DBL_INT_ACCURACY),
                         a.getDbl(ANA_DBL_FACTOR_SENS),
                         a.getDbl(ANA_DBL_CALIB_SCALE),
                         a.getDbl(ANA_DBL_CALIB_ACCURACY),
                         a.getStr(ANA_STR_STATE_INP),
                         a.getStr(ANA_STR_PARAM_INP),
                         a.getStr(ANA_STR_INIT_AUXIL_INP),
                         a.getStr(ANA_STR_AUXIL_INP),
                         a.getStr(ANA_STR_SET_REP),
                         a.getStr(ANA_STR_INIT_REP),
                         a.getStr(ANA_STR_DYN_REP),
                         a.getStr(ANA_STR_AVG_REP),
                         a.getStr(ANA_STR_SENS_REP),
                         a.getStr(ANA_STR_CALIB_REP),
                         a.getStr(ANA_STR_OPTIM_REP),
                         a.getStr(ANA_STR_BIFUR_REP));
//*************************SWITCH TO PCMODEL**********************************//
    returnValue = pclake->main();
    //
//*************************SWITCH TO PCMODEL**********************************//
    delete pclake;
    //
    return returnValue;
    //
}

void performOneSimulation()
//
{   System::main(Analyser::getAna());
    //
}

/*------------------------------------------------------------------------*/

void PCLake61316DestructAna(Analyser &a)
{   cout << "Destruct analyser" << endl;
    //
//*************************SWITCH TO PCMODEL**********************************//
    PCModel::destructAnalyser();
    //
//*************************SWITCH TO PCMODEL**********************************//
    a;
    //
}

/*------------------------------------------------------------------------*/

void PCLake61316ConstructSys(System &s)
// function to construct the system for a new simulation
{   cout << endl << "Construct system" << endl;
    s.setNewSysTask(s.getAna().getSimDblTime(), PCLake61316StartSys);
    // start a new task that builds up the system at the time specified in the first dbl column of the analyser table
    s.setNewSysTask(365.0 * s.getAna().getDbl(ANA_DBL_REP_START), PCLake61316RepSys);
    // start a new task for reporting at the time specified in the first dbl column of the analyser table
    s.setNewSysTask(365.0 * s.getAna().getDbl(ANA_DBL_AVG_START), PCLake61316ColAvgSys);
    // start a new task for averaging at the time specified in the 'start averaging' column of the analyser table
    s.setNewSysTask(365.0 * s.getAna().getDbl(ANA_DBL_AVG_START) + 365.0, PCLake61316RepAvgSys);
    // start a new task for averaging at the time specified in the 'start averaging' column of the analyser table
    s.setNewSysTask(365.0 * s.getAna().getAnaDblReady() + 0.0000001, PCLake61316StopSys);
    // start a new task ending the simulation at  the time specified in the second dbl column of the analyser table
    s.setNewSysTask(s.getAna().getSimDblTime() + 0.0000002, PCLake61316PerformOneIntegrationStep);
    // start a new task for performing the simulation at the time specified in the first dbl column of the analyser table
} // Note that adding or deleting tasks has consequenses for the TaskIdToParamId array through the parameter MAX_SYS_TASK

int setNewReadParamSysTask(long id, double startTime)
//
{    Analyser &a = Analyser::getAna();
     //
     System &s = a.getSys();
     //
     SystemTask &t = s.setNewSysTask(startTime, PCLake61316ReadParam);
     //
     t.setNumber(id);
     //
     return t.getTaskId();
     //
}

int setNewCompDataSysTask(long id, double startTime)
//
{    Analyser &a = Analyser::getAna();
     //
     System &s = a.getSys();
     //
     SystemTask &t = s.setNewSysTask(startTime, PCLake61316CompData);
     //
     t.setNumber(id);
     //
     return t.getTaskId();
     //
}

/*------------------------------------------------------------------------*/

void PCLake61316DestructSys(System &s)
//
{   cout << "Destruct system" << endl;
    //
    s;
    //
}

/*------------------------------------------------------------------------*/


double PCLake61316StartSys(SystemTask &t)
// task to start the system
{
//*************************SWITCH TO PCMODEL**********************************//
    pclake->startSys();
    // do what is necessary to start the simulation
//*************************SWITCH TO PCMODEL**********************************//
    t.setStopTask();
    // tell the task manager that this task should not be performed again
    return 0; // no return time necessary because this task is only performed once
    // return next time this function should be performed, in this case redundant
}

double PCLake61316StopSys(SystemTask &t)
// task to end the simulation
{
//*************************SWITCH TO PCMODEL**********************************//
    pclake->stopSys();
    // do what is necessary to stop the simulation
//*************************SWITCH TO PCMODEL**********************************//
    t.setStopSimulation();
    // tell the task manager to stop the simulation
    return 0; // no return time necessary because this task is only performed once
    // return next time this function should be performed, in this case redundant
}

double PCLake61316RepSys(SystemTask &t)
// write time dependent output
{   double taskTime = t.getTime();
    // get the current time
    System &s = t.getSys();
    // get a reference to the system object
    Analyser &a = s.getAna();
    // get a reference to the analyser object
//*************************SWITCH TO PCMODEL**********************************//
    pclake->repSys(taskTime);
    // write time dependent output
//*************************SWITCH TO PCMODEL**********************************//
    return taskTime + a.getDbl(ANA_DBL_REP_STEP);
    // return the time at which this task should be performed again, in this case the current time plus the reporting time step
}

double PCLake61316ColAvgSys(SystemTask &t)
// collect the averages of the time dependent output
{   double taskTime = t.getTime();
    // get the current time
    System &s = t.getSys();
    // get a reference to the system object
    Analyser &a = s.getAna();
    // get a reference to the analyser object
//*************************SWITCH TO PCMODEL**********************************//
    pclake->colAvgSys(taskTime, a.getDbl(ANA_DBL_AVG_START_WITHIN_YEAR), a.getDbl(ANA_DBL_AVG_END_WITHIN_YEAR));
    // collect the averages of the time dependent output
//*************************SWITCH TO PCMODEL**********************************//
    return taskTime + a.getDbl(ANA_DBL_AVG_STEP);
    // return the time at which this task should be performed again, in this case the current time plus the averaging time step
}

double PCLake61316RepAvgSys(SystemTask &t)
{   // task to print averages during the simulation
    double taskTime = t.getTime();
    // get the current time
//*************************SWITCH TO PCMODEL**********************************//
    pclake->repAvgSys();
    // write time dependent average output
//*************************SWITCH TO PCMODEL**********************************//
    return taskTime + 365.0;
    // return the time at which this task should be performed again, in this case the current time plus the averaging time step
}

double PCLake61316PerformOneIntegrationStep(SystemTask &t)
// task to perform the simulation
{   System &s = t.getSys();
    // get a reference to the system object
    double taskTime = t.getTime();
    // get the current time
    pclake->performOneIntegrationStep(taskTime);
    // perform one time step of the integration
    s.setSimDblTime(taskTime);
    // set the current time of the system object, NB THIS TIME IS MODIFIED BY THE INTEGRATION ROUTINE
    return taskTime;
    // return the time at which this task should be performed again
}

double PCLake61316CompData(SystemTask &t)
// task to compare observed with modelled data
{   double taskTime = t.getTime();
    // get the current time
    double nextTaskTime = 0;
    // declare a variable to hold the next taks time, this value is set by the compData function
    long number = t.getNumber();
    //
    int stopTask = 0;
    // declare a logical variable that determines if this task is to be performed again, this value is set by the compData function
    pclake->compareWithData(taskTime, nextTaskTime, number, stopTask);
    // compare the observed with the modelled data
    if (stopTask) t.setStopTask();
    // if compData has decided that this task is to be stopped, stop it
    return nextTaskTime;
    // return the time at which this task should be performed again
}

double PCLake61316ReadParam(SystemTask &t)
// task to handle the forcing functions, including interpolation <------------------ this function could be split in two functions, one that does the reading and one that does the interpolation
{   System &s = t.getSys();
    // get a reference to the system object
    Analyser &a = s.getAna();
    // get a reference to the analyser object
    double taskTime = t.getTime();
    // get the current time
    double nextTaskTime = taskTime + a.getDbl(ANA_DBL_INT_STEP);
    // define the next time this task should be performed as the current task plus the integration time step
    long number = t.getNumber();
    //
    int stopTask = 0;
    // declare a logical variable that determines if this task is to be performed again, this value is set by the compData function
//*************************SWITCH TO PCMODEL**********************************//
    pclake->readForcingFunction(taskTime, nextTaskTime, number, stopTask);
//*************************SWITCH TO PCMODEL**********************************//
    // task to handle the forcing functions, including interpolation
    if (stopTask) t.setStopTask();
    // if readParam has decided that this task is to be stopped, stop it
    return nextTaskTime;
    // return the time at which this task should be performed again
}

/*------------------------------------------------------------------------*/

