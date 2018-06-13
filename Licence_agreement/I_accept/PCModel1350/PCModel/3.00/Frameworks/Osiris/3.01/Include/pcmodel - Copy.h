#ifndef __PCMODEL_H
#define __PCMODEL_H

#include "wmstrobj.h"
// header of string class
#include "wmascobj.h"
// header of ascii file class
#include "wmsumobj.h"
// header of summation class
#include "wmoutobj.h"
// header of output class
#include "wmodeobj.h"
// header of ode class
#include "wmoptobj.h"
// header of opt class
#include "../pclake/pl61316dim.cpp"
//#include "../pcditch/pd21316dim.cpp"
// cpp file containing the dimensions of the arrays

class PCModel;
class State;
class Parameter;
class InitialAuxiliary;
class Auxiliary;
class Method
{ public:
  protected :
    long id;
    //
    StrObject repOutputFilename;
    //
    ofstream repOutputFile;
    // declare the output file
  private :
    //
};

class SingleRun : Method
// single simulation
  { public :
    //SingleRun(){}
    SingleRun(int intm, double ints, double inta, StrObject &setrfn, StrObject &initrfn, StrObject &dynrfn, StrObject &avgrfn) :
      intMethod(intm), intStep(ints), intAccuracy(inta), setRepOutputFilename(setrfn), initRepOutputFilename(initrfn), dynRepOutputFilename(dynrfn), avgRepOutputFilename(avgrfn)
    {   if (constructionId == 0)
        {   setRep.open(setRepOutputFilename);
            // open the set rep file
            initRep.open(initRepOutputFilename);
            // open the init rep file
            dynRep.open(dynRepOutputFilename);
            // open the set rep file
            avgRep.open(avgRepOutputFilename);
            // open the init rep file
        }
        else
        {   setRep.open(setRepOutputFilename, ios::app);
            // open the set rep file
            initRep.open(initRepOutputFilename, ios::app);
            // open the init rep file
            dynRep.open(dynRepOutputFilename, ios::app);
            // open the set rep file
            avgRep.open(avgRepOutputFilename, ios::app);
            // open the init rep file
        }
        constructionId++;
    }
    ~SingleRun()
    {   setRep.close();
        // close the set rep file
        initRep.close();
        // close the init rep file
        dynRep.close();
        // close the dyn rep file
        avgRep.close();
        // close the avg rep file
    }
  private :
    void performSingleRun();
    //
    static long constructionId;
    //
    int intMethod;
    //
    double intStep;
    //
    double intAccuracy;
    //
    StrObject setRepOutputFilename;
    //
    StrObject initRepOutputFilename;
    //
    StrObject dynRepOutputFilename;
    //
    StrObject avgRepOutputFilename;
    //
    ofstream setRep;
    // declare the set rep output file
    ofstream initRep;
    // declare the init rep file
    ofstream dynRep;
    // declare the dyn rep file
    ofstream avgRep;
    // declare the avg rep file
    SumObject timeSum;
    // allocate memory for the sumobject that will hold the averages of the time ???
  friend class PCModel;
  //
};

class Sensitivity : Method
// sensitivity analysis
  { public :
    Sensitivity(double sensf, StrObject &sensrfn) :
      sensitivityRunId(0), sensRepOutputFilename(sensrfn), factorSens(sensf), paramSensId(0), paramSensLow(0.0), paramSensHigh(0.0), auxilSensId(0), auxilSensLow(0.0), auxilSensHigh(0.0), elasticity(0.0)
    {   if (constructionId == 0)
        {   sensRep.open(sensRepOutputFilename);
            // open the sens rep file
        }
        else
        {   sensRep.open(sensRepOutputFilename, ios::app);
            // open the sens rep file
        }
        constructionId++;
    }
    ~Sensitivity()
    {   sensRep.close();
        // close the sens rep file
    }
  private :
    void performSensitivityAnalysis(int *paramSens, int *auxilSens);
    //
    void initializeSensitivityAnalysis(StrObject *paramNames, double *paramValue);
    //
    void collectDataForSensitivityAnalysis(StrObject *auxilNames, SumObject *auxilSum);
    //
    void writeSensRepFileHeader();
    //
    void writeSensRepLine();
    //
    static long constructionId;
    //
    int sensitivityRunId;
    //
    int paramSensId;
    //
    int auxilSensId;
    //
    double factorSens;
    //
    double paramSensLow;
    //
    double paramSensHigh;
    //
    double auxilSensLow;
    //
    double auxilSensHigh;
    //
    double elasticity;
    //
    StrObject paramSensName;
    //
    StrObject auxilSensName;
    //
    StrObject sensRepOutputFilename;
    //
    ofstream sensRep;
    // declare the sens rep file
  friend class PCModel;
  //
};

class Calibration : Method
// calibration
  { public :
    Calibration(int calibt, int calibmi, double calibs, double caliba, StrObject &calibrfn, StrObject &optimrfn) :
      calibRepOutputFilename(calibrfn), optimRepOutputFilename(optimrfn), calibrationRunId(0),
      auxilCalibAim(0.0), auxilCalibMod(0.0), auxilCalibSsq(0.0),
      calibrationType(calibt), calibrationMaxIterations(calibmi), calibrationScale(calibs), calibrationAccuracy(caliba),
      ssq(0.0), param(0)
    {   paramCalibId = new int[MAX_PARAM];
        //
        paramCalibName = new StrObject[MAX_PARAM];
        //
        paramCalibTry = new double[MAX_PARAM];
        //
        paramType = new int[MAX_PARAM];
        //
        paramScale = new double[MAX_PARAM];
        //
        paramMinValue = new double[MAX_PARAM];
        //
        paramMaxValue = new double[MAX_PARAM];
        //
        paramDefaultValue = new double[MAX_PARAM];
        //
        if (constructionId == 0)
        {   calibRep.open(calibRepOutputFilename);
            // open the calib rep file
            optimRep.open(optimRepOutputFilename);
            // open the optim rep file
        }
        else
        if (constructionId == 0)
        {   calibRep.open(calibRepOutputFilename, ios::app);
            // open the calib rep file
            optimRep.open(optimRepOutputFilename, ios::app);
            // open the optim rep file
        }
        constructionId++;
    }    //
    ~Calibration()
    {   calibRep.close();
        // close the calib rep file
        optimRep.close();
        // close the optim rep filereturn;
        if (paramCalibId) delete[] paramCalibId;
        //
        if (paramCalibName) delete[] paramCalibName;
        //
        if (paramCalibTry) delete[] paramCalibTry;
        //
        if (paramType) delete[] paramType;
        //
        if (paramScale) delete[] paramScale;
        //
        if (paramMinValue) delete[] paramMinValue;
        //
        if (paramMaxValue) delete[] paramMaxValue;
        //
        if (paramDefaultValue) delete[] paramDefaultValue;
        //
    }
    double *param;
    //
    double ssq;
    //
    void writeCalibRepLine();
    //
  private :
    void performCalibration(int *paramCalib, double *paramMinValuePtr, double *paramMaxValuePtr, double *paramDefaultValuePtr, int *auxilCalib, double *auxilTargetValue);
    //
    void initializeCalibration(StrObject *paramNames, double *paramValue);
    //
    void collectDataForCalibration(StrObject *auxilNames, SumObject *auxilSum);
    //
    void writeCalibRepFileHeader();
    //
    void writeOptimRepFileHeader();
    //
    static long constructionId;
    //
    int calibrationRunId;
    //
    int auxilCalibId;
    //
    int calibrationMaxIterations;
    //
    int calibrationType;
    //
    int paramMax;
    //
    int *paramCalibId;
    //
    int *paramType;
    //
    double auxilCalibAim;
    //
    double auxilCalibMod;
    //
    double auxilCalibSsq;
    //
    double calibrationScale;
    //
    double calibrationAccuracy;
    //
    double *paramCalibTry;
    //
    double *paramScale;
    //
    double *paramMinValue;
    //
    double *paramMaxValue;
    //
    double *paramDefaultValue;
    //
    StrObject *paramCalibName;
    //
    StrObject auxilCalibName;
    //
    StrObject calibRepOutputFilename;
    //
    StrObject optimRepOutputFilename;
    //
    ofstream calibRep;
    // declare the calib rep file
    ofstream optimRep;
    // declare the optim rep file
  friend class PCModel;
  //
};

class Bifurcation : Method
// bifurcation
  { public :
    Bifurcation(StrObject &bifurrfn) :
      bifurRepOutputFilename(bifurrfn), bifurcationRunId(0)
    {   if (constructionId == 0)
        {   bifurRep.open(bifurRepOutputFilename);
            // open the bifur rep file;
        }
        else
        {   bifurRep.open(bifurRepOutputFilename, ios::app);
            // open the bifur rep file;
        }
        constructionId++;
    }
    //
    ~Bifurcation()
    {   bifurRep.close();
        // close the optim rep filereturn;
    }
    //
  private :
    void performBifurcationAnalysis(int *paramBifur, int *auxilBifur);
    //
    void initializeBifurcationAnalysis(StrObject *paramNames, double *paramValue, double *paramMinValue, double *paramMaxValue);
    //
    void collectDataForBifurcationAnalysis(StrObject *auxilNames, SumObject *auxilSum);
    //
    void writeBifurRepFileHeader();
    //
    void writeBifurRepLine();
    //
    static long constructionId;
    //
    int bifurcationRunId;
    //
    int paramBifurId;
    //
    int auxilBifurId;
    //
    double paramBifurValue;
    //
    double auxilBifurValue;
    //
    StrObject paramBifurName;
    //
    StrObject auxilBifurName;
    //
    StrObject bifurRepOutputFilename;
    //
    ofstream bifurRep;
    // declare the bifur rep file
  friend class PCModel;
  //
};

class Variable
{ public :
  protected :
    //StrObject *names;
    // declare a pointer to the array of names
    //int *rep;
    // declare a pointer to the array of ints the defines whether an initial state should be reported
    //double *value;
    // declare a pointer to initial state array
    //SumObject *sum;
    // declare a pointer to the array sumobjects that will hold the averages for the initial states
  private :
};

#define STATE_INP_LNG_ID                   0
#define STATE_INP_LNG_NUMBER               1
#define STATE_INP_INT_REP_INIT_STATE       0
#define STATE_INP_INT_REP_STATE            1
#define STATE_INP_INT_REP_DERIV            2
#define STATE_INP_INT_NUMBER               3
#define STATE_INP_STR_INIT_STATE_NAME      0
#define STATE_INP_STR_STATE_NAME           1
#define STATE_INP_STR_DERIV_NAME           2
#define STATE_INP_STR_STATE_UNIT           3
#define STATE_INP_STR_DEFAULT_0            4
#define STATE_INP_STR_SET_1                5
#define STATE_INP_STR_SET_2                6
#define STATE_INP_STR_SET_3                7
#define STATE_INP_STR_SET_CHANGED_0_1      8
#define STATE_INP_STR_SET_CHANGED_0_2      9
#define STATE_INP_STR_SET_CHANGED_0_3     10
#define STATE_INP_STR_DESCRIPTION         11
#define STATE_INP_STR_REMARKS             12
#define STATE_INP_STR_MUMBER              13

class InitialState : Variable
{ public :
    InitialState() {}
    ~InitialState() {}
  private :
  friend class PCModel;
  //
};

class State : Variable
{ public :
    State(int states, StrObject &stateifn) : stateSet(states), stateInputFilename(stateifn)
    //
    {   stateInp.readFile(stateInputFilename);
        // read the state ascii file
        maxState = stateInp.getRecordNumber();
        // set the maximum number of states
        initStateNames = new StrObject[maxState];
        // allocate memory for the array of state names
        stateNames = new StrObject[maxState];
        // allocate memory for the array of state names
        derivNames = new StrObject[maxState];
        // allocate memory for the array of derivative names
        initStateSum = new SumObject[maxState];
        // allocate memory for the array sumobjects that will hold the averages for the initial states
        stateSum = new SumObject[maxState];
        // allocate memory for the array sumobjects that will hold the averages for the states
        derivSum = new SumObject[maxState];
        // allocate memory for the array sumobjects that will hold the averages for the derivates
        initStateValue = new double[maxState * 4];
        // allocate memory for the state, derivative and initial state array, hence of length maxState * 3
        stateValue = initStateValue + maxState;
        // set a pointer to where the derivates start within the state/derivative/initial state array
        derivValue = stateValue + maxState;
        // set a pointer to where the initial states start within the state/derivative/initial state array
        derivScale = derivValue + maxState;
        // set a pointer to where the initial states start within the state/derivative/initial state array
        initStateRep = new int[maxState];
        // allocate memory for the array of ints the defines whether a state should be reported
        stateRep = new int[maxState];
        // allocate memory for the array of ints the defines whether a state should be reported
        derivRep = new int[maxState];
        // allocate memory for the array of ints the defines whether a derivative should be reported
    }
    ~State()
    {   if (initStateNames) delete[] initStateNames;
        // deallocate memory for the array of initial state names
        if (stateNames) delete[] stateNames;
        // deallocate memory for the array of state names
        if (derivNames) delete[] derivNames;
        // deallocate memory for the array of derivative names
        if (initStateSum) delete[] initStateSum;
        // deallocate memory for the array sumobjects that will hold the averages for the states
        if (stateSum) delete[] stateSum;
        // deallocate memory for the array sumobjects that will hold the averages for the states
        if (derivSum) delete[] derivSum;
        // deallocate memory for the array sumobjects that will hold the averages for the derivates
        if (initStateValue) delete[] initStateValue;
        // deallocate memory for the state, derivative and initial state array, hence of length maxState * 3
        if (initStateRep) delete[] initStateRep;
        // deallocate memory for the array of ints the defines whether a state should be reported
        if (stateRep) delete[] stateRep;
        // deallocate memory for the array of ints the defines whether a state should be reported
        if (derivRep) delete[] derivRep;
        // deallocate memory for the array of ints the defines whether a derivative should be reported
    }
  private :
    StrObject *stateNames;
    // declare a pointer to the array of state names
    int *stateRep;
    // declare a pointer to the array of ints the defines whether a state should be reported
    double *stateValue;
    // declare a pointer to the state array
    SumObject *stateSum;
    // declare a pointer to the array sumobjects that will hold the averages for the states

    int stateSet;
    //
    StrObject stateInputFilename;
    //
    int maxState;
    //
    AscObject stateInp;
    // contains an in memory copy of the text file with the state information
    static double stateInitRep[MAX_STATE][4];
    //

    StrObject *initStateNames;
    // declare a pointer to the array of derivative names
    double *initStateValue;
    // declare a pointer to derivates array
    int *initStateRep;
    // declare a pointer to the array of ints the defines whether a derivative should be reported
    SumObject *initStateSum;
    // declare a pointer to the array sumobjects that will hold the averages for the derivates

    //int maxInitState;
    //

    StrObject *derivNames;
    // declare a pointer to the array of derivative names
    double *derivValue;
    // declare a pointer to derivates array
    int *derivRep;
    // declare a pointer to the array of ints the defines whether a derivative should be reported
    SumObject *derivSum;
    // declare a pointer to the array sumobjects that will hold the averages for the derivates

    double *derivScale;
    // declare a pointer to derivates array
    static double derivInitRep[MAX_DERIV][4];
    //
    //int maxDeriv;
    //
  friend class PCModel;
  //
};

#define PARAM_INP_LNG_ID                   0
#define PARAM_INP_LNG_NUMBER               1
#define PARAM_INP_INT_REP_PARAM            0
#define PARAM_INP_INT_SENSITIVITY          1
#define PARAM_INP_INT_CALIBRATION          2
#define PARAM_INP_INT_BIFURCATION          3
#define PARAM_INP_INT_NUMBER               4
#define PARAM_INP_STR_PARAM_NAME           0
#define PARAM_INP_STR_PARAM_UNIT           1
#define PARAM_INP_STR_MIN_VALUE            2
#define PARAM_INP_STR_MAX_VALUE            3
#define PARAM_INP_STR_DEFAULT_0            4
#define PARAM_INP_STR_SET_1                5
#define PARAM_INP_STR_SET_2                6
#define PARAM_INP_STR_SET_3                7
#define PARAM_INP_STR_SET_CHANGED_0_1      8
#define PARAM_INP_STR_SET_CHANGED_0_2      9
#define PARAM_INP_STR_SET_CHANGED_0_3     10
#define PARAM_INP_STR_DESCRIPTION         11
#define PARAM_INP_STR_REMARKS             12
#define PARAM_INP_STR_NUMBER              13

class Parameter : Variable
{ public :
    Parameter(int params, StrObject &paramifn);
    ~Parameter();
  private :
    StrObject *paramNames;
    // declare a pointer to the array of parameter names
    int *paramRep;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    double *paramValue;
    // declare a pointer to the parameter array
    SumObject *paramSum;
    // declare a pointer to the array sumobjects that will hold the averages for the parameters

    AscObject *readParamInp;
    // declare a pointer to the array if input files that can contain the forcing functions
    int *readParamId;
    // declare a pointer to the array that contains the line number until where the forcing function file has been read

    int *paramSens;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    int *paramCalib;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    int *paramBifur;
    // declare a pointer to the array of ints the defines whether a parameter should be reported

    double *paramDefaultValue;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    double *paramMinValue;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    double *paramMaxValue;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    int paramSet;
    //
    StrObject paramInputFilename;
    //
    int maxParam;
    //
    AscObject paramInp;
    // contains an in memory copy of the text file with the parameter information
  friend class PCModel;
  //
  //friend class Sensitivity;
  //
};

#define FORCING_FUNCTION_DBL_TIME          0
#define FORCING_FUNCTION_DBL_VALUE         1
#define FORCING_FUNCTION_DBL_NUMBER        2

class ForcingFunction : Variable
{ public :
    ForcingFunction() {}
    ~ForcingFunction() {}
  private :
  friend class PCModel;
  //
};

#define INIT_AUXIL_INP_LNG_ID              0
#define INIT_AUXIL_INP_LNG_NUMBER          1
#define INIT_AUXIL_INP_INT_REP_INIT_AUXIL  0
#define INIT_AUXIL_INP_INT_NUMBER          1
#define INIT_AUXIL_INP_STR_INIT_AUXIL_NAME 0
#define INIT_AUXIL_INP_STR_INIT_AUXIL_UNIT 1
#define INIT_AUXIL_INP_STR_EQUATION        2
#define INIT_AUXIL_INP_STR_DESCRIPTION     3
#define INIT_AUXIL_INP_STR_REMARKS         4
#define INIT_AUXIL_INP_STR_NUMBER          5

class InitialAuxiliary : Variable
{ public :
    InitialAuxiliary(int initauxils, StrObject &initauxilifn) : initAuxilSet(initauxils), initAuxilInputFilename(initauxilifn)
    {   initAuxilInp.readFile(initAuxilInputFilename);
        // read the initAuxil ascii file
        maxInitAuxil = initAuxilInp.getRecordNumber();
        // set the maximum number of initial auxiliaries
        initAuxilNames = new StrObject[maxInitAuxil];
        // allocate memory for the array of initial auxiliary names
        initAuxilSum = new SumObject[maxInitAuxil];
        // allocate memory for the array sumobjects that will hold the averages for the initialauxiliaries
        initAuxilValue = new double[maxInitAuxil];
        // allocate memory for the initial auxiliaries array
        initAuxilRep = new int[maxInitAuxil];
        // allocate memory for the array of ints the defines whether a initial auxiliary should be reported
    }
    ~InitialAuxiliary()
    {   if (initAuxilNames) delete[] initAuxilNames;
        // deallocate memory for the array of initial auxiliary names
        if (initAuxilSum) delete[] initAuxilSum;
        // deallocate memory for the array sumobjects that will hold the averages for the initial auxiliaries
        if (initAuxilValue) delete[] initAuxilValue;
        // deallocate memory for the initial auxiliaries array
        if (initAuxilRep) delete[] initAuxilRep;
        // deallocate memory for the array of ints the defines whether a initial auxiliary should be reported
    }
    void readInitAuxil();
    //
  private :
    StrObject *initAuxilNames;
    // declare a pointer to the array of initial auxiliary names
    double *initAuxilValue;
    // declare a pointer to the initial auxiliaries array
    int *initAuxilRep;
    // declare a pointer to the array of ints the defines whether a initial auxiliary should be reported
    SumObject *initAuxilSum;
    // declare a pointer to the array sumobjects that will hold the averages for the initial auxiliaries
    int initAuxilSet;
    //
    StrObject initAuxilInputFilename;
    //
    int maxInitAuxil;
    //
    AscObject initAuxilInp;
    // contains an in memory copy of the text file with the initial auxiliary information
    static double initAuxilInitRep[MAX_INIT_AUXIL][4];
    //
  friend class PCModel;
};

#define AUXIL_INP_LNG_ID                   0
#define AUXIL_INP_LNG_NUMBER               1
#define AUXIL_INP_INT_REP_AUXIL            0
#define AUXIL_INP_INT_SENSITIVITY          1
#define AUXIL_INP_INT_CALIBRATION          2
#define AUXIL_INP_INT_BIFURCATION          3
#define AUXIL_INP_INT_NUMBER               4
#define AUXIL_INP_STR_AUXIL_NAME           0
#define AUXIL_INP_STR_AUXIL_UNIT           1
#define AUXIL_INP_STR_TARGET_VALUE         2
#define AUXIL_INP_STR_PROCESS              3
#define AUXIL_INP_STR_DESCRIPTION          4
#define AUXIL_INP_STR_REMARKS              5
#define AUXIL_INP_STR_NUMBER               6

class Auxiliary : Variable
{ public :
    Auxiliary(int auxils, StrObject &auxilifn);
    ~Auxiliary();
    int auxilSet;
    //
  private :
    StrObject *auxilNames;
    // declare a pointer to the array of auxiliary names
    int *auxilRep;
    // declare a pointer to the array of ints the defines whether a auxiliary should be reported
    double *auxilValue;
    // declare a pointer to the auxiliaries array
    SumObject *auxilSum;
    // declare a pointer to the array sumobjects that will hold the averages for the auxiliaries

    AscObject *readAuxilInp;
    // declare a pointer to the array if input files that can contain the data for the auxiliaries
    int *readAuxilId;
    // declare a pointer to the array that contains the line number until where the forcing function file has been read

    int *auxilSens;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    int *auxilCalib;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    int *auxilBifur;
    // declare a pointer to the array of ints the defines whether a parameter should be reported

    double *auxilTargetValue;
    // declare a pointer to the array of ints the defines whether a parameter should be reported
    StrObject auxilInputFilename;
    //
    int maxAuxil;
    //
    AscObject auxilInp;
    // contains an in memory copy of the text file with the auxiliary information
    static double auxilInitRep[MAX_AUXIL][4];
    //
  friend class PCModel;
  //
  //friend class Sensitivity;
  //
};

#define OBSERVED_DATA_DBL_TIME             0
#define OBSERVED_DATA_DBL_OBSERVED         1
#define OBSERVED_DATA_DBL_MODELLED         2
#define OBSERVED_DATA_DBL_NUMBER           3

class ObservedData : Variable
{ public :
    ObservedData() {}
    ~ObservedData() {}
  private :
  friend class PCModel;
  //
};

class PCModel
// class to define, run and analyze PCModel
{ public :
  // functions and data visible to the outside world
    PCModel();
    PCModel(long runi,
            int runt, int intm, int states, int params, int initauxils, int auxils, int calibt, int calibmi,
            double ints, double inta, double sensf, double calibs, double caliba,
            StrObject stateifn, StrObject paramifn, StrObject initauxilifn, StrObject auxilifn,
            StrObject setrfn, StrObject initrfn, StrObject dynrfn, StrObject avgrfn,
            StrObject sensrfn, StrObject calibrfn, StrObject optimrfn, StrObject bifurrfn);
    // constructor of class PCModel
    ~PCModel();
    // destructor of class PCModel
  // should be accessible to OSIRIS
    static void constructAnalyser();
    //
    int main();
    //
    static void destructAnalyser();
    //
    void startSys();
    // do what is necessary to start the simulation

    void repSys(double taskTime);
    // write time dependent output
    void colAvgSys(double taskTime, double averageStartWithinYear, double averageEndWithinYear);
    // collect the averages of the time dependent output
    void repAvgSys();
    // write time dependent average output
    void stopSys();
    // do what is necessary to stop the simulation
    void performOneIntegrationStep(double &taskTime);
    //
    void readForcingFunction(double taskTime, double nextTaskTime, long number, int &stopTask);
    //
    void compareWithData(double taskTime, double &nextTaskTime, long number, int &stopTask);
    //
  // should be accessible to the integrator
    void calculateAuxilDef(double t, double *state, double *param, double *auxil, double *deriv);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    void calculateAuxilSet0(double t, double *state, double *param, double *auxil, double *deriv);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    void calculateAuxilSet1(double t, double *state, double *param, double *auxil, double *deriv);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    void calculateAuxilSet2(double t, double *state, double *param, double *auxil, double *deriv);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    void calculateAuxilSet3(double t, double *state, double *param, double *auxil, double *deriv);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
  // Objects
    Auxiliary auxil;
    //
    Calibration calib;
    //
    long runId;
    //
  private :
    int runType;
    //
    long starttime;
    // data member that holds the start time of the simulation
    long endtime;
    // data member that holds the end time of the simulation
  // Objects
    SingleRun single;
    //
    Sensitivity sens;
    //
    Bifurcation bifur;
    //
    State state;
    //
    Parameter param;
    //
    ForcingFunction forcingFunc;
    //
    InitialAuxiliary initAuxil;
    //
    ObservedData obsData;
    //
  // functions related to simulation
    void setStartTime() {time(&starttime);}
    // member function that sets the start time of the simulation
    void setEndTime() {time(&endtime);}
    // member function that sets the end time of the simulation
    long getRunTime() {return endtime - starttime;}
    // member function that returns the timespan of the simulation
    void storeInitValues();
    //
  // functions to initialize a simulation
    void setDefaultValues();
    // perform the function that reads the default values for the states and parameters from the c++ code
    void calculateInitAuxil();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateInitAuxilDef();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateInitAuxilSet0();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateInitAuxilSet1();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateInitAuxilSet2();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateInitAuxilSet3();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    void calculateAuxil(double t);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    void readInitStates();
    //
    void readStates();
    //
    void readDerivs();
    //
    void readParameters();
    //
    void readAuxil();
    //
  // functions to write file headers
    void writeSetRepFileHeader();
    //
    void writeInitRepFileHeader();
    //
    void writeDynFileHeader();
    // write the header of the file with the time dependent information
    void writeAvgFileHeader();
    // write the header of the file with the averages
    void initializeMemory();
};

double linIntPol(double tx, double t0, double t1, double x0, double x1);
// function for linear interpolation
int setNewReadParamSysTask(long id, double startTime);
//
int setNewCompDataSysTask(long id, double startTime);
//
void performOneSimulation();
//

#endif

