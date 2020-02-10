#include "pcmodel_ditch.h"
// header file containing specification of the pcmodel class

int intCounter = 0;
double intRepArray[1000][4];

ofstream steprep;
//
long SingleRun::constructionId = 0;
//
long Sensitivity::constructionId = 0;
//
long Calibration::constructionId = 0;
//
long Bifurcation::constructionId = 0;
//
double Parameter::paramInitRep[MAX_PARAM][4];
// array that holds the parameter data for the dump file
double State::initStateInitRep[MAX_STATE][4];
// array that holds the initial state data for the dump file
double InitialAuxiliary::initAuxilInitRep[MAX_INIT_AUXIL][4];
// array that holds the initAuxil data for the dump file
double State::stateInitRep[MAX_STATE][4];
// array that holds the state data for the dump file
double Auxiliary::auxilInitRep[MAX_AUXIL][4];
// array that holds the auxil data for the dump file
double State::derivInitRep[MAX_DERIV][4];
// array that holds the deriv data for the dump file

Parameter::Parameter(int params, StrObject &paramifn) : paramSet(params), paramInputFilename(paramifn)
{   paramInp.readFile(paramInputFilename);
    // read the parameter ascii file
    maxParam = paramInp.getRecordNumber();
    // set the maximum number of parameters
    paramNames = new StrObject[maxParam];
    // allocate memory for the array of parameter names
    paramSum = new SumObject[maxParam];
    // allocate memory for the array sumobjects that will hold the averages for the parameters
    paramValue = new double[maxParam];
    // allocate memory for the parameter array
    paramRep = new int[maxParam];
    // allocate memory for the array of ints the defines whether a parameter should be reported
    readParamInp = new AscObject[maxParam];
    // allocate memory for the array if input files that can contain the forcing functions
    readParamId = new int[maxParam];
    // allocate memory for the array that contains the line number until where the forcing function file has been read
    paramSens = new int[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the sensitivity analysis as input
    paramCalib = new int[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the calibration analysis as input
    paramBifur = new int[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the bifurcation analysis as input
    paramMinValue = new double[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the sensitivity analysis as input
    paramMaxValue = new double[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the sensitivity analysis as input
    paramDefaultValue = new double[maxParam];
    // allocate memory for the array of ints that defines whether a parameter is part of the sensitivity analysis as input
    int i;
    //
    for (i = 0; i < MAX_PARAM; i++)
    // go over all the input parameters
    {   paramSens[i] = paramInp.getInt(i, PARAM_INP_INT_SENSITIVITY);
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
        paramCalib[i] = paramInp.getInt(i, PARAM_INP_INT_CALIBRATION);
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
        paramBifur[i] = paramInp.getInt(i, PARAM_INP_INT_BIFURCATION);
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
        paramMinValue[i] = atof(paramInp.getStr(i, PARAM_INP_STR_MIN_VALUE));
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
        paramMaxValue[i] = atof(paramInp.getStr(i, PARAM_INP_STR_MAX_VALUE));
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
        paramDefaultValue[i] = atof(paramInp.getStr(i, PARAM_INP_STR_DEFAULT_0));
        // fill the array of ints that defines whether a parameter is part of the sensitivity analysis as input
    }
}

Parameter::~Parameter()
{   if (paramNames) delete[] paramNames;
    // deallocate memory for the array of parameter names
    if (paramSum) delete[] paramSum;
    // deallocate memory for the array sumobjects that will hold the averages for the parameters
    if (paramValue) delete[] paramValue;
    // deallocate memory for the parameter array
    if (paramRep) delete[] paramRep;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (readParamInp) delete[] readParamInp;
    // deallocate memory for the array if input files that can contain the forcing functions
    if (readParamId) delete[] readParamId;
    // deallocate memory for the array that contains the line number until where the forcing function file has been read
    if (paramSens) delete[] paramSens;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (paramCalib) delete[] paramCalib;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (paramBifur) delete[] paramBifur;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (paramMinValue) delete[] paramMinValue;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (paramMaxValue) delete[] paramMaxValue;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (paramDefaultValue) delete[] paramDefaultValue;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
}

Auxiliary::Auxiliary(int auxils, StrObject &auxilifn) : auxilSet(auxils), auxilInputFilename(auxilifn)
{   auxilInp.readFile(auxilInputFilename);
    // read the auxiliary ascii file
    maxAuxil = auxilInp.getRecordNumber() - MAX_STATE; // <----------------------------------------------------
    // set the maximum number of auxiliaries
    auxilNames = new StrObject[maxAuxil];
    // allocate memory for the array of auxiliary names
    auxilSum = new SumObject[maxAuxil];
    // allocate memory for the array sumobjects that will hold the averages for the auxiliaries
    auxilValue = new double[maxAuxil];
    // allocate memory for the auxiliaries array
    auxilRep = new int[maxAuxil];
    // allocate memory for the array of ints the defines whether a auxiliary should be reported
    readAuxilInp = new AscObject[maxAuxil];
    // allocate memory for the array if input files that can contain the data for the auxiliaries
    readAuxilId = new int[maxAuxil];
    // allocate memory for the array that contains the line number until where the forcing function file has been read
    auxilSens = new int[maxAuxil];
    // allocate memory for the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
    auxilCalib = new int[maxAuxil];
    // allocate memory for the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
    auxilBifur = new int[maxAuxil];
    // allocate memory for the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
    auxilTargetValue = new double[maxAuxil];
    // allocate memory for the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
    int i;
    //
    for (i = 0; i < MAX_AUXIL; i++)
    // go over all the auxiliaries
    {   auxilSens[i] = auxilInp.getInt(i, AUXIL_INP_INT_SENSITIVITY);
        // fill the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
        auxilCalib[i] = auxilInp.getInt(i, AUXIL_INP_INT_CALIBRATION);
        // fill the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
        auxilBifur[i] = auxilInp.getInt(i, AUXIL_INP_INT_BIFURCATION);
        // fill the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
        auxilTargetValue[i] = atof(auxilInp.getStr(i, AUXIL_INP_STR_TARGET_VALUE));
        // fill the array of ints the defines whether an auxiliary is part of the sensitivity analysis as output
    }
}

Auxiliary::~Auxiliary()
{   if (auxilNames) delete[] auxilNames;
    // deallocate memory for the array of auxiliary names
    if (auxilSum) delete[] auxilSum;
    // deallocate memory for the array sumobjects that will hold the averages for the auxiliaries
    if (auxilValue) delete[] auxilValue;
    // deallocate memory for the auxiliaries array
    if (auxilRep) delete[] auxilRep;
    // deallocate memory for the array of ints the defines whether a auxiliary should be reported
    if (readAuxilInp) delete[] readAuxilInp;
    // deallocate memory for the array if input files that can contain the forcing functions
    if (readAuxilId) delete[] readAuxilId;
    // deallocate memory for the array that contains the line number until where the forcing function file has been read
    if (auxilSens) delete[] auxilSens;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (auxilCalib) delete[] auxilCalib;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (auxilBifur) delete[] auxilBifur;
    // deallocate memory for the array of ints the defines whether a parameter should be reported
    if (auxilTargetValue) delete[] auxilTargetValue;
    // deallocate memory for the array of ints the defines whether a parameter should be reported

}

PCModel *pcmodel;

PCModel::PCModel(long runi, // long arguments
                 int runt, int intm, int states, int params, int initauxils, int auxils, int calibt, int calibmi, // int arguments
                 double ints, double inta, double sensf, double calibs, double caliba, // double arguments
                 StrObject stateifn, StrObject paramifn, StrObject initauxilifn, StrObject auxilifn, // input filenames
                 StrObject setrfn, StrObject initrfn, StrObject dynrfn, StrObject avgrfn, // output filenames
                 StrObject sensrfn, StrObject calibrfn, StrObject optimrfn, StrObject bifurrfn) : // output filenames

                 runId(runi), runType(runt), // data members PCModel

                 single(intm, ints, inta, setrfn, initrfn, dynrfn, avgrfn), // data members SingleRun
                 sens(sensf, sensrfn), // data members Sensitivity
                 calib(calibt, calibmi, calibs, caliba, calibrfn, optimrfn), // data members Calibration
                 bifur(bifurrfn), // data members Bifurcation

                 state(states, stateifn), // data members State
                 param(params, paramifn), // data members Parameter
                 initAuxil(initauxils, initauxilifn), // data members InitialAuxiliary
                 auxil(auxils, auxilifn)// data members Auxiliary
{   cout << "Constructor PCModel" << endl;
    // write to the screen
    pcmodel = this;
    // assign the global pointer to a model object with a pointer to this model object
}

PCModel::~PCModel()
{   cout << "Destructor PCModel" << endl;
    // write to the screen
}

void PCModel::constructAnalyser()
//
{  steprep.open("txt/steprep.txt");
   //
   steprep << "t" << "\t"
            << "tintnext" << "\t"
            << "sstore" << "\t"
            << "sstored" << "\t"
            << "stry" << "\t"
            << "sdid" << "\t"
            << "snext" << "\t" << endl;
}

//#define PRESS_ANY_KEY_TO_EXIT

void PCModel::destructAnalyser()
//
{   ofstream intRep;
    intRep.open("txt/intrep.txt");
    int i, j;
    for (i = 0; i < 1000; i++)
    {   intRep << i;
        for (j = 0; j < 4; j++) intRep << " " << intRepArray[i][j];
        intRep << endl;
    }
    intRep.close();
    #ifdef PRESS_ANY_KEY_TO_EXIT
    OutObject::pressAnyKey();
    //
#else
    long waittime;
    //
    long endwaittime = time(&waittime) + 3;
    //
    while (waittime < endwaittime) time(&waittime);
    //
#endif
}

int PCModel::main()
//
{   if (runType == 0) single.performSingleRun();
    //
    else if(runType == 1) sens.performSensitivityAnalysis(param.paramSens, auxil.auxilSens);
    //
    else if(runType == 2) calib.performCalibration(param.paramCalib, param.paramMinValue, param.paramMaxValue, param.paramDefaultValue, auxil.auxilCalib, auxil.auxilTargetValue);
    //
    else if(runType == 3) bifur.performBifurcationAnalysis(param.paramBifur, auxil.auxilBifur);
    //
    else cout << "Unknown run type ";
    //
    return runType;
    //
}

///////////////////////////////SINGLERUN////////////////////////////////////////

void SingleRun::performSingleRun()
//
{   performOneSimulation();
    //
}

///////////////////////////////SENSIIVITY///////////////////////////////////////

void Sensitivity::performSensitivityAnalysis(int *paramSens, int *auxilSens)
// perform a sensitivity analysis
{   cout << ": run type sensitivity analysis" << endl;
    // write to the screen
    int i, j;
    // declare loop parameters
    for (i = 0; i < MAX_AUXIL; i++) if (auxilSens[i])
    //  loop over the auxiliaries that are part of the sensitivity analysis as output
    {   for (j = 0; j < MAX_PARAM; j++) if (paramSens[j])
        // loop over the paramaters that are part of the sensitivity analysis as input
        {   paramSensId = j;
            // store the id of the focal parameter in parSensId for use during input manipulation
            auxilSensId = i;
            // store the id of the focal auxiliary in auxilSensId for use during output collection
            cout << "calculating sensitivity for " << i << " " << j << endl;
            //
            sensitivityRunId++;
            // set the sensitivityRunId to an odd value non zero value for checking the low parameter value
            performOneSimulation();
            // perform a simulation
            sensitivityRunId++;
            // set the sensitivityRunId to an even non zero value for checking the low parameter value
            performOneSimulation();
            // perform a simulation
            elasticity = ((auxilSensHigh - auxilSensLow + 1.0e-12) / (0.5 * auxilSensHigh + 0.5 * auxilSensLow + 1.0e-12)) / ((paramSensHigh - paramSensLow + 1.0e-12) / (0.5 * paramSensHigh + 0.5 * paramSensLow + 1.0e-12));
            // calculate the elasticity of the focal output auxiliary to the focal input parameter
            writeSensRepLine();
            // write a line to the sensitivity report
        }
    }
}

void Sensitivity::initializeSensitivityAnalysis(StrObject *paramNames, double *paramValue)
//
{   paramSensName = paramNames[paramSensId];
    //
    if (sensitivityRunId % 2 == 1) paramSensLow = paramValue[paramSensId] = paramValue[paramSensId] * (1.0 - factorSens);
    //
    else if (sensitivityRunId % 2 == 0) paramSensHigh = paramValue[paramSensId] = paramValue[paramSensId] * (1.0 + factorSens);
    //
}

void Sensitivity::collectDataForSensitivityAnalysis(StrObject *auxilNames, SumObject *auxilSum)
//
{   auxilSensName = auxilNames[auxilSensId];
    //
    if (sensitivityRunId % 2 == 1) auxilSensLow = auxilSum[auxilSensId].getAvgX();
    //
    else if (sensitivityRunId % 2 == 0) auxilSensHigh = auxilSum[auxilSensId].getAvgX();
    //
}

void Sensitivity::writeSensRepFileHeader()
//
{   sensRep << "RunId "
            << "SensitivityRunId "
            << "FactorSens "
            << "ParamSensId "
            << "ParamSensName "
            << "ParamSensLow "
            << "ParamSensHigh "
            << "AuxilSensId "
            << "AuxilSensName "
            << "AuxilSensLow "
            << "AuxilSensHigh "
            << "Elasticity" << endl;
    //
}

void Sensitivity::writeSensRepLine()
{   sensRep << pcmodel->runId << " "
            << sensitivityRunId << " "
            << factorSens << " "
            << paramSensId << " "
            << paramSensName << " "
            << paramSensLow << " "
            << paramSensHigh <<  " "
            << auxilSensId << " "
            << auxilSensName << " "
            << auxilSensLow << " "
            << auxilSensHigh << " "
            << elasticity << endl;
}

///////////////////////////////SENSIIVITY///////////////////////////////////////

///////////////////////////////CALIBRATION//////////////////////////////////////
double calculateFit(double *p)
{   pcmodel->calib.param = p;
    performOneSimulation();
    pcmodel->calib.writeCalibRepLine();
    return pcmodel->calib.ssq;
}

void Calibration::performCalibration(int *paramCalib, double *paramMinValuePtr, double *paramMaxValuePtr, double *paramDefaultValuePtr, int *auxilCalib, double *auxilTargetValue)
//
{   cout << ": run type calibration" << endl;
    //
    int i, j;
    //
    for (i = 0; i < MAX_AUXIL; i++) if (auxilCalib[i])
    //  loop over the auxiliaries that are part of the sensitivity analysis as output
    {   auxilCalibId = i;
        // store the id of the focal auxiliary in auxilSens for use during input manipulation and output collection
        auxilCalibAim = auxilTargetValue[i];
        //
        for (j = 0; j < MAX_PARAM; j++) if (paramCalib[j])
        // loop over the paramaters that are part of the sensitivity analysis as input
        {   paramMax = 1;
            //
            cout << "calib " << i << " " << j << " " << paramMax << endl;
            //
            int p = 0;
            //
            paramCalibId[p] = j;
            // store the ids of the focal parameters in parSens for use during input manipulation and output collection
            paramType[p] = calibrationType;
            //
            paramMinValue[p] = paramMinValuePtr[j];
            //
            paramMaxValue[p] = paramMaxValuePtr[j];
            //
            paramDefaultValue[p] = paramDefaultValuePtr[j];
            //
            paramScale[p] = (paramMaxValue[p] - paramMinValue[p]) * calibrationScale;
            //
            calibrationRunId = 1;
            //
            OptObject myOptimizer(optimRep,
                                  calculateFit,
                                  paramMax,
                                  paramType,
                                  paramScale,
                                  paramMinValue,
                                  paramMaxValue,
                                  calibrationAccuracy,
                                  calibrationMaxIterations);
            //
            myOptimizer.minimize(paramDefaultValue);
            //
        }
    }
    calibrationRunId = 0;
    //
}

void Calibration::initializeCalibration(StrObject *paramNames, double *paramValue)
//
{   int i;
    //
    for (i = 0; i < paramMax; i++)
    {   paramCalibName[i] = paramNames[paramCalibId[i]];
        //
        paramCalibTry[i] = paramValue[paramCalibId[i]] = param[i];
        //
    }
}

void Calibration::collectDataForCalibration(StrObject *auxilNames, SumObject *auxilSum)
//
{   auxilCalibName = auxilNames[auxilCalibId];
    //
    auxilCalibMod = auxilSum[auxilCalibId].getAvgX();
    //
    auxilCalibSsq = pow(auxilCalibMod - auxilCalibAim, 2) + 1.0;
    //
    ssq = auxilCalibSsq;
}

void Calibration::writeCalibRepFileHeader()
//
{   int i;
    //
    calibRep << "RunId "
             << "CalibrationRunId ";
    for (i = 0; i < paramMax; i++)
    {   calibRep << "ParamCalibId_" << i << " "
                 << "ParamCalibName_" << i << " "
                 << "ParamCalibTry_" << i << " ";
    }
    calibRep << "AuxilCalibId "
             << "AuxilCalibName "
             << "AuxilCalibMod "
             << "AuxilCalibSsq " << endl;
    //
}

void Calibration::writeCalibRepLine()
{   int i;
    //
    calibRep << pcmodel->runId << " "
             << calibrationRunId << " ";
    for (i = 0; i < paramMax; i++)
    {   calibRep << paramCalibId[i] << " "
                 << paramCalibName[i] << " "
                 << paramCalibTry[i] <<  " ";
    }
    calibRep << auxilCalibId << " "
             << auxilCalibName << " "
             << auxilCalibMod << " "
             << auxilCalibSsq << endl;

}

void Calibration::writeOptimRepFileHeader()
//
{
}

///////////////////////////////CALIBRATION//////////////////////////////////////

///////////////////////////////BIFURCATION//////////////////////////////////////

int maxBifurIntervals = 20;

void Bifurcation::performBifurcationAnalysis(int *paramBifur, int *auxilBifur)
// perform a bifurcation analysis
{   cout << ": run type bifurcation analysis" << endl;
    // write to the screen
    int i, j, k;
    // declare loop parameters
    for (i = 0; i < MAX_AUXIL; i++) if (auxilBifur[i])
    //  loop over the auxiliaries that are part of the sensitivity analysis as output
    {   for (j = 0; j < MAX_PARAM; j++) if (paramBifur[j])
        // loop over the paramaters that are part of the sensitivity analysis as input
        {   paramBifurId = j;
            // store the id of the focal parameter in parSensId for use during input manipulation
            auxilBifurId = i;
            // store the id of the focal auxiliary in auxilSensId for use during output collection
            cout << "calculating bifurcation for " << i << " " << j << endl;
            //
            for (k = 0; k <= maxBifurIntervals; k++)
            {   bifurcationRunId++;
                //
                performOneSimulation();
                // perform a simulation
                writeBifurRepLine();
                // write a line to the sensitivity report
            }
        }
    }
}

void Bifurcation::initializeBifurcationAnalysis(StrObject *paramNames, double *paramValue, double *paramMinValue, double *paramMaxValue)
//
{   paramBifurName = paramNames[paramBifurId];
    //
    //if (maxBifurIntervals) paramBifurValue = paramValue[paramBifurId] = paramMinValue[paramBifurId] + (double(bifurcationRunId - 1) / double(maxBifurIntervals)) * (paramMaxValue[paramBifurId] - paramMinValue[paramBifurId]);
    //
    if (pcmodel->runId % 2 == 0)
    //
    {    cout << "a" << endl;
         if (maxBifurIntervals) paramBifurValue = paramValue[paramBifurId] = paramMinValue[paramBifurId] + (double(bifurcationRunId - 1) / double(maxBifurIntervals)) * (paramMaxValue[paramBifurId] - paramMinValue[paramBifurId]);
         //
    }
    else
    //
    {    if (maxBifurIntervals) paramBifurValue = paramValue[paramBifurId] = paramMaxValue[paramBifurId] - (double(bifurcationRunId - 1) / double(maxBifurIntervals)) * (paramMaxValue[paramBifurId] - paramMinValue[paramBifurId]);
         //
    }
    //
}

void Bifurcation::collectDataForBifurcationAnalysis(StrObject *auxilNames, SumObject *auxilSum)
//
{   auxilBifurName = auxilNames[auxilBifurId];
    //
    auxilBifurValue = auxilSum[auxilBifurId].getAvgX();
}

void Bifurcation::writeBifurRepFileHeader()
//
{   bifurRep << "RunId "
             << "BifurcationRunId "
             << "ParamBifurId "
             << "ParamBifurName "
             << "ParamBifurValue "
             << "AuxilBifurId "
             << "AuxilBifurName "
             << "AuxilBifurValue" << endl;
    //
}

void Bifurcation::writeBifurRepLine()
{   bifurRep << pcmodel->runId << " "
             << bifurcationRunId << " "
             << paramBifurId << " "
             << paramBifurName << " "
             << paramBifurValue << " "
             << auxilBifurId << " "
             << auxilBifurName << " "
             << auxilBifurValue << endl;
}

///////////////////////////////BIFURCATION//////////////////////////////////////

int header1NotYetWritten = 1;
int header2NotYetWritten = 1;

void PCModel::startSys()
// do what is necessary to start the simulation
{   intCounter = 0;
    if (header1NotYetWritten)
    // if the run id = 0
    {   writeSetRepFileHeader();
        //
        writeInitRepFileHeader();
        //
        sens.writeSensRepFileHeader();
        //
        calib.writeCalibRepFileHeader();
        //
        calib.writeOptimRepFileHeader();
        //
        bifur.writeBifurRepFileHeader();
        //
        header1NotYetWritten = 0;
    }
    initializeMemory();
    //
    setDefaultValues();
    // perform the function that reads the default values for the states and parameters from the c++ code
    readInitStates();
    //
    readStates();
    //
    readDerivs();
    //
    readParameters();
    //
    if (sens.sensitivityRunId) sens.initializeSensitivityAnalysis(param.paramNames, param.paramValue);
    //
    else if (calib.calibrationRunId) calib.initializeCalibration(param.paramNames, param.paramValue);
    //
    else if (bifur.bifurcationRunId) bifur.initializeBifurcationAnalysis(param.paramNames, param.paramValue, param.paramMinValue, param.paramMaxValue);
    //
    initAuxil.readInitAuxil();
    //
    readAuxil();
    //
    calculateInitAuxil();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    calculateAuxil(0.0);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    single.setRep << -1 << endl;
    // write the last line to the set rep file
    storeInitValues();
    //
    if (header2NotYetWritten)
    // if the run id = 0
    {   writeDynFileHeader();
        // write the start of the header of the file with time dependent output
        writeAvgFileHeader();
        // write the start of the header of the file with averages
        header2NotYetWritten = 0;
    }
    setStartTime();
    // record the start time
}

void PCModel::stopSys()
// do what is necessary to stop the simulation
{   setEndTime();
    // record the end time
    single.dynRep << -1 << endl;
    single.avgRep << -1 << endl;
    cout << " time elapsed: " << getRunTime() << endl;
}

void PCModel::initializeMemory()
{   int i;
    // declare loop variable
    for (i = 0; i < state.maxState * 4; i++) state.initStateValue[i] = 0.0;
    // initialize memory for the state, derivative and initial state array, hence of length maxState * 3
    for (i = 0; i < param.maxParam; i++) param.paramValue[i] = 0.0;
    // initialize memory for the parameter array
    for (i = 0; i < initAuxil.maxInitAuxil; i++) initAuxil.initAuxilValue[i] = 0.0;
    // initialize memory for the initial auxiliaries array
    for (i = 0; i < auxil.maxAuxil; i++) auxil.auxilValue[i] = 0.0;
    // initialize memory for the auxiliaries array
    for (i = 0; i < state.maxState; i++) state.initStateRep[i] = 0;
    // initialize memory for the array of ints the defines whether a state should be reported
    for (i = 0; i < state.maxState; i++) state.stateRep[i] = 0;
    // initialize memory for the array of ints the defines whether a state should be reported
    for (i = 0; i < state.maxState; i++) state.derivRep[i] = 0;
    // initialize memory for the array of ints the defines whether a derivative should be reported
    for (i = 0; i < param.maxParam; i++) param.paramRep[i] = 0;
    // initialize memory for the array of ints the defines whether a parameter should be reported
    for (i = 0; i < initAuxil.maxInitAuxil; i++) initAuxil.initAuxilRep[i] = 0;
    // initialize memory for the array of ints the defines whether a initial auxiliary should be reported
    for (i = 0; i < auxil.maxAuxil; i++) auxil.auxilRep[i] = 0;
    // initialize memory for the array of ints the defines whether a auxiliary should be reported
    for (i = 0; i < param.maxParam; i++) param.readParamId[i] = 0;
    // initialize memory for the array that contains the line number until where the forcing function file has been read
    for (i = 0; i < auxil.maxAuxil; i++) auxil.readAuxilId[i] = 0;
    // initialize memory for the array that contains the line number until where the forcing function file has been read
}

void PCModel::calculateInitAuxil()
{   if (initAuxil.initAuxilSet == -1) calculateInitAuxilDef();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    else if (initAuxil.initAuxilSet == 0) calculateInitAuxilSet0();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    else if (initAuxil.initAuxilSet == 1) calculateInitAuxilSet1();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    else if (initAuxil.initAuxilSet == 2) calculateInitAuxilSet2();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    else if (initAuxil.initAuxilSet == 3) calculateInitAuxilSet3();
    // perform the function that calculates initial auxiliaries and expands the small state input set to the full input state set if initcalc is set to 1
    else OutObject::printError("Illegal id of initial auxiliary set");
}

void PCModel::calculateAuxil(double t)
{   if (auxil.auxilSet == -1) calculateAuxilDef(t, state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    else if (auxil.auxilSet == 0) calculateAuxilSet0(t, state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    else if (auxil.auxilSet == 1) calculateAuxilSet1(t, state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    else if (auxil.auxilSet == 2) calculateAuxilSet2(t, state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    else if (auxil.auxilSet == 3) calculateAuxilSet3(t, state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue);
    // perform the function that calculates the auxiliaries and the derivatives so that these are already known at t=0
    else OutObject::printError("Illegal id of auxiliary set");
}

void PCModel::writeSetRepFileHeader()
//
{   single.setRep << "RunId" << "\t"
                      << "Identifier" << "\t"
                      << "Default" << "\t"
                      << "Value" << "\t"
                      << "Difference" << "\t"
                      << -1 << endl;
    // write the header of the set rep file
}

void PCModel::writeInitRepFileHeader()
//
{   single.initRep << "Type Id Name Set0 Set1 Set2 Set3 Rel0-1 Rel0-2 Rel0-3 Dif0-1 Dif0-2 Dif0-3" << endl;
    //
}

void PCModel::writeDynFileHeader()
// write the start of the header of the file with time dependent output
{   int i;
    single.dynRep << "RunId" << "\t" << "Time";
    for (i = 0; i < state.maxState; i++) if (state.initStateRep[i] == 1) single.dynRep << "\t" << state.initStateNames[i];
    // for all initial states if this initial state should be reported, write the state name in the header
    for (i = 0; i < state.maxState; i++) if (state.stateRep[i] == 1) single.dynRep << "\t" << state.stateNames[i];
    // for all states if this state should be reported, write the state name in the header
    for (i = 0; i < state.maxState; i++) if (state.derivRep[i] == 1) single.dynRep << "\t" << state.derivNames[i];
    // for all derivatives if this derivative should be reported, write the derivative name in the header
    for (i = 0; i < param.maxParam; i++) if (param.paramRep[i] == 1) single.dynRep << "\t" << param.paramNames[i];
    // for all parameters if this parameter should be reported, write the parameter name in the header
    for (i = 0; i < initAuxil.maxInitAuxil; i++) if (initAuxil.initAuxilRep[i] == 1) single.dynRep << "\t" << initAuxil.initAuxilNames[i];
    // for all initial auxiliaries if this initial auxiliary should be reported, write the initial auxiliary name in the header
    for (i = 0; i < auxil.maxAuxil; i++) if (auxil.auxilRep[i] == 1) single.dynRep << "\t" << auxil.auxilNames[i];
    // for all auxiliaries if this auxiliary should be reported, write the auxiliary name in the header
    single.dynRep << "\t" << -1 << endl;
    // write the last line of the file with time dependent output
}

void PCModel::writeAvgFileHeader()
// write the start of the header of the file with averages
{   int i;
    single.avgRep << "RunId Time";
    for (i = 0; i < state.maxState; i++) if (state.initStateRep[i] == 1) single.avgRep << "\t" << state.initStateNames[i];
    // for all initial states if this initial state should be reported, write the state name in the header
    for (i = 0; i < state.maxState; i++) if (state.stateRep[i] == 1) single.avgRep << "\t" << state.stateNames[i];
    // for all states if this state should be reported, write the state name in the header
    for (i = 0; i < state.maxState; i++) if (state.derivRep[i] == 1) single.avgRep << "\t" << state.derivNames[i];
    // for all derivatives if this derivative should be reported, write the derivative name in the header
    for (i = 0; i < param.maxParam; i++) if (param.paramRep[i] == 1) single.avgRep << "\t" << param.paramNames[i];
    // for all parameters if this parameter should be reported, write the parameter name in the header
    for (i = 0; i < initAuxil.maxInitAuxil; i++) if (initAuxil.initAuxilRep[i] == 1) single.avgRep << "\t" << initAuxil.initAuxilNames[i];
    // for all initial auxiliaries if this initial auxiliary should be reported, write the initial auxiliary name in the header
    for (i = 0; i < auxil.maxAuxil; i++) if (auxil.auxilRep[i] == 1) single.avgRep << "\t" << auxil.auxilNames[i];
    // for all auxiliaries if this auxiliary should be reported, write the auxiliary name in the header
    single.avgRep << "\t" << -1 << endl;
    // write the last line of the file with averages
}

int runIdInitRep = 0;

void PCModel::storeInitValues()
{   int i, j;
    if (runIdInitRep >= 0 && runIdInitRep <= 3)
    {   for (i = 0; i < param.maxParam; i++) param.paramInitRep[i][runIdInitRep] = param.paramValue[i];
        for (i = 0; i < state.maxState; i++) state.initStateInitRep[i][runIdInitRep] = state.initStateValue[i];
        for (i = 0; i < initAuxil.maxInitAuxil; i++) initAuxil.initAuxilInitRep[i][runIdInitRep] = initAuxil.initAuxilValue[i];
        for (i = 0; i < state.maxState; i++) state.stateInitRep[i][runIdInitRep] = state.stateValue[i];
        for (i = 0; i < auxil.maxAuxil; i++) auxil.auxilInitRep[i][runIdInitRep] = auxil.auxilValue[i];
        for (i = 0; i < state.maxState; i++) state.derivInitRep[i][runIdInitRep] = state.derivValue[i];
        if (runIdInitRep == 3)
        {
//            for (i = 0; i < param.maxParam; i++)
//            {   single.initRep << "Parameter" << " " << i << " " << param.paramNames[i];
//                for (j = 0; j < 4; j++) single.initRep << " " << param.paramValue[i];
//                for (j = 1; j < 4; j++) single.initRep << " 0";
//                for (j = 1; j < 4; j++) single.initRep << " 0";
//                single.initRep << endl;
//            }
            for (i = 0; i < param.maxParam; i++)
            {   single.initRep << "Parameter" << " " << i << " " << param.paramNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << param.paramInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((param.paramInitRep[i][j] - param.paramInitRep[i][0]) / (param.paramInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (param.paramInitRep[i][j] != param.paramInitRep[i][0]);
                single.initRep << endl;
            }
//            for (i = 0; i < state.maxState; i++)
//            {   single.initRep << "InitState" << " " << i << " " << state.initStateNames[i];
//                for (j = 0; j < 4; j++) single.initRep << " " << state.initStateValue[i];
//                for (j = 1; j < 4; j++) single.initRep << " 0";
//                for (j = 1; j < 4; j++) single.initRep << " 0";
//                single.initRep << endl;
//            }
            for (i = 0; i < state.maxState; i++)
            {   single.initRep << "State" << " " << i << " " << state.initStateNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << state.initStateInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((state.initStateInitRep[i][j] - state.initStateInitRep[i][0]) / (state.initStateInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (state.initStateInitRep[i][j] != state.initStateInitRep[i][0]);
                single.initRep << endl;
            }
            for (i = 0; i < initAuxil.maxInitAuxil; i++)
            {   single.initRep << "InitAuxil" << " " << i << " " << initAuxil.initAuxilNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << initAuxil.initAuxilInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((initAuxil.initAuxilInitRep[i][j] - initAuxil.initAuxilInitRep[i][0]) / (initAuxil.initAuxilInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (initAuxil.initAuxilInitRep[i][j] != initAuxil.initAuxilInitRep[i][0]);
                single.initRep << endl;
            }
            for (i = 0; i < state.maxState; i++)
            {   single.initRep << "State" << " " << i << " " << state.stateNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << state.stateInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((state.stateInitRep[i][j] - state.stateInitRep[i][0]) / (state.stateInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (state.stateInitRep[i][j] != state.stateInitRep[i][0]);
                single.initRep << endl;
            }
            for (i = 0; i < auxil.maxAuxil; i++)
            {   single.initRep << "Auxiliary" << " " << i << " " << auxil.auxilNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << auxil.auxilInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((auxil.auxilInitRep[i][j] - auxil.auxilInitRep[i][0]) / (auxil.auxilInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (auxil.auxilInitRep[i][j] != auxil.auxilInitRep[i][0]);
                single.initRep << endl;
            }
            for (i = 0; i < state.maxState; i++)
            {   single.initRep << "Derivative" << " " << i << " " << state.derivNames[i];
                for (j = 0; j < 4; j++) single.initRep << " " << state.derivInitRep[i][j];
                for (j = 1; j < 4; j++) single.initRep << " " << fabs((state.derivInitRep[i][j] - state.derivInitRep[i][0]) / (state.derivInitRep[i][0] + 1.0e-10));
                for (j = 1; j < 4; j++) single.initRep << " " << (state.derivInitRep[i][j] != state.derivInitRep[i][0]);
                single.initRep << endl;
            }
            single.initRep << "-1" << endl;
            single.initRep.close();
        }
        runIdInitRep++;
    }
}

double linIntPol(double tx, double t0, double t1, double x0, double x1)
// function for linear interpolation
{   return t0 == t1 ? x0 : x0 + (x1 - x0)*(tx - t0)/(t1 - t0);
    // return the interpolated value
}

void PCModel::readInitStates()
//
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    double val;
    // declare a variable to hold a value
    StrObject valString;
    // temporary variabele to hold input text
    for (i = 0; i < MAX_STATE; i++)
    // go over all states
    {   id = int(state.stateInp.getLng(i, STATE_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the state
        if (i != id) cout << "Warning: incorrect id in state input file in line " << i << endl;
        // print a warning if there is an incorrect state id in the state input file
        state.initStateRep[i] = state.stateInp.getInt(i, STATE_INP_INT_REP_INIT_STATE);
        // read the first int column at the i-th line: should we report this state?
        state.initStateNames[i] = state.stateInp.getStr(i, STATE_INP_STR_INIT_STATE_NAME);
        // read the first str column at the i-th line: state name
        if ((state.stateSet < -1) || (state.stateSet > state.stateInp.getStrNumber() - 10)) // <------------ The 10 should be replaced with a logical value!
        // the requested state set in the analyser table exceeds the number of provided state sets in the state input table
        {   theOutObject.printError("Illegal id of state set");
            // print error message and terminate program
        }
        if (state.stateSet != -1)
        {   val = atof(state.stateInp.getStr(i, STATE_INP_STR_DEFAULT_0 + state.stateSet));
            // read the requested state input value column and turn the string into value
            if (fabs(state.initStateValue[i] - val) > 1.0e-6)
            // compare the default value from the c++ code with the value taken from the state input file
            {   single.setRep << runId << "\t" << state.stateNames[i] << "\t" << state.initStateValue[i] << "\t" << val << "\t" << (state.initStateValue[i] - val) << "\t" << -1 << endl;
                // if the difference is larger than 1.0e-6, print the default value, the input value and their difference to the set rep file
                state.initStateValue[i] = val;
                // overwrite the default value with the input value
            }
        }
    }
}

void PCModel::readStates()
//
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    for (i = 0; i < MAX_STATE; i++)
    // go over all states
    {   id = int(state.stateInp.getLng(i, STATE_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the state
        if (i != id) cout << "Warning: incorrect id in state input file in line " << i << endl;
        // print a warning if there is an incorrect state id in the state input file
        state.stateRep[i] = state.stateInp.getInt(i, STATE_INP_INT_REP_STATE);
        // read the first int column at the i-th line: should we report this state?
        state.stateNames[i] = state.stateInp.getStr(i, STATE_INP_STR_STATE_NAME);
        // read the first str column at the i-th line: state name
    }
}

void PCModel::readDerivs()
//
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    for (i = 0; i < MAX_STATE; i++)
    // go over all states
    {   id = int(state.stateInp.getLng(i, STATE_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the state
        if (i != id) cout << "Warning: incorrect id in state input file in line " << i << endl;
        // print a warning if there is an incorrect state id in the state input file
        state.derivRep[i] = state.stateInp.getInt(i, STATE_INP_INT_REP_DERIV);
        // read the second int column at the i-th line: should we report the derivative of this state?
        state.derivNames[i] = state.stateInp.getStr(i, STATE_INP_STR_DERIV_NAME);
        // read the second str column of the i-th line: derivative name
    }
}

void PCModel::readParameters()
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    double val;
    // declare a variable to hold a value
    StrObject valString;
    // temporary variabele to hold input text
    for (i = 0; i < MAX_PARAM; i++)
    // go over all the parameters
    {   id = int(param.paramInp.getLng(i, PARAM_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the parameter
        if (i != id) cout << "Warning: incorrect id in parameter input file in line " << i << endl;
        // print a warning if there is an incorrect state id in the state input file
        param.paramRep[i] = param.paramInp.getInt(i, PARAM_INP_INT_REP_PARAM);
        // read the first int column at the i-th line: should we report this parameter?
        param.paramNames[i] = param.paramInp.getStr(i, PARAM_INP_STR_PARAM_NAME);
        // read the first str column at the i-th line: parameter name
        if ((param.paramSet < -1) || (param.paramSet > param.paramInp.getStrNumber() - 8)) // <------------ The 8 should be replaced with a logical value!
        // the requested parameter set in the analyser table exceeds the number of provided parameter sets in the parameter input table
        {   theOutObject.printError("Illegal id of parameter set");
            // print error message and terminate program
        }
        if (param.paramSet != -1)
        {   valString = param.paramInp.getStr(i, PARAM_INP_STR_DEFAULT_0 + param.paramSet);
            // read the requested parameter input value column as a string
            if (valString.isInStr('/') || valString.isInStr('\\'))
            // if the string contains a '/' or '\' symbol, interpret this as the file name of a forcing function
            {   param.readParamInp[i].readFile(valString);
                // read the forcing function file into parameter input file array at location i
                single.setRep << runId << "\t" << param.paramNames[i] << "\t" << param.paramValue[i] << "\t" << valString << "\t" << 0 << "\t" << -1 << endl;
                // print the parameter name, the parameter i and the filename to the set rep file
                param.paramValue[i] = param.readParamInp[i].getDbl(0, FORCING_FUNCTION_DBL_VALUE);
                // write the value in the second dbl column of the forcing function file to the parametervalue array at location i
                if (param.readParamInp[i].getRecordNumber() > 1)
                // if there are more lines in the forcing function file
                {   int taskId = setNewReadParamSysTask(i, param.readParamInp[i].getDbl(0, FORCING_FUNCTION_DBL_TIME) + 0.0000002);
                    // generate a new system task to read the next line of the forcing function file at the time specified in the first dbl column at the first line in the forcing function file
                    cout << "Task " << taskId << " constructed!" << endl;
                    //
                }
            }
            else
            {   val = atof(valString);
                // else, a numerical value is provided
                if (fabs(param.paramValue[i] - val) > 1.0e-6)
                // compare the default value from the c++ code with the value taken from the parameter input file
                {   single.setRep << runId << "\t" << param.paramNames[i] << "\t" << param.paramValue[i] << "\t" << val << "\t" << (param.paramValue[i] - val) << "\t" << -1 << endl;
                    // if the difference is larger than 1.0e-6, print the default value, the input value and their difference to the set rep file
                    param.paramValue[i] = val;
                    // overwrite the default value with the input value
                }
            }
        }
    }
}

void InitialAuxiliary::readInitAuxil()
//
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    for (i = 0; i < MAX_INIT_AUXIL; i++)
    // go over all the initial auxiliary
    {   id = int(initAuxilInp.getLng(i, INIT_AUXIL_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the initial auxiliary
        if (i != id) cout << "Warning: incorrect id in initial auxiliary input file in line " << i << endl;
        // print a warning if there is an incorrect id in the initial auxiliary input file
        initAuxilRep[i] = initAuxilInp.getInt(i, INIT_AUXIL_INP_INT_REP_INIT_AUXIL);
        // read the first int column at the i-th line: should we report this initial auxiliary?
        initAuxilNames[i] = initAuxilInp.getStr(i, INIT_AUXIL_INP_STR_INIT_AUXIL_NAME);
        // read the second str column of the i-th line: initial auxiliary name
    }
}

void PCModel::readAuxil()
//
{   int i;
    // declare a loop counter
    long id;
    // declare variables to hold the id
    StrObject valString;
    // temporary variabele to hold input text
    for (i = 0; i < MAX_AUXIL; i++)
    // go over all the auxiliary
    {   id = int(auxil.auxilInp.getLng(i, AUXIL_INP_LNG_ID));
        // read the first lng column at the i-th line: the id of the auxiliary
        if (i != id) cout << "Warning: incorrect id in auxiliary input file in line " << i << endl;
        // print a warning if there is an incorrect id in the auxiliary input file
        auxil.auxilRep[i] = auxil.auxilInp.getInt(i, AUXIL_INP_INT_REP_AUXIL);
        // read the first int column at the i-th line: should we report this auxiliary?
        if (sens.sensitivityRunId > 0 && auxil.auxilInp.getInt(i, AUXIL_INP_INT_SENSITIVITY)) auxil.auxilRep[i] = 1;
        // overwrite the report value if this is a sensitivity analysis and this auxiliary is requested for output
        else if (calib.calibrationRunId > 0 && auxil.auxilInp.getInt(i, AUXIL_INP_INT_CALIBRATION)) auxil.auxilRep[i] = 1;
        // overwrite the report value if this is a calibration and this auxiliary is requested for output
        else if (bifur.bifurcationRunId > 0 && auxil.auxilInp.getInt(i, AUXIL_INP_INT_BIFURCATION)) auxil.auxilRep[i] = 1;
        // overwrite the report value if this is a bifurcation analysis and this auxiliary is requested for output
        auxil.auxilNames[i] = auxil.auxilInp.getStr(i, AUXIL_INP_STR_AUXIL_NAME);
        // read the second str column of the i-th line: auxiliary name
        if (auxil.auxilSet >= 0 && sens.sensitivityRunId == 0 && calib.calibrationRunId == 0 && bifur.bifurcationRunId == 0)
        // if this is a user defined auxiliary set AND not a sensitivity analysis
        {   valString = auxil.auxilInp.getStr(i, AUXIL_INP_STR_REMARKS);
            // read the requested auxiliary input value column as a string
            if (valString.isInStr('/') || valString.isInStr('\\'))
            // if the string contains a '/' or '\' symbol, interpret this as the file name of a forcing function
            {   auxil.readAuxilInp[id].readFile(valString);
                // read the forcing function file into parameter input file array at location id
                single.setRep << runId << "\t" << auxil.auxilNames[i] << "\t" << auxil.auxilValue[i] << "\t" << valString << "\t" << 0 << "\t" << -1 << endl;
                // print the parameter name, the parameter i and the filename to the set rep file
                if (auxil.readAuxilInp[id].getRecordNumber() > 0)
                // if is a line in the data file
                {   int taskId = setNewCompDataSysTask(i, auxil.readAuxilInp[i].getDbl(0, OBSERVED_DATA_DBL_TIME));
                    // generate a new system task to read the next line of the forcing function file at the time specified in the first dbl column at the first line in the forcing function file
                    cout << "Task " << taskId << " constructed!" << endl;
                    //
                }
                else
                {   StrObject f(valString  + ".txt");
                    auxil.readAuxilInp[id].writeFile(f);
                }
            }
        }
    }
}

void PCModel::repSys(double taskTime)
// write time dependent output
{   int i;
    // declare a loop counter
    single.dynRep << runId << "\t" << taskTime;
    // write the time to the time dependent output file
    for (i = 0; i < MAX_STATE; i++) if (state.initStateRep[i] == 1) single.dynRep << "\t" << state.initStateValue[i];
    // for all initial states if this state should be reported, write the state to the time dependent output file
    for (i = 0; i < MAX_STATE; i++) if (state.stateRep[i] == 1) single.dynRep << "\t" << state.stateValue[i];
    // for all states if this state should be reported, write the state to the time dependent output file
    for (i = 0; i < MAX_STATE; i++) if (state.derivRep[i] == 1) single.dynRep << "\t" << state.derivValue[i];
    // for all derivatives if this derivative should be reported, write the derivative to the time dependent output file
    for (i = 0; i < MAX_PARAM; i++) if (param.paramRep[i] == 1) single.dynRep << "\t" << param.paramValue[i];
    // for all parameters if this parameter should be reported, write the parameter to the time dependent output file
    for (i = 0; i < MAX_INIT_AUXIL; i++) if (initAuxil.initAuxilRep[i] == 1) single.dynRep << "\t" << initAuxil.initAuxilValue[i];
    // for all initial auxiliaries if this initial auxiliary should be reported, write the initial auxiliary to the time dependent output file
    for (i = 0; i < MAX_AUXIL; i++) if (auxil.auxilRep[i] == 1) single.dynRep << "\t" << auxil.auxilValue[i];
    // for all auxiliaries if this auxiliary should be reported, write the auxiliary to the time dependent output file
    single.dynRep << "\t" << -1 << endl;
    // write the end of the line to the time dependent output file
}

void PCModel::colAvgSys(double taskTime, double averageStartWithinYear, double averageEndWithinYear)
// collect the averages of the time dependent output <---------- the time mechanism could be moved to the OSIRIS task function
{   int i;
    // declare a loop counter
    double day = taskTime - double(int(taskTime / 365) * 365);
    // calculate the daynumber within the year
    if (day >= averageStartWithinYear && day <= averageEndWithinYear)
    // if the daynumber falls within the start and end daynumber specified for averaging
    {   single.timeSum.addX(taskTime);
        // add the current time
        for (i = 0; i < MAX_STATE; i++) if (state.initStateRep[i] == 1) state.initStateSum[i].addX(state.initStateValue[i]);
        // for all states if this state should be reported, add the current value to the sumobject for this state
        for (i = 0; i < MAX_STATE; i++) if (state.stateRep[i] == 1) state.stateSum[i].addX(state.stateValue[i]);
        // for all states if this state should be reported, add the current value to the sumobject for this state
        for (i = 0; i < MAX_STATE; i++) if (state.derivRep[i] == 1) state.derivSum[i].addX(state.derivValue[i]);
        // for all derivatives if this derivative should be reported, add the current value to the sumobject for this derivative
        for (i = 0; i < MAX_PARAM; i++) if (param.paramRep[i] == 1) param.paramSum[i].addX(param.paramValue[i]);
        // for all parameters if this parameter should be reported, add the current value to the sumobject for this parameter
        for (i = 0; i < MAX_INIT_AUXIL; i++) if (initAuxil.initAuxilRep[i] == 1) initAuxil.initAuxilSum[i].addX(initAuxil.initAuxilValue[i]);
        // for all initial auxiliaries if this initial auxiliary should be reported, add the current value to the sumobject for this initial auxiliary
        for (i = 0; i < MAX_AUXIL; i++) if (auxil.auxilRep[i] == 1) auxil.auxilSum[i].addX(auxil.auxilValue[i]);
        // for all auxiliaries if this auxiliary should be reported, add the current value to the sumobject for this auxiliary
    }
}

void PCModel::repAvgSys()
// write time dependent average output
{   int i;
    // declare a loop counter
    single.avgRep << runId << "\t" << single.timeSum.getAvgX();
    // write the average of the sampling times
    single.timeSum.flush();
    // reset the object that collects the averages
    for (i = 0; i < MAX_STATE; i++) if (state.initStateRep[i] == 1) single.avgRep << "\t" << state.initStateSum[i].getAvgX(), state.initStateSum[i].flush();
    // for all initial states, if this initial state should be reported, write the average to the file with averages, thereafter reset the object
    for (i = 0; i < MAX_STATE; i++) if (state.stateRep[i] == 1) single.avgRep << "\t" << state.stateSum[i].getAvgX(), state.stateSum[i].flush();
    // for all , if this state should be reported, write the average to the file with averages, thereafter reset the object
    for (i = 0; i < MAX_STATE; i++) if (state.derivRep[i] == 1) single.avgRep << "\t" << state.derivSum[i].getAvgX(), state.derivSum[i].flush();
    // for all derivatives, if this derivative should be reported, write the average to the file with averages, thereafter reset the object
    for (i = 0; i < MAX_PARAM; i++) if (param.paramRep[i] == 1) single.avgRep << "\t" << param.paramSum[i].getAvgX(), param.paramSum[i].flush();
    // for all parameters, if this parameter should be reported, write the average to the file with averages, thereafter reset the object
    for (i = 0; i < MAX_INIT_AUXIL; i++) if (initAuxil.initAuxilRep[i] == 1) single.avgRep << "\t" << initAuxil.initAuxilSum[i].getAvgX(), initAuxil.initAuxilSum[i].flush();
    // for all initial auxiliaries, if this initial auxiliaries should be reported, write the average to the file with averages, thereafter reset the object
    for (i = 0; i < MAX_AUXIL; i++)
    // for all auxiliaries
    {    if (auxil.auxilRep[i] == 1)
         {    single.avgRep << "\t" << auxil.auxilSum[i].getAvgX();
              // if this auxiliaries should be reported, write the average to the file with averages
              if (sens.sensitivityRunId && i == sens.auxilSensId) sens.collectDataForSensitivityAnalysis(auxil.auxilNames, auxil.auxilSum);
              // collect data for the sensitivity analysis
              else if (calib.calibrationRunId && i == calib.auxilCalibId) calib.collectDataForCalibration(auxil.auxilNames, auxil.auxilSum);
              // collect data for the calibration
              else if (bifur.bifurcationRunId && i == bifur.auxilBifurId) bifur.collectDataForBifurcationAnalysis(auxil.auxilNames, auxil.auxilSum);
              // collect data for the sensitivity analysis
              auxil.auxilSum[i].flush();
              // reset the object that collects the averages
         }
    }
    single.avgRep << "\t" << -1 << endl;
    // write end of the line to the file with averages
}

#define EULER 0
#define RK4   1
#define RKCK_WITH_FIXED_INTERMEDIATE_STEPS  2
#define RKCK  3

//void calculateDerivs(double time, double *state, double *param, double *auxil, double *deriv)
//{   pcmodel->calculateAuxil(time, state, param, auxil, deriv);
//}

void calculateDerivs(double time, double *state, double *param, double *auxil, double *deriv)
{   if (pcmodel->auxil.auxilSet == -1) pcmodel->calculateAuxilDef(time, state, param, auxil, deriv);
    // definition of the function that calculates the auxiliaries and the derivatives
    else if (pcmodel->auxil.auxilSet == -0) pcmodel->calculateAuxilSet0(time, state, param, auxil, deriv);
    // definition of the function that calculates the auxiliaries and the derivatives
    else if (pcmodel->auxil.auxilSet == 1) pcmodel->calculateAuxilSet1(time, state, param, auxil, deriv);
    // definition of the function that calculates the auxiliaries and the derivatives
    else if (pcmodel->auxil.auxilSet == 2) pcmodel->calculateAuxilSet2(time, state, param, auxil, deriv);
    // definition of the function that calculates the auxiliaries and the derivatives
    else if (pcmodel->auxil.auxilSet == 3) pcmodel->calculateAuxilSet3(time, state, param, auxil, deriv);
    // definition of the function that calculates the auxiliaries and the derivatives
    else theOutObject.printError("Illegal id of auxiliary set");
}

double sStore = 0.0;
//
int sStored = 0;
//
#define STEP_REP

void PCModel::performOneIntegrationStep(double &taskTime)
// takes the simulation one time step further
{   if (intCounter < 1000) intRepArray[intCounter][runId] = auxil.auxilValue[0];
    int i;
    // loop counter
    if (single.intMethod == EULER) {eulerDbl(taskTime, single.intStep, state.stateValue, MAX_STATE, param.paramValue, auxil.auxilValue, state.derivValue, calculateDerivs); auxil.auxilValue[0] = taskTime;}
    // euler intergration, call the integration routine and provide it with the time, stepsize, state array, number of states, parameter array, auxiliary array, derivative array and the function to calculate derivatives
    else if (single.intMethod == RK4) {rk4Dbl(taskTime, single.intStep, state.stateValue, MAX_STATE, param.paramValue, auxil.auxilValue, state.derivValue, calculateDerivs); auxil.auxilValue[0] = taskTime;}
    // Runge Kutta fourth order intergration, call the integration routine and provide it with the time, stepsize, state array, number of states, parameter array, auxiliary array, derivative array and the function to calculate derivatives
    else if (single.intMethod == RKCK_WITH_FIXED_INTERMEDIATE_STEPS)
    {   for (i = 0; i < MAX_STATE; i++) state.derivScale[i] = fabs(state.stateValue[i]) + fabs(state.derivValue[i] * single.intStep) + 1.0E-6;
		// fill the array to scale the errors in the derivatives
        double eps = single.intAccuracy;
        //double eps = double(a.getSimFltValue());
        // machine precision
        double t = taskTime;
        // current time
        double s = single.intStep;
        // current time step
        double nextFixedMoment = double(int(t) + 1) + 0.000001;
        //
        if (fabs(t - nextFixedMoment) > 0.000002)
        {   if ((t + s) > nextFixedMoment)
            {   sStore = s;
                sStored = 1;
                s = nextFixedMoment - t;
            }
        }
        else
        {   if (s > 1.0)
            {  s = 1.0;
            }
        }
        double hdid = 0.0;
        // realized timestep
        double hnext = 0.0;
        // next timestep;
        rkqsDbl(state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue, MAX_STATE, taskTime, s, eps, state.derivScale, hdid, hnext, calculateDerivs);
        // call the integration routine and provide it with the time, stepsize, state array, number of states, parameter array, auxiliary array, derivative array and the function to calculate derivatives
        auxil.auxilValue[0] = taskTime;
#ifdef STEP_REP
        if (steprep)
        {   steprep << t << "\t" << nextFixedMoment << "\t" << sStore << "\t" << sStored << "\t" << s << "\t" << hdid << "\t" << hnext << endl;
        }
        else
    	{   cout << "steprep is not open";
            for (;;);
        }
#endif
        if (sStored)
        {   single.intStep = sStore;
            sStore = 0.0;
            sStored = 0;
        }
        else
        {   single.intStep = hnext;
        }
    }
    else if (single.intMethod == RKCK)
    {   for (i = 0; i < MAX_STATE; i++) state.derivScale[i] = fabs(state.stateValue[i]) + fabs(state.derivValue[i] * single.intStep) + 1.0E-6;
		// fill the array to scale the errors in the derivatives
        double eps = single.intAccuracy;
        //double eps = double(a.getSimFltValue());
        // machine precision
        double t = taskTime; t;
        // current time
        double s = single.intStep;
        // current time step
        double hdid = 0.0;
        // realized timestep
        double hnext = 0.0;
        // next timestep;
        rkqsDbl(state.stateValue, param.paramValue, auxil.auxilValue, state.derivValue, MAX_STATE, taskTime, s, eps, state.derivScale, hdid, hnext, calculateDerivs);
        // call the integration routine and provide it with the time, stepsize, state array, number of states, parameter array, auxiliary array, derivative array and the function to calculate derivatives
        auxil.auxilValue[0] = taskTime;
#ifdef STEP_REP
        if (steprep)
        {   steprep << t << "\t" << s << "\t" << hdid << "\t" << hnext << endl;
        }
        else
    	{   cout << "steprep is not open";
            for (;;);
        }
#endif
        single.intStep = hnext;
    }
    else
    // if the specified intergration method is not 0, 1, 2 or 3
    {   theOutObject.printError("Unknown integration routine identifier");
        // print an error message and terminate the program
    }
    intCounter++;
}

void PCModel::compareWithData(double taskTime, double &nextTaskTime, long auxilId, int &stopTask)
// compare the observed with the modelled data
{   AscObject &dataInp = auxil.readAuxilInp[auxilId];
    // get a reference to the forcing function file for this auxiliary
    int readId = auxil.readAuxilId[auxilId];
    // ask at which we were reading
    double obsVal = dataInp.getDbl(readId, OBSERVED_DATA_DBL_OBSERVED);
    // get the value from the forcing function file at the current line
    double modVal = auxil.auxilValue[auxilId];
    // get the value from the forcing function file at the current line
    dataInp.getDblRef(readId, OBSERVED_DATA_DBL_MODELLED) = modVal;
    // write the modelled value to the file with the observations
    cout << endl << taskTime << " " << obsVal << " " << modVal << endl;
    // print the time, observed value and modelled value to the screen
    int nextReadId;
    // declare a variable to hold the next read id
    if (readId < dataInp.getRecordNumber() - 1)
    // if there is a next line
    {   nextReadId = readId + 1;
        // so this will be the next line
        nextTaskTime = dataInp.getDbl(nextReadId, FORCING_FUNCTION_DBL_TIME);
        // get the time from the forcing function file at the next line
        auxil.readAuxilId[auxilId] = nextReadId;
        // store the next read id
    }
    else
    // if there is no next line
    {   stopTask = 1;
        //
        StrObject f(auxil.auxilInp.getStr(auxilId, AUXIL_INP_STR_REMARKS) + ".txt");
        dataInp.writeFile(f);
        // do not perform this task again
    }
}

void PCModel::readForcingFunction(double taskTime, double nextTaskTime, long paramId, int &stopTask)
// task to handle the forcing functions, including interpolation <------------------ this function could be split in two functions, one that does the reading and one that does the interpolation
{   AscObject &forcingInp = param.readParamInp[paramId];
    // get a reference to the forcing function file for this parameter
    int readId = param.readParamId[paramId];
    // ask at which we wer reading
    int nextReadId = readId + 1;
    // so this will be the next line
    double time = forcingInp.getDbl(readId, FORCING_FUNCTION_DBL_TIME);
    // get the time from the forcing function file at the current line
    double nextTime = forcingInp.getDbl(nextReadId, FORCING_FUNCTION_DBL_TIME);
    // get the time from the forcing function file at the next line
    double val = forcingInp.getDbl(readId, FORCING_FUNCTION_DBL_VALUE);
    // get the value from the forcing function file at the current line
    double nextVal = forcingInp.getDbl(nextReadId, FORCING_FUNCTION_DBL_VALUE);
    // get the value from the forcing function file at the next line
    param.paramValue[paramId] = linIntPol(taskTime, time, nextTime, val, nextVal);
    // interpolate between these values for the current time and store this value in the parameter array
    //cout << taskTime << " " << param[paramId] << " " << time << " " << nextTime << " " << val << " " << nextVal << endl;
    //
    if (nextTaskTime > nextTime)
    // if the time on the next line is larger than the current time
    {   if (nextReadId == forcingInp.getRecordNumber() - 1)
        // if there is no next line
        {   stopTask = 1;
            // do not perform this task again
        }
        else
        // if there is a next line
        {   readId++;
            // increase the current line number by one
            param.readParamId[paramId] = readId;
            // store the new current line number
        }
    }
}

