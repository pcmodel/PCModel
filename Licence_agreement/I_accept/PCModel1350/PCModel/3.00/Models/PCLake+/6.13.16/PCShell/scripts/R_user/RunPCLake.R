rm(list=ls())

options(scipen = 999)
nearZero <- 1E-28


dirHome <- "C:/Users/SvenT/Documents/PCModel/PCModel-master/Licence_agreement/I_accept/"	# location of the PCModel1350 folder
nameWorkCase <- "PCLake_plus_default"
fileDATM <- "PL613162PLUS.xls"


## load all the functions
source(paste(dirHome,"PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/scripts/R_system/functions_PCLake.R",sep="")) 

## NB: you cannot use different combinations of forcings. e.g. 
##      - in model run 1: mQInEpi & mQInHyp
##      - in model run 2: mPLoad
##     If you want to do this you'll have to do for both: mQInEpi, mQInHyp AND mPLoad. 
##     This is due to the fact that the include file for the parameters (...rp.cpp file) has to be 
##     adjusted before compilation - the file will be built into the code.  
##     This adjustment entails changing parameters that are given as forcings should become "dummy". 
##     BTW: this has never been possible! 
##     How is this possible in the Excel version?

## Order of actions
##   1. Making folder structure for running the model
##   2. Load file 
##   < Make adjustments to the model > 
##   3. Make cpp files
##   4. Compile model
##   5. Initialize model
##   6. Run model


## 1. Making folder structure 
PCModelWorkCaseSetup(dirHOME = dirHome, 
                     nameWORKCASE = nameWorkCase)

## 2. Load file
lDATM_SETTINGS <- PCModelReadDATMFile_PCLakePlus(dirHOME  = dirHome, 
                                                 fileDATM = fileDATM)

## Optional: change sediment settings
# lDATM_SETTINGS$params <- adjustSedimentParamSettings(lDATM_SETTINGS$params, paramset = 2, sediment_type = "clay")


## 3. Make and adjust cpp files
##    - nRUN_SET determines which forcings are switched on
PCModelAdjustCPPfiles(dirHOME = dirHome,
                      nameWORKCASE = nameWorkCase,
                      lDATM = lDATM_SETTINGS,
                      nRUN_SET = 0)

## 4. Compile model
PCModelCompileModelWorkCase(dirHOME = dirHome,
                            nameWORKCASE = nameWorkCase)

## 5. Initialize model 
##    - make all initial states according to the run settings
InitStates <- PCModelInitializeModel(lDATM = lDATM_SETTINGS,
                                     dirHOME = dirHome,
                                     nameWORKCASE = nameWorkCase)

## 6. run one model
##    - Error catching on run_state & restart (if run_state = 0 & you use restart should you be able to do so?)
PCModel_run01 <- PCmodelSingleRun(lDATM = lDATM_SETTINGS,
                                  nRUN_SET = 0,
                                  dfSTATES = InitStates,
                                  integrator_method = "vode",
                                  dirHOME = dirHome,
                                  nameWORKCASE = nameWorkCase)




