##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Default PCLake+ R script
##
## This script was developed by Lilith Kramer, Sven Teurlincx and Luuk van Gerven
##
## Order of actions
##   1. Setting directories and sourcing helper scripts
##   2. Making folder structure for running the model
##   3. Load DATM file 
##   4. Optional: Make adjustments to the model 
##   5. Make and adjust cpp files
##   6. Compile model
##   7. Initialize model
##   8. Run model
##   9. Plot
## 
## NB: you cannot use different combinations of forcings for different lakes. e.g. 
##      - in lake 1: mQInEpi & mQInHyp
##      - in lake 2: mPLoad
##     You'll have to add for both: mQInEpi, mQInHyp AND mPLoad, and adjust the forcing values accordingly 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Clean work environment (optional)
# rm(list=ls())

## Global settings (optional)
options(scipen = 999) ## turn off scientific notation for small or very large numbers
nearZero <- 1E-28

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Setting directories and sourcing helper scripts -------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Set directories and names
dirHome <- "C:/Users/LilithK/Documents/PCModel/PCModel - official version/Licence_agreement/I_accept/"	# location of the PCModel1350 folder
dirShell <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PCShell") ## location of the PCShell folder
dirCpp_root <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Frameworks", "Osiris", "3.01", "PCLake_plus") ## location of the PCLake C++ files
nameWorkCase <- "PCLake_plus_default"
fileDATM <- "PL613162PLUS.xls"
fileXls <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", fileDATM)

## source helper scripts
source(file.path(dirShell, "scripts", "R_system", "functions_PCLake.R")) 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Creating a work_case and making the work_case folder structure ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PCModelWorkCaseSetup(dirSHELL = dirShell, 
                     dirCPP_ROOT = dirCpp_root,
                     nameWORKCASE = nameWorkCase)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3. Load DATM file ----------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lDATM_SETTINGS <- PCModelReadDATMFile_PCLakePlus(fileXLS  = fileXls,
                                                 locDATM = "excel",
                                                 locFORCING = "excel")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4. Optional: adjust model settings------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For example, change the sediment settings of a lake
# lDATM_SETTINGS$params <- adjustSedimentParamSettings(lDATM_SETTINGS$params, paramset = 2, sediment_type = "clay")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.  Make and adjust cpp files ----------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## The nRUN_SET determines which forcings are switched on
PCModelAdjustCPPfiles(dirSHELL = dirShell,
                      nameWORKCASE = nameWorkCase,
                      lDATM = lDATM_SETTINGS,
                      nRUN_SET = 0)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.  Compile the model ------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PCModelCompileModelWorkCase(dirSHELL = dirShell,
                            nameWORKCASE = nameWorkCase)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.  Initialize model  ------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Make all initial states according to the run settings
InitStates <- PCModelInitializeModel(lDATM = lDATM_SETTINGS,
                                     dirSHELL = dirShell,
                                     nameWORKCASE = nameWorkCase)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 8.  Run model  -------------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##    - Error catching on run_state & restart (if run_state = 0 & you use restart should you be able to do so?)
PCModel_run01 <- PCmodelSingleRun(lDATM = lDATM_SETTINGS,
                                  nRUN_SET = 0,
                                  dfSTATES = InitStates,
                                  integrator_method = "vode",
                                  dirHOME = dirHome,
                                  nameWORKCASE = nameWorkCase)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 9.  Plot model -------------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(PCModel_run01$time, PCModel_run01$oChlaEpi)



