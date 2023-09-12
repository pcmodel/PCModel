##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SECTION SPECIFIC
##
##  DESCRIPTION
##    This script contains the higher level functions to run PCLake(S)+ in R
##    This script uses helper/base functions from the script functions.R
##    This script was developed by Lilith Kramer, Sven Teurlincx and Luuk van Gerven.
##  
##  VERSION
##    30 Jun 2023
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## source helper functions
source(paste(dirHome, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/scripts/R_system/functions.R", sep="")) 

## load necessary packages 
loadPackage("deSolve") ## load this before compilation, otherwise you can get namespace errors

## ***********************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## prepInitials
##
##   Function to prepare new initial conditions from the output of a PCModel run
##   ('restart values')
##
##   Arguments are: 
##	  - listPCModelRun = the output from a PCModel run
##    - day = the day number you would like to use as new input
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ***********************************************************************

prepInitials <- function(listPCModelRun, day){
  # listPCModelRun <- PCModel_run01
  # day <- 10*365
  restartCol <- c("uDepthW", 
                  "uDepthMixMeas",
                  "oNH4WHyp",
                  "oNO3WHyp",
                  "oPO4WHyp",
                  "oPAIMWHyp",
                  "oSiO2WHyp",
                  "oO2WHyp",
                  "oDDetWHyp",
                  "oNDetWHyp",
                  "oPDetWHyp",
                  "oSiDetWHyp",
                  "oDIMWHyp",
                  "oDDiatWHyp",
                  "oNDiatWHyp",
                  "oPDiatWHyp",
                  "oDGrenWHyp",
                  "oNGrenWHyp",
                  "oPGrenWHyp",
                  "oDBlueWHyp",
                  "oNBlueWHyp",
                  "oPBlueWHyp",
                  "oDZooHyp",
                  "oNZooHyp",
                  "oPZooHyp",
                  "aDFiAd",
                  "aDFiJv",
                  "aNFiAd",
                  "aNFiJv",
                  "aPFiAd",
                  "aPFiJv",
                  "aDPisc",
                  "aNH4S",
                  "aNO3S",
                  "aPO4S",
                  "aPAIMS",
                  "aDDetS",
                  "aNDetS",
                  "aPDetS",
                  "aSiDetS",
                  "aDHumS",
                  "aNHumS",
                  "aPHumS",
                  "aDIMS",
                  "aDDiatS",
                  "aNDiatS",
                  "aPDiatS",
                  "aDGrenS",
                  "aNGrenS",
                  "aPGrenS",
                  "aDBlueS",
                  "aNBlueS",
                  "aPBlueS",
                  "aDVeg",
                  "aNVeg",
                  "aPVeg",
                  "aVegHeight",
                  "aDBent",
                  "aNBent",
                  "aPBent",
                  "uDepthWM",
                  "oNH4WM",
                  "oNO3WM",
                  "oPO4WM",
                  "oPAIMWM",
                  "oSiO2WM",
                  "oO2WM",
                  "oDDetWM",
                  "oNDetWM",
                  "oPDetWM",
                  "oSiDetWM",
                  "oDIMWM",
                  "oDDiatWM",
                  "oNDiatWM",
                  "oPDiatWM",
                  "oDGrenWM",
                  "oNGrenWM",
                  "oPGrenWM",
                  "oDBlueWM",
                  "oNBlueWM",
                  "oPBlueWM",
                  "oDZooM",
                  "oNZooM",
                  "oPZooM",
                  "aNH4SM",
                  "aNO3SM",
                  "aPO4SM",
                  "aPAIMSM",
                  "aDDetSM",
                  "aNDetSM",
                  "aPDetSM",
                  "aSiDetSM",
                  "aDHumSM",
                  "aNHumSM",
                  "aPHumSM",
                  "aDIMSM",
                  "aDRootPhra",
                  "aDShootPhra",
                  "aNRootPhra",
                  "aNShootPhra",
                  "aPRootPhra",
                  "aPShootPhra",
                  "oNH4WEpi",
                  "oNO3WEpi",
                  "oPO4WEpi",
                  "oPAIMWEpi",
                  "oSiO2WEpi",
                  "oO2WEpi",
                  "oDDetWEpi",
                  "oNDetWEpi",
                  "oPDetWEpi",
                  "oSiDetWEpi",
                  "oDIMWEpi",
                  "oDDiatWEpi",
                  "oNDiatWEpi",
                  "oPDiatWEpi",
                  "oDGrenWEpi",
                  "oNGrenWEpi",
                  "oPGrenWEpi",
                  "oDBlueWEpi",
                  "oNBlueWEpi",
                  "oPBlueWEpi",
                  "oDZooEpi",
                  "oNZooEpi",
                  "oPZooEpi",
                  "aDBlueSurf",
                  "aNBlueSurf",
                  "aPBlueSurf",
                  "aDExtTotT",
                  "aNExtTotT",
                  "aPExtTotT",
                  "aSiExtTotT",
                  "aO2ExtTotT",
                  "aDepthExtTotT")
  
  restart_data_sel <- listPCModelRun[day, c("time", restartCol)]
  restart_data_sel_long <- as.data.frame(tidyr::pivot_longer(data = restart_data_sel, cols = colnames(restart_data_sel)[2:length(colnames(restart_data_sel))], names_to = "variable", values_to = "value"))
  restart_data_sel_long$variable <- gsub("^a|^o|^u", "s", restart_data_sel_long$variable)
  restart_data_sel_long$variable <- gsub("sDepthMixMeas", "sMixDepthW", restart_data_sel_long$variable)
  #which(!restart_data_sel_long$variable %in% rownames(lDATM_SETTINGS2$states))
  return(restart_data_sel_long)
}


## ***********************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelWorkCaseSetup
##
##   Function to set up the folder structure for working with PCModel in R.
##     Folders are only made if they were not present before 
##     and files are only copied into the folders if none were present yet.
##     No files will be removed or overwritten by this function. 
##
##   Arguments are: 
##	  - dirSHELL = the directory in which the work_cases folder can be found
##    - dirCPP_ROOT = the PCLake directory that contains the latest version of the cpp files
##    - nameWORKCASE = the name of the work case folder you want to create
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ***********************************************************************

PCModelWorkCaseSetup <- function(dirSHELL, 
                                 dirCPP_ROOT,
                                 nameWORKCASE){
  
  ## debug
  # nameWORKCASE <- nameWorkCase
  # dirSHELL <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PCShell")
  # dirCPP_ROOT <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Frameworks", "Osiris", "3.01", "PCLake_plus")
  
  ## define directories of files
  dirWORKCASE <- file.path(dirSHELL, "work_cases", nameWORKCASE)
  
  ## if it did not exist yet: create main directory
  if(!dir.exists(dirWORKCASE)) dir.create(dirWORKCASE, recursive = T, showWarnings = T)
  
  ## if it did not exist yet: create directory for model output 
  if(!dir.exists(file.path(dirWORKCASE, "output"))) dir.create(file.path(dirWORKCASE, "output"), showWarnings = T)
  
  ## create a directory for cpp files containing model code and copy the cpp files to it
  ## cpp files = c++ code with the equations and initial settings
  ##   these are loaded in the script as the user may wish to create multiple different models (different cpp's) and compare them
  ##   In that case the user will have to compile multiple different DATM instances and save the cpp files to different folders,
  ##   the names of which can be looped through 
  if(!dir.exists(file.path(dirWORKCASE, "source_cpp"))) dir.create(file.path(dirWORKCASE, "source_cpp"), showWarnings = T)
  
  cpp_files <- list.files(dirCPP_ROOT, full.names = TRUE)[
    which((lapply(strsplit(x = list.files(dirCPP_ROOT, 
                                          full.names = TRUE), 
                           split="[/]"), 
                  function(x) which(x %in% c("pl61316ra.cpp","pl61316rc.cpp","pl61316rd.cpp","pl61316ri.cpp","pl61316rp.cpp","pl61316rs.cpp",
                                             "pl61316sa.cpp","pl61316sc.cpp","pl61316sd.cpp","pl61316si.cpp","pl61316sp.cpp","pl61316ss.cpp")))>0)==TRUE)]		
  
  if(length(list.files(file.path(dirWORKCASE, "source_cpp")))>0) print("The source_cpp folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(file.path(dirWORKCASE, "source_cpp")))==0) file.copy(cpp_files, file.path(dirWORKCASE, "source_cpp"), overwrite = F)
  
  ## create a directory for the cpp files ['include files' of the model] to be adjusted in
  if(!dir.exists(file.path(dirWORKCASE, "source_cpp_adjusted"))) dir.create(file.path(dirWORKCASE, "source_cpp_adjusted"), showWarnings = T)
  
  ## create a directory for extra input you might want to use in your model
  ## think of time series etc. This folder will not be cleared if it already existed. I do not want to accidentally remove raw data or some such. 
  if(!dir.exists(file.path(dirWORKCASE, "input"))) dir.create(file.path(dirWORKCASE, "input"), showWarnings = T)
  
  ## create a directory for the model code, this folder will be used for compilation
  ## copy template code in here
  if(!dir.exists(file.path(dirWORKCASE, "model_code"))) dir.create(file.path(dirWORKCASE, "model_code"), showWarnings = T)
  copy_model_files <- list.files(file.path(dirSHELL, "scripts", "cpp2R"), full.names = TRUE)[
    which((lapply(strsplit(x = list.files(file.path(dirSHELL, "scripts", "cpp2R"), full.names = TRUE), split="[/]"), 
                  function(x) which(x %in% c("compile_model_cpp.cmd", "model_base.cpp")))>0)==TRUE)]		
  
  if(length(list.files(file.path(dirWORKCASE, "model_code")))>0) print("The model_code folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(file.path(dirWORKCASE, "model_code")))==0)  file.copy(copy_model_files, file.path(dirWORKCASE, "model_code"), overwrite = F)
  
  print(paste0("Work case ", nameWORKCASE, " is ready for use."))
  
}


## ***********************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelReadDATMFile_PCLakePlus
##
##   Function to read run settings, states, parameters, auxiliary data, and forcings from PCLake files.
##     You can load the model code either from excel or from txt files. You can also load the forcings
##     either from excel or from txt files. You can select the source of the files via the arguments locDATM (model code)
##     and locFORCING (forcings). The location of the files / folder should be given via the arguments fileXLS or folderTXT.
##     
##   All column names from the excel sheets are hardcoded. 
##
##   Required: 
##     - R packages readxl and tidyr 
##
##    Arguments are:
##	   - fileXLS = the directory + filename + xls file extention of the PCLake excel file model you want to load
##	   - folderTXT = the directory of the PCLake txt files you want to load
##     - locDATM = either excel or txt
##     - locFORCING = either excel or txt 
##     - readAllForcings = switch to read all forcings from the file, regardless of them being switched on or not
##	
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ***********************************************************************

PCModelReadDATMFile_PCLakePlus <- function(fileXLS = NULL,
                                           folderTXT = NULL, 
                                           locDATM = c("excel", "txt"),
                                           locFORCING = c("excel", "txt"),
                                           readAllForcings = F,
                                           interpolation_timestep = 1){
  
  ## debug
  # dirHOME = dirHome
  # fileXLS = fileDATM
  # folderTXT = file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")
  # locDATM = "excel"
  # locFORCING = "txt"
  
  if(is.null(fileXLS) & is.null(folderTXT) == TRUE){stop("Please enter either excel filename into the fileXls argument or the location of the txt folder into the folderTxt argument.")}
  ## NB: Would be good to add the error catching for locDATM / locFORCING and path entries. 
  
  ## Loading (and if missing, installing) packages 
  loadPackage("readxl")
  loadPackage("tidyr")
  
  ## Define path to DATM file (excel) and txt files
  # pathTxt  <- file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")
  if(!is.null(fileXLS)){pathXls <- file.path(fileXLS)}else{pathXls <- ""}
  if(!is.null(folderTXT)){pathTxt <- file.path(folderTXT)}else{pathTxt <- ""}
  
  ifelse(locFORCING == "txt", pathForc <- pathTxt, pathForc <- pathXls)
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load sheets from DATM (excel) file 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Run settings ----
  
  if(locDATM == "txt"){
    dfRUN_SETTINGS_PREP <- read.table(file.path(pathTxt, "Control.txt"), skip = 29, fill = T)
    dfRUN_SETTINGS <- dfRUN_SETTINGS_PREP[-c(unique(which(is.na(dfRUN_SETTINGS_PREP), arr.ind = T)[,1])), ]
  }
  
  if(locDATM != "txt"){
    dfRUN_SETTINGS_PREP <- as.data.frame(read_excel(pathXls, sheet = "Control", skip = 29, col_names = T))
    rownames(dfRUN_SETTINGS_PREP) <- as.character(dfRUN_SETTINGS_PREP[, 1])
    dfRUN_SETTINGS <- dfRUN_SETTINGS_PREP[-c(unique(which(is.na(dfRUN_SETTINGS_PREP), arr.ind = T)[,1])), -1]
  }
  
  for(nCOL in 1:ncol(dfRUN_SETTINGS)){ dfRUN_SETTINGS[, nCOL]	<-	as.numeric(gsub(",", ".", as.character(unlist(dfRUN_SETTINGS[, nCOL])))) }
  
  
  ## Initial states ----
  if(locDATM == "txt"){
    dfSTATES_PRICE_RAW <- read.table(file.path(pathTxt, "states.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfSTATES_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "states"))
  }
  dfSTATES <- dfSTATES_PRICE_RAW[which(dfSTATES_PRICE_RAW[, 2]!=""), 
                                 which(colnames(dfSTATES_PRICE_RAW) %in% c("sStateName",
                                                                           "sInitialStateName",
                                                                           "iReportState",
                                                                           "sDefaultSetTurbid0",
                                                                           "sDefaultSetClear1",
                                                                           "sAltenativeSet2",
                                                                           "sTurbidSet15mDeep"))]  
  rownames(dfSTATES) <- as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sStateName")]))
  dfSTATES <- dfSTATES[, -which(colnames(dfSTATES)=="sStateName")]
  rownames(dfSTATES) <- gsub("_", "", rownames(dfSTATES))
  dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")] <- gsub("_", "", as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")])))
  for(nCOL in which(colnames(dfSTATES) %in% c("iReportState", 
                                              "sDefaultSetTurbid0",
                                              "sDefaultSetClear1",
                                              "sAltenativeSet2",
                                              "sTurbidSet15mDeep"))){ 
    dfSTATES[, nCOL] <-	as.numeric(as.character(unlist(dfSTATES[,nCOL])))
  }
  
  
  ## Parameters ----
  if(locDATM == "txt"){
    dfPARAMS_PRICE_RAW <- read.table(file.path(pathTxt, "parameters.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfPARAMS_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "parameters"))
  }
  dfPARAMS <- dfPARAMS_PRICE_RAW[which(dfPARAMS_PRICE_RAW[,2]!=""), 
                                 which(colnames(dfPARAMS_PRICE_RAW) %in% c("sName",
                                                                           "iReport",
                                                                           "sMinValue",
                                                                           "sMaxValue",
                                                                           "sDefault0",
                                                                           "sSet1",
                                                                           "sSet2",
                                                                           "sSet3"))] 
  rownames(dfPARAMS) <-	as.character(unlist(dfPARAMS[which(colnames(dfPARAMS)=="sName")]))
  dfPARAMS <- dfPARAMS[, -which(colnames(dfPARAMS)=="sName")]
  rownames(dfPARAMS) <-	gsub("_", "", rownames(dfPARAMS))
  dfINTERMEDIATE <- dfPARAMS ## preparation for if you'd want to use the .txt names for loading the params
  ## warnings are suppressed for the following action, because they should occur in case of a text file, and the ensuing behavior is always wanted
  for(nCOL in 1:ncol(dfPARAMS)){ dfPARAMS[, nCOL]	<- suppressWarnings(as.numeric(as.character(unlist(dfPARAMS[,nCOL])))) } #!# Here all parameter values are put to numeric. If txt was written in, this will become NA. For forcings see "Forcing" section.
  
  
  ## Forcings ----
  
  ## Get all the reads
  # readAllForcings <- F
  if(readAllForcings == T){
    readOn <- which(is.na(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ]) == FALSE, arr.ind = T)
  }else{
    readOn <- which(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ] == 1, arr.ind = T)
  }
  
  if(length(unique(table(readOn[,2]))) != 1){ warning(paste0("You are trying to load an excelfile that has different forcings switched on per run scenario.",
                                                             " Please be aware of the fact that the compiled R model will expect all runs to have the same combination of forcings switched on.",
                                                             " The forcing values themselves may differ, but the ReadForcing values should be equal."))}
  realNamesOfForcings <- matchSwitchToForcing(unique(rownames(readOn)))
  dfINTERMEDIATE_sel <- dfINTERMEDIATE[which(rownames(dfINTERMEDIATE) %in% realNamesOfForcings),]
  for(nCOL in 1:ncol(dfINTERMEDIATE_sel)){ dfINTERMEDIATE_sel[, nCOL]	<- as.character(unlist(dfINTERMEDIATE_sel[,nCOL])) } 
  dfINTERMEDIATE_sel$param_names <- rownames(dfINTERMEDIATE_sel)
  dfINTERMEDIATE_sel$iReport <- NULL
  dfINTERMEDIATE_sel$sMinValue <- NULL
  dfINTERMEDIATE_sel$sMaxValue <- NULL
  dfINTERMEDIATE_sel2 <- dfINTERMEDIATE_sel[, c(length(colnames(dfINTERMEDIATE_sel)), 1:length(colnames(dfINTERMEDIATE_sel))-1)] 
  dfINTERMEDIATE_long <- pivot_longer(dfINTERMEDIATE_sel2, cols = c(2:length(colnames(dfINTERMEDIATE_sel2))), names_to = "run_name") 
  #dfINTERMEDIATE_long2 <- dfINTERMEDIATE_long[grep("txt", dfINTERMEDIATE_long$value, value = F),]
  dfINTERMEDIATE_long$value <- gsub("txt\\/|\\.txt", "", dfINTERMEDIATE_long$value)
  lsINTERMEDIATE <- split(as.data.frame(dfINTERMEDIATE_long), dfINTERMEDIATE_long$run_name, drop = T)
  
  dfMATCH_RUN_PARAM <- data.frame(run_set = c(0, 1, 2, 3),
                                  param_set = c("sDefault0", "sSet1", "sSet2", "sSet3"))
  
  lsFORCINGS <- list()
  ## load all defined forcings
  for(i in names(lsINTERMEDIATE)) {
    
    ## debug
    # i <- names(lsINTERMEDIATE)[1]
    
    ## get the real amount of timesteps for the forcings so they can be interpolated to their full extent
    runtime_years <- dfRUN_SETTINGS["dReady", 
                                    which(dfRUN_SETTINGS["iRuniD",] == dfMATCH_RUN_PARAM[dfMATCH_RUN_PARAM$param_set == i, "run_set"])]
    times_forcing  <- seq(0, 365*runtime_years, interpolation_timestep)
    
    
    ## make a placeholder list for the forcings
    lsFORCINGS_prep <- list()
    lsFORCINGS_prep[[i]] <- vector("list", length(lsINTERMEDIATE[[i]]$value))
    names(lsFORCINGS_prep[[i]]) <- lsINTERMEDIATE[[i]]$param_names
    
    
    ## use function to load all forcings and interpolate them as well
    lsFORCINGS_prep[[i]] <- sapply(names(lsFORCINGS_prep[[i]][which(names(lsFORCINGS_prep[[i]])!="time")]), 
                                   getForcingAndInterpolate, 
                                   location = locFORCING, 
                                   pathLoc = pathForc,
                                   metadata = lsINTERMEDIATE[[i]],
                                   timesteps = times_forcing,
                                   simplify = F)
    
    lsFORCINGS[[i]] <- c(list(time = data.frame(time = times_forcing, value = times_forcing)), lsFORCINGS_prep[[i]])
  }
  
  
  ## Auxiliaries (output) ---- 
  if(locDATM == "txt"){
    dfAUXIL_PRICE_RAW <- read.table(file.path(pathTxt, "derivatives.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfAUXIL_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "derivatives"))
  }
  
  dfAUXIL <- dfAUXIL_PRICE_RAW[which(dfAUXIL_PRICE_RAW[, 2]!=""), which(colnames(dfAUXIL_PRICE_RAW) %in% c("sName",
                                                                                                           "iReport"))]
  rownames(dfAUXIL) <- as.character(unlist(dfAUXIL[which(colnames(dfAUXIL)=="sName")]))
  dfAUXIL <- dfAUXIL[, -which(colnames(dfAUXIL)=="sName"), drop = FALSE]
  rownames(dfAUXIL)	<- gsub("_", "", rownames(dfAUXIL))
  for(nCOL in 1:ncol(dfAUXIL)){ dfAUXIL[, nCOL] <- as.numeric(as.character(unlist(dfAUXIL[, nCOL]))) }
  
  
  ## Parameter adjustment ----
  ## Adjust parameter values so there are no NA values present in the df anymore
  ## This is necessary for the initialization of the states in PCModelAdjustCPPfile() 
  dfPARAMS_na_to_fill <- which(is.na(dfPARAMS), arr.ind = T) ## get the location of all NA values
  if(length(dfPARAMS_na_to_fill>0)){
    ## put to -99999 the ones that will not be used (the ones that are not part of the forcings)
    params_na_to_make_min99999 <- setdiff(unique(row.names(dfPARAMS_na_to_fill)), 
                                          names(lsFORCINGS[[1]])[which(!names(lsFORCINGS[[1]])%in% "time")])
    rows_to_put_to_zero <- which(rownames(dfPARAMS) %in% params_na_to_make_min99999)
    for(single_row in rows_to_put_to_zero){dfPARAMS[single_row, which(is.na(dfPARAMS[single_row, ]))] <- -99999}
    
    ## put the first value of the forcing in the NA slots that have forcings
    dfPARAMS_na_for_forc <- which(is.na(dfPARAMS), arr.ind = T)
    for(rownr in 1:nrow(dfPARAMS_na_for_forc)){
      ## rownr <- 1
      row_to_use <- dfPARAMS_na_for_forc[rownr, 1]
      col_to_use <- dfPARAMS_na_for_forc[rownr, 2]
      dfPARAMS[row_to_use, col_to_use] <- lsFORCINGS[[colnames(dfPARAMS)[col_to_use]]][[rownames(dfPARAMS)[row_to_use]]]$value[1]
    }
  }
  
  ## return 
  return(list(run_settings = dfRUN_SETTINGS, 
              states = dfSTATES,
              params = dfPARAMS,
              forcings = lsFORCINGS,
              auxils = dfAUXIL))
  
}

## ***********************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelReadDATMFile_PCLakeSPlus
##
##   Function to read PCLakeS+ run settings, PCLakeS+ states, PCLakeS+ parameters, auxiliary data, and forcings from the DATM file.
##     All column names for state set 1 and param set 1 from the excel sheets are hard coded. 
##     You can only load the forcings from either excel or txt. 
##
##   Required: 
##     - R packages readxl
##
##    Arguments are:
##	   - fileXLS = the directory + filename + xls file extention of the PCLake excel file model you want to load
##     - folderTXT = the directory of the PCLake txt files you want to load
##	   - locFORCING = location of the forcings, either excel or txt file 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ***********************************************************************

PCModelReadDATMFile_PCLakeSplus <- function(fileXLS,
                                            folderTXT = NULL,
                                            locFORCING = c("txt", "excel")){
  
  ## debug
  # fileXLS = fileXls
  # folderTXT = NULL,
  # locFORCING = "excel"
  
  if(is.null(fileXLS) == TRUE){stop("Please enter the path + PCLake excel filename + file extention into the fileXls argument.")}
  if(locFORCING == "txt" & is.null(folderTXT)){"You have selected 'txt' for the forcings, but have not entered the location of the txt folder. Please enter the path to the txt folder to the folderTXT argument."}
  
  ## Loading (and if missing, installing) packages 
  loadPackage("readxl")
  loadPackage("tidyr")
  
  ## Define path to DATM file (excel) and txt files
  # locFORCING <- match.arg(locFORCING) ## check if locFORCING has the right name
  pathXls <- file.path(fileXLS)
  ifelse(locFORCING == "txt", pathForc <- folderTXT, pathForc <- pathXls)
  
  ## get the sheet names of the excel
  # sheetsDATM <- excel_sheets(pathXls)
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load sheets from DATM (excel) file 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Run settings ----
  dfRUN_SETTINGS <- as.data.frame(read_excel(pathXls, sheet = "PCLakeS+_control"))
  rownames(dfRUN_SETTINGS) <- as.character(dfRUN_SETTINGS[, 1])
  dfRUN_SETTINGS$Type <- NULL
  for(nCOL in 1:ncol(dfRUN_SETTINGS)){ dfRUN_SETTINGS[, nCOL]	<-	as.numeric(gsub(",", ".", as.character(unlist(dfRUN_SETTINGS[, nCOL])))) }
  
  ## Initial states ----
  dfSTATES_RAW <-	as.data.frame(read_excel(pathXls, sheet = "PCLakeS+_states"))
  state_set_names <- colnames(dfSTATES_RAW)[c((which(colnames(dfSTATES_RAW) == "sInitialStateUnit")+1):length(colnames(dfSTATES_RAW)))]
  dfSTATES <- dfSTATES_RAW[which(dfSTATES_RAW[, 2]!=""),
                           which(colnames(dfSTATES_RAW) %in% c("sStateName",
                                                               "sInitialStateName",
                                                               "iReportState",
                                                               state_set_names))]
  rownames(dfSTATES) <- as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sStateName")]))
  dfSTATES <- dfSTATES[, -which(colnames(dfSTATES)=="sStateName")]
  rownames(dfSTATES) <- gsub("_", "", rownames(dfSTATES))
  dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")] <- gsub("_", "", as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")])))
  for(nCOL in which(colnames(dfSTATES) %in% c("iReportState", state_set_names))){ 
    dfSTATES[, nCOL] <-	as.numeric(as.character(unlist(dfSTATES[,nCOL])))
  }
  
  
  ## Parameters ----
  dfPARAMS_RAW <- as.data.frame(read_excel(pathXls, sheet = "PCLakeS+_parameters"))
  param_set_names <- colnames(dfPARAMS_RAW)[c((which(colnames(dfPARAMS_RAW) == "sMaxValue")+1):length(colnames(dfPARAMS_RAW)))]
  dfPARAMS <- dfPARAMS_RAW[which(dfPARAMS_RAW[,2]!=""), which(colnames(dfPARAMS_RAW) %in% c("sName",
                                                                                            "iReport",
                                                                                            "sMinValue",
                                                                                            "sMaxValue",
                                                                                            param_set_names))] 
  rownames(dfPARAMS) <-	as.character(unlist(dfPARAMS[which(colnames(dfPARAMS)=="sName")]))
  dfPARAMS <- dfPARAMS[, -which(colnames(dfPARAMS)=="sName")]
  rownames(dfPARAMS) <-	gsub("_", "", rownames(dfPARAMS))
  dfINTERMEDIATE <- dfPARAMS ## preparation for if you'd want to use the .txt names for loading the params
  ## warnings are suppressed for the following action, because they should occur in case of a text file, and the ensuing behavior is always wanted
  for(nCOL in 1:ncol(dfPARAMS)){ dfPARAMS[, nCOL]	<- suppressWarnings(as.numeric(as.character(unlist(dfPARAMS[,nCOL])))) } #!# Here all parameter values are put to numeric. If txt was written in, this will become NA. For forcings see "Forcing" section.
  
  
  ## Forcings ----
  
  ## Get all the reads 
  
  readOn <- which(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), -which(colnames(dfINTERMEDIATE) %in% c("iReport", "sMinValue", "sMaxValue"))] == 1, arr.ind = T)
  #readOn <- which(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ] == 1, arr.ind = T)
  if(length(unique(table(readOn[,2]))) != 1){ warning(paste0("You are trying to load an excelfile that has different forcings switched on per run scenario.",
                                                             " Please be aware of the fact that the compiled R model will expect all runs to have the same combination of forcings switched on.",
                                                             " The forcing values themselves may differ, but the ReadForcing values should be equal."))}
  realNamesOfForcings <- matchSwitchToForcing(unique(rownames(readOn)))
  dfINTERMEDIATE_sel <- dfINTERMEDIATE[which(rownames(dfINTERMEDIATE) %in% realNamesOfForcings),]
  for(nCOL in 1:ncol(dfINTERMEDIATE_sel)){ dfINTERMEDIATE_sel[, nCOL]	<- as.character(unlist(dfINTERMEDIATE_sel[,nCOL])) } 
  dfINTERMEDIATE_sel$param_names <- rownames(dfINTERMEDIATE_sel)
  dfINTERMEDIATE_sel$iReport <- NULL
  dfINTERMEDIATE_sel$sMinValue <- NULL
  dfINTERMEDIATE_sel$sMaxValue <- NULL
  dfINTERMEDIATE_sel2 <- dfINTERMEDIATE_sel[, c(length(colnames(dfINTERMEDIATE_sel)), 1:length(colnames(dfINTERMEDIATE_sel))-1)] 
  dfINTERMEDIATE_long <- pivot_longer(dfINTERMEDIATE_sel2, cols = c(2:length(colnames(dfINTERMEDIATE_sel2))), names_to = "run_name") 
  dfINTERMEDIATE_long$value <- gsub("txt\\/|\\.txt", "", dfINTERMEDIATE_long$value)
  dfINTERMEDIATE_long$run_name <- factor(dfINTERMEDIATE_long$run_name, levels=unique(dfINTERMEDIATE_long$run_name))
  lsINTERMEDIATE <- split(as.data.frame(dfINTERMEDIATE_long), dfINTERMEDIATE_long$run_name, drop = T)
  ## split could become slow for large datasets, data.table might be a solution there
  ## https://stackoverflow.com/questions/17611734/r-split-preserving-natural-order-of-factors
  
  ## match run set with paramset
  # dfMATCH_RUN_PARAM <- data.frame(run_set = seq(0, length(lsINTERMEDIATE)-1, 1),
  #                                param_set = intersect(colnames(dfINTERMEDIATE_sel), names(lsINTERMEDIATE))) ## intersect keeps the order of the first element, and the elements of both, param_set is always a subset of colnames(dfINTERMEDIATE_sel)
  
  
  ## get the real amount of timesteps for the forcings so they can be interpolated to their full extent
  runtime_years <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dReady"), "Set0"]
  times_forcing  <- seq(0, 365*runtime_years)
  
  lsFORCINGS <- list()
  ## load all defined forcings
  for(i in names(lsINTERMEDIATE)) {
    
    ## debug
    # i <- names(lsINTERMEDIATE)[6]
    
    ## make a placeholder list for the forcings
    lsFORCINGS_prep <- list()
    lsFORCINGS_prep[[i]] <- vector("list", length(lsINTERMEDIATE[[i]]$value))
    names(lsFORCINGS_prep[[i]]) <- lsINTERMEDIATE[[i]]$param_names
    
    ## use function to load all forcings and interpolate them as well
    lsFORCINGS_prep[[i]] <- sapply(names(lsFORCINGS_prep[[i]][which(names(lsFORCINGS_prep[[i]])!="time")]), 
                                   getForcingAndInterpolate, 
                                   location = locFORCING,
                                   pathLoc = pathForc,
                                   metadata = lsINTERMEDIATE[[i]],
                                   timesteps = times_forcing,
                                   simplify = F)
    
    lsFORCINGS[[i]] <- c(list(time = data.frame(time = times_forcing, value = times_forcing)), lsFORCINGS_prep[[i]])
  }
  
  
  ## Auxiliaries (output) ---- 
  dfAUXIL_RAW	<- as.data.frame(read_excel(pathXls, sheet = "derivatives")) 
  dfAUXIL <- dfAUXIL_RAW[which(dfAUXIL_RAW[, 2]!=""), which(colnames(dfAUXIL_RAW) %in% c("sName",
                                                                                         "iReport"))]
  rownames(dfAUXIL) <- as.character(unlist(dfAUXIL[which(colnames(dfAUXIL)=="sName")]))
  dfAUXIL <- dfAUXIL[, -which(colnames(dfAUXIL)=="sName"), drop = FALSE]
  rownames(dfAUXIL)	<- gsub("_", "", rownames(dfAUXIL))
  for(nCOL in 1:ncol(dfAUXIL)){ dfAUXIL[, nCOL] <- as.numeric(as.character(unlist(dfAUXIL[, nCOL]))) }
  
  
  ## Parameter adjustment ----
  ## Adjust parameter values so there are no NA values present in the df anymore; an NA occurs when there was a textfile instead of a number in the excel cell
  ## This is necessary for the initialization of the states in PCModelAdjustCPPfile() 
  dfPARAMS_na_to_fill <- which(is.na(dfPARAMS), arr.ind = T) ## get the location of all NA values
  if(length(dfPARAMS_na_to_fill>0)){
    ## put to -99999 the ones that will not be used (the ones that are not part of the forcings)
    params_na_to_make_min99999 <- setdiff(unique(row.names(dfPARAMS_na_to_fill)), 
                                          names(lsFORCINGS[[1]])[which(!names(lsFORCINGS[[1]])%in% "time")])
    rows_to_put_to_zero <- which(rownames(dfPARAMS) %in% params_na_to_make_min99999)
    for(single_row in rows_to_put_to_zero){dfPARAMS[single_row, which(is.na(dfPARAMS[single_row, ]))] <- -99999}
    
    ## put the first value of the forcing in the NA slots that have forcings
    dfPARAMS_na_for_forc <- which(is.na(dfPARAMS), arr.ind = T)
    for(rownr in 1:nrow(dfPARAMS_na_for_forc)){
      ## rownr <- 1
      row_to_use <- dfPARAMS_na_for_forc[rownr, 1]
      col_to_use <- dfPARAMS_na_for_forc[rownr, 2]
      dfPARAMS[row_to_use, col_to_use] <- lsFORCINGS[[colnames(dfPARAMS)[col_to_use]]][[rownames(dfPARAMS)[row_to_use]]]$value[1]
    }
  }
  
  ## return 
  return(list(run_settings = dfRUN_SETTINGS, 
              states = dfSTATES,
              params = dfPARAMS,
              forcings = lsFORCINGS,
              auxils = dfAUXIL))
  
}



## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelAdjustCPPfiles
## 
##   Edit c++ files from PCLake/PCDitch (created by OSIRIS)
##  
##   Two parts are edited: 
##      - include files 
##      - the template for the model
##  
##   The include files are adjusted according to parameter settings, forcings and initial conditions
##   The template model file is adjusted according to forcings, auxiliary output and model version name
##  
##   Arguments are:
##      - dirSHELL = the directory in which the work_cases folder can be found
##      - nameWORKCASE = the name of the work case folder you're working in
##      - lDATM = the name of the DATM object
##      - dfRUN_SETTINGS = optional, can be used when lDATM is missing; lDATM$run_settings
##      - nRUN_SET = which run set should be used for the model? Can be either 0, 1, 2, or 3
##      - dfPARAM = optional, can be used when lDATM is missing; lDATM$params
##      - dfSTATES = optional, can be used when lDATM is missing; lDATM$states
##      - lFORCINGS = optional, can be used when lDATM is missing; lDATM$forcings,
##      - dfAUXIL = optional, can be used when lDATM is missing; lDATM$auxils
##
##  Due to the way the function is written, the 'optional' data frames will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
##  or on it own argument (when one is given) 
##  using lDATM is the simple way to get the data frames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelAdjustCPPfiles <- function(dirSHELL,
                                  nameWORKCASE, 
                                  lDATM = NULL, 
                                  dfRUN_SETTINGS = lDATM$run_settings,
                                  nRUN_SET = c(0, 1, 2, 3), 
                                  dfPARAM = lDATM$params, 
                                  dfSTATES = lDATM$states,
                                  lFORCINGS = lDATM$forcings,
                                  dfAUXIL = lDATM$auxils){ 
  
  ## debug
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # nRUN_SET <- 0
  # dfPARAM <- lDATM_SETTINGS$params
  # dfSTATES <- lDATM_SETTINGS$states
  # lFORCINGS <- lDATM_SETTINGS$forcings
  # dfAUXIL <- lDATM_SETTINGS$auxils
  
  ## error catching
  if(length(nRUN_SET) > 1 | any(!(nRUN_SET %in% c(0, 1, 2, 3))) == TRUE){stop("Please enter one value between 0 and 3 for nRUN_SET")}
  
  
  ## define directories of files
  dirWORKCASE  =	file.path(dirSHELL, "work_cases", nameWORKCASE)     # location of work case
  dirCPP        = file.path(dirWORKCASE, "source_cpp")                    # location of cpp files
  dir_CPP_adj   = file.path(dirWORKCASE, "source_cpp_adjusted")           # location of output cpp files
  dir_MODEL_adj = file.path(dirWORKCASE, "model_code")                    # location of output model cpp file
  
  ## Get the right sets of data, based on the run
  # set_state <- dfRUN_SETTINGS["iStateSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_state <- dfRUN_SETTINGS["iStateSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))]]
  # set_param <- dfRUN_SETTINGS["iParamSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_param <- dfRUN_SETTINGS["iParamSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))]]
  set_forc  <- names(lFORCINGS)[which(colnames(dfPARAM)[4+set_param] == names(lFORCINGS))]
  
  
  ## Load forcings
  vFORCING_NAMES <- names(lFORCINGS[[set_forc]])
  ## note that we remove the time forcing, as it will lead to double definitions
  if('time' %in% vFORCING_NAMES){ vFORCING_NAMES	= vFORCING_NAMES[-which(vFORCING_NAMES=='time')]}
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PART A: adjust CPP files that work as include files to the model.cpp
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  ## edit c++ files from PCLake/PCDitch (created by OSIRIS)
  ## 1. set parameters: 
  ##       - user-defined parameters
  ##       - forcing function parameters (switches, e.g. ReadTemp)
  ## 2. set (user-defined) initial conditions 
  ## 3. edit declaration files:
  ##       - remove forcing function parameters (e.g. mTemp) from declaration list to prevent double declarations (as both a parameter and a time series)
  ## 4. determine the length of the declaration arrays and store them
  ## 5. write all files: 
  ##       - underscores will be removed
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  ## get names of cpp files to be adjusted
  cpp_files <- list.files(dirCPP, pattern = ".cpp")
  
  ## placeholder for information about the declaration files. This information is used in the model.cpp!
  arrays <- vector()
  
  for(cpp_file in cpp_files){
    
    ## cpp_file <- cpp_files[5]
    
    tmp_cpp_file <- readLines(file.path(dirCPP, cpp_file))
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 1. Set parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    if (grepl("sp", cpp_file)) {
      
      all_param <- dfPARAM[, 4 + set_param] ## get all model parameters
      names(all_param) <- row.names(dfPARAM) ## make them into a names vector so setValues() understands the format
      
      tmp_cpp_file <- setValues(tmp_cpp_file, all_param)  ## set user-defined parameters
      
      rm(list = c("all_param")) ## clean up
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 2. Set initial conditions 
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (grepl("sc", cpp_file)) { 
      
      all_inits <- dfSTATES[, 3 + set_state] ## get all model states
      names(all_inits) <- dfSTATES[, "sInitialStateName"] ## make them into a names vector so setValues() understands the format
      
      tmp_cpp_file <- setValues(tmp_cpp_file, all_inits) ## set user-defined initial conditions
      
      rm(list = c("all_inits")) ## clean up
    } 
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 3. Remove forcing function parameters (e.g. mTemp) from parameter declaration list
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (grepl("rp", cpp_file)) { 
      
      ## Extra action: write a rp2.cpp file with all the parameters in there. 
      ## This file will be used in the initialization. 
      tmp_cpp_file2 <- tmp_cpp_file
      tmp_cpp_file2 <- gsub("_","", tmp_cpp_file2) ## remove underscores
      writeLines(tmp_cpp_file2, file.path(dir_CPP_adj, gsub("rp.cpp", "rp2.cpp", cpp_file))) ## write adjusted information to new file
      
      ## regular script
      i <- 0
      for (name in vFORCING_NAMES) {
        i   <- i + 1
        tmp_cpp_file <- gsub(paste("_", name, "_", sep = ""), paste("_dummy", i, "_", sep = ""), tmp_cpp_file) 
      }
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 4. Determine the length of the declaration arrays and store them
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if ((grepl("ra", cpp_file) || grepl("rp", cpp_file) || grepl("rs", cpp_file) || grepl("ri", cpp_file))) {  
      
      array_name <- substring(tmp_cpp_file[1], regexpr("=", tmp_cpp_file[1])[1]+2, regexpr("\\[", tmp_cpp_file[1])[1]-1)
      array_length <- strsplit(tmp_cpp_file[length(tmp_cpp_file)]," ")[[1]][3]
      arrays <- c(arrays, paste("static double ", array_name, "[", array_length, "];", sep=""))
    }
    
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 5. Write output
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## remove underscores for easy R interpretation & write files
    tmp_cpp_file <- gsub("_", "", tmp_cpp_file) ## remove underscores
    writeLines(tmp_cpp_file, file.path(dir_CPP_adj, cpp_file)) ## write adjusted information to new file
  }
  
  writeLines(arrays, file.path(dir_CPP_adj, "arrays.cpp")) ## write length of declaration arrays to arrays.cpp file
  
  ## END of PART A
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PART B: Adjust the model.cpp file
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## edit c++ model (scripts/cpp2R/model_base.cpp) for compilation:
  ## 1. define output auxiliaries
  ## 2. define forcing functions 
  ## 3. refer to the right model version name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  model_base_cpp <- readLines(file.path(dir_MODEL_adj, "model_base.cpp")) # read the c++ model
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 1. set output for auxiliaries
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  id             <- grep(x = model_base_cpp, pattern = "output_auxiliaries")
  codelines      <- vector()
  aux_names      <- rownames(dfAUXIL[which(dfAUXIL[, 1] == 1), , drop = F])
  aux_number     <- length(aux_names)
  i              <- 0
  if (length(aux_names)>0) {
    for (aux_name in aux_names) { # define user-defined output auxiliaries as output_auxiliaries
      codelines <- c(codelines,paste("  yout[",i,"] = ", aux_name, ";", sep = "")) 
      i <- i + 1
    }
  } else { # if there are no output auxiliaries; make at least one 'dummy' output auxiliary, as desired by DeSolve
    codelines   <- "  yout[0]=0;"
    aux_number  <- 1
    aux_names   <- "dummy"
    aux_units   <- "-"
  }
  
  model_cpp <- c(model_base_cpp[1:(id-1)], codelines, model_base_cpp[(id+1):length(model_base_cpp)])
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 2. set forcing functions 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  id        <- grep(x = model_cpp, pattern = "input_forcings")
  codelines <- paste("static double forc[", (1+length(vFORCING_NAMES)), "];", sep="")
  codelines <- c(codelines, "double &time = forc[0];") # define time as an external forcing
  i         <- 0
  for (name in vFORCING_NAMES) { # define user-defined forcings as external forcings
    i         <- i + 1
    codelines <- c(codelines, paste("double &", name, " = forc[",i,"];", sep=""))
  }
  codelines <- c(codelines, paste("#define MAXFORC ", (1+length(vFORCING_NAMES)), sep=""))
  model_cpp <- c(model_cpp[1:(id-1)], codelines, model_cpp[(id+1):length(model_cpp)])
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. refer to the right model version
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cpp_files     <- list.files(file.path(dirWORKCASE, "source_cpp"), pattern = ".cpp") ## this is only to get the model version name!
  stop_id       <- regexpr(pattern = "...cpp", cpp_files[1])[[1]]-1
  model_version <- substr(cpp_files[1], start = 1, stop = stop_id) #get model version
  model_cpp     <- sub(pattern = "model_version", replacement = model_version, x = model_cpp) # insert model version into c++ file
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. write to file
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # writeLines(model_cpp, file.path(dir_SCHIL,"scripts", "cpp2R", "model.cpp")
  writeLines(model_cpp, file.path(dir_MODEL_adj, "model.cpp"))
  
}



## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelAdjustlDATMandCPPfiles_PCLakeSplus
##
## Edit c++ files from PCLake/PCDitch (created by OSIRIS)
## 
## Two parts are edited: 
##    - include files 
##    - the template for the model
##
## The include files are adjusted according to parameter settings, forcings and initial conditions
## The template model file is adjusted according to forcings, auxiliary output and model version name
## All files are also adjusted according to the amount of nodes in the network. Copies of the contents are made and labelled with the node names. 
## Additionally, the transport formulas and parameters are added to the respective files. 
##
## Arguments are:
##   - dirSHELL = the directory in which the work_cases folder can be found
##   - nameWORKCASE = the name of the work case folder you're working in
##   - dfNODES = the name of the data frame containing the nodes
##   - dfNETWORK = the name of the data frame containing the edges/the network
##   - dfSUBST = the name of the data frame listing the substances you're using for transport
##   - lDATM = the name of the DATM object
##   - dfRUN_SETTINGS = optional, can be used when lDATM is missing; lDATM$run_settings
##   - dfPARAM = optional, can be used when lDATM is missing; lDATM$params
##   - dfSTATES = optional, can be used when lDATM is missing; lDATM$states
##   - lFORCINGS = optional, can be used when lDATM is missing; lDATM$forcings,
##   - dfAUXIL = optional, can be used when lDATM is missing; lDATM$auxils
##
## Due to the way the function is written, the 'optional' data frames will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
## or on it own argument (when one is given) 
## using lDATM is the simple way to get the data frames
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelAdjustlDATMandCPPfiles_PCLakeSplus <- function(dirSHELL,
                                                      nameWORKCASE,
                                                      dfNODES,
                                                      dfNETWORK,
                                                      dfSUBST,
                                                      lDATM = NULL, 
                                                      dfRUN_SETTINGS = lDATM$run_settings,
                                                      dfPARAM = lDATM$params, 
                                                      dfSTATES = lDATM$states,
                                                      lFORCINGS = lDATM$forcings,
                                                      dfAUXIL = lDATM$auxils){
  
  # # debug
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  # dfNODES <- dfNodes
  # dfNETWORK <- dfNetwork
  # dfSUBST <- tran_subst
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # dfPARAM <- lDATM_SETTINGS$params
  # dfSTATES <- lDATM_SETTINGS$states
  # lFORCINGS <- lDATM_SETTINGS$forcings
  # dfAUXIL <- lDATM_SETTINGS$auxils
  
  ## define directories of files
  dirWORKCASE  =	file.path(dirSHELL, "work_cases", nameWORKCASE)     # location of work case
  dirCPP        = file.path(dirWORKCASE, "source_cpp")                    # location of cpp files
  dir_CPP_adj   = file.path(dirWORKCASE, "source_cpp_adjusted")           # location of output cpp files
  dir_MODEL_adj = file.path(dirWORKCASE, "model_code")                    # location of output model cpp file
  
  ## we need to add a node number to all PCLake variables
  ## therefore we need to find and replace all variables in PCLake 
  ## excluding the variables that are part of the function expression (e.g. EQ, TRUE, FALSE, LT, ...)
  ## here we build the regular expression that can select the right parts of the code
  model_base_cpp <- readLines(file.path(dir_MODEL_adj, "model_base.cpp")) # open the file with the variables that are essential to the calculation
  function_words <- unlist(lapply(strsplit(grep("#define", model_base_cpp, value = T), split = " "), function(x) (x[2])))
  build_pattern <- paste0("(_(?!", paste(function_words, collapse = "|"), ")[[:alnum:]]+)(_)") 
  ## the pattern starts with an underscore _
  ## then we exclude the terms essential to the calculation with (?! ... )
  ## all other numeric and regular characters are allowed [:alnum:]
  ## if they are present one or more times +
  ## the pattern ends with a _
  ## two main groups are present, indicated by the (), this allows us later on to select the location for an insertion
  ## https://regex101.com/ this website helps you build reg expressions, it's great!
  
  ## how many and which nodes are present?
  ## possibly redundant: takes only positive node numbers
  nodes <- sort(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to))[which(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to)) >= 0)])
  if(!all(nodes %in% unique(dfNODES$node_nr))){stop("Not all nodes in dfNETWORK are accounted for in dfNODES. Please correct!")}
  
  ## which connections are present
  # dfNETWORK$box_conn <- paste0("cn", dfNETWORK$node_nr_from, "to", dfNETWORK$node_nr_to)
  
  
  ## get names of cpp files to be adjusted
  cpp_files <- list.files(dirCPP, pattern = ".cpp")
  
  ## only these cpp files do not have be changed, as they are not used:
  ## - sp = initial state values 
  ## - sc = parameter values 
  
  cpp_array_connection <- file(file.path(dir_CPP_adj, "arrays.cpp"), open = "w") ## open connection to which everything shall be written
  arrays <- vector()
  
  for(cpp_file in cpp_files){
    
    ## cpp_file <- cpp_files[5]
    cpp_file_org_content <- readLines(file.path(dirCPP, cpp_file)) ## read content
    
    cpp_file_write_connection <- file(file.path(dir_CPP_adj, cpp_file), open = "w") ## open connection to which everything shall be written
    
    ## placeholder for information about the declaration files. This information is used in the model.cpp!
    
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 1. Set parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## sp.cpp ####
    if (grepl("sp", cpp_file)) {
      
      for(sel_node in nodes){
        
        # sel_node <- nodes[4]
        
        ## get param set
        set_param <- dfRUN_SETTINGS["iParamSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == sel_node)]]
        all_param <- dfPARAM[, 4 + set_param] ## get all model parameters
        names(all_param) <- row.names(dfPARAM) ## make them into a names vector so SetParameters() understands the format
        
        ## set parameters
        tmp_cpp_file <- setValues(cpp_file_org_content, all_param)  ## set user-defined parameters
        tmp_cpp_file <- c(tmp_cpp_file, paste0("_mSurfArea_ = ", dfNODES[dfNODES$node_nr == sel_node, "surface_area"], ";"))
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), tmp_cpp_file, perl = T) ## remove underscores
        
        ## add the fractions
        if(sel_node %in% dfNETWORK$node_nr_to){
          for(node_from in dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"]){
            # node_from <- dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"]
            tmp_cpp_file <- c(tmp_cpp_file, paste0("_mFracN", node_from, "N", sel_node, "_ = ", dfNETWORK[dfNETWORK$node_nr_to == sel_node & dfNETWORK$node_nr_from == node_from, "fraction"], ";"))
          }
        }
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
        
        ## clean up
        rm(list = c("set_param", "all_param"))
      }
      
      close(cpp_file_write_connection)
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 2. Set initial conditions 
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## sc.cpp ####
    if (grepl("sc", cpp_file)) { 
      
      for(sel_node in nodes){
        
        # sel_node <- nodes[1]
        set_state <- dfRUN_SETTINGS["iStateSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == sel_node)]]
        all_inits <- dfSTATES[, 3 + set_state] ## get all model states
        names(all_inits) <- dfSTATES[, "sInitialStateName"] ## make them into a names vector so SetParameters() understands the format
        
        tmp_cpp_file <- setValues(cpp_file_org_content, all_inits) ## set user-defined initial conditions
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), tmp_cpp_file, perl = T) ## remove underscores
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
        
        ## clean up
        rm(list = c("set_state", "all_inits"))
      } 
      
      close(cpp_file_write_connection)
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 3. Remove forcing function parameters (e.g. mTemp) from parameter declaration list
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## rp.cpp ####
    if (grepl("rp", cpp_file)) { 
      
      ## open an extra connection to write to for the ..rp2.cpp file 
      ## This file does not have the word "dummy" in it and can therefore be used in the initialization 
      cpp_file_write_connection2 <- file(file.path(dir_CPP_adj, gsub("rp.cpp", "rp2.cpp", cpp_file)), open = "w") ## open connection to which everything shall be written
      length_tmp_cpp_file <- 0
      length_tmp_cpp_file2 <- 0
      
      for(sel_node in nodes){
        
        # sel_node <- 1
        
        ## Action for rp2.cpp file
        tmp_cpp_file2 <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), cpp_file_org_content, perl = T) ## add node numbers
        tmp_cpp_file2 <- tmp_cpp_file2[-grep("#define", x = tmp_cpp_file2)] ## remove last line with the max amount of params on it, we'll add it back later with the proper amount corrected for the amount of nodes
        tmp_cpp_file2 <- c(tmp_cpp_file2, paste0("double &_mSurfAreaN", sel_node, "_ = param[0];"))
        ## add the fractions
        if(sel_node %in% dfNETWORK$node_nr_to){
          for(node_from in dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"]){
            # node_from <- dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"][1]
            tmp_cpp_file2 <- c(tmp_cpp_file2, paste0("double &_mFracN", node_from, "N", sel_node, "_ = param[0];"))
          }
        }
        ## add the sequence numbers
        for(position_in_file in 1:length(tmp_cpp_file2)){ 
          tmp_cpp_file2[position_in_file] <- gsub("([[:alpha:]]+)([[0-9]+])(;)", 
                                                  paste0("\\1[", (position_in_file-1)+(length_tmp_cpp_file2), "]\\3"), 
                                                  tmp_cpp_file2[position_in_file], 
                                                  perl = T)
        }
        length_tmp_cpp_file2 <- length_tmp_cpp_file2 + length(tmp_cpp_file2)
        tmp_cpp_file2 <- gsub("_","", tmp_cpp_file2) ## remove underscores
        writeLines(tmp_cpp_file2, con = cpp_file_write_connection2) ## write adjusted information to new file
        
        
        ## Actions for rp.cpp file
        tmp_cpp_file <- cpp_file_org_content
        
        set_param <- dfRUN_SETTINGS["iParamSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == sel_node)]]
        set_forc  <- names(lFORCINGS)[which(colnames(dfPARAM)[4+set_param] == names(lFORCINGS))]
        
        vFORCING_NAMES <- names(lFORCINGS[[set_forc]]) ## Load forcings
        if('time' %in% vFORCING_NAMES){ vFORCING_NAMES	= vFORCING_NAMES[-which(vFORCING_NAMES=='time')]} ## note that we remove the time forcing, as that would lead to double definitions
        
        i <- 0
        for (name in vFORCING_NAMES) {
          i   <- i + 1
          tmp_cpp_file <- gsub(paste("_", name, "_", sep = ""), paste("_dummy", i, "_", sep = ""), tmp_cpp_file) 
        }
        
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), tmp_cpp_file, perl = T)  ## add the node numbers
        tmp_cpp_file <- tmp_cpp_file[-grep("#define", x = tmp_cpp_file)] ## remove last line with the max amount of params on it, we'll add it back later with the proper amount corrected for the amount of nodes
        tmp_cpp_file <- c(tmp_cpp_file, paste0("double &_mSurfAreaN", sel_node, "_ = param[0];"))
        if(sel_node %in% dfNETWORK$node_nr_to){
          j <- length(vFORCING_NAMES) + 1
          for(node_from in dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"]){
            # node_from <- dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"][1]
            # tmp_cpp_file <- c(tmp_cpp_file, paste0("double &_mFracN", node_from, "N", sel_node, "_ = param[0];"))
            tmp_cpp_file <- c(tmp_cpp_file, paste0("double &_dummy", j, "N", node_from, "N", sel_node, "_ = param[0];"))
            j <- j + 1
          }
        }
        
        for(position_in_file in 1:length(tmp_cpp_file)){
          # position_in_file <- 1
          tmp_cpp_file[position_in_file] <- gsub("([[:alpha:]]+)([[0-9]+])(;)", 
                                                 paste0("\\1[", (position_in_file-1)+(length_tmp_cpp_file), "]\\3"), 
                                                 tmp_cpp_file[position_in_file], 
                                                 perl = T)
        }
        length_tmp_cpp_file <- length_tmp_cpp_file + length(tmp_cpp_file)
        
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
      }
      
      writeLines(paste0("#define MAXPARAM ", 
                        length_tmp_cpp_file2), 
                 con = cpp_file_write_connection2) ## write adjusted information to new file
      writeLines(paste0("#define MAXPARAM ", 
                        length_tmp_cpp_file), 
                 con = cpp_file_write_connection) ## write adjusted information to new file
      
      close(cpp_file_write_connection2)
      close(cpp_file_write_connection)
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 4. Add transport terms to auxiliary .cpp
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## sa.cpp ####
    if(grepl("sa", cpp_file)){ 
      
      transport_org <- list() ## list to hold the original transport formulas in
      transport_add <- list() ## list to hold the additional transport formulas in (inflow from upstream nodes) 
      for(sel_node in nodes){
        
        #print(sel_node)
        # sel_node <- nodes[2]
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), cpp_file_org_content, perl = T) ## remove underscores
        
        
        ## ~~~~~~~~~
        ## make additional transport auxiliaries, if the current node has inflow from other nodes
        ## and save these in a separate list - these additional formulas have to be added to the end of the sa.cpp file 
        ## as all (lake internal) calculations have to happen before transport takes place, that's why. 
        if(sel_node %in% dfNETWORK$node_nr_to){
          
          ## make all transport formulas per connection
          additional_auxiliaries_list <- list()
          for(node_from in dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"]){
            
            # node_from <- dfNETWORK[dfNETWORK$node_nr_to %in% sel_node, "node_nr_from"][1]
            
            additional_auxiliaries_list[[paste0("node_from", node_from)]] <- paste0("((_", dfSUBST$outflow, "N", node_from, "_", ## get substance
                                                                                    " * ", 
                                                                                    "_uDepthW", gsub("([[:alnum:]]+)(Epi|Hyp)", "\\2", dfSUBST$outflow, perl = T), "N", node_from,"_",
                                                                                    " * ", 
                                                                                    "_mFracN", node_from, "N", sel_node, "_ * ",
                                                                                    "( _mSurfAreaN", node_from, "_",
                                                                                    " / _mSurfAreaN", sel_node, "_ )) / uDepthWEpiN", sel_node, ")")   
            
          }
          
          ## add up the different connections
          ## and combine the result into the final formula's
          ## save this result to a list
          if(length(additional_auxiliaries_list) >= 1){
            
            ## remove all nulls from list, if they're there
            add_aux <- additional_auxiliaries_list[which(!sapply(additional_auxiliaries_list, is.null))]
            additional_auxiliaries_list <- do.call(paste, c(add_aux, sep = " + "))
            additional_auxiliaries_list <- gsub("_", "", additional_auxiliaries_list)
            
            add_aux_temp <- paste0(dfSUBST$tran_subst, 
                                   "Upst",
                                   "N", 
                                   sel_node, 
                                   " = IF (FALSE EQ InclTranN", sel_node, ") THEN 0.0 ELSE ",
                                   additional_auxiliaries_list,
                                   " ENDIF;")
            add_aux_temp[grep("Hyp", add_aux_temp)] <- gsub(paste0(" = IF (FALSE EQ InclTranN", sel_node, ") THEN 0.0 ELSE "),
                                                            paste0(" = IF ((FALSE EQ InclTranN", sel_node, ") OR (aInclStratN", sel_node, " EQ FALSE)) THEN 0.0 ELSE "),
                                                            add_aux_temp[grep("Hyp", add_aux_temp)], fixed = T)
            
            ## add the totals to the mix! 
            ## we need the totals so we can add them in to mass balance formula's
            ## grab all P, N, D, Si and O2 and reshape them such that they become the right formula's
            add_tot_temp <- paste0(makeTranUpstTotFormulas(dfSUBSTANCES = dfSUBST, sel_node = sel_node), ";")
            
            transport_add[[paste0("node_", sel_node)]] <- c(add_aux_temp, add_tot_temp)
            
          }
          
          
        }
        
        ## grab tran terms & save in different location
        ## and remove terms from cpp file
        ids <- grep(x = tmp_cpp_file, 
                    pattern = paste0(paste0("^_", dfSUBST$tran_subst, "N", sel_node, "_"), collapse = "|"),
                    ignore.case = TRUE)
        transport_org[[paste0("node_", sel_node)]] <- tmp_cpp_file[c(ids)]
        tmp_cpp_file <- tmp_cpp_file[-c(ids)]
        # if(length(ids)!=length(dfSUBST$tran)) print(paste("Warning: defined transport substances to be changed do not match with the transport substances in the cpp file.", sep = ""))
        
        
        ## if there's upstream nodes, add the additional inflow term to the transport_org,
        if(sel_node %in% dfNETWORK$node_nr_to){
          transport_org[[paste0("node_", sel_node)]] <- gsub("(^_[[:alnum:]]+)(N[[:digit:]]+_)(.+)(_ENDIF_.+)", 
                                                             paste0("\\1\\2\\3+ \\1Upst\\2 \\4"), 
                                                             transport_org[[paste0("node_", sel_node)]],
                                                             perl = T)}
        id_phyt <- grep(x = tmp_cpp_file, 
                        pattern = "TranPhyt",
                        ignore.case = TRUE)
        transport_org[[paste0("node_", sel_node)]] <- c(transport_org[[paste0("node_", sel_node)]], tmp_cpp_file[c(id_phyt)])
        tmp_cpp_file <- tmp_cpp_file[-c(id_phyt)]
        
        transport_org[[paste0("node_", sel_node)]] <- gsub("_", "", transport_org[[paste0("node_", sel_node)]])
        
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
        
      }
      
      ## lets go to the writing here! 
      ## first the tmp cpp file minus the trans terms (tmp_cpp_file) [--> happened right above this line]
      ## then the additional stuff (transport_add)
      ## then the transport terms (transport_org)
      
      writeLines(unlist(transport_add), con = cpp_file_write_connection)
      writeLines(unlist(transport_org), con = cpp_file_write_connection)
      close(cpp_file_write_connection)
      
      rm(list = c("additional_auxiliaries_list", "node_from", "add_aux"))
      
    }
    
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 5. Add new transport terms to auxiliary ra.cpp
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## ra.cpp ####
    if(grepl("ra", cpp_file)){
      
      save_last_line <- grep("#define", x = cpp_file_org_content, value = T)
      length_tmp_cpp_file <- 0
      
      transport_add_ra <- list()
      transport_org_ra <- list()
      for(sel_node in nodes){
        
        # sel_node <- nodes[2]
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), cpp_file_org_content, perl = T) ## add node numbers
        
        ## remove line with #define on it, should be added after all nodes have been added
        tmp_cpp_file <- tmp_cpp_file[-grep("#define", x = tmp_cpp_file)]
        
        
        if(sel_node %in% dfNETWORK$node_nr_to){
          
          upstTotNames <- gsub("(^w[[:alnum:]]{1,2})(TranW)(Hyp|Epi)(UpstTot)(N[[:digit:]]+)($|.+)",
                               "\\1\\2\\3\\4\\5", 
                               makeTranUpstTotFormulas(dfSUBSTANCES = dfSUBST, sel_node = sel_node))
          
          ## add additional transport terms to ra.cpp
          transport_add_ra[[paste0("node_", sel_node)]] <- c(paste0("double &_",
                                                                    dfSUBST$tran_subst, 
                                                                    "UpstN", 
                                                                    sel_node,
                                                                    "_ = auxil[0];"),
                                                             paste0("double &_",
                                                                    upstTotNames,
                                                                    "_ = auxil[0];"))
          
          
        }
        
        ## remove the phytoplankton from the original, so we can paste them near the bottom
        ## this is to keep the right order of the derivatives
        ## otherwise the phytoplankton totals appear above the part where they are calculated
        phyt_id <- grep(x = tmp_cpp_file, 
                        pattern = "TranPhyt",
                        ignore.case = TRUE)
        tran_id <- grep(x = tmp_cpp_file, 
                        pattern = paste0(paste0("_", dfSUBST$tran_subst, "N", sel_node, "_"), collapse = "|"),
                        ignore.case = TRUE)
        transport_org_ra[[paste0("node_", sel_node)]] <- tmp_cpp_file[c(tran_id, phyt_id)]
        tmp_cpp_file <- tmp_cpp_file[-c(tran_id, phyt_id)]
        
        
        ## adjust the sequence of numbers (only applicable to cpp files with the letter "r")
        for(position_in_file in 1:length(tmp_cpp_file)){
          # position_in_file <- 1
          tmp_cpp_file[position_in_file] <- gsub("([[:alpha:]]+)([[0-9]+])(;)", 
                                                 paste0("\\1[", (position_in_file-1)+(length_tmp_cpp_file), "]\\3"), 
                                                 tmp_cpp_file[position_in_file], 
                                                 perl = T)
        }
        
        length_tmp_cpp_file <- length_tmp_cpp_file + length(tmp_cpp_file)
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
        
        
      }
      
      temp <- c(unlist(transport_add_ra), unlist(transport_org_ra))
      for(position_in_file in 1:length(temp)){
        # position_in_file <- 1
        temp[position_in_file] <- gsub("([[:alpha:]]+)([[0-9]+])(;)", 
                                       paste0("\\1[", (position_in_file-1)+(length_tmp_cpp_file), "]\\3"), 
                                       temp[position_in_file], 
                                       perl = T)
      }
      temp <- gsub("_", "", temp)
      length_tmp_cpp_file <- length_tmp_cpp_file + length(temp)
      
      writeLines(temp, con = cpp_file_write_connection)
      
      writeLines(gsub("_", "", gsub("([[:upper:]_]+ )([0-9]+)", paste0("\\1", length_tmp_cpp_file), save_last_line, perl = T)),
                 con = cpp_file_write_connection) 
      
      close(cpp_file_write_connection)
    }  
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 6. Adjust derivative function (add transport to the error check term)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## sd.cpp ####
    if(grepl("sd", cpp_file)){
      
      for(sel_node in nodes){
        
        ## sel_node <- nodes[2]
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), cpp_file_org_content, perl = T) ## remove underscores
        
        ## for each node that has an incoming fraction of water, make sure the tranport fractions are complete for the mass balance error calculation
        if(sel_node %in% dfNETWORK$node_nr_to){
          ## add the additional transport fractions to the ExtTotT terms, so as to keep the mass balance intact!
          ## but skip O2, as O2 already accounts for the transport fraction
          # tmp_cpp_file[grep("(^_d)([[:alnum:]]{1,2})(ExtTotTN)([[:digit:]]_)(.*)(;)", tmp_cpp_file, perl = T)]
          tmp_cpp_file <- gsub("(^_d)([^O]{1,2})(ExtTotT)(N[[:digit:]]+_)(.*)(;)", 
                               "\\1\\2\\3\\4\\5 + (_w\\2TranWEpiUpstTot\\4 * _uDepthWEpi\\4) + (_w\\2TranWHypUpstTot\\4 * _uDepthWEpi\\4)\\6", tmp_cpp_file)
          
        }
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
      }
      
      close(cpp_file_write_connection)
    } 
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 6. Adjust all other files with the node numbers 
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## other.cpp ####
    if(!grepl("rp|sc|sp|sa|ra|sd", cpp_file)){
      
      save_last_line <- grep("#define", x = cpp_file_org_content, value = T)
      length_tmp_cpp_file <- 0
      
      for(sel_node in nodes){
        
        # sel_node <- nodes[1]
        
        ## add the node numbers
        tmp_cpp_file <- gsub(build_pattern, paste0("\\1N", sel_node, "\\2"), cpp_file_org_content, perl = T) ## add node numbers
        
        ## remove line with #define on it, should be added after all nodes have been added
        if(grepl("r[[:alpha:]].cpp", cpp_file)){tmp_cpp_file <- tmp_cpp_file[-grep("#define", x = tmp_cpp_file)]}
        
        ## adjust the sequence of numbers (only applicable to cpp files with the letter "r")
        if(grepl("r[[:alpha:]].cpp", cpp_file)){
          for(position_in_file in 1:length(tmp_cpp_file)){
            # position_in_file <- 1
            tmp_cpp_file[position_in_file] <- gsub("([[:alpha:]]+)([[0-9]+])(;)", 
                                                   paste0("\\1[", (position_in_file-1)+(length_tmp_cpp_file), "]\\3"), 
                                                   tmp_cpp_file[position_in_file], 
                                                   perl = T)
          }
          
          length_tmp_cpp_file <- length_tmp_cpp_file + length(tmp_cpp_file)
        }
        
        
        
        ## remove underscores for easy R access
        tmp_cpp_file <- gsub("_", "", tmp_cpp_file)
        
        ## write info to file
        writeLines(tmp_cpp_file, con = cpp_file_write_connection) ## write adjusted information to new file
        
      }
      
      writeLines(gsub("_", "", gsub("([[:upper:]_]+ )([0-9]+)", paste0("\\1", length(tmp_cpp_file)*length(nodes)), save_last_line, perl = T)),
                 con = cpp_file_write_connection) 
      
      close(cpp_file_write_connection)
      
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 7. Determine the length of the declaration arrays and store them
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## array.cpp ####
    if ((grepl("ra", cpp_file) || grepl("rp", cpp_file) || grepl("rs", cpp_file) || grepl("ri", cpp_file))) {  
      
      array_name <- substring(tmp_cpp_file[1], regexpr("=", tmp_cpp_file[1])[1]+2, regexpr("\\[", tmp_cpp_file[1])[1]-1)
      array_length <- length_tmp_cpp_file
      writeLines(paste("static double ", array_name, "[", array_length, "];", sep=""), con = cpp_array_connection) ## write length of declaration arrays to arrays.cpp file
      
    }
    
  }
  
  close(cpp_array_connection)
  
  ## END of PART A
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PART B: Adjust the model.cpp file
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## edit c++ model (scripts/cpp2R/model_base.cpp) for compilation:
  ## 1. define output auxiliaries
  ## 2. define forcing functions 
  ## 3. refer to the right model version name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## model.cpp ####
  model_base_cpp <- readLines(file.path(dir_MODEL_adj, "model_base.cpp")) # read the c++ model
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 1. set output for auxiliaries
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## NB if you make a change here, don't forget to update the PCModelNetworkRun too!!
  
  id             <- grep(x = model_base_cpp, pattern = "output_auxiliaries")
  codelines      <- vector()
  
  aux_names <- makeAuxNames_PCLakeSplus(dfAUXILARIES = dfAUXIL, 
                                        dfSUBSTANCES = dfSUBST, 
                                        vNODES = nodes, 
                                        dfNETWORK = dfNETWORK)
  
  ## get the length
  aux_number  <- length(aux_names)
  
  i              <- 0
  if (length(aux_names)>0) {
    for (aux_name in aux_names) { # define user-defined output auxiliaries as output_auxiliaries
      codelines <- c(codelines,paste("  yout[",i,"] = ", aux_name, ";", sep = "")) 
      i <- i + 1
    }
  } else { # if there are no output auxiliaries; make at least one 'dummy' output auxiliary, as desired by DeSolve
    codelines   <- "  yout[0]=0;"
    aux_number  <- 1
    aux_names   <- "dummy"
    aux_units   <- "-"
  }
  
  model_cpp <- c(model_base_cpp[1:(id-1)], codelines, model_base_cpp[(id+1):length(model_base_cpp)])
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 2. set forcing functions 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  vFORCING_NAMES <- names(lFORCINGS[[1]]) ## always grab the first set, otherwise you will always work with the last set due to the previous section, should not matter because you have to have the same forcings everywhere, but I think this is a more intuitive choice
  if('time' %in% vFORCING_NAMES){ vFORCING_NAMES	= vFORCING_NAMES[-which(vFORCING_NAMES=='time')]} ## note that we remove the time forcing, as that would lead to double definitions
  vFORCING_NAMES_adj_p <- paste0(vFORCING_NAMES,
                                 "N", 
                                 sort(rep(nodes, length(vFORCING_NAMES))))
  vFORCING_NAMES_adj <- c(vFORCING_NAMES_adj_p, dfNETWORK$param_names) 
  
  id        <- grep(x = model_cpp, pattern = "input_forcings")
  codelines <- paste("static double forc[", (1+length(vFORCING_NAMES_adj)), "];", sep="")
  codelines <- c(codelines, "double &time = forc[0];") # define time as an external forcing
  i         <- 0
  
  for (name in vFORCING_NAMES_adj) { # define user-defined forcings as external forcings
    i         <- i + 1
    codelines <- c(codelines, paste("double &", name, " = forc[",i,"];", sep=""))
  }
  codelines <- c(codelines,paste("#define MAXFORC ", (1+length(vFORCING_NAMES_adj)), sep=""))
  model_cpp <- c(model_cpp[1:(id-1)], codelines, model_cpp[(id+1):length(model_cpp)])
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. refer to the right model version
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cpp_files     <- list.files(file.path(dirWORKCASE, "source_cpp"), pattern = ".cpp") ## this is only to get the model version name!
  stop_id       <- regexpr(pattern = "...cpp", cpp_files[1])[[1]]-1
  model_version <- substr(cpp_files[1], start = 1, stop = stop_id) #get model version
  model_cpp     <- sub(pattern = "model_version", replacement = model_version, x = model_cpp) # insert model version into c++ file
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. write to file
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # writeLines(model_cpp, file.path(dirSHELL,"scripts", "cpp2R", "model.cpp"))
  writeLines(model_cpp, file.path(dir_MODEL_adj, "model.cpp"))
  
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelCompileModelWorkCase
##
##   Compiling the model, a.k.a. building a .dll on windows or a .so on mac
##
##   Arguments are:
##   - dirHOME = the directory in which the folder 'PCModel1350' can be found
##   - nameWORKCASE = the name of the work case folder you're working in
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelCompileModelWorkCase <- function(dirSHELL, nameWORKCASE) { 
  
  ## define directory of model.cpp file
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  dir_MODEL_adj <-	file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code") 
  
  setwd(dir_MODEL_adj)
  # system("compile_model_cpp.cmd", show.output.on.console = T, invisible = FALSE) ## testing 
  file.remove("model.o", "model.dll")
  # file.remove("model.dll")
  # system("R CMD SHLIB model.cpp") 
  out <- system("R CMD SHLIB model.cpp", intern=TRUE)
  nISERROR <- grep("Error", out[length(out)])
  return(ifelse(length(nISERROR)==0, "Model compiled successfully", "Model compile error"))
  
  # system(paste("R --arch x64 CMD SHLIB ", dir_SCHIL,"scripts/cpp2R/","model.cpp",sep=""))
  ## NB. can throw error on architecture x64
  # system("R --arch x64 CMD SHLIB model.cpp") 
}

## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelInitializeModel
##
## Initialization PCLake+ model
##
## Arguments are: 
## - lDATM = the name of the DATM object
## - dfRUN_SETTINGS = optional, can be used when lDATM is missing; lDATM$run_settings,
## - dfSTATES = optional, can be used when lDATM is missing; lDATM$states,
## - dfPARAMS = optional, can be used when lDATM is missing; lDATM$params, 
## - dirSHELL = the directory in which the work_cases folder can be found
## - nameWORKCASE = the name of the work case folder you're working in
##
## Due to the way the function is written, dfSTATES/dfPARAMS will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
## or on it own argument (when one is given) 
## using lDATM is the simple way to get to dfSTATES and dfPARAMS, lDATM itself is not used beyond the df's for states and params
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelInitializeModel <- function(lDATM = NULL, 
                                   dfRUN_SETTINGS = lDATM$run_settings,
                                   dfSTATES = lDATM$states,
                                   dfPARAMS = lDATM$params, 
                                   dirSHELL,
                                   nameWORKCASE) {
  
  ## due to the way the function is written, dfSTATES/dfPARAMS will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
  ## or on its own argument (when one is given) 
  ## using lDATM is the simple way to get to dfSTATES and dfPARAMS, lDATM itself is not used beyond the df's for states and params
  
  ## debug
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # dfSTATES <- lDATM_SETTINGS$states
  # dfPARAMS <- lDATM_SETTINGS$params
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  
  ## error catching
  if(is.null(lDATM)==TRUE){
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with states to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with states to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  ## define directories of files
  #dir_CPP =	file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp")  
  #dir_CPP_adj =	file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp_adjusted")  
  dir_DLL =	file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code")     
  
  ## prepare the dataframe to connect the output to
  dfSTATES_RESULTS <- as.data.frame(dfSTATES[, which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
  
  ## load initial states calculation
  dyn.load(file.path(dir_DLL, "model.dll"))
  ini <- function(parm, y, nr_of_states){.C("InitializeModel", param = parm, initState = y, state = double(nr_of_states))}
  
  ## loop over all runs in the dfRUN_SETTINGS
  for(i in colnames(dfRUN_SETTINGS)){
    print(i)
    # i <- colnames(dfRUN_SETTINGS)[1]
    state_set <- dfRUN_SETTINGS["iStateSet", i]
    param_set <- dfRUN_SETTINGS["iParamSet", i]
    
    sel_states <- dfSTATES[, state_set + 3]
    names(sel_states)	=	dfSTATES$sInitialStateName
    
    sel_params <- dfPARAMS[, param_set + 4]
    names(sel_params) <- row.names(dfPARAMS)
    # sel_params[which(is.na(sel_params)==T)] <- 0
    
    ## calculate initial states
    calc_inits <- ini(sel_params, sel_states, nrow(dfSTATES))
    calc_inits_sel <- calc_inits$state ## get initial values of state variables
    names(calc_inits_sel) <- row.names(dfSTATES) ## combine name and value
    
    ## plak alles aan elkaar vast: 
    dfSTATES_RESULTS <- cbind.data.frame(dfSTATES_RESULTS, new_col = calc_inits_sel)
    colnames(dfSTATES_RESULTS)[which(colnames(dfSTATES_RESULTS) == "new_col")] <- paste0("runSettings_", i)
    
  }
  
  dyn.unload(file.path(dir_DLL, "model.dll")) ## decouple model.dll
  
  return(dfSTATES_RESULTS)
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelInitializeModel_PCLakeSplus
##
## Initialization PCLakeS+ model, this function creates all initial states based on the states given, 
##   rather than the user selecting one lake for which initial states should be calculated (as is the case for PCModelInitializeModel). 
##
## Arguments are: 
## - lDATM = the name of the DATM object
## - dfSTATES = optional, can be used when lDATM is missing; lDATM$states,
## - dfPARAMS = optional, can be used when lDATM is missing; lDATM$params, 
## - dfNETWORK = the name of the data frame containing the edges/the network
## - dirSHELL = the directory in which the work_cases folder can be found
## - nameWORKCASE = the name of the work case folder you're working in
##
## Due to the way the function is written, dfSTATES/dfPARAMS will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
## or on it own argument (when one is given) 
## using lDATM is the simple way to get to dfSTATES and dfPARAMS, lDATM itself is not used beyond the df's for states and params
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelInitializeModel_PCLakeSplus <- function(lDATM = NULL, 
                                               dfSTATES = lDATM$states,
                                               dfPARAMS = lDATM$params, 
                                               dfNETWORK,
                                               dirSHELL,
                                               nameWORKCASE) {
  

  # # debug
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # dfSTATES <- lDATM_SETTINGS$states
  # dfPARAMS <- lDATM_SETTINGS$params
  # dirHOME <- dirHome
  # nameWORKCASE <- nameWorkCase
  # dfNETWORK <- dfNetwork
  
  loadPackage("plyr")
  
  ## error catching
  if(is.null(lDATM)==TRUE){
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with states to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with states to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  if(is.null(dfNETWORK)==TRUE){stop("Please add a data frame with the network information to the dfNETWORK argument.")}
  
  
  ## define directories of files
  dir_CPP_adj =	file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp_adjusted")  
  dir_DLL =	file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code")     
  
  ## get the nodes in the network
  nodes <- sort(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to))[which(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to)) >= 0)])
  
  ## get names of cpp files to be adjusted
  cpp_sp_file <- readLines(list.files(dir_CPP_adj, pattern = "sp.cpp", full.names = T)) ## parameters
  sp_file <- data.frame(do.call(rbind, strsplit(cpp_sp_file, split = " = ")))
  vPARAMS <- as.numeric(as.character(gsub(";", "", sp_file$X2)))
  names(vPARAMS) <- as.character(sp_file$X1)
  
  cpp_sc_file <- readLines(list.files(dir_CPP_adj, pattern = "sc.cpp", full.names = T)) ## initial states
  sc_file <- data.frame(do.call(rbind, strsplit(cpp_sc_file, split = " = ")))
  sc_file$X3 <- mapvalues(sc_file$X1, from = paste0(dfSTATES$sInitialStateName, "N", rep(nodes, each = nrow(dfSTATES))),
                          to = paste0(rownames(dfSTATES), "N", rep(nodes, each = nrow(dfSTATES))))
  vSTATES <- as.numeric(as.character(gsub(";", "", sc_file$X2)))
  names(vSTATES) <- as.character(sc_file$X3)
  # tail(names(vSTATES))
  
  ## prepare the states dataframe to connect the output to
  lSTATES <- list()
  for(sel_node in nodes){
    # sel_node <- nodes[1]
    lSTATES[[paste0("node_", sel_node)]] <- as.data.frame(dfSTATES[, which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
    rownames(lSTATES[[paste0("node_", sel_node)]]) <- paste0(rownames(lSTATES[[paste0("node_", sel_node)]]), "N", sel_node)
    lSTATES[[paste0("node_", sel_node)]]$"sInitialStateName" <- paste0(lSTATES[[paste0("node_", sel_node)]]$"sInitialStateName", "N", sel_node)
  }
  dfSTATES_RESULTS <- do.call("rbind", lSTATES)
  row.names(dfSTATES_RESULTS) <- gsub("node_[[:digit:]]+\\.", "", row.names(dfSTATES_RESULTS), perl = T)
  
  
  
  ## load initial states calculation & calculate the initial states
  dyn.load(file.path(dir_DLL, "model.dll"))
  ini <- function(parm, y, nr_of_states){.C("InitializeModel", param = parm, initState = y, state = double(nr_of_states))}
  calc_inits <- ini(vPARAMS, vSTATES, nrow(dfSTATES_RESULTS))
  dfSTATES_RESULTS <- cbind.data.frame(dfSTATES_RESULTS, initStates = calc_inits$state)
  
  dyn.unload(file.path(dir_DLL, "model.dll")) ## decouple model.dll
  
  return(dfSTATES_RESULTS)
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCmodelSingleRun
##
## Run a single PCLake(+) model (one node - regular PCLake+ use).
##
## Arguments are: 
## - lDATM = the name of the DATM object
## - dfRUN_SETTINGS =  optional, can be used when lDATM is missing; lDATM$run_settings,
## - nRUN_SET
## - dfSTATES = optional, can be used when lDATM is missing; lDATM$states,
## - dfPARAMS = optional, can be used when lDATM is missing; lDATM$params, 
## - dfAUXIL = optional, can be used when lDATM is missing; lDATM$auxils
## - lFORCINGS = optional, can be used when lDATM is missing; lDATM$forcings,
## - intergrator_method = an integrator method from the package deSolve
## - dirHOME = the directory in which the folder 'PCModel1350' can be found
## - nameWORKCASE = the name of the work case folder you're working in
## - tAVERAGE = TRUE or FALSE. If TRUE the output data frame will be reduced to only one line (the average). Uses the start and end day of the control settings. 
##
## Due to the way the function is written, the 'optional' data frames will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
## or on it own argument (when one is given) 
## using lDATM is the simple way to get the data frames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

## Run a single instance of PCLake
PCmodelSingleRun <- function(lDATM = NULL,
                             dfRUN_SETTINGS = lDATM$run_settings,
                             nRUN_SET,
                             dfSTATES = lDATM$states, 
                             dfPARAMS = lDATM$params, 
                             dfAUXIL = lDATM$auxils, 
                             lFORCINGS = lDATM$forcings,
                             integrator_method,
                             dirHOME,
                             nameWORKCASE,
                             tAVERAGE = FALSE){
  
  ## debug
  # lDATM          <- lDATM_SETTINGS     ## list with all PCmodel settings
  # dfRUN_SETTINGS  <- lDATM$run_settings ## default df with PCmodel control settings
  # nRUN_SET       <- 0                  ## the run set which will be used to run the model
  # dfSTATES       <- InitStates         ## default df with PCmodel states;
  # dfPARAMS       <- lDATM$params       ## default df with PCmodel parameters;
  # dfAUXIL        <- lDATM$auxils       ## default df with PCmodel auxiliaries
  # lFORCINGS      <- lDATM$forcings     ## default df with PCmodel forcings
  # integrator_method <- "vode"          ## selected integrator method
  # dirHOME <- dirHome
  # nameWORKCASE   <- nameWorkCase
  # tAVERAGE       <- FALSE             ## use averaging on the output dataframe;
  
  ## check if the input data is present
  if(is.null(lDATM)==TRUE){
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with parameters to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfAUXIL)==TRUE){stop("Please add a data frame with auxilaries to the dfAUXILS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(lFORCINGS)==TRUE){stop("Please add a list of matrices with forcings (one for each forcing parameter) to the dfFORCINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with run settings to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  ## define the model dll location
  dirMODEL <- paste0(dirHOME, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/work_cases/", nameWORKCASE, "/model_code/")
  
  
  
  ##== Get run data ====
  run_set     <- which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))
  state_set   <- dfRUN_SETTINGS["iStateSet", run_set]
  param_set   <- dfRUN_SETTINGS["iParamSet", run_set]
  forcing_set <- names(lFORCINGS)[which(colnames(dfPARAMS)[4+set_param] == names(lFORCINGS))]

  sel_params <- dfPARAMS[, param_set + 4]
  names(sel_params) <- rownames(dfPARAMS)
  
  sel_inistates	<- dfSTATES[, state_set + 3]
  names(sel_inistates) <- rownames(dfSTATES)  
  
  sel_forcings <- lFORCINGS[[forcing_set]]
  
  
  
  ##== Run settings (control panel) ==== 
  
  ## define time over which results will be reported
  fREP_START_YEAR <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS) == "dRepStart"), run_set] 
  
  ## Define time over which results will be averaged for e.g. bifurcation analysis 
  ## Generally refers to a summer growing season period of e.g. day 150-210 (standard setting PCLake) or day 91-259 (the summer half of the year, 1 April to 30 Sept)
  fAVG_START_YEAR <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgStart"), run_set]
  fAVG_START_DAY	<- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgStartWithinYear"), run_set]
  fAVG_END_DAY    <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgEndWithinYear"), run_set]
  
  ## timestep at which derivatives are calculated (if integrator uses fixed time step)
  internal_time_step <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dIntStep"), run_set]    
  runtime_years      <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dReady"), run_set] # model run time (in years)
  output_time_step   <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dRepStep"), run_set] # time step at which output is generated (in days)
  times              <- seq(0, 365*runtime_years, by = output_time_step) # output time step, in dagen
  

  ##== Output settings ====
  state_names <- rownames(dfSTATES[which(dfSTATES$iReportState == 1),, drop = F])
  aux_names 	<- rownames(dfAUXIL[which(dfAUXIL$iReport == 1),, drop = F])
  

  ## == Run the model ====
  int        <- integrator_method
  error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates, 
                                                                times, 
                                                                sel_params, 
                                                                sel_forcings, 
                                                                length(aux_names), 
                                                                aux_names, 
                                                                int, 
                                                                state_names, 
                                                                internal_time_step, 
                                                                dirMODEL)), error = function(e) e))[1] == "simpleError"
  if(any(is.na(output)) | error | nrow(output)<max(times)) {  # run the model again when integrator "vode" returns negative or NA outputs, rerun with integrator "daspk"
    int        <- "ode45"
    error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates, 
                                                                  times,
                                                                  sel_params,
                                                                  sel_forcings,
                                                                  length(aux_names), 
                                                                  aux_names,
                                                                  int,
                                                                  state_names,
                                                                  internal_time_step,
                                                                  dirMODEL)), error = function(e) e))[1] == "simpleError"
    if(any(is.na(output)) | error| nrow(output)<max(times)) { # run the model again when integrator "daspk" returns negative or NA outputs, rerun with integrator "euler"
      int        <- "euler"
      error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates,
                                                                    times,
                                                                    sel_params,
                                                                    sel_forcings,
                                                                    length(aux_names),
                                                                    aux_names,
                                                                    int,
                                                                    state_names,
                                                                    0.003,
                                                                    dirMODEL)),error = function(e) e))[1] == "simpleError"
      if(any(is.na(output)) | error| nrow(output)<max(times)) { # run the model again when integrator "euler" returns negative or NA outputs, rerun with integrator "euler" with timestep 0.001
        error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates,
                                                                      times,
                                                                      sel_params,
                                                                      sel_forcings,
                                                                      length(aux_names),
                                                                      aux_names,
                                                                      int,
                                                                      state_names,
                                                                      0.001,
                                                                      dirMODEL)), error = function(e) e))[1] == "simpleError"
      }
    }
  }
  
  output <- as.data.frame(subset(output, subset = (time %in% c((fREP_START_YEAR*365):max(times)))))							
  
  if(tAVERAGE == FALSE){					
    return(output)
  }else{
    return(output_avg <- as.data.frame(t(colMeans(subset(output, 
                                                         subset = (time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY))))))))
  }
  
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCModelNetworkRun
##
## Run a PCLakeS+ model (multiple nodes).
##
## Arguments are: 
## - lDATM = the name of the DATM object
## - dfRUN_SETTINGS =  optional, can be used when lDATM is missing; lDATM$run_settings,
## - dfSTATES = optional, can be used when lDATM is missing; lDATM$states,
## - dfPARAMS = optional, can be used when lDATM is missing; lDATM$params, 
## - dfAUXIL = optional, can be used when lDATM is missing; lDATM$auxils
## - lFORCINGS = optional, can be used when lDATM is missing; lDATM$forcings,
## - dfNETWORK = the name of the data frame containing the edges/the network
## - lFRACFORC = the name of the list containing the forcings for the water transport fractions in the network
## - dfSUBST = the name of the data frame listing the substances you're using for transport
## - integrator_method = an integrator method from the package deSolve
## - dirSHELL = the directory in which the work_cases folder can be found
## - nameWORKCASE = the name of the work case folder you're working in
##
## Due to the way the function is written, the 'optional' data frames will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
## or on it own argument (when one is given) 
## using lDATM is the simple way to get the data frames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelNetworkRun <- function(lDATM = NULL,
                              dfRUN_SETTINGS = lDATM$run_settings,
                              dfSTATES = lDATM$states, 
                              dfPARAMS = lDATM$params, 
                              dfAUXIL = lDATM$auxils,
                              lFORCINGS = lDATM$forcings,
                              dfNETWORK,
                              lFRACFORC,
                              dfSUBST,
                              integrator_method,
                              dirSHELL,
                              nameWORKCASE){ 
  
  ## debug
  # lDATM             <- lDATM_SETTINGS
  # dfRUN_SETTINGS    <- lDATM$run_settings ## default df with PCmodel control settings
  # dfNETWORK         <- dfNetwork
  # lFRACFORC         <- lFracForc
  # dfSTATES          <- lDATM$states       ## default df with PCmodel states;
  # dfPARAMS          <- lDATM$params       ## default df with PCmodel parameters;
  # dfAUXIL           <- lDATM$auxils       ## default df with PCmodel auxiliaries
  # lFORCINGS         <- lDATM$forcings     ## default df with PCmodel forcings
  # integrator_method <- "vode"         ## selected integrator method
  # # dirHOME           <- "C:/Users/Lilith Kramer/PCModel/PCModel-master/Licence_agreement/I_accept/"	## location of the PCModel1350 folder
  # dirSHELL           <- dirShell
  # nameWORKCASE      <- nameWorkCase
  # dfSUBST <- tran_subst
  # 
  
  if(is.null(lDATM) == TRUE){
    if(is.null(dfRUN_SETTINGS) == TRUE){ stop("Please add a data frame with run settings to the dfRUNSETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES) == TRUE){ stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS) == TRUE){ stop("Please add a data frame with parameters to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfAUXIL) == TRUE){ stop("Please add a data frame with auxilaries to the dfAUXILS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(lFORCINGS) == TRUE){ stop("Please add a list of matrices with forcings (one for each forcing parameter) to the lFORCINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
   }
  
  ## define the model dll location
  dirMODEL <- file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code")
  dir_CPP_adj <- file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp_adjusted")	# location of PCShell
  
  
  ## model initialization
  dfSTATES <- PCModelInitializeModel_PCLakeSplus(lDATM = lDATM,
                                                 dirSHELL = dirSHELL,
                                                 dfNETWORK = dfNETWORK,
                                                 nameWORKCASE = nameWORKCASE)
  
  
  ##== Get run data ====
  
  ## get the nodes in the network
  nodes <- sort(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to))[which(unique(c(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to)) >= 0)])
  
  ## prepare the state & parameter vector to put the input in
  sel_inistates <- dfSTATES$initStates
  names(sel_inistates) <- rownames(dfSTATES)
  # tail(names(sel_inistates))
  
  ## get the parameters from the cpp file
  cpp_sp_file <- readLines(list.files(dir_CPP_adj, pattern = "sp.cpp", full.names = T)) ## parameters
  sp_file <- data.frame(do.call(rbind, strsplit(cpp_sp_file, split = " = ")))
  sel_params <- as.numeric(as.character(gsub(";", "", sp_file$X2)))
  names(sel_params) <- as.character(sp_file$X1)
  
  
  ## select the runs settings that are part of the network
  rs_cols <- which(dfRUN_SETTINGS["iRuniD",] %in% nodes) ## get the right columns
  loop_nodes <- dfRUN_SETTINGS[, rs_cols]
  rm(list = c("rs_cols"))
  
  sel_forcings <- lFORCINGS[[1]][1] ## get time
  for(i in unname(unlist(loop_nodes[rownames(loop_nodes) == "iRuniD",]))){
    # i <- unname(unlist(loop_nodes[rownames(loop_nodes) == "iRuniD",]))[5]
    ## get run info
    param_set <- loop_nodes["iParamSet", which(loop_nodes["iRuniD",] %in% i)]
    forcing_set <- lFORCINGS[[which(names(lFORCINGS) == colnames(dfPARAMS)[param_set + 4])]]
    forcing_set <- forcing_set[-which(names(forcing_set) == "time")]
    names(forcing_set) <- paste0(names(forcing_set), "N", i)
    sel_forcings <- c(sel_forcings, forcing_set)
  }
  
  sel_forcings <- c(sel_forcings, lFRACFORC)
  #tail(sel_params)
  
  
  ##== Run settings (control panel) ==== 
  
  ## grab settings from first column
  run_set <- 1
  
  ## define time over which results will be reported
  fREP_START_YEAR <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS) == "dRepStart"), run_set] 
  
  ## Define time over which results will be averaged for e.g. bifurcation analysis 
  ## Generally refers to a summer growing season period of e.g. day 150-210 (standard setting PCLake) or day 91-259 (the summer half of the year, 1 April to 30 Sept)
  fAVG_START_YEAR <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgStart"), run_set]
  fAVG_START_DAY	<- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgStartWithinYear"), run_set]
  fAVG_END_DAY    <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dAvgEndWithinYear"), run_set]
  
  ## timestep at which derivatives are calculated (if integrator uses fixed time step)
  internal_time_step <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dIntStep"), run_set]    
  runtime_years      <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dReady"), run_set] # model run time (in years)
  output_time_step   <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dRepStep"), run_set] # time step at which output is generated (in days)
  times              <- seq(0, 365*runtime_years, by = output_time_step) # output time step, in days
  
  
  ##== Output settings ====
  state_names <- rownames(dfSTATES[which(dfSTATES$iReportState == 1),, drop = F])
  aux_names <- makeAuxNames_PCLakeSplus(dfAUXILARIES = dfAUXIL, 
                                        dfSUBSTANCES = dfSUBST, 
                                        vNODES = nodes, 
                                        dfNETWORK = dfNETWORK)
  # tail(aux_names)        
  
  ## == run PCLake and store the output ====
  int        <- integrator_method
  if(!int %in% c("rk2","rk23","rk23bs","rk34f","rk45f","rk45ck",
                 "rk45e","rk45dp6","rk45dp7","rk78dp","rk78f",
                 "lsoda","lsode","lsodes","lsodar","vode","daspk",
                 "ode23","ode45", "radau","bdf","bdf_d","adams", 
                 "impAdams","impAdams_d","iteration", "euler","rk4")){
    stop("Integrator name does not exist. Please check spelling.")
  }
  
  output <- as.data.frame(RunModel(sel_inistates,
                                   times,
                                   sel_params,
                                   sel_forcings,
                                   length(aux_names),
                                   aux_names,
                                   int,
                                   state_names,
                                   internal_time_step,
                                   dirMODEL))
  
  output <- as.data.frame(subset(output, subset = (time %in% c((fREP_START_YEAR*365):max(times)))))							
  output$int <- int
  
  lDATM$output <- output
  
  return(lDATM)
  
}



