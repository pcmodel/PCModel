

## source helper functions
source(paste(dirHome, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/scripts/R_system/functions.R", sep=""))  	#load base functions by Luuk van Gerven (2012-2016)

## load necessary packages 
loadPackage("deSolve") ## load this before compilation, otherwise you can get namespace errors

# ***********************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCModelWorkCaseSetup
#
#   Function to set up the folder structure for working with PCModel in R.
#     Folders are only made if they were not present before 
#     and files are only copied into the folders if none were present yet.
#     No files will be removed or overwritten by this function. 
#
#   Arguments are: 
#	  - dirHOME = the directory in which the folder 'PCModel1350' can be found
#   - nameWORKCASE = the name of the main folder in which you want to keep all model information
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ***********************************************************************

PCModelWorkCaseSetup <- function(dirHOME, 
                                 nameWORKCASE){
  
  ## debug
  # nameWORKCASE <- work_case_name
  # dirHOME   <- dirHOME
  
  ## define directories of files
  dirSHELL    <- paste0(dirHOME,"PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/")
  dirWORKCASE <- paste0(dirSHELL, "work_cases/", nameWORKCASE, "/")
  
  ## if it didnot exist yet: create main directory
  if(!dir.exists(dirWORKCASE)) dir.create(dirWORKCASE, recursive = T, showWarnings = T)
  
  ## if it didnot exist yet: create directory for model output 
  if(!dir.exists(paste0(dirWORKCASE, "output/"))) dir.create(paste0(dirWORKCASE, "output/"), showWarnings = T)
  
  ## create a directory for cpp files containing model code and copy the cpp files to it
  ## cpp files = c++ code with the equations and initial settings
  ##   these are loaded in the script as the user may wish to create multiple different models (different cpp's) and compare them
  ##   In that case the user will have to compile multiple different DATM instances and save the cpp files to different folders,
  ##   the names of which can be looped through 
  if(!dir.exists(paste0(dirWORKCASE, "source_cpp/"))) dir.create(paste0(dirWORKCASE, "source_cpp/"), showWarnings = T)
  cpp_files <- list.files(file.path(dirHOME, paste("PCModel1350/PCModel/3.00/Frameworks/Osiris/3.01/PCLake_plus/", sep="")), full.names = TRUE)[
    which((lapply(strsplit(x = list.files(file.path(dirHOME, paste("PCModel1350/PCModel/3.00/Frameworks/Osiris/3.01/PCLake_plus/", sep="")), full.names = TRUE), split="[/]"), 
                  function(x) which(x %in% c("pl61316ra.cpp","pl61316rc.cpp","pl61316rd.cpp","pl61316ri.cpp","pl61316rp.cpp","pl61316rs.cpp",
                                             "pl61316sa.cpp","pl61316sc.cpp","pl61316sd.cpp","pl61316si.cpp","pl61316sp.cpp","pl61316ss.cpp")))>0)==TRUE)]		
  
  if(length(list.files(paste0(dirWORKCASE, "source_cpp/")))>0) print("The source_cpp folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(paste0(dirWORKCASE, "source_cpp/")))==0) file.copy(cpp_files, file.path(paste0(dirWORKCASE, "source_cpp")), overwrite = F)
  
  ## create a directory for the 'include files' for the model to be adjusted in
  if(!dir.exists(paste0(dirWORKCASE, "source_cpp_adjusted/"))) dir.create(paste0(dirWORKCASE, "source_cpp_adjusted/"), showWarnings = T)
  
  ## create a directory for extra input you might want to use in your model
  ## think of time series etc. This folder will not be cleared. I do not want to acccidentally remove raw data or some such. 
  if(!dir.exists(paste0(dirWORKCASE, "input/"))) dir.create(paste0(dirWORKCASE, "input/"), showWarnings = T)
  
  ## create a directory for the model code, this folder will be used for compilation
  ## copy template code in here
  if(!dir.exists(paste0(dirWORKCASE, "model_code/"))) dir.create(paste0(dirWORKCASE, "model_code/"), showWarnings = T)
  copy_model_files <- list.files(file.path(paste0(dirSHELL, "scripts/cpp2R")), full.names = TRUE)[
    which((lapply(strsplit(x = list.files(file.path(paste0(dirSHELL, "scripts/cpp2R")), full.names = TRUE), split="[/]"), 
                  function(x) which(x %in% c("compile_model_cpp.cmd", "model_base.cpp")))>0)==TRUE)]		
  
  if(length(list.files(paste0(dirWORKCASE, "model_code/")))>0) print("The model_code folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(paste0(dirWORKCASE, "model_code/")))==0)  file.copy(copy_model_files, file.path(paste0(dirWORKCASE, "model_code/")), overwrite = F)
  
  print(paste0("Work case ", nameWORKCASE, " is ready for use."))
  
}




# ***********************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCModelReadDATMFile_PCLakePlus
#
#   Function to read run settings, states, parameters, auxiliary data, and forcings from the DATM file
#     All column names from the excel sheets are hardcoded. 
#     All names from the forcing files are hardcoded.
#
#   Required: 
#     - Java (www.java.com)
#     - R packages rJava and XLConnect 
#
#    Arguments are:
#	   - dirHOME: the directory in which the folder 'PCModel1350' can be found
#    - fileDATM: the name of the DATM file you want to import (including the '.xls' extention)
#	
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ***********************************************************************


PCModelReadDATMFile_PCLakePlus <- function(dirHOME, fileDATM){
  
  ## debug
  # dirHOME = dirHome
  # fileDATM = "PL613162PLUS_20200914_LK4_8feb2021.xls"
  
  ## set java memory settings prior to loading java enabled packaged (e.g. rJava)
  options(java.parameters = "-Xmx4g")
  
  ## Loading (and if missing, installing) packages 
  loadPackage("rJava") 
  loadPackage("XLConnect")
  loadPackage("tidyr")

  ## Define path to DATM file (excel) 
  pathDATM <- paste0(dirHOME, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/", fileDATM)
  
  ## load the workbook
  wbDATM <- loadWorkbook(file = pathDATM)

  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load sheets from DATM (excel) file 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Run settings ----
  dfRUN_SETTINGS_PRICE_RAW	<- readWorksheet(wbDATM, sheet = "Control", startRow = 1, endRow = 90, startCol = 1, endCol = 5)
  dfRUN_SETTINGS1 <- dfRUN_SETTINGS_PRICE_RAW[complete.cases(dfRUN_SETTINGS_PRICE_RAW),]
  dfRUN_SETTINGS <- dfRUN_SETTINGS1[-1,-1]
  rownames(dfRUN_SETTINGS) <- as.character(unlist(dfRUN_SETTINGS1[-1, 1]))
  colnames(dfRUN_SETTINGS) <- as.character(unlist(dfRUN_SETTINGS1[1, -1]))
  for(nCOL in 1:ncol(dfRUN_SETTINGS)){ dfRUN_SETTINGS[, nCOL]	<-	as.numeric(gsub(",", ".", as.character(unlist(dfRUN_SETTINGS[, nCOL])))) }
  
  ## Initial states ----
  dfSTATES_PRICE_RAW <-	readWorksheet(wbDATM, sheet = "states", startRow = 1, endRow = 200, startCol = 1, endCol = 90)
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
  dfPARAMS_PRICE_RAW <- readWorksheet(wbDATM, sheet = "parameters", startRow = 1, endRow = 1200, startCol = 1, endCol = 90)
  dfPARAMS <- dfPARAMS_PRICE_RAW[which(dfPARAMS_PRICE_RAW[,2]!=""), which(colnames(dfPARAMS_PRICE_RAW) %in% c("sName",
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
  readOn <- which(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ] == 1, arr.ind = T)
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
    runtime_years <- dfRUN_SETTINGS[which(rownames(dfRUN_SETTINGS)=="dReady"), which(colnames(dfRUN_SETTINGS) == dfMATCH_RUN_PARAM[dfMATCH_RUN_PARAM$param_set == i, "run_set"])]
    times_forcing  <- seq(0, 365*runtime_years)
    
    
    ## make a placeholder list for the forcings
    lsFORCINGS_prep <- list()
    lsFORCINGS_prep[[i]] <- vector("list", length(lsINTERMEDIATE[[i]]$value))
    names(lsFORCINGS_prep[[i]]) <- lsINTERMEDIATE[[i]]$param_names
    
    
    ## use function to load all forcings and interpolate them as well
    lsFORCINGS_prep[[i]] <- sapply(names(lsFORCINGS_prep[[i]][which(names(lsFORCINGS_prep[[i]])!="time")]), 
                                   getForcingAndInterpolate, 
                                   workbook = wbDATM, 
                                   metadata = lsINTERMEDIATE[[i]],
                                   timesteps = times_forcing,
                                   simplify = F)
    
    lsFORCINGS[[i]] <- c(list(time = data.frame(time = times_forcing, value = times_forcing)), lsFORCINGS_prep[[i]])
  }
  

  ## Auxiliaries (output) ---- 
  dfAUXIL_PRICE_RAW	<- readWorksheet(wbDATM, sheet = "derivatives", startRow = 1, endRow = 2800, startCol = 1, endCol = 15)
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



## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Edit c++ files from PCLake/PCDitch (created by OSIRIS)
##
## Two parts are edited: 
##    - include files 
##    - the template for the model
##
## The include files are adjusted according to parameter settings, forcings and initial conditions
## The template model file is adjusted according to forcings, auxiliary output and model version name
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelAdjustCPPfiles <- function(dirHOME, 
                                  nameWORKCASE, 
                                  lDATM = NULL, 
                                  dfRUN_SETTINGS = lDATM$run_settings,
                                  nRUN_SET = c(0, 1, 2, 3), 
                                  dfPARAM = lDATM$params, 
                                  dfSTATES = lDATM$states,
                                  lFORCINGS = lDATM$forcings,
                                  dfAUXIL = lDATM$auxils){ 
  
  ## debug
  # dirHOME <- dirHome
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
  dir_SCHIL     =	paste0(dirHOME,"PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/")	# location of PCShell
  dir_SCEN      =	paste0(dir_SCHIL, "work_cases/", nameWORKCASE, "/")                         # location of work case
  dir_CPP_adj   = paste0(dir_SCEN, "source_cpp_adjusted/")                                    # location of output include cpp files
  dir_MODEL_adj = paste0(dir_SCEN, "model_code/")                                             # location of output model cpp file
  
  ## Get the right sets of data, based on the run
  set_state <- dfRUN_SETTINGS["iStateSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_param <- dfRUN_SETTINGS["iParamSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_forc  <- names(lFORCINGS)[which(colnames(dfPARAM)[4+set_param] %in% names(lFORCINGS))]
  
  
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
  cpp_files <- list.files(paste(dir_SCEN, "source_cpp/", sep = ""), pattern = ".cpp")
  
  ## placeholder for information about the declaration files. This information is used in the model.cpp!
  arrays <- vector()
  
  for(cpp_file in cpp_files){
    
    ## cpp_file <- cpp_files[5]
    
    tmp_cpp_file <- readLines(paste0(dir_SCEN, "source_cpp/", cpp_file))
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 1. Set parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    if (grepl("sp", cpp_file)) {
      
      all_param <- dfPARAM[, 4 + set_param] ## get all model parameters
      names(all_param) <- row.names(dfPARAM) ## make them into a names vector so SetParameters() understands the format
      
      tmp_cpp_file <- setParameters(tmp_cpp_file, all_param)  ## set user-defined parameters
      
      rm(list = c("all_param")) ## clean up
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 2. Set initial conditions 
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (grepl("sc", cpp_file)) { 
      
      all_inits <- dfSTATES[, 3 + set_state] ## get all model states
      names(all_inits) <- dfSTATES[, "sInitialStateName"] ## make them into a names vector so SetParameters() understands the format
      
      tmp_cpp_file <- setParameters(tmp_cpp_file, all_inits) ## set user-defined initial conditions
      
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
      writeLines(tmp_cpp_file2, paste0(dir_CPP_adj, gsub("rp.cpp", "rp2.cpp", cpp_file))) ## write adjusted information to new file
      
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
    writeLines(tmp_cpp_file, paste0(dir_CPP_adj, cpp_file)) ## write adjusted information to new file
  }
  
  writeLines(arrays, paste0(dir_CPP_adj, "arrays.cpp")) ## write length of declaration arrays to arrays.cpp file
  
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
  
  model_base_cpp <- readLines(paste0(dir_MODEL_adj, "model_base.cpp")) # read the c++ model
  
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
  codelines <- c(codelines,paste("#define MAXFORC ", (1+length(vFORCING_NAMES)), sep=""))
  model_cpp <- c(model_cpp[1:(id-1)], codelines, model_cpp[(id+1):length(model_cpp)])
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. refer to the right model version
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cpp_files     <- list.files(paste(dir_SCEN, "source_cpp/", sep = ""), pattern = ".cpp") ## this is only to get the model version name!
  stop_id       <- regexpr(pattern = "...cpp", cpp_files[1])[[1]]-1
  model_version <- substr(cpp_files[1], start = 1, stop = stop_id) #get model version
  model_cpp     <- sub(pattern = "model_version", replacement = model_version, x = model_cpp) # insert model version into c++ file
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. write to file
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  writeLines(model_cpp, paste(dir_SCHIL,"scripts/cpp2R/model.cpp",sep=""))
  writeLines(model_cpp, paste0(dir_MODEL_adj, "model.cpp"))
  
}



PCModelCompileModelWorkCase <- function(dirHOME, nameWORKCASE) { 
  ## define directory of model.cpp file
  dir_MODEL <-	paste0(dirHOME,
                       "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/work_cases/", 
                       nameWORKCASE, 
                       "/model_code/") 

  setwd(dir_MODEL)
  # system("compile_model_cpp.cmd", show.output.on.console = T, invisible = FALSE) ## testing 
  file.remove("model.o")
  file.remove("model.dll")
  #system("R CMD SHLIB model.cpp") 
  out <- system("R CMD SHLIB model.cpp", intern=TRUE)
  nISERROR <- grep("Error", out[length(out)])
  return(ifelse(length(nISERROR)==0, "Model compiled successfully", "Model compile error"))
  
  # system(paste("R --arch x64 CMD SHLIB ", dir_SCHIL,"scripts/cpp2R/","model.cpp",sep=""))
  ## NB. can throw error on architecture x64
  # system("R --arch x64 CMD SHLIB model.cpp") 
}


#initialization, cpp copying, cpp editing, etc
#	either integrated into each function
#		make into a function called within each function
#make into separate function and call manually
#	avoids calling when model is already set up, but risks that model is not set up
#		make a check to see if the model is set up already
PCModelInitializeModel <- function(lDATM = NULL, 
                                   dfRUN_SETTINGS = lDATM$run_settings,
                                   dfSTATES = lDATM$states,
                                   dfPARAMS = lDATM$params, 
                                   dirHOME,
                                   nameWORKCASE) {
  
  ## due to the way the function is written, dfSTATES/dfPARAMS will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
  ## or on it own argument (when one is given) 
  ## using lDATM is the simple way to get to dfSTATES and dfPARAMS, lDATM itself is not used beyond the df's for states and params
  
  ## debug
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # dfSTATES <- lDATM_SETTINGS$states
  # dfPARAMS <- lDATM_SETTINGS$params
  # dirHOME <- dirHome
  # nameWORKCASE <- nameWorkCase
  
  ## error catching
  if(is.null(lDATM)==TRUE){
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with states to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with states to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  ## define directories of files
  dir_SCHIL = paste0(dirHOME, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/")	# location of PCShell
  dir_CPP =	paste0(dir_SCHIL, "work_cases/", nameWORKCASE, "/source_cpp/")  
  dir_CPP_adj =	paste0(dir_SCHIL, "work_cases/", nameWORKCASE, "/source_cpp_adjusted/")  
  dir_DLL =	paste0(dir_SCHIL, "work_cases/", nameWORKCASE, "/model_code/")     
  
  ## prepare the dataframe to connect the output to
  dfSTATES_RESULTS <- as.data.frame(dfSTATES[, which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
  
  ## load initial states calculation
  dyn.load(paste(dir_DLL, "model.dll", sep = ""))
  ini <- function(parm, y, nr_of_states){.C("InitializeModel", param = parm, initState = y, state = double(nr_of_states))}

  ## loop over all runs in the dfRUN_SETTINGS
  for(i in colnames(dfRUN_SETTINGS)){
    
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
  
  dyn.unload(paste(dir_DLL, "model.dll", sep = "")) ## decouple model.dll
  
  return(dfSTATES_RESULTS)
}


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
  # dirHOME        <- "C:/Users/Lilith Kramer/PCModel/PCModel-master/Licence_agreement/I_accept/"	## location of the PCModel1350 folder
  # nameWORKCASE   <- nameWorkCase
  # tAVERAGE       <- FALSE             ## use averaging on the output dataframe; full dataframe will be reduced to only one line. Uses the start and end day of the control settings. 
  
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
  run_set     <- which(colnames(dfRUN_SETTINGS) == nRUN_SET)
  state_set   <- dfRUN_SETTINGS["iStateSet", run_set]
  param_set   <- dfRUN_SETTINGS["iParamSet", run_set]
  forcing_set <- which(names(lFORCINGS) == colnames(dfPARAMS)[param_set+4])
  
  
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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCModelNetworkRun: WORK IN PROGRESS (used to be PCmodelMultiRun)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


PCModelMultiRun <- function(lDATM = NULL,
                              dfRUN_SETTINGS = lDATM$run_settings,
                              nRUN_SET,
                              dfSTATES = lDATM$states, 
                              dfPARAMS = lDATM$params, 
                              dfAUXIL = lDATM$auxils,
                              lFORCINGS = lDATM$forcings,
                              runSTATUS = lDATM$run_status,
                              integrator_method,
                              dirHOME,
                              nameWORKCASE,
                              runTYPE = c("initialize", "restart"), ## run PCModelInitializeModel() on lDATM or not
                              dfRESTART = NULL){ ## which output to use for restart data (adjust initial states)
                             

  #  ## debug
    # lDATM             <- lDATM_SETTINGS_multi[[1]]
    # lDATM             <- lDATM_SETTINGS
    # dfRUN_SETTINGS    <- lDATM$run_settings ## default df with PCmodel control settings
    # nRUN_SET          <- 0
    # dfSTATES          <- lDATM$states       ## default df with PCmodel states;
    # dfPARAMS          <- lDATM$params       ## default df with PCmodel parameters;
    # dfAUXIL           <- lDATM$auxils       ## default df with PCmodel auxiliaries
    # lFORCINGS         <- lDATM$forcings     ## default df with PCmodel forcings
    # integrator_method <- "vode"         ## selected integrator method
    # dirHOME           <- dirHome
    # dirHOME           <- "C:/Users/Lilith Kramer/PCModel/PCModel-master/Licence_agreement/I_accept/"	## location of the PCModel1350 folder
    # nameWORKCASE      <- nameWorkCase
    # runTYPE           <- 'initialize'  # c("initialize", "restart")
    # dfRESTART         <- NULL

  ## error catching & check on input data
  if(length(runTYPE) > 1 | any(!(runTYPE %in%  c("initialize", "restart"))) == TRUE){stop("Please enter either the value 'initialize' or 'restart' for runTYPE.")}
  if(length(nRUN_SET) > 1 | length(which(colnames(dfRUN_SETTINGS) == nRUN_SET)) == 0){stop("Please enter ONE value that matches a column name of a run set in dfRUN_SETTINGS")}
 
  
   
  if(is.null(lDATM) == TRUE){
    if(is.null(dfRUN_SETTINGS) == TRUE){ stop("Please add a data frame with run settings to the dfRUNSETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES) == TRUE){ stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS) == TRUE){ stop("Please add a data frame with parameters to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfAUXIL) == TRUE){ stop("Please add a data frame with auxilaries to the dfAUXILS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(lFORCINGS) == TRUE){ stop("Please add a list of matrices with forcings (one for each forcing parameter) to the lFORCINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(runSTATUS) == TRUE){stop("The lDATM$run_status is missing from lDATM. You should provide the run_status (a list element containing one positive number indicating the amount of times the model has run.")}
  }
  
  ## define the model dll location
  dirMODEL <- paste0(dirHOME, "PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell/work_cases/", nameWORKCASE, "/model_code/")
  
  
  ## model initialization
  if(runTYPE == 'initialize'){
    
    dfSTATES <- PCModelInitializeModel(lDATM = lDATM,
                                       dirHOME = dirHOME,
                                       nameWORKCASE = nameWORKCASE)
    
  }
  
  ## CHECK THIS PART!! ----
  ## restart the model 
  if(runTYPE == 'restart'){
    if(is.null(dfRESTART)) {stop("dfRESTART is missing. Please add a data frame with the restart values to the dfRESTART argument.") }
    
    ## add restart data names here so people don't have to input this.
    # restart_data <- test_multi$output ## debug
    restart_data_sel <- restart_data[nrow(restart_data), colnames(restart_data) %in% c("time", restart_names)]
    
    # nSTATE_SET = 0
    restart_data_sel_long <- as.data.frame(tidyr::pivot_longer(data = restart_data_sel, cols = colnames(restart_data_sel)[2:length(colnames(restart_data_sel))], names_to = "variable", values_to = "value"))
    restart_data_sel_long$variable <- gsub("^a|^o|^u", "s", restart_data_sel_long$variable)
    ifelse(all(restart_data_sel_long$variable %in% rownames(lDATM$states)), 
           lDATM$states[restart_data_sel_long$variable, nSTATE_SET+3] <- restart_data_sel_long$value, 
           stop("column restart_data_sel_long$variables does not match rownames(lDATM$states)")) 
    
    
    dfSTATES <- PCModelInitializeModel(lDATM = lDATM,
                                       dirHOME = dirHOME,
                                       nameWORKCASE = nameWORKCASE)
    
    
  }
  
  
  ##== Get run data ====
  run_set     <- which(colnames(dfRUN_SETTINGS) == nRUN_SET)
  state_set   <- dfRUN_SETTINGS["iStateSet", run_set]
  param_set   <- dfRUN_SETTINGS["iParamSet", run_set]
  forcing_set <- which(names(lFORCINGS) == colnames(dfPARAMS)[param_set + 4])
  
  
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
  times              <- seq(0, 365*runtime_years, by = output_time_step) # output time step, in days
  
  
  ##== Output settings ====
  state_names <- rownames(dfSTATES[which(dfSTATES$iReportState == 1),, drop = F])
  aux_names 	<- rownames(dfAUXIL[which(dfAUXIL$iReport == 1),, drop = F])
  
  
  
  ## == run PCLake and store the output ====
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
                                                                dirMODEL)), 
                               error = function(e) e))[1] == "simpleError"
  
  if(exists("output") == FALSE){
    stop("Something went wrong while trying to run the model and the output could not be made. Please check the error of the RunModel function inside the PCModelMultiRun function.")
  }
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
                                                                  dirMODEL)), 
                                 error = function(e) e))[1] == "simpleError"
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
                                                                    dirMODEL)), 
                                   error = function(e) e))[1] == "simpleError"
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
                                                                      dirMODEL)), 
                                     error = function(e) e))[1] == "simpleError"
      }
    }
  }
  
  output <- as.data.frame(subset(output, subset = (time %in% c((fREP_START_YEAR*365):max(times)))))							
  
  # if(tAVERAGE==TRUE){	output <- as.data.frame(t(colMeans(subset(output, subset=(time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY)))))))}
  lDATM$output <- output
  
  lDATM$run_status <- runSTATUS + 1 
  
  return(lDATM)
  
}


