##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SECTION GENERAL
##
##  DESCRIPTION:
##    This file contains general functions
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Loading or installing a package
loadPackage <- function(package) {
  
  if (require(package, character.only = T)) {
    ## Library loaded
    cat(paste("\nPackage '",package,"' succesfully loaded.\n", sep = ""))
  } else {
    ## Library not loaded
    ## Try to install
    install.packages(package)
    if (require(package, character.only = T)) {
      ## Library loaded
    } else {
      # Library not loaded
      # Can't install package 
      cat(paste("\nCan't install package '", package, "'.\n", sep = ""))
    }
  }
}


## Supply the name of the forcing based on the name of a Read switch
matchSwitchToForcing <- function(namesActivatedForcings) {
  
  ## function which determines
  ## 1. the active forcings (forcing_parameter = 1) (stored in par_forcing)
  ## 2. the corresponding forcing variables to be read (stored in names_forcing)
  
  ## Old:
  ## Function would take the list of imposed forcings
  # imposed_forcings <- vFORCINGS_READ
  # par_forcing      <- names(imposed_forcings)[which(imposed_forcings==1)]
  # ReadNutFrac      <- as.numeric(imposed_forcings["ReadNutFrac"])
  # ReadTranFrac     <- as.numeric(imposed_forcings["ReadTranFrac"])
  
  ## debug
  # namesActivatedForcings <- unique(rownames(readOn))
  
  
  par_forcing <- namesActivatedForcings
  
  ReadNutFrac <- ifelse("ReadNutFrac" %in% namesActivatedForcings, 1, 0) 
  ReadTranFrac <- ifelse("ReadTranFrac" %in% namesActivatedForcings, 1, 0) 
  
  CalcMixDepth <- ifelse("calcMixDepth" %in% namesActivatedForcings, 1, 0)
  InitMixDepth <- ifelse("InitMixDepth" %in% namesActivatedForcings, 1, 0)
  
  forcings_par_aux <- c(ReadDepthW = "mDepthW",
                        ReadLOut = "mLOut",
                        ReadVWind = "mVWind",
                        ReadQPrec = "mQPrec",
                        ReadQEv = "mQEv",
                        ReadPLoadPhyt = "mPLoadPhytTotEpi",
                        ReadDLoadDet = "mDLoadDetEpi",
                        ReadVegShade = "mVegShade",
                        ReadDLoadIM = "mDLoadIMEpi")
  
  names_forcing <- character()
  
  for (name in par_forcing) {
    if (name %in% names(forcings_par_aux)) {
      names_forcing <- c(names_forcing,as.character(forcings_par_aux[which(names(forcings_par_aux)%in%name)]))
    }
    
    if(name == "InclStrat"){
      if(InitMixDepth == 1){
        names_forcing <- c(names_forcing, "mMixDepth")
      }
      
      if(CalcMixDepth == 0){
        names_forcing <- c(names_forcing, "mMixDepth")
      }

    }
    
    if (name == "ReadTemp"){
      names_forcing <- c(names_forcing, "mTempEpi", "mTempHyp")
    }
    
    if (name == "ReadStrat"){
      names_forcing <- c(names_forcing, "mStrat")
    }
    
    if (name == "ReadQIn"){
      names_forcing <- c(names_forcing, "mQInEpi", "mQInHyp")
    }
    
    if (name == "ReadQOut"){
      names_forcing <- c(names_forcing, "mQOutEpi", "mQOutHyp")
    }
    
    if (name == "ReadQGround"){
      names_forcing <- c(names_forcing, "mQInSeep", "mQOutInf")
    }
    
    if(ReadTranFrac == 1){
      
      names_forcing <- c(names_forcing,
                         "mQInEpi", "mQInHyp",
                         
                         "mPLoadPO4Epi", "mPLoadAIMEpi", "mDLoadGrenEpi", "mPLoadDiatEpi", "mPLoadBlueEpi", "mPLoadDetEpi", "mPLoadZooEpi",
                         "mNLoadNH4Epi", "mNLoadNO3Epi", "mNLoadGrenEpi", "mNLoadDiatEpi", "mNLoadBlueEpi", "mNLoadDetEpi", "mNLoadZooEpi",
                         "mDLoadDetEpi", "mDLoadIMEpi", "mDLoadGrenEpi", "mDLoadDiatEpi", "mDLoadBlueEpi", "mDLoadZooEpi",
                         "mSiLoadSiO2Epi", "mSiLoadDetEpi",
                         "mO2LoadEpi",
                         
                         "mPLoadPO4Hyp", "mPLoadAIMHyp", "mDLoadGrenHyp", "mPLoadDiatHyp", "mPLoadBlueHyp", "mPLoadDetHyp", "mPLoadZooHyp",
                         "mNLoadNH4Hyp", "mNLoadNO3Hyp", "mNLoadGrenHyp", "mNLoadDiatHyp", "mNLoadBlueHyp", "mNLoadDetHyp", "mNLoadZooHyp",
                         "mDLoadDetHyp", "mDLoadIMHyp", "mDLoadGrenHyp", "mDLoadDiatHyp", "mDLoadBlueHyp", "mDLoadZooHyp",
                         "mSiLoadSiO2Hyp", "mSiLoadDetHyp",
                         "mO2LoadHyp",
                         
                         "mPLoadBlueSurf", "mNLoadBlueSurf", "mDLoadBlueSurf")
    }
    
    if(ReadTranFrac == 0){
      
      if (name == "ReadPLoad") {
        if (ReadNutFrac == 0) {
          names_forcing <- c(names_forcing,"mPLoadEpi", "mPLoadEpiMult")
        } else if (ReadNutFrac == 1) {
          names_forcing <- c(names_forcing,"mPLoadPO4Epi","mPLoadOrgEpi")
        }
      }
      
      if (name == "ReadNLoad") {
        if (ReadNutFrac == 0) {
          names_forcing <- c(names_forcing,"mNLoadEpi")
        } else if (ReadNutFrac == 1) {
          names_forcing <- c(names_forcing,"mNLoadNH4Epi","mNLoadNO3Epi","mNLoadOrgEpi", "mNLoadPhytTotEpi")
        }
      }
    }
    
  }
  
  ## remove duplicated names (e.g. when ReadTranFrac is used AND ReadQIn is on, then mQInEpi is loaded 2x)
  names_forcing <- unique(names_forcing)
  
  return(names_forcing)
}


getForcingAndInterpolate <- function(forcing_name, workbook, metadata, timesteps){
  
  ## debug
  # workbook = wbDATM
  # forcing_name = names(lsFORCINGS_prep[[1]])[1]
  # timesteps = times_forcing
  # metadata = lsINTERMEDIATE[[1]]

  forcing_file_info <- metadata[metadata$param_names %in% forcing_name, "value"]
  
  if(!is.na(suppressWarnings(as.numeric(forcing_file_info)))){
    df_int <- data.frame(time = timesteps, value = as.numeric(forcing_file_info))
  }
  
  if(is.na(suppressWarnings(as.numeric(forcing_file_info)))){
  forcing_from_wb <- readWorksheet(workbook, sheet = forcing_file_info, startRow = 1, startCol = 1, endCol = 2)
  if(forcing_from_wb[nrow(forcing_from_wb), "dTime"] == -1){ forcing_from_wb <- forcing_from_wb[-nrow(forcing_from_wb), ]}
  list_int <- approx(x = forcing_from_wb$dTime, y = forcing_from_wb$dValue, xout = timesteps, method = "linear", rule = 2:1) #interpolate missing day values linearly
  df_int <- data.frame(time = list_int$x, value = list_int$y)
  }
  
  return(df_int)
}

## this function allows you to adjust the parameter settings to defaults for clay, peat or sand
adjustSedimentParamSettings <- function(dfPARAMS, paramset = c(0, 1, 2, 3), sediment_type = c("clay", "peat", "sand")){
  
  ## debug
  # dfPARAMS <- lDATM_SETTINGS$params
  # paramset <- 2
  # sediment_type <- "sand" 
  
  ## error catching
  if(length(paramset) > 1 | any(!(paramset %in% c(0, 1, 2, 3))) == TRUE){stop("Please enter one value between 0 and 3 for paramset")}
  if(length(sediment_type) > 1 | any(!(sediment_type %in% c("clay", "peat", "sand"))) == TRUE){stop("This option is not available. Please choose one of the following options: clay, peat, sand.")}
  
  
  dfPARAMS$temp_id <- row.names(dfPARAMS)
  
  ## fFeDim & fAlDIM = 0.1 * fLutum
  sediment <-  data.frame(par_name = c("fDTotS0","fDOrgS0","fDOrgSoil","fLutum","fFeDIM","fAlDIM"),
                          clay     = c(0.3, 0.08, 0.08, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          peat     = c(0.1, 0.25, 0.25, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          sand     = c(0.5, 0.08, 0.08, 0.03, 0.1 * 0.03, 0.1 * 0.03)) 
  
  dfPARAMS[, paramset + 4][match(sediment$par_name, dfPARAMS$temp_id)] <- sediment[, sediment_type]
  dfPARAMS$temp_id <- NULL
  
  return(dfPARAMS)
}

## read data from cpp file and adjust the values of variables
##   underscores in variable names are removed
setParameters <- function(codelines, pars){
  if (length(pars) > 0){
    for (i in 1:length(pars)){
      # i <- 1
      # codelines <- tmp
      # tmp[id]
      id <- grep(x = codelines, pattern = paste("_", names(pars)[i], "_", sep = ""), ignore.case = TRUE)
      items <- unlist(strsplit(codelines[id], "="))
      codelines[id] <- paste(items[1], "= ", as.numeric(pars[i]), ";", sep = "")
      
      if (length(id)==0) print(paste("Warning: defined parameter/initial conditions to be changed '", names(pars)[i], "' does not exist", sep = ""))
    }
  }
  return(codelines)
}


RunModel <- function(states, 
                     times_output, 
                     parms, 
                     forcings, 
                     aux_number, 
                     aux_names, 
                     integrator, 
                     state_names, 
                     internal_time_step, 
                     dirMODEL) {
  
  dyn.load(paste(dirMODEL, "model.dll", sep = ''))
  
  # solve differential equations 	
  if (integrator %in% c("rk2","rk23","rk23bs","rk34f","rk45f","rk45ck",
                        "rk45e","rk45dp6","rk45dp7","rk78dp","rk78f")) {
    outC <- rk(y = states, 
               times = times_output, 
               func = "CalculateDerivatives", 
               parms = parms, 
               dllname = "model", 
               initforc = "forcc", 
               forcings = forcings,
               initfunc = "initmod", 
               nout = aux_number, 
               outnames = aux_names, 
               method = integrator)
  } else if (integrator %in% c("lsoda","lsode","lsodes","lsodar","vode","daspk","ode23","ode45",
                               "radau","bdf","bdf_d","adams","impAdams","impAdams_d","iteration")) {    
    outC <- ode(y = states, 
                times = times_output, 
                func = "CalculateDerivatives", 
                parms = parms, 
                dllname = "model", 
                initforc = "forcc", 
                forcings = forcings, 
                initfunc = "initmod", 
                nout = aux_number, 
                outnames = aux_names, 
                method = integrator)
  } else if (integrator %in% c("euler","rk4")) { # fixed time step   
    outC <- ode(y = states, 
                times = times_output, 
                func = "CalculateDerivatives", 
                parms = parms, 
                dllname = "model",
                initforc = "forcc",
                forcings = forcings,
                initfunc = "initmod", 
                nout = aux_number, 
                outnames = aux_names, 
                method = integrator, 
                hini = internal_time_step)
  }
  
  # store output (only for selected state variables and auxiliaries)
  outC <- outC[, c(1, which(colnames(outC) %in% c(state_names, aux_names)))]
  
  dyn.unload(paste(dirMODEL,"model.dll",sep=''))
  return(assign("outC", outC, envir = .GlobalEnv))
}

