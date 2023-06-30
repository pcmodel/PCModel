##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PCLake(S)+ base/helper functions
##
##  DESCRIPTION
##    This script contains helper functions to run PCLake(S)+ in R
##    This script was developed by Lilith Kramer, Sven Teurlincx, 
##      and Luuk van Gerven.
##  
##  VERSION
##    30 Jun 2023
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Loading or installing a package
loadPackage <- function(package) {
  
  if (suppressWarnings(require(package, character.only = T))) {
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
  
  ## function that determines
  ## 1. the active forcings (forcing_parameter = 1) (stored in par_forcing)
  ## 2. the corresponding forcing variables to be read (stored in names_forcing)
  
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
    
    if (name == "ReadQOutAbst"){
      names_forcing <- c(names_forcing, "mQOutAbstEpi", "mQOutAbstHyp")
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
          names_forcing <- c(names_forcing,"mNLoadNH4Epi","mNLoadNO3Epi","mNLoadOrgEpi")
        }
      }
    }
  }
  
  ## remove duplicated names (e.g. when ReadTranFrac is used AND ReadQIn is on, then mQInEpi is loaded 2x)
  names_forcing <- unique(names_forcing)
  
  return(names_forcing)
}

## Interpolate forcings so there is a value for every timestep
getForcingAndInterpolate <- function(forcing_name, 
                                     location = c(NULL, "txt", "excel"),
                                     pathLoc = NULL,
                                     metadata, 
                                     timesteps){
  
  forcing_file_info <- metadata[metadata$param_names %in% forcing_name, "value"]
  
  ## if the value in the forcing is numeric, a.k.a. is constant, we can simply interpolate this value
  if(!is.na(suppressWarnings(as.numeric(forcing_file_info)))){
    df_int <- data.frame(time = timesteps, value = as.numeric(forcing_file_info))
  }
  
  ## if the value in the forcing is NOT numeric, a.k.a. refers to a file
  ## we need to load the info from the file
  ## however, this file can be either located in the workbook itself, or in the txt folder
  if(is.na(suppressWarnings(as.numeric(forcing_file_info)))){
    if(!location %in% c("txt", "excel")){stop("Please enter either 'txt' or 'excel' as value to location argument. Enter txt if you use the txt folder to enter your forcing data. Enter excel if your forcing data is located in sheets in your workbook.")}
    if(location == "excel"){
      if(is.null(pathLoc)){stop("Please enter a pathLoc.")}
      forcing_from_wb <- as.data.frame(read_excel(pathLoc, sheet = forcing_file_info))
    }
    if(location == "txt"){
      if(is.null(pathLoc)){stop("Please enter a pathLoc.")}
      forcing_from_wb <- read.delim(file.path(pathLoc, paste0(forcing_file_info, ".txt")))
    }
    
    ## remove -1 at end of file, if it is there
    if(forcing_from_wb[nrow(forcing_from_wb), "dTime"] == -1){ forcing_from_wb <- forcing_from_wb[-nrow(forcing_from_wb), ]} 
    
    ## interpolate
    list_int <- approx(x = as.numeric(as.character(forcing_from_wb$dTime)), y = as.numeric(as.character(forcing_from_wb$dValue)), xout = timesteps, method = "linear", rule = 2:1) ## interpolate missing day values linearly
    df_int <- data.frame(time = list_int$x, value = list_int$y)
  }
  return(df_int)
}

## Adjust the sediment parameter settings - including the bank - to defaults for clay, peat or sand
adjustSedimentParamSettings_inclBank <- function(dfPARAMS, paramset = c(0, 1, 2, 3), sediment_type = c("clay", "peat", "sand")){
  
  ## error catching
  if(length(paramset) > 1 | any(!(paramset %in% c(0, 1, 2, 3))) == TRUE){stop("Please enter one value between 0 and 3 for paramset")}
  if(length(sediment_type) > 1 | any(!(sediment_type %in% c("clay", "peat", "sand"))) == TRUE){stop("This option is not available. Please choose one of the following options: clay, peat, sand.")}
  
  dfPARAMS$temp_id <- row.names(dfPARAMS)
  
  ## fFeDim & fAlDIM = 0.1 * fLutum
  ## fDTotS0 = dry matter
  ## fDOrgSo = organic matter
  ## fDOrgSoil = organic matter component in the banks (connected to erosion)
  ## fLutum = lutum
  ## fFeDIM = iron component
  ## fAlDIM = alumnimum component
  sediment <-  data.frame(par_name = c("fDTotS0","fDOrgS0","fDOrgSoil","fLutum","fFeDIM","fAlDIM"),
                          clay     = c(0.3, 0.08, 0.08, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          peat     = c(0.1, 0.25, 0.25, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          sand     = c(0.5, 0.08, 0.08, 0.03, 0.1 * 0.03, 0.1 * 0.03),
                          clay_peat = c(0.2, 0.165, 0.165, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          clay_sand = c(0.4, 0.08, 0.08, 0.215, 0.1 * 0.215, 0.1 * 0.215)) 
  
  dfPARAMS[, paramset + 4][match(sediment$par_name, dfPARAMS$temp_id)] <- sediment[, sediment_type]
  dfPARAMS$temp_id <- NULL
  
  return(dfPARAMS)
}

## Adjust the sediment parameter settings to defaults for clay, peat, sand, clay_peat, clay_sand. Requires a column_name rather than a paramset. 
adjustSedimentParamSettings_PCLakeSplus <- function(dfPARAMS, column_name, sediment_type = c("clay", "peat", "sand", "clay_peat", "clay_sand")){
  
  ## debug
  # dfPARAMS <- lDATM_SETTINGS$params
  # column_name <- "sDefault0"
  # sediment_type <- "peat" 
  
  ## error catching
  if(length(sediment_type) > 1 | any(!(sediment_type %in% c("clay", "peat", "sand", "clay_peat", "clay_sand"))) == TRUE){stop("This option is not available. Please choose one of the following options: clay, peat, sand, clay_peat, clay_sand.")}
  
  dfPARAMS$temp_id <- row.names(dfPARAMS)
  
  ## fFeDim & fAlDIM = 0.1 * fLutum
  ## fDTotS0 = dry matter
  ## fDOrgSo = organic matter
  ## fDOrgSoil = organic matter component in the banks (connected to erosion)
  ## fLutum = lutum
  ## fFeDIM = iron component
  ## fAlDIM = alumnimum component
  sediment <-  data.frame(par_name = c("fDTotS0","fDOrgS0","fLutum","fFeDIM","fAlDIM"),
                          clay     = c(0.3, 0.08, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          peat     = c(0.1, 0.25, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          sand     = c(0.5, 0.08, 0.03, 0.1 * 0.03, 0.1 * 0.03),
                          clay_peat = c(0.2, 0.165, 0.4, 0.1 * 0.4, 0.1 * 0.4),
                          clay_sand = c(0.4, 0.08, 0.215, 0.1 * 0.215, 0.1 * 0.215)) 
  
  dfPARAMS[match(sediment$par_name, dfPARAMS$temp_id),]
  dfPARAMS[, column_name][match(sediment$par_name, dfPARAMS$temp_id)] <- sediment[, sediment_type]
  dfPARAMS$temp_id <- NULL
  
  return(dfPARAMS[, column_name])
}

## Read data from cpp file and adjust the values of variables
##   underscores in variable names are removed
setValues <- function(codelines, pars){
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

## OLD version of setValues: setParameters 
#setParameters <- function(codelines, pars){
#  if (length(pars) > 0){
#    for (i in 1:length(pars)){
#      # i <- 1
#      # codelines <- tmp
#      # tmp[id]
#      id <- grep(x = codelines, pattern = paste("_", names(pars)[i], "_", sep = ""), ignore.case = TRUE)
#      items <- unlist(strsplit(codelines[id], "="))
#      codelines[id] <- paste(items[1], "= ", as.numeric(pars[i]), ";", sep = "")
#      
#      if (length(id)==0) print(paste("Warning: defined parameter/initial conditions to be changed '", names(pars)[i], "' does not exist", sep = ""))
#    }
#  }
#  return(codelines)
#}


## PCLakeS+ specific: make the equations for the upstream transport totals (e.g. wSiTranWHypUpstTotN1)
## These equations are used in the (external component of) the mass balance error 

## pt 1. 
## reg expression to make the equations for the upstream transport totals
makeUpstTotFormula <- function(x){gsub("(^w[[:alnum:]]{1,2})(Tran[[:alnum:]]+)(Hyp|Epi)(Upst)(N[[:digit:]]+)($|.+)",
                                       "\\1TranW\\3UpstTot\\5 = \\1\\2\\3\\4\\5\\6", x)}

## pt 2. 
## apply makeUpstTotFormula to the data
makeTranUpstTotFormulas <- function(dfSUBSTANCES, sel_node){
  
  ## debug
  # dfSUBSTANCES <- tran_subst #dfSUBST
  # sel_node <- 11
  
  dfUpstNames <- data.frame(tran_subst = paste0(dfSUBSTANCES[-grep("Zoo", dfSUBSTANCES$tran_subst), "tran_subst"], "Upst", "N", sel_node)) ## remove zooplankton fraction, as the transport fraction of zooplankton is already accounted for separately in the mass balances.
  dfUpstNames$type <- gsub("(w)([[:alnum:]]{1,2})(Tran.+)", "\\2", dfUpstNames$tran_subst)
  dfUpstNames$layer <- gsub("(w[[:alnum:]]{1,2}Tran.+)(Epi|Hyp)(.+)", "\\2", dfUpstNames$tran_subst) 
  dfUpstNames_split <- split(dfUpstNames, list(dfUpstNames$type, dfUpstNames$layer))
  dfUpstNames_split_clean <- lapply(dfUpstNames_split, function(x) y <- x$tran_subst)
  dfUpstNames_prep_formula <- lapply(dfUpstNames_split_clean, paste0, collapse = " + ")
  ## add the transport part of Si in Diats separately, because there is no state voor Si in Diats
  dfUpstNames_prep_formula$Si.Epi <- paste0(dfUpstNames_prep_formula$Si.Epi, " + (cSiDDiatN", sel_node, " * wDTranDiatEpiUpstN", sel_node, ")")
  dfUpstNames_prep_formula$Si.Hyp <- paste0(dfUpstNames_prep_formula$Si.Hyp, " + (cSiDDiatN", sel_node, " * wDTranDiatHypUpstN", sel_node, ")")
  dfUpstNames_2_formula <- lapply(dfUpstNames_prep_formula, makeUpstTotFormula)
  vecUpstNames <- as.vector(do.call(rbind, dfUpstNames_2_formula))
}


## PCLakeS+ specific: update the list of auxiliaries
makeAuxNames_PCLakeSplus <- function(dfAUXILARIES, dfSUBSTANCES, dfNETWORK, vNODES){
  
  ## debug
  #   dfAUXILARIES <- lDATM_SETTINGS$auxils
  #   dfSUBSTANCES <- tran_subst
  #   dfNETWORK <- pointer_df
  #   vNODES <- node_info$node_nr
  
  aux_names 	<- paste0(rownames(dfAUXILARIES[which(dfAUXILARIES$iReport == 1),, drop = F]),
                       "N", 
                       sort(rep(vNODES, length(rownames(dfAUXILARIES[which(dfAUXILARIES[, 1] == 1), , drop = F])))))
  aux_names <- c(aux_names, paste0(rep(dfSUBSTANCES$tran_subst, length(vNODES)), "UpstN", rep(vNODES,  each = length(dfSUBSTANCES$tran_subst))))
  
  ## remove the section of the added tran_subst names if their node only exists in the column "node_nr_from". 
  ## Because this means that they do not receive an upstream transport fraction, and therefore this aux does not exist, and should not be made. 
  if(length(grep("UpstN", aux_names))>0){
    if(length(unique(setdiff(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to)))>0){
      aux_to_remove <- grep(paste0(paste0("UpstN", unique(setdiff(dfNETWORK$node_nr_from, dfNETWORK$node_nr_to))), collapse = "|"), aux_names)
      aux_names <- aux_names[-aux_to_remove]
    }
  }
  
  ## add the totals
  aux_upstTotNames <- gsub("(^w[[:alnum:]]{1,2})(TranW)(Hyp|Epi)(UpstTot)(N[[:digit:]]+)($|.+)",
                           "\\1\\2\\3\\4\\5", 
                           unlist(lapply(unique(dfNETWORK$node_nr_to), function(x) makeTranUpstTotFormulas(dfSUBSTANCES = dfSUBSTANCES, sel_node = x))))
  aux_names <- c(aux_names, aux_upstTotNames)
  # tail(aux_names)
  
  return(aux_names)
}

## Run the model
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
  
  dyn.load(file.path(dirMODEL, "model.dll"))
  
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
               rtol = 1e-10,
               atol = 1e-12,
               outnames = aux_names, 
               hmax = 0.5, ## maximum time step size. This should not reach 1, due to the vegetation equations part of PCLake
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
                hmax = 0.5, # was 0.5
                rtol = 1e-10,
                atol = 1e-12,
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
                hmax = 0.5, # was 0.5
                rtol = 1e-12,
                atol = 1e-14,
                method = integrator, 
                hini = internal_time_step)
  }
  
  # store output (only for selected state variables and auxiliaries)
  outC <- outC[, c(1, which(colnames(outC) %in% c(state_names, aux_names)))]
  
  dyn.unload(file.path(dirMODEL, "model.dll"))
  # return(assign("outC", outC, envir = .GlobalEnv))
  return(outC)
}