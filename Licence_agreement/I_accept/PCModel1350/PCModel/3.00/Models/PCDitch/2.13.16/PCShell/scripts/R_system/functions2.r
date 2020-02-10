#==================================================================================================
# SECTION GENERAL
#
#  DESCRIPTION:
#    This file contains general functions
#
#==================================================================================================

CompileModel <- function(dir_) { # compile model code (cpp) and produce dll
  setwd(paste(dir_SCHIL,"scripts/cpp2R/",sep=""))
  #system("compile_model_cpp.cmd",show.output.on.console = FALSE,invisible = FALSE)
  file.remove("model.o")
  file.remove("model.dll")
  #system("R CMD SHLIB model.cpp") 
  system("R --arch x64 CMD SHLIB model.cpp") 
}

InitializeModel <- function(number_of_states, vSTATES_LIST) {
  for (file_cpp in list.files(Dir_source_adjusted,pattern=".cpp")) {
    if (grepl("sp",file_cpp))  ConvertFileToVector(paste(Dir_source_adjusted,file_cpp,sep=""),"parms")      # parameter values
    #if (grepl("sc",file_cpp))  ConvertFileToVector(paste(Dir_source_adjusted,file_cpp,sep=""),"initStates") # initial values of state variables 
	if (grepl("rs",file_cpp))  tmp <- gsub("double &","",read.table(paste(Dir_source_adjusted,file_cpp,sep=""), header=F, stringsAsFactors = F, sep="=")$V1) #get names of state variables (in right order)
  }
  initStates=vSTATES_LIST
  dyn.load("model.dll")
  ini <- function(y, nr_of_states){
	 .C("InitializeModel",  initState=y, state=double(nr_of_states))
  }
  inits <- ini(initStates,number_of_states)
  # make vector with initial values of state variables
  states <- inits$state #get initial values of state variables
  names(states) <- gsub(" ","",tmp) # combine name and value
  dyn.unload("model.dll")
  return(assign("states", states, envir=.GlobalEnv))
}

InitializeModel_MultiThread <- function(number_of_states) {
  for (file_cpp in list.files(Dir_source_adjusted,pattern=".cpp")) {
    if (grepl("sp",file_cpp))  ConvertFileToVector(paste(Dir_source_adjusted,file_cpp,sep=""),"parms")      # parameter values
    if (grepl("sc",file_cpp))  ConvertFileToVector(paste(Dir_source_adjusted,file_cpp,sep=""),"initStates") # initial values of state variables 
    if (grepl("rs",file_cpp))  tmp <- gsub("double &","",read.table(paste(Dir_source_adjusted,file_cpp,sep=""), header=F, stringsAsFactors = F, sep="=")$V1) #get names of state variables (in right order)
  }
  dyn.load("model.dll")
  ini <- function(y, nr_of_states){
	 .C("InitializeModel",  initState=y, state=double(nr_of_states))
  }
  inits <- ini(initStates,number_of_states)
  # make vector with initial values of state variables
  new_states <- inits$state #get initial values of state variables
  names(new_states) <- gsub(" ","",tmp) # combine name and value
  dyn.unload("model.dll")
  return(new_states)
}

RunModel <- function(states,times_output,parms,forcings,aux_number,aux_names,integrator,state_names,internal_time_step,dirMODEL) {
  dyn.load(paste(dirMODEL,"model.dll",sep=''))
  
  # solve differential equations 	
  if (integrator %in% c("rk2","rk23","rk23bs","rk34f","rk45f","rk45ck","rk45e","rk45dp6","rk45dp7","rk78dp","rk78f")) {
     outC <- rk(y=states, times=times_output, func = "CalculateDerivatives", parms = parms, dllname = "model",initforc = "forcc",forcings=forcings,initfunc = "initmod", nout = aux_number, outnames = aux_names, method = integrator)
  } else if (integrator %in% c("lsoda","lsode","lsodes","lsodar","vode","daspk","ode23","ode45","radau","bdf","bdf_d","adams","impAdams","impAdams_d","iteration")) {    
     outC <- ode(y=states, times=times_output, func = "CalculateDerivatives", parms = parms, dllname = "model",initforc = "forcc",forcings=forcings,initfunc = "initmod", nout = aux_number, outnames = aux_names, method = integrator)
  } else if (integrator %in% c("euler","rk4")) { # fixed time step   
     outC <- ode(y=states, times=times_output, func = "CalculateDerivatives", parms = parms, dllname = "model",initforc = "forcc",forcings=forcings,initfunc = "initmod", nout = aux_number, outnames = aux_names, method = integrator, hini=internal_time_step)
  }
  
  # store output (only for selected state variables and auxiliaries)
  outC <- outC[,c(1,which(colnames(outC) %in% c(state_names,aux_names)))]

  dyn.unload(paste(dirMODEL,"model.dll",sep=''))
  return(assign("outC", outC, envir=.GlobalEnv))
}

Store_results <- function(outC,out_total,variant) {
  outC <- cbind(outC,"variant"=variant)
  if (nrow(out_total) > 500000) {  # if matrix becomes too big --> store results, to prevent problems with limited computer memory
	write.table(x=out_total, file=paste(Dir_output,"time_series_state_variables_",(variant-1),".csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  		# write all output 
	out_total <- outC
  } else {
    if(nrow(out_total)==1) {
	  out_total <- outC
    } else {   
	  out_total <- rbind(out_total,outC)
	}
  }
  return(out_total)
}  

LoadPackage <- function(package) {
if (require(package,character.only=T)) {
 # library loaded
 cat(paste("\nPackage '",package,"' succesfully loaded.\n",sep=""))
} else {
 # library not loaded
 # Try to install?
 install.packages(package)
 if (require(package,character.only=T)) {
  # library loaded
 } else {
  # library not loaded
  # Can't install package 
  cat(paste("\nCan't install package '",package,"'.\n",sep=""))
 }
}
}

addForcing <- function(read, x) { 
# Deze functie controleert of de variable x als tijdreeks (forcing function) wordt opgegeven.
# read is the value to test (e.g. ReadTemp)
# x is the variable name (e.g mTemp)
 if (get(read)) {
  par_forcing <<- c(par_forcing, paste("_", read, "_", sep=""))
  names_forcing <<- c(names_forcing, x)
 }
}

GetForcing <- function(imposed_forcings) {
  # function which determines 
  # 1. the active forcings (forcing_parameter = 1) (stored in par_forcing)
  # 2. the corresponding forcing variables to be read (stored in names_forcing)  
  par_forcing      <<- names(imposed_forcings)[which(imposed_forcings==1)]
  ReadNutFrac      <- as.numeric(imposed_forcings["ReadNutFrac"])
  forcings_par_aux <- c(ReadTemp="mTemp",ReadLOut="mLOut",ReadVWind="mVWind",ReadQIn="mQIn",ReadQOut="mQOut",ReadQEv="mQEv",ReadPLoadPhyt="mPLoadPhytTot",ReadDLoadDet="mDLoadDet",ReadDLoadIM="mDLoadIM")  
  names_forcing    <- character()
  for (name in par_forcing) {
    if (name %in% names(forcings_par_aux)) {
       names_forcing <- c(names_forcing,as.character(forcings_par_aux[which(names(forcings_par_aux)==name)]))     
    }
    if (name == "ReadPLoad") {
      if (ReadNutFrac == 0) {
        names_forcing <- c(names_forcing,"mPLoad")
      } else if (ReadNutFrac == 1) {
        names_forcing <- c(names_forcing,"mPLoadPO4","mPLoadOrg")
      }
    }
    if (name == "ReadNLoad") {
      if (ReadNutFrac == 0) {
        names_forcing <- c(names_forcing,"mNLoad")
      } else if (ReadNutFrac == 1) {
        names_forcing <- c(names_forcing,"mNLoadNH4","mNLoadNO3","mNLoadOrg")
      }
    }
  }
  return(names_forcing)
}

GetForcing_allbounds <- function(imposed_forcings) { # same function as GetForcing but now for version of PCDitch in which for all the water states a forcing can be applied
  # function which determines 
  # 1. the active forcings (forcing_parameter = 1) (stored in par_forcing)
  # 2. the corresponding forcing variables to be read (stored in names_forcing)  
  par_forcing      <<- names(imposed_forcings)[which(imposed_forcings==1)]
  ReadNutFrac      <- as.numeric(imposed_forcings["ReadNutFrac"])
  forcings_par_aux <- c(ReadPLoadPO4="mPLoadPO4",ReadPLoadPhyt="mPLoadPhytTot",ReadPLoadDet="mPLoadDet",ReadPLoadAIM="mPLoadAIM",ReadNLoadNH4="mNLoadNH4",ReadNLoadNO3="mNLoadNO3",ReadNLoadPhyt="mNLoadPhytTot",ReadNLoadDet="mNLoadDet",
                       ReadDLoadDet="mDLoadDet",ReadDLoadPhyt="mDLoadPhytTot",ReadDLoadIM="mDLoadIM",ReadO2Load="mO2Load",ReadQIn="mQIn",ReadQOut="mQOut",ReadQEv="mQEv")
  names_forcing    <- character()
  for (name in par_forcing) {
    if (name %in% names(forcings_par_aux)) {
       names_forcing <- c(names_forcing,as.character(forcings_par_aux[which(names(forcings_par_aux)==name)]))     
    }
  }
  return(names_forcing)
}

SetParameters <- function(codelines,pars) {
  if (length(pars)>0) {
	 for (i in 1:length(pars)) {
        id <- grep(x=codelines,pattern=paste("_",names(pars)[i],"_",sep=""),ignore.case = TRUE)
	    items <- unlist(strsplit(codelines[id],"="))
	    codelines[id] <- paste(items[1],"= ",as.numeric(pars[i]),";",sep="")
        if (length(id)==0) WriteLogFile(LogFile,ln=paste("Warning: defined parameter/initial conditions to be changed '",names(pars)[i],"' does not exist",sep=""))
     }
  }
  return(codelines)
}  

CheckSheetNames <- function(dir_SCEN,sheet_names) {
  # function that checks if a sheet existst in Input_PCShell.xls
  available_sheets      <- unlist(strsplit(list.files(paste(dir_SCEN,"/txt",sep="")),".txt"))
  for (sheet in sheet_names) {
    if (!(sheet %in% available_sheets)) WriteLogFileError(LogFile,ln=paste("There is no sheet called '",sheet,"' in Input_PCShell.xls, while expected",sep="")) 
  }
}

SetSedimentType <- function(sediment_type) {
  # function to define sediment types in terms of parameters
  # available sediment types: 1="clay",2="peat",3="sand"
  soil_param <- vector()
  if (sediment_type!=0) {
    if (sediment_type==1) { 
         FDTOTS0   <- 0.3
         FDORGS0   <- 0.08
         FDORGSOIL <- 0.08
         FLUTUM    <- 0.4
    } else if (sediment_type==2) {
         FDTOTS0   <- 0.10        
         FDORGS0   <- 0.25         
         FDORGSOIL <- 0.25      
         FLUTUM    <- 0.4          
    } else if (sediment_type==3) {
         FDTOTS0   <- 0.5
         FDORGS0   <- 0.08
         FDORGSOIL <- 0.08
         FLUTUM    <- 0.03
    } else {
      WriteLogFileError(LogFile,ln="sediment_type is defined wrong (it should be 0, 1, 2 or 3)")
    }   
    FFEDIM            <- 0.1 * FLUTUM  #[gFe/gD]
    FALDIM            <- 0.1 * FLUTUM  #[gAl/gD]
    soil_param        <- c(FDTOTS0,FDORGS0,FDORGSOIL,FLUTUM,FFEDIM,FALDIM)
    names(soil_param) <- c("fDTotS0","fDOrgS0","fDOrgSoil","fLutum","fFeDIM","fAlDIM")
  }
  return(soil_param)
}


GetForcing_allbounds_PCLake <- function(imposed_forcings) { # same function as GetForcing but now for version of PCDitch in which for all the water states a forcing can be applied
  # function which determines 
  # 1. the active forcings (forcing_parameter = 1) (stored in par_forcing)
  # 2. the corresponding forcing variables to be read (stored in names_forcing)  
  par_forcing      <<- names(imposed_forcings)[which(imposed_forcings==1)]
  forcings_par_aux <- 
  c( ReadTemp="mTemp",
     ReadLOut="mLOut",
     ReadVWind="mVWind",
     ReadQIn="mQIn",
     ReadQOut="mQOut",
     ReadQEv="mQEv",
     ReadPLoadPO4="mPLoadPO4",
     ReadNLoadNH4="mNLoadNH4",
     ReadNLoadNO3="mNLoadNO3",
     ReadDLoadBlue="mDLoadBlue",
     ReadPLoadBlue="mPLoadBlue",
     ReadNLoadBlue="mNLoadBlue",
     ReadDLoadDiat="mDLoadDiat",
     ReadPLoadDiat="mPLoadDiat",
     ReadNLoadDiat="mNLoadDiat",
     ReadDLoadGren="mDLoadGren",
     ReadPLoadGren="mPLoadGren",
     ReadNLoadGren="mNLoadGren",
     ReadDLoadDet="mDLoadDet",
     ReadPLoadDet="mPLoadDet",
     ReadNLoadDet="mNLoadDet",
     ReadSiLoadDet="mSiLoadDet",
     ReadDLoadIM="mDLoadIM",
     ReadPLoadAIM="mPLoadAIM",
     ReadSiO2Load="mSiO2Load",
     ReadO2Load="mO2Load"           ) 
  names_forcing    <- character()
  for (name in par_forcing) {
    if (name %in% names(forcings_par_aux)) {
       names_forcing <- c(names_forcing,as.character(forcings_par_aux[which(names(forcings_par_aux)==name)]))     
    }
  }
  return(names_forcing)
}

ConvertFileToVector       <- function(nmFile,nmVector){
#     #---------------------------------------------------------------------------------------------
#     # FUNCTION ConvertFileToVector
#     #
#     #  SYNOPSIS:
#     #    - nmFile    : name and location of file
#     #    - nmVector  : name of the created vector
#     #  DESCRIPTION:
#     #    converts file content to vector
#     #
#     #---------------------------------------------------------------------------------------------
 tmp <- read.table(nmFile, header=F, stringsAsFactors = F, sep="=")
 tmp$V2 <- as.numeric(gsub(";", "", tmp$V2)) 
 tmp2 <- tmp$V2
 tmp$V1 <- gsub(" ","",tmp$V1)
 names(tmp2) <- tmp$V1
 return(assign(nmVector, tmp2, envir=.GlobalEnv))
}

OpenLogFil           <- function(LogFile){
     #---------------------------------------------------------------------------------------------
     #  SYNOPSIS:
     #    - LogFile       : name and location of logfile
     #
     #  DESCRIPTION:
     #    routine for opening xml logfile
     #
     #---------------------------------------------------------------------------------------------
     write("######################################",LogFile,append=FALSE)
     write("PCShell (cpp) version 1.0",LogFile,append=TRUE)
	 write("Author: Luuk van Gerven (NIOO)",LogFile,append=TRUE)
     write("######################################",LogFile,append=TRUE)
     write("",LogFile,append=TRUE)
	 write(paste("start time: ",Sys.time(),sep=""),LogFile,append=TRUE)
     return(LogFile)
}
WriteLogFile      <- function(LogFile,ln){
     #---------------------------------------------------------------------------------------------
     #  SYNOPSIS:
     #    - LogFile     : name and location of logfile
	 #    - ln          : information te be written to file
     #
     #  DESCRIPTION:
     #    routine for writing information to logfile
     #
     #---------------------------------------------------------------------------------------------
     write(ln,LogFile,append=TRUE)
}
WriteLogFileError      <- function(LogFile,ln){
     #---------------------------------------------------------------------------------------------
     #  SYNOPSIS:
     #    - LogFile     : name and location of logfile
	 #    - ln          : information te be written to file
     #
     #  DESCRIPTION:
     #    routine for writing error information to logfile
     #
     #---------------------------------------------------------------------------------------------
     write(paste("ERROR: ",ln,sep=""),LogFile,append=TRUE)
	 print(paste("ERROR: ",ln,sep=""))
	 stop("See ERROR(S) above")
}

RmvDir               <- function(path_dir,name_dir){
     #---------------------------------------------------------------------------------------------
     # FUNCTION RmvDir
     #
     #  SYNOPSIS:
     #    - path_dir    : path of directory where directory 'name_dir' is located
	 #    - name_dir    : name of directory
     #
     #  DESCRIPTION:
     #    Deletes directory 'name_dir'
     #
     #  COMMENT:
     #    Works for Windows XP and NT
     #
     #---------------------------------------------------------------------------------------------
      setwd(path_dir)
	  if (file.is.dir(paste("./",name_dir,sep="")) == TRUE) {
          ClrDir(paste("./",name_dir,sep=""))
          setwd(path_dir)
          system(paste("cmd /c rd ",name_dir, sep=""))
      }
}
ClrDir               <- function(path_dir){
     #---------------------------------------------------------------------------------------------
     # FUNCTION ClrDir
     #
     #  SYNOPSIS:
     #    - path_dir       : path name of directory (including directory name)
     #
     #  DESCRIPTION:
     #    Deletes all files in a directory
     #
     #---------------------------------------------------------------------------------------------
     if (file.is.dir(path_dir) == TRUE) {
	    setwd(path_dir)
	 
        Files = list.files()
        if (length(Files) != 0) {
            for (i in 1:length(Files)) {
                rmvFil <- Files[i]
                if (file.is.dir(paste("./",rmvFil,sep="")) != TRUE) {file.remove(paste("./",rmvFil,sep=""))}
            }
        }
        Files = list.files()
        if (length(Files) != 0) {
            for (i in 1:length(Files)) {
                rmvFil <- Files[i]
                if (file.is.dir(paste("./",rmvFil,sep="")) == TRUE) {RmvDir(path_dir,rmvFil)}
            }
        }
     }
}  
file.is.dir          <- function(path_file){
     #---------------------------------------------------------------------------------------------
     # FUNCTION file.is.dir
     #
     #  SYNOPSIS:
     #    - path_file       : path name of file (including file name itself)
     #
     #  DESCRIPTION:
     #    Check if file is directory
     #
     #---------------------------------------------------------------------------------------------
     isdir <- file.info(path_file)$isdir
     isdir && !is.na(isdir)
}

CheckFileExistError       <- function(nmFile,LogFile){
     #---------------------------------------------------------------------------------------------
     # FUNCTION CheckFileExist
     #
     #  SYNOPSIS:
     #    - nmFile    : name and location of file
     #    - LogFile   : name of LogFile
     #  DESCRIPTION:
     #    check if file exists
     #
     #---------------------------------------------------------------------------------------------
     if (file.exists(nmFile) == FALSE) {
         WriteLogFile(LogFile,ln=paste("ERROR: file does not exist: ",nmFile,sep=""))
         print(paste("ERROR: file does not exists (",nmFile,")",sep=""))
         stop("See ERROR(S) above")
     }
}

delSpaces            <- function(ln){
     #---------------------------------------------------------------------------------------------
     # FUNCTION delSpaces
     #
     #  SYNOPSIS:
     #    - ln        : characterstring
     #
     #  DESCRIPTION:
     #    delete double spaces and last spaces in characterstring
     #
     #---------------------------------------------------------------------------------------------
     ln <- gsub(pattern="\t",replacement=" ",x=ln)
     while(substr(x=ln,start=nchar(ln),stop=nchar(ln)) == " ") ln <- substr(x=ln,start=1,stop=(nchar(ln)-1))
     while(length(grep(pattern ="  ",x=ln)) != 0) ln <- gsub(pattern="  ",replacement = " ", x=ln)
     if (substr(x=ln,start=1,stop=1) == " ") {ln <- substr(x=ln,start=2,stop=nchar(ln))}
     return(ln)
}
