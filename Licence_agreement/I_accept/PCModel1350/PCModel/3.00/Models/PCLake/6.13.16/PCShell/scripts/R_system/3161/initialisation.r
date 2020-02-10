# ---------------------------------------------------
# Initialisation
# ---------------------------------------------------

#set java memory settings prior to loading java enabled packaged (e.g. xlsx)
options( java.parameters = "-Xmx4g")

# load needed libraries
LoadPackage("ggplot2")
LoadPackage("deSolve")
LoadPackage("reshape2") 
#LoadPackage("xlsx") 
LoadPackage("XLConnect") 

# define path of working directory
dir_SCEN        <- paste(dir_SCHIL,work_case,"/",sep="")
path_DATM		=	paste(dir_DATM, "Models/PCLake/6.13.16/",file_DATM, sep='')

# create logfile
start_time      <- Sys.time()
LogFile         <- paste(dir_SCEN,"logfile.txt",sep="")
OpenLogFil(LogFile)
WriteLogFile(LogFile,ln="")
WriteLogFile(LogFile,ln="initializing......")

# create directory for model output (which content is cleared, if any)
Dir_output      <- paste(dir_SCEN,"output/",sep="")
dir.create(Dir_output,showWarnings=FALSE)
ClrDir(Dir_output)

# create directory for model code to be compiled 
Dir_source_adjusted <- paste(dir_SCHIL,"/scripts/source_cpp_adjusted/",sep="")
dir.create(Dir_source_adjusted,showWarnings=FALSE)
ClrDir(Dir_source_adjusted)

# ---------------------------------------------------
# 	Read input from DATM implementation in excel directly
# ---------------------------------------------------
WriteLogFile(LogFile,ln=paste("Reading DATM excel input from file ",file_DATM,sep=''))

#read in the workbook
wbDATM=loadWorkbook(file=path_DATM)

#---Read control values for the model
dfRUNSETTINGS_RAW	=	readWorksheet(wbDATM, sheet="Control", startRow=1, endRow=90 ,startCol=1,endCol=5)
#read.xlsx2(file=path_DATM, sheetName="Control", startRow=1, endRow=90 ,colIndex=1:5, as.data.frame=TRUE, header=TRUE)
dfRUNSETTINGS1		=	dfRUNSETTINGS_RAW[which(dfRUNSETTINGS_RAW[,2]!=""),]
dfRUNSETTINGS		=	dfRUNSETTINGS1[-1,-1]
rownames(dfRUNSETTINGS)	=	as.character(unlist(dfRUNSETTINGS1[-1,1]))
colnames(dfRUNSETTINGS)	=	as.character(unlist(dfRUNSETTINGS1[1,-1]))
for(nCOL in 1:ncol(dfRUNSETTINGS)){ dfRUNSETTINGS[,nCOL]	=	as.numeric(as.character(unlist(dfRUNSETTINGS[,nCOL]))) }

#---Read state values for the model
dfSTATES_RAW		=	readWorksheet(wbDATM, sheet="states", startRow=1, endRow=200 ,startCol=1,endCol=90)
#read.xlsx2(file=path_DATM, sheetName="states", startRow=1, endRow=200, colIndex=1:90, as.data.frame=TRUE, header=TRUE)
dfSTATES			=	dfSTATES_RAW[which(dfSTATES_RAW[,2]!=""),which(colnames(dfSTATES_RAW) %in% 
										c("sStateName","iReportState","sDefaultSetTurbid0","sDefaultSetClear1","sAltenativeSet2","sAlternativeSet3"))]
rownames(dfSTATES)	=	as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sStateName")]))
dfSTATES			=	dfSTATES[,-which(colnames(dfSTATES)=="sStateName")]
rownames(dfSTATES)	=	gsub("_","",rownames(dfSTATES))
for(nCOL in 1:ncol(dfSTATES)){ dfSTATES[,nCOL]	=	as.numeric(as.character(unlist(dfSTATES[,nCOL]))) }


#---Read parameter values for the model
dfPARAMS_RAW		=	readWorksheet(wbDATM, sheet="parameters", startRow=1, endRow=1200 ,startCol=1,endCol=90)
#read.xlsx2(file=path_DATM, sheetName="parameters", startRow=1, endRow=1200, colIndex=1:90, as.data.frame=TRUE, header=TRUE)
dfPARAMS			=	dfPARAMS_RAW[which(dfPARAMS_RAW[,2]!=""),which(colnames(dfPARAMS_RAW) %in% 
										c("sName","iReport","sDefault0","sSet1","sSet2","sSet3"))]
rownames(dfPARAMS)	=	as.character(unlist(dfPARAMS[which(colnames(dfPARAMS)=="sName")]))
dfPARAMS			=	dfPARAMS[,-which(colnames(dfPARAMS)=="sName")]
rownames(dfPARAMS)	=	gsub("_","",rownames(dfPARAMS))
for(nCOL in 1:ncol(dfPARAMS)){ dfPARAMS[,nCOL]	=	as.numeric(as.character(unlist(dfPARAMS[,nCOL]))) }


#---Read auxilliaries to report from the DATM file 
dfAUXIL_RAW		=	readWorksheet(wbDATM, sheet="derivatives", startRow=1, endRow=2800 ,startCol=1,endCol=15)
#read.xlsx2(file=path_DATM, sheetName="derivatives", startRow=1, endRow=2000, colIndex=c(1,5), as.data.frame=TRUE, header=TRUE)
dfAUXIL			=	dfAUXIL_RAW[which(dfAUXIL_RAW[,2]!=""),which(colnames(dfAUXIL_RAW) %in% 
										c("sName","iReport"))]
rownames(dfAUXIL)	=	as.character(unlist(dfAUXIL[which(colnames(dfAUXIL)=="sName")]))
dfAUXIL			=	dfAUXIL[,-which(colnames(dfAUXIL)=="sName"),drop=FALSE]
rownames(dfAUXIL)	=	gsub("_","",rownames(dfAUXIL))
for(nCOL in 1:ncol(dfAUXIL)){ dfAUXIL[,nCOL]	=	as.numeric(as.character(unlist(dfAUXIL[,nCOL]))) }


# ---------------------------------------------------

# create extra directories related to the interface
if (InclInterface) {
  Dir_input <- paste(dir_SCEN,"input_manipulation/",sep="")
  dir.create(Dir_input,showWarnings=FALSE)
  setwd(Dir_input)
  Dir_ref <- paste(Dir_input,"reference_settings/",sep="")
  dir.create(Dir_ref,showWarnings=FALSE)
  ClrDir(Dir_ref)                                           # if directory already exists -> content is cleared
  Dir_changes <- paste(Dir_input,"changes/",sep="")
  dir.create(Dir_changes,showWarnings=FALSE)
  ClrDir(Dir_changes)                                       # if directory already exists -> content is cleared
}

# ********************************************************
# --------------------------------------------------------
# Define run settings or read run setting from interface:
# 1. run time, integrator, output time step 
# 2. initial state to be changed
# 3. parameters to be changed
# 4. forcing functions to be imposed on the model 
# 5. define output variables (states and auxilaries) 
# --------------------------------------------------------
# ********************************************************
WriteLogFile(LogFile,ln="- reading run settings")
# -----------------------------------------------------------
# 1. run time, integrator, output time step 
# -----------------------------------------------------------
runtime_years      <- 30     # model run time (in years)
output_time_step   <- 1      # time step at which output is generated (in days)
integrator         <- 8     # integrator to solve the model equations (ordinary first order differential equations)
internal_time_step <- 0.001   # timestep at which derivatives are calculated (if integrator uses fixed time step)
                             # available intergrators:
                             # 1 = Euler (fixed time step)
                             # 2 = Runge Kutta 2nd order (Heun) (fixed time step)
                             # 3 = Runge Kutta 4th order (fixed time step)
                             # 4 = Runge Kutta pair of order 3(2) (variable time step)
                             # 5 = Runge Kutta pair of order 3(2), according to Bogacki & Shampine (1989) (variable time step)
                             # 6 = Runge Kutta pair of order 4(3), according to Fehlberg (1967) (variable time step)
                             # 7 = Runge Kutta pair of order 5(4), according to Fehlberg (1967) (variable time step)
                             # 8 = Runge Kutta pair of order 5(4), according to Cash & Karp (1990) (variable time step)
                             # 9 = Runge Kutta pair of order 5(4), according to ..... (variable time step)
                             # 10= Runge Kutta pair of order 6(5(4)), according to Dormand & Prince (1980) (variable time step)
                             # 11= Runge Kutta pair of order 7(5(4)), according to Dormand & Prince (1980) (variable time step)
                             # 12= Runge Kutta pair of order 8(7), according to Dormand & Prince (1980) (variable time step)
                             # 13= Runge Kutta pair of order 8(7), according to Fehlberg (1967) (variable time step)
                             # 14= "lsoda"
                             # 15= "lsode"
                             # 16= "lsodes"
                             # 17= "lsodar"
                             # 18= "vode" (RECOMMENDED!!!)
                             # 19= "daspk"
                             # 20= "ode23"
                             # 21= "ode45"
                             # 22= "radau"
                             # 23= "bdf"
                             # 24= "bdf_d"
                             # 25= "adams"
                             # 26= "impAdams"
                             # 27= "impAdams_d"
                             # 28= "iteration"


# -----------------------------------------------------------
# 2. initial state to be changed
# -----------------------------------------------------------

inits_to_change <- c(
#  sDepthW = 2.0,
#  sPO4W = 0.00001
)
  
# -----------------------------------------------------------
# 3. parameters to be changed
# -----------------------------------------------------------

# define the sediment type
sediment_type  <- 0                              # available sediment types: 0=default settings (/source_cpp/*sp.cpp), 1=clay, 2=peat, 3=sand

# get parameter values that define the sediment_type
soil_param     <- SetSedimentType(sediment_type) 

# define parameters to change
pars_to_change <- c(
# 	InitCalc = 1 #strongly recommended if initial state is changed
)

# -----------------------------------------------------------
# 4. forcing functions to be imposed on the model 
# -----------------------------------------------------------

# define forcing functions to impose on the model
#---Setup forcing function data from DATM excel
vFORCINGS_READ	=	rowSums(dfPARAMS[grep(x=rownames(dfPARAMS), pattern="Read"),-1])
vFORCING_NAMES	=	GetForcing(imposed_forcings=vFORCINGS_READ)
if(length(vFORCING_NAMES)>0){
	dfFORCING_TIME	=	readWorksheet(wbDATM, sheet=vFORCING_NAMES[1], startRow=1, startCol=1,endCol=1)
	#read.xlsx2(file=path_DATM, sheetName=vFORCING_NAMES[1], startRow=1, colIndex=1, as.data.frame=TRUE, header=TRUE)
	dfFORCINGS		=	data.frame(matrix(NA,nrow(dfFORCING_TIME)-1,0))
	dfFORCINGS		=	cbind.data.frame(dfFORCINGS, day=as.numeric(as.character(unlist(dfFORCING_TIME[-nrow(dfFORCING_TIME),]))))
	for(sNAME in vFORCING_NAMES){
		dfFORCING		=	readWorksheet(wbDATM, sheet=sNAME, startRow=1, startCol=2,endCol=2)
		#read.xlsx2(file=path_DATM, sheetName=sNAME, startRow=1, colIndex=2, as.data.frame=TRUE, header=TRUE)
		dfFORCING		=	as.numeric(as.character(unlist(dfFORCING)))
		dfFORCINGS		=	cbind.data.frame(dfFORCINGS, dfFORCING)
	}
	colnames(dfFORCINGS)	=	c("day",vFORCING_NAMES)
	# read time series of forcings  
	data_forcing        <- melt(dfFORCINGS,id="day",measure=names(dfFORCINGS)[2:ncol(dfFORCINGS)])[,c(2,1,3)]
	names(data_forcing) <- c("forcing","time","value")
}else{
	dfFORCINGS		=		NA
	data_forcing	=		data.frame(matrix(NA,0,3))
	names(data_forcing) <- c("forcing","time","value")
}


# -----------------------------------------------------------
# 5. define output variables (states and auxilaries)
# -----------------------------------------------------------

#extract states to output
state_names =	rownames(dfSTATES[which(dfSTATES[,1]==1),])

aux_names 	=	rownames(dfPARAMS[which(dfPARAMS[,1]==1),])




















if (!(InclInterface)) {

   source(paste(dir_SCHIL,"scripts/R/define_run_settings.R",sep=""))    # source user-defined run settings

} else { # definition of run settings for the interface version

  # -------------------------------------------------------------------------------------
  # 1. run time, integrator, output time step, as well as forcing functions to be defined 
  # -------------------------------------------------------------------------------------

  # read user defined settings (control) from "Input_PCShell.xls"
  CheckSheetNames(dir_SCEN,"control")  #check if sheet 'control' exists
  # remove rows from control sheet with R path   
  controls         <- readLines(paste(dir_SCEN,"txt/control.txt",sep=""))
  if (any(grep("location_R",controls))) controls <- controls[-grep("location_R",controls)] 
  writeLines(controls,paste(dir_SCEN,"txt/control.txt",sep=""))
  # get run time, integrator, output time step and other run settings
  controls         <- read.table(paste(dir_SCEN,"txt/control.txt",sep=""), header=T)
  for (i in 1:nrow(controls)) {
     if (controls$type[i] != "forcing_function") assign(as.character(controls$name[i]),controls$value[i])
  }

  # -------------------------------------------------------------------------------------
  # 2. initial state to be changed
  # -------------------------------------------------------------------------------------

  CheckSheetNames(dir_SCEN,"state_variables")  #check if sheet 'state_variables' exists
  state_variables        <- read.table(paste(dir_SCEN,"txt/state_variables.txt",sep=""), header=T)
  inits_to_change        <- state_variables$base_run_initial_value[which(state_variables$change==T)]                 # get values of initial state values to change
  names(inits_to_change) <- gsub("_","",as.character(state_variables$name[which(state_variables$change==T)]))        # get names of initial state values to change and get rid of underscores

  # -------------------------------------------------------------------------------------
  # 3. parameters to be changed
  # -------------------------------------------------------------------------------------

  # get sediment parameters to be changes
  soil_param <- SetSedimentType(sediment_type) # get parameter values that define the sediment_type
  # get parameters to be changes
  CheckSheetNames(dir_SCEN,"parameters")  #check if sheet 'parameters' exists
  parameters_all        <- read.table(paste(dir_SCEN,"txt/parameters.txt",sep=""), header=T)
  pars_to_change        <- parameters_all$base_run_value[which(parameters_all$change_default==T)]                   # get values of parameters to change
  names(pars_to_change) <- gsub("_","",as.character(parameters_all$name[which(parameters_all$change_default==T)]))  # get names of parameters to change and get rid of underscores
  # get parameters to be changed in sensitivity analysis
  parameters 		    <- subset(parameters_all, subset=(change_analysis==T))
  parameters[,6]        <- as.numeric(as.character(parameters[,6]))               # change class factor to numeric
  parameters[,7]        <- as.numeric(as.character(parameters[,7]))               # change class factor to numeric

  # -------------------------------------------------------------------------------------
  # 4. forcing functions to be imposed on the model 
  # -------------------------------------------------------------------------------------

  # get forcing parameters from control sheet
  controls_tmp     <- subset(controls,subset=(type=="forcing_function")) 
  imposed_forcings <- controls_tmp$value  
  names(imposed_forcings) <- controls_tmp$name
  # get list of 'active forcings' (par_forcing) and corresponding variable names (names_forcing)
  names_forcing <- GetForcing(imposed_forcings)
  # read time series of forcings  
  CheckSheetNames(dir_SCEN,names_forcing)
  data_forcing <- data.frame()
  for (name in names_forcing) {    # read forcing functions
    temp          <- read.table(paste(dir_SCEN,"txt/",name,".txt",sep=""), header=T)
    data_forcing  <- rbind(data_forcing,data.frame(forcing=name,time=temp[,1],value=temp[,2]))
  }  
  write.table(x=data_forcing, file=paste(Dir_ref,"forcing_functions.csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  # write forcing functions to file
 
  # -------------------------------------------------------------------------------------
  # 5. define output variables (states and auxilaries)
  # -------------------------------------------------------------------------------------

  # get output state variables and their units
  state_names              <- gsub("_","",as.character(state_variables$name[which(state_variables$output==1)]))        # get names of output auxliaries
  state_units              <- gsub("_","",as.character(state_variables$unit[which(state_variables$output==1)]))        # get units of output auxliaries
  # get output auxiliaries and their units
  CheckSheetNames(dir_SCEN,"auxiliaries")  #check if sheet 'auxiliaries' exists
  auxiliaries            <- read.table(paste(dir_SCEN,"txt/auxiliaries.txt",sep=""), header=T)
  aux_names              <- gsub("_","",as.character(auxiliaries$name[which(auxiliaries$output==1)]))        # get names of output auxliaries
  aux_units              <- gsub("_","",as.character(auxiliaries$unit[which(auxiliaries$output==1)]))        # get units of output auxliaries 
}

# ****************************************************************************************************************************************************
# ----------------------------------------------------------------------------------------------------------------------------------------------------
# edit c++ files from PCLake/PCDitch (created by OSIRIS)
# 1. set changed parameters: 
#       - user-defined parameters
#       - sediment type parameters
#       - forcing function parameters (switches, e.g. ReadTemp)
# 2. set (user-defined) changed initial conditions 
# 3. edit declaration files:
#       - remove forcing function parameters (e.g. mTemp) from declaration list to prevent double declarations (as both a parameter and a time series)
#       - determine the length of the declaration arrays and store them
# ----------------------------------------------------------------------------------------------------------------------------------------------------
# ****************************************************************************************************************************************************

WriteLogFile(LogFile,ln="- pass run settings to c++ files (the model)")
cpp_files          <- list.files(paste(dir_SCEN,"source_cpp/",sep=""),pattern=".cpp")
arrays             <- vector()
for (cpp_file in cpp_files) {
  tmp <- readLines(paste(dir_SCEN,"source_cpp/",cpp_file,sep=""))
  # 1. set changed parameters
  if (grepl("sp",cpp_file)) {
     tmp <- SetParameters(tmp,pars_to_change)   #set user-defined parameters
     tmp <- SetParameters(tmp,soil_param)       #set sediment type
     tmp <- SetParameters(tmp,imposed_forcings) #set forcing function parameters
  }
  # 2. set (user-defined) changed initial conditions 
  if (grepl("sc",cpp_file)) { 
     if (length(inits_to_change) > 0) {
        temp <- inits_to_change
        names(temp) <- paste("c",substr(names(temp),start=2,stop=nchar(names(temp))),"0",sep="") #change names of initial states (e.g. change 'sDepthW' in 'cDepthW0')
        tmp <- SetParameters(tmp,temp)   #set user-defined initial conditions
     }
  }
  # 3. edit declaration files  
  if (grepl("rp",cpp_file)) { 
     i <- 0
	 for (name in names_forcing) {
        i   <- i + 1
		tmp <- gsub(paste("_",name,"_",sep=""),paste("_dummy",i,"_",sep=""),tmp) # remove forcing function parameters (e.g. mTemp) from parameter declaration list
	 }
  }
  if ((grepl("ra",cpp_file) || grepl("rp",cpp_file) || grepl("rs",cpp_file) || grepl("ri",cpp_file))) {  # determine the length of the declaration arrays and store them
     array_name <- substring(tmp[1], regexpr("=", tmp[1])[1]+2, regexpr("\\[", tmp[1])[1]-1)
	 array_length <- strsplit(tmp[length(tmp)]," ")[[1]][3]
     if (grepl("rs",cpp_file)) n_states <- as.numeric(array_length)
	 arrays <- c(arrays,paste("static double ",array_name,"[",array_length,"];",sep=""))
  }
  # 4. remove underscores (otherwise R cannot read it) and write adjusted cpp files to file  
  tmp <- gsub("_","",tmp) #remove underscores
  writeLines(tmp,paste(Dir_source_adjusted,cpp_file,sep=""))

  # 5. miscellaneous
  if (grepl("sp",cpp_file))  ConvertFileToVector(paste(Dir_source_adjusted,cpp_file,sep=""),"ref_pars")              # get reference parameters (stored in 'ref_pars')
  if(InclInterface) if ((grepl("sp",cpp_file)||grepl("sc",cpp_file))) writeLines(tmp,paste(Dir_ref,cpp_file,sep="")) # for interface version: store new parameters and initial states in file
}
writeLines(arrays,paste(Dir_source_adjusted,"arrays.cpp",sep="")) # write length of declaration arrays to file

# **************************************************************
# --------------------------------------------------------------
# edit c++ model (scripts/cpp2R/model_base.cpp) for compilation:
# 1. define output auxiliaries
# 2. define forcing functions 
# 3. refer to the right cpp files (in source_cpp/)
# --------------------------------------------------------------
# **************************************************************
WriteLogFile(LogFile,ln="- prepare model for compilation")
model_base_cpp <- readLines(paste(dir_SCHIL,"scripts/cpp2R/model_base.cpp",sep="")) # read the c++ model

# --------------------------------------------------------------
# 1. define output auxiliaries
# --------------------------------------------------------------
id             <- grep(x=model_base_cpp,pattern="output_auxiliaries")
codelines      <- vector()
aux_number     <- length(aux_names)
i              <- 0
if (length(aux_names)>0) {
   for (aux_name in aux_names) { # define user-defined output auxiliaries as output_auxiliaries
	 codelines <- c(codelines,paste("  yout[",i,"] = ",aux_name,";",sep="")) 
     i <- i + 1
   }
} else { # if there are no output auxiliaries; make at least one 'dummy' output auxiliary, as desired by DeSolve
   codelines   <- "  yout[0]=0;"
   aux_number  <- 1
   aux_names   <- "dummy"
   aux_units   <- "-"
}
model_cpp <- c(model_base_cpp[1:(id-1)],codelines,model_base_cpp[(id+1):length(model_base_cpp)])

# --------------------------------------------------------------
# 2. define forcing functions 
# --------------------------------------------------------------
id        <- grep(x=model_cpp,pattern="input_forcings")
codelines <- paste("static double forc[",(1+length(names_forcing)),"];",sep="")
codelines <- c(codelines,"double &time = forc[0];") # define time as an external forcing
i         <- 0
for (name in names_forcing) { # define user-defined forcings as external forcings
   i         <- i + 1
   codelines <- c(codelines,paste("double &",name," = forc[",i,"];",sep=""))
}
codelines <- c(codelines,paste("#define MAXFORC ",(1+length(names_forcing)),sep=""))
model_cpp <- c(model_cpp[1:(id-1)],codelines,model_cpp[(id+1):length(model_cpp)])

# --------------------------------------------------------------
# 3. refer to the right cpp files (in source_cpp/)
# --------------------------------------------------------------
cpp_files     <- list.files(paste(dir_SCEN,"source_cpp/",sep=""),pattern=".cpp")
stop_id       <- regexpr(pattern="...cpp",cpp_files[1])[[1]]-1
model_version <- substr(cpp_files[1],start=1,stop=stop_id) #get model version
model_cpp     <- sub(pattern="model_version", replacement=model_version, x=model_cpp) # insert model version into c++ file

# write the final model_cpp to file
writeLines(model_cpp,paste(dir_SCHIL,"scripts/cpp2R/model.cpp",sep=""))

# ****************************************
# ----------------------------------------
# compile cpp model (model.dll is created)
# ----------------------------------------
# ****************************************
WriteLogFile(LogFile,ln="- compile model")
#beTime <- Sys.time()
CompileModel()
#compTime <- Sys.time()-beTime


# ***********************************************************************
# -----------------------------------------------------------------------
# Initialize Model (calculate state variables at t=0; stored in 'states')
# -----------------------------------------------------------------------
# ***********************************************************************

WriteLogFile(LogFile,ln="- initialize model")
InitializeModel(n_states)

# ***********************************************************************************
# -----------------------------------------------------------------------------------
# convert time series of forcing function to input format of DeSolve + set integrator
# -----------------------------------------------------------------------------------
# ***********************************************************************************

# define run time and output time step
times 			<- seq(0,365*runtime_years,by=output_time_step)  # output time step
times_forcing   <- seq(0,365*runtime_years)
# define forcing functions
forcings <- list(time=cbind("time"=times_forcing,"value"=times_forcing))
for (name in names_forcing) {
   tmp <- subset(data_forcing,subset=(forcing==name))
   tmp_int <- approx(x=tmp$time,y=tmp$value,xout=times_forcing, method="linear",rule = 2:1) #interpolate missing day values linearly
   if (any(is.na(tmp_int$y))) WriteLogFileError(LogFile,ln=paste("Time series of forcing function '",name,"' is not defined for total run time of the model (",max(times_forcing)," days) (see Input_PCShell.xls/time_series_model_forcings.csv)",sep="")) 
   forcings <- c(forcings,list(forcing=cbind("time"=times_forcing,"value"=tmp_int$y)))
}   
# define integrator that is used to solve the differential equations
integrators <- c("euler","rk2","rk4","rk23","rk23bs","rk34f","rk45f","rk45ck","rk45e","rk45dp6","rk45dp7","rk78dp","rk78f","lsoda","lsode","lsodes","lsodar","vode","daspk","ode23","ode45","radau","bdf","bdf_d","adams","impAdams","impAdams_d","iteration")
integrator_method <- integrators[integrator]
