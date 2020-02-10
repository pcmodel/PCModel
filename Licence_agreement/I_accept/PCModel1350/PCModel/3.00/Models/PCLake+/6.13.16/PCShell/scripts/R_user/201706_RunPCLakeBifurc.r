rm(list=ls())
# **************************************************************************
# --------------------------------------------------------------------------
# PCLake bifurcation				
# author: Sven Teurlincx (March 2017), based on Luuk van Gerven	(November 2015)
# used libraries: deSolve (to run PCLake), foreach (multithreading), doSNOW (multithreading)
# --------------------------------------------------------------------------
# **************************************************************************

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# user defined settings
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
dir_HOME			=	"D:/Users/SvenT/Documents/PCLake/PCLake_3162/"							# location of the PCModel1350 folder
dir_DATM			=	paste(dir_HOME,"PCModel1350/PCModel/3.00/",sep='')						# location of DATM implementation (excel)
dir_SCHIL           =	paste(dir_DATM,"Models/PCLake/6.13.16/PCShell/",sep='')		# location of PCShell
file_DATM			=	"PL613162.xls"															# file name of the DATM implementation
work_case           =	"R_base_work_case"                      								# name of work case
modelname 			=	"_org"																	# name of the model (suffix to specific model files)


# ***********define the bifurcation parameter P loading range*************
# the model will run for each of these P loads
lBIFURC_PLOAD		=	NA 																		#set to NA to load this information from the DATM

#define the number of logical processers (cores) to use for the computation
nCORES	=	4

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# initialisation of PCLake R Shell
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
source(paste(dir_SCHIL,"scripts/R_system/functions.R",sep=""))  					 # Define functions
#here we load the cpp files containing the model code (the equations and initial settings) 
#	these are loaded in the script as the user may wish to create multiple different models (different cpp's) and compare them
#	In that case the user will have to compile multiple different DATM instances and save the cpp files to different folders,
#		the names of which can be looped through 

cpp_files <- list.files(file.path(dir_DATM,paste("Frameworks/Osiris/3.01/PCLake/",sep="")), full.names = TRUE)[
					which((lapply(strsplit(x=list.files(file.path(dir_DATM,paste("Frameworks/Osiris/3.01/PCLake/",sep="")), full.names = TRUE), split="[/]"), 
							function(x) which(x %in% c("pl61316ra.cpp","pl61316rc.cpp","pl61316rd.cpp","pl61316ri.cpp","pl61316rp.cpp","pl61316rs.cpp",
														"pl61316sa.cpp","pl61316sc.cpp","pl61316sd.cpp","pl61316si.cpp","pl61316sp.cpp","pl61316ss.cpp")))>0)==TRUE)]		
file.copy(cpp_files, file.path(dir_SCHIL, work_case,"source_cpp"),overwrite=T)

source(paste(dir_SCHIL,"scripts/R_system/201703_initialisationDATM.R",sep=""))    	 # Initialisation (read user defined input + convert cpp files of model + compile model)

# ***********************************************************************
# -----------------------------------------------------------------------
# Initialize Model (calculate state variables at t=0; stored in 'states')
# -----------------------------------------------------------------------
# ***********************************************************************

WriteLogFile(LogFile,ln="- initialize model")
dfSTATES_INIT_T0	= 	as.data.frame(dfSTATES[,which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
dfSTATES_INIT		=	as.data.frame(dfSTATES[,-which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
for (nSET in 1:ncol(dfSTATES_INIT)){
	
	vSTATES_LIST		=	dfSTATES_INIT[,nSET]
	names(vSTATES_LIST)	=	dfSTATES$sInitialStateName
	InitializeModel(n_states, vSTATES_LIST)
	dfSTATES_INIT_T0=cbind.data.frame(dfSTATES_INIT_T0,states)
}
colnames(dfSTATES_INIT_T0)=colnames(dfSTATES)

dfPARAMS_INIT	=	as.data.frame(dfPARAMS[,-which(colnames(dfPARAMS) %in% c('iReport','sMinValue','sMaxValue')),drop=F])

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run the bifurcation of PCLake
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define the lBIFURC_PLOAD based on the excel DATM when not defined by the user
if(is.na(lBIFURC_PLOAD)[1] == TRUE){

	lBIFURC_PLOAD	=	seq(from=dfPARAMS[which(rownames(dfPARAMS)=='mPLoad'),which(colnames(dfPARAMS) == 'sMinValue')],
							to=dfPARAMS[which(rownames(dfPARAMS)=='mPLoad'),which(colnames(dfPARAMS) == 'sMaxValue')],
							length.out=21)
	}
	
# Select the states for bifurcation ( we use Set0 {turbid} and Set1 {clear} as the default)
dfSTATES_BIFUR	=	as.data.frame(dfSTATES_INIT_T0[,which(colnames(dfSTATES_INIT_T0) %in% c('sDefaultSetTurbid0','sDefaultSetClear1'))])

# Change the parameters to setup the bifurcation
# We copy set0 parameters as the parameters should be similar throughout a bifurcation analysis with the exception of the bifurcation parameter
dfPARAMS_BIFUR	=	as.data.frame(rep(dfPARAMS_INIT[,which(colnames(dfPARAMS_INIT) %in% c('sDefault0')),drop=F],2))
rownames(dfPARAMS_BIFUR)	=	rownames(dfPARAMS_INIT)

# Prepare model to be run on different cores (multithreading: initialize the threads/clusters)
LoadPackage("foreach")
LoadPackage("doSNOW")
snowCLUSTER <- makeCluster(nCORES, type="SOCK")
clusterExport(cl=snowCLUSTER,list= c('dfSTATES_BIFUR','dfPARAMS_BIFUR','times','forcings','aux_number','aux_names','state_names','internal_time_step','fAVG_START_YEAR','fAVG_START_DAY','fAVG_END_DAY'), envir = .GlobalEnv) #list must hold all pre-defined variables used within the foreach loop
registerDoSNOW(snowCLUSTER)	


WriteLogFile(LogFile,ln=paste("start running model at: ",Sys.time(),sep=""))
dfRES <- foreach (fBIFURC_PLOAD = lBIFURC_PLOAD, .combine='rbind', .packages=c('deSolve'), 
					.export=c("CompileModel", "InitializeModel"))%dopar% {

  # make data.frame to store model output and load the functions needed to run the model  
  output_final <- data.frame() 
  run_nr <- 0

  # Set up the parameters for bifurcation
  dfPARAMS_BIFUR[which(rownames(dfPARAMS_BIFUR)=="cPLoad"),]=fBIFURC_PLOAD
  dfPARAMS_BIFUR[which(rownames(dfPARAMS_BIFUR)=="cNLoad"),]=fBIFURC_PLOAD*7

  # -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Run the model using parameter and state set combinations
  #	Note that at current the implementation assumes a direct coupling between state set and parameter set 
  #	meaning that parameter set 1 will use state set 1, and parameter set 2 will use state set 2
  # -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  for (nSET in 1:ncol(dfPARAMS_BIFUR)) {      # loop over sets
	
	# Provide the model with the parameter settings of the given set
	new_pars     =	dfPARAMS_BIFUR[,nSET]
	names(new_pars) <- rownames(dfPARAMS_BIFUR) 
	
	# Provide the model with the initial states of the given set  
	new_states	=	dfSTATES_BIFUR[,nSET]
	names(new_states) <- rownames(dfSTATES_BIFUR)  
   
    # run PCLake and store the summer averaged output in the last year (1 April till 30 Sept)
    run_nr     <- run_nr + 1
    int        <- "vode"
    #output     <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"vode",state_names,internal_time_step))  # Run the model (with reference settings)
    error      <- class(tryCatch(output <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"vode",state_names,internal_time_step)),error = function(e) e))[1] == "simpleError"
    output_avg <- as.data.frame(t(colMeans(subset(output, subset=(time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY)))))))       # get summer averaged value (last year) of all output variables 
    if(any(is.na(output_avg)) | error) {  # run the model again when integrator "vode" returns negative or NA outputs, rerun with integrator "daspk"
      int        <- "daspk"
      #output     <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"daspk",state_names,internal_time_step))  # Run the model (with reference settings)
      error      <- class(tryCatch(output <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"daspk",state_names,internal_time_step)),error = function(e) e))[1] == "simpleError"
      output_avg <- as.data.frame(t(colMeans(subset(output, subset=(time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY)))))))       # get summer averaged value (last year) of all output variables 
      if(any(is.na(output_avg)) | error) { # run the model again when integrator "daspk" returns negative or NA outputs, rerun with integrator "euler"
        int        <- "euler"
        #output     <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"euler",state_names,0.003))  # Run the model (with reference settings)
        error      <- class(tryCatch(output <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"euler",state_names,0.003)),error = function(e) e))[1] == "simpleError"
        output_avg <- as.data.frame(t(colMeans(subset(output, subset=(time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY)))))))
        if(any(is.na(output_avg)) | error) { # run the model again when integrator "euler" returns negative or NA outputs, rerun with integrator "euler" with timesept 0.002
          #output     <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"euler",state_names,0.002))  # Run the model (with reference settings)
          error      <- class(tryCatch(output <- as.data.frame(RunModel(new_states,times,new_pars,forcings,aux_number,aux_names,"euler",state_names,0.002)),error = function(e) e))[1] == "simpleError"
          output_avg <- as.data.frame(t(colMeans(subset(output, subset=(time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY)))))))
        }
      }
    }
    if (run_nr%%5==0) WriteLogFile(LogFile,ln=paste("completed model run on set ",nSET," with PLoad = ",fBIFURC_PLOAD," at: ",Sys.time(),sep=""))

    # store results
    output_final <- rbind(output_final,cbind(data.frame(cPLoad=fBIFURC_PLOAD,sSet=nSET,integrator=int,error=error),output_avg))  
  }
  #return the output_final at the end of each run on a core
  write.table(x=output_final, file=paste(dir_SCEN,"results/","bifurcation_results_",fBIFURC_PLOAD,".csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  		# write all output 
  return(output_final)  
}

# Stop multithreading (close clusters)
stopCluster(snowCLUSTER)

# write model output to file 
write.table(x=dfRES, file=paste(dir_SCEN,"/results/bifurcation_results_all.csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  		# write all output 

# write end time to logfile
WriteLogFile(LogFile,ln=paste("end of PCShell at: ",Sys.time(),sep=""))