rm(list=ls())
# **************************************************************************
# --------------------------------------------------------------------------
# PCLake Initial states and t=0 settings				
# author: Sven Teurlincx (June 2017), based on Luuk van Gerven	(November 2015)
# used libraries: deSolve (to run PCLake)
# --------------------------------------------------------------------------
# **************************************************************************

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# user defined settings
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
dir_SCHIL           =	"D:/Users/SvenT/Documents/PCLake/PCLake_3162/PCModel1350/PCModel/3.00/Models/PCLake/6.13.16/PCShell/"	# location of PCShell
dir_DATM			=	"D:/Users/SvenT/Documents/PCLake/PCLake_3162/PCModel1350/PCModel/3.00/"					# location of DATM implementation (excel)
file_DATM			=	"PL613162.xls"																			# file name of the DATM implementation
work_case           =	"R_base_work_case"                      											# name of work case
modelname 			=	"_org"																					# name of the model (suffix to specific model files)

#define the number of logical processers (cores) to use for the computation
nCORES	=	4

#Set to true if the initialisation should output all states, parameters and auxiliaries
tGENERATE_INIT	=	TRUE

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

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create extra sets of parameter and intial state settings
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#this is where we should add state sets and parameter sets

## #example of adding states
## #we first copy the old sets
## dfSTATES_NEW				=	as.data.frame(dfSTATES[,-which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
## colnames(dfSTATES_NEW)		=	paste('Set', c(4:7),sep='')
## #we change a variable (e.g. PO4) for three of the four sets (set1, set2, set3)
## dfSTATES_NEW[rownames(dfSTATES_NEW)=='sPO4S', c(2,3,4)]=0.06
## #then we bind the new states to the old states
## dfSTATES=cbind(dfSTATES,dfSTATES_NEW)
## 
## #also add some parameter sets to go along with the new state sets
## #we first copy the old sets
## dfPARAMS_NEW				=	as.data.frame(dfPARAMS[,-which(colnames(dfPARAMS) %in% c('iReport','sMinValue','sMaxValue'))])
## colnames(dfPARAMS_NEW)		=	paste('Set', c(4:7),sep='')
## #we change a variable (e.g. kHarvFishSum) for three of the four sets (set1, set2, set3)
## dfPARAMS_NEW[rownames(dfPARAMS_NEW)=='kHarvFishSum', c(2,3,4)]=c(0.2,0.3,0.4)
## #then we bind the new states to the old states
## dfPARAMS=cbind(dfPARAMS,dfPARAMS_NEW)
 
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
# Run the model using parameter and state set combinations
#	Note that at current the implementation assumes a direct coupling between state set and parameter set 
#	meaning that parameter set 1 will use state set 1, and parameter set 2 will use state set 2
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

for (nSET in 1:ncol(dfPARAMS_INIT)) {      # loop over sets
	
	# Provide the model with the parameter settings of the given set
	new_pars     =	dfPARAMS_INIT[,nSET]
	names(new_pars) <- rownames(dfPARAMS_INIT) 
	
	# Provide the model with the initial states of the given set  
	new_states	=	dfSTATES_INIT_T0[,nSET+2]
	names(new_states) <- rownames(dfSTATES_INIT_T0) 
	
	#define integrator
	int        <- "vode"
	
	#single run
	dyn.load("model.dll")
	single_run=ode(y=new_states, times=times, func = "CalculateDerivatives", parms = new_pars, dllname = "model",initforc = "forcc",forcings=forcings,initfunc = "initmod", nout = aux_number, outnames = aux_names, method = int)
	dyn.unload("model.dll")
	
	#write output to dataframe
	if(colnames(dfPARAMS_INIT)[nSET]=="sDefault0"){
		dfOUTPUT_FINAL	=	cbind.data.frame(
				Type	=	c(rep("InitParameter",length(new_pars)),rep("InitStates",length(new_states)),rep("Calculated",length(single_run[1,]))),
				Id		=	rep(1:length(c(new_pars,new_states, single_run[1,]))),
				Name	=	c(names(new_pars),names(new_states), names(single_run[1,])),
				nSET=c(new_pars,new_states, single_run[1,])				
						)
					
	}else{
		dfOUTPUT_FINAL	=	cbind.data.frame(dfOUTPUT_FINAL,
								nSET=as.data.frame(c(new_pars,new_states, single_run[1,])))
	}
	WriteLogFile(LogFile,ln=paste("Initials recorded for Set_",nSET-1,sep=""))
}
colnames(dfOUTPUT_FINAL)	=	c("Type","Id","Name", paste("Set_",(1:ncol(dfPARAMS_INIT)-1),sep=''))

write.table(x=dfOUTPUT_FINAL, file=paste(dir_SCEN,"results/","initrep_",work_case,".csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE) 	
	
# write end time to logfile
WriteLogFile(LogFile,ln=paste("end of PCShell at: ",Sys.time(),sep=""))

