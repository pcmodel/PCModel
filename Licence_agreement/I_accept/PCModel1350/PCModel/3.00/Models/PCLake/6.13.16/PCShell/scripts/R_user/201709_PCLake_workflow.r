rm(list=ls())
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# user defined settings
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
dirHOME			=	"F:/Manqi/software/PCLakeMQ_topic3/Licence_agreement/I_accept/"				# location of the PCModel1350 folder
fileDATM		=	"PL613162_rebuildv11_surf_v53.xls"												              # file name of the DATM implementation
#fileDATM		=	"PL613162_PCLakePLUS_online_v10.xls"												              # file name of the DATM implementation
#fileDATM		=	"PL613162_PCLakePLUS_online_v11-surf - v53.xls"												              # file name of the DATM implementation
#fileParameters <- read.csv('C:/Users/WNet/stack/PClakeDitch/PCModel1350/PCModel/3.00/Models/PCLake/6.13.16/R_base_work_case_Naardermeer/input/states_parameters - Uitspoelverhoog.csv', sep =';')
#mPNQEvload <- read.csv('C:/Users/WNet/stack/PClakeDitch/PCModel1350/PCModel/3.00/Models/PCLake/6.13.16/R_base_work_case_Naardermeer/input/states_parameters - Uitspoelverhoog.csv', sep =';') 

#load all the functions
source(paste(dirHOME,"PCModel1350/PCModel/3.00/Models/PCLake/6.13.16/PCShell/scripts/R_system/functions_PCLake.R",sep="")) 

#load settings from the DATM file
lDATM_SETTINGS		=	PCModelReadDATMFile(dirHOME=dirHOME, 
										fileDATM=fileDATM)



##--------------------------------------------------NOT USING-------------------------------------------
#hier een loop over de EAGS voor alle forcings
# je zou hier per EAG dan een single run kunnen doen en dan alles weer onder elkaar plakken
#make a list to store all inputs
lFORCINGS_EAGS = list()
# dan moet je de nPARAM_SET en de nSTATE_SET in PCmodelSingleRun instellen op de index van de eag set (zie onder voor multirun)

for(sEAG_SET in 1:8){ #maak hier zelf iets mooiers van
  
  #aanpassen van forcings (gemeten tijdseries)
  #  voorbeeld van mQIn
  #  get all names of forcings
  vFORCING_NAMES = names(lDATM_SETTINGS$forcings)
  # change a given forcing by name
  #for(sFORCING in names(lDATM_SETTINGS$forcings)[2:length(vFORCING_NAMES)]){# dit is 2:length omdat 1 altijd de 'time' bevat
    nFORCING=2#change to loop through forcing names
    #laad hier je tijdserie uit een bestand voor desbetreffende EAG en desbetreffence variabele
    #vTIME_SERIES = dfTIME_SERIES_PER_EAG[,sFORCING] #bijvoorbeeld op zo'n manier, pas aan naar datastructuur
    vTIME_SERIES = seq(from=0.1, to=3, length=365) #example time series for one year
    vTIME_FORCINGS = lDATM_SETTINGS$forcings$time[,1] #get a vector with the forcing time steps used in PCLake (i.e. the number of days of the total model run)
    #now we add the time series to the forcing list.
    #  this means we select the lDATM_SETTINGS forcing by name (from vFORCINGS_NAMES) and select the value column (column number 2)
    #  We then replicate the measured time series over the entire forcing time length (all run days of PCLake)
    #   thereby effectively repeating the average year time series for every given year
    lDATM_SETTINGS$forcings[[vFORCING_NAMES[nFORCING]]][,'value'] = rep_len(vTIME_SERIES, length.out=length(vTIME_FORCINGS))#put whatever time series you want here
  #}
  #make a list of forcing input per eag
  #lFORCINGS_EAGS = list(lFORCINGS_EAGS, lDATM_SETTINGS$forcings)
  # names(lFORCINGS_EAGS) = c('EAG1','EAG2')#namen van je EAGS
}  

# extra parameterssets obv ingevoerde parameters
dfPARAMS <- cbind(lDATM_SETTINGS$params[,c(1:3)], do.call("cbind.data.frame", replicate(nrow(fileParameters), lDATM_SETTINGS$params[,4], simplify = FALSE)))
colnames(dfPARAMS) = c(colnames(dfPARAMS)[1:3], as.character(fileParameters$Modeleag))
for(i in 2:ncol(fileParameters)){
  parname<- colnames(fileParameters)[i]
  dfPARAMS[parname == rownames(dfPARAMS), c(4:(nrow(fileParameters)+3))] <- fileParameters[,i]
  }
dfPARAMS[, c(4:ncol(dfPARAMS))]=sapply(dfPARAMS[, c(4:(nrow(fileParameters)+3))], as.numeric)
#put data frame back into lDATM_SETTINGS to maintain same IDs in script below
lDATM_SETTINGS$params=dfPARAMS

# extra states obv ingevoerde parameters
dfSTATES <- cbind(lDATM_SETTINGS$states[,c(1:2)], do.call("cbind.data.frame", replicate(nrow(fileParameters), lDATM_SETTINGS$states[,5], simplify = FALSE)))
colnames(dfSTATES) = c(colnames(dfSTATES)[1:2], as.character(fileParameters$Modeleag))
for(i in 2:ncol(fileParameters)){
  parname<- colnames(fileParameters)[i]
  dfSTATES[parname == rownames(dfSTATES), c(3:(nrow(fileParameters)+2))] <- fileParameters[,i]
}
dfSTATES[, c(3:(nrow(fileParameters)+2))]= sapply(dfSTATES[, c(3:(nrow(fileParameters)+2))], as.numeric)
#put data frame back into lDATM_SETTINGS
lDATM_SETTINGS$states=dfSTATES
##-------------------------------------------------END NOT USING-------------------------------------------



#setup the model folder structure and copy cpp files
lMODEL_SETUP		=	PCModelSetup(
							dfSTATES=lDATM_SETTINGS$states, 
							dfPARAMS=lDATM_SETTINGS$params, 
							dfAUXIL=lDATM_SETTINGS$auxils, 
							dfRUNSETTINGS=lDATM_SETTINGS$run_settings, 
							dirHOME=dirHOME,
							sediment_type       = 	0,                          # available sediment types: 0=default settings (/source_cpp/*sp.cpp), 1=clay, 2=peat, 3=sand
							work_case           =	"R_base_work_case",         # name of output folder (work case)
							modelname 			=	"_org",					              	# name of the model (suffix to specific model files)
						 	tGENERATE_INIT		=	FALSE						# trigger whether to generate all variables as output for init_rep
						)

#compile the model to c++ omzetten naar binair
PCModelCompileModel(dirHOME=dirHOME)


#set the model's initial values based on current parameters and state inits
lMODEL_INIT			=	PCModelInitializeModel(dfSTATES=lDATM_SETTINGS$states, 
												dfPARAMS=lDATM_SETTINGS$params, # parameters worden waarschijnlijk niet aangepast obv 
												dirHOME=dirHOME)


#run a single instance of the model
dfPCLAKE_RUN		=	PCmodelSingleRun(	dfSTATES			=	lMODEL_INIT$states_init, # LMODEL ipv lDATM omdat bepaalde initele states (massabalans C:N:P) worden herberekend
											dfPARAMS			=	lMODEL_INIT$params_init, 
											dfAUXIL				=	lDATM_SETTINGS$auxils,
											dfRUNSETTINGS		=	lDATM_SETTINGS$run_settings,
											dfFORCINGS			=	lDATM_SETTINGS$forcings,
											nPARAM_SET			=	0,
											nSTATE_SET			=	0,
											integrator_method	=	"vode",
											dirHOME				=	dirHOME,
											tGENERATE_INIT		=	FALSE,
											tAVERAGE  = FALSE)


#make a bifurcation
##make a data frame of states (40*clear and 40*turbid)
##make a data frame of parameters (80*set1 with changed cPLoad and cNLoad)
#advanced:*8 scenario's
#advanced: *8 different sets of parameters that are different for scenarios

dfPClakeMultiRun = lapply(1:(ncol(lDATM_SETTINGS$params)-3), function(i) PCmodelSingleRun(dfSTATES			=	lMODEL_INIT$states_init, 
                                                 dfPARAMS			=	lMODEL_INIT$params_init, 
                                                 dfAUXIL				=	lDATM_SETTINGS$auxils,
                                                 dfRUNSETTINGS		=	lDATM_SETTINGS$run_settings,
                                                 dfFORCINGS			=	lDATM_SETTINGS$forcings,
                                                 nPARAM_SET			=	i,
                                                 nSTATE_SET			=	i,
                                                 integrator_method	=	"vode",
                                                 dirHOME				=	dirHOME,
                                                 tGENERATE_INIT		=	FALSE,
                                                 tAVERAGE  = TRUE))

names(dfPClakeMultiRun) <- as.character(unlist(fileParameters[1]))
multirunDF <- do.call('rbind', dfPClakeMultiRun)
# Eag namen toevoegen aan iedere lijst
multirunDF$EAG <- rep(names(dfPClakeMultiRun), each = nrow(multirunDF)/length(names(dfPClakeMultiRun)))

# plotjes maken van resultaten
ggplot(multirunDF, aes(time, aCovVeg))+
  geom_point()+
  facet_wrap(~EAG)

ggplot(multirunDF, aes(time, oChla))+
  geom_line()+
  facet_wrap(~EAG)

ggplot(multirunDF[multirunDF$time>15000,], aes(time, oChla))+
  geom_line()+
  facet_wrap(~EAG)

ggplot(multirunDF[multirunDF$time>15000,], aes(time, oPTotW))+
  geom_line()+
  facet_wrap(~EAG)

ggplot(multirunDF[multirunDF$time>15000,], aes(time, aCovVeg))+
  geom_line()+
  facet_wrap(~EAG)

ggplot(multirunDF[multirunDF$time>15000,], aes(time, oChlaBlue))+
  geom_line()+
  facet_wrap(~EAG)

# run a bifurcation
# a bifurcation is essentially the same model with one (or a few) changed parameters (e.g. cPLoad and cNLoad)
# This parameter is increased step by step and model results are averaged over a certain period to 
#   examine the effect of the parameter increase on some model auxiliary or state variable (e.g. Chla)
# Furthermore, to examine hysteresis the model is ran from two initial starting points (e.g. clear vs turbid)

#Let's build a simple bifurcation in which we slowly increase cPLoad and extract averaged results for each run
# define the P loadings
lBIFURC_VALS=c(	seq(from=lDATM_SETTINGS$params['mPLoad',2], to=lDATM_SETTINGS$params['mPLoad',3],  length.out=30))

# make data.frame to store model output
dfBIFURC_OUT <- data.frame() 

#We start by defining the parameter sets 
dfPARAMS_BIFURC = lMODEL_INIT$params_init[,c(1:3)]
# we copy set 0 op positie 4 many times as the base set and bind it to the first few columns of the parameter data frame
dfPARAMS_BIFURC = cbind.data.frame(dfPARAMS_BIFURC, 
                              do.call(cbind, replicate(length(lBIFURC_VALS),lMODEL_INIT$params_init[,c(4),drop=F],simplify=FALSE)) )
#set the cPLoad variable to the values in lBIFURC_VALS
#!!NOTE if ReadPLoad and ReadNLoad are not 0, this will not work as the model will be using uPLoad rather than cPLoad
dfPARAMS_BIFURC['ReadPLoad',-c(1:3)] = 0
dfPARAMS_BIFURC['cPLoad',-c(1:3)] = lBIFURC_VALS
dfPARAMS_BIFURC['cNLoad',-c(1:3)] = lBIFURC_VALS*10 #when running a nutrient loading bifurcation it is common to change both N and P. By default cNLoad = cPLoad*10

#now we loop over all the parameter sets
for(nPAR_SET in 1:length(lBIFURC_VALS)){
  #we run the model twice, once with state set 0 (turbid default) and once with state set 1 (clear default)
  # note that tAVERAGE  = TRUE so that the runs return the average values of the output variables, averaged as per the user defined settings
  dfRUN_TURBID  = PCmodelSingleRun(	dfSTATES			=	lMODEL_INIT$states_init, 
                                    dfPARAMS			=	dfPARAMS_BIFURC, 
                                    dfAUXIL				=	lDATM_SETTINGS$auxils,
                                    dfRUNSETTINGS		=	lDATM_SETTINGS$run_settings,
                                    dfFORCINGS			=	lDATM_SETTINGS$forcings,
                                    nPARAM_SET			=	nPAR_SET,
                                    nSTATE_SET			=	1,
                                    integrator_method	=	"vode",
                                    dirHOME				=	dirHOME,
                                    tGENERATE_INIT		=	FALSE,
                                    tAVERAGE  = TRUE)
  dfRUN_CLEAR  = PCmodelSingleRun(	dfSTATES			=	lMODEL_INIT$states_init, 
                                    dfPARAMS			=	dfPARAMS_BIFURC, 
                                    dfAUXIL				=	lDATM_SETTINGS$auxils,
                                    dfRUNSETTINGS		=	lDATM_SETTINGS$run_settings,
                                    dfFORCINGS			=	lDATM_SETTINGS$forcings,
                                    nPARAM_SET			=	nPAR_SET,
                                    nSTATE_SET			=	2,
                                    integrator_method	=	"vode",
                                    dirHOME				=	dirHOME,
                                    tGENERATE_INIT		=	FALSE,
                                    tAVERAGE  = TRUE)
  
  #generate the output by binding columns of the two runs, adding this to existing data and adding info on the bifurcation parameter
  dfBIFURC_OUT = rbind(dfBIFURC_OUT,cbind(data.frame(bifurc_val=lBIFURC_VALS[nPAR_SET], start='turbid'),dfRUN_TURBID))
  dfBIFURC_OUT = rbind(dfBIFURC_OUT,cbind(data.frame(bifurc_val=lBIFURC_VALS[nPAR_SET], start='clear'),dfRUN_CLEAR))

}

ggplot(dfBIFURC_OUT, aes(x=bifurc_val, y=oChla, color=start,linetype=start,symbol=start,group=start))+
  geom_point()+geom_line()







