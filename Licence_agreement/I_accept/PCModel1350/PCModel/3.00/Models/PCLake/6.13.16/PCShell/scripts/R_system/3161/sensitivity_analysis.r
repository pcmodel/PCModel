# ---------------------------------------------------
# Routine to manipulate parameters (sensitivity analysis) in PCShell
# ---------------------------------------------------
																																			
# write to logfile																	
if (run_type==2 & sensitivity_method==3) WriteLogFileError(LogFile,ln="multidimensional sensitivity analysis (run_type = 2) cannot be performed when picking parameters from a linear distribution (sensitivity_method = 3)")
if (run_type==1) WriteLogFile(LogFile,ln="performing sensitivity analysis (one by one).........")
if (run_type==2) WriteLogFile(LogFile,ln="performing sensitivity analysis (multidimensional).........")
if (nrow(parameters)==0) WriteLogFileError(LogFile,ln="no parameters are selected for the sensitivity analysis (see Input_PCShell.xlsx)")
if (run_type==1) WriteLogFile(LogFile,ln=paste("  - ",nrow(parameters)," parameters are selected and each parameter is varied ",parameter_sets," times (in total: ",nrow(parameters)*parameter_sets," model runs)",sep=""))
if (run_type==2) WriteLogFile(LogFile,ln=paste("  - ",nrow(parameters)," parameters are selected analysis (in total: ",parameter_sets," model runs)",sep=""))
if (sensitivity_method==1) WriteLogFile(LogFile,ln="  - parameter values are picked from a normal distribution (based on mean value and standard deviation)")
if (sensitivity_method==2) WriteLogFile(LogFile,ln="  - parameter values are picked from a uniform distribution (based on minimum and maximum value)")
if (sensitivity_method==3) WriteLogFile(LogFile,ln="  - parameter values are picked from a 'linear' distribution (based on minimum and maximum value)")

# ----------------------------------------------------------
# Sensitivity analysis (one by one)
# ----------------------------------------------------------
if (run_type==1) {
   nvar <- 0   
   par_list <- data.frame()
   for (i in 1:nrow(parameters)	) { 
      parms <- ref_pars 
      id <- grep(parameters$name[i],paste("_",names(parms),"_",sep=""))
      if (length(id)!=1) WriteLogFileError(LogFile,ln=paste("parameter '",parameters$name[i],"' in Input_PCShell.xlsx does not exist",sep="")) 					# check if parameter to be changed exists
      if (sensitivity_method==1) values <- rnorm(parameter_sets,mean=parameters$min_mean[i],sd=parameters$max_sd[i]) 
      if (sensitivity_method==2) values <- runif(parameter_sets,min=parameters$min_mean[i],max=parameters$max_sd[i])
      for (j in 1:parameter_sets) {          	  
         nvar <- nvar + 1 																																		# variant number
         if (sensitivity_method %in% c(1,2)) value <- max(1e-20,values[j])   																					# make sure parameter value > 0 
   	     if (sensitivity_method==3) value <- parameters$min_mean[i] + ((j-1)/(parameter_sets-1))*(parameters$max_sd[i]-parameters$min_mean[i])                  # new parameter value
         par_list_tmp <- data.frame(variant=nvar,changed_parameter=parameters$name[i],unit=parameters$unit[i],default_value=ref_pars[id],new_value=value)							# store new parameter values	 
         par_list <- rbind(par_list,par_list_tmp)
         parms[id] <- value																																		# overwrite default parameter values        
		 RunModel(states,times,parms,forcings,aux_number,aux_names,integrator_method,state_names,internal_time_step) 																							# Run Model
         out_total <- Store_results(outC,out_total,nvar)
         if ((nvar%%10)==0) WriteLogFile(LogFile,ln=paste("computed variant ",nvar," at: ",Sys.time(),sep=""))
      }
   }
   write.table(x=par_list, file=paste(Dir_changes,"parameters_sensitivity_analysis.csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  		# write changed parameters and their new value to file
}

# ----------------------------------------------------------
# Sensitivity analysis (multidimensional) (uncertainty analysis)
# ----------------------------------------------------------

if (run_type==2) {
   nvar <- 0   
   par_list <- data.frame()
   par_changes <- data.frame()
   parms <- ref_pars 
   for (i in 1:nrow(parameters)	) { 
      id <- grep(parameters$name[i],paste("_",names(parms),"_",sep=""))
      if (length(id)!=1) WriteLogFileError(LogFile,ln=paste("parameter '",parameters$name[i],"' in Input_PCShell.xlsx does not exist",sep="")) 								# check if parameter to be changed exists
      if (sensitivity_method==1) values <- rnorm(parameter_sets,mean=parameters$min_mean[i],sd=parameters$max_sd[i]) 
      if (sensitivity_method==2) values <- runif(parameter_sets,min=parameters$min_mean[i],max=parameters$max_sd[i])
      par_changes<- rbind(par_changes,data.frame(name=parameters$name[i],unit=parameters$unit,id=id,value=values,run=c(1:parameter_sets)))	    												# store new parameter values 
      par_changes$value <- replace(par_changes$value,par_changes$value<1e-20,1e-20)																							# make sure that new parameter values > 0
   }
   for (j in 1:parameter_sets) {         	  
	  nvar <- nvar + 1 																																						# variant number
      par_changes_run <- subset(par_changes,subset=(run==j))
      par_list_tmp <- data.frame(variant=nvar,changed_parameter=par_changes_run$name,unit=par_changes_run$unit,default_value=ref_pars[par_changes_run$id],new_value=par_changes_run$value)	# store new parameter values	 
      par_list <- rbind(par_list,par_list_tmp)
      parms[par_changes_run$id] <- par_changes_run$value      																												# overwrite default parameter values
      RunModel(states,times,parms,forcings,aux_number,aux_names,integrator_method,state_names,internal_time_step) 																											# Run Model
      out_total <- Store_results(outC,out_total,nvar)
      if ((nvar%%10)==0) WriteLogFile(LogFile,ln=paste("computed variant ",nvar," at: ",Sys.time(),sep=""))
   }
   write.table(x=par_list, file=paste(Dir_changes,"parameters_sensitivity_analysis.csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  					# write changed parameters and their new value to file
}

