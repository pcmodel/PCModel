# ---------------------------------------------------
# Single model run (with reference settings)
# ---------------------------------------------------

WriteLogFile(LogFile,ln="")
WriteLogFile(LogFile,ln="running the model......")
WriteLogFile(LogFile,ln=paste("running the model...... starting at: ",Sys.time(),sep=""))

out_total <- matrix()
RunModel(states,times,ref_pars,forcings,aux_number,aux_names,integrator_method,state_names,internal_time_step) # Run Model
out_total <- Store_results(outC,out_total,0)
WriteLogFile(LogFile,ln=paste("running the model...... ended at: ",Sys.time(),sep=""))
