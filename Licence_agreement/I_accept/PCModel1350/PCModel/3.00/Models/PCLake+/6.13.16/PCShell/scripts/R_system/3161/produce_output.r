# ---------------------------------------------------
# Output routine of PCShell
# ---------------------------------------------------

# creating graphs
WriteLogFile(LogFile,ln="")
WriteLogFile(LogFile,ln="creating model output......")

# plot results (time series of state variables)
out_total <- as.data.frame(out_total)
if (length(unique(out_total$variant))<= 11 && ncol(out_total)<=102) {  # plot only when plotting is 'fast': for <= 10 model variants and <= 100 variables  
  tmp          <- melt(out_total,id=names(out_total)[c(1,ncol(out_total))],measure=names(out_total)[2:(ncol(out_total)-1)]) 
  if (InclInterface) { # get units of output states and auxiliaries
    tmp          <- merge(tmp,rbind(data.frame(name=state_names,unit=state_units),data.frame(name=aux_names,unit=aux_units)),by.x="variable",by.y="name")
    tmp$variable <- paste(tmp$variable," [",tmp$unit,"]")
  }  
  tmp$variant  <- as.factor(tmp$variant)
  hoogte       <- 2.3+1.8*(ceiling(length(unique(tmp$variable))/3)-1) # height of plot
  G = ggplot(tmp, aes(x=time,y=value,colour=variant))
  G = G + geom_line()
  G = G + facet_wrap(~ variable, ncol = 3, scales="free")
  G = G + labs(y="", x="time (days)")
  pdf(paste(Dir_output,"time_series_state_variables.pdf",sep=""), 10, hoogte)
  print(G)
  dev.off()
}

# write output data.frame (time series of all state variables and selected auxiliaries) to file
write.table(x=out_total, file=paste(Dir_output,"time_series_state_variables_final.csv",sep=""),sep=',',row.names=FALSE, col.names = TRUE, quote = FALSE)  		# write all output 

# write to logfile
end_time <- Sys.time()
WriteLogFile(LogFile,ln="")
WriteLogFile(LogFile,ln=paste("end of PCShell at: ",end_time,sep=""))
