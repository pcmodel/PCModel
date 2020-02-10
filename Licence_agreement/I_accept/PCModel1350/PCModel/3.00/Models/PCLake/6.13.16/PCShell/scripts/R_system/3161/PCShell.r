# **************************************************************************
# --------------------------------------------------------------------------
# PCSHELL (compiling cpp modelcode) (Main Program)				
# author: Luuk van Gerven	(april 2012)
# used libraries: ggplot2, deSolve, RODBC
# --------------------------------------------------------------------------
# **************************************************************************

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# user defined settings
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
InclInterface <- T        # include PCShell interface; yes (T) or no (F)

if (!(InclInterface)) {
  # set location of PCShell and name of work case
  dir_SCHIL           <- "D:/Luuk/Models/PCDitch/R/PCShell/"              	# location of PCShell
  work_case           <- "benchmark_default"                                        # name of work case
} else {
  #get name of working directory and path of PCShell from argument provided by batch file
  path_work_dir       <- commandArgs(trailingOnly = TRUE)
  tmp                 <- unlist(strsplit(path_work_dir,"/"))
  work_case           <- tmp[length(tmp)]
  dir_SCHIL           <- paste(paste(tmp[1:(length(tmp)-1)],collapse="/"),"/",sep="")
}

# set these things when running this script stand alone (not from PCShell macro) and including the PCShell interface
#dir_SCHIL           <- "D:/Luuk/Models/PCDitch/R/PCShell/"              	# location of PCShell
#work_case           <- "PCLake_default"                                        # name of work case

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# PCShell computation
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#begTime <- Sys.time()
source(paste(dir_SCHIL,"scripts/R/functions.R",sep=""))  										                    # Define functions
source(paste(dir_SCHIL,"scripts/R/initialisation.R",sep=""))     								                    # Initialisation (read user defined input + convert cpp files of model + compile model)
source(paste(dir_SCHIL,"scripts/R/single_run.R",sep="")) 										                    # Run the model (with reference settings)
#runTime <- Sys.time()-begTime
if (InclInterface) if (run_type %in% c(1,2)) source(paste(dir_SCHIL,"scripts/R/sensitivity_analysis.R",sep="")) 	# Perform sensitivity analysis (run the model with adjusted parameter settings)
source(paste(dir_SCHIL,"scripts/R/produce_output.R",sep=""))  	    							                    # Create output in graphs + files (of time series of state variables and auxiliaries)
