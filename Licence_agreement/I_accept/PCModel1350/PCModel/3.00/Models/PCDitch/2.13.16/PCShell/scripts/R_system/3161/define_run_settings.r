# -----------------------------------------------------------
# definition of run settings for the 'interface-free' version
# -----------------------------------------------------------

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
# define parameters to change
pars_to_change <- c(
# InitCalc = 1 #strongly recommended if initial state is changed
)

# -----------------------------------------------------------
# 4. forcing functions to be imposed on the model 
# -----------------------------------------------------------

# define forcing functions to impose on the model
imposed_forcings <- c(
  ReadTemp = 0,
  ReadLOut = 0,
  ReadVWind = 0,
  ReadQIn = 0,
  ReadQOut = 0,
  ReadQEv = 0,
  ReadPLoadPhyt = 0,
  ReadDLoadDet = 0,
  ReadDLoadIM = 0,
  ReadPLoad = 0,
  ReadNLoad = 0,
  ReadNutFrac = 0
)


# -----------------------------------------------------------
# 5. define output variables (states and auxilaries)
# -----------------------------------------------------------

state_names <- c("sDepthW")
aux_names <- c("oPTotW","oNTotW","oChla","aSecchi","aCovVeg")

# -----------------------------------------------------------
# post processing...
# -----------------------------------------------------------
# get parameter values that define the sediment_type
soil_param     <- SetSedimentType(sediment_type) 
# get list of 'active forcings' (par_forcing) and corresponding variable names (names_forcing)
names_forcing       <- GetForcing(imposed_forcings)
# read time series of forcings  
data_forcing        <- data.frame()
temp                <- read.csv(paste(dir_SCEN,"time_series_model_forcings.csv",sep=""))
data_forcing        <- melt(temp,id="day",measure=names(temp)[2:ncol(temp)])[,c(2,1,3)]
names(data_forcing) <- c("forcing","time","value")

