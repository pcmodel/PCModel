##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Default PCLakeS+ R script
##
## This script was developed by Lilith Kramer
##
## Order of actions
##   1. Setting directories and sourcing helper scripts
##   2. Making folder structure for running the model
##   3. Load DATM file 
##   4. Optional: Make adjustments to the model 
##   5. Preparing the network
##   6. Optional: Plotting the network
##   7. Make and adjust cpp files
##   8. Compile model
##   9. Run model
##
## NB: you cannot use different combinations of forcings for different lakes. e.g. 
##      - in lake 1: mQInEpi & mQInHyp
##      - in lake 2: mPLoad
##     You'll have to add for both: mQInEpi, mQInHyp AND mPLoad, and adjust the forcing values accordingly 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Clean work environment (optional)
# rm(list=ls())

## Global settings (optional)
options(scipen = 999) ## turn off scientific notation for small or very large numbers


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Setting directories and sourcing helper scripts -------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Set directories and names
dirHome <- "C:/Users/LilithK/Documents/PCModel/PCModel - official version/Licence_agreement/I_accept/"	## location of the PCModel1350 folder
dirShell <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PCShell") ## location of the PCShell folder
dirCpp_root <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Frameworks", "Osiris", "3.01", "PCLake_plus") ## location of the PCLake C++ files
nameWorkCase <- "PCLakeS_plus_default"
fileDATM <- "PL613162PLUS_Network.xls"
fileXls <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", fileDATM)
# folderTxt  <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")

## source helper scripts
source(file.path(dirShell, "scripts", "R_system", "functions.R"))
source(file.path(dirShell, "scripts", "R_system", "functions_PCLake.R")) 

## load additional libraries
loadPackage("igraph") ## for step 6. (optional)
loadPackage("data.table") ## for step 9 and 10 (saving data)
loadPackage("dplyr") ## for step 10. (select function)
loadPackage("ggplot2") ## for step 10. (plotting)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Creating a work_case and making the work_case folder structure ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PCModelWorkCaseSetup(dirSHELL = dirShell,
                     dirCPP_ROOT = dirCpp_root,
                     nameWORKCASE = nameWorkCase)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3. Load DATM file ----------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lDATM_SETTINGS <- PCModelReadDATMFile_PCLakeSplus(fileXLS = fileXls,
                                                  locFORCING = 'excel')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4. Optional: adjust model settings------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For example, change the sediment settings of each lake
lDATM_SETTINGS$params$sSet0 <- adjustSedimentParamSettings_PCLakeSplus(lDATM_SETTINGS$params, "sSet0", sediment_type = "peat")
lDATM_SETTINGS$params$sSet1 <- adjustSedimentParamSettings_Rnet(lDATM_SETTINGS$params, "sSet1", sediment_type = "sand")
lDATM_SETTINGS$params$sSet2 <- adjustSedimentParamSettings_Rnet(lDATM_SETTINGS$params, "sSet2", sediment_type = "clay_peat")
lDATM_SETTINGS$params$sSet3 <- adjustSedimentParamSettings_Rnet(lDATM_SETTINGS$params, "sSet3", sediment_type = "peat")
lDATM_SETTINGS$params$sSet4 <- adjustSedimentParamSettings_Rnet(lDATM_SETTINGS$params, "sSet4", sediment_type = "clay_peat")
# lDATM_SETTINGS$params[c( 118, 119, 80, 86, 87),] ## check


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5. Preparing the network ---------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Define the network
dfNetwork <- data.frame(node_nr_from = c(0, 0, 1, 2, 3), ## The node numbers are based on the iRuniD's in the Excel sheet of PCLakeS+
                        node_nr_to  = c(1, 2, 3, 3, 4), ## The node numbers are based on the iRuniD's in the Excel sheet of PCLakeS+
                        fraction   = c(0.3, 0.7, 1, 1, 1))

## Define the lakes
dfNodes <- data.frame(node_nr = c(0, 1, 2, 3, 4), ## The node numbers are based on the iRuniD's in the Excel sheet of PCLakeS+
                      node_name = c("Lake A", "Lake B", "Lake C", "Lake D", "Lake E"), ## This column is present for convenience, it is not used by the functions to run the model
                      surface_area  = c(1, 2, 1, 1, 1), ## (relative) surface areas of each lake
                      lat = c(0, 1, 1, 2, 3), ## This column is present for convenience, it is used to plot the nodes
                      lon = c(2, 2.5, 1, 2, 1.5)) ## This column is present for convenience, it is used to plot the nodes

## Build a forcing list from the fractions
dfNetwork$param_names <- paste0("mFracN", dfNetwork$node_nr_from, "N", dfNetwork$node_nr_to)
dfNetwork$value <- dfNetwork$fraction
lFracForc <- sapply(dfNetwork$param_names, 
                    getForcingAndInterpolate, 
                    location = "excel",
                    pathLoc = fileXls,
                    metadata = dfNetwork,
                    timesteps = seq(0, 365*lDATM_SETTINGS[["run_settings"]]["dReady", "Set0"]),
                    simplify = F)

## Define the substances that will be transported
##  In normal runs the transportable substances should be all substances that are (passively) transportable by water
##  For regular use, this table should not be adjusted. 
##  This data frame contains three columns
##   - loads: the names of the substances when they enter the system as loads
##   - outflow: the names of the substances when they flow out of the system
##   - tran_subst: the names of the substances in the transport formulas
tran_subst <- data.frame(loads = c("uPLoadPO4Epi", "uPLoadAIMEpi", "uPLoadGrenEpi", "uPLoadDiatEpi", "uPLoadBlueEpi", "uPLoadDetEpi", "uPLoadZooEpi",
                                   "uNLoadNH4Epi", "uNLoadNO3Epi", "uNLoadGrenEpi", "uNLoadDiatEpi", "uNLoadBlueEpi", "uNLoadDetEpi", "uNLoadZooEpi",
                                   "uDLoadDetEpi", "uDLoadIMEpi", "uDLoadGrenEpi", "uDLoadDiatEpi", "uDLoadBlueEpi", "uDLoadZooEpi",
                                   "uSiLoadSiO2Epi", "uSiLoadDetEpi",
                                   "uO2LoadEpi",
                                   
                                   "uPLoadPO4Hyp", "uPLoadAIMHyp", "uPLoadGrenHyp", "uPLoadDiatHyp", "uPLoadBlueHyp", "uPLoadDetHyp", "uPLoadZooHyp",
                                   "uNLoadNH4Hyp", "uNLoadNO3Hyp", "uNLoadGrenHyp", "uNLoadDiatHyp", "uNLoadBlueHyp", "uNLoadDetHyp", "uNLoadZooHyp",
                                   "uDLoadDetHyp", "uDLoadIMHyp", "uDLoadGrenHyp", "uDLoadDiatHyp", "uDLoadBlueHyp", "uDLoadZooHyp",
                                   "uSiLoadSiO2Hyp", "uSiLoadDetHyp",
                                   "uO2LoadHyp"),
                         
                         outflow = c("wPDilPO4Epi", "wPDilAIMEpi", "wPDilGrenEpi", "wPDilDiatEpi", "wPDilBlueEpi", "wPDilDetEpi", "wPDilZooEpi",
                                     "wNDilNH4Epi", "wNDilNO3Epi", "wNDilGrenEpi", "wNDilDiatEpi", "wNDilBlueEpi", "wNDilDetEpi", "wNDilZooEpi",
                                     "wDDilDetEpi", "wDDilIMEpi", "wDDilGrenEpi", "wDDilDiatEpi", "wDDilBlueEpi", "wDDilZooEpi",
                                     "wSiDilSiO2Epi", "wSiDilDetEpi",
                                     "wO2OutflEpi",
                                     
                                     "wPDilPO4Hyp", "wPDilAIMHyp", "wPDilGrenHyp", "wPDilDiatHyp", "wPDilBlueHyp", "wPDilDetHyp", "wPDilZooHyp",
                                     "wNDilNH4Hyp", "wNDilNO3Hyp", "wNDilGrenHyp", "wNDilDiatHyp", "wNDilBlueHyp", "wNDilDetHyp", "wNDilZooHyp",
                                     "wDDilDetHyp", "wDDilIMHyp", "wDDilGrenHyp", "wDDilDiatHyp", "wDDilBlueHyp", "wDDilZooHyp",
                                     "wSiDilSiO2Hyp", "wSiDilDetHyp",
                                     "wO2OutflHyp"),
                         
                         tran_subst = c("wPTranPO4WEpi", "wPTranAIMWEpi", "wPTranGrenEpi", "wPTranDiatEpi", "wPTranBlueEpi", "wPTranDetWEpi", "wPTranZooEpi",
                                        "wNTranNH4WEpi", "wNTranNO3WEpi", "wNTranGrenEpi", "wNTranDiatEpi", "wNTranBlueEpi", "wNTranDetWEpi", "wNTranZooEpi",
                                        "wDTranDetWEpi", "wDTranIMWEpi", "wDTranGrenEpi", "wDTranDiatEpi", "wDTranBlueEpi", "wDTranZooEpi",
                                        "wSiTranSiO2Epi", "wSiTranDetWEpi",
                                        "wO2TranWEpi",
                                        
                                        "wPTranPO4WHyp", "wPTranAIMWHyp", "wPTranGrenHyp", "wPTranDiatHyp", "wPTranBlueHyp", "wPTranDetWHyp", "wPTranZooHyp",
                                        "wNTranNH4WHyp", "wNTranNO3WHyp", "wNTranGrenHyp", "wNTranDiatHyp", "wNTranBlueHyp", "wNTranDetWHyp", "wNTranZooHyp",
                                        "wDTranDetWHyp", "wDTranIMWHyp", "wDTranGrenHyp", "wDTranDiatHyp", "wDTranBlueHyp", "wDTranZooHyp",
                                        "wSiTranSiO2Hyp", "wSiTranDetWHyp",
                                        "wO2TranWHyp"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6. Optional: Plotting the network-------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## This section can help you to check if you have defined your network as you intended.

## create an igraph network
network_igraph <- graph.data.frame(d = dfNetwork, 
                                   vertices = dfNodes,
                                   directed = T)

## adjust elements of the network
V(network_igraph)$size <-V(network_igraph)$surface_area * 20
V(network_igraph)$latitude <- dfNodes$lat[match(V(network_igraph)$name, dfNodes$node_nr)]
V(network_igraph)$longitude <- dfNodes$lon[match(V(network_igraph)$name, dfNodes$node_nr)]
location <- matrix(c(V(network_igraph)$latitude, V(network_igraph)$longitude), ncol = 2)
E(network_igraph)$edge.color <- "gray80"
E(network_igraph)$weight  <- E(network_igraph)$fraction * 100

## plot the network
plot(network_igraph, 
     edge.curved = .1,
     vertex.color="lightblue", 
     vertex.frame.color="#ffffff",
     edge.width = E(network_igraph)$fraction * 5,
     #layout=layout.grid,
     layout = location,
     rescale = F, #asp = 0,
     xlim = range(V(network_igraph)$lat), 
     ylim = range(V(network_igraph)$lon),
     vertex.label.color="black",
     edge.color = "#6599FF",
     edge.label = dfNetwork$fraction,
     edge.label.color = "black",
     edge.label.cex = 0.8) # vertex.label=V(network_igraph)$box_name


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.  Make and adjust cpp files + update lDATM -------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PCModelAdjustlDATMandCPPfiles_PCLakeSplus(dirSHELL = dirShell,
                                          nameWORKCASE = nameWorkCase,
                                          dfNODES = dfNodes,
                                          dfNETWORK = dfNetwork,
                                          dfSUBST = tran_subst,
                                          lDATM = lDATM_SETTINGS)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 8.  Compile the model ------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## During compilation the adjusted cpp files will be converted into a dll file. 
## Depending on the size of the model (>15 nodes) this step can require large amounts of computer memory (>15Gb).
PCModelCompileModelWorkCase(dirSHELL = dirShell,
                            nameWORKCASE = nameWorkCase)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 9.  Run the model ----------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Note: Initialization happens inside the function
results <- PCModelNetworkRun(lDATM = lDATM_SETTINGS,
                             dfNETWORK = dfNetwork,
                             lFRACFORC = lFracForc,
                             dirSHELL = dirShell,
                             dfSUBST = tran_subst,
                             integrator_method = "rk45ck",
                             nameWORKCASE = nameWorkCase)

## save the output                  
data.table::fwrite(results$output,
                   file = file.path(dirShell, "work_cases", nameWorkCase, "output", "results.txt"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 10.  Make a plot -----------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Besides this example, you could also take a look at the R shiny app on: https://github.com/LilithKramer/ShinyPCModel
## This interactive app makes a quick look at your output data easier.

## colors for the lakes
colors <- c("N0" = "#CD534CFF",
            "N1" = "#8F7700FF",
            "N2" = "#003C67FF",
            "N3" = "#0073C2FF",
            "N4" = "#7AA6DCFF")

## labels 
variable_names <- c(`oChlaEpi` = "Chlorophyll-a~summer~avg~{}(mg%*%m^-3)",
                    `sDVeg` = "Submerged~macrophyte~summer~avg~{}(gDW%*%m^-2)") 

## convert output from the model into something that is easy to use with ggplot
output_long <- results$output %>%
  dplyr::select(colnames(results$output)[grep("oChlaEpiN|sDVegN|time", colnames(results$output))]) %>%
  pivot_longer(cols = colnames(results$output)[grep("oChlaEpiN|sDVegN", colnames(results$output))],
               names_to = "type", 
               values_to = "values") %>%
  separate(type, into = c("parameter", "node"), -2) 

## plot the data
plot <- ggplot(data = output_long, aes(time, values, col = node)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = colors, name = "Lake") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0.0, NA)) +
  theme_classic() +
  ylab("") +
  xlab("Tot. P load to N0 ("~gP%*%m^-2%*%d^-1~")") + 
  facet_grid("parameter", scales = "free_y", labeller = as_labeller(variable_names, label_parsed)) + 
  theme(strip.background = element_rect(fill = "lightgrey", colour = "lightgrey"))
plot

## save the plot
ggsave(file.path(dirShell, "work_cases", nameWorkCase, "output", "plot.png"),
       width = 7.76, height = 6.51, 
       type = "cairo",
       plot)

