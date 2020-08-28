
## OUTPUT ==== 


## auxil ----


##  water transport derivatives
output_WaterFlowDerivatives <- c("oNH4WHyp","oNO3WHyp",
                                 "oPO4WHyp","oPAIMWHyp",
                                 "SiO2WHyp","oO2WHyp",
                                 "oDDetWHyp","oNDetWHyp","oPDetWHyp","oSiDetWHyp",
                                 "oDIMWHyp",
                                 "oDDiatWHyp","oNDiatWHyp","oPDiatWHyp",
                                 "oDGrenWHyp","oNGrenWHyp","oPGrenWHyp",
                                 "oDBlueWHyp","oNBlueWHyp","oPBlueWHyp",
                                 "oDZooHyp","oNZooHyp","oPZooHyp",
                                 "oNH4WEpi","oNO3WEpi","oPO4WEpi","oPAIMWEpi","oSiO2WEpi","oO2WEpi",
                                 "oDDetWEpi","oNDetWEpi","oPDetWEpi","oSiDetWEpi",
                                 "oDIMWEpi",
                                 "oDDiatWEpi","oNDiatWEpi","oPDiatWEpi",
                                 "oDGrenWEpi","oNGrenWEpi","oPGrenWEpi",
                                 "oDBlueWEpi","oNBlueWEpi","oPBlueWEpi",
                                 "oDZooEpi","oNZooEpi","oPZooEpi")

## water transport states
output_WaterStates <-  c("sNH4WEpi","sNO3WEpi",
                         "sPO4WEpi","sPAIMWEpi",
                         "sSiO2WEpi","sO2WEpi",
                         "sDDetWEpi","sPDetWEpi","sNDetWEpi","sSiDetWEpi",
                         "sDIMWEpi",
                         "sDDiatWEpi","sPDiatWEpi","sNDiatWEpi",
                         "sDGrenWEpi","sPGrenWEpi","sNGrenWEpi",
                         "sDBlueWEpi","sPBlueWEpi","sNBlueWEpi",
                         "sDZooEpi",  "sPZooEpi", "sNZooEpi",
                         "sNH4WHyp","sNO3WHyp",
                         "sPO4WHyp","sPAIMWHyp",
                         "sSiO2WHyp","sO2WHyp",
                         "sDDetWHyp","sPDetWHyp","sNDetWHyp","sSiDetWHyp",
                         "sDIMWHyp",
                         "sDDiatWHyp","sPDiatWHyp","sNDiatWHyp",
                         "sDGrenWHyp","sPGrenWHyp","sNGrenWHyp",
                         "sDBlueWHyp","sPBlueWHyp","sNBlueWHyp",
                         "sDZooHyp","sPZooHyp", "sNZooHyp")

output_Outflow <- c("uQOutEpi",
                    "uQOutHyp")

output_Depth <- c("uDepthWEpi",
                  "uDepthWHyp")

## Outflow rate in d-1, can be used to calculate nutrient outflow
output_OutflowRate <- c("ukOutEpi", "ukOutHyp") ## ukOut = QOut / mmPerm / uDepth, with mmPerm = 1000

## Water balance
output_WaterBalance <- c(
   #"uQInSeason" ,   ## seasonal Q in       
   #"uQEvSinus"  ,   ## evaporation via simple sinus function
   #"aQEv"       ,   ## evaporation via calculation
   "uQEv"       ,   ## total evaporation
   "uQInExtra"  ,   ##
   "uQIn"       ,   ## inflow Q
   "uQOutExtra" ,   ##
   "uQOutEpi"   ,   ## outflow Q   
   "uQOutHyp"   ,   ## = 0.0
   #"uQDilEpi"   ,   ## inflow - evap = uQOutEpi
   #"uQDilHyp"   ,   ## = uQOutHyp
   "uQEvPhra"       ## evap of reed = uQEv
) 

## Water balance extra
output_FlowExtra <- c("aRelDeltaWEpi", "afVolMarshEpi", 
                      "MassEpi", "MassHyp",
                      "aStrat","vTranHypEpiW",
                      "uDepthWEpi", "uDepthWHyp", 
                      "ukOutEpi", "ukOutHyp")


## Full Restart file (all the initial conditions in order of appearance)
output_RestartAll <- c("uDepthW",
                     "oNH4WHyp","oNO3WHyp",
                     "oPO4WHyp","oPAIMWHyp",
                     "oSiO2WHyp","oO2WHyp",
                     "oDDetWHyp","oNDetWHyp","oPDetWHyp","oSiDetWHyp",
                     "oDIMWHyp",
                     "oDDiatWHyp","oNDiatWHyp","oPDiatWHyp",
                     "oDGrenWHyp","oNGrenWHyp","oPGrenWHyp",
                     "oDBlueWHyp","oNBlueWHyp","oPBlueWHyp",
                     "oDZooHyp","oNZooHyp","oPZooHyp",
                     "aDFiAd","aDFiJv","aNFiAd","aNFiJv","aPFiAd","aPFiJv","aDPisc",
                     "aNH4S","aNO3S","aPO4S","aPAIMS",
                     "aDDetS","aNDetS","aPDetS","aSiDetS",
                     "aDHumS","aNHumS","aPHumS","aDIMS",
                     "aDDiatS","aNDiatS","aPDiatS",
                     "aDGrenS","aNGrenS","aPGrenS",
                     "aDBlueS","aNBlueS","aPBlueS",
                     "aDVeg","aNVeg","aPVeg",
                     "aVegHeight",
                     "aDBent","aNBent","aPBent",
                     "uDepthWM",
                     "oNH4WM","oNO3WM","oPO4WM","oPAIMWM","oSiO2WM","oO2WM",
                     "oDDetWM","oNDetWM","oPDetWM","oSiDetWM",
                     "oDIMWM",
                     "oDDiatWM","oNDiatWM","oPDiatWM",
                     "oDGrenWM","oNGrenWM","oPGrenWM",
                     "oDBlueWM","oNBlueWM","oPBlueWM",
                     "oDZooM","oNZooM","oPZooM",
                     "aNH4SM","aNO3SM","aPO4SM","aPAIMSM",
                     "aDDetSM","aNDetSM","aPDetSM","aSiDetSM",
                     "aDHumSM","aNHumSM","aPHumSM",
                     "aDIMSM","aDRootPhra","aDShootPhra","aNRootPhra","aNShootPhra","aPRootPhra","aPShootPhra",
                     "oNH4WEpi","oNO3WEpi","oPO4WEpi","oPAIMWEpi","oSiO2WEpi","oO2WEpi",
                     "oDDetWEpi","oNDetWEpi","oPDetWEpi","oSiDetWEpi",
                     "oDIMWEpi",
                     "oDDiatWEpi","oNDiatWEpi","oPDiatWEpi",
                     "oDGrenWEpi","oNGrenWEpi","oPGrenWEpi",
                     "oDBlueWEpi","oNBlueWEpi","oPBlueWEpi",
                     "oDZooEpi","oNZooEpi","oPZooEpi",
                     "aDExtTotT","aNExtTotT","aPExtTotT","aSiExtTotT","aO2ExtTotT")

## output for mass balance P
output_pMassBalance <- c("tPHarvPisc"     ,"tPHarvFish"     ,"tPAssVegBird"  ,
                         "tPMigrVeg"      ,"tPManVeg"       ,"uPLoad"         ,"wPOutflTotEpi" ,
                         "uDepthWEpi"     ,"uQEv"           ,"wPOutflTotHyp"  ,"uDepthWHyp"    ,
                         "cPBackLoad"     ,"tPMarsTotT"     ,"tPDredNetTot"   ,"tPMigrFiJv"    ,
                         "tPMigrFiAd"     ,"tPMigrPisc "    ,"tPMigrBent"     ,"wPTranZooHyp"  ,
                         "wPTranZooEpi"   ,"tPMortPiscBot"  ," tPMortFiJvBot" ,"tPMortFiAdBot" ,
                         "tPBurHum"       ,"tPBurDet"       ,"tPBurAIM"       ,"tPBurPO4"      ,
                         "uPErosOM"       ,"tPChemPO4"      ,"tPInfPO4S"      ,"tPDifGroundPO4")


## output for N
output_nMassBalance <- c("oNH4WEpi","oNO3WEpi","oNDetWEpi","oNDiatWEpi","oNGrenWEpi","oNBlueWEpi", "oNZooEpi", 
                         "oNTotWEpi", "oNkjWEpi",  
                         
                         "aNFish", "aNPisc", "aNVeg", "aNBent", 
                         "aNTotM", "aNError", 'aNTotT', 'aNExtTotT', "aNRelTotT", "aNTotTHyp", 
                         "aNH4S","aNO3S","aNDetS","aNHumS", "aNDiatS","aNGrenS","aNBlueS",
                         
                         "uNLoad", "uNLoadNO3", "uNLoadSeason", "uNLoadPhytTot",       
                         "uNLoadDet", "uNLoadOrg", "uNLoadDiss", 
                         "uNLoadNH4", "uNLoadDiat", "uNLoadGren", " uNLoadBlue",  
                         
                         "wNOutflTotEpi", "wNOutflTotHyp", "wNTranZooHyp",  "wNTranZooEpi", "wNTranNO3WEpi", "wNAbioNO3WEpi","wNPrimNO3WEpi",
                         "wNExchNO3WEpi", "wNAdvNO3W", "wNAdvNO3WM", "wNBedNO3WEpi" , "wNWebNO3WEpi" , 
                         "wNDilNo3Epi", "wNMinDetWEpi", "wNNitrWEpi",
                         
                         "tNBurTot", "tNAbioTotT", "tNPrimTotT", "tNBedTotT","tNWebTotT","tNMarsTotT","tNDredNetTot",
                         "tNInfNH4WEpi", "tNDifNH4Epi",
                         
                         "dNO3WEpi")




## FORCINGS ----

## forcings waterstates
forcings_WaterStates <-  c("mNLoadNH4Epi","mNLoadNO3Epi",
                           "mPLoadPO4Epi","mPLoadAIMEpi",
                           "mSiO2LoadEpi","mO2LoadEpi",
                           "mDLoadDetEpi","mPLoadDetEpi","mNLoadDetEpi","mSiLoadDetEpi",
                           "mDLoadIMEpi",
                           "mDLoadDiatEpi","mPLoadDiatEpi","mNLoadDiatEpi",
                           "mDLoadGrenEpi","mPLoadGrenEpi","mNLoadGrenEpi",
                           "mDLoadBlueEpi","mPLoadBlueEpi","mNLoadBlueEpi",
                           "mDLoadZooEpi", "mPLoadZooEpi", "mNLoadZooEpi",
                           "mNLoadNH4Hyp","mNLoadNO3Hyp",
                           "mPLoadPO4Hyp","mPLoadAIMHyp",
                           "mSiO2LoadHyp","mO2LoadHyp",
                           "mDLoadDetHyp","mPLoadDetHyp","mNLoadDetHyp","mSiLoadDetHyp",
                           "mDLoadIMHyp",
                           "mDLoadDiatHyp","mPLoadDiatHyp","mNLoadDiatHyp",
                           "mDLoadGrenHyp","mPLoadGrenHyp","mNLoadGrenHyp",
                           "mDLoadBlueHyp","mPLoadBlueHyp","mNLoadBlueHyp",
                           "mDLoadZooHyp", "mPLoadZooHyp", "mNLoadZooHyp")

forcings_Inflow  <- c("mQInEpi",
                     "mQInHyp")


## READ ----

read_forcings_WaterStates <-  c("ReadNLoadNH4Epi","ReadNLoadNO3Epi",
                                "ReadPLoadPO4Epi","ReadPLoadAIMEpi",
                                "ReadSiO2LoadEpi","ReadO2LoadEpi",
                                "ReadDLoadDetEpi","ReadPLoadDetEpi","ReadNLoadDetEpi","ReadSiLoadDetEpi",
                                "ReadDLoadIMEpi",
                                "ReadDLoadDiatEpi","ReadPLoadDiatEpi","ReadNLoadDiatEpi",
                                "ReadDLoadGrenEpi","ReadPLoadGrenEpi","ReadNLoadGrenEpi",
                                "ReadDLoadBlueEpi","ReadPLoadBlueEpi","ReadNLoadBlueEpi",
                                "ReadDLoadZooEpi", "ReadPLoadZooEpi", "ReadNLoadZooEpi",
                                "ReadNLoadNH4Hyp", "ReadNLoadNO3Hyp",
                                "ReadPLoadPO4Hyp", "ReadPLoadAIMHyp",
                                "ReadSiO2LoadHyp", "ReadO2LoadHyp",
                                "ReadDLoadDetHyp", "ReadPLoadDetHyp","ReadNLoadDetHyp","ReadSiLoadDetHyp",
                                "ReadDLoadIMHyp",
                                "ReadDLoadDiatHyp","ReadPLoadDiatHyp","ReadNLoadDiatHyp",
                                "ReadDLoadGrenHyp","ReadPLoadGrenHyp","ReadNLoadGrenHyp",
                                "ReadDLoadBlueHyp","ReadPLoadBlueHyp","ReadNLoadBlueHyp",
                                "ReadDLoadZooHyp", "ReadPLoadZooHyp", "ReadNLoadZooHyp")

read_forcings_Inflow  <- c("ReadQInEpi",
                           "ReadQInHyp")

