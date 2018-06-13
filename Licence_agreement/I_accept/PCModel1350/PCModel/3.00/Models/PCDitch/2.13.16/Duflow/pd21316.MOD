/* ########################################### */
/* ------------------------------------------- */
/* pcditch version pd21316 */
/* ------------------------------------------- */
/* ########################################### */

/* ############################ */
/* State variables              */
/* ############################ */

BOTTOM  sDDetS     [ 363.4059807 ]                         gDW/m2 ;    Detritus DW in ditch sediment
WATER   sDDetW     [ 2 ]                                   gDW/m3 ;    Detritus DW in ditch water
BOTTOM  sDHumS     [ 3270.653827 ]                         gDW/m2 ;    Humus DW in ditch sediment
BOTTOM  sDIMS      [ 32706.53827 ]                         gDW/m2 ;    Inorganic matter in ditch sediment
WATER   sDIMW      [ 5 ]                                   gDW/m3 ;    Inorganic matter in ditch water
BOTTOM  sNDetS     [ 9.085149518 ]                         gN/m2 ;     Detritus N in ditch sediment
WATER   sNDetW     [ 0.05 ]                                gN/m3 ;     Detritus N in ditch water
BOTTOM  sNH4S      [ 0.02 ]                                gN/m2 ;     N in NH4 in ditch sediment interstitial water
WATER   sNH4W      [ 0.1 ]                                 gN/m3 ;     N in NH4 in ditch water
BOTTOM  sNHumS     [ 163.5326913 ]                         gN/m2 ;     Humus N in ditch sediment
BOTTOM  sNO3S      [ 0.002 ]                               gN/m2 ;     N in NO3 in ditch sediment interstitial water
WATER   sNO3W      [ 0.1 ]                                 gN/m3 ;     N in NO3 in ditch water
WATER   sO2W       [ 10 ]                                  gO2/m3 ;    Oxygen in ditch water
BOTTOM  sPAIMS     [ 17.98859605 ]                         gP/m2 ;     P adsorbed onto IM in ditch sediment
WATER   sPAIMW     [ 0 ]                                   gP/m3 ;     P adsorbed onto IM in ditch water
BOTTOM  sPDetS     [ 0.908514952 ]                         gP/m2 ;     Detritus P in ditch sediment
WATER   sPDetW     [ 0.005 ]                               gP/m3 ;     Detritus P in ditch water
BOTTOM  sPHumS     [ 16.35326913 ]                         gP/m2 ;     Humus P in ditch sediment
BOTTOM  sPO4S      [ 0.18170299 ]                          gP/m2 ;     P in PO4 in ditch sediment interstitial water
WATER   sPO4W      [ 0.01 ]                                gP/m3 ;     P in PO4 in ditch water
WATER   sDPhytW    [ 1 ]                                   gDW/m3 ;    DW of Phytoplankton
WATER   sPPhytW    [ 0.01 ]                                gP/m3 ;     P in Phytoplankton
WATER   sNPhytW    [ 0.1 ]                                 gN/m3 ;     N in Phytoplankton
BOTTOM  sDPhytS    [ 0.001 ]                               gDW/m2 ;    DW of settled phytoplankton
BOTTOM  sPPhytS    [ 0.00001 ]                             gP/m2 ;     P in settled phytoplankton
BOTTOM  sNPhytS    [ 0.0001 ]                              gN/m2 ;     N in settled phytoplankton
BOTTOM  sDElod     [ 1 ]                                   gDW/m2 ;    DW of Elodeids
BOTTOM  sDChar     [ 1 ]                                   gDW/m2 ;    DW of Characeans
BOTTOM  sDCera     [ 1 ]                                   gDW/m2 ;    DW of Ceratophyllids
BOTTOM  sDLemn     [ 1 ]                                   gDW/m2 ;    DW of Lemnacaea
BOTTOM  sDNymp     [ 1 ]                                   gDW/m2 ;    DW of Nymphaeids
BOTTOM  sDHelo     [ 1 ]                                   gDW/m2 ;    DW of helophytes
BOTTOM  sPElod     [ 0.002 ]                               gP/m2 ;     P in Elodeids
BOTTOM  sPChar     [ 0.002 ]                               gP/m2 ;     P in Characeans
BOTTOM  sPCera     [ 0.002 ]                               gP/m2 ;     P in Ceratophyllids
BOTTOM  sPLemn     [ 0.005 ]                               gP/m2 ;     P in Lemnacaea
BOTTOM  sPNymp     [ 0.002 ]                               gP/m2 ;     P in Nymphaeids
BOTTOM  sPHelo     [ 0.002 ]                               gP/m2 ;     P in helophytes
BOTTOM  sNElod     [ 0.02 ]                                gN/m2 ;     N in Elodeids
BOTTOM  sNChar     [ 0.02 ]                                gN/m2 ;     N in Characeans
BOTTOM  sNCera     [ 0.02 ]                                gN/m2 ;     N in Ceratophyllids
BOTTOM  sNLemn     [ 0.05 ]                                gN/m2 ;     N in Lemnacaea
BOTTOM  sNNymp     [ 0.02 ]                                gN/m2 ;     N in Nymphaeids
BOTTOM  sNHelo     [ 0.02 ]                                gN/m2 ;     N in helophytes
BOTTOM  sDExtTotT  [ 0 ]                                   gDW/m2 ;    Total amount of DW moved into or out from the system
BOTTOM  sNExtTotT  [ 0 ]                                   gN/m2 ;     Total amount of N moved into or out from the system
BOTTOM  sPExtTotT  [ 0 ]                                   gP/m2 ;     Total amount of P moved into or out from the system
BOTTOM  time       [ 0.0 ]                                 day ;       day of the year

/* ############################ */
/* Parameters                   */
/* ############################ */

PARM   BeginTime         [  0  ]                                   day ;                       begintime
PARM   ConstDepth        [  1  ]                                   - ;                         if 1 water depth kept constant by daily dredging
PARM   ReadTemp          [  0  ]                                   - ;                         if 1 use measured time-series of temperature otherwise sinus
PARM   ReadLOut          [  0  ]                                   - ;                         if 1 use measured time-series of light otherwise sinus
PARM   ReadVWind         [  0  ]                                   - ;                         if 1 use measured time-series of wind speed otherwise constant
PARM   InitCalc          [  0  ]                                   - ;                         if 1 calculate initial values
PARM   InclTran          [  0  ]                                   - ;                         if 1 use transportmodule (in Duflow transport module can not be used
PARM   fDTotS0           [  0.3  ]                                 gDW/g_sediment ;            initial dry-weight fraction in sediment
PARM   fDOrgS0           [  0.1  ]                                 gOM/gDW ;                   initial organic fraction of sediment DW
PARM   fDDetS0           [  0.1  ]                                 dDet/gOM ;                  initial detritus fraction of sediment organic matter
PARM   fPInorgS0         [  0.0005  ]                              gP/gDW ;                    initial inorganic P fraction in sediment
PARM   fPAdsS0           [  0.99  ]                                gP_Ads/gP_IM ;              initial adsorbed fraction of inorganic P in sediment
PARM   cPDDet0           [  0.0025  ]                              gP/gDW ;                    initial P fraction in detritus
PARM   cNDDet0           [  0.025  ]                               gN/gDW ;                    initial N fraction in detritus
PARM   cPDHum0           [  0.005  ]                               gP/gDW ;                    initial P fraction in humus
PARM   cNDHum0           [  0.05  ]                                gN/gDW ;                    initial N fraction in humus
PARM   cPDPhyt0          [  0.01  ]                                gP/gDW ;                    initial P fraction in phytoplankton
PARM   cNDPhyt0          [  0.1  ]                                 gN/gDW ;                    initial N fraction in phytoplankton
PARM   cPDElod0          [  0.002  ]                               gP/gDW ;                    initial P fraction in Elodeids
PARM   cNDElod0          [  0.02  ]                                gN/gDW ;                    initial N fraction in Elodeids
PARM   cPDChar0          [  0.002  ]                               gP/gDW ;                    initial P fraction in Characeans
PARM   cNDChar0          [  0.02  ]                                gN/gDW ;                    initial N fraction in Characeans
PARM   cPDCera0          [  0.002  ]                               gP/gDW ;                    initial P fraction in Ceratophyllids
PARM   cNDCera0          [  0.02  ]                                gN/gDW ;                    initial N fraction in Ceraphyllids
PARM   cPDLemn0          [  0.005  ]                               gP/gDW ;                    initial P fraction in Lemnacaea
PARM   cNDLemn0          [  0.05  ]                                gN/gDW ;                    initial N fraction in Lemnacaea
PARM   cPDNymp0          [  0.002  ]                               gP/gDW ;                    initial P fraction in Nymphaeids
PARM   cNDNymp0          [  0.02  ]                                gN/gDW ;                    initial N fraction in Nymphaeids
PARM   cPDHelo0          [  0.002  ]                               gP/gDW ;                    initial P fraction in helophytes
PARM   cNDHelo0          [  0.02  ]                                gN/gDW ;                    initial N fraction in helophytes
PARM   cQInf             [  0  ]                                   mm/day ;                    infiltration rate
PARM   cPBackLoad        [  0  ]                                   gP/m2/day ;                 Background PO4 loading
PARM   cNBackLoad        [  0  ]                                   gP/m2/day ;                 Background NH4 loading
PARM   cNLoadS           [  0  ]                                   gN/m2/day ;                 N fertilizer to sediment
PARM   fNH4LoadS         [  0.5  ]                                 - ;                         NH4 fraction of N fertilizer to sediment
PARM   cPO4Ground        [  0.1  ]                                 gP/m3 ;                     PO4 concentration in groundwater
PARM   cNH4Ground        [  1  ]                                   gN/m3 ;                     NH4 concentration in groundwater
PARM   cNO3Ground        [  0.1  ]                                 gN/m3 ;                     NO3 concentration in groundwater
PARM   cDredInterval     [  9999000  ]                             year ;                      dredging interval
PARM   cDredStart        [  9999000  ]                             year ;                      first dredging year (should be n times cDredInterval )
PARM   cDepthRef         [  0.0000000000000000000000000001  ]      m ;                         reference water depth for dredging
PARM   cLengDred         [  10  ]                                  day ;                       length of dredging period
PARM   fEffDred          [  0.95  ]                                - ;                         dredging efficiency (<10)
PARM   fEffDredLemn      [  0.5  ]                                 - ;                         dredging efficiency for duckweed (<10)
PARM   cFetch            [  0  ]                                   m ;                         wind fetch
PARM   cTmAve            [  12  ]                                  oC ;                        average water temperature
PARM   cTmVar            [  10  ]                                  oC ;                        annual temperature variation
PARM   cTimeLag          [  40  ]                                  day ;                       time lag for temperature
PARM   cVWind            [  5  ]                                   m/s ;                       average wind speed needed to calculate reaeration
PARM   cLDayAve          [  10000000  ]                            J/m2/day ;                  annual average radiation
PARM   cLDayVar          [  8000000  ]                             J/m2/day ;                  annual variation in radiation
PARM   cfDayAve          [  0.5  ]                                 - ;                         average day length
PARM   cfDayVar          [  0.2  ]                                 - ;                         annual variation in day length
PARM   fRefl             [  0.2  ]                                 - ;                         the fraction photosynthetically active radiation reflected at the surface
PARM   fPAR              [  0.48  ]                                - ;                         fraction photosynthetically active radiation (PAR)
PARM   cDayManVeg1       [ -9999000  ]                            day ;                       first mowing day (default non-existent)
PARM   cDayManVeg2       [ -9999000  ]                            day ;                       second mowing day ( 259 = 16 Sep)
PARM   fManVeg           [  0.8  ]                                 - ;                         fraction removed by management for submerged plants
PARM   fManLemn          [  0.4  ]                                 - ;                         fraction of Lemnacae removed by management
PARM   fManHelo          [  0.8  ]                                 - ;                         fraction of helophytes and Nymphaeids removed by management
PARM   cLengMan          [  10  ]                                  day ;                       length of mowing period
PARM   cYearStartBirds   [  0  ]                                   year ;                      first year of birds presence
PARM   cDayStartBirds    [  46  ]                                  day ;                       yearly first day of birds presence
PARM   cDayEndBirds      [  288  ]                                 day ;                       yearly last day of birds presence
PARM   cBirdsPerha       [  0  ]                                   n/ha ;                      number of birds per ha
PARM   cDGrazPerBird     [  45  ]                                  gDW/coot/day ;              daily grazing of birds
PARM   hDVegBird         [  5  ]                                   gDW/m2 ;                    half-sat vegetation biomass for birds grazing
PARM   fDAssBird         [  0.5  ]                                 - ;                         birds assim efficiency
PARM   fDissEgesBird     [  0.25  ]                                - ;                         fraction dissolved nutrient of coot egestion
PARM   cDErosTot         [  10  ]                                  gDW/m2/day ;                DW input by erosion
PARM   cExtWat           [  0.5  ]                                 m-1 ;                       background extinction
PARM   cExtSpDet         [  0.15  ]                                m2/gDW ;                    specific extinction detritus
PARM   cExtSpIM          [  0.05  ]                                m2/gDW ;                    specific extinction inert matter
PARM   cExtSpPhyt        [  0.25  ]                                m2/gDW ;                    specific extinction phytoplankton
PARM   fDOrgSoil         [  0.1  ]                                 - ;                         fraction soil organic matter
PARM   cPDSoilOM         [  0.001  ]                               gP/gDW ;                    P/DW ratio of soil organic matter
PARM   cNDSoilOM         [  0.01  ]                                gN/gDW ;                    N/DW ratio of soil organic matter
PARM   cDepthS           [  0.1  ]                                 m ;                         sediment depth
PARM   fLutum            [  0.1  ]                                 - ;                         lutum content of inorganic matter
PARM   fFeDIM            [  0.01  ]                                gFe/gDW ;                   Fe content of inorganic matter
PARM   fAlDIM            [  0.01  ]                                gAl/gDW ;                   Al content of inorganic matter
PARM   cCPerDW           [  0.4  ]                                 gC/gDW ;                    C content of organic matter
PARM   cRhoIM            [  2500000  ]                             g/m3_solid ;                density of sediment inorganic matter
PARM   cRhoOM            [  1400000  ]                             g/m3 ;                      density of sediment detritus
PARM   cAerRoot          [  0.727  ]                               - ;                         reaeration coefficient for sqrt(VWind)
PARM   cAerLin           [ -0.371  ]                              s/day ;                     reaeration coefficient for Vwind
PARM   cAerSquare        [  0.0376  ]                              - ;                         reaeration coefficient for Vwind^2
PARM   cThetaAer         [  1.024  ]                               1/e^oC ;                    temperature coeff for reaeration (Downing & Truesdale 1955)
PARM   kLemnAer          [  0.01  ]                                m2/gDW ;                    Reaeration reduction coeff for duckweed
PARM   fSedErosIM        [  0.95  ]                                - ;                         instantly sedimentating fraction of inorganic matter
PARM   cVSetIM           [  1  ]                                   m/day ;                     maximum sedimentation velocity of inert organic matter
PARM   cVSetDet          [  0.25  ]                                m/day ;                     maximum sedimentation velocity of detritus
PARM   cThetaSet         [  1.01  ]                                1/e^oC ;                    temperature parameter of sedimentation
PARM   cSuspRef          [  0.5  ]                                 - ;                         reference suspended matter function
PARM   cSuspMin          [  6.1  ]                                 - ;                         minimum value of logistic empirical suspended matter function
PARM   cSuspMax          [  25.2  ]                                - ;                         maximum value of logistic empirical suspended matter function
PARM   cSuspSlope        [  2.1  ]                                 - ;                         slope of logistic empirical suspended matter function
PARM   hDepthSusp        [  2  ]                                   - ;                         half sat value of depth in logistic empirical suspended matter function
PARM   cFetchRef         [  1000  ]                                m ;                         reference fetch
PARM   fLutumRef         [  0.2  ]                                 - ;                         reference lutum fraction of sediment
PARM   kVegResus         [  0.01  ]                                m2/gDW ;                    resuspension reduction per g vegetation
PARM   kResusPhytMax     [  0.25  ]                                day-1 ;                     maximum resuspension of phytoplankton
PARM   cResusPhytExp     [ -0.379  ]                              (gDW/m2/day)-1 ;            coefficient for phytoplankton resuspension
PARM   kPDifPO4          [  0.000072  ]                            m2/day ;                    mol PO4 diffusion constant
PARM   kNDifNO3          [  0.000086  ]                            m2/day ;                    mol NO3 diffusion constant
PARM   kNDifNH4          [  0.000112  ]                            m2/day ;                    mol NH4 diffusion constant
PARM   kO2Dif            [  0.000026  ]                            m2/day ;                    mol O2 diffusion constant
PARM   cThetaDif         [  1.02  ]                                1/eoC ;                     Temperature coefficient for diffusion
PARM   fDepthDifS        [  0.5  ]                                 - ;                         nutrient diffusion distance as fraction of sediment depth
PARM   cTurbDifNut       [  5  ]                                   - ;                         bioturbation factor for diffusion
PARM   cTurbDifO2        [  5  ]                                   - ;                         bioturbation factor for diffusion
PARM   kPSorp            [  0.05  ]                                day-1 ;                     P sorption rate constant (not too high -> model speed)
PARM   cRelPAdsD         [  0.00003  ]                             gP/gDW ;                    maximum P adsorption per g DW
PARM   cRelPAdsFe        [  0.065  ]                               gP/gFe ;                    maximum P adsorption per g Fe
PARM   cRelPAdsAl        [  0.134  ]                               gP/gAl ;                    maximum P adsorption per g Al
PARM   cKPAdsOx          [  0.6  ]                                 m3/gP ;                     P adsorption affinity at oxidized conditions
PARM   fRedMax           [  0.9  ]                                 - ;                         maximum reduction factor of P adsorption affinity
PARM   coPO4Max          [  2  ]                                   gP/m3 ;                     maximum SRP concentration in pore water
PARM   kPChemPO4         [  0.03  ]                                day-1 ;                     chemical PO4 loss rate by precipitation
PARM   cTmRef            [  20  ]                                  oC ;                        reference temperature
PARM   fRefrDetS         [  0.15  ]                                - ;                         refractory fraction of sediment detritus
PARM   cThetaMinW        [  1.07  ]                                - ;                         exponential temperature constant of mineralization in water
PARM   kDMinDetW         [  0.01  ]                                day-1 ;                     decomposition constant of detritus
PARM   hO2BOD            [  1  ]                                   gO2/m3 ;                    half-sat oxygen conc for BOD
PARM   cThetaMinS        [  1.07  ]                                - ;                         exponential temperature constant of sediment mineralization
PARM   kDMinDetS         [  0.002  ]                               day-1 ;                     decomposition constant of sediment detritus
PARM   kDMinHum          [  0.00001  ]                             day-1 ;                     maximum decomposition constant of humic material
PARM   hNO3Denit         [  2  ]                                   gN/m3 ;                     quadratic half-sat NO3 concentration for denitrification
PARM   NO3PerC           [  0.8  ]                                 - ;                         mol NO3 denitrified per mol C mineralised
PARM   kNitrW            [  0.1  ]                                 day-1 ;                     nitrification rate constant in water
PARM   kNitrS            [  1  ]                                   day-1 ;                     nitrification rate constant in sediment
PARM   cThetaNitr        [  1.08  ]                                1/eoC ;                     temperature coefficient of nitrification
PARM   O2PerNH4          [  2  ]                                   - ;                         mol O2 used per mol NH4+ nitrified
PARM   hO2Nitr           [  2  ]                                   gO2/m3 ;                    half-sat O2 concentration for nitrification in water
PARM   fDissMortVeg      [  0.25  ]                                - ;                         fraction dissolved nutrients from died plants
PARM   cLengAllo         [  15  ]                                  day ;                       duration of allocation and reallocation phase
PARM   cLengMort         [  15  ]                                  day ;                       duration of autumn mortality period
PARM   fRootElodSum      [  0.1  ]                                 gDW_Root/gDW ;              root fraction in growing season
PARM   fRootElodWin      [  0.6  ]                                 gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatElod        [  0  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergElod        [  0  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Elod       [  0  ]                                   - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Elod       [  1  ]                                   - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerElod       [  0  ]                                   gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpElod        [  0.5  ]                                 %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   kMigrElod         [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDElodIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitElod       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrElod        [  500  ]                                 gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxElod        [  0.32  ]                                gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdElod      [  1.2  ]                                 - ;                         temperature quotient of production
PARM   hLRefElod         [  32  ]                                  W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpElod        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespElod        [  0.024  ]                               day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespElod      [  1.5  ]                                 - ;                         temperature quotient of respiration
PARM   cDayWinElod       [  259  ]                                 day ;                       end of growing season (259 = 16 Sep)
PARM   kMortElodSum      [  0.005  ]                               day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinElod          [  0.2  ]                                 - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortElod     [  0.5  ]                                 - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefElodBird     [  1  ]                                   - ;                         edibility for birds
PARM   cVPUptMaxElod     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptElod      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDElodMin        [  0.0008  ]                              gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDElodMax        [  0.0035  ]                              gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxElod     [  0.1  ]                                 gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptElod      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDElodMin        [  0.01  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDElodMax        [  0.035  ]                               gN/gDW ;                    maximum N/D ratio vegetation
PARM   fRootCharSum      [  0.05  ]                                gDW_Root/gDW ;              root fraction in growing season
PARM   fRootCharWin      [  0.1  ]                                 gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatChar        [  0  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergChar        [  0  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Char       [  0.5  ]                                 - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Char       [  1  ]                                   - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerChar       [  0  ]                                   gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpChar        [  0.5  ]                                 %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   kMigrChar         [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDCharIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitChar       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrChar        [  500  ]                                 gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxChar        [  0.22  ]                                gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdChar      [  1.2  ]                                 - ;                         temperature quotient of production
PARM   hLRefChar         [  19  ]                                  W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpChar        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespChar        [  0.025  ]                               day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespChar      [  1.2  ]                                 - ;                         temperature quotient of respiration
PARM   cDayWinChar       [  259  ]                                 day ;                       end of growing season (259 = 16 Sep)
PARM   kMortCharSum      [  0.005  ]                               day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinChar          [  0.9  ]                                 - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortChar     [  0.5  ]                                 - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefCharBird     [  0.5  ]                                 - ;                         edibility for birds
PARM   cVPUptMaxChar     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptChar      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDCharMin        [  0.0012  ]                              gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDCharMax        [  0.0035  ]                              gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxChar     [  0.1  ]                                 gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptChar      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDCharMin        [  0.01  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDCharMax        [  0.035  ]                               gN/gDW ;                    maximum N/D ratio vegetation
PARM   fRootCeraSum      [  0  ]                                   gDW_Root/gDW ;              root fraction in growing season
PARM   fRootCeraWin      [  0  ]                                   gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatCera        [  0  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergCera        [  0  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Cera       [  0  ]                                   - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Cera       [  0.5  ]                                 - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerCera       [  0  ]                                   gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpCera        [  0.5  ]                                 %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   kMigrCera         [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDCeraIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitCera       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrCera        [  500  ]                                 gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxCera        [  0.21  ]                                gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdCera      [  1.5  ]                                 - ;                         temperature quotient of production
PARM   hLRefCera         [  25  ]                                  W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpCera        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespCera        [  0.024  ]                               day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespCera      [  2  ]                                   - ;                         temperature quotient of respiration
PARM   cDayWinCera       [  259  ]                                 day ;                       end of growing season (259 = 16 Sep)
PARM   kMortCeraSum      [  0.005  ]                               day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinCera          [  0.1  ]                                 - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortCera     [  0.5  ]                                 - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefCeraBird     [  0  ]                                   - ;                         edibility for birds
PARM   cVPUptMaxCera     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptCera      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDCeraMin        [  0.0012  ]                              gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDCeraMax        [  0.0035  ]                              gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxCera     [  0.1  ]                                 gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptCera      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDCeraMin        [  0.01  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDCeraMax        [  0.033  ]                               gN/gDW ;                    maximum N/D ratio vegetation
PARM   fObstrLemn        [  1  ]                                   - ;                         obstructed fraction of Lemnacaea outflow
PARM   fRootLemnSum      [  0  ]                                   gDW_Root/gDW ;              root fraction in growing season
PARM   fRootLemnWin      [  0  ]                                   gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatLemn        [  1  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergLemn        [  0  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Lemn       [  0  ]                                   - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Lemn       [  0  ]                                   - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerLemn       [  100  ]                                 gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpLemn        [  1  ]                                   %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   ckMigrLemn        [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDLemnIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitLemn       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrLemn        [  575  ]                                 gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxLemn        [  0.4  ]                                 gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdLemn      [  2.5  ]                                 - ;                         temperature quotient of production
PARM   hLRefLemn         [  7.5  ]                                 W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpLemn        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespLemn        [  0.03  ]                                day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespLemn      [  3  ]                                   - ;                         temperature quotient of respiration
PARM   cDayWinLemn       [  289  ]                                 day ;                       end of growing season (289 = 16 Oct)
PARM   kMortLemnSum      [  0.02  ]                                day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinLemn          [  0.2  ]                                 - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortLemn     [  0.8  ]                                 - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefLemnBird     [  0  ]                                   - ;                         edibility for birds
PARM   cVPUptMaxLemn     [  0.005  ]                               gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptLemn      [  0.02  ]                                m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDLemnMin        [  0.004  ]                               gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDLemnMax        [  0.026  ]                               gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxLemn     [  0.05  ]                                gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptLemn      [  0.02  ]                                m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDLemnMin        [  0.04  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDLemnMax        [  0.1  ]                                 gN/gDW ;                    maximum N/D ratio vegetation
PARM   fRootNympSum      [  0.75  ]                                gDW_Root/gDW ;              root fraction in growing season
PARM   fRootNympWin      [  0.95  ]                                gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatNymp        [  1  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergNymp        [  0  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Nymp       [  0  ]                                   - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Nymp       [  0  ]                                   - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerNymp       [  100  ]                                 gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpNymp        [  0.5  ]                                 %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   kMigrNymp         [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDNympIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitNymp       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrNymp        [  500  ]                                 gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxNymp        [  0.1  ]                                 gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdNymp      [  1.5  ]                                 - ;                         temperature quotient of production
PARM   hLRefNymp         [  25  ]                                  W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpNymp        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespNymp        [  0.01  ]                                day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespNymp      [  2  ]                                   - ;                         temperature quotient of respiration
PARM   cDayWinNymp       [  259  ]                                 day ;                       end of growing season (259 = 16 Sep)
PARM   kMortNympSum      [  0.005  ]                               day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinNymp          [  0.333  ]                               - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortNymp     [  0.25  ]                                - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefNympBird     [  0  ]                                   - ;                         edibility for birds
PARM   cVPUptMaxNymp     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptNymp      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDNympMin        [  0.001  ]                               gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDNympMax        [  0.0075  ]                              gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxNymp     [  0.1  ]                                 gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptNymp      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDNympMin        [  0.01  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDNympMax        [  0.03  ]                                gN/gDW ;                    maximum N/D ratio vegetation
PARM   fRootHeloSum      [  0.5  ]                                 gDW_Root/gDW ;              root fraction in growing season
PARM   fRootHeloWin      [  0.8  ]                                 gDW_Root/gDW ;              root fraction outside growing season
PARM   fFloatHelo        [  0  ]                                   gDW_Floating/gDW_Shoot ;    floating fraction of shoot
PARM   fEmergHelo        [  1  ]                                   gDW_Emergent/gDW_Shoot ;    emergent fraction of shoot
PARM   fDepth1Helo       [  0  ]                                   - ;                         maximum upper depth of submerged vegetation layer as fraction of water depth
PARM   fDepth2Helo       [  0  ]                                   - ;                         maximum lower depth of submerged vegetation layer as fraction of water depth
PARM   cDLayerHelo       [  0  ]                                   gDW/m2 ;                    biomass of a single layer floating leaves
PARM   cCovSpHelo        [  0.05  ]                                %_cover_per_gDW_Shoot/m2 ;  specific cover
PARM   kMigrHelo         [  0.00001  ]                             day-1 ;                     vegetation migration rate
PARM   cDHeloIn          [  0.1  ]                                 gDW/m2 ;                    external vegetation density
PARM   cTmInitHelo       [  10  ]                                  oC ;                        temperature for start of growing season
PARM   cDCarrHelo        [  2000  ]                                gDW/m2 ;                    maximum vegetation biomass
PARM   cMuMaxHelo        [  0.1  ]                                 gDW/gDW_Shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdHelo      [  1.5  ]                                 - ;                         temperature quotient of production
PARM   hLRefHelo         [  25  ]                                  W/m2_PAR ;                  half-sat light intensity at 20 oC
PARM   cExtSpHelo        [  0.01  ]                                m2/gDW ;                    specific ligth extinction
PARM   kDRespHelo        [  0.01  ]                                day-1 ;                     dark respiration rate of vegetation
PARM   cQ10RespHelo      [  2  ]                                   - ;                         temperature quotient of respiration
PARM   cDayWinHelo       [  259  ]                                 day ;                       end of growing season (259 = 16 Sep)
PARM   kMortHeloSum      [  0.005  ]                               day-1 ;                     vegetation mortality rate in Spring and Summer (low)
PARM   fWinHelo          [  0.333  ]                               - ;                         fraction of vegetation DW surviving in winter
PARM   fDetWMortHelo     [  0.25  ]                                - ;                         fraction of shoot mortality becoming water detritus
PARM   cPrefHeloBird     [  0  ]                                   - ;                         edibility for birds
PARM   cVPUptMaxHelo     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of vegetation at 20 oC
PARM   cAffPUptHelo      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity vegetation
PARM   cPDHeloMin        [  0.001  ]                               gP/gDW ;                    minimum P/D ratio vegetation
PARM   cPDHeloMax        [  0.0075  ]                              gP/gDW ;                    maximum P/D ratio vegetation
PARM   cVNUptMaxHelo     [  0.1  ]                                 gN/gDW/day ;                maximum N uptake capacity of vegetation at 20oC
PARM   cAffNUptHelo      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity vegetation
PARM   cNDHeloMin        [  0.01  ]                                gN/gDW ;                    minimum N/D ratio vegetation
PARM   cNDHeloMax        [  0.03  ]                                gN/gDW ;                    maximum N/D ratio vegetation
PARM   cPACoefMin        [  1.5  ]                                 - ;                         minimum Poole-Atkins coefficient
PARM   cPACoefMax        [  2.5  ]                                 - ;                         maximum Poole-Atkins coefficient
PARM   hPACoef           [  3  ]                                   g/m2 ;                      decrease constant for PA coefficient with DOMW
PARM   cSecchiPlus       [  0  ]                                   m ;                         maximum Secchi depth above water depth
PARM   cEuph             [  1.7  ]                                 - ;                         constant to convert Secchi depth to euphotic depth
PARM   cCovSpPhyt        [  2  ]                                   %/gDW/m2 ;                  specific coverage
PARM   cTmOptLoss        [  25  ]                                  oC ;                        optimum tempearture for grazing
PARM   cSigTmLoss        [  13  ]                                  oC ;                        temperature constant of grazing(sigma in Gaussian curve)
PARM   fDissMortPhyt     [  0.2  ]                                 - ;                         soluble nutrient fraction of died phytoplankton
PARM   fDissLoss         [  0.25  ]                                - ;                         dissolved nutrient fraction of grazing loss
PARM   cMuMaxPhyt        [  1.9  ]                                 day-1 ;                     maximum growth rate phytoplankton
PARM   cTmOptPhyt        [  25  ]                                  oC ;                        optimum temperature of phytoplankton
PARM   cSigTmPhyt        [  15  ]                                  oC ;                        temperature constant phytoplankton(sigma in Gaussian curve)
PARM   UseSteelePhyt     [  0  ]                                   - ;                         if 1 use Steele function (light inhibition) if use Lehman function (no light inhibition)
PARM   hLRefPhyt         [  10.2  ]                                W/m2 ;                      half-sat PAR for phytoplankton at 20 oC(Lehman function)
PARM   cLOptRefPhyt      [  1000  ]                                W/m2 ;                      optimum PAR at 20 oC (Steele function)
PARM   cChDPhytMin       [  0.01  ]                                gChl/gDW ;                  minimum chlorophyll/C ratio phytoplankton
PARM   cChDPhytMax       [  0.02  ]                                gChl/gDW ;                  maximum chlorophyll/C ratio phytoplankton
PARM   kDRespPhyt        [  0.1  ]                                 day-1 ;                     maintenance respiration constant phytoplankton
PARM   kLossPhyt         [  0  ]                                   - ;                         grazing loss rate for phytoplankton
PARM   kMortPhytW        [  0.01  ]                                day-1 ;                     mortality constant of phytoplankton in water
PARM   cVSetPhyt         [  0.1  ]                                 m/day ;                     sedimentation velocity phytoplankton
PARM   kMortPhytS        [  0.05  ]                                day-1 ;                     mortality constant sed phytoplankton
PARM   cVPUptMaxPhyt     [  0.01  ]                                gP/gDW/day ;                maximum P uptake capacity of phytoplankton at 20 oC
PARM   cAffPUptPhyt      [  0.2  ]                                 m3/gDW ;                    initial P uptake affinity phytoplankton
PARM   cPDPhytMin        [  0.002  ]                               gP/gDW ;                    minimum P/D ratio phytoplankton
PARM   cPDPhytMax        [  0.015  ]                               gP/gDW ;                    maximum P/D ratio phytoplankton
PARM   cVNUptMaxPhyt     [  0.07  ]                                gN/gDW/day ;                maximum N uptake capacity of phytoplankton at 20 oC
PARM   cAffNUptPhyt      [  0.2  ]                                 m3/gDW ;                    initial N uptake affinity phytoplankton
PARM   cNDPhytMin        [  0.02  ]                                gN/gDW ;                    minimum N/D ratio phytoplankton
PARM   cNDPhytMax        [  0.1  ]                                 gN/gDW ;                    maximum N/D ratio phytoplankton
PARM   O2PerNO3          [  1.5  ]                                 - ;                         mol O2 formed per mol NO3- ammonified
PARM   cDayApril1        [  91  ]                                  day ;                       April 1
PARM   cDayOct1          [  273  ]                                 day ;                       October 1
PARM   cLengChange       [  10  ]                                  day ;                       length of season change
PARM   DaysPerYear       [  365  ]                                 day/y ;                     days per year
PARM   TenDays           [  10  ]                                  day ;                       ten days
PARM   SecsPerDay        [  86400  ]                               s/day ;                     secs per day
PARM   mmPerm            [  1000  ]                                mm/m ;                      mm per m
PARM   m2Perha           [  10000  ]                               m2/ha ;                     m2 per ha
PARM   mgPerg            [  1000  ]                                mg/g ;                      mg per g
PARM   PerCent           [  0.01  ]                                % ;                         per cent
PARM   NearZero          [  0.0000000000000000000000000001  ]      - ;                         very small number used to avoid dividing by zero
PARM   molO2molC         [  2.6667  ]                              gO2/gC ;                    ratio of molar weights
PARM   molO2molN         [  2.2857  ]                              gO2/gN ;                    ratio of molar weights
PARM   molNmolC          [  1.1667  ]                              gN/gC ;                     ratio of molar weights
PARM   cRhoWat           [  1000000  ]                             g/m3 ;                      density of water
PARM   Pi                [  3.14159265358979  ]                    - ;                         Pi

/* ############################ */
/* External Variables           */
/* ############################ */

XT  mTemp  [ 0 ]  oC ;  measured time-series of temperature
XT  mLOut  [ 0 ]  W/m2 ;  measured time-series of light
XT  mVWind  [ 0 ]  m/s ;  measured time-series of wind speed

/* ############################ */
/* Flow Variables               */
/* ############################ */

FLOW  Z  [ 0.5 ]  m ;  Ditch water depth

{

/* ############################ */
/* Auxiliary variables          */
/* ############################ */

sDepthW = Z;
/* Ditch water depth [m] */

/* ---------------------------------- */
/* Initialization */ 
/* ---------------------------------- */

bPorS = (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat / ( fDTotS0 + (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat ) ;
/* porosity [m3_water/m3_sediment] */
bPorCorS = ( (bPorS )^ (bPorS + 1.0)) ;
/* sediment porosity corrected for tortuosity [-] */

/* ---------------------------------- */
/* Forcing */ 
/* ---------------------------------- */

sTime = time ;
/* time [day] */
TimeYears = sTime / DaysPerYear ;
/* time in years [year] */
Day = sTime - int(TimeYears) * DaysPerYear ;
/* time (daynumber) within the year [day] */
if (ReadTemp == 1 ) {
   uTm = mTemp ;
} else {
   uTm = cTmAve - cTmVar * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysPerYear) ;
}
/* forcing function temperature [oC] */
if (ReadVWind == 1) {
   uVWind = mVWind ;
} else {
   uVWind = cVWind ;
}
/* forcing function wind speed [m/s] */
ufDay = cfDayAve - cfDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) ;
/* day length [h/24h] */
if (ReadLOut == 1 ) {
   uLDay = 0.0 ;
} else {
   uLDay = cLDayAve - cLDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) ;
}
/* total daily radiation [total_J/m2/d] */
if (ReadLOut == 1 ) {
   uLOut = mLOut / ufDay ;
} else {
   uLOut = uLDay / SecsPerDay / ufDay ;
}
/* average light intensity during daytime [total_J/m2/s_=_W/m2] */
uLPAR0 = fPAR * (1.0 - fRefl) * uLOut ;
/* average PAR at zero depth [-] */
aExtPhyt = cExtSpPhyt * sDPhytW ;
/* contribution of phytoplankton to extinction [m-1] */
aExtDet = cExtSpDet * sDDetW ;
/* detrital contribution to extinction [m-1] */
aExtIM = cExtSpIM * sDIMW ;
/* contribution of inorganic matter to extinction [m-1] */
aExtCoefOpen = cExtWat + aExtIM + aExtDet + aExtPhyt ;
/* extinction coefficient without vegetation [m-1] */

/* ---------------------------------- */
/* Transport */ 
/* ---------------------------------- */

uQInSeason = 0.0 ;
uQEvSinus = 0.0 ;
uQEv = 0.0 ;
uQInExtra = 0.0 ;
uQOutExtra = 0.0 ;
uQIn = 0.0 ;
uQOut = 0.0 ;
uQDil = 0.0 ;
ukDil = 0.0 ;
ukDilWat = 0.0 ;
ukOut = 0.0 ;
uTauWat = 0.0 ;
uTauSubst = 0.0 ;
vTranDepthW = 0.0 ;

/* ---------------------------------- */
/* General */ 
/* ---------------------------------- */

oDPhytW = sDPhytW ;
/* total DW phytoplankton in ditch water [g/m3] */
oPPhytW = sPPhytW ;
/* total P phytoplankton in ditch water [g/m3] */
oNPhytW = sNPhytW ;
/* total N phytoplankton in ditch water [g/m3] */
aDPhytS = sDPhytS ;
/* total DW phytoplankton on ditch sediment [g/m2] */
aPPhytS = sPPhytS ;
/* total P phytoplankton on ditch sediment [g/m2] */
aNPhytS = sNPhytS ;
/* total N phytoplankton on ditch sediment [g/m2] */
oDOMW = sDDetW + oDPhytW ;
/* organic seston [gDW/m3] */
oDSestW = oDOMW + sDIMW ;
/* total seston [gDW/m3] */
oPOMW = oPPhytW + sPDetW ;
/* organic P in water [gP/m3] */
oPSestW = oPPhytW + sPDetW + sPAIMW ;
/* total seston P including adsorbed P [gP/m3] */
oPInorgW = sPO4W + sPAIMW ;
/* inorganic P in water [gP/m3] */
oPTotW = oPSestW + sPO4W ;
/* total P in water (without vegetation) [gP/m3] */
oNDissW = sNO3W + sNH4W ;
/* SRN in water [gN/m3] */
oNOMW = oNPhytW + sNDetW ;
/* organic seston N [gN/m3] */
oNSestW = oNOMW ;
/* total seston N [gN/m3] */
oNkjW = oNSestW + sNH4W ;
/* kjeldahl N in water [gN/m3] */
oNTotW = oNkjW + sNO3W ;
/* total N in water (without vegetation) [gN/m3] */
aDTotS = sDIMS + sDHumS + sDDetS ;
/* total sediment DW (excluding biota) [gDW/m2] */
aRhoTotS = aDTotS / cDepthS ;
/* (apparent) bulk density of sediment [g_solid/m3_sediment] */
aRhoSolidS = (sDIMS * cRhoIM + (sDHumS + sDDetS) * cRhoOM) / aDTotS ;
/* average solid density [g/m3_solid] */
afDTotS = 1.0 / (1.0 + bPorS/(1.0-bPorS) * cRhoWat / aRhoSolidS) ;
/* sediment dry-weight fraction [g_solid/g_sediment] */
afDOrgS = (sDHumS + sDDetS) / aDTotS ;
/* total organic fraction of sediment DW [-] */
afDetS = sDDetS / (sDHumS + sDDetS) ;
/* detrital fraction of sediment organic DW [-] */
afDetTotS = sDDetS / (sDIMS + sDHumS + sDDetS) ;
/* detrital fraction of total sediment DW [-] */
aPInorgS = sPO4S + sPAIMS ;
/* inorganic P in sediment [gP/m2] */
aPTotAvailS = sPDetS + aPInorgS + aPPhytS ;
/* total P in sediment (without humus and vegetation) [gP/m2] */
aPTotS = aPTotAvailS + sPHumS ;
/* total P in sediment (without vegetation) [gP/m2] */
afPInorgS = aPInorgS / aDTotS ;
/* fraction inorganic P in sediment [gP/gDW] */
afPTotS = aPTotS / aDTotS ;
/* total P fraction in sediment [gP/gDW] */
afPO4S = sPO4S / (aPTotAvailS + NearZero) ;
/* fraction dissolved P in sediment [-] */
oPO4S = sPO4S / cDepthS / bPorS ;
/* concentration dissolved P in interstitial water [gP/m3] */
aNDissS = sNH4S + sNO3S ;
/* total dissolved N in interstitial water [gN/m2] */
aNkjAvailS = sNDetS + aNPhytS + sNH4S ;
/* kjeldahl N in sediment excluding humus [gN/m2] */
aNkjS = aNkjAvailS + sNHumS ;
/* kjeldahl N in sediment [gN/m2] */
aNTotAvailS = aNkjAvailS + sNO3S ;
/* total N in sediment excluding humus [gN/m2] */
aNTotS = aNkjS + sNO3S ;
/* total N in sediment [gN/m2] */
afNInorgS = aNDissS / aDTotS ;
/* fraction inorganic N in sediment [gN/gDW] */
afNTotS = aNTotS / aDTotS ;
/* total N fraction in sediment [gN/gDW] */
oNO3S = sNO3S / cDepthS / bPorS ;
/* concentration dissolved N-NO3 in interstitial water [gN/m3] */
oNH4S = sNH4S / cDepthS / bPorS ;
/* concentration dissolved N-NH4 in interstitial water [gN/m3] */
oNDissS = aNDissS / cDepthS / bPorS ;
/* SRN concentration in interstitial water [gN/m3] */
rPDIMW = sPAIMW / sDIMW ;
/* P/DW ratio of water DIM [gP/gDW] */
rPDIMS = sPAIMS / sDIMS ;
/* P/DW ratio of sediment DIM [gP/gDW] */
rPDDetW = sPDetW / (sDDetW+NearZero) ;
/* P/DW ratio of water detritus [gP/gDW] */
rNDDetW = sNDetW / (sDDetW+NearZero) ;
/* N/DW ratio of water detritus [gN/gDW] */
rPDHumS = sPHumS / (sDHumS+NearZero) ;
/* P/DW ratio of sediment OM [gP/gDW] */
rNDHumS = sNHumS / (sDHumS+NearZero) ;
/* N/DW ratio of sediment OM [gN/gDW] */
rPDDetS = sPDetS / (sDDetS+NearZero) ;
/* P/DW ratio of sediment detritus [gP/gDW] */
rNDDetS = sNDetS / (sDDetS+NearZero) ;
/* N/DW ratio of sediment detritus [gN/gDW] */

/* ---------------------------------- */
/* Transport */ 
/* ---------------------------------- */

uPLoadSeason = 0.0 ;
uPLoad = 0.0 ;
uPLoadPO4 = 0.0 ;
uPLoadOrg = 0.0 ;
uPLoadPhytTot = 0.0 ;
uPLoadDet = 0.0 ;
uPLoadAIM = 0.0 ;
uNLoadSeason = 0.0 ;
uNLoadPhytTot = 0.0 ;
uNLoad = 0.0 ;
uNLoadDet = 0.0 ;
uNLoadOrg = 0.0 ;
uNLoadDiss = 0.0 ;
uNLoadNH4 = 0.0 ;
uNLoadNO3 = 0.0 ;
uNTotIn = 0.0 ;
uDLoadDet = 0.0 ;
uDLoadPhytTot = 0.0 ;
uDLoadIM = 0.0 ;
uDLoad = 0.0 ;
uPTotIn = 0.0 ;
wDDilIM = 0.0 ;
wDDilDet = 0.0 ;
wPDilPO4 = 0.0 ;
wPDilDet = 0.0 ;
wPDilAIM = 0.0 ;
wNDilNH4 = 0.0 ;
wNDilNO3 = 0.0 ;
wNDilDet = 0.0 ;
wO2Inflow = 0.0 ;
wO2Outfl = 0.0 ;
wDDilPhyt = 0.0 ;
wPDilPhyt = 0.0 ;
wNDilPhyt = 0.0 ;
wDOutflTot = 0.0 ;
wPOutflTot = 0.0 ;
wNOutflTot = 0.0 ;
uDLoadPhyt = 0.0 ;
uPLoadPhyt = 0.0 ;
uNLoadPhyt = 0.0 ;
wDTranPhyt = 0.0 ;
wPTranPhyt = 0.0 ;
wNTranPhyt = 0.0 ;
wDTranIMW = 0.0 ;
wDTranDetW = 0.0 ;
wO2TranW = 0.0 ;
wPTranPO4W = 0.0 ;
wPTranAIMW = 0.0 ;
wPTranDetW = 0.0 ;
wNTranNH4W = 0.0 ;
wNTranNO3W = 0.0 ;
wNTranDetW = 0.0 ;
wDDilTot = 0.0 ;
wPDilTot = 0.0 ;
wNDilTot = 0.0 ;
tPInfPO4W = 0.0 ;
tNInfNH4W = 0.0 ;
tNInfNO3W = 0.0 ;
tPInfPO4S = 0.0 ;
tNInfNH4S = 0.0 ;
tNInfNO3S = 0.0 ;

/* ---------------------------------- */
/* Abiotic */ 
/* ---------------------------------- */

tNH4LoadS = fNH4LoadS * cNLoadS ;
/* NH4 load to sediment from artificial fertilizer [gN/m2/day] */
tNO3LoadS = cNLoadS - tNH4LoadS ;
/* NO3 load to sediment from artificial fertilizer [gN/m2/day] */
uDErosIM = (1.0 - fDOrgSoil) * cDErosTot ;
/* IM input from banks [gDW/m2/day] */
uDErosIMS = fSedErosIM * uDErosIM ;
/* IM input to sediment from banks [gDW/m2/day] */
uDErosIMW = uDErosIM - uDErosIMS ;
/* IM input to water column from banks [gDW/m2/day] */
uDErosOM = fDOrgSoil * cDErosTot ;
/* organic matter input from banks [gDW/m2/day] */
uPErosOM = cPDSoilOM * uDErosOM ;
/* organic P input from banks [gP/m2/day] */
uNErosOM = cNDSoilOM * uDErosOM ;
/* organic N input from banks [gN/m2/day] */
uO2Sat = 14.652 - 0.41022 * uTm + 0.007991 * uTm*uTm - 0.000077774 * uTm*uTm*uTm ;
/* oxygen saturation concentration [gO2/m3] */
kAer = cAerRoot * ((uVWind )^ (0.5)) + cAerLin * uVWind + cAerSquare * uVWind*uVWind ;
/* reaeration coefficient [m/day] */
uFunTmAer = ( (cThetaAer )^ (uTm-cTmRef)) ;
/* temperature function of reaeration [-] */
aFunLemnAer = max(0.0, 1.0 - (kLemnAer * sDLemn)) ;
/* duckweed function of reaeration [-] */
tO2Aer = kAer * uFunTmAer * (uO2Sat - sO2W) * aFunLemnAer ;
/* reaeration flux of O2 into the water [gO2/m2/day] */
tDTurbFish = 0.0 ;
/* bioturbation by fish [g/m2/day] */
tDTurbFishIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDTurbFish ;
/* IM bioturbation by fish [g/m2/day] */
aDVeg = sDElod + sDChar + sDCera + sDLemn + sDNymp + sDHelo ;
/* total plant biomass [gDW/m2/day] */
aFunVegResus = max(1.0 - kVegResus * aDVeg, 0.0) ;
/* vegetation dependence of resuspension [-] */
if (uTm >= 0.1) {
   aFunDimSusp = cSuspRef * ((cSuspMin + cSuspMax / (1.0 + exp(cSuspSlope * (sDepthW - hDepthSusp)))) * ((((cFetch +NearZero)/ cFetchRef))^(0.5))) ;
} else {
   aFunDimSusp = 0.0 ;
}
/* Empirical suspended matter function (logistic fit to data) [-] */
tDResusTauDead = min(aFunDimSusp, (((aFunDimSusp +NearZero))^(0.5))) * (((fLutum / fLutumRef))^(0.5)) * bPorS ;
/* resuspension due to shear stress [gDW/m2/day] */
tDResusBareDead = tDResusTauDead + tDTurbFish ;
/* resuspension due to shear stress and fish [gDW/m2/day] */
tDResusDead = tDResusBareDead * aFunVegResus ;
/* resuspension corrected for vegetation effect [gDW/m2/day] */
tDResusIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDResusDead ;
/* IM resuspension [gDW/m2/day] */
tDResusDet = sDDetS / (fLutum * sDIMS + sDDetS) * tDResusDead ;
/* detrital resuspension [gDW/m2/day] */
akResusPhytRef = kResusPhytMax * (1.0 - exp(cResusPhytExp * tDResusDead)) ;
/* phytoplankton resuspension rate constant [day-1] */
tDResusPhytTot = akResusPhytRef * aDPhytS ;
/* phytoplankton resuspension [gDW/m2/day] */
tPResusDet = rPDDetS * tDResusDet ;
/* resuspension flux of detrital P [gP/m2/day] */
tPResusPO4 = sPO4S / sDDetS * tDResusDet ;
/* resuspension flux of dissolved P [gP/m2/day] */
tPResusAIM = sPAIMS / sDIMS * tDResusIM ;
/* resuspension flux of P adsorbed onto inorganic matter [gP/m2/day] */
tNResusNO3 = sNO3S / sDDetS * tDResusDet ;
/* resuspension flux of nitrate [gN/m2/day] */
tNResusNH4 = sNH4S / sDDetS * tDResusDet ;
/* resuspension flux of ammonium [gN/m2/day] */
tNResusDet = rNDDetS * tDResusDet ;
/* resuspension flux of detrital N [gN/m2/day] */
aFunTauSetOM = min(1.0 / (((aFunDimSusp +NearZero))^(0.5)), 1.0) ;
/* correction factor for IM settling rate (<= 1) [-] */
aFunTauSetIM = aFunTauSetOM ;
/* correction factor for OM settling rate (<= 1) [-] */
uFunTmSet = ( (cThetaSet )^ (uTm-cTmRef)) ;
/* temperature correction of sedimentation [-] */
uCorVSetIM = aFunTauSetIM * (((fLutumRef/fLutum))^(0.5)) * uFunTmSet * cVSetIM ;
/* corrected sedimentation velocity of IM [m/day] */
tDSetIM = uCorVSetIM * sDIMW ;
/* sedimentation flux of inorganic matter [gDW/m2/day] */
tPSetAIM = sPAIMW / sDIMW * tDSetIM ;
/* sedimentation flux of P adsorbed onto inorganic matter [gP/m2/day] */
uCorVSetDet = cVSetDet * aFunTauSetOM * uFunTmSet ;
/* corrected sedimentation velocity of detritus [m/day] */
tDSetDet = uCorVSetDet * sDDetW ;
/* sedimentation flux of detritus [gDW/m2/day] */
tPSetDet = uCorVSetDet * sPDetW ;
/* sedimentation flux of detrital P [gP/m2/day] */
tNSetDet = uCorVSetDet * sNDetW ;
/* sedimentation flux of detrital N [gN/m2/day] */
kPMinDetW = kDMinDetW ;
/* P mineralization constant in water [day-1] */
kNMinDetW = kDMinDetW ;
/* N mineralization constant in water [day-1] */
uFunTmMinW = ( (cThetaMinW )^ (uTm-cTmRef)) ;
/* temperature function of mineralization in water [-] */
wDMinDetW = kDMinDetW * uFunTmMinW * sDDetW ;
/* decomposition [g/m3/day] */
wPMinDetW = kPMinDetW * uFunTmMinW * sPDetW ;
/* mineralization [gP/m3/day] */
wNMinDetW = kNMinDetW * uFunTmMinW * sNDetW ;
/* mineralization [gN/m3/day] */
aCorO2BOD = sO2W / (hO2BOD + sO2W) ;
/* correction of O2 demand in water at low oxygen concentration [-] */
wO2MinDetW = molO2molC * cCPerDW * aCorO2BOD * wDMinDetW ;
/* O2 flux due to mineralization of detritus [gO2/m3/day] */
wDDenitW = sNO3W*sNO3W / (hNO3Denit*hNO3Denit + sNO3W*sNO3W) * (1.0 - aCorO2BOD) * wDMinDetW ;
/* mineralization flux by denitrification [gDW/m2/day] */
wNDenitW = NO3PerC * molNmolC * cCPerDW * wDDenitW ;
/* Denitrification flux [gN/m2/day] */
uFunTmNitr = ( (cThetaNitr )^ (uTm-cTmRef)) ;
/* Temperature dependence for nitrification [-] */
aCorO2NitrW = sO2W*sO2W / (hO2Nitr*hO2Nitr + sO2W*sO2W) ;
/* oxygen dependence of nitrification [-] */
wNNitrW = kNitrW * uFunTmNitr * aCorO2NitrW * sNH4W ;
/* nitrification flux [gN/m3/day] */
wO2NitrW = O2PerNH4 * molO2molN * wNNitrW ;
/* O2 flux due to nitrification [gO2/m3/day] */
kPMinDetS = kDMinDetS ;
/* P mineralization constant in sediment [day-1] */
kNMinDetS = kDMinDetS ;
/* N mineralization constant in sediment [day-1] */
uFunTmMinS = ( (cThetaMinS )^ (uTm-cTmRef)) ;
/* temperature function [-] */
tDMinDetS = kDMinDetS * uFunTmMinS * sDDetS ;
/* decomposition of upper sediment [gDW/m2/day] */
tPMinDetS = kPMinDetS * uFunTmMinS * sPDetS ;
/* mineralization of P in upper sediment [gP/m2/day] */
tNMinDetS = kNMinDetS * uFunTmMinS * sNDetS ;
/* mineralization of N in upper sediment [gN/m2/day] */
uFunTmDif = ( (cThetaDif )^ (uTm-cTmRef)) ;
/* temperature function of diffusion [-] */
akO2DifCor = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorS ;
/* corrected O2 diffusion coefficient [m2/day] */
tSOD = (molO2molC * cCPerDW * (1.0 - fRefrDetS) * tDMinDetS + O2PerNH4 * molO2molN * kNitrS * uFunTmNitr * sNH4S) / cDepthS ;
/* sediment oxygen demand [gO2/m3/day] */
aDepthOxySed = (((2.0 * sO2W * akO2DifCor / tSOD))^(0.5)) ;
/* oxygen penetration depth [m] */
afOxySed = aDepthOxySed / cDepthS ;
/* fraction aerobic sediment [-] */
tDMinOxyDetS = afOxySed * (1.0 - fRefrDetS) * tDMinDetS ;
/* aerobic mineralization [gDW/m2/day] */
tO2MinDetS = molO2molC * cCPerDW * tDMinOxyDetS ;
/* sediment oxygen demand [gO2/m2/day] */
tDDenitS = oNO3S*oNO3S / (hNO3Denit*hNO3Denit + oNO3S*oNO3S) * (1.0 - afOxySed) * (1.0 - fRefrDetS) * tDMinDetS ;
/* mineralization flux by denitrification [gDW/m2/day] */
tNDenitS = NO3PerC * molNmolC * cCPerDW * tDDenitS ;
/* Denitrification flux [gN/m2/day] */
tNNitrS = afOxySed * kNitrS * uFunTmNitr * sNH4S ;
/* nitrification flux [gN/m2/day] */
tO2NitrS = O2PerNH4 * molO2molN * tNNitrS ;
/* O2 flux due to nitrification [gO2/m2/day] */
tDMinHumS = kDMinHum * uFunTmMinS * afOxySed * sDHumS ;
/* decomposition of upper sediment humus [gDW/m2/day] */
tPMinHumS = kDMinHum * uFunTmMinS * afOxySed * sPHumS ;
/* mineralization of P in upper sediment humus [gP/m2/day] */
tNMinHumS = kDMinHum * uFunTmMinS * afOxySed * sNHumS ;
/* mineralization of N in upper sediment humus [gN/m2/day] */
aDepthDif = fDepthDifS * cDepthS ;
/* average diffusion distance [m] */
tPDifPO4 = kPDifPO4 * uFunTmDif * cTurbDifNut * bPorCorS * (oPO4S - sPO4W ) / aDepthDif ;
/* diffusion flux of dissolved P from sediment to water [gP/m2/day] */
tNDifNO3 = kNDifNO3 * uFunTmDif * cTurbDifNut * bPorCorS * (oNO3S - sNO3W ) / aDepthDif ;
/* diffusion flux of NO3 from sediment to water [gP/m2/day] */
tNDifNH4 = kNDifNH4 * uFunTmDif * cTurbDifNut * bPorCorS * (oNH4S - sNH4W ) / aDepthDif ;
/* diffusion flux of NH4 from sediment to water [gP/m2/day] */
tO2Dif = kO2Dif / aDepthDif * uFunTmDif * cTurbDifO2 * bPorCorS * sO2W ;
/* O2 diffusion from water to sediment [gO2/m2/day] */
tPDifGroundPO4 = 0.0 ;
/* diffusion flux of dissolved P from interstitial water to ground water [gP/m2/day] */
tNDifGroundNO3 = 0.0 ;
/* diffusion flux of dissolved NO3 from interstitial water to ground water [gN/m2/day] */
tNDifGroundNH4 = 0.0 ;
/* diffusion flux of dissolved NH4 from interstitial water to ground water [gN/m2/day] */
aPAdsMaxW = cRelPAdsD + aCorO2BOD * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
/* maximum P adsorption per g inorganic matter in water [gP/gDW] */
aKPAdsW = (1.0 - fRedMax * (1.0-aCorO2BOD)) * cKPAdsOx ;
/* P adsorption affinity corrected for redox conditions [m3/gP] */
aPIsoAdsW = aPAdsMaxW * aKPAdsW * sPO4W / (1.0 + aKPAdsW * sPO4W) ;
/* P adsorption isotherm onto inorganic matter in sediment [gP/gDW] */
aPEqIMW = aPIsoAdsW * sDIMW ;
/* equilibrium concentration [gP/m3] */
wPSorpIMW = kPSorp * (aPEqIMW - sPAIMW) ;
/* sorption flux in water [gP/m3/day] */
aPAdsMaxS = cRelPAdsD + afOxySed * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
/* maximum P adsorption per g inorganic matter in sediment [gP/gDW] */
aKPAdsS = (1.0 - fRedMax * (1.0-afOxySed)) * cKPAdsOx ;
/* P adsorption affinity corrected for redox conditions [m3/gP] */
aPIsoAdsS = aPAdsMaxS * aKPAdsS * oPO4S / (1.0 + aKPAdsS * oPO4S) ;
/* P adsorption isotherm onto inorganic matter in sediment [gP/gDW] */
aPEqIMS = aPIsoAdsS * sDIMS ;
/* equilibrium amount [gP/m2] */
tPSorpIMS = kPSorp * (aPEqIMS - sPAIMS) ;
/* sorption [gP/m2/day] */
tPChemPO4 = max( 0.0, kPChemPO4 * (oPO4S - coPO4Max) ) ;
/* chemical loss of dissolved P from interstitial water by precipitation [gP/m2/day] */
wDAbioIMW = (uDErosIMW - tDSetIM + tDResusIM) / sDepthW ;
/* total abiotic/microbial DW inorganic matter flux in water [gDW/m3/day] */
wDAbioDetW = (- tDSetDet + tDResusDet ) / sDepthW - wDMinDetW ;
/* total abiotic/microbial DW detritus flux in water [gDW/m3/day] */
tDAbioIMS = uDErosIMS + tDSetIM - tDResusIM ;
/* total abiotic/microbial DW inorganic matter flux in sediment [gDW/m2/day] */
tDAbioDetS = tDSetDet - tDResusDet - tDMinDetS ;
/* total abiotic/microbial DW detritus flux in sediment [gDW/m2/day] */
tDAbioHumS = uDErosOM + fRefrDetS * tDMinDetS - tDMinHumS ;
/* total abiotic/microbial DW humus flux in sediment [gDW/m2/day] */
tDAbioTotT = cDErosTot - wDMinDetW * sDepthW - (1.0 - fRefrDetS) * tDMinDetS - tDMinHumS ;
/* total abiotic/microbial DW flux for mass balance check [gDW/m2/day] */
wO2AbioW = tO2Aer / sDepthW - wO2MinDetW - wO2NitrW - (tO2MinDetS + tO2NitrS) / sDepthW ;
/* total abiotic/microbial O2 flux in water [gO2/m3/day] */
wPAbioDetW = - wPMinDetW - (tPSetDet - tPResusDet) / sDepthW ;
/* total abiotic/microbial P detritus flux in water [gP/m3/day] */
wPAbioPO4W = wPMinDetW - wPSorpIMW - tPInfPO4W / sDepthW + tPDifPO4 / sDepthW + tPResusPO4 / sDepthW ;
/* total abiotic/microbial dissolved P flux in water [gP/m3/day] */
wPAbioAIMW = (- tPSetAIM + tPResusAIM) / sDepthW + wPSorpIMW ;
/* total abiotic/microbial P absorbed onto inorganic matter flux in water [gP/m3/day] */
tPAbioDetS = - tPMinDetS + tPSetDet - tPResusDet ;
/* total abiotic/microbial P detritus flux in sediment [gP/m2/day] */
tPAbioHumS = uPErosOM + fRefrDetS * tPMinDetS - tPMinHumS ;
/* total abiotic/microbial P humus flux in sediment [gP/m2/day] */
tPAbioPO4S = tPInfPO4W - tPInfPO4S + (1.0-fRefrDetS) * tPMinDetS + tPMinHumS - tPSorpIMS - tPResusPO4 - tPDifPO4 - tPDifGroundPO4 - tPChemPO4 ;
/* total abiotic/microbial dissolved P flux in sediment [gP/m2/day] */
tPAbioAIMS = tPSetAIM - tPResusAIM + tPSorpIMS ;
/* total abiotic/microbial P absorbed onto inorganic matter flux in sediment [gP/m2/day] */
tPAbioTotT = uPErosOM - tPChemPO4 - tPInfPO4S - tPDifGroundPO4 ;
/* total abiotic/microbial P flux for mass balance check [gP/m2/day] */
wNAbioNH4W = wNMinDetW - wNNitrW - tNInfNH4W / sDepthW + (tNDifNH4 + tNResusNH4) / sDepthW ;
/* total abiotic/microbial N NH4 flux in water [gN/m3/day] */
wNAbioNO3W = wNNitrW - wNDenitW + (tNDifNO3 + tNResusNO3 - tNInfNO3W) / sDepthW ;
/* total abiotic/microbial N NO3 flux in water [gN/m3/day] */
wNAbioDetW = - wNMinDetW - (tNSetDet - tNResusDet ) / sDepthW ;
/* total abiotic/microbial N detritus flux in water [gN/m3/day] */
tNAbioNH4S = tNInfNH4W - tNInfNH4S + (1.0-fRefrDetS) * tNMinDetS + tNMinHumS - tNResusNH4 - tNDifNH4 - tNDifGroundNH4 - tNNitrS ;
/* total abiotic/microbial N NH4 flux in sediment [gN/m2/day] */
tNAbioNO3S = tNInfNO3W - tNInfNO3S + tNNitrS - tNDenitS - tNResusNO3 - tNDifNO3 - tNDifGroundNO3 ;
/* total abiotic/microbial N NO3 flux in sediment [gN/m2/day] */
tNAbioDetS = - tNMinDetS + tNSetDet - tNResusDet ;
/* total abiotic/microbial N detritus flux in sediment [gN/m2/day] */
tNAbioHumS = uNErosOM + fRefrDetS * tNMinDetS - tNMinHumS ;
/* total abiotic/microbial N humus flux in sediment [gN/m2/day] */
tNAbioTotT = uNErosOM - tNDenitS - wNDenitW * sDepthW - tNInfNH4S - tNInfNO3S - tNDifGroundNO3 - tNDifGroundNH4 ;
/* total abiotic/microbial N flux for mass balance check [gN/m2/day] */

/* ---------------------------------- */
/* Vegetation */ 
/* ---------------------------------- */

fManElod = fManVeg ;
/* fraction of DW biomass removed by management [-] */
fManCera = fManVeg ;
/* fraction of DW biomass removed by management [-] */
fManChar = fManVeg ;
/* fraction of DW biomass removed by management [-] */
fManNymp = fManHelo ;
/* fraction of DW biomass removed by management [-] */
kMigrLemn = ckMigrLemn + (1.0 - fObstrLemn) * ukOut ;
/* migration constant for Lemnacaea [day-1] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitElod = 367 ;
} else if (uTm >= cTmInitElod && aDayInitElod > 366) {
   aDayInitElod = Day ;
} else {
   aDayInitElod = aDayInitElod ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitElod) {
   bfRootElod = fRootElodWin ;
} else if (Day < aDayInitElod + cLengAllo) {
   bfRootElod = 0.5*(fRootElodWin + fRootElodSum) + 0.5*(fRootElodWin - fRootElodSum) * cos(Pi/cLengAllo * (Day - aDayInitElod)) ;
} else if (Day < cDayWinElod) {
   bfRootElod = fRootElodSum ;
} else if (Day < cDayWinElod + cLengAllo) {
   bfRootElod = 0.5*(fRootElodWin + fRootElodSum) - 0.5*(fRootElodWin - fRootElodSum) * cos(Pi/cLengAllo * (Day - cDayWinElod)) ;
} else {
   bfRootElod = fRootElodWin ;
}
/* root fraction [g_root/gDW] */
bfShootElod = 1.0 - bfRootElod ;
/* shoot fraction [g_shoot/gDW] */
aDRootElod = bfRootElod * sDElod ;
/* root biomass [gDW/m2] */
aDShootElod = bfShootElod * sDElod ;
/* shoot biomass [gDW/m2] */
aDEmergElod = fEmergElod * aDShootElod ;
/* emergent biomass [gDW/m2] */
aDFloatElod = fFloatElod * aDShootElod ;
/* floating biomass [gDW/m2] */
bfSubElod = 1.0 - fFloatElod - fEmergElod ;
/* submerged fraction of shoot [-] */
aDSubElod = bfSubElod * aDShootElod ;
/* submerged biomass [gDW/m2] */
aExtElod = cExtSpElod * aDSubElod / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Elod = fDepth1Elod * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Elod = fDepth2Elod * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfElod = min(1.0, max(aDFloatElod / (cDLayerElod + NearZero), aDEmergElod / (fEmergElod * cDCarrElod + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergElod = min(1.0, PerCent * cCovSpElod * aDEmergElod) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovElod = min(100.0, cCovSpElod * aDShootElod) ;
/* percentage of water surface covered by plant species [%] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitChar = 367 ;
} else if (uTm >= cTmInitChar && aDayInitChar > 366) {
   aDayInitChar = Day ;
} else {
   aDayInitChar = aDayInitChar ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitChar) {
   bfRootChar = fRootCharWin ;
} else if (Day < aDayInitChar + cLengAllo) {
   bfRootChar = 0.5*(fRootCharWin + fRootCharSum) + 0.5*(fRootCharWin - fRootCharSum) * cos(Pi/cLengAllo * (Day - aDayInitChar)) ;
} else if (Day < cDayWinChar) {
   bfRootChar = fRootCharSum ;
} else if (Day < cDayWinChar + cLengAllo) {
   bfRootChar = 0.5*(fRootCharWin + fRootCharSum) - 0.5*(fRootCharWin - fRootCharSum) * cos(Pi/cLengAllo * (Day - cDayWinChar)) ;
} else {
   bfRootChar = fRootCharWin ;
}
/* root fraction [g_root/gDW] */
bfShootChar = 1.0 - bfRootChar ;
/* shoot fraction [g_shoot/gDW] */
aDRootChar = bfRootChar * sDChar ;
/* root biomass [gDW/m2] */
aDShootChar = bfShootChar * sDChar ;
/* shoot biomass [gDW/m2] */
aDEmergChar = fEmergChar * aDShootChar ;
/* emergent biomass [gDW/m2] */
aDFloatChar = fFloatChar * aDShootChar ;
/* floating biomass [gDW/m2] */
bfSubChar = 1.0 - fFloatChar - fEmergChar ;
/* submerged fraction of shoot [-] */
aDSubChar = bfSubChar * aDShootChar ;
/* submerged biomass [gDW/m2] */
aExtChar = cExtSpChar * aDSubChar / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Char = fDepth1Char * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Char = fDepth2Char * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfChar = min(1.0, max(aDFloatChar / (cDLayerChar + NearZero), aDEmergChar / (fEmergChar * cDCarrChar + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergChar = min(1.0, PerCent * cCovSpChar * aDEmergChar) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovChar = min(100.0, cCovSpChar * aDShootChar) ;
/* percentage of water surface covered by plant species [%] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitCera = 367 ;
} else if (uTm >= cTmInitCera && aDayInitCera > 366) {
   aDayInitCera = Day ;
} else {
   aDayInitCera = aDayInitCera ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitCera) {
   bfRootCera = fRootCeraWin ;
} else if (Day < aDayInitCera + cLengAllo) {
   bfRootCera = 0.5*(fRootCeraWin + fRootCeraSum) + 0.5*(fRootCeraWin - fRootCeraSum) * cos(Pi/cLengAllo * (Day - aDayInitCera)) ;
} else if (Day < cDayWinCera) {
   bfRootCera = fRootCeraSum ;
} else if (Day < cDayWinCera + cLengAllo) {
   bfRootCera = 0.5*(fRootCeraWin + fRootCeraSum) - 0.5*(fRootCeraWin - fRootCeraSum) * cos(Pi/cLengAllo * (Day - cDayWinCera)) ;
} else {
   bfRootCera = fRootCeraWin ;
}
/* root fraction [g_root/gDW] */
bfShootCera = 1.0 - bfRootCera ;
/* shoot fraction [g_shoot/gDW] */
aDRootCera = bfRootCera * sDCera ;
/* root biomass [gDW/m2] */
aDShootCera = bfShootCera * sDCera ;
/* shoot biomass [gDW/m2] */
aDEmergCera = fEmergCera * aDShootCera ;
/* emergent biomass [gDW/m2] */
aDFloatCera = fFloatCera * aDShootCera ;
/* floating biomass [gDW/m2] */
bfSubCera = 1.0 - fFloatCera - fEmergCera ;
/* submerged fraction of shoot [-] */
aDSubCera = bfSubCera * aDShootCera ;
/* submerged biomass [gDW/m2] */
aExtCera = cExtSpCera * aDSubCera / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Cera = fDepth1Cera * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Cera = fDepth2Cera * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfCera = min(1.0, max(aDFloatCera / (cDLayerCera + NearZero), aDEmergCera / (fEmergCera * cDCarrCera + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergCera = min(1.0, PerCent * cCovSpCera * aDEmergCera) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovCera = min(100.0, cCovSpCera * aDShootCera) ;
/* percentage of water surface covered by plant species [%] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitLemn = 367 ;
} else if (uTm >= cTmInitLemn && aDayInitLemn > 366) {
   aDayInitLemn = Day ;
} else {
   aDayInitLemn = aDayInitLemn ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitLemn) {
   bfRootLemn = fRootLemnWin ;
} else if (Day < aDayInitLemn + cLengAllo) {
   bfRootLemn = 0.5*(fRootLemnWin + fRootLemnSum) + 0.5*(fRootLemnWin - fRootLemnSum) * cos(Pi/cLengAllo * (Day - aDayInitLemn)) ;
} else if (Day < cDayWinLemn) {
   bfRootLemn = fRootLemnSum ;
} else if (Day < cDayWinLemn + cLengAllo) {
   bfRootLemn = 0.5*(fRootLemnWin + fRootLemnSum) - 0.5*(fRootLemnWin - fRootLemnSum) * cos(Pi/cLengAllo * (Day - cDayWinLemn)) ;
} else {
   bfRootLemn = fRootLemnWin ;
}
/* root fraction [g_root/gDW] */
bfShootLemn = 1.0 - bfRootLemn ;
/* shoot fraction [g_shoot/gDW] */
aDRootLemn = bfRootLemn * sDLemn ;
/* root biomass [gDW/m2] */
aDShootLemn = bfShootLemn * sDLemn ;
/* shoot biomass [gDW/m2] */
aDEmergLemn = fEmergLemn * aDShootLemn ;
/* emergent biomass [gDW/m2] */
aDFloatLemn = fFloatLemn * aDShootLemn ;
/* floating biomass [gDW/m2] */
bfSubLemn = 1.0 - fFloatLemn - fEmergLemn ;
/* submerged fraction of shoot [-] */
aDSubLemn = bfSubLemn * aDShootLemn ;
/* submerged biomass [gDW/m2] */
aExtLemn = cExtSpLemn * aDSubLemn / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Lemn = fDepth1Lemn * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Lemn = fDepth2Lemn * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfLemn = min(1.0, max(aDFloatLemn / (cDLayerLemn + NearZero), aDEmergLemn / (fEmergLemn * cDCarrLemn + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergLemn = min(1.0, PerCent * cCovSpLemn * aDEmergLemn) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovLemn = min(100.0, cCovSpLemn * aDShootLemn) ;
/* percentage of water surface covered by plant species [%] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitNymp = 367 ;
} else if (uTm >= cTmInitNymp && aDayInitNymp > 366) {
   aDayInitNymp = Day ;
} else {
   aDayInitNymp = aDayInitNymp ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitNymp) {
   bfRootNymp = fRootNympWin ;
} else if (Day < aDayInitNymp + cLengAllo) {
   bfRootNymp = 0.5*(fRootNympWin + fRootNympSum) + 0.5*(fRootNympWin - fRootNympSum) * cos(Pi/cLengAllo * (Day - aDayInitNymp)) ;
} else if (Day < cDayWinNymp) {
   bfRootNymp = fRootNympSum ;
} else if (Day < cDayWinNymp + cLengAllo) {
   bfRootNymp = 0.5*(fRootNympWin + fRootNympSum) - 0.5*(fRootNympWin - fRootNympSum) * cos(Pi/cLengAllo * (Day - cDayWinNymp)) ;
} else {
   bfRootNymp = fRootNympWin ;
}
/* root fraction [g_root/gDW] */
bfShootNymp = 1.0 - bfRootNymp ;
/* shoot fraction [g_shoot/gDW] */
aDRootNymp = bfRootNymp * sDNymp ;
/* root biomass [gDW/m2] */
aDShootNymp = bfShootNymp * sDNymp ;
/* shoot biomass [gDW/m2] */
aDEmergNymp = fEmergNymp * aDShootNymp ;
/* emergent biomass [gDW/m2] */
aDFloatNymp = fFloatNymp * aDShootNymp ;
/* floating biomass [gDW/m2] */
bfSubNymp = 1.0 - fFloatNymp - fEmergNymp ;
/* submerged fraction of shoot [-] */
aDSubNymp = bfSubNymp * aDShootNymp ;
/* submerged biomass [gDW/m2] */
aExtNymp = cExtSpNymp * aDSubNymp / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Nymp = fDepth1Nymp * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Nymp = fDepth2Nymp * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfNymp = min(1.0, max(aDFloatNymp / (cDLayerNymp + NearZero), aDEmergNymp / (fEmergNymp * cDCarrNymp + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergNymp = min(1.0, PerCent * cCovSpNymp * aDEmergNymp) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovNymp = min(100.0, cCovSpNymp * aDShootNymp) ;
/* percentage of water surface covered by plant species [%] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitHelo = 367 ;
} else if (uTm >= cTmInitHelo && aDayInitHelo > 366) {
   aDayInitHelo = Day ;
} else {
   aDayInitHelo = aDayInitHelo ;
}
/* day of the year at which growing season starts [day] */
if (Day < aDayInitHelo) {
   bfRootHelo = fRootHeloWin ;
} else if (Day < aDayInitHelo + cLengAllo) {
   bfRootHelo = 0.5*(fRootHeloWin + fRootHeloSum) + 0.5*(fRootHeloWin - fRootHeloSum) * cos(Pi/cLengAllo * (Day - aDayInitHelo)) ;
} else if (Day < cDayWinHelo) {
   bfRootHelo = fRootHeloSum ;
} else if (Day < cDayWinHelo + cLengAllo) {
   bfRootHelo = 0.5*(fRootHeloWin + fRootHeloSum) - 0.5*(fRootHeloWin - fRootHeloSum) * cos(Pi/cLengAllo * (Day - cDayWinHelo)) ;
} else {
   bfRootHelo = fRootHeloWin ;
}
/* root fraction [g_root/gDW] */
bfShootHelo = 1.0 - bfRootHelo ;
/* shoot fraction [g_shoot/gDW] */
aDRootHelo = bfRootHelo * sDHelo ;
/* root biomass [gDW/m2] */
aDShootHelo = bfShootHelo * sDHelo ;
/* shoot biomass [gDW/m2] */
aDEmergHelo = fEmergHelo * aDShootHelo ;
/* emergent biomass [gDW/m2] */
aDFloatHelo = fFloatHelo * aDShootHelo ;
/* floating biomass [gDW/m2] */
bfSubHelo = 1.0 - fFloatHelo - fEmergHelo ;
/* submerged fraction of shoot [-] */
aDSubHelo = bfSubHelo * aDShootHelo ;
/* submerged biomass [gDW/m2] */
aExtHelo = cExtSpHelo * aDSubHelo / sDepthW ;
/* contribution of plant species to extinction [m-1] */
aDepth1Helo = fDepth1Helo * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Helo = fDepth2Helo * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
afCovSurfHelo = min(1.0, max(aDFloatHelo / (cDLayerHelo + NearZero), aDEmergHelo / (fEmergHelo * cDCarrHelo + NearZero) ) ) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergHelo = min(1.0, PerCent * cCovSpHelo * aDEmergHelo) ;
/* fraction of water surface covered by emergent biomass [-] */
aCovHelo = min(100.0, cCovSpHelo * aDShootHelo) ;
/* percentage of water surface covered by plant species [%] */
aPVeg = sPElod + sPChar + sPCera + sPLemn + sPNymp + sPHelo ;
/* total P in vegetation [gP/m2/day] */
aNVeg = sNElod + sNChar + sNCera + sNLemn + sNNymp + sNHelo ;
/* total N in vegetation [gN/m2/day] */
afCovSurfVeg = min(1.0, afCovSurfElod + afCovSurfChar + afCovSurfCera + afCovSurfLemn + afCovSurfNymp + afCovSurfHelo) ;
/* fraction of water surface covered by plant species [-] */
afCovEmergVeg = min(1.0, afCovEmergElod + afCovEmergChar + afCovEmergCera + afCovEmergLemn + afCovEmergNymp + afCovEmergHelo) ;
/* fraction emergent coverage [-] */
aExtVeg = aExtElod + aExtChar + aExtCera + aExtLemn + aExtNymp + aExtHelo ;
/* extinction coefficient of total vegetation [day-1] */
aExtCoef = aExtCoefOpen + aExtVeg ;
/* extinction coefficient of water colum including vegetation [m-1] */
aLPARBot = uLPAR0 * exp(- aExtCoef * sDepthW) ;
/* light at the bottom [W/m2_PAR] */
rPDElod = sPElod / (sDElod+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDElod = sNElod / (sDElod+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrElod = kMigrElod * (cDElodIn - sDElod) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrElod = kMigrElod * (cPDElod0* cDElodIn - sPElod) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrElod = kMigrElod * (cNDElod0* cDElodIn - sNElod) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdElod = ( (cQ10ProdElod )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespElod = ( (cQ10RespElod )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrElod = max( 0.0, cVPUptMaxElod * uFunTmProdElod * (cPDElodMax-rPDElod) / (cPDElodMax-cPDElodMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptElodW = sPO4W * aVPUptMaxCrElod / (aVPUptMaxCrElod / cAffPUptElod + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptElodW = aVPUptElodW * (aDSubElod + aDFloatElod) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptElodS = oPO4S * aVPUptMaxCrElod / (aVPUptMaxCrElod / cAffPUptElod + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptElodS = aVPUptElodS * aDRootElod ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptElod = tPUptElodW + tPUptElodS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrElod = max( 0.0, cVNUptMaxElod * uFunTmProdElod * (cNDElodMax - rNDElod) / (cNDElodMax - cNDElodMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptElod = aVNUptMaxCrElod / cAffNUptElod ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptElodW = oNDissW * aVNUptMaxCrElod / (ahNUptElod + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptElodW = aVNUptElodW * (aDSubElod + aDFloatElod) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptElodW = sNH4W * sNO3W / ((ahNUptElod + sNH4W) * (ahNUptElod + sNO3W +NearZero)) + sNH4W * ahNUptElod / ((sNH4W + sNO3W +NearZero) * (ahNUptElod + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4ElodW = afNH4UptElodW * tNUptElodW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3ElodW = tNUptElodW - tNUptNH4ElodW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptElodS = oNDissS * aVNUptMaxCrElod / (ahNUptElod + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptElodS = aVNUptElodS * aDRootElod ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptElodS = oNH4S * oNO3S / ((ahNUptElod + oNH4S +NearZero) * (ahNUptElod + oNO3S +NearZero)) + oNH4S * ahNUptElod / ((oNH4S + oNO3S+NearZero) * (ahNUptElod + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4ElodS = afNH4UptElodS * tNUptElodS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3ElodS = tNUptElodS - tNUptNH4ElodS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptElod = tNUptElodW + tNUptElodS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Elod = uLPAR0 * exp(- aExtCoefOpen * aDepth1Elod) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Elod = aLPAR1Elod * exp(- aExtCoef * (aDepth2Elod - aDepth1Elod)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLElod = hLRefElod * uFunTmProdElod ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootElod = fEmergElod + fFloatElod * (1.0 - afCovEmergVeg) + bfSubElod * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Elod / uhLElod) / (1.0 + aLPAR2Elod / uhLElod)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLElod = ufDay * bfShootElod * aLLimShootElod * uFunTmProdElod * cMuMaxElod ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimElod = max(0.0, (1.0 - cPDElodMin / (rPDElod + NearZero)) * cPDElodMax / (cPDElodMax - cPDElodMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimElod = max(0.0, (1.0 - cNDElodMin / (rNDElod + NearZero)) * cNDElodMax / (cNDElodMax - cNDElodMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimElod = min( aPLimElod, aNLimElod) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuElod = aMuTmLElod * aNutLimElod ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinElod) {
   bkMortElod = kMortElodSum ;
} else if (Day < cDayWinElod + cLengMort) {
   bkMortElod = - ln(fWinElod) / cLengMort ;
} else {
   bkMortElod = kMortElodSum ;
}
/* mortality of plant species [day-1] */
akDIncrElod = aMuTmLElod - kDRespElod * uFunTmRespElod - bkMortElod ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvElod = max(0.0, akDIncrElod / (cDCarrElod+NearZero) * sDElod*sDElod) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdElod = aMuElod / cMuMaxElod * tDEnvElod ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdElod = max(0.0, aMuElod * sDElod - tDEnvProdElod) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubElod = bfSubElod * tDProdElod ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespElod = kDRespElod * uFunTmRespElod * sDElod ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortElod = tDEnvElod - tDEnvProdElod ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortElod = bkMortElod * sDElod + tDEnvMortElod ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortElodW = fDetWMortElod * (1.0 - bfRootElod) * tDMortElod ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortElodS = tDMortElod - tDMortElodW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazElodBird = cPrefElodBird * sDElod / (hDVegBird + sDElod) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazElodBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManElod = -ln(1.0 - fManElod) / cLengMan ;
} else {
   bkManElod = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManElod = bkManElod * sDElod ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManElod = rPDElod * tDManElod ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManElod = rNDElod * tDManElod ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedElod = tDMigrElod + tDProdElod - tDRespElod - tDMortElod - tDGrazElodBird - tDManElod ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdElod = molO2molC * cCPerDW * tDProdElod ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespElodW = molO2molC * cCPerDW * bfSubElod * tDRespElod * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespElodS = molO2molC * cCPerDW * bfRootElod * tDRespElod * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdElodS = min (tO2RespElodS, tO2ProdElod) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdElodW = min( tO2ProdElod - tO2ProdElodS, bfSubElod * tO2ProdElod) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3ElodW = O2PerNO3 * molO2molN * bfSubElod * tNUptNO3ElodW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3ElodS = O2PerNO3 * molO2molN * tNUptNO3ElodS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrElod = (2.0*rPDElod) / (cPDElodMax + rPDElod) * rPDElod * tDRespElod ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrElodS = bfRootElod * tPExcrElod ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrElodW = tPExcrElod - tPExcrElodS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortElod = rPDElod * tDMortElod ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortElodPO4 = fDissMortVeg * tPMortElod ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortElodPO4S = bfRootElod * tPMortElodPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortElodPO4W = tPMortElodPO4 - tPMortElodPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortElodDet = tPMortElod - tPMortElodPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortElodDetW = fDetWMortElod * (1.0 - bfRootElod) * tPMortElodDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortElodDetS = tPMortElodDet - tPMortElodDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazElodBird = rPDElod * tDGrazElodBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedElod = tPMigrElod + tPUptElod - tPExcrElod - tPMortElod - tPGrazElodBird - tPManElod ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrElod = (2.0*rNDElod) / (cNDElodMax + rNDElod) * rNDElod * tDRespElod ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrElodS = bfRootElod * tNExcrElod ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrElodW = tNExcrElod - tNExcrElodS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortElod = rNDElod * tDMortElod ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortElodNH4 = fDissMortVeg * tNMortElod ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortElodNH4S = bfRootElod * tNMortElodNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortElodNH4W = tNMortElodNH4 - tNMortElodNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortElodDet = tNMortElod - tNMortElodNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortElodDetW = fDetWMortElod * (1.0 - bfRootElod) * tNMortElodDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortElodDetS = tNMortElodDet - tNMortElodDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazElodBird = rNDElod * tDGrazElodBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedElod = tNMigrElod + tNUptElod - tNExcrElod - tNMortElod - tNGrazElodBird - tNManElod ;
/* derivative of vegetation N [gN/m2/day] */
rPDChar = sPChar / (sDChar+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDChar = sNChar / (sDChar+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrChar = kMigrChar * (cDCharIn - sDChar) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrChar = kMigrChar * (cPDChar0* cDCharIn - sPChar) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrChar = kMigrChar * (cNDChar0* cDCharIn - sNChar) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdChar = ( (cQ10ProdChar )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespChar = ( (cQ10RespChar )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrChar = max( 0.0, cVPUptMaxChar * uFunTmProdChar * (cPDCharMax-rPDChar) / (cPDCharMax-cPDCharMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptCharW = sPO4W * aVPUptMaxCrChar / (aVPUptMaxCrChar / cAffPUptChar + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptCharW = aVPUptCharW * (aDSubChar + aDFloatChar) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptCharS = oPO4S * aVPUptMaxCrChar / (aVPUptMaxCrChar / cAffPUptChar + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptCharS = aVPUptCharS * aDRootChar ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptChar = tPUptCharW + tPUptCharS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrChar = max( 0.0, cVNUptMaxChar * uFunTmProdChar * (cNDCharMax - rNDChar) / (cNDCharMax - cNDCharMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptChar = aVNUptMaxCrChar / cAffNUptChar ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptCharW = oNDissW * aVNUptMaxCrChar / (ahNUptChar + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptCharW = aVNUptCharW * (aDSubChar + aDFloatChar) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptCharW = sNH4W * sNO3W / ((ahNUptChar + sNH4W) * (ahNUptChar + sNO3W +NearZero)) + sNH4W * ahNUptChar / ((sNH4W + sNO3W +NearZero) * (ahNUptChar + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4CharW = afNH4UptCharW * tNUptCharW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3CharW = tNUptCharW - tNUptNH4CharW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptCharS = oNDissS * aVNUptMaxCrChar / (ahNUptChar + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptCharS = aVNUptCharS * aDRootChar ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptCharS = oNH4S * oNO3S / ((ahNUptChar + oNH4S +NearZero) * (ahNUptChar + oNO3S +NearZero)) + oNH4S * ahNUptChar / ((oNH4S + oNO3S+NearZero) * (ahNUptChar + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4CharS = afNH4UptCharS * tNUptCharS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3CharS = tNUptCharS - tNUptNH4CharS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptChar = tNUptCharW + tNUptCharS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Char = uLPAR0 * exp(- aExtCoefOpen * aDepth1Char) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Char = aLPAR1Char * exp(- aExtCoef * (aDepth2Char - aDepth1Char)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLChar = hLRefChar * uFunTmProdChar ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootChar = fEmergChar + fFloatChar * (1.0 - afCovEmergVeg) + bfSubChar * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Char / uhLChar) / (1.0 + aLPAR2Char / uhLChar)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLChar = ufDay * bfShootChar * aLLimShootChar * uFunTmProdChar * cMuMaxChar ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimChar = max(0.0, (1.0 - cPDCharMin / (rPDChar + NearZero)) * cPDCharMax / (cPDCharMax - cPDCharMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimChar = max(0.0, (1.0 - cNDCharMin / (rNDChar + NearZero)) * cNDCharMax / (cNDCharMax - cNDCharMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimChar = min( aPLimChar, aNLimChar) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuChar = aMuTmLChar * aNutLimChar ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinChar) {
   bkMortChar = kMortCharSum ;
} else if (Day < cDayWinChar + cLengMort) {
   bkMortChar = - ln(fWinChar) / cLengMort ;
} else {
   bkMortChar = kMortCharSum ;
}
/* mortality of plant species [day-1] */
akDIncrChar = aMuTmLChar - kDRespChar * uFunTmRespChar - bkMortChar ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvChar = max(0.0, akDIncrChar / (cDCarrChar+NearZero) * sDChar*sDChar) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdChar = aMuChar / cMuMaxChar * tDEnvChar ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdChar = max(0.0, aMuChar * sDChar - tDEnvProdChar) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubChar = bfSubChar * tDProdChar ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespChar = kDRespChar * uFunTmRespChar * sDChar ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortChar = tDEnvChar - tDEnvProdChar ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortChar = bkMortChar * sDChar + tDEnvMortChar ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortCharW = fDetWMortChar * (1.0 - bfRootChar) * tDMortChar ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortCharS = tDMortChar - tDMortCharW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazCharBird = cPrefCharBird * sDChar / (hDVegBird + sDChar) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazCharBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManChar = -ln(1.0 - fManChar) / cLengMan ;
} else {
   bkManChar = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManChar = bkManChar * sDChar ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManChar = rPDChar * tDManChar ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManChar = rNDChar * tDManChar ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedChar = tDMigrChar + tDProdChar - tDRespChar - tDMortChar - tDGrazCharBird - tDManChar ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdChar = molO2molC * cCPerDW * tDProdChar ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespCharW = molO2molC * cCPerDW * bfSubChar * tDRespChar * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespCharS = molO2molC * cCPerDW * bfRootChar * tDRespChar * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdCharS = min (tO2RespCharS, tO2ProdChar) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdCharW = min( tO2ProdChar - tO2ProdCharS, bfSubChar * tO2ProdChar) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3CharW = O2PerNO3 * molO2molN * bfSubChar * tNUptNO3CharW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3CharS = O2PerNO3 * molO2molN * tNUptNO3CharS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrChar = (2.0*rPDChar) / (cPDCharMax + rPDChar) * rPDChar * tDRespChar ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrCharS = bfRootChar * tPExcrChar ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrCharW = tPExcrChar - tPExcrCharS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortChar = rPDChar * tDMortChar ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortCharPO4 = fDissMortVeg * tPMortChar ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortCharPO4S = bfRootChar * tPMortCharPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortCharPO4W = tPMortCharPO4 - tPMortCharPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortCharDet = tPMortChar - tPMortCharPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortCharDetW = fDetWMortChar * (1.0 - bfRootChar) * tPMortCharDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortCharDetS = tPMortCharDet - tPMortCharDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazCharBird = rPDChar * tDGrazCharBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedChar = tPMigrChar + tPUptChar - tPExcrChar - tPMortChar - tPGrazCharBird - tPManChar ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrChar = (2.0*rNDChar) / (cNDCharMax + rNDChar) * rNDChar * tDRespChar ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrCharS = bfRootChar * tNExcrChar ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrCharW = tNExcrChar - tNExcrCharS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortChar = rNDChar * tDMortChar ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortCharNH4 = fDissMortVeg * tNMortChar ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortCharNH4S = bfRootChar * tNMortCharNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortCharNH4W = tNMortCharNH4 - tNMortCharNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortCharDet = tNMortChar - tNMortCharNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortCharDetW = fDetWMortChar * (1.0 - bfRootChar) * tNMortCharDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortCharDetS = tNMortCharDet - tNMortCharDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazCharBird = rNDChar * tDGrazCharBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedChar = tNMigrChar + tNUptChar - tNExcrChar - tNMortChar - tNGrazCharBird - tNManChar ;
/* derivative of vegetation N [gN/m2/day] */
rPDCera = sPCera / (sDCera+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDCera = sNCera / (sDCera+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrCera = kMigrCera * (cDCeraIn - sDCera) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrCera = kMigrCera * (cPDCera0* cDCeraIn - sPCera) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrCera = kMigrCera * (cNDCera0* cDCeraIn - sNCera) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdCera = ( (cQ10ProdCera )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespCera = ( (cQ10RespCera )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrCera = max( 0.0, cVPUptMaxCera * uFunTmProdCera * (cPDCeraMax-rPDCera) / (cPDCeraMax-cPDCeraMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptCeraW = sPO4W * aVPUptMaxCrCera / (aVPUptMaxCrCera / cAffPUptCera + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptCeraW = aVPUptCeraW * (aDSubCera + aDFloatCera) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptCeraS = oPO4S * aVPUptMaxCrCera / (aVPUptMaxCrCera / cAffPUptCera + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptCeraS = aVPUptCeraS * aDRootCera ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptCera = tPUptCeraW + tPUptCeraS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrCera = max( 0.0, cVNUptMaxCera * uFunTmProdCera * (cNDCeraMax - rNDCera) / (cNDCeraMax - cNDCeraMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptCera = aVNUptMaxCrCera / cAffNUptCera ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptCeraW = oNDissW * aVNUptMaxCrCera / (ahNUptCera + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptCeraW = aVNUptCeraW * (aDSubCera + aDFloatCera) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptCeraW = sNH4W * sNO3W / ((ahNUptCera + sNH4W) * (ahNUptCera + sNO3W +NearZero)) + sNH4W * ahNUptCera / ((sNH4W + sNO3W +NearZero) * (ahNUptCera + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4CeraW = afNH4UptCeraW * tNUptCeraW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3CeraW = tNUptCeraW - tNUptNH4CeraW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptCeraS = oNDissS * aVNUptMaxCrCera / (ahNUptCera + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptCeraS = aVNUptCeraS * aDRootCera ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptCeraS = oNH4S * oNO3S / ((ahNUptCera + oNH4S +NearZero) * (ahNUptCera + oNO3S +NearZero)) + oNH4S * ahNUptCera / ((oNH4S + oNO3S+NearZero) * (ahNUptCera + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4CeraS = afNH4UptCeraS * tNUptCeraS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3CeraS = tNUptCeraS - tNUptNH4CeraS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptCera = tNUptCeraW + tNUptCeraS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Cera = uLPAR0 * exp(- aExtCoefOpen * aDepth1Cera) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Cera = aLPAR1Cera * exp(- aExtCoef * (aDepth2Cera - aDepth1Cera)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLCera = hLRefCera * uFunTmProdCera ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootCera = fEmergCera + fFloatCera * (1.0 - afCovEmergVeg) + bfSubCera * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Cera / uhLCera) / (1.0 + aLPAR2Cera / uhLCera)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLCera = ufDay * bfShootCera * aLLimShootCera * uFunTmProdCera * cMuMaxCera ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimCera = max(0.0, (1.0 - cPDCeraMin / (rPDCera + NearZero)) * cPDCeraMax / (cPDCeraMax - cPDCeraMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimCera = max(0.0, (1.0 - cNDCeraMin / (rNDCera + NearZero)) * cNDCeraMax / (cNDCeraMax - cNDCeraMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimCera = min( aPLimCera, aNLimCera) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuCera = aMuTmLCera * aNutLimCera ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinCera) {
   bkMortCera = kMortCeraSum ;
} else if (Day < cDayWinCera + cLengMort) {
   bkMortCera = - ln(fWinCera) / cLengMort ;
} else {
   bkMortCera = kMortCeraSum ;
}
/* mortality of plant species [day-1] */
akDIncrCera = aMuTmLCera - kDRespCera * uFunTmRespCera - bkMortCera ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvCera = max(0.0, akDIncrCera / (cDCarrCera+NearZero) * sDCera*sDCera) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdCera = aMuCera / cMuMaxCera * tDEnvCera ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdCera = max(0.0, aMuCera * sDCera - tDEnvProdCera) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubCera = bfSubCera * tDProdCera ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespCera = kDRespCera * uFunTmRespCera * sDCera ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortCera = tDEnvCera - tDEnvProdCera ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortCera = bkMortCera * sDCera + tDEnvMortCera ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortCeraW = fDetWMortCera * (1.0 - bfRootCera) * tDMortCera ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortCeraS = tDMortCera - tDMortCeraW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazCeraBird = cPrefCeraBird * sDCera / (hDVegBird + sDCera) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazCeraBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManCera = -ln(1.0 - fManCera) / cLengMan ;
} else {
   bkManCera = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManCera = bkManCera * sDCera ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManCera = rPDCera * tDManCera ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManCera = rNDCera * tDManCera ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedCera = tDMigrCera + tDProdCera - tDRespCera - tDMortCera - tDGrazCeraBird - tDManCera ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdCera = molO2molC * cCPerDW * tDProdCera ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespCeraW = molO2molC * cCPerDW * bfSubCera * tDRespCera * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespCeraS = molO2molC * cCPerDW * bfRootCera * tDRespCera * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdCeraS = min (tO2RespCeraS, tO2ProdCera) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdCeraW = min( tO2ProdCera - tO2ProdCeraS, bfSubCera * tO2ProdCera) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3CeraW = O2PerNO3 * molO2molN * bfSubCera * tNUptNO3CeraW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3CeraS = O2PerNO3 * molO2molN * tNUptNO3CeraS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrCera = (2.0*rPDCera) / (cPDCeraMax + rPDCera) * rPDCera * tDRespCera ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrCeraS = bfRootCera * tPExcrCera ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrCeraW = tPExcrCera - tPExcrCeraS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortCera = rPDCera * tDMortCera ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortCeraPO4 = fDissMortVeg * tPMortCera ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortCeraPO4S = bfRootCera * tPMortCeraPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortCeraPO4W = tPMortCeraPO4 - tPMortCeraPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortCeraDet = tPMortCera - tPMortCeraPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortCeraDetW = fDetWMortCera * (1.0 - bfRootCera) * tPMortCeraDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortCeraDetS = tPMortCeraDet - tPMortCeraDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazCeraBird = rPDCera * tDGrazCeraBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedCera = tPMigrCera + tPUptCera - tPExcrCera - tPMortCera - tPGrazCeraBird - tPManCera ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrCera = (2.0*rNDCera) / (cNDCeraMax + rNDCera) * rNDCera * tDRespCera ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrCeraS = bfRootCera * tNExcrCera ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrCeraW = tNExcrCera - tNExcrCeraS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortCera = rNDCera * tDMortCera ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortCeraNH4 = fDissMortVeg * tNMortCera ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortCeraNH4S = bfRootCera * tNMortCeraNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortCeraNH4W = tNMortCeraNH4 - tNMortCeraNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortCeraDet = tNMortCera - tNMortCeraNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortCeraDetW = fDetWMortCera * (1.0 - bfRootCera) * tNMortCeraDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortCeraDetS = tNMortCeraDet - tNMortCeraDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazCeraBird = rNDCera * tDGrazCeraBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedCera = tNMigrCera + tNUptCera - tNExcrCera - tNMortCera - tNGrazCeraBird - tNManCera ;
/* derivative of vegetation N [gN/m2/day] */
rPDLemn = sPLemn / (sDLemn+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDLemn = sNLemn / (sDLemn+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrLemn = kMigrLemn * (cDLemnIn - sDLemn) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrLemn = kMigrLemn * (cPDLemn0* cDLemnIn - sPLemn) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrLemn = kMigrLemn * (cNDLemn0* cDLemnIn - sNLemn) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdLemn = ( (cQ10ProdLemn )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespLemn = ( (cQ10RespLemn )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrLemn = max( 0.0, cVPUptMaxLemn * uFunTmProdLemn * (cPDLemnMax-rPDLemn) / (cPDLemnMax-cPDLemnMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptLemnW = sPO4W * aVPUptMaxCrLemn / (aVPUptMaxCrLemn / cAffPUptLemn + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptLemnW = aVPUptLemnW * (aDSubLemn + aDFloatLemn) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptLemnS = oPO4S * aVPUptMaxCrLemn / (aVPUptMaxCrLemn / cAffPUptLemn + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptLemnS = aVPUptLemnS * aDRootLemn ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptLemn = tPUptLemnW + tPUptLemnS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrLemn = max( 0.0, cVNUptMaxLemn * uFunTmProdLemn * (cNDLemnMax - rNDLemn) / (cNDLemnMax - cNDLemnMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptLemn = aVNUptMaxCrLemn / cAffNUptLemn ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptLemnW = oNDissW * aVNUptMaxCrLemn / (ahNUptLemn + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptLemnW = aVNUptLemnW * (aDSubLemn + aDFloatLemn) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptLemnW = sNH4W * sNO3W / ((ahNUptLemn + sNH4W) * (ahNUptLemn + sNO3W +NearZero)) + sNH4W * ahNUptLemn / ((sNH4W + sNO3W +NearZero) * (ahNUptLemn + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4LemnW = afNH4UptLemnW * tNUptLemnW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3LemnW = tNUptLemnW - tNUptNH4LemnW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptLemnS = oNDissS * aVNUptMaxCrLemn / (ahNUptLemn + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptLemnS = aVNUptLemnS * aDRootLemn ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptLemnS = oNH4S * oNO3S / ((ahNUptLemn + oNH4S +NearZero) * (ahNUptLemn + oNO3S +NearZero)) + oNH4S * ahNUptLemn / ((oNH4S + oNO3S+NearZero) * (ahNUptLemn + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4LemnS = afNH4UptLemnS * tNUptLemnS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3LemnS = tNUptLemnS - tNUptNH4LemnS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptLemn = tNUptLemnW + tNUptLemnS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Lemn = uLPAR0 * exp(- aExtCoefOpen * aDepth1Lemn) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Lemn = aLPAR1Lemn * exp(- aExtCoef * (aDepth2Lemn - aDepth1Lemn)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLLemn = hLRefLemn * uFunTmProdLemn ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootLemn = fEmergLemn + fFloatLemn * (1.0 - afCovEmergVeg) + bfSubLemn * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Lemn / uhLLemn) / (1.0 + aLPAR2Lemn / uhLLemn)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLLemn = ufDay * bfShootLemn * aLLimShootLemn * uFunTmProdLemn * cMuMaxLemn ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimLemn = max(0.0, (1.0 - cPDLemnMin / (rPDLemn + NearZero)) * cPDLemnMax / (cPDLemnMax - cPDLemnMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimLemn = max(0.0, (1.0 - cNDLemnMin / (rNDLemn + NearZero)) * cNDLemnMax / (cNDLemnMax - cNDLemnMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimLemn = min( aPLimLemn, aNLimLemn) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuLemn = aMuTmLLemn * aNutLimLemn ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinLemn) {
   bkMortLemn = kMortLemnSum ;
} else if (Day < cDayWinLemn + cLengMort) {
   bkMortLemn = - ln(fWinLemn) / cLengMort ;
} else {
   bkMortLemn = kMortLemnSum ;
}
/* mortality of plant species [day-1] */
akDIncrLemn = aMuTmLLemn - kDRespLemn * uFunTmRespLemn - bkMortLemn ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvLemn = max(0.0, akDIncrLemn / (cDCarrLemn+NearZero) * sDLemn*sDLemn) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdLemn = aMuLemn / cMuMaxLemn * tDEnvLemn ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdLemn = max(0.0, aMuLemn * sDLemn - tDEnvProdLemn) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubLemn = bfSubLemn * tDProdLemn ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespLemn = kDRespLemn * uFunTmRespLemn * sDLemn ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortLemn = tDEnvLemn - tDEnvProdLemn ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortLemn = bkMortLemn * sDLemn + tDEnvMortLemn ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortLemnW = fDetWMortLemn * (1.0 - bfRootLemn) * tDMortLemn ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortLemnS = tDMortLemn - tDMortLemnW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazLemnBird = cPrefLemnBird * sDLemn / (hDVegBird + sDLemn) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazLemnBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManLemn = -ln(1.0 - fManLemn) / cLengMan ;
} else {
   bkManLemn = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManLemn = bkManLemn * sDLemn ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManLemn = rPDLemn * tDManLemn ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManLemn = rNDLemn * tDManLemn ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedLemn = tDMigrLemn + tDProdLemn - tDRespLemn - tDMortLemn - tDGrazLemnBird - tDManLemn ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdLemn = molO2molC * cCPerDW * tDProdLemn ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespLemnW = molO2molC * cCPerDW * bfSubLemn * tDRespLemn * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespLemnS = molO2molC * cCPerDW * bfRootLemn * tDRespLemn * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdLemnS = min (tO2RespLemnS, tO2ProdLemn) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdLemnW = min( tO2ProdLemn - tO2ProdLemnS, bfSubLemn * tO2ProdLemn) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3LemnW = O2PerNO3 * molO2molN * bfSubLemn * tNUptNO3LemnW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3LemnS = O2PerNO3 * molO2molN * tNUptNO3LemnS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrLemn = (2.0*rPDLemn) / (cPDLemnMax + rPDLemn) * rPDLemn * tDRespLemn ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrLemnS = bfRootLemn * tPExcrLemn ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrLemnW = tPExcrLemn - tPExcrLemnS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortLemn = rPDLemn * tDMortLemn ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortLemnPO4 = fDissMortVeg * tPMortLemn ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortLemnPO4S = bfRootLemn * tPMortLemnPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortLemnPO4W = tPMortLemnPO4 - tPMortLemnPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortLemnDet = tPMortLemn - tPMortLemnPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortLemnDetW = fDetWMortLemn * (1.0 - bfRootLemn) * tPMortLemnDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortLemnDetS = tPMortLemnDet - tPMortLemnDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazLemnBird = rPDLemn * tDGrazLemnBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedLemn = tPMigrLemn + tPUptLemn - tPExcrLemn - tPMortLemn - tPGrazLemnBird - tPManLemn ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrLemn = (2.0*rNDLemn) / (cNDLemnMax + rNDLemn) * rNDLemn * tDRespLemn ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrLemnS = bfRootLemn * tNExcrLemn ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrLemnW = tNExcrLemn - tNExcrLemnS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortLemn = rNDLemn * tDMortLemn ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortLemnNH4 = fDissMortVeg * tNMortLemn ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortLemnNH4S = bfRootLemn * tNMortLemnNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortLemnNH4W = tNMortLemnNH4 - tNMortLemnNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortLemnDet = tNMortLemn - tNMortLemnNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortLemnDetW = fDetWMortLemn * (1.0 - bfRootLemn) * tNMortLemnDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortLemnDetS = tNMortLemnDet - tNMortLemnDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazLemnBird = rNDLemn * tDGrazLemnBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedLemn = tNMigrLemn + tNUptLemn - tNExcrLemn - tNMortLemn - tNGrazLemnBird - tNManLemn ;
/* derivative of vegetation N [gN/m2/day] */
rPDNymp = sPNymp / (sDNymp+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDNymp = sNNymp / (sDNymp+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrNymp = kMigrNymp * (cDNympIn - sDNymp) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrNymp = kMigrNymp * (cPDNymp0* cDNympIn - sPNymp) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrNymp = kMigrNymp * (cNDNymp0* cDNympIn - sNNymp) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdNymp = ( (cQ10ProdNymp )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespNymp = ( (cQ10RespNymp )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrNymp = max( 0.0, cVPUptMaxNymp * uFunTmProdNymp * (cPDNympMax-rPDNymp) / (cPDNympMax-cPDNympMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptNympW = sPO4W * aVPUptMaxCrNymp / (aVPUptMaxCrNymp / cAffPUptNymp + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptNympW = aVPUptNympW * (aDSubNymp + aDFloatNymp) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptNympS = oPO4S * aVPUptMaxCrNymp / (aVPUptMaxCrNymp / cAffPUptNymp + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptNympS = aVPUptNympS * aDRootNymp ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptNymp = tPUptNympW + tPUptNympS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrNymp = max( 0.0, cVNUptMaxNymp * uFunTmProdNymp * (cNDNympMax - rNDNymp) / (cNDNympMax - cNDNympMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptNymp = aVNUptMaxCrNymp / cAffNUptNymp ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptNympW = oNDissW * aVNUptMaxCrNymp / (ahNUptNymp + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptNympW = aVNUptNympW * (aDSubNymp + aDFloatNymp) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptNympW = sNH4W * sNO3W / ((ahNUptNymp + sNH4W) * (ahNUptNymp + sNO3W +NearZero)) + sNH4W * ahNUptNymp / ((sNH4W + sNO3W +NearZero) * (ahNUptNymp + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4NympW = afNH4UptNympW * tNUptNympW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3NympW = tNUptNympW - tNUptNH4NympW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptNympS = oNDissS * aVNUptMaxCrNymp / (ahNUptNymp + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptNympS = aVNUptNympS * aDRootNymp ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptNympS = oNH4S * oNO3S / ((ahNUptNymp + oNH4S +NearZero) * (ahNUptNymp + oNO3S +NearZero)) + oNH4S * ahNUptNymp / ((oNH4S + oNO3S+NearZero) * (ahNUptNymp + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4NympS = afNH4UptNympS * tNUptNympS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3NympS = tNUptNympS - tNUptNH4NympS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNymp = tNUptNympW + tNUptNympS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Nymp = uLPAR0 * exp(- aExtCoefOpen * aDepth1Nymp) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Nymp = aLPAR1Nymp * exp(- aExtCoef * (aDepth2Nymp - aDepth1Nymp)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLNymp = hLRefNymp * uFunTmProdNymp ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootNymp = fEmergNymp + fFloatNymp * (1.0 - afCovEmergVeg) + bfSubNymp * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Nymp / uhLNymp) / (1.0 + aLPAR2Nymp / uhLNymp)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLNymp = ufDay * bfShootNymp * aLLimShootNymp * uFunTmProdNymp * cMuMaxNymp ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimNymp = max(0.0, (1.0 - cPDNympMin / (rPDNymp + NearZero)) * cPDNympMax / (cPDNympMax - cPDNympMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimNymp = max(0.0, (1.0 - cNDNympMin / (rNDNymp + NearZero)) * cNDNympMax / (cNDNympMax - cNDNympMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimNymp = min( aPLimNymp, aNLimNymp) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuNymp = aMuTmLNymp * aNutLimNymp ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinNymp) {
   bkMortNymp = kMortNympSum ;
} else if (Day < cDayWinNymp + cLengMort) {
   bkMortNymp = - ln(fWinNymp) / cLengMort ;
} else {
   bkMortNymp = kMortNympSum ;
}
/* mortality of plant species [day-1] */
akDIncrNymp = aMuTmLNymp - kDRespNymp * uFunTmRespNymp - bkMortNymp ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvNymp = max(0.0, akDIncrNymp / (cDCarrNymp+NearZero) * sDNymp*sDNymp) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdNymp = aMuNymp / cMuMaxNymp * tDEnvNymp ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdNymp = max(0.0, aMuNymp * sDNymp - tDEnvProdNymp) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubNymp = bfSubNymp * tDProdNymp ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespNymp = kDRespNymp * uFunTmRespNymp * sDNymp ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortNymp = tDEnvNymp - tDEnvProdNymp ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortNymp = bkMortNymp * sDNymp + tDEnvMortNymp ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortNympW = fDetWMortNymp * (1.0 - bfRootNymp) * tDMortNymp ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortNympS = tDMortNymp - tDMortNympW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazNympBird = cPrefNympBird * sDNymp / (hDVegBird + sDNymp) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazNympBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManNymp = -ln(1.0 - fManNymp) / cLengMan ;
} else {
   bkManNymp = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManNymp = bkManNymp * sDNymp ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManNymp = rPDNymp * tDManNymp ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManNymp = rNDNymp * tDManNymp ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedNymp = tDMigrNymp + tDProdNymp - tDRespNymp - tDMortNymp - tDGrazNympBird - tDManNymp ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdNymp = molO2molC * cCPerDW * tDProdNymp ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespNympW = molO2molC * cCPerDW * bfSubNymp * tDRespNymp * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespNympS = molO2molC * cCPerDW * bfRootNymp * tDRespNymp * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdNympS = min (tO2RespNympS, tO2ProdNymp) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdNympW = min( tO2ProdNymp - tO2ProdNympS, bfSubNymp * tO2ProdNymp) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3NympW = O2PerNO3 * molO2molN * bfSubNymp * tNUptNO3NympW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3NympS = O2PerNO3 * molO2molN * tNUptNO3NympS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrNymp = (2.0*rPDNymp) / (cPDNympMax + rPDNymp) * rPDNymp * tDRespNymp ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrNympS = bfRootNymp * tPExcrNymp ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrNympW = tPExcrNymp - tPExcrNympS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortNymp = rPDNymp * tDMortNymp ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortNympPO4 = fDissMortVeg * tPMortNymp ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortNympPO4S = bfRootNymp * tPMortNympPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortNympPO4W = tPMortNympPO4 - tPMortNympPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortNympDet = tPMortNymp - tPMortNympPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortNympDetW = fDetWMortNymp * (1.0 - bfRootNymp) * tPMortNympDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortNympDetS = tPMortNympDet - tPMortNympDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazNympBird = rPDNymp * tDGrazNympBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedNymp = tPMigrNymp + tPUptNymp - tPExcrNymp - tPMortNymp - tPGrazNympBird - tPManNymp ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrNymp = (2.0*rNDNymp) / (cNDNympMax + rNDNymp) * rNDNymp * tDRespNymp ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrNympS = bfRootNymp * tNExcrNymp ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrNympW = tNExcrNymp - tNExcrNympS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortNymp = rNDNymp * tDMortNymp ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortNympNH4 = fDissMortVeg * tNMortNymp ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortNympNH4S = bfRootNymp * tNMortNympNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortNympNH4W = tNMortNympNH4 - tNMortNympNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortNympDet = tNMortNymp - tNMortNympNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortNympDetW = fDetWMortNymp * (1.0 - bfRootNymp) * tNMortNympDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortNympDetS = tNMortNympDet - tNMortNympDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazNympBird = rNDNymp * tDGrazNympBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedNymp = tNMigrNymp + tNUptNymp - tNExcrNymp - tNMortNymp - tNGrazNympBird - tNManNymp ;
/* derivative of vegetation N [gN/m2/day] */
rPDHelo = sPHelo / (sDHelo+NearZero) ;
/* P/DW ratio of vegetation [gP/gDW] */
rNDHelo = sNHelo / (sDHelo+NearZero) ;
/* N/DW ratio of vegetation [gN/gDW] */
tDMigrHelo = kMigrHelo * (cDHeloIn - sDHelo) ;
/* migration flux of vegetation DW [gDW/m2/day] */
tPMigrHelo = kMigrHelo * (cPDHelo0* cDHeloIn - sPHelo) ;
/* migration flux of vegetation P [gP/m2/day] */
tNMigrHelo = kMigrHelo * (cNDHelo0* cDHeloIn - sNHelo) ;
/* migration flux of vegetation N [gN/m2/day] */
uFunTmProdHelo = ( (cQ10ProdHelo )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespHelo = ( (cQ10RespHelo )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
aVPUptMaxCrHelo = max( 0.0, cVPUptMaxHelo * uFunTmProdHelo * (cPDHeloMax-rPDHelo) / (cPDHeloMax-cPDHeloMin) ) ;
/* maximum P uptake rate of vegetation corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptHeloW = sPO4W * aVPUptMaxCrHelo / (aVPUptMaxCrHelo / cAffPUptHelo + sPO4W) ;
/* P uptake rate by submerged and floating parts [gP/gDW/day] */
tPUptHeloW = aVPUptHeloW * (aDSubHelo + aDFloatHelo) ;
/* P uptake from water by submerged and floating parts [gP/m2/day] */
aVPUptHeloS = oPO4S * aVPUptMaxCrHelo / (aVPUptMaxCrHelo / cAffPUptHelo + oPO4S) ;
/* P uptake rate by roots [gP/gDW/day] */
tPUptHeloS = aVPUptHeloS * aDRootHelo ;
/* P uptake from interstitial water by rooted part [gP/m2/day] */
tPUptHelo = tPUptHeloW + tPUptHeloS ;
/* total P uptake by vegetation [gP/m2/day] */
aVNUptMaxCrHelo = max( 0.0, cVNUptMaxHelo * uFunTmProdHelo * (cNDHeloMax - rNDHelo) / (cNDHeloMax - cNDHeloMin)) ;
/* maximum N uptake rate of vegetation corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptHelo = aVNUptMaxCrHelo / cAffNUptHelo ;
/* half-saturation concentration for N uptake [gN/m3] */
aVNUptHeloW = oNDissW * aVNUptMaxCrHelo / (ahNUptHelo + oNDissW) ;
/* N uptake rate by submerged and floating parts [gN/gDW/day] */
tNUptHeloW = aVNUptHeloW * (aDSubHelo + aDFloatHelo) ;
/* N uptake from water by submerged and floating parts [gN/m2/day] */
afNH4UptHeloW = sNH4W * sNO3W / ((ahNUptHelo + sNH4W) * (ahNUptHelo + sNO3W +NearZero)) + sNH4W * ahNUptHelo / ((sNH4W + sNO3W +NearZero) * (ahNUptHelo + sNO3W +NearZero)) ;
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
tNUptNH4HeloW = afNH4UptHeloW * tNUptHeloW ;
/* NH4 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
tNUptNO3HeloW = tNUptHeloW - tNUptNH4HeloW ;
/* NO3 uptake of vegetation from water by submerged and floating parts [gN/m2/day] */
aVNUptHeloS = oNDissS * aVNUptMaxCrHelo / (ahNUptHelo + oNDissS) ;
/* N uptake rate by roots [gN/gDW/day] */
tNUptHeloS = aVNUptHeloS * aDRootHelo ;
/* N uptake from interstitial water by rooted part [gN/m2/day] */
afNH4UptHeloS = oNH4S * oNO3S / ((ahNUptHelo + oNH4S +NearZero) * (ahNUptHelo + oNO3S +NearZero)) + oNH4S * ahNUptHelo / ((oNH4S + oNO3S+NearZero) * (ahNUptHelo + oNO3S+NearZero)) ;
/* fraction ammonium uptake from interstitial water (from WASP model  EPA) [-] */
tNUptNH4HeloS = afNH4UptHeloS * tNUptHeloS ;
/* NH4 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptNO3HeloS = tNUptHeloS - tNUptNH4HeloS ;
/* NO3 uptake from interstitial water by rooted part [gN/m2/day] */
tNUptHelo = tNUptHeloW + tNUptHeloS ;
/* total N uptake by vegetation [gN/m2/day] */
aLPAR1Helo = uLPAR0 * exp(- aExtCoefOpen * aDepth1Helo) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Helo = aLPAR1Helo * exp(- aExtCoef * (aDepth2Helo - aDepth1Helo)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLHelo = hLRefHelo * uFunTmProdHelo ;
/* half-saturation light intensity for veg production at current temperature [W/m2_PAR] */
aLLimShootHelo = fEmergHelo + fFloatHelo * (1.0 - afCovEmergVeg) + bfSubHelo * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Helo / uhLHelo) / (1.0 + aLPAR2Helo / uhLHelo)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLHelo = ufDay * bfShootHelo * aLLimShootHelo * uFunTmProdHelo * cMuMaxHelo ;
/* maximum growth rate at current temperature and light [g_prod/gDW_total_biomass/day] */
aPLimHelo = max(0.0, (1.0 - cPDHeloMin / (rPDHelo + NearZero)) * cPDHeloMax / (cPDHeloMax - cPDHeloMin)) ;
/* P limitation on vegetation growth according to Droop function [-] */
aNLimHelo = max(0.0, (1.0 - cNDHeloMin / (rNDHelo + NearZero)) * cNDHeloMax / (cNDHeloMax - cNDHeloMin)) ;
/* N limitation on vegetation growth according to Droop function [-] */
aNutLimHelo = min( aPLimHelo, aNLimHelo) ;
/* nutrient limitation on veg growth according to Liebigs law of the minimum [-] */
aMuHelo = aMuTmLHelo * aNutLimHelo ;
/* actual growth rate of vegetation [g_prod/gDW_total_biomass/day] */
if (Day < cDayWinHelo) {
   bkMortHelo = kMortHeloSum ;
} else if (Day < cDayWinHelo + cLengMort) {
   bkMortHelo = - ln(fWinHelo) / cLengMort ;
} else {
   bkMortHelo = kMortHeloSum ;
}
/* mortality of plant species [day-1] */
akDIncrHelo = aMuTmLHelo - kDRespHelo * uFunTmRespHelo - bkMortHelo ;
/* net growth rate of vegetation (growth minus respiration minus mortality) [day-1] */
tDEnvHelo = max(0.0, akDIncrHelo / (cDCarrHelo+NearZero) * sDHelo*sDHelo) ;
/* logistic correction of vegetation biomass [gDW/m2/day] */
tDEnvProdHelo = aMuHelo / cMuMaxHelo * tDEnvHelo ;
/* logistic correction of vegetation growth [gDW/m2/day] */
tDProdHelo = max(0.0, aMuHelo * sDHelo - tDEnvProdHelo) ;
/* vegetation growth rate [gDW/m2/day] */
tDProdSubHelo = bfSubHelo * tDProdHelo ;
/* growth rate of submerged parts [gDW/m2/day] */
tDRespHelo = kDRespHelo * uFunTmRespHelo * sDHelo ;
/* dark respiration rate of vegetation [gDW/m2/day] */
tDEnvMortHelo = tDEnvHelo - tDEnvProdHelo ;
/* logistic correction of mortality [gDW/m2/day] */
tDMortHelo = bkMortHelo * sDHelo + tDEnvMortHelo ;
/* vegetation mortality rate [gDW/m2/day] */
tDMortHeloW = fDetWMortHelo * (1.0 - bfRootHelo) * tDMortHelo ;
/* mortality flux becoming water detritus [gDW/m2/day] */
tDMortHeloS = tDMortHelo - tDMortHeloW ;
/* mortality flux becoming sediment detritus [gDW/m2/day] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazHeloBird = cPrefHeloBird * sDHelo / (hDVegBird + sDHelo) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazHeloBird = 0.0 ;
}
/* biomass loss due to grazing of birds [gDW/m2/day] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManHelo = -ln(1.0 - fManHelo) / cLengMan ;
} else {
   bkManHelo = 0.0 ;
}
/* loss rate by mowing during mowing period [day-1] */
tDManHelo = bkManHelo * sDHelo ;
/* vegetation loss of vegetation DW by mowing [gDW/m2/day] */
tPManHelo = rPDHelo * tDManHelo ;
/* vegetation loss of vegetation P by mowing [gP/m2/day] */
tNManHelo = rNDHelo * tDManHelo ;
/* vegetation loss of vegetation N by mowing [gN/m2/day] */
tDBedHelo = tDMigrHelo + tDProdHelo - tDRespHelo - tDMortHelo - tDGrazHeloBird - tDManHelo ;
/* derivative of vegetation DW [gDW/m2/day] */
tO2ProdHelo = molO2molC * cCPerDW * tDProdHelo ;
/* O2 production by vegetation [gO2/m2/day] */
tO2RespHeloW = molO2molC * cCPerDW * bfSubHelo * tDRespHelo * aCorO2BOD ;
/* O2 respiration of submerged part [gO2/m2/day] */
tO2RespHeloS = molO2molC * cCPerDW * bfRootHelo * tDRespHelo * afOxySed ;
/* O2 respiration of rooted part [gO2/m2/day] */
tO2ProdHeloS = min (tO2RespHeloS, tO2ProdHelo) ;
/* O2 production by rooted part [gO2/m2/day] */
tO2ProdHeloW = min( tO2ProdHelo - tO2ProdHeloS, bfSubHelo * tO2ProdHelo) ;
/* O2 production by submerged part [gO2/m2/day] */
tO2UptNO3HeloW = O2PerNO3 * molO2molN * bfSubHelo * tNUptNO3HeloW ;
/* O2 production due to NO3 uptake by submerged part [gO2/m2/day] */
tO2UptNO3HeloS = O2PerNO3 * molO2molN * tNUptNO3HeloS ;
/* O2 production due to NO3 uptake by rooted part [gO2/m2/day] */
tPExcrHelo = (2.0*rPDHelo) / (cPDHeloMax + rPDHelo) * rPDHelo * tDRespHelo ;
/* P excretion by vegetation [gP/m2/day] */
tPExcrHeloS = bfRootHelo * tPExcrHelo ;
/* P excretion by rooted part [gP/m2/day] */
tPExcrHeloW = tPExcrHelo - tPExcrHeloS ;
/* P excretion by submerged part [gP/m2/day] */
tPMortHelo = rPDHelo * tDMortHelo ;
/* P associated with mortality of vegetation [gP/m2/day] */
tPMortHeloPO4 = fDissMortVeg * tPMortHelo ;
/* PO4 associated with mortality of vegetation [gP/m2/day] */
tPMortHeloPO4S = bfRootHelo * tPMortHeloPO4 ;
/* PO4 associated with mortality of rooted part [gP/m2/day] */
tPMortHeloPO4W = tPMortHeloPO4 - tPMortHeloPO4S ;
/* PO4 associated with mortality of submerged part [gP/m2/day] */
tPMortHeloDet = tPMortHelo - tPMortHeloPO4 ;
/* Detritus P associated with mortality of vegetation [gP/m2/day] */
tPMortHeloDetW = fDetWMortHelo * (1.0 - bfRootHelo) * tPMortHeloDet ;
/* Detritus P associated with mortality of rooted part [gP/m2/day] */
tPMortHeloDetS = tPMortHeloDet - tPMortHeloDetW ;
/* Detritus P associated with mortality of submerged part [gP/m2/day] */
tPGrazHeloBird = rPDHelo * tDGrazHeloBird ;
/* P associated with loss of vegetation by herbivorous birds [gP/m2/day] */
tPBedHelo = tPMigrHelo + tPUptHelo - tPExcrHelo - tPMortHelo - tPGrazHeloBird - tPManHelo ;
/* derivative of vegetation P [gP/m2/day] */
tNExcrHelo = (2.0*rNDHelo) / (cNDHeloMax + rNDHelo) * rNDHelo * tDRespHelo ;
/* N excretion by vegetation [gN/m2/day] */
tNExcrHeloS = bfRootHelo * tNExcrHelo ;
/* N excretion by rooted part [gN/m2/day] */
tNExcrHeloW = tNExcrHelo - tNExcrHeloS ;
/* N excretion by submerged part [gN/m2/day] */
tNMortHelo = rNDHelo * tDMortHelo ;
/* N associated with mortality of vegetation [gN/m2/day] */
tNMortHeloNH4 = fDissMortVeg * tNMortHelo ;
/* NH4 associated with mortality of vegetation [gN/m2/day] */
tNMortHeloNH4S = bfRootHelo * tNMortHeloNH4 ;
/* NH4 associated with mortality of rooted part [gN/m2/day] */
tNMortHeloNH4W = tNMortHeloNH4 - tNMortHeloNH4S ;
/* NH4 associated with mortality of submerged part [gN/m2/day] */
tNMortHeloDet = tNMortHelo - tNMortHeloNH4 ;
/* Detritus N associated with mortality of vegetation [gN/m2/day] */
tNMortHeloDetW = fDetWMortHelo * (1.0 - bfRootHelo) * tNMortHeloDet ;
/* Detritus N associated with mortality of rooted part [gN/m2/day] */
tNMortHeloDetS = tNMortHeloDet - tNMortHeloDetW ;
/* Detritus N associated with mortality of submerged part [gN/m2/day] */
tNGrazHeloBird = rNDHelo * tDGrazHeloBird ;
/* N associated with loss of vegetation by herbivorous birds [gN/m2/day] */
tNBedHelo = tNMigrHelo + tNUptHelo - tNExcrHelo - tNMortHelo - tNGrazHeloBird - tNManHelo ;
/* derivative of vegetation N [gN/m2/day] */
tDMigrVeg = tDMigrElod + tDMigrChar + tDMigrCera + tDMigrLemn + tDMigrNymp + tDMigrHelo ;
/* total vegetation migration flux [gDW/m2/day] */
tPMigrVeg = tPMigrElod + tPMigrChar + tPMigrCera + tPMigrLemn + tPMigrNymp + tPMigrHelo ;
/* total vegetation net migration flux [gP/m2/day] */
tNMigrVeg = tNMigrElod + tNMigrChar + tNMigrCera + tNMigrLemn + tNMigrNymp + tNMigrHelo ;
/* total vegetation net migration flux [gN/m2/day] */
tDProdVeg = tDProdElod + tDProdChar + tDProdCera + tDProdLemn + tDProdNymp + tDProdHelo ;
/* total vegetation production [gDW/m2/day] */
tPUptVegW = tPUptElodW + tPUptCharW + tPUptCeraW + tPUptLemnW + tPUptNympW + tPUptHeloW ;
/* total vegetation P uptake from water [gP/m2/day] */
tPUptVegS = tPUptElodS + tPUptCharS + tPUptCeraS + tPUptLemnS + tPUptNympS + tPUptHeloS ;
/* total vegetation P uptake from interstitial water by rooted part [gP/m2/day] */
tNUptNH4VegW = tNUptNH4ElodW + tNUptNH4CharW + tNUptNH4CeraW + tNUptNH4LemnW + tNUptNH4NympW + tNUptNH4HeloW ;
/* total vegetation NH4 uptake from water [gN/m2/day] */
tNUptNH4VegS = tNUptNH4ElodS + tNUptNH4CharS + tNUptNH4CeraS + tNUptNH4LemnS + tNUptNH4NympS + tNUptNH4HeloS ;
/* total vegetation NH4 uptake from sediment [gN/m2/day] */
tNUptNO3VegW = tNUptNO3ElodW + tNUptNO3CharW + tNUptNO3CeraW + tNUptNO3LemnW + tNUptNO3NympW + tNUptNO3HeloW ;
/* total vegetation NO3 uptake from water [gN/m2/day] */
tNUptNO3VegS = tNUptNO3ElodS + tNUptNO3CharS + tNUptNO3CeraS + tNUptNO3LemnS + tNUptNO3NympS + tNUptNO3HeloS ;
/* total vegetation NO3 uptake from sediment [gN/m2/day] */
tDRespVeg = tDRespElod + tDRespChar + tDRespCera + tDRespLemn + tDRespNymp + tDRespHelo ;
/* total vegetation dark respiration [gDW/m2/day] */
tPExcrVegW = tPExcrElodW + tPExcrCharW + tPExcrCeraW + tPExcrLemnW + tPExcrNympW + tPExcrHeloW ;
/* total vegetation P excretion in water [gP/m2/day] */
tPExcrVegS = tPExcrElodS + tPExcrCharS + tPExcrCeraS + tPExcrLemnS + tPExcrNympS + tPExcrHeloS ;
/* total vegetation P excretion in sediment [gP/m2/day] */
tNExcrVegW = tNExcrElodW + tNExcrCharW + tNExcrCeraW + tNExcrLemnW + tNExcrNympW + tNExcrHeloW ;
/* total vegetation N excretion to water [gN/m2/day] */
tNExcrVegS = tNExcrElodS + tNExcrCharS + tNExcrCeraS + tNExcrLemnS + tNExcrNympS + tNExcrHeloS ;
/* total vegetation N excretion to sediment [gN/m2/day] */
tO2ProdVeg = tO2ProdElod + tO2ProdChar + tO2ProdCera + tO2ProdLemn + tO2ProdNymp + tO2ProdHelo ;
/* total vegetation O2 production [gO2/m2/day] */
tO2ProdVegW = tO2ProdElodW + tO2ProdCharW + tO2ProdCeraW + tO2ProdLemnW + tO2ProdNympW + tO2ProdHeloW ;
/* total vegetation O2 used for production [gO2/m2/day] */
tO2ProdVegS = tO2ProdElodS + tO2ProdCharS + tO2ProdCeraS + tO2ProdLemnS + tO2ProdNympS + tO2ProdHeloS ;
/* total vegetation O2 transport to roots [gO2/m2/day] */
tO2RespVegW = tO2RespElodW + tO2RespCharW + tO2RespCeraW + tO2RespLemnW + tO2RespNympW + tO2RespHeloW ;
/* total vegetation submerged O2 respiration [gO2/m2/day] */
tO2RespVegS = tO2RespElodS + tO2RespCharS + tO2RespCeraS + tO2RespLemnS + tO2RespNympS + tO2RespHeloS ;
/* total vegetation root O2 respiration [gO2/m2/day] */
tO2UptNO3VegW = tO2UptNO3ElodW + tO2UptNO3CharW + tO2UptNO3CeraW + tO2UptNO3LemnW + tO2UptNO3NympW + tO2UptNO3HeloW ;
/* total vegetation O2 production to water due to NO3 uptake [gO2/m2/day] */
tO2UptNO3VegS = tO2UptNO3ElodS + tO2UptNO3CharS + tO2UptNO3CeraS + tO2UptNO3LemnS + tO2UptNO3NympS + tO2UptNO3HeloS ;
/* total vegetation O2 production due to NO3 uptake from sediment [gO2/m2/day] */
tDMortVegW = tDMortElodW + tDMortCharW + tDMortCeraW + tDMortLemnW + tDMortNympW + tDMortHeloW ;
/* total vegetation mortality flux becoming water detritus [gDW/m2/day] */
tDMortVegS = tDMortElodS + tDMortCharS + tDMortCeraS + tDMortLemnS + tDMortNympS + tDMortHeloS ;
/* total vegetation mortality flux becoming sediment detritus [gDW/m2/day] */
tPMortVegPO4W = tPMortElodPO4W + tPMortCharPO4W + tPMortCeraPO4W + tPMortLemnPO4W + tPMortNympPO4W + tPMortHeloPO4W ;
/* total vegetation mortality flux becoming dissolved P in water [gP/m2/day] */
tPMortVegDetW = tPMortElodDetW + tPMortCharDetW + tPMortCeraDetW + tPMortLemnDetW + tPMortNympDetW + tPMortHeloDetW ;
/* total vegetation mortality flux becoming detritus P in water [gP/m2/day] */
tPMortVegPO4S = tPMortElodPO4S + tPMortCharPO4S + tPMortCeraPO4S + tPMortLemnPO4S + tPMortNympPO4S + tPMortHeloPO4S ;
/* total vegetation mortality flux becoming dissolved P in sediment [gP/m2/day] */
tPMortVegDetS = tPMortElodDetS + tPMortCharDetS + tPMortCeraDetS + tPMortLemnDetS + tPMortNympDetS + tPMortHeloDetS ;
/* total vegetation mortality flux becoming detritus P in sediment [gP/m2/day] */
tNMortVegNH4W = tNMortElodNH4W + tNMortCharNH4W + tNMortCeraNH4W + tNMortLemnNH4W + tNMortNympNH4W + tNMortHeloNH4W ;
/* total vegetation mortality flux becoming dissolved N in water [gN/m2/day] */
tNMortVegDetW = tNMortElodDetW + tNMortCharDetW + tNMortCeraDetW + tNMortLemnDetW + tNMortNympDetW + tNMortHeloDetW ;
/* total vegetation mortality flux becoming detritus N in water [gN/m2/day] */
tNMortVegNH4S = tNMortElodNH4S + tNMortCharNH4S + tNMortCeraNH4S + tNMortLemnNH4S + tNMortNympNH4S + tNMortHeloNH4S ;
/* total vegetation mortality flux becoming dissolved N in sediment [gN/m2/day] */
tNMortVegDetS = tNMortElodDetS + tNMortCharDetS + tNMortCeraDetS + tNMortLemnDetS + tNMortNympDetS + tNMortHeloDetS ;
/* total vegetation mortality flux becoming detritus N in sediment [gN/m2/day] */
tDGrazVegBird = tDGrazElodBird + tDGrazCharBird + tDGrazCeraBird + tDGrazLemnBird + tDGrazNympBird + tDGrazHeloBird ;
/* total vegetation loss due to herbivorous birds [g/m2/day] */
tPGrazVegBird = tPGrazElodBird + tPGrazCharBird + tPGrazCeraBird + tPGrazLemnBird + tPGrazNympBird + tPGrazHeloBird ;
/* total vegetation P mortality flux by herbivorous birds [gP/m2/day] */
tNGrazVegBird = tNGrazElodBird + tNGrazCharBird + tNGrazCeraBird + tNGrazLemnBird + tNGrazNympBird + tNGrazHeloBird ;
/* total vegetation N mortality flux by herbivorous birds [gN/m2/day] */
tDManVeg = tDManElod + tDManChar + tDManCera + tDManLemn + tDManNymp + tDManHelo ;
/* total vegetation Mowing DW [gDW/m2/day] */
tPManVeg = tPManElod + tPManChar + tPManCera + tPManLemn + tPManNymp + tPManHelo ;
/* total vegetation Mowing P [gP/m2/day] */
tNManVeg = tNManElod + tNManChar + tNManCera + tNManLemn + tNManNymp + tNManHelo ;
/* total vegetation Mowing N [gN/m2/day] */
aCovSub = min(100.0,aCovElod + aCovChar + aCovCera) ;
/* total vegetation submerged coverage [%] */
tDAssVegBird = fDAssBird * tDGrazVegBird ;
/* DW assimilation by herbivorous birds [gDW/m2/day] */
tDEgesBird = tDGrazVegBird - tDAssVegBird ;
/* DW egestion by herbivorous birds [gDW/m2/day] */
tPAssVegBird = fDAssBird * tPGrazVegBird ;
/* P assimilation by herbivorous birds [gP/m2/day] */
tPEgesBird = tPGrazVegBird - tPAssVegBird ;
/* P egestion by herbivorous birds [gP/m2/day] */
tPEgesBirdPO4 = fDissEgesBird * tPEgesBird ;
/* PO4 egestion by herbivorous birds [gP/m2/day] */
tPEgesBirdDet = tPEgesBird - tPEgesBirdPO4 ;
/* P detritus egestion by herbivorous birds [gP/m2/day] */
tNAssVegBird = fDAssBird * tNGrazVegBird ;
/* N assimilation by herbivorous birds [gN/m2/day] */
tNEgesBird = tNGrazVegBird - tNAssVegBird ;
/* N egestion by herbivorous birds [gN/m2/day] */
tNEgesBirdNH4 = fDissEgesBird * tNEgesBird ;
/* NH4 egestion by herbivorous birds [gN/m2/day] */
tNEgesBirdDet = tNEgesBird - tNEgesBirdNH4 ;
/* N detritus egestion by herbivorous birds [gN/m2/day] */
wDBedDetW = (tDMortVegW + tDEgesBird) / sDepthW ;
/* total vegetation DW flux to water detritus [gDW/m3/day] */
tDBedDetS = tDMortVegS ;
/* total vegetation DW flux to sediment detritus [gDW/m2/day] */
tDBedTotT = tDMigrVeg + tDProdVeg - tDRespVeg - tDManVeg - tDAssVegBird ;
/* total vegetation DW flux [gDW/m2/day] */
wPBedPO4W = (- tPUptVegW + tPExcrVegW + tPMortVegPO4W + tPEgesBirdPO4) /sDepthW ;
/* total vegetation P flux to PO4 in water [gP/m3/day] */
wPBedDetW = (tPMortVegDetW + tPEgesBirdDet) / sDepthW ;
/* total vegetation P flux to water detritus [gP/m3/day] */
tPBedPO4S = - tPUptVegS + tPExcrVegS + tPMortVegPO4S ;
/* total vegetation P flux to interstitial water PO4 [gP/m2/day] */
tPBedDetS = tPMortVegDetS ;
/* total vegetation P flux to sediment detritus [gP/m2/day] */
tPBedTotT = tPMigrVeg - tPManVeg - tPAssVegBird ;
/* total vegetation P flux [gP/m2/day] */
wNBedNH4W = (- tNUptNH4VegW + tNExcrVegW + tNMortVegNH4W + tNEgesBirdNH4) / sDepthW ;
/* total vegetation N flux to NH4 in water [gN/m3/day] */
wNBedNO3W = - tNUptNO3VegW / sDepthW ;
/* total vegetation N flux to NO3 in water [gN/m3/day] */
wNBedDetW = (tNMortVegDetW + tNEgesBirdDet) / sDepthW ;
/* total vegetation N flux to water detritus [gN/m3/day] */
tNBedNH4S = - tNUptNH4VegS + tNExcrVegS + tNMortVegNH4S ;
/* total vegetation N flux to NH4 in interstitial water [gN/m2/day] */
tNBedNO3S = - tNUptNO3VegS ;
/* total vegetation N flux to NO3 in interstitial water [gN/m2/day] */
tNBedDetS = tNMortVegDetS ;
/* total vegetation N flux to sediment detritus [gN/m2/day] */
tNBedTotT = tNMigrVeg - tNManVeg - tNAssVegBird ;
/* total vegetation N flux [gN/m2/day] */
tO2BedW = tO2ProdVegW - tO2RespVegW + tO2UptNO3VegW ;
/* total vegetation O2 flux to water [gO2/m2/day] */
tO2BedS = tO2ProdVegS - tO2RespVegS + tO2UptNO3VegS ;
/* total vegetation O2 flux to sediment [gO2/m2/day] */

/* ---------------------------------- */
/* Algae */ 
/* ---------------------------------- */

UseLoss = 1.0 ;
/* switch to indicate if phytoplankton is affected by grazing (1=yes and 0=no) [-] */
uFunTmLoss = exp(-0.5/(cSigTmLoss*cSigTmLoss) *(((uTm-cTmOptLoss)*(uTm-cTmOptLoss)) -(cTmRef-cTmOptLoss)*(cTmRef-cTmOptLoss))) ;
/* temperature function of grazing [-] */
rPDPhytW = sPPhytW /(sDPhytW+NearZero) ;
/* P/DW ratio of phytoplankton [gP/gDW] */
rNDPhytW = sNPhytW /(sDPhytW+NearZero) ;
/* N/DW ratio of phytoplankton [gN/gDW] */
rPDPhytS = sPPhytS /(sDPhytS+NearZero) ;
/* P/DW ratio of settled phytoplankton [gP/gDW] */
rNDPhytS = sNPhytS /(sDPhytS+NearZero) ;
/* N/DW ratio of settled phytoplankton [gN/gDW] */
uFunTmPhyt = exp(-0.5/(cSigTmPhyt*cSigTmPhyt) *((uTm-cTmOptPhyt)*(uTm-cTmOptPhyt) - (cTmRef-cTmOptPhyt)*(cTmRef-cTmOptPhyt))) ;
/* temperature function of phytoplankton growth and respiration [-] */
uFunTmProdPhyt = uFunTmPhyt ;
/* temperature function of phytoplankton growth [-] */
uFunTmRespPhyt = uFunTmPhyt ;
/* temperature function of phytoplankton respiration [-] */
aVPUptMaxCrPhyt = max(0.0,cVPUptMaxPhyt * uFunTmProdPhyt *(cPDPhytMax - rPDPhytW) /(cPDPhytMax - cPDPhytMin)) ;
/* maximum P uptake rate phytoplankton corrected for P/DW ratio and temperature [gP/gDW/day] */
aVPUptPhyt = sPO4W * aVPUptMaxCrPhyt /(aVPUptMaxCrPhyt / cAffPUptPhyt + sPO4W) ;
/* actual P uptake rate of phytoplankton [gP/gDW/day] */
wPUptPhyt = aVPUptPhyt * sDPhytW ;
/* total P uptake by phytoplankton [gP/m3/day] */
aVNUptMaxCrPhyt = max(0.0,cVNUptMaxPhyt * uFunTmProdPhyt * (cNDPhytMax - rNDPhytW) /(cNDPhytMax - cNDPhytMin)) ;
/* maximum N uptake rate ophytoplankton corrected for N/DW ratio and temperature [gN/gDW/day] */
ahNUptPhyt = aVNUptMaxCrPhyt / cAffNUptPhyt ;
/* half-saturation SRN concentration for uptake by phytoplankton [gN/m3] */
aVNUptPhyt = oNDissW * aVNUptMaxCrPhyt /(ahNUptPhyt + oNDissW) ;
/* actual N uptake rate of phytoplankton [gN/gDW/day] */
wNUptPhyt = aVNUptPhyt * sDPhytW ;
/* total N uptake by phytoplankton [gN/m3/day] */
afNH4UptPhyt = sNH4W * sNO3W /((ahNUptPhyt + sNH4W) *(ahNUptPhyt + sNO3W)) + sNH4W * ahNUptPhyt /((sNH4W + sNO3W) *(ahNUptPhyt + sNO3W)) ;
/* fraction ammonium uptake by phytoplankton (from EPA) [-] */
wNUptNH4Phyt = afNH4UptPhyt * wNUptPhyt ;
/* total NH4-N uptake by phytoplankton [gN/m3/day] */
wNUptNO3Phyt = wNUptPhyt - wNUptNH4Phyt ;
/* total NO3-N uptake by phytoplankton [gN/m3/day] */
uMuMaxTmPhyt = cMuMaxPhyt * uFunTmProdPhyt ;
/* maximum growth rate of phytoplankton at current temperature [day-1] */
aPLimPhyt = max(0.0,(1.0 - cPDPhytMin / (rPDPhytW + NearZero)) * cPDPhytMax /(cPDPhytMax - cPDPhytMin)) ;
/* P limitation on phytoplankton growth according to Droop function [-] */
aNLimPhyt = max(0.0,(1.0 - cNDPhytMin / (rNDPhytW + NearZero)) * cNDPhytMax /(cNDPhytMax - cNDPhytMin)) ;
/* N limitation on phytoplankton growth according to Droop function [-] */
aLLimPhyt = UseSteelePhyt *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cLOptRefPhyt * uFunTmProdPhyt)) - exp(- uLPAR0 /(cLOptRefPhyt * uFunTmProdPhyt)))) +(1.0 - UseSteelePhyt) *(1.0 /(aExtCoef * sDepthW) * ln((1.0 + uLPAR0 / (hLRefPhyt * uFunTmProdPhyt)) / (1.0 + aLPARBot /(hLRefPhyt * uFunTmProdPhyt)))) ;
/* light limitation phytoplankton growth according to Steele or Lehman function [-] */
aMuTmLPhyt = ufDay *(1.0 - afCovSurfVeg) * aLLimPhyt * uMuMaxTmPhyt ;
/* phytoplankton growth rate at current light and temperature [day-1] */
aNutLimPhyt = min(aPLimPhyt,aNLimPhyt) ;
/* nutrient limitation phytoplankton growth (Liebigs law of the minimum) [-] */
aMuPhyt = aNutLimPhyt * aMuTmLPhyt ;
/* phytoplankton growth rate [day-1] */
wDAssPhyt = aMuPhyt*sDPhytW ;
/* total phytoplankton growth [gDW/m3/day] */
rChDPhyt = cChDPhytMax -(cChDPhytMax - cChDPhytMin) * aLLimPhyt ;
/* chlorophyll-a/DW ratio phytoplankton [gChla/gDW] */
oChlaPhyt = mgPerg * rChDPhyt * sDPhytW ;
/* chlorophyll-a concentration [mgChla/m3] */
aExtChPhyt = cExtSpPhyt / rChDPhyt ;
/* specific light extinction per unit chlorophyll-a [m2/mgChla] */
ukDRespTmPhyt = kDRespPhyt * uFunTmRespPhyt ;
/* temperature corrected respiration constant of phytoplankton [day-1] */
wDRespPhytW = ukDRespTmPhyt * sDPhytW ;
/* total phytoplankton respiration in water [gDW/m3/day] */
ukLossTmPhyt = UseLoss * kLossPhyt * uFunTmLoss ;
/* daily grazing on phytoplankton [day-1] */
wDLossPhyt = ukLossTmPhyt * sDPhytW ;
/* total phytoplankton grazing loss [gDW/m3/day] */
wDMortPhytW = kMortPhytW * sDPhytW ;
/* total phytoplankton mortality in water [gDW/m3/day] */
uCorVSetPhyt = cVSetPhyt * aFunTauSetOM * uFunTmSet ;
/* corrected settling velocity of phytoplankton [m/day] */
tDSetPhyt = uCorVSetPhyt * sDPhytW ;
/* total phytoplankton settling [gDW/m2/day] */
tDResusPhyt = sDPhytS /(aDPhytS+NearZero) * tDResusPhytTot ;
/* resuspension of settled phytoplankton [gDW/m2/day] */
tDRespPhytS = ukDRespTmPhyt * sDPhytS ;
/* respiration of settled phytoplankton [g/m2/day] */
tDMortPhytS = kMortPhytS * sDPhytS ;
/* mortality of settled phytoplankton [g/m2/day] */
ukDDecPhyt = ukDRespTmPhyt + ukLossTmPhyt + kMortPhytW +(uCorVSetPhyt * uFunTmSet) / sDepthW ;
/* total loss rate of phytoplankton in water (without dilution) [day-1] */
wPExcrPhytW = (2.0*rPDPhytW) /(cPDPhytMax + rPDPhytW) * rPDPhytW * wDRespPhytW ;
/* total P excretion phytoplankton in water [gP/m3/day] */
wPLossPhyt = rPDPhytW * wDLossPhyt ;
/* total P loss by grazing on phytoplankton [gP/m3/day] */
wPMortPhytW = wDMortPhytW * rPDPhytW ;
/* total P mortality phytoplankton in water [gP/m3/day] */
tPSetPhyt = rPDPhytW * tDSetPhyt ;
/* total P associated with settling of phytoplankton [gP/m2/day] */
tPResusPhyt = rPDPhytS * tDResusPhyt ;
/* total P resuspension of settled phytoplankton [gP/m2/day] */
tPExcrPhytS = (2.0*rPDPhytS) /(cPDPhytMax + rPDPhytS) * rPDPhytS * tDRespPhytS ;
/* total P excretion by settled phytoplankton [gP/m2/day] */
tPMortPhytS = tDMortPhytS * rPDPhytS ;
/* total P phytoplankton mortality [gP/m2/day] */
wNExcrPhytW = (2.0*rNDPhytW) /(cNDPhytMax + rNDPhytW) * rNDPhytW * wDRespPhytW ;
/* total N excretion phytoplankton in water [gN/m3/day] */
wNLossPhyt = rNDPhytW * wDLossPhyt ;
/* total N loss by grazing on phytoplankton [gN/m3/day] */
wNMortPhytW = wDMortPhytW * rNDPhytW ;
/* total N mortality phytoplankton in water [gN/m3/day] */
tNSetPhyt = rNDPhytW * tDSetPhyt ;
/* total N associated with settling of phytoplankton [gN/m2/day] */
tNResusPhyt = rNDPhytS * tDResusPhyt ;
/* total N resuspension of settled phytoplankton [gN/m2/day] */
tNExcrPhytS = (2.0*rNDPhytS) /(cNDPhytMax + rNDPhytS) * rNDPhytS * tDRespPhytS ;
/* total N excretion by settled phytoplankton [gN/m2/day] */
tNMortPhytS = tDMortPhytS * rNDPhytS ;
/* total N phytoplankton mortality [gN/m2/day] */
wDPrimPhytW = wDAssPhyt - wDRespPhytW - wDLossPhyt - wDMortPhytW -(tDSetPhyt - tDResusPhyt) / sDepthW ;
/* total of phytoplankton DW processes in water [gDW/m3/day] */
wPPrimPhytW = wPUptPhyt - wPExcrPhytW - wPLossPhyt - wPMortPhytW -(tPSetPhyt - tPResusPhyt) / sDepthW ;
/* total of phytoplankton P processes in water [gP/m3/day] */
wNPrimPhytW = wNUptPhyt - wNExcrPhytW - wNLossPhyt - wNMortPhytW -(tNSetPhyt - tNResusPhyt) / sDepthW ;
/* total of phytoplankton N processes in water [gN/m3/day] */
tDPrimPhytS = tDSetPhyt - tDResusPhyt - tDMortPhytS - tDRespPhytS ;
/* total of settled phytoplankton DW processes [g/m2/day] */
tPPrimPhytS = tPSetPhyt - tPResusPhyt - tPMortPhytS - tPExcrPhytS ;
/* total of settled phytoplankton P processes [gP/m2/day] */
tNPrimPhytS = tNSetPhyt - tNResusPhyt - tNMortPhytS - tNExcrPhytS ;
/* total of settled phytoplankton N processes [gN/m2/day] */
oChla = oChlaPhyt ;
/* total chlorophyll-a concentration [mgChla/m3] */
wDPrimDetW = wDMortPhytW + wDLossPhyt ;
/* total detritus flux to water by phytoplankton [g/m3/day] */
tDPrimDetS = tDMortPhytS ;
/* total detritus flux to sediment by settled phytoplankton [g/m3/day] */
tDPrimTotT = (wDAssPhyt - wDRespPhytW) * sDepthW - tDRespPhytS ;
/* total DW flux associated with phytoplankton [gDW/m2/day] */
wO2ProdPhyt = molO2molC * cCPerDW * wDAssPhyt ;
/* O2 production by phytoplankton [gO2/m3/day] */
wO2RespPhytW = molO2molC * cCPerDW * wDRespPhytW * aCorO2BOD ;
/* O2 respiration by water phytoplankton [gO2/m3/day] */
wO2UptNO3Phyt = O2PerNO3 * molO2molN * wNUptNO3Phyt ;
/* O2 production due to NO3 uptake by phytoplankton [gO2/m3/day] */
wO2PrimW = wO2ProdPhyt - wO2RespPhytW + wO2UptNO3Phyt ;
/* O2 flux by water phytoplankton [gO2/m3/day] */
tO2RespPhytS = molO2molC * cCPerDW * tDRespPhytS * afOxySed ;
/* O2 respiration by settled phytoplankton [gO2/m2/day] */
tO2PrimS = tO2RespPhytS ;
/* O2 flux by settled phytoplankton [gO2/m2/day] */
wPMortPhytPO4W = fDissMortPhyt * wPMortPhytW ;
/* soluble P flux due to water phytoplankton mortality [gP/m3/day] */
wPMortPhytDetW = wPMortPhytW - wPMortPhytPO4W ;
/* detrital P flux due to water phytoplankton mortality [gP/m3/day] */
wPLossPhytPO4 = fDissLoss * wPLossPhyt ;
/* soluble P flux due to grazing on phytoplankton [gP/m3/day] */
wPLossPhytDet = wPLossPhyt - wPLossPhytPO4 ;
/* detrital P flux due to grazing on phytoplankton [gP/m3/day] */
wPPrimPO4W = - wPUptPhyt + wPExcrPhytW + wPLossPhytPO4 + wPMortPhytPO4W ;
/* SRP flux in water due to phytoplankton [gP/m3/day] */
wPPrimDetW = wPLossPhytDet + wPMortPhytDetW ;
/* detrital P flux in water due to phytoplankton [gP/m3/day] */
tPMortPhytPO4S = fDissMortPhyt * tPMortPhytS ;
/* pore water SRP flux due to settled phytoplankton mortality [gP/m2/day] */
tPMortPhytDetS = tPMortPhytS - tPMortPhytPO4S ;
/* detrital P flux due to settled phytoplankton mortality [gP/m2/day] */
tPPrimDetS = tPMortPhytDetS ;
/* detrital P flux in sediment due to phytoplankton [gP/m2/day] */
tPPrimPO4S = tPExcrPhytS + tPMortPhytPO4S ;
/* pore water SRP flux due to phytoplankton [gP/m2/day] */
tPPrimTotT = 0.0 ;
/* total P flux associated with phytoplankton [gP/m2/day] */
wNMortPhytNH4W = fDissMortPhyt * wNMortPhytW ;
/* ammonium flux due to water phytoplankton mortality [gN/m3/day] */
wNMortPhytDetW = wNMortPhytW - wNMortPhytNH4W ;
/* detrital N flux due to water phytoplankton mortality [gN/m3/day] */
wNLossPhytNH4 = fDissLoss * wNLossPhyt ;
/* ammonium flux due to grazing on phytoplankton [gN/m3/day] */
wNLossPhytDet = wNLossPhyt - wNLossPhytNH4 ;
/* detrital N flux due to grazing on phytoplankton [gN/m3/day] */
wNPrimNH4W = - wNUptNH4Phyt + wNExcrPhytW + wNLossPhytNH4 + wNMortPhytNH4W ;
/* ammonium flux in water due to phytoplankton [gN/m3/day] */
wNPrimNO3W = - wNUptNO3Phyt ;
/* nitrate flux in water due to phytoplankton [gN/m3/day] */
wNPrimDetW = wNLossPhytDet + wNMortPhytDetW ;
/* detrital N flux in water due to phytoplankton [gN/m3/day] */
tNMortPhytNH4S = fDissMortPhyt * tNMortPhytS ;
/* pore water ammonium flux due to settled phytoplankton mortality [gN/m2/day] */
tNMortPhytDetS = tNMortPhytS - tNMortPhytNH4S ;
/* sediment detrital N flux due to settled phytoplankton mortality [gN/m2/day] */
tNPrimNH4S = tNExcrPhytS + tNMortPhytNH4S ;
/* pore water ammonium flux due to phytoplankton [gN/m2/day] */
tNPrimNO3S = 0.0 ;
/* pore water nitrate flux due to phytoplankton [gN/m2/day] */
tNPrimDetS = tNMortPhytDetS ;
/* sediment detrital N flux due to phytoplankton [gN/m2/day] */
tNPrimTotT = 0.0 ;
/* total N flux associated with phytoplankton [gN/m2/day] */
aPACoef = cPACoefMin +(cPACoefMax - cPACoefMin) * hPACoef / (hPACoef + oDOMW) ;
/* Poole-Atkins coefficient to calculate Secchi depth [-] */
bSecchiMax = sDepthW + cSecchiPlus ;
/* maximum Secchi depth [m] */
aSecchi = min(bSecchiMax,aPACoef / aExtCoefOpen) ;
/* Secchi depth [m] */
aDepthEuph = cEuph * aSecchi ;
/* euphotic depth [m] */
aRelDepthEuph = aDepthEuph / sDepthW ;
/* relative euphotic depth [m] */
aChlaH = oChla * sDepthW ;
/* mg Chla per m2 [mgChla/m2] */
aCovPhytW = cCovSpPhyt *(oDPhytW * sDepthW) ;
/* % horizontal coverage with phytoplankton [%] */
rExtChPhyt = aExtPhyt /(oChla / mgPerg + NearZero) ;
/* average specific light extinction of phytoplankton per unit chlorophyl-a [m2/gChla] */

/* ---------------------------------- */
/* Miscellaneous */ 
/* ---------------------------------- */

tDSetTot = tDSetIM + tDSetDet + tDSetPhyt ;
/* total settling DW flux [gDW/m2/day] */
tPSetTot = tPSetAIM + tPSetDet + tPSetPhyt ;
/* total settling P flux [gP/m2/day] */
tNSetTot = tNSetDet + tNSetPhyt ;
/* total settling N flux [gN/m2/day] */
tDResusTot = tDResusDead + tDResusPhytTot ;
/* total resuspension DW flux [gDW/m2/day] */
tPResusTot = tPResusDet + tPResusAIM + tPResusPhyt + tPResusPO4 ;
/* total resuspension P flux [gP/m2/day] */
tNResusTot = tNResusDet + tNResusNH4 + tNResusNO3 + tNResusPhyt ;
/* total resuspension N flux [gN/m2/day] */
if (sTime >= cDredStart * DaysPerYear) {
   bTimeDred = (int(TimeYears/cDredInterval) * cDredInterval) * DaysPerYear ;
} else {
   bTimeDred = -9999.999 ;
}
/* start of dredging period [day] */
if (sTime == bTimeDred) {
   aDepthStart = sDepthW ;
} else {
   aDepthStart = aDepthStart ;
}
/* water depth at the beginning of dredging period [day] */
if ((sTime >= bTimeDred) && (sTime < bTimeDred + cLengDred) && (aDepthStart <= cDepthRef - cDepthS)) {
   akDredDepth = (ln(cDepthRef / aDepthStart)) / cLengDred ;
} else {
   akDredDepth = 0.0 ;
}
/* deepening rate constant during dredging period [day-1] */
if ((sTime >= bTimeDred) &&(sTime < bTimeDred + cLengDred) &&(aDepthStart <= cDepthRef - cDepthS)) {
   akDred = (- ln(1.0 - fEffDred)) / cLengDred ;
} else {
   akDred = 0.0 ;
}
/* dredging rate constant during dredging period [day-1] */
if ((sTime >= bTimeDred) &&(sTime < bTimeDred + cLengDred) &&(aDepthStart <= cDepthRef - cDepthS)) {
   akDredLemn = (- ln(1.0 - fEffDredLemn)) / cLengDred ;
} else {
   akDredLemn = 0.0 ;
}
/* dredging rate constant during dredging period for Lemnacaea [day-1] */
vDredDepthW = akDredDepth * sDepthW ;
/* change in water depth due to dredging [m/day] */
tDDredDetS = akDred * sDDetS ;
/* dredging flux of DW Detritus in ditch sediment [gDW/m2/day] */
tPDredDetS = akDred * sPDetS ;
/* dredging flux of P Detritus in ditch sediment [gP/m2/day] */
tNDredDetS = akDred * sNDetS ;
/* dredging flux of N Detritus in ditch sediment [gN/m2/day] */
tPDredAIMS = akDred * sPAIMS ;
/* dredging flux of P absorbed onto inorganic matter in ditch sediment [gP/m2/day] */
bRhoSolidSoil = fDOrgSoil * cRhoOM +(1 - fDOrgSoil) * cRhoIM ;
/* average solid density of soil material [g/m3] */
tDDredNetSoil = -(tDDredDetS / cRhoOM) * bRhoSolidSoil ;
/* dredging flux of DW NetSoil in ditch sediment [gDW/m2/day] */
tDDredNetIMS = (1 - fDOrgSoil) * tDDredNetSoil ;
/* dredging flux of DW NetIMS in ditch sediment [gDW/m2/day] */
tDDredNetHumS = fDOrgSoil * tDDredNetSoil ;
/* dredging flux of DW NetHum in ditch sediment [gDW/m2/day] */
tPDredNetHumS = cPDSoilOM * tDDredNetHumS ;
/* dredging flux of P NetHum in ditch sediment [gP/m2/day] */
tNDredNetHumS = cNDSoilOM * tDDredNetHumS ;
/* dredging flux of N NetHum in ditch sediment [gN/m2/day] */
tDDredPhytS = akDred * sDPhytS ;
/* dredging flux of DW phytoplankton on ditch sediment [gDW/m2/day] */
tPDredPhytS = akDred * sPPhytS ;
/* dredging flux of P phytoplankton on ditch sediment [gP/m2/day] */
tNDredPhytS = akDred * sNPhytS ;
/* dredging flux of N phytoplankton on ditch sediment [gN/m2/day] */
tDDredElod = akDred * sDElod ;
/* dredging flux of DW Elodeids on ditch sediment [gDW/m2/day] */
tPDredElod = akDred * sPElod ;
/* dredging flux of P Elodeids on ditch sediment [gP/m2/day] */
tNDredElod = akDred * sNElod ;
/* dredging flux of N Elodeids on ditch sediment [gN/m2/day] */
tDDredCera = akDred * sDCera ;
/* dredging flux of DW Ceratophyllids on ditch sediment [gDW/m2/day] */
tPDredCera = akDred * sPCera ;
/* dredging flux of P Ceratophyllids on ditch sediment [gP/m2/day] */
tNDredCera = akDred * sNCera ;
/* dredging flux of N Ceratophyllids on ditch sediment [gN/m2/day] */
tDDredChar = akDred * sDChar ;
/* dredging flux of DW Characeans on ditch sediment [gDW/m2/day] */
tPDredChar = akDred * sPChar ;
/* dredging flux of P Characeans on ditch sediment [gP/m2/day] */
tNDredChar = akDred * sNChar ;
/* dredging flux of N Characeans on ditch sediment [gN/m2/day] */
tDDredLemn = akDredLemn * sDLemn ;
/* dredging flux of DW Lemnacaea on ditch sediment [gDW/m2/day] */
tPDredLemn = akDredLemn * sPLemn ;
/* dredging flux of P Lemnacaea on ditch sediment [gP/m2/day] */
tNDredLemn = akDredLemn * sNLemn ;
/* dredging flux of N Lemnacaea on ditch sediment [gN/m2/day] */
tDDredNymp = akDred * sDNymp ;
/* dredging flux of DW Nymphaeids on ditch sediment [gDW/m2/day] */
tPDredNymp = akDred * sPNymp ;
/* dredging flux of P Nymphaeids on ditch sediment [gP/m2/day] */
tNDredNymp = akDred * sNNymp ;
/* dredging flux of N Nymphaeids on ditch sediment [gN/m2/day] */
tDDredHelo = akDred * sDHelo ;
/* dredging flux of DW helophytes on ditch sediment [gDW/m2/day] */
tPDredHelo = akDred * sPHelo ;
/* dredging flux of P helophytes on ditch sediment [gP/m2/day] */
tNDredHelo = akDred * sNHelo ;
/* dredging flux of N helophytes on ditch sediment [gN/m2/day] */
tDDredVeg = tDDredElod+tDDredCera+tDDredChar+tDDredLemn+tDDredNymp+tDDredHelo ;
/* dredging flux of DW total vegetation on ditch sediment [gDW/m2/day] */
tPDredVeg = tPDredElod+tPDredCera+tPDredChar+tPDredLemn+tPDredNymp+tPDredHelo ;
/* dredging flux of P total vegetation on ditch sediment [gP/m2/day] */
tNDredVeg = tNDredElod+tNDredCera+tNDredChar+tNDredLemn+tNDredNymp+tNDredHelo ;
/* dredging flux of N total vegetation on ditch sediment [gN/m2/day] */
tDDredNetTot = tDDredDetS - tDDredNetSoil + tDDredPhytS + tDDredVeg ;
/* total DW dredging flux [gDW/m2/day] */
tPDredNetTot = tPDredDetS - tPDredNetHumS + tPDredAIMS + tPDredPhytS + tPDredVeg ;
/* total P dredging flux [gP/m2/day] */
tNDredNetTot = tNDredDetS - tNDredNetHumS + tNDredPhytS + tNDredVeg ;
/* total N dredging flux [gN/m2/day] */
tDIMS = tDAbioIMS ;
/* increase in DW inorganic matter in sediment [gDW/m2/day] */
tDHumS = tDAbioHumS ;
/* increase in DW sediment humus in ditch [gDW/m2/day] */
tDDetS = tDAbioDetS + tDPrimDetS + tDBedDetS ;
/* increase in DW sediment detritus in ditch [gDW/m2/day] */
vDeltaS = (tDIMS / cRhoIM +(tDHumS + tDDetS) / cRhoOM) /(1.0 - bPorS) ;
/* turnover depth in ditch [m/day] */
if (vDeltaS >= 0.0) {
   tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / ((sDHumS + sDDetS) / sDIMS + cRhoOM / cRhoIM) ;
} else {
   tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / (fDOrgSoil /(1.0 - fDOrgSoil) + cRhoOM / cRhoIM) ;
}
/* burial flux of DW in inorganic matter in ditch [gDW/m2/day] */
if (vDeltaS >= 0.0) {
   tDBurOM = (sDHumS + sDDetS) / sDIMS * tDBurIM ;
} else {
   tDBurOM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIM ;
}
/* burial flux of DW in organic matter in ditch [gDW/m2/day] */
if (vDeltaS >= 0.0) {
   tDBurDet = sDDetS /(sDHumS + sDDetS) * tDBurOM ;
} else {
   tDBurDet = 0.0 ;
}
/* burial flux of DW in detritus in ditch [gDW/m2/day] */
if (vDeltaS >= 0.0) {
   tDBurHum = tDBurOM - tDBurDet ;
} else {
   tDBurHum = tDBurOM ;
}
/* burial flux of DW in humus in ditch [gDW/m2/day] */
if (vDeltaS >= 0.0) {
   tDBurTot = tDBurIM + tDBurOM ;
} else {
   tDBurTot = tDBurIM + tDBurOM ;
}
/* total burial flux of DW in ditch [gDW/m2/day] */
if (vDeltaS >= 0.0) {
   tPBurHum = rPDHumS * tDBurHum ;
} else {
   tPBurHum = cPDSoilOM * tDBurHum ;
}
/* burial flux of P in humus in ditch [gP/m2/day] */
if (vDeltaS >= 0.0) {
   tPBurDet = rPDDetS * tDBurDet ;
} else {
   tPBurDet = 0.0 ;
}
/* burial flux of P in detritus in ditch [gP/m2/day] */
if (vDeltaS >= 0.0) {
   tPBurAIM = sPAIMS / sDIMS * tDBurIM ;
} else {
   tPBurAIM = 0.0 ;
}
/* burial flux of P absorbed onto inorganic matter in ditch [gP/m2/day] */
if (vDeltaS >= 0.0) {
   tPBurPO4 = sPO4S *(vDeltaS / cDepthS) ;
} else {
   tPBurPO4 = cPO4Ground *(bPorS * vDeltaS) ;
}
/* burial flux of SRP in ditch [gP/m2/day] */
if (vDeltaS >= 0.0) {
   tPBurTot = tPBurDet + tPBurHum + tPBurAIM + tPBurPO4 ;
} else {
   tPBurTot = tPBurHum + tPBurAIM + tPBurPO4 ;
}
/* total burial flux of P in ditch [gP/m2/day] */
if (vDeltaS >= 0.0) {
   tNBurHum = rNDHumS * tDBurHum ;
} else {
   tNBurHum = cNDSoilOM * tDBurHum ;
}
/* burial flux of N in humus in ditch [gN/m2/day] */
if (vDeltaS >= 0.0) {
   tNBurDet = rNDDetS * tDBurDet ;
} else {
   tNBurDet = 0.0 ;
}
/* burial flux of N in detritus in ditch [gN/m2/day] */
if (vDeltaS >= 0.0) {
   tNBurNH4 = sNH4S *(vDeltaS / cDepthS) ;
} else {
   tNBurNH4 = cNH4Ground *(bPorS * vDeltaS) ;
}
/* burial flux of dissolved NH4 in ditch [gN/m2/day] */
if (vDeltaS >= 0.0) {
   tNBurNO3 = sNO3S *(vDeltaS / cDepthS) ;
} else {
   tNBurNO3 = cNO3Ground *(bPorS * vDeltaS) ;
}
/* burial flux of dissolved NO3 in ditch [gN/m2/day] */
if (vDeltaS >= 0.0) {
   tNBurTot = tNBurDet + tNBurHum + tNBurNH4 + tNBurNO3 ;
} else {
   tNBurTot = tNBurHum + tNBurNH4 + tNBurNO3 ;
}
/* total burial flux of N in ditch [gN/m2/day] */
if (ConstDepth == 1 ) {
   vDeltaW = 0.0 ;
} else {
   vDeltaW = - vDeltaS ;
}
/* rate of change in ditch water depth [m/day] */
aRelDeltaW = (vDeltaW + vDredDepthW) / sDepthW ;
/* relative water depth change due to sediment turnover and dredging [day-1] */
aDTotT =(sDIMW + sDDetW + sDPhytW) * sDepthW + aDVeg + sDIMS + sDHumS + sDDetS + aDPhytS ;
/* total DW in system [gDW/m2] */
aNTotT =(sNH4W + sNO3W + sNDetW + sNPhytW) * sDepthW + aNVeg + sNH4S + sNO3S + sNDetS + sNHumS + sNPhytS ;
/* total N in system [gN/m2] */
aPTotT =(sPO4W + sPDetW + sPPhytW + sPAIMW) * sDepthW + aPVeg + sPO4S + sPDetS + sPHumS + sPPhytS + sPAIMS ;
/* total P in system [gP/m2] */
aDError = aDTotT - sDExtTotT ;
/* DW mass balance error [gDW/m2] */
aNError = aNTotT - sNExtTotT ;
/* N mass balance error [gN/m2] */
aPError = aPTotT - sPExtTotT ;
/* P mass balance error [gP/m2] */

/* ---------------------------------- */
/* Derivatives */ 
/* ---------------------------------- */

dPO4W = wPAbioPO4W + wPPrimPO4W + wPBedPO4W + cPBackLoad/sDepthW - aRelDeltaW * sPO4W + wPTranPO4W ;
/* derivative of P in PO4 in ditch water [gP/m3/day] */
dPAIMW = wPAbioAIMW - aRelDeltaW * sPAIMW +wPTranAIMW ;
/* derivative of P adsorbed onto IM in ditch water [gP/m3/day] */
dNH4W = wNAbioNH4W + wNPrimNH4W + wNBedNH4W + cNBackLoad/sDepthW - aRelDeltaW * sNH4W+wNTranNH4W ;
/* derivative of N in NH4 in ditch water [gN/m3/day] */
dNO3W = wNAbioNO3W + wNPrimNO3W + wNBedNO3W - aRelDeltaW * sNO3W +wNTranNO3W ;
/* derivative of N in NO3 in ditch water [gN/m3/day] */
dO2W = wO2AbioW + wO2PrimW + tO2BedW / sDepthW - aRelDeltaW * sO2W +wO2TranW ;
/* derivative of Oxygen in ditch water [gO2/m3/day] */
dDIMW = wDAbioIMW - aRelDeltaW * sDIMW +wDTranIMW ;
/* derivative of Inorganic matter in ditch water [g/m3/day] */
dDDetW = wDAbioDetW + wDPrimDetW + wDBedDetW - aRelDeltaW * sDDetW +wDTranDetW ;
/* derivative of Detritus DW in ditch water [g/m3/day] */
dPDetW = wPAbioDetW + wPPrimDetW + wPBedDetW - aRelDeltaW * sPDetW +wPTranDetW ;
/* derivative of Detritus P in ditch water [gP/m3/day] */
dNDetW = wNAbioDetW + wNPrimDetW + wNBedDetW - aRelDeltaW * sNDetW +wNTranDetW ;
/* derivative of Detritus N in ditch water [gN/m3/day] */
dDPhytW = wDPrimPhytW - aRelDeltaW * sDPhytW +wDTranPhyt ;
/* derivative of DW of Phytoplankton [gDW/m3/day] */
dPPhytW = wPPrimPhytW - aRelDeltaW * sPPhytW +wPTranPhyt ;
/* derivative of P in Phytoplankton [gP/m3/day] */
dNPhytW = wNPrimPhytW - aRelDeltaW * sNPhytW +wNTranPhyt ;
/* derivative of N in Phytoplankton [gN/m3/day] */
dPO4S = tPAbioPO4S - tPBurPO4 + tPPrimPO4S + tPBedPO4S ;
/* derivative of P in PO4 in ditch sediment interstitial water [gP/m2/day] */
dPAIMS = tPAbioAIMS - tPBurAIM - tPDredAIMS ;
/* derivative of P adsorbed onto IM in ditch sediment [gP/m2/day] */
dNH4S = tNAbioNH4S - tNBurNH4 + tNPrimNH4S + tNBedNH4S ;
/* derivative of N in NH4 in ditch sediment interstitial water [gN/m2/day] */
dNO3S = tNAbioNO3S - tNBurNO3 + tNPrimNO3S + tNBedNO3S ;
/* derivative of N in NO3 in ditch sediment interstitial water [gN/m2/day] */
dDIMS = tDAbioIMS - tDBurIM - tDDredNetIMS ;
/* derivative of Inorganic matter in ditch sediment [gDW/m2/day] */
dDHumS = tDAbioHumS - tDBurHum - tDDredNetHumS ;
/* derivative of Humus DW in ditch sediment [gDW/m2/day] */
dPHumS = tPAbioHumS - tPBurHum - tPDredNetHumS ;
/* derivative of Humus P in ditch sediment [gP/m2/day] */
dNHumS = tNAbioHumS - tNBurHum - tNDredNetHumS ;
/* derivative of Humus N in ditch sediment [gN/m2/day] */
dDDetS = tDAbioDetS - tDBurDet + tDPrimDetS + tDBedDetS - tDDredDetS ;
/* derivative of Detritus DW in ditch sediment [g/m2/day] */
dPDetS = tPAbioDetS - tPBurDet + tPPrimDetS + tPBedDetS - tPDredDetS ;
/* derivative of Detritus P in ditch sediment [gP/m2/day] */
dNDetS = tNAbioDetS - tNBurDet + tNPrimDetS + tNBedDetS - tNDredDetS ;
/* derivative of Detritus N in ditch sediment [gN/m2/day] */
dDPhytS = tDPrimPhytS - tDDredPhytS ;
/* derivative of DW of settled phytoplankton [gDW/m2/day] */
dPPhytS = tPPrimPhytS - tPDredPhytS ;
/* derivative of P in settled phytoplankton [gP/m2/day] */
dNPhytS = tNPrimPhytS - tNDredPhytS ;
/* derivative of N in settled phytoplankton [gN/m2/day] */
dDElod = tDBedElod - tDDredElod ;
/* derivative of DW of Elodeids [gDW/m2/day] */
dPElod = tPBedElod - tPDredElod ;
/* derivative of P in Elodeids [gP/m2/day] */
dNElod = tNBedElod - tNDredElod ;
/* derivative of N in Elodeids [gN/m2/day] */
dDChar = tDBedChar - tDDredChar ;
/* derivative of DW of Characeans [gDW/m2/day] */
dPChar = tPBedChar - tPDredChar ;
/* derivative of P in Characeans [gP/m2/day] */
dNChar = tNBedChar - tNDredChar ;
/* derivative of N in Characeans [gN/m2/day] */
dDCera = tDBedCera - tDDredCera ;
/* derivative of DW of Ceratophyllids [gDW/m2/day] */
dPCera = tPBedCera - tPDredCera ;
/* derivative of P in Ceratophyllids [gP/m2/day] */
dNCera = tNBedCera - tNDredCera ;
/* derivative of N in Ceratophyllids [gN/m2/day] */
dDLemn = tDBedLemn - tDDredLemn ;
/* derivative of DW of Lemnacaea [gDW/m2/day] */
dPLemn = tPBedLemn - tPDredLemn ;
/* derivative of P in Lemnacaea [gP/m2/day] */
dNLemn = tNBedLemn - tNDredLemn ;
/* derivative of N in Lemnacaea [gN/m2/day] */
dDNymp = tDBedNymp - tDDredNymp ;
/* derivative of DW of Nymphaeids [gDW/m2/day] */
dPNymp = tPBedNymp - tPDredNymp ;
/* derivative of P in Nymphaeids [gP/m2/day] */
dNNymp = tNBedNymp - tNDredNymp ;
/* derivative of N in Nymphaeids [gN/m2/day] */
dDHelo = tDBedHelo - tDDredHelo ;
/* derivative of DW of helophytes [gDW/m2/day] */
dPHelo = tPBedHelo - tPDredHelo ;
/* derivative of P in helophytes [gP/m2/day] */
dNHelo = tNBedHelo - tNDredHelo ;
/* derivative of N in helophytes [gN/m2/day] */
dDExtTotT = uDLoad - wDOutflTot*sDepthW - tDBurTot + tDAbioTotT + tDPrimTotT + tDBedTotT - tDDredNetTot ;
/* derivative of Total amount of DW moved into or out the system [gDW/m2/day] */
dNExtTotT = uNLoad - wNOutflTot*sDepthW + cNBackLoad - tNBurTot - tNDredNetTot + tNAbioTotT + tNPrimTotT + tNBedTotT ;
/* derivative of Total amount of N moved into or out the system [gN/m2/day] */
dPExtTotT = uPLoad - wPOutflTot*sDepthW + cPBackLoad - tPBurTot - tPDredNetTot + tPAbioTotT + tPPrimTotT + tPBedTotT ;
/* derivative of Total amount of P moved into or out the system [gP/m2/day] */

/* ############################ */
/* Numerical Integration        */
/* ############################ */

k0(sPO4W) = dPO4W ;
/* P in PO4 in ditch water [gP/m3] */
k0(sPAIMW) = dPAIMW ;
/* P adsorbed onto IM in ditch water [gP/m3] */
k0(sNH4W) = dNH4W ;
/* N in NH4 in ditch water [gN/m3] */
k0(sNO3W) = dNO3W ;
/* N in NO3 in ditch water [gN/m3] */
k0(sO2W) = dO2W ;
/* Oxygen in ditch water [gO2/m3] */
k0(sDIMW) = dDIMW ;
/* Inorganic matter in ditch water [g/m3] */
k0(sDDetW) = dDDetW ;
/* Detritus DW in ditch water [g/m3] */
k0(sPDetW) = dPDetW ;
/* Detritus P in ditch water [gP/m3] */
k0(sNDetW) = dNDetW ;
/* Detritus N in ditch water [gN/m3] */
k0(sDPhytW) = dDPhytW ;
/* DW of Phytoplankton [gDW/m3] */
k0(sPPhytW) = dPPhytW ;
/* P in Phytoplankton [gP/m3] */
k0(sNPhytW) = dNPhytW ;
/* N in Phytoplankton [gN/m3] */
k0(sPO4S) = dPO4S ;
/* P in PO4 in ditch sediment interstitial water [gP/m2] */
k0(sPAIMS) = dPAIMS ;
/* P adsorbed onto IM in ditch sediment [gP/m2] */
k0(sNH4S) = dNH4S ;
/* N in NH4 in ditch sediment interstitial water [gN/m2] */
k0(sNO3S) = dNO3S ;
/* N in NO3 in ditch sediment interstitial water [gN/m2] */
k0(sDIMS) = dDIMS ;
/* Inorganic matter in ditch sediment [gDW/m2] */
k0(sDHumS) = dDHumS ;
/* Humus DW in ditch sediment [gDW/m2] */
k0(sPHumS) = dPHumS ;
/* Humus P in ditch sediment [gP/m2] */
k0(sNHumS) = dNHumS ;
/* Humus N in ditch sediment [gN/m2] */
k0(sDDetS) = dDDetS ;
/* Detritus DW in ditch sediment [g/m2] */
k0(sPDetS) = dPDetS ;
/* Detritus P in ditch sediment [gP/m2] */
k0(sNDetS) = dNDetS ;
/* Detritus N in ditch sediment [gN/m2] */
k0(sDPhytS) = dDPhytS ;
/* DW of settled phytoplankton [gDW/m2] */
k0(sPPhytS) = dPPhytS ;
/* P in settled phytoplankton [gP/m2] */
k0(sNPhytS) = dNPhytS ;
/* N in settled phytoplankton [gN/m2] */
k0(sDElod) = dDElod ;
/* DW of Elodeids [gDW/m2] */
k0(sPElod) = dPElod ;
/* P in Elodeids [gP/m2] */
k0(sNElod) = dNElod ;
/* N in Elodeids [gN/m2] */
k0(sDChar) = dDChar ;
/* DW of Characeans [gDW/m2] */
k0(sPChar) = dPChar ;
/* P in Characeans [gP/m2] */
k0(sNChar) = dNChar ;
/* N in Characeans [gN/m2] */
k0(sDCera) = dDCera ;
/* DW of Ceratophyllids [gDW/m2] */
k0(sPCera) = dPCera ;
/* P in Ceratophyllids [gP/m2] */
k0(sNCera) = dNCera ;
/* N in Ceratophyllids [gN/m2] */
k0(sDLemn) = dDLemn ;
/* DW of Lemnacaea [gDW/m2] */
k0(sPLemn) = dPLemn ;
/* P in Lemnacaea [gP/m2] */
k0(sNLemn) = dNLemn ;
/* N in Lemnacaea [gN/m2] */
k0(sDNymp) = dDNymp ;
/* DW of Nymphaeids [gDW/m2] */
k0(sPNymp) = dPNymp ;
/* P in Nymphaeids [gP/m2] */
k0(sNNymp) = dNNymp ;
/* N in Nymphaeids [gN/m2] */
k0(sDHelo) = dDHelo ;
/* DW of helophytes [gDW/m2] */
k0(sPHelo) = dPHelo ;
/* P in helophytes [gP/m2] */
k0(sNHelo) = dNHelo ;
/* N in helophytes [gN/m2] */
k0(sDExtTotT) = dDExtTotT ;
/* Total amount of DW moved into or out the system [gDW/m2] */
k0(sNExtTotT) = dNExtTotT ;
/* Total amount of N moved into or out the system [gN/m2] */
k0(sPExtTotT) = dPExtTotT ;
/* Total amount of P moved into or out the system [gP/m2] */
k0(time) = 1.0 ;
/* time [d] */

}

