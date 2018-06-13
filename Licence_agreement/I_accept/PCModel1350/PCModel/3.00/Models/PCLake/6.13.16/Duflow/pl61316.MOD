/* ########################################### */
/* ------------------------------------------- */
/* pclake version pl61316 */
/* ------------------------------------------- */
/* ########################################### */

/* ############################ */
/* State variables              */
/* ############################ */

WATER   sNH4W        [ 0.10000000149 ]                       gN/m3 ;    N in NH4 in lake water
WATER   sNO3W        [ 0.10000000149 ]                       gN/m3 ;    N in NO3 in lake water
WATER   sPO4W        [ 0.00999999978 ]                       gP/m3 ;    P in PO4 in lake water
WATER   sPAIMW       [ 0 ]                                   gP/m3 ;    P adsorbed onto IM in lake water
WATER   sSiO2W       [ 3 ]                                   gSi/m3 ;   Dissolved Si in lake water
WATER   sO2W         [ 10 ]                                  gO2/m3 ;   Oxygen in lake water
WATER   sDDetW       [ 2 ]                                   gDW/m3 ;   Detritus DW in lake water
WATER   sNDetW       [ 0.05000000075 ]                       gN/m3 ;    Detritus N in lake water
WATER   sPDetW       [ 0.00499999989 ]                       gP/m3 ;    Detritus P in lake water
WATER   sSiDetW      [ 0.01999999955 ]                       gSi/m3 ;   Detritus Si in lake water
WATER   sDIMW        [ 5 ]                                   gDIM/m3 ;  Inorganic matter in lake water
WATER   sDDiatW      [ 0.5 ]                                 gDW/m3 ;   Diatoms DW in lake water
WATER   sNDiatW      [ 0.05000000075 ]                       gN/m3 ;    Diatoms N in lake water
WATER   sPDiatW      [ 0.00499999989 ]                       gP/m3 ;    Diatoms P in lake water
WATER   sDGrenW      [ 0.5 ]                                 gDW/m3 ;   Green algae DW in lake water
WATER   sNGrenW      [ 0.05000000075 ]                       gN/m3 ;    Green algae N in lake water
WATER   sPGrenW      [ 0.00499999989 ]                       gP/m3 ;    Green algae P in lake water
WATER   sDBlueW      [ 3 ]                                   gDW/m3 ;   Blue-greens DW in lake water
WATER   sNBlueW      [ 0.30000000447 ]                       gN/m3 ;    Blue-greens N in lake water
WATER   sPBlueW      [ 0.02999999933 ]                       gP/m3 ;    Blue-greens P in lake water
WATER   sDZoo        [ 0.05000000075 ]                       gDW/m3 ;   Zooplankton DW in lake water
WATER   sNZoo        [ 0.00350000007 ]                       gN/m3 ;    Zooplankton N in lake water
WATER   sPZoo        [ 0.000499999996275 ]                   gP/m3 ;    Zooplankton P in lake water
BOTTOM  sDFiAd       [ 2 ]                                   gDW/m2 ;   Adult fish DW in lake water
BOTTOM  sDFiJv       [ 0.5 ]                                 gDW/m2 ;   Young fish DW in lake water
BOTTOM  sNFiAd       [ 0.20000000298 ]                       gN/m2 ;    Adult fish N in lake water
BOTTOM  sNFiJv       [ 0.05000000075 ]                       gN/m2 ;    Young fish N in lake water
BOTTOM  sPFiAd       [ 0.04399999976 ]                       gP/m2 ;    Adult fish P in lake water
BOTTOM  sPFiJv       [ 0.01099999994 ]                       gP/m2 ;    Young fish P in lake water
BOTTOM  sDPisc       [ 0.00999999978 ]                       gDW/m2 ;   Predatory fish DW in lake water
BOTTOM  sNH4S        [ 0.01999999955 ]                       gN/m2 ;    N in NH4 in lake sediment pore water
BOTTOM  sNO3S        [ 0.00200000009 ]                       gN/m2 ;    N in NO3 in lake sediment pore water
BOTTOM  sPO4S        [ 0.18170283715 ]                       gP/m2 ;    P in PO4 in lake sediment pore water
BOTTOM  sPAIMS       [ 17.9885982064 ]                       gP/m2 ;    P adsorbed onto IM in lake sediment
BOTTOM  sDDetS       [ 181.70300722 ]                        gDW/m2 ;   Detritus DW in lake sediment
BOTTOM  sNDetS       [ 4.54257524819 ]                       gN/m2 ;    Detritus N in lake sediment
BOTTOM  sPDetS       [ 0.4542575079 ]                        gP/m2 ;    Detritus P in lake sediment
BOTTOM  sSiDetS      [ 1.81703003159 ]                       gSi/m2 ;   Detritus Si in lake sediment
BOTTOM  sDHumS       [ 3452.35708303 ]                       gDW/m2 ;   Humus DW in lake sediment
BOTTOM  sNHumS       [ 172.617856724 ]                       gN/m2 ;    Humus N in lake sediment
BOTTOM  sPHumS       [ 17.2617850293 ]                       gP/m2 ;    Humus P in lake sediment
BOTTOM  sDIMS        [ 32706.5402707 ]                       gDIM/m2 ;  Inorganic matter in lake sediment
BOTTOM  sDDiatS      [ 0.00100000005 ]                       gDW/m2 ;   Diatoms DW on lake sediment
BOTTOM  sNDiatS      [ 0.00010000000624 ]                    gN/m2 ;    Diatoms N on lake sediment
BOTTOM  sPDiatS      [ 0.0000100000002515 ]                   gP/m2 ;    Diatoms P on lake sediment
BOTTOM  sDGrenS      [ 0.00100000005 ]                       gDW/m2 ;   Green algae DW on lake sediment
BOTTOM  sNGrenS      [ 0.00010000000624 ]                    gN/m2 ;    Green algae N on lake sediment
BOTTOM  sPGrenS      [ 0.0000100000002515 ]                   gP/m2 ;    Green algae P on lake sediment
BOTTOM  sDBlueS      [ 0.00100000005 ]                       gDW/m2 ;   Blue-greens DW on lake sediment
BOTTOM  sNBlueS      [ 0.00010000000624 ]                    gN/m2 ;    Blue-greens N on lake sediment
BOTTOM  sPBlueS      [ 0.0000100000002515 ]                   gP/m2 ;    Blue-greens P on lake sediment
BOTTOM  sDVeg        [ 1 ]                                   gDW/m2 ;   Vegetation DW in lake water
BOTTOM  sNVeg        [ 0.01999999955 ]                       gN/m2 ;    Vegetation N in lake water
BOTTOM  sPVeg        [ 0.00200000009 ]                       gP/m2 ;    Vegetation P in lake water
BOTTOM  sDBent       [ 1 ]                                   gDW/m2 ;   Zoobenthos DW in lake sediment
BOTTOM  sNBent       [ 0.0700000003 ]                        gN/m2 ;    Zoobenthos N in lake sediment
BOTTOM  sPBent       [ 0.00999999978 ]                       gP/m2 ;    Zoobenthos P in lake sediment
BOTTOM  sDepthWM     [ 0.5 ]                                 m ;        Depth of marsh water
WATER   sNH4WM       [ 0.10000000149 ]                       gN/m3 ;    N in NH4 in marsh water
WATER   sNO3WM       [ 0.10000000149 ]                       gN/m3 ;    N in NO3 in marsh water
WATER   sPO4WM       [ 0.00999999978 ]                       gP/m3 ;    P in PO4 in marsh water
WATER   sPAIMWM      [ 0 ]                                   gP/m3 ;    P adsorbed onto IM in marsh water
WATER   sSiO2WM      [ 3 ]                                   gSi/m3 ;   Dissolved Si in marsh water
WATER   sO2WM        [ 10 ]                                  gO2/m3 ;   Oxygen in marsh water
WATER   sDDetWM      [ 2 ]                                   gDW/m3 ;   Detritus DW in marsh water
WATER   sNDetWM      [ 0.05000000075 ]                       gN/m3 ;    Detritus N in marsh water
WATER   sPDetWM      [ 0.00499999989 ]                       gP/m3 ;    Detritus P in marsh water
WATER   sSiDetWM     [ 0.01999999955 ]                       gSi/m3 ;   Detritus Si in marsh water
WATER   sDIMWM       [ 5 ]                                   gDIM/m3 ;  Inorganic matter in marsh water
WATER   sDDiatWM     [ 0.5 ]                                 gDW/m3 ;   Diatoms DW in marsh water
WATER   sNDiatWM     [ 0.05000000075 ]                       gN/m3 ;    Diatoms N in marsh water
WATER   sPDiatWM     [ 0.00499999989 ]                       gP/m3 ;    Diatoms P in marsh water
WATER   sDGrenWM     [ 0.5 ]                                 gDW/m3 ;   Green algae DW in marsh water
WATER   sNGrenWM     [ 0.05000000075 ]                       gN/m3 ;    Green algae N in marsh water
WATER   sPGrenWM     [ 0.00499999989 ]                       gP/m3 ;    Green algae P in marsh water
WATER   sDBlueWM     [ 3 ]                                   gDW/m3 ;   Blue-greens DW in marsh water
WATER   sNBlueWM     [ 0.30000000447 ]                       gN/m3 ;    Blue-greens N in marsh water
WATER   sPBlueWM     [ 0.02999999933 ]                       gP/m3 ;    Blue-greens P in marsh water
WATER   sDZooM       [ 0.05000000075 ]                       gDW/m3 ;   Zooplankton DW in marsh water
WATER   sNZooM       [ 0.00350000007 ]                       gN/m3 ;    Zooplankton N in marsh water
WATER   sPZooM       [ 0.000499999996275 ]                   gP/m3 ;    Zooplankton P in marsh water
BOTTOM  sNH4SM       [ 1 ]                                   gN/m2 ;    N in NH4 in marsh sediment pore water
BOTTOM  sNO3SM       [ 0.00999999978 ]                       gN/m2 ;    N in NO3 in marsh sediment pore water
BOTTOM  sPO4SM       [ 0.18170283715 ]                       gP/m2 ;    P in PO4 in marsh sediment pore water
BOTTOM  sPAIMSM      [ 17.9885982064 ]                       gP/m2 ;    P adsorbed onto IM in marsh sediment
BOTTOM  sDDetSM      [ 181.70300722 ]                        gDW/m2 ;   Detritus DW in marsh sediment
BOTTOM  sNDetSM      [ 4.54257524819 ]                       gN/m2 ;    Detritus N in marsh sediment
BOTTOM  sPDetSM      [ 0.4542575079 ]                        gP/m2 ;    Detritus P in marsh sediment
BOTTOM  sSiDetSM     [ 1.81703003159 ]                       gSi/m2 ;   Detritus Si in marsh sediment
BOTTOM  sDHumSM      [ 3452.35708303 ]                       gDW/m2 ;   Humus DW in marsh sediment
BOTTOM  sNHumSM      [ 172.617856724 ]                       gN/m2 ;    Humus N in marsh sediment
BOTTOM  sPHumSM      [ 17.2617850293 ]                       gP/m2 ;    Humus Pin marsh sediment
BOTTOM  sDIMSM       [ 32706.5402707 ]                       gDIM/m2 ;  Inorganic matter in marsh sediment
BOTTOM  sDRootPhra   [ 5000 ]                                gDW/m2 ;   Root biomass DW in marsh sediment
BOTTOM  sDShootPhra  [ 1000 ]                                gDW/m2 ;   Shoot biomass DW in marsh water
BOTTOM  sNRootPhra   [ 99.9999977648 ]                       gN/m2 ;    Root biomass N in marsh sediment
BOTTOM  sNShootPhra  [ 19.999999553 ]                        gN/m2 ;    Shoot biomass N in marsh water
BOTTOM  sPRootPhra   [ 10.000000475 ]                        gP/m2 ;    Root biomass P in marsh sediment
BOTTOM  sPShootPhra  [ 2.00000009499 ]                       gP/m2 ;    Shoot biomass P in marsh water
BOTTOM  sDExtTotT    [ 36367.21336 ]                         gDW/m2 ;   Total amount of DW moved into or out from the system
BOTTOM  sNExtTotT    [ 178.830732 ]                          gN/m2 ;    Total amount of N moved into or out from the system
BOTTOM  sPExtTotT    [ 36.06459358 ]                         gP/m2 ;    Total amount of P moved into or out from the system
BOTTOM  sSiExtTotT   [ 8.007180153 ]                         gSi/m2 ;   Total amount of Si moved into or out from the system
BOTTOM  time         [ 0.0 ]                                 day ;      day of the year

/* ############################ */
/* Parameters                   */
/* ############################ */

PARM   InitCalc          [  0  ]                                   - ;                     If T skip calculation of initial values used in case of REINIT command
PARM   ConstDepth        [  1  ]                                   - ;                     If T water depth kept constant by daily dredging
PARM   InclTran          [  0  ]                                   - ;                     transport processes
PARM   InclPhytS         [  1  ]                                   - ;                     Include phytoplankton module
PARM   InclBed           [  1  ]                                   - ;                     Include vegetation module
PARM   InclWeb           [  1  ]                                   - ;                     Include food web module
PARM   InclMarsh         [  0  ]                                   - ;                     Include marsh zone
PARM   InclSeason        [  1  ]                                   - ;                     Include season
PARM   ReadTemp          [  0  ]                                   - ;                     If TRUE use measured time-series of water temperature otherwise sinus
PARM   ReadLOut          [  0  ]                                   - ;                     If TRUE use measured time-series of light otherwise sinus
PARM   ReadVWind         [  0  ]                                   - ;                     If TRUE use measured time-series of wind otherwise constant
PARM   BeginTime         [  0  ]                                   day ;                   begintime
PARM   EndTime           [  365  ]                                 day ;                   (=1 year)
PARM   YearZero          [  0  ]                                   - ;                     Note also  Day no 1 = 1 Jan of this year
PARM   cFetch            [  1000  ]                                m ;                     wind fetch
PARM   fMarsh            [  0  ]                                   m2_marsh/m2_lake ;      relative marsh area
PARM   fLutum            [  0.1  ]                                 - ;                     lutum content of inorg matter
PARM   fFeDIM            [  0.01  ]                                gFe/gDW ;               Fe content of inorg matter
PARM   fAlDIM            [  0.01  ]                                gAl/gDW ;               Al content of inorg matter
PARM   cTmAve            [  12  ]                                  oC ;                    average water temperature
PARM   cTmVar            [  10  ]                                  oC ;                    annual water temperature variation
PARM   cTimeLag          [  40  ]                                  day ;                   time lag for temperature
PARM   cVWind            [  5  ]                                   m/s ;                   average wind speed
PARM   cQInf             [  0  ]                                   mm/day ;                infiltration rate
PARM   cPBackLoad        [  0  ]                                   gP/m2/d ;               Background P loading
PARM   cNBackLoad        [  0  ]                                   gP/m2/d ;               Background N loading
PARM   cLDayAve          [  10000000  ]                            J/m2/day ;              annual average radiation
PARM   cLDayVar          [  8000000  ]                             J/m2/day ;              annual variation in radiation
PARM   cfDayAve          [  0.5  ]                                 - ;                     average day length
PARM   cfDayVar          [  0.2  ]                                 - ;                     annual variation in day length
PARM   fRefl             [  0.2  ]                                 - ;                     the fraction photosynthetically active radiation reflected at the surface
PARM   cExtWat           [  0.5  ]                                 m-1 ;                   background extinction
PARM   cDredInterval     [  9999000  ]                             y ;                     dredging interval
PARM   cDredStart        [  9999000  ]                             y ;                     first dredging year (should be n times  cDredInterval )
PARM   cDepthRef         [  0.0000000000000000000000000001  ]      m ;                     reference water depth for dredging
PARM   cLengDred         [  10  ]                                  day ;                   length of dredging period
PARM   fEffDred          [  0.95  ]                                - ;                     dredging efficiency (<10)
PARM   fEffDredBent      [  0.5  ]                                 - ;                     dredging efficiency for zoobenthos (<10)
PARM   fPAR              [  0.48  ]                                - ;                     fraction photosynthetically active radiation (PAR)
PARM   cExtSpDet         [  0.15  ]                                m2/gDW ;                specific extinction detritus
PARM   cExtSpIM          [  0.05  ]                                m2/gDW ;                specific extinction inert matter
PARM   fDTotS0           [  0.3  ]                                 g_solid/g_sediment ;    initial dry-weight fraction in sediment
PARM   fDOrgS0           [  0.1  ]                                 g/g ;                   initial organic fraction of sediment DW
PARM   fDDetS0           [  0.05  ]                                g/g ;                   initial detritus fraction of sediment organic matter
PARM   fSedPhyt0         [  0.01  ]                                g/g ;                   Fraction diatoms DW on lake sediment
PARM   fPInorgS0         [  0.0005  ]                              gP/gDW ;                initial inorg P fraction in sed
PARM   fPAdsS0           [  0.99  ]                                - ;                     initial adsorbed fraction of inorg P in sed
PARM   cPDDet0           [  0.0025  ]                              gP/gDW_Detritus ;       initial P fraction in detritus
PARM   cNDDet0           [  0.025  ]                               gN/gDW_Detritus ;       initial N fraction in detritus
PARM   cSiDDet0          [  0.01  ]                                gSi/gDW_Detritus ;      initial Si fraction in detritus Tentative
PARM   cPDHum0           [  0.005  ]                               gP/gDW_Detritus ;       initial P fraction in humus
PARM   cNDHum0           [  0.05  ]                                gN/gDW_Detritus ;       initial N fraction in humus
PARM   cPDPhyt0          [  0.01  ]                                gP/gDW ;                initial P fraction in algae
PARM   cNDPhyt0          [  0.1  ]                                 gN/gDW ;                initial N fraction in algae
PARM   cPDDiat0          [  0.01  ]                                gP/gDW ;                initial P fraction in diatoms
PARM   cNDDiat0          [  0.1  ]                                 gN/gDW ;                initial N fraction in diatoms
PARM   cPDGren0          [  0.01  ]                                gP/gDW ;                initial P fraction in green algae
PARM   cNDGren0          [  0.1  ]                                 gN/gDW ;                initial N fraction in green algae
PARM   cPDBlue0          [  0.01  ]                                gP/gDW ;                initial P fraction in blue-green algae
PARM   cNDBlue0          [  0.1  ]                                 gN/gDW ;                initial N fraction in blue-green algae
PARM   cPDVeg0           [  0.002  ]                               gP/gDW ;                initial P fraction in veg
PARM   cNDVeg0           [  0.02  ]                                gN/gDW ;                initial N fraction in veg
PARM   cSiDDiat          [  0.15  ]                                mgSi/mgDW ;             Si/DW ratio of daitoms
PARM   cPDZooRef         [  0.01  ]                                mgP/mgDW ;              reference P/C-ratio herb zooplankton
PARM   cNDZooRef         [  0.07  ]                                mgN/mgDW ;              reference N/C-ratio herb zooplankton
PARM   cPDBentRef        [  0.01  ]                                mgP/mgDW ;              reference P/C ratio of zoobenthos
PARM   cNDBentRef        [  0.07  ]                                mgN/mgDW ;              reference N/C ratio of zoobenthos
PARM   cPDFishRef        [  0.022  ]                               mgP/mgDW ;              reference P/C ratio of Fish
PARM   cNDFishRef        [  0.1  ]                                 mgN/mgDW ;              reference N/C ratio of Fish
PARM   cPDPisc           [  0.022  ]                               mgP/mgDW ;              reference P/C ratio of  Pi sc
PARM   cNDPisc           [  0.1  ]                                 mgN/mgDW ;              reference N/C ratio of  Pi sc
PARM   cDayApril1        [  91  ]                                  day ;                   April 1
PARM   cDayOct1          [  273  ]                                 day ;                   October 1
PARM   cLengChange       [  10  ]                                  day ;                   length of season change
PARM   cNLoadS           [  0  ]                                   gN/m2/day ;             N fertilizer to sediment
PARM   fNH4LoadS         [  0.5  ]                                 - ;                     NH4 fraction of N fertilizer to sediment
PARM   cDErosTot         [  0.1  ]                                 g/m2/day ;              Erosion input (tentative)
PARM   fSedErosIM        [  0.95  ]                                - ;                     instantly sedimentating fraction of IM
PARM   fDOrgSoil         [  0.1  ]                                 - ;                     fraction soil organic matter
PARM   cPDSoilOM         [  0.001  ]                               gP/gDW ;                P/day ratio of soil organic matter
PARM   cNDSoilOM         [  0.01  ]                                gN/gDW ;                N/day ratio of soil organic matter
PARM   cPO4Ground        [  0.1  ]                                 mgP/l ;                 PO4 cone in groundwater
PARM   cNH4Ground        [  1  ]                                   mgN/l ;                 NH4 cone in groundwater
PARM   cNO3Ground        [  0.1  ]                                 mgN/l ;                 NO3 cone in groundwater
PARM   cDepthS           [  0.1  ]                                 m ;                     sediment depth
PARM   cCPerDW           [  0.4  ]                                 gC/gDW ;                C content of organic matter
PARM   cRhoIM            [  2500000  ]                             g/m3_solid ;            density of sediment IM
PARM   cRhoOM            [  1400000  ]                             g/m3 ;                  density of sediment detritus
PARM   cTmRef            [  20  ]                                  oC ;                    reference temperature
PARM   cAerRoot          [  0.727  ]                               - ;                     coefficient for VWind^05
PARM   cAerLin           [ -0.371  ]                              s/day ;                 coefficient for VWind (is negative)
PARM   cAerSquare        [  0.0376  ]                              - ;                     coefficient for VWind^2
PARM   cThetaAer         [  1.024  ]                               1/e^oC ;                Temperature coeff for reaeration (Downing & Truesdale 1955)
PARM   cVSetIM           [  1  ]                                   m/day ;                 max sedimentation velocity of inert org matter (10)
PARM   cVSetDet          [  0.25  ]                                m/day ;                 max sedimentation velocity of detritus
PARM   cThetaSet         [  1.01  ]                                1/e^oC ;                temp parameter of sedimentation
PARM   cSuspMin          [  6.1  ]                                 - ;                     minimum value of logistic empirical suspended matter function
PARM   cSuspMax          [  25.2  ]                                - ;                     maximum value of logistic empirical suspended matter function
PARM   cSuspSlope        [  2.1  ]                                 - ;                     slope of logistic empirical suspended matter function
PARM   hDepthSusp        [  2  ]                                   - ;                     half sat value of depth in logistic empirical suspended matter function
PARM   cFetchRef         [  1000  ]                                m ;                     reference fetch
PARM   fLutumRef         [  0.2  ]                                 - ;                     reference lutum fraction (of sandy clay soils)
PARM   cSuspRef          [  0.5  ]                                 - ;                     reference supsended matter function
PARM   kVegResus         [  0.01  ]                                m2/gDW ;                rel resuspension reduction per g vegetation
PARM   kTurbFish         [  1  ]                                   g/g_fish/day ;          relative resuspension by adult fish browsing
PARM   kResusPhytMax     [  0.25  ]                                day-1 ;                 max phytopl resuspension
PARM   cResusPhytExp     [ -0.379  ]                              (gDW/m2/day)-1 ;        exp par for phytopl resuspension
PARM   cThetaMinW        [  1.07  ]                                - ;                     expon temp constant of mineralization in water
PARM   kDMinDetW         [  0.01  ]                                day-1 ;                 decomposition constant of detritus
PARM   hO2BOD            [  1  ]                                   mgO2/l ;                half-sat oxygen conc for BOD
PARM   O2PerNO3          [  1.5  ]                                 - ;                     mol O2 formed per mol NO3- ammonified
PARM   cThetaMinS        [  1.07  ]                                - ;                     expon temp constant of sediment mineralization
PARM   kDMinDetS         [  0.002  ]                               day-1 ;                 decomposition constant of sediment detritus
PARM   fRefrDetS         [  0.15  ]                                - ;                     refractory fraction of sed detritus
PARM   hNO3Denit         [  2  ]                                   mgN/l ;                 quadratic half-sat NO3 conc for denitrification
PARM   NO3PerC           [  0.8  ]                                 - ;                     mol NO3 denitrified per mol C mineralised
PARM   kDMinHum          [  0.00001  ]                             day-1 ;                 maximum decomposition constant of humic material (1D-5)
PARM   kNitrW            [  0.1  ]                                 day-1 ;                 nitrification rate constant in water
PARM   kNitrS            [  1  ]                                   day-1 ;                 nitrification rate constant in sediment
PARM   cThetaNitr        [  1.08  ]                                1/eoC ;                 temperature coefficient of nitrification
PARM   O2PerNH4          [  2  ]                                   - ;                     mol O2 used per mol NH4+ nitrified
PARM   hO2Nitr           [  2  ]                                   mgO2/l ;                half-sat O2 conc for nitrification in water
PARM   kPDifPO4          [  0.000072  ]                            m2/day ;                mol PO4 diffusion constant
PARM   kNDifNO3          [  0.000086  ]                            m2/day ;                mol NO3 diffusion constant
PARM   kNDifNH4          [  0.000112  ]                            m2/day ;                mol NH4 diffusion constant
PARM   kO2Dif            [  0.000026  ]                            m2/day ;                mol O2 diffusion constant
PARM   cThetaDif         [  1.02  ]                                1/eoC ;                 Temperature coefficient for diffusion
PARM   fDepthDifS        [  0.5  ]                                 - ;                     nutrient diffusion distance as fraction of sediment depth
PARM   cTurbDifNut       [  5  ]                                   - ;                     bioturbation factor for diffusion
PARM   cTurbDifO2        [  5  ]                                   - ;                     bioturbation factor for diffusion
PARM   kPSorp            [  0.05  ]                                day-1 ;                 P sorption rate constant not too high -> model speed
PARM   cRelPAdsD         [  0.00003  ]                             gP/gDW ;                max P adsorption per g DW
PARM   cRelPAdsFe        [  0.065  ]                               gP/gFe ;                max P adsorption per g Fe
PARM   cRelPAdsAl        [  0.134  ]                               gP/gAl ;                max P adsorption per g Al
PARM   cKPAdsOx          [  0.6  ]                                 m3/gP ;                 P adsorption affinity at oxidized conditions
PARM   fRedMax           [  0.9  ]                                 - ;                     max reduction factor of P adsorption affinity
PARM   coPO4Max          [  1  ]                                   mgP/l ;                 max SRP conc in pore water
PARM   kPChemPO4         [  0.03  ]                                day-1 ;                 chem PO4 loss rate
PARM   cDayManVeg1       [ -9999000  ]                            day ;                   first mowing day (default non-existent)
PARM   cDayManVeg2       [ -9999000  ]                            day ;                   second mowing day (Note 259 = 16 Sep)
PARM   fManVeg           [  0  ]                                   - ;                     Fraction removed by management  for submerged plants
PARM   cLengMan          [  10  ]                                  day ;                   length of mowing period
PARM   cYearStartBirds   [  0  ]                                   y ;                     first year of birds presence
PARM   cDayStartBirds    [  46  ]                                  day ;                   yearly first day of birds presence
PARM   cDayEndBirds      [  288  ]                                 day ;                   yearly last day of birds presence
PARM   cBirdsPerha       [  0  ]                                   n/ha ;                  number of birds per ha vegetated lake (Default = 0)
PARM   cDGrazPerBird     [  45  ]                                  gDW/coot/day ;          daily grazing of birds
PARM   hDVegBird         [  5  ]                                   gDW/m2 ;                half-sat vegetation biomassfor birds grazing
PARM   fDAssBird         [  0.5  ]                                 - ;                     birds assim efficiency
PARM   fDissEgesBird     [  0.25  ]                                - ;                     fraction dissolved nutrient of coot egestion
PARM   fDissMortVeg      [  0.25  ]                                - ;                     fraction dissolved nutrients from died plants
PARM   cLengAllo         [  15  ]                                  day ;                   duration of allocation and reallocation phase
PARM   cLengMort         [  15  ]                                  day ;                   duration of autumn mortality period
PARM   UseEmpUpt         [  0  ]                                   - ;                     false = do not use this empirical relation
PARM   fSedUptVegMax     [  0.998  ]                               - ;                     maximum sediment fraction of nutrient uptake
PARM   fSedUptVegCoef    [  2.66  ]                                - ;                     sigm regr coeff for sediment fraction of nutrient uptake
PARM   fSedUptVegExp     [ -0.83  ]                               - ;                     exponent in sigm regr for sediment fraction of nutrient uptake
PARM   fRootVegSum       [  0.1  ]                                 g_root_/_g_veg ;        root fraction outside growing season
PARM   fRootVegWin       [  0.6  ]                                 g_root_/_g_veg ;        root fraction outside growing season
PARM   fFloatVeg         [  0  ]                                   g_floating_/_g_shoot ;  floating fraction of shoot
PARM   fEmergVeg         [  0  ]                                   g_floating_/_g_shoot ;  emergent fraction of shoot
PARM   fDepth1Veg        [  0  ]                                   - ;                     max upper depth of submerged veget layer as fraction of water depth
PARM   fDepth2Veg        [  1  ]                                   - ;                     max lower depth of submerged veget layer as fraction of water depth
PARM   cDLayerVeg        [  0  ]                                   gDW/m2 ;                biomass of a single layer floating leaves
PARM   cCovSpVeg         [  0.5  ]                                 %_cover/gDW/m2 ;        specific cover
PARM   kMigrVeg          [  0.001  ]                               day-1 ;                 vegetation migration rate
PARM   cDVegIn           [  1  ]                                   gDW/m2 ;                external vegetation density
PARM   cTmInitVeg        [  9  ]                                   oC ;                    temperature for initial growth
PARM   cDCarrVeg         [  400  ]                                 gDW/m2 ;                max vegetation standing crop
PARM   cMuMaxVeg         [  0.2  ]                                 g/g_shoot/day ;         maximum growth rate of vegetation at 20oC
PARM   cQ10ProdVeg       [  1.2  ]                                 - ;                     temperature quotient of production
PARM   hLRefVeg          [  17  ]                                  W/m2_PAR ;              half-sat light at 20 oC
PARM   cExtSpVeg         [  0.01  ]                                m2/gDW ;                specific extinction
PARM   kDRespVeg         [  0.02  ]                                day-1 ;                 dark respiration rate of vegetation
PARM   cQ10RespVeg       [  2  ]                                   - ;                     temperature quotient of respiration
PARM   kMortVegSum       [  0.005  ]                               day-1 ;                 vegetation mortality rate in Spring and Summer (low)
PARM   fWinVeg           [  0.3  ]                                 - ;                     fraction surviving in winter
PARM   cDayWinVeg        [  259  ]                                 day ;                   end of growing season = 16 Sep
PARM   fDetWMortVeg      [  0.1  ]                                 - ;                     fraction of shoot mortality becoming water detritus
PARM   cPrefVegBird      [  1  ]                                   - ;                     edibility for birds
PARM   cVPUptMaxVeg      [  0.01  ]                                mgP/mgDW/day ;          maximum P uptake capacity of vegetation
PARM   cAffPUptVeg       [  0.2  ]                                 l/mgDW/day ;            initial P uptake affinity vegetation
PARM   cPDVegMin         [  0.0008  ]                              mgP/mg ;                minimum P/day ratio vegetation
PARM   cPDVegMax         [  0.0035  ]                              mgP/mgDW ;              maximum P/day ratio vegetation
PARM   cVNUptMaxVeg      [  0.1  ]                                 mgN/mgDW/day ;          maximum N uptake capacity of vegetation
PARM   cAffNUptVeg       [  0.2  ]                                 l/mgDW/day ;            initial N uptake affinity vegetation
PARM   cNDVegMin         [  0.01  ]                                mgN/mgDW ;              minimum N/day ratio vegetation
PARM   cNDVegMax         [  0.035  ]                               mgN/mgDW ;              maximum N/day ratio vegetation
PARM   cPACoefMin        [  1.5  ]                                 - ;                     minimum Poole-Atkins coefficient
PARM   cPACoefMax        [  2.5  ]                                 - ;                     maximum Poole-Atkins coefficient
PARM   hPACoef           [  3  ]                                   g/m2 ;                  decrease constant for PA coeff with DOMW
PARM   cSecchiPlus       [  0  ]                                   m ;                     maximum Secchi depth above water depth
PARM   cEuph             [  1.7  ]                                 - ;                     conversion constant Secchi depth -> euphotic depth
PARM   cCovSpPhyt        [  2  ]                                   %/gDW/m2 ;              specific coverage Tentative
PARM   cTmOptLoss        [  25  ]                                  oC ;                    optimum temp for grazing
PARM   cSigTmLoss        [  13  ]                                  oC ;                    temperature constant of grazing(sigma in Gaussian curve)
PARM   fDissMortPhyt     [  0.2  ]                                 - ;                     soluble nutrient fraction of died Algae
PARM   fDissLoss         [  0.25  ]                                - ;                     dissolved nutrient fraction of grazing loss
PARM   cMuMaxDiat        [  2  ]                                   day-1 ;                 maximum growth rate Diatoms
PARM   cTmOptDiat        [  18  ]                                  oC ;                    optimum temp diatoms
PARM   cSigTmDiat        [  20  ]                                  oC ;                    temperature constant diatoms(sigma in Gaussian curve)
PARM   cExtSpDiat        [  0.25  ]                                m2/gDW ;                specific extinction Diatoms
PARM   UseSteeleDiat     [  1  ]                                   - ;                     Flag 1 = use Steele function0 = use Lehman function
PARM   cLOptRefDiat      [  54  ]                                  W/m2 ;                  optimum PAR for Diatoms at 20 oC(Steele function)
PARM   hLRefDiat         [  1000  ]                                W/m2 ;                  half-sat PAR at 20 oC(Lehmann function) Fake value
PARM   cChDDiatMin       [  0.004  ]                               mgChl/mgDW ;            min chlorophyll/C ratio Diatoms
PARM   cChDDiatMax       [  0.012  ]                               mgChl/mgDW ;            max chlorophyll/C ratio Diatoms
PARM   kDRespDiat        [  0.1  ]                                 day-1 ;                 maintenance respiration constant diatoms(= 005 * MuMax)
PARM   kLossDiat         [  0.25  ]                                - ;                     grazing loss rate for Diatoms
PARM   kMortDiatW        [  0.01  ]                                day-1 ;                 mortality constant of Diatoms in water
PARM   kMortDiatS        [  0.05  ]                                day-1 ;                 mortality constant of sed Diatoms
PARM   cVSetDiat         [  0.5  ]                                 m/day ;                 sedimentation velocity Diatoms
PARM   cVPUptMaxDiat     [  0.01  ]                                mgP/mgDW/day ;          maximum P uptake capacity of Diatoms
PARM   cAffPUptDiat      [  0.2  ]                                 l/mgDW/day ;            initial P uptake affinity Diatoms
PARM   cPDDiatMin        [  0.0005  ]                              mgP/mgDW ;              minimum P/day ratio Diatoms
PARM   cPDDiatMax        [  0.005  ]                               mgP/mgDW ;              max P/day ratio Diatoms
PARM   cVNUptMaxDiat     [  0.07  ]                                mgN/mgDW/day ;          maximum N uptake capacity of Diatoms
PARM   cAffNUptDiat      [  0.2  ]                                 l/mgDW/day ;            initial N uptake affinity Diatoms
PARM   cNDDiatMin        [  0.01  ]                                mgN/mgDW ;              minimum N/day ratio Diatoms
PARM   cNDDiatMax        [  0.05  ]                                mgN/mgDW ;              max N/day ratio Diatoms
PARM   hSiAssDiat        [  0.09  ]                                mgSi/l ;                half sat Si for diatoms
PARM   cMuMaxGren        [  1.5  ]                                 day-1 ;                 maximum growth rate greens
PARM   cTmOptGren        [  25  ]                                  oC ;                    optimum temp of greens
PARM   cSigTmGren        [  15  ]                                  oC ;                    temperature constant greens(sigma in Gaussian curve)
PARM   cExtSpGren        [  0.25  ]                                m2/gDW ;                specific extinction greens
PARM   UseSteeleGren     [  0  ]                                   - ;                     Flag 1 = use Steele function0 = use Lehman function
PARM   hLRefGren         [  17  ]                                  W/m2 ;                  half-sat PAR for green algae at 20 oC(Lehmann function)
PARM   cLOptRefGren      [  1000  ]                                W/m2 ;                  optimum PAR at 20 oC(Steele function) Fake value
PARM   cChDGrenMin       [  0.01  ]                                mgChl/mgDW ;            min chlorophyll/C ratio greens
PARM   cChDGrenMax       [  0.02  ]                                mgChl/mgDW ;            max chlorophyll/C ratio greens
PARM   kDRespGren        [  0.075  ]                               day-1 ;                 maintenance respiration constant greens(= 005 * MuMax)
PARM   kLossGren         [  0.25  ]                                - ;                     grazing loss rate for greens
PARM   kMortGrenW        [  0.01  ]                                day-1 ;                 mortality constant of Diatoms in water
PARM   kMortGrenS        [  0.05  ]                                day-1 ;                 mortality constant greens
PARM   cVSetGren         [  0.2  ]                                 m/day ;                 sedimentation velocity of greens
PARM   cVPUptMaxGren     [  0.01  ]                                mgP/mgDW/day ;          maximum P uptake capacity of greens
PARM   cAffPUptGren      [  0.2  ]                                 l/mgDW/day ;            initial P uptake affinity greens
PARM   cPDGrenMin        [  0.0015  ]                              mgP/mgDW ;              minimum P/day ratio greens
PARM   cPDGrenMax        [  0.015  ]                               mgP/mgDW ;              max P/day ratio greens
PARM   cVNUptMaxGren     [  0.07  ]                                mgN/mgDW/day ;          maximum N uptake capacity of greens
PARM   cAffNUptGren      [  0.2  ]                                 l/mgDW/day ;            initial N uptake affinity greens
PARM   cNDGrenMin        [  0.02  ]                                mgN/mgDW ;              minimum N/day ratio greens
PARM   cNDGrenMax        [  0.1  ]                                 mgN/mgDW ;              max N/day ratio greens
PARM   hSiAssGren        [  0  ]                                   mgSi/l ;                half-sat Si conc for growth of green algae = 0
PARM   cMuMaxBlue        [  0.6  ]                                 day-1 ;                 maximum growth rate Bluegreens
PARM   cTmOptBlue        [  25  ]                                  oC ;                    optimum temp blue-greens
PARM   cSigTmBlue        [  12  ]                                  oC ;                    temperature constant blue-greens(sigma in Gaussian curve)
PARM   cExtSpBlue        [  0.35  ]                                m2/gDW ;                specific extinction Bluegreens
PARM   UseSteeleBlue     [  1  ]                                   - ;                     Flag 1 = use Steele function0 = use Lehman function
PARM   cLOptRefBlue      [  13.6  ]                                W/m2 ;                  optimum PAR for blue-greens at 20 oC(Steele function)
PARM   hLRefBlue         [  1000  ]                                W/m2 ;                  half-sat PAR at 20 oC(Lehmann function) Fake value
PARM   cChDBlueMin       [  0.005  ]                               mgChl/mgDW ;            min chlorophyll/C ratio Bluegreens
PARM   cChDBlueMax       [  0.015  ]                               mgChl/mgDW ;            max chlorophyll/C ratio Bluegreens
PARM   cCyDBlueMin       [  0.004  ]                               mgChl/mgDW ;            min c-phycocyanin/C ratio Bluegreens
PARM   cCyDBlueMax       [  0.06  ]                                mgChl/mgDW ;            max c-phycocyanin/C ratio Bluegreens
PARM   kDRespBlue        [  0.03  ]                                day-1 ;                 maintenance respiration constant blue-greens(= 005 * MuMax)
PARM   kLossBlue         [  0.03  ]                                - ;                     grazing loss rate for Blue-greens
PARM   kMortBlueW        [  0.01  ]                                day-1 ;                 mortality constant of blue-greens in water
PARM   kMortBlueS        [  0.2  ]                                 day-1 ;                 mortality constant Bluegreens
PARM   cVSetBlue         [  0.06  ]                                m/day ;                 sedimentation velocity Blue-greens
PARM   cVPUptMaxBlue     [  0.04  ]                                mgP/mgDW/day ;          maximum P uptake capacity of Bluegreens
PARM   cAffPUptBlue      [  0.8  ]                                 l/mgDW/day ;            initial P uptake affinity Bluegreens
PARM   cPDBlueMin        [  0.0025  ]                              mgP/mgDW ;              minimum P/day ratio Bluegreens
PARM   cPDBlueMax        [  0.025  ]                               mgP/mgDW ;              max P/day ratio blue-greens
PARM   cVNUptMaxBlue     [  0.07  ]                                mgN/mgDW/day ;          maximum N uptake capacity of Bluegreens
PARM   cAffNUptBlue      [  0.2  ]                                 l/mgDW/day ;            initial N uptake affinity Bluegreens
PARM   cNDBlueMin        [  0.03  ]                                mgN/mgDW ;              minimum N/day ratio Bluegreens
PARM   cNDBlueMax        [  0.15  ]                                mgN/mgDW ;              max N/DW ratio blue-greens
PARM   hSiAssBlue        [  0  ]                                   mgSi/l ;                half-sat Si conc for growth of blue-greens = 0
PARM   cDBentIn          [  0.01  ]                                gDW/m2 ;                external zoobenthos density
PARM   kMigrBent         [  0.001  ]                               day-1 ;                 zoobenthos migration rate
PARM   kMigrFish         [  0.001  ]                               day-1 ;                 fish migration rate
PARM   cDFiJvIn          [  0.005  ]                               gDW/m2 ;                external fish density
PARM   cDFiAdIn          [  0.005  ]                               gDW/m2 ;                external fish density
PARM   kHarvFishWin      [  0  ]                                   day-1 ;                 fish harvesting fraction in winter
PARM   kHarvFishSum      [  0  ]                                   day-1 ;                 fish harvesting fraction in summer
PARM   cDPiscIn          [  0.001  ]                               gDW/m2 ;                external  Pi sc density
PARM   kMigrPisc         [  0.001  ]                               day-1 ;                 Pi sc migration rate
PARM   kHarvPiscWin      [  0  ]                                   day-1 ;                 Pi sc harvesting fraction in winter
PARM   kHarvPiscSum      [  0  ]                                   day-1 ;                 Pi sc harvesting fraction in summer
PARM   cFiltMax          [  4.5  ]                                 ltr/mgDW/day ;          maximum filtering rate(when DOMW=0)
PARM   hFilt             [  1  ]                                   mgDW/l ;                half-sat food conc for filtering
PARM   cDCarrZoo         [  25  ]                                  mg/l ;                  carrying capacity of zooplankton
PARM   cPrefDiat         [  0.75  ]                                - ;                     selection factor for Diatoms
PARM   cPrefGren         [  0.75  ]                                - ;                     selection factor for Greens
PARM   cPrefBlue         [  0.125  ]                               - ;                     selection factor for Bluegreens Cal
PARM   cPrefDet          [  0.25  ]                                - ;                     selection factor for detritus
PARM   fDAssZoo          [  0.35  ]                                - ;                     DW-assimilation efficiency of herb zooplankton
PARM   fDissEgesZoo      [  0.25  ]                                - ;                     soluble nutrient fraction of by herbzoopl egested food
PARM   kDRespZoo         [  0.15  ]                                day-1 ;                 maintenance respiration constant herbzooplankton
PARM   kMortZoo          [  0.04  ]                                day-1 ;                 mortality constant herbzooplankton
PARM   fDissMortZoo      [  0.1  ]                                 - ;                     soluble nutrient fraction of died zooplankton
PARM   cTmOptZoo         [  25  ]                                  oC ;                    optimum temp zooplankton
PARM   cSigTmZoo         [  13  ]                                  oC ;                    temperature constant zooplankton(sigma in Gaussian curve)
PARM   cDCarrBent        [  10  ]                                  gDW/m2 ;                carrying capacity of zoobenthos
PARM   kDAssBent         [  0.1  ]                                 day-1 ;                 maximum assimilation rate
PARM   hDFoodBent        [  200  ]                                 g/m2 ;                  half-saturating food for zoobenthos
PARM   fDAssBent         [  0.3  ]                                 - ;                     C ass efficiency of zoobenthos
PARM   fDissEgesBent     [  0.25  ]                                - ;                     soluble nutrient fraction of by zoobenthos egested food
PARM   kDRespBent        [  0.005  ]                               day-1 ;                 maint respiration constant of zoobenthos
PARM   kMortBent         [  0.005  ]                               day-1 ;                 mortality constant of zoobenthos
PARM   fDissMortBent     [  0.1  ]                                 - ;                     soluble P fraction of died zoobenthos P
PARM   cTmOptBent        [  25  ]                                  oC ;                    optimum temp of zoobenthos
PARM   cSigTmBent        [  16  ]                                  oC ;                    temperature constant of zoobenthos(sigma in Gaussian curve)
PARM   fDBone            [  0.35  ]                                - ;                     fraction of fish C fixed in bones and scales
PARM   fPBone            [  0.5  ]                                 - ;                     fraction of fish P fixed in bones and scales
PARM   cDCarrFish        [  15  ]                                  gDW/m2 ;                carrying capacity of fish(= 100 gFW/m2Grimm 1983)
PARM   fDissEgesFish     [  0.25  ]                                - ;                     soluble nutrient fraction of by fish egested food
PARM   fDissMortFish     [  0.1  ]                                 - ;                     soluble nutrient fraction of died fish(excl bones and scales
PARM   cTmOptFish        [  25  ]                                  oC ;                    optimum temp of fish
PARM   cSigTmFish        [  10  ]                                  oC ;                    temperature constant of fish(sigma in Gaussian curve)
PARM   cDayReprFish      [  120  ]                                 - ;                     reproduction date of fish = 1 May
PARM   fReprFish         [  0.02  ]                                - ;                     yearly reproduction fraction of adult fish
PARM   fAgeFish          [  0.5  ]                                 - ;                     yearly ageing fraction of young fish
PARM   cRelVegFish       [  0.009  ]                               - ;                     decrease of fish feeding per % vegetation cover(max 001)
PARM   kDAssFiJv         [  0.12  ]                                day-1 ;                 maximum assimilation rate of young fish
PARM   hDZooFiJv         [  1.25  ]                                g/m2 ;                  half-saturating zooplankton biomass for young fish predation
PARM   fDAssFiJv         [  0.4  ]                                 - ;                     C assimilation efficiency of young fish
PARM   kDRespFiJv        [  0.01  ]                                day-1 ;                 maintenance respiration constant of young fish
PARM   kMortFiJv         [  0.00137  ]                             day-1 ;                 specific mortality of young fish(= 01 y-1)
PARM   kDAssFiAd         [  0.06  ]                                day-1 ;                 maximum assimilation rate of adult fish
PARM   hDBentFiAd        [  2.5  ]                                 g/m2 ;                  half-saturating zoobenthos biomass for adult fish predation
PARM   fDAssFiAd         [  0.4  ]                                 - ;                     C assimilation efficiency of adult fish
PARM   kDRespFiAd        [  0.004  ]                               day-1 ;                 maintenance respiration constant of adult fish
PARM   kMortFiAd         [  0.00027  ]                             day-1 ;                 specific mortality of adult fish(= 01 y-1)
PARM   cDCarrPiscMax     [  1.2  ]                                 gDW/m2 ;                maximum carrying capacity of  Pi sc(=75 kg/ha)
PARM   cDCarrPiscMin     [  0.1  ]                                 gDW/m2 ;                minimum carrying capacity of  Pi sc(=6 kg/ha)
PARM   cDCarrPiscBare    [  0.1  ]                                 gDW/m2 ;                carrying capacity of  Pi sc for lake without marsh zone
PARM   cDPhraMinPisc     [  50  ]                                  gDW/m2 ;                min reed biomass for  Pi sc
PARM   cCovVegMin        [  40  ]                                  % ;                     min submveg coverage for  Pi sc
PARM   cRelPhraPisc      [  0.075  ]                               gDW/m2/% ;              rel  Pi sc density per % reed if submveg absent
PARM   cRelVegPisc       [  0.03  ]                                gDW/m2/% ;              extra rel  Pi sc density per % reed if  aCovVeg  >  cCovVegMin
PARM   kDAssPisc         [  0.025  ]                               day-1 ;                 maximum assimilation rate
PARM   hDVegPisc         [  5  ]                                   g/m2 ;                  half-sat vegetation biomass for  Pi sc growth
PARM   hDFishPisc        [  1  ]                                   g/m2 ;                  half-saturating DFish for  Pi sc predation
PARM   fDAssPisc         [  0.4  ]                                 - ;                     C ass efficiency of  Pi sc
PARM   fDissEgesPisc     [  0.25  ]                                - ;                     soluble P fraction of by fish egested food
PARM   kDRespPisc        [  0.005  ]                               day-1 ;                 maint respiration constant of  Pi sc
PARM   kMortPisc         [  0.00027  ]                             day-1 ;                 specific mortality of  Pi sc = 01 y-1
PARM   fDissMortPisc     [  0.1  ]                                 - ;                     soluble nutrient fraction of died  Pi sc(excl bones and scales
PARM   cTmOptPisc        [  25  ]                                  oC ;                    optimum temp of  Pi sc
PARM   cSigTmPisc        [  10  ]                                  oC ;                    temperature constant of  Pi sc(sigma in Gaussian curve)
PARM   cDepthSM          [  0.1  ]                                 m ;                     sediment depth
PARM   kExchMaxM         [  1  ]                                   m3/m3_marshwater/day ;  maximum dispersive marsh water exchange coefficient
PARM   hfMarsh           [  0.1  ]                                 - ;                     rel marsh area where exchange is 50%
PARM   fDTotSM0          [  0.3  ]                                 g_solid/g_sediment ;    initial dry-weight fraction in sediment
PARM   fDOrgSM0          [  0.1  ]                                 g_AFDW\g_solid ;        initial organic fraction of sed
PARM   fDDetSM0          [  0.05  ]                                g/g ;                   initial detritus fraction of sediment organic matter
PARM   fPInorgSM0        [  0.0005  ]                              gP/gDW ;                initial inorg P fraction in sed
PARM   cPDPhra0          [  0.002  ]                               gP/gDW ;                initial P/day ratio of reed
PARM   cNDPhra0          [  0.02  ]                                gN/gDW ;                initial N/day ratio of reed
PARM   cDensStemPhra     [  61.5  ]                                m-2 ;                   density stem(+/- 139)
PARM   cTmInitPhra       [  8  ]                                   oC ;                    tempstart initial growth
PARM   fDAllPhra         [  0.3  ]                                 - ;                     allocation fraction
PARM   kDAllPhra         [  0.05  ]                                1/day ;                 allocation rate
PARM   cDStemPhra        [  6  ]                                   g/m ;                   average stem weight
PARM   cQ10ProdPhra      [  2  ]                                   - ;                     temp quotient of production
PARM   cMuPhraMax        [  0.03  ]                                1/day ;                 maximum growth rate reed
PARM   cDShootPhraMax    [  3500  ]                                gDW/m2 ;                max shoot biomass of reed
PARM   cCovSpPhra        [  0.1  ]                                 %_cover_per_gD/m2 ;     specific coverage
PARM   cPDPhraMin        [  0.0008  ]                              - ;                     minPhra P/day -ratio
PARM   cPDPhraMax        [  0.003  ]                               - ;                     maxPhra P/day -ratio
PARM   cNDPhraMin        [  0.008  ]                               - ;                     minPhra N/day -ratio
PARM   cNDPhraMax        [  0.03  ]                                - ;                     maxPhra N/day -ratio
PARM   cAffNUptPhra      [  0.0002  ]                              l/mgDW/DWay ;           N uptake affinity reed
PARM   cAffPUptPhra      [  0.0002  ]                              l/mgDW/DWay ;           P uptake affinity reed
PARM   cVNUptPhraMax     [  0.1  ]                                 mgN/mgD/day ;           max uptake rate N 001
PARM   cVPUptPhraMax     [  0.01  ]                                mgP/mgD/day ;           max uptake rate P 0001
PARM   kDRespPhra        [  0.001  ]                               1/day ;                 respiration rate of reed
PARM   cQ10RespPhra      [  2.5  ]                                 1/e^oC ;                temp quotient of respiration
PARM   fDayWin           [  0.52  ]                                - ;                     Start autumn
PARM   fDRealPhra        [  0.85  ]                                - ;                     reallocated fraction day
PARM   kDRealPhra        [  0.05  ]                                1/day ;                 reallocation rate day
PARM   kDMortShootPhra   [  0  ]                                   1/day ;                 mortality rate shoots
PARM   kDMortRootPhra    [  0.000391  ]                            1/day ;                 mortality rate roots
PARM   cDayWinPhra       [  259  ]                                 day ;                   begin autumn(16 sept)
PARM   cDayManPhra       [  255  ]                                 day ;                   time of management
PARM   fManPhra          [  0  ]                                   - ;                     fraction biomass loss by management
PARM   kDManShootPhra    [  1  ]                                   1/day ;                 rate of management
PARM   DaysPerYear       [  365  ]                                 d/y ;                   DaysPerYear
PARM   TenDays           [  10  ]                                  d ;                     TenDays
PARM   HoursPerDay       [  24  ]                                  h/d ;                   HoursPerDay
PARM   SecsPerDay        [  86400  ]                               s/d ;                   SecsPerDay
PARM   mmPerm            [  1000  ]                                mm/m ;                  mmPerm
PARM   m2Perha           [  10000  ]                               m2/ha ;                 m2Perha
PARM   mgPerg            [  1000  ]                                mg/g ;                  mgPerg
PARM   gPerkg            [  1000  ]                                g/kg ;                  gPerkg
PARM   gPerton           [  1000000  ]                             g/ton ;                 gPerton
PARM   PerCent           [  0.01  ]                                % ;                     PerCent
PARM   NearZero          [  0.0000000000000000000000000001  ]      - ;                     very small number used to avoid dividing by zero
PARM   molO2molC         [  2.6667  ]                              gO2/gC ;                ratio of molweights
PARM   molO2molN         [  2.2857  ]                              gO2/gN ;                ratio of molweights
PARM   molNmolC          [  1.1667  ]                              gN/gC ;                 ratio of molweights
PARM   cRhoWat           [  1000000  ]                             g/m3 ;                  density of water
PARM   Pi                [  3.14159265358979  ]                    - ;                     Pi (approx 314159)

/* ############################ */
/* External Variables           */
/* ############################ */

XT  mTemp  [ 0 ]  oC ;  measured time-series of water temperature
XT  mLOut  [ 0 ]  W/m2 ;  measured time-series of light
XT  mVWind  [ 0 ]  m/s ;  measured time-series of wind

/* ############################ */
/* Flow Variables               */
/* ############################ */

FLOW  Z  [ 2 ]  m ;  Depth of lake water

{

/* ############################ */
/* Auxiliary variables          */
/* ############################ */

sDepthW = Z;
/* Depth of lake water [m] */

/* ---------------------------------- */
/* Forcing */ 
/* ---------------------------------- */

sTime = time ;
/* time [d] */
TimeYears = sTime / DaysPerYear ;
/* Time in years [y] */
Day = sTime - int(TimeYears) * DaysPerYear ;
/* Time (daynumber) within the year (0-365) [-] */
Years = YearZero + TimeYears ;
/* Time in calendar years [y] */
if (0 == InclSeason) {
   uTm = cTmAve ;
} else if (ReadTemp == 1) {
   uTm = mTemp ;
} else {
   uTm = cTmAve - cTmVar * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysPerYear) ;
}
/* Forcing function temperature [oC] */
if (ReadVWind == 1) {
   uVWind = mVWind ;
} else {
   uVWind = cVWind ;
}
/* Forcing function wind speed [m/s] */
if (0 == InclSeason) {
   ufDay = cfDayAve ;
} else {
   ufDay = cfDayAve - cfDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) ;
}
/* day length [h/24h] */
if (0 == InclSeason) {
   uLDay = cLDayAve ;
} else if (ReadLOut == 1) {
   uLDay = 0.0 ;
} else {
   uLDay = cLDayAve - cLDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) ;
}
/* total daily radiation [total_J/m2/d] */
if (0 == InclSeason) {
   uLOut = uLDay / SecsPerDay / ufDay ;
} else if (ReadLOut == 1) {
   uLOut = mLOut / ufDay ;
} else {
   uLOut = uLDay / SecsPerDay / ufDay ;
}
/* average light intensity during daytime [total_J/m2/s_=_W/m2] */
uLPARSurf = fPAR * (1.0 - fRefl) * uLOut ;
/* average PAR at zero depth [J/m2/s_PAR_=_W/m2_PAR] */
aExtPhyt = cExtSpDiat * sDDiatW + cExtSpGren * sDGrenW + cExtSpBlue * sDBlueW ;
/* contribution of algae to extinction [m-1] */
aExtDet = cExtSpDet * sDDetW ;
/* detrital contribution to extinction [m-1] */
aExtIM = cExtSpIM * sDIMW ;
/* contribution of inert matter to extinction [m-1] */
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
akExchM = 0.0 ;
afVolMarsh = 0.0 ;
akExchL = 0.0 ;

/* ---------------------------------- */
/* General */ 
/* ---------------------------------- */

oDPhytW = sDDiatW + sDGrenW + sDBlueW ;
/* total DW phytoplankton in lake water [g/m3] */

/* ---------------------------------- */
/* Lake_water_total */ 
/* ---------------------------------- */

oPPhytW = sPDiatW + sPGrenW + sPBlueW ;
/* total P phytoplankton in lake water [g/m3] */
oNPhytW = sNDiatW + sNGrenW + sNBlueW ;
/* total N phytoplankton in lake water [g/m3] */
if (InclPhytS == 1) {
   aDPhytS = sDDiatS + sDGrenS + sDBlueS ;
} else {
   aDPhytS = NearZero ;
}
/* total DW phytoplankton on lake sediment [g/m2] */
if (InclPhytS == 1) {
   aPPhytS = sPDiatS + sPGrenS + sPBlueS ;
} else {
   aPPhytS = NearZero ;
}
/* total P phytoplankton on lake sediment [g/m2] */
if (InclPhytS == 1) {
   aNPhytS = sNDiatS + sNGrenS + sNBlueS ;
} else {
   aNPhytS = NearZero ;
}
/* total N phytoplankton on lake sediment [g/m2] */
oDOMW = sDDetW + oDPhytW ;
/* organic seston [gDW/m3] */
oDSestW = oDOMW + sDIMW ;
/* total seston [mgDW/l] */
oPOMW = oPPhytW + sPDetW ;
/* organic P in water [gP/m3] */
oPSestW = oPPhytW + sPDetW + sPAIMW ;
/* total seston P (incl adsorbed [gP/m3] */
oPInorgW = sPO4W + sPAIMW ;
/* inorganic P in water [gP/m3] */
oPTotW = oPSestW + sPO4W ;
/* total P in water (excl animals AND vegetation) [gP/m3] */
oNDissW = sNO3W + sNH4W ;
/* SRN in water [gN/m3] */
oNOMW = oNPhytW + sNDetW ;
/* orgseston N [gN/m3] */
oNSestW = oNOMW ;
/* total seston N [gN/m3] */
oNkjW = oNSestW + sNH4W ;
/* kjeldahl N in water [gN/m3] */
oNTotW = oNkjW + sNO3W ;
/* total N in water (without animals AND vegetation) [gN/m3] */

/* ---------------------------------- */
/* Lake_sediment_total */ 
/* ---------------------------------- */

bPorS = (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat / ( fDTotS0 + (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat ) ;
/* porosity [m3_water_m-3_sediment] */
bPorCorS = ((bPorS )^ (bPorS + 1.0)) ;
/* sediment porosity corrected for tortuosity [-] */
aDTotS = sDIMS + sDHumS + sDDetS ;
/* total sediment (excl biota) [g/m2] */
aRhoTotS = aDTotS / cDepthS ;
/* (apparent) bulk density of sediment [g_solid_m-3_sediment] */
aRhoSolidS = (sDIMS * cRhoIM + (sDHumS + sDDetS) * cRhoOM) / aDTotS ;
/* average solid density [g/m-3_solid] */
afDTotS = 1.0 / (1.0 + bPorS/(1.0-bPorS) * cRhoWat / aRhoSolidS) ;
/* sediment dry-weight fraction [g_solid_g-1_sediment] */
afDOrgS = (sDHumS + sDDetS) / aDTotS ;
/* total organic fraction of sediment DW [-] */
afDetS = sDDetS / (sDHumS + sDDetS) ;
/* detrital fraction of sediment organic DW [-] */
afDetTotS = sDDetS / (sDIMS + sDHumS + sDDetS) ;
/* detrital fraction of total sediment DW [-] */
aPInorgS = sPO4S + sPAIMS ;
/* inorganic P in sediment [gP/m2] */
aPTotAvailS = sPDetS + aPInorgS + aPPhytS ;
/* total P in sediment (excl humus animals AND vegetation) [gP/m2] */
aPTotS = aPTotAvailS + sPHumS ;
/* total P in sediment (excl animals AND vegetation) [gP/m2] */
afPInorgS = aPInorgS / aDTotS ;
/* fraction inorganic P in sediment [gP/gD] */
afPTotS = aPTotS / aDTotS ;
/* total P fraction in sediment [gP/gD] */
afPO4S = sPO4S / (aPTotAvailS + NearZero) ;
/* fraction dissolved P in sediment [-] */
oPO4S = sPO4S / cDepthS / bPorS ;
/* conc dissolved P in interstitial water [gP/m3] */
aNDissS = sNH4S + sNO3S ;
/* total dissolved N in pore water [gN/m2] */
aNkjAvailS = sNDetS + aNPhytS + sNH4S ;
/* kjeldahl N in sediment excl humus [gN/m2] */
aNkjS = aNkjAvailS + sNHumS ;
/* kjeldahl N in sediment [gN/m2] */
aNTotAvailS = aNkjAvailS + sNO3S ;
/* total N in sediment excl humus [gN/m2] */
aNTotS = aNkjS + sNO3S ;
/* total N in sediment [gN/m2] */
afNInorgS = aNDissS / aDTotS ;
/* fraction inorganic N in sediment [gN/gD] */
afNTotS = aNTotS / aDTotS ;
/* total N fraction in sediment [gN/gD] */
oNO3S = sNO3S / cDepthS / bPorS ;
/* conc dissolved N-NO3 in interstitial water [gN/m3] */
oNH4S = sNH4S / cDepthS / bPorS ;
/* conc dissolved N-NH4 in interstitial water [gN/m3] */
oNDissS = aNDissS / cDepthS / bPorS ;
/* Dissolved N conc in sediment needed for calc of veg uptake rate [mgN/l] */

/* ---------------------------------- */
/* Nutrient_D/W_ratios */ 
/* ---------------------------------- */

rPDIMW = sPAIMW / sDIMW ;
/* P/D ratio of water DIM [gP/gD] */
rPDIMS = sPAIMS / sDIMS ;
/* P/D ratio of sediment DIM [gP/gD] */
rPDDetW = sPDetW / (sDDetW+NearZero) ;
/* P/D ratio of water detritus [gP/gD] */
rNDDetW = sNDetW / (sDDetW+NearZero) ;
/* N/D ratio of water detritus [gN/gD] */
rSiDDetW = sSiDetW / (sDDetW+NearZero) ;
/* Si/D ratio of water detritus [gSi/gD] */
rPDHumS = sPHumS / (sDHumS+NearZero) ;
/* P content of sediment OM [gP/gDW] */
rNDHumS = sNHumS / (sDHumS+NearZero) ;
/* N content of sediment OM [gN/gDW] */
rPDDetS = sPDetS / (sDDetS+NearZero) ;
/* P content of sediment detritus [gP/gDW] */
rNDDetS = sNDetS / (sDDetS+NearZero) ;
/* N content of sediment detritus [gN/gDW] */
rSiDDetS = sSiDetS / (sDDetS+NearZero) ;
/* Si content of sediment detritus [gSi/gDW] */

/* ---------------------------------- */
/* Marsh_water_total */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   oDPhytWM = sDDiatWM + sDGrenWM + sDBlueWM ;
} else {
   oDPhytWM = 0.0 ;
}
/* total DW phytoplankton in marsh water [gSi/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPPhytWM = sPDiatWM + sPGrenWM + sPBlueWM ;
} else {
   oPPhytWM = 0.0 ;
}
/* total P phytoplankton in marsh water [gSi/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNPhytWM = sNDiatWM + sNGrenWM + sNBlueWM ;
} else {
   oNPhytWM = 0.0 ;
}
/* total N phytoplankton in marsh water [gSi/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oSiDiatWM = cSiDDiat * sDDiatWM ;
} else {
   oSiDiatWM = 0.0 ;
}
/* total Si diatoms in marsh water [gSi/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oDOMWM = sDDetWM + oDPhytWM ;
} else {
   oDOMWM = 0.0 ;
}
/* organic seston [mgD/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oDSestWM = oDOMWM + sDIMWM ;
} else {
   oDSestWM = 0.0 ;
}
/* total seston [mgDW/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPOMWM = oPPhytWM + sPDetWM ;
} else {
   oPOMWM = 0.0 ;
}
/* organic P in water [mgP/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPSestWM = oPPhytWM + sPDetWM + sPAIMWM ;
} else {
   oPSestWM = 0.0 ;
}
/* total seston P(incl adsorbed [mgP/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPInorgWM = sPO4WM + sPAIMWM ;
} else {
   oPInorgWM = 0.0 ;
}
/* inorganic P in water [mgP/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPTotWM = oPSestWM + sPO4WM ;
} else {
   oPTotWM = 0.0 ;
}
/* total P in water [mgP/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNDissWM = sNO3WM + sNH4WM ;
} else {
   oNDissWM = 0.0 ;
}
/* SRN in water [mgN/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNOMWM = oNPhytWM + sNDetWM ;
} else {
   oNOMWM = 0.0 ;
}
/* orgseston N [mgN/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNSestWM = oNOMWM ;
} else {
   oNSestWM = 0.0 ;
}
/* total seston N [mgN/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNkjWM = oNSestWM + sNH4WM ;
} else {
   oNkjWM = 0.0 ;
}
/* kjeldahl N in water [mgN/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNTotWM = oNkjWM + sNO3WM ;
} else {
   oNTotWM = 0.0 ;
}
/* total N in water [mgN/l] */

/* ---------------------------------- */
/* Marsh_sediment_total */ 
/* ---------------------------------- */

bPorSM = (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) / cRhoWat /(fDTotSM0 + (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) / cRhoWat) ;
/* porosity [m3_water_m-3_sediment] */
bPorCorSM = ((bPorSM )^ (bPorSM + 1.0)) ;
/* sediment porosity corrected for tortuosity [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aDTotSM = sDIMSM + sDHumSM + sDDetSM ;
} else {
   aDTotSM = 0.0 ;
}
/* total sediment(excl biota) [g/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aRhoTotSM = aDTotSM / cDepthSM ;
} else {
   aRhoTotSM = 0.0 ;
}
/* (apparent) bulk density of sediment [g_solid_m-3_sediment] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aRhoSolidSM = (sDIMSM * cRhoIM +(sDHumSM + sDDetSM) * cRhoOM) / aDTotSM ;
} else {
   aRhoSolidSM = 0.0 ;
}
/* average solid density [g_m-3_solid] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afDTotSM = 1.0 /(1.0 + bPorSM/(1.0-bPorSM) * cRhoWat / aRhoSolidSM) ;
} else {
   afDTotSM = 0.0 ;
}
/* sediment dry-weight fraction [g_solid_g-1_sediment] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afDOrgSM = (sDHumSM + sDDetSM) / aDTotSM ;
} else {
   afDOrgSM = 0.0 ;
}
/* total organic fraction of sediment DW [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afDetSM = sDDetSM /(sDHumSM + sDDetSM) ;
} else {
   afDetSM = 0.0 ;
}
/* detrital fraction of sediment organic DW [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afDetTotSM = sDDetSM /(sDIMSM + sDHumSM + sDDetSM) ;
} else {
   afDetTotSM = 0.0 ;
}
/* detrital fraction of total sediment DW [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPInorgSM = sPO4SM + sPAIMSM ;
} else {
   aPInorgSM = 0.0 ;
}
/* inorganic P in sediment [gP/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPTotAvailSM = sPDetSM + aPInorgSM ;
} else {
   aPTotAvailSM = 0.0 ;
}
/* total P in sediment(excl humusanimals AND vegetation) [gP/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPTotSM = aPTotAvailSM + sPHumSM ;
} else {
   aPTotSM = 0.0 ;
}
/* total P in sediment(excl animals AND vegetation) [gP/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afPInorgSM = aPInorgSM / aDTotSM ;
} else {
   afPInorgSM = 0.0 ;
}
/* fraction inorganic P in sediment [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afPTotSM = aPTotSM / aDTotSM ;
} else {
   afPTotSM = 0.0 ;
}
/* total P fraction in sediment [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afPO4SM = sPO4SM /(aPTotAvailSM + NearZero) ;
} else {
   afPO4SM = 0.0 ;
}
/* fraction dissolved P in sediment [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oPO4SM = sPO4SM / cDepthSM / bPorSM ;
} else {
   oPO4SM = 0.0 ;
}
/* conc dissolved P in interstitial water [gP/m3] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNDissSM = sNH4SM + sNO3SM ;
} else {
   aNDissSM = 0.0 ;
}
/* total dissolved N in pore water [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNkjAvailSM = sNDetSM + sNH4SM ;
} else {
   aNkjAvailSM = 0.0 ;
}
/* kjeldahl N in sedimentexcl humus [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNkjSM = aNkjAvailSM + sNHumSM ;
} else {
   aNkjSM = 0.0 ;
}
/* kjeldahl N in sediment [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNTotAvailSM = aNkjAvailSM + sNO3SM ;
} else {
   aNTotAvailSM = 0.0 ;
}
/* total N in sedimentexcl humus [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNTotSM = aNkjSM + sNO3SM ;
} else {
   aNTotSM = 0.0 ;
}
/* total N in sediment [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afNInorgSM = aNDissSM / aDTotSM ;
} else {
   afNInorgSM = 0.0 ;
}
/* fraction inorganic N in sediment [gN/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afNTotSM = aNTotSM / aDTotSM ;
} else {
   afNTotSM = 0.0 ;
}
/* total N fraction in sediment [gN/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNO3SM = sNO3SM / cDepthSM / bPorSM ;
} else {
   oNO3SM = 0.0 ;
}
/* conc dissolved N-NO3 in interstitial marsh water [gN/m3] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNH4SM = sNH4SM / cDepthSM / bPorSM ;
} else {
   oNH4SM = 0.0 ;
}
/* conc dissolved N-NH4 in interstitial marsh water [gN/m3] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   oNDissSM = aNDissSM / cDepthSM / bPorSM ;
} else {
   oNDissSM = 0.0 ;
}
/* Dissolved N conc in marsh sediment [mgN/l] */

/* ---------------------------------- */
/* Marsh_nutrient_D/W_ratios */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDIMWM = sPAIMWM / sDIMWM ;
} else {
   rPDIMWM = 0.0 ;
}
/* P/D ratio of DIM in marsh water [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDIMSM = sPAIMSM / sDIMSM ;
} else {
   rPDIMSM = 0.0 ;
}
/* P/D ratio of DIM marsh sediment [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDDetWM = sPDetWM /(sDDetWM+NearZero) ;
} else {
   rPDDetWM = 0.0 ;
}
/* P/D ratio of marsh water detritus [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rNDDetWM = sNDetWM /(sDDetWM+NearZero) ;
} else {
   rNDDetWM = 0.0 ;
}
/* N/D ratio of marsh water detritus [gN/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rSiDDetWM = sSiDetWM /(sDDetWM+NearZero) ;
} else {
   rSiDDetWM = 0.0 ;
}
/* Si/D ratio of marsh water detritus [gSi/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDHumSM = sPHumSM /(sDHumSM+NearZero) ;
} else {
   rPDHumSM = 0.0 ;
}
/* P content of marsh sediment OM [gP/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rNDHumSM = sNHumSM /(sDHumSM+NearZero) ;
} else {
   rNDHumSM = 0.0 ;
}
/* N content of marsh sediment OM [gN/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDDetSM = sPDetSM /(sDDetSM+NearZero) ;
} else {
   rPDDetSM = 0.0 ;
}
/* P content of marsh sediment detritus [gP/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rNDDetSM = sNDetSM /(sDDetSM+NearZero) ;
} else {
   rNDDetSM = 0.0 ;
}
/* N content of marsh sediment detritus [gN/gDW] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rSiDDetSM = sSiDetSM /(sDDetSM+NearZero) ;
} else {
   rSiDDetSM = 0.0 ;
}
/* Si content of marsh sediment detritus [gSi/gDW] */

/* ---------------------------------- */
/* Marsh_total */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   aDTotM = ((sDIMWM + sDDetWM + oDPhytWM + sDZooM) * sDepthWM + sDIMSM + sDHumSM + sDDetSM + sDShootPhra + sDRootPhra) * fMarsh ;
} else {
   aDTotM = 0.0 ;
}
/* total D in marsh [gD/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPTotM = ((sPO4WM + sPDetWM + sPAIMWM + oPPhytWM + sPZooM) * sDepthWM + sPO4SM + sPHumSM + sPDetSM + sPAIMSM + sPShootPhra + sPRootPhra) * fMarsh ;
} else {
   aPTotM = 0.0 ;
}
/* total P in marsh [gP/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNTotM = ((sNH4WM + sNO3WM + sNDetWM + oNPhytWM + sNZooM) * sDepthWM + sNH4SM + sNO3SM + sNHumSM + sNDetSM + sNShootPhra + sNRootPhra) * fMarsh ;
} else {
   aNTotM = 0.0 ;
}
/* total N in marsh [gN/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aSiTotM = ((sSiO2WM + sSiDetWM + oSiDiatWM) * sDepthWM + sSiDetSM) * fMarsh ;
} else {
   aSiTotM = 0.0 ;
}
/* total Si in marsh [gSi/m2] */

/* ---------------------------------- */
/* Transport */ 
/* ---------------------------------- */

iPPulse = 0.0 ;
uPLoadSeason = 0.0 ;
uPLoad = 0.0 ;
uPLoadPO4 = 0.0 ;
uPLoadOrg = 0.0 ;
uPLoadPhytTot = 0.0 ;
uPLoadDet = 0.0 ;
uPLoadAIM = 0.0 ;
iNPulse = 0.0 ;
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
uDLoadDiat = 0.0 ;
uPLoadDiat = 0.0 ;
uNLoadDiat = 0.0 ;
uDLoadGren = 0.0 ;
uPLoadGren = 0.0 ;
uNLoadGren = 0.0 ;
uDLoadBlue = 0.0 ;
uPLoadBlue = 0.0 ;
uNLoadBlue = 0.0 ;
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
wDDilDiat = 0.0 ;
wPDilDiat = 0.0 ;
wNDilDiat = 0.0 ;
wDDilGren = 0.0 ;
wPDilGren = 0.0 ;
wNDilGren = 0.0 ;
wDDilBlue = 0.0 ;
wPDilBlue = 0.0 ;
wNDilBlue = 0.0 ;
wDDilPhyt = 0.0 ;
wPDilPhyt = 0.0 ;
wNDilPhyt = 0.0 ;
wDOutflTot = 0.0 ;
wPOutflTot = 0.0 ;
wNOutflTot = 0.0 ;
wDTranDiat = 0.0 ;
wPTranDiat = 0.0 ;
wNTranDiat = 0.0 ;
wDTranGren = 0.0 ;
wPTranGren = 0.0 ;
wNTranGren = 0.0 ;
wDTranBlue = 0.0 ;
wPTranBlue = 0.0 ;
wNTranBlue = 0.0 ;
wDTranPhyt = 0.0 ;
wPTranPhyt = 0.0 ;
wNTranPhyt = 0.0 ;
uSiLoadSiO2 = 0.0 ;
uSiLoadDet = 0.0 ;
uSiLoadDiat = 0.0 ;
uSiLoad = 0.0 ;
wSiDilSiO2 = 0.0 ;
wSiDilDet = 0.0 ;
wSiDilDiat = 0.0 ;
wSiOutflTot = 0.0 ;
wSiTranSiO2 = 0.0 ;
wSiTranDetW = 0.0 ;
tSiTranTotT = 0.0 ;
wDTranZoo = 0.0 ;
wPTranZoo = 0.0 ;
wNTranZoo = 0.0 ;
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
wSiDilTot = 0.0 ;
tDTranTotT = 0.0 ;
tPTranTotT = 0.0 ;
tNTranTotT = 0.0 ;

/* ---------------------------------- */
/* Exchange_fluxes_for_marsh */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchIMM = akExchM *(sDIMW - sDIMWM) ;
} else {
   wDExchIMM = 0.0 ;
}
/* exchange flux of DW in IMM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchPO4M = akExchM *(sPO4W - sPO4WM) ;
} else {
   wPExchPO4M = 0.0 ;
}
/* exchange flux of P in PO4M between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchAIMM = akExchM *(sPAIMW - sPAIMWM) ;
} else {
   wPExchAIMM = 0.0 ;
}
/* exchange flux of P in AIMM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchNH4M = akExchM *(sNH4W - sNH4WM) ;
} else {
   wNExchNH4M = 0.0 ;
}
/* exchange flux of N in NH4M between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchNO3M = akExchM *(sNO3W - sNO3WM) ;
} else {
   wNExchNO3M = 0.0 ;
}
/* exchange flux of N in NO3M between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchSiO2M = akExchM *(sSiO2W - sSiO2WM) ;
} else {
   wSiExchSiO2M = 0.0 ;
}
/* exchange flux of Si in hSiO2M between marsh and lake water [gS/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wO2ExchM = akExchM *(sO2W - sO2WM) ;
} else {
   wO2ExchM = 0.0 ;
}
/* exchange flux of O2 in hM between marsh and lake water [gO2/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchDetM = akExchM *(sDDetW - sDDetWM) ;
} else {
   wDExchDetM = 0.0 ;
}
/* exchange flux of DW in DetM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchDetM = akExchM *(sPDetW - sPDetWM) ;
} else {
   wPExchDetM = 0.0 ;
}
/* exchange flux of P in DetM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchDetM = akExchM *(sNDetW - sNDetWM) ;
} else {
   wNExchDetM = 0.0 ;
}
/* exchange flux of N in DetM between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchDetM = akExchM *(sSiDetW - sSiDetWM) ;
} else {
   wSiExchDetM = 0.0 ;
}
/* exchange flux of Si in hDetM between marsh and lake water [gSi/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchDiatM = akExchM *(sDDiatW - sDDiatWM) ;
} else {
   wDExchDiatM = 0.0 ;
}
/* exchange flux of DW in DiatM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchDiatM = akExchM *(sPDiatW - sPDiatWM) ;
} else {
   wPExchDiatM = 0.0 ;
}
/* exchange flux of P in DiatM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchDiatM = akExchM *(sNDiatW - sNDiatWM) ;
} else {
   wNExchDiatM = 0.0 ;
}
/* exchange flux of N in DiatM between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchDiatM = cSiDDiat * wDExchDiatM ;
} else {
   wSiExchDiatM = 0.0 ;
}
/* exchange flux of Si in DiatM between marsh and lake water [gS/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchGrenM = akExchM *(sDGrenW - sDGrenWM) ;
} else {
   wDExchGrenM = 0.0 ;
}
/* exchange flux of DW in GrenM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchGrenM = akExchM *(sPGrenW - sPGrenWM) ;
} else {
   wPExchGrenM = 0.0 ;
}
/* exchange flux of P in GrenM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchGrenM = akExchM *(sNGrenW - sNGrenWM) ;
} else {
   wNExchGrenM = 0.0 ;
}
/* exchange flux of N in GrenM between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchBlueM = akExchM *(sDBlueW - sDBlueWM) ;
} else {
   wDExchBlueM = 0.0 ;
}
/* exchange flux of DW in BlueM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchBlueM = akExchM *(sPBlueW - sPBlueWM) ;
} else {
   wPExchBlueM = 0.0 ;
}
/* exchange flux of P in BlueM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchBlueM = akExchM *(sNBlueW - sNBlueWM) ;
} else {
   wNExchBlueM = 0.0 ;
}
/* exchange flux of N in BlueM between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchZooM = akExchM *(sDZoo - sDZooM) ;
} else {
   wDExchZooM = 0.0 ;
}
/* exchange flux of DW in ZooM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchZooM = akExchM *(sPZoo - sPZooM) ;
} else {
   wPExchZooM = 0.0 ;
}
/* exchange flux of P in ZooM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchZooM = akExchM *(sNZoo - sNZooM) ;
} else {
   wNExchZooM = 0.0 ;
}
/* exchange flux of N in ZooM between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchIM = akExchL *(sDIMW - sDIMWM) ;
} else {
   wDExchIM = 0.0 ;
}
/* exchange flux of DW in IM between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchPO4 = akExchL *(sPO4W - sPO4WM) ;
} else {
   wPExchPO4 = 0.0 ;
}
/* exchange flux of P in PO4 between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchAIM = akExchL *(sPAIMW - sPAIMWM) ;
} else {
   wPExchAIM = 0.0 ;
}
/* exchange flux of P in AIM between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchNH4 = akExchL *(sNH4W - sNH4WM) ;
} else {
   wNExchNH4 = 0.0 ;
}
/* exchange flux of N in NH4 between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchNO3 = akExchL *(sNO3W - sNO3WM) ;
} else {
   wNExchNO3 = 0.0 ;
}
/* exchange flux of N in NO3 between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchSiO2 = akExchL *(sSiO2W - sSiO2WM) ;
} else {
   wSiExchSiO2 = 0.0 ;
}
/* exchange flux of Si in SiO2 between marsh and lake water [gSi/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wO2Exch = akExchL *(sO2W - sO2WM) ;
} else {
   wO2Exch = 0.0 ;
}
/* exchange flux of O2 in O2 between marsh and lake water [gO2/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchDet = akExchL *(sDDetW - sDDetWM) ;
} else {
   wDExchDet = 0.0 ;
}
/* exchange flux of DW in Det between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchDet = akExchL *(sPDetW - sPDetWM) ;
} else {
   wPExchDet = 0.0 ;
}
/* exchange flux of P in Det between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchDet = akExchL *(sNDetW - sNDetWM) ;
} else {
   wNExchDet = 0.0 ;
}
/* exchange flux of N in Det between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchDet = akExchL *(sSiDetW - sSiDetWM) ;
} else {
   wSiExchDet = 0.0 ;
}
/* exchange flux of Si in Det between marsh and lake water [gS/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchDiat = akExchL *(sDDiatW - sDDiatWM) ;
} else {
   wDExchDiat = 0.0 ;
}
/* exchange flux of DW in Diat between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchDiat = akExchL *(sPDiatW - sPDiatWM) ;
} else {
   wPExchDiat = 0.0 ;
}
/* exchange flux of P in Diat between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchDiat = akExchL *(sNDiatW - sNDiatWM) ;
} else {
   wNExchDiat = 0.0 ;
}
/* exchange flux of N in Diat between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiExchDiat = cSiDDiat * wDExchDiat ;
} else {
   wSiExchDiat = 0.0 ;
}
/* exchange flux of Si in Diat between marsh and lake water [gSi/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchGren = akExchL *(sDGrenW - sDGrenWM) ;
} else {
   wDExchGren = 0.0 ;
}
/* exchange flux of DW in Gren between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchGren = akExchL *(sPGrenW - sPGrenWM) ;
} else {
   wPExchGren = 0.0 ;
}
/* exchange flux of P in Gren between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchGren = akExchL *(sNGrenW - sNGrenWM) ;
} else {
   wNExchGren = 0.0 ;
}
/* exchange flux of N in Gren between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchBlue = akExchL *(sDBlueW - sDBlueWM) ;
} else {
   wDExchBlue = 0.0 ;
}
/* exchange flux of DW in Blue between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchBlue = akExchL *(sPBlueW - sPBlueWM) ;
} else {
   wPExchBlue = 0.0 ;
}
/* exchange flux of P in Blue between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchBlue = akExchL *(sNBlueW - sNBlueWM) ;
} else {
   wNExchBlue = 0.0 ;
}
/* exchange flux of N in Blue between marsh and lake water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wDExchZoo = akExchL *(sDZoo - sDZooM) ;
} else {
   wDExchZoo = 0.0 ;
}
/* exchange flux of DW in Zoo between marsh and lake water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPExchZoo = akExchL *(sPZoo - sPZooM) ;
} else {
   wPExchZoo = 0.0 ;
}
/* exchange flux of P in Zoo between marsh and lake water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNExchZoo = akExchL *(sNZoo - sNZooM) ;
} else {
   wNExchZoo = 0.0 ;
}
/* exchange flux of N in Zoo between marsh and lake water [gN/m3/d] */

/* ---------------------------------- */
/* Infiltration */ 
/* ---------------------------------- */

if (cQInf >= 0.0) {
   tPInfPO4W = cQInf / mmPerm * sPO4W ;
} else {
   tPInfPO4W = cQInf / mmPerm * oPO4S ;
}
/* infiltr of SRP [gP/m2/d] */
if (cQInf >= 0.0) {
   tNInfNH4W = cQInf / mmPerm * sNH4W ;
} else {
   tNInfNH4W = cQInf / mmPerm * oNH4S ;
}
/* infiltr of ammonium [gN/m2/d] */
if (cQInf >= 0.0) {
   tNInfNO3W = cQInf / mmPerm * sNO3W ;
} else {
   tNInfNO3W = cQInf / mmPerm * oNO3S ;
}
/* infiltr of nitrate [gN/m2/d] */
if (cQInf >= 0.0) {
   tPInfPO4S = cQInf / mmPerm * oPO4S ;
} else {
   tPInfPO4S = cQInf / mmPerm * cPO4Ground ;
}
/* infiltration of interst PO4 [gP/m2/d] */
if (cQInf >= 0.0) {
   tNInfNH4S = cQInf / mmPerm * oNH4S ;
} else {
   tNInfNH4S = cQInf / mmPerm * cNH4Ground ;
}
/* infiltration of interst NH4 [gN/m2/d] */
if (cQInf >= 0.0) {
   tNInfNO3S = cQInf / mmPerm * oNO3S ;
} else {
   tNInfNO3S = cQInf / mmPerm * cNO3Ground ;
}
/* infiltration of interst NO3 [gN/m2/d] */

/* ---------------------------------- */
/* Loading */ 
/* ---------------------------------- */

tNH4LoadS = fNH4LoadS * cNLoadS ;
/* NH4 load to sediment from artificial fertilizer [gN/m2/d] */
tNO3LoadS = cNLoadS - tNH4LoadS ;
/* NO3 load to sediment from artificial fertilizer [gN/m2/d] */

/* ---------------------------------- */
/* Erosion */ 
/* ---------------------------------- */

uDErosIM = (1.0 - fDOrgSoil) * cDErosTot ;
/* IM input from banks [gD/m2/d] */
uDErosIMS = fSedErosIM * uDErosIM ;
/* IM input to sediment from banks [gD/m2/d] */
uDErosIMW = uDErosIM - uDErosIMS ;
/* IM input to water column from banks [gD/m2/d] */
uDErosOM = fDOrgSoil * cDErosTot ;
/* organic matter input from banks [gD/m2/d] */
uPErosOM = cPDSoilOM * uDErosOM ;
/* organic P input from banks [gP/m2/d] */
uNErosOM = cNDSoilOM * uDErosOM ;
/* organic N input from banks [gN/m2/d] */

/* ---------------------------------- */
/* reaeration */ 
/* ---------------------------------- */

uO2Sat = 14.652 - 0.41022 * uTm + 0.007991 * uTm*uTm - 0.000077774 * uTm*uTm*uTm ;
/* oxygen saturation concentration [gO2/m3] */
kAer = cAerRoot * ((uVWind )^ (0.5)) + cAerLin * uVWind + cAerSquare * uVWind*uVWind ;
/* reaeration coefficient [m/d] */
uFunTmAer = ((cThetaAer )^ (uTm-cTmRef)) ;
/* temperature function of reaeration [-] */
aFunLemnAer = 1.0 ;
/* duckweed function of reaeration [-] */
tO2Aer = kAer * uFunTmAer * (uO2Sat - sO2W) * aFunLemnAer ;
/* reaeration flux of O2 into the water [gO2/m2/d] */

/* ---------------------------------- */
/* Resuspension */ 
/* ---------------------------------- */

if (InclWeb == 1) {
   uFunTmFish = exp( -0.5/(cSigTmFish*cSigTmFish) * ((uTm - cTmOptFish)*(uTm - cTmOptFish) - (cTmRef - cTmOptFish)*(cTmRef - cTmOptFish))) ;
} else {
   uFunTmFish = 0.0 ;
}
/* temp function of fish [-] */
if (InclWeb == 1) {
   tDTurbFish = (kTurbFish * uFunTmFish * sDFiAd) ;
} else {
   tDTurbFish = 0.0 ;
}
/* bioturbation by fish [g/m2/d] */
tDTurbFishIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDTurbFish ;
/* IM bioturbation by fish [g/m2/d] */
aFunVegResus = max(1.0 - kVegResus * sDVeg, 0.0) ;
/* vegetation dependence of resuspension [-] */
if (uTm >= 0.1) {
   aFunDimSusp = cSuspRef * ((cSuspMin + cSuspMax / (1.0 + exp(cSuspSlope * (sDepthW - hDepthSusp)))) * ((((cFetch +NearZero)/ cFetchRef) )^ (0.5))) ;
} else {
   aFunDimSusp = 0.0 ;
}
/* Empirical suspended matter function (logistic fit to data) [-] */
tDResusTauDead = min(aFunDimSusp, ((aFunDimSusp +NearZero )^ (0.5))) * ((fLutum / fLutumRef )^ (0.5)) * bPorS ;
/* resuspension due to shear stress [gD/m2/d] */
tDResusBareDead = tDResusTauDead + tDTurbFish ;
/* resuspension due to shear stress AND fish [gD/m2/d] */
tDResusDead = tDResusBareDead * aFunVegResus ;
/* resuspension corrected for vegetation effect [gD/m2/d] */
tDResusIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDResusDead ;
/* IM resuspension [gD/m2/d] */
tDResusDet = sDDetS / (fLutum * sDIMS + sDDetS) * tDResusDead ;
/* detrital resuspension [gD/m2/d] */
akResusPhytRef = kResusPhytMax * (1.0 - exp(cResusPhytExp * tDResusDead)) ;
/* phytoplankton resuspension rate constant [d-1] */
tDResusPhytTot = akResusPhytRef * aDPhytS ;
/* phytoplankton resuspension [gD/m2/d] */
tPResusDet = rPDDetS * tDResusDet ;
/* resuspension flux of detrital P [gP/m2/d] */
tPResusPO4 = sPO4S / sDDetS * tDResusDet ;
/* resuspension flux of dissolved P [gP/m2/d] */
tPResusAIM = sPAIMS / sDIMS * tDResusIM ;
/* resuspension flux of P adsorbed onto inert matter [gP/m2/d] */
tNResusNO3 = sNO3S / sDDetS * tDResusDet ;
/* resuspension flux of nitrate [gN/m2/d] */
tNResusNH4 = sNH4S / sDDetS * tDResusDet ;
/* resuspension flux of ammonium [gN/m2/d] */
tNResusDet = rNDDetS * tDResusDet ;
/* resuspension flux of detrital N [gN/m2/d] */
tSiResusDet = rSiDDetS * tDResusDet ;
/* resuspension flux of detrial SI [gSi/m2/d] */

/* ---------------------------------- */
/* Sedimentation */ 
/* ---------------------------------- */

aFunTauSetOM = min(1.0 / ((aFunDimSusp +NearZero )^ (0.5)), 1.0) ;
/* correction factor for IM settling rate (<= 1) [-] */
aFunTauSetIM = aFunTauSetOM ;
/* correction factor for OM settling rate (<= 1) [-] */
uFunTmSet = ((cThetaSet )^ (uTm-cTmRef)) ;
/* temperature correction of sedimentation [-] */
uCorVSetIM = aFunTauSetIM * (((fLutumRef/fLutum) )^ (0.5)) * uFunTmSet * cVSetIM ;
/* corrected sedimentation velocity of IM [m/d] */
tDSetIM = uCorVSetIM * sDIMW ;
/* sedimentation flux of inert matter [gDW/m2/d] */
tPSetAIM = sPAIMW / sDIMW * tDSetIM ;
/* sedimentation flux of P adsorbed onto inert org matter [gP/m2/d] */
uCorVSetDet = cVSetDet * aFunTauSetOM * uFunTmSet ;
/* corrected sedimentation velocity of detritus [m/d] */
tDSetDet = uCorVSetDet * sDDetW ;
/* sedimentation flux of detritus [gDW/m2/d] */
tPSetDet = uCorVSetDet * sPDetW ;
/* sedimentation flux of detrital P [gP/m2/d] */
tNSetDet = uCorVSetDet * sNDetW ;
/* sedimentation flux of detrital N [gN/m2/d] */
tSiSetDet = uCorVSetDet * sSiDetW ;
/* sedimentation flux of detrital Si [gSi/m2/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

kPMinDetW = kDMinDetW ;
/* P mineralisation constant in water [d-1] */
kNMinDetW = kDMinDetW ;
/* N mineralisation constant in water [d-1] */
kSiMinDetW = kDMinDetW ;
/* Si mineralisation constant in water [d-1] */
uFunTmMinW = ((cThetaMinW )^ (uTm-cTmRef)) ;
/* temp function of mineralization in water [-] */
wDMinDetW = kDMinDetW * uFunTmMinW * sDDetW ;
/* decomposition [g/m3/d] */
wPMinDetW = kPMinDetW * uFunTmMinW * sPDetW ;
/* mineralization [gP/m3/d] */
wNMinDetW = kNMinDetW * uFunTmMinW * sNDetW ;
/* mineralization [gN/m3/d] */
wSiMinDetW = kSiMinDetW * uFunTmMinW * sSiDetW ;
/* mineralization [gSi/m3/d] */
aCorO2BOD = sO2W / (hO2BOD + sO2W) ;
/* correction of O2 demand in water at low oxygen conc [-] */
wO2MinDetW = molO2molC * cCPerDW * aCorO2BOD * wDMinDetW ;
/* O2 flux due to mineralization of detritus [gO2/m3/d] */

/* ---------------------------------- */
/* Denitirification */ 
/* ---------------------------------- */

wDDenitW = sNO3W*sNO3W / (hNO3Denit*hNO3Denit + sNO3W*sNO3W) * (1.0 - aCorO2BOD) * wDMinDetW ;
/* mineralisation flux by denitrification [gDW/m2/d] */
wNDenitW = NO3PerC * molNmolC * cCPerDW * wDDenitW ;
/* Denitrification flux [gN/m2/d] */

/* ---------------------------------- */
/* Nitrification */ 
/* ---------------------------------- */

uFunTmNitr = ((cThetaNitr )^ (uTm-cTmRef)) ;
/* Temperature dependence for nitrification [-] */
aCorO2NitrW = sO2W*sO2W / (hO2Nitr*hO2Nitr + sO2W*sO2W) ;
/* oxygen consumption during nitrification [-] */
wNNitrW = kNitrW * uFunTmNitr * aCorO2NitrW * sNH4W ;
/* nitrification flux [mgN/l/d] */
wO2NitrW = O2PerNH4 * molO2molN * wNNitrW ;
/* O2 flux due to nitrification [gO2/m3/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

kPMinDetS = kDMinDetS ;
/* P mineralisation constant in sed [d-1] */
kNMinDetS = kDMinDetS ;
/* N mineralisation constant in sed [d-1] */
kSiMinDetS = kDMinDetS ;
/* Si mineralisation constant in sed [d-1] */
uFunTmMinS = ((cThetaMinS )^ (uTm-cTmRef)) ;
/* temp function [-] */
tDMinDetS = kDMinDetS * uFunTmMinS * sDDetS ;
/* decomposition of upper sediment [gDW/m2/d] */
tPMinDetS = kPMinDetS * uFunTmMinS * sPDetS ;
/* mineralization of P in upper sediment [gP/m2/d] */
tNMinDetS = kNMinDetS * uFunTmMinS * sNDetS ;
/* mineralization of N in upper sediment [gN/m2/d] */
tSiMinDetS = kSiMinDetS * uFunTmMinS * sSiDetS ;
/* mineralization of Si in upper sediment [gN/m2/d] */

/* ---------------------------------- */
/* Oxygen_depth */ 
/* ---------------------------------- */

uFunTmDif = ((cThetaDif )^ (uTm-cTmRef)) ;
/* temperature function of diffusion [-] */
akO2DifCor = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorS ;
/* corrected O2 diffusion coefficient [m2/d] */
tSOD = (molO2molC * cCPerDW * (1.0 - fRefrDetS) * tDMinDetS + O2PerNH4 * molO2molN * kNitrS * uFunTmNitr * sNH4S) / cDepthS ;
/* sediment oxygen demand [gO2/m3/d] */
aDepthOxySed = (((2.0 * sO2W * akO2DifCor / tSOD) )^ (0.5)) ;
/* oxygen penetration depth [m] */
afOxySed = aDepthOxySed / cDepthS ;
/* fraction aerobic sediment [-] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

tDMinOxyDetS = afOxySed * (1.0 - fRefrDetS) * tDMinDetS ;
/* aerobic mineralisation [gDW/m2/d] */
tO2MinDetS = molO2molC * cCPerDW * tDMinOxyDetS ;
/* sediment oxygen demand [gO2/m2/d] */

/* ---------------------------------- */
/* Denitirification */ 
/* ---------------------------------- */

tDDenitS = oNO3S*oNO3S / (hNO3Denit*hNO3Denit + oNO3S*oNO3S) * (1.0 - afOxySed) * (1.0 - fRefrDetS) * tDMinDetS ;
/* mineralisation flux by denitrification [gD/m/d] */
tNDenitS = NO3PerC * molNmolC * cCPerDW * tDDenitS ;
/* Denitrification flux [gN/m2/d] */

/* ---------------------------------- */
/* Nitrification */ 
/* ---------------------------------- */

tNNitrS = afOxySed * kNitrS * uFunTmNitr * sNH4S ;
/* nitrification flux [gN/m2/d] */
tO2NitrS = O2PerNH4 * molO2molN * tNNitrS ;
/* O2 flux due to nitrification [gO2/m2/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

tDMinHumS = kDMinHum * uFunTmMinS * afOxySed * sDHumS ;
/* decomposition of upper sediment humus [gDW/m2/d] */
tPMinHumS = kDMinHum * uFunTmMinS * afOxySed * sPHumS ;
/* mineralization of P in upper sediment humus [gP/m2/d] */
tNMinHumS = kDMinHum * uFunTmMinS * afOxySed * sNHumS ;
/* mineralization of N in upper sediment humus [gN/m2/d] */

/* ---------------------------------- */
/* Diffusion */ 
/* ---------------------------------- */

aDepthDif = fDepthDifS * cDepthS ;
/* average diffusion distance [m] */
tPDifPO4 = kPDifPO4 * uFunTmDif * cTurbDifNut * bPorCorS * (oPO4S - sPO4W ) / aDepthDif ;
/* diffusion flux of dissolved P from sediment to water [gP/m2/d] */
tNDifNO3 = kNDifNO3 * uFunTmDif * cTurbDifNut * bPorCorS * (oNO3S - sNO3W ) / aDepthDif ;
/* diffusion flux of NO3 from sediment to water [gP/m2/d] */
tNDifNH4 = kNDifNH4 * uFunTmDif * cTurbDifNut * bPorCorS * (oNH4S - sNH4W ) / aDepthDif ;
/* diffusion flux of NH4 from sediment to water [gP/m2/d] */
tO2Dif = kO2Dif / aDepthDif * uFunTmDif * cTurbDifO2 * bPorCorS * sO2W ;
/* O2 diffusion (water -> sediment) [gO2/m2/d] */
tPDifGroundPO4 = 0.0 ;
/* diffusion flux of dissolved P from pore water to ground water [gP/m2/d] */
tNDifGroundNO3 = 0.0 ;
/* diffusion flux of dissolved NO3 from pore water to ground water [gN/m2/d] */
tNDifGroundNH4 = 0.0 ;
/* diffusion flux of dissolved NH4 from pore water to ground water [gN/m2/d] */

/* ---------------------------------- */
/* P_sorption */ 
/* ---------------------------------- */

aPAdsMaxW = cRelPAdsD + aCorO2BOD * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
/* max P adsorption per g inorg matter in water [gP/gD] */
aKPAdsW = (1.0 - fRedMax * (1.0-aCorO2BOD)) * cKPAdsOx ;
/* P adsorption affinity corrected for redox conditions [m3/gP] */
aPIsoAdsW = aPAdsMaxW * aKPAdsW * sPO4W / (1.0 + aKPAdsW * sPO4W) ;
/* P adsorption isotherm onto inorg matter in sediment [gP/gD] */
aPEqIMW = aPIsoAdsW * sDIMW ;
/* equilibrium conc [gP/m3] */
wPSorpIMW = kPSorp * (aPEqIMW - sPAIMW) ;
/* sorption flux in water [gP/m3/d] */
aPAdsMaxS = cRelPAdsD + afOxySed * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
/* max P adsorption per g inorg matter in sediment [gP/gD] */
aKPAdsS = (1.0 - fRedMax * (1.0-afOxySed)) * cKPAdsOx ;
/* P adsorption affinity corrected for redox conditions [m3/gP] */
aPIsoAdsS = aPAdsMaxS * aKPAdsS * oPO4S / (1.0 + aKPAdsS * oPO4S) ;
/* P adsorption isotherm onto inorg matter in sediment [gP/gD] */
aPEqIMS = aPIsoAdsS * sDIMS ;
/* equilibrium amount [gP/m2] */
tPSorpIMS = kPSorp * (aPEqIMS - sPAIMS) ;
/* sorption [gP/m2/d] */

/* ---------------------------------- */
/* Total_abiotic/microbial_fluxes */ 
/* ---------------------------------- */

tPChemPO4 = max( 0.0, kPChemPO4 * (oPO4S - coPO4Max) ) ;
/* chem loss of dissolved P from pore water [gP/m2/d] */
wDAbioIMW = (uDErosIMW - tDSetIM + tDResusIM) / sDepthW ;
/* total abiotic/microbial DW inorganic matter flux in water [gDW/m3/d] */
wDAbioDetW = (- tDSetDet + tDResusDet ) / sDepthW - wDMinDetW ;
/* total abiotic/microbial DW detritus flux in water [gDW/m3/d] */
tDAbioIMS = uDErosIMS + tDSetIM - tDResusIM ;
/* total abiotic/microbial DW inorganic matter flux in sediment [gDW/m2/d] */
tDAbioDetS = tDSetDet - tDResusDet - tDMinDetS ;
/* total abiotic/microbial DW detritus flux in sediment [gDW/m2/d] */
tDAbioHumS = uDErosOM + fRefrDetS * tDMinDetS - tDMinHumS ;
/* total abiotic/microbial DW humus flux in sediment [gDW/m2/d] */
tDAbioTotT = cDErosTot - wDMinDetW * sDepthW - (1.0 - fRefrDetS) * tDMinDetS - tDMinHumS ;
/* total abiotic/microbial DW flux for mass balance check [gDW/m2/d] */
wO2AbioW = tO2Aer / sDepthW - wO2MinDetW - wO2NitrW - (tO2MinDetS + tO2NitrS) / sDepthW ;
/* total abiotic/microbial O2 flux in water [gO2/m3/d] */
wPAbioDetW = - wPMinDetW - (tPSetDet - tPResusDet) / sDepthW ;
/* total abiotic/microbial P detritus flux in water [gP/m3/d] */
wPAbioPO4W = wPMinDetW - wPSorpIMW - tPInfPO4W / sDepthW + tPDifPO4 / sDepthW + tPResusPO4 / sDepthW ;
/* total abiotic/microbial dissolved P flux in water [gP/m3/d] */
wPAbioAIMW = (- tPSetAIM + tPResusAIM) / sDepthW + wPSorpIMW ;
/* total abiotic/microbial P absorbed onto inorganic matter flux in water [gP/m3/d] */
tPAbioDetS = - tPMinDetS + tPSetDet - tPResusDet ;
/* total abiotic/microbial P detritus flux in sediment [gP/m2/d] */
tPAbioHumS = uPErosOM + fRefrDetS * tPMinDetS - tPMinHumS ;
/* total abiotic/microbial P humus flux in sediment [gP/m2/d] */
tPAbioPO4S = tPInfPO4W - tPInfPO4S + (1.0-fRefrDetS) * tPMinDetS + tPMinHumS - tPSorpIMS - tPResusPO4 - tPDifPO4 - tPDifGroundPO4 - tPChemPO4 ;
/* total abiotic/microbial dissolved P flux in sediment [gP/m2/d] */
tPAbioAIMS = tPSetAIM - tPResusAIM + tPSorpIMS ;
/* total abiotic/microbial P absorbed onto inorganic matter flux in sediment [gP/m2/d] */
tPAbioTotT = uPErosOM - tPChemPO4 - tPInfPO4S - tPDifGroundPO4 ;
/* total abiotic/microbial P flux for mass balance check [gP/m2/d] */
wNAbioNH4W = wNMinDetW - wNNitrW - tNInfNH4W / sDepthW + (tNDifNH4 + tNResusNH4) / sDepthW ;
/* total abiotic/microbial N NH4 flux in water [gN/m3/d] */
wNAbioNO3W = wNNitrW - wNDenitW + (tNDifNO3 + tNResusNO3 - tNInfNO3W) / sDepthW ;
/* total abiotic/microbial N NO3 flux in water [gN/m3/d] */
wNAbioDetW = - wNMinDetW - (tNSetDet - tNResusDet ) / sDepthW ;
/* total abiotic/microbial N detritus flux in water [gN/m3/d] */
tNAbioNH4S = tNInfNH4W - tNInfNH4S + (1.0-fRefrDetS) * tNMinDetS + tNMinHumS - tNResusNH4 - tNDifNH4 - tNDifGroundNH4 - tNNitrS ;
/* total abiotic/microbial N NH4 flux in sediment [gN/m2/d] */
tNAbioNO3S = tNInfNO3W - tNInfNO3S + tNNitrS - tNDenitS - tNResusNO3 - tNDifNO3 - tNDifGroundNO3 ;
/* total abiotic/microbial N NO3 flux in sediment [gN/m2/d] */
tNAbioDetS = - tNMinDetS + tNSetDet - tNResusDet ;
/* total abiotic/microbial N detritus flux in sediment [gN/m2/d] */
tNAbioHumS = uNErosOM + fRefrDetS * tNMinDetS - tNMinHumS ;
/* total abiotic/microbial N humus flux in sediment [gN/m2/d] */
tNAbioTotT = uNErosOM - tNDenitS - wNDenitW * sDepthW - tNInfNH4S - tNInfNO3S - tNDifGroundNO3 - tNDifGroundNH4 ;
/* total abiotic/microbial N flux for mass balance check [gN/m2/d] */
wSiAbioSiO2W = wSiMinDetW + (1.0 - fRefrDetS) * tSiMinDetS / sDepthW ;
/* total abiotic/microbial Si SiO2 flux in water [gSi/m3/d] */
wSiAbioDetW = - wSiMinDetW - (tSiSetDet - tSiResusDet) / sDepthW ;
/* total abiotic/microbial Si detritus flux in water [gSi/m3/d] */
tSiAbioDetS = - tSiMinDetS + tSiSetDet - tSiResusDet ;
/* total abiotic/microbial Si detritus flux in sediment [gSi/m2/d] */
tSiAbioTotT = - fRefrDetS * tSiMinDetS ;
/* total abiotic/microbial Si flux for mass balance check [gSi/m2/d] */

/* ---------------------------------- */
/* Process_equations */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   uQEvPhra = uQEv ;
} else {
   uQEvPhra = 0.0 ;
}
/* reed evaporation(set EQUAL to lake evaporation) [mm/d] */

/* ---------------------------------- */
/* Nutrient_flux_to_pore_water */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tPEvPO4WM = uQEvPhra / mmPerm * sPO4WM ;
} else {
   tPEvPO4WM = 0.0 ;
}
/* SRP flux [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNEvNH4WM = uQEvPhra / mmPerm * sNH4WM ;
} else {
   tNEvNH4WM = 0.0 ;
}
/* ammonium flux [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNEvNO3WM = uQEvPhra / mmPerm * sNO3WM ;
} else {
   tNEvNO3WM = 0.0 ;
}
/* nitrate flux [gN/m2/d] */

/* ---------------------------------- */
/* Infiltration */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tPInfPO4WM = 0.0 ;
} else if (cQInf >= 0.0) {
   tPInfPO4WM = cQInf / mmPerm * sPO4WM ;
} else {
   tPInfPO4WM = cQInf / mmPerm * oPO4SM ;
}
/* infiltr of SRP [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNInfNH4WM = 0.0 ;
} else if (cQInf >= 0.0) {
   tNInfNH4WM = cQInf / mmPerm * sNH4WM ;
} else {
   tNInfNH4WM = cQInf / mmPerm * oNH4SM ;
}
/* infiltr of ammonium [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNInfNO3WM = 0.0 ;
} else if (cQInf >= 0.0) {
   tNInfNO3WM = cQInf / mmPerm * sNO3WM ;
} else {
   tNInfNO3WM = cQInf / mmPerm * oNO3SM ;
}
/* infiltr of nitrate [gN/m2/d] */

/* ---------------------------------- */
/* Seepage */ 
/* ---------------------------------- */

if (0 == InclMarsh || fMarsh <= NearZero) {
   tPInfPO4SM = 0.0 ;
} else if (cQInf >= 0.0) {
   tPInfPO4SM = cQInf / mmPerm * oPO4SM ;
} else {
   tPInfPO4SM = cQInf / mmPerm * cPO4Ground ;
}
/* infiltration of interst PO4 [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNInfNH4SM = 0.0 ;
} else if (cQInf >= 0.0) {
   tNInfNH4SM = cQInf / mmPerm * oNH4SM ;
} else {
   tNInfNH4SM = cQInf / mmPerm * oNH4SM ;
}
/* infiltration of interst NH4 [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNInfNO3SM = 0.0 ;
} else if (cQInf >= 0.0) {
   tNInfNO3SM = cQInf / mmPerm * oNO3SM ;
} else {
   tNInfNO3SM = cQInf / mmPerm * oNO3SM ;
}
/* infiltration of interst NO3 [gN/m2/d] */

/* ---------------------------------- */
/* reaeration */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2AerM = kAer * uFunTmAer *(uO2Sat - sO2WM) ;
} else {
   tO2AerM = 0.0 ;
}
/* reaeration flux of O2 into the water [gO2/m2/d] */

/* ---------------------------------- */
/* Sedimentation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetIMM = cVSetIM * uFunTmSet * sDIMWM ;
} else {
   tDSetIMM = 0.0 ;
}
/* sedimentation flux of inert matter [g/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetAIMM = cVSetIM * uFunTmSet * sPAIMWM ;
} else {
   tPSetAIMM = 0.0 ;
}
/* sedimentation flux of P adsorbed onto inert org matter [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetDetM = cVSetDet * uFunTmSet * sDDetWM ;
} else {
   tDSetDetM = 0.0 ;
}
/* sedimentation flux of detritus [g/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetDetM = cVSetDet * uFunTmSet* sPDetWM ;
} else {
   tPSetDetM = 0.0 ;
}
/* sedimentation flux of detrital P [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNSetDetM = cVSetDet * uFunTmSet* sNDetWM ;
} else {
   tNSetDetM = 0.0 ;
}
/* sedimentation flux of detrital N [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tSiSetDetM = cVSetDet * uFunTmSet * sSiDetWM ;
} else {
   tSiSetDetM = 0.0 ;
}
/* sedimentation flux of detrital Si [gSi/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetDiatM = cVSetDiat * uFunTmSet * sDDiatWM ;
} else {
   tDSetDiatM = 0.0 ;
}
/* sedimentation flux of detritus [g/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetDiatM = cVSetDiat * uFunTmSet* sPDiatWM ;
} else {
   tPSetDiatM = 0.0 ;
}
/* sedimentation flux of detrital P [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNSetDiatM = cVSetDiat * uFunTmSet* sNDiatWM ;
} else {
   tNSetDiatM = 0.0 ;
}
/* sedimentation flux of detrital N [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tSiSetDiatM = cVSetDiat * uFunTmSet * oSiDiatWM ;
} else {
   tSiSetDiatM = 0.0 ;
}
/* sedimentation flux of detrital Si [gSi/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetGrenM = cVSetGren * uFunTmSet * sDGrenWM ;
} else {
   tDSetGrenM = 0.0 ;
}
/* sedimentation flux of detritus [g/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetGrenM = cVSetGren * uFunTmSet* sPGrenWM ;
} else {
   tPSetGrenM = 0.0 ;
}
/* sedimentation flux of detrital P [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNSetGrenM = cVSetGren * uFunTmSet* sNGrenWM ;
} else {
   tNSetGrenM = 0.0 ;
}
/* sedimentation flux of detrital N [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetBlueM = cVSetBlue * uFunTmSet * sDBlueWM ;
} else {
   tDSetBlueM = 0.0 ;
}
/* sedimentation flux of detritus [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetBlueM = cVSetBlue * uFunTmSet* sPBlueWM ;
} else {
   tPSetBlueM = 0.0 ;
}
/* sedimentation flux of detrital P [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNSetBlueM = cVSetBlue * uFunTmSet* sNBlueWM ;
} else {
   tNSetBlueM = 0.0 ;
}
/* sedimentation flux of detrital N [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetPhytM = tDSetDiatM + tDSetGrenM + tDSetBlueM ;
} else {
   tDSetPhytM = 0.0 ;
}
/* sedimentation flux of detritus [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSetPhytM = tPSetDiatM + tPSetGrenM + tPSetBlueM ;
} else {
   tPSetPhytM = 0.0 ;
}
/* sedimentation flux of detrital P [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNSetPhytM = tNSetDiatM + tNSetGrenM + tNSetBlueM ;
} else {
   tNSetPhytM = 0.0 ;
}
/* sedimentation flux of detrital N [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDSetTotM = tDSetIMM + tDSetDetM + tDSetPhytM ;
} else {
   tDSetTotM = 0.0 ;
}
/* total sedimentation in marsh [gDW/m2/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   wDMinDetWM = kDMinDetW * uFunTmMinW * sDDetWM ;
} else {
   wDMinDetWM = 0.0 ;
}
/* decomposition [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPMinDetWM = kPMinDetW * uFunTmMinW * sPDetWM ;
} else {
   wPMinDetWM = 0.0 ;
}
/* mineralization [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNMinDetWM = kNMinDetW * uFunTmMinW * sNDetWM ;
} else {
   wNMinDetWM = 0.0 ;
}
/* mineralization [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wSiMinDetWM = kSiMinDetW * uFunTmMinW * sSiDetWM ;
} else {
   wSiMinDetWM = 0.0 ;
}
/* mineralization [gSi/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aCorO2BODM = sO2WM /(hO2BOD + sO2WM) ;
} else {
   aCorO2BODM = 0.0 ;
}
/* correction of O2 demand in water at low oxygen conc [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wO2MinDetWM = molO2molC * cCPerDW * aCorO2BODM * wDMinDetWM ;
} else {
   wO2MinDetWM = 0.0 ;
}
/* O2 flux due to mineralization of detritus [gO2/m3/d] */

/* ---------------------------------- */
/* Denitirification */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   wDDenitWM = sNO3WM*sNO3WM /(hNO3Denit*hNO3Denit + sNO3WM*sNO3WM) *(1.0 - aCorO2BODM) * wDMinDetWM ;
} else {
   wDDenitWM = 0.0 ;
}
/* mineralisation flux by denitrification [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNDenitWM = NO3PerC * molNmolC * cCPerDW * wDDenitWM ;
} else {
   wNDenitWM = 0.0 ;
}
/* Denitrification flux [gN/m2/d] */

/* ---------------------------------- */
/* Nitrification */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   aCorO2NitrWM = sO2WM * sO2WM /(hO2Nitr * hO2Nitr + sO2WM * sO2WM) ;
} else {
   aCorO2NitrWM = 0.0 ;
}
/* oxygen use for nitrification in marsh water [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wNNitrWM = kNitrW * uFunTmNitr * aCorO2NitrWM * sNH4WM ;
} else {
   wNNitrWM = 0.0 ;
}
/* nitrification flux [mgN/l/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wO2NitrWM = O2PerNH4 * molO2molN * wNNitrWM ;
} else {
   wO2NitrWM = 0.0 ;
}
/* O2 flux due to nitrification [gO2/m3/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDMinDetSM = kDMinDetS * uFunTmMinS * sDDetSM ;
} else {
   tDMinDetSM = 0.0 ;
}
/* decomposition of upper sediment [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPMinDetSM = kPMinDetS * uFunTmMinS * sPDetSM ;
} else {
   tPMinDetSM = 0.0 ;
}
/* mineralization of P in upper sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNMinDetSM = kNMinDetS * uFunTmMinS * sNDetSM ;
} else {
   tNMinDetSM = 0.0 ;
}
/* mineralization of N in upper sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tSiMinDetSM = kSiMinDetS * uFunTmMinS * sSiDetSM ;
} else {
   tSiMinDetSM = 0.0 ;
}
/* mineralization of Si in upper sediment [gN/m2/d] */

/* ---------------------------------- */
/* Oxygen_depth */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   akO2DifCorM = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorSM ;
} else {
   akO2DifCorM = 0.0 ;
}
/* corrected O2 diffusion coefficient [m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tSODM = (molO2molC * cCPerDW *(1.0 - fRefrDetS) * tDMinDetSM + O2PerNH4 * molO2molN * kNitrS * uFunTmNitr * sNH4SM) / cDepthSM ;
} else {
   tSODM = 0.0 ;
}
/* sediment oxygen demand [gO2/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aDepthOxySedM = (((2.0 * sO2WM * akO2DifCorM / tSODM) )^ (0.5)) ;
} else {
   aDepthOxySedM = 0.0 ;
}
/* oxygen penetration depth [m] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   afOxySedM = aDepthOxySedM / cDepthSM ;
} else {
   afOxySedM = 0.0 ;
}
/* fraction aerobic sediment [-] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDMinOxyDetSM = afOxySedM *(1.0 - fRefrDetS) * tDMinDetSM ;
} else {
   tDMinOxyDetSM = 0.0 ;
}
/* aerobic mineralisation [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2MinDetSM = molO2molC * cCPerDW * tDMinOxyDetSM ;
} else {
   tO2MinDetSM = 0.0 ;
}
/* sediment oxygen demand [gO2/m2/d] */

/* ---------------------------------- */
/* Denitirification */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDDenitSM = oNO3SM*oNO3SM /(hNO3Denit*hNO3Denit + oNO3SM*oNO3SM) *(1.0 - afOxySedM) *(1.0 - fRefrDetS) * tDMinDetSM ;
} else {
   tDDenitSM = 0.0 ;
}
/* mineralisation flux by denitrification [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNDenitSM = NO3PerC * molNmolC * cCPerDW * tDDenitSM ;
} else {
   tNDenitSM = 0.0 ;
}
/* Denitrification flux [gN/m2/d] */

/* ---------------------------------- */
/* Nitrification */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tNNitrSM = afOxySedM * kNitrS * uFunTmNitr * sNH4SM ;
} else {
   tNNitrSM = 0.0 ;
}
/* nitrification flux [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2NitrSM = O2PerNH4 * molO2molN * tNNitrSM ;
} else {
   tO2NitrSM = 0.0 ;
}
/* O2 flux due to nitrification [gO2/m2/d] */

/* ---------------------------------- */
/* Mineralisation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sDHumSM ;
} else {
   tDMinHumSM = 0.0 ;
}
/* decomposition of upper sediment humus [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sPHumSM ;
} else {
   tPMinHumSM = 0.0 ;
}
/* mineralization of P in upper sediment humus [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sNHumSM ;
} else {
   tNMinHumSM = 0.0 ;
}
/* mineralization of N in upper sediment humus [gN/m2/d] */

/* ---------------------------------- */
/* Diffusion */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   aDepthDifM = fDepthDifS * cDepthSM ;
} else {
   aDepthDifM = 0.0 ;
}
/* average diffusion distance [m] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPDifPO4M = kPDifPO4 * uFunTmDif * cTurbDifNut * bPorCorSM * (oPO4SM - sPO4WM) / aDepthDifM ;
} else {
   tPDifPO4M = 0.0 ;
}
/* diffusion flux of dissolved P from sediment to water [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNDifNO3M = kNDifNO3 * uFunTmDif * cTurbDifNut * bPorCorSM * (oNO3SM - sNO3WM) / aDepthDifM ;
} else {
   tNDifNO3M = 0.0 ;
}
/* diffusion flux of NO3 from sediment to water [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNDifNH4M = kNDifNH4 * uFunTmDif * cTurbDifNut * bPorCorSM * (oNH4SM - sNH4WM) / aDepthDifM ;
} else {
   tNDifNH4M = 0.0 ;
}
/* diffusion flux of NH4 from sediment to water [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2DifM = kO2Dif / aDepthDifM * uFunTmDif * cTurbDifO2 * bPorCorSM * sO2WM ;
} else {
   tO2DifM = 0.0 ;
}
/* O2 diffusion(water -> sediment) [gO2/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPDifGroundPO4M = 0.0 ;
} else {
   tPDifGroundPO4M = 0.0 ;
}
/* diffusion flux of dissolved P from pore water to ground water [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNDifGroundNO3M = 0.0 ;
} else {
   tNDifGroundNO3M = 0.0 ;
}
/* diffusion flux of NO3 from pore water to ground water [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNDifGroundNH4M = 0.0 ;
} else {
   tNDifGroundNH4M = 0.0 ;
}
/* diffusion flux of NH4 from pore water to ground water [gN/m2/d] */

/* ---------------------------------- */
/* P_sorption */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   aPAdsMaxWM = cRelPAdsD + aCorO2BODM * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
} else {
   aPAdsMaxWM = 0.0 ;
}
/* max P adsorption per g inorg matter in water marsh [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aKPAdsWM = (1.0 - fRedMax *(1.0-aCorO2BODM)) * cKPAdsOx ;
} else {
   aKPAdsWM = 0.0 ;
}
/* P adsorption affinitycorrected for redox conditions [m3/gP] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPIsoAdsWM = aPAdsMaxWM * aKPAdsWM * sPO4WM /(1.0 + aKPAdsWM * sPO4WM) ;
} else {
   aPIsoAdsWM = 0.0 ;
}
/* P adsorption isotherm onto inorg matter in sediment [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPEqIMWM = aPIsoAdsWM * sDIMWM ;
} else {
   aPEqIMWM = 0.0 ;
}
/* equilibrium conc [gP/m3] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   wPSorpIMWM = kPSorp *(aPEqIMWM - sPAIMWM) ;
} else {
   wPSorpIMWM = 0.0 ;
}
/* sorption flux in water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPAdsMaxSM = cRelPAdsD + afOxySedM * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM ;
} else {
   aPAdsMaxSM = 0.0 ;
}
/* max P adsorption per g inorg matter in sediment marsh [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aKPAdsSM = (1.0 - fRedMax *(1.0-afOxySedM)) * cKPAdsOx ;
} else {
   aKPAdsSM = 0.0 ;
}
/* P adsorption affinitycorrected for redox conditions [m3/gP] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPIsoAdsSM = aPAdsMaxSM * aKPAdsSM * oPO4SM /(1.0 + aKPAdsSM * oPO4SM) ;
} else {
   aPIsoAdsSM = 0.0 ;
}
/* P adsorption isotherm onto inorg matter in sediment [gP/gD] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPEqIMSM = aPIsoAdsSM * sDIMSM ;
} else {
   aPEqIMSM = 0.0 ;
}
/* equilibrium amount [gP/m2] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPSorpIMSM = kPSorp *(aPEqIMSM - sPAIMSM) ;
} else {
   tPSorpIMSM = 0.0 ;
}
/* sorption [gP/m2/d] */

/* ---------------------------------- */
/* Loss_of_SRP_from_pore_water */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tPChemPO4M = max(0.0,kPChemPO4 *(oPO4SM - coPO4Max)) ;
} else {
   tPChemPO4M = 0.0 ;
}
/* chem loss of dissolved P from pore water [gP/m2/d] */

/* ---------------------------------- */
/* Vegetation_processes */ 
/* ---------------------------------- */

if (Day < 1.0 || sTime < BeginTime + 1.0) {
   aDayInitVeg = 367 ;
} else if (uTm >= cTmInitVeg && aDayInitVeg > 366) {
   aDayInitVeg = Day ;
} else {
   aDayInitVeg = aDayInitVeg ;
}
/* Initial growth only once a year [day] */
if (0 == InclSeason) {
   bfRootVeg = fRootVegSum ;
} else if (Day < aDayInitVeg) {
   bfRootVeg = fRootVegWin ;
} else if (Day < aDayInitVeg + cLengAllo) {
   bfRootVeg = 0.5*(fRootVegWin + fRootVegSum) + 0.5*(fRootVegWin - fRootVegSum) * cos(Pi/cLengAllo * (Day - aDayInitVeg)) ;
} else if (Day < cDayWinVeg) {
   bfRootVeg = fRootVegSum ;
} else if (Day < cDayWinVeg + cLengAllo) {
   bfRootVeg = 0.5*(fRootVegWin + fRootVegSum) - 0.5*(fRootVegWin - fRootVegSum) * cos(Pi/cLengAllo * (Day - cDayWinVeg)) ;
} else {
   bfRootVeg = fRootVegWin ;
}
/* setting root fration [-] */
bfShootVeg = 1.0 - bfRootVeg ;
/* shoot fraction [-] */
aDRootVeg = bfRootVeg * sDVeg ;
/* root biomass [g/m2] */
aDShootVeg = bfShootVeg * sDVeg ;
/* shoot biomass [g/m2] */
aDEmergVeg = fEmergVeg * aDShootVeg ;
/* emergent biomass [g/m2] */
aDFloatVeg = fFloatVeg * aDShootVeg ;
/* floating biomass [g/m2] */
bfSubVeg = 1.0 - fFloatVeg - fEmergVeg ;
/* submerged fraction of shoot [-] */
aDSubVeg = bfSubVeg * aDShootVeg ;
/* submerged biomass [g/m2] */
if (0 == InclBed) {
   aExtVeg = 0.0 ;
} else {
   aExtVeg = cExtSpVeg * aDSubVeg / sDepthW ;
}
/* contribution of plant species to extinction (submerged) [m-1] */
aDepth1Veg = fDepth1Veg * sDepthW ;
/* upper depth of vegetation layer (minimum = 0 m = surface) [m] */
aDepth2Veg = fDepth2Veg * sDepthW ;
/* lower depth of vegetation layer (maximum = water depth) [m] */
if (0 == InclBed) {
   afCovSurfVeg = 0.0 ;
} else {
   afCovSurfVeg = min(1.0, max(aDFloatVeg / (cDLayerVeg + NearZero), aDEmergVeg / (fEmergVeg * cDCarrVeg + NearZero) ) ) ;
}
/* fraction of water SURFACE covered by plant species [-] */
afCovEmergVeg = min(1.0, PerCent * cCovSpVeg * aDEmergVeg) ;
/* fraction emergent coverage [-] */
aCovVeg = min(100.0, cCovSpVeg * aDShootVeg) ;
/* percent cover [-] */
aDVeg = sDVeg ;
/* total plant biomass [gDW/m2/d] */
aPVeg = sPVeg ;
/* total P in vegetation [gP/m2/d] */
aNVeg = sNVeg ;
/* total N in vegetation [gN/m2/d] */
aExtCoef = aExtCoefOpen + aExtVeg ;
/* extinction coefficient incl vegetation [m-1] */
aLPARBot = uLPARSurf * exp(- aExtCoef * sDepthW) ;
/* light at the bottom [W/m2_PAR] */
rPDVeg = sPVeg / (sDVeg+NearZero) ;
/* P/DW ratio of vegetation [mgP/mgDW] */
rNDVeg = sNVeg / (sDVeg+NearZero) ;
/* N/DW ratio of vegetation [mgN/mgDW] */
tDMigrVeg = kMigrVeg * (cDVegIn - sDVeg) ;
/* migration flux [gDW/m2/d] */
tPMigrVeg = kMigrVeg * (cPDVeg0* cDVegIn - sPVeg) ;
/* net migration flux [gP/m2/d] */
tNMigrVeg = kMigrVeg * (cNDVeg0* cDVegIn - sNVeg) ;
/* net migration flux [gN/m2/d] */
uFunTmProdVeg = ((cQ10ProdVeg )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation production [-] */
uFunTmRespVeg = ((cQ10RespVeg )^ (0.1 * (uTm - cTmRef))) ;
/* temperature function of vegetation respiration [-] */
if (0 == UseEmpUpt) {
   afPUptVegS = 0.0 ;
} else if (bfRootVeg <= NearZero) {
   afPUptVegS = 0.0 ;
} else if (fFloatVeg + bfSubVeg <= NearZero) {
   afPUptVegS = 1.0 ;
} else {
   afPUptVegS = fSedUptVegMax / (1.0 + fSedUptVegCoef * ((((oPO4S+NearZero) / (sPO4W+NearZero)) )^ fSedUptVegExp)) ;
}
/* fraction of P uptake from sediment [-] */
if (0 == UseEmpUpt) {
   afNUptVegS = 0.0 ;
} else if (bfRootVeg <= NearZero) {
   afNUptVegS = 0.0 ;
} else if (fFloatVeg + bfSubVeg <= NearZero) {
   afNUptVegS = 1.0 ;
} else {
   afNUptVegS = fSedUptVegMax / (1.0 + fSedUptVegCoef * ((((oNDissS+NearZero) / (oNDissW+NearZero)) )^ fSedUptVegExp)) ;
}
/* fraction of N uptake from sediment [-] */
if (0 == UseEmpUpt) {
   aVPUptMaxCrVeg = max( 0.0, cVPUptMaxVeg * uFunTmProdVeg * (cPDVegMax-rPDVeg) / (cPDVegMax-cPDVegMin) ) ;
} else {
   aVPUptMaxCrVeg = max( 0.0, cVPUptMaxVeg * uFunTmProdVeg * (cPDVegMax-rPDVeg) / (cPDVegMax-cPDVegMin) ) ;
}
/* maximum P uptake rate of vegetation corrected for P/D ratio [-] */
if (0 == UseEmpUpt) {
   aVPUptVegW = sPO4W * aVPUptMaxCrVeg / (aVPUptMaxCrVeg / cAffPUptVeg + sPO4W) ;
} else {
   aVPUptVegW = 0.0 ;
}
/* P uptake RATE by subm AND floating parts [mgP/mgD/d] */
if (0 == UseEmpUpt) {
   aVPUptVegS = oPO4S * aVPUptMaxCrVeg / (aVPUptMaxCrVeg / cAffPUptVeg + oPO4S) ;
} else {
   aVPUptVegS = 0.0 ;
}
/* P uptake rate by roots [mgP/mgD/d] */
if (0 == UseEmpUpt) {
   tPUptVegW = aVPUptVegW * (aDSubVeg + aDFloatVeg) ;
} else {
   tPUptVegW = (1.0 - afPUptVegS) * aVPUptMaxCrVeg * sPO4W / (aVPUptMaxCrVeg / cAffPUptVeg + sPO4W) * sDVeg ;
}
/* P uptake from water [gP/m2/d] */
if (0 == UseEmpUpt) {
   tPUptVegS = aVPUptVegS * aDRootVeg ;
} else {
   tPUptVegS = afPUptVegS * aVPUptMaxCrVeg * oPO4S / (aVPUptMaxCrVeg / cAffPUptVeg + oPO4S) * sDVeg ;
}
/* P uptake from pore water (by root fraction) [gP/m2/d] */
if (0 == UseEmpUpt) {
   tPUptVeg = tPUptVegW + tPUptVegS ;
} else {
   tPUptVeg = tPUptVegW + tPUptVegS ;
}
/* total P uptake vegetation [gP/m2/d] */
if (0 == UseEmpUpt) {
   aVNUptMaxCrVeg = max( 0.0, cVNUptMaxVeg * uFunTmProdVeg * (cNDVegMax - rNDVeg) / (cNDVegMax - cNDVegMin)) ;
} else {
   aVNUptMaxCrVeg = max( 0.0, cVNUptMaxVeg * uFunTmProdVeg * (cNDVegMax - rNDVeg) / (cNDVegMax - cNDVegMin)) ;
}
/* maximum N uptake rate of vegetation corrected for N/D ratio [-] */
if (0 == UseEmpUpt) {
   ahNUptVeg = aVNUptMaxCrVeg / cAffNUptVeg ;
} else {
   ahNUptVeg = aVNUptMaxCrVeg / cAffNUptVeg ;
}
/* half-sat constant for N uptake [gN/m3] */
if (0 == UseEmpUpt) {
   aVNUptVegW = oNDissW * aVNUptMaxCrVeg / (ahNUptVeg + oNDissW) ;
} else {
   aVNUptVegW = 0.0 ;
}
/* N uptake RATE by subm AND floating parts [mgN/mgD/d] */
if (0 == UseEmpUpt) {
   afNH4UptVegW = sNH4W * sNO3W / ((ahNUptVeg + sNH4W) * (ahNUptVeg + sNO3W + NearZero)) + sNH4W * ahNUptVeg / ((sNH4W + sNO3W + NearZero) * (ahNUptVeg + sNO3W + NearZero)) ;
} else {
   afNH4UptVegW = sNH4W * sNO3W / ((ahNUptVeg + sNH4W) * (ahNUptVeg + sNO3W + NearZero)) + sNH4W * ahNUptVeg / ((sNH4W + sNO3W + NearZero) * (ahNUptVeg + sNO3W + NearZero)) ;
}
/* fraction ammonium uptake from water column (from WASP model EPA) [-] */
if (0 == UseEmpUpt) {
   tNUptVegW = aVNUptVegW * (aDSubVeg + aDFloatVeg) ;
} else {
   tNUptVegW = (1.0 - afNUptVegS) * aVNUptMaxCrVeg * oNDissW / (aVNUptMaxCrVeg / cAffNUptVeg + oNDissW) * sDVeg ;
}
/* N uptake from water (by shoots) [gN/m2/d] */
if (0 == UseEmpUpt) {
   tNUptNH4VegW = afNH4UptVegW * tNUptVegW ;
} else {
   tNUptNH4VegW = afNH4UptVegW * tNUptVegW ;
}
/* NH4 uptake of vegetation from water [gN/m2/d] */
if (0 == UseEmpUpt) {
   tNUptNO3VegW = tNUptVegW - tNUptNH4VegW ;
} else {
   tNUptNO3VegW = tNUptVegW - tNUptNH4VegW ;
}
/* NO3 uptake of vegetation from water [gN/m2/d] */
if (0 == UseEmpUpt) {
   aVNUptVegS = oNDissS * aVNUptMaxCrVeg / (ahNUptVeg + oNDissS) ;
} else {
   aVNUptVegS = 0.0 ;
}
/* N uptake RATE of roots [mgN/mgD/d] */
if (0 == UseEmpUpt) {
   tNUptVegS = aVNUptVegS * aDRootVeg ;
} else {
   tNUptVegS = afNUptVegS * aVNUptMaxCrVeg * oNDissS / (aVNUptMaxCrVeg / cAffNUptVeg + oNDissS) * sDVeg ;
}
/* N uptake from pore water (by roots) [gN/m2/d] */
if (0 == UseEmpUpt) {
   afNH4UptVegS = oNH4S * oNO3S / ((ahNUptVeg + oNH4S +NearZero) * (ahNUptVeg + oNO3S +NearZero)) + oNH4S * ahNUptVeg / ((oNH4S + oNO3S+NearZero) * (ahNUptVeg + oNO3S+NearZero)) ;
} else {
   afNH4UptVegS = oNH4S * oNO3S / ((ahNUptVeg + oNH4S +NearZero) * (ahNUptVeg + oNO3S +NearZero)) + oNH4S * ahNUptVeg / ((oNH4S + oNO3S+NearZero) * (ahNUptVeg + oNO3S+NearZero)) ;
}
/* fraction ammonium uptake from pore water (from WASP model EPA) [-] */
if (0 == UseEmpUpt) {
   tNUptNH4VegS = afNH4UptVegS * tNUptVegS ;
} else {
   tNUptNH4VegS = afNH4UptVegS * tNUptVegS ;
}
/* NH4 uptake of vegetation from sediment [gN/m2/d] */
if (0 == UseEmpUpt) {
   tNUptNO3VegS = tNUptVegS - tNUptNH4VegS ;
} else {
   tNUptNO3VegS = tNUptVegS - tNUptNH4VegS ;
}
/* NO3 uptake of vegetation from sediment [gN/m2/d] */
if (0 == UseEmpUpt) {
   tNUptVeg = tNUptVegW + tNUptVegS ;
} else {
   tNUptVeg = tNUptVegW + tNUptVegS ;
}
/* total N uptake vegetation [gN/m2/d] */
aLPAR1Veg = uLPARSurf * exp(- aExtCoefOpen * aDepth1Veg) ;
/* light at top of vegetation layer [W/m2_PAR] */
aLPAR2Veg = aLPAR1Veg * exp(- aExtCoef * (aDepth2Veg - aDepth1Veg)) ;
/* light at bottom of vegetation layer [W/m2_PAR] */
uhLVeg = hLRefVeg * uFunTmProdVeg ;
/* half-sat light for vegetation production at current temp [W/m2] */
aLLimShootVeg = fEmergVeg + fFloatVeg * (1.0 - afCovEmergVeg) + bfSubVeg * (1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * ln( (1.0 + aLPAR1Veg / uhLVeg) / (1.0 + aLPAR2Veg / uhLVeg)) ;
/* light function of growth based on shoot fraction [-] */
aMuTmLVeg = ufDay * bfShootVeg * aLLimShootVeg * uFunTmProdVeg * cMuMaxVeg ;
/* max growth rate at current temp AND light [g_prod./g_total_biomass/d] */
aPLimVeg = max(0.0, (1.0 - cPDVegMin / rPDVeg) * cPDVegMax / (cPDVegMax - cPDVegMin) ) ;
/* Droop function (P) for vegetation [-] */
aNLimVeg = max(0.0, (1.0 - cNDVegMin / rNDVeg) * cNDVegMax / (cNDVegMax - cNDVegMin) ) ;
/* Droop function (N) for vegetation [-] */
aNutLimVeg = min( aPLimVeg, aNLimVeg) ;
/* nutrient limitation function of vegetation [-] */
aMuVeg = aMuTmLVeg * aNutLimVeg ;
/* actual growth rate of vegetation [g_prod./g_total_biomass/d] */
if (0 == InclSeason) {
   bkMortVeg = kMortVegSum ;
} else if (Day < cDayWinVeg) {
   bkMortVeg = kMortVegSum ;
} else if (Day < cDayWinVeg + cLengMort) {
   bkMortVeg = - ln(fWinVeg) / cLengMort ;
} else {
   bkMortVeg = kMortVegSum ;
}
/* mortality constant [d-1] */
akDIncrVeg = aMuTmLVeg - kDRespVeg * uFunTmRespVeg - bkMortVeg ;
/* intrinsic net increase rate of vegetation [d-1] */
tDEnvVeg = max(0.0, akDIncrVeg / (cDCarrVeg+NearZero) * sDVeg*sDVeg) ;
/* logistic correction of vegetation [gDW/m2/d] */
tDEnvProdVeg = aMuVeg / cMuMaxVeg * tDEnvVeg ;
/* logistic correction of production [gDW/m2/d] */
tDProdVeg = max(0.0, aMuVeg * sDVeg - tDEnvProdVeg) ;
/* vegetation production [gDW/m2/d] */
tDProdSubVeg = bfSubVeg * tDProdVeg ;
/* submerged production [gDW/m2/d] */
tDRespVeg = kDRespVeg * uFunTmRespVeg * sDVeg ;
/* dark respiration of vegetation [g.m-2.d-1] */
tDEnvMortVeg = tDEnvVeg - tDEnvProdVeg ;
/* logistic correction of mortality [gDW/m2/d] */
tDMortVeg = bkMortVeg * sDVeg + tDEnvMortVeg ;
/* total mortality flux DW vegetation [gDW/m2/d] */
tDMortVegW = fDetWMortVeg * (1.0 - bfRootVeg) * tDMortVeg ;
/* mortality flux becoming water detritus [gDW/m2/d] */
tDMortVegS = tDMortVeg - tDMortVegW ;
/* mortality flux becoming sediment detritus [gDW/m2/d] */
if ( (sTime >= cYearStartBirds * DaysPerYear) && (Day >= cDayStartBirds) && (Day <= cDayEndBirds) ) {
   tDGrazVegBird = cPrefVegBird * sDVeg / (hDVegBird + sDVeg) * cBirdsPerha / m2Perha * cDGrazPerBird ;
} else {
   tDGrazVegBird = 0.0 ;
}
/* biomass loss due to grazing of birds [g/m2/d] */
if ((Day >= cDayManVeg1 && Day < cDayManVeg1 + cLengMan) || (Day >= cDayManVeg2 && Day < cDayManVeg2 + cLengMan)) {
   bkManVeg = -ln(1.0 - fManVeg) / cLengMan ;
} else {
   bkManVeg = 0.0 ;
}
/* rate constant during mowing period [d-1] */
tDManVeg = bkManVeg * sDVeg ;
/* Mowing of vegetation DW [gDW/m2/d] */
tPManVeg = rPDVeg * tDManVeg ;
/* Mowing of vegetation P [gP/m2/d] */
tNManVeg = rNDVeg * tDManVeg ;
/* Mowing of vegetation N [gN/m2/d] */
tDBedVeg = tDMigrVeg + tDProdVeg - tDRespVeg - tDMortVeg - tDGrazVegBird - tDManVeg ;
/* derivative of vegetation biomass [gDW/m2/d] */
tO2ProdVeg = molO2molC * cCPerDW * tDProdVeg ;
/* vegetation O2 production [gO2/m2/d] */
tO2RespVegW = molO2molC * cCPerDW * bfSubVeg * tDRespVeg * aCorO2BOD ;
/* submerged O2 respiration [gO2/m2/d] */
tO2RespVegS = molO2molC * cCPerDW * bfRootVeg * tDRespVeg * afOxySed ;
/* root O2 respiration [gO2/m2/d] */
tO2ProdVegS = min (tO2RespVegS, tO2ProdVeg) ;
/* O2 transport to roots [gO2/m2/d] */
tO2ProdVegW = min( tO2ProdVeg - tO2ProdVegS, bfSubVeg * tO2ProdVeg) ;
/* O2 used for vegetation production [gO2/m2/d] */
tO2UptNO3VegW = O2PerNO3 * molO2molN * bfSubVeg * tNUptNO3VegW ;
/* O2 production to water due to NO3 uptake by macrophytes [gO2/m2/d] */
tO2UptNO3VegS = O2PerNO3 * molO2molN * tNUptNO3VegS ;
/* O2 production due to NO3 uptake from sed by macrophytes [gO2/m2/d] */
tPExcrVeg = (rPDVeg *2.0)/ (cPDVegMax + rPDVeg) * rPDVeg * tDRespVeg ;
/* P excretion by vegetation [gP/m2/d] */
tPExcrVegS = bfRootVeg * tPExcrVeg ;
/* P excretion by vegetation in sediment [gP/m2/d] */
tPExcrVegW = tPExcrVeg - tPExcrVegS ;
/* P excretion by vegetation in water [gP/m2/d] */
tPMortVeg = rPDVeg * tDMortVeg ;
/* P mortality flux of vegetation [gP/m2/d] */
tPMortVegPO4 = fDissMortVeg * tPMortVeg ;
/* mortality flux of vegetation becoming dissolved P [gP/m2/d] */
tPMortVegPO4S = bfRootVeg * tPMortVegPO4 ;
/* mortality flux of vegetation becoming dissolved P in sediment [gP/m2/d] */
tPMortVegPO4W = tPMortVegPO4 - tPMortVegPO4S ;
/* mortality flux of vegetation becoming dissolved P in water [gP/m2/d] */
tPMortVegDet = tPMortVeg - tPMortVegPO4 ;
/* mortality flux of vegetation becoming detritus P [gP/m2/d] */
tPMortVegDetW = fDetWMortVeg * (1.0 - bfRootVeg) * tPMortVegDet ;
/* mortality flux of vegetation becoming detritus P in water [gP/m2/d] */
tPMortVegDetS = tPMortVegDet - tPMortVegDetW ;
/* mortality flux of vegetation becoming detritus P in sediment [gP/m2/d] */
tPGrazVegBird = rPDVeg * tDGrazVegBird ;
/* P mortality flux of vegetation by bird grazing [gP/m2/d] */
tPBedVeg = tPMigrVeg + tPUptVeg - tPExcrVeg - tPMortVeg - tPGrazVegBird - tPManVeg ;
/* total vegetation P flux in bed module [gP/m2/d] */
tNExcrVeg = (2.0 * rNDVeg) / (cNDVegMax + rNDVeg) * rNDVeg * tDRespVeg ;
/* N excretion by vegetation [gN/m2/d] */
tNExcrVegS = bfRootVeg * tNExcrVeg ;
/* N excretion by vegetation to sediment [gN/m2/d] */
tNExcrVegW = tNExcrVeg - tNExcrVegS ;
/* N excretion by vegetation to water [gN/m2/d] */
tNMortVeg = rNDVeg * tDMortVeg ;
/* N mortality flux of vegetation [gN/m2/d] */
tNMortVegNH4 = fDissMortVeg * tNMortVeg ;
/* mortality flux of vegetation becoming dissolved N [gN/m2/d] */
tNMortVegNH4S = bfRootVeg * tNMortVegNH4 ;
/* mortality flux of vegetation becoming dissolved N in sediment [gN/m2/d] */
tNMortVegNH4W = tNMortVegNH4 - tNMortVegNH4S ;
/* mortality flux of vegetation becoming dissolved N in water [gN/m2/d] */
tNMortVegDet = tNMortVeg - tNMortVegNH4 ;
/* mortality flux of vegetation becoming detritus N [gN/m2/d] */
tNMortVegDetW = fDetWMortVeg * (1.0 - bfRootVeg) * tNMortVegDet ;
/* mortality flux of vegetation becoming detritus N in water [gN/m2/d] */
tNMortVegDetS = tNMortVegDet - tNMortVegDetW ;
/* mortality flux of vegetation becoming detritus N in sediment [gN/m2/d] */
tNGrazVegBird = rNDVeg * tDGrazVegBird ;
/* N mortality flux of vegetation by bird grazing [gN/m2/d] */
tNBedVeg = tNMigrVeg + tNUptVeg - tNExcrVeg - tNMortVeg - tNGrazVegBird - tNManVeg ;
/* total vegetation N flux in bed module [gN/m2/d] */
tDAssVegBird = fDAssBird * tDGrazVegBird ;
/* DW assimilation by herbivorous birds [gDW/m2/d] */
tDEgesBird = tDGrazVegBird - tDAssVegBird ;
/* DW egestion by herbivorous birds [gDW/m2/d] */
tPAssVegBird = fDAssBird * tPGrazVegBird ;
/* P assimilation by herbivorous birds [gP/m2/d] */
tPEgesBird = tPGrazVegBird - tPAssVegBird ;
/* P egestion by herbivorous birds [gP/m2/d] */
tPEgesBirdPO4 = fDissEgesBird * tPEgesBird ;
/* PO4 egestion by herbivorous birds [gP/m2/d] */
tPEgesBirdDet = tPEgesBird - tPEgesBirdPO4 ;
/* P detritus egestion by herbivorous birds [gP/m2/d] */
tNAssVegBird = fDAssBird * tNGrazVegBird ;
/* N assimilation by herbivorous birds [gN/m2/d] */
tNEgesBird = tNGrazVegBird - tNAssVegBird ;
/* N egestion by herbivorous birds [gN/m2/d] */
tNEgesBirdNH4 = fDissEgesBird * tNEgesBird ;
/* NH4 egestion by herbivorous birds [gN/m2/d] */
tNEgesBirdDet = tNEgesBird - tNEgesBirdNH4 ;
/* N detritus egestion by herbivorous birds [gN/m2/d] */
wDBedDetW = (tDMortVegW + tDEgesBird) / sDepthW ;
/* total DW flux from Vegetation module to water detritus [gD.m-3.d-1] */
tDBedDetS = tDMortVegS ;
/* total DW flux from Vegetation module to sediment detritus [gDW/m2/d] */
tDBedTotT = tDMigrVeg + tDProdVeg - tDRespVeg - tDManVeg - tDAssVegBird ;
/* total DW flux from Vegetation module [gDW/m2/d] */
wPBedPO4W = (- tPUptVegW + tPExcrVegW + tPMortVegPO4W + tPEgesBirdPO4) /sDepthW ;
/* total P flux from Vegetation module to PO4 in water [gP/m3/d] */
wPBedDetW = (tPMortVegDetW + tPEgesBirdDet) / sDepthW ;
/* total P flux from Vegetation module to water detritus [gP/m3/d] */
tPBedPO4S = - tPUptVegS + tPExcrVegS + tPMortVegPO4S ;
/* total P flux from Vegetation module to pore water PO4 [gP/m2/d] */
tPBedDetS = tPMortVegDetS ;
/* total P flux from Vegetation module to sediment detritus [gP/m2/d] */
tPBedTotT = tPMigrVeg - tPManVeg - tPAssVegBird ;
/* total P flux from Vegetation module [gP/m2/d] */
wNBedNH4W = (- tNUptNH4VegW + tNExcrVegW + tNMortVegNH4W + tNEgesBirdNH4) / sDepthW ;
/* total N flux from Vegetation module to NH4 in water [gN/m3/d] */
wNBedNO3W = - tNUptNO3VegW / sDepthW ;
/* total N flux from Vegetation module to NO3 in water [gN/m3/d] */
wNBedDetW = (tNMortVegDetW + tNEgesBirdDet) / sDepthW ;
/* total N flux from Vegetation module to water detritus [gN/m3/d] */
tNBedNH4S = - tNUptNH4VegS + tNExcrVegS + tNMortVegNH4S ;
/* total N flux from Vegetation module to NH4 in pore water [gN/m2/d] */
tNBedNO3S = - tNUptNO3VegS ;
/* total N flux from Vegetation module to NO3 in pore water [gN/m2/d] */
tNBedDetS = tNMortVegDetS ;
/* total N flux from Vegetation module to sediment detritus [gN/m2/d] */
tNBedTotT = tNMigrVeg - tNManVeg - tNAssVegBird ;
/* total N flux from Vegetation module [gN/m2/d] */
tO2BedW = tO2ProdVegW - tO2RespVegW + tO2UptNO3VegW ;
/* total water O2 flux in vegetation module [gO2/m2/d] */
tO2BedS = tO2ProdVegS - tO2RespVegS + tO2UptNO3VegS ;
/* total sediment O2 flux in vegetation module [gO2/m2/d] */

/* ---------------------------------- */
/* Phytoplankton_processes */ 
/* ---------------------------------- */

if (InclWeb == 1) {
   UseLoss = 0.0 ;
} else {
   UseLoss = 1.0 ;
}
/* - [gO2/m2/d] */
uFunTmLoss = exp(-0.5/(cSigTmLoss*cSigTmLoss) *((uTm-cTmOptLoss)*(uTm-cTmOptLoss) -(cTmRef-cTmOptLoss)*(cTmRef-cTmOptLoss))) ;
/* temp function of grazing [-] */

/* ---------------------------------- */
/* Phytoplankton_processes_Blue */ 
/* ---------------------------------- */

rPDBlueW = sPBlueW /(sDBlueW+NearZero) ;
/* P/D ratio of Algae [-] */
rNDBlueW = sNBlueW /(sDBlueW+NearZero) ;
/* N/D ratio of Algae [-] */
rPDBlueS = sPBlueS /(sDBlueS+NearZero) ;
/* P/D ratio of Algae [-] */
rNDBlueS = sNBlueS /(sDBlueS+NearZero) ;
/* N/D ratio of Algae [-] */
uFunTmBlue = exp(-0.5/(cSigTmBlue*cSigTmBlue) *((uTm-cTmOptBlue)*(uTm-cTmOptBlue) - (cTmRef-cTmOptBlue)*(cTmRef-cTmOptBlue))) ;
/* temperature function of Algae [-] */
uFunTmProdBlue = uFunTmBlue ;
/* temperature function of Algae [-] */
uFunTmRespBlue = uFunTmBlue ;
/* temperature function of Algae [-] */
aVPUptMaxCrBlue = max(0.0,cVPUptMaxBlue * uFunTmProdBlue *(cPDBlueMax - rPDBlueW) /(cPDBlueMax - cPDBlueMin)) ;
/* maximum P uptake rate of Algaecorrected for P/D ratio [-] */
aVPUptBlue = sPO4W * aVPUptMaxCrBlue /(aVPUptMaxCrBlue / cAffPUptBlue + sPO4W) ;
/* P uptake rate of Algae [mgP/mgDW/d] */
wPUptBlue = aVPUptBlue * sDBlueW ;
/* P uptake Algae [gP/m2/d] */
aVNUptMaxCrBlue = max(0.0,cVNUptMaxBlue * uFunTmProdBlue * (cNDBlueMax - rNDBlueW) /(cNDBlueMax - cNDBlueMin)) ;
/* maximum N uptake rate of Algaecorrected for N/D ratio [-] */
ahNUptBlue = aVNUptMaxCrBlue / cAffNUptBlue ;
/* half-sat NDissW for uptake by Algae [mgN/l] */
aVNUptBlue = oNDissW * aVNUptMaxCrBlue /(ahNUptBlue + oNDissW) ;
/* N uptake rate of Algae [mgN/mgDW/d] */
wNUptBlue = aVNUptBlue * sDBlueW ;
/* N uptake Algae [mgN/l/d] */
afNH4UptBlue = sNH4W * sNO3W /((ahNUptBlue + sNH4W) *(ahNUptBlue + sNO3W)) + sNH4W * ahNUptBlue /((sNH4W + sNO3W) *(ahNUptBlue + sNO3W)) ;
/* fraction ammonium uptake by Algae [-] */
wNUptNH4Blue = afNH4UptBlue * wNUptBlue ;
/* ammonium uptake by Algae [mgN/l/d] */
wNUptNO3Blue = wNUptBlue - wNUptNH4Blue ;
/* nitrate uptake by Algae [mgN/l/d] */
uMuMaxTmBlue = cMuMaxBlue * uFunTmProdBlue ;
/* max growth rate of Algae at ambient temperature [d-1] */
aPLimBlue = max(0.0,(1.0 - cPDBlueMin / rPDBlueW) * cPDBlueMax /(cPDBlueMax - cPDBlueMin)) ;
/* Droop function(P) for Algae [-] */
aNLimBlue = max(0.0,(1.0 - cNDBlueMin / rNDBlueW) * cNDBlueMax /(cNDBlueMax - cNDBlueMin)) ;
/* Droop function(N) for Algae [-] */
aSiLimBlue = sSiO2W /(hSiAssBlue + sSiO2W) ;
/* silica dependence of growth rate [-] */
aLLimBlue = UseSteeleBlue *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cLOptRefBlue * uFunTmProdBlue)) - exp(- uLPARSurf /(cLOptRefBlue * uFunTmProdBlue)))) +(1.0 - UseSteeleBlue) *(1.0 /(aExtCoef * sDepthW) * ln((1.0 + uLPARSurf / (hLRefBlue * uFunTmProdBlue)) / (1.0 + aLPARBot /(hLRefBlue * uFunTmProdBlue)))) ;
/* Light function [-] */
aMuTmLBlue = ufDay *(1.0 - afCovSurfVeg) * aLLimBlue * uMuMaxTmBlue ;
/* growth rate at current light AND temp [d-1] */
aNutLimBlue = min(aPLimBlue,(min(aNLimBlue,aSiLimBlue))) ;
/* nutrient limitation function of Algae [d-1] */
aMuBlue = aNutLimBlue * aMuTmLBlue ;
/* growth rate [d-1] */
wDAssBlue = aMuBlue*sDBlueW ;
/* assimilation Algae [gD/m3/d] */
rChDBlue = cChDBlueMax -(cChDBlueMax - cChDBlueMin) * aLLimBlue ;
/* chlorophyll-a/DW ratio Algae [mg/mg] */
oChlaBlue = mgPerg * rChDBlue * sDBlueW ;
/* chlorophyll-a conc [mg/m3] */
aExtChBlue = cExtSpBlue / rChDBlue ;
/* specific extinction per unit chlorophyll-a [m2/mg_chl-a] */
ukDRespTmBlue = kDRespBlue * uFunTmRespBlue ;
/* temp corrected respiration constant of Algae [d-1] */
wDRespBlueW = ukDRespTmBlue * sDBlueW ;
/* respiration of Algae in water [g/m2/d] */
ukLossTmBlue = UseLoss * kLossBlue * uFunTmLoss ;
/* daily grazing on Algae [d-1] */
wDLossBlue = ukLossTmBlue * sDBlueW ;
/* Algae grazing loss [mgDW/l/d] */
wDMortBlueW = kMortBlueW * sDBlueW ;
/* mortality in water [g/m3/d] */
uCorVSetBlue = cVSetBlue * aFunTauSetOM * uFunTmSet ;
/* corrected sedimentation velocity of Algae [m/d] */
tDSetBlue = uCorVSetBlue * sDBlueW ;
/* sedimentation flux of Algae [g/m2/d] */
tDResusBlue = sDBlueS /(aDPhytS+NearZero) * tDResusPhytTot ;
/* resuspension DW blue-greens [g/m2/d] */
tDRespBlueS = ukDRespTmBlue * sDBlueS ;
/* respiration of sediment Algae [gD/m2/d] */
tDMortBlueS = kMortBlueS * sDBlueS ;
/* mortality in sed [g/m2/d] */
ukDDecBlue = ukDRespTmBlue + ukLossTmBlue + kMortBlueW +(uCorVSetBlue * uFunTmSet) / sDepthW ;
/* total loss rate of algae in water(excl dilution) [d-1] */
wPExcrBlueW = (rPDBlueW * 2.0 )/(cPDBlueMax + rPDBlueW) * rPDBlueW * wDRespBlueW ;
/* P excretion Algae in water [gP/m3/d] */
wPLossBlue = rPDBlueW * wDLossBlue ;
/* Algae grazing loss [mgP/l/d] */
wPMortBlueW = kMortBlueW * sPBlueW ;
/* mortality Algae in water [gP/m3/d] */
tPSetBlue = rPDBlueW * tDSetBlue ;
/* sedimentation [gP/m2/d] */
tPResusBlue = rPDBlueS * tDResusBlue ;
/* Resuspension of algae [gD/m2/d] */
tPExcrBlueS = (rPDBlueS *2.0)/(cPDBlueMax + rPDBlueS) * rPDBlueS * tDRespBlueS ;
/* P excretion of algae in sediment [gP/m2/d] */
tPMortBlueS = kMortBlueS * sPBlueS ;
/* P mortality of algae in sediment [gP/m2/d] */
wNExcrBlueW = (rNDBlueW *2.0)/(cNDBlueMax + rNDBlueW) * rNDBlueW * wDRespBlueW ;
/* N excretion Algae in water [gN/m3/d] */
wNLossBlue = rNDBlueW * wDLossBlue ;
/* Algae grazing loss [mgN/l/d] */
wNMortBlueW = kMortBlueW * sNBlueW ;
/* mortality Algae in water [gN/m3/d] */
tNSetBlue = rNDBlueW * tDSetBlue ;
/* sedimentation [gN/m2/d] */
tNResusBlue = rNDBlueS * tDResusBlue ;
/* Resuspension of algae [gD/m2/d] */
tNExcrBlueS = (2.0 * rNDBlueS) /(cNDBlueMax + rNDBlueS) * rNDBlueS * tDRespBlueS ;
/* N excretion of algae in sediment [gN/m2/d] */
tNMortBlueS = kMortBlueS * sNBlueS ;
/* N mortality of algae in sediment [gN/m2/d] */
wDPrimBlueW = wDAssBlue - wDRespBlueW - wDLossBlue - wDMortBlueW -(tDSetBlue - tDResusBlue) / sDepthW ;
/* total PRIM flux to algae in water [mgD/l/d] */
wPPrimBlueW = wPUptBlue - wPExcrBlueW - wPLossBlue - wPMortBlueW -(tPSetBlue - tPResusBlue) / sDepthW ;
/* Total PRIM flux to Algae [mgP/l/d] */
wNPrimBlueW = wNUptBlue - wNExcrBlueW - wNLossBlue - wNMortBlueW -(tNSetBlue - tNResusBlue) / sDepthW ;
/* Total PRIM flux to Algae [mgN/l/d] */
if (InclPhytS == 1) {
   tDPrimBlueS = tDSetBlue - tDResusBlue - tDMortBlueS - tDRespBlueS ;
} else {
   tDPrimBlueS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gD/m2/d] */
if (InclPhytS == 1) {
   tPPrimBlueS = tPSetBlue - tPResusBlue - tPMortBlueS - tPExcrBlueS ;
} else {
   tPPrimBlueS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gP/m2/d] */
if (InclPhytS == 1) {
   tNPrimBlueS = tNSetBlue - tNResusBlue - tNMortBlueS - tNExcrBlueS ;
} else {
   tNPrimBlueS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gN/m2/d] */

/* ---------------------------------- */
/* Phytoplankton_processes_Green */ 
/* ---------------------------------- */

rPDGrenW = sPGrenW /(sDGrenW+NearZero) ;
/* P/D ratio of Algae [-] */
rNDGrenW = sNGrenW /(sDGrenW+NearZero) ;
/* N/D ratio of Algae [-] */
rPDGrenS = sPGrenS /(sDGrenS+NearZero) ;
/* P/D ratio of Algae [-] */
rNDGrenS = sNGrenS /(sDGrenS+NearZero) ;
/* N/D ratio of Algae [-] */
uFunTmGren = exp(-0.5/(cSigTmGren*cSigTmGren) *((uTm-cTmOptGren)*(uTm-cTmOptGren) - (cTmRef-cTmOptGren)*(cTmRef-cTmOptGren))) ;
/* temperature function of Algae [-] */
uFunTmProdGren = uFunTmGren ;
/* temperature function of Algae [-] */
uFunTmRespGren = uFunTmGren ;
/* temperature function of Algae [-] */
aVPUptMaxCrGren = max(0.0,cVPUptMaxGren * uFunTmProdGren *(cPDGrenMax - rPDGrenW) /(cPDGrenMax - cPDGrenMin)) ;
/* maximum P uptake rate of Algaecorrected for P/D ratio [-] */
aVPUptGren = sPO4W * aVPUptMaxCrGren /(aVPUptMaxCrGren / cAffPUptGren + sPO4W) ;
/* P uptake rate of Algae [mgP/mgDW/d] */
wPUptGren = aVPUptGren * sDGrenW ;
/* P uptake Algae [gP/m2/d] */
aVNUptMaxCrGren = max(0.0,cVNUptMaxGren * uFunTmProdGren * (cNDGrenMax - rNDGrenW) /(cNDGrenMax - cNDGrenMin)) ;
/* maximum N uptake rate of Algaecorrected for N/D ratio [-] */
ahNUptGren = aVNUptMaxCrGren / cAffNUptGren ;
/* half-sat NDissW for uptake by Algae [mgN/l] */
aVNUptGren = oNDissW * aVNUptMaxCrGren /(ahNUptGren + oNDissW) ;
/* N uptake rate of Algae [mgN/mgDW/d] */
wNUptGren = aVNUptGren * sDGrenW ;
/* N uptake Algae [mgN/l/d] */
afNH4UptGren = sNH4W * sNO3W /((ahNUptGren + sNH4W) *(ahNUptGren + sNO3W)) + sNH4W * ahNUptGren /((sNH4W + sNO3W) *(ahNUptGren + sNO3W)) ;
/* fraction ammonium uptake by Algae [-] */
wNUptNH4Gren = afNH4UptGren * wNUptGren ;
/* ammonium uptake by Algae [mgN/l/d] */
wNUptNO3Gren = wNUptGren - wNUptNH4Gren ;
/* nitrate uptake by Algae [mgN/l/d] */
uMuMaxTmGren = cMuMaxGren * uFunTmProdGren ;
/* max growth rate of Algae at ambient temperature [d-1] */
aPLimGren = max(0.0,(1.0 - cPDGrenMin / rPDGrenW) * cPDGrenMax /(cPDGrenMax - cPDGrenMin)) ;
/* Droop function(P) for Algae [-] */
aNLimGren = max(0.0,(1.0 - cNDGrenMin / rNDGrenW) * cNDGrenMax /(cNDGrenMax - cNDGrenMin)) ;
/* Droop function(N) for Algae [-] */
aSiLimGren = sSiO2W /(hSiAssGren + sSiO2W) ;
/* silica dependence of growth rate [-] */
aLLimGren = UseSteeleGren *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cLOptRefGren * uFunTmProdGren)) - exp(- uLPARSurf /(cLOptRefGren * uFunTmProdGren)))) +(1.0 - UseSteeleGren) *(1.0 /(aExtCoef * sDepthW) * ln((1.0 + uLPARSurf / (hLRefGren * uFunTmProdGren)) / (1.0 + aLPARBot /(hLRefGren * uFunTmProdGren)))) ;
/* Light function [-] */
aMuTmLGren = ufDay *(1.0 - afCovSurfVeg) * aLLimGren * uMuMaxTmGren ;
/* growth rate at current light AND temp [d-1] */
aNutLimGren = min(aPLimGren,(min(aNLimGren,aSiLimGren))) ;
/* nutrient limitation function of Algae [d-1] */
aMuGren = aNutLimGren * aMuTmLGren ;
/* growth rate [d-1] */
wDAssGren = aMuGren*sDGrenW ;
/* assimilation Algae [gD/m3/d] */
rChDGren = cChDGrenMax -(cChDGrenMax - cChDGrenMin) * aLLimGren ;
/* chlorophyll-a/DW ratio Algae [mg/mg] */
oChlaGren = mgPerg * rChDGren * sDGrenW ;
/* chlorophyll-a conc [mg/m3] */
aExtChGren = cExtSpGren / rChDGren ;
/* specific extinction per unit chlorophyll-a [m2/mg_chl-a] */
ukDRespTmGren = kDRespGren * uFunTmRespGren ;
/* temp corrected respiration constant of Algae [d-1] */
wDRespGrenW = ukDRespTmGren * sDGrenW ;
/* respiration of Algae in water [g/m2/d] */
ukLossTmGren = UseLoss * kLossGren * uFunTmLoss ;
/* daily grazing on Algae [d-1] */
wDLossGren = ukLossTmGren * sDGrenW ;
/* Algae grazing loss [mgDW/l/d] */
wDMortGrenW = kMortGrenW * sDGrenW ;
/* mortality in water [g/m3/d] */
uCorVSetGren = cVSetGren * aFunTauSetOM * uFunTmSet ;
/* corrected sedimentation velocity of Algae [m/d] */
tDSetGren = uCorVSetGren * sDGrenW ;
/* sedimentation flux of Algae [g/m2/d] */
tDResusGren = sDGrenS /(aDPhytS+NearZero) * tDResusPhytTot ;
/* resuspension of Algae [gD/m2/d] */
tDRespGrenS = ukDRespTmGren * sDGrenS ;
/* respiration of sediment Algae [gD/m2/d] */
tDMortGrenS = kMortGrenS * sDGrenS ;
/* mortality in sed [g/m2/d] */
ukDDecGren = ukDRespTmGren + ukLossTmGren + kMortGrenW +(uCorVSetGren * uFunTmSet) / sDepthW ;
/* total loss rate of algae in water(excl dilution) [d-1] */
wPExcrGrenW = (2.0 *rPDGrenW) /(cPDGrenMax + rPDGrenW) * rPDGrenW * wDRespGrenW ;
/* P excretion Algae in water [gP/m3/d] */
wPLossGren = rPDGrenW * wDLossGren ;
/* Algae grazing loss [mgP/l/d] */
wPMortGrenW = kMortGrenW * sPGrenW ;
/* mortality Algae in water [gP/m3/d] */
tPSetGren = rPDGrenW * tDSetGren ;
/* sedimentation [gP/m2/d] */
tPResusGren = rPDGrenS * tDResusGren ;
/* Resuspension of algae [gD/m2/d] */
tPExcrGrenS = (2.0 * rPDGrenS) /(cPDGrenMax + rPDGrenS) * rPDGrenS * tDRespGrenS ;
/* P excretion of algae in sediment [gP/m2/d] */
tPMortGrenS = kMortGrenS * sPGrenS ;
/* P mortality of algae in sediment [gP/m2/d] */
wNExcrGrenW = (2.0 * rNDGrenW) /(cNDGrenMax + rNDGrenW) * rNDGrenW * wDRespGrenW ;
/* N excretion Algae in water [gN/m3/d] */
wNLossGren = rNDGrenW * wDLossGren ;
/* Algae grazing loss [mgN/l/d] */
wNMortGrenW = kMortGrenW * sNGrenW ;
/* mortality Algae in water [gN/m3/d] */
tNSetGren = rNDGrenW * tDSetGren ;
/* sedimentation [gN/m2/d] */
tNResusGren = rNDGrenS * tDResusGren ;
/* Resuspension of algae [gD/m2/d] */
tNExcrGrenS = (2.0 * rNDGrenS) /(cNDGrenMax + rNDGrenS) * rNDGrenS * tDRespGrenS ;
/* N excretion of algae in sediment [gN/m2/d] */
tNMortGrenS = kMortGrenS * sNGrenS ;
/* N mortality of algae in sediment [gN/m2/d] */
wDPrimGrenW = wDAssGren - wDRespGrenW - wDLossGren - wDMortGrenW -(tDSetGren - tDResusGren) / sDepthW ;
/* total PRIM flux to algae in water [mgD/l/d] */
wPPrimGrenW = wPUptGren - wPExcrGrenW - wPLossGren - wPMortGrenW -(tPSetGren - tPResusGren) / sDepthW ;
/* Total PRIM flux to Algae [mgP/l/d] */
wNPrimGrenW = wNUptGren - wNExcrGrenW - wNLossGren - wNMortGrenW -(tNSetGren - tNResusGren) / sDepthW ;
/* Total PRIM flux to Algae [mgN/l/d] */
if (InclPhytS == 1) {
   tDPrimGrenS = tDSetGren - tDResusGren - tDMortGrenS - tDRespGrenS ;
} else {
   tDPrimGrenS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gD/m2/d] */
if (InclPhytS == 1) {
   tPPrimGrenS = tPSetGren - tPResusGren - tPMortGrenS - tPExcrGrenS ;
} else {
   tPPrimGrenS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gP/m2/d] */
if (InclPhytS == 1) {
   tNPrimGrenS = tNSetGren - tNResusGren - tNMortGrenS - tNExcrGrenS ;
} else {
   tNPrimGrenS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gN/m2/d] */

/* ---------------------------------- */
/* Phytoplankton_processes_Diatom */ 
/* ---------------------------------- */

rPDDiatW = sPDiatW /(sDDiatW+NearZero) ;
/* P/D ratio of Algae [-] */
rNDDiatW = sNDiatW /(sDDiatW+NearZero) ;
/* N/D ratio of Algae [-] */
rPDDiatS = sPDiatS /(sDDiatS+NearZero) ;
/* P/D ratio of Algae [-] */
rNDDiatS = sNDiatS /(sDDiatS+NearZero) ;
/* N/D ratio of Algae [-] */
uFunTmDiat = exp(-0.5/(cSigTmDiat*cSigTmDiat) *((uTm-cTmOptDiat)*(uTm-cTmOptDiat) - (cTmRef-cTmOptDiat)*(cTmRef-cTmOptDiat))) ;
/* temperature function of Algae [-] */
uFunTmProdDiat = uFunTmDiat ;
/* temperature function production of Algae [-] */
uFunTmRespDiat = uFunTmDiat ;
/* temperature function respiration of Algae [-] */
aVPUptMaxCrDiat = max(0.0,cVPUptMaxDiat * uFunTmProdDiat *(cPDDiatMax - rPDDiatW) /(cPDDiatMax - cPDDiatMin)) ;
/* maximum P uptake rate of Algaecorrected for P/D ratio [-] */
aVPUptDiat = sPO4W * aVPUptMaxCrDiat /(aVPUptMaxCrDiat / cAffPUptDiat + sPO4W) ;
/* P uptake rate of Algae [mgP/mgDW/d] */
wPUptDiat = aVPUptDiat * sDDiatW ;
/* P uptake Algae [gP/m2/d] */
aVNUptMaxCrDiat = max(0.0,cVNUptMaxDiat * uFunTmProdDiat * (cNDDiatMax - rNDDiatW) /(cNDDiatMax - cNDDiatMin)) ;
/* maximum N uptake rate of Algaecorrected for N/D ratio [-] */
ahNUptDiat = aVNUptMaxCrDiat / cAffNUptDiat ;
/* half-sat NDissW for uptake by Algae [mgN/l] */
aVNUptDiat = oNDissW * aVNUptMaxCrDiat /(ahNUptDiat + oNDissW) ;
/* N uptake rate of Algae [mgN/mgDW/d] */
wNUptDiat = aVNUptDiat * sDDiatW ;
/* N uptake Algae [mgN/l/d] */
afNH4UptDiat = sNH4W * sNO3W /((ahNUptDiat + sNH4W) *(ahNUptDiat + sNO3W)) + sNH4W * ahNUptDiat /((sNH4W + sNO3W) *(ahNUptDiat + sNO3W)) ;
/* fraction ammonium uptake by Algae [-] */
wNUptNH4Diat = afNH4UptDiat * wNUptDiat ;
/* ammonium uptake by Algae [mgN/l/d] */
wNUptNO3Diat = wNUptDiat - wNUptNH4Diat ;
/* nitrate uptake by Algae [mgN/l/d] */
uMuMaxTmDiat = cMuMaxDiat * uFunTmProdDiat ;
/* max growth rate of Algae at ambient temperature [d-1] */
aPLimDiat = max(0.0,(1.0 - cPDDiatMin / rPDDiatW) * cPDDiatMax /(cPDDiatMax - cPDDiatMin)) ;
/* Droop function(P) for Algae [-] */
aNLimDiat = max(0.0,(1.0 - cNDDiatMin / rNDDiatW) * cNDDiatMax /(cNDDiatMax - cNDDiatMin)) ;
/* Droop function(N) for Algae [-] */
aSiLimDiat = sSiO2W /(hSiAssDiat + sSiO2W) ;
/* silica dependence of growth rate [-] */
aLLimDiat = UseSteeleDiat *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cLOptRefDiat * uFunTmProdDiat)) - exp(- uLPARSurf /(cLOptRefDiat * uFunTmProdDiat)))) +(1.0 - UseSteeleDiat) *(1.0 /(aExtCoef * sDepthW) * ln((1.0 + uLPARSurf / (hLRefDiat * uFunTmProdDiat)) / (1.0 + aLPARBot /(hLRefDiat * uFunTmProdDiat)))) ;
/* Light function [-] */
aMuTmLDiat = ufDay *(1.0 - afCovSurfVeg) * aLLimDiat * uMuMaxTmDiat ;
/* growth rate at current light AND temp [d-1] */
aNutLimDiat = min(aPLimDiat,(min(aNLimDiat,aSiLimDiat))) ;
/* nutrient limitation function of Algae [d-1] */
aMuDiat = aNutLimDiat * aMuTmLDiat ;
/* growth rate [d-1] */
wDAssDiat = aMuDiat*sDDiatW ;
/* assimilation Algae [gD/m3/d] */
rChDDiat = cChDDiatMax -(cChDDiatMax - cChDDiatMin) * aLLimDiat ;
/* chlorophyll-a/DW ratio Algae [mg/mg] */
oChlaDiat = mgPerg * rChDDiat * sDDiatW ;
/* chlorophyll-a conc [mg/m3] */
aExtChDiat = cExtSpDiat / rChDDiat ;
/* specific extinction per unit chlorophyll-a [m2/mg_chl-a] */
ukDRespTmDiat = kDRespDiat * uFunTmRespDiat ;
/* temp corrected respiration constant of Algae [d-1] */
wDRespDiatW = ukDRespTmDiat * sDDiatW ;
/* respiration of Algae in water [g/m2/d] */
ukLossTmDiat = UseLoss * kLossDiat * uFunTmLoss ;
/* daily grazing on Algae [d-1] */
wDLossDiat = ukLossTmDiat * sDDiatW ;
/* Algae grazing loss [mgDW/l/d] */
wDMortDiatW = kMortDiatW * sDDiatW ;
/* mortality in water [g/m3/d] */
uCorVSetDiat = cVSetDiat * aFunTauSetOM * uFunTmSet ;
/* corrected sedimentation velocity of Algae [m/d] */
tDSetDiat = uCorVSetDiat * sDDiatW ;
/* sedimentation flux of Algae [g/m2/d] */
tDResusDiat = sDDiatS /(aDPhytS+NearZero) * tDResusPhytTot ;
/* resuspension of Algae [gD/m2/d] */
tDRespDiatS = ukDRespTmDiat * sDDiatS ;
/* respiration of sediment Algae [gD/m2/d] */
tDMortDiatS = kMortDiatS * sDDiatS ;
/* mortality in sed [g/m2/d] */
ukDDecDiat = ukDRespTmDiat + ukLossTmDiat + kMortDiatW +(uCorVSetDiat * uFunTmSet) / sDepthW ;
/* total loss rate of algae in water(excl dilution) [d-1] */
wPExcrDiatW = (2.0 * rPDDiatW) /(cPDDiatMax + rPDDiatW) * rPDDiatW * wDRespDiatW ;
/* P excretion Algae in water [gP/m3/d] */
wPLossDiat = rPDDiatW * wDLossDiat ;
/* Algae grazing loss [mgP/l/d] */
wPMortDiatW = kMortDiatW * sPDiatW ;
/* mortality Algae in water [gP/m3/d] */
tPSetDiat = rPDDiatW * tDSetDiat ;
/* sedimentation [gP/m2/d] */
tPResusDiat = rPDDiatS * tDResusDiat ;
/* Resuspension of algae [gD/m2/d] */
tPExcrDiatS = (2.0 * rPDDiatS) /(cPDDiatMax + rPDDiatS) * rPDDiatS * tDRespDiatS ;
/* P excretion of algae in sediment [gP/m2/d] */
tPMortDiatS = kMortDiatS * sPDiatS ;
/* P mortality of algae in sediment [gP/m2/d] */
wNExcrDiatW = (2.0 * rNDDiatW) /(cNDDiatMax + rNDDiatW) * rNDDiatW * wDRespDiatW ;
/* N excretion Algae in water [gN/m3/d] */
wNLossDiat = rNDDiatW * wDLossDiat ;
/* Algae grazing loss [mgN/l/d] */
wNMortDiatW = kMortDiatW * sNDiatW ;
/* mortality Algae in water [gN/m3/d] */
tNSetDiat = rNDDiatW * tDSetDiat ;
/* sedimentation [gN/m2/d] */
tNResusDiat = rNDDiatS * tDResusDiat ;
/* Resuspension of algae [gD/m2/d] */
tNExcrDiatS = (2.0 * rNDDiatS) /(cNDDiatMax + rNDDiatS) * rNDDiatS * tDRespDiatS ;
/* N excretion of algae in sediment [gN/m2/d] */
tNMortDiatS = kMortDiatS * sNDiatS ;
/* N mortality of algae in sediment [gN/m2/d] */
wDPrimDiatW = wDAssDiat - wDRespDiatW - wDLossDiat - wDMortDiatW -(tDSetDiat - tDResusDiat) / sDepthW ;
/* total PRIM flux to algae in water [mgD/l/d] */
wPPrimDiatW = wPUptDiat - wPExcrDiatW - wPLossDiat - wPMortDiatW -(tPSetDiat - tPResusDiat) / sDepthW ;
/* Total PRIM flux to Algae [mgP/l/d] */
wNPrimDiatW = wNUptDiat - wNExcrDiatW - wNLossDiat - wNMortDiatW -(tNSetDiat - tNResusDiat) / sDepthW ;
/* Total PRIM flux to Algae [mgN/l/d] */
if (InclPhytS == 1) {
   tDPrimDiatS = tDSetDiat - tDResusDiat - tDMortDiatS - tDRespDiatS ;
} else {
   tDPrimDiatS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gD/m2/d] */
if (InclPhytS == 1) {
   tPPrimDiatS = tPSetDiat - tPResusDiat - tPMortDiatS - tPExcrDiatS ;
} else {
   tPPrimDiatS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gP/m2/d] */
if (InclPhytS == 1) {
   tNPrimDiatS = tNSetDiat - tNResusDiat - tNMortDiatS - tNExcrDiatS ;
} else {
   tNPrimDiatS = 0.0 ;
}
/* total flux from PRIM module to sediment Algae [gN/m2/d] */

/* ---------------------------------- */
/* Phytoplankton_processes_total */ 
/* ---------------------------------- */

oChla = oChlaDiat + oChlaGren + oChlaBlue ;
/* total chlorophyll-a [mg/m3] */
wDAssPhyt = wDAssDiat + wDAssGren + wDAssBlue ;
/* total algal growth [gDW/m3/d] */
wDRespPhytW = wDRespDiatW + wDRespGrenW + wDRespBlueW ;
/* total algal respiration in water [gD/m3/d] */
wDMortPhytW = wDMortDiatW + wDMortGrenW + wDMortBlueW ;
/* total algal mortality in water [gD/m3/d] */
tDSetPhyt = tDSetDiat + tDSetGren + tDSetBlue ;
/* total phytoplankton sedimentation [gDW/m2/d] */
wDLossPhyt = wDLossDiat + wDLossGren + wDLossBlue ;
/* total phytoplankton grazing loss [gDW/m3/d] */
wDPrimPhytW = wDPrimDiatW + wDPrimGrenW + wDPrimBlueW ;
/* total of PRIM processes of algae in water [gD/m3/d] */
wPUptPhyt = wPUptDiat + wPUptGren + wPUptBlue ;
/* total P uptake phytoplankton [gP/m3/d] */
wPExcrPhytW = wPExcrDiatW + wPExcrGrenW + wPExcrBlueW ;
/* total P excretion phytoplankton in water [gP/m3/d] */
wPMortPhytW = wPMortDiatW + wPMortGrenW + wPMortBlueW ;
/* total P mortality phytoplankton in water [gP/m3/d] */
tPSetPhyt = tPSetDiat + tPSetGren + tPSetBlue ;
/* total sedimentation of algae [gP/m2/d] */
tPResusPhyt = tPResusDiat + tPResusGren + tPResusBlue ;
/* - [gP/m2/d] */
wPLossPhyt = wPLossDiat + wPLossGren + wPLossBlue ;
/* total grazing loss [mgP/l/d] */
wPPrimPhytW = wPPrimDiatW + wPPrimGrenW + wPPrimBlueW ;
/* total of PRIM processes of algae in water [gP/m3/d] */
wNUptPhyt = wNUptDiat + wNUptGren + wNUptBlue ;
/* total N uptake phytoplankton [mgN/l/d] */
wNUptNH4Phyt = wNUptNH4Diat + wNUptNH4Gren + wNUptNH4Blue ;
/* total ammonium-N uptake phytoplankton [mgN/l/d] */
wNUptNO3Phyt = wNUptNO3Diat + wNUptNO3Gren + wNUptNO3Blue ;
/* total nitrate-N uptake phytoplankton [mgN/l/d] */
wNExcrPhytW = wNExcrDiatW + wNExcrGrenW + wNExcrBlueW ;
/* total N excretion phytoplankton in water [gN/m3/d] */
wNMortPhytW = wNMortDiatW + wNMortGrenW + wNMortBlueW ;
/* total N mortality phytoplankton in water [gN/m3/d] */
tNSetPhyt = tNSetDiat + tNSetGren + tNSetBlue ;
/* total sedimentation of algae [gN/m2/d] */
tNResusPhyt = tNResusDiat + tNResusGren + tNResusBlue ;
/* - [gN/m2/d] */
wNLossPhyt = wNLossDiat + wNLossGren + wNLossBlue ;
/* total grazing loss [mgN/l/d] */
wNPrimPhytW = wNPrimDiatW + wNPrimGrenW + wNPrimBlueW ;
/* total of PRIM processes of algae in water [gN/m3/d] */
tDRespPhytS = tDRespDiatS + tDRespGrenS + tDRespBlueS ;
/* respiration of algae on bottom [g/m2/d] */
tDMortPhytS = tDMortDiatS + tDMortGrenS + tDMortBlueS ;
/* mortality of algae on bottom [g/m2/d] */
tDPrimPhytS = tDPrimDiatS + tDPrimGrenS + tDPrimBlueS ;
/* total flux of algae on bottom [g/m2/d] */
tPExcrPhytS = tPExcrDiatS + tPExcrGrenS + tPExcrBlueS ;
/* total P excretion sediment phytoplankton [gP/m2/d] */
tPMortPhytS = tPMortDiatS + tPMortGrenS + tPMortBlueS ;
/* total phytoplankton mortality [gP/m2/d] */
tPPrimPhytS = tPPrimDiatS + tPPrimGrenS + tPPrimBlueS ;
/* total flux of algae on bottom [gP/m2/d] */
tNExcrPhytS = tNExcrDiatS + tNExcrGrenS + tNExcrBlueS ;
/* total N excretion sediment phytoplankton [gN/m2/d] */
tNMortPhytS = tNMortDiatS + tNMortGrenS + tNMortBlueS ;
/* total phytoplankton mortality [gN/m2/d] */
tNPrimPhytS = tNPrimDiatS + tNPrimGrenS + tNPrimBlueS ;
/* total flux of algae on bottom [gN/m2/d] */

/* ---------------------------------- */
/* Phytoplankton_processes */ 
/* ---------------------------------- */

wSiUptDiat = cSiDDiat * wDAssDiat ;
/* Diatoms silica uptake [mgSi/l/d] */
wSiExcrDiatW = cSiDDiat * wDRespDiatW ;
/* Si excretion [mgSi/l/d] */
wSiLossDiat = cSiDDiat * wDLossDiat ;
/* diatom grazing loss [mgSi/l/d] */
wSiMortDiatW = cSiDDiat * wDMortDiatW ;
/* Diatoms mortality in water [gSi/m3/d] */
tSiSetDiat = cSiDDiat * tDSetDiat ;
/* Diatoms sedimentation [gSi/m2/d] */
tSiResusDiat = cSiDDiat * tDResusDiat ;
/* Diatoms sedimentation [gSi/m3/d] */
wSiPrimDiatW = wSiUptDiat - tSiSetDiat / sDepthW - wSiExcrDiatW - wSiMortDiatW - wSiLossDiat + tSiResusDiat / sDepthW ;
/* total Si flux to sed diatoms in PRIM module [gSi/m2/d] */
rCyDBlue = cCyDBlueMax -(cCyDBlueMax - cCyDBlueMin) * aLLimBlue ;
/* C-phycocyanin/DW-ratio blue-greens [mg/mg] */
oCyan = rCyDBlue * sDBlueW * mgPerg ;
/* C-phycocyanin [mg/m3] */
fDDiat = sDDiatW /(sDDiatW + sDGrenW + sDBlueW + NearZero) ;
/* DW fraction of algal group of total algae [-] */

/* ---------------------------------- */
/* total_fluxes_to_detritus_and_dissolved_nutrients_in_'prim'_module */ 
/* ---------------------------------- */

wDPrimDetW = wDMortPhytW + wDLossPhyt ;
/* Flux to water detritus [mg/l/d] */
if (InclPhytS == 1) {
   tDPrimDetS = tDMortPhytS ;
} else {
   tDPrimDetS = tDSetPhyt ;
}
/* Flux to sediment detritus [mg/l/d] */
tDPrimTotT = (wDAssPhyt - wDRespPhytW) * sDepthW - tDRespPhytS ;
/* total DW flux [gD/m2/d] */
wO2ProdPhyt = molO2molC * cCPerDW * wDAssPhyt ;
/* O2 production by phytoplankton [gO2/m3/d] */
wO2RespPhytW = molO2molC * cCPerDW * wDRespPhytW * aCorO2BOD ;
/* O2 production by phytoplankton [gO2/m3/d] */
wO2UptNO3Phyt = O2PerNO3 * molO2molN * wNUptNO3Phyt ;
/* O2 production due to NO3 uptake by phytopl [gO2/m3/d] */
wO2PrimW = wO2ProdPhyt - wO2RespPhytW + wO2UptNO3Phyt ;
/* O2 flux by water algae [gO2/m3/d] */
tO2RespPhytS = molO2molC * cCPerDW * tDRespPhytS * afOxySed ;
/* O2 respiration by sediment algae [gO2/m2/d] */
tO2PrimS = tO2RespPhytS ;
/* O2 flux by sediment algae [gO2/m2/d] */
wPMortPhytPO4W = fDissMortPhyt * wPMortPhytW ;
/* soluble P flux from died Algae [mgP/l/d] */
wPMortPhytDetW = wPMortPhytW - wPMortPhytPO4W ;
/* detrital P flux from died Algae [mgP/l/d] */
wPLossPhytPO4 = fDissLoss * wPLossPhyt ;
/* soluble P grazing loss [mgP/l/d] */
wPLossPhytDet = wPLossPhyt - wPLossPhytPO4 ;
/* detrital P grazing loss [mgP/l/d] */
wPPrimPO4W = - wPUptPhyt + wPExcrPhytW + wPLossPhytPO4 + wPMortPhytPO4W ;
/* SRP in water [mgP/l/d] */
wPPrimDetW = wPLossPhytDet + wPMortPhytDetW ;
/* Detritus in water [mgP/l/d] */
tPMortPhytPO4S = fDissMortPhyt * tPMortPhytS ;
/* soluble P flux from died Algae [gP/m2/d] */
tPMortPhytDetS = tPMortPhytS - tPMortPhytPO4S ;
/* detrital P flux from died Algae [gP/m2/d] */
if (InclPhytS == 1) {
   tPPrimDetS = tPMortPhytDetS ;
} else {
   tPPrimDetS = tPSetPhyt ;
}
/* Sediment detritus [gP/m2/d] */
tPPrimPO4S = tPExcrPhytS + tPMortPhytPO4S ;
/* Pore water P [gP/m2/d] */
tPPrimTotT = 0.0 ;
/* total P flux [gP/m2/d] */
wNMortPhytNH4W = fDissMortPhyt * wNMortPhytW ;
/* ammonium flux from died Algae [mgN/l/d] */
wNMortPhytDetW = wNMortPhytW - wNMortPhytNH4W ;
/* detrital N flux from died Algae [mgN/l/d] */
wNLossPhytNH4 = fDissLoss * wNLossPhyt ;
/* NH4-N grazing loss [mgN/l/d] */
wNLossPhytDet = wNLossPhyt - wNLossPhytNH4 ;
/* detrital N grazing loss [mgN/l/d] */
wNPrimNH4W = - wNUptNH4Phyt + wNExcrPhytW + wNLossPhytNH4 + wNMortPhytNH4W ;
/* ammonium in water [mgN/l/d] */
wNPrimNO3W = - wNUptNO3Phyt ;
/* nitrate in water [mgN/l/d] */
wNPrimDetW = wNLossPhytDet + wNMortPhytDetW ;
/* Detritus in water [mgN/l/d] */
tNMortPhytNH4S = fDissMortPhyt * tNMortPhytS ;
/* ammonium flux from died Algae [gN/m2/d] */
tNMortPhytDetS = tNMortPhytS - tNMortPhytNH4S ;
/* detrital N flux from died Algae [gN/m2/d] */
tNPrimNH4S = tNExcrPhytS + tNMortPhytNH4S ;
/* Pore water ammonium [gN/m2/d] */
tNPrimNO3S = 0.0 ;
/* Pore water nitrate [gN/m2/d] */
if (InclPhytS == 1) {
   tNPrimDetS = tNMortPhytDetS ;
} else {
   tNPrimDetS = tNSetPhyt ;
}
/* Sediment detritus [gN/m2/d] */
tNPrimTotT = 0.0 ;
/* total N flux [gN/m2/d] */
tSiExcrDiatS = cSiDDiat * tDRespDiatS ;
/* Si excretion of bottom Algae [gSi/m2/d] */
tSiMortDiatS = cSiDDiat * tDMortDiatS ;
/* mortality of bottom Algae [gSi/m2/d] */
wSiPrimSiO2W = wSiExcrDiatW - wSiUptDiat + tSiExcrDiatS / sDepthW ;
/* total Si flux to SiO2 in PRIM module [mgSi/l/d] */
wSiPrimDetW = wSiMortDiatW + wSiLossDiat ;
/* total Si flux to sed detritus in PRIM module [mgSi/l/d] */
if (InclPhytS == 1) {
   tSiPrimDiatS = tSiSetDiat - tSiResusDiat - tSiExcrDiatS - tSiMortDiatS ;
} else {
   tSiPrimDiatS = 0.0 ;
}
/* total Si flux to sed diatoms in PRIM module [gSi/m2/d] */
if (InclPhytS == 1) {
   tSiPrimDetS = tSiMortDiatS ;
} else {
   tSiPrimDetS = tSiSetDiat ;
}
/* Sediment detritus [gSi/m2/d] */
tSiPrimTotT = 0.0 ;
/* total Si flux [gSi/m2/d] */

/* ---------------------------------- */
/* extra_ouput_variables_of_phytoplankton */ 
/* ---------------------------------- */

aPACoef = cPACoefMin +(cPACoefMax - cPACoefMin) * hPACoef / (hPACoef + oDOMW) ;
/* Poole-Atkins coefficient [-] */
bSecchiMax = sDepthW + cSecchiPlus ;
/* max Secchi depth [m] */
aSecchi = min(bSecchiMax,aPACoef / aExtCoefOpen) ;
/* Secchi depth [m] */
aTransparency = aSecchi / sDepthW ;
/* Secchi depth [m] */
aDepthEuph = cEuph * aSecchi ;
/* euphotic depth [m] */
aRelDepthEuph = aDepthEuph / sDepthW ;
/* relative euphotic depth [m] */
aChlaH = oChla * sDepthW ;
/* Chla per m2 [mg_chlorophyll-a/m2] */
aCovPhytW = cCovSpPhyt *(oDPhytW * sDepthW) ;
/* % cover with algae [%] */
rExtChPhyt = aExtPhyt /(oChla / mgPerg + NearZero) ;
/* average spec extinction of algae per unit chl-a [m2/g_chl-a] */

/* ---------------------------------- */
/* Zooplankton_processes */ 
/* ---------------------------------- */

uFunTmZoo = exp(-0.5/(cSigTmZoo*cSigTmZoo) *((uTm-cTmOptZoo)*(uTm-cTmOptZoo) -(cTmRef-cTmOptZoo)*(cTmRef-cTmOptZoo))) ;
/* temp function of zooplankton [-] */
rPDZoo = sPZoo /(sDZoo+NearZero) ;
/* P/D ratio herbzooplankton [mgP/mgDW] */
rNDZoo = sNZoo/(sDZoo+NearZero) ;
/* N/C ratio herbzooplankton [mgN/mgDW] */
oDFoodZoo = cPrefDiat * sDDiatW + cPrefGren * sDGrenW + cPrefBlue * sDBlueW + cPrefDet * sDDetW ;
/* food for zooplankton [mgD/l] */
aFilt = cFiltMax * uFunTmZoo * hFilt /(hFilt + oDOMW) ;
/* filtering rate [ltr/mgDW/d] */
ukDAssTmZoo = fDAssZoo * cFiltMax * uFunTmZoo * hFilt ;
/* max assimilation rate of zooplanktontemp corrected [d-1] */
aDSatZoo = oDFoodZoo /(hFilt + oDOMW) ;
/* food saturation function of zooplankton [-] */
ukDRespTmZoo = kDRespZoo * uFunTmZoo ;
/* respiration constant of zooplankton [d-1] */
ukDIncrZoo = ukDAssTmZoo - ukDRespTmZoo - kMortZoo ;
/* intrinsic rate of increase of zooplankton [d-1] */
wDEnvZoo = max(0.0,ukDIncrZoo / cDCarrZoo * sDZoo*sDZoo) ;
/* environmental correction of zooplankton [mg/l/d] */
wDAssZoo = aDSatZoo *(ukDAssTmZoo * sDZoo - wDEnvZoo) ;
/* assimilation of zooplankton [mg/l/d] */
wDConsZoo = wDAssZoo / fDAssZoo ;
/* consumption of zooplankton [mg/l/d] */
wDConsDetZoo = cPrefDet*sDDetW / oDFoodZoo * wDConsZoo ;
/* DW detritus consumption by zooplankton [mg/l/d] */
wDConsDiatZoo = cPrefDiat*sDDiatW / oDFoodZoo * wDConsZoo ;
/* DW diatoms consumption by zooplankton [mg/l/d] */
wDConsGrenZoo = cPrefGren*sDGrenW / oDFoodZoo * wDConsZoo ;
/* DW greens consumption by zooplankton [mg/l/d] */
wDConsBlueZoo = cPrefBlue*sDBlueW / oDFoodZoo * wDConsZoo ;
/* DW blue-greens consumption by zooplankton [mg/l/d] */
wDConsPhytZoo = wDConsDiatZoo + wDConsGrenZoo + wDConsBlueZoo ;
/* phytoplankton consumption by zooplankton [mg/l/d] */
wDEgesZoo = wDConsZoo - wDAssZoo ;
/* egestion of zooplankton [mg/l/d] */
aCorDRespZoo = max(cPDZooRef / rPDZoo,cNDZooRef / rNDZoo) ;
/* corr factor of zoopl respiration for P and N content [-] */
wDRespZoo = aCorDRespZoo * ukDRespTmZoo * sDZoo ;
/* zoopl respiration [mg/l/d] */
wDMortZoo = kMortZoo * sDZoo +(1.0 - aDSatZoo) * wDEnvZoo ;
/* zoopl mortalityincl environmental correction [mg/l/d] */
oPFoodZoo = cPrefDiat*sPDiatW + cPrefGren*sPGrenW + cPrefBlue*sPBlueW + cPrefDet * sPDetW ;
/* Zooplankton food [mgP/l] */
rPDFoodZoo = oPFoodZoo /(oDFoodZoo+NearZero) ;
/* P/D ratio of zooplankton food [mgP/mgDW] */
wPConsDiatZoo = rPDDiatW * wDConsDiatZoo ;
/* P diatom consumption by zoopl [mgP/l/d] */
wPConsGrenZoo = rPDGrenW * wDConsGrenZoo ;
/* P green consumption by zoopl [mgP/l/d] */
wPConsBlueZoo = rPDBlueW * wDConsBlueZoo ;
/* P bluegreen consumption by zoopl [mgP/l/d] */
wPConsPhytZoo = wPConsDiatZoo + wPConsGrenZoo + wPConsBlueZoo ;
/* total P phytoplankton consumption by zoopl [mgP/l/d] */
wPConsDetZoo = rPDDetW * wDConsDetZoo ;
/* consumption of detrital P [mgP/l/d] */
wPConsZoo = wPConsPhytZoo + wPConsDetZoo ;
/* total P consumption [mgP/l/d] */
afPAssZoo = min(1.0,cPDZooRef / rPDFoodZoo * fDAssZoo) ;
/* P assimilation efficiency of herbivores [-] */
wPAssZoo = afPAssZoo * wPConsZoo ;
/* assimilation by herbivores [mgP/l/d] */
wPEgesZoo = wPConsZoo - wPAssZoo ;
/* P egestion [mgP/l/d] */
wPEgesZooPO4 = fDissEgesZoo*wPEgesZoo ;
/* soluble P egestion [mgP/l/d] */
wPEgesZooDet = wPEgesZoo - wPEgesZooPO4 ;
/* detrital P egestion [mgP/l/d] */
akPExcrZoo = rPDZoo / cPDZooRef * kDRespZoo * uFunTmZoo ;
/* P excretion rate of herbivores [d-1] */
wPExcrZoo = akPExcrZoo*sPZoo ;
/* P excretion [mgP/l/d] */
wPMortZoo = rPDZoo * wDMortZoo ;
/* mortality [mgP/l/d] */
wPMortZooPO4 = fDissMortZoo * wPMortZoo ;
/* soluble P mortality [mgP/l/d] */
wPMortZooDet = wPMortZoo - wPMortZooPO4 ;
/* detrital P mortality [mgP/l/d] */
oNFoodZoo = cPrefDiat*sNDiatW + cPrefGren*sNGrenW + cPrefBlue*sNBlueW + cPrefDet*sNDetW ;
/* Zooplankton food [mgN/l] */
rNDFoodZoo = oNFoodZoo /(oDFoodZoo+NearZero) ;
/* N/C ratio of zooplankton food [mgN/mgDW] */
wNConsDiatZoo = rNDDiatW*wDConsDiatZoo ;
/* N diatom consumption by zoopl [mgN/l/d] */
wNConsGrenZoo = rNDGrenW*wDConsGrenZoo ;
/* N green consumption by zoopl [mgN/l/d] */
wNConsBlueZoo = rNDBlueW*wDConsBlueZoo ;
/* N bluegreen consumption by zoopl [mgN/l/d] */
wNConsPhytZoo = wNConsDiatZoo + wNConsGrenZoo + wNConsBlueZoo ;
/* total N phytoplankton consumption by zoopl [mgN/l/d] */
wNConsDetZoo = rNDDetW*wDConsDetZoo ;
/* consumption of detrital N [mgN/l/d] */
wNConsZoo = wNConsPhytZoo + wNConsDetZoo ;
/* total N consumption [mgN/l/d] */
afNAssZoo = min(1.0,cNDZooRef / rNDFoodZoo * fDAssZoo) ;
/* N assimilation efficiency of herbivores [-] */
wNAssZoo = afNAssZoo*wNConsZoo ;
/* assimilation by herbivores [mgN/l/d] */
wNEgesZoo = wNConsZoo - wNAssZoo ;
/* N egestion [mgN/l/d] */
wNEgesZooNH4 = fDissEgesZoo*wNEgesZoo ;
/* soluble N egestion [mgN/l/d] */
wNEgesZooDet = wNEgesZoo - wNEgesZooNH4 ;
/* detrital N egestion [mgN/l/d] */
kNExcrZoo = rNDZoo / cNDZooRef * kDRespZoo * uFunTmZoo ;
/* N excretion rate of herbivores [d-1] */
wNExcrZoo = kNExcrZoo*sNZoo ;
/* N excretion [mgN/l/d] */
wNMortZoo = rNDZoo*wDMortZoo ;
/* mortality [mgN/l/d] */
wNMortZooNH4 = fDissMortZoo*wNMortZoo ;
/* soluble N mortality [mgN/l/d] */
wNMortZooDet = wNMortZoo - wNMortZooNH4 ;
/* detrital N mortality [mgN/l/d] */
wSiConsDiatZoo = cSiDDiat * wDConsDiatZoo ;
/* consumption of diatoms [mgSi/l/d] */

/* ---------------------------------- */
/* Zoobenthal_processes */ 
/* ---------------------------------- */

uFunTmBent = exp(-0.5/(cSigTmBent*cSigTmBent) *((uTm-cTmOptBent)*(uTm-cTmOptBent) - (cTmRef-cTmOptBent)*(cTmRef-cTmOptBent))) ;
/* temp function of zoobenthos [-] */
aDFoodBent = sDDetS + aDPhytS ;
/* food for zoobenthos [gD/m2] */
rPDBent = sPBent /(sDBent+NearZero) ;
/* P/D ratio of zoobenthos [gP/gD] */
rNDBent = sNBent /(sDBent+NearZero) ;
/* N/D ratio of zoobenthos [gN/g] */
tDMigrBent = kMigrBent *(cDBentIn - sDBent) ;
/* migration flux [gD/m2/d] */
aDSatBent = aDFoodBent /(hDFoodBent + aDFoodBent) ;
/* food limitation function of zoobenthos [-] */
ukDIncrBent = (kDAssBent - kDRespBent) * uFunTmBent - kMortBent ;
/* intrinsic net increase rate of zoobenthos [d-1] */
tDEnvBent = max(0.0,ukDIncrBent / cDCarrBent * sDBent*sDBent) ;
/* environmental correction of zoobenthos [gD/m2/d] */
tDAssBent = aDSatBent *(kDAssBent * uFunTmBent * sDBent - tDEnvBent) ;
/* assimilation of zoobenthos [gD/m2/d] */
aDAssBentSp = tDAssBent / sDBent ;
/* specific assimilation rate of zoobenthos [d-1] */
tDConsBent = tDAssBent / fDAssBent ;
/* consumption of zoobenthos [gD/m2/d] */
tDConsDetBent = sDDetS / aDFoodBent * tDConsBent ;
/* detritus consumption by zoobenthos [gD/m2/d] */
tDConsDiatBent = sDDiatS / aDFoodBent * tDConsBent ;
/* diatoms consumption by zoobenthos [gD/m2/d] */
tDConsGrenBent = sDGrenS / aDFoodBent * tDConsBent ;
/* greens consumption by zoobenthos [gD/m2/d] */
tDConsBlueBent = sDBlueS / aDFoodBent * tDConsBent ;
/* blue-greens consumption by zoobenthos [gD/m2/d] */
tDConsPhytBent = tDConsDiatBent + tDConsGrenBent + tDConsBlueBent ;
/* phutoplankton consumption by zoobenthos [gD/m2/d] */
tDEgesBent = tDConsBent - tDAssBent ;
/* egestion of zoobenthos [gD/m2/d] */
tDRespBent = (cPDBentRef / rPDBent) * kDRespBent * uFunTmBent * sDBent ;
/* respiration of zoobenthos [gD/m2/d] */
tDMortBent = kMortBent*sDBent +(1.0 - aDSatBent) * tDEnvBent ;
/* zoobenthos mortality incl environmental correction [gD/m2/d] */
aPFoodBent = sPDetS + aPPhytS ;
/* food for zoobenthos [gP/m2] */
rPDFoodBent = aPFoodBent /(aDFoodBent+NearZero) ;
/* average P/D ratio of zoobenthos food [gP/gD] */
tPConsDetBent = rPDDetS * tDConsDetBent ;
/* detrital P consumption by zoobenthos [gP/m2/d] */
tPConsDiatBent = rPDDiatS * tDConsDiatBent ;
/* diatom P consumption by zoobenthos [gP/m2/d] */
tPConsGrenBent = rPDGrenS * tDConsGrenBent ;
/* greens P consumption by zoobenthos [gP/m2/d] */
tPConsBlueBent = rPDBlueS * tDConsBlueBent ;
/* blue-greens P consumption by zoobenthos [gP/m2/d] */
tPConsPhytBent = tPConsDiatBent + tPConsGrenBent + tPConsBlueBent ;
/* phytoplankton P consumption by zoobenthos [gP/m2/d] */
tPConsBent = tPConsDetBent + tPConsPhytBent ;
/* total P consumption of zoobenthos [gP/m2/d] */
afPAssBent = min(1.0,cPDBentRef / rPDFoodBent * fDAssBent) ;
/* P assim efficiency of zoobenthos [-] */
tPAssBent = afPAssBent * tPConsBent ;
/* P assimilation of zoobenthos [gP/m2/d] */
tPEgesBent = tPConsBent - tPAssBent ;
/* egestion of zoobenthos [gP/m2/d] */
tPEgesBentPO4 = fDissEgesBent * tPEgesBent ;
/* SRP egestion of zoobenthos [gP/m2/d] */
tPEgesBentDet = (1.0 - fDissEgesBent) * tPEgesBent ;
/* detrital P egestion of zoobenthos [gP/m2/d] */
tPExcrBent = (rPDBent / cPDBentRef) * kDRespBent * uFunTmBent * sPBent ;
/* P excretion of zoobenthos [d-1] */
tPMortBent = rPDBent * tDMortBent ;
/* mortality of zoobenthos [gP/m2/d] */
tPMortBentPO4 = fDissMortBent * tPMortBent ;
/* part of died zoobenthos P becoming dissolved P [gP/m2/d] */
tPMortBentDet = (1.0-fDissMortBent)*tPMortBent ;
/* part of died zoobenthos P becoming detrital P [gP/m2/d] */
tPMigrBent = kMigrBent *(cPDBentRef*cDBentIn - sPBent) ;
/* net migration flux [gP/m2/d] */
aNFoodBent = sNDetS + aNPhytS ;
/* food for zoobenthos [gN/m2] */
rNDFoodBent = aNFoodBent /(aDFoodBent+NearZero) ;
/* average N/D ratio of zoobenthos food [gN/gD] */
tNMigrBent = kMigrBent *(cNDBentRef*cDBentIn - sNBent) ;
/* Net migration flux [gN/m2/d] */
tNConsDetBent = rNDDetS * tDConsDetBent ;
/* detrital N consumption by zoobenthos [gN/m2/d] */
tNConsDiatBent = rNDDiatS * tDConsDiatBent ;
/* diatom N consumption by zoobenthos [gN/m2/d] */
tNConsGrenBent = rNDGrenS * tDConsGrenBent ;
/* greens N consumption by zoobenthos [gN/m2/d] */
tNConsBlueBent = rNDBlueS * tDConsBlueBent ;
/* blue-greens N consumption by zoobenthos [gN/m2/d] */
tNConsPhytBent = tNConsDiatBent + tNConsGrenBent + tNConsBlueBent ;
/* phytoplankton N consumption by zoobenthos [gN/m2/d] */
tNConsBent = tNConsDetBent + tNConsPhytBent ;
/* total N consumption of zoobenthos [gN/m2/d] */
afNAssBent = min(1.0,cNDBentRef / rNDFoodBent * fDAssBent) ;
/* N assim efficiency of zoobenthos [-] */
tNAssBent = afNAssBent * tNConsBent ;
/* N assimilation of zoobenthos [gN/m2/d] */
tNEgesBent = tNConsBent - tNAssBent ;
/* egestion of zoobenthos [gN/m2/d] */
tNEgesBentNH4 = fDissEgesBent * tNEgesBent ;
/* NH4 egestion of zoobenthos [gN/m2/d] */
tNEgesBentDet = (1.0 - fDissEgesBent) * tNEgesBent ;
/* detrital N egestion of zoobenthos [gN/m2/d] */
tNExcrBent = (rNDBent / cNDBentRef) * kDRespBent * uFunTmBent * sNBent ;
/* N excretion of zoobenthos [d-1] */
tNMortBent = rNDBent * tDMortBent ;
/* mortality of zoobenthos [gN/m2/d] */
tNMortBentNH4 = fDissMortBent*tNMortBent ;
/* part of died zoobenthos N becoming ammonium-N [gN/m2/d] */
tNMortBentDet = (1.0-fDissMortBent)*tNMortBent ;
/* part of died zoobenthos N becoming detrital N [gN/m2/d] */
tSiConsDiatBent = cSiDDiat * tDConsDiatBent ;
/* diatom consumption by zoobenthos [gSi/m2/d] */

/* ---------------------------------- */
/* Bentfish_processes */ 
/* ---------------------------------- */

aDFish = sDFiJv + sDFiAd ;
/* total fish biomass [gD_m-2] */
aPFish = sPFiJv + sPFiAd ;
/* total fish biomass [gP_m-2] */
aNFish = sNFiJv + sNFiAd ;
/* total fish biomass [gN_m-2] */
rPDFiJv = sPFiJv /(sDFiJv+NearZero) ;
/* P/D ratio of young fish [gP/gD] */
rPDFiAd = sPFiAd /(sDFiAd+NearZero) ;
/* P/D ratio of adult fish [gP/gD] */
rNDFiJv = sNFiJv /(sDFiJv+NearZero) ;
/* N/D ratio of young fish [gN/gD] */
rNDFiAd = sNFiAd /(sDFiAd+NearZero) ;
/* N/D ratio of adult fish [gN/gD] */
if (0 == InclSeason) {
   tDReprFish = ((1/DaysPerYear)*fReprFish) * sDFiAd ;
} else if (Day >= cDayReprFish && Day < cDayReprFish + 1.0) {
   tDReprFish = fReprFish * sDFiAd ;
} else {
   tDReprFish = 0.0 ;
}
/* Reproduction flux [gD_m-2_d-1] */
if (0 == InclSeason) {
   tDAgeFish = ((1/DaysPerYear)*fAgeFish) * sDFiJv ;
} else if (Day >= 364.0) {
   tDAgeFish = fAgeFish * sDFiJv ;
} else {
   tDAgeFish = 0.0 ;
}
/* Ageing [gD_m-2_d-1] */
aFunVegFish = max(0.0,1.0 - cRelVegFish * aCovVeg) ;
/* vegetation dependence of fish feeding [-] */

/* ---------------------------------- */
/* Bentfish_processes_in_juvinile_fish */ 
/* ---------------------------------- */

aDSatFiJv = (sDZoo * sDepthW) *(sDZoo * sDepthW) /(hDZooFiJv * hDZooFiJv + (sDZoo * sDepthW) *(sDZoo * sDepthW)) ;
/* food limitation function of young fish [-] */
ukDIncrFiJv = (kDAssFiJv - kDRespFiJv) * uFunTmFish - kMortFiJv ;
/* intrinsic net increase rate of fish [d-1] */
tDEnvFiJv = max(0.0,ukDIncrFiJv /(cDCarrFish - sDFiAd) * sDFiJv*sDFiJv) ;
/* environmental correction of fish [gD/m2/d] */
tDAssFiJv = aDSatFiJv *(kDAssFiJv * uFunTmFish * sDFiJv - tDEnvFiJv) ;
/* assimilation of fish [gD/m2/d] */
tDConsFiJv = tDAssFiJv / fDAssFiJv ;
/* zooplankton consumption of fish [gD/m2/d] */
tDEgesFiJv = tDConsFiJv - tDAssFiJv ;
/* egestion of fish [gD/m2/d] */
tDRespFiJv = (cPDFishRef / rPDFiJv) * kDRespFiJv * uFunTmFish * sDFiJv ;
/* respiration of fish [gD/m2/d] */
tDMortFiJv = kMortFiJv * sDFiJv +(1.0 - aDSatFiJv) * tDEnvFiJv ;
/* fish mortality incl environmental correction [gD/m2/d] */
tDMigrFiJv = kMigrFish *(cDFiJvIn - sDFiJv) ;
/* migration flux [gD/m2/d] */

/* ---------------------------------- */
/* Bentfish_processes_in_adult_fish */ 
/* ---------------------------------- */

aDSatFiAd = (aFunVegFish * sDBent) *(aFunVegFish * sDBent) /(hDBentFiAd * hDBentFiAd + (aFunVegFish * sDBent) *(aFunVegFish * sDBent)) ;
/* food limitation function of adult fish [-] */
ukDIncrFiAd = (kDAssFiAd - kDRespFiAd) * uFunTmFish - kMortFiAd ;
/* intrinsic net increase rate of fish [d-1] */
tDEnvFiAd = max(0.0,ukDIncrFiAd /(cDCarrFish - sDFiJv) * sDFiAd*sDFiAd) ;
/* environmental correction of fish [gD/m2/d] */
tDAssFiAd = aDSatFiAd *(kDAssFiAd * uFunTmFish * sDFiAd - tDEnvFiAd) ;
/* assimilation of fish [gD/m2/d] */
tDConsFiAd = tDAssFiAd / fDAssFiAd ;
/* zoobenthos consumption of fish [gD/m2/d] */
tDEgesFiAd = tDConsFiAd - tDAssFiAd ;
/* egestion of fish [gD/m2/d] */
tDRespFiAd = (cPDFishRef / rPDFiAd) * kDRespFiAd * uFunTmFish * sDFiAd ;
/* respiration of fish [gD/m2/d] */
tDMortFiAd = kMortFiAd * sDFiAd +(1.0 - aDSatFiAd) * tDEnvFiAd ;
/* fish mortality incl environmental correction [gD/m2/d] */
if (cos(2.0 * Pi * sTime / DaysPerYear) > 0.1) {
   ukHarvFish = kHarvFishWin ;
} else {
   ukHarvFish = kHarvFishSum ;
}
/* fish harvesting constant [d-1] */
tDHarvFish = ukHarvFish * sDFiAd ;
/* harvesting of fish [gD/m2/d] */
tDMigrFiAd = kMigrFish *(cDFiAdIn - sDFiAd) ;
/* migration flux [gD/m2/d] */

/* ---------------------------------- */
/* Total_fish_loss_fluxes */ 
/* ---------------------------------- */

tDMortFish = tDMortFiJv + tDMortFiAd ;
/* bent fish mortality [gD/m2/d] */
tDMortFishBot = fDBone * tDMortFish ;
/* part of died fish DW fixed in bones and scales [g/m2/d] */
tDMortFishDet = tDMortFish - tDMortFishBot ;
/* part of died fish DW becoming detritus [gD/m2/d] */

/* ---------------------------------- */
/* Fish_processes */ 
/* ---------------------------------- */

tPReprFish = rPDFiAd * tDReprFish ;
/* Reproduction flux [gP_m-2_d-1] */
tPAgeFish = rPDFiJv * tDAgeFish ;
/* Ageing [gD_m-2_d-1] */

/* ---------------------------------- */
/* Fish_processes_of_young_fish */ 
/* ---------------------------------- */

tPMigrFiJv = kMigrFish *(cPDFishRef * cDFiJvIn - sPFiJv) ;
/* net migration flux [gP/m2/d] */
tPConsFiJv = rPDZoo * tDConsFiJv ;
/* (zooplankton) P consumption by FiJv [gP/m2/d] */
afPAssFiJv = min(1.0,cPDFishRef / rPDZoo * fDAssFiJv) ;
/* P assim efficiency of FiJv [-] */
tPAssFiJv = afPAssFiJv * tPConsFiJv ;
/* P assimilation of FiJv [gP/m2/d] */
tPEgesFiJv = tPConsFiJv - tPAssFiJv ;
/* egestion of FiJv [gP/m2/d] */
tPExcrFiJv = (rPDFiJv / cPDFishRef) * kDRespFiJv * uFunTmFish * sPFiJv ;
/* P excretion of FiJv [d-1] */
tPMortFiJv = rPDFiJv * tDMortFiJv ;
/* mortality of FiJv [gP/m2/d] */

/* ---------------------------------- */
/* Fish_processes_of_adult_fish */ 
/* ---------------------------------- */

tPMigrFiAd = kMigrFish *(cPDFishRef * cDFiAdIn - sPFiAd) ;
/* net migration flux [gP/m2/d] */
tPConsFiAd = rPDBent * tDConsFiAd ;
/* (zoobenthos) P consumption by FiAd [gP/m2/d] */
afPAssFiAd = min(1.0,cPDFishRef / rPDBent * fDAssFiAd) ;
/* P assim efficiency of FiAd [-] */
tPAssFiAd = afPAssFiAd * tPConsFiAd ;
/* P assimilation of FiAd [gP/m2/d] */
tPEgesFiAd = tPConsFiAd - tPAssFiAd ;
/* egestion of FiAd [gP/m2/d] */
tPExcrFiAd = (rPDFiAd / cPDFishRef) * kDRespFiAd * uFunTmFish * sPFiAd ;
/* P excretion of FiAd [d-1] */
tPMortFiAd = rPDFiAd * tDMortFiAd ;
/* mortality of FiAd [gP/m2/d] */
tPHarvFish = rPDFiAd * tDHarvFish ;
/* harvesting of FiAd [gP/m2/d] */

/* ---------------------------------- */
/* Total_fish_loss_fluxes */ 
/* ---------------------------------- */

tPMortFish = tPMortFiJv + tPMortFiAd ;
/* - [gP/m2/d] */
tPMortFishBot = fPBone * tPMortFish ;
/* part of died fish P fixed in bones AND scales [gP/m2/d] */
tPMortFishPO4 = fDissMortFish *(tPMortFish - tPMortFishBot) ;
/* part of died fish P becoming dissolved P [gP/m2/d] */
tPMortFishDet = tPMortFish - tPMortFishBot - tPMortFishPO4 ;
/* part of died fish PW becoming detritus [gP/m2/d] */
tPEgesFish = tPEgesFiJv + tPEgesFiAd ;
/* total fish egestion [gP_m-2_d-1] */
tPEgesFishPO4 = fDissEgesFish * tPEgesFish ;
/* SRP egestion of fish [gP/m2/d] */
tPEgesFishDet = tPEgesFish - tPEgesFishPO4 ;
/* detrital P egestion of fish [gP/m2/d] */

/* ---------------------------------- */
/* Fish_processes */ 
/* ---------------------------------- */

tNReprFish = rNDFiAd * tDReprFish ;
/* Reproduction flux [gN_m-2_d-1] */
tNAgeFish = rNDFiJv * tDAgeFish ;
/* Ageing [gD_m-2_d-1] */

/* ---------------------------------- */
/* Fish_processes_of_young_fish */ 
/* ---------------------------------- */

tNMigrFiJv = kMigrFish *(cNDFishRef * cDFiJvIn - sNFiJv) ;
/* net migration flux [gN/m2/d] */
tNConsFiJv = rNDZoo * tDConsFiJv ;
/* (zooplankton) N consumption by FiJv [gN/m2/d] */
afNAssFiJv = min(1.0,cNDFishRef / rNDZoo * fDAssFiJv) ;
/* N assim efficiency of FiJv [-] */
tNAssFiJv = afNAssFiJv * tNConsFiJv ;
/* N assimilation of FiJv [gN/m2/d] */
tNEgesFiJv = tNConsFiJv - tNAssFiJv ;
/* egestion of FiJv [gN/m2/d] */
tNExcrFiJv = (rNDFiJv / cNDFishRef) * kDRespFiJv * uFunTmFish * sNFiJv ;
/* N excretion of FiJv [d-1] */
tNMortFiJv = rNDFiJv * tDMortFiJv ;
/* mortality of FiJv [gN/m2/d] */

/* ---------------------------------- */
/* Fish_processes_of_adult_fish */ 
/* ---------------------------------- */

tNMigrFiAd = kMigrFish *(cNDFishRef * cDFiAdIn - sNFiAd) ;
/* net migration flux [gN/m2/d] */
tNConsFiAd = rNDBent * tDConsFiAd ;
/* (zoobenthos) N consumption by FiAd [gN/m2/d] */
afNAssFiAd = min(1.0,cNDFishRef / rNDBent * fDAssFiAd) ;
/* N assim efficiency of FiAd [-] */
tNAssFiAd = afNAssFiAd * tNConsFiAd ;
/* N assimilation of FiAd [gN/m2/d] */
tNEgesFiAd = tNConsFiAd - tNAssFiAd ;
/* egestion of FiAd [gN/m2/d] */
tNExcrFiAd = (rNDFiAd / cNDFishRef) * kDRespFiAd * uFunTmFish * sNFiAd ;
/* N excretion of FiAd [d-1] */
tNMortFiAd = rNDFiAd * tDMortFiAd ;
/* mortality of FiAd [gN/m2/d] */
tNHarvFish = rNDFiAd * tDHarvFish ;
/* harvesting of FiAd [gN/m2/d] */

/* ---------------------------------- */
/* Total_fish_loss_fluxes */ 
/* ---------------------------------- */

tNMortFish = tNMortFiJv + tNMortFiAd ;
/* - [gN/m2/d] */
tNMortFishBot = fDBone * tNMortFish ;
/* part of died fish N fixed in bones AND scales [gN/m2/d] */
tNMortFishNH4 = fDissMortFish *(tNMortFish - tNMortFishBot) ;
/* part of died fish N becoming dissolved N [gN/m2/d] */
tNMortFishDet = tNMortFish - tNMortFishBot - tNMortFishNH4 ;
/* part of died fish NW becoming detritus [gN/m2/d] */
tNEgesFish = tNEgesFiJv + tNEgesFiAd ;
/* total fish egestion [gN_m-2_d-1] */
tNEgesFishNH4 = fDissEgesFish * tNEgesFish ;
/* NH4 egestion of fish [gN/m2/d] */
tNEgesFishDet = tNEgesFish - tNEgesFishNH4 ;
/* detrital xN egestion of fish [gxN/m2/d] */

/* ---------------------------------- */
/* Pisc_fish_processes */ 
/* ---------------------------------- */

uFunTmPisc = exp(-0.5 /(cSigTmPisc*cSigTmPisc) *((uTm - cTmOptPisc)*(uTm - cTmOptPisc) - (cTmRef - cTmOptPisc)*(cTmRef - cTmOptPisc))) ;
/* temp function of Pisc [-] */
tDMigrPisc = kMigrPisc *(cDPiscIn - sDPisc) ;
/* migration flux [gD/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,cDCarrPiscBare)) ;
} else if (sDShootPhra < cDPhraMinPisc) {
   aDCarrPisc = cDCarrPiscMin ;
} else if (aCovVeg < cCovVegMin) {
   aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,fMarsh *(1.0/PerCent) * cRelPhraPisc)) ;
} else {
   aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,fMarsh *(1.0/PerCent) *(cRelPhraPisc + cRelVegPisc))) ;
}
/* Carrying capacity of Pisc for lake without OR with marsh zone resp [gD.m-2] */
aFunVegPisc = aDSubVeg /(hDVegPisc + aDSubVeg + NearZero) ;
/* vegetation dependence of Pisc growth rate [-] */
aDSatPisc = aDFish*aDFish /(hDFishPisc*hDFishPisc + aDFish*aDFish) ;
/* food limitation function of Pisc [-] */
akDIncrPisc = (kDAssPisc * aFunVegPisc - kDRespPisc) * uFunTmPisc - kMortPisc ;
/* intrinsic net increase rate of Pisc [d-1] */
tDEnvPisc = max(0.0,akDIncrPisc / aDCarrPisc * sDPisc*sDPisc) ;
/* environmental correction of Pisc [gD/m2/d] */
tDAssPisc = aDSatPisc *(kDAssPisc * aFunVegPisc * uFunTmPisc * sDPisc - tDEnvPisc) ;
/* assimilation of Pisc [gD/m2/d] */
tDConsPisc = tDAssPisc / fDAssPisc ;
/* consumption of Pisc [gD/m2/d] */
tDEgesPisc = tDConsPisc - tDAssPisc ;
/* egestion of Pisc [gD/m2/d] */
tDConsFiJvPisc = sDFiJv / aDFish * tDConsPisc ;
/* young fish consumption by Pisc [gD/m2/d] */
tDConsFiAdPisc = tDConsPisc - tDConsFiJvPisc ;
/* adult fish consumption by Pisc [gD/m2/d] */
tDRespPisc = kDRespPisc * uFunTmPisc * sDPisc ;
/* respiration of Pisc [gD/m2/d] */
tDMortPisc = kMortPisc * sDPisc +(1.0 - aDSatPisc) * tDEnvPisc ;
/* mortality of Pisc(incl environmental correction) [gD/m2/d] */
tDMortPiscBot = fDBone * tDMortPisc ;
/* part of died fish DW fixed in bones AND scales [g/m2/d] */
tDMortPiscDet = tDMortPisc - tDMortPiscBot ;
/* part of died Pisc DW becoming detritus [gD/m2/d] */
if (cos(2.0*Pi * sTime / DaysPerYear) > 0.1) {
   ukHarvPisc = kHarvPiscWin ;
} else {
   ukHarvPisc = kHarvPiscSum ;
}
/* fish harvesting constant [d-1] */
tDHarvPisc = ukHarvPisc * sDPisc ;
/* harvesting of Pisc [gD/m2/d] */
aPPisc = cPDPisc * sDPisc ;
/* Piscivorous fish [gP/m2] */
tPConsFiJvPisc = rPDFiJv * tDConsFiJvPisc ;
/* young fish consumption by Pisc [gP/m2/d] */
tPConsFiAdPisc = rPDFiAd * tDConsFiAdPisc ;
/* adult fish consumption by Pisc [gP/m2/d] */
tPConsPisc = tPConsFiJvPisc + tPConsFiAdPisc ;
/* total P consumption by Pisc [gP_m-2_d-1] */
rPDFoodPisc = tPConsPisc / tDConsPisc ;
/* average P/D ratio of Pisc food [gP/gD] */
afPAssPisc = min(1.0,cPDPisc / rPDFoodPisc * fDAssPisc) ;
/* P assim efficiency of Pisc [-] */
tPAssPisc = afPAssPisc * tPConsPisc ;
/* P assimilation of Pisc [gP/m2/d] */
tPEgesPisc = tPConsPisc - tPAssPisc ;
/* egestion of Pisc [gP/m2/d] */
tPEgesPiscPO4 = fDissEgesPisc * tPEgesPisc ;
/* SRP egestion of Pisc [gP/m2/d] */
tPEgesPiscDet = tPEgesPisc - tPEgesPiscPO4 ;
/* detrital P egestion of Pisc [gP/m2/d] */
tPExcrPisc = cPDPisc * tDRespPisc ;
/* respiration of Pisc [gP/m2/d] */
tPMortPisc = cPDPisc * tDMortPisc ;
/* mortality of Pisc [gP/m2/d] */
tPMortPiscBot = fPBone * tPMortPisc ;
/* part of died Pisc P fixed in bones AND scales [gP/m2/d] */
tPMortPiscPO4 = fDissMortPisc *(tPMortPisc - tPMortPiscBot) ;
/* part of died fish P becoming dissolved P [gP/m2/d] */
tPMortPiscDet = tPMortPisc - tPMortPiscBot - tPMortPiscPO4 ;
/* part of died Pisc P becoming detrital P [gP/m2/d] */
tPMigrPisc = kMigrPisc *(cPDPisc * cDPiscIn - aPPisc) ;
/* net migration flux [gP/m2/d] */
tPHarvPisc = cPDPisc * tDHarvPisc ;
/* harvesting of Pisc [gP/m2/d] */
aNPisc = cNDPisc * sDPisc ;
/* Piscivorous fish [gN_m-2] */
tNConsFiJvPisc = rNDFiJv * tDConsFiJvPisc ;
/* young fish consumption by Pisc [gN/m2/d] */
tNConsFiAdPisc = rNDFiAd * tDConsFiAdPisc ;
/* adult fish consumption by Pisc [gN/m2/d] */
tNConsPisc = tNConsFiJvPisc + tNConsFiAdPisc ;
/* total N consumption by Pisc [gN_m-2_d-1] */
rNDFoodPisc = tNConsPisc / tDConsPisc ;
/* average N/D ratio of Pisc food [gN/gD] */
afNAssPisc = min(1.0,cNDPisc / rNDFoodPisc * fDAssPisc) ;
/* N assim efficiency of Pisc [-] */
tNAssPisc = afNAssPisc * tNConsPisc ;
/* N assimilation of Pisc [gN/m2/d] */
tNEgesPisc = tNConsPisc - tNAssPisc ;
/* egestion of Pisc [gN/m2/d] */
tNEgesPiscNH4 = fDissEgesPisc * tNEgesPisc ;
/* SRN egestion of Pisc [gN/m2/d] */
tNEgesPiscDet = tNEgesPisc - tNEgesPiscNH4 ;
/* detrital N egestion of Pisc [gN/m2/d] */
tNExcrPisc = cNDPisc * tDRespPisc ;
/* respiration of Pisc [gN/m2/d] */
tNMortPisc = cNDPisc * tDMortPisc ;
/* mortality of Pisc [gN/m2/d] */
tNMortPiscBot = fDBone * tNMortPisc ;
/* part of died Pisc N fixed in bones AND scales [gN/m2/d] */
tNMortPiscNH4 = fDissMortPisc *(tNMortPisc - tNMortPiscBot) ;
/* part of died fish N becoming dissolved N [gN/m2/d] */
tNMortPiscDet = tNMortPisc - tNMortPiscBot - tNMortPiscNH4 ;
/* part of died Pisc N becoming detrital N [gN/m2/d] */
tNMigrPisc = kMigrPisc *(cNDPisc * cDPiscIn - aNPisc) ;
/* net migration flux [gN/m2/d] */
tNHarvPisc = cNDPisc * tDHarvPisc ;
/* harvesting of Pisc [gN/m2/d] */

/* ---------------------------------- */
/* Total_fluxes_for_animal_groups */ 
/* ---------------------------------- */

wDWebZoo = wDAssZoo - wDRespZoo - wDMortZoo - tDConsFiJv / sDepthW ;
/* total foodweb flux of DW in Herbivorous zooplankton [mgDW/l/d] */
wPWebZoo = wPAssZoo - wPExcrZoo - wPMortZoo - tPConsFiJv / sDepthW ;
/* total foodweb flux of P in Herbivorous zooplankton [mgP/l/d] */
wNWebZoo = wNAssZoo - wNExcrZoo - wNMortZoo - tNConsFiJv / sDepthW ;
/* total foodweb flux of N in Herbivorous zooplankton [mgN/l/d] */
tDWebBent = tDMigrBent + tDAssBent - tDConsFiAd - tDRespBent - tDMortBent ;
/* total foodweb flux of DW in Zoobenthos [gDW/m2/d] */
tPWebBent = tPMigrBent + tPAssBent - tPConsFiAd - tPExcrBent - tPMortBent ;
/* total foodweb flux of P in Zoobenthos [gP/m2/d] */
tNWebBent = tNMigrBent + tNAssBent - tNConsFiAd - tNExcrBent - tNMortBent ;
/* total foodweb flux of N in Zoobenthos [gN/m2/d] */
tDWebFiJv = tDMigrFiJv + tDReprFish - tDAgeFish + tDAssFiJv - tDRespFiJv - tDMortFiJv - tDConsFiJvPisc ;
/* total foodweb flux of DW in Young fish [gDW/m2/d] */
tPWebFiJv = tPMigrFiJv + tPReprFish - tPAgeFish + tPAssFiJv - tPExcrFiJv - tPMortFiJv - tPConsFiJvPisc ;
/* total foodweb flux of P in Young fish [gP/m2/d] */
tNWebFiJv = tNMigrFiJv + tNReprFish - tNAgeFish + tNAssFiJv - tNExcrFiJv - tNMortFiJv - tNConsFiJvPisc ;
/* total foodweb flux of N in Young fish [gN/m2/d] */
tDWebFiAd = tDMigrFiAd + tDAssFiAd - tDRespFiAd - tDMortFiAd - tDReprFish + tDAgeFish - tDConsFiAdPisc - tDHarvFish ;
/* total foodweb flux of DW in Adult fish [gDW/m2/d] */
tPWebFiAd = tPMigrFiAd + tPAssFiAd - tPExcrFiAd - tPMortFiAd - tPReprFish + tPAgeFish - tPConsFiAdPisc - tPHarvFish ;
/* total foodweb flux of P in Adult fish [gP/m2/d] */
tNWebFiAd = tNMigrFiAd + tNAssFiAd - tNExcrFiAd - tNMortFiAd - tNReprFish + tNAgeFish - tNConsFiAdPisc - tNHarvFish ;
/* total foodweb flux of N in Adult fish [gN/m2/d] */
tDWebPisc = tDMigrPisc + tDAssPisc - tDRespPisc - tDMortPisc - tDHarvPisc ;
/* total foodweb flux of DW in predatory fish [g/m2/d] */

/* ---------------------------------- */
/* Total_fluxes_to/from_other_components */ 
/* ---------------------------------- */

if (0 == InclWeb) {
   wDWebDetW = 0.0 ;
} else {
   wDWebDetW = - wDConsDetZoo + wDEgesZoo + wDMortZoo +(tDEgesFiJv + tDEgesFiAd + tDMortFishDet + tDEgesPisc + tDMortPiscDet) / sDepthW ;
}
/* total foodweb flux of DW in Detritus in lake water [mg/l/d] */
if (0 == InclWeb) {
   wDWebDiatW = 0.0 ;
} else {
   wDWebDiatW = - wDConsDiatZoo ;
}
/* total foodweb flux of DW in Diatoms in lake water [mg/l/d] */
if (0 == InclWeb) {
   wDWebGrenW = 0.0 ;
} else {
   wDWebGrenW = - wDConsGrenZoo ;
}
/* total foodweb flux of DW in Greens in lake water [mg/l/d] */
if (0 == InclWeb) {
   wDWebBlueW = 0.0 ;
} else {
   wDWebBlueW = - wDConsBlueZoo ;
}
/* total foodweb flux of DW in Blue-greens in lake water [mg/l/d] */
if (0 == InclWeb) {
   tDWebDetS = 0.0 ;
} else {
   tDWebDetS = - tDConsDetBent + tDEgesBent + tDMortBent ;
}
/* total foodweb flux of DW in Sediment detritus in lake [gDW/m2/d] */
if (0 == InclWeb) {
   tDWebDiatS = 0.0 ;
} else {
   tDWebDiatS = - tDConsDiatBent ;
}
/* total foodweb flux of DW in sediment diatoms in lake [g/m2/d] */
if (0 == InclWeb) {
   tDWebGrenS = 0.0 ;
} else {
   tDWebGrenS = - tDConsGrenBent ;
}
/* total foodweb flux of DW in sediment greens in lake [g/m2/d] */
if (0 == InclWeb) {
   tDWebBlueS = 0.0 ;
} else {
   tDWebBlueS = - tDConsBlueBent ;
}
/* total foodweb flux of DW in sediment blue-greens in lake [g/m2/d] */
tDWebPhytS = tDWebDiatS + tDWebGrenS + tDWebBlueS ;
/* total food web flux of sediment algae [gDW/m2/d] */
if (0 == InclWeb) {
   tDWebTotT = 0.0 ;
} else {
   tDWebTotT = - wDRespZoo * sDepthW + tDMigrFiJv + tDMigrFiAd + tDMigrPisc + tDMigrBent - tDRespFiJv - tDRespFiAd - tDRespPisc - tDRespBent - tDMortFishBot - tDMortPiscBot - tDHarvFish - tDHarvPisc ;
}
/* total DW in system [gDW/m2/d] */
if (0 == InclWeb) {
   wPWebPO4W = 0.0 ;
} else {
   wPWebPO4W = wPExcrZoo + wPEgesZooPO4 + wPMortZooPO4 +(tPExcrFiJv + tPExcrFiAd + tPEgesFishPO4 + tPMortFishPO4 + tPExcrPisc + tPEgesPiscPO4 + tPMortPiscPO4) / sDepthW ;
}
/* total foodweb flux of P in SRP in water in lake water [mgP/l/d] */
if (0 == InclWeb) {
   wPWebDetW = 0.0 ;
} else {
   wPWebDetW = - wPConsDetZoo + wPEgesZooDet + wPMortZooDet +(tPEgesFishDet + tPMortFishDet + tPEgesPiscDet + tPMortPiscDet) / sDepthW ;
}
/* total foodweb flux of P in Detritus in lake water [mgP/l/d] */
if (0 == InclWeb) {
   wPWebDiatW = 0.0 ;
} else {
   wPWebDiatW = - wPConsDiatZoo ;
}
/* total foodweb flux of P in Diatoms in lake water [mgP/l/d] */
if (0 == InclWeb) {
   wPWebGrenW = 0.0 ;
} else {
   wPWebGrenW = - wPConsGrenZoo ;
}
/* total foodweb flux of P in Greens in lake water [mgP/l/d] */
if (0 == InclWeb) {
   wPWebBlueW = 0.0 ;
} else {
   wPWebBlueW = - wPConsBlueZoo ;
}
/* total foodweb flux of P in Blue-greens in lake water [mgP/l/d] */
if (0 == InclWeb) {
   tPWebPO4S = 0.0 ;
} else {
   tPWebPO4S = tPExcrBent + tPEgesBentPO4 + tPMortBentPO4 ;
}
/* total foodweb flux of P in Pore water P in lake sediment [gP/m2/d] */
if (0 == InclWeb) {
   tPWebDetS = 0.0 ;
} else {
   tPWebDetS = - tPConsDetBent + tPEgesBentDet + tPMortBentDet ;
}
/* total foodweb flux of P in Sediment P in lake [gP/m2/d] */
if (0 == InclWeb) {
   tPWebDiatS = 0.0 ;
} else {
   tPWebDiatS = - tPConsDiatBent ;
}
/* total foodweb flux of P in sediment diatoms in lake [gP/m2/d] */
if (0 == InclWeb) {
   tPWebGrenS = 0.0 ;
} else {
   tPWebGrenS = - tPConsGrenBent ;
}
/* total foodweb flux of P in sediment greens in lake [gP/m2/d] */
if (0 == InclWeb) {
   tPWebBlueS = 0.0 ;
} else {
   tPWebBlueS = - tPConsBlueBent ;
}
/* total foodweb flux of P in sediment blue-greens in lake [gP/m2/d] */
tPWebPhytS = tPWebDiatS + tPWebGrenS + tPWebBlueS ;
/* total food web flux of sediment algae [gP/m2/d] */
if (0 == InclWeb) {
   tPWebTotT = 0.0 ;
} else {
   tPWebTotT = tPMigrFiJv + tPMigrFiAd + tPMigrPisc + tPMigrBent - tPMortFishBot - tPMortPiscBot - tPHarvFish - tPHarvPisc ;
}
/* total P in system [gP/m2/d] */
if (0 == InclWeb) {
   wNWebNH4W = 0.0 ;
} else {
   wNWebNH4W = wNExcrZoo + wNEgesZooNH4 + wNMortZooNH4 +(tNExcrFiJv + tNExcrFiAd + tNEgesFishNH4 + tNMortFishNH4 + tNExcrPisc + tNEgesPiscNH4 + tNMortPiscNH4) / sDepthW ;
}
/* total foodweb flux of N in ammonium in water in lake water [mgN/l/d] */
if (0 == InclWeb) {
   wNWebNO3W = 0.0 ;
} else {
   wNWebNO3W = 0.0 ;
}
/* total foodweb flux of N in nitrate in water in lake water [mgN/l/d] */
if (0 == InclWeb) {
   wNWebDetW = 0.0 ;
} else {
   wNWebDetW = - wNConsDetZoo + wNEgesZooDet + wNMortZooDet +(tNEgesFishDet + tNMortFishDet + tNEgesPiscDet + tNMortPiscDet) / sDepthW ;
}
/* total foodweb flux of N in Detritus in lake water [mgN/l/d] */
if (0 == InclWeb) {
   wNWebDiatW = 0.0 ;
} else {
   wNWebDiatW = - wNConsDiatZoo ;
}
/* total foodweb flux of N in Diatoms in lake water [mgN/l/d] */
if (0 == InclWeb) {
   wNWebGrenW = 0.0 ;
} else {
   wNWebGrenW = - wNConsGrenZoo ;
}
/* total foodweb flux of N in Greens in lake water [mgN/l/d] */
if (0 == InclWeb) {
   wNWebBlueW = 0.0 ;
} else {
   wNWebBlueW = - wNConsBlueZoo ;
}
/* total foodweb flux of N in Blue-greens in lake water [mgN/l/d] */
if (0 == InclWeb) {
   tNWebNH4S = 0.0 ;
} else {
   tNWebNH4S = tNExcrBent + tNEgesBentNH4 + tNMortBentNH4 ;
}
/* total foodweb flux of N in Pore water ammonium in lake sediment [gN/m2/d] */
if (0 == InclWeb) {
   tNWebNO3S = 0.0 ;
} else {
   tNWebNO3S = 0.0 ;
}
/* total foodweb flux of N in Pore water nitrate in lake sediment [gN/m2/d] */
if (0 == InclWeb) {
   tNWebDetS = 0.0 ;
} else {
   tNWebDetS = - tNConsDetBent + tNEgesBentDet + tNMortBentDet ;
}
/* total foodweb flux of N in Sediment N in lake sediment [gN/m2/d] */
if (0 == InclWeb) {
   tNWebDiatS = 0.0 ;
} else {
   tNWebDiatS = - tNConsDiatBent ;
}
/* total foodweb flux of N in sediment diatoms in lake [gN/m2/d] */
if (0 == InclWeb) {
   tNWebGrenS = 0.0 ;
} else {
   tNWebGrenS = - tNConsGrenBent ;
}
/* total foodweb flux of N in sediment greens in lake [gN/m2/d] */
if (0 == InclWeb) {
   tNWebBlueS = 0.0 ;
} else {
   tNWebBlueS = - tNConsBlueBent ;
}
/* total foodweb flux of N in sediment blue-greens in lake [gN/m2/d] */
tNWebPhytS = tNWebDiatS + tNWebGrenS + tNWebBlueS ;
/* total food web flux of sediment algae [gN/m2/d] */
if (0 == InclWeb) {
   tNWebTotT = 0.0 ;
} else {
   tNWebTotT = tNMigrFiJv + tNMigrFiAd + tNMigrPisc + tNMigrBent - tNMortFishBot - tNMortPiscBot - tNHarvFish - tNHarvPisc ;
}
/* total N in system [gN/m2/d] */
wSiWebSiO2W = 0.0 ;
/* total foodweb flux of silica in SiO2 lake water [gSi/m3/d] */
if (0 == InclWeb) {
   wSiWebDetW = 0.0 ;
} else {
   wSiWebDetW = wSiConsDiatZoo ;
}
/* total foodweb flux of silica in lake water detritus [gSi/m3/d] */
if (0 == InclWeb) {
   tSiWebDetS = 0.0 ;
} else {
   tSiWebDetS = tSiConsDiatBent ;
}
/* total foodweb flux of silica in sediment detritus [gSi/m2/d] */
if (0 == InclWeb) {
   tSiWebTotT = 0.0 ;
} else {
   tSiWebTotT = 0.0 ;
}
/* total foodweb flux of silica [gSi/m2/d] */
aPrefAve = (cPrefDiat * sDDiatW + cPrefGren * sDGrenW + cPrefBlue * sDBlueW + cPrefDet * sDDetW) / oDOMW ;
/* average selection factor [-] */
wDConsZoo2 = aFilt * aPrefAve * oDOMW * sDZoo ;
/* total zoopl consumption(check) [mg/l/d] */
aDConsZooSp = wDConsZoo / sDZoo ;
/* specific consumption rate of zoopl(daily ration) [d-1] */
aDAssZooSp = wDAssZoo / sDZoo ;
/* specific C assimilation of zooplankton [d-1] */
aDGrazSp = wDConsZoo / oDOMW ;
/* specific DW grazing(daily grazing) [d-1] */
aPConsZooSp = wPConsZoo / sPZoo ;
/* specific P consumption OR daily ration [d-1] */
aPGrazSp = wPConsZoo / oPOMW ;
/* specific P grazing OR daily grazing [d-1] */
aNConsZooSp = wNConsZoo / sNZoo ;
/* specific N consumption OR daily ration [d-1] */
aNGrazSp = wNConsZoo / oNOMW ;
/* specific N grazing OR daily grazing [d-1] */

/* ---------------------------------- */
/* Marsh_vegetation */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   afDShootPhra = sDShootPhra /(sDRootPhra + sDShootPhra) ;
} else {
   afDShootPhra = 0.0 ;
}
/* Shoot/total -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rDSRPhra = sDShootPhra/sDRootPhra ;
} else {
   rDSRPhra = 0.0 ;
}
/* Shoot/Root -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDShootPhra = sPShootPhra/sDShootPhra ;
} else {
   rPDShootPhra = 0.0 ;
}
/* Shoot P/D -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rNDShootPhra = sNShootPhra/sDShootPhra ;
} else {
   rNDShootPhra = 0.0 ;
}
/* Shoot N/D -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rPDRootPhra = sPRootPhra/sDRootPhra ;
} else {
   rPDRootPhra = 0.0 ;
}
/* Root P/D -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   rNDRootPhra = sNRootPhra/sDRootPhra ;
} else {
   rNDRootPhra = 0.0 ;
}
/* Root N/D -ratio [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aLengShootPhra = sDShootPhra / cDStemPhra / cDensStemPhra ;
} else {
   aLengShootPhra = 0.0 ;
}
/* - [-] */
if (Day < 1.0 || sTime < BeginTime + 1.0) {
   bDayInitPhra = 367 ;
} else if (uTm >= cTmInitPhra && bDayInitPhra > 366) {
   bDayInitPhra = Day ;
} else {
   bDayInitPhra = bDayInitPhra ;
}
/* marks start of root allocation to shoot of phragmities [-] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   aDAllPhra = 0.0 ;
} else if (Day < bDayInitPhra) {
   aDAllPhra = 0.0 ;
} else if (Day <= bDayInitPhra + 1.0 / kDAllPhra) {
   aDAllPhra = fDAllPhra * sDRootPhra ;
} else {
   aDAllPhra = 0.0 ;
}
/* root biomass available for allocation to shoot [gD/m2] */
if (0 == InclSeason) {
   tDAllPhra = 0.0 ;
} else if (0 == InclMarsh || fMarsh <= NearZero) {
   tDAllPhra = 0.0 ;
} else if (Day < bDayInitPhra) {
   tDAllPhra = 0.0 ;
} else if (Day <= bDayInitPhra + 1.0 / kDAllPhra) {
   tDAllPhra = kDAllPhra * aDAllPhra ;
} else {
   tDAllPhra = 0.0 ;
}
/* allocation flux [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNTransPhra = rNDRootPhra * tDAllPhra ;
} else {
   tNTransPhra = 0.0 ;
}
/* translocation of N initial growth [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPTransPhra = rPDRootPhra * tDAllPhra ;
} else {
   tPTransPhra = 0.0 ;
}
/* translocation of P initial growth [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aVNUptPhraMaxCr = max(0.0,cVNUptPhraMax * ((cQ10ProdPhra )^ (0.1 *(uTm - cTmRef))) *(cNDPhraMax - rNDRootPhra) /(cNDPhraMax - cNDPhraMin)) ;
} else {
   aVNUptPhraMaxCr = 0.0 ;
}
/* max uptake rate at current N/D ratio AND temp [mgN/mgD/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   ahNUptPhraS = aVNUptPhraMaxCr / cAffNUptPhra ;
} else {
   ahNUptPhraS = 0.0 ;
}
/* half-saturating N concentration [mgP/l] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aVNUptPhraS = aVNUptPhraMaxCr * oNDissSM /(ahNUptPhraS + oNDissSM) ;
} else {
   aVNUptPhraS = 0.0 ;
}
/* N uptake rate(by roots) [mgN/mgD/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNUptPhraS = aVNUptPhraS * sDRootPhra ;
} else {
   tNUptPhraS = 0.0 ;
}
/* Total N uptake of reed [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNUptNH4PhraS = sNH4SM / aNDissSM * tNUptPhraS ;
} else {
   tNUptNH4PhraS = 0.0 ;
}
/* NH4 uptake of reed [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNUptNO3PhraS = tNUptPhraS - tNUptNH4PhraS ;
} else {
   tNUptNO3PhraS = 0.0 ;
}
/* NO3 uptake of reed [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNUptShootPhra = afDShootPhra * tNUptPhraS ;
} else {
   tNUptShootPhra = 0.0 ;
}
/* N uptake shoot [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNUptRootPhra = tNUptPhraS - tNUptShootPhra ;
} else {
   tNUptRootPhra = 0.0 ;
}
/* N uptake root [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aVPUptPhraMaxCr = max(0.0,cVPUptPhraMax * ((cQ10ProdPhra )^ (0.1 *(uTm - cTmRef))) *(cPDPhraMax - rPDRootPhra) /(cPDPhraMax - cPDPhraMin)) ;
} else {
   aVPUptPhraMaxCr = 0.0 ;
}
/* max uptake rate at current P/D ratio AND temp [mgP/mgD/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   ahPUptPhraS = aVPUptPhraMaxCr / cAffPUptPhra ;
} else {
   ahPUptPhraS = 0.0 ;
}
/* half-saturating P concentration [gP/m3] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aVPUptPhraS = aVPUptPhraMaxCr * oPO4SM /(ahPUptPhraS + oPO4SM) ;
} else {
   aVPUptPhraS = 0.0 ;
}
/* P uptake rate(by roots) [mgP/mgD/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPUptPhraS = aVPUptPhraS * sDRootPhra ;
} else {
   tPUptPhraS = 0.0 ;
}
/* Total P uptake of reed [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPUptShootPhra = afDShootPhra * tPUptPhraS ;
} else {
   tPUptShootPhra = 0.0 ;
}
/* P uptake shoot [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPUptRootPhra = tPUptPhraS - tPUptShootPhra ;
} else {
   tPUptRootPhra = 0.0 ;
}
/* P uptake root [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   uFunTmProdPhra = ((cQ10ProdPhra )^ (0.1 *(uTm - cTmRef))) ;
} else {
   uFunTmProdPhra = 0.0 ;
}
/* tempfunction production vegetation [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   ukDRespTmPhra = kDRespPhra * ((cQ10RespPhra )^ (0.1 *(uTm - cTmRef))) ;
} else {
   ukDRespTmPhra = 0.0 ;
}
/* maintenance respiration rate at current temperature [1/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   aMuPhotPhra = 0.0 ;
} else if (aLengShootPhra >= sDepthWM) {
   aMuPhotPhra = cMuPhraMax * uFunTmProdPhra * ufDay ;
} else {
   aMuPhotPhra = 0.0 ;
}
/* max photosynthetic rate at current light AND temp [1/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNLimProdPhra = (1.0 - cNDPhraMin / rNDRootPhra) * cNDPhraMax /(cNDPhraMax - cNDPhraMin) ;
} else {
   aNLimProdPhra = 0.0 ;
}
/* Droop function N-limitation [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aPLimProdPhra = (1.0 - cPDPhraMin / rPDRootPhra) * cPDPhraMax /(cPDPhraMax - cPDPhraMin) ;
} else {
   aPLimProdPhra = 0.0 ;
}
/* Droop function P-limitation [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aNutLimPhra = min(aNLimProdPhra,aPLimProdPhra) ;
} else {
   aNutLimPhra = 0.0 ;
}
/* nutrient reduction function [-] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aMuPhra = aMuPhotPhra * aNutLimPhra ;
} else {
   aMuPhra = 0.0 ;
}
/* growth rate [1/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   akDIncrPhra = aMuPhotPhra - ukDRespTmPhra - kDMortShootPhra ;
} else {
   akDIncrPhra = 0.0 ;
}
/* intrinsic net increase rate of reed [1/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDDensPhra = max(0.0,akDIncrPhra / cDShootPhraMax * sDShootPhra * sDShootPhra) ;
} else {
   tDDensPhra = 0.0 ;
}
/* density correction of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDDensProdPhra = aMuPhra / cMuPhraMax * tDDensPhra ;
} else {
   tDDensProdPhra = 0.0 ;
}
/* density correction of production [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDProdPhra = aMuPhra * sDShootPhra - tDDensProdPhra ;
} else {
   tDProdPhra = 0.0 ;
}
/* production of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDProdShootPhra = afDShootPhra * tDProdPhra ;
} else {
   tDProdShootPhra = 0.0 ;
}
/* production shoot of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDProdRootPhra = tDProdPhra - tDProdShootPhra ;
} else {
   tDProdRootPhra = 0.0 ;
}
/* production root of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDRespShootPhra = ukDRespTmPhra * sDShootPhra ;
} else {
   tDRespShootPhra = 0.0 ;
}
/* maintenance respiration shoot of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDRespRootPhra = ukDRespTmPhra * sDRootPhra ;
} else {
   tDRespRootPhra = 0.0 ;
}
/* maintenance respiration root of reed [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2RespRootPhra = molO2molC * cCPerDW * tDRespRootPhra * afOxySedM ;
} else {
   tO2RespRootPhra = 0.0 ;
}
/* root O2 respiration [gO2.m-2.d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tO2FlowPhra = tO2RespRootPhra ;
} else {
   tO2FlowPhra = 0.0 ;
}
/* O2 flux to sediment [gO2/m2/d] */
if (Day < 180.0) {
   bDayRealPhra = 367 ;
} else if (ufDay <= fDayWin && bDayRealPhra > 366) {
   bDayRealPhra = Day ;
} else {
   bDayRealPhra = bDayRealPhra ;
}
/* - [-] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   aDRealPhra = 0.0 ;
} else if (Day < bDayRealPhra) {
   aDRealPhra = 0.0 ;
} else if (Day <= bDayRealPhra + 1.0 / kDRealPhra) {
   aDRealPhra = fDRealPhra * sDShootPhra ;
} else {
   aDRealPhra = 0.0 ;
}
/* shoot biomass available for reallocation to root [gDW/m2] */
if (0 == InclSeason) {
   tDRealPhra = 0.0 ;
} else if (0 == InclMarsh || fMarsh <= NearZero) {
   tDRealPhra = 0.0 ;
} else if (Day < bDayRealPhra) {
   tDRealPhra = 0.0 ;
} else if (Day <= bDayRealPhra + 1.0 / kDRealPhra) {
   tDRealPhra = kDRealPhra * aDRealPhra ;
} else {
   tDRealPhra = 0.0 ;
}
/* reallocation of D per day at end of growing season [gDW/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tNRetrPhra = rNDShootPhra * tDRealPhra ;
} else {
   tNRetrPhra = 0.0 ;
}
/* retranslocation of N end growing season [gN/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tPRetrPhra = rPDShootPhra * tDRealPhra ;
} else {
   tPRetrPhra = 0.0 ;
}
/* retranslocation of P end growing season [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDMortShootPhra = kDMortShootPhra * sDShootPhra ;
} else {
   tDMortShootPhra = 0.0 ;
}
/* mortality of shoots [gDW/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tNMortShootPhra = rNDShootPhra * tDMortShootPhra ;
} else {
   tNMortShootPhra = 0.0 ;
}
/* mortality of shoots [gN/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tPMortShootPhra = rPDShootPhra * tDMortShootPhra ;
} else {
   tPMortShootPhra = 0.0 ;
}
/* mortality of shoots [gP/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tDMortRootPhra = kDMortRootPhra * sDRootPhra ;
} else {
   tDMortRootPhra = 0.0 ;
}
/* mortality of roots [gDW/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tNMortRootPhra = rNDRootPhra * tDMortRootPhra ;
} else {
   tNMortRootPhra = 0.0 ;
}
/* mortality of roots [gN/m2/d] */
if  (InclMarsh == 1 && fMarsh > NearZero) {
   tPMortRootPhra = rPDRootPhra * tDMortRootPhra ;
} else {
   tPMortRootPhra = 0.0 ;
}
/* mortality of roots [gP/m2/d] */
if (0 == InclMarsh || fMarsh > NearZero) {
   tDManShootPhra = 0.0 ;
} else if ((Day >= cDayManPhra) && (Day < cDayManPhra + 1.0)) {
   tDManShootPhra = fManPhra * sDShootPhra ;
} else {
   tDManShootPhra = 0.0 ;
}
/* loss flux of biomass by management [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNManShootPhra = tDManShootPhra * rNDShootPhra ;
} else {
   tNManShootPhra = 0.0 ;
}
/* loss flux of N through management [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPManShootPhra = tDManShootPhra * rPDShootPhra ;
} else {
   tPManShootPhra = 0.0 ;
}
/* loss flux of P through management [gDW/m2/d] */

/* ---------------------------------- */
/* Burial_of_sediment */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDIMSM = tDSetIMM ;
} else {
   tDIMSM = 0.0 ;
}
/* increase in inorganic matter in sediment [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDHumSM = fRefrDetS * tDMinDetSM - tDMinHumSM ;
} else {
   tDHumSM = 0.0 ;
}
/* increase in sediment humus in marsh [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tDDetSM = tDSetDetM - tDMinDetSM + tDSetPhytM + tDMortRootPhra ;
} else {
   tDDetSM = 0.0 ;
}
/* increase in sediment detritus in marsh [gDW/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   vDeltaSM = (tDIMSM / cRhoIM +(tDHumSM + tDDetSM) / cRhoOM)/(1.0 - bPorS) ;
} else {
   vDeltaSM = 0.0 ;
}
/* turnover depth in marsh [m/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tDBurIMM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tDBurIMM = ((tDHumSM + tDDetSM) +(cRhoOM / cRhoIM) * tDIMSM) / ((sDHumSM + sDDetSM) / sDIMSM + cRhoOM / cRhoIM) ;
} else {
   tDBurIMM = ((tDHumSM + tDDetSM) +(cRhoOM / cRhoIM) * tDIMSM) / (fDOrgSoil /(1.0 - fDOrgSoil) + cRhoOM / cRhoIM) ;
}
/* burial flux of DW in inorganic matter in marsh [gDW/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tDBurOMM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tDBurOMM = (sDHumSM + sDDetSM) / sDIMSM * tDBurIMM ;
} else {
   tDBurOMM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIMM ;
}
/* burial flux of DW in organic matter in marsh [gDW/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tDBurDetM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tDBurDetM = sDDetSM /(sDHumSM + sDDetSM) * tDBurOMM ;
} else {
   tDBurDetM = 0.0 ;
}
/* burial flux of DW in detritus in marsh [gDW/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tDBurHumM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tDBurHumM = tDBurOMM - tDBurDetM ;
} else {
   tDBurHumM = tDBurOMM ;
}
/* burial flux of DW in humus in marsh [gDW/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tDBurTotM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tDBurTotM = tDBurIMM + tDBurOMM ;
} else {
   tDBurTotM = tDBurIMM + tDBurOMM ;
}
/* total DW burial flux in marsh [gDW/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tPBurHumM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tPBurHumM = rPDHumSM * tDBurHumM ;
} else {
   tPBurHumM = cPDSoilOM * tDBurHumM ;
}
/* burial flux of P in humus in marsh [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tPBurDetM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tPBurDetM = rPDDetSM * tDBurDetM ;
} else {
   tPBurDetM = 0.0 ;
}
/* burial flux of P in detritus in marsh [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tPBurAIMM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tPBurAIMM = sPAIMSM / sDIMSM * tDBurIMM ;
} else {
   tPBurAIMM = 0.0 ;
}
/* burial flux of P absorbed onto inorganic matter in marsh [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tPBurPO4M = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tPBurPO4M = sPO4SM *(vDeltaSM / cDepthSM) ;
} else {
   tPBurPO4M = cPO4Ground *(bPorSM * vDeltaSM) ;
}
/* burial flux of dissolved P in marsh [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tPBurTotM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tPBurTotM = tPBurDetM + tPBurHumM + tPBurAIMM + tPBurPO4M ;
} else {
   tPBurTotM = tPBurHumM + tPBurAIMM + tPBurPO4M ;
}
/* total P burial flux in marsh [gP/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNBurHumM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tNBurHumM = rNDHumSM * tDBurHumM ;
} else {
   tNBurHumM = cNDSoilOM * tDBurHumM ;
}
/* burial flux of N in humus in marsh [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNBurDetM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tNBurDetM = rNDDetSM * tDBurDetM ;
} else {
   tNBurDetM = 0.0 ;
}
/* burial flux of N in detritus in marsh [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNBurNH4M = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tNBurNH4M = sNH4SM *(vDeltaSM / cDepthSM) ;
} else {
   tNBurNH4M = cNH4Ground *(bPorSM * vDeltaSM) ;
}
/* burial flux of dissolved NH4 in marsh [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNBurNO3M = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tNBurNO3M = sNO3SM *(vDeltaSM / cDepthSM) ;
} else {
   tNBurNO3M = cNO3Ground *(bPorSM * vDeltaSM) ;
}
/* burial flux of dissolved NO3 in marsh [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tNBurTotM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tNBurTotM = tNBurDetM + tNBurHumM + tNBurNH4M + tNBurNO3M ;
} else {
   tNBurTotM = tNBurHumM + tNBurNH4M + tNBurNO3M ;
}
/* total N burial flux in marsh [gN/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tSiBurDetM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tSiBurDetM = rSiDDetSM * tDBurDetM ;
} else {
   tSiBurDetM = 0.0 ;
}
/* burial flux of Si in detritus in marsh [gSi/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   tSiBurTotM = 0.0 ;
} else if (vDeltaSM >= 0.0) {
   tSiBurTotM = tSiBurDetM ;
} else {
   tSiBurTotM = 0.0 ;
}
/* total Si burial flux in marsh [gSi/m2/d] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   vDeltaWM = 0.0 ;
} else if (ConstDepth == 1) {
   vDeltaWM = 0.0 ;
} else {
   vDeltaWM = - vDeltaSM ;
}
/* marsh water depth change [m/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   aRelDeltaWM = vDeltaWM / sDepthWM ;
} else {
   aRelDeltaWM = 0.0 ;
}
/* relative marsh water depth change [d-1] */

/* ---------------------------------- */
/* Total_settling */ 
/* ---------------------------------- */

tDSetTot = tDSetIM + tDSetDet + tDSetPhyt ;
/* total settling [gD/m2/d] */
tPSetTot = tPSetAIM + tPSetDet + tPSetPhyt ;
/* total settling [gP/m2/d] */
tNSetTot = tNSetDet + tNSetPhyt ;
/* total settling [gN/m2/d] */
tDResusTot = tDResusDead + tDResusPhytTot ;
/* total resuspension [gD/m2/d] */
tPResusTot = tPResusDet + tPResusAIM + tPResusPhyt + tPResusPO4 ;
/* total P resuspension flux [gP/m2/d] */
tNResusTot = tNResusDet + tNResusNH4 + tNResusNO3 + tNResusPhyt ;
/* total N resuspension flux [gN/m2/d] */

/* ---------------------------------- */
/* dredging */ 
/* ---------------------------------- */

if (sTime >= cDredStart * DaysPerYear) {
   bTimeDred = (int(TimeYears/cDredInterval) * cDredInterval) * DaysPerYear ;
} else {
   bTimeDred = -9999.999 ;
}
/* dredging time(every nth year) [d] */
if (sTime == bTimeDred) {
   aDepthStart = sDepthW ;
} else {
   aDepthStart = aDepthStart ;
}
/* update dredget layer [d] */
if ((sTime >= bTimeDred) && (sTime < bTimeDred + cLengDred) && (aDepthStart <= cDepthRef - cDepthS)) {
   akDredDepth = (ln(cDepthRef / aDepthStart)) / cLengDred ;
} else {
   akDredDepth = 0.0 ;
}
/* rate constant of deepening [d-1] */
if ((sTime >= bTimeDred) && (sTime < bTimeDred + cLengDred) && (aDepthStart <= cDepthRef - cDepthS)) {
   akDred = (- ln(1.0 - fEffDred)) / cLengDred ;
} else {
   akDred = 0.0 ;
}
/* rate constant of dredging(exponential function) [d-1] */
if ((sTime >= bTimeDred) && (sTime < bTimeDred + cLengDred) && (aDepthStart <= cDepthRef - cDepthS)) {
   akDredBent = (- ln(1.0 - fEffDredBent)) / cLengDred ;
} else {
   akDredBent = 0.0 ;
}
/* rate constant of dredging for zoobenthos [d-1] */
vDredDepthW = akDredDepth * sDepthW ;
/* change in water depth due to dredging [m/d] */
tDDredDetS = akDred * sDDetS ;
/* dredging flux of DW Detritus in lake sediment [gDW/m2/d] */
tPDredDetS = akDred * sPDetS ;
/* dredging flux of P Detritus in lake sediment [gP/m2/d] */
tNDredDetS = akDred * sNDetS ;
/* dredging flux of N Detritus in lake sediment [gN/m2/d] */
tSiDredDetS = akDred * sSiDetS ;
/* dredging flux of Si Det in lake sediment [gSi/m2/d] */
tPDredAIMS = akDred * sPAIMS ;
/* dredging flux of P absorbed onto inorganic matter in lake sediment [gP/m2/d] */
bRhoSolidSoil = fDOrgSoil * cRhoOM +(1.0 - fDOrgSoil) * cRhoIM ;
/* average solid density of soil material [g/m3] */
tDDredNetSoil = -(tDDredDetS / cRhoOM) * bRhoSolidSoil ;
/* dredging flux of DW NetSoil in lake sediment [gDW/m2/d] */
tDDredNetIMS = (1.0 - fDOrgSoil) * tDDredNetSoil ;
/* dredging flux of DW NetIMS in lake sediment [gDW/m2/d] */
tDDredNetHumS = fDOrgSoil * tDDredNetSoil ;
/* dredging flux of DW NetHum in lake sediment [gDW/m2/d] */
tPDredNetHumS = cPDSoilOM * tDDredNetHumS ;
/* dredging flux of P NetHum in lake sediment [gP/m2/d] */
tNDredNetHumS = cNDSoilOM * tDDredNetHumS ;
/* dredging flux of N NetHum in lake sediment [gN/m2/d] */
tDDredDiatS = akDred * sDDiatS ;
/* dredging flux of DW Diat on lake sediment [gDW/m2/d] */
tPDredDiatS = akDred * sPDiatS ;
/* dredging flux of P Diat on lake sediment [gP/m2/d] */
tNDredDiatS = akDred * sNDiatS ;
/* dredging flux of N Diat on lake sediment [gN/m2/d] */
tDDredGrenS = akDred * sDGrenS ;
/* dredging flux of DW Gren on lake sediment [gDW/m2/d] */
tPDredGrenS = akDred * sPGrenS ;
/* dredging flux of P Gren on lake sediment [gP/m2/d] */
tNDredGrenS = akDred * sNGrenS ;
/* dredging flux of N Gren on lake sediment [gN/m2/d] */
tDDredBlueS = akDred * sDBlueS ;
/* dredging flux of DW Blue on lake sediment [gDW/m2/d] */
tPDredBlueS = akDred * sPBlueS ;
/* dredging flux of P Blue on lake sediment [gP/m2/d] */
tNDredBlueS = akDred * sNBlueS ;
/* dredging flux of N Blue on lake sediment [gN/m2/d] */
tDDredPhytS = tDDredDiatS+tDDredGrenS+tDDredBlueS ;
/* dredging flux of DW Phyt on lake sediment [gDW/m2/d] */
tPDredPhytS = tPDredDiatS+tPDredGrenS+tPDredBlueS ;
/* dredging flux of P Phyt on lake sediment [gP/m2/d] */
tNDredPhytS = tNDredDiatS+tNDredGrenS+tNDredBlueS ;
/* dredging flux of N Phyt on lake sediment [gN/m2/d] */
if (InclWeb == 1) {
   tDDredBent = akDredBent * sDBent ;
} else {
   tDDredBent = 0.0 ;
}
/* dredging flux of DW Bent on lake sediment [gDW/m2/d] */
if (InclWeb == 1) {
   tPDredBent = akDredBent * sPBent ;
} else {
   tPDredBent = 0.0 ;
}
/* dredging flux of P Bent on lake sediment [gP/m2/d] */
if (InclWeb == 1) {
   tNDredBent = akDredBent * sNBent ;
} else {
   tNDredBent = 0.0 ;
}
/* dredging flux of N Bent on lake sediment [gN/m2/d] */
tDDredVeg = akDred * sDVeg ;
/* dredging flux of DW Veg on lake sediment [gDW/m2/d] */
tPDredVeg = akDred * sPVeg ;
/* dredging flux of P Veg on lake sediment [gP/m2/d] */
tNDredVeg = akDred * sNVeg ;
/* dredging flux of N Veg on lake sediment [gN/m2/d] */
tDDredNetTot = tDDredDetS - tDDredNetSoil + tDDredPhytS + tDDredBent + tDDredVeg ;
/* total DW dredging flux [gDW/m2/d] */
tPDredNetTot = tPDredDetS - tPDredNetHumS + tPDredAIMS + tPDredPhytS + tPDredBent + tPDredVeg ;
/* total P dredging flux [gP/m2/d] */
tNDredNetTot = tNDredDetS - tNDredNetHumS + tNDredPhytS + tNDredBent + tNDredVeg ;
/* total N dredging flux [gN/m2/d] */
tSiDredTot = tSiDredDetS + cSiDDiat * tDDredDiatS ;
/* total Si dredging flux [gSi/m2/d] */

/* ---------------------------------- */
/* burial */ 
/* ---------------------------------- */

tDIMS = tDAbioIMS ;
/* increase in inorganic matter in sediment [gDW/m2/d] */
tDHumS = tDAbioHumS ;
/* increase in sediment humus in lake [gDW/m2/d] */
tDDetS = tDAbioDetS + tDPrimDetS + tDWebDetS + tDBedDetS ;
/* increase in sediment detritus in lake [gDW/m2/d] */
vDeltaS = (tDIMS / cRhoIM +(tDHumS + tDDetS) / cRhoOM)/(1.0 - bPorS) ;
/* turnover depth in lake [m/d] */
if (vDeltaS >= 0.0) {
   tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / ((sDHumS + sDDetS) / sDIMS + cRhoOM / cRhoIM) ;
} else {
   tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / (fDOrgSoil /(1.0 - fDOrgSoil) + cRhoOM / cRhoIM) ;
}
/* burial flux of DW in inorganic matter in lake [gDW/m2/d] */
if (vDeltaS >= 0.0) {
   tDBurOM = (sDHumS + sDDetS) / sDIMS * tDBurIM ;
} else {
   tDBurOM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIM ;
}
/* burial flux of DW in organic matter in lake [gDW/m2/d] */
if (vDeltaS >= 0.0) {
   tDBurDet = sDDetS /(sDHumS + sDDetS) * tDBurOM ;
} else {
   tDBurDet = 0.0 ;
}
/* burial flux of DW in detritus in lake [gDW/m2/d] */
if (vDeltaS >= 0.0) {
   tDBurHum = tDBurOM - tDBurDet ;
} else {
   tDBurHum = tDBurOM ;
}
/* burial flux of DW in humus in lake [gDW/m2/d] */
if (vDeltaS >= 0.0) {
   tDBurTot = tDBurIM + tDBurOM ;
} else {
   tDBurTot = tDBurIM + tDBurOM ;
}
/* total DW burial flux in lake [gDW/m2/d] */
if (vDeltaS >= 0.0) {
   tPBurHum = rPDHumS * tDBurHum ;
} else {
   tPBurHum = cPDSoilOM * tDBurHum ;
}
/* burial flux of P in humus in lake [gP/m2/d] */
if (vDeltaS >= 0.0) {
   tPBurDet = rPDDetS * tDBurDet ;
} else {
   tPBurDet = 0.0 ;
}
/* burial flux of P in detritus in lake [gP/m2/d] */
if (vDeltaS >= 0.0) {
   tPBurAIM = sPAIMS / sDIMS * tDBurIM ;
} else {
   tPBurAIM = 0.0 ;
}
/* burial flux of P absorbed onto inorganic matter in lake [gP/m2/d] */
if (vDeltaS >= 0.0) {
   tPBurPO4 = sPO4S *(vDeltaS / cDepthS) ;
} else {
   tPBurPO4 = cPO4Ground *(bPorS * vDeltaS) ;
}
/* burial flux of dissolved P in lake [gP/m2/d] */
if (vDeltaS >= 0.0) {
   tPBurTot = tPBurDet + tPBurHum + tPBurAIM + tPBurPO4 ;
} else {
   tPBurTot = tPBurHum + tPBurAIM + tPBurPO4 ;
}
/* total P burial flux in lake [gP/m2/d] */
if (vDeltaS >= 0.0) {
   tNBurHum = rNDHumS * tDBurHum ;
} else {
   tNBurHum = cNDSoilOM * tDBurHum ;
}
/* burial flux of N in humus in lake [gN/m2/d] */
if (vDeltaS >= 0.0) {
   tNBurDet = rNDDetS * tDBurDet ;
} else {
   tNBurDet = 0.0 ;
}
/* burial flux of N in detritus in lake [gN/m2/d] */
if (vDeltaS >= 0.0) {
   tNBurNH4 = sNH4S *(vDeltaS / cDepthS) ;
} else {
   tNBurNH4 = cNH4Ground *(bPorS * vDeltaS) ;
}
/* burial flux of dissolved NH4 in lake [gN/m2/d] */
if (vDeltaS >= 0.0) {
   tNBurNO3 = sNO3S *(vDeltaS / cDepthS) ;
} else {
   tNBurNO3 = cNO3Ground *(bPorS * vDeltaS) ;
}
/* burial flux of dissolved NO3 in lake [gN/m2/d] */
if (vDeltaS >= 0.0) {
   tNBurTot = tNBurDet + tNBurHum + tNBurNH4 + tNBurNO3 ;
} else {
   tNBurTot = tNBurHum + tNBurNH4 + tNBurNO3 ;
}
/* total N burial flux in lake [gN/m2/d] */
if (vDeltaS >= 0.0) {
   tSiBurDet = rSiDDetS * tDBurDet ;
} else {
   tSiBurDet = 0.0 ;
}
/* burial flux of Si in detritus in lake [gSi/m2/d] */
if (vDeltaS >= 0.0) {
   tSiBurTot = tSiBurDet ;
} else {
   tSiBurTot = 0.0 ;
}
/* total Si burial flux in lake [gSi/m2/d] */
if (ConstDepth == 1) {
   vDeltaW = 0.0 ;
} else {
   vDeltaW = - vDeltaS ;
}
/* lake water depth change [m/d] */
aRelDeltaW = (vDeltaW + vDredDepthW) / sDepthW ;
/* relative water depth change due to sediment turnover AND dredging [d-1] */

/* ---------------------------------- */
/* mass_balance */ 
/* ---------------------------------- */

if (InclMarsh == 1 && fMarsh > NearZero) {
   tDMarsTotT = (- tDBurTotM - wDMinDetWM * sDepthWM -(1.0 - fRefrDetS) * tDMinDetSM - tDMinHumSM + tDProdPhra - tDRespShootPhra - tDRespRootPhra - tDManShootPhra) * fMarsh ;
} else {
   tDMarsTotT = 0.0 ;
}
/* Mass balance totals of DW marsh water and vegetation module [g/m2lake/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tPMarsTotT = (- tPInfPO4SM - tPDifGroundPO4M - tPBurTotM - tPChemPO4M - tPManShootPhra) * fMarsh ;
} else {
   tPMarsTotT = 0.0 ;
}
/* Mass balance totals of P marsh water and vegetation module [g/m2lake/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tNMarsTotT = (- tNInfNH4SM - tNInfNO3SM - tNDifGroundNO3M - tNDifGroundNH4M - tNBurTotM - wNDenitWM * sDepthWM - tNDenitSM - tNManShootPhra) * fMarsh ;
} else {
   tNMarsTotT = 0.0 ;
}
/* Mass balance totals of N marsh water and vegetation module [g/m2lake/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   tSiMarsTotT = - tSiBurDetM * fMarsh ;
} else {
   tSiMarsTotT = 0.0 ;
}
/* Mass balance totals of SI marsh water and vegetation module [g/m2lake/d] */
aDTotT = (sDIMW+sDDetW+sDDiatW+sDGrenW+sDBlueW+sDZoo) * sDepthW +aDFish+sDPisc+aDVeg +sDIMS+sDHumS+sDDetS+sDDiatS+sDGrenS+sDBlueS+sDBent + aDTotM ;
/* total DW in system [gD/m2] */
aNTotT = (sNH4W+sNO3W+sNDetW+sNDiatW+sNGrenW+sNBlueW +sNZoo)*sDepthW +aNFish+aNPisc+aNVeg +sNH4S+sNO3S+sNDetS+sNHumS+sNDiatS+sNGrenS+sNBlueS +sNBent + aNTotM ;
/* total N in system [gN/m2] */
aPTotT = (sPO4W+sPDetW+sPDiatW+sPGrenW+sPBlueW +sPZoo+sPAIMW) * sDepthW +aPFish+aPPisc+aPVeg +sPO4S+sPDetS+sPHumS+sPDiatS+sPGrenS+sPBlueS +sPAIMS+sPBent + aPTotM ;
/* total P in system [gP/m2] */
aSiTotT = (sSiO2W + sSiDetW + cSiDDiat*sDDiatW) *sDepthW + sSiDetS + cSiDDiat*sDDiatS + aSiTotM ;
/* total Si in system [gSi/m2] */
aDError = aDTotT - sDExtTotT ;
/* DW mass balance error [gD/m2] */
aNError = aNTotT - sNExtTotT ;
/* N mass balance error [gN/m2] */
aPError = aPTotT - sPExtTotT ;
/* P mass balance error [gP/m2] */
aSiError = aSiTotT - sSiExtTotT ;
/* Si mass balance error [gSi/m2] */

/* ---------------------------------- */
/* Derivatives */ 
/* ---------------------------------- */

dNH4W = wNTranNH4W + wNAbioNH4W + wNPrimNH4W + wNBedNH4W + wNWebNH4W + cNBackLoad / sDepthW - aRelDeltaW * sNH4W - wNExchNH4 ;
/* derivative for N ammonium in water in lake water [mgN/l/d] */
dNO3W = wNTranNO3W + wNAbioNO3W + wNPrimNO3W + wNBedNO3W + wNWebNO3W - aRelDeltaW * sNO3W - wNExchNO3 ;
/* derivative for N nitrate in water in lake water [mgN/l/d] */
dPO4W = wPTranPO4W + wPAbioPO4W + wPPrimPO4W + wPBedPO4W + wPWebPO4W + cPBackLoad / sDepthW - aRelDeltaW * sPO4W - wPExchPO4 ;
/* derivative for P SRP in water in lake water [mgP/l/d] */
dPAIMW = wPTranAIMW + wPAbioAIMW - aRelDeltaW * sPAIMW - wPExchAIM ;
/* derivative for P P-adsorbed onto IM in water in lake water [mgP/l/d] */
dSiO2W = wSiTranSiO2 + wSiAbioSiO2W + wSiPrimSiO2W - aRelDeltaW * sSiO2W - wSiExchSiO2 ;
/* derivative for Si dissolved silica in water in lake water [mgSi/l/d] */
dO2W = wO2TranW + wO2AbioW + wO2PrimW + tO2BedW / sDepthW - aRelDeltaW * sO2W - wO2Exch ;
/* derivative for O2 oxygen in water in lake water [gO2/m3/d] */
dDDetW = wDTranDetW + wDAbioDetW + wDPrimDetW + wDBedDetW + wDWebDetW - aRelDeltaW * sDDetW - wDExchDet ;
/* derivative for DW Detritus in lake water [mg/l/d] */
dNDetW = wNTranDetW + wNAbioDetW + wNPrimDetW + wNBedDetW + wNWebDetW - aRelDeltaW * sNDetW - wNExchDet ;
/* derivative for N Detritus in lake water [mgN/l/d] */
dPDetW = wPTranDetW + wPAbioDetW + wPPrimDetW + wPBedDetW + wPWebDetW - aRelDeltaW * sPDetW - wPExchDet ;
/* derivative for P Detritus in lake water [mgP/l/d] */
dSiDetW = wSiTranDetW + wSiAbioDetW + wSiPrimDetW + wSiWebDetW - aRelDeltaW * sSiDetW - wSiExchDet ;
/* derivative for Si Detritus in lake water [mgSi/l/d] */
dDIMW = wDTranIMW + wDAbioIMW - aRelDeltaW * sDIMW - wDExchIM ;
/* derivative for DW inorg matter in water in lake water [mg/l/d] */
dDDiatW = wDTranDiat + wDPrimDiatW + wDWebDiatW - aRelDeltaW * sDDiatW - wDExchDiat ;
/* derivative for DW Diatoms in lake water [mg/l/d] */
dNDiatW = wNTranDiat + wNPrimDiatW + wNWebDiatW - aRelDeltaW * sNDiatW - wNExchDiat ;
/* derivative for N Diatoms in lake water [mgN/l/d] */
dPDiatW = wPTranDiat + wPPrimDiatW + wPWebDiatW - aRelDeltaW * sPDiatW - wPExchDiat ;
/* derivative for P Diatoms in lake water [mgP/l/d] */
dDGrenW = wDTranGren + wDPrimGrenW + wDWebGrenW - aRelDeltaW * sDGrenW - wDExchGren ;
/* derivative for DW Greens in lake water [mg/l/d] */
dNGrenW = wNTranGren + wNPrimGrenW + wNWebGrenW - aRelDeltaW * sNGrenW - wNExchGren ;
/* derivative for N Greens in lake water [mgN/l/d] */
dPGrenW = wPTranGren + wPPrimGrenW + wPWebGrenW - aRelDeltaW * sPGrenW - wPExchGren ;
/* derivative for P Greens in lake water [mgP/l/d] */
dDBlueW = wDTranBlue + wDPrimBlueW + wDWebBlueW - aRelDeltaW * sDBlueW - wDExchBlue ;
/* derivative for DW Bluegreens in lake water [mg/l/d] */
dNBlueW = wNTranBlue + wNPrimBlueW + wNWebBlueW - aRelDeltaW * sNBlueW - wNExchBlue ;
/* derivative for N Bluegreens in lake water [mgN/l/d] */
dPBlueW = wPTranBlue + wPPrimBlueW + wPWebBlueW - aRelDeltaW * sPBlueW - wPExchBlue ;
/* derivative for P Bluegreens in lake water [mgP/l/d] */
dDZoo = wDTranZoo + wDWebZoo - aRelDeltaW * sDZoo - wDExchZoo ;
/* derivative for DW Zooplankton in lake water [mg/l/d] */
dNZoo = wNTranZoo + wNWebZoo - aRelDeltaW * sNZoo - wNExchZoo ;
/* derivative for N Zooplankton in lake water [mgN/l/d] */
dPZoo = wPTranZoo + wPWebZoo - aRelDeltaW * sPZoo - wPExchZoo ;
/* derivative for P Zooplankton in lake water [mgP/l/d] */
dDFiAd = tDWebFiAd ;
/* derivative for DW Adult whitefish in lake water [gD/m2/d] */
dDFiJv = tDWebFiJv ;
/* derivative for DW Juvenile whitefish in lake water [gD/m2/d] */
dNFiAd = tNWebFiAd ;
/* derivative for N Adult whitefish in lake water [gN/m2/d] */
dNFiJv = tNWebFiJv ;
/* derivative for N Juvenile whitefish in lake water [gN/m2/d] */
dPFiAd = tPWebFiAd ;
/* derivative for P Adult whitefish in lake water [gP/m2/d] */
dPFiJv = tPWebFiJv ;
/* derivative for P Juvenile whitefish in lake water [gP/m2/d] */
dDPisc = tDWebPisc ;
/* derivative for DW predatory fish in lake water [g/m2/d] */
dNH4S = tNAbioNH4S - tNBurNH4 + tNPrimNH4S + tNBedNH4S + tNWebNH4S ;
/* derivative for N Pore water ammonium in lake water [gN/m2/d] */
dNO3S = tNAbioNO3S - tNBurNO3 + tNPrimNO3S + tNBedNO3S + tNWebNO3S ;
/* derivative for N Pore water nitrate in lake water [gN/m2/d] */
dPO4S = tPAbioPO4S - tPBurPO4 + tPPrimPO4S + tPBedPO4S + tPWebPO4S ;
/* derivative for P Pore water SRP in lake water [gP/m2/d] */
dPAIMS = tPAbioAIMS - tPBurAIM - tPDredAIMS ;
/* derivative for P P-adsorbed onto IM in sediment in lake sediment [gP/m2/d] */
dDDetS = tDAbioDetS - tDBurDet + tDPrimDetS + tDBedDetS + tDWebDetS - tDDredDetS ;
/* derivative for DW Sediment detritus in lake sediment [g/m2/d] */
dNDetS = tNAbioDetS - tNBurDet + tNPrimDetS + tNBedDetS + tNWebDetS - tNDredDetS ;
/* derivative for N Sediment detritus N in lake sediment [gN/m2/d] */
dPDetS = tPAbioDetS - tPBurDet + tPPrimDetS + tPBedDetS + tPWebDetS - tPDredDetS ;
/* derivative for P Sediment detritus P in lake sediment [gP/m2/d] */
dSiDetS = tSiAbioDetS - tSiBurDet + tSiPrimDetS + tSiWebDetS - tSiDredDetS ;
/* derivative for Si Sediment detritus Si in lake sediment [gSi/m2/d] */
dDHumS = tDAbioHumS - tDBurHum - tDDredNetHumS ;
/* derivative for DW humus in lake sediment [gD/m2/d] */
dNHumS = tNAbioHumS - tNBurHum - tNDredNetHumS ;
/* derivative for N humus in lake sediment [gN/m2/d] */
dPHumS = tPAbioHumS - tPBurHum - tPDredNetHumS ;
/* derivative for P humus in lake sediment [gP/m2/d] */
dDIMS = tDAbioIMS - tDBurIM - tDDredNetIMS ;
/* derivative for DW inorg matter in sediment in lake sediment [gD/m2/d] */
dDDiatS = tDPrimDiatS + tDWebDiatS - tDDredDiatS ;
/* derivative for DW Sed Diatoms in lake sediment [g/m2/d] */
dNDiatS = tNPrimDiatS + tNWebDiatS - tNDredDiatS ;
/* derivative for N Sediment diatoms in lake sediment [gN/m2/d] */
dPDiatS = tPPrimDiatS + tPWebDiatS - tPDredDiatS ;
/* derivative for P Sediment diatoms in lake sediment [gP/m2/d] */
dDGrenS = tDPrimGrenS + tDWebGrenS - tDDredGrenS ;
/* derivative for DW Sed Greens in lake sediment [g/m2/d] */
dNGrenS = tNPrimGrenS + tNWebGrenS - tPDredGrenS ;
/* derivative for N Sediment green algae in lake sediment [gN/m2/d] */
dPGrenS = tPPrimGrenS + tPWebGrenS - tPDredGrenS ;
/* derivative for P Sediment green algae in lake sediment [gP/m2/d] */
dDBlueS = tDPrimBlueS + tDWebBlueS - tDDredBlueS ;
/* derivative for DW Sed Blue-greens in lake sediment [g/m2/d] */
dNBlueS = tNPrimBlueS + tNWebBlueS - tNDredBlueS ;
/* derivative for N Sediment blue-greens in lake sediment [gN/m2/d] */
dPBlueS = tPPrimBlueS + tPWebBlueS - tPDredBlueS ;
/* derivative for P Sediment blue-greens in lake sediment [gP/m2/d] */
dDVeg = tDBedVeg - tDDredVeg ;
/* derivative for DW Vegetation in lake sediment [gD/m2/d] */
dNVeg = tNBedVeg - tNDredVeg ;
/* derivative for N Vegetation in lake sediment [gN/m2/d] */
dPVeg = tPBedVeg - tPDredVeg ;
/* derivative for P Vegetation in lake sediment [gP/m2/d] */
dDBent = tDWebBent - tDDredBent ;
/* derivative for DW Zoobenthos in lake sediment [gD/m2/d] */
dNBent = tNWebBent - tNDredBent ;
/* derivative for N Zoobenthos in lake sediment [gN/m2/d] */
dPBent = tPWebBent - tPDredBent ;
/* derivative for P Zoobenthos in lake sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNH4WM = tNDifNH4M/sDepthWM - wNNitrWM + wNMinDetWM - tNEvNH4WM/sDepthWM - tNInfNH4WM/sDepthWM + wNExchNH4M - aRelDeltaWM * sNH4WM ;
} else {
   dNH4WM = 0.0 ;
}
/* derivative for N NH4 in water in marsh water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNO3WM = tNDifNO3M/sDepthWM + wNNitrWM - wNDenitWM - tNEvNO3WM/sDepthWM - tNInfNO3WM/sDepthWM + wNExchNO3M - aRelDeltaWM * sNO3WM ;
} else {
   dNO3WM = 0.0 ;
}
/* derivative for N NO3 in water in marsh water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPO4WM = - tPInfPO4WM / sDepthWM + tPDifPO4M / sDepthWM + wPMinDetWM - tPEvPO4WM / sDepthWM - wPSorpIMWM + wPExchPO4M - aRelDeltaWM * sPO4WM ;
} else {
   dPO4WM = 0.0 ;
}
/* derivative for P PO4 in water in marsh water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPAIMWM = - tPSetAIMM / sDepthWM + wPSorpIMWM + wPExchAIMM - aRelDeltaWM * sPAIMWM ;
} else {
   dPAIMWM = 0.0 ;
}
/* derivative for P P adsorbed onto IM in water in marsh water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dSiO2WM = wSiMinDetWM + tSiMinDetSM / sDepthWM + wSiExchSiO2M - aRelDeltaWM * sSiO2WM ;
} else {
   dSiO2WM = 0.0 ;
}
/* derivative for Si SiO2 in marsh water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dO2WM = tO2AerM / sDepthWM - wO2MinDetWM - wO2NitrWM -(tO2MinDetSM + tO2NitrSM) / sDepthWM + wO2ExchM - aRelDeltaWM * sO2WM ;
} else {
   dO2WM = 0.0 ;
}
/* derivative for O2 O2 in water in marsh water [gO2/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDDetWM = tDMortShootPhra/sDepthWM - tDSetDetM/sDepthWM - wDMinDetWM + wDExchDetM - aRelDeltaWM * sDDetWM ;
} else {
   dDDetWM = 0.0 ;
}
/* derivative for DW Detritus in marsh water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNDetWM = tNMortShootPhra / sDepthWM - tNSetDetM / sDepthWM - wNMinDetWM + wNExchDetM - aRelDeltaWM * sNDetWM ;
} else {
   dNDetWM = 0.0 ;
}
/* derivative for N detritus in marsh water [gN/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPDetWM = tPMortShootPhra / sDepthWM - tPSetDetM / sDepthWM - wPMinDetWM + wPExchDetM - aRelDeltaWM * sPDetWM ;
} else {
   dPDetWM = 0.0 ;
}
/* derivative for P detritus in marsh water [gP/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dSiDetWM = - tSiSetDetM / sDepthWM - wSiMinDetWM + wSiExchDetM - aRelDeltaWM * sSiDetWM ;
} else {
   dSiDetWM = 0.0 ;
}
/* derivative for Si detritus in marsh water [gSi/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDIMWM = - tDSetIMM/sDepthWM + wDExchIMM - aRelDeltaWM * sDIMWM ;
} else {
   dDIMWM = 0.0 ;
}
/* derivative for DW Inorg matter in marsh water [gD/m3/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDDiatWM = wDExchDiatM - tDSetDiatM / sDepthWM - aRelDeltaWM * sDDiatWM ;
} else {
   dDDiatWM = 0.0 ;
}
/* derivative for DW diatoms in marsh water [gD_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNDiatWM = wNExchDiatM - tNSetDiatM / sDepthWM - aRelDeltaWM * sNDiatWM ;
} else {
   dNDiatWM = 0.0 ;
}
/* derivative for N diatoms in marsh water [gN_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPDiatWM = wPExchDiatM - tPSetDiatM / sDepthWM - aRelDeltaWM * sPDiatWM ;
} else {
   dPDiatWM = 0.0 ;
}
/* derivative for P diatoms in marsh water [gP_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDGrenWM = wDExchGrenM - tDSetGrenM / sDepthWM - aRelDeltaWM * sDGrenWM ;
} else {
   dDGrenWM = 0.0 ;
}
/* derivative for DW greens in marsh water [gD_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNGrenWM = wNExchGrenM - tNSetGrenM / sDepthWM - aRelDeltaWM * sNGrenWM ;
} else {
   dNGrenWM = 0.0 ;
}
/* derivative for N greens in marsh water [gN_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPGrenWM = wPExchGrenM - tPSetGrenM / sDepthWM - aRelDeltaWM * sPGrenWM ;
} else {
   dPGrenWM = 0.0 ;
}
/* derivative for P greens in marsh water [gP_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDBlueWM = wDExchBlueM - tDSetBlueM / sDepthWM - aRelDeltaWM * sDBlueWM ;
} else {
   dDBlueWM = 0.0 ;
}
/* derivative for DW blue-greens in marsh water [gD_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNBlueWM = wNExchBlueM - tNSetBlueM / sDepthWM - aRelDeltaWM * sNBlueWM ;
} else {
   dNBlueWM = 0.0 ;
}
/* derivative for N blue-greens in marsh water [gN_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPBlueWM = wPExchBlueM - tPSetBlueM / sDepthWM - aRelDeltaWM * sPBlueWM ;
} else {
   dPBlueWM = 0.0 ;
}
/* derivative for P blue-greens in marsh water [gP_m-3_d-1] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   dDZooM = 0.0 ;
} else if (InclWeb == 1) {
   dDZooM = wDExchZooM - aRelDeltaWM * sDZooM ;
} else {
   dDZooM = 0.0 ;
}
/* derivative for DW zooplankton in marsh water [gD_m-3_d-1] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   dNZooM = 0.0 ;
} else if (InclWeb == 1) {
   dNZooM = wNExchZooM - aRelDeltaWM * sNZooM ;
} else {
   dNZooM = 0.0 ;
}
/* derivative for N zooplankton in marsh water [gN_m-3_d-1] */
if (0 == InclMarsh || fMarsh <= NearZero) {
   dPZooM = 0.0 ;
} else if (InclWeb == 1) {
   dPZooM = wPExchZooM - aRelDeltaWM * sPZooM ;
} else {
   dPZooM = 0.0 ;
}
/* derivative for P zooplankton in marsh water [gP_m-3_d-1] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNH4SM = tNInfNH4WM - tNInfNH4SM +(1.0-fRefrDetS) * tNMinDetSM + tNMinHumSM - tNDifNH4M - tNDifGroundNH4M - tNNitrSM - tNBurNH4M - tNUptNH4PhraS + tNEvNH4WM ;
} else {
   dNH4SM = 0.0 ;
}
/* derivative for N NH4 in water in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNO3SM = tNInfNO3WM - tNInfNO3SM + tNNitrSM - tNDenitSM - tNDifNO3M - tNDifGroundNO3M - tNBurNO3M - tNUptNO3PhraS + tNEvNO3WM ;
} else {
   dNO3SM = 0.0 ;
}
/* derivative for N NO3 in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPO4SM = tPInfPO4WM - tPInfPO4SM + tPEvPO4WM +(1.0-fRefrDetS) * tPMinDetSM + tPMinHumSM - tPSorpIMSM - tPDifPO4M - tPDifGroundPO4M - tPChemPO4M - tPUptPhraS - tPBurPO4M ;
} else {
   dPO4SM = 0.0 ;
}
/* derivative for P PO4 in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPAIMSM = tPSetAIMM - tPBurAIMM + tPSorpIMSM ;
} else {
   dPAIMSM = 0.0 ;
}
/* derivative for P P adsorbed onto IM in marsh sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDDetSM = tDMortRootPhra + tDSetDetM - tDMinDetSM + tDSetPhytM - tDBurDetM ;
} else {
   dDDetSM = 0.0 ;
}
/* derivative for DW Detritus in marsh sediment [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNDetSM = tNMortRootPhra + tNSetDetM - tNMinDetSM + tNSetPhytM - tNBurDetM ;
} else {
   dNDetSM = 0.0 ;
}
/* derivative for N detritus in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPDetSM = tPMortRootPhra + tPSetDetM - tPMinDetSM + tPSetPhytM - tPBurDetM ;
} else {
   dPDetSM = 0.0 ;
}
/* derivative for P detritus in marsh sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dSiDetSM = tSiSetDetM - tSiMinDetSM + cSiDDiat * tDSetDiatM - tSiBurDetM ;
} else {
   dSiDetSM = 0.0 ;
}
/* derivative for P detritus in marsh sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDHumSM = fRefrDetS * tDMinDetSM - tDMinHumSM - tDBurHumM ;
} else {
   dDHumSM = 0.0 ;
}
/* derivative for DW sediment humus in marsh sediment [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNHumSM = fRefrDetS * tNMinDetSM - tNMinHumSM - tNBurHumM ;
} else {
   dNHumSM = 0.0 ;
}
/* derivative for N sediment humus in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPHumSM = fRefrDetS * tPMinDetSM - tPMinHumSM - tPBurHumM ;
} else {
   dPHumSM = 0.0 ;
}
/* derivative for P sediment humus in marsh sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDIMSM = tDSetIMM - tDBurIMM ;
} else {
   dDIMSM = 0.0 ;
}
/* derivative for DW Inorg matter in sediment in marsh sediment [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDRootPhra = tDProdRootPhra - tDRespRootPhra - tDMortRootPhra - tDAllPhra + tDRealPhra ;
} else {
   dDRootPhra = 0.0 ;
}
/* derivative for DW biomass root reed in marsh sediment [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dDShootPhra = tDProdShootPhra - tDRespShootPhra - tDMortShootPhra + tDAllPhra - tDRealPhra - tDManShootPhra ;
} else {
   dDShootPhra = 0.0 ;
}
/* derivative for DW biomass shoot reed in marsh sediment [gD/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNRootPhra = tNUptRootPhra - tNMortRootPhra - tNTransPhra + tNRetrPhra ;
} else {
   dNRootPhra = 0.0 ;
}
/* derivative for N N in root in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dNShootPhra = tNUptShootPhra - tNMortShootPhra + tNTransPhra - tNRetrPhra - tNManShootPhra ;
} else {
   dNShootPhra = 0.0 ;
}
/* derivative for N N in shoot in marsh sediment [gN/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPRootPhra = tPUptRootPhra - tPMortRootPhra - tPTransPhra + tPRetrPhra ;
} else {
   dPRootPhra = 0.0 ;
}
/* derivative for P P in root in marsh sediment [gP/m2/d] */
if (InclMarsh == 1 && fMarsh > NearZero) {
   dPShootPhra = tPUptShootPhra - tPMortShootPhra + tPTransPhra - tPRetrPhra - tPManShootPhra ;
} else {
   dPShootPhra = 0.0 ;
}
/* derivative for P P in shoot in marsh sediment [gP/m2/d] */
dDExtTotT = uDLoad - wDOutflTot*sDepthW + wDTranZoo * sDepthW - tDBurTot + tDAbioTotT + tDPrimTotT + tDBedTotT + tDWebTotT + tDMarsTotT - tDDredNetTot ;
/* derivative for total external DW flux [gD/m2/d] */
dNExtTotT = uNLoad - wNOutflTot * sDepthW + wNTranZoo * sDepthW + cNBackLoad - tNBurTot + tNAbioTotT + tNPrimTotT + tNBedTotT + tNWebTotT + tNMarsTotT - tNDredNetTot ;
/* derivative for total external N flux [gN/m2/d] */
dPExtTotT = uPLoad - wPOutflTot * sDepthW + wPTranZoo * sDepthW + cPBackLoad - tPBurTot + tPAbioTotT + tPPrimTotT + tPBedTotT + tPWebTotT + tPMarsTotT - tPDredNetTot ;
/* derivative for total external P flux [gP/m2/d] */
dSiExtTotT = uSiLoad - wSiDilTot*sDepthW + tSiAbioTotT - tSiBurTot + tSiPrimTotT + tSiMarsTotT - tSiDredTot ;
/* derivative for total external Si flux [gSi/m2/d] */

/* ############################ */
/* Numerical Integration        */
/* ############################ */

k0(sNH4W) = dNH4W ;
/* derivative for N ammonium in water in lake water [mgN/l/d] */
k0(sNO3W) = dNO3W ;
/* derivative for N nitrate in water in lake water [mgN/l/d] */
k0(sPO4W) = dPO4W ;
/* derivative for P SRP in water in lake water [mgP/l/d] */
k0(sPAIMW) = dPAIMW ;
/* derivative for P P-adsorbed onto IM in water in lake water [mgP/l/d] */
k0(sSiO2W) = dSiO2W ;
/* derivative for Si dissolved silica in water in lake water [mgSi/l/d] */
k0(sO2W) = dO2W ;
/* derivative for O2 oxygen in water in lake water [gO2/m3/d] */
k0(sDDetW) = dDDetW ;
/* derivative for DW Detritus in lake water [mg/l/d] */
k0(sNDetW) = dNDetW ;
/* derivative for N Detritus in lake water [mgN/l/d] */
k0(sPDetW) = dPDetW ;
/* derivative for P Detritus in lake water [mgP/l/d] */
k0(sSiDetW) = dSiDetW ;
/* derivative for Si Detritus in lake water [mgSi/l/d] */
k0(sDIMW) = dDIMW ;
/* derivative for DW inorg matter in water in lake water [mg/l/d] */
k0(sDDiatW) = dDDiatW ;
/* derivative for DW Diatoms in lake water [mg/l/d] */
k0(sNDiatW) = dNDiatW ;
/* derivative for N Diatoms in lake water [mgN/l/d] */
k0(sPDiatW) = dPDiatW ;
/* derivative for P Diatoms in lake water [mgP/l/d] */
k0(sDGrenW) = dDGrenW ;
/* derivative for DW Greens in lake water [mg/l/d] */
k0(sNGrenW) = dNGrenW ;
/* derivative for N Greens in lake water [mgN/l/d] */
k0(sPGrenW) = dPGrenW ;
/* derivative for P Greens in lake water [mgP/l/d] */
k0(sDBlueW) = dDBlueW ;
/* derivative for DW Bluegreens in lake water [mg/l/d] */
k0(sNBlueW) = dNBlueW ;
/* derivative for N Bluegreens in lake water [mgN/l/d] */
k0(sPBlueW) = dPBlueW ;
/* derivative for P Bluegreens in lake water [mgP/l/d] */
k0(sDZoo) = dDZoo ;
/* derivative for DW Zooplankton in lake water [mg/l/d] */
k0(sNZoo) = dNZoo ;
/* derivative for N Zooplankton in lake water [mgN/l/d] */
k0(sPZoo) = dPZoo ;
/* derivative for P Zooplankton in lake water [mgP/l/d] */
k0(sDFiAd) = dDFiAd ;
/* derivative for DW Adult whitefish in lake water [gD/m2/d] */
k0(sDFiJv) = dDFiJv ;
/* derivative for DW Juvenile whitefish in lake water [gD/m2/d] */
k0(sNFiAd) = dNFiAd ;
/* derivative for N Adult whitefish in lake water [gN/m2/d] */
k0(sNFiJv) = dNFiJv ;
/* derivative for N Juvenile whitefish in lake water [gN/m2/d] */
k0(sPFiAd) = dPFiAd ;
/* derivative for P Adult whitefish in lake water [gP/m2/d] */
k0(sPFiJv) = dPFiJv ;
/* derivative for P Juvenile whitefish in lake water [gP/m2/d] */
k0(sDPisc) = dDPisc ;
/* derivative for DW predatory fish in lake water [g/m2/d] */
k0(sNH4S) = dNH4S ;
/* derivative for N Pore water ammonium in lake water [gN/m2/d] */
k0(sNO3S) = dNO3S ;
/* derivative for N Pore water nitrate in lake water [gN/m2/d] */
k0(sPO4S) = dPO4S ;
/* derivative for P Pore water SRP in lake water [gP/m2/d] */
k0(sPAIMS) = dPAIMS ;
/* derivative for P P-adsorbed onto IM in sediment in lake sediment [gP/m2/d] */
k0(sDDetS) = dDDetS ;
/* derivative for DW Sediment detritus in lake sediment [g/m2/d] */
k0(sNDetS) = dNDetS ;
/* derivative for N Sediment detritus N in lake sediment [gN/m2/d] */
k0(sPDetS) = dPDetS ;
/* derivative for P Sediment detritus P in lake sediment [gP/m2/d] */
k0(sSiDetS) = dSiDetS ;
/* derivative for Si Sediment detritus Si in lake sediment [gSi/m2/d] */
k0(sDHumS) = dDHumS ;
/* derivative for DW humus in lake sediment [gD/m2/d] */
k0(sNHumS) = dNHumS ;
/* derivative for N humus in lake sediment [gN/m2/d] */
k0(sPHumS) = dPHumS ;
/* derivative for P humus in lake sediment [gP/m2/d] */
k0(sDIMS) = dDIMS ;
/* derivative for DW inorg matter in sediment in lake sediment [gD/m2/d] */
k0(sDDiatS) = dDDiatS ;
/* derivative for DW Sed Diatoms in lake sediment [g/m2/d] */
k0(sNDiatS) = dNDiatS ;
/* derivative for N Sediment diatoms in lake sediment [gN/m2/d] */
k0(sPDiatS) = dPDiatS ;
/* derivative for P Sediment diatoms in lake sediment [gP/m2/d] */
k0(sDGrenS) = dDGrenS ;
/* derivative for DW Sed Greens in lake sediment [g/m2/d] */
k0(sNGrenS) = dNGrenS ;
/* derivative for N Sediment green algae in lake sediment [gN/m2/d] */
k0(sPGrenS) = dPGrenS ;
/* derivative for P Sediment green algae in lake sediment [gP/m2/d] */
k0(sDBlueS) = dDBlueS ;
/* derivative for DW Sed Blue-greens in lake sediment [g/m2/d] */
k0(sNBlueS) = dNBlueS ;
/* derivative for N Sediment blue-greens in lake sediment [gN/m2/d] */
k0(sPBlueS) = dPBlueS ;
/* derivative for P Sediment blue-greens in lake sediment [gP/m2/d] */
k0(sDVeg) = dDVeg ;
/* derivative for DW Vegetation in lake sediment [gD/m2/d] */
k0(sNVeg) = dNVeg ;
/* derivative for N Vegetation in lake sediment [gN/m2/d] */
k0(sPVeg) = dPVeg ;
/* derivative for P Vegetation in lake sediment [gP/m2/d] */
k0(sDBent) = dDBent ;
/* derivative for DW Zoobenthos in lake sediment [gD/m2/d] */
k0(sNBent) = dNBent ;
/* derivative for N Zoobenthos in lake sediment [gN/m2/d] */
k0(sPBent) = dPBent ;
/* derivative for P Zoobenthos in lake sediment [gP/m2/d] */
k0(sNH4WM) = dNH4WM ;
/* derivative for N NH4 in water in marsh water [gN/m3/d] */
k0(sNO3WM) = dNO3WM ;
/* derivative for N NO3 in water in marsh water [gN/m3/d] */
k0(sPO4WM) = dPO4WM ;
/* derivative for P PO4 in water in marsh water [gP/m3/d] */
k0(sPAIMWM) = dPAIMWM ;
/* derivative for P P adsorbed onto IM in water in marsh water [gP/m3/d] */
k0(sSiO2WM) = dSiO2WM ;
/* derivative for Si SiO2 in marsh water [gP/m3/d] */
k0(sO2WM) = dO2WM ;
/* derivative for O2 O2 in water in marsh water [gO2/m3/d] */
k0(sDDetWM) = dDDetWM ;
/* derivative for DW Detritus in marsh water [gD/m3/d] */
k0(sNDetWM) = dNDetWM ;
/* derivative for N detritus in marsh water [gN/m3/d] */
k0(sPDetWM) = dPDetWM ;
/* derivative for P detritus in marsh water [gP/m3/d] */
k0(sSiDetWM) = dSiDetWM ;
/* derivative for Si detritus in marsh water [gSi/m3/d] */
k0(sDIMWM) = dDIMWM ;
/* derivative for DW Inorg matter in marsh water [gD/m3/d] */
k0(sDDiatWM) = dDDiatWM ;
/* derivative for DW diatoms in marsh water [gD_m-3_d-1] */
k0(sNDiatWM) = dNDiatWM ;
/* derivative for N diatoms in marsh water [gN_m-3_d-1] */
k0(sPDiatWM) = dPDiatWM ;
/* derivative for P diatoms in marsh water [gP_m-3_d-1] */
k0(sDGrenWM) = dDGrenWM ;
/* derivative for DW greens in marsh water [gD_m-3_d-1] */
k0(sNGrenWM) = dNGrenWM ;
/* derivative for N greens in marsh water [gN_m-3_d-1] */
k0(sPGrenWM) = dPGrenWM ;
/* derivative for P greens in marsh water [gP_m-3_d-1] */
k0(sDBlueWM) = dDBlueWM ;
/* derivative for DW blue-greens in marsh water [gD_m-3_d-1] */
k0(sNBlueWM) = dNBlueWM ;
/* derivative for N blue-greens in marsh water [gN_m-3_d-1] */
k0(sPBlueWM) = dPBlueWM ;
/* derivative for P blue-greens in marsh water [gP_m-3_d-1] */
k0(sDZooM) = dDZooM ;
/* derivative for DW zooplankton in marsh water [gD_m-3_d-1] */
k0(sNZooM) = dNZooM ;
/* derivative for N zooplankton in marsh water [gN_m-3_d-1] */
k0(sPZooM) = dPZooM ;
/* derivative for P zooplankton in marsh water [gP_m-3_d-1] */
k0(sNH4SM) = dNH4SM ;
/* derivative for N NH4 in water in marsh sediment [gN/m2/d] */
k0(sNO3SM) = dNO3SM ;
/* derivative for N NO3 in marsh sediment [gN/m2/d] */
k0(sPO4SM) = dPO4SM ;
/* derivative for P PO4 in marsh sediment [gN/m2/d] */
k0(sPAIMSM) = dPAIMSM ;
/* derivative for P P adsorbed onto IM in marsh sediment [gP/m2/d] */
k0(sDDetSM) = dDDetSM ;
/* derivative for DW Detritus in marsh sediment [gD/m2/d] */
k0(sNDetSM) = dNDetSM ;
/* derivative for N detritus in marsh sediment [gN/m2/d] */
k0(sPDetSM) = dPDetSM ;
/* derivative for P detritus in marsh sediment [gP/m2/d] */
k0(sSiDetSM) = dSiDetSM ;
/* derivative for P detritus in marsh sediment [gP/m2/d] */
k0(sDHumSM) = dDHumSM ;
/* derivative for DW sediment humus in marsh sediment [gD/m2/d] */
k0(sNHumSM) = dNHumSM ;
/* derivative for N sediment humus in marsh sediment [gN/m2/d] */
k0(sPHumSM) = dPHumSM ;
/* derivative for P sediment humus in marsh sediment [gP/m2/d] */
k0(sDIMSM) = dDIMSM ;
/* derivative for DW Inorg matter in sediment in marsh sediment [gD/m2/d] */
k0(sDRootPhra) = dDRootPhra ;
/* derivative for DW biomass root reed in marsh sediment [gD/m2/d] */
k0(sDShootPhra) = dDShootPhra ;
/* derivative for DW biomass shoot reed in marsh sediment [gD/m2/d] */
k0(sNRootPhra) = dNRootPhra ;
/* derivative for N N in root in marsh sediment [gN/m2/d] */
k0(sNShootPhra) = dNShootPhra ;
/* derivative for N N in shoot in marsh sediment [gN/m2/d] */
k0(sPRootPhra) = dPRootPhra ;
/* derivative for P P in root in marsh sediment [gP/m2/d] */
k0(sPShootPhra) = dPShootPhra ;
/* derivative for P P in shoot in marsh sediment [gP/m2/d] */
k0(sDExtTotT) = dDExtTotT ;
/* derivative for total external DW flux [gD/m2/d] */
k0(sNExtTotT) = dNExtTotT ;
/* derivative for total external N flux [gN/m2/d] */
k0(sPExtTotT) = dPExtTotT ;
/* derivative for total external P flux [gP/m2/d] */
k0(sSiExtTotT) = dSiExtTotT ;
/* derivative for total external Si flux [gSi/m2/d] */
k0(time) = 1.0 ;
/* time [d] */

}

