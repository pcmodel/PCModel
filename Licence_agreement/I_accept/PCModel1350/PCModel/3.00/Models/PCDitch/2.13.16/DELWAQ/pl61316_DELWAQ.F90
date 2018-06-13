      subroutine pclk       ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'pclk_' :: pclk_
!
!*******************************************************************************
!
      IMPLICIT NONE
!
   !   
   !     Type    Name          I/O Description 
   !   
         real(4) pmsa(*)      !I/O Process Manager System Array, window of routine to process library  
         real(4) fl(*)        ! O  Array of fluxes made by this process in mass/volume/time    
         integer ipoint(2035) ! I  Array of pointers in pmsa to get and store the data 
         integer increm(2035) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying  
         integer noseg        ! I  Number of computational elements in the whole model schematisation  
         integer noflux       ! I  Number of fluxes, increment in the fl array 
         integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces  
         integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use   
         integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh) 
         integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid   
         integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward 
         integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)  
         integer ipnt(2035)   !    Local work array for the pointering 
         integer iseg         !    Local loop counter for computational element loop   
!
!*******************************************************************************
!
!     Type    Name          I/O Description                                        Unit
!
!     /******************************************************************************/
!     /*                                                                            */
!     /* Eutrophication model pclake pl61316 for DELWAQ                            */
!     /*                                                                            */
!     /*                                                                            */
!     /*    A.B.G. Janssen                                                          */
!     /*    NIOO KNAW                                                               */
!     /*    Droevendaalsesteeg 10                                                   */
!     /*    PO Box 50                                                               */
!     /*    6700 AB  Wageningen                                                     */
!     /*                                                                            */
!     /* PCLake v50900 (PCLK50900) is based on PCLake v50900 by J.H. Janse (pbl)    */
!     /*                                                                            */
!     /******************************************************************************/
!
!
!     /* ==============================  */
!     /* declaration of state variables  */
!     /* ==============================  */
      real(4) sNH4W          	! [      1.00E-01 ]	gN/m3		N_in_NH4_in_lake_water
      real(4) sNO3W          	! [      1.00E-01 ]	gN/m3		N_in_NO3_in_lake_water
      real(4) sPO4W          	! [      1.00E-02 ]	gP/m3		P_in_PO4_in_lake_water
      real(4) sPAIMW         	! [             0 ]	gP/m3		P_adsorbed_onto_IM_in_lake_water
      real(4) sSiO2W         	! [             3 ]	gSi/m3		Dissolved_Si_in_lake_water
      real(4) sO2W           	! [            10 ]	gO2/m3		Oxygen_in_lake_water
      real(4) sDDetW         	! [             2 ]	gDW/m3		Detritus_DW_in_lake_water
      real(4) sNDetW         	! [      5.00E-02 ]	gN/m3		Detritus_N_in_lake_water
      real(4) sPDetW         	! [      5.00E-03 ]	gP/m3		Detritus_P_in_lake_water
      real(4) sSiDetW        	! [      2.00E-02 ]	gSi/m3		Detritus_Si_in_lake_water
      real(4) sDIMW          	! [             5 ]	gDIM/m3		Inorganic_matter_in_lake_water
      real(4) sDDiatW        	! [           0.5 ]	gDW/m3		Diatoms_DW_in_lake_water
      real(4) sNDiatW        	! [      5.00E-02 ]	gN/m3		Diatoms_N_in_lake_water
      real(4) sPDiatW        	! [      5.00E-03 ]	gP/m3		Diatoms_P_in_lake_water
      real(4) sDGrenW        	! [           0.5 ]	gDW/m3		Green_algae_DW_in_lake_water
      real(4) sNGrenW        	! [      5.00E-02 ]	gN/m3		Green_algae_N_in_lake_water
      real(4) sPGrenW        	! [      5.00E-03 ]	gP/m3		Green_algae_P_in_lake_water
      real(4) sDBlueW        	! [             3 ]	gDW/m3		Blue-greens_DW_in_lake_water
      real(4) sNBlueW        	! [      3.00E-01 ]	gN/m3		Blue-greens_N_in_lake_water
      real(4) sPBlueW        	! [      3.00E-02 ]	gP/m3		Blue-greens_P_in_lake_water
      real(4) sDZoo          	! [      5.00E-02 ]	gDW/m3		Zooplankton_DW_in_lake_water
      real(4) sNZoo          	! [      3.50E-03 ]	gN/m3		Zooplankton_N_in_lake_water
      real(4) sPZoo          	! [      5.00E-04 ]	gP/m3		Zooplankton_P_in_lake_water
      real(4) sDFiAd         	! [             2 ]	gDW/m2		Adult_fish_DW_in_lake_water
      real(4) sDFiJv         	! [           0.5 ]	gDW/m2		Young_fish_DW_in_lake_water
      real(4) sNFiAd         	! [      2.00E-01 ]	gN/m2		Adult_fish_N_in_lake_water
      real(4) sNFiJv         	! [      5.00E-02 ]	gN/m2		Young_fish_N_in_lake_water
      real(4) sPFiAd         	! [      4.40E-02 ]	gP/m2		Adult_fish_P_in_lake_water
      real(4) sPFiJv         	! [      1.10E-02 ]	gP/m2		Young_fish_P_in_lake_water
      real(4) sDPisc         	! [      1.00E-02 ]	gDW/m2		Predatory_fish_DW_in_lake_water
      real(4) sNH4S          	! [      2.00E-02 ]	gN/m2		N_in_NH4_in_lake_sediment_pore_water
      real(4) sNO3S          	! [      2.00E-03 ]	gN/m2		N_in_NO3_in_lake_sediment_pore_water
      real(4) sPO4S          	! [      1.82E-01 ]	gP/m2		P_in_PO4_in_lake_sediment_pore_water
      real(4) sPAIMS         	! [      1.80E+01 ]	gP/m2		P_adsorbed_onto_IM_in_lake_sediment
      real(4) sDDetS         	! [      1.82E+02 ]	gDW/m2		Detritus_DW_in_lake_sediment
      real(4) sNDetS         	! [      4.54E+00 ]	gN/m2		Detritus_N_in_lake_sediment
      real(4) sPDetS         	! [      4.54E-01 ]	gP/m2		Detritus_P_in_lake_sediment
      real(4) sSiDetS        	! [      1.82E+00 ]	gSi/m2		Detritus_Si_in_lake_sediment
      real(4) sDHumS         	! [      3.45E+03 ]	gDW/m2		Humus_DW_in_lake_sediment
      real(4) sNHumS         	! [      1.73E+02 ]	gN/m2		Humus_N_in_lake_sediment
      real(4) sPHumS         	! [      1.73E+01 ]	gP/m2		Humus_P_in_lake_sediment
      real(4) sDIMS          	! [      3.27E+04 ]	gDIM/m2		Inorganic_matter_in_lake_sediment
      real(4) sDDiatS        	! [      1.00E-03 ]	gDW/m2		Diatoms_DW_on_lake_sediment
      real(4) sNDiatS        	! [      1.00E-04 ]	gN/m2		Diatoms_N_on_lake_sediment
      real(4) sPDiatS        	! [      1.00E-05 ]	gP/m2		Diatoms_P_on_lake_sediment
      real(4) sDGrenS        	! [      1.00E-03 ]	gDW/m2		Green_algae_DW_on_lake_sediment
      real(4) sNGrenS        	! [      1.00E-04 ]	gN/m2		Green_algae_N_on_lake_sediment
      real(4) sPGrenS        	! [      1.00E-05 ]	gP/m2		Green_algae_P_on_lake_sediment
      real(4) sDBlueS        	! [      1.00E-03 ]	gDW/m2		Blue-greens_DW_on_lake_sediment
      real(4) sNBlueS        	! [      1.00E-04 ]	gN/m2		Blue-greens_N_on_lake_sediment
      real(4) sPBlueS        	! [      1.00E-05 ]	gP/m2		Blue-greens_P_on_lake_sediment
      real(4) sDVeg          	! [             1 ]	gDW/m2		Vegetation_DW_in_lake_water
      real(4) sNVeg          	! [      2.00E-02 ]	gN/m2		Vegetation_N_in_lake_water
      real(4) sPVeg          	! [      2.00E-03 ]	gP/m2		Vegetation_P_in_lake_water
      real(4) sDBent         	! [             1 ]	gDW/m2		Zoobenthos_DW_in_lake_sediment
      real(4) sNBent         	! [      7.00E-02 ]	gN/m2		Zoobenthos_N_in_lake_sediment
      real(4) sPBent         	! [      1.00E-02 ]	gP/m2		Zoobenthos_P_in_lake_sediment
      real(4) sDepthWM       	! [           0.5 ]	m		Depth_of_marsh_water
      real(4) sNH4WM         	! [      1.00E-01 ]	gN/m3		N_in_NH4_in_marsh_water
      real(4) sNO3WM         	! [      1.00E-01 ]	gN/m3		N_in_NO3_in_marsh_water
      real(4) sPO4WM         	! [      1.00E-02 ]	gP/m3		P_in_PO4_in_marsh_water
      real(4) sPAIMWM        	! [             0 ]	gP/m3		P_adsorbed_onto_IM_in_marsh_water
      real(4) sSiO2WM        	! [             3 ]	gSi/m3		Dissolved_Si_in_marsh_water
      real(4) sO2WM          	! [            10 ]	gO2/m3		Oxygen_in_marsh_water
      real(4) sDDetWM        	! [             2 ]	gDW/m3		Detritus_DW_in_marsh_water
      real(4) sNDetWM        	! [      5.00E-02 ]	gN/m3		Detritus_N_in_marsh_water
      real(4) sPDetWM        	! [      5.00E-03 ]	gP/m3		Detritus_P_in_marsh_water
      real(4) sSiDetWM       	! [      2.00E-02 ]	gSi/m3		Detritus_Si_in_marsh_water
      real(4) sDIMWM         	! [             5 ]	gDIM/m3		Inorganic_matter_in_marsh_water
      real(4) sDDiatWM       	! [           0.5 ]	gDW/m3		Diatoms_DW_in_marsh_water
      real(4) sNDiatWM       	! [      5.00E-02 ]	gN/m3		Diatoms_N_in_marsh_water
      real(4) sPDiatWM       	! [      5.00E-03 ]	gP/m3		Diatoms_P_in_marsh_water
      real(4) sDGrenWM       	! [           0.5 ]	gDW/m3		Green_algae_DW_in_marsh_water
      real(4) sNGrenWM       	! [      5.00E-02 ]	gN/m3		Green_algae_N_in_marsh_water
      real(4) sPGrenWM       	! [      5.00E-03 ]	gP/m3		Green_algae_P_in_marsh_water
      real(4) sDBlueWM       	! [             3 ]	gDW/m3		Blue-greens_DW_in_marsh_water
      real(4) sNBlueWM       	! [      3.00E-01 ]	gN/m3		Blue-greens_N_in_marsh_water
      real(4) sPBlueWM       	! [      3.00E-02 ]	gP/m3		Blue-greens_P_in_marsh_water
      real(4) sDZooM         	! [      5.00E-02 ]	gDW/m3		Zooplankton_DW_in_marsh_water
      real(4) sNZooM         	! [      3.50E-03 ]	gN/m3		Zooplankton_N_in_marsh_water
      real(4) sPZooM         	! [      5.00E-04 ]	gP/m3		Zooplankton_P_in_marsh_water
      real(4) sNH4SM         	! [             1 ]	gN/m2		N_in_NH4_in_marsh_sediment_pore_water
      real(4) sNO3SM         	! [      1.00E-02 ]	gN/m2		N_in_NO3_in_marsh_sediment_pore_water
      real(4) sPO4SM         	! [      1.82E-01 ]	gP/m2		P_in_PO4_in_marsh_sediment_pore_water
      real(4) sPAIMSM        	! [      1.80E+01 ]	gP/m2		P_adsorbed_onto_IM_in_marsh_sediment
      real(4) sDDetSM        	! [      1.82E+02 ]	gDW/m2		Detritus_DW_in_marsh_sediment
      real(4) sNDetSM        	! [      4.54E+00 ]	gN/m2		Detritus_N_in_marsh_sediment
      real(4) sPDetSM        	! [      4.54E-01 ]	gP/m2		Detritus_P_in_marsh_sediment
      real(4) sSiDetSM       	! [      1.82E+00 ]	gSi/m2		Detritus_Si_in_marsh_sediment
      real(4) sDHumSM        	! [      3.45E+03 ]	gDW/m2		Humus_DW_in_marsh_sediment
      real(4) sNHumSM        	! [      1.73E+02 ]	gN/m2		Humus_N_in_marsh_sediment
      real(4) sPHumSM        	! [      1.73E+01 ]	gP/m2		Humus_Pin_marsh_sediment
      real(4) sDIMSM         	! [      3.27E+04 ]	gDIM/m2		Inorganic_matter_in_marsh_sediment
      real(4) sDRootPhra     	! [          5000 ]	gDW/m2		Root_biomass_DW_in_marsh_sediment
      real(4) sDShootPhra    	! [          1000 ]	gDW/m2		Shoot_biomass_DW_in_marsh_water
      real(4) sNRootPhra     	! [      1.00E+02 ]	gN/m2		Root_biomass_N_in_marsh_sediment
      real(4) sNShootPhra    	! [      2.00E+01 ]	gN/m2		Shoot_biomass_N_in_marsh_water
      real(4) sPRootPhra     	! [      1.00E+01 ]	gP/m2		Root_biomass_P_in_marsh_sediment
      real(4) sPShootPhra    	! [      2.00E+00 ]	gP/m2		Shoot_biomass_P_in_marsh_water
      real(4) sDExtTotT      	! [      3.64E+04 ]	gDW/m2		Total_amount_of_DW_moved_into_or_out_from_the_system
      real(4) sNExtTotT      	! [      1.79E+02 ]	gN/m2		Total_amount_of_N_moved_into_or_out_from_the_system
      real(4) sPExtTotT      	! [      3.61E+01 ]	gP/m2		Total_amount_of_P_moved_into_or_out_from_the_system
      real(4) sSiExtTotT     	! [      8.01E+00 ]	gSi/m2		Total_amount_of_Si_moved_into_or_out_from_the_system
!
!
!     /* ==============================  */
!     /* declaration parameters          */
!     /* ==============================  */
      real(4) ITIME                     ! [           0]	sec                     Time of DELWAQ
      real(4) TotalDepth                ! [           0]	m                       depth of DELWAQ
      real(4) InitCalc            	! [           0]	-                   	If_T_skip_calculation_of_initial_values_used_in_case_of_REINIT_command
      real(4) ConstDepth          	! [           1]	-                   	If_T_water_depth_kept_constant_by_daily_dredging
      real(4) InclTran            	! [           0]	-                   	transport_processes
      real(4) InclPhytS           	! [           1]	-                   	Include_phytoplankton_module
      real(4) InclBed             	! [           1]	-                   	Include_vegetation_module
      real(4) InclWeb             	! [           1]	-                   	Include_food_web_module
      real(4) InclMarsh           	! [           0]	-                   	Include_marsh_zone
      real(4) InclSeason          	! [           1]	-                   	Include_season
      real(4) ReadTemp            	! [           0]	-                   	If_TRUE_use_measured_time-series_of_water_temperature_otherwise_sinus
      real(4) ReadLOut            	! [           0]	-                   	If_TRUE_use_measured_time-series_of_light_otherwise_sinus
      real(4) ReadVWind           	! [           0]	-                   	If_TRUE_use_measured_time-series_of_wind_otherwise_constant
      real(4) ReadQIn             	! [           0]	-                   	If_TRUE_use_measured_time-series_of_inflow_otherwise_constant
      real(4) ReadQOut            	! [           0]	-                   	If_TRUE_use_measured_time-series_of_inflow_otherwise_constant
      real(4) ReadQEv             	! [           0]	-                   	If_TRUE_use_measured_time-series_of_evaporation_otherwise_constant
      real(4) ReadPLoad           	! [           0]	-                   	If_TRUE_use_measured_time-series_of_P_loading_otherwise_constant
      real(4) ReadNLoad           	! [           0]	-                   	If_TRUE_use_measured_time-series_of_N_loading_otherwise_constant
      real(4) ReadNutFrac         	! [           0]	-                   	If_TRUE_use_measured_time-series_of_loading_with_diff_nutrient_fractions
      real(4) ReadPLoadPhyt       	! [           0]	-                   	If_TRUE_use_measured_time-series_of_P_loading_algal_input_otherwise_constant
      real(4) ReadDLoadDet        	! [           0]	-                   	If_TRUE_use_measured_time-series_of_DDet_loading_otherwise_constant
      real(4) ReadDLoadIM         	! [           0]	-                   	If_TRUE_use_measured_time-series_of_DIM_loading_otherwise_constant
      real(4) UseSeasonLoad       	! [           0]	-                   	If_TRUE_use_different_inflow_and_loading_for_summer_and_winter_periods
      real(4) UsePulseLoad        	! [           0]	-                   	If_TRUE_use_a_pulse-wise_nutrient_loading
      real(4) mTemp               	! [           0]	oC                  	measured_time-series_of_water_temperature
      real(4) mLOut               	! [           0]	W/m2                	measured_time-series_of_light
      real(4) mVWind              	! [           0]	m/s                 	measured_time-series_of_wind
      real(4) mQIn                	! [           0]	mm/day              	use_measured_time-series_of_inflow
      real(4) mQOut               	! [           0]	mm/day              	use_measured_time-series_of_outflow
      real(4) mQEv                	! [           0]	mm/day              	use_measured_time-series_of_evaporation
      real(4) mPLoad              	! [           0]	gP/m2/day           	use_measured_time-series_of_P_loading
      real(4) mPLoadPO4           	! [           0]	gP/m2/day           	use_measured_time-series_of_PO4_loading
      real(4) mPLoadOrg           	! [           0]	gP/m2/day           	use_measured_time-series_of_loading_P_bound_to_org_matter
      real(4) mPLoadPhytTot       	! [           0]	gP/m2/day           	use_measured_time-series_of_P_loading_algal_input
      real(4) mNLoad              	! [           0]	gN/m2/day           	use_measured_time-series_of_N_loading
      real(4) mNLoadNH4           	! [           0]	gN/m2/day           	use_measured_time-series_of_NH4_loading
      real(4) mNLoadNO3           	! [           0]	gN/m2/day           	use_measured_time-series_of_NO3_loading
      real(4) mNLoadOrg           	! [           0]	gN/m2/day           	use_measured_time-series_of_loading_N_bound_to_org_matter
      real(4) mDLoadDet           	! [           0]	gDW/m2/day          	use_measured_time-series_of_Detritus_loading
      real(4) mDLoadIM            	! [           0]	gDW/m2/day          	use_measured_time-series_of_loading_of_DW_of_inorg_matter
      real(4) BeginTime           	! [           0]	day                 	begintime
      real(4) EndTime             	! [         365]	day                 	(=1_year)
      real(4) YearZero            	! [           0]	-                   	Note_also__Day_no_1_=_1_Jan_of_this_year
      real(4) cFetch              	! [        1000]	m                   	wind_fetch
      real(4) fMarsh              	! [           0]	m2_marsh/m2_lake    	relative_marsh_area
      real(4) fLutum              	! [         0.1]	-                   	lutum_content_of_inorg_matter
      real(4) fFeDIM              	! [        0.01]	gFe/gDW             	Fe_content_of_inorg_matter
      real(4) fAlDIM              	! [        0.01]	gAl/gDW             	Al_content_of_inorg_matter
      real(4) cTmAve              	! [          12]	oC                  	average_water_temperature
      real(4) cTmVar              	! [          10]	oC                  	annual_water_temperature_variation
      real(4) cTimeLag            	! [          40]	day                 	time_lag_for_temperature
      real(4) cVWind              	! [           5]	m/s                 	average_wind_speed
      real(4) cQInf               	! [           0]	mm/day              	infiltration_rate
      real(4) cPBackLoad          	! [           0]	gP/m2/d             	Background_P_loading
      real(4) cNBackLoad          	! [           0]	gP/m2/d             	Background_N_loading
      real(4) cLDayAve            	! [    1.00E+07]	J/m2/day            	annual_average_radiation
      real(4) cLDayVar            	! [     8000000]	J/m2/day            	annual_variation_in_radiation
      real(4) cfDayAve            	! [         0.5]	-                   	average_day_length
      real(4) cfDayVar            	! [         0.2]	-                   	annual_variation_in_day_length
      real(4) fRefl               	! [         0.2]	-                   	the_fraction_photosynthetically_active_radiation_reflected_at_the_surface
      real(4) cExtWat             	! [         0.5]	m-1                 	background_extinction
      real(4) cDredInterval       	! [     9999000]	y                   	dredging_interval
      real(4) cDredStart          	! [     9999000]	y                   	first_dredging_year_(should_be_n_times__cDredInterval_)
      real(4) cDepthRef           	! [       1E-28]	m                   	reference_water_depth_for_dredging
      real(4) cLengDred           	! [          10]	day                 	length_of_dredging_period
      real(4) fEffDred            	! [        0.95]	-                   	dredging_efficiency_(<10)
      real(4) fEffDredBent        	! [         0.5]	-                   	dredging_efficiency_for_zoobenthos_(<10)
      real(4) fPAR                	! [        0.48]	-                   	fraction_photosynthetically_active_radiation_(PAR)
      real(4) cExtSpDet           	! [        0.15]	m2/gDW              	specific_extinction_detritus
      real(4) cExtSpIM            	! [        0.05]	m2/gDW              	specific_extinction_inert_matter
      real(4) fDTotS0             	! [         0.3]	g_solid/g_sediment  	initial_dry-weight_fraction_in_sediment
      real(4) fDOrgS0             	! [         0.1]	g/g                 	initial_organic_fraction_of_sediment_DW
      real(4) fDDetS0             	! [        0.05]	g/g                 	initial_detritus_fraction_of_sediment_organic_matter
      real(4) fSedPhyt0           	! [        0.01]	g/g                 	Fraction_diatoms_DW_on_lake_sediment
      real(4) fPInorgS0           	! [      0.0005]	gP/gDW              	initial_inorg_P_fraction_in_sed
      real(4) fPAdsS0             	! [        0.99]	-                   	initial_adsorbed_fraction_of_inorg_P_in_sed
      real(4) cPDDet0             	! [      0.0025]	gP/gDW_Detritus     	initial_P_fraction_in_detritus
      real(4) cNDDet0             	! [       0.025]	gN/gDW_Detritus     	initial_N_fraction_in_detritus
      real(4) cSiDDet0            	! [        0.01]	gSi/gDW_Detritus    	initial_Si_fraction_in_detritus_Tentative
      real(4) cPDHum0             	! [       0.005]	gP/gDW_Detritus     	initial_P_fraction_in_humus
      real(4) cNDHum0             	! [        0.05]	gN/gDW_Detritus     	initial_N_fraction_in_humus
      real(4) cPDPhyt0            	! [        0.01]	gP/gDW              	initial_P_fraction_in_algae
      real(4) cNDPhyt0            	! [         0.1]	gN/gDW              	initial_N_fraction_in_algae
      real(4) cPDDiat0            	! [        0.01]	gP/gDW              	initial_P_fraction_in_diatoms
      real(4) cNDDiat0            	! [         0.1]	gN/gDW              	initial_N_fraction_in_diatoms
      real(4) cPDGren0            	! [        0.01]	gP/gDW              	initial_P_fraction_in_green_algae
      real(4) cNDGren0            	! [         0.1]	gN/gDW              	initial_N_fraction_in_green_algae
      real(4) cPDBlue0            	! [        0.01]	gP/gDW              	initial_P_fraction_in_blue-green_algae
      real(4) cNDBlue0            	! [         0.1]	gN/gDW              	initial_N_fraction_in_blue-green_algae
      real(4) cPDVeg0             	! [       0.002]	gP/gDW              	initial_P_fraction_in_veg
      real(4) cNDVeg0             	! [        0.02]	gN/gDW              	initial_N_fraction_in_veg
      real(4) cSiDDiat            	! [        0.15]	mgSi/mgDW           	Si/DW_ratio_of_daitoms
      real(4) cPDZooRef           	! [        0.01]	mgP/mgDW            	reference_P/C-ratio_herb_zooplankton
      real(4) cNDZooRef           	! [        0.07]	mgN/mgDW            	reference_N/C-ratio_herb_zooplankton
      real(4) cPDBentRef          	! [        0.01]	mgP/mgDW            	reference_P/C_ratio_of_zoobenthos
      real(4) cNDBentRef          	! [        0.07]	mgN/mgDW            	reference_N/C_ratio_of_zoobenthos
      real(4) cPDFishRef          	! [       0.022]	mgP/mgDW            	reference_P/C_ratio_of_Fish
      real(4) cNDFishRef          	! [         0.1]	mgN/mgDW            	reference_N/C_ratio_of_Fish
      real(4) cPDPisc             	! [       0.022]	mgP/mgDW            	reference_P/C_ratio_of__Pi_sc
      real(4) cNDPisc             	! [         0.1]	mgN/mgDW            	reference_N/C_ratio_of__Pi_sc
      real(4) cQIn                	! [          20]	mm/day              	standard_water_inflow_if_not_measured
      real(4) cQInSum             	! [          20]	mm/day              	summer_water_inflow_if_not_measured
      real(4) cQInWin             	! [          20]	mm/day              	winter_water_inflow_if_not_measured
      real(4) cDepthWMax          	! [           5]	m                   	maximum_water_depth
      real(4) cQInExtraApril1     	! [           0]	mm/day              	extra_inflow_at_start_of_summer
      real(4) cQInExtraOct1       	! [           0]	mm/day              	extra_inflow_at_start_of_winter
      real(4) cQOutExtraApril1    	! [           0]	mm/day              	extra_outflow_at_start_of_summer
      real(4) cQOutExtraOct1      	! [           0]	mm/day              	extra_outflow_at_start_of_winter
      real(4) cQEvAve             	! [         1.5]	mm/day              	standard_average_evaporation
      real(4) cQEvVar             	! [         1.3]	mm/day              	standard_variation_in_evaporation
      real(4) cPLoad              	! [       0.005]	gP/m2/day           	standard_P_loading_if_not_measured
      real(4) cPLoadSum           	! [       0.005]	gP/m2/day           	summer_P_loading_if_not_measured
      real(4) cPLoadWin           	! [       0.005]	gP/m2/day           	winter_P_loading_if_not_measured
      real(4) fPO4In              	! [         0.5]	-                   	fraction_PO4_in_input_(if_PO4_input_not_measured)
      real(4) fPhytInWin          	! [        0.02]	-                   	minimum_algal_fraction_in_organic_P_input
      real(4) fPhytInSum          	! [         0.1]	-                   	maximum_algal_fraction_in_organic_P_input
      real(4) fDiatPhytIn         	! [        0.33]	-                   	diatoms_fraction_of_algal_input
      real(4) fGrenPhytIn         	! [        0.34]	-                   	greens_fraction_of_algal_input
      real(4) fBluePhytIn         	! [        0.33]	-                   	blue-greens_fraction_of_algal_input
      real(4) cNLoad              	! [        0.05]	gN/m2/day           	standard_N_loading
      real(4) cNLoadSum           	! [        0.05]	gN/m2/day           	summer_N_loading
      real(4) cNLoadWin           	! [        0.05]	gN/m2/day           	winter_N_loading
      real(4) cNPLoadMeas         	! [           7]	gN/gP               	N/P_loading_if_P_is_measured_and_N_not
      real(4) cNPPhytIn           	! [           7]	gP/gDW              	N/P_ratio_of_algal_input
      real(4) cNPDetIn            	! [           7]	gP/gDW              	N/P_ratio_of_detrital_input
      real(4) fNH4DissIn          	! [         0.5]	-                   	NH4_fraction_of_dissolved_N_load_(if_NH4_not_measured)
      real(4) cNDPhytIn           	! [        0.07]	gN/gDW              	N/day_ratio_of_algal_input
      real(4) cNDDetIn            	! [        0.07]	gN/gDW              	N/P_ratio_of_detrital_input
      real(4) cDIMIn              	! [           5]	mgDW/l              	IM_conc_in_inflow
      real(4) cO2In               	! [           5]	mgO2/l              	O2_conc_in_inflow
      real(4) cSiO2In             	! [           3]	mgSi/l              	SiO2_conc_in_inflow
      real(4) cSiDDetIn           	! [        0.05]	gSi/gDW             	Si_content_of_sediment_detritus
      real(4) cDZooIn             	! [         0.1]	mgDW/l              	zoopl_conc_in_inflowing_water
      real(4) cDayApril1          	! [          91]	day                 	April_1
      real(4) cDayOct1            	! [         273]	day                 	October_1
      real(4) cLengChange         	! [          10]	day                 	length_of_season_change
      real(4) cNLoadS             	! [           0]	gN/m2/day           	N_fertilizer_to_sediment
      real(4) fNH4LoadS           	! [         0.5]	-                   	NH4_fraction_of_N_fertilizer_to_sediment
      real(4) cDErosTot           	! [         0.1]	g/m2/day            	Erosion_input_(tentative)
      real(4) fSedErosIM          	! [        0.95]	-                   	instantly_sedimentating_fraction_of_IM
      real(4) fDOrgSoil           	! [         0.1]	-                   	fraction_soil_organic_matter
      real(4) cPDSoilOM           	! [       0.001]	gP/gDW              	P/day_ratio_of_soil_organic_matter
      real(4) cNDSoilOM           	! [        0.01]	gN/gDW              	N/day_ratio_of_soil_organic_matter
      real(4) cPO4Ground          	! [         0.1]	mgP/l               	PO4_cone_in_groundwater
      real(4) cNH4Ground          	! [           1]	mgN/l               	NH4_cone_in_groundwater
      real(4) cNO3Ground          	! [         0.1]	mgN/l               	NO3_cone_in_groundwater
      real(4) cDepthS             	! [         0.1]	m                   	sediment_depth
      real(4) cCPerDW             	! [         0.4]	gC/gDW              	C_content_of_organic_matter
      real(4) cRhoIM              	! [     2500000]	g/m3_solid          	density_of_sediment_IM
      real(4) cRhoOM              	! [     1400000]	g/m3                	density_of_sediment_detritus
      real(4) cTmRef              	! [          20]	oC                  	reference_temperature
      real(4) cAerRoot            	! [       0.727]	-                   	coefficient_for_VWind^05
      real(4) cAerLin             	! [      -0.371 ]	s/day               	coefficient_for_VWind_(is_negative)
      real(4) cAerSquare          	! [      0.0376]	-                   	coefficient_for_VWind^2
      real(4) cThetaAer           	! [       1.024]	1/e^oC              	Temperature_coeff_for_reaeration_(Downing_&_Truesdale_1955)
      real(4) cVSetIM             	! [           1]	m/day               	max_sedimentation_velocity_of_inert_org_matter_(10)
      real(4) cVSetDet            	! [        0.25]	m/day               	max_sedimentation_velocity_of_detritus
      real(4) cThetaSet           	! [        1.01]	1/e^oC              	temp_parameter_of_sedimentation
      real(4) cSuspMin            	! [         6.1]	-                   	minimum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSuspMax            	! [        25.2]	-                   	maximum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSuspSlope          	! [         2.1]	-                   	slope_of_logistic_empirical_suspended_matter_function
      real(4) hDepthSusp          	! [           2]	-                   	half_sat_value_of_depth_in_logistic_empirical_suspended_matter_function
      real(4) cFetchRef           	! [        1000]	m                   	reference_fetch
      real(4) fLutumRef           	! [         0.2]	-                   	reference_lutum_fraction_(of_sandy_clay_soils)
      real(4) cSuspRef            	! [         0.5]	-                   	reference_supsended_matter_function
      real(4) kVegResus           	! [        0.01]	m2/gDW              	rel_resuspension_reduction_per_g_vegetation
      real(4) kTurbFish           	! [           1]	g/g_fish/day        	relative_resuspension_by_adult_fish_browsing
      real(4) kResusPhytMax       	! [        0.25]	day-1               	max_phytopl_resuspension
      real(4) cResusPhytExp       	! [      -0.379 ]	(gDW/m2/day)-1      	exp_par_for_phytopl_resuspension
      real(4) cThetaMinW          	! [        1.07]	-                   	expon_temp_constant_of_mineralization_in_water
      real(4) kDMinDetW           	! [        0.01]	day-1               	decomposition_constant_of_detritus
      real(4) hO2BOD              	! [           1]	mgO2/l              	half-sat_oxygen_conc_for_BOD
      real(4) O2PerNO3            	! [         1.5]	-                   	mol_O2_formed_per_mol_NO3-_ammonified
      real(4) cThetaMinS          	! [        1.07]	-                   	expon_temp_constant_of_sediment_mineralization
      real(4) kDMinDetS           	! [       0.002]	day-1               	decomposition_constant_of_sediment_detritus
      real(4) fRefrDetS           	! [        0.15]	-                   	refractory_fraction_of_sed_detritus
      real(4) hNO3Denit           	! [           2]	mgN/l               	quadratic_half-sat_NO3_conc_for_denitrification
      real(4) NO3PerC             	! [         0.8]	-                   	mol_NO3_denitrified_per_mol_C_mineralised
      real(4) kDMinHum            	! [     0.00001]	day-1               	maximum_decomposition_constant_of_humic_material_(1D-5)
      real(4) kNitrW              	! [         0.1]	day-1               	nitrification_rate_constant_in_water
      real(4) kNitrS              	! [           1]	day-1               	nitrification_rate_constant_in_sediment
      real(4) cThetaNitr          	! [        1.08]	1/eoC               	temperature_coefficient_of_nitrification
      real(4) O2PerNH4            	! [           2]	-                   	mol_O2_used_per_mol_NH4+_nitrified
      real(4) hO2Nitr             	! [           2]	mgO2/l              	half-sat_O2_conc_for_nitrification_in_water
      real(4) kPDifPO4            	! [    7.20E-05]	m2/day              	mol_PO4_diffusion_constant
      real(4) kNDifNO3            	! [    8.60E-05]	m2/day              	mol_NO3_diffusion_constant
      real(4) kNDifNH4            	! [    1.12E-04]	m2/day              	mol_NH4_diffusion_constant
      real(4) kO2Dif              	! [    2.60E-05]	m2/day              	mol_O2_diffusion_constant
      real(4) cThetaDif           	! [        1.02]	1/eoC               	Temperature_coefficient_for_diffusion
      real(4) fDepthDifS          	! [         0.5]	-                   	nutrient_diffusion_distance_as_fraction_of_sediment_depth
      real(4) cTurbDifNut         	! [           5]	-                   	bioturbation_factor_for_diffusion
      real(4) cTurbDifO2          	! [           5]	-                   	bioturbation_factor_for_diffusion
      real(4) kPSorp              	! [        0.05]	day-1               	P_sorption_rate_constant_not_too_high_->_model_speed
      real(4) cRelPAdsD           	! [     0.00003]	gP/gDW              	max_P_adsorption_per_g_DW
      real(4) cRelPAdsFe          	! [       0.065]	gP/gFe              	max_P_adsorption_per_g_Fe
      real(4) cRelPAdsAl          	! [       0.134]	gP/gAl              	max_P_adsorption_per_g_Al
      real(4) cKPAdsOx            	! [         0.6]	m3/gP               	P_adsorption_affinity_at_oxidized_conditions
      real(4) fRedMax             	! [         0.9]	-                   	max_reduction_factor_of_P_adsorption_affinity
      real(4) coPO4Max            	! [           1]	mgP/l               	max_SRP_conc_in_pore_water
      real(4) kPChemPO4           	! [        0.03]	day-1               	chem_PO4_loss_rate
      real(4) cDayManVeg1         	! [   -1.00E+07 ]	day                 	first_mowing_day_(default_non-existent)
      real(4) cDayManVeg2         	! [   -1.00E+07 ]	day                 	second_mowing_day_(Note_259_=_16_Sep)
      real(4) fManVeg             	! [           0]	-                   	Fraction_removed_by_management__for_submerged_plants
      real(4) cLengMan            	! [          10]	day                 	length_of_mowing_period
      real(4) cYearStartBirds     	! [           0]	y                   	first_year_of_birds_presence
      real(4) cDayStartBirds      	! [          46]	day                 	yearly_first_day_of_birds_presence
      real(4) cDayEndBirds        	! [         288]	day                 	yearly_last_day_of_birds_presence
      real(4) cBirdsPerha         	! [           0]	n/ha                	number_of_birds_per_ha_vegetated_lake_(Default_=_0)
      real(4) cDGrazPerBird       	! [          45]	gDW/coot/day        	daily_grazing_of_birds
      real(4) hDVegBird           	! [           5]	gDW/m2              	half-sat_vegetation_biomassfor_birds_grazing
      real(4) fDAssBird           	! [         0.5]	-                   	birds_assim_efficiency
      real(4) fDissEgesBird       	! [        0.25]	-                   	fraction_dissolved_nutrient_of_coot_egestion
      real(4) fDissMortVeg        	! [        0.25]	-                   	fraction_dissolved_nutrients_from_died_plants
      real(4) cLengAllo           	! [          15]	day                 	duration_of_allocation_and_reallocation_phase
      real(4) cLengMort           	! [          15]	day                 	duration_of_autumn_mortality_period
      real(4) UseEmpUpt           	! [           0]	-                   	false_=_do_not_use_this_empirical_relation
      real(4) fSedUptVegMax       	! [       0.998]	-                   	maximum_sediment_fraction_of_nutrient_uptake
      real(4) fSedUptVegCoef      	! [        2.66]	-                   	sigm_regr_coeff_for_sediment_fraction_of_nutrient_uptake
      real(4) fSedUptVegExp       	! [       -0.83 ]	-                   	exponent_in_sigm_regr_for_sediment_fraction_of_nutrient_uptake
      real(4) fRootVegSum         	! [         0.1]	g_root_/_g_veg      	root_fraction_outside_growing_season
      real(4) fRootVegWin         	! [         0.6]	g_root_/_g_veg      	root_fraction_outside_growing_season
      real(4) fFloatVeg           	! [           0]	g_floating_/_g_shoot	floating_fraction_of_shoot
      real(4) fEmergVeg           	! [           0]	g_floating_/_g_shoot	emergent_fraction_of_shoot
      real(4) fDepth1Veg          	! [           0]	-                   	max_upper_depth_of_submerged_veget_layer_as_fraction_of_water_depth
      real(4) fDepth2Veg          	! [           1]	-                   	max_lower_depth_of_submerged_veget_layer_as_fraction_of_water_depth
      real(4) cDLayerVeg          	! [           0]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpVeg           	! [         0.5]	%_cover/gDW/m2      	specific_cover
      real(4) kMigrVeg            	! [       0.001]	day-1               	vegetation_migration_rate
      real(4) cDVegIn             	! [           1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitVeg          	! [           9]	oC                  	temperature_for_initial_growth
      real(4) cDCarrVeg           	! [         400]	gDW/m2              	max_vegetation_standing_crop
      real(4) cMuMaxVeg           	! [         0.2]	g/g_shoot/day       	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdVeg         	! [         1.2]	-                   	temperature_quotient_of_production
      real(4) hLRefVeg            	! [          17]	W/m2_PAR            	half-sat_light_at_20_oC
      real(4) cExtSpVeg           	! [        0.01]	m2/gDW              	specific_extinction
      real(4) kDRespVeg           	! [        0.02]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespVeg         	! [           2]	-                   	temperature_quotient_of_respiration
      real(4) kMortVegSum         	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinVeg             	! [         0.3]	-                   	fraction_surviving_in_winter
      real(4) cDayWinVeg          	! [         259]	day                 	end_of_growing_season_=_16_Sep
      real(4) fDetWMortVeg        	! [         0.1]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefVegBird        	! [           1]	-                   	edibility_for_birds
      real(4) cVPUptMaxVeg        	! [        0.01]	mgP/mgDW/day        	maximum_P_uptake_capacity_of_vegetation
      real(4) cAffPUptVeg         	! [         0.2]	l/mgDW/day          	initial_P_uptake_affinity_vegetation
      real(4) cPDVegMin           	! [      0.0008]	mgP/mg              	minimum_P/day_ratio_vegetation
      real(4) cPDVegMax           	! [      0.0035]	mgP/mgDW            	maximum_P/day_ratio_vegetation
      real(4) cVNUptMaxVeg        	! [         0.1]	mgN/mgDW/day        	maximum_N_uptake_capacity_of_vegetation
      real(4) cAffNUptVeg         	! [         0.2]	l/mgDW/day          	initial_N_uptake_affinity_vegetation
      real(4) cNDVegMin           	! [        0.01]	mgN/mgDW            	minimum_N/day_ratio_vegetation
      real(4) cNDVegMax           	! [       0.035]	mgN/mgDW            	maximum_N/day_ratio_vegetation
      real(4) cPACoefMin          	! [         1.5]	-                   	minimum_Poole-Atkins_coefficient
      real(4) cPACoefMax          	! [         2.5]	-                   	maximum_Poole-Atkins_coefficient
      real(4) hPACoef             	! [           3]	g/m2                	decrease_constant_for_PA_coeff_with_DOMW
      real(4) cSecchiPlus         	! [           0]	m                   	maximum_Secchi_depth_above_water_depth
      real(4) cEuph               	! [         1.7]	-                   	conversion_constant_Secchi_depth_->_euphotic_depth
      real(4) cCovSpPhyt          	! [           2]	%/gDW/m2            	specific_coverage_Tentative
      real(4) cTmOptLoss          	! [          25]	oC                  	optimum_temp_for_grazing
      real(4) cSigTmLoss          	! [          13]	oC                  	temperature_constant_of_grazing(sigma_in_Gaussian_curve)
      real(4) fDissMortPhyt       	! [         0.2]	-                   	soluble_nutrient_fraction_of_died_Algae
      real(4) fDissLoss           	! [        0.25]	-                   	dissolved_nutrient_fraction_of_grazing_loss
      real(4) cMuMaxDiat          	! [           2]	day-1               	maximum_growth_rate_Diatoms
      real(4) cTmOptDiat          	! [          18]	oC                  	optimum_temp_diatoms
      real(4) cSigTmDiat          	! [          20]	oC                  	temperature_constant_diatoms(sigma_in_Gaussian_curve)
      real(4) cExtSpDiat          	! [        0.25]	m2/gDW              	specific_extinction_Diatoms
      real(4) UseSteeleDiat       	! [           1]	-                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) cLOptRefDiat        	! [          54]	W/m2                	optimum_PAR_for_Diatoms_at_20_oC(Steele_function)
      real(4) hLRefDiat           	! [        1000]	W/m2                	half-sat_PAR_at_20_oC(Lehmann_function)_Fake_value
      real(4) cChDDiatMin         	! [       0.004]	mgChl/mgDW          	min_chlorophyll/C_ratio_Diatoms
      real(4) cChDDiatMax         	! [       0.012]	mgChl/mgDW          	max_chlorophyll/C_ratio_Diatoms
      real(4) kDRespDiat          	! [         0.1]	day-1               	maintenance_respiration_constant_diatoms(=_005_*_MuMax)
      real(4) kLossDiat           	! [        0.25]	-                   	grazing_loss_rate_for_Diatoms
      real(4) kMortDiatW          	! [        0.01]	day-1               	mortality_constant_of_Diatoms_in_water
      real(4) kMortDiatS          	! [        0.05]	day-1               	mortality_constant_of_sed_Diatoms
      real(4) cVSetDiat           	! [         0.5]	m/day               	sedimentation_velocity_Diatoms
      real(4) cVPUptMaxDiat       	! [        0.01]	mgP/mgDW/day        	maximum_P_uptake_capacity_of_Diatoms
      real(4) cAffPUptDiat        	! [         0.2]	l/mgDW/day          	initial_P_uptake_affinity_Diatoms
      real(4) cPDDiatMin          	! [      0.0005]	mgP/mgDW            	minimum_P/day_ratio_Diatoms
      real(4) cPDDiatMax          	! [       0.005]	mgP/mgDW            	max_P/day_ratio_Diatoms
      real(4) cVNUptMaxDiat       	! [        0.07]	mgN/mgDW/day        	maximum_N_uptake_capacity_of_Diatoms
      real(4) cAffNUptDiat        	! [         0.2]	l/mgDW/day          	initial_N_uptake_affinity_Diatoms
      real(4) cNDDiatMin          	! [        0.01]	mgN/mgDW            	minimum_N/day_ratio_Diatoms
      real(4) cNDDiatMax          	! [        0.05]	mgN/mgDW            	max_N/day_ratio_Diatoms
      real(4) hSiAssDiat          	! [        0.09]	mgSi/l              	half_sat_Si_for_diatoms
      real(4) cMuMaxGren          	! [         1.5]	day-1               	maximum_growth_rate_greens
      real(4) cTmOptGren          	! [          25]	oC                  	optimum_temp_of_greens
      real(4) cSigTmGren          	! [          15]	oC                  	temperature_constant_greens(sigma_in_Gaussian_curve)
      real(4) cExtSpGren          	! [        0.25]	m2/gDW              	specific_extinction_greens
      real(4) UseSteeleGren       	! [           0]	-                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) hLRefGren           	! [          17]	W/m2                	half-sat_PAR_for_green_algae_at_20_oC(Lehmann_function)
      real(4) cLOptRefGren        	! [        1000]	W/m2                	optimum_PAR_at_20_oC(Steele_function)_Fake_value
      real(4) cChDGrenMin         	! [        0.01]	mgChl/mgDW          	min_chlorophyll/C_ratio_greens
      real(4) cChDGrenMax         	! [        0.02]	mgChl/mgDW          	max_chlorophyll/C_ratio_greens
      real(4) kDRespGren          	! [       0.075]	day-1               	maintenance_respiration_constant_greens(=_005_*_MuMax)
      real(4) kLossGren           	! [        0.25]	-                   	grazing_loss_rate_for_greens
      real(4) kMortGrenW          	! [        0.01]	day-1               	mortality_constant_of_Diatoms_in_water
      real(4) kMortGrenS          	! [        0.05]	day-1               	mortality_constant_greens
      real(4) cVSetGren           	! [         0.2]	m/day               	sedimentation_velocity_of_greens
      real(4) cVPUptMaxGren       	! [        0.01]	mgP/mgDW/day        	maximum_P_uptake_capacity_of_greens
      real(4) cAffPUptGren        	! [         0.2]	l/mgDW/day          	initial_P_uptake_affinity_greens
      real(4) cPDGrenMin          	! [      0.0015]	mgP/mgDW            	minimum_P/day_ratio_greens
      real(4) cPDGrenMax          	! [       0.015]	mgP/mgDW            	max_P/day_ratio_greens
      real(4) cVNUptMaxGren       	! [        0.07]	mgN/mgDW/day        	maximum_N_uptake_capacity_of_greens
      real(4) cAffNUptGren        	! [         0.2]	l/mgDW/day          	initial_N_uptake_affinity_greens
      real(4) cNDGrenMin          	! [        0.02]	mgN/mgDW            	minimum_N/day_ratio_greens
      real(4) cNDGrenMax          	! [         0.1]	mgN/mgDW            	max_N/day_ratio_greens
      real(4) hSiAssGren          	! [           0]	mgSi/l              	half-sat_Si_conc_for_growth_of_green_algae_=_0
      real(4) cMuMaxBlue          	! [         0.6]	day-1               	maximum_growth_rate_Bluegreens
      real(4) cTmOptBlue          	! [          25]	oC                  	optimum_temp_blue-greens
      real(4) cSigTmBlue          	! [          12]	oC                  	temperature_constant_blue-greens(sigma_in_Gaussian_curve)
      real(4) cExtSpBlue          	! [        0.35]	m2/gDW              	specific_extinction_Bluegreens
      real(4) UseSteeleBlue       	! [           1]	-                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) cLOptRefBlue        	! [        13.6]	W/m2                	optimum_PAR_for_blue-greens_at_20_oC(Steele_function)
      real(4) hLRefBlue           	! [        1000]	W/m2                	half-sat_PAR_at_20_oC(Lehmann_function)_Fake_value
      real(4) cChDBlueMin         	! [       0.005]	mgChl/mgDW          	min_chlorophyll/C_ratio_Bluegreens
      real(4) cChDBlueMax         	! [       0.015]	mgChl/mgDW          	max_chlorophyll/C_ratio_Bluegreens
      real(4) cCyDBlueMin         	! [       0.004]	mgChl/mgDW          	min_c-phycocyanin/C_ratio_Bluegreens
      real(4) cCyDBlueMax         	! [        0.06]	mgChl/mgDW          	max_c-phycocyanin/C_ratio_Bluegreens
      real(4) kDRespBlue          	! [        0.03]	day-1               	maintenance_respiration_constant_blue-greens(=_005_*_MuMax)
      real(4) kLossBlue           	! [        0.03]	-                   	grazing_loss_rate_for_Blue-greens
      real(4) kMortBlueW          	! [        0.01]	day-1               	mortality_constant_of_blue-greens_in_water
      real(4) kMortBlueS          	! [         0.2]	day-1               	mortality_constant_Bluegreens
      real(4) cVSetBlue           	! [        0.06]	m/day               	sedimentation_velocity_Blue-greens
      real(4) cVPUptMaxBlue       	! [        0.04]	mgP/mgDW/day        	maximum_P_uptake_capacity_of_Bluegreens
      real(4) cAffPUptBlue        	! [         0.8]	l/mgDW/day          	initial_P_uptake_affinity_Bluegreens
      real(4) cPDBlueMin          	! [      0.0025]	mgP/mgDW            	minimum_P/day_ratio_Bluegreens
      real(4) cPDBlueMax          	! [       0.025]	mgP/mgDW            	max_P/day_ratio_blue-greens
      real(4) cVNUptMaxBlue       	! [        0.07]	mgN/mgDW/day        	maximum_N_uptake_capacity_of_Bluegreens
      real(4) cAffNUptBlue        	! [         0.2]	l/mgDW/day          	initial_N_uptake_affinity_Bluegreens
      real(4) cNDBlueMin          	! [        0.03]	mgN/mgDW            	minimum_N/day_ratio_Bluegreens
      real(4) cNDBlueMax          	! [        0.15]	mgN/mgDW            	max_N/DW_ratio_blue-greens
      real(4) hSiAssBlue          	! [           0]	mgSi/l              	half-sat_Si_conc_for_growth_of_blue-greens_=_0
      real(4) cDBentIn            	! [        0.01]	gDW/m2              	external_zoobenthos_density
      real(4) kMigrBent           	! [       0.001]	day-1               	zoobenthos_migration_rate
      real(4) kMigrFish           	! [       0.001]	day-1               	fish_migration_rate
      real(4) cDFiJvIn            	! [       0.005]	gDW/m2              	external_fish_density
      real(4) cDFiAdIn            	! [       0.005]	gDW/m2              	external_fish_density
      real(4) kHarvFishWin        	! [           0]	day-1               	fish_harvesting_fraction_in_winter
      real(4) kHarvFishSum        	! [           0]	day-1               	fish_harvesting_fraction_in_summer
      real(4) cDPiscIn            	! [       0.001]	gDW/m2              	external__Pi_sc_density
      real(4) kMigrPisc           	! [       0.001]	day-1               	_Pi_sc_migration_rate
      real(4) kHarvPiscWin        	! [           0]	day-1               	_Pi_sc_harvesting_fraction_in_winter
      real(4) kHarvPiscSum        	! [           0]	day-1               	_Pi_sc_harvesting_fraction_in_summer
      real(4) cFiltMax            	! [         4.5]	ltr/mgDW/day        	maximum_filtering_rate(when_DOMW=0)
      real(4) hFilt               	! [           1]	mgDW/l              	half-sat_food_conc_for_filtering
      real(4) cDCarrZoo           	! [          25]	mg/l                	carrying_capacity_of_zooplankton
      real(4) cPrefDiat           	! [        0.75]	-                   	selection_factor_for_Diatoms
      real(4) cPrefGren           	! [        0.75]	-                   	selection_factor_for_Greens
      real(4) cPrefBlue           	! [       0.125]	-                   	selection_factor_for_Bluegreens_Cal
      real(4) cPrefDet            	! [        0.25]	-                   	selection_factor_for_detritus
      real(4) fDAssZoo            	! [        0.35]	-                   	DW-assimilation_efficiency_of_herb_zooplankton
      real(4) fDissEgesZoo        	! [        0.25]	-                   	soluble_nutrient_fraction_of_by_herbzoopl_egested_food
      real(4) kDRespZoo           	! [        0.15]	day-1               	maintenance_respiration_constant_herbzooplankton
      real(4) kMortZoo            	! [        0.04]	day-1               	mortality_constant_herbzooplankton
      real(4) fDissMortZoo        	! [         0.1]	-                   	soluble_nutrient_fraction_of_died_zooplankton
      real(4) cTmOptZoo           	! [          25]	oC                  	optimum_temp_zooplankton
      real(4) cSigTmZoo           	! [          13]	oC                  	temperature_constant_zooplankton(sigma_in_Gaussian_curve)
      real(4) cDCarrBent          	! [          10]	gDW/m2              	carrying_capacity_of_zoobenthos
      real(4) kDAssBent           	! [         0.1]	day-1               	maximum_assimilation_rate
      real(4) hDFoodBent          	! [         200]	g/m2                	half-saturating_food_for_zoobenthos
      real(4) fDAssBent           	! [         0.3]	-                   	C_ass_efficiency_of_zoobenthos
      real(4) fDissEgesBent       	! [        0.25]	-                   	soluble_nutrient_fraction_of_by_zoobenthos_egested_food
      real(4) kDRespBent          	! [       0.005]	day-1               	maint_respiration_constant_of_zoobenthos
      real(4) kMortBent           	! [       0.005]	day-1               	mortality_constant_of_zoobenthos
      real(4) fDissMortBent       	! [         0.1]	-                   	soluble_P_fraction_of_died_zoobenthos_P
      real(4) cTmOptBent          	! [          25]	oC                  	optimum_temp_of_zoobenthos
      real(4) cSigTmBent          	! [          16]	oC                  	temperature_constant_of_zoobenthos(sigma_in_Gaussian_curve)
      real(4) fDBone              	! [        0.35]	-                   	fraction_of_fish_C_fixed_in_bones_and_scales
      real(4) fPBone              	! [         0.5]	-                   	fraction_of_fish_P_fixed_in_bones_and_scales
      real(4) cDCarrFish          	! [          15]	gDW/m2              	carrying_capacity_of_fish(=_100_gFW/m2Grimm_1983)
      real(4) fDissEgesFish       	! [        0.25]	-                   	soluble_nutrient_fraction_of_by_fish_egested_food
      real(4) fDissMortFish       	! [         0.1]	-                   	soluble_nutrient_fraction_of_died_fish(excl_bones_and_scales
      real(4) cTmOptFish          	! [          25]	oC                  	optimum_temp_of_fish
      real(4) cSigTmFish          	! [          10]	oC                  	temperature_constant_of_fish(sigma_in_Gaussian_curve)
      real(4) cDayReprFish        	! [         120]	-                   	reproduction_date_of_fish_=_1_May
      real(4) fReprFish           	! [        0.02]	-                   	yearly_reproduction_fraction_of_adult_fish
      real(4) fAgeFish            	! [         0.5]	-                   	yearly_ageing_fraction_of_young_fish
      real(4) cRelVegFish         	! [       0.009]	-                   	decrease_of_fish_feeding_per_%_vegetation_cover(max_001)
      real(4) kDAssFiJv           	! [        0.12]	day-1               	maximum_assimilation_rate_of_young_fish
      real(4) hDZooFiJv           	! [        1.25]	g/m2                	half-saturating_zooplankton_biomass_for_young_fish_predation
      real(4) fDAssFiJv           	! [         0.4]	-                   	C_assimilation_efficiency_of_young_fish
      real(4) kDRespFiJv          	! [        0.01]	day-1               	maintenance_respiration_constant_of_young_fish
      real(4) kMortFiJv           	! [     0.00137]	day-1               	specific_mortality_of_young_fish(=_01_y-1)
      real(4) kDAssFiAd           	! [        0.06]	day-1               	maximum_assimilation_rate_of_adult_fish
      real(4) hDBentFiAd          	! [         2.5]	g/m2                	half-saturating_zoobenthos_biomass_for_adult_fish_predation
      real(4) fDAssFiAd           	! [         0.4]	-                   	C_assimilation_efficiency_of_adult_fish
      real(4) kDRespFiAd          	! [       0.004]	day-1               	maintenance_respiration_constant_of_adult_fish
      real(4) kMortFiAd           	! [     0.00027]	day-1               	specific_mortality_of_adult_fish(=_01_y-1)
      real(4) cDCarrPiscMax       	! [         1.2]	gDW/m2              	maximum_carrying_capacity_of__Pi_sc(=75_kg/ha)
      real(4) cDCarrPiscMin       	! [         0.1]	gDW/m2              	minimum_carrying_capacity_of__Pi_sc(=6_kg/ha)
      real(4) cDCarrPiscBare      	! [         0.1]	gDW/m2              	carrying_capacity_of__Pi_sc_for_lake_without_marsh_zone
      real(4) cDPhraMinPisc       	! [          50]	gDW/m2              	min_reed_biomass_for__Pi_sc
      real(4) cCovVegMin          	! [          40]	%                   	min_submveg_coverage_for__Pi_sc
      real(4) cRelPhraPisc        	! [       0.075]	gDW/m2/%            	rel__Pi_sc_density_per_%_reed_if_submveg_absent
      real(4) cRelVegPisc         	! [        0.03]	gDW/m2/%            	extra_rel__Pi_sc_density_per_%_reed_if__aCovVeg__>__cCovVegMin
      real(4) kDAssPisc           	! [       0.025]	day-1               	maximum_assimilation_rate
      real(4) hDVegPisc           	! [           5]	g/m2                	half-sat_vegetation_biomass_for__Pi_sc_growth
      real(4) hDFishPisc          	! [           1]	g/m2                	half-saturating_DFish_for__Pi_sc_predation
      real(4) fDAssPisc           	! [         0.4]	-                   	C_ass_efficiency_of__Pi_sc
      real(4) fDissEgesPisc       	! [        0.25]	-                   	soluble_P_fraction_of_by_fish_egested_food
      real(4) kDRespPisc          	! [       0.005]	day-1               	maint_respiration_constant_of__Pi_sc
      real(4) kMortPisc           	! [     0.00027]	day-1               	specific_mortality_of__Pi_sc_=_01_y-1
      real(4) fDissMortPisc       	! [         0.1]	-                   	soluble_nutrient_fraction_of_died__Pi_sc(excl_bones_and_scales
      real(4) cTmOptPisc          	! [          25]	oC                  	optimum_temp_of__Pi_sc
      real(4) cSigTmPisc          	! [          10]	oC                  	temperature_constant_of__Pi_sc(sigma_in_Gaussian_curve)
      real(4) cDepthSM            	! [         0.1]	m                   	sediment_depth
      real(4) kExchMaxM           	! [           1]	m3/m3_marshwater/day	maximum_dispersive_marsh_water_exchange_coefficient
      real(4) hfMarsh             	! [         0.1]	-                   	rel_marsh_area_where_exchange_is_50%
      real(4) fDTotSM0            	! [         0.3]	g_solid/g_sediment  	initial_dry-weight_fraction_in_sediment
      real(4) fDOrgSM0            	! [         0.1]	g_AFDW\g_solid      	initial_organic_fraction_of_sed
      real(4) fDDetSM0            	! [        0.05]	g/g                 	initial_detritus_fraction_of_sediment_organic_matter
      real(4) fPInorgSM0          	! [      0.0005]	gP/gDW              	initial_inorg_P_fraction_in_sed
      real(4) cPDPhra0            	! [       0.002]	gP/gDW              	initial_P/day_ratio_of_reed
      real(4) cNDPhra0            	! [        0.02]	gN/gDW              	initial_N/day_ratio_of_reed
      real(4) cDensStemPhra       	! [        61.5]	m-2                 	density_stem(+/-_139)
      real(4) cTmInitPhra         	! [           8]	oC                  	tempstart_initial_growth
      real(4) fDAllPhra           	! [         0.3]	-                   	allocation_fraction
      real(4) kDAllPhra           	! [        0.05]	1/day               	allocation_rate
      real(4) cDStemPhra          	! [           6]	g/m                 	average_stem_weight
      real(4) cQ10ProdPhra        	! [           2]	-                   	temp_quotient_of_production
      real(4) cMuPhraMax          	! [        0.03]	1/day               	maximum_growth_rate_reed
      real(4) cDShootPhraMax      	! [        3500]	gDW/m2              	max_shoot_biomass_of_reed
      real(4) cCovSpPhra          	! [         0.1]	%_cover_per_gD/m2   	specific_coverage
      real(4) cPDPhraMin          	! [      0.0008]	-                   	minPhra_P/day_-ratio
      real(4) cPDPhraMax          	! [       0.003]	-                   	maxPhra_P/day_-ratio
      real(4) cNDPhraMin          	! [       0.008]	-                   	minPhra_N/day_-ratio
      real(4) cNDPhraMax          	! [        0.03]	-                   	maxPhra_N/day_-ratio
      real(4) cAffNUptPhra        	! [      0.0002]	l/mgDW/DWay         	N_uptake_affinity_reed
      real(4) cAffPUptPhra        	! [      0.0002]	l/mgDW/DWay         	P_uptake_affinity_reed
      real(4) cVNUptPhraMax       	! [         0.1]	mgN/mgD/day         	max_uptake_rate_N_001
      real(4) cVPUptPhraMax       	! [        0.01]	mgP/mgD/day         	max_uptake_rate_P_0001
      real(4) kDRespPhra          	! [       0.001]	1/day               	respiration_rate_of_reed
      real(4) cQ10RespPhra        	! [         2.5]	1/e^oC              	temp_quotient_of_respiration
      real(4) fDayWin             	! [        0.52]	-                   	Start_autumn
      real(4) fDRealPhra          	! [        0.85]	-                   	reallocated_fraction_day
      real(4) kDRealPhra          	! [        0.05]	1/day               	reallocation_rate_day
      real(4) kDMortShootPhra     	! [           0]	1/day               	mortality_rate_shoots
      real(4) kDMortRootPhra      	! [    3.91E-04]	1/day               	mortality_rate_roots
      real(4) cDayWinPhra         	! [         259]	day                 	begin_autumn(16_sept)
      real(4) cDayManPhra         	! [         255]	day                 	time_of_management
      real(4) fManPhra            	! [           0]	-                   	fraction_biomass_loss_by_management
      real(4) kDManShootPhra      	! [           1]	1/day               	rate_of_management
      real(4) DaysPerYear         	! [         365]	d/y                 	DaysPerYear
      real(4) TenDays             	! [          10]	d                   	TenDays
      real(4) HoursPerDay         	! [          24]	h/d                 	HoursPerDay
      real(4) SecsPerDay          	! [       86400]	s/d                 	SecsPerDay
      real(4) mmPerm              	! [        1000]	mm/m                	mmPerm
      real(4) m2Perha             	! [       10000]	m2/ha               	m2Perha
      real(4) mgPerg              	! [        1000]	mg/g                	mgPerg
      real(4) gPerkg              	! [        1000]	g/kg                	gPerkg
      real(4) gPerton             	! [     1000000]	g/ton               	gPerton
      real(4) PerCent             	! [        0.01]	%                   	PerCent
      real(4) NearZero            	! [       1E-28]	-                   	very_small_number_used_to_avoid_dividing_by_zero_
      real(4) molO2molC           	! [      2.6667]	gO2/gC              	ratio_of_molweights
      real(4) molO2molN           	! [      2.2857]	gO2/gN              	ratio_of_molweights
      real(4) molNmolC            	! [      1.1667]	gN/gC               	ratio_of_molweights
      real(4) cRhoWat             	! [     1000000]	g/m3                	density_of_water
      real(4) Pi                  	! [    3.14E+00]	-                   	Pi_(approx_314159)
!
!
!     /* ==============================  */
!     /* declaration auxiliaries         */
!     /* ==============================  */
      real(4) sDepthW              	! Water depth
      real(4) sTime               	! sTime
      real(4) TimeYears           	! TimeYears
      real(4) Day                 	! Day
      real(4) Years               	! Years
      real(4) uTm                 	! uTm
      real(4) uVWind              	! uVWind
      real(4) ufDay               	! ufDay
      real(4) uLDay               	! uLDay
      real(4) uLOut               	! uLOut
      real(4) uLPARSurf           	! uLPARSurf
      real(4) aExtPhyt            	! aExtPhyt
      real(4) aExtDet             	! aExtDet
      real(4) aExtIM              	! aExtIM
      real(4) aExtCoefOpen        	! aExtCoefOpen
      real(4) uQInSeason          	! uQInSeason
      real(4) uQEvSinus           	! uQEvSinus
      real(4) uQEv                	! uQEv
      real(4) uQInExtra           	! uQInExtra
      real(4) uQOutExtra          	! uQOutExtra
      real(4) uQIn                	! uQIn
      real(4) uQOut               	! uQOut
      real(4) uQDil               	! uQDil
      real(4) ukDil               	! ukDil
      real(4) ukDilWat            	! ukDilWat
      real(4) ukOut               	! ukOut
      real(4) uTauWat             	! uTauWat
      real(4) uTauSubst           	! uTauSubst
      real(4) vTranDepthW         	! vTranDepthW
      real(4) akExchM             	! akExchM
      real(4) afVolMarsh          	! afVolMarsh
      real(4) akExchL             	! akExchL
      real(4) oDPhytW             	! oDPhytW
      real(4) oPPhytW             	! oPPhytW
      real(4) oNPhytW             	! oNPhytW
      real(4) aDPhytS             	! aDPhytS
      real(4) aPPhytS             	! aPPhytS
      real(4) aNPhytS             	! aNPhytS
      real(4) oDOMW               	! oDOMW
      real(4) oDSestW             	! oDSestW
      real(4) oPOMW               	! oPOMW
      real(4) oPSestW             	! oPSestW
      real(4) oPInorgW            	! oPInorgW
      real(4) oPTotW              	! oPTotW
      real(4) oNDissW             	! oNDissW
      real(4) oNOMW               	! oNOMW
      real(4) oNSestW             	! oNSestW
      real(4) oNkjW               	! oNkjW
      real(4) oNTotW              	! oNTotW
      real(4) bPorS               	! bPorS
      real(4) bPorCorS            	! bPorCorS
      real(4) aDTotS              	! aDTotS
      real(4) aRhoTotS            	! aRhoTotS
      real(4) aRhoSolidS          	! aRhoSolidS
      real(4) afDTotS             	! afDTotS
      real(4) afDOrgS             	! afDOrgS
      real(4) afDetS              	! afDetS
      real(4) afDetTotS           	! afDetTotS
      real(4) aPInorgS            	! aPInorgS
      real(4) aPTotAvailS         	! aPTotAvailS
      real(4) aPTotS              	! aPTotS
      real(4) afPInorgS           	! afPInorgS
      real(4) afPTotS             	! afPTotS
      real(4) afPO4S              	! afPO4S
      real(4) oPO4S               	! oPO4S
      real(4) aNDissS             	! aNDissS
      real(4) aNkjAvailS          	! aNkjAvailS
      real(4) aNkjS               	! aNkjS
      real(4) aNTotAvailS         	! aNTotAvailS
      real(4) aNTotS              	! aNTotS
      real(4) afNInorgS           	! afNInorgS
      real(4) afNTotS             	! afNTotS
      real(4) oNO3S               	! oNO3S
      real(4) oNH4S               	! oNH4S
      real(4) oNDissS             	! oNDissS
      real(4) rPDIMW              	! rPDIMW
      real(4) rPDIMS              	! rPDIMS
      real(4) rPDDetW             	! rPDDetW
      real(4) rNDDetW             	! rNDDetW
      real(4) rSiDDetW            	! rSiDDetW
      real(4) rPDHumS             	! rPDHumS
      real(4) rNDHumS             	! rNDHumS
      real(4) rPDDetS             	! rPDDetS
      real(4) rNDDetS             	! rNDDetS
      real(4) rSiDDetS            	! rSiDDetS
      real(4) oDPhytWM            	! oDPhytWM
      real(4) oPPhytWM            	! oPPhytWM
      real(4) oNPhytWM            	! oNPhytWM
      real(4) oSiDiatWM           	! oSiDiatWM
      real(4) oDOMWM              	! oDOMWM
      real(4) oDSestWM            	! oDSestWM
      real(4) oPOMWM              	! oPOMWM
      real(4) oPSestWM            	! oPSestWM
      real(4) oPInorgWM           	! oPInorgWM
      real(4) oPTotWM             	! oPTotWM
      real(4) oNDissWM            	! oNDissWM
      real(4) oNOMWM              	! oNOMWM
      real(4) oNSestWM            	! oNSestWM
      real(4) oNkjWM              	! oNkjWM
      real(4) oNTotWM             	! oNTotWM
      real(4) bPorSM              	! bPorSM
      real(4) bPorCorSM           	! bPorCorSM
      real(4) aDTotSM             	! aDTotSM
      real(4) aRhoTotSM           	! aRhoTotSM
      real(4) aRhoSolidSM         	! aRhoSolidSM
      real(4) afDTotSM            	! afDTotSM
      real(4) afDOrgSM            	! afDOrgSM
      real(4) afDetSM             	! afDetSM
      real(4) afDetTotSM          	! afDetTotSM
      real(4) aPInorgSM           	! aPInorgSM
      real(4) aPTotAvailSM        	! aPTotAvailSM
      real(4) aPTotSM             	! aPTotSM
      real(4) afPInorgSM          	! afPInorgSM
      real(4) afPTotSM            	! afPTotSM
      real(4) afPO4SM             	! afPO4SM
      real(4) oPO4SM              	! oPO4SM
      real(4) aNDissSM            	! aNDissSM
      real(4) aNkjAvailSM         	! aNkjAvailSM
      real(4) aNkjSM              	! aNkjSM
      real(4) aNTotAvailSM        	! aNTotAvailSM
      real(4) aNTotSM             	! aNTotSM
      real(4) afNInorgSM          	! afNInorgSM
      real(4) afNTotSM            	! afNTotSM
      real(4) oNO3SM              	! oNO3SM
      real(4) oNH4SM              	! oNH4SM
      real(4) oNDissSM            	! oNDissSM
      real(4) rPDIMWM             	! rPDIMWM
      real(4) rPDIMSM             	! rPDIMSM
      real(4) rPDDetWM            	! rPDDetWM
      real(4) rNDDetWM            	! rNDDetWM
      real(4) rSiDDetWM           	! rSiDDetWM
      real(4) rPDHumSM            	! rPDHumSM
      real(4) rNDHumSM            	! rNDHumSM
      real(4) rPDDetSM            	! rPDDetSM
      real(4) rNDDetSM            	! rNDDetSM
      real(4) rSiDDetSM           	! rSiDDetSM
      real(4) aDTotM              	! aDTotM
      real(4) aPTotM              	! aPTotM
      real(4) aNTotM              	! aNTotM
      real(4) aSiTotM             	! aSiTotM
      real(4) iPPulse             	! iPPulse
      real(4) uPLoadSeason        	! uPLoadSeason
      real(4) uPLoad              	! uPLoad
      real(4) uPLoadPO4           	! uPLoadPO4
      real(4) uPLoadOrg           	! uPLoadOrg
      real(4) uPLoadPhytTot       	! uPLoadPhytTot
      real(4) uPLoadDet           	! uPLoadDet
      real(4) uPLoadAIM           	! uPLoadAIM
      real(4) iNPulse             	! iNPulse
      real(4) uNLoadSeason        	! uNLoadSeason
      real(4) uNLoadPhytTot       	! uNLoadPhytTot
      real(4) uNLoad              	! uNLoad
      real(4) uNLoadDet           	! uNLoadDet
      real(4) uNLoadOrg           	! uNLoadOrg
      real(4) uNLoadDiss          	! uNLoadDiss
      real(4) uNLoadNH4           	! uNLoadNH4
      real(4) uNLoadNO3           	! uNLoadNO3
      real(4) uNTotIn             	! uNTotIn
      real(4) uDLoadDet           	! uDLoadDet
      real(4) uDLoadPhytTot       	! uDLoadPhytTot
      real(4) uDLoadIM            	! uDLoadIM
      real(4) uDLoad              	! uDLoad
      real(4) uPTotIn             	! uPTotIn
      real(4) uDLoadDiat          	! uDLoadDiat
      real(4) uPLoadDiat          	! uPLoadDiat
      real(4) uNLoadDiat          	! uNLoadDiat
      real(4) uDLoadGren          	! uDLoadGren
      real(4) uPLoadGren          	! uPLoadGren
      real(4) uNLoadGren          	! uNLoadGren
      real(4) uDLoadBlue          	! uDLoadBlue
      real(4) uPLoadBlue          	! uPLoadBlue
      real(4) uNLoadBlue          	! uNLoadBlue
      real(4) wDDilIM             	! wDDilIM
      real(4) wDDilDet            	! wDDilDet
      real(4) wPDilPO4            	! wPDilPO4
      real(4) wPDilDet            	! wPDilDet
      real(4) wPDilAIM            	! wPDilAIM
      real(4) wNDilNH4            	! wNDilNH4
      real(4) wNDilNO3            	! wNDilNO3
      real(4) wNDilDet            	! wNDilDet
      real(4) wO2Inflow           	! wO2Inflow
      real(4) wO2Outfl            	! wO2Outfl
      real(4) wDDilDiat           	! wDDilDiat
      real(4) wPDilDiat           	! wPDilDiat
      real(4) wNDilDiat           	! wNDilDiat
      real(4) wDDilGren           	! wDDilGren
      real(4) wPDilGren           	! wPDilGren
      real(4) wNDilGren           	! wNDilGren
      real(4) wDDilBlue           	! wDDilBlue
      real(4) wPDilBlue           	! wPDilBlue
      real(4) wNDilBlue           	! wNDilBlue
      real(4) wDDilPhyt           	! wDDilPhyt
      real(4) wPDilPhyt           	! wPDilPhyt
      real(4) wNDilPhyt           	! wNDilPhyt
      real(4) wDOutflTot          	! wDOutflTot
      real(4) wPOutflTot          	! wPOutflTot
      real(4) wNOutflTot          	! wNOutflTot
      real(4) wDTranDiat          	! wDTranDiat
      real(4) wPTranDiat          	! wPTranDiat
      real(4) wNTranDiat          	! wNTranDiat
      real(4) wDTranGren          	! wDTranGren
      real(4) wPTranGren          	! wPTranGren
      real(4) wNTranGren          	! wNTranGren
      real(4) wDTranBlue          	! wDTranBlue
      real(4) wPTranBlue          	! wPTranBlue
      real(4) wNTranBlue          	! wNTranBlue
      real(4) wDTranPhyt          	! wDTranPhyt
      real(4) wPTranPhyt          	! wPTranPhyt
      real(4) wNTranPhyt          	! wNTranPhyt
      real(4) uSiLoadSiO2         	! uSiLoadSiO2
      real(4) uSiLoadDet          	! uSiLoadDet
      real(4) uSiLoadDiat         	! uSiLoadDiat
      real(4) uSiLoad             	! uSiLoad
      real(4) wSiDilSiO2          	! wSiDilSiO2
      real(4) wSiDilDet           	! wSiDilDet
      real(4) wSiDilDiat          	! wSiDilDiat
      real(4) wSiOutflTot         	! wSiOutflTot
      real(4) wSiTranSiO2         	! wSiTranSiO2
      real(4) wSiTranDetW         	! wSiTranDetW
      real(4) tSiTranTotT         	! tSiTranTotT
      real(4) wDTranZoo           	! wDTranZoo
      real(4) wPTranZoo           	! wPTranZoo
      real(4) wNTranZoo           	! wNTranZoo
      real(4) wDTranIMW           	! wDTranIMW
      real(4) wDTranDetW          	! wDTranDetW
      real(4) wO2TranW            	! wO2TranW
      real(4) wPTranPO4W          	! wPTranPO4W
      real(4) wPTranAIMW          	! wPTranAIMW
      real(4) wPTranDetW          	! wPTranDetW
      real(4) wNTranNH4W          	! wNTranNH4W
      real(4) wNTranNO3W          	! wNTranNO3W
      real(4) wNTranDetW          	! wNTranDetW
      real(4) wDDilTot            	! wDDilTot
      real(4) wPDilTot            	! wPDilTot
      real(4) wNDilTot            	! wNDilTot
      real(4) wSiDilTot           	! wSiDilTot
      real(4) tDTranTotT          	! tDTranTotT
      real(4) tPTranTotT          	! tPTranTotT
      real(4) tNTranTotT          	! tNTranTotT
      real(4) wDExchIMM           	! wDExchIMM
      real(4) wPExchPO4M          	! wPExchPO4M
      real(4) wPExchAIMM          	! wPExchAIMM
      real(4) wNExchNH4M          	! wNExchNH4M
      real(4) wNExchNO3M          	! wNExchNO3M
      real(4) wSiExchSiO2M        	! wSiExchSiO2M
      real(4) wO2ExchM            	! wO2ExchM
      real(4) wDExchDetM          	! wDExchDetM
      real(4) wPExchDetM          	! wPExchDetM
      real(4) wNExchDetM          	! wNExchDetM
      real(4) wSiExchDetM         	! wSiExchDetM
      real(4) wDExchDiatM         	! wDExchDiatM
      real(4) wPExchDiatM         	! wPExchDiatM
      real(4) wNExchDiatM         	! wNExchDiatM
      real(4) wSiExchDiatM        	! wSiExchDiatM
      real(4) wDExchGrenM         	! wDExchGrenM
      real(4) wPExchGrenM         	! wPExchGrenM
      real(4) wNExchGrenM         	! wNExchGrenM
      real(4) wDExchBlueM         	! wDExchBlueM
      real(4) wPExchBlueM         	! wPExchBlueM
      real(4) wNExchBlueM         	! wNExchBlueM
      real(4) wDExchZooM          	! wDExchZooM
      real(4) wPExchZooM          	! wPExchZooM
      real(4) wNExchZooM          	! wNExchZooM
      real(4) wDExchIM            	! wDExchIM
      real(4) wPExchPO4           	! wPExchPO4
      real(4) wPExchAIM           	! wPExchAIM
      real(4) wNExchNH4           	! wNExchNH4
      real(4) wNExchNO3           	! wNExchNO3
      real(4) wSiExchSiO2         	! wSiExchSiO2
      real(4) wO2Exch             	! wO2Exch
      real(4) wDExchDet           	! wDExchDet
      real(4) wPExchDet           	! wPExchDet
      real(4) wNExchDet           	! wNExchDet
      real(4) wSiExchDet          	! wSiExchDet
      real(4) wDExchDiat          	! wDExchDiat
      real(4) wPExchDiat          	! wPExchDiat
      real(4) wNExchDiat          	! wNExchDiat
      real(4) wSiExchDiat         	! wSiExchDiat
      real(4) wDExchGren          	! wDExchGren
      real(4) wPExchGren          	! wPExchGren
      real(4) wNExchGren          	! wNExchGren
      real(4) wDExchBlue          	! wDExchBlue
      real(4) wPExchBlue          	! wPExchBlue
      real(4) wNExchBlue          	! wNExchBlue
      real(4) wDExchZoo           	! wDExchZoo
      real(4) wPExchZoo           	! wPExchZoo
      real(4) wNExchZoo           	! wNExchZoo
      real(4) tPInfPO4W           	! tPInfPO4W
      real(4) tNInfNH4W           	! tNInfNH4W
      real(4) tNInfNO3W           	! tNInfNO3W
      real(4) tPInfPO4S           	! tPInfPO4S
      real(4) tNInfNH4S           	! tNInfNH4S
      real(4) tNInfNO3S           	! tNInfNO3S
      real(4) tNH4LoadS           	! tNH4LoadS
      real(4) tNO3LoadS           	! tNO3LoadS
      real(4) uDErosIM            	! uDErosIM
      real(4) uDErosIMS           	! uDErosIMS
      real(4) uDErosIMW           	! uDErosIMW
      real(4) uDErosOM            	! uDErosOM
      real(4) uPErosOM            	! uPErosOM
      real(4) uNErosOM            	! uNErosOM
      real(4) uO2Sat              	! uO2Sat
      real(4) kAer                	! kAer
      real(4) uFunTmAer           	! uFunTmAer
      real(4) aFunLemnAer         	! aFunLemnAer
      real(4) tO2Aer              	! tO2Aer
      real(4) uFunTmFish          	! uFunTmFish
      real(4) tDTurbFish          	! tDTurbFish
      real(4) tDTurbFishIM        	! tDTurbFishIM
      real(4) aFunVegResus        	! aFunVegResus
      real(4) aFunDimSusp         	! aFunDimSusp
      real(4) tDResusTauDead      	! tDResusTauDead
      real(4) tDResusBareDead     	! tDResusBareDead
      real(4) tDResusDead         	! tDResusDead
      real(4) tDResusIM           	! tDResusIM
      real(4) tDResusDet          	! tDResusDet
      real(4) akResusPhytRef      	! akResusPhytRef
      real(4) tDResusPhytTot      	! tDResusPhytTot
      real(4) tPResusDet          	! tPResusDet
      real(4) tPResusPO4          	! tPResusPO4
      real(4) tPResusAIM          	! tPResusAIM
      real(4) tNResusNO3          	! tNResusNO3
      real(4) tNResusNH4          	! tNResusNH4
      real(4) tNResusDet          	! tNResusDet
      real(4) tSiResusDet         	! tSiResusDet
      real(4) aFunTauSetOM        	! aFunTauSetOM
      real(4) aFunTauSetIM        	! aFunTauSetIM
      real(4) uFunTmSet           	! uFunTmSet
      real(4) uCorVSetIM          	! uCorVSetIM
      real(4) tDSetIM             	! tDSetIM
      real(4) tPSetAIM            	! tPSetAIM
      real(4) uCorVSetDet         	! uCorVSetDet
      real(4) tDSetDet            	! tDSetDet
      real(4) tPSetDet            	! tPSetDet
      real(4) tNSetDet            	! tNSetDet
      real(4) tSiSetDet           	! tSiSetDet
      real(4) kPMinDetW           	! kPMinDetW
      real(4) kNMinDetW           	! kNMinDetW
      real(4) kSiMinDetW          	! kSiMinDetW
      real(4) uFunTmMinW          	! uFunTmMinW
      real(4) wDMinDetW           	! wDMinDetW
      real(4) wPMinDetW           	! wPMinDetW
      real(4) wNMinDetW           	! wNMinDetW
      real(4) wSiMinDetW          	! wSiMinDetW
      real(4) aCorO2BOD           	! aCorO2BOD
      real(4) wO2MinDetW          	! wO2MinDetW
      real(4) wDDenitW            	! wDDenitW
      real(4) wNDenitW            	! wNDenitW
      real(4) uFunTmNitr          	! uFunTmNitr
      real(4) aCorO2NitrW         	! aCorO2NitrW
      real(4) wNNitrW             	! wNNitrW
      real(4) wO2NitrW            	! wO2NitrW
      real(4) kPMinDetS           	! kPMinDetS
      real(4) kNMinDetS           	! kNMinDetS
      real(4) kSiMinDetS          	! kSiMinDetS
      real(4) uFunTmMinS          	! uFunTmMinS
      real(4) tDMinDetS           	! tDMinDetS
      real(4) tPMinDetS           	! tPMinDetS
      real(4) tNMinDetS           	! tNMinDetS
      real(4) tSiMinDetS          	! tSiMinDetS
      real(4) uFunTmDif           	! uFunTmDif
      real(4) akO2DifCor          	! akO2DifCor
      real(4) tSOD                	! tSOD
      real(4) aDepthOxySed        	! aDepthOxySed
      real(4) afOxySed            	! afOxySed
      real(4) tDMinOxyDetS        	! tDMinOxyDetS
      real(4) tO2MinDetS          	! tO2MinDetS
      real(4) tDDenitS            	! tDDenitS
      real(4) tNDenitS            	! tNDenitS
      real(4) tNNitrS             	! tNNitrS
      real(4) tO2NitrS            	! tO2NitrS
      real(4) tDMinHumS           	! tDMinHumS
      real(4) tPMinHumS           	! tPMinHumS
      real(4) tNMinHumS           	! tNMinHumS
      real(4) aDepthDif           	! aDepthDif
      real(4) tPDifPO4            	! tPDifPO4
      real(4) tNDifNO3            	! tNDifNO3
      real(4) tNDifNH4            	! tNDifNH4
      real(4) tO2Dif              	! tO2Dif
      real(4) tPDifGroundPO4      	! tPDifGroundPO4
      real(4) tNDifGroundNO3      	! tNDifGroundNO3
      real(4) tNDifGroundNH4      	! tNDifGroundNH4
      real(4) aPAdsMaxW           	! aPAdsMaxW
      real(4) aKPAdsW             	! aKPAdsW
      real(4) aPIsoAdsW           	! aPIsoAdsW
      real(4) aPEqIMW             	! aPEqIMW
      real(4) wPSorpIMW           	! wPSorpIMW
      real(4) aPAdsMaxS           	! aPAdsMaxS
      real(4) aKPAdsS             	! aKPAdsS
      real(4) aPIsoAdsS           	! aPIsoAdsS
      real(4) aPEqIMS             	! aPEqIMS
      real(4) tPSorpIMS           	! tPSorpIMS
      real(4) tPChemPO4           	! tPChemPO4
      real(4) wDAbioIMW           	! wDAbioIMW
      real(4) wDAbioDetW          	! wDAbioDetW
      real(4) tDAbioIMS           	! tDAbioIMS
      real(4) tDAbioDetS          	! tDAbioDetS
      real(4) tDAbioHumS          	! tDAbioHumS
      real(4) tDAbioTotT          	! tDAbioTotT
      real(4) wO2AbioW            	! wO2AbioW
      real(4) wPAbioDetW          	! wPAbioDetW
      real(4) wPAbioPO4W          	! wPAbioPO4W
      real(4) wPAbioAIMW          	! wPAbioAIMW
      real(4) tPAbioDetS          	! tPAbioDetS
      real(4) tPAbioHumS          	! tPAbioHumS
      real(4) tPAbioPO4S          	! tPAbioPO4S
      real(4) tPAbioAIMS          	! tPAbioAIMS
      real(4) tPAbioTotT          	! tPAbioTotT
      real(4) wNAbioNH4W          	! wNAbioNH4W
      real(4) wNAbioNO3W          	! wNAbioNO3W
      real(4) wNAbioDetW          	! wNAbioDetW
      real(4) tNAbioNH4S          	! tNAbioNH4S
      real(4) tNAbioNO3S          	! tNAbioNO3S
      real(4) tNAbioDetS          	! tNAbioDetS
      real(4) tNAbioHumS          	! tNAbioHumS
      real(4) tNAbioTotT          	! tNAbioTotT
      real(4) wSiAbioSiO2W        	! wSiAbioSiO2W
      real(4) wSiAbioDetW         	! wSiAbioDetW
      real(4) tSiAbioDetS         	! tSiAbioDetS
      real(4) tSiAbioTotT         	! tSiAbioTotT
      real(4) uQEvPhra            	! uQEvPhra
      real(4) tPEvPO4WM           	! tPEvPO4WM
      real(4) tNEvNH4WM           	! tNEvNH4WM
      real(4) tNEvNO3WM           	! tNEvNO3WM
      real(4) tPInfPO4WM          	! tPInfPO4WM
      real(4) tNInfNH4WM          	! tNInfNH4WM
      real(4) tNInfNO3WM          	! tNInfNO3WM
      real(4) tPInfPO4SM          	! tPInfPO4SM
      real(4) tNInfNH4SM          	! tNInfNH4SM
      real(4) tNInfNO3SM          	! tNInfNO3SM
      real(4) tO2AerM             	! tO2AerM
      real(4) tDSetIMM            	! tDSetIMM
      real(4) tPSetAIMM           	! tPSetAIMM
      real(4) tDSetDetM           	! tDSetDetM
      real(4) tPSetDetM           	! tPSetDetM
      real(4) tNSetDetM           	! tNSetDetM
      real(4) tSiSetDetM          	! tSiSetDetM
      real(4) tDSetDiatM          	! tDSetDiatM
      real(4) tPSetDiatM          	! tPSetDiatM
      real(4) tNSetDiatM          	! tNSetDiatM
      real(4) tSiSetDiatM         	! tSiSetDiatM
      real(4) tDSetGrenM          	! tDSetGrenM
      real(4) tPSetGrenM          	! tPSetGrenM
      real(4) tNSetGrenM          	! tNSetGrenM
      real(4) tDSetBlueM          	! tDSetBlueM
      real(4) tPSetBlueM          	! tPSetBlueM
      real(4) tNSetBlueM          	! tNSetBlueM
      real(4) tDSetPhytM          	! tDSetPhytM
      real(4) tPSetPhytM          	! tPSetPhytM
      real(4) tNSetPhytM          	! tNSetPhytM
      real(4) tDSetTotM           	! tDSetTotM
      real(4) wDMinDetWM          	! wDMinDetWM
      real(4) wPMinDetWM          	! wPMinDetWM
      real(4) wNMinDetWM          	! wNMinDetWM
      real(4) wSiMinDetWM         	! wSiMinDetWM
      real(4) aCorO2BODM          	! aCorO2BODM
      real(4) wO2MinDetWM         	! wO2MinDetWM
      real(4) wDDenitWM           	! wDDenitWM
      real(4) wNDenitWM           	! wNDenitWM
      real(4) aCorO2NitrWM        	! aCorO2NitrWM
      real(4) wNNitrWM            	! wNNitrWM
      real(4) wO2NitrWM           	! wO2NitrWM
      real(4) tDMinDetSM          	! tDMinDetSM
      real(4) tPMinDetSM          	! tPMinDetSM
      real(4) tNMinDetSM          	! tNMinDetSM
      real(4) tSiMinDetSM         	! tSiMinDetSM
      real(4) akO2DifCorM         	! akO2DifCorM
      real(4) tSODM               	! tSODM
      real(4) aDepthOxySedM       	! aDepthOxySedM
      real(4) afOxySedM           	! afOxySedM
      real(4) tDMinOxyDetSM       	! tDMinOxyDetSM
      real(4) tO2MinDetSM         	! tO2MinDetSM
      real(4) tDDenitSM           	! tDDenitSM
      real(4) tNDenitSM           	! tNDenitSM
      real(4) tNNitrSM            	! tNNitrSM
      real(4) tO2NitrSM           	! tO2NitrSM
      real(4) tDMinHumSM          	! tDMinHumSM
      real(4) tPMinHumSM          	! tPMinHumSM
      real(4) tNMinHumSM          	! tNMinHumSM
      real(4) aDepthDifM          	! aDepthDifM
      real(4) tPDifPO4M           	! tPDifPO4M
      real(4) tNDifNO3M           	! tNDifNO3M
      real(4) tNDifNH4M           	! tNDifNH4M
      real(4) tO2DifM             	! tO2DifM
      real(4) tPDifGroundPO4M     	! tPDifGroundPO4M
      real(4) tNDifGroundNO3M     	! tNDifGroundNO3M
      real(4) tNDifGroundNH4M     	! tNDifGroundNH4M
      real(4) aPAdsMaxWM          	! aPAdsMaxWM
      real(4) aKPAdsWM            	! aKPAdsWM
      real(4) aPIsoAdsWM          	! aPIsoAdsWM
      real(4) aPEqIMWM            	! aPEqIMWM
      real(4) wPSorpIMWM          	! wPSorpIMWM
      real(4) aPAdsMaxSM          	! aPAdsMaxSM
      real(4) aKPAdsSM            	! aKPAdsSM
      real(4) aPIsoAdsSM          	! aPIsoAdsSM
      real(4) aPEqIMSM            	! aPEqIMSM
      real(4) tPSorpIMSM          	! tPSorpIMSM
      real(4) tPChemPO4M          	! tPChemPO4M
      real(4), save :: aDayInitVeg
      real(4) bfRootVeg           	! bfRootVeg
      real(4) bfShootVeg          	! bfShootVeg
      real(4) aDRootVeg           	! aDRootVeg
      real(4) aDShootVeg          	! aDShootVeg
      real(4) aDEmergVeg          	! aDEmergVeg
      real(4) aDFloatVeg          	! aDFloatVeg
      real(4) bfSubVeg            	! bfSubVeg
      real(4) aDSubVeg            	! aDSubVeg
      real(4) aExtVeg             	! aExtVeg
      real(4) aDepth1Veg          	! aDepth1Veg
      real(4) aDepth2Veg          	! aDepth2Veg
      real(4) afCovSurfVeg        	! afCovSurfVeg
      real(4) afCovEmergVeg       	! afCovEmergVeg
      real(4) aCovVeg             	! aCovVeg
      real(4) aDVeg               	! aDVeg
      real(4) aPVeg               	! aPVeg
      real(4) aNVeg               	! aNVeg
      real(4) aExtCoef            	! aExtCoef
      real(4) aLPARBot            	! aLPARBot
      real(4) rPDVeg              	! rPDVeg
      real(4) rNDVeg              	! rNDVeg
      real(4) tDMigrVeg           	! tDMigrVeg
      real(4) tPMigrVeg           	! tPMigrVeg
      real(4) tNMigrVeg           	! tNMigrVeg
      real(4) uFunTmProdVeg       	! uFunTmProdVeg
      real(4) uFunTmRespVeg       	! uFunTmRespVeg
      real(4) afPUptVegS          	! afPUptVegS
      real(4) afNUptVegS          	! afNUptVegS
      real(4) aVPUptMaxCrVeg      	! aVPUptMaxCrVeg
      real(4) aVPUptVegW          	! aVPUptVegW
      real(4) aVPUptVegS          	! aVPUptVegS
      real(4) tPUptVegW           	! tPUptVegW
      real(4) tPUptVegS           	! tPUptVegS
      real(4) tPUptVeg            	! tPUptVeg
      real(4) aVNUptMaxCrVeg      	! aVNUptMaxCrVeg
      real(4) ahNUptVeg           	! ahNUptVeg
      real(4) aVNUptVegW          	! aVNUptVegW
      real(4) afNH4UptVegW        	! afNH4UptVegW
      real(4) tNUptVegW           	! tNUptVegW
      real(4) tNUptNH4VegW        	! tNUptNH4VegW
      real(4) tNUptNO3VegW        	! tNUptNO3VegW
      real(4) aVNUptVegS          	! aVNUptVegS
      real(4) tNUptVegS           	! tNUptVegS
      real(4) afNH4UptVegS        	! afNH4UptVegS
      real(4) tNUptNH4VegS        	! tNUptNH4VegS
      real(4) tNUptNO3VegS        	! tNUptNO3VegS
      real(4) tNUptVeg            	! tNUptVeg
      real(4) aLPAR1Veg           	! aLPAR1Veg
      real(4) aLPAR2Veg           	! aLPAR2Veg
      real(4) uhLVeg              	! uhLVeg
      real(4) aLLimShootVeg       	! aLLimShootVeg
      real(4) aMuTmLVeg           	! aMuTmLVeg
      real(4) aPLimVeg            	! aPLimVeg
      real(4) aNLimVeg            	! aNLimVeg
      real(4) aNutLimVeg          	! aNutLimVeg
      real(4) aMuVeg              	! aMuVeg
      real(4) bkMortVeg           	! bkMortVeg
      real(4) akDIncrVeg          	! akDIncrVeg
      real(4) tDEnvVeg            	! tDEnvVeg
      real(4) tDEnvProdVeg        	! tDEnvProdVeg
      real(4) tDProdVeg           	! tDProdVeg
      real(4) tDProdSubVeg        	! tDProdSubVeg
      real(4) tDRespVeg           	! tDRespVeg
      real(4) tDEnvMortVeg        	! tDEnvMortVeg
      real(4) tDMortVeg           	! tDMortVeg
      real(4) tDMortVegW          	! tDMortVegW
      real(4) tDMortVegS          	! tDMortVegS
      real(4) tDGrazVegBird       	! tDGrazVegBird
      real(4) bkManVeg            	! bkManVeg
      real(4) tDManVeg            	! tDManVeg
      real(4) tPManVeg            	! tPManVeg
      real(4) tNManVeg            	! tNManVeg
      real(4) tDBedVeg            	! tDBedVeg
      real(4) tO2ProdVeg          	! tO2ProdVeg
      real(4) tO2RespVegW         	! tO2RespVegW
      real(4) tO2RespVegS         	! tO2RespVegS
      real(4) tO2ProdVegS         	! tO2ProdVegS
      real(4) tO2ProdVegW         	! tO2ProdVegW
      real(4) tO2UptNO3VegW       	! tO2UptNO3VegW
      real(4) tO2UptNO3VegS       	! tO2UptNO3VegS
      real(4) tPExcrVeg           	! tPExcrVeg
      real(4) tPExcrVegS          	! tPExcrVegS
      real(4) tPExcrVegW          	! tPExcrVegW
      real(4) tPMortVeg           	! tPMortVeg
      real(4) tPMortVegPO4        	! tPMortVegPO4
      real(4) tPMortVegPO4S       	! tPMortVegPO4S
      real(4) tPMortVegPO4W       	! tPMortVegPO4W
      real(4) tPMortVegDet        	! tPMortVegDet
      real(4) tPMortVegDetW       	! tPMortVegDetW
      real(4) tPMortVegDetS       	! tPMortVegDetS
      real(4) tPGrazVegBird       	! tPGrazVegBird
      real(4) tPBedVeg            	! tPBedVeg
      real(4) tNExcrVeg           	! tNExcrVeg
      real(4) tNExcrVegS          	! tNExcrVegS
      real(4) tNExcrVegW          	! tNExcrVegW
      real(4) tNMortVeg           	! tNMortVeg
      real(4) tNMortVegNH4        	! tNMortVegNH4
      real(4) tNMortVegNH4S       	! tNMortVegNH4S
      real(4) tNMortVegNH4W       	! tNMortVegNH4W
      real(4) tNMortVegDet        	! tNMortVegDet
      real(4) tNMortVegDetW       	! tNMortVegDetW
      real(4) tNMortVegDetS       	! tNMortVegDetS
      real(4) tNGrazVegBird       	! tNGrazVegBird
      real(4) tNBedVeg            	! tNBedVeg
      real(4) tDAssVegBird        	! tDAssVegBird
      real(4) tDEgesBird          	! tDEgesBird
      real(4) tPAssVegBird        	! tPAssVegBird
      real(4) tPEgesBird          	! tPEgesBird
      real(4) tPEgesBirdPO4       	! tPEgesBirdPO4
      real(4) tPEgesBirdDet       	! tPEgesBirdDet
      real(4) tNAssVegBird        	! tNAssVegBird
      real(4) tNEgesBird          	! tNEgesBird
      real(4) tNEgesBirdNH4       	! tNEgesBirdNH4
      real(4) tNEgesBirdDet       	! tNEgesBirdDet
      real(4) wDBedDetW           	! wDBedDetW
      real(4) tDBedDetS           	! tDBedDetS
      real(4) tDBedTotT           	! tDBedTotT
      real(4) wPBedPO4W           	! wPBedPO4W
      real(4) wPBedDetW           	! wPBedDetW
      real(4) tPBedPO4S           	! tPBedPO4S
      real(4) tPBedDetS           	! tPBedDetS
      real(4) tPBedTotT           	! tPBedTotT
      real(4) wNBedNH4W           	! wNBedNH4W
      real(4) wNBedNO3W           	! wNBedNO3W
      real(4) wNBedDetW           	! wNBedDetW
      real(4) tNBedNH4S           	! tNBedNH4S
      real(4) tNBedNO3S           	! tNBedNO3S
      real(4) tNBedDetS           	! tNBedDetS
      real(4) tNBedTotT           	! tNBedTotT
      real(4) tO2BedW             	! tO2BedW
      real(4) tO2BedS             	! tO2BedS
      real(4) UseLoss             	! UseLoss
      real(4) uFunTmLoss          	! uFunTmLoss
      real(4) rPDBlueW            	! rPDBlueW
      real(4) rNDBlueW            	! rNDBlueW
      real(4) rPDBlueS            	! rPDBlueS
      real(4) rNDBlueS            	! rNDBlueS
      real(4) uFunTmBlue          	! uFunTmBlue
      real(4) uFunTmProdBlue      	! uFunTmProdBlue
      real(4) uFunTmRespBlue      	! uFunTmRespBlue
      real(4) aVPUptMaxCrBlue     	! aVPUptMaxCrBlue
      real(4) aVPUptBlue          	! aVPUptBlue
      real(4) wPUptBlue           	! wPUptBlue
      real(4) aVNUptMaxCrBlue     	! aVNUptMaxCrBlue
      real(4) ahNUptBlue          	! ahNUptBlue
      real(4) aVNUptBlue          	! aVNUptBlue
      real(4) wNUptBlue           	! wNUptBlue
      real(4) afNH4UptBlue        	! afNH4UptBlue
      real(4) wNUptNH4Blue        	! wNUptNH4Blue
      real(4) wNUptNO3Blue        	! wNUptNO3Blue
      real(4) uMuMaxTmBlue        	! uMuMaxTmBlue
      real(4) aPLimBlue           	! aPLimBlue
      real(4) aNLimBlue           	! aNLimBlue
      real(4) aSiLimBlue          	! aSiLimBlue
      real(4) aLLimBlue           	! aLLimBlue
      real(4) aMuTmLBlue          	! aMuTmLBlue
      real(4) aNutLimBlue         	! aNutLimBlue
      real(4) aMuBlue             	! aMuBlue
      real(4) wDAssBlue           	! wDAssBlue
      real(4) rChDBlue            	! rChDBlue
      real(4) oChlaBlue           	! oChlaBlue
      real(4) aExtChBlue          	! aExtChBlue
      real(4) ukDRespTmBlue       	! ukDRespTmBlue
      real(4) wDRespBlueW         	! wDRespBlueW
      real(4) ukLossTmBlue        	! ukLossTmBlue
      real(4) wDLossBlue          	! wDLossBlue
      real(4) wDMortBlueW         	! wDMortBlueW
      real(4) uCorVSetBlue        	! uCorVSetBlue
      real(4) tDSetBlue           	! tDSetBlue
      real(4) tDResusBlue         	! tDResusBlue
      real(4) tDRespBlueS         	! tDRespBlueS
      real(4) tDMortBlueS         	! tDMortBlueS
      real(4) ukDDecBlue          	! ukDDecBlue
      real(4) wPExcrBlueW         	! wPExcrBlueW
      real(4) wPLossBlue          	! wPLossBlue
      real(4) wPMortBlueW         	! wPMortBlueW
      real(4) tPSetBlue           	! tPSetBlue
      real(4) tPResusBlue         	! tPResusBlue
      real(4) tPExcrBlueS         	! tPExcrBlueS
      real(4) tPMortBlueS         	! tPMortBlueS
      real(4) wNExcrBlueW         	! wNExcrBlueW
      real(4) wNLossBlue          	! wNLossBlue
      real(4) wNMortBlueW         	! wNMortBlueW
      real(4) tNSetBlue           	! tNSetBlue
      real(4) tNResusBlue         	! tNResusBlue
      real(4) tNExcrBlueS         	! tNExcrBlueS
      real(4) tNMortBlueS         	! tNMortBlueS
      real(4) wDPrimBlueW         	! wDPrimBlueW
      real(4) wPPrimBlueW         	! wPPrimBlueW
      real(4) wNPrimBlueW         	! wNPrimBlueW
      real(4) tDPrimBlueS         	! tDPrimBlueS
      real(4) tPPrimBlueS         	! tPPrimBlueS
      real(4) tNPrimBlueS         	! tNPrimBlueS
      real(4) rPDGrenW            	! rPDGrenW
      real(4) rNDGrenW            	! rNDGrenW
      real(4) rPDGrenS            	! rPDGrenS
      real(4) rNDGrenS            	! rNDGrenS
      real(4) uFunTmGren          	! uFunTmGren
      real(4) uFunTmProdGren      	! uFunTmProdGren
      real(4) uFunTmRespGren      	! uFunTmRespGren
      real(4) aVPUptMaxCrGren     	! aVPUptMaxCrGren
      real(4) aVPUptGren          	! aVPUptGren
      real(4) wPUptGren           	! wPUptGren
      real(4) aVNUptMaxCrGren     	! aVNUptMaxCrGren
      real(4) ahNUptGren          	! ahNUptGren
      real(4) aVNUptGren          	! aVNUptGren
      real(4) wNUptGren           	! wNUptGren
      real(4) afNH4UptGren        	! afNH4UptGren
      real(4) wNUptNH4Gren        	! wNUptNH4Gren
      real(4) wNUptNO3Gren        	! wNUptNO3Gren
      real(4) uMuMaxTmGren        	! uMuMaxTmGren
      real(4) aPLimGren           	! aPLimGren
      real(4) aNLimGren           	! aNLimGren
      real(4) aSiLimGren          	! aSiLimGren
      real(4) aLLimGren           	! aLLimGren
      real(4) aMuTmLGren          	! aMuTmLGren
      real(4) aNutLimGren         	! aNutLimGren
      real(4) aMuGren             	! aMuGren
      real(4) wDAssGren           	! wDAssGren
      real(4) rChDGren            	! rChDGren
      real(4) oChlaGren           	! oChlaGren
      real(4) aExtChGren          	! aExtChGren
      real(4) ukDRespTmGren       	! ukDRespTmGren
      real(4) wDRespGrenW         	! wDRespGrenW
      real(4) ukLossTmGren        	! ukLossTmGren
      real(4) wDLossGren          	! wDLossGren
      real(4) wDMortGrenW         	! wDMortGrenW
      real(4) uCorVSetGren        	! uCorVSetGren
      real(4) tDSetGren           	! tDSetGren
      real(4) tDResusGren         	! tDResusGren
      real(4) tDRespGrenS         	! tDRespGrenS
      real(4) tDMortGrenS         	! tDMortGrenS
      real(4) ukDDecGren          	! ukDDecGren
      real(4) wPExcrGrenW         	! wPExcrGrenW
      real(4) wPLossGren          	! wPLossGren
      real(4) wPMortGrenW         	! wPMortGrenW
      real(4) tPSetGren           	! tPSetGren
      real(4) tPResusGren         	! tPResusGren
      real(4) tPExcrGrenS         	! tPExcrGrenS
      real(4) tPMortGrenS         	! tPMortGrenS
      real(4) wNExcrGrenW         	! wNExcrGrenW
      real(4) wNLossGren          	! wNLossGren
      real(4) wNMortGrenW         	! wNMortGrenW
      real(4) tNSetGren           	! tNSetGren
      real(4) tNResusGren         	! tNResusGren
      real(4) tNExcrGrenS         	! tNExcrGrenS
      real(4) tNMortGrenS         	! tNMortGrenS
      real(4) wDPrimGrenW         	! wDPrimGrenW
      real(4) wPPrimGrenW         	! wPPrimGrenW
      real(4) wNPrimGrenW         	! wNPrimGrenW
      real(4) tDPrimGrenS         	! tDPrimGrenS
      real(4) tPPrimGrenS         	! tPPrimGrenS
      real(4) tNPrimGrenS         	! tNPrimGrenS
      real(4) rPDDiatW            	! rPDDiatW
      real(4) rNDDiatW            	! rNDDiatW
      real(4) rPDDiatS            	! rPDDiatS
      real(4) rNDDiatS            	! rNDDiatS
      real(4) uFunTmDiat          	! uFunTmDiat
      real(4) uFunTmProdDiat      	! uFunTmProdDiat
      real(4) uFunTmRespDiat      	! uFunTmRespDiat
      real(4) aVPUptMaxCrDiat     	! aVPUptMaxCrDiat
      real(4) aVPUptDiat          	! aVPUptDiat
      real(4) wPUptDiat           	! wPUptDiat
      real(4) aVNUptMaxCrDiat     	! aVNUptMaxCrDiat
      real(4) ahNUptDiat          	! ahNUptDiat
      real(4) aVNUptDiat          	! aVNUptDiat
      real(4) wNUptDiat           	! wNUptDiat
      real(4) afNH4UptDiat        	! afNH4UptDiat
      real(4) wNUptNH4Diat        	! wNUptNH4Diat
      real(4) wNUptNO3Diat        	! wNUptNO3Diat
      real(4) uMuMaxTmDiat        	! uMuMaxTmDiat
      real(4) aPLimDiat           	! aPLimDiat
      real(4) aNLimDiat           	! aNLimDiat
      real(4) aSiLimDiat          	! aSiLimDiat
      real(4) aLLimDiat           	! aLLimDiat
      real(4) aMuTmLDiat          	! aMuTmLDiat
      real(4) aNutLimDiat         	! aNutLimDiat
      real(4) aMuDiat             	! aMuDiat
      real(4) wDAssDiat           	! wDAssDiat
      real(4) rChDDiat            	! rChDDiat
      real(4) oChlaDiat           	! oChlaDiat
      real(4) aExtChDiat          	! aExtChDiat
      real(4) ukDRespTmDiat       	! ukDRespTmDiat
      real(4) wDRespDiatW         	! wDRespDiatW
      real(4) ukLossTmDiat        	! ukLossTmDiat
      real(4) wDLossDiat          	! wDLossDiat
      real(4) wDMortDiatW         	! wDMortDiatW
      real(4) uCorVSetDiat        	! uCorVSetDiat
      real(4) tDSetDiat           	! tDSetDiat
      real(4) tDResusDiat         	! tDResusDiat
      real(4) tDRespDiatS         	! tDRespDiatS
      real(4) tDMortDiatS         	! tDMortDiatS
      real(4) ukDDecDiat          	! ukDDecDiat
      real(4) wPExcrDiatW         	! wPExcrDiatW
      real(4) wPLossDiat          	! wPLossDiat
      real(4) wPMortDiatW         	! wPMortDiatW
      real(4) tPSetDiat           	! tPSetDiat
      real(4) tPResusDiat         	! tPResusDiat
      real(4) tPExcrDiatS         	! tPExcrDiatS
      real(4) tPMortDiatS         	! tPMortDiatS
      real(4) wNExcrDiatW         	! wNExcrDiatW
      real(4) wNLossDiat          	! wNLossDiat
      real(4) wNMortDiatW         	! wNMortDiatW
      real(4) tNSetDiat           	! tNSetDiat
      real(4) tNResusDiat         	! tNResusDiat
      real(4) tNExcrDiatS         	! tNExcrDiatS
      real(4) tNMortDiatS         	! tNMortDiatS
      real(4) wDPrimDiatW         	! wDPrimDiatW
      real(4) wPPrimDiatW         	! wPPrimDiatW
      real(4) wNPrimDiatW         	! wNPrimDiatW
      real(4) tDPrimDiatS         	! tDPrimDiatS
      real(4) tPPrimDiatS         	! tPPrimDiatS
      real(4) tNPrimDiatS         	! tNPrimDiatS
      real(4) oChla               	! oChla
      real(4) wDAssPhyt           	! wDAssPhyt
      real(4) wDRespPhytW         	! wDRespPhytW
      real(4) wDMortPhytW         	! wDMortPhytW
      real(4) tDSetPhyt           	! tDSetPhyt
      real(4) wDLossPhyt          	! wDLossPhyt
      real(4) wDPrimPhytW         	! wDPrimPhytW
      real(4) wPUptPhyt           	! wPUptPhyt
      real(4) wPExcrPhytW         	! wPExcrPhytW
      real(4) wPMortPhytW         	! wPMortPhytW
      real(4) tPSetPhyt           	! tPSetPhyt
      real(4) tPResusPhyt         	! tPResusPhyt
      real(4) wPLossPhyt          	! wPLossPhyt
      real(4) wPPrimPhytW         	! wPPrimPhytW
      real(4) wNUptPhyt           	! wNUptPhyt
      real(4) wNUptNH4Phyt        	! wNUptNH4Phyt
      real(4) wNUptNO3Phyt        	! wNUptNO3Phyt
      real(4) wNExcrPhytW         	! wNExcrPhytW
      real(4) wNMortPhytW         	! wNMortPhytW
      real(4) tNSetPhyt           	! tNSetPhyt
      real(4) tNResusPhyt         	! tNResusPhyt
      real(4) wNLossPhyt          	! wNLossPhyt
      real(4) wNPrimPhytW         	! wNPrimPhytW
      real(4) tDRespPhytS         	! tDRespPhytS
      real(4) tDMortPhytS         	! tDMortPhytS
      real(4) tDPrimPhytS         	! tDPrimPhytS
      real(4) tPExcrPhytS         	! tPExcrPhytS
      real(4) tPMortPhytS         	! tPMortPhytS
      real(4) tPPrimPhytS         	! tPPrimPhytS
      real(4) tNExcrPhytS         	! tNExcrPhytS
      real(4) tNMortPhytS         	! tNMortPhytS
      real(4) tNPrimPhytS         	! tNPrimPhytS
      real(4) wSiUptDiat          	! wSiUptDiat
      real(4) wSiExcrDiatW        	! wSiExcrDiatW
      real(4) wSiLossDiat         	! wSiLossDiat
      real(4) wSiMortDiatW        	! wSiMortDiatW
      real(4) tSiSetDiat          	! tSiSetDiat
      real(4) tSiResusDiat        	! tSiResusDiat
      real(4) wSiPrimDiatW        	! wSiPrimDiatW
      real(4) rCyDBlue            	! rCyDBlue
      real(4) oCyan               	! oCyan
      real(4) fDDiat              	! fDDiat
      real(4) wDPrimDetW          	! wDPrimDetW
      real(4) tDPrimDetS          	! tDPrimDetS
      real(4) tDPrimTotT          	! tDPrimTotT
      real(4) wO2ProdPhyt         	! wO2ProdPhyt
      real(4) wO2RespPhytW        	! wO2RespPhytW
      real(4) wO2UptNO3Phyt       	! wO2UptNO3Phyt
      real(4) wO2PrimW            	! wO2PrimW
      real(4) tO2RespPhytS        	! tO2RespPhytS
      real(4) tO2PrimS            	! tO2PrimS
      real(4) wPMortPhytPO4W      	! wPMortPhytPO4W
      real(4) wPMortPhytDetW      	! wPMortPhytDetW
      real(4) wPLossPhytPO4       	! wPLossPhytPO4
      real(4) wPLossPhytDet       	! wPLossPhytDet
      real(4) wPPrimPO4W          	! wPPrimPO4W
      real(4) wPPrimDetW          	! wPPrimDetW
      real(4) tPMortPhytPO4S      	! tPMortPhytPO4S
      real(4) tPMortPhytDetS      	! tPMortPhytDetS
      real(4) tPPrimDetS          	! tPPrimDetS
      real(4) tPPrimPO4S          	! tPPrimPO4S
      real(4) tPPrimTotT          	! tPPrimTotT
      real(4) wNMortPhytNH4W      	! wNMortPhytNH4W
      real(4) wNMortPhytDetW      	! wNMortPhytDetW
      real(4) wNLossPhytNH4       	! wNLossPhytNH4
      real(4) wNLossPhytDet       	! wNLossPhytDet
      real(4) wNPrimNH4W          	! wNPrimNH4W
      real(4) wNPrimNO3W          	! wNPrimNO3W
      real(4) wNPrimDetW          	! wNPrimDetW
      real(4) tNMortPhytNH4S      	! tNMortPhytNH4S
      real(4) tNMortPhytDetS      	! tNMortPhytDetS
      real(4) tNPrimNH4S          	! tNPrimNH4S
      real(4) tNPrimNO3S          	! tNPrimNO3S
      real(4) tNPrimDetS          	! tNPrimDetS
      real(4) tNPrimTotT          	! tNPrimTotT
      real(4) tSiExcrDiatS        	! tSiExcrDiatS
      real(4) tSiMortDiatS        	! tSiMortDiatS
      real(4) wSiPrimSiO2W        	! wSiPrimSiO2W
      real(4) wSiPrimDetW         	! wSiPrimDetW
      real(4) tSiPrimDiatS        	! tSiPrimDiatS
      real(4) tSiPrimDetS         	! tSiPrimDetS
      real(4) tSiPrimTotT         	! tSiPrimTotT
      real(4) aPACoef             	! aPACoef
      real(4) bSecchiMax          	! bSecchiMax
      real(4) aSecchi             	! aSecchi
      real(4) aTransparency       	! aTransparency
      real(4) aDepthEuph          	! aDepthEuph
      real(4) aRelDepthEuph       	! aRelDepthEuph
      real(4) aChlaH              	! aChlaH
      real(4) aCovPhytW           	! aCovPhytW
      real(4) rExtChPhyt          	! rExtChPhyt
      real(4) uFunTmZoo           	! uFunTmZoo
      real(4) rPDZoo              	! rPDZoo
      real(4) rNDZoo              	! rNDZoo
      real(4) oDFoodZoo           	! oDFoodZoo
      real(4) aFilt               	! aFilt
      real(4) ukDAssTmZoo         	! ukDAssTmZoo
      real(4) aDSatZoo            	! aDSatZoo
      real(4) ukDRespTmZoo        	! ukDRespTmZoo
      real(4) ukDIncrZoo          	! ukDIncrZoo
      real(4) wDEnvZoo            	! wDEnvZoo
      real(4) wDAssZoo            	! wDAssZoo
      real(4) wDConsZoo           	! wDConsZoo
      real(4) wDConsDetZoo        	! wDConsDetZoo
      real(4) wDConsDiatZoo       	! wDConsDiatZoo
      real(4) wDConsGrenZoo       	! wDConsGrenZoo
      real(4) wDConsBlueZoo       	! wDConsBlueZoo
      real(4) wDConsPhytZoo       	! wDConsPhytZoo
      real(4) wDEgesZoo           	! wDEgesZoo
      real(4) aCorDRespZoo        	! aCorDRespZoo
      real(4) wDRespZoo           	! wDRespZoo
      real(4) wDMortZoo           	! wDMortZoo
      real(4) oPFoodZoo           	! oPFoodZoo
      real(4) rPDFoodZoo          	! rPDFoodZoo
      real(4) wPConsDiatZoo       	! wPConsDiatZoo
      real(4) wPConsGrenZoo       	! wPConsGrenZoo
      real(4) wPConsBlueZoo       	! wPConsBlueZoo
      real(4) wPConsPhytZoo       	! wPConsPhytZoo
      real(4) wPConsDetZoo        	! wPConsDetZoo
      real(4) wPConsZoo           	! wPConsZoo
      real(4) afPAssZoo           	! afPAssZoo
      real(4) wPAssZoo            	! wPAssZoo
      real(4) wPEgesZoo           	! wPEgesZoo
      real(4) wPEgesZooPO4        	! wPEgesZooPO4
      real(4) wPEgesZooDet        	! wPEgesZooDet
      real(4) akPExcrZoo          	! akPExcrZoo
      real(4) wPExcrZoo           	! wPExcrZoo
      real(4) wPMortZoo           	! wPMortZoo
      real(4) wPMortZooPO4        	! wPMortZooPO4
      real(4) wPMortZooDet        	! wPMortZooDet
      real(4) oNFoodZoo           	! oNFoodZoo
      real(4) rNDFoodZoo          	! rNDFoodZoo
      real(4) wNConsDiatZoo       	! wNConsDiatZoo
      real(4) wNConsGrenZoo       	! wNConsGrenZoo
      real(4) wNConsBlueZoo       	! wNConsBlueZoo
      real(4) wNConsPhytZoo       	! wNConsPhytZoo
      real(4) wNConsDetZoo        	! wNConsDetZoo
      real(4) wNConsZoo           	! wNConsZoo
      real(4) afNAssZoo           	! afNAssZoo
      real(4) wNAssZoo            	! wNAssZoo
      real(4) wNEgesZoo           	! wNEgesZoo
      real(4) wNEgesZooNH4        	! wNEgesZooNH4
      real(4) wNEgesZooDet        	! wNEgesZooDet
      real(4) kNExcrZoo           	! kNExcrZoo
      real(4) wNExcrZoo           	! wNExcrZoo
      real(4) wNMortZoo           	! wNMortZoo
      real(4) wNMortZooNH4        	! wNMortZooNH4
      real(4) wNMortZooDet        	! wNMortZooDet
      real(4) wSiConsDiatZoo      	! wSiConsDiatZoo
      real(4) uFunTmBent          	! uFunTmBent
      real(4) aDFoodBent          	! aDFoodBent
      real(4) rPDBent             	! rPDBent
      real(4) rNDBent             	! rNDBent
      real(4) tDMigrBent          	! tDMigrBent
      real(4) aDSatBent           	! aDSatBent
      real(4) ukDIncrBent         	! ukDIncrBent
      real(4) tDEnvBent           	! tDEnvBent
      real(4) tDAssBent           	! tDAssBent
      real(4) aDAssBentSp         	! aDAssBentSp
      real(4) tDConsBent          	! tDConsBent
      real(4) tDConsDetBent       	! tDConsDetBent
      real(4) tDConsDiatBent      	! tDConsDiatBent
      real(4) tDConsGrenBent      	! tDConsGrenBent
      real(4) tDConsBlueBent      	! tDConsBlueBent
      real(4) tDConsPhytBent      	! tDConsPhytBent
      real(4) tDEgesBent          	! tDEgesBent
      real(4) tDRespBent          	! tDRespBent
      real(4) tDMortBent          	! tDMortBent
      real(4) aPFoodBent          	! aPFoodBent
      real(4) rPDFoodBent         	! rPDFoodBent
      real(4) tPConsDetBent       	! tPConsDetBent
      real(4) tPConsDiatBent      	! tPConsDiatBent
      real(4) tPConsGrenBent      	! tPConsGrenBent
      real(4) tPConsBlueBent      	! tPConsBlueBent
      real(4) tPConsPhytBent      	! tPConsPhytBent
      real(4) tPConsBent          	! tPConsBent
      real(4) afPAssBent          	! afPAssBent
      real(4) tPAssBent           	! tPAssBent
      real(4) tPEgesBent          	! tPEgesBent
      real(4) tPEgesBentPO4       	! tPEgesBentPO4
      real(4) tPEgesBentDet       	! tPEgesBentDet
      real(4) tPExcrBent          	! tPExcrBent
      real(4) tPMortBent          	! tPMortBent
      real(4) tPMortBentPO4       	! tPMortBentPO4
      real(4) tPMortBentDet       	! tPMortBentDet
      real(4) tPMigrBent          	! tPMigrBent
      real(4) aNFoodBent          	! aNFoodBent
      real(4) rNDFoodBent         	! rNDFoodBent
      real(4) tNMigrBent          	! tNMigrBent
      real(4) tNConsDetBent       	! tNConsDetBent
      real(4) tNConsDiatBent      	! tNConsDiatBent
      real(4) tNConsGrenBent      	! tNConsGrenBent
      real(4) tNConsBlueBent      	! tNConsBlueBent
      real(4) tNConsPhytBent      	! tNConsPhytBent
      real(4) tNConsBent          	! tNConsBent
      real(4) afNAssBent          	! afNAssBent
      real(4) tNAssBent           	! tNAssBent
      real(4) tNEgesBent          	! tNEgesBent
      real(4) tNEgesBentNH4       	! tNEgesBentNH4
      real(4) tNEgesBentDet       	! tNEgesBentDet
      real(4) tNExcrBent          	! tNExcrBent
      real(4) tNMortBent          	! tNMortBent
      real(4) tNMortBentNH4       	! tNMortBentNH4
      real(4) tNMortBentDet       	! tNMortBentDet
      real(4) tSiConsDiatBent     	! tSiConsDiatBent
      real(4) aDFish              	! aDFish
      real(4) aPFish              	! aPFish
      real(4) aNFish              	! aNFish
      real(4) rPDFiJv             	! rPDFiJv
      real(4) rPDFiAd             	! rPDFiAd
      real(4) rNDFiJv             	! rNDFiJv
      real(4) rNDFiAd             	! rNDFiAd
      real(4) tDReprFish          	! tDReprFish
      real(4) tDAgeFish           	! tDAgeFish
      real(4) aFunVegFish         	! aFunVegFish
      real(4) aDSatFiJv           	! aDSatFiJv
      real(4) ukDIncrFiJv         	! ukDIncrFiJv
      real(4) tDEnvFiJv           	! tDEnvFiJv
      real(4) tDAssFiJv           	! tDAssFiJv
      real(4) tDConsFiJv          	! tDConsFiJv
      real(4) tDEgesFiJv          	! tDEgesFiJv
      real(4) tDRespFiJv          	! tDRespFiJv
      real(4) tDMortFiJv          	! tDMortFiJv
      real(4) tDMigrFiJv          	! tDMigrFiJv
      real(4) aDSatFiAd           	! aDSatFiAd
      real(4) ukDIncrFiAd         	! ukDIncrFiAd
      real(4) tDEnvFiAd           	! tDEnvFiAd
      real(4) tDAssFiAd           	! tDAssFiAd
      real(4) tDConsFiAd          	! tDConsFiAd
      real(4) tDEgesFiAd          	! tDEgesFiAd
      real(4) tDRespFiAd          	! tDRespFiAd
      real(4) tDMortFiAd          	! tDMortFiAd
      real(4) ukHarvFish          	! ukHarvFish
      real(4) tDHarvFish          	! tDHarvFish
      real(4) tDMigrFiAd          	! tDMigrFiAd
      real(4) tDMortFish          	! tDMortFish
      real(4) tDMortFishBot       	! tDMortFishBot
      real(4) tDMortFishDet       	! tDMortFishDet
      real(4) tPReprFish          	! tPReprFish
      real(4) tPAgeFish           	! tPAgeFish
      real(4) tPMigrFiJv          	! tPMigrFiJv
      real(4) tPConsFiJv          	! tPConsFiJv
      real(4) afPAssFiJv          	! afPAssFiJv
      real(4) tPAssFiJv           	! tPAssFiJv
      real(4) tPEgesFiJv          	! tPEgesFiJv
      real(4) tPExcrFiJv          	! tPExcrFiJv
      real(4) tPMortFiJv          	! tPMortFiJv
      real(4) tPMigrFiAd          	! tPMigrFiAd
      real(4) tPConsFiAd          	! tPConsFiAd
      real(4) afPAssFiAd          	! afPAssFiAd
      real(4) tPAssFiAd           	! tPAssFiAd
      real(4) tPEgesFiAd          	! tPEgesFiAd
      real(4) tPExcrFiAd          	! tPExcrFiAd
      real(4) tPMortFiAd          	! tPMortFiAd
      real(4) tPHarvFish          	! tPHarvFish
      real(4) tPMortFish          	! tPMortFish
      real(4) tPMortFishBot       	! tPMortFishBot
      real(4) tPMortFishPO4       	! tPMortFishPO4
      real(4) tPMortFishDet       	! tPMortFishDet
      real(4) tPEgesFish          	! tPEgesFish
      real(4) tPEgesFishPO4       	! tPEgesFishPO4
      real(4) tPEgesFishDet       	! tPEgesFishDet
      real(4) tNReprFish          	! tNReprFish
      real(4) tNAgeFish           	! tNAgeFish
      real(4) tNMigrFiJv          	! tNMigrFiJv
      real(4) tNConsFiJv          	! tNConsFiJv
      real(4) afNAssFiJv          	! afNAssFiJv
      real(4) tNAssFiJv           	! tNAssFiJv
      real(4) tNEgesFiJv          	! tNEgesFiJv
      real(4) tNExcrFiJv          	! tNExcrFiJv
      real(4) tNMortFiJv          	! tNMortFiJv
      real(4) tNMigrFiAd          	! tNMigrFiAd
      real(4) tNConsFiAd          	! tNConsFiAd
      real(4) afNAssFiAd          	! afNAssFiAd
      real(4) tNAssFiAd           	! tNAssFiAd
      real(4) tNEgesFiAd          	! tNEgesFiAd
      real(4) tNExcrFiAd          	! tNExcrFiAd
      real(4) tNMortFiAd          	! tNMortFiAd
      real(4) tNHarvFish          	! tNHarvFish
      real(4) tNMortFish          	! tNMortFish
      real(4) tNMortFishBot       	! tNMortFishBot
      real(4) tNMortFishNH4       	! tNMortFishNH4
      real(4) tNMortFishDet       	! tNMortFishDet
      real(4) tNEgesFish          	! tNEgesFish
      real(4) tNEgesFishNH4       	! tNEgesFishNH4
      real(4) tNEgesFishDet       	! tNEgesFishDet
      real(4) uFunTmPisc          	! uFunTmPisc
      real(4) tDMigrPisc          	! tDMigrPisc
      real(4) aDCarrPisc          	! aDCarrPisc
      real(4) aFunVegPisc         	! aFunVegPisc
      real(4) aDSatPisc           	! aDSatPisc
      real(4) akDIncrPisc         	! akDIncrPisc
      real(4) tDEnvPisc           	! tDEnvPisc
      real(4) tDAssPisc           	! tDAssPisc
      real(4) tDConsPisc          	! tDConsPisc
      real(4) tDEgesPisc          	! tDEgesPisc
      real(4) tDConsFiJvPisc      	! tDConsFiJvPisc
      real(4) tDConsFiAdPisc      	! tDConsFiAdPisc
      real(4) tDRespPisc          	! tDRespPisc
      real(4) tDMortPisc          	! tDMortPisc
      real(4) tDMortPiscBot       	! tDMortPiscBot
      real(4) tDMortPiscDet       	! tDMortPiscDet
      real(4) ukHarvPisc          	! ukHarvPisc
      real(4) tDHarvPisc          	! tDHarvPisc
      real(4) aPPisc              	! aPPisc
      real(4) tPConsFiJvPisc      	! tPConsFiJvPisc
      real(4) tPConsFiAdPisc      	! tPConsFiAdPisc
      real(4) tPConsPisc          	! tPConsPisc
      real(4) rPDFoodPisc         	! rPDFoodPisc
      real(4) afPAssPisc          	! afPAssPisc
      real(4) tPAssPisc           	! tPAssPisc
      real(4) tPEgesPisc          	! tPEgesPisc
      real(4) tPEgesPiscPO4       	! tPEgesPiscPO4
      real(4) tPEgesPiscDet       	! tPEgesPiscDet
      real(4) tPExcrPisc          	! tPExcrPisc
      real(4) tPMortPisc          	! tPMortPisc
      real(4) tPMortPiscBot       	! tPMortPiscBot
      real(4) tPMortPiscPO4       	! tPMortPiscPO4
      real(4) tPMortPiscDet       	! tPMortPiscDet
      real(4) tPMigrPisc          	! tPMigrPisc
      real(4) tPHarvPisc          	! tPHarvPisc
      real(4) aNPisc              	! aNPisc
      real(4) tNConsFiJvPisc      	! tNConsFiJvPisc
      real(4) tNConsFiAdPisc      	! tNConsFiAdPisc
      real(4) tNConsPisc          	! tNConsPisc
      real(4) rNDFoodPisc         	! rNDFoodPisc
      real(4) afNAssPisc          	! afNAssPisc
      real(4) tNAssPisc           	! tNAssPisc
      real(4) tNEgesPisc          	! tNEgesPisc
      real(4) tNEgesPiscNH4       	! tNEgesPiscNH4
      real(4) tNEgesPiscDet       	! tNEgesPiscDet
      real(4) tNExcrPisc          	! tNExcrPisc
      real(4) tNMortPisc          	! tNMortPisc
      real(4) tNMortPiscBot       	! tNMortPiscBot
      real(4) tNMortPiscNH4       	! tNMortPiscNH4
      real(4) tNMortPiscDet       	! tNMortPiscDet
      real(4) tNMigrPisc          	! tNMigrPisc
      real(4) tNHarvPisc          	! tNHarvPisc
      real(4) wDWebZoo            	! wDWebZoo
      real(4) wPWebZoo            	! wPWebZoo
      real(4) wNWebZoo            	! wNWebZoo
      real(4) tDWebBent           	! tDWebBent
      real(4) tPWebBent           	! tPWebBent
      real(4) tNWebBent           	! tNWebBent
      real(4) tDWebFiJv           	! tDWebFiJv
      real(4) tPWebFiJv           	! tPWebFiJv
      real(4) tNWebFiJv           	! tNWebFiJv
      real(4) tDWebFiAd           	! tDWebFiAd
      real(4) tPWebFiAd           	! tPWebFiAd
      real(4) tNWebFiAd           	! tNWebFiAd
      real(4) tDWebPisc           	! tDWebPisc
      real(4) wDWebDetW           	! wDWebDetW
      real(4) wDWebDiatW          	! wDWebDiatW
      real(4) wDWebGrenW          	! wDWebGrenW
      real(4) wDWebBlueW          	! wDWebBlueW
      real(4) tDWebDetS           	! tDWebDetS
      real(4) tDWebDiatS          	! tDWebDiatS
      real(4) tDWebGrenS          	! tDWebGrenS
      real(4) tDWebBlueS          	! tDWebBlueS
      real(4) tDWebPhytS          	! tDWebPhytS
      real(4) tDWebTotT           	! tDWebTotT
      real(4) wPWebPO4W           	! wPWebPO4W
      real(4) wPWebDetW           	! wPWebDetW
      real(4) wPWebDiatW          	! wPWebDiatW
      real(4) wPWebGrenW          	! wPWebGrenW
      real(4) wPWebBlueW          	! wPWebBlueW
      real(4) tPWebPO4S           	! tPWebPO4S
      real(4) tPWebDetS           	! tPWebDetS
      real(4) tPWebDiatS          	! tPWebDiatS
      real(4) tPWebGrenS          	! tPWebGrenS
      real(4) tPWebBlueS          	! tPWebBlueS
      real(4) tPWebPhytS          	! tPWebPhytS
      real(4) tPWebTotT           	! tPWebTotT
      real(4) wNWebNH4W           	! wNWebNH4W
      real(4) wNWebNO3W           	! wNWebNO3W
      real(4) wNWebDetW           	! wNWebDetW
      real(4) wNWebDiatW          	! wNWebDiatW
      real(4) wNWebGrenW          	! wNWebGrenW
      real(4) wNWebBlueW          	! wNWebBlueW
      real(4) tNWebNH4S           	! tNWebNH4S
      real(4) tNWebNO3S           	! tNWebNO3S
      real(4) tNWebDetS           	! tNWebDetS
      real(4) tNWebDiatS          	! tNWebDiatS
      real(4) tNWebGrenS          	! tNWebGrenS
      real(4) tNWebBlueS          	! tNWebBlueS
      real(4) tNWebPhytS          	! tNWebPhytS
      real(4) tNWebTotT           	! tNWebTotT
      real(4) wSiWebSiO2W         	! wSiWebSiO2W
      real(4) wSiWebDetW          	! wSiWebDetW
      real(4) tSiWebDetS          	! tSiWebDetS
      real(4) tSiWebTotT          	! tSiWebTotT
      real(4) aPrefAve            	! aPrefAve
      real(4) wDConsZoo2          	! wDConsZoo2
      real(4) aDConsZooSp         	! aDConsZooSp
      real(4) aDAssZooSp          	! aDAssZooSp
      real(4) aDGrazSp            	! aDGrazSp
      real(4) aPConsZooSp         	! aPConsZooSp
      real(4) aPGrazSp            	! aPGrazSp
      real(4) aNConsZooSp         	! aNConsZooSp
      real(4) aNGrazSp            	! aNGrazSp
      real(4) afDShootPhra        	! afDShootPhra
      real(4) rDSRPhra            	! rDSRPhra
      real(4) rPDShootPhra        	! rPDShootPhra
      real(4) rNDShootPhra        	! rNDShootPhra
      real(4) rPDRootPhra         	! rPDRootPhra
      real(4) rNDRootPhra         	! rNDRootPhra
      real(4) aLengShootPhra      	! aLengShootPhra
      real(4), save :: bDayInitPhra
      real(4) aDAllPhra           	! aDAllPhra
      real(4) tDAllPhra           	! tDAllPhra
      real(4) tNTransPhra         	! tNTransPhra
      real(4) tPTransPhra         	! tPTransPhra
      real(4) aVNUptPhraMaxCr     	! aVNUptPhraMaxCr
      real(4) ahNUptPhraS         	! ahNUptPhraS
      real(4) aVNUptPhraS         	! aVNUptPhraS
      real(4) tNUptPhraS          	! tNUptPhraS
      real(4) tNUptNH4PhraS       	! tNUptNH4PhraS
      real(4) tNUptNO3PhraS       	! tNUptNO3PhraS
      real(4) tNUptShootPhra      	! tNUptShootPhra
      real(4) tNUptRootPhra       	! tNUptRootPhra
      real(4) aVPUptPhraMaxCr     	! aVPUptPhraMaxCr
      real(4) ahPUptPhraS         	! ahPUptPhraS
      real(4) aVPUptPhraS         	! aVPUptPhraS
      real(4) tPUptPhraS          	! tPUptPhraS
      real(4) tPUptShootPhra      	! tPUptShootPhra
      real(4) tPUptRootPhra       	! tPUptRootPhra
      real(4) uFunTmProdPhra      	! uFunTmProdPhra
      real(4) ukDRespTmPhra       	! ukDRespTmPhra
      real(4) aMuPhotPhra         	! aMuPhotPhra
      real(4) aNLimProdPhra       	! aNLimProdPhra
      real(4) aPLimProdPhra       	! aPLimProdPhra
      real(4) aNutLimPhra         	! aNutLimPhra
      real(4) aMuPhra             	! aMuPhra
      real(4) akDIncrPhra         	! akDIncrPhra
      real(4) tDDensPhra          	! tDDensPhra
      real(4) tDDensProdPhra      	! tDDensProdPhra
      real(4) tDProdPhra          	! tDProdPhra
      real(4) tDProdShootPhra     	! tDProdShootPhra
      real(4) tDProdRootPhra      	! tDProdRootPhra
      real(4) tDRespShootPhra     	! tDRespShootPhra
      real(4) tDRespRootPhra      	! tDRespRootPhra
      real(4) tO2RespRootPhra     	! tO2RespRootPhra
      real(4) tO2FlowPhra         	! tO2FlowPhra
      real(4), save :: bDayRealPhra
      real(4) aDRealPhra          	! aDRealPhra
      real(4) tDRealPhra          	! tDRealPhra
      real(4) tNRetrPhra          	! tNRetrPhra
      real(4) tPRetrPhra          	! tPRetrPhra
      real(4) tDMortShootPhra     	! tDMortShootPhra
      real(4) tNMortShootPhra     	! tNMortShootPhra
      real(4) tPMortShootPhra     	! tPMortShootPhra
      real(4) tDMortRootPhra      	! tDMortRootPhra
      real(4) tNMortRootPhra      	! tNMortRootPhra
      real(4) tPMortRootPhra      	! tPMortRootPhra
      real(4) tDManShootPhra      	! tDManShootPhra
      real(4) tNManShootPhra      	! tNManShootPhra
      real(4) tPManShootPhra      	! tPManShootPhra
      real(4) tDIMSM              	! tDIMSM
      real(4) tDHumSM             	! tDHumSM
      real(4) tDDetSM             	! tDDetSM
      real(4) vDeltaSM            	! vDeltaSM
      real(4) tDBurIMM            	! tDBurIMM
      real(4) tDBurOMM            	! tDBurOMM
      real(4) tDBurDetM           	! tDBurDetM
      real(4) tDBurHumM           	! tDBurHumM
      real(4) tDBurTotM           	! tDBurTotM
      real(4) tPBurHumM           	! tPBurHumM
      real(4) tPBurDetM           	! tPBurDetM
      real(4) tPBurAIMM           	! tPBurAIMM
      real(4) tPBurPO4M           	! tPBurPO4M
      real(4) tPBurTotM           	! tPBurTotM
      real(4) tNBurHumM           	! tNBurHumM
      real(4) tNBurDetM           	! tNBurDetM
      real(4) tNBurNH4M           	! tNBurNH4M
      real(4) tNBurNO3M           	! tNBurNO3M
      real(4) tNBurTotM           	! tNBurTotM
      real(4) tSiBurDetM          	! tSiBurDetM
      real(4) tSiBurTotM          	! tSiBurTotM
      real(4) vDeltaWM            	! vDeltaWM
      real(4) aRelDeltaWM         	! aRelDeltaWM
      real(4) tDSetTot            	! tDSetTot
      real(4) tPSetTot            	! tPSetTot
      real(4) tNSetTot            	! tNSetTot
      real(4) tDResusTot          	! tDResusTot
      real(4) tPResusTot          	! tPResusTot
      real(4) tNResusTot          	! tNResusTot
      real(4) bTimeDred           	! bTimeDred
      real(4), save :: aDepthStart
      real(4) akDredDepth         	! akDredDepth
      real(4) akDred              	! akDred
      real(4) akDredBent          	! akDredBent
      real(4) vDredDepthW         	! vDredDepthW
      real(4) tDDredDetS          	! tDDredDetS
      real(4) tPDredDetS          	! tPDredDetS
      real(4) tNDredDetS          	! tNDredDetS
      real(4) tSiDredDetS         	! tSiDredDetS
      real(4) tPDredAIMS          	! tPDredAIMS
      real(4) bRhoSolidSoil       	! bRhoSolidSoil
      real(4) tDDredNetSoil       	! tDDredNetSoil
      real(4) tDDredNetIMS        	! tDDredNetIMS
      real(4) tDDredNetHumS       	! tDDredNetHumS
      real(4) tPDredNetHumS       	! tPDredNetHumS
      real(4) tNDredNetHumS       	! tNDredNetHumS
      real(4) tDDredDiatS         	! tDDredDiatS
      real(4) tPDredDiatS         	! tPDredDiatS
      real(4) tNDredDiatS         	! tNDredDiatS
      real(4) tDDredGrenS         	! tDDredGrenS
      real(4) tPDredGrenS         	! tPDredGrenS
      real(4) tNDredGrenS         	! tNDredGrenS
      real(4) tDDredBlueS         	! tDDredBlueS
      real(4) tPDredBlueS         	! tPDredBlueS
      real(4) tNDredBlueS         	! tNDredBlueS
      real(4) tDDredPhytS         	! tDDredPhytS
      real(4) tPDredPhytS         	! tPDredPhytS
      real(4) tNDredPhytS         	! tNDredPhytS
      real(4) tDDredBent          	! tDDredBent
      real(4) tPDredBent          	! tPDredBent
      real(4) tNDredBent          	! tNDredBent
      real(4) tDDredVeg           	! tDDredVeg
      real(4) tPDredVeg           	! tPDredVeg
      real(4) tNDredVeg           	! tNDredVeg
      real(4) tDDredNetTot        	! tDDredNetTot
      real(4) tPDredNetTot        	! tPDredNetTot
      real(4) tNDredNetTot        	! tNDredNetTot
      real(4) tSiDredTot          	! tSiDredTot
      real(4) tDIMS               	! tDIMS
      real(4) tDHumS              	! tDHumS
      real(4) tDDetS              	! tDDetS
      real(4) vDeltaS             	! vDeltaS
      real(4) tDBurIM             	! tDBurIM
      real(4) tDBurOM             	! tDBurOM
      real(4) tDBurDet            	! tDBurDet
      real(4) tDBurHum            	! tDBurHum
      real(4) tDBurTot            	! tDBurTot
      real(4) tPBurHum            	! tPBurHum
      real(4) tPBurDet            	! tPBurDet
      real(4) tPBurAIM            	! tPBurAIM
      real(4) tPBurPO4            	! tPBurPO4
      real(4) tPBurTot            	! tPBurTot
      real(4) tNBurHum            	! tNBurHum
      real(4) tNBurDet            	! tNBurDet
      real(4) tNBurNH4            	! tNBurNH4
      real(4) tNBurNO3            	! tNBurNO3
      real(4) tNBurTot            	! tNBurTot
      real(4) tSiBurDet           	! tSiBurDet
      real(4) tSiBurTot           	! tSiBurTot
      real(4) vDeltaW             	! vDeltaW
      real(4) aRelDeltaW          	! aRelDeltaW
      real(4) tDMarsTotT          	! tDMarsTotT
      real(4) tPMarsTotT          	! tPMarsTotT
      real(4) tNMarsTotT          	! tNMarsTotT
      real(4) tSiMarsTotT         	! tSiMarsTotT
      real(4) aDTotT              	! aDTotT
      real(4) aNTotT              	! aNTotT
      real(4) aPTotT              	! aPTotT
      real(4) aSiTotT             	! aSiTotT
      real(4) aDError             	! aDError
      real(4) aNError             	! aNError
      real(4) aPError             	! aPError
      real(4) aSiError            	! aSiError
      real(4) dNH4W               	! dNH4W
      real(4) dNO3W               	! dNO3W
      real(4) dPO4W               	! dPO4W
      real(4) dPAIMW              	! dPAIMW
      real(4) dSiO2W              	! dSiO2W
      real(4) dO2W                	! dO2W
      real(4) dDDetW              	! dDDetW
      real(4) dNDetW              	! dNDetW
      real(4) dPDetW              	! dPDetW
      real(4) dSiDetW             	! dSiDetW
      real(4) dDIMW               	! dDIMW
      real(4) dDDiatW             	! dDDiatW
      real(4) dNDiatW             	! dNDiatW
      real(4) dPDiatW             	! dPDiatW
      real(4) dDGrenW             	! dDGrenW
      real(4) dNGrenW             	! dNGrenW
      real(4) dPGrenW             	! dPGrenW
      real(4) dDBlueW             	! dDBlueW
      real(4) dNBlueW             	! dNBlueW
      real(4) dPBlueW             	! dPBlueW
      real(4) dDZoo               	! dDZoo
      real(4) dNZoo               	! dNZoo
      real(4) dPZoo               	! dPZoo
      real(4) dDFiAd              	! dDFiAd
      real(4) dDFiJv              	! dDFiJv
      real(4) dNFiAd              	! dNFiAd
      real(4) dNFiJv              	! dNFiJv
      real(4) dPFiAd              	! dPFiAd
      real(4) dPFiJv              	! dPFiJv
      real(4) dDPisc              	! dDPisc
      real(4) dNH4S               	! dNH4S
      real(4) dNO3S               	! dNO3S
      real(4) dPO4S               	! dPO4S
      real(4) dPAIMS              	! dPAIMS
      real(4) dDDetS              	! dDDetS
      real(4) dNDetS              	! dNDetS
      real(4) dPDetS              	! dPDetS
      real(4) dSiDetS             	! dSiDetS
      real(4) dDHumS              	! dDHumS
      real(4) dNHumS              	! dNHumS
      real(4) dPHumS              	! dPHumS
      real(4) dDIMS               	! dDIMS
      real(4) dDDiatS             	! dDDiatS
      real(4) dNDiatS             	! dNDiatS
      real(4) dPDiatS             	! dPDiatS
      real(4) dDGrenS             	! dDGrenS
      real(4) dNGrenS             	! dNGrenS
      real(4) dPGrenS             	! dPGrenS
      real(4) dDBlueS             	! dDBlueS
      real(4) dNBlueS             	! dNBlueS
      real(4) dPBlueS             	! dPBlueS
      real(4) dDVeg               	! dDVeg
      real(4) dNVeg               	! dNVeg
      real(4) dPVeg               	! dPVeg
      real(4) dDBent              	! dDBent
      real(4) dNBent              	! dNBent
      real(4) dPBent              	! dPBent
      real(4) dDepthWM            	! dDepthWM
      real(4) dNH4WM              	! dNH4WM
      real(4) dNO3WM              	! dNO3WM
      real(4) dPO4WM              	! dPO4WM
      real(4) dPAIMWM             	! dPAIMWM
      real(4) dSiO2WM             	! dSiO2WM
      real(4) dO2WM               	! dO2WM
      real(4) dDDetWM             	! dDDetWM
      real(4) dNDetWM             	! dNDetWM
      real(4) dPDetWM             	! dPDetWM
      real(4) dSiDetWM            	! dSiDetWM
      real(4) dDIMWM              	! dDIMWM
      real(4) dDDiatWM            	! dDDiatWM
      real(4) dNDiatWM            	! dNDiatWM
      real(4) dPDiatWM            	! dPDiatWM
      real(4) dDGrenWM            	! dDGrenWM
      real(4) dNGrenWM            	! dNGrenWM
      real(4) dPGrenWM            	! dPGrenWM
      real(4) dDBlueWM            	! dDBlueWM
      real(4) dNBlueWM            	! dNBlueWM
      real(4) dPBlueWM            	! dPBlueWM
      real(4) dDZooM              	! dDZooM
      real(4) dNZooM              	! dNZooM
      real(4) dPZooM              	! dPZooM
      real(4) dNH4SM              	! dNH4SM
      real(4) dNO3SM              	! dNO3SM
      real(4) dPO4SM              	! dPO4SM
      real(4) dPAIMSM             	! dPAIMSM
      real(4) dDDetSM             	! dDDetSM
      real(4) dNDetSM             	! dNDetSM
      real(4) dPDetSM             	! dPDetSM
      real(4) dSiDetSM            	! dSiDetSM
      real(4) dDHumSM             	! dDHumSM
      real(4) dNHumSM             	! dNHumSM
      real(4) dPHumSM             	! dPHumSM
      real(4) dDIMSM              	! dDIMSM
      real(4) dDRootPhra          	! dDRootPhra
      real(4) dDShootPhra         	! dDShootPhra
      real(4) dNRootPhra          	! dNRootPhra
      real(4) dNShootPhra         	! dNShootPhra
      real(4) dPRootPhra          	! dPRootPhra
      real(4) dPShootPhra         	! dPShootPhra
      real(4) dDExtTotT           	! dDExtTotT
      real(4) dNExtTotT           	! dNExtTotT
      real(4) dPExtTotT           	! dPExtTotT
      real(4) dSiExtTotT          	! dSiExtTotT
!
!
!     /* ==============================  */
!     /* declaration fluxes              */
!     /* ==============================  */
      real(4) D0sNH4W          	 ! flux of sNH4W
      real(4) D0sNO3W          	 ! flux of sNO3W
      real(4) D0sPO4W          	 ! flux of sPO4W
      real(4) D0sPAIMW         	 ! flux of sPAIMW
      real(4) D0sSiO2W         	 ! flux of sSiO2W
      real(4) D0sO2W           	 ! flux of sO2W
      real(4) D0sDDetW         	 ! flux of sDDetW
      real(4) D0sNDetW         	 ! flux of sNDetW
      real(4) D0sPDetW         	 ! flux of sPDetW
      real(4) D0sSiDetW        	 ! flux of sSiDetW
      real(4) D0sDIMW          	 ! flux of sDIMW
      real(4) D0sDDiatW        	 ! flux of sDDiatW
      real(4) D0sNDiatW        	 ! flux of sNDiatW
      real(4) D0sPDiatW        	 ! flux of sPDiatW
      real(4) D0sDGrenW        	 ! flux of sDGrenW
      real(4) D0sNGrenW        	 ! flux of sNGrenW
      real(4) D0sPGrenW        	 ! flux of sPGrenW
      real(4) D0sDBlueW        	 ! flux of sDBlueW
      real(4) D0sNBlueW        	 ! flux of sNBlueW
      real(4) D0sPBlueW        	 ! flux of sPBlueW
      real(4) D0sDZoo          	 ! flux of sDZoo
      real(4) D0sNZoo          	 ! flux of sNZoo
      real(4) D0sPZoo          	 ! flux of sPZoo
      real(4) D0sDFiAd         	 ! flux of sDFiAd
      real(4) D0sDFiJv         	 ! flux of sDFiJv
      real(4) D0sNFiAd         	 ! flux of sNFiAd
      real(4) D0sNFiJv         	 ! flux of sNFiJv
      real(4) D0sPFiAd         	 ! flux of sPFiAd
      real(4) D0sPFiJv         	 ! flux of sPFiJv
      real(4) D0sDPisc         	 ! flux of sDPisc
      real(4) D0sNH4S          	 ! flux of sNH4S
      real(4) D0sNO3S          	 ! flux of sNO3S
      real(4) D0sPO4S          	 ! flux of sPO4S
      real(4) D0sPAIMS         	 ! flux of sPAIMS
      real(4) D0sDDetS         	 ! flux of sDDetS
      real(4) D0sNDetS         	 ! flux of sNDetS
      real(4) D0sPDetS         	 ! flux of sPDetS
      real(4) D0sSiDetS        	 ! flux of sSiDetS
      real(4) D0sDHumS         	 ! flux of sDHumS
      real(4) D0sNHumS         	 ! flux of sNHumS
      real(4) D0sPHumS         	 ! flux of sPHumS
      real(4) D0sDIMS          	 ! flux of sDIMS
      real(4) D0sDDiatS        	 ! flux of sDDiatS
      real(4) D0sNDiatS        	 ! flux of sNDiatS
      real(4) D0sPDiatS        	 ! flux of sPDiatS
      real(4) D0sDGrenS        	 ! flux of sDGrenS
      real(4) D0sNGrenS        	 ! flux of sNGrenS
      real(4) D0sPGrenS        	 ! flux of sPGrenS
      real(4) D0sDBlueS        	 ! flux of sDBlueS
      real(4) D0sNBlueS        	 ! flux of sNBlueS
      real(4) D0sPBlueS        	 ! flux of sPBlueS
      real(4) D0sDVeg          	 ! flux of sDVeg
      real(4) D0sNVeg          	 ! flux of sNVeg
      real(4) D0sPVeg          	 ! flux of sPVeg
      real(4) D0sDBent         	 ! flux of sDBent
      real(4) D0sNBent         	 ! flux of sNBent
      real(4) D0sPBent         	 ! flux of sPBent
      real(4) D0sDepthWM       	 ! flux of sDepthWM
      real(4) D0sNH4WM         	 ! flux of sNH4WM
      real(4) D0sNO3WM         	 ! flux of sNO3WM
      real(4) D0sPO4WM         	 ! flux of sPO4WM
      real(4) D0sPAIMWM        	 ! flux of sPAIMWM
      real(4) D0sSiO2WM        	 ! flux of sSiO2WM
      real(4) D0sO2WM          	 ! flux of sO2WM
      real(4) D0sDDetWM        	 ! flux of sDDetWM
      real(4) D0sNDetWM        	 ! flux of sNDetWM
      real(4) D0sPDetWM        	 ! flux of sPDetWM
      real(4) D0sSiDetWM       	 ! flux of sSiDetWM
      real(4) D0sDIMWM         	 ! flux of sDIMWM
      real(4) D0sDDiatWM       	 ! flux of sDDiatWM
      real(4) D0sNDiatWM       	 ! flux of sNDiatWM
      real(4) D0sPDiatWM       	 ! flux of sPDiatWM
      real(4) D0sDGrenWM       	 ! flux of sDGrenWM
      real(4) D0sNGrenWM       	 ! flux of sNGrenWM
      real(4) D0sPGrenWM       	 ! flux of sPGrenWM
      real(4) D0sDBlueWM       	 ! flux of sDBlueWM
      real(4) D0sNBlueWM       	 ! flux of sNBlueWM
      real(4) D0sPBlueWM       	 ! flux of sPBlueWM
      real(4) D0sDZooM         	 ! flux of sDZooM
      real(4) D0sNZooM         	 ! flux of sNZooM
      real(4) D0sPZooM         	 ! flux of sPZooM
      real(4) D0sNH4SM         	 ! flux of sNH4SM
      real(4) D0sNO3SM         	 ! flux of sNO3SM
      real(4) D0sPO4SM         	 ! flux of sPO4SM
      real(4) D0sPAIMSM        	 ! flux of sPAIMSM
      real(4) D0sDDetSM        	 ! flux of sDDetSM
      real(4) D0sNDetSM        	 ! flux of sNDetSM
      real(4) D0sPDetSM        	 ! flux of sPDetSM
      real(4) D0sSiDetSM       	 ! flux of sSiDetSM
      real(4) D0sDHumSM        	 ! flux of sDHumSM
      real(4) D0sNHumSM        	 ! flux of sNHumSM
      real(4) D0sPHumSM        	 ! flux of sPHumSM
      real(4) D0sDIMSM         	 ! flux of sDIMSM
      real(4) D0sDRootPhra     	 ! flux of sDRootPhra
      real(4) D0sDShootPhra    	 ! flux of sDShootPhra
      real(4) D0sNRootPhra     	 ! flux of sNRootPhra
      real(4) D0sNShootPhra    	 ! flux of sNShootPhra
      real(4) D0sPRootPhra     	 ! flux of sPRootPhra
      real(4) D0sPShootPhra    	 ! flux of sPShootPhra
      real(4) D0sDExtTotT      	 ! flux of sDExtTotT
      real(4) D0sNExtTotT      	 ! flux of sNExtTotT
      real(4) D0sPExtTotT      	 ! flux of sPExtTotT
      real(4) D0sSiExtTotT     	 ! flux of sSiExtTotT
!
!
!     /* ==============================  */
!     /* declaration pointer to flux     */
!     /* ==============================  */
      integer ID0sNH4W          	 ! pointer to flux variable 
      integer ID0sNO3W          	 ! pointer to flux variable 
      integer ID0sPO4W          	 ! pointer to flux variable 
      integer ID0sPAIMW         	 ! pointer to flux variable 
      integer ID0sSiO2W         	 ! pointer to flux variable 
      integer ID0sO2W           	 ! pointer to flux variable 
      integer ID0sDDetW         	 ! pointer to flux variable 
      integer ID0sNDetW         	 ! pointer to flux variable 
      integer ID0sPDetW         	 ! pointer to flux variable 
      integer ID0sSiDetW        	 ! pointer to flux variable 
      integer ID0sDIMW          	 ! pointer to flux variable 
      integer ID0sDDiatW        	 ! pointer to flux variable 
      integer ID0sNDiatW        	 ! pointer to flux variable 
      integer ID0sPDiatW        	 ! pointer to flux variable 
      integer ID0sDGrenW        	 ! pointer to flux variable 
      integer ID0sNGrenW        	 ! pointer to flux variable 
      integer ID0sPGrenW        	 ! pointer to flux variable 
      integer ID0sDBlueW        	 ! pointer to flux variable 
      integer ID0sNBlueW        	 ! pointer to flux variable 
      integer ID0sPBlueW        	 ! pointer to flux variable 
      integer ID0sDZoo          	 ! pointer to flux variable 
      integer ID0sNZoo          	 ! pointer to flux variable 
      integer ID0sPZoo          	 ! pointer to flux variable 
      integer ID0sDFiAd         	 ! pointer to flux variable 
      integer ID0sDFiJv         	 ! pointer to flux variable 
      integer ID0sNFiAd         	 ! pointer to flux variable 
      integer ID0sNFiJv         	 ! pointer to flux variable 
      integer ID0sPFiAd         	 ! pointer to flux variable 
      integer ID0sPFiJv         	 ! pointer to flux variable 
      integer ID0sDPisc         	 ! pointer to flux variable 
      integer ID0sNH4S          	 ! pointer to flux variable 
      integer ID0sNO3S          	 ! pointer to flux variable 
      integer ID0sPO4S          	 ! pointer to flux variable 
      integer ID0sPAIMS         	 ! pointer to flux variable 
      integer ID0sDDetS         	 ! pointer to flux variable 
      integer ID0sNDetS         	 ! pointer to flux variable 
      integer ID0sPDetS         	 ! pointer to flux variable 
      integer ID0sSiDetS        	 ! pointer to flux variable 
      integer ID0sDHumS         	 ! pointer to flux variable 
      integer ID0sNHumS         	 ! pointer to flux variable 
      integer ID0sPHumS         	 ! pointer to flux variable 
      integer ID0sDIMS          	 ! pointer to flux variable 
      integer ID0sDDiatS        	 ! pointer to flux variable 
      integer ID0sNDiatS        	 ! pointer to flux variable 
      integer ID0sPDiatS        	 ! pointer to flux variable 
      integer ID0sDGrenS        	 ! pointer to flux variable 
      integer ID0sNGrenS        	 ! pointer to flux variable 
      integer ID0sPGrenS        	 ! pointer to flux variable 
      integer ID0sDBlueS        	 ! pointer to flux variable 
      integer ID0sNBlueS        	 ! pointer to flux variable 
      integer ID0sPBlueS        	 ! pointer to flux variable 
      integer ID0sDVeg          	 ! pointer to flux variable 
      integer ID0sNVeg          	 ! pointer to flux variable 
      integer ID0sPVeg          	 ! pointer to flux variable 
      integer ID0sDBent         	 ! pointer to flux variable 
      integer ID0sNBent         	 ! pointer to flux variable 
      integer ID0sPBent         	 ! pointer to flux variable 
      integer ID0sDepthWM       	 ! pointer to flux variable 
      integer ID0sNH4WM         	 ! pointer to flux variable 
      integer ID0sNO3WM         	 ! pointer to flux variable 
      integer ID0sPO4WM         	 ! pointer to flux variable 
      integer ID0sPAIMWM        	 ! pointer to flux variable 
      integer ID0sSiO2WM        	 ! pointer to flux variable 
      integer ID0sO2WM          	 ! pointer to flux variable 
      integer ID0sDDetWM        	 ! pointer to flux variable 
      integer ID0sNDetWM        	 ! pointer to flux variable 
      integer ID0sPDetWM        	 ! pointer to flux variable 
      integer ID0sSiDetWM       	 ! pointer to flux variable 
      integer ID0sDIMWM         	 ! pointer to flux variable 
      integer ID0sDDiatWM       	 ! pointer to flux variable 
      integer ID0sNDiatWM       	 ! pointer to flux variable 
      integer ID0sPDiatWM       	 ! pointer to flux variable 
      integer ID0sDGrenWM       	 ! pointer to flux variable 
      integer ID0sNGrenWM       	 ! pointer to flux variable 
      integer ID0sPGrenWM       	 ! pointer to flux variable 
      integer ID0sDBlueWM       	 ! pointer to flux variable 
      integer ID0sNBlueWM       	 ! pointer to flux variable 
      integer ID0sPBlueWM       	 ! pointer to flux variable 
      integer ID0sDZooM         	 ! pointer to flux variable 
      integer ID0sNZooM         	 ! pointer to flux variable 
      integer ID0sPZooM         	 ! pointer to flux variable 
      integer ID0sNH4SM         	 ! pointer to flux variable 
      integer ID0sNO3SM         	 ! pointer to flux variable 
      integer ID0sPO4SM         	 ! pointer to flux variable 
      integer ID0sPAIMSM        	 ! pointer to flux variable 
      integer ID0sDDetSM        	 ! pointer to flux variable 
      integer ID0sNDetSM        	 ! pointer to flux variable 
      integer ID0sPDetSM        	 ! pointer to flux variable 
      integer ID0sSiDetSM       	 ! pointer to flux variable 
      integer ID0sDHumSM        	 ! pointer to flux variable 
      integer ID0sNHumSM        	 ! pointer to flux variable 
      integer ID0sPHumSM        	 ! pointer to flux variable 
      integer ID0sDIMSM         	 ! pointer to flux variable 
      integer ID0sDRootPhra     	 ! pointer to flux variable 
      integer ID0sDShootPhra    	 ! pointer to flux variable 
      integer ID0sNRootPhra     	 ! pointer to flux variable 
      integer ID0sNShootPhra    	 ! pointer to flux variable 
      integer ID0sPRootPhra     	 ! pointer to flux variable 
      integer ID0sPShootPhra    	 ! pointer to flux variable 
      integer ID0sDExtTotT      	 ! pointer to flux variable 
      integer ID0sNExtTotT      	 ! pointer to flux variable 
      integer ID0sPExtTotT      	 ! pointer to flux variable 
      integer ID0sSiExtTotT     	 ! pointer to flux variable 
integer, save :: counter1 = 0
integer, save :: counter2 = 0
integer, save :: counter3 = 0
CHARACTER :: textfile
 !   
 !*******************************************************************************    
 !   
      ipnt        = ipoint
      ID0sNH4W          	= 1 
      ID0sNO3W          	= 2 
      ID0sPO4W          	= 3 
      ID0sPAIMW         	= 4 
      ID0sSiO2W         	= 5 
      ID0sO2W           	= 6 
      ID0sDDetW         	= 7 
      ID0sNDetW         	= 8 
      ID0sPDetW         	= 9 
      ID0sSiDetW        	= 10 
      ID0sDIMW          	= 11 
      ID0sDDiatW        	= 12 
      ID0sNDiatW        	= 13 
      ID0sPDiatW        	= 14 
      ID0sDGrenW        	= 15 
      ID0sNGrenW        	= 16 
      ID0sPGrenW        	= 17 
      ID0sDBlueW        	= 18 
      ID0sNBlueW        	= 19 
      ID0sPBlueW        	= 20 
      ID0sDZoo          	= 21 
      ID0sNZoo          	= 22 
      ID0sPZoo          	= 23 
      ID0sDFiAd         	= 24 
      ID0sDFiJv         	= 25 
      ID0sNFiAd         	= 26 
      ID0sNFiJv         	= 27 
      ID0sPFiAd         	= 28 
      ID0sPFiJv         	= 29 
      ID0sDPisc         	= 30 
      ID0sNH4S          	= 31 
      ID0sNO3S          	= 32 
      ID0sPO4S          	= 33 
      ID0sPAIMS         	= 34 
      ID0sDDetS         	= 35 
      ID0sNDetS         	= 36 
      ID0sPDetS         	= 37 
      ID0sSiDetS        	= 38 
      ID0sDHumS         	= 39 
      ID0sNHumS         	= 40 
      ID0sPHumS         	= 41 
      ID0sDIMS          	= 42 
      ID0sDDiatS        	= 43 
      ID0sNDiatS        	= 44 
      ID0sPDiatS        	= 45 
      ID0sDGrenS        	= 46 
      ID0sNGrenS        	= 47 
      ID0sPGrenS        	= 48 
      ID0sDBlueS        	= 49 
      ID0sNBlueS        	= 50 
      ID0sPBlueS        	= 51 
      ID0sDVeg          	= 52 
      ID0sNVeg          	= 53 
      ID0sPVeg          	= 54 
      ID0sDBent         	= 55 
      ID0sNBent         	= 56 
      ID0sPBent         	= 57 
      ID0sDepthWM       	= 58 
      ID0sNH4WM         	= 59 
      ID0sNO3WM         	= 60 
      ID0sPO4WM         	= 61 
      ID0sPAIMWM        	= 62 
      ID0sSiO2WM        	= 63 
      ID0sO2WM          	= 64 
      ID0sDDetWM        	= 65 
      ID0sNDetWM        	= 66 
      ID0sPDetWM        	= 67 
      ID0sSiDetWM       	= 68 
      ID0sDIMWM         	= 69 
      ID0sDDiatWM       	= 70 
      ID0sNDiatWM       	= 71 
      ID0sPDiatWM       	= 72 
      ID0sDGrenWM       	= 73 
      ID0sNGrenWM       	= 74 
      ID0sPGrenWM       	= 75 
      ID0sDBlueWM       	= 76 
      ID0sNBlueWM       	= 77 
      ID0sPBlueWM       	= 78 
      ID0sDZooM         	= 79 
      ID0sNZooM         	= 80 
      ID0sPZooM         	= 81 
      ID0sNH4SM         	= 82 
      ID0sNO3SM         	= 83 
      ID0sPO4SM         	= 84 
      ID0sPAIMSM        	= 85 
      ID0sDDetSM        	= 86 
      ID0sNDetSM        	= 87 
      ID0sPDetSM        	= 88 
      ID0sSiDetSM       	= 89 
      ID0sDHumSM        	= 90 
      ID0sNHumSM        	= 91 
      ID0sPHumSM        	= 92 
      ID0sDIMSM         	= 93 
      ID0sDRootPhra     	= 94 
      ID0sDShootPhra    	= 95 
      ID0sNRootPhra     	= 96 
      ID0sNShootPhra    	= 97 
      ID0sPRootPhra     	= 98 
      ID0sPShootPhra    	= 99 
      ID0sDExtTotT      	= 100 
      ID0sNExtTotT      	= 101 
      ID0sPExtTotT      	= 102 
      ID0sSiExtTotT     	= 103 
 !   
       do 9000 iseg = 1 , noseg
 !   
         sNH4W               	= pmsa( ipnt( 1) )
         sNO3W               	= pmsa( ipnt( 2) )
         sPO4W               	= pmsa( ipnt( 3) )
         sPAIMW              	= pmsa( ipnt( 4) )
         sSiO2W              	= pmsa( ipnt( 5) )
         sO2W                	= pmsa( ipnt( 6) )
         sDDetW              	= pmsa( ipnt( 7) )
         sNDetW              	= pmsa( ipnt( 8) )
         sPDetW              	= pmsa( ipnt( 9) )
         sSiDetW             	= pmsa( ipnt( 10) )
         sDIMW               	= pmsa( ipnt( 11) )
         sDDiatW             	= pmsa( ipnt( 12) )
         sNDiatW             	= pmsa( ipnt( 13) )
         sPDiatW             	= pmsa( ipnt( 14) )
         sDGrenW             	= pmsa( ipnt( 15) )
         sNGrenW             	= pmsa( ipnt( 16) )
         sPGrenW             	= pmsa( ipnt( 17) )
         sDBlueW             	= pmsa( ipnt( 18) )
         sNBlueW             	= pmsa( ipnt( 19) )
         sPBlueW             	= pmsa( ipnt( 20) )
         sDZoo               	= pmsa( ipnt( 21) )
         sNZoo               	= pmsa( ipnt( 22) )
         sPZoo               	= pmsa( ipnt( 23) )
         sDFiAd              	= pmsa( ipnt( 24) )
         sDFiJv              	= pmsa( ipnt( 25) )
         sNFiAd              	= pmsa( ipnt( 26) )
         sNFiJv              	= pmsa( ipnt( 27) )
         sPFiAd              	= pmsa( ipnt( 28) )
         sPFiJv              	= pmsa( ipnt( 29) )
         sDPisc              	= pmsa( ipnt( 30) )
         sNH4S               	= pmsa( ipnt( 31) )
         sNO3S               	= pmsa( ipnt( 32) )
         sPO4S               	= pmsa( ipnt( 33) )
         sPAIMS              	= pmsa( ipnt( 34) )
         sDDetS              	= pmsa( ipnt( 35) )
         sNDetS              	= pmsa( ipnt( 36) )
         sPDetS              	= pmsa( ipnt( 37) )
         sSiDetS             	= pmsa( ipnt( 38) )
         sDHumS              	= pmsa( ipnt( 39) )
         sNHumS              	= pmsa( ipnt( 40) )
         sPHumS              	= pmsa( ipnt( 41) )
         sDIMS               	= pmsa( ipnt( 42) )
         sDDiatS             	= pmsa( ipnt( 43) )
         sNDiatS             	= pmsa( ipnt( 44) )
         sPDiatS             	= pmsa( ipnt( 45) )
         sDGrenS             	= pmsa( ipnt( 46) )
         sNGrenS             	= pmsa( ipnt( 47) )
         sPGrenS             	= pmsa( ipnt( 48) )
         sDBlueS             	= pmsa( ipnt( 49) )
         sNBlueS             	= pmsa( ipnt( 50) )
         sPBlueS             	= pmsa( ipnt( 51) )
         sDVeg               	= pmsa( ipnt( 52) )
         sNVeg               	= pmsa( ipnt( 53) )
         sPVeg               	= pmsa( ipnt( 54) )
         sDBent              	= pmsa( ipnt( 55) )
         sNBent              	= pmsa( ipnt( 56) )
         sPBent              	= pmsa( ipnt( 57) )
         sDepthWM            	= pmsa( ipnt( 58) )
         sNH4WM              	= pmsa( ipnt( 59) )
         sNO3WM              	= pmsa( ipnt( 60) )
         sPO4WM              	= pmsa( ipnt( 61) )
         sPAIMWM             	= pmsa( ipnt( 62) )
         sSiO2WM             	= pmsa( ipnt( 63) )
         sO2WM               	= pmsa( ipnt( 64) )
         sDDetWM             	= pmsa( ipnt( 65) )
         sNDetWM             	= pmsa( ipnt( 66) )
         sPDetWM             	= pmsa( ipnt( 67) )
         sSiDetWM            	= pmsa( ipnt( 68) )
         sDIMWM              	= pmsa( ipnt( 69) )
         sDDiatWM            	= pmsa( ipnt( 70) )
         sNDiatWM            	= pmsa( ipnt( 71) )
         sPDiatWM            	= pmsa( ipnt( 72) )
         sDGrenWM            	= pmsa( ipnt( 73) )
         sNGrenWM            	= pmsa( ipnt( 74) )
         sPGrenWM            	= pmsa( ipnt( 75) )
         sDBlueWM            	= pmsa( ipnt( 76) )
         sNBlueWM            	= pmsa( ipnt( 77) )
         sPBlueWM            	= pmsa( ipnt( 78) )
         sDZooM              	= pmsa( ipnt( 79) )
         sNZooM              	= pmsa( ipnt( 80) )
         sPZooM              	= pmsa( ipnt( 81) )
         sNH4SM              	= pmsa( ipnt( 82) )
         sNO3SM              	= pmsa( ipnt( 83) )
         sPO4SM              	= pmsa( ipnt( 84) )
         sPAIMSM             	= pmsa( ipnt( 85) )
         sDDetSM             	= pmsa( ipnt( 86) )
         sNDetSM             	= pmsa( ipnt( 87) )
         sPDetSM             	= pmsa( ipnt( 88) )
         sSiDetSM            	= pmsa( ipnt( 89) )
         sDHumSM             	= pmsa( ipnt( 90) )
         sNHumSM             	= pmsa( ipnt( 91) )
         sPHumSM             	= pmsa( ipnt( 92) )
         sDIMSM              	= pmsa( ipnt( 93) )
         sDRootPhra          	= pmsa( ipnt( 94) )
         sDShootPhra         	= pmsa( ipnt( 95) )
         sNRootPhra          	= pmsa( ipnt( 96) )
         sNShootPhra         	= pmsa( ipnt( 97) )
         sPRootPhra          	= pmsa( ipnt( 98) )
         sPShootPhra         	= pmsa( ipnt( 99) )
         sDExtTotT           	= pmsa( ipnt( 100) )
         sNExtTotT           	= pmsa( ipnt( 101) )
         sPExtTotT           	= pmsa( ipnt( 102) )
         sSiExtTotT          	= pmsa( ipnt( 103) )
         ITIME               	= pmsa( ipnt( 104) )
         TotalDepth        	= pmsa( ipnt( 105) )
         InitCalc            	= pmsa( ipnt( 106) )
         ConstDepth          	= pmsa( ipnt( 107) )
         InclTran            	= pmsa( ipnt( 108) )
         InclPhytS           	= pmsa( ipnt( 109) )
         InclBed             	= pmsa( ipnt( 110) )
         InclWeb             	= pmsa( ipnt( 111) )
         InclMarsh           	= pmsa( ipnt( 112) )
         InclSeason          	= pmsa( ipnt( 113) )
         ReadTemp            	= pmsa( ipnt( 114) )
         ReadLOut            	= pmsa( ipnt( 115) )
         ReadVWind           	= pmsa( ipnt( 116) )
         ReadQIn             	= pmsa( ipnt( 117) )
         ReadQOut            	= pmsa( ipnt( 118) )
         ReadQEv             	= pmsa( ipnt( 119) )
         ReadPLoad           	= pmsa( ipnt( 120) )
         ReadNLoad           	= pmsa( ipnt( 121) )
         ReadNutFrac         	= pmsa( ipnt( 122) )
         ReadPLoadPhyt       	= pmsa( ipnt( 123) )
         ReadDLoadDet        	= pmsa( ipnt( 124) )
         ReadDLoadIM         	= pmsa( ipnt( 125) )
         UseSeasonLoad       	= pmsa( ipnt( 126) )
         UsePulseLoad        	= pmsa( ipnt( 127) )
         mTemp               	= pmsa( ipnt( 128) )
         mLOut               	= pmsa( ipnt( 129) )
         mVWind              	= pmsa( ipnt( 130) )
         mQIn                	= pmsa( ipnt( 131) )
         mQOut               	= pmsa( ipnt( 132) )
         mQEv                	= pmsa( ipnt( 133) )
         mPLoad              	= pmsa( ipnt( 134) )
         mPLoadPO4           	= pmsa( ipnt( 135) )
         mPLoadOrg           	= pmsa( ipnt( 136) )
         mPLoadPhytTot       	= pmsa( ipnt( 137) )
         mNLoad              	= pmsa( ipnt( 138) )
         mNLoadNH4           	= pmsa( ipnt( 139) )
         mNLoadNO3           	= pmsa( ipnt( 140) )
         mNLoadOrg           	= pmsa( ipnt( 141) )
         mDLoadDet           	= pmsa( ipnt( 142) )
         mDLoadIM            	= pmsa( ipnt( 143) )
         BeginTime           	= pmsa( ipnt( 144) )
         EndTime             	= pmsa( ipnt( 145) )
         YearZero            	= pmsa( ipnt( 146) )
         cFetch              	= pmsa( ipnt( 147) )
         fMarsh              	= pmsa( ipnt( 148) )
         fLutum              	= pmsa( ipnt( 149) )
         fFeDIM              	= pmsa( ipnt( 150) )
         fAlDIM              	= pmsa( ipnt( 151) )
         cTmAve              	= pmsa( ipnt( 152) )
         cTmVar              	= pmsa( ipnt( 153) )
         cTimeLag            	= pmsa( ipnt( 154) )
         cVWind              	= pmsa( ipnt( 155) )
         cQInf               	= pmsa( ipnt( 156) )
         cPBackLoad          	= pmsa( ipnt( 157) )
         cNBackLoad          	= pmsa( ipnt( 158) )
         cLDayAve            	= pmsa( ipnt( 159) )
         cLDayVar            	= pmsa( ipnt( 160) )
         cfDayAve            	= pmsa( ipnt( 161) )
         cfDayVar            	= pmsa( ipnt( 162) )
         fRefl               	= pmsa( ipnt( 163) )
         cExtWat             	= pmsa( ipnt( 164) )
         cDredInterval       	= pmsa( ipnt( 165) )
         cDredStart          	= pmsa( ipnt( 166) )
         cDepthRef           	= pmsa( ipnt( 167) )
         cLengDred           	= pmsa( ipnt( 168) )
         fEffDred            	= pmsa( ipnt( 169) )
         fEffDredBent        	= pmsa( ipnt( 170) )
         fPAR                	= pmsa( ipnt( 171) )
         cExtSpDet           	= pmsa( ipnt( 172) )
         cExtSpIM            	= pmsa( ipnt( 173) )
         fDTotS0             	= pmsa( ipnt( 174) )
         fDOrgS0             	= pmsa( ipnt( 175) )
         fDDetS0             	= pmsa( ipnt( 176) )
         fSedPhyt0           	= pmsa( ipnt( 177) )
         fPInorgS0           	= pmsa( ipnt( 178) )
         fPAdsS0             	= pmsa( ipnt( 179) )
         cPDDet0             	= pmsa( ipnt( 180) )
         cNDDet0             	= pmsa( ipnt( 181) )
         cSiDDet0            	= pmsa( ipnt( 182) )
         cPDHum0             	= pmsa( ipnt( 183) )
         cNDHum0             	= pmsa( ipnt( 184) )
         cPDPhyt0            	= pmsa( ipnt( 185) )
         cNDPhyt0            	= pmsa( ipnt( 186) )
         cPDDiat0            	= pmsa( ipnt( 187) )
         cNDDiat0            	= pmsa( ipnt( 188) )
         cPDGren0            	= pmsa( ipnt( 189) )
         cNDGren0            	= pmsa( ipnt( 190) )
         cPDBlue0            	= pmsa( ipnt( 191) )
         cNDBlue0            	= pmsa( ipnt( 192) )
         cPDVeg0             	= pmsa( ipnt( 193) )
         cNDVeg0             	= pmsa( ipnt( 194) )
         cSiDDiat            	= pmsa( ipnt( 195) )
         cPDZooRef           	= pmsa( ipnt( 196) )
         cNDZooRef           	= pmsa( ipnt( 197) )
         cPDBentRef          	= pmsa( ipnt( 198) )
         cNDBentRef          	= pmsa( ipnt( 199) )
         cPDFishRef          	= pmsa( ipnt( 200) )
         cNDFishRef          	= pmsa( ipnt( 201) )
         cPDPisc             	= pmsa( ipnt( 202) )
         cNDPisc             	= pmsa( ipnt( 203) )
         cQIn                	= pmsa( ipnt( 204) )
         cQInSum             	= pmsa( ipnt( 205) )
         cQInWin             	= pmsa( ipnt( 206) )
         cDepthWMax          	= pmsa( ipnt( 207) )
         cQInExtraApril1     	= pmsa( ipnt( 208) )
         cQInExtraOct1       	= pmsa( ipnt( 209) )
         cQOutExtraApril1    	= pmsa( ipnt( 210) )
         cQOutExtraOct1      	= pmsa( ipnt( 211) )
         cQEvAve             	= pmsa( ipnt( 212) )
         cQEvVar             	= pmsa( ipnt( 213) )
         cPLoad              	= pmsa( ipnt( 214) )
         cPLoadSum           	= pmsa( ipnt( 215) )
         cPLoadWin           	= pmsa( ipnt( 216) )
         fPO4In              	= pmsa( ipnt( 217) )
         fPhytInWin          	= pmsa( ipnt( 218) )
         fPhytInSum          	= pmsa( ipnt( 219) )
         fDiatPhytIn         	= pmsa( ipnt( 220) )
         fGrenPhytIn         	= pmsa( ipnt( 221) )
         fBluePhytIn         	= pmsa( ipnt( 222) )
         cNLoad              	= pmsa( ipnt( 223) )
         cNLoadSum           	= pmsa( ipnt( 224) )
         cNLoadWin           	= pmsa( ipnt( 225) )
         cNPLoadMeas         	= pmsa( ipnt( 226) )
         cNPPhytIn           	= pmsa( ipnt( 227) )
         cNPDetIn            	= pmsa( ipnt( 228) )
         fNH4DissIn          	= pmsa( ipnt( 229) )
         cNDPhytIn           	= pmsa( ipnt( 230) )
         cNDDetIn            	= pmsa( ipnt( 231) )
         cDIMIn              	= pmsa( ipnt( 232) )
         cO2In               	= pmsa( ipnt( 233) )
         cSiO2In             	= pmsa( ipnt( 234) )
         cSiDDetIn           	= pmsa( ipnt( 235) )
         cDZooIn             	= pmsa( ipnt( 236) )
         cDayApril1          	= pmsa( ipnt( 237) )
         cDayOct1            	= pmsa( ipnt( 238) )
         cLengChange         	= pmsa( ipnt( 239) )
         cNLoadS             	= pmsa( ipnt( 240) )
         fNH4LoadS           	= pmsa( ipnt( 241) )
         cDErosTot           	= pmsa( ipnt( 242) )
         fSedErosIM          	= pmsa( ipnt( 243) )
         fDOrgSoil           	= pmsa( ipnt( 244) )
         cPDSoilOM           	= pmsa( ipnt( 245) )
         cNDSoilOM           	= pmsa( ipnt( 246) )
         cPO4Ground          	= pmsa( ipnt( 247) )
         cNH4Ground          	= pmsa( ipnt( 248) )
         cNO3Ground          	= pmsa( ipnt( 249) )
         cDepthS             	= pmsa( ipnt( 250) )
         cCPerDW             	= pmsa( ipnt( 251) )
         cRhoIM              	= pmsa( ipnt( 252) )
         cRhoOM              	= pmsa( ipnt( 253) )
         cTmRef              	= pmsa( ipnt( 254) )
         cAerRoot            	= pmsa( ipnt( 255) )
         cAerLin             	= pmsa( ipnt( 256) )
         cAerSquare          	= pmsa( ipnt( 257) )
         cThetaAer           	= pmsa( ipnt( 258) )
         cVSetIM             	= pmsa( ipnt( 259) )
         cVSetDet            	= pmsa( ipnt( 260) )
         cThetaSet           	= pmsa( ipnt( 261) )
         cSuspMin            	= pmsa( ipnt( 262) )
         cSuspMax            	= pmsa( ipnt( 263) )
         cSuspSlope          	= pmsa( ipnt( 264) )
         hDepthSusp          	= pmsa( ipnt( 265) )
         cFetchRef           	= pmsa( ipnt( 266) )
         fLutumRef           	= pmsa( ipnt( 267) )
         cSuspRef            	= pmsa( ipnt( 268) )
         kVegResus           	= pmsa( ipnt( 269) )
         kTurbFish           	= pmsa( ipnt( 270) )
         kResusPhytMax       	= pmsa( ipnt( 271) )
         cResusPhytExp       	= pmsa( ipnt( 272) )
         cThetaMinW          	= pmsa( ipnt( 273) )
         kDMinDetW           	= pmsa( ipnt( 274) )
         hO2BOD              	= pmsa( ipnt( 275) )
         O2PerNO3            	= pmsa( ipnt( 276) )
         cThetaMinS          	= pmsa( ipnt( 277) )
         kDMinDetS           	= pmsa( ipnt( 278) )
         fRefrDetS           	= pmsa( ipnt( 279) )
         hNO3Denit           	= pmsa( ipnt( 280) )
         NO3PerC             	= pmsa( ipnt( 281) )
         kDMinHum            	= pmsa( ipnt( 282) )
         kNitrW              	= pmsa( ipnt( 283) )
         kNitrS              	= pmsa( ipnt( 284) )
         cThetaNitr          	= pmsa( ipnt( 285) )
         O2PerNH4            	= pmsa( ipnt( 286) )
         hO2Nitr             	= pmsa( ipnt( 287) )
         kPDifPO4            	= pmsa( ipnt( 288) )
         kNDifNO3            	= pmsa( ipnt( 289) )
         kNDifNH4            	= pmsa( ipnt( 290) )
         kO2Dif              	= pmsa( ipnt( 291) )
         cThetaDif           	= pmsa( ipnt( 292) )
         fDepthDifS          	= pmsa( ipnt( 293) )
         cTurbDifNut         	= pmsa( ipnt( 294) )
         cTurbDifO2          	= pmsa( ipnt( 295) )
         kPSorp              	= pmsa( ipnt( 296) )
         cRelPAdsD           	= pmsa( ipnt( 297) )
         cRelPAdsFe          	= pmsa( ipnt( 298) )
         cRelPAdsAl          	= pmsa( ipnt( 299) )
         cKPAdsOx            	= pmsa( ipnt( 300) )
         fRedMax             	= pmsa( ipnt( 301) )
         coPO4Max            	= pmsa( ipnt( 302) )
         kPChemPO4           	= pmsa( ipnt( 303) )
         cDayManVeg1         	= pmsa( ipnt( 304) )
         cDayManVeg2         	= pmsa( ipnt( 305) )
         fManVeg             	= pmsa( ipnt( 306) )
         cLengMan            	= pmsa( ipnt( 307) )
         cYearStartBirds     	= pmsa( ipnt( 308) )
         cDayStartBirds      	= pmsa( ipnt( 309) )
         cDayEndBirds        	= pmsa( ipnt( 310) )
         cBirdsPerha         	= pmsa( ipnt( 311) )
         cDGrazPerBird       	= pmsa( ipnt( 312) )
         hDVegBird           	= pmsa( ipnt( 313) )
         fDAssBird           	= pmsa( ipnt( 314) )
         fDissEgesBird       	= pmsa( ipnt( 315) )
         fDissMortVeg        	= pmsa( ipnt( 316) )
         cLengAllo           	= pmsa( ipnt( 317) )
         cLengMort           	= pmsa( ipnt( 318) )
         UseEmpUpt           	= pmsa( ipnt( 319) )
         fSedUptVegMax       	= pmsa( ipnt( 320) )
         fSedUptVegCoef      	= pmsa( ipnt( 321) )
         fSedUptVegExp       	= pmsa( ipnt( 322) )
         fRootVegSum         	= pmsa( ipnt( 323) )
         fRootVegWin         	= pmsa( ipnt( 324) )
         fFloatVeg           	= pmsa( ipnt( 325) )
         fEmergVeg           	= pmsa( ipnt( 326) )
         fDepth1Veg          	= pmsa( ipnt( 327) )
         fDepth2Veg          	= pmsa( ipnt( 328) )
         cDLayerVeg          	= pmsa( ipnt( 329) )
         cCovSpVeg           	= pmsa( ipnt( 330) )
         kMigrVeg            	= pmsa( ipnt( 331) )
         cDVegIn             	= pmsa( ipnt( 332) )
         cTmInitVeg          	= pmsa( ipnt( 333) )
         cDCarrVeg           	= pmsa( ipnt( 334) )
         cMuMaxVeg           	= pmsa( ipnt( 335) )
         cQ10ProdVeg         	= pmsa( ipnt( 336) )
         hLRefVeg            	= pmsa( ipnt( 337) )
         cExtSpVeg           	= pmsa( ipnt( 338) )
         kDRespVeg           	= pmsa( ipnt( 339) )
         cQ10RespVeg         	= pmsa( ipnt( 340) )
         kMortVegSum         	= pmsa( ipnt( 341) )
         fWinVeg             	= pmsa( ipnt( 342) )
         cDayWinVeg          	= pmsa( ipnt( 343) )
         fDetWMortVeg        	= pmsa( ipnt( 344) )
         cPrefVegBird        	= pmsa( ipnt( 345) )
         cVPUptMaxVeg        	= pmsa( ipnt( 346) )
         cAffPUptVeg         	= pmsa( ipnt( 347) )
         cPDVegMin           	= pmsa( ipnt( 348) )
         cPDVegMax           	= pmsa( ipnt( 349) )
         cVNUptMaxVeg        	= pmsa( ipnt( 350) )
         cAffNUptVeg         	= pmsa( ipnt( 351) )
         cNDVegMin           	= pmsa( ipnt( 352) )
         cNDVegMax           	= pmsa( ipnt( 353) )
         cPACoefMin          	= pmsa( ipnt( 354) )
         cPACoefMax          	= pmsa( ipnt( 355) )
         hPACoef             	= pmsa( ipnt( 356) )
         cSecchiPlus         	= pmsa( ipnt( 357) )
         cEuph               	= pmsa( ipnt( 358) )
         cCovSpPhyt          	= pmsa( ipnt( 359) )
         cTmOptLoss          	= pmsa( ipnt( 360) )
         cSigTmLoss          	= pmsa( ipnt( 361) )
         fDissMortPhyt       	= pmsa( ipnt( 362) )
         fDissLoss           	= pmsa( ipnt( 363) )
         cMuMaxDiat          	= pmsa( ipnt( 364) )
         cTmOptDiat          	= pmsa( ipnt( 365) )
         cSigTmDiat          	= pmsa( ipnt( 366) )
         cExtSpDiat          	= pmsa( ipnt( 367) )
         UseSteeleDiat       	= pmsa( ipnt( 368) )
         cLOptRefDiat        	= pmsa( ipnt( 369) )
         hLRefDiat           	= pmsa( ipnt( 370) )
         cChDDiatMin         	= pmsa( ipnt( 371) )
         cChDDiatMax         	= pmsa( ipnt( 372) )
         kDRespDiat          	= pmsa( ipnt( 373) )
         kLossDiat           	= pmsa( ipnt( 374) )
         kMortDiatW          	= pmsa( ipnt( 375) )
         kMortDiatS          	= pmsa( ipnt( 376) )
         cVSetDiat           	= pmsa( ipnt( 377) )
         cVPUptMaxDiat       	= pmsa( ipnt( 378) )
         cAffPUptDiat        	= pmsa( ipnt( 379) )
         cPDDiatMin          	= pmsa( ipnt( 380) )
         cPDDiatMax          	= pmsa( ipnt( 381) )
         cVNUptMaxDiat       	= pmsa( ipnt( 382) )
         cAffNUptDiat        	= pmsa( ipnt( 383) )
         cNDDiatMin          	= pmsa( ipnt( 384) )
         cNDDiatMax          	= pmsa( ipnt( 385) )
         hSiAssDiat          	= pmsa( ipnt( 386) )
         cMuMaxGren          	= pmsa( ipnt( 387) )
         cTmOptGren          	= pmsa( ipnt( 388) )
         cSigTmGren          	= pmsa( ipnt( 389) )
         cExtSpGren          	= pmsa( ipnt( 390) )
         UseSteeleGren       	= pmsa( ipnt( 391) )
         hLRefGren           	= pmsa( ipnt( 392) )
         cLOptRefGren        	= pmsa( ipnt( 393) )
         cChDGrenMin         	= pmsa( ipnt( 394) )
         cChDGrenMax         	= pmsa( ipnt( 395) )
         kDRespGren          	= pmsa( ipnt( 396) )
         kLossGren           	= pmsa( ipnt( 397) )
         kMortGrenW          	= pmsa( ipnt( 398) )
         kMortGrenS          	= pmsa( ipnt( 399) )
         cVSetGren           	= pmsa( ipnt( 400) )
         cVPUptMaxGren       	= pmsa( ipnt( 401) )
         cAffPUptGren        	= pmsa( ipnt( 402) )
         cPDGrenMin          	= pmsa( ipnt( 403) )
         cPDGrenMax          	= pmsa( ipnt( 404) )
         cVNUptMaxGren       	= pmsa( ipnt( 405) )
         cAffNUptGren        	= pmsa( ipnt( 406) )
         cNDGrenMin          	= pmsa( ipnt( 407) )
         cNDGrenMax          	= pmsa( ipnt( 408) )
         hSiAssGren          	= pmsa( ipnt( 409) )
         cMuMaxBlue          	= pmsa( ipnt( 410) )
         cTmOptBlue          	= pmsa( ipnt( 411) )
         cSigTmBlue          	= pmsa( ipnt( 412) )
         cExtSpBlue          	= pmsa( ipnt( 413) )
         UseSteeleBlue       	= pmsa( ipnt( 414) )
         cLOptRefBlue        	= pmsa( ipnt( 415) )
         hLRefBlue           	= pmsa( ipnt( 416) )
         cChDBlueMin         	= pmsa( ipnt( 417) )
         cChDBlueMax         	= pmsa( ipnt( 418) )
         cCyDBlueMin         	= pmsa( ipnt( 419) )
         cCyDBlueMax         	= pmsa( ipnt( 420) )
         kDRespBlue          	= pmsa( ipnt( 421) )
         kLossBlue           	= pmsa( ipnt( 422) )
         kMortBlueW          	= pmsa( ipnt( 423) )
         kMortBlueS          	= pmsa( ipnt( 424) )
         cVSetBlue           	= pmsa( ipnt( 425) )
         cVPUptMaxBlue       	= pmsa( ipnt( 426) )
         cAffPUptBlue        	= pmsa( ipnt( 427) )
         cPDBlueMin          	= pmsa( ipnt( 428) )
         cPDBlueMax          	= pmsa( ipnt( 429) )
         cVNUptMaxBlue       	= pmsa( ipnt( 430) )
         cAffNUptBlue        	= pmsa( ipnt( 431) )
         cNDBlueMin          	= pmsa( ipnt( 432) )
         cNDBlueMax          	= pmsa( ipnt( 433) )
         hSiAssBlue          	= pmsa( ipnt( 434) )
         cDBentIn            	= pmsa( ipnt( 435) )
         kMigrBent           	= pmsa( ipnt( 436) )
         kMigrFish           	= pmsa( ipnt( 437) )
         cDFiJvIn            	= pmsa( ipnt( 438) )
         cDFiAdIn            	= pmsa( ipnt( 439) )
         kHarvFishWin        	= pmsa( ipnt( 440) )
         kHarvFishSum        	= pmsa( ipnt( 441) )
         cDPiscIn            	= pmsa( ipnt( 442) )
         kMigrPisc           	= pmsa( ipnt( 443) )
         kHarvPiscWin        	= pmsa( ipnt( 444) )
         kHarvPiscSum        	= pmsa( ipnt( 445) )
         cFiltMax            	= pmsa( ipnt( 446) )
         hFilt               	= pmsa( ipnt( 447) )
         cDCarrZoo           	= pmsa( ipnt( 448) )
         cPrefDiat           	= pmsa( ipnt( 449) )
         cPrefGren           	= pmsa( ipnt( 450) )
         cPrefBlue           	= pmsa( ipnt( 451) )
         cPrefDet            	= pmsa( ipnt( 452) )
         fDAssZoo            	= pmsa( ipnt( 453) )
         fDissEgesZoo        	= pmsa( ipnt( 454) )
         kDRespZoo           	= pmsa( ipnt( 455) )
         kMortZoo            	= pmsa( ipnt( 456) )
         fDissMortZoo        	= pmsa( ipnt( 457) )
         cTmOptZoo           	= pmsa( ipnt( 458) )
         cSigTmZoo           	= pmsa( ipnt( 459) )
         cDCarrBent          	= pmsa( ipnt( 460) )
         kDAssBent           	= pmsa( ipnt( 461) )
         hDFoodBent          	= pmsa( ipnt( 462) )
         fDAssBent           	= pmsa( ipnt( 463) )
         fDissEgesBent       	= pmsa( ipnt( 464) )
         kDRespBent          	= pmsa( ipnt( 465) )
         kMortBent           	= pmsa( ipnt( 466) )
         fDissMortBent       	= pmsa( ipnt( 467) )
         cTmOptBent          	= pmsa( ipnt( 468) )
         cSigTmBent          	= pmsa( ipnt( 469) )
         fDBone              	= pmsa( ipnt( 470) )
         fPBone              	= pmsa( ipnt( 471) )
         cDCarrFish          	= pmsa( ipnt( 472) )
         fDissEgesFish       	= pmsa( ipnt( 473) )
         fDissMortFish       	= pmsa( ipnt( 474) )
         cTmOptFish          	= pmsa( ipnt( 475) )
         cSigTmFish          	= pmsa( ipnt( 476) )
         cDayReprFish        	= pmsa( ipnt( 477) )
         fReprFish           	= pmsa( ipnt( 478) )
         fAgeFish            	= pmsa( ipnt( 479) )
         cRelVegFish         	= pmsa( ipnt( 480) )
         kDAssFiJv           	= pmsa( ipnt( 481) )
         hDZooFiJv           	= pmsa( ipnt( 482) )
         fDAssFiJv           	= pmsa( ipnt( 483) )
         kDRespFiJv          	= pmsa( ipnt( 484) )
         kMortFiJv           	= pmsa( ipnt( 485) )
         kDAssFiAd           	= pmsa( ipnt( 486) )
         hDBentFiAd          	= pmsa( ipnt( 487) )
         fDAssFiAd           	= pmsa( ipnt( 488) )
         kDRespFiAd          	= pmsa( ipnt( 489) )
         kMortFiAd           	= pmsa( ipnt( 490) )
         cDCarrPiscMax       	= pmsa( ipnt( 491) )
         cDCarrPiscMin       	= pmsa( ipnt( 492) )
         cDCarrPiscBare      	= pmsa( ipnt( 493) )
         cDPhraMinPisc       	= pmsa( ipnt( 494) )
         cCovVegMin          	= pmsa( ipnt( 495) )
         cRelPhraPisc        	= pmsa( ipnt( 496) )
         cRelVegPisc         	= pmsa( ipnt( 497) )
         kDAssPisc           	= pmsa( ipnt( 498) )
         hDVegPisc           	= pmsa( ipnt( 499) )
         hDFishPisc          	= pmsa( ipnt( 500) )
         fDAssPisc           	= pmsa( ipnt( 501) )
         fDissEgesPisc       	= pmsa( ipnt( 502) )
         kDRespPisc          	= pmsa( ipnt( 503) )
         kMortPisc           	= pmsa( ipnt( 504) )
         fDissMortPisc       	= pmsa( ipnt( 505) )
         cTmOptPisc          	= pmsa( ipnt( 506) )
         cSigTmPisc          	= pmsa( ipnt( 507) )
         cDepthSM            	= pmsa( ipnt( 508) )
         kExchMaxM           	= pmsa( ipnt( 509) )
         hfMarsh             	= pmsa( ipnt( 510) )
         fDTotSM0            	= pmsa( ipnt( 511) )
         fDOrgSM0            	= pmsa( ipnt( 512) )
         fDDetSM0            	= pmsa( ipnt( 513) )
         fPInorgSM0          	= pmsa( ipnt( 514) )
         cPDPhra0            	= pmsa( ipnt( 515) )
         cNDPhra0            	= pmsa( ipnt( 516) )
         cDensStemPhra       	= pmsa( ipnt( 517) )
         cTmInitPhra         	= pmsa( ipnt( 518) )
         fDAllPhra           	= pmsa( ipnt( 519) )
         kDAllPhra           	= pmsa( ipnt( 520) )
         cDStemPhra          	= pmsa( ipnt( 521) )
         cQ10ProdPhra        	= pmsa( ipnt( 522) )
         cMuPhraMax          	= pmsa( ipnt( 523) )
         cDShootPhraMax      	= pmsa( ipnt( 524) )
         cCovSpPhra          	= pmsa( ipnt( 525) )
         cPDPhraMin          	= pmsa( ipnt( 526) )
         cPDPhraMax          	= pmsa( ipnt( 527) )
         cNDPhraMin          	= pmsa( ipnt( 528) )
         cNDPhraMax          	= pmsa( ipnt( 529) )
         cAffNUptPhra        	= pmsa( ipnt( 530) )
         cAffPUptPhra        	= pmsa( ipnt( 531) )
         cVNUptPhraMax       	= pmsa( ipnt( 532) )
         cVPUptPhraMax       	= pmsa( ipnt( 533) )
         kDRespPhra          	= pmsa( ipnt( 534) )
         cQ10RespPhra        	= pmsa( ipnt( 535) )
         fDayWin             	= pmsa( ipnt( 536) )
         fDRealPhra          	= pmsa( ipnt( 537) )
         kDRealPhra          	= pmsa( ipnt( 538) )
         kDMortShootPhra     	= pmsa( ipnt( 539) )
         kDMortRootPhra      	= pmsa( ipnt( 540) )
         cDayWinPhra         	= pmsa( ipnt( 541) )
         cDayManPhra         	= pmsa( ipnt( 542) )
         fManPhra            	= pmsa( ipnt( 543) )
         kDManShootPhra      	= pmsa( ipnt( 544) )
         DaysPerYear         	= pmsa( ipnt( 545) )
         TenDays             	= pmsa( ipnt( 546) )
         HoursPerDay         	= pmsa( ipnt( 547) )
         SecsPerDay          	= pmsa( ipnt( 548) )
         mmPerm              	= pmsa( ipnt( 549) )
         m2Perha             	= pmsa( ipnt( 550) )
         mgPerg              	= pmsa( ipnt( 551) )
         gPerkg              	= pmsa( ipnt( 552) )
         gPerton             	= pmsa( ipnt( 553) )
         PerCent             	= pmsa( ipnt( 554) )
         NearZero            	= pmsa( ipnt( 555) )
         molO2molC           	= pmsa( ipnt( 556) )
         molO2molN           	= pmsa( ipnt( 557) )
         molNmolC            	= pmsa( ipnt( 558) )
         cRhoWat             	= pmsa( ipnt( 559) )
         Pi                  	= pmsa( ipnt( 560) )

!   *****     DUPROL code inserted here    *****

!
!
!     /* ==============================  */
!     /* Model equations                 */
!     /* ==============================  */
! 	time
		sTime = ITIME / 86400
! 	depth
		sDepthW = TotalDepth
! 	Time_in_years
		TimeYears = sTime / DaysPerYear
! 	Time_(daynumber)_within_the_year_(0-365)
		Day = sTime - floor(TimeYears) * DaysPerYear
! 	Time_in_calendar_years
		Years = YearZero + TimeYears
! 	Forcing_function_temperature
		if (0 == InclSeason) then
		uTm = cTmAve 		else if (ReadTemp == 1) then		uTm = mTemp 		else		uTm = cTmAve - cTmVar * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysPerYear) 		endif
! 	Forcing_function_wind_speed
		if (ReadVWind == 1)then
		uVWind = mVWind 		else		uVWind = cVWind 		endif
! 	day_length
		if (0 == InclSeason) then
		ufDay = cfDayAve 		else		ufDay = cfDayAve - cfDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) 		endif
! 	total_daily_radiation
		if (0 == InclSeason) then
		uLDay = cLDayAve 		else if (ReadLOut == 1) then		uLDay = 0.0 		else		uLDay = cLDayAve - cLDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) 		endif
! 	average_light_intensity_during_daytime
		if (0 == InclSeason) then
		uLOut = uLDay / SecsPerDay / ufDay 		else if (ReadLOut == 1) then		uLOut = mLOut / ufDay 		else		uLOut = uLDay / SecsPerDay / ufDay 		endif
! 	average_PAR_at_zero_depth
		uLPARSurf = fPAR * (1.0 - fRefl) * uLOut
! 	contribution_of_algae_to_extinction
		aExtPhyt = cExtSpDiat * sDDiatW + cExtSpGren * sDGrenW + cExtSpBlue * sDBlueW
! 	detrital_contribution_to_extinction
		aExtDet = cExtSpDet * sDDetW
! 	contribution_of_inert_matter_to_extinction
		aExtIM = cExtSpIM * sDIMW
! 	extinction_coefficient_without_vegetation
		aExtCoefOpen = cExtWat + aExtIM + aExtDet + aExtPhyt
! 	seasonal_inflow
		if (0 == UseSeasonLoad) then
		uQInSeason = 0.0 		else if (Day < cDayApril1 - 0.5*cLengChange) then		uQInSeason = cQInWin 		else if (Day < cDayApril1 + 0.5*cLengChange) then		uQInSeason = 0.5*(cQInWin + cQInSum) + 0.5*(cQInWin - cQInSum) * cos(Pi/cLengCha&
		&nge * (Day - cDayApril1)) 		else if (Day < cDayOct1 - 0.5*cLengChange) then		uQInSeason = cQInSum 		else if (Day < cDayOct1 + 0.5*cLengChange) then		uQInSeason = 0.5*(cQInWin + cQInSum) - 0.5*(cQInWin - cQInSum) * cos(Pi/cLengCha&
		&nge * (Day - cDayOct1)) 		else		uQInSeason = cQInWin 		endif
! 	sinusoid_evaporation
		uQEvSinus = cQEvAve - cQEvVar * cos(2.0*Pi * (sTime + TenDays - cTimeLag) / Days&
		&PerYear)
! 	evaporation
		if (0 == InclSeason) then
		uQEv = cQEvAve 		else if (ReadQEv == 1) then		uQEv = mQEv 		else		uQEv = uQEvSinus 		endif
! 	extra_inflow_(for_periodic_water_level_regulation
		if ((Day >= cDayApril1 - 0.5*cLengChange) .and. (Day < cDayApril1 + 0.5*cLengCha&
		&nge)) then
		uQInExtra = cQInExtraApril1 		else if ( (Day >= cDayOct1 - 0.5*cLengChange) .and. (Day < cDayOct1 + 0.5*cLengC&
		&hange) ) then		uQInExtra = cQInExtraOct1 		else		uQInExtra = 0.0 		endif
! 	extra_outflow_(for_periodic_water_level_regulation
		if ((Day >= cDayApril1 - 0.5*cLengChange) .and. (Day < cDayApril1 + 0.5*cLengCha&
		&nge)) then
		uQOutExtra = cQOutExtraApril1 		else if ( (Day >= cDayOct1 - 0.5*cLengChange) .and. (Day < cDayOct1 + 0.5*cLengC&
		&hange) ) then		uQOutExtra = cQOutExtraOct1 		else		uQOutExtra = 0.0 		endif
! 	inflow
		if (ReadQIn == 1) then
		uQIn = mQIn 		else if (UseSeasonLoad == 1) then		uQIn = uQInSeason + uQInExtra 		else		uQIn = cQIn + uQInExtra 		endif
! 	outflow
		if (ReadQOut == 1) then
		uQOut = max(mQOut, (sDepthW - cDepthWMax) * mmPerm) 		else		uQOut = max(0.0, (uQIn - uQInExtra) - uQEv - cQInf) + uQOutExtra 		endif
! 	inflow_minus_evaporation
		uQDil = uQIn - uQEv
! 	dilution_rate_of_substances
		ukDil = uQDil / mmPerm / sDepthW
! 	dilution_rate_of_water
		ukDilWat = uQIn / mmPerm / sDepthW
! 	outflow_rate
		ukOut = uQOut / mmPerm / sDepthW
! 	water_residence_time
		uTauWat = 1.0 / (ukDilWat+NearZero)
! 	residence_time_of_substances
		uTauSubst = 1.0 / (ukDil+NearZero)
! 	change_in_water_depth
		if (0 == InclTran) then
		vTranDepthW = 0.0 		else if (0 == InclMarsh .or. fMarsh <= NearZero) then		vTranDepthW = (uQIn - uQEv - cQInf - uQOut) / mmPerm 		else		vTranDepthW = ((uQIn - uQEv - cQInf - uQOut) / mmPerm) / (1.0 + fMarsh) 		endif
! 	marsh_water_exchange_coefficient
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		akExchM = kExchMaxM * hfMarsh /(hfMarsh + fMarsh) + vTranDepthW / sDepthWM 		else		akExchM = 0.0 		endif
! 	relative_marsh_volume
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afVolMarsh = fMarsh * sDepthWM / sDepthW 		else		afVolMarsh = 0.0 		endif
! 	lake_water_exchange_coefficient
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		akExchL = akExchM * afVolMarsh 		else		akExchL = 0.0 		endif
! 	total_DW_phytoplankton_in_lake_water
		oDPhytW = sDDiatW + sDGrenW + sDBlueW
! 	total_P_phytoplankton_in_lake_water
		oPPhytW = sPDiatW + sPGrenW + sPBlueW
! 	total_N_phytoplankton_in_lake_water
		oNPhytW = sNDiatW + sNGrenW + sNBlueW
! 	total_DW_phytoplankton_on_lake_sediment
		if (InclPhytS == 1) then
		aDPhytS = sDDiatS + sDGrenS + sDBlueS 		else		aDPhytS = NearZero 		endif
! 	total_P_phytoplankton_on_lake_sediment
		if (InclPhytS == 1) then
		aPPhytS = sPDiatS + sPGrenS + sPBlueS 		else		aPPhytS = NearZero 		endif
! 	total_N_phytoplankton_on_lake_sediment
		if (InclPhytS == 1) then
		aNPhytS = sNDiatS + sNGrenS + sNBlueS 		else		aNPhytS = NearZero 		endif
! 	organic_seston
		oDOMW = sDDetW + oDPhytW
! 	total_seston
		oDSestW = oDOMW + sDIMW
! 	organic_P_in_water
		oPOMW = oPPhytW + sPDetW
! 	total_seston_P_(incl_adsorbed
		oPSestW = oPPhytW + sPDetW + sPAIMW
! 	inorganic_P_in_water
		oPInorgW = sPO4W + sPAIMW
! 	total_P_in_water_(excl_animals_AND_vegetation)
		oPTotW = oPSestW + sPO4W
! 	SRN_in_water
		oNDissW = sNO3W + sNH4W
! 	orgseston_N
		oNOMW = oNPhytW + sNDetW
! 	total_seston_N
		oNSestW = oNOMW
! 	kjeldahl_N_in_water
		oNkjW = oNSestW + sNH4W
! 	total_N_in_water_(without_animals_AND_vegetation)
		oNTotW = oNkjW + sNO3W
! 	porosity
		bPorS = (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat &
		&/ ( fDTotS0 + (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cR&
		&hoWat )
! 	sediment_porosity_corrected_for_tortuosity
		bPorCorS = ((bPorS )** (bPorS + 1.0))
! 	total_sediment_(excl_biota)
		aDTotS = sDIMS + sDHumS + sDDetS
! 	(apparent)_bulk_density_of_sediment
		aRhoTotS = aDTotS / cDepthS
! 	average_solid_density
		aRhoSolidS = (sDIMS * cRhoIM + (sDHumS + sDDetS) * cRhoOM) / aDTotS
! 	sediment_dry-weight_fraction
		afDTotS = 1.0 / (1.0 + bPorS/(1.0-bPorS) * cRhoWat / aRhoSolidS)
! 	total_organic_fraction_of_sediment_DW
		afDOrgS = (sDHumS + sDDetS) / aDTotS
! 	detrital_fraction_of_sediment_organic_DW
		afDetS = sDDetS / (sDHumS + sDDetS)
! 	detrital_fraction_of_total_sediment_DW
		afDetTotS = sDDetS / (sDIMS + sDHumS + sDDetS)
! 	inorganic_P_in_sediment
		aPInorgS = sPO4S + sPAIMS
! 	total_P_in_sediment_(excl_humus_animals_AND_vegetation)
		aPTotAvailS = sPDetS + aPInorgS + aPPhytS
! 	total_P_in_sediment_(excl_animals_AND_vegetation)
		aPTotS = aPTotAvailS + sPHumS
! 	fraction_inorganic_P_in_sediment
		afPInorgS = aPInorgS / aDTotS
! 	total_P_fraction_in_sediment
		afPTotS = aPTotS / aDTotS
! 	fraction_dissolved_P_in_sediment
		afPO4S = sPO4S / (aPTotAvailS + NearZero)
! 	conc_dissolved_P_in_interstitial_water
		oPO4S = sPO4S / cDepthS / bPorS
! 	total_dissolved_N_in_pore_water
		aNDissS = sNH4S + sNO3S
! 	kjeldahl_N_in_sediment_excl_humus
		aNkjAvailS = sNDetS + aNPhytS + sNH4S
! 	kjeldahl_N_in_sediment
		aNkjS = aNkjAvailS + sNHumS
! 	total_N_in_sediment_excl_humus
		aNTotAvailS = aNkjAvailS + sNO3S
! 	total_N_in_sediment
		aNTotS = aNkjS + sNO3S
! 	fraction_inorganic_N_in_sediment
		afNInorgS = aNDissS / aDTotS
! 	total_N_fraction_in_sediment
		afNTotS = aNTotS / aDTotS
! 	conc_dissolved_N-NO3_in_interstitial_water
		oNO3S = sNO3S / cDepthS / bPorS
! 	conc_dissolved_N-NH4_in_interstitial_water
		oNH4S = sNH4S / cDepthS / bPorS
! 	Dissolved_N_conc_in_sediment_needed_for_calc_of_veg_uptake_rate
		oNDissS = aNDissS / cDepthS / bPorS
! 	P/D_ratio_of_water_DIM
		rPDIMW = sPAIMW / sDIMW
! 	P/D_ratio_of_sediment_DIM
		rPDIMS = sPAIMS / sDIMS
! 	P/D_ratio_of_water_detritus
		rPDDetW = sPDetW / (sDDetW+NearZero)
! 	N/D_ratio_of_water_detritus
		rNDDetW = sNDetW / (sDDetW+NearZero)
! 	Si/D_ratio_of_water_detritus
		rSiDDetW = sSiDetW / (sDDetW+NearZero)
! 	P_content_of_sediment_OM
		rPDHumS = sPHumS / (sDHumS+NearZero)
! 	N_content_of_sediment_OM
		rNDHumS = sNHumS / (sDHumS+NearZero)
! 	P_content_of_sediment_detritus
		rPDDetS = sPDetS / (sDDetS+NearZero)
! 	N_content_of_sediment_detritus
		rNDDetS = sNDetS / (sDDetS+NearZero)
! 	Si_content_of_sediment_detritus
		rSiDDetS = sSiDetS / (sDDetS+NearZero)
! 	total_DW_phytoplankton_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oDPhytWM = sDDiatWM + sDGrenWM + sDBlueWM 		else		oDPhytWM = 0.0 		endif
! 	total_P_phytoplankton_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPPhytWM = sPDiatWM + sPGrenWM + sPBlueWM 		else		oPPhytWM = 0.0 		endif
! 	total_N_phytoplankton_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNPhytWM = sNDiatWM + sNGrenWM + sNBlueWM 		else		oNPhytWM = 0.0 		endif
! 	total_Si_diatoms_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oSiDiatWM = cSiDDiat * sDDiatWM 		else		oSiDiatWM = 0.0 		endif
! 	organic_seston
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oDOMWM = sDDetWM + oDPhytWM 		else		oDOMWM = 0.0 		endif
! 	total_seston
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oDSestWM = oDOMWM + sDIMWM 		else		oDSestWM = 0.0 		endif
! 	organic_P_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPOMWM = oPPhytWM + sPDetWM 		else		oPOMWM = 0.0 		endif
! 	total_seston_P(incl_adsorbed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPSestWM = oPPhytWM + sPDetWM + sPAIMWM 		else		oPSestWM = 0.0 		endif
! 	inorganic_P_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPInorgWM = sPO4WM + sPAIMWM 		else		oPInorgWM = 0.0 		endif
! 	total_P_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPTotWM = oPSestWM + sPO4WM 		else		oPTotWM = 0.0 		endif
! 	SRN_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNDissWM = sNO3WM + sNH4WM 		else		oNDissWM = 0.0 		endif
! 	orgseston_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNOMWM = oNPhytWM + sNDetWM 		else		oNOMWM = 0.0 		endif
! 	total_seston_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNSestWM = oNOMWM 		else		oNSestWM = 0.0 		endif
! 	kjeldahl_N_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNkjWM = oNSestWM + sNH4WM 		else		oNkjWM = 0.0 		endif
! 	total_N_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNTotWM = oNkjWM + sNO3WM 		else		oNTotWM = 0.0 		endif
! 	porosity
		bPorSM = (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) / cRhoW&
		&at /(fDTotSM0 + (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) &
		&/ cRhoWat)
! 	sediment_porosity_corrected_for_tortuosity
		bPorCorSM = ((bPorSM )** (bPorSM + 1.0))
! 	total_sediment(excl_biota)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aDTotSM = sDIMSM + sDHumSM + sDDetSM 		else		aDTotSM = 0.0 		endif
! 	(apparent)_bulk_density_of_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aRhoTotSM = aDTotSM / cDepthSM 		else		aRhoTotSM = 0.0 		endif
! 	average_solid_density
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aRhoSolidSM = (sDIMSM * cRhoIM +(sDHumSM + sDDetSM) * cRhoOM) / aDTotSM 		else		aRhoSolidSM = 0.0 		endif
! 	sediment_dry-weight_fraction
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afDTotSM = 1.0 /(1.0 + bPorSM/(1.0-bPorSM) * cRhoWat / aRhoSolidSM) 		else		afDTotSM = 0.0 		endif
! 	total_organic_fraction_of_sediment_DW
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afDOrgSM = (sDHumSM + sDDetSM) / aDTotSM 		else		afDOrgSM = 0.0 		endif
! 	detrital_fraction_of_sediment_organic_DW
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afDetSM = sDDetSM /(sDHumSM + sDDetSM) 		else		afDetSM = 0.0 		endif
! 	detrital_fraction_of_total_sediment_DW
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afDetTotSM = sDDetSM /(sDIMSM + sDHumSM + sDDetSM) 		else		afDetTotSM = 0.0 		endif
! 	inorganic_P_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPInorgSM = sPO4SM + sPAIMSM 		else		aPInorgSM = 0.0 		endif
! 	total_P_in_sediment(excl_humusanimals_AND_vegetation)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPTotAvailSM = sPDetSM + aPInorgSM 		else		aPTotAvailSM = 0.0 		endif
! 	total_P_in_sediment(excl_animals_AND_vegetation)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPTotSM = aPTotAvailSM + sPHumSM 		else		aPTotSM = 0.0 		endif
! 	fraction_inorganic_P_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afPInorgSM = aPInorgSM / aDTotSM 		else		afPInorgSM = 0.0 		endif
! 	total_P_fraction_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afPTotSM = aPTotSM / aDTotSM 		else		afPTotSM = 0.0 		endif
! 	fraction_dissolved_P_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afPO4SM = sPO4SM /(aPTotAvailSM + NearZero) 		else		afPO4SM = 0.0 		endif
! 	conc_dissolved_P_in_interstitial_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oPO4SM = sPO4SM / cDepthSM / bPorSM 		else		oPO4SM = 0.0 		endif
! 	total_dissolved_N_in_pore_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNDissSM = sNH4SM + sNO3SM 		else		aNDissSM = 0.0 		endif
! 	kjeldahl_N_in_sedimentexcl_humus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNkjAvailSM = sNDetSM + sNH4SM 		else		aNkjAvailSM = 0.0 		endif
! 	kjeldahl_N_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNkjSM = aNkjAvailSM + sNHumSM 		else		aNkjSM = 0.0 		endif
! 	total_N_in_sedimentexcl_humus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNTotAvailSM = aNkjAvailSM + sNO3SM 		else		aNTotAvailSM = 0.0 		endif
! 	total_N_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNTotSM = aNkjSM + sNO3SM 		else		aNTotSM = 0.0 		endif
! 	fraction_inorganic_N_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afNInorgSM = aNDissSM / aDTotSM 		else		afNInorgSM = 0.0 		endif
! 	total_N_fraction_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afNTotSM = aNTotSM / aDTotSM 		else		afNTotSM = 0.0 		endif
! 	conc_dissolved_N-NO3_in_interstitial_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNO3SM = sNO3SM / cDepthSM / bPorSM 		else		oNO3SM = 0.0 		endif
! 	conc_dissolved_N-NH4_in_interstitial_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNH4SM = sNH4SM / cDepthSM / bPorSM 		else		oNH4SM = 0.0 		endif
! 	Dissolved_N_conc_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		oNDissSM = aNDissSM / cDepthSM / bPorSM 		else		oNDissSM = 0.0 		endif
! 	P/D_ratio_of_DIM_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDIMWM = sPAIMWM / sDIMWM 		else		rPDIMWM = 0.0 		endif
! 	P/D_ratio_of_DIM_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDIMSM = sPAIMSM / sDIMSM 		else		rPDIMSM = 0.0 		endif
! 	P/D_ratio_of_marsh_water_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDDetWM = sPDetWM /(sDDetWM+NearZero) 		else		rPDDetWM = 0.0 		endif
! 	N/D_ratio_of_marsh_water_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rNDDetWM = sNDetWM /(sDDetWM+NearZero) 		else		rNDDetWM = 0.0 		endif
! 	Si/D_ratio_of_marsh_water_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rSiDDetWM = sSiDetWM /(sDDetWM+NearZero) 		else		rSiDDetWM = 0.0 		endif
! 	P_content_of_marsh_sediment_OM
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDHumSM = sPHumSM /(sDHumSM+NearZero) 		else		rPDHumSM = 0.0 		endif
! 	N_content_of_marsh_sediment_OM
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rNDHumSM = sNHumSM /(sDHumSM+NearZero) 		else		rNDHumSM = 0.0 		endif
! 	P_content_of_marsh_sediment_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDDetSM = sPDetSM /(sDDetSM+NearZero) 		else		rPDDetSM = 0.0 		endif
! 	N_content_of_marsh_sediment_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rNDDetSM = sNDetSM /(sDDetSM+NearZero) 		else		rNDDetSM = 0.0 		endif
! 	Si_content_of_marsh_sediment_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rSiDDetSM = sSiDetSM /(sDDetSM+NearZero) 		else		rSiDDetSM = 0.0 		endif
! 	total_D_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aDTotM = ((sDIMWM + sDDetWM + oDPhytWM + sDZooM) * sDepthWM + sDIMSM + sDHumSM +&
		& sDDetSM + sDShootPhra + sDRootPhra) * fMarsh 		else		aDTotM = 0.0 		endif
! 	total_P_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPTotM = ((sPO4WM + sPDetWM + sPAIMWM + oPPhytWM + sPZooM) * sDepthWM + sPO4SM +&
		& sPHumSM + sPDetSM + sPAIMSM + sPShootPhra + sPRootPhra) * fMarsh 		else		aPTotM = 0.0 		endif
! 	total_N_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNTotM = ((sNH4WM + sNO3WM + sNDetWM + oNPhytWM + sNZooM) * sDepthWM + sNH4SM + &
		&sNO3SM + sNHumSM + sNDetSM + sNShootPhra + sNRootPhra) * fMarsh 		else		aNTotM = 0.0 		endif
! 	total_Si_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aSiTotM = ((sSiO2WM + sSiDetWM + oSiDiatWM) * sDepthWM + sSiDetSM) * fMarsh 		else		aSiTotM = 0.0 		endif
! 	-
		iPPulse = 1
! 	seasonal_P_load
		if (0 == UseSeasonLoad) then
		uPLoadSeason = 0.0 		else if (Day < cDayApril1 - 0.5*cLengChange) then		uPLoadSeason = cPLoadWin 		else if (Day < cDayApril1 + 0.5*cLengChange) then		uPLoadSeason = 0.5*(cPLoadWin + cPLoadSum) + 0.5*(cPLoadWin - cPLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayApril1)) 		else if (Day < cDayOct1 - 0.5*cLengChange) then		uPLoadSeason = cPLoadSum 		else if (Day < cDayOct1 + 0.5*cLengChange) then		uPLoadSeason = 0.5*(cPLoadWin + cPLoadSum) - 0.5*(cPLoadWin - cPLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayOct1)) 		else		uPLoadSeason = cPLoadWin 		endif
! 	P_load
		if (ReadPLoad == 1 .and. ReadNutFrac == 1) then
		uPLoad = mPLoadPO4 + mPLoadOrg 		else if (ReadPLoad == 1 .and. 0 == ReadNutFrac) then		uPLoad = mPLoad 		else if (UsePulseLoad == 1) then		uPLoad = 0.0 		else if (UseSeasonLoad == 1) then		uPLoad = uPLoadSeason 		else		uPLoad = cPLoad 		endif
! 	P_load_PO4
		if (0 == ReadPLoad) then
		uPLoadPO4 = fPO4In * uPLoad 		else if (ReadNutFrac == 1) then		uPLoadPO4 = mPLoadPO4 		else		uPLoadPO4 = fPO4In * mPLoad 		endif
! 	P_load_bound_to_org_matter
		if (0 == ReadPLoad) then
		uPLoadOrg = (1.0 - fPO4In) * uPLoad 		else if (ReadNutFrac == 1) then		uPLoadOrg = mPLoadOrg 		else		uPLoadOrg = (1.0 - fPO4In) * mPLoad 		endif
! 	(total)_algal_P_input
		if (0 == InclSeason) then
		uPLoadPhytTot = (((fPhytInSum + fPhytInWin)/2)*uPLoadOrg) 		else if (ReadPLoadPhyt == 1) then		uPLoadPhytTot = mPLoadPhytTot 		else		uPLoadPhytTot = ((fPhytInSum+fPhytInWin)/2.0 - (fPhytInSum-fPhytInWin)/2.0 * cos&
		&(2.0*Pi * (sTime + TenDays - cTimeLag) / DaysPerYear)) * uPLoadOrg 		endif
! 	detrital_P_input
		uPLoadDet = uPLoadOrg - uPLoadPhytTot
! 	Adsorbed_P_loading_(=0)
		uPLoadAIM = 0.0
! 	-
		iNPulse = 1
! 	seasonal_N_load
		if (0 == UseSeasonLoad) then
		uNLoadSeason = 0.0 		else if (Day < cDayApril1 - 0.5*cLengChange) then		uNLoadSeason = cNLoadWin 		else if (Day < cDayApril1 + 0.5*cLengChange) then		uNLoadSeason = 0.5*(cNLoadWin + cNLoadSum) + 0.5*(cNLoadWin - cNLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayApril1)) 		else if (Day < cDayOct1 - 0.5*cLengChange) then		uNLoadSeason = cNLoadSum 		else if (Day < cDayOct1 + 0.5*cLengChange) then		uNLoadSeason = 0.5*(cNLoadWin + cNLoadSum) - 0.5*(cNLoadWin - cNLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayOct1)) 		else		uNLoadSeason = cNLoadWin 		endif
! 	(total)_algal_N_input
		uNLoadPhytTot = cNPPhytIn * uPLoadPhytTot
! 	N_load
		if (ReadNLoad == 1 .and. ReadNutFrac == 1) then
		uNLoad = mNLoadNH4 + mNLoadNO3 + mNLoadOrg 		else if (ReadNLoad == 1 .and. 0 == ReadNutFrac) then		uNLoad = mNLoad 		else if (UsePulseLoad == 1) then		uNLoad = 0.0 		else if (ReadPLoad == 1) then		uNLoad = cNPLoadMeas * uPLoad 		else if (UseSeasonLoad == 1) then		uNLoad = uNLoadSeason 		else		uNLoad = cNLoad 		endif
! 	N_load_detritus
		if (0 == ReadNLoad) then
		uNLoadDet = min(cNPDetIn * uPLoadDet, uNLoad - uNLoadPhytTot) 		else if (ReadNutFrac == 1) then		uNLoadDet = 0.0 		else		uNLoadDet = min(cNPDetIn * uPLoadDet, uNLoad - uNLoadPhytTot) 		endif
! 	loading_N_bound_to_org_matter
		if (0 == ReadNLoad) then
		uNLoadOrg = uNLoadPhytTot + uNLoadDet 		else if (ReadNutFrac == 1) then		uNLoadOrg = mNLoadOrg 		else		uNLoadOrg = uNLoadPhytTot + uNLoadDet 		endif
! 	N_loading_dissolved_(sum_of_NO2_and_NH4)
		if (0 == ReadNLoad) then
		uNLoadDiss = uNLoad - uNLoadOrg 		else if (ReadNutFrac == 1) then		uNLoadDiss = 0.0 		else		uNLoadDiss = uNLoad - uNLoadOrg 		endif
! 	NH4_loading
		if (0 == ReadNLoad) then
		uNLoadNH4 = fNH4DissIn * uNLoadDiss 		else if (ReadNutFrac == 1) then		uNLoadNH4 = mNLoadNH4 		else		uNLoadNH4 = fNH4DissIn * uNLoadDiss 		endif
! 	NO3_loading
		if (0 == ReadNLoad) then
		uNLoadNO3 = (1.0 - fNH4DissIn) * uNLoadDiss 		else if (ReadNutFrac == 1) then		uNLoadNO3 = mNLoadNO3 		else		uNLoadNO3 = (1.0 - fNH4DissIn) * uNLoadDiss 		endif
! 	external_N_conc
		uNTotIn = uNLoad / (uQIn / mmPerm + NearZero)
! 	detrital_DW_loading
		if (ReadDLoadDet == 1) then
		uDLoadDet = mDLoadDet 		else		uDLoadDet = uNLoadDet / cNDDetIn 		endif
! 	(total)_algal_DW_input
		uDLoadPhytTot = uNLoadPhytTot / cNDPhytIn
! 	loading_of_DW_of_inorg_matter
		if (ReadDLoadIM == 1) then
		uDLoadIM = mDLoadIM 		else		uDLoadIM = cDIMIn * uQIn / mmPerm 		endif
! 	total_DW_input
		uDLoad = uDLoadIM + uDLoadDet + uDLoadPhytTot
! 	external_P_concentration
		uPTotIn = uPLoad / (uQIn / mmPerm + NearZero)
! 	Diat_input
		uDLoadDiat = fDiatPhytIn * uDLoadPhytTot
! 	Diat_input
		uPLoadDiat = fDiatPhytIn * uPLoadPhytTot
! 	Diat_input
		uNLoadDiat = fDiatPhytIn * uNLoadPhytTot
! 	Gren_input
		uDLoadGren = fGrenPhytIn * uDLoadPhytTot
! 	Gren_input
		uPLoadGren = fGrenPhytIn * uPLoadPhytTot
! 	Gren_input
		uNLoadGren = fGrenPhytIn * uNLoadPhytTot
! 	Blue_input
		uDLoadBlue = fBluePhytIn * uDLoadPhytTot
! 	Blue_input
		uPLoadBlue = fBluePhytIn * uPLoadPhytTot
! 	Blue_input
		uNLoadBlue = fBluePhytIn * uNLoadPhytTot
! 	dilution_of_DW_IM
		wDDilIM = ukDil * sDIMW
! 	dilllution_of_detritus
		wDDilDet = ukDil * sDDetW
! 	dilution_of_SRP
		wPDilPO4 = ukDil * sPO4W
! 	dilution_of_detritus
		wPDilDet = ukDil*sPDetW
! 	dilution_of_IM-ads_P
		wPDilAIM = ukDil * sPAIMW
! 	dilution_of_ammonium
		wNDilNH4 = ukDil * sNH4W
! 	dilution_of_nitrate
		wNDilNO3 = ukDil * sNO3W
! 	dilution_of_detritus
		wNDilDet = ukDil * sNDetW
! 	oxygen_inflow
		wO2Inflow = ukDilWat * cO2In
! 	oxygen_outflow
		wO2Outfl = ukDil * sO2W
! 	dilution_of_Diat
		wDDilDiat = ukDil * sDDiatW
! 	dilution_of_Diat
		wPDilDiat = ukDil * sPDiatW
! 	dilution_of_Diat
		wNDilDiat = ukDil * sNDiatW
! 	dilution_of_Gren
		wDDilGren = ukDil * sDGrenW
! 	dilution_of_Gren
		wPDilGren = ukDil * sPGrenW
! 	dilution_of_Gren
		wNDilGren = ukDil * sNGrenW
! 	dilution_of_Blue
		wDDilBlue = ukDil * sDBlueW
! 	dilution_of_Blue
		wPDilBlue = ukDil * sPBlueW
! 	dilution_of_Blue
		wNDilBlue = ukDil * sNBlueW
! 	total_algal_dilution
		wDDilPhyt = wDDilDiat + wDDilGren + wDDilBlue
! 	total_algal_dilution
		wPDilPhyt = wPDilDiat + wPDilGren + wPDilBlue
! 	total_algal_dilution
		wNDilPhyt = wNDilDiat + wNDilGren + wNDilBlue
! 	Outflow_of_DW
		wDOutflTot = ukOut * oDSestW
! 	Outflow_of_P
		wPOutflTot = ukOut * oPTotW
! 	Outflow_of_N
		wNOutflTot = ukOut * oNTotW
! 	transport_flux_of_D_in_Diat
		if (0 == InclTran) then
		wDTranDiat = 0.0 		else		wDTranDiat = uDLoadDiat / sDepthW - wDDilDiat 		endif
! 	transport_flux_of_P_in_Diat
		if (0 == InclTran) then
		wPTranDiat = 0.0 		else		wPTranDiat = uPLoadDiat / sDepthW - wPDilDiat 		endif
! 	transport_flux_of_N_in_Diat
		if (0 == InclTran) then
		wNTranDiat = 0.0 		else		wNTranDiat = uNLoadDiat / sDepthW - wNDilDiat 		endif
! 	transport_flux_of_D_in_Gren
		if (0 == InclTran) then
		wDTranGren = 0.0 		else		wDTranGren = uDLoadGren / sDepthW - wDDilGren 		endif
! 	transport_flux_of_P_in_Gren
		if (0 == InclTran) then
		wPTranGren = 0.0 		else		wPTranGren = uPLoadGren / sDepthW - wPDilGren 		endif
! 	transport_flux_of_N_in_Gren
		if (0 == InclTran) then
		wNTranGren = 0.0 		else		wNTranGren = uNLoadGren / sDepthW - wNDilGren 		endif
! 	transport_flux_of_D_in_Blue
		if (0 == InclTran) then
		wDTranBlue = 0.0 		else		wDTranBlue = uDLoadBlue / sDepthW - wDDilBlue 		endif
! 	transport_flux_of_P_in_Blue
		if (0 == InclTran) then
		wPTranBlue = 0.0 		else		wPTranBlue = uPLoadBlue / sDepthW - wPDilBlue 		endif
! 	transport_flux_of_N_in_Blue
		if (0 == InclTran) then
		wNTranBlue = 0.0 		else		wNTranBlue = uNLoadBlue / sDepthW - wNDilBlue 		endif
! 	total_transport_flux_of_D_in_Phyt
		if (0 == InclTran) then
		wDTranPhyt = 0.0 		else		wDTranPhyt = wDTranDiat + wDTranGren + wDTranBlue 		endif
! 	total_transport_flux_of_P_in_Phyt
		if (0 == InclTran) then
		wPTranPhyt = 0.0 		else		wPTranPhyt = wPTranDiat + wPTranGren + wPTranBlue 		endif
! 	total_transport_flux_of_N_in_Phyt
		if (0 == InclTran) then
		wNTranPhyt = 0.0 		else		wNTranPhyt = wNTranDiat + wNTranGren + wNTranBlue 		endif
! 	total_transport_flux_of_Si_in_SiO2
		uSiLoadSiO2 = cSiO2In * uQIn / mmPerm
! 	total_transport_flux_of_Si_in_Det
		uSiLoadDet = cSiDDetIn * uDLoadDet
! 	total_transport_flux_of_Si_in_Diat
		uSiLoadDiat = cSiDDiat * uDLoadDiat
! 	Silica_loading
		uSiLoad = uSiLoadSiO2 + uSiLoadDet + uSiLoadDiat
! 	Dilution_of_Si_in_SiO2
		wSiDilSiO2 = ukDil * sSiO2W
! 	Dilution_of_Si_in_detritus
		wSiDilDet = ukDil * sSiDetW
! 	Dilution_of_Si_in_diatoms
		wSiDilDiat = cSiDDiat * wDDilDiat
! 	total_Si_surface_outflow
		wSiOutflTot = ukOut * (sSiO2W + sSiDetW + cSiDDiat * sDDiatW)
! 	transport_flux_of_Si_in_SIO2
		if (0 == InclTran) then
		wSiTranSiO2 = 0.0 		else		wSiTranSiO2 = uSiLoadSiO2 / sDepthW - wSiDilSiO2 		endif
! 	transport_flux_of_Si_in_detritus
		if (0 == InclTran) then
		wSiTranDetW = 0.0 		else		wSiTranDetW = uSiLoadDet / sDepthW - wSiDilDet 		endif
! 	total_Si_transport_flux
		if (0 == InclTran) then
		tSiTranTotT = 0.0 		else		tSiTranTotT = uSiLoadSiO2 + uSiLoadDet + uSiLoadDiat - (wSiDilDet + wSiDilSiO2 +&
		& wSiDilDiat) * sDepthW 		endif
! 	net_migration_flux_of_D_in_Zoo
		if (0 == InclTran) then
		wDTranZoo = 0.0 		else		wDTranZoo =( ukDilWat * cDZooIn - ukDil*sDZoo) 		endif
! 	net_migration_flux_of_P_in_ZOO
		if (0 == InclTran) then
		wPTranZoo = 0.0 		else		wPTranZoo =(ukDilWat * cPDZooRef*cDZooIn - ukDil*sPZoo) 		endif
! 	net_migration_flux_of_N_in_Zoo
		if (0 == InclTran) then
		wNTranZoo = 0.0 		else		wNTranZoo =(ukDilWat * cNDZooRef*cDZooIn - ukDil * sNZoo) 		endif
! 	transport_flux_DW_in_IM
		if (0 == InclTran) then
		wDTranIMW = 0.0 		else		wDTranIMW = uDLoadIM / sDepthW - wDDilIM 		endif
! 	transport_flux_DW_in_detritus
		if (0 == InclTran) then
		wDTranDetW = 0.0 		else		wDTranDetW = uDLoadDet / sDepthW - wDDilDet 		endif
! 	transport_flux_O2
		if (0 == InclTran) then
		wO2TranW = 0.0 		else		wO2TranW = wO2Inflow - wO2Outfl 		endif
! 	transport_flux_of_P_in_PO4
		if (0 == InclTran) then
		wPTranPO4W = 0.0 		else		wPTranPO4W = uPLoadPO4 / sDepthW - wPDilPO4 		endif
! 	transport_flux_of_P_in_AIM
		if (0 == InclTran) then
		wPTranAIMW = 0.0 		else		wPTranAIMW = uPLoadAIM / sDepthW - wPDilAIM 		endif
! 	transport_flux_of_P_in_detritus
		if (0 == InclTran) then
		wPTranDetW = 0.0 		else		wPTranDetW = uPLoadDet / sDepthW - wPDilDet 		endif
! 	transport_flux_of_N_in_NH4
		if (0 == InclTran) then
		wNTranNH4W = 0.0 		else		wNTranNH4W = uNLoadNH4 / sDepthW - wNDilNH4 		endif
! 	transport_flux_of_N_in_NO3
		if (0 == InclTran) then
		wNTranNO3W = 0.0 		else		wNTranNO3W = uNLoadNO3 / sDepthW - wNDilNO3 		endif
! 	transport_flux_of_N_in_detritus
		if (0 == InclTran) then
		wNTranDetW = 0.0 		else		wNTranDetW = uNLoadDet / sDepthW - wNDilDet 		endif
! 	Total_DW_dilution_fluxes
		wDDilTot = wDDilIM + wDDilDet + wDDilPhyt
! 	Total_P_dilution_fluxes
		wPDilTot = wPDilDet + wPDilPO4 + wPDilAIM + wPDilPhyt
! 	Total_N_dilution_fluxes
		wNDilTot = wNDilDet + wNDilNO3 + wNDilNH4 + wNDilPhyt
! 	Total_SI_dilution_fluxes
		wSiDilTot = wSiDilDet + wSiDilSiO2 + wSiDilDiat
! 	total_transport_fluxes_of_DW_for_mass_balance_equations
		if (0 == InclTran) then
		tDTranTotT = 0.0 		else		tDTranTotT = uDLoad - wDDilTot * sDepthW 		endif
! 	total_transport_fluxes_of_P_for_mass_balance_equations
		if (0 == InclTran) then
		tPTranTotT = 0.0 		else		tPTranTotT = uPLoad - wPDilTot * sDepthW 		endif
! 	total_transport_fluxes_of_N_for_mass_balance_equations
		if (0 == InclTran) then
		tNTranTotT = 0.0 		else		tNTranTotT = uNLoad - wNDilTot * sDepthW 		endif
! 	exchange_flux_of_DW_in_IMM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchIMM = akExchM *(sDIMW - sDIMWM) 		else		wDExchIMM = 0.0 		endif
! 	exchange_flux_of_P_in_PO4M_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchPO4M = akExchM *(sPO4W - sPO4WM) 		else		wPExchPO4M = 0.0 		endif
! 	exchange_flux_of_P_in_AIMM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchAIMM = akExchM *(sPAIMW - sPAIMWM) 		else		wPExchAIMM = 0.0 		endif
! 	exchange_flux_of_N_in_NH4M_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchNH4M = akExchM *(sNH4W - sNH4WM) 		else		wNExchNH4M = 0.0 		endif
! 	exchange_flux_of_N_in_NO3M_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchNO3M = akExchM *(sNO3W - sNO3WM) 		else		wNExchNO3M = 0.0 		endif
! 	exchange_flux_of_Si_in_hSiO2M_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchSiO2M = akExchM *(sSiO2W - sSiO2WM) 		else		wSiExchSiO2M = 0.0 		endif
! 	exchange_flux_of_O2_in_hM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wO2ExchM = akExchM *(sO2W - sO2WM) 		else		wO2ExchM = 0.0 		endif
! 	exchange_flux_of_DW_in_DetM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchDetM = akExchM *(sDDetW - sDDetWM) 		else		wDExchDetM = 0.0 		endif
! 	exchange_flux_of_P_in_DetM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchDetM = akExchM *(sPDetW - sPDetWM) 		else		wPExchDetM = 0.0 		endif
! 	exchange_flux_of_N_in_DetM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchDetM = akExchM *(sNDetW - sNDetWM) 		else		wNExchDetM = 0.0 		endif
! 	exchange_flux_of_Si_in_hDetM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchDetM = akExchM *(sSiDetW - sSiDetWM) 		else		wSiExchDetM = 0.0 		endif
! 	exchange_flux_of_DW_in_DiatM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchDiatM = akExchM *(sDDiatW - sDDiatWM) 		else		wDExchDiatM = 0.0 		endif
! 	exchange_flux_of_P_in_DiatM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchDiatM = akExchM *(sPDiatW - sPDiatWM) 		else		wPExchDiatM = 0.0 		endif
! 	exchange_flux_of_N_in_DiatM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchDiatM = akExchM *(sNDiatW - sNDiatWM) 		else		wNExchDiatM = 0.0 		endif
! 	exchange_flux_of_Si_in_DiatM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchDiatM = cSiDDiat * wDExchDiatM 		else		wSiExchDiatM = 0.0 		endif
! 	exchange_flux_of_DW_in_GrenM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchGrenM = akExchM *(sDGrenW - sDGrenWM) 		else		wDExchGrenM = 0.0 		endif
! 	exchange_flux_of_P_in_GrenM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchGrenM = akExchM *(sPGrenW - sPGrenWM) 		else		wPExchGrenM = 0.0 		endif
! 	exchange_flux_of_N_in_GrenM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchGrenM = akExchM *(sNGrenW - sNGrenWM) 		else		wNExchGrenM = 0.0 		endif
! 	exchange_flux_of_DW_in_BlueM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchBlueM = akExchM *(sDBlueW - sDBlueWM) 		else		wDExchBlueM = 0.0 		endif
! 	exchange_flux_of_P_in_BlueM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchBlueM = akExchM *(sPBlueW - sPBlueWM) 		else		wPExchBlueM = 0.0 		endif
! 	exchange_flux_of_N_in_BlueM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchBlueM = akExchM *(sNBlueW - sNBlueWM) 		else		wNExchBlueM = 0.0 		endif
! 	exchange_flux_of_DW_in_ZooM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchZooM = akExchM *(sDZoo - sDZooM) 		else		wDExchZooM = 0.0 		endif
! 	exchange_flux_of_P_in_ZooM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchZooM = akExchM *(sPZoo - sPZooM) 		else		wPExchZooM = 0.0 		endif
! 	exchange_flux_of_N_in_ZooM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchZooM = akExchM *(sNZoo - sNZooM) 		else		wNExchZooM = 0.0 		endif
! 	exchange_flux_of_DW_in_IM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchIM = akExchL *(sDIMW - sDIMWM) 		else		wDExchIM = 0.0 		endif
! 	exchange_flux_of_P_in_PO4_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchPO4 = akExchL *(sPO4W - sPO4WM) 		else		wPExchPO4 = 0.0 		endif
! 	exchange_flux_of_P_in_AIM_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchAIM = akExchL *(sPAIMW - sPAIMWM) 		else		wPExchAIM = 0.0 		endif
! 	exchange_flux_of_N_in_NH4_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchNH4 = akExchL *(sNH4W - sNH4WM) 		else		wNExchNH4 = 0.0 		endif
! 	exchange_flux_of_N_in_NO3_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchNO3 = akExchL *(sNO3W - sNO3WM) 		else		wNExchNO3 = 0.0 		endif
! 	exchange_flux_of_Si_in_SiO2_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchSiO2 = akExchL *(sSiO2W - sSiO2WM) 		else		wSiExchSiO2 = 0.0 		endif
! 	exchange_flux_of_O2_in_O2_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wO2Exch = akExchL *(sO2W - sO2WM) 		else		wO2Exch = 0.0 		endif
! 	exchange_flux_of_DW_in_Det_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchDet = akExchL *(sDDetW - sDDetWM) 		else		wDExchDet = 0.0 		endif
! 	exchange_flux_of_P_in_Det_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchDet = akExchL *(sPDetW - sPDetWM) 		else		wPExchDet = 0.0 		endif
! 	exchange_flux_of_N_in_Det_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchDet = akExchL *(sNDetW - sNDetWM) 		else		wNExchDet = 0.0 		endif
! 	exchange_flux_of_Si_in_Det_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchDet = akExchL *(sSiDetW - sSiDetWM) 		else		wSiExchDet = 0.0 		endif
! 	exchange_flux_of_DW_in_Diat_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchDiat = akExchL *(sDDiatW - sDDiatWM) 		else		wDExchDiat = 0.0 		endif
! 	exchange_flux_of_P_in_Diat_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchDiat = akExchL *(sPDiatW - sPDiatWM) 		else		wPExchDiat = 0.0 		endif
! 	exchange_flux_of_N_in_Diat_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchDiat = akExchL *(sNDiatW - sNDiatWM) 		else		wNExchDiat = 0.0 		endif
! 	exchange_flux_of_Si_in_Diat_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiExchDiat = cSiDDiat * wDExchDiat 		else		wSiExchDiat = 0.0 		endif
! 	exchange_flux_of_DW_in_Gren_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchGren = akExchL *(sDGrenW - sDGrenWM) 		else		wDExchGren = 0.0 		endif
! 	exchange_flux_of_P_in_Gren_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchGren = akExchL *(sPGrenW - sPGrenWM) 		else		wPExchGren = 0.0 		endif
! 	exchange_flux_of_N_in_Gren_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchGren = akExchL *(sNGrenW - sNGrenWM) 		else		wNExchGren = 0.0 		endif
! 	exchange_flux_of_DW_in_Blue_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchBlue = akExchL *(sDBlueW - sDBlueWM) 		else		wDExchBlue = 0.0 		endif
! 	exchange_flux_of_P_in_Blue_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchBlue = akExchL *(sPBlueW - sPBlueWM) 		else		wPExchBlue = 0.0 		endif
! 	exchange_flux_of_N_in_Blue_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchBlue = akExchL *(sNBlueW - sNBlueWM) 		else		wNExchBlue = 0.0 		endif
! 	exchange_flux_of_DW_in_Zoo_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDExchZoo = akExchL *(sDZoo - sDZooM) 		else		wDExchZoo = 0.0 		endif
! 	exchange_flux_of_P_in_Zoo_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPExchZoo = akExchL *(sPZoo - sPZooM) 		else		wPExchZoo = 0.0 		endif
! 	exchange_flux_of_N_in_Zoo_between_marsh_and_lake_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNExchZoo = akExchL *(sNZoo - sNZooM) 		else		wNExchZoo = 0.0 		endif
! 	infiltr_of_SRP
		if (cQInf >= 0.0) then
		tPInfPO4W = cQInf / mmPerm * sPO4W 		else		tPInfPO4W = cQInf / mmPerm * oPO4S 		endif
! 	infiltr_of_ammonium
		if (cQInf >= 0.0) then
		tNInfNH4W = cQInf / mmPerm * sNH4W 		else		tNInfNH4W = cQInf / mmPerm * oNH4S 		endif
! 	infiltr_of_nitrate
		if (cQInf >= 0.0) then
		tNInfNO3W = cQInf / mmPerm * sNO3W 		else		tNInfNO3W = cQInf / mmPerm * oNO3S 		endif
! 	infiltration_of_interst_PO4
		if (cQInf >= 0.0) then
		tPInfPO4S = cQInf / mmPerm * oPO4S 		else		tPInfPO4S = cQInf / mmPerm * cPO4Ground 		endif
! 	infiltration_of_interst_NH4
		if (cQInf >= 0.0) then
		tNInfNH4S = cQInf / mmPerm * oNH4S 		else		tNInfNH4S = cQInf / mmPerm * cNH4Ground 		endif
! 	infiltration_of_interst_NO3
		if (cQInf >= 0.0) then
		tNInfNO3S = cQInf / mmPerm * oNO3S 		else		tNInfNO3S = cQInf / mmPerm * cNO3Ground  		endif
! 	NH4_load_to_sediment_from_artificial_fertilizer
		tNH4LoadS = fNH4LoadS * cNLoadS
! 	NO3_load_to_sediment_from_artificial_fertilizer
		tNO3LoadS = cNLoadS - tNH4LoadS
! 	IM_input_from_banks
		uDErosIM = (1.0 - fDOrgSoil) * cDErosTot
! 	IM_input_to_sediment_from_banks
		uDErosIMS = fSedErosIM * uDErosIM
! 	IM_input_to_water_column_from_banks
		uDErosIMW = uDErosIM - uDErosIMS
! 	organic_matter_input_from_banks
		uDErosOM = fDOrgSoil * cDErosTot
! 	organic_P_input_from_banks
		uPErosOM = cPDSoilOM * uDErosOM
! 	organic_N_input_from_banks
		uNErosOM = cNDSoilOM * uDErosOM
! 	oxygen_saturation_concentration
		uO2Sat = 14.652 - 0.41022 * uTm + 0.007991 * uTm*uTm - 0.000077774 * uTm*uTm*uTm
! 	reaeration_coefficient
		kAer = cAerRoot * ((uVWind )** (0.5)) + cAerLin * uVWind + cAerSquare * uVWind*u&
		&VWind
! 	temperature_function_of_reaeration
		uFunTmAer = ((cThetaAer )** (uTm-cTmRef))
! 	duckweed_function_of_reaeration
		aFunLemnAer = 1.0
! 	reaeration_flux_of_O2_into_the_water
		tO2Aer = kAer * uFunTmAer * (uO2Sat - sO2W) * aFunLemnAer
! 	temp_function_of_fish
		if (InclWeb == 1) then
		uFunTmFish = exp( -0.5/(cSigTmFish*cSigTmFish) * ((uTm - cTmOptFish)*(uTm - cTmO&
		&ptFish) - (cTmRef - cTmOptFish)*(cTmRef - cTmOptFish))) 		else		uFunTmFish = 0.0 		endif
! 	bioturbation_by_fish
		if (InclWeb == 1) then
		tDTurbFish = (kTurbFish * uFunTmFish * sDFiAd) 		else		tDTurbFish = 0.0 		endif
! 	IM_bioturbation_by_fish
		tDTurbFishIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDTurbFish
! 	vegetation_dependence_of_resuspension
		aFunVegResus = max(1.0 - kVegResus * sDVeg, 0.0)
! 	Empirical_suspended_matter_function_(logistic_fit_to_data)
		if (uTm >= 0.1) then
		aFunDimSusp = cSuspRef * ((cSuspMin + cSuspMax / (1.0 + exp(cSuspSlope * (sDepth&
		&W - hDepthSusp)))) * ((((cFetch +NearZero)/ cFetchRef) )** (0.5))) 		else		aFunDimSusp = 0.0 		endif
! 	resuspension_due_to_shear_stress
		tDResusTauDead = min(aFunDimSusp, ((aFunDimSusp +NearZero )** (0.5))) * ((fLutum&
		& / fLutumRef )** (0.5)) * bPorS
! 	resuspension_due_to_shear_stress_AND_fish
		tDResusBareDead = tDResusTauDead + tDTurbFish
! 	resuspension_corrected_for_vegetation_effect
		tDResusDead = tDResusBareDead * aFunVegResus
! 	IM_resuspension
		tDResusIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDResusDead
! 	detrital_resuspension
		tDResusDet = sDDetS / (fLutum * sDIMS + sDDetS) * tDResusDead
! 	phytoplankton_resuspension_rate_constant
		akResusPhytRef = kResusPhytMax * (1.0 - exp(cResusPhytExp * tDResusDead))
! 	phytoplankton_resuspension
		tDResusPhytTot = akResusPhytRef * aDPhytS
! 	resuspension_flux_of_detrital_P
		tPResusDet = rPDDetS * tDResusDet
! 	resuspension_flux_of_dissolved_P
		tPResusPO4 = sPO4S / sDDetS * tDResusDet
! 	resuspension_flux_of_P_adsorbed_onto_inert_matter
		tPResusAIM = sPAIMS / sDIMS * tDResusIM
! 	resuspension_flux_of_nitrate
		tNResusNO3 = sNO3S / sDDetS * tDResusDet
! 	resuspension_flux_of_ammonium
		tNResusNH4 = sNH4S / sDDetS * tDResusDet
! 	resuspension_flux_of_detrital_N
		tNResusDet = rNDDetS * tDResusDet
! 	resuspension_flux_of_detrial_SI
		tSiResusDet = rSiDDetS * tDResusDet
! 	correction_factor_for_IM_settling_rate_(<=_1)
		aFunTauSetOM = min(1.0 / ((aFunDimSusp +NearZero )** (0.5)), 1.0)
! 	correction_factor_for_OM_settling_rate_(<=_1)
		aFunTauSetIM = aFunTauSetOM
! 	temperature_correction_of_sedimentation
		uFunTmSet = ((cThetaSet )** (uTm-cTmRef))
! 	corrected_sedimentation_velocity_of_IM
		uCorVSetIM = aFunTauSetIM * (((fLutumRef/fLutum) )** (0.5)) * uFunTmSet * cVSetI&
		&M
! 	sedimentation_flux_of_inert_matter
		tDSetIM = uCorVSetIM * sDIMW
! 	sedimentation_flux_of_P_adsorbed_onto_inert_org_matter
		tPSetAIM = sPAIMW / sDIMW * tDSetIM
! 	corrected_sedimentation_velocity_of_detritus
		uCorVSetDet = cVSetDet * aFunTauSetOM * uFunTmSet
! 	sedimentation_flux_of_detritus
		tDSetDet = uCorVSetDet * sDDetW
! 	sedimentation_flux_of_detrital_P
		tPSetDet = uCorVSetDet * sPDetW
! 	sedimentation_flux_of_detrital_N
		tNSetDet = uCorVSetDet * sNDetW
! 	sedimentation_flux_of_detrital_Si
		tSiSetDet = uCorVSetDet * sSiDetW
! 	P_mineralisation_constant_in_water
		kPMinDetW = kDMinDetW
! 	N_mineralisation_constant_in_water
		kNMinDetW = kDMinDetW
! 	Si_mineralisation_constant_in_water
		kSiMinDetW = kDMinDetW
! 	temp_function_of_mineralization_in_water
		uFunTmMinW = ((cThetaMinW )** (uTm-cTmRef))
! 	decomposition
		wDMinDetW = kDMinDetW * uFunTmMinW * sDDetW
! 	mineralization
		wPMinDetW = kPMinDetW * uFunTmMinW * sPDetW
! 	mineralization
		wNMinDetW = kNMinDetW * uFunTmMinW * sNDetW
! 	mineralization
		wSiMinDetW = kSiMinDetW * uFunTmMinW * sSiDetW
! 	correction_of_O2_demand_in_water_at_low_oxygen_conc
		aCorO2BOD = sO2W / (hO2BOD + sO2W)
! 	O2_flux_due_to_mineralization_of_detritus
		wO2MinDetW = molO2molC * cCPerDW * aCorO2BOD * wDMinDetW
! 	mineralisation_flux_by_denitrification
		wDDenitW = sNO3W*sNO3W / (hNO3Denit*hNO3Denit + sNO3W*sNO3W) * (1.0 - aCorO2BOD)&
		& * wDMinDetW
! 	Denitrification_flux
		wNDenitW = NO3PerC * molNmolC * cCPerDW * wDDenitW
! 	Temperature_dependence_for_nitrification
		uFunTmNitr = ((cThetaNitr )** (uTm-cTmRef))
! 	oxygen_consumption_during_nitrification
		aCorO2NitrW = sO2W*sO2W / (hO2Nitr*hO2Nitr + sO2W*sO2W)
! 	nitrification_flux
		wNNitrW = kNitrW * uFunTmNitr * aCorO2NitrW * sNH4W
! 	O2_flux_due_to_nitrification
		wO2NitrW = O2PerNH4 * molO2molN * wNNitrW
! 	P_mineralisation_constant_in_sed
		kPMinDetS = kDMinDetS
! 	N_mineralisation_constant_in_sed
		kNMinDetS = kDMinDetS
! 	Si_mineralisation_constant_in_sed
		kSiMinDetS = kDMinDetS
! 	temp_function
		uFunTmMinS = ((cThetaMinS )** (uTm-cTmRef))
! 	decomposition_of_upper_sediment
		tDMinDetS = kDMinDetS * uFunTmMinS * sDDetS
! 	mineralization_of_P_in_upper_sediment
		tPMinDetS = kPMinDetS * uFunTmMinS * sPDetS
! 	mineralization_of_N_in_upper_sediment
		tNMinDetS = kNMinDetS * uFunTmMinS * sNDetS
! 	mineralization_of_Si_in_upper_sediment
		tSiMinDetS = kSiMinDetS * uFunTmMinS * sSiDetS
! 	temperature_function_of_diffusion
		uFunTmDif = ((cThetaDif )** (uTm-cTmRef))
! 	corrected_O2_diffusion_coefficient
		akO2DifCor = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorS
! 	sediment_oxygen_demand
		tSOD = (molO2molC * cCPerDW * (1.0 - fRefrDetS) * tDMinDetS + O2PerNH4 * molO2mo&
		&lN * kNitrS * uFunTmNitr * sNH4S) / cDepthS
! 	oxygen_penetration_depth
		aDepthOxySed = (((2.0 * sO2W * akO2DifCor / tSOD) )** (0.5))
! 	fraction_aerobic_sediment
		afOxySed = aDepthOxySed / cDepthS
! 	aerobic_mineralisation
		tDMinOxyDetS = afOxySed * (1.0 - fRefrDetS) * tDMinDetS
! 	sediment_oxygen_demand
		tO2MinDetS = molO2molC * cCPerDW * tDMinOxyDetS
! 	mineralisation_flux_by_denitrification
		tDDenitS = oNO3S*oNO3S / (hNO3Denit*hNO3Denit + oNO3S*oNO3S) * (1.0 - afOxySed) &
		&* (1.0 - fRefrDetS) * tDMinDetS
! 	Denitrification_flux
		tNDenitS = NO3PerC * molNmolC * cCPerDW * tDDenitS
! 	nitrification_flux
		tNNitrS = afOxySed * kNitrS * uFunTmNitr * sNH4S
! 	O2_flux_due_to_nitrification
		tO2NitrS = O2PerNH4 * molO2molN * tNNitrS
! 	decomposition_of_upper_sediment_humus
		tDMinHumS = kDMinHum * uFunTmMinS * afOxySed * sDHumS
! 	mineralization_of_P_in_upper_sediment_humus
		tPMinHumS = kDMinHum * uFunTmMinS * afOxySed * sPHumS
! 	mineralization_of_N_in_upper_sediment_humus
		tNMinHumS = kDMinHum * uFunTmMinS * afOxySed * sNHumS
! 	average_diffusion_distance
		aDepthDif = fDepthDifS * cDepthS
! 	diffusion_flux_of_dissolved_P_from_sediment_to_water
		tPDifPO4 = kPDifPO4 * uFunTmDif * cTurbDifNut * bPorCorS * (oPO4S - sPO4W ) / aD&
		&epthDif
! 	diffusion_flux_of_NO3_from_sediment_to_water
		tNDifNO3 = kNDifNO3 * uFunTmDif * cTurbDifNut * bPorCorS * (oNO3S - sNO3W ) / aD&
		&epthDif
! 	diffusion_flux_of_NH4_from_sediment_to_water
		tNDifNH4 = kNDifNH4 * uFunTmDif * cTurbDifNut * bPorCorS * (oNH4S - sNH4W ) / aD&
		&epthDif
! 	O2_diffusion_(water_->_sediment)
		tO2Dif = kO2Dif / aDepthDif * uFunTmDif * cTurbDifO2 * bPorCorS * sO2W
! 	diffusion_flux_of_dissolved_P_from_pore_water_to_ground_water
		tPDifGroundPO4 = 0.0
! 	diffusion_flux_of_dissolved_NO3_from_pore_water_to_ground_water
		tNDifGroundNO3 = 0.0
! 	diffusion_flux_of_dissolved_NH4_from_pore_water_to_ground_water
		tNDifGroundNH4 = 0.0
! 	max_P_adsorption_per_g_inorg_matter_in_water
		aPAdsMaxW = cRelPAdsD + aCorO2BOD * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsW = (1.0 - fRedMax * (1.0-aCorO2BOD)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		aPIsoAdsW = aPAdsMaxW * aKPAdsW * sPO4W / (1.0 + aKPAdsW * sPO4W)
! 	equilibrium_conc
		aPEqIMW = aPIsoAdsW * sDIMW
! 	sorption_flux_in_water
		wPSorpIMW = kPSorp * (aPEqIMW - sPAIMW)
! 	max_P_adsorption_per_g_inorg_matter_in_sediment
		aPAdsMaxS = cRelPAdsD + afOxySed * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsS = (1.0 - fRedMax * (1.0-afOxySed)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		aPIsoAdsS = aPAdsMaxS * aKPAdsS * oPO4S / (1.0 + aKPAdsS * oPO4S)
! 	equilibrium_amount
		aPEqIMS = aPIsoAdsS * sDIMS
! 	sorption
		tPSorpIMS = kPSorp * (aPEqIMS - sPAIMS)
! 	chem_loss_of_dissolved_P_from_pore_water
		tPChemPO4 = max( 0.0, kPChemPO4 * (oPO4S - coPO4Max) )
! 	total_abiotic/microbial_DW_inorganic_matter_flux_in_water
		wDAbioIMW = (uDErosIMW - tDSetIM + tDResusIM) / sDepthW
! 	total_abiotic/microbial_DW_detritus_flux_in_water
		wDAbioDetW = (- tDSetDet + tDResusDet ) / sDepthW - wDMinDetW
! 	total_abiotic/microbial_DW_inorganic_matter_flux_in_sediment
		tDAbioIMS = uDErosIMS + tDSetIM - tDResusIM
! 	total_abiotic/microbial_DW_detritus_flux_in_sediment
		tDAbioDetS = tDSetDet - tDResusDet - tDMinDetS
! 	total_abiotic/microbial_DW_humus_flux_in_sediment
		tDAbioHumS = uDErosOM + fRefrDetS * tDMinDetS - tDMinHumS
! 	total_abiotic/microbial_DW_flux_for_mass_balance_check
		tDAbioTotT = cDErosTot - wDMinDetW * sDepthW - (1.0 - fRefrDetS) * tDMinDetS - t&
		&DMinHumS
! 	total_abiotic/microbial_O2_flux_in_water
		wO2AbioW = tO2Aer / sDepthW - wO2MinDetW - wO2NitrW - (tO2MinDetS + tO2NitrS) / &
		&sDepthW
! 	total_abiotic/microbial_P_detritus_flux_in_water
		wPAbioDetW = - wPMinDetW - (tPSetDet - tPResusDet) / sDepthW
! 	total_abiotic/microbial_dissolved_P_flux_in_water
		wPAbioPO4W = wPMinDetW - wPSorpIMW - tPInfPO4W / sDepthW + tPDifPO4 / sDepthW + &
		&tPResusPO4 / sDepthW
! 	total_abiotic/microbial_P_absorbed_onto_inorganic_matter_flux_in_water
		wPAbioAIMW = (- tPSetAIM + tPResusAIM) / sDepthW + wPSorpIMW
! 	total_abiotic/microbial_P_detritus_flux_in_sediment
		tPAbioDetS = - tPMinDetS + tPSetDet - tPResusDet
! 	total_abiotic/microbial_P_humus_flux_in_sediment
		tPAbioHumS = uPErosOM + fRefrDetS * tPMinDetS - tPMinHumS
! 	total_abiotic/microbial_dissolved_P_flux_in_sediment
		tPAbioPO4S = tPInfPO4W - tPInfPO4S + (1.0-fRefrDetS) * tPMinDetS + tPMinHumS - t&
		&PSorpIMS - tPResusPO4 - tPDifPO4 - tPDifGroundPO4 - tPChemPO4
! 	total_abiotic/microbial_P_absorbed_onto_inorganic_matter_flux_in_sediment
		tPAbioAIMS = tPSetAIM - tPResusAIM + tPSorpIMS
! 	total_abiotic/microbial_P_flux_for_mass_balance_check
		tPAbioTotT = uPErosOM - tPChemPO4 - tPInfPO4S - tPDifGroundPO4
! 	total_abiotic/microbial_N_NH4_flux_in_water
		wNAbioNH4W = wNMinDetW - wNNitrW - tNInfNH4W / sDepthW + (tNDifNH4 + tNResusNH4)&
		& / sDepthW
! 	total_abiotic/microbial_N_NO3_flux_in_water
		wNAbioNO3W = wNNitrW - wNDenitW + (tNDifNO3 + tNResusNO3 - tNInfNO3W) / sDepthW
! 	total_abiotic/microbial_N_detritus_flux_in_water
		wNAbioDetW = - wNMinDetW - (tNSetDet - tNResusDet ) / sDepthW
! 	total_abiotic/microbial_N_NH4_flux_in_sediment
		tNAbioNH4S = tNInfNH4W - tNInfNH4S + (1.0-fRefrDetS) * tNMinDetS + tNMinHumS - t&
		&NResusNH4 - tNDifNH4 - tNDifGroundNH4 - tNNitrS
! 	total_abiotic/microbial_N_NO3_flux_in_sediment
		tNAbioNO3S = tNInfNO3W - tNInfNO3S + tNNitrS - tNDenitS - tNResusNO3 - tNDifNO3 &
		&- tNDifGroundNO3
! 	total_abiotic/microbial_N_detritus_flux_in_sediment
		tNAbioDetS = - tNMinDetS + tNSetDet - tNResusDet
! 	total_abiotic/microbial_N_humus_flux_in_sediment
		tNAbioHumS = uNErosOM + fRefrDetS * tNMinDetS - tNMinHumS
! 	total_abiotic/microbial_N_flux_for_mass_balance_check
		tNAbioTotT = uNErosOM - tNDenitS - wNDenitW * sDepthW - tNInfNH4S - tNInfNO3S - &
		&tNDifGroundNO3 - tNDifGroundNH4
! 	total_abiotic/microbial_Si_SiO2_flux_in_water
		wSiAbioSiO2W = wSiMinDetW + (1.0 - fRefrDetS) * tSiMinDetS / sDepthW
! 	total_abiotic/microbial_Si_detritus_flux_in_water
		wSiAbioDetW = - wSiMinDetW - (tSiSetDet - tSiResusDet) / sDepthW
! 	total_abiotic/microbial_Si_detritus_flux_in_sediment
		tSiAbioDetS = - tSiMinDetS + tSiSetDet - tSiResusDet
! 	total_abiotic/microbial_Si_flux_for_mass_balance_check
		tSiAbioTotT = - fRefrDetS * tSiMinDetS
! 	reed_evaporation(set_EQUAL_to_lake_evaporation)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		uQEvPhra = uQEv 		else		uQEvPhra = 0.0 		endif
! 	SRP_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPEvPO4WM = uQEvPhra / mmPerm * sPO4WM 		else		tPEvPO4WM = 0.0 		endif
! 	ammonium_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNEvNH4WM = uQEvPhra / mmPerm * sNH4WM 		else		tNEvNH4WM = 0.0 		endif
! 	nitrate_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNEvNO3WM = uQEvPhra / mmPerm * sNO3WM 		else		tNEvNO3WM = 0.0 		endif
! 	infiltr_of_SRP
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPInfPO4WM = 0.0 		else if (cQInf >= 0.0) then		tPInfPO4WM = cQInf / mmPerm * sPO4WM 		else		tPInfPO4WM = cQInf / mmPerm * oPO4SM 		endif
! 	infiltr_of_ammonium
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNInfNH4WM = 0.0 		else if (cQInf >= 0.0) then		tNInfNH4WM = cQInf / mmPerm * sNH4WM 		else		tNInfNH4WM = cQInf / mmPerm * oNH4SM 		endif
! 	infiltr_of_nitrate
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNInfNO3WM = 0.0 		else if (cQInf >= 0.0) then		tNInfNO3WM = cQInf / mmPerm * sNO3WM 		else		tNInfNO3WM = cQInf / mmPerm * oNO3SM 		endif
! 	infiltration_of_interst_PO4
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPInfPO4SM = 0.0 		else if (cQInf >= 0.0) then		tPInfPO4SM = cQInf / mmPerm * oPO4SM 		else		tPInfPO4SM = cQInf / mmPerm * cPO4Ground 		endif
! 	infiltration_of_interst_NH4
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNInfNH4SM = 0.0 		else if (cQInf >= 0.0) then		tNInfNH4SM = cQInf / mmPerm * oNH4SM 		else		tNInfNH4SM = cQInf / mmPerm * oNH4SM 		endif
! 	infiltration_of_interst_NO3
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNInfNO3SM = 0.0 		else if (cQInf >= 0.0) then		tNInfNO3SM = cQInf / mmPerm * oNO3SM 		else		tNInfNO3SM = cQInf / mmPerm * oNO3SM 		endif
! 	reaeration_flux_of_O2_into_the_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2AerM = kAer * uFunTmAer *(uO2Sat - sO2WM) 		else		tO2AerM = 0.0 		endif
! 	sedimentation_flux_of_inert_matter
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetIMM = cVSetIM * uFunTmSet * sDIMWM 		else		tDSetIMM = 0.0 		endif
! 	sedimentation_flux_of_P_adsorbed_onto_inert_org_matter
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetAIMM = cVSetIM * uFunTmSet * sPAIMWM 		else		tPSetAIMM = 0.0 		endif
! 	sedimentation_flux_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetDetM = cVSetDet * uFunTmSet * sDDetWM 		else		tDSetDetM = 0.0 		endif
! 	sedimentation_flux_of_detrital_P
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetDetM = cVSetDet * uFunTmSet* sPDetWM 		else		tPSetDetM = 0.0 		endif
! 	sedimentation_flux_of_detrital_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNSetDetM = cVSetDet * uFunTmSet* sNDetWM 		else		tNSetDetM = 0.0 		endif
! 	sedimentation_flux_of_detrital_Si
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tSiSetDetM = cVSetDet * uFunTmSet * sSiDetWM 		else		tSiSetDetM = 0.0 		endif
! 	sedimentation_flux_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetDiatM = cVSetDiat * uFunTmSet * sDDiatWM 		else		tDSetDiatM = 0.0 		endif
! 	sedimentation_flux_of_detrital_P
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetDiatM = cVSetDiat * uFunTmSet* sPDiatWM 		else		tPSetDiatM = 0.0 		endif
! 	sedimentation_flux_of_detrital_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNSetDiatM = cVSetDiat * uFunTmSet* sNDiatWM 		else		tNSetDiatM = 0.0 		endif
! 	sedimentation_flux_of_detrital_Si
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tSiSetDiatM = cVSetDiat * uFunTmSet * oSiDiatWM 		else		tSiSetDiatM = 0.0 		endif
! 	sedimentation_flux_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetGrenM = cVSetGren * uFunTmSet * sDGrenWM 		else		tDSetGrenM = 0.0 		endif
! 	sedimentation_flux_of_detrital_P
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetGrenM = cVSetGren * uFunTmSet* sPGrenWM 		else		tPSetGrenM = 0.0 		endif
! 	sedimentation_flux_of_detrital_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNSetGrenM = cVSetGren * uFunTmSet* sNGrenWM 		else		tNSetGrenM = 0.0 		endif
! 	sedimentation_flux_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetBlueM = cVSetBlue * uFunTmSet * sDBlueWM 		else		tDSetBlueM = 0.0 		endif
! 	sedimentation_flux_of_detrital_P
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetBlueM = cVSetBlue * uFunTmSet* sPBlueWM 		else		tPSetBlueM = 0.0 		endif
! 	sedimentation_flux_of_detrital_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNSetBlueM = cVSetBlue * uFunTmSet* sNBlueWM 		else		tNSetBlueM = 0.0 		endif
! 	sedimentation_flux_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetPhytM = tDSetDiatM + tDSetGrenM + tDSetBlueM 		else		tDSetPhytM = 0.0 		endif
! 	sedimentation_flux_of_detrital_P
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSetPhytM = tPSetDiatM + tPSetGrenM + tPSetBlueM 		else		tPSetPhytM = 0.0 		endif
! 	sedimentation_flux_of_detrital_N
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNSetPhytM = tNSetDiatM + tNSetGrenM + tNSetBlueM 		else		tNSetPhytM = 0.0 		endif
! 	total_sedimentation_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDSetTotM = tDSetIMM + tDSetDetM + tDSetPhytM 		else		tDSetTotM = 0.0 		endif
! 	decomposition
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDMinDetWM = kDMinDetW * uFunTmMinW * sDDetWM 		else		wDMinDetWM = 0.0 		endif
! 	mineralization
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPMinDetWM = kPMinDetW * uFunTmMinW * sPDetWM 		else		wPMinDetWM = 0.0 		endif
! 	mineralization
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNMinDetWM = kNMinDetW * uFunTmMinW * sNDetWM 		else		wNMinDetWM = 0.0 		endif
! 	mineralization
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wSiMinDetWM = kSiMinDetW * uFunTmMinW * sSiDetWM 		else		wSiMinDetWM = 0.0 		endif
! 	correction_of_O2_demand_in_water_at_low_oxygen_conc
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aCorO2BODM = sO2WM /(hO2BOD + sO2WM) 		else		aCorO2BODM = 0.0 		endif
! 	O2_flux_due_to_mineralization_of_detritus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wO2MinDetWM = molO2molC * cCPerDW * aCorO2BODM * wDMinDetWM 		else		wO2MinDetWM = 0.0 		endif
! 	mineralisation_flux_by_denitrification
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wDDenitWM = sNO3WM*sNO3WM /(hNO3Denit*hNO3Denit + sNO3WM*sNO3WM) *(1.0 - aCorO2B&
		&ODM) * wDMinDetWM 		else		wDDenitWM = 0.0 		endif
! 	Denitrification_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNDenitWM = NO3PerC * molNmolC * cCPerDW * wDDenitWM 		else		wNDenitWM = 0.0 		endif
! 	oxygen_use_for_nitrification_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aCorO2NitrWM = sO2WM * sO2WM /(hO2Nitr * hO2Nitr + sO2WM * sO2WM) 		else		aCorO2NitrWM = 0.0 		endif
! 	nitrification_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wNNitrWM = kNitrW * uFunTmNitr * aCorO2NitrWM * sNH4WM 		else		wNNitrWM = 0.0 		endif
! 	O2_flux_due_to_nitrification
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wO2NitrWM = O2PerNH4 * molO2molN * wNNitrWM 		else		wO2NitrWM = 0.0 		endif
! 	decomposition_of_upper_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMinDetSM = kDMinDetS * uFunTmMinS * sDDetSM 		else		tDMinDetSM = 0.0 		endif
! 	mineralization_of_P_in_upper_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPMinDetSM = kPMinDetS * uFunTmMinS * sPDetSM 		else		tPMinDetSM = 0.0 		endif
! 	mineralization_of_N_in_upper_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNMinDetSM = kNMinDetS * uFunTmMinS * sNDetSM 		else		tNMinDetSM = 0.0 		endif
! 	mineralization_of_Si_in_upper_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tSiMinDetSM = kSiMinDetS * uFunTmMinS * sSiDetSM 		else		tSiMinDetSM = 0.0 		endif
! 	corrected_O2_diffusion_coefficient
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		akO2DifCorM = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorSM 		else		akO2DifCorM = 0.0 		endif
! 	sediment_oxygen_demand
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tSODM =(molO2molC * cCPerDW *(1.0 - fRefrDetS) * tDMinDetSM + O2PerNH4 * molO2mo&
		&lN * kNitrS * uFunTmNitr * sNH4SM) / cDepthSM 		else		tSODM = 0.0 		endif
! 	oxygen_penetration_depth
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aDepthOxySedM = (((2.0 * sO2WM * akO2DifCorM / tSODM) )** (0.5)) 		else		aDepthOxySedM = 0.0 		endif
! 	fraction_aerobic_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afOxySedM = aDepthOxySedM / cDepthSM 		else		afOxySedM = 0.0 		endif
! 	aerobic_mineralisation
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMinOxyDetSM = afOxySedM *(1.0 - fRefrDetS) * tDMinDetSM 		else		tDMinOxyDetSM = 0.0 		endif
! 	sediment_oxygen_demand
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2MinDetSM = molO2molC * cCPerDW * tDMinOxyDetSM 		else		tO2MinDetSM = 0.0 		endif
! 	mineralisation_flux_by_denitrification
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDDenitSM = oNO3SM*oNO3SM /(hNO3Denit*hNO3Denit + oNO3SM*oNO3SM) *(1.0 - afOxySe&
		&dM) *(1.0 - fRefrDetS) * tDMinDetSM 		else		tDDenitSM = 0.0 		endif
! 	Denitrification_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNDenitSM = NO3PerC * molNmolC * cCPerDW * tDDenitSM 		else		tNDenitSM = 0.0 		endif
! 	nitrification_flux
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNNitrSM = afOxySedM * kNitrS * uFunTmNitr * sNH4SM 		else		tNNitrSM = 0.0 		endif
! 	O2_flux_due_to_nitrification
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2NitrSM = O2PerNH4 * molO2molN * tNNitrSM 		else		tO2NitrSM = 0.0 		endif
! 	decomposition_of_upper_sediment_humus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sDHumSM 		else		tDMinHumSM = 0.0 		endif
! 	mineralization_of_P_in_upper_sediment_humus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sPHumSM 		else		tPMinHumSM = 0.0 		endif
! 	mineralization_of_N_in_upper_sediment_humus
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNMinHumSM = kDMinHum * uFunTmMinS * afOxySedM * sNHumSM 		else		tNMinHumSM = 0.0 		endif
! 	average_diffusion_distance
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aDepthDifM = fDepthDifS * cDepthSM 		else		aDepthDifM = 0.0 		endif
! 	diffusion_flux_of_dissolved_P_from_sediment_to_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPDifPO4M = kPDifPO4 * uFunTmDif * cTurbDifNut * bPorCorSM * (oPO4SM - sPO4WM) /&
		& aDepthDifM 		else		tPDifPO4M = 0.0 		endif
! 	diffusion_flux_of_NO3_from_sediment_to_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNDifNO3M = kNDifNO3 * uFunTmDif * cTurbDifNut * bPorCorSM * (oNO3SM - sNO3WM) /&
		& aDepthDifM 		else		tNDifNO3M = 0.0 		endif
! 	diffusion_flux_of_NH4_from_sediment_to_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNDifNH4M = kNDifNH4 * uFunTmDif * cTurbDifNut * bPorCorSM * (oNH4SM - sNH4WM) /&
		& aDepthDifM 		else		tNDifNH4M = 0.0 		endif
! 	O2_diffusion(water_->_sediment)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2DifM = kO2Dif / aDepthDifM * uFunTmDif * cTurbDifO2 * bPorCorSM * sO2WM 		else		tO2DifM = 0.0 		endif
! 	diffusion_flux_of_dissolved_P_from_pore_water_to_ground_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPDifGroundPO4M = 0.0 		else		tPDifGroundPO4M = 0.0 		endif
! 	diffusion_flux_of_NO3_from_pore_water_to_ground_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNDifGroundNO3M = 0.0 		else		tNDifGroundNO3M = 0.0 		endif
! 	diffusion_flux_of_NH4_from_pore_water_to_ground_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNDifGroundNH4M = 0.0 		else		tNDifGroundNH4M = 0.0 		endif
! 	max_P_adsorption_per_g_inorg_matter_in_water_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPAdsMaxWM = cRelPAdsD + aCorO2BODM * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM &
		&		else		aPAdsMaxWM = 0.0 		endif
! 	P_adsorption_affinitycorrected_for_redox_conditions
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aKPAdsWM =(1.0 - fRedMax *(1.0-aCorO2BODM)) * cKPAdsOx 		else		aKPAdsWM = 0.0 		endif
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPIsoAdsWM = aPAdsMaxWM * aKPAdsWM * sPO4WM /(1.0 + aKPAdsWM * sPO4WM) 		else		aPIsoAdsWM = 0.0 		endif
! 	equilibrium_conc
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPEqIMWM = aPIsoAdsWM * sDIMWM 		else		aPEqIMWM = 0.0 		endif
! 	sorption_flux_in_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		wPSorpIMWM = kPSorp *(aPEqIMWM - sPAIMWM) 		else		wPSorpIMWM = 0.0 		endif
! 	max_P_adsorption_per_g_inorg_matter_in_sediment_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPAdsMaxSM = cRelPAdsD + afOxySedM * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM 		else		aPAdsMaxSM = 0.0 		endif
! 	P_adsorption_affinitycorrected_for_redox_conditions
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aKPAdsSM =(1.0 - fRedMax *(1.0-afOxySedM)) * cKPAdsOx 		else		aKPAdsSM = 0.0 		endif
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPIsoAdsSM = aPAdsMaxSM * aKPAdsSM * oPO4SM /(1.0 + aKPAdsSM * oPO4SM) 		else		aPIsoAdsSM = 0.0 		endif
! 	equilibrium_amount
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPEqIMSM = aPIsoAdsSM * sDIMSM 		else		aPEqIMSM = 0.0 		endif
! 	sorption
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPSorpIMSM = kPSorp *(aPEqIMSM - sPAIMSM) 		else		tPSorpIMSM = 0.0 		endif
! 	chem_loss_of_dissolved_P_from_pore_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPChemPO4M = max(0.0,kPChemPO4 *(oPO4SM - coPO4Max)) 		else		tPChemPO4M = 0.0 		endif
! 	Initial_growth_only_once_a_year
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitVeg = 367 		else if (uTm >= cTmInitVeg .and. aDayInitVeg > 366) then		aDayInitVeg = Day 		else		aDayInitVeg = aDayInitVeg 		endif
! 	setting_root_fration
		if (0 == InclSeason) then
		bfRootVeg = fRootVegSum 		else if (Day < aDayInitVeg) then		bfRootVeg = fRootVegWin 		else if (Day < aDayInitVeg + cLengAllo) then		bfRootVeg = 0.5*(fRootVegWin + fRootVegSum) + 0.5*(fRootVegWin - fRootVegSum) * &
		&cos(Pi/cLengAllo * (Day - aDayInitVeg)) 		else if (Day < cDayWinVeg) then		bfRootVeg = fRootVegSum 		else if (Day < cDayWinVeg + cLengAllo) then		bfRootVeg = 0.5*(fRootVegWin + fRootVegSum) - 0.5*(fRootVegWin - fRootVegSum) * &
		&cos(Pi/cLengAllo * (Day - cDayWinVeg)) 		else		bfRootVeg = fRootVegWin 		endif
! 	shoot_fraction
		bfShootVeg = 1.0 - bfRootVeg
! 	root_biomass
		aDRootVeg = bfRootVeg * sDVeg
! 	shoot_biomass
		aDShootVeg = bfShootVeg * sDVeg
! 	emergent_biomass
		aDEmergVeg = fEmergVeg * aDShootVeg
! 	floating_biomass
		aDFloatVeg = fFloatVeg * aDShootVeg
! 	submerged_fraction_of_shoot
		bfSubVeg = 1.0 - fFloatVeg - fEmergVeg
! 	submerged_biomass
		aDSubVeg = bfSubVeg * aDShootVeg
! 	contribution_of_plant_species_to_extinction_(submerged)
		if (0 == InclBed) then
		aExtVeg = 0.0 		else		aExtVeg = cExtSpVeg * aDSubVeg / sDepthW 		endif
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Veg = fDepth1Veg * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Veg = fDepth2Veg * sDepthW
! 	fraction_of_water_SURFACE_covered_by_plant_species
		if (0 == InclBed) then
		afCovSurfVeg = 0.0 		else		afCovSurfVeg = min(1.0, max(aDFloatVeg / (cDLayerVeg + NearZero), aDEmergVeg / (&
		&fEmergVeg * cDCarrVeg + NearZero) ) ) 		endif
! 	fraction_emergent_coverage
		afCovEmergVeg = min(1.0, PerCent * cCovSpVeg * aDEmergVeg)
! 	percent_cover
		aCovVeg = min(100.0, cCovSpVeg * aDShootVeg)
! 	total_plant_biomass
		aDVeg = sDVeg
! 	total_P_in_vegetation
		aPVeg = sPVeg
! 	total_N_in_vegetation
		aNVeg = sNVeg
! 	extinction_coefficient_incl_vegetation
		aExtCoef = aExtCoefOpen + aExtVeg
! 	light_at_the_bottom
		aLPARBot = uLPARSurf * exp(- aExtCoef * sDepthW)
! 	P/DW_ratio_of_vegetation
		rPDVeg = sPVeg / (sDVeg+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDVeg = sNVeg / (sDVeg+NearZero)
! 	migration_flux
		tDMigrVeg = kMigrVeg * (cDVegIn - sDVeg)
! 	net_migration_flux
		tPMigrVeg = kMigrVeg * (cPDVeg0* cDVegIn - sPVeg)
! 	net_migration_flux
		tNMigrVeg = kMigrVeg * (cNDVeg0* cDVegIn - sNVeg)
! 	temperature_function_of_vegetation_production
		uFunTmProdVeg = ((cQ10ProdVeg )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespVeg = ((cQ10RespVeg )** (0.1 * (uTm - cTmRef)))
! 	fraction_of_P_uptake_from_sediment
		if (0 == UseEmpUpt) then
		afPUptVegS = 0.0 		else if (bfRootVeg <= NearZero) then		afPUptVegS = 0.0 		else if (fFloatVeg + bfSubVeg <= NearZero) then		afPUptVegS = 1.0 		else		afPUptVegS = fSedUptVegMax / (1.0 + fSedUptVegCoef * ((((oPO4S+NearZero) / (sPO4&
		&W+NearZero)) )** fSedUptVegExp)) 		endif
! 	fraction_of_N_uptake_from_sediment
		if (0 == UseEmpUpt) then
		afNUptVegS = 0.0 		else if (bfRootVeg <= NearZero) then		afNUptVegS = 0.0 		else if (fFloatVeg + bfSubVeg <= NearZero) then		afNUptVegS = 1.0 		else		afNUptVegS = fSedUptVegMax / (1.0 + fSedUptVegCoef * ((((oNDissS+NearZero) / (oN&
		&DissW+NearZero)) )** fSedUptVegExp)) 		endif
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/D_ratio
		if (0 == UseEmpUpt) then
		aVPUptMaxCrVeg = max( 0.0, cVPUptMaxVeg * uFunTmProdVeg * (cPDVegMax-rPDVeg) / (&
		&cPDVegMax-cPDVegMin) ) 		else		aVPUptMaxCrVeg = max( 0.0, cVPUptMaxVeg * uFunTmProdVeg * (cPDVegMax-rPDVeg) / (&
		&cPDVegMax-cPDVegMin) ) 		endif
! 	P_uptake_RATE_by_subm_AND_floating_parts
		if (0 == UseEmpUpt) then
		aVPUptVegW = sPO4W * aVPUptMaxCrVeg / (aVPUptMaxCrVeg / cAffPUptVeg + sPO4W) 		else		aVPUptVegW = 0.0 		endif
! 	P_uptake_rate_by_roots
		if (0 == UseEmpUpt) then
		aVPUptVegS = oPO4S * aVPUptMaxCrVeg / (aVPUptMaxCrVeg / cAffPUptVeg + oPO4S) 		else		aVPUptVegS = 0.0 		endif
! 	P_uptake_from_water
		if (0 == UseEmpUpt) then
		tPUptVegW = aVPUptVegW * (aDSubVeg + aDFloatVeg) 		else		tPUptVegW = (1.0 - afPUptVegS) * aVPUptMaxCrVeg * sPO4W / (aVPUptMaxCrVeg / cAff&
		&PUptVeg + sPO4W) * sDVeg 		endif
! 	P_uptake_from_pore_water_(by_root_fraction)
		if (0 == UseEmpUpt) then
		tPUptVegS = aVPUptVegS * aDRootVeg 		else		tPUptVegS = afPUptVegS * aVPUptMaxCrVeg * oPO4S / (aVPUptMaxCrVeg / cAffPUptVeg &
		&+ oPO4S) * sDVeg 		endif
! 	total_P_uptake_vegetation
		if (0 == UseEmpUpt) then
		tPUptVeg = tPUptVegW + tPUptVegS 		else		tPUptVeg = tPUptVegW + tPUptVegS 		endif
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/D_ratio
		if (0 == UseEmpUpt) then
		aVNUptMaxCrVeg = max( 0.0, cVNUptMaxVeg * uFunTmProdVeg * (cNDVegMax - rNDVeg) /&
		& (cNDVegMax - cNDVegMin)) 		else		aVNUptMaxCrVeg = max( 0.0, cVNUptMaxVeg * uFunTmProdVeg * (cNDVegMax - rNDVeg) /&
		& (cNDVegMax - cNDVegMin)) 		endif
! 	half-sat_constant_for_N_uptake
		if (0 == UseEmpUpt) then
		ahNUptVeg = aVNUptMaxCrVeg / cAffNUptVeg 		else		ahNUptVeg = aVNUptMaxCrVeg / cAffNUptVeg 		endif
! 	N_uptake_RATE_by_subm_AND_floating_parts
		if (0 == UseEmpUpt) then
		aVNUptVegW = oNDissW * aVNUptMaxCrVeg / (ahNUptVeg + oNDissW) 		else		aVNUptVegW = 0.0 		endif
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		if (0 == UseEmpUpt) then
		afNH4UptVegW = sNH4W * sNO3W / ((ahNUptVeg + sNH4W) * (ahNUptVeg + sNO3W + NearZ&
		&ero)) + sNH4W * ahNUptVeg / ((sNH4W + sNO3W + NearZero) * (ahNUptVeg + sNO3W + N&
		&earZero)) 		else		afNH4UptVegW = sNH4W * sNO3W / ((ahNUptVeg + sNH4W) * (ahNUptVeg + sNO3W + NearZ&
		&ero)) + sNH4W * ahNUptVeg / ((sNH4W + sNO3W + NearZero) * (ahNUptVeg + sNO3W + N&
		&earZero)) 		endif
! 	N_uptake_from_water_(by_shoots)
		if (0 == UseEmpUpt) then
		tNUptVegW = aVNUptVegW * (aDSubVeg + aDFloatVeg) 		else		tNUptVegW = (1.0 - afNUptVegS) * aVNUptMaxCrVeg * oNDissW / (aVNUptMaxCrVeg / cA&
		&ffNUptVeg + oNDissW) * sDVeg 		endif
! 	NH4_uptake_of_vegetation_from_water
		if (0 == UseEmpUpt) then
		tNUptNH4VegW = afNH4UptVegW * tNUptVegW 		else		tNUptNH4VegW = afNH4UptVegW * tNUptVegW 		endif
! 	NO3_uptake_of_vegetation_from_water
		if (0 == UseEmpUpt) then
		tNUptNO3VegW = tNUptVegW - tNUptNH4VegW 		else		tNUptNO3VegW = tNUptVegW - tNUptNH4VegW 		endif
! 	N_uptake_RATE_of_roots
		if (0 == UseEmpUpt) then
		aVNUptVegS = oNDissS * aVNUptMaxCrVeg / (ahNUptVeg + oNDissS) 		else		aVNUptVegS = 0.0 		endif
! 	N_uptake_from_pore_water_(by_roots)
		if (0 == UseEmpUpt) then
		tNUptVegS = aVNUptVegS * aDRootVeg 		else		tNUptVegS = afNUptVegS * aVNUptMaxCrVeg * oNDissS / (aVNUptMaxCrVeg / cAffNUptVe&
		&g + oNDissS) * sDVeg 		endif
! 	fraction_ammonium_uptake_from_pore_water_(from_WASP_model_EPA)
		if (0 == UseEmpUpt) then
		afNH4UptVegS = oNH4S * oNO3S / ((ahNUptVeg + oNH4S +NearZero) * (ahNUptVeg + oNO&
		&3S +NearZero)) + oNH4S * ahNUptVeg / ((oNH4S + oNO3S+NearZero) * (ahNUptVeg + oN&
		&O3S+NearZero)) 		else		afNH4UptVegS = oNH4S * oNO3S / ((ahNUptVeg + oNH4S +NearZero) * (ahNUptVeg + oNO&
		&3S +NearZero)) + oNH4S * ahNUptVeg / ((oNH4S + oNO3S+NearZero) * (ahNUptVeg + oN&
		&O3S+NearZero)) 		endif
! 	NH4_uptake_of_vegetation_from_sediment
		if (0 == UseEmpUpt) then
		tNUptNH4VegS = afNH4UptVegS * tNUptVegS 		else		tNUptNH4VegS = afNH4UptVegS * tNUptVegS 		endif
! 	NO3_uptake_of_vegetation_from_sediment
		if (0 == UseEmpUpt) then
		tNUptNO3VegS = tNUptVegS - tNUptNH4VegS 		else		tNUptNO3VegS = tNUptVegS - tNUptNH4VegS 		endif
! 	total_N_uptake_vegetation
		if (0 == UseEmpUpt) then
		tNUptVeg = tNUptVegW + tNUptVegS 		else		tNUptVeg = tNUptVegW + tNUptVegS 		endif
! 	light_at_top_of_vegetation_layer
		aLPAR1Veg = uLPARSurf * exp(- aExtCoefOpen * aDepth1Veg)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Veg = aLPAR1Veg * exp(- aExtCoef * (aDepth2Veg - aDepth1Veg))
! 	half-sat_light_for_vegetation_production_at_current_temp
		uhLVeg = hLRefVeg * uFunTmProdVeg
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootVeg = fEmergVeg + fFloatVeg * (1.0 - afCovEmergVeg) + bfSubVeg * (1.0 &
		&- afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Veg / uhLVeg) /&
		& (1.0 + aLPAR2Veg / uhLVeg))
! 	max_growth_rate_at_current_temp_AND_light
		aMuTmLVeg = ufDay * bfShootVeg * aLLimShootVeg * uFunTmProdVeg * cMuMaxVeg
! 	Droop_function_(P)_for_vegetation
		aPLimVeg = max(0.0, (1.0 - cPDVegMin / rPDVeg) * cPDVegMax / (cPDVegMax - cPDVeg&
		&Min) )
! 	Droop_function_(N)_for_vegetation
		aNLimVeg = max(0.0, (1.0 - cNDVegMin / rNDVeg) * cNDVegMax / (cNDVegMax - cNDVeg&
		&Min) )
! 	nutrient_limitation_function_of_vegetation
		aNutLimVeg = min( aPLimVeg, aNLimVeg)
! 	actual_growth_rate_of_vegetation
		aMuVeg = aMuTmLVeg * aNutLimVeg
! 	mortality_constant
		if (0 == InclSeason) then
		bkMortVeg = kMortVegSum 		else if (Day < cDayWinVeg) then		bkMortVeg = kMortVegSum 		else if (Day < cDayWinVeg + cLengMort) then		bkMortVeg = - log(fWinVeg) / cLengMort 		else		bkMortVeg = kMortVegSum 		endif
! 	intrinsic_net_increase_rate_of_vegetation
		akDIncrVeg = aMuTmLVeg - kDRespVeg * uFunTmRespVeg - bkMortVeg
! 	logistic_correction_of_vegetation
		tDEnvVeg = max(0.0, akDIncrVeg / (cDCarrVeg+NearZero) * sDVeg*sDVeg)
! 	logistic_correction_of_production
		tDEnvProdVeg = aMuVeg / cMuMaxVeg * tDEnvVeg
! 	vegetation_production
		tDProdVeg = max(0.0, aMuVeg * sDVeg - tDEnvProdVeg)
! 	submerged_production
		tDProdSubVeg = bfSubVeg * tDProdVeg
! 	dark_respiration_of_vegetation
		tDRespVeg = kDRespVeg * uFunTmRespVeg * sDVeg
! 	logistic_correction_of_mortality
		tDEnvMortVeg = tDEnvVeg - tDEnvProdVeg
! 	total_mortality_flux_DW_vegetation
		tDMortVeg = bkMortVeg * sDVeg + tDEnvMortVeg
! 	mortality_flux_becoming_water_detritus
		tDMortVegW = fDetWMortVeg * (1.0 - bfRootVeg) * tDMortVeg
! 	mortality_flux_becoming_sediment_detritus
		tDMortVegS = tDMortVeg - tDMortVegW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazVegBird = cPrefVegBird * sDVeg / (hDVegBird + sDVeg) * cBirdsPerha / m2Per&
		&ha * cDGrazPerBird 		else		tDGrazVegBird = 0.0 		endif
! 	rate_constant_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManVeg = -log(1.0 - fManVeg) / cLengMan 		else		bkManVeg = 0.0 		endif
! 	Mowing_of_vegetation_DW
		tDManVeg = bkManVeg * sDVeg
! 	Mowing_of_vegetation_P
		tPManVeg = rPDVeg * tDManVeg
! 	Mowing_of_vegetation_N
		tNManVeg = rNDVeg * tDManVeg
! 	derivative_of_vegetation_biomass
		tDBedVeg = tDMigrVeg + tDProdVeg - tDRespVeg - tDMortVeg - tDGrazVegBird - tDMan&
		&Veg
! 	vegetation_O2_production
		tO2ProdVeg = molO2molC * cCPerDW * tDProdVeg
! 	submerged_O2_respiration
		tO2RespVegW = molO2molC * cCPerDW * bfSubVeg * tDRespVeg * aCorO2BOD
! 	root_O2_respiration
		tO2RespVegS = molO2molC * cCPerDW * bfRootVeg * tDRespVeg * afOxySed
! 	O2_transport_to_roots
		tO2ProdVegS = min (tO2RespVegS, tO2ProdVeg)
! 	O2_used_for_vegetation_production
		tO2ProdVegW = min( tO2ProdVeg - tO2ProdVegS, bfSubVeg * tO2ProdVeg)
! 	O2_production_to_water_due_to_NO3_uptake_by_macrophytes
		tO2UptNO3VegW = O2PerNO3 * molO2molN * bfSubVeg * tNUptNO3VegW
! 	O2_production_due_to_NO3_uptake_from_sed_by_macrophytes
		tO2UptNO3VegS = O2PerNO3 * molO2molN * tNUptNO3VegS
! 	P_excretion_by_vegetation
		tPExcrVeg = (rPDVeg *2.0)/ (cPDVegMax + rPDVeg) * rPDVeg * tDRespVeg
! 	P_excretion_by_vegetation_in_sediment
		tPExcrVegS = bfRootVeg * tPExcrVeg
! 	P_excretion_by_vegetation_in_water
		tPExcrVegW = tPExcrVeg - tPExcrVegS
! 	P_mortality_flux_of_vegetation
		tPMortVeg = rPDVeg * tDMortVeg
! 	mortality_flux_of_vegetation_becoming_dissolved_P
		tPMortVegPO4 = fDissMortVeg * tPMortVeg
! 	mortality_flux_of_vegetation_becoming_dissolved_P_in_sediment
		tPMortVegPO4S = bfRootVeg * tPMortVegPO4
! 	mortality_flux_of_vegetation_becoming_dissolved_P_in_water
		tPMortVegPO4W = tPMortVegPO4 - tPMortVegPO4S
! 	mortality_flux_of_vegetation_becoming_detritus_P
		tPMortVegDet = tPMortVeg - tPMortVegPO4
! 	mortality_flux_of_vegetation_becoming_detritus_P_in_water
		tPMortVegDetW = fDetWMortVeg * (1.0 - bfRootVeg) * tPMortVegDet
! 	mortality_flux_of_vegetation_becoming_detritus_P_in_sediment
		tPMortVegDetS = tPMortVegDet - tPMortVegDetW
! 	P_mortality_flux_of_vegetation_by_bird_grazing
		tPGrazVegBird = rPDVeg * tDGrazVegBird
! 	total_vegetation_P_flux_in_bed_module
		tPBedVeg = tPMigrVeg + tPUptVeg - tPExcrVeg - tPMortVeg - tPGrazVegBird - tPManV&
		&eg
! 	N_excretion_by_vegetation
		tNExcrVeg = (2.0 * rNDVeg) / (cNDVegMax + rNDVeg) * rNDVeg * tDRespVeg
! 	N_excretion_by_vegetation_to_sediment
		tNExcrVegS = bfRootVeg * tNExcrVeg
! 	N_excretion_by_vegetation_to_water
		tNExcrVegW = tNExcrVeg - tNExcrVegS
! 	N_mortality_flux_of_vegetation
		tNMortVeg = rNDVeg * tDMortVeg
! 	mortality_flux_of_vegetation_becoming_dissolved_N
		tNMortVegNH4 = fDissMortVeg * tNMortVeg
! 	mortality_flux_of_vegetation_becoming_dissolved_N_in_sediment
		tNMortVegNH4S = bfRootVeg * tNMortVegNH4
! 	mortality_flux_of_vegetation_becoming_dissolved_N_in_water
		tNMortVegNH4W = tNMortVegNH4 - tNMortVegNH4S
! 	mortality_flux_of_vegetation_becoming_detritus_N
		tNMortVegDet = tNMortVeg - tNMortVegNH4
! 	mortality_flux_of_vegetation_becoming_detritus_N_in_water
		tNMortVegDetW = fDetWMortVeg * (1.0 - bfRootVeg) * tNMortVegDet
! 	mortality_flux_of_vegetation_becoming_detritus_N_in_sediment
		tNMortVegDetS = tNMortVegDet - tNMortVegDetW
! 	N_mortality_flux_of_vegetation_by_bird_grazing
		tNGrazVegBird = rNDVeg * tDGrazVegBird
! 	total_vegetation_N_flux_in_bed_module
		tNBedVeg = tNMigrVeg + tNUptVeg - tNExcrVeg - tNMortVeg - tNGrazVegBird - tNManV&
		&eg
! 	DW_assimilation_by_herbivorous_birds
		tDAssVegBird = fDAssBird * tDGrazVegBird
! 	DW_egestion_by_herbivorous_birds
		tDEgesBird = tDGrazVegBird - tDAssVegBird
! 	P_assimilation_by_herbivorous_birds
		tPAssVegBird = fDAssBird * tPGrazVegBird
! 	P_egestion_by_herbivorous_birds
		tPEgesBird = tPGrazVegBird - tPAssVegBird
! 	PO4_egestion_by_herbivorous_birds
		tPEgesBirdPO4 = fDissEgesBird * tPEgesBird
! 	P_detritus_egestion_by_herbivorous_birds
		tPEgesBirdDet = tPEgesBird - tPEgesBirdPO4
! 	N_assimilation_by_herbivorous_birds
		tNAssVegBird = fDAssBird * tNGrazVegBird
! 	N_egestion_by_herbivorous_birds
		tNEgesBird = tNGrazVegBird - tNAssVegBird
! 	NH4_egestion_by_herbivorous_birds
		tNEgesBirdNH4 = fDissEgesBird * tNEgesBird
! 	N_detritus_egestion_by_herbivorous_birds
		tNEgesBirdDet = tNEgesBird - tNEgesBirdNH4
! 	total_DW_flux_from_Vegetation_module_to_water_detritus
		wDBedDetW = (tDMortVegW + tDEgesBird) / sDepthW
! 	total_DW_flux_from_Vegetation_module_to_sediment_detritus
		tDBedDetS = tDMortVegS
! 	total_DW_flux_from_Vegetation_module
		tDBedTotT = tDMigrVeg + tDProdVeg - tDRespVeg - tDManVeg - tDAssVegBird
! 	total_P_flux_from_Vegetation_module_to_PO4_in_water
		wPBedPO4W = (- tPUptVegW + tPExcrVegW + tPMortVegPO4W + tPEgesBirdPO4) /sDepthW
! 	total_P_flux_from_Vegetation_module_to_water_detritus
		wPBedDetW = (tPMortVegDetW + tPEgesBirdDet) / sDepthW
! 	total_P_flux_from_Vegetation_module_to_pore_water_PO4
		tPBedPO4S = - tPUptVegS + tPExcrVegS + tPMortVegPO4S
! 	total_P_flux_from_Vegetation_module_to_sediment_detritus
		tPBedDetS = tPMortVegDetS
! 	total_P_flux_from_Vegetation_module
		tPBedTotT = tPMigrVeg - tPManVeg - tPAssVegBird
! 	total_N_flux_from_Vegetation_module_to_NH4_in_water
		wNBedNH4W = (- tNUptNH4VegW + tNExcrVegW + tNMortVegNH4W + tNEgesBirdNH4) / sDep&
		&thW
! 	total_N_flux_from_Vegetation_module_to_NO3_in_water
		wNBedNO3W = - tNUptNO3VegW / sDepthW
! 	total_N_flux_from_Vegetation_module_to_water_detritus
		wNBedDetW = (tNMortVegDetW + tNEgesBirdDet) / sDepthW
! 	total_N_flux_from_Vegetation_module_to_NH4_in_pore_water
		tNBedNH4S = - tNUptNH4VegS + tNExcrVegS + tNMortVegNH4S
! 	total_N_flux_from_Vegetation_module_to_NO3_in_pore_water
		tNBedNO3S = - tNUptNO3VegS
! 	total_N_flux_from_Vegetation_module_to_sediment_detritus
		tNBedDetS = tNMortVegDetS
! 	total_N_flux_from_Vegetation_module
		tNBedTotT = tNMigrVeg - tNManVeg - tNAssVegBird
! 	total_water_O2_flux_in_vegetation_module
		tO2BedW = tO2ProdVegW - tO2RespVegW + tO2UptNO3VegW
! 	total_sediment_O2_flux_in_vegetation_module
		tO2BedS = tO2ProdVegS - tO2RespVegS + tO2UptNO3VegS
! 	-
		if (InclWeb == 1) then
		UseLoss = 0.0 		else		UseLoss = 1.0 		endif
! 	temp_function_of_grazing
		uFunTmLoss = exp(-0.5/(cSigTmLoss*cSigTmLoss) *((uTm-cTmOptLoss)*(uTm-cTmOptLoss&
		&) -(cTmRef-cTmOptLoss)*(cTmRef-cTmOptLoss)))
! 	P/D_ratio_of_Algae
		rPDBlueW = sPBlueW /(sDBlueW+NearZero)
! 	N/D_ratio_of_Algae
		rNDBlueW = sNBlueW /(sDBlueW+NearZero)
! 	P/D_ratio_of_Algae
		rPDBlueS = sPBlueS /(sDBlueS+NearZero)
! 	N/D_ratio_of_Algae
		rNDBlueS = sNBlueS /(sDBlueS+NearZero)
! 	temperature_function_of_Algae
		uFunTmBlue = exp(-0.5/(cSigTmBlue*cSigTmBlue) *((uTm-cTmOptBlue)*(uTm-cTmOptBlue&
		&) - (cTmRef-cTmOptBlue)*(cTmRef-cTmOptBlue)))
! 	temperature_function_of_Algae
		uFunTmProdBlue = uFunTmBlue
! 	temperature_function_of_Algae
		uFunTmRespBlue = uFunTmBlue
! 	maximum_P_uptake_rate_of_Algaecorrected_for_P/D_ratio
		aVPUptMaxCrBlue = max(0.0,cVPUptMaxBlue * uFunTmProdBlue *(cPDBlueMax - rPDBlueW&
		&) /(cPDBlueMax - cPDBlueMin))
! 	P_uptake_rate_of_Algae
		aVPUptBlue = sPO4W * aVPUptMaxCrBlue /(aVPUptMaxCrBlue / cAffPUptBlue + sPO4W)
! 	P_uptake_Algae
		wPUptBlue = aVPUptBlue * sDBlueW
! 	maximum_N_uptake_rate_of_Algaecorrected_for_N/D_ratio
		aVNUptMaxCrBlue = max(0.0,cVNUptMaxBlue * uFunTmProdBlue * (cNDBlueMax - rNDBlue&
		&W) /(cNDBlueMax - cNDBlueMin))
! 	half-sat_NDissW_for_uptake_by_Algae
		ahNUptBlue = aVNUptMaxCrBlue / cAffNUptBlue
! 	N_uptake_rate_of_Algae
		aVNUptBlue = oNDissW * aVNUptMaxCrBlue /(ahNUptBlue + oNDissW)
! 	N_uptake_Algae
		wNUptBlue = aVNUptBlue * sDBlueW
! 	fraction_ammonium_uptake_by_Algae
		afNH4UptBlue = sNH4W * sNO3W /((ahNUptBlue + sNH4W) *(ahNUptBlue + sNO3W)) + sNH&
		&4W * ahNUptBlue /((sNH4W + sNO3W) *(ahNUptBlue + sNO3W))
! 	ammonium_uptake_by_Algae
		wNUptNH4Blue = afNH4UptBlue * wNUptBlue
! 	nitrate_uptake_by_Algae
		wNUptNO3Blue = wNUptBlue - wNUptNH4Blue
! 	max_growth_rate_of_Algae_at_ambient_temperature
		uMuMaxTmBlue = cMuMaxBlue * uFunTmProdBlue
! 	Droop_function(P)_for_Algae
		aPLimBlue = max(0.0,(1.0 - cPDBlueMin / rPDBlueW) * cPDBlueMax /(cPDBlueMax - cP&
		&DBlueMin))
! 	Droop_function(N)_for_Algae
		aNLimBlue = max(0.0,(1.0 - cNDBlueMin / rNDBlueW) * cNDBlueMax /(cNDBlueMax - cN&
		&DBlueMin))
! 	silica_dependence_of_growth_rate
		aSiLimBlue = sSiO2W /(hSiAssBlue + sSiO2W)
! 	Light_function
		aLLimBlue = UseSteeleBlue *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cL&
		&OptRefBlue * uFunTmProdBlue)) - exp(- uLPARSurf /(cLOptRefBlue * uFunTmProdBlue)&
		&))) +(1.0 - UseSteeleBlue) *(1.0 /(aExtCoef * sDepthW) * log((1.0 + uLPARSurf / &
		&(hLRefBlue * uFunTmProdBlue)) / (1.0 + aLPARBot /(hLRefBlue * uFunTmProdBlue))))
! 	growth_rate_at_current_light_AND_temp
		aMuTmLBlue = ufDay *(1.0 - afCovSurfVeg) * aLLimBlue * uMuMaxTmBlue
! 	nutrient_limitation_function_of_Algae
		aNutLimBlue = min(aPLimBlue,(min(aNLimBlue,aSiLimBlue)))
! 	growth_rate
		aMuBlue = aNutLimBlue * aMuTmLBlue
! 	assimilation_Algae
		wDAssBlue = aMuBlue*sDBlueW
! 	chlorophyll-a/DW_ratio_Algae
		rChDBlue = cChDBlueMax -(cChDBlueMax - cChDBlueMin) * aLLimBlue
! 	chlorophyll-a_conc
		oChlaBlue = mgPerg * rChDBlue * sDBlueW
! 	specific_extinction_per_unit_chlorophyll-a
		aExtChBlue = cExtSpBlue / rChDBlue
! 	temp_corrected_respiration_constant_of_Algae
		ukDRespTmBlue = kDRespBlue * uFunTmRespBlue
! 	respiration_of_Algae_in_water
		wDRespBlueW = ukDRespTmBlue * sDBlueW
! 	daily_grazing_on_Algae
		ukLossTmBlue = UseLoss * kLossBlue * uFunTmLoss
! 	Algae_grazing_loss
		wDLossBlue = ukLossTmBlue * sDBlueW
! 	mortality_in_water
		wDMortBlueW = kMortBlueW * sDBlueW
! 	corrected_sedimentation_velocity_of_Algae
		uCorVSetBlue = cVSetBlue * aFunTauSetOM * uFunTmSet
! 	sedimentation_flux_of_Algae
		tDSetBlue = uCorVSetBlue * sDBlueW
! 	resuspension_DW_blue-greens
		tDResusBlue = sDBlueS /(aDPhytS+NearZero) * tDResusPhytTot
! 	respiration_of_sediment_Algae
		tDRespBlueS = ukDRespTmBlue * sDBlueS
! 	mortality_in_sed
		tDMortBlueS = kMortBlueS * sDBlueS
! 	total_loss_rate_of_algae_in_water(excl_dilution)
		ukDDecBlue = ukDRespTmBlue + ukLossTmBlue + kMortBlueW +(uCorVSetBlue * uFunTmSe&
		&t) / sDepthW
! 	P_excretion_Algae_in_water
		wPExcrBlueW = (rPDBlueW * 2.0 )/(cPDBlueMax + rPDBlueW) * rPDBlueW * wDRespBlueW
! 	Algae_grazing_loss
		wPLossBlue = rPDBlueW * wDLossBlue
! 	mortality_Algae_in_water
		wPMortBlueW = kMortBlueW * sPBlueW
! 	sedimentation
		tPSetBlue = rPDBlueW * tDSetBlue
! 	Resuspension_of_algae
		tPResusBlue = rPDBlueS * tDResusBlue
! 	P_excretion_of_algae_in_sediment
		tPExcrBlueS = (rPDBlueS *2.0)/(cPDBlueMax + rPDBlueS) * rPDBlueS * tDRespBlueS
! 	P_mortality_of_algae_in_sediment
		tPMortBlueS = kMortBlueS * sPBlueS
! 	N_excretion_Algae_in_water
		wNExcrBlueW = (rNDBlueW *2.0)/(cNDBlueMax + rNDBlueW) * rNDBlueW * wDRespBlueW
! 	Algae_grazing_loss
		wNLossBlue = rNDBlueW * wDLossBlue
! 	mortality_Algae_in_water
		wNMortBlueW = kMortBlueW * sNBlueW
! 	sedimentation
		tNSetBlue = rNDBlueW * tDSetBlue
! 	Resuspension_of_algae
		tNResusBlue = rNDBlueS * tDResusBlue
! 	N_excretion_of_algae_in_sediment
		tNExcrBlueS = (2.0 * rNDBlueS) /(cNDBlueMax + rNDBlueS) * rNDBlueS * tDRespBlueS
! 	N_mortality_of_algae_in_sediment
		tNMortBlueS = kMortBlueS * sNBlueS
! 	total_PRIM_flux_to_algae_in_water
		wDPrimBlueW = wDAssBlue - wDRespBlueW - wDLossBlue - wDMortBlueW -(tDSetBlue - t&
		&DResusBlue) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wPPrimBlueW = wPUptBlue - wPExcrBlueW - wPLossBlue - wPMortBlueW -(tPSetBlue - t&
		&PResusBlue) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wNPrimBlueW = wNUptBlue - wNExcrBlueW - wNLossBlue - wNMortBlueW -(tNSetBlue - t&
		&NResusBlue) / sDepthW
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tDPrimBlueS = tDSetBlue - tDResusBlue - tDMortBlueS - tDRespBlueS 		else		tDPrimBlueS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tPPrimBlueS = tPSetBlue - tPResusBlue - tPMortBlueS - tPExcrBlueS 		else		tPPrimBlueS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tNPrimBlueS = tNSetBlue - tNResusBlue - tNMortBlueS - tNExcrBlueS 		else		tNPrimBlueS = 0.0 		endif
! 	P/D_ratio_of_Algae
		rPDGrenW = sPGrenW /(sDGrenW+NearZero)
! 	N/D_ratio_of_Algae
		rNDGrenW = sNGrenW /(sDGrenW+NearZero)
! 	P/D_ratio_of_Algae
		rPDGrenS = sPGrenS /(sDGrenS+NearZero)
! 	N/D_ratio_of_Algae
		rNDGrenS = sNGrenS /(sDGrenS+NearZero)
! 	temperature_function_of_Algae
		uFunTmGren = exp(-0.5/(cSigTmGren*cSigTmGren) *((uTm-cTmOptGren)*(uTm-cTmOptGren&
		&) - (cTmRef-cTmOptGren)*(cTmRef-cTmOptGren)))
! 	temperature_function_of_Algae
		uFunTmProdGren = uFunTmGren
! 	temperature_function_of_Algae
		uFunTmRespGren = uFunTmGren
! 	maximum_P_uptake_rate_of_Algaecorrected_for_P/D_ratio
		aVPUptMaxCrGren = max(0.0,cVPUptMaxGren * uFunTmProdGren *(cPDGrenMax - rPDGrenW&
		&) /(cPDGrenMax - cPDGrenMin))
! 	P_uptake_rate_of_Algae
		aVPUptGren = sPO4W * aVPUptMaxCrGren /(aVPUptMaxCrGren / cAffPUptGren + sPO4W)
! 	P_uptake_Algae
		wPUptGren = aVPUptGren * sDGrenW
! 	maximum_N_uptake_rate_of_Algaecorrected_for_N/D_ratio
		aVNUptMaxCrGren = max(0.0,cVNUptMaxGren * uFunTmProdGren * (cNDGrenMax - rNDGren&
		&W) /(cNDGrenMax - cNDGrenMin))
! 	half-sat_NDissW_for_uptake_by_Algae
		ahNUptGren = aVNUptMaxCrGren / cAffNUptGren
! 	N_uptake_rate_of_Algae
		aVNUptGren = oNDissW * aVNUptMaxCrGren /(ahNUptGren + oNDissW)
! 	N_uptake_Algae
		wNUptGren = aVNUptGren * sDGrenW
! 	fraction_ammonium_uptake_by_Algae
		afNH4UptGren = sNH4W * sNO3W /((ahNUptGren + sNH4W) *(ahNUptGren + sNO3W)) + sNH&
		&4W * ahNUptGren /((sNH4W + sNO3W) *(ahNUptGren + sNO3W))
! 	ammonium_uptake_by_Algae
		wNUptNH4Gren = afNH4UptGren * wNUptGren
! 	nitrate_uptake_by_Algae
		wNUptNO3Gren = wNUptGren - wNUptNH4Gren
! 	max_growth_rate_of_Algae_at_ambient_temperature
		uMuMaxTmGren = cMuMaxGren * uFunTmProdGren
! 	Droop_function(P)_for_Algae
		aPLimGren = max(0.0,(1.0 - cPDGrenMin / rPDGrenW) * cPDGrenMax /(cPDGrenMax - cP&
		&DGrenMin))
! 	Droop_function(N)_for_Algae
		aNLimGren = max(0.0,(1.0 - cNDGrenMin / rNDGrenW) * cNDGrenMax /(cNDGrenMax - cN&
		&DGrenMin))
! 	silica_dependence_of_growth_rate
		aSiLimGren = sSiO2W /(hSiAssGren + sSiO2W)
! 	Light_function
		aLLimGren = UseSteeleGren *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cL&
		&OptRefGren * uFunTmProdGren)) - exp(- uLPARSurf /(cLOptRefGren * uFunTmProdGren)&
		&))) +(1.0 - UseSteeleGren) *(1.0 /(aExtCoef * sDepthW) * log((1.0 + uLPARSurf / &
		&(hLRefGren * uFunTmProdGren)) / (1.0 + aLPARBot /(hLRefGren * uFunTmProdGren))))
! 	growth_rate_at_current_light_AND_temp
		aMuTmLGren = ufDay *(1.0 - afCovSurfVeg) * aLLimGren * uMuMaxTmGren
! 	nutrient_limitation_function_of_Algae
		aNutLimGren = min(aPLimGren,(min(aNLimGren,aSiLimGren)))
! 	growth_rate
		aMuGren = aNutLimGren * aMuTmLGren
! 	assimilation_Algae
		wDAssGren = aMuGren*sDGrenW
! 	chlorophyll-a/DW_ratio_Algae
		rChDGren = cChDGrenMax -(cChDGrenMax - cChDGrenMin) * aLLimGren
! 	chlorophyll-a_conc
		oChlaGren = mgPerg * rChDGren * sDGrenW
! 	specific_extinction_per_unit_chlorophyll-a
		aExtChGren = cExtSpGren / rChDGren
! 	temp_corrected_respiration_constant_of_Algae
		ukDRespTmGren = kDRespGren * uFunTmRespGren
! 	respiration_of_Algae_in_water
		wDRespGrenW = ukDRespTmGren * sDGrenW
! 	daily_grazing_on_Algae
		ukLossTmGren = UseLoss * kLossGren * uFunTmLoss
! 	Algae_grazing_loss
		wDLossGren = ukLossTmGren * sDGrenW
! 	mortality_in_water
		wDMortGrenW = kMortGrenW * sDGrenW
! 	corrected_sedimentation_velocity_of_Algae
		uCorVSetGren = cVSetGren * aFunTauSetOM * uFunTmSet
! 	sedimentation_flux_of_Algae
		tDSetGren = uCorVSetGren * sDGrenW
! 	resuspension_of_Algae
		tDResusGren = sDGrenS /(aDPhytS+NearZero) * tDResusPhytTot
! 	respiration_of_sediment_Algae
		tDRespGrenS = ukDRespTmGren * sDGrenS
! 	mortality_in_sed
		tDMortGrenS = kMortGrenS * sDGrenS
! 	total_loss_rate_of_algae_in_water(excl_dilution)
		ukDDecGren = ukDRespTmGren + ukLossTmGren + kMortGrenW +(uCorVSetGren * uFunTmSe&
		&t) / sDepthW
! 	P_excretion_Algae_in_water
		wPExcrGrenW = (2.0 *rPDGrenW) /(cPDGrenMax + rPDGrenW) * rPDGrenW * wDRespGrenW
! 	Algae_grazing_loss
		wPLossGren = rPDGrenW * wDLossGren
! 	mortality_Algae_in_water
		wPMortGrenW = kMortGrenW * sPGrenW
! 	sedimentation
		tPSetGren = rPDGrenW * tDSetGren
! 	Resuspension_of_algae
		tPResusGren = rPDGrenS * tDResusGren
! 	P_excretion_of_algae_in_sediment
		tPExcrGrenS = (2.0 * rPDGrenS) /(cPDGrenMax + rPDGrenS) * rPDGrenS * tDRespGrenS
! 	P_mortality_of_algae_in_sediment
		tPMortGrenS = kMortGrenS * sPGrenS
! 	N_excretion_Algae_in_water
		wNExcrGrenW = (2.0 * rNDGrenW) /(cNDGrenMax + rNDGrenW) * rNDGrenW * wDRespGrenW
! 	Algae_grazing_loss
		wNLossGren = rNDGrenW * wDLossGren
! 	mortality_Algae_in_water
		wNMortGrenW = kMortGrenW * sNGrenW
! 	sedimentation
		tNSetGren = rNDGrenW * tDSetGren
! 	Resuspension_of_algae
		tNResusGren = rNDGrenS * tDResusGren
! 	N_excretion_of_algae_in_sediment
		tNExcrGrenS = (2.0 * rNDGrenS) /(cNDGrenMax + rNDGrenS) * rNDGrenS * tDRespGrenS
! 	N_mortality_of_algae_in_sediment
		tNMortGrenS = kMortGrenS * sNGrenS
! 	total_PRIM_flux_to_algae_in_water
		wDPrimGrenW = wDAssGren - wDRespGrenW - wDLossGren - wDMortGrenW -(tDSetGren - t&
		&DResusGren) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wPPrimGrenW = wPUptGren - wPExcrGrenW - wPLossGren - wPMortGrenW -(tPSetGren - t&
		&PResusGren) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wNPrimGrenW = wNUptGren - wNExcrGrenW - wNLossGren - wNMortGrenW -(tNSetGren - t&
		&NResusGren) / sDepthW
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tDPrimGrenS = tDSetGren - tDResusGren - tDMortGrenS - tDRespGrenS 		else		tDPrimGrenS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tPPrimGrenS = tPSetGren - tPResusGren - tPMortGrenS - tPExcrGrenS 		else		tPPrimGrenS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tNPrimGrenS = tNSetGren - tNResusGren - tNMortGrenS - tNExcrGrenS 		else		tNPrimGrenS = 0.0 		endif
! 	P/D_ratio_of_Algae
		rPDDiatW = sPDiatW /(sDDiatW+NearZero)
! 	N/D_ratio_of_Algae
		rNDDiatW = sNDiatW /(sDDiatW+NearZero)
! 	P/D_ratio_of_Algae
		rPDDiatS = sPDiatS /(sDDiatS+NearZero)
! 	N/D_ratio_of_Algae
		rNDDiatS = sNDiatS /(sDDiatS+NearZero)
! 	temperature_function_of_Algae
		uFunTmDiat = exp(-0.5/(cSigTmDiat*cSigTmDiat) *((uTm-cTmOptDiat)*(uTm-cTmOptDiat&
		&) - (cTmRef-cTmOptDiat)*(cTmRef-cTmOptDiat)))
! 	temperature_function_production_of_Algae
		uFunTmProdDiat = uFunTmDiat
! 	temperature_function_respiration_of_Algae
		uFunTmRespDiat = uFunTmDiat
! 	maximum_P_uptake_rate_of_Algaecorrected_for_P/D_ratio
		aVPUptMaxCrDiat = max(0.0,cVPUptMaxDiat * uFunTmProdDiat *(cPDDiatMax - rPDDiatW&
		&) /(cPDDiatMax - cPDDiatMin))
! 	P_uptake_rate_of_Algae
		aVPUptDiat = sPO4W * aVPUptMaxCrDiat /(aVPUptMaxCrDiat / cAffPUptDiat + sPO4W)
! 	P_uptake_Algae
		wPUptDiat = aVPUptDiat * sDDiatW
! 	maximum_N_uptake_rate_of_Algaecorrected_for_N/D_ratio
		aVNUptMaxCrDiat = max(0.0,cVNUptMaxDiat * uFunTmProdDiat * (cNDDiatMax - rNDDiat&
		&W) /(cNDDiatMax - cNDDiatMin))
! 	half-sat_NDissW_for_uptake_by_Algae
		ahNUptDiat = aVNUptMaxCrDiat / cAffNUptDiat
! 	N_uptake_rate_of_Algae
		aVNUptDiat = oNDissW * aVNUptMaxCrDiat /(ahNUptDiat + oNDissW)
! 	N_uptake_Algae
		wNUptDiat = aVNUptDiat * sDDiatW
! 	fraction_ammonium_uptake_by_Algae
		afNH4UptDiat = sNH4W * sNO3W /((ahNUptDiat + sNH4W) *(ahNUptDiat + sNO3W)) + sNH&
		&4W * ahNUptDiat /((sNH4W + sNO3W) *(ahNUptDiat + sNO3W))
! 	ammonium_uptake_by_Algae
		wNUptNH4Diat = afNH4UptDiat * wNUptDiat
! 	nitrate_uptake_by_Algae
		wNUptNO3Diat = wNUptDiat - wNUptNH4Diat
! 	max_growth_rate_of_Algae_at_ambient_temperature
		uMuMaxTmDiat = cMuMaxDiat * uFunTmProdDiat
! 	Droop_function(P)_for_Algae
		aPLimDiat = max(0.0,(1.0 - cPDDiatMin / rPDDiatW) * cPDDiatMax /(cPDDiatMax - cP&
		&DDiatMin))
! 	Droop_function(N)_for_Algae
		aNLimDiat = max(0.0,(1.0 - cNDDiatMin / rNDDiatW) * cNDDiatMax /(cNDDiatMax - cN&
		&DDiatMin))
! 	silica_dependence_of_growth_rate
		aSiLimDiat = sSiO2W /(hSiAssDiat + sSiO2W)
! 	Light_function
		aLLimDiat = UseSteeleDiat *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cL&
		&OptRefDiat * uFunTmProdDiat)) - exp(- uLPARSurf /(cLOptRefDiat * uFunTmProdDiat)&
		&))) +(1.0 - UseSteeleDiat) *(1.0 /(aExtCoef * sDepthW) * log((1.0 + uLPARSurf / &
		&(hLRefDiat * uFunTmProdDiat)) / (1.0 + aLPARBot /(hLRefDiat * uFunTmProdDiat))))
! 	growth_rate_at_current_light_AND_temp
		aMuTmLDiat = ufDay *(1.0 - afCovSurfVeg) * aLLimDiat * uMuMaxTmDiat
! 	nutrient_limitation_function_of_Algae
		aNutLimDiat = min(aPLimDiat,(min(aNLimDiat,aSiLimDiat)))
! 	growth_rate
		aMuDiat = aNutLimDiat * aMuTmLDiat
! 	assimilation_Algae
		wDAssDiat = aMuDiat*sDDiatW
! 	chlorophyll-a/DW_ratio_Algae
		rChDDiat = cChDDiatMax -(cChDDiatMax - cChDDiatMin) * aLLimDiat
! 	chlorophyll-a_conc
		oChlaDiat = mgPerg * rChDDiat * sDDiatW
! 	specific_extinction_per_unit_chlorophyll-a
		aExtChDiat = cExtSpDiat / rChDDiat
! 	temp_corrected_respiration_constant_of_Algae
		ukDRespTmDiat = kDRespDiat * uFunTmRespDiat
! 	respiration_of_Algae_in_water
		wDRespDiatW = ukDRespTmDiat * sDDiatW
! 	daily_grazing_on_Algae
		ukLossTmDiat = UseLoss * kLossDiat * uFunTmLoss
! 	Algae_grazing_loss
		wDLossDiat = ukLossTmDiat * sDDiatW
! 	mortality_in_water
		wDMortDiatW = kMortDiatW * sDDiatW
! 	corrected_sedimentation_velocity_of_Algae
		uCorVSetDiat = cVSetDiat * aFunTauSetOM * uFunTmSet
! 	sedimentation_flux_of_Algae
		tDSetDiat = uCorVSetDiat * sDDiatW
! 	resuspension_of_Algae
		tDResusDiat = sDDiatS /(aDPhytS+NearZero) * tDResusPhytTot
! 	respiration_of_sediment_Algae
		tDRespDiatS = ukDRespTmDiat * sDDiatS
! 	mortality_in_sed
		tDMortDiatS = kMortDiatS * sDDiatS
! 	total_loss_rate_of_algae_in_water(excl_dilution)
		ukDDecDiat = ukDRespTmDiat + ukLossTmDiat + kMortDiatW +(uCorVSetDiat * uFunTmSe&
		&t) / sDepthW
! 	P_excretion_Algae_in_water
		wPExcrDiatW = (2.0 * rPDDiatW) /(cPDDiatMax + rPDDiatW) * rPDDiatW * wDRespDiatW
! 	Algae_grazing_loss
		wPLossDiat = rPDDiatW * wDLossDiat
! 	mortality_Algae_in_water
		wPMortDiatW = kMortDiatW * sPDiatW
! 	sedimentation
		tPSetDiat = rPDDiatW * tDSetDiat
! 	Resuspension_of_algae
		tPResusDiat = rPDDiatS * tDResusDiat
! 	P_excretion_of_algae_in_sediment
		tPExcrDiatS = (2.0 * rPDDiatS) /(cPDDiatMax + rPDDiatS) * rPDDiatS * tDRespDiatS
! 	P_mortality_of_algae_in_sediment
		tPMortDiatS = kMortDiatS * sPDiatS
! 	N_excretion_Algae_in_water
		wNExcrDiatW = (2.0 * rNDDiatW) /(cNDDiatMax + rNDDiatW) * rNDDiatW * wDRespDiatW
! 	Algae_grazing_loss
		wNLossDiat = rNDDiatW * wDLossDiat
! 	mortality_Algae_in_water
		wNMortDiatW = kMortDiatW * sNDiatW
! 	sedimentation
		tNSetDiat = rNDDiatW * tDSetDiat
! 	Resuspension_of_algae
		tNResusDiat = rNDDiatS * tDResusDiat
! 	N_excretion_of_algae_in_sediment
		tNExcrDiatS = (2.0 * rNDDiatS) /(cNDDiatMax + rNDDiatS) * rNDDiatS * tDRespDiatS
! 	N_mortality_of_algae_in_sediment
		tNMortDiatS = kMortDiatS * sNDiatS
! 	total_PRIM_flux_to_algae_in_water
		wDPrimDiatW = wDAssDiat - wDRespDiatW - wDLossDiat - wDMortDiatW -(tDSetDiat - t&
		&DResusDiat) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wPPrimDiatW = wPUptDiat - wPExcrDiatW - wPLossDiat - wPMortDiatW -(tPSetDiat - t&
		&PResusDiat) / sDepthW
! 	Total_PRIM_flux_to_Algae
		wNPrimDiatW = wNUptDiat - wNExcrDiatW - wNLossDiat - wNMortDiatW -(tNSetDiat - t&
		&NResusDiat) / sDepthW
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tDPrimDiatS = tDSetDiat - tDResusDiat - tDMortDiatS - tDRespDiatS 		else		tDPrimDiatS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tPPrimDiatS = tPSetDiat - tPResusDiat - tPMortDiatS - tPExcrDiatS 		else		tPPrimDiatS = 0.0 		endif
! 	total_flux_from_PRIM_module_to_sediment_Algae
		if (InclPhytS == 1) then
		tNPrimDiatS = tNSetDiat - tNResusDiat - tNMortDiatS - tNExcrDiatS 		else		tNPrimDiatS = 0.0 		endif
! 	total_chlorophyll-a
		oChla = oChlaDiat + oChlaGren + oChlaBlue
! 	total_algal_growth
		wDAssPhyt = wDAssDiat + wDAssGren + wDAssBlue
! 	total_algal_respiration_in_water
		wDRespPhytW = wDRespDiatW + wDRespGrenW + wDRespBlueW
! 	total_algal_mortality_in_water
		wDMortPhytW = wDMortDiatW + wDMortGrenW + wDMortBlueW
! 	total_phytoplankton_sedimentation
		tDSetPhyt = tDSetDiat + tDSetGren + tDSetBlue
! 	total_phytoplankton_grazing_loss
		wDLossPhyt = wDLossDiat + wDLossGren + wDLossBlue
! 	total_of_PRIM_processes_of_algae_in_water
		wDPrimPhytW = wDPrimDiatW + wDPrimGrenW + wDPrimBlueW
! 	total_P_uptake_phytoplankton
		wPUptPhyt = wPUptDiat + wPUptGren + wPUptBlue
! 	total_P_excretion_phytoplankton_in_water
		wPExcrPhytW = wPExcrDiatW + wPExcrGrenW + wPExcrBlueW
! 	total_P_mortality_phytoplankton_in_water
		wPMortPhytW = wPMortDiatW + wPMortGrenW + wPMortBlueW
! 	total_sedimentation_of_algae
		tPSetPhyt = tPSetDiat + tPSetGren + tPSetBlue
! 	-
		tPResusPhyt = tPResusDiat + tPResusGren + tPResusBlue
! 	total_grazing_loss
		wPLossPhyt = wPLossDiat + wPLossGren + wPLossBlue
! 	total_of_PRIM_processes_of_algae_in_water
		wPPrimPhytW = wPPrimDiatW + wPPrimGrenW + wPPrimBlueW
! 	total_N_uptake_phytoplankton
		wNUptPhyt = wNUptDiat + wNUptGren + wNUptBlue
! 	total_ammonium-N_uptake_phytoplankton
		wNUptNH4Phyt = wNUptNH4Diat + wNUptNH4Gren + wNUptNH4Blue
! 	total_nitrate-N_uptake_phytoplankton
		wNUptNO3Phyt = wNUptNO3Diat + wNUptNO3Gren + wNUptNO3Blue
! 	total_N_excretion_phytoplankton_in_water
		wNExcrPhytW = wNExcrDiatW + wNExcrGrenW + wNExcrBlueW
! 	total_N_mortality_phytoplankton_in_water
		wNMortPhytW = wNMortDiatW + wNMortGrenW + wNMortBlueW
! 	total_sedimentation_of_algae
		tNSetPhyt = tNSetDiat + tNSetGren + tNSetBlue
! 	-
		tNResusPhyt = tNResusDiat + tNResusGren + tNResusBlue
! 	total_grazing_loss
		wNLossPhyt = wNLossDiat + wNLossGren + wNLossBlue
! 	total_of_PRIM_processes_of_algae_in_water
		wNPrimPhytW = wNPrimDiatW + wNPrimGrenW + wNPrimBlueW
! 	respiration_of_algae_on_bottom
		tDRespPhytS = tDRespDiatS + tDRespGrenS + tDRespBlueS
! 	mortality_of_algae_on_bottom
		tDMortPhytS = tDMortDiatS + tDMortGrenS + tDMortBlueS
! 	total_flux_of_algae_on_bottom
		tDPrimPhytS = tDPrimDiatS + tDPrimGrenS + tDPrimBlueS
! 	total_P_excretion_sediment_phytoplankton
		tPExcrPhytS = tPExcrDiatS + tPExcrGrenS + tPExcrBlueS
! 	total_phytoplankton_mortality
		tPMortPhytS = tPMortDiatS + tPMortGrenS + tPMortBlueS
! 	total_flux_of_algae_on_bottom
		tPPrimPhytS = tPPrimDiatS + tPPrimGrenS + tPPrimBlueS
! 	total_N_excretion_sediment_phytoplankton
		tNExcrPhytS = tNExcrDiatS + tNExcrGrenS + tNExcrBlueS
! 	total_phytoplankton_mortality
		tNMortPhytS = tNMortDiatS + tNMortGrenS + tNMortBlueS
! 	total_flux_of_algae_on_bottom
		tNPrimPhytS = tNPrimDiatS + tNPrimGrenS + tNPrimBlueS
! 	Diatoms_silica_uptake
		wSiUptDiat = cSiDDiat * wDAssDiat
! 	Si_excretion
		wSiExcrDiatW = cSiDDiat * wDRespDiatW
! 	diatom_grazing_loss
		wSiLossDiat = cSiDDiat * wDLossDiat
! 	Diatoms_mortality_in_water
		wSiMortDiatW = cSiDDiat * wDMortDiatW
! 	Diatoms_sedimentation
		tSiSetDiat = cSiDDiat * tDSetDiat
! 	Diatoms_sedimentation
		tSiResusDiat = cSiDDiat * tDResusDiat
! 	total_Si_flux_to_sed_diatoms_in_PRIM_module
		wSiPrimDiatW = wSiUptDiat - tSiSetDiat / sDepthW - wSiExcrDiatW - wSiMortDiatW -&
		& wSiLossDiat + tSiResusDiat / sDepthW
! 	C-phycocyanin/DW-ratio_blue-greens
		rCyDBlue = cCyDBlueMax -(cCyDBlueMax - cCyDBlueMin) * aLLimBlue
! 	C-phycocyanin
		oCyan = rCyDBlue * sDBlueW * mgPerg
! 	DW_fraction_of_algal_group_of_total_algae
		fDDiat = sDDiatW /(sDDiatW + sDGrenW + sDBlueW + NearZero)
! 	Flux_to_water_detritus
		wDPrimDetW = wDMortPhytW + wDLossPhyt
! 	Flux_to_sediment_detritus
		if (InclPhytS == 1) then
		tDPrimDetS = tDMortPhytS 		else		tDPrimDetS = tDSetPhyt 		endif
! 	total_DW_flux
		tDPrimTotT = (wDAssPhyt - wDRespPhytW) * sDepthW - tDRespPhytS
! 	O2_production_by_phytoplankton
		wO2ProdPhyt = molO2molC * cCPerDW * wDAssPhyt
! 	O2_production_by_phytoplankton
		wO2RespPhytW = molO2molC * cCPerDW * wDRespPhytW * aCorO2BOD
! 	O2_production_due_to_NO3_uptake_by_phytopl
		wO2UptNO3Phyt = O2PerNO3 * molO2molN * wNUptNO3Phyt
! 	O2_flux_by_water_algae
		wO2PrimW = wO2ProdPhyt - wO2RespPhytW + wO2UptNO3Phyt
! 	O2_respiration_by_sediment_algae
		tO2RespPhytS = molO2molC * cCPerDW * tDRespPhytS * afOxySed
! 	O2_flux_by_sediment_algae
		tO2PrimS = tO2RespPhytS
! 	soluble_P_flux_from_died_Algae
		wPMortPhytPO4W = fDissMortPhyt * wPMortPhytW
! 	detrital_P_flux_from_died_Algae
		wPMortPhytDetW = wPMortPhytW - wPMortPhytPO4W
! 	soluble_P_grazing_loss
		wPLossPhytPO4 = fDissLoss * wPLossPhyt
! 	detrital_P_grazing_loss
		wPLossPhytDet = wPLossPhyt - wPLossPhytPO4
! 	SRP_in_water
		wPPrimPO4W = - wPUptPhyt + wPExcrPhytW + wPLossPhytPO4 + wPMortPhytPO4W
! 	Detritus_in_water
		wPPrimDetW = wPLossPhytDet + wPMortPhytDetW
! 	soluble_P_flux_from_died_Algae
		tPMortPhytPO4S = fDissMortPhyt * tPMortPhytS
! 	detrital_P_flux_from_died_Algae
		tPMortPhytDetS = tPMortPhytS - tPMortPhytPO4S
! 	Sediment_detritus
		if (InclPhytS == 1) then
		tPPrimDetS = tPMortPhytDetS 		else		tPPrimDetS = tPSetPhyt 		endif
! 	Pore_water_P
		tPPrimPO4S = tPExcrPhytS + tPMortPhytPO4S
! 	total_P_flux
		tPPrimTotT = 0.0
! 	ammonium_flux_from_died_Algae
		wNMortPhytNH4W = fDissMortPhyt * wNMortPhytW
! 	detrital_N_flux_from_died_Algae
		wNMortPhytDetW = wNMortPhytW - wNMortPhytNH4W
! 	NH4-N_grazing_loss
		wNLossPhytNH4 = fDissLoss * wNLossPhyt
! 	detrital_N_grazing_loss
		wNLossPhytDet = wNLossPhyt - wNLossPhytNH4
! 	ammonium_in_water
		wNPrimNH4W = - wNUptNH4Phyt + wNExcrPhytW + wNLossPhytNH4 + wNMortPhytNH4W
! 	nitrate_in_water
		wNPrimNO3W = - wNUptNO3Phyt
! 	Detritus_in_water
		wNPrimDetW = wNLossPhytDet + wNMortPhytDetW
! 	ammonium_flux_from_died_Algae
		tNMortPhytNH4S = fDissMortPhyt * tNMortPhytS
! 	detrital_N_flux_from_died_Algae
		tNMortPhytDetS = tNMortPhytS - tNMortPhytNH4S
! 	Pore_water_ammonium
		tNPrimNH4S = tNExcrPhytS + tNMortPhytNH4S
! 	Pore_water_nitrate
		tNPrimNO3S = 0.0
! 	Sediment_detritus
		if (InclPhytS == 1) then
		tNPrimDetS = tNMortPhytDetS 		else		tNPrimDetS = tNSetPhyt 		endif
! 	total_N_flux
		tNPrimTotT = 0.0
! 	Si_excretion_of_bottom_Algae
		tSiExcrDiatS = cSiDDiat * tDRespDiatS
! 	mortality_of_bottom_Algae
		tSiMortDiatS = cSiDDiat * tDMortDiatS
! 	total_Si_flux_to_SiO2_in_PRIM_module
		wSiPrimSiO2W = wSiExcrDiatW - wSiUptDiat + tSiExcrDiatS / sDepthW
! 	total_Si_flux_to_sed_detritus_in_PRIM_module
		wSiPrimDetW = wSiMortDiatW + wSiLossDiat
! 	total_Si_flux_to_sed_diatoms_in_PRIM_module
		if (InclPhytS == 1) then
		tSiPrimDiatS = tSiSetDiat - tSiResusDiat - tSiExcrDiatS - tSiMortDiatS 		else		tSiPrimDiatS = 0.0 		endif
! 	Sediment_detritus
		if (InclPhytS == 1) then
		tSiPrimDetS = tSiMortDiatS 		else		tSiPrimDetS = tSiSetDiat 		endif
! 	total_Si_flux
		tSiPrimTotT = 0.0
! 	Poole-Atkins_coefficient
		aPACoef = cPACoefMin +(cPACoefMax - cPACoefMin) * hPACoef / (hPACoef + oDOMW)
! 	max_Secchi_depth
		bSecchiMax = sDepthW + cSecchiPlus
! 	Secchi_depth
		aSecchi = min(bSecchiMax,aPACoef / aExtCoefOpen)
! 	Secchi_depth
		aTransparency = aSecchi / sDepthW
! 	euphotic_depth
		aDepthEuph = cEuph * aSecchi
! 	relative_euphotic_depth
		aRelDepthEuph = aDepthEuph / sDepthW
! 	Chla_per_m2
		aChlaH = oChla * sDepthW
! 	%_cover_with_algae
		aCovPhytW = cCovSpPhyt *(oDPhytW * sDepthW)
! 	average_spec_extinction_of_algae_per_unit_chl-a
		rExtChPhyt = aExtPhyt /(oChla / mgPerg + NearZero)
! 	temp_function_of_zooplankton
		uFunTmZoo = exp(-0.5/(cSigTmZoo*cSigTmZoo) *((uTm-cTmOptZoo)*(uTm-cTmOptZoo) -(c&
		&TmRef-cTmOptZoo)*(cTmRef-cTmOptZoo)))
! 	P/D_ratio_herbzooplankton
		rPDZoo = sPZoo /(sDZoo+NearZero)
! 	N/C_ratio_herbzooplankton
		rNDZoo = sNZoo/(sDZoo+NearZero)
! 	food_for_zooplankton
		oDFoodZoo = cPrefDiat * sDDiatW + cPrefGren * sDGrenW + cPrefBlue * sDBlueW + cP&
		&refDet * sDDetW
! 	filtering_rate
		aFilt = cFiltMax * uFunTmZoo * hFilt /(hFilt + oDOMW)
! 	max_assimilation_rate_of_zooplanktontemp_corrected
		ukDAssTmZoo = fDAssZoo * cFiltMax * uFunTmZoo * hFilt
! 	food_saturation_function_of_zooplankton
		aDSatZoo = oDFoodZoo /(hFilt + oDOMW)
! 	respiration_constant_of_zooplankton
		ukDRespTmZoo = kDRespZoo * uFunTmZoo
! 	intrinsic_rate_of_increase_of_zooplankton
		ukDIncrZoo = ukDAssTmZoo - ukDRespTmZoo - kMortZoo
! 	environmental_correction_of_zooplankton
		wDEnvZoo = max(0.0,ukDIncrZoo / cDCarrZoo * sDZoo*sDZoo)
! 	assimilation_of_zooplankton
		wDAssZoo = aDSatZoo *(ukDAssTmZoo * sDZoo - wDEnvZoo)
! 	consumption_of_zooplankton
		wDConsZoo = wDAssZoo / fDAssZoo
! 	DW_detritus_consumption_by_zooplankton
		wDConsDetZoo = cPrefDet*sDDetW / oDFoodZoo * wDConsZoo
! 	DW_diatoms_consumption_by_zooplankton
		wDConsDiatZoo = cPrefDiat*sDDiatW / oDFoodZoo * wDConsZoo
! 	DW_greens_consumption_by_zooplankton
		wDConsGrenZoo = cPrefGren*sDGrenW / oDFoodZoo * wDConsZoo
! 	DW_blue-greens_consumption_by_zooplankton
		wDConsBlueZoo = cPrefBlue*sDBlueW / oDFoodZoo * wDConsZoo
! 	phytoplankton_consumption_by_zooplankton
		wDConsPhytZoo = wDConsDiatZoo + wDConsGrenZoo + wDConsBlueZoo
! 	egestion_of_zooplankton
		wDEgesZoo = wDConsZoo - wDAssZoo
! 	corr_factor_of_zoopl_respiration_for_P_and_N_content
		aCorDRespZoo = max(cPDZooRef / rPDZoo,cNDZooRef / rNDZoo)
! 	zoopl_respiration
		wDRespZoo = aCorDRespZoo * ukDRespTmZoo * sDZoo
! 	zoopl_mortalityincl_environmental_correction
		wDMortZoo = kMortZoo * sDZoo +(1.0 - aDSatZoo) * wDEnvZoo
! 	Zooplankton_food
		oPFoodZoo = cPrefDiat*sPDiatW + cPrefGren*sPGrenW + cPrefBlue*sPBlueW + cPrefDet&
		& * sPDetW
! 	P/D_ratio_of_zooplankton_food
		rPDFoodZoo = oPFoodZoo /(oDFoodZoo+NearZero)
! 	P_diatom_consumption_by_zoopl
		wPConsDiatZoo = rPDDiatW * wDConsDiatZoo
! 	P_green_consumption_by_zoopl
		wPConsGrenZoo = rPDGrenW * wDConsGrenZoo
! 	P_bluegreen_consumption_by_zoopl
		wPConsBlueZoo = rPDBlueW * wDConsBlueZoo
! 	total_P_phytoplankton_consumption_by_zoopl
		wPConsPhytZoo = wPConsDiatZoo + wPConsGrenZoo + wPConsBlueZoo
! 	consumption_of_detrital_P
		wPConsDetZoo = rPDDetW * wDConsDetZoo
! 	total_P_consumption
		wPConsZoo = wPConsPhytZoo + wPConsDetZoo
! 	P_assimilation_efficiency_of_herbivores
		afPAssZoo = min(1.0,cPDZooRef / rPDFoodZoo * fDAssZoo)
! 	assimilation_by_herbivores
		wPAssZoo = afPAssZoo * wPConsZoo
! 	P_egestion
		wPEgesZoo = wPConsZoo - wPAssZoo
! 	soluble_P_egestion
		wPEgesZooPO4 = fDissEgesZoo*wPEgesZoo
! 	detrital_P_egestion
		wPEgesZooDet = wPEgesZoo - wPEgesZooPO4
! 	P_excretion_rate_of_herbivores
		akPExcrZoo = rPDZoo / cPDZooRef * kDRespZoo * uFunTmZoo
! 	P_excretion
		wPExcrZoo = akPExcrZoo*sPZoo
! 	mortality
		wPMortZoo = rPDZoo * wDMortZoo
! 	soluble_P_mortality
		wPMortZooPO4 = fDissMortZoo * wPMortZoo
! 	detrital_P_mortality
		wPMortZooDet = wPMortZoo - wPMortZooPO4
! 	Zooplankton_food
		oNFoodZoo = cPrefDiat*sNDiatW + cPrefGren*sNGrenW + cPrefBlue*sNBlueW + cPrefDet&
		&*sNDetW
! 	N/C_ratio_of_zooplankton_food
		rNDFoodZoo = oNFoodZoo /(oDFoodZoo+NearZero)
! 	N_diatom_consumption_by_zoopl
		wNConsDiatZoo = rNDDiatW*wDConsDiatZoo
! 	N_green_consumption_by_zoopl
		wNConsGrenZoo = rNDGrenW*wDConsGrenZoo
! 	N_bluegreen_consumption_by_zoopl
		wNConsBlueZoo = rNDBlueW*wDConsBlueZoo
! 	total_N_phytoplankton_consumption_by_zoopl
		wNConsPhytZoo = wNConsDiatZoo + wNConsGrenZoo + wNConsBlueZoo
! 	consumption_of_detrital_N
		wNConsDetZoo = rNDDetW*wDConsDetZoo
! 	total_N_consumption
		wNConsZoo = wNConsPhytZoo + wNConsDetZoo
! 	N_assimilation_efficiency_of_herbivores
		afNAssZoo = min(1.0,cNDZooRef / rNDFoodZoo * fDAssZoo)
! 	assimilation_by_herbivores
		wNAssZoo = afNAssZoo*wNConsZoo
! 	N_egestion
		wNEgesZoo = wNConsZoo - wNAssZoo
! 	soluble_N_egestion
		wNEgesZooNH4 = fDissEgesZoo*wNEgesZoo
! 	detrital_N_egestion
		wNEgesZooDet = wNEgesZoo - wNEgesZooNH4
! 	N_excretion_rate_of_herbivores
		kNExcrZoo = rNDZoo / cNDZooRef * kDRespZoo * uFunTmZoo
! 	N_excretion
		wNExcrZoo = kNExcrZoo*sNZoo
! 	mortality
		wNMortZoo = rNDZoo*wDMortZoo
! 	soluble_N_mortality
		wNMortZooNH4 = fDissMortZoo*wNMortZoo
! 	detrital_N_mortality
		wNMortZooDet = wNMortZoo - wNMortZooNH4
! 	consumption_of_diatoms
		wSiConsDiatZoo = cSiDDiat * wDConsDiatZoo
! 	temp_function_of_zoobenthos
		uFunTmBent = exp(-0.5/(cSigTmBent*cSigTmBent) *((uTm-cTmOptBent)*(uTm-cTmOptBent&
		&) - (cTmRef-cTmOptBent)*(cTmRef-cTmOptBent)))
! 	food_for_zoobenthos
		aDFoodBent = sDDetS + aDPhytS
! 	P/D_ratio_of_zoobenthos
		rPDBent = sPBent /(sDBent+NearZero)
! 	N/D_ratio_of_zoobenthos
		rNDBent = sNBent /(sDBent+NearZero)
! 	migration_flux
		tDMigrBent = kMigrBent *(cDBentIn - sDBent)
! 	food_limitation_function_of_zoobenthos
		aDSatBent = aDFoodBent /(hDFoodBent + aDFoodBent)
! 	intrinsic_net_increase_rate_of_zoobenthos
		ukDIncrBent = (kDAssBent - kDRespBent) * uFunTmBent - kMortBent
! 	environmental_correction_of_zoobenthos
		tDEnvBent = max(0.0,ukDIncrBent / cDCarrBent * sDBent*sDBent)
! 	assimilation_of_zoobenthos
		tDAssBent = aDSatBent *(kDAssBent * uFunTmBent * sDBent - tDEnvBent)
! 	specific_assimilation_rate_of_zoobenthos
		aDAssBentSp = tDAssBent / sDBent
! 	consumption_of_zoobenthos
		tDConsBent = tDAssBent / fDAssBent
! 	detritus_consumption_by_zoobenthos
		tDConsDetBent = sDDetS / aDFoodBent * tDConsBent
! 	diatoms_consumption_by_zoobenthos
		tDConsDiatBent = sDDiatS / aDFoodBent * tDConsBent
! 	greens_consumption_by_zoobenthos
		tDConsGrenBent = sDGrenS / aDFoodBent * tDConsBent
! 	blue-greens_consumption_by_zoobenthos
		tDConsBlueBent = sDBlueS / aDFoodBent * tDConsBent
! 	phutoplankton_consumption_by_zoobenthos
		tDConsPhytBent = tDConsDiatBent + tDConsGrenBent + tDConsBlueBent
! 	egestion_of_zoobenthos
		tDEgesBent = tDConsBent - tDAssBent
! 	respiration_of_zoobenthos
		tDRespBent = (cPDBentRef / rPDBent) * kDRespBent * uFunTmBent * sDBent
! 	zoobenthos_mortality_incl_environmental_correction
		tDMortBent = kMortBent*sDBent +(1.0 - aDSatBent) * tDEnvBent
! 	food_for_zoobenthos
		aPFoodBent = sPDetS + aPPhytS
! 	average_P/D_ratio_of_zoobenthos_food
		rPDFoodBent = aPFoodBent /(aDFoodBent+NearZero)
! 	detrital_P_consumption_by_zoobenthos
		tPConsDetBent = rPDDetS * tDConsDetBent
! 	diatom_P_consumption_by_zoobenthos
		tPConsDiatBent = rPDDiatS * tDConsDiatBent
! 	greens_P_consumption_by_zoobenthos
		tPConsGrenBent = rPDGrenS * tDConsGrenBent
! 	blue-greens_P_consumption_by_zoobenthos
		tPConsBlueBent = rPDBlueS * tDConsBlueBent
! 	phytoplankton_P_consumption_by_zoobenthos
		tPConsPhytBent = tPConsDiatBent + tPConsGrenBent + tPConsBlueBent
! 	total_P_consumption_of_zoobenthos
		tPConsBent = tPConsDetBent + tPConsPhytBent
! 	P_assim_efficiency_of_zoobenthos
		afPAssBent = min(1.0,cPDBentRef / rPDFoodBent * fDAssBent)
! 	P_assimilation_of_zoobenthos
		tPAssBent = afPAssBent * tPConsBent
! 	egestion_of_zoobenthos
		tPEgesBent = tPConsBent - tPAssBent
! 	SRP_egestion_of_zoobenthos
		tPEgesBentPO4 = fDissEgesBent * tPEgesBent
! 	detrital_P_egestion_of_zoobenthos
		tPEgesBentDet = (1.0 - fDissEgesBent) * tPEgesBent
! 	P_excretion_of_zoobenthos
		tPExcrBent = (rPDBent / cPDBentRef) * kDRespBent * uFunTmBent * sPBent
! 	mortality_of_zoobenthos
		tPMortBent = rPDBent * tDMortBent
! 	part_of_died_zoobenthos_P_becoming_dissolved_P
		tPMortBentPO4 = fDissMortBent * tPMortBent
! 	part_of_died_zoobenthos_P_becoming_detrital_P
		tPMortBentDet = (1.0-fDissMortBent)*tPMortBent
! 	net_migration_flux
		tPMigrBent = kMigrBent *(cPDBentRef*cDBentIn - sPBent)
! 	food_for_zoobenthos
		aNFoodBent = sNDetS + aNPhytS
! 	average_N/D_ratio_of_zoobenthos_food
		rNDFoodBent = aNFoodBent /(aDFoodBent+NearZero)
! 	Net_migration_flux
		tNMigrBent = kMigrBent *(cNDBentRef*cDBentIn - sNBent)
! 	detrital_N_consumption_by_zoobenthos
		tNConsDetBent = rNDDetS * tDConsDetBent
! 	diatom_N_consumption_by_zoobenthos
		tNConsDiatBent = rNDDiatS * tDConsDiatBent
! 	greens_N_consumption_by_zoobenthos
		tNConsGrenBent = rNDGrenS * tDConsGrenBent
! 	blue-greens_N_consumption_by_zoobenthos
		tNConsBlueBent = rNDBlueS * tDConsBlueBent
! 	phytoplankton_N_consumption_by_zoobenthos
		tNConsPhytBent = tNConsDiatBent + tNConsGrenBent + tNConsBlueBent
! 	total_N_consumption_of_zoobenthos
		tNConsBent = tNConsDetBent + tNConsPhytBent
! 	N_assim_efficiency_of_zoobenthos
		afNAssBent = min(1.0,cNDBentRef / rNDFoodBent * fDAssBent)
! 	N_assimilation_of_zoobenthos
		tNAssBent = afNAssBent * tNConsBent
! 	egestion_of_zoobenthos
		tNEgesBent = tNConsBent - tNAssBent
! 	NH4_egestion_of_zoobenthos
		tNEgesBentNH4 = fDissEgesBent * tNEgesBent
! 	detrital_N_egestion_of_zoobenthos
		tNEgesBentDet = (1.0 - fDissEgesBent) * tNEgesBent
! 	N_excretion_of_zoobenthos
		tNExcrBent = (rNDBent / cNDBentRef) * kDRespBent * uFunTmBent * sNBent
! 	mortality_of_zoobenthos
		tNMortBent = rNDBent * tDMortBent
! 	part_of_died_zoobenthos_N_becoming_ammonium-N
		tNMortBentNH4 = fDissMortBent*tNMortBent
! 	part_of_died_zoobenthos_N_becoming_detrital_N
		tNMortBentDet = (1.0-fDissMortBent)*tNMortBent
! 	diatom_consumption_by_zoobenthos
		tSiConsDiatBent = cSiDDiat * tDConsDiatBent
! 	total_fish_biomass
		aDFish = sDFiJv + sDFiAd
! 	total_fish_biomass
		aPFish = sPFiJv + sPFiAd
! 	total_fish_biomass
		aNFish = sNFiJv + sNFiAd
! 	P/D_ratio_of_young_fish
		rPDFiJv = sPFiJv /(sDFiJv+NearZero)
! 	P/D_ratio_of_adult_fish
		rPDFiAd = sPFiAd /(sDFiAd+NearZero)
! 	N/D_ratio_of_young_fish
		rNDFiJv = sNFiJv /(sDFiJv+NearZero)
! 	N/D_ratio_of_adult_fish
		rNDFiAd = sNFiAd /(sDFiAd+NearZero)
! 	Reproduction_flux
		if (0 == InclSeason) then
		tDReprFish = ((1/DaysPerYear)*fReprFish) * sDFiAd 		else if (Day >= cDayReprFish .and. Day < cDayReprFish + 1.0) then		tDReprFish = fReprFish * sDFiAd 		else		tDReprFish = 0.0 		endif
! 	Ageing
		if (0 == InclSeason) then
		tDAgeFish = ((1/DaysPerYear)*fAgeFish) * sDFiJv 		else if (Day >= 364.0) then		tDAgeFish = fAgeFish * sDFiJv 		else		tDAgeFish = 0.0 		endif
! 	vegetation_dependence_of_fish_feeding
		aFunVegFish = max(0.0,1.0 - cRelVegFish * aCovVeg)
! 	food_limitation_function_of_young_fish
		aDSatFiJv = (sDZoo * sDepthW) *(sDZoo * sDepthW) /(hDZooFiJv * hDZooFiJv + (sDZo&
		&o * sDepthW) *(sDZoo * sDepthW))
! 	intrinsic_net_increase_rate_of_fish
		ukDIncrFiJv = (kDAssFiJv - kDRespFiJv) * uFunTmFish - kMortFiJv
! 	environmental_correction_of_fish
		tDEnvFiJv = max(0.0,ukDIncrFiJv /(cDCarrFish - sDFiAd) * sDFiJv*sDFiJv)
! 	assimilation_of_fish
		tDAssFiJv = aDSatFiJv *(kDAssFiJv * uFunTmFish * sDFiJv - tDEnvFiJv)
! 	zooplankton_consumption_of_fish
		tDConsFiJv = tDAssFiJv / fDAssFiJv
! 	egestion_of_fish
		tDEgesFiJv = tDConsFiJv - tDAssFiJv
! 	respiration_of_fish
		tDRespFiJv = (cPDFishRef / rPDFiJv) * kDRespFiJv * uFunTmFish * sDFiJv
! 	fish_mortality_incl_environmental_correction
		tDMortFiJv = kMortFiJv * sDFiJv +(1.0 - aDSatFiJv) * tDEnvFiJv
! 	migration_flux
		tDMigrFiJv = kMigrFish *(cDFiJvIn - sDFiJv)
! 	food_limitation_function_of_adult_fish
		aDSatFiAd = (aFunVegFish * sDBent) *(aFunVegFish * sDBent) /(hDBentFiAd * hDBent&
		&FiAd + (aFunVegFish * sDBent) *(aFunVegFish * sDBent))
! 	intrinsic_net_increase_rate_of_fish
		ukDIncrFiAd = (kDAssFiAd - kDRespFiAd) * uFunTmFish - kMortFiAd
! 	environmental_correction_of_fish
		tDEnvFiAd = max(0.0,ukDIncrFiAd /(cDCarrFish - sDFiJv) * sDFiAd*sDFiAd)
! 	assimilation_of_fish
		tDAssFiAd = aDSatFiAd *(kDAssFiAd * uFunTmFish * sDFiAd - tDEnvFiAd)
! 	zoobenthos_consumption_of_fish
		tDConsFiAd = tDAssFiAd / fDAssFiAd
! 	egestion_of_fish
		tDEgesFiAd = tDConsFiAd - tDAssFiAd
! 	respiration_of_fish
		tDRespFiAd = (cPDFishRef / rPDFiAd) * kDRespFiAd * uFunTmFish * sDFiAd
! 	fish_mortality_incl_environmental_correction
		tDMortFiAd = kMortFiAd * sDFiAd +(1.0 - aDSatFiAd) * tDEnvFiAd
! 	fish_harvesting_constant
		if (cos(2.0 * Pi * sTime / DaysPerYear) > 0.1) then
		ukHarvFish = kHarvFishWin 		else		ukHarvFish = kHarvFishSum 		endif
! 	harvesting_of_fish
		tDHarvFish = ukHarvFish * sDFiAd
! 	migration_flux
		tDMigrFiAd = kMigrFish *(cDFiAdIn - sDFiAd)
! 	bent_fish_mortality
		tDMortFish = tDMortFiJv + tDMortFiAd
! 	part_of_died_fish_DW_fixed_in_bones_and_scales
		tDMortFishBot = fDBone * tDMortFish
! 	part_of_died_fish_DW_becoming_detritus
		tDMortFishDet = tDMortFish - tDMortFishBot
! 	Reproduction_flux
		tPReprFish = rPDFiAd * tDReprFish
! 	Ageing
		tPAgeFish = rPDFiJv * tDAgeFish
! 	net_migration_flux
		tPMigrFiJv = kMigrFish *(cPDFishRef * cDFiJvIn - sPFiJv)
! 	(zooplankton)_P_consumption_by_FiJv
		tPConsFiJv = rPDZoo * tDConsFiJv
! 	P_assim_efficiency_of_FiJv
		afPAssFiJv = min(1.0,cPDFishRef / rPDZoo * fDAssFiJv)
! 	P_assimilation_of_FiJv
		tPAssFiJv = afPAssFiJv * tPConsFiJv
! 	egestion_of_FiJv
		tPEgesFiJv = tPConsFiJv - tPAssFiJv
! 	P_excretion_of_FiJv
		tPExcrFiJv = (rPDFiJv / cPDFishRef) * kDRespFiJv * uFunTmFish * sPFiJv
! 	mortality_of_FiJv
		tPMortFiJv = rPDFiJv * tDMortFiJv
! 	net_migration_flux
		tPMigrFiAd = kMigrFish *(cPDFishRef * cDFiAdIn - sPFiAd)
! 	(zoobenthos)_P_consumption_by_FiAd
		tPConsFiAd = rPDBent * tDConsFiAd
! 	P_assim_efficiency_of_FiAd
		afPAssFiAd = min(1.0,cPDFishRef / rPDBent * fDAssFiAd)
! 	P_assimilation_of_FiAd
		tPAssFiAd = afPAssFiAd * tPConsFiAd
! 	egestion_of_FiAd
		tPEgesFiAd = tPConsFiAd - tPAssFiAd
! 	P_excretion_of_FiAd
		tPExcrFiAd = (rPDFiAd / cPDFishRef) * kDRespFiAd * uFunTmFish * sPFiAd
! 	mortality_of_FiAd
		tPMortFiAd = rPDFiAd * tDMortFiAd
! 	harvesting_of_FiAd
		tPHarvFish = rPDFiAd * tDHarvFish
! 	-
		tPMortFish = tPMortFiJv + tPMortFiAd
! 	part_of_died_fish_P_fixed_in_bones_AND_scales
		tPMortFishBot = fPBone * tPMortFish
! 	part_of_died_fish_P_becoming_dissolved_P
		tPMortFishPO4 = fDissMortFish *(tPMortFish - tPMortFishBot)
! 	part_of_died_fish_PW_becoming_detritus
		tPMortFishDet = tPMortFish - tPMortFishBot - tPMortFishPO4
! 	total_fish_egestion
		tPEgesFish = tPEgesFiJv + tPEgesFiAd
! 	SRP_egestion_of_fish
		tPEgesFishPO4 = fDissEgesFish * tPEgesFish
! 	detrital_P_egestion_of_fish
		tPEgesFishDet = tPEgesFish - tPEgesFishPO4
! 	Reproduction_flux
		tNReprFish = rNDFiAd * tDReprFish
! 	Ageing
		tNAgeFish = rNDFiJv * tDAgeFish
! 	net_migration_flux
		tNMigrFiJv = kMigrFish *(cNDFishRef * cDFiJvIn - sNFiJv)
! 	(zooplankton)_N_consumption_by_FiJv
		tNConsFiJv = rNDZoo * tDConsFiJv
! 	N_assim_efficiency_of_FiJv
		afNAssFiJv = min(1.0,cNDFishRef / rNDZoo * fDAssFiJv)
! 	N_assimilation_of_FiJv
		tNAssFiJv = afNAssFiJv * tNConsFiJv
! 	egestion_of_FiJv
		tNEgesFiJv = tNConsFiJv - tNAssFiJv
! 	N_excretion_of_FiJv
		tNExcrFiJv = (rNDFiJv / cNDFishRef) * kDRespFiJv * uFunTmFish * sNFiJv
! 	mortality_of_FiJv
		tNMortFiJv = rNDFiJv * tDMortFiJv
! 	net_migration_flux
		tNMigrFiAd = kMigrFish *(cNDFishRef * cDFiAdIn - sNFiAd)
! 	(zoobenthos)_N_consumption_by_FiAd
		tNConsFiAd = rNDBent * tDConsFiAd
! 	N_assim_efficiency_of_FiAd
		afNAssFiAd = min(1.0,cNDFishRef / rNDBent * fDAssFiAd)
! 	N_assimilation_of_FiAd
		tNAssFiAd = afNAssFiAd * tNConsFiAd
! 	egestion_of_FiAd
		tNEgesFiAd = tNConsFiAd - tNAssFiAd
! 	N_excretion_of_FiAd
		tNExcrFiAd = (rNDFiAd / cNDFishRef) * kDRespFiAd * uFunTmFish * sNFiAd
! 	mortality_of_FiAd
		tNMortFiAd = rNDFiAd * tDMortFiAd
! 	harvesting_of_FiAd
		tNHarvFish = rNDFiAd * tDHarvFish
! 	-
		tNMortFish = tNMortFiJv + tNMortFiAd
! 	part_of_died_fish_N_fixed_in_bones_AND_scales
		tNMortFishBot = fDBone * tNMortFish
! 	part_of_died_fish_N_becoming_dissolved_N
		tNMortFishNH4 = fDissMortFish *(tNMortFish - tNMortFishBot)
! 	part_of_died_fish_NW_becoming_detritus
		tNMortFishDet = tNMortFish - tNMortFishBot - tNMortFishNH4
! 	total_fish_egestion
		tNEgesFish = tNEgesFiJv + tNEgesFiAd
! 	NH4_egestion_of_fish
		tNEgesFishNH4 = fDissEgesFish * tNEgesFish
! 	detrital_xN_egestion_of_fish
		tNEgesFishDet = tNEgesFish - tNEgesFishNH4
! 	temp_function_of_Pisc
		uFunTmPisc = exp(-0.5 /(cSigTmPisc*cSigTmPisc) *((uTm - cTmOptPisc)*(uTm - cTmOp&
		&tPisc) - (cTmRef - cTmOptPisc)*(cTmRef - cTmOptPisc)))
! 	migration_flux
		tDMigrPisc = kMigrPisc *(cDPiscIn - sDPisc)
! 	Carrying_capacity_of_Pisc_for_lake_without_OR_with_marsh_zone_resp
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,cDCarrPiscBare)) 		else if (sDShootPhra < cDPhraMinPisc) then		aDCarrPisc = cDCarrPiscMin 		else if (aCovVeg < cCovVegMin) then		aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,fMarsh *(1.0/PerCent) * cRelPhr&
		&aPisc)) 		else		aDCarrPisc = max(cDCarrPiscMin,min(cDCarrPiscMax,fMarsh *(1.0/PerCent) *(cRelPhr&
		&aPisc + cRelVegPisc))) 		endif
! 	vegetation_dependence_of_Pisc_growth_rate
		aFunVegPisc = aDSubVeg /(hDVegPisc + aDSubVeg + NearZero)
! 	food_limitation_function_of_Pisc
		aDSatPisc = aDFish*aDFish /(hDFishPisc*hDFishPisc + aDFish*aDFish)
! 	intrinsic_net_increase_rate_of_Pisc
		akDIncrPisc = (kDAssPisc * aFunVegPisc - kDRespPisc) * uFunTmPisc - kMortPisc
! 	environmental_correction_of_Pisc
		tDEnvPisc = max(0.0,akDIncrPisc / aDCarrPisc * sDPisc*sDPisc)
! 	assimilation_of_Pisc
		tDAssPisc = aDSatPisc *(kDAssPisc * aFunVegPisc * uFunTmPisc * sDPisc - tDEnvPis&
		&c)
! 	consumption_of_Pisc
		tDConsPisc = tDAssPisc / fDAssPisc
! 	egestion_of_Pisc
		tDEgesPisc = tDConsPisc - tDAssPisc
! 	young_fish_consumption_by_Pisc
		tDConsFiJvPisc = sDFiJv / aDFish * tDConsPisc
! 	adult_fish_consumption_by_Pisc
		tDConsFiAdPisc = tDConsPisc - tDConsFiJvPisc
! 	respiration_of_Pisc
		tDRespPisc = kDRespPisc * uFunTmPisc * sDPisc
! 	mortality_of_Pisc(incl_environmental_correction)
		tDMortPisc = kMortPisc * sDPisc +(1.0 - aDSatPisc) * tDEnvPisc
! 	part_of_died_fish_DW_fixed_in_bones_AND_scales
		tDMortPiscBot = fDBone * tDMortPisc
! 	part_of_died_Pisc_DW_becoming_detritus
		tDMortPiscDet = tDMortPisc - tDMortPiscBot
! 	fish_harvesting_constant
		if (cos(2.0*Pi * sTime / DaysPerYear) > 0.1) then
		ukHarvPisc = kHarvPiscWin 		else		ukHarvPisc = kHarvPiscSum 		endif
! 	harvesting_of_Pisc
		tDHarvPisc = ukHarvPisc * sDPisc
! 	_Piscivorous_fish
		aPPisc = cPDPisc * sDPisc
! 	young_fish_consumption_by_Pisc
		tPConsFiJvPisc = rPDFiJv * tDConsFiJvPisc
! 	adult_fish_consumption_by_Pisc
		tPConsFiAdPisc = rPDFiAd * tDConsFiAdPisc
! 	total_P_consumption_by_Pisc
		tPConsPisc = tPConsFiJvPisc + tPConsFiAdPisc
! 	average_P/D_ratio_of_Pisc_food
		rPDFoodPisc = tPConsPisc / tDConsPisc
! 	P_assim_efficiency_of_Pisc
		afPAssPisc = min(1.0,cPDPisc / rPDFoodPisc * fDAssPisc)
! 	P_assimilation_of_Pisc
		tPAssPisc = afPAssPisc * tPConsPisc
! 	egestion_of_Pisc
		tPEgesPisc = tPConsPisc - tPAssPisc
! 	SRP_egestion_of_Pisc
		tPEgesPiscPO4 = fDissEgesPisc * tPEgesPisc
! 	detrital_P_egestion_of_Pisc
		tPEgesPiscDet = tPEgesPisc - tPEgesPiscPO4
! 	respiration_of_Pisc
		tPExcrPisc = cPDPisc * tDRespPisc
! 	mortality_of_Pisc
		tPMortPisc = cPDPisc * tDMortPisc
! 	part_of_died_Pisc_P_fixed_in_bones_AND_scales
		tPMortPiscBot = fPBone * tPMortPisc
! 	part_of_died_fish_P_becoming_dissolved_P
		tPMortPiscPO4 = fDissMortPisc *(tPMortPisc - tPMortPiscBot)
! 	part_of_died_Pisc_P_becoming_detrital_P
		tPMortPiscDet = tPMortPisc - tPMortPiscBot - tPMortPiscPO4
! 	net_migration_flux
		tPMigrPisc = kMigrPisc *(cPDPisc * cDPiscIn - aPPisc)
! 	harvesting_of_Pisc
		tPHarvPisc = cPDPisc * tDHarvPisc
! 	Piscivorous_fish
		aNPisc = cNDPisc * sDPisc
! 	young_fish_consumption_by_Pisc
		tNConsFiJvPisc = rNDFiJv * tDConsFiJvPisc
! 	adult_fish_consumption_by_Pisc
		tNConsFiAdPisc = rNDFiAd * tDConsFiAdPisc
! 	total_N_consumption_by_Pisc
		tNConsPisc = tNConsFiJvPisc + tNConsFiAdPisc
! 	average_N/D_ratio_of_Pisc_food
		rNDFoodPisc = tNConsPisc / tDConsPisc
! 	N_assim_efficiency_of_Pisc
		afNAssPisc = min(1.0,cNDPisc / rNDFoodPisc * fDAssPisc)
! 	N_assimilation_of_Pisc
		tNAssPisc = afNAssPisc * tNConsPisc
! 	egestion_of_Pisc
		tNEgesPisc = tNConsPisc - tNAssPisc
! 	SRN_egestion_of_Pisc
		tNEgesPiscNH4 = fDissEgesPisc * tNEgesPisc
! 	detrital_N_egestion_of_Pisc
		tNEgesPiscDet = tNEgesPisc - tNEgesPiscNH4
! 	respiration_of_Pisc
		tNExcrPisc = cNDPisc * tDRespPisc
! 	mortality_of_Pisc
		tNMortPisc = cNDPisc * tDMortPisc
! 	part_of_died_Pisc_N_fixed_in_bones_AND_scales
		tNMortPiscBot = fDBone * tNMortPisc
! 	part_of_died_fish_N_becoming_dissolved_N
		tNMortPiscNH4 = fDissMortPisc *(tNMortPisc - tNMortPiscBot)
! 	part_of_died_Pisc_N_becoming_detrital_N
		tNMortPiscDet = tNMortPisc - tNMortPiscBot - tNMortPiscNH4
! 	net_migration_flux
		tNMigrPisc = kMigrPisc *(cNDPisc * cDPiscIn - aNPisc)
! 	harvesting_of_Pisc
		tNHarvPisc = cNDPisc * tDHarvPisc
! 	total_foodweb_flux_of_DW_in_Herbivorous_zooplankton
		wDWebZoo = wDAssZoo - wDRespZoo - wDMortZoo - tDConsFiJv / sDepthW
! 	total_foodweb_flux_of_P_in_Herbivorous_zooplankton
		wPWebZoo = wPAssZoo - wPExcrZoo - wPMortZoo - tPConsFiJv / sDepthW
! 	total_foodweb_flux_of_N_in_Herbivorous_zooplankton
		wNWebZoo = wNAssZoo - wNExcrZoo - wNMortZoo - tNConsFiJv / sDepthW
! 	total_foodweb_flux_of_DW_in_Zoobenthos
		tDWebBent = tDMigrBent + tDAssBent - tDConsFiAd - tDRespBent - tDMortBent
! 	total_foodweb_flux_of_P_in_Zoobenthos
		tPWebBent = tPMigrBent + tPAssBent - tPConsFiAd - tPExcrBent - tPMortBent
! 	total_foodweb_flux_of_N_in_Zoobenthos
		tNWebBent = tNMigrBent + tNAssBent - tNConsFiAd - tNExcrBent - tNMortBent
! 	total_foodweb_flux_of_DW_in_Young_fish
		tDWebFiJv = tDMigrFiJv + tDReprFish - tDAgeFish + tDAssFiJv - tDRespFiJv - tDMor&
		&tFiJv - tDConsFiJvPisc
! 	total_foodweb_flux_of_P_in_Young_fish
		tPWebFiJv = tPMigrFiJv + tPReprFish - tPAgeFish + tPAssFiJv - tPExcrFiJv - tPMor&
		&tFiJv - tPConsFiJvPisc
! 	total_foodweb_flux_of_N_in_Young_fish
		tNWebFiJv = tNMigrFiJv + tNReprFish - tNAgeFish + tNAssFiJv - tNExcrFiJv - tNMor&
		&tFiJv - tNConsFiJvPisc
! 	total_foodweb_flux_of_DW_in_Adult_fish
		tDWebFiAd = tDMigrFiAd + tDAssFiAd - tDRespFiAd - tDMortFiAd - tDReprFish + tDAg&
		&eFish - tDConsFiAdPisc - tDHarvFish
! 	total_foodweb_flux_of_P_in_Adult_fish
		tPWebFiAd = tPMigrFiAd + tPAssFiAd - tPExcrFiAd - tPMortFiAd - tPReprFish + tPAg&
		&eFish - tPConsFiAdPisc - tPHarvFish
! 	total_foodweb_flux_of_N_in_Adult_fish
		tNWebFiAd = tNMigrFiAd + tNAssFiAd - tNExcrFiAd - tNMortFiAd - tNReprFish + tNAg&
		&eFish - tNConsFiAdPisc - tNHarvFish
! 	total_foodweb_flux_of_DW_in_predatory_fish
		tDWebPisc = tDMigrPisc + tDAssPisc - tDRespPisc - tDMortPisc - tDHarvPisc
! 	total_foodweb_flux_of_DW_in_Detritus_in_lake_water
		if (0 == InclWeb) then
		wDWebDetW = 0.0 		else		wDWebDetW = - wDConsDetZoo + wDEgesZoo + wDMortZoo +(tDEgesFiJv + tDEgesFiAd + t&
		&DMortFishDet + tDEgesPisc + tDMortPiscDet) / sDepthW 		endif
! 	total_foodweb_flux_of_DW_in_Diatoms_in_lake_water
		if (0 == InclWeb) then
		wDWebDiatW = 0.0 		else		wDWebDiatW = - wDConsDiatZoo 		endif
! 	total_foodweb_flux_of_DW_in_Greens_in_lake_water
		if (0 == InclWeb) then
		wDWebGrenW = 0.0 		else		wDWebGrenW = - wDConsGrenZoo 		endif
! 	total_foodweb_flux_of_DW_in_Blue-greens_in_lake_water
		if (0 == InclWeb) then
		wDWebBlueW = 0.0 		else		wDWebBlueW = - wDConsBlueZoo 		endif
! 	total_foodweb_flux_of_DW_in_Sediment_detritus_in_lake
		if (0 == InclWeb) then
		tDWebDetS = 0.0 		else		tDWebDetS = - tDConsDetBent + tDEgesBent + tDMortBent 		endif
! 	total_foodweb_flux_of_DW_in_sediment_diatoms_in_lake
		if (0 == InclWeb) then
		tDWebDiatS = 0.0 		else		tDWebDiatS = - tDConsDiatBent 		endif
! 	total_foodweb_flux_of_DW_in_sediment_greens_in_lake
		if (0 == InclWeb) then
		tDWebGrenS = 0.0 		else		tDWebGrenS = - tDConsGrenBent 		endif
! 	total_foodweb_flux_of_DW_in_sediment_blue-greens_in_lake
		if (0 == InclWeb) then
		tDWebBlueS = 0.0 		else		tDWebBlueS = - tDConsBlueBent 		endif
! 	total_food_web_flux_of_sediment_algae
		tDWebPhytS = tDWebDiatS + tDWebGrenS + tDWebBlueS
! 	total_DW_in_system
		if (0 == InclWeb) then
		tDWebTotT = 0.0 		else		tDWebTotT = - wDRespZoo * sDepthW + tDMigrFiJv + tDMigrFiAd + tDMigrPisc + tDMig&
		&rBent - tDRespFiJv - tDRespFiAd - tDRespPisc - tDRespBent - tDMortFishBot - tDMo&
		&rtPiscBot - tDHarvFish - tDHarvPisc 		endif
! 	total_foodweb_flux_of_P_in_SRP_in_water_in_lake_water
		if (0 == InclWeb) then
		wPWebPO4W = 0.0 		else		wPWebPO4W = wPExcrZoo + wPEgesZooPO4 + wPMortZooPO4 +(tPExcrFiJv + tPExcrFiAd + &
		&tPEgesFishPO4 + tPMortFishPO4 + tPExcrPisc + tPEgesPiscPO4 + tPMortPiscPO4) / sD&
		&epthW 		endif
! 	total_foodweb_flux_of_P_in_Detritus_in_lake_water
		if (0 == InclWeb) then
		wPWebDetW = 0.0 		else		wPWebDetW = - wPConsDetZoo + wPEgesZooDet + wPMortZooDet +(tPEgesFishDet + tPMor&
		&tFishDet + tPEgesPiscDet + tPMortPiscDet) / sDepthW 		endif
! 	total_foodweb_flux_of_P_in_Diatoms_in_lake_water
		if (0 == InclWeb) then
		wPWebDiatW = 0.0 		else		wPWebDiatW = - wPConsDiatZoo 		endif
! 	total_foodweb_flux_of_P_in_Greens_in_lake_water
		if (0 == InclWeb) then
		wPWebGrenW = 0.0 		else		wPWebGrenW = - wPConsGrenZoo 		endif
! 	total_foodweb_flux_of_P_in_Blue-greens_in_lake_water
		if (0 == InclWeb) then
		wPWebBlueW = 0.0 		else		wPWebBlueW = - wPConsBlueZoo 		endif
! 	total_foodweb_flux_of_P_in_Pore_water_P_in_lake_sediment
		if (0 == InclWeb) then
		tPWebPO4S = 0.0 		else		tPWebPO4S = tPExcrBent + tPEgesBentPO4 + tPMortBentPO4 		endif
! 	total_foodweb_flux_of_P_in_Sediment_P_in_lake
		if (0 == InclWeb) then
		tPWebDetS = 0.0 		else		tPWebDetS = - tPConsDetBent + tPEgesBentDet + tPMortBentDet 		endif
! 	total_foodweb_flux_of_P_in_sediment_diatoms_in_lake
		if (0 == InclWeb) then
		tPWebDiatS = 0.0 		else		tPWebDiatS = - tPConsDiatBent 		endif
! 	total_foodweb_flux_of_P_in_sediment_greens_in_lake
		if (0 == InclWeb) then
		tPWebGrenS = 0.0 		else		tPWebGrenS = - tPConsGrenBent 		endif
! 	total_foodweb_flux_of_P_in_sediment_blue-greens_in_lake
		if (0 == InclWeb) then
		tPWebBlueS = 0.0 		else		tPWebBlueS = - tPConsBlueBent 		endif
! 	total_food_web_flux_of_sediment_algae
		tPWebPhytS = tPWebDiatS + tPWebGrenS + tPWebBlueS
! 	total_P_in_system
		if (0 == InclWeb) then
		tPWebTotT = 0.0 		else		tPWebTotT = tPMigrFiJv + tPMigrFiAd + tPMigrPisc + tPMigrBent - tPMortFishBot - &
		&tPMortPiscBot - tPHarvFish - tPHarvPisc 		endif
! 	total_foodweb_flux_of_N_in_ammonium_in_water_in_lake_water
		if (0 == InclWeb) then
		wNWebNH4W = 0.0 		else		wNWebNH4W = wNExcrZoo + wNEgesZooNH4 + wNMortZooNH4 +(tNExcrFiJv + tNExcrFiAd + &
		&tNEgesFishNH4 + tNMortFishNH4 + tNExcrPisc + tNEgesPiscNH4 + tNMortPiscNH4) / sD&
		&epthW 		endif
! 	total_foodweb_flux_of_N_in_nitrate_in_water_in_lake_water
		if (0 == InclWeb) then
		wNWebNO3W = 0.0 		else		wNWebNO3W = 0.0 		endif
! 	total_foodweb_flux_of_N_in_Detritus_in_lake_water
		if (0 == InclWeb) then
		wNWebDetW = 0.0 		else		wNWebDetW = - wNConsDetZoo + wNEgesZooDet + wNMortZooDet +(tNEgesFishDet + tNMor&
		&tFishDet + tNEgesPiscDet + tNMortPiscDet) / sDepthW 		endif
! 	total_foodweb_flux_of_N_in_Diatoms_in_lake_water
		if (0 == InclWeb) then
		wNWebDiatW = 0.0 		else		wNWebDiatW = - wNConsDiatZoo 		endif
! 	total_foodweb_flux_of_N_in_Greens_in_lake_water
		if (0 == InclWeb) then
		wNWebGrenW = 0.0 		else		wNWebGrenW = - wNConsGrenZoo 		endif
! 	total_foodweb_flux_of_N_in_Blue-greens_in_lake_water
		if (0 == InclWeb) then
		wNWebBlueW = 0.0 		else		wNWebBlueW = - wNConsBlueZoo 		endif
! 	total_foodweb_flux_of_N_in_Pore_water_ammonium_in_lake_sediment
		if (0 == InclWeb) then
		tNWebNH4S = 0.0 		else		tNWebNH4S = tNExcrBent + tNEgesBentNH4 + tNMortBentNH4 		endif
! 	total_foodweb_flux_of_N_in_Pore_water_nitrate_in_lake_sediment
		if (0 == InclWeb) then
		tNWebNO3S = 0.0 		else		tNWebNO3S = 0.0 		endif
! 	total_foodweb_flux_of_N_in_Sediment_N_in_lake_sediment
		if (0 == InclWeb) then
		tNWebDetS = 0.0 		else		tNWebDetS = - tNConsDetBent + tNEgesBentDet + tNMortBentDet 		endif
! 	total_foodweb_flux_of_N_in_sediment_diatoms_in_lake
		if (0 == InclWeb) then
		tNWebDiatS = 0.0 		else		tNWebDiatS = - tNConsDiatBent 		endif
! 	total_foodweb_flux_of_N_in_sediment_greens_in_lake
		if (0 == InclWeb) then
		tNWebGrenS = 0.0 		else		tNWebGrenS = - tNConsGrenBent 		endif
! 	total_foodweb_flux_of_N_in_sediment_blue-greens_in_lake
		if (0 == InclWeb) then
		tNWebBlueS = 0.0 		else		tNWebBlueS = - tNConsBlueBent 		endif
! 	total_food_web_flux_of_sediment_algae
		tNWebPhytS = tNWebDiatS + tNWebGrenS + tNWebBlueS
! 	total_N_in_system
		if (0 == InclWeb) then
		tNWebTotT = 0.0 		else		tNWebTotT = tNMigrFiJv + tNMigrFiAd + tNMigrPisc + tNMigrBent - tNMortFishBot - &
		&tNMortPiscBot - tNHarvFish - tNHarvPisc 		endif
! 	total_foodweb_flux_of_silica_in_SiO2_lake_water
		wSiWebSiO2W = 0.0
! 	total_foodweb_flux_of_silica_in_lake_water_detritus
		if (0 == InclWeb) then
		wSiWebDetW = 0.0 		else		wSiWebDetW = wSiConsDiatZoo 		endif
! 	total_foodweb_flux_of_silica_in_sediment_detritus
		if (0 == InclWeb) then
		tSiWebDetS = 0.0 		else		tSiWebDetS = tSiConsDiatBent 		endif
! 	total_foodweb_flux_of_silica
		if (0 == InclWeb) then
		tSiWebTotT = 0.0 		else		tSiWebTotT = 0.0 		endif
! 	average_selection_factor
		aPrefAve = (cPrefDiat * sDDiatW + cPrefGren * sDGrenW + cPrefBlue * sDBlueW + cP&
		&refDet * sDDetW) / oDOMW
! 	total_zoopl_consumption(check)
		wDConsZoo2 = aFilt * aPrefAve * oDOMW * sDZoo
! 	specific_consumption_rate_of_zoopl(daily_ration)
		aDConsZooSp = wDConsZoo / sDZoo
! 	specific_C_assimilation_of_zooplankton
		aDAssZooSp = wDAssZoo / sDZoo
! 	specific_DW_grazing(daily_grazing)
		aDGrazSp = wDConsZoo / oDOMW
! 	specific_P_consumption_OR_daily_ration
		aPConsZooSp = wPConsZoo / sPZoo
! 	specific_P_grazing_OR_daily_grazing
		aPGrazSp = wPConsZoo / oPOMW
! 	specific_N_consumption_OR_daily_ration
		aNConsZooSp = wNConsZoo / sNZoo
! 	specific_N_grazing_OR_daily_grazing
		aNGrazSp = wNConsZoo / oNOMW
! 	Shoot/total_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		afDShootPhra = sDShootPhra /(sDRootPhra + sDShootPhra) 		else		afDShootPhra = 0.0 		endif
! 	Shoot/Root_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rDSRPhra = sDShootPhra/sDRootPhra 		else		rDSRPhra = 0.0 		endif
! 	Shoot_P/D_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDShootPhra = sPShootPhra/sDShootPhra 		else		rPDShootPhra = 0.0 		endif
! 	Shoot_N/D_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rNDShootPhra = sNShootPhra/sDShootPhra 		else		rNDShootPhra = 0.0 		endif
! 	Root_P/D_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rPDRootPhra = sPRootPhra/sDRootPhra 		else		rPDRootPhra = 0.0 		endif
! 	Root_N/D_-ratio
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		rNDRootPhra = sNRootPhra/sDRootPhra 		else		rNDRootPhra = 0.0 		endif
! 	-
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aLengShootPhra = sDShootPhra / cDStemPhra / cDensStemPhra 		else		aLengShootPhra = 0.0 		endif
! 	marks_start_of_root_allocation_to_shoot_of_phragmities
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		bDayInitPhra = 367 		else if (uTm >= cTmInitPhra .and. bDayInitPhra > 366) then		bDayInitPhra = Day 		else		bDayInitPhra = bDayInitPhra 		endif
! 	root_biomass_available_for_allocation_to_shoot
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		aDAllPhra = 0.0 		else if (Day < bDayInitPhra) then		aDAllPhra = 0.0 		else if (Day <= bDayInitPhra + 1.0 / kDAllPhra) then		aDAllPhra = fDAllPhra * sDRootPhra 		else		aDAllPhra = 0.0 		endif
! 	allocation_flux
		if (0 == InclSeason) then
		tDAllPhra = 0.0 		else if (0 == InclMarsh .or. fMarsh <= NearZero) then		tDAllPhra = 0.0 		else if (Day < bDayInitPhra) then		tDAllPhra = 0.0 		else if (Day <= bDayInitPhra + 1.0 / kDAllPhra) then		tDAllPhra = kDAllPhra * aDAllPhra 		else		tDAllPhra = 0.0 		endif
! 	translocation_of_N_initial_growth
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNTransPhra = rNDRootPhra * tDAllPhra 		else		tNTransPhra = 0.0 		endif
! 	translocation_of_P_initial_growth
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPTransPhra = rPDRootPhra * tDAllPhra 		else		tPTransPhra = 0.0 		endif
! 	max_uptake_rate_at_current_N/D_ratio_AND_temp
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aVNUptPhraMaxCr = max(0.0,cVNUptPhraMax * ((cQ10ProdPhra )** (0.1 *(uTm - cTmRef&
		&))) *(cNDPhraMax - rNDRootPhra) /(cNDPhraMax - cNDPhraMin)) 		else		aVNUptPhraMaxCr = 0.0 		endif
! 	half-saturating_N_concentration
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		ahNUptPhraS = aVNUptPhraMaxCr / cAffNUptPhra 		else		ahNUptPhraS = 0.0 		endif
! 	N_uptake_rate(by_roots)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aVNUptPhraS = aVNUptPhraMaxCr * oNDissSM /(ahNUptPhraS + oNDissSM) 		else		aVNUptPhraS = 0.0 		endif
! 	Total_N_uptake_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNUptPhraS = aVNUptPhraS * sDRootPhra 		else		tNUptPhraS = 0.0 		endif
! 	NH4_uptake_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNUptNH4PhraS = sNH4SM / aNDissSM * tNUptPhraS 		else		tNUptNH4PhraS = 0.0 		endif
! 	NO3_uptake_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNUptNO3PhraS = tNUptPhraS - tNUptNH4PhraS 		else		tNUptNO3PhraS = 0.0 		endif
! 	N_uptake_shoot
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNUptShootPhra = afDShootPhra * tNUptPhraS 		else		tNUptShootPhra = 0.0 		endif
! 	N_uptake_root
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNUptRootPhra = tNUptPhraS - tNUptShootPhra 		else		tNUptRootPhra = 0.0 		endif
! 	max_uptake_rate_at_current_P/D_ratio_AND_temp
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aVPUptPhraMaxCr = max(0.0,cVPUptPhraMax * ((cQ10ProdPhra )** (0.1 *(uTm - cTmRef&
		&))) *(cPDPhraMax - rPDRootPhra) /(cPDPhraMax - cPDPhraMin)) 		else		aVPUptPhraMaxCr = 0.0 		endif
! 	half-saturating_P_concentration
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		ahPUptPhraS = aVPUptPhraMaxCr / cAffPUptPhra 		else		ahPUptPhraS = 0.0 		endif
! 	P_uptake_rate(by_roots)
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aVPUptPhraS = aVPUptPhraMaxCr * oPO4SM /(ahPUptPhraS + oPO4SM) 		else		aVPUptPhraS = 0.0 		endif
! 	Total_P_uptake_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPUptPhraS = aVPUptPhraS * sDRootPhra 		else		tPUptPhraS = 0.0 		endif
! 	P_uptake_shoot
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPUptShootPhra = afDShootPhra * tPUptPhraS 		else		tPUptShootPhra = 0.0 		endif
! 	P_uptake_root
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPUptRootPhra = tPUptPhraS - tPUptShootPhra 		else		tPUptRootPhra = 0.0 		endif
! 	tempfunction_production_vegetation
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		uFunTmProdPhra = ((cQ10ProdPhra )** (0.1 *(uTm - cTmRef))) 		else		uFunTmProdPhra = 0.0 		endif
! 	maintenance_respiration_rate_at_current_temperature
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		ukDRespTmPhra = kDRespPhra * ((cQ10RespPhra )** (0.1 *(uTm - cTmRef))) 		else		ukDRespTmPhra = 0.0 		endif
! 	max_photosynthetic_rate_at_current_light_AND_temp
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		aMuPhotPhra = 0.0 		else if (aLengShootPhra >= sDepthWM) then		aMuPhotPhra = cMuPhraMax * uFunTmProdPhra * ufDay 		else		aMuPhotPhra = 0.0 		endif
! 	Droop_function_N-limitation
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNLimProdPhra = (1.0 - cNDPhraMin / rNDRootPhra) * cNDPhraMax /(cNDPhraMax - cND&
		&PhraMin) 		else		aNLimProdPhra = 0.0 		endif
! 	Droop_function_P-limitation
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aPLimProdPhra = (1.0 - cPDPhraMin / rPDRootPhra) * cPDPhraMax /(cPDPhraMax - cPD&
		&PhraMin) 		else		aPLimProdPhra = 0.0 		endif
! 	nutrient_reduction_function
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aNutLimPhra = min(aNLimProdPhra,aPLimProdPhra) 		else		aNutLimPhra = 0.0 		endif
! 	growth_rate
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aMuPhra = aMuPhotPhra * aNutLimPhra 		else		aMuPhra = 0.0 		endif
! 	intrinsic_net_increase_rate_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		akDIncrPhra = aMuPhotPhra - ukDRespTmPhra - kDMortShootPhra 		else		akDIncrPhra = 0.0 		endif
! 	density_correction_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDDensPhra = max(0.0,akDIncrPhra / cDShootPhraMax * sDShootPhra * sDShootPhra) 		else		tDDensPhra = 0.0 		endif
! 	density_correction_of_production
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDDensProdPhra = aMuPhra / cMuPhraMax * tDDensPhra 		else		tDDensProdPhra = 0.0 		endif
! 	production_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDProdPhra = aMuPhra * sDShootPhra - tDDensProdPhra 		else		tDProdPhra = 0.0 		endif
! 	production_shoot_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDProdShootPhra = afDShootPhra * tDProdPhra 		else		tDProdShootPhra = 0.0 		endif
! 	production_root_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDProdRootPhra = tDProdPhra - tDProdShootPhra 		else		tDProdRootPhra = 0.0 		endif
! 	maintenance_respiration_shoot_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDRespShootPhra = ukDRespTmPhra * sDShootPhra 		else		tDRespShootPhra = 0.0 		endif
! 	maintenance_respiration_root_of_reed
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDRespRootPhra = ukDRespTmPhra * sDRootPhra 		else		tDRespRootPhra = 0.0 		endif
! 	root_O2_respiration
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2RespRootPhra = molO2molC * cCPerDW * tDRespRootPhra * afOxySedM 		else		tO2RespRootPhra = 0.0 		endif
! 	O2_flux_to_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tO2FlowPhra = tO2RespRootPhra 		else		tO2FlowPhra = 0.0 		endif
! 	-
		if (Day < 180.0) then
		bDayRealPhra = 367 		else if (ufDay <= fDayWin .and. bDayRealPhra > 366) then		bDayRealPhra = Day 		else		bDayRealPhra = bDayRealPhra 		endif
! 	shoot_biomass_available_for_reallocation_to_root
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		aDRealPhra = 0.0 		else if (Day < bDayRealPhra) then		aDRealPhra = 0.0 		else if (Day <= bDayRealPhra + 1.0 / kDRealPhra) then		aDRealPhra = fDRealPhra * sDShootPhra 		else		aDRealPhra = 0.0 		endif
! 	reallocation_of_D_per_day_at_end_of_growing_season
		if (0 == InclSeason) then
		tDRealPhra = 0.0 		else if (0 == InclMarsh .or. fMarsh <= NearZero) then		tDRealPhra = 0.0 		else if (Day < bDayRealPhra) then		tDRealPhra = 0.0 		else if (Day <= bDayRealPhra + 1.0 / kDRealPhra) then		tDRealPhra = kDRealPhra * aDRealPhra 		else		tDRealPhra = 0.0 		endif
! 	retranslocation_of_N_end_growing_season
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNRetrPhra = rNDShootPhra * tDRealPhra 		else		tNRetrPhra = 0.0 		endif
! 	retranslocation_of_P_end_growing_season
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPRetrPhra = rPDShootPhra * tDRealPhra 		else		tPRetrPhra = 0.0 		endif
! 	mortality_of_shoots
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMortShootPhra = kDMortShootPhra * sDShootPhra 		else		tDMortShootPhra = 0.0 		endif
! 	mortality_of_shoots
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNMortShootPhra = rNDShootPhra * tDMortShootPhra 		else		tNMortShootPhra = 0.0 		endif
! 	mortality_of_shoots
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPMortShootPhra = rPDShootPhra * tDMortShootPhra 		else		tPMortShootPhra = 0.0 		endif
! 	mortality_of_roots
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMortRootPhra = kDMortRootPhra * sDRootPhra 		else		tDMortRootPhra = 0.0 		endif
! 	mortality_of_roots
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNMortRootPhra = rNDRootPhra * tDMortRootPhra 		else		tNMortRootPhra = 0.0 		endif
! 	mortality_of_roots
		if  (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPMortRootPhra = rPDRootPhra * tDMortRootPhra 		else		tPMortRootPhra = 0.0 		endif
! 	loss_flux_of_biomass_by_management
		if (0 == InclMarsh .or. fMarsh > NearZero) then
		tDManShootPhra = 0.0 		else if ((Day >= cDayManPhra) .and. (Day < cDayManPhra + 1.0)) then		tDManShootPhra = fManPhra * sDShootPhra 		else		tDManShootPhra = 0.0 		endif
! 	loss_flux_of_N_through_management
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNManShootPhra = tDManShootPhra * rNDShootPhra 		else		tNManShootPhra = 0.0 		endif
! 	loss_flux_of_P_through_management
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPManShootPhra = tDManShootPhra * rPDShootPhra 		else		tPManShootPhra = 0.0 		endif
! 	increase_in_inorganic_matter_in_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDIMSM = tDSetIMM 		else		tDIMSM = 0.0 		endif
! 	increase_in_sediment_humus_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDHumSM = fRefrDetS * tDMinDetSM - tDMinHumSM 		else		tDHumSM = 0.0 		endif
! 	increase_in_sediment_detritus_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDDetSM = tDSetDetM - tDMinDetSM + tDSetPhytM + tDMortRootPhra 		else		tDDetSM = 0.0 		endif
! 	turnover_depth_in_marsh
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		vDeltaSM = (tDIMSM / cRhoIM +(tDHumSM + tDDetSM) / cRhoOM)/(1.0 - bPorS) 		else		vDeltaSM = 0.0 		endif
! 	burial_flux_of_DW_in_inorganic_matter_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tDBurIMM = 0.0 		else if (vDeltaSM >= 0.0) then		tDBurIMM = ((tDHumSM + tDDetSM) +(cRhoOM / cRhoIM) * tDIMSM) / ((sDHumSM + sDDet&
		&SM) / sDIMSM + cRhoOM / cRhoIM) 		else		tDBurIMM = ((tDHumSM + tDDetSM) +(cRhoOM / cRhoIM) * tDIMSM) / (fDOrgSoil /(1.0 &
		&- fDOrgSoil) + cRhoOM / cRhoIM) 		endif
! 	burial_flux_of_DW_in_organic_matter_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tDBurOMM = 0.0 		else if (vDeltaSM >= 0.0) then		tDBurOMM = (sDHumSM + sDDetSM) / sDIMSM * tDBurIMM 		else		tDBurOMM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIMM 		endif
! 	burial_flux_of_DW_in_detritus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tDBurDetM = 0.0 		else if (vDeltaSM >= 0.0) then		tDBurDetM = sDDetSM /(sDHumSM + sDDetSM) * tDBurOMM 		else		tDBurDetM = 0.0 		endif
! 	burial_flux_of_DW_in_humus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tDBurHumM = 0.0 		else if (vDeltaSM >= 0.0) then		tDBurHumM = tDBurOMM - tDBurDetM 		else		tDBurHumM = tDBurOMM 		endif
! 	total_DW_burial_flux_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tDBurTotM = 0.0 		else if (vDeltaSM >= 0.0) then		tDBurTotM = tDBurIMM + tDBurOMM 		else		tDBurTotM = tDBurIMM + tDBurOMM 		endif
! 	burial_flux_of_P_in_humus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPBurHumM = 0.0 		else if (vDeltaSM >= 0.0) then		tPBurHumM = rPDHumSM * tDBurHumM 		else		tPBurHumM = cPDSoilOM * tDBurHumM 		endif
! 	burial_flux_of_P_in_detritus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPBurDetM = 0.0 		else if (vDeltaSM >= 0.0) then		tPBurDetM = rPDDetSM * tDBurDetM 		else		tPBurDetM = 0.0 		endif
! 	burial_flux_of_P_absorbed_onto_inorganic_matter_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPBurAIMM = 0.0 		else if (vDeltaSM >= 0.0) then		tPBurAIMM = sPAIMSM / sDIMSM * tDBurIMM 		else		tPBurAIMM = 0.0 		endif
! 	burial_flux_of_dissolved_P_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPBurPO4M = 0.0 		else if (vDeltaSM >= 0.0) then		tPBurPO4M = sPO4SM *(vDeltaSM / cDepthSM) 		else		tPBurPO4M = cPO4Ground *(bPorSM * vDeltaSM) 		endif
! 	total_P_burial_flux_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tPBurTotM = 0.0 		else if (vDeltaSM >= 0.0) then		tPBurTotM = tPBurDetM + tPBurHumM + tPBurAIMM + tPBurPO4M 		else		tPBurTotM = tPBurHumM + tPBurAIMM + tPBurPO4M 		endif
! 	burial_flux_of_N_in_humus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNBurHumM = 0.0 		else if (vDeltaSM >= 0.0) then		tNBurHumM = rNDHumSM * tDBurHumM 		else		tNBurHumM = cNDSoilOM * tDBurHumM 		endif
! 	burial_flux_of_N_in_detritus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNBurDetM = 0.0 		else if (vDeltaSM >= 0.0) then		tNBurDetM = rNDDetSM * tDBurDetM 		else		tNBurDetM = 0.0 		endif
! 	burial_flux_of_dissolved_NH4_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNBurNH4M = 0.0 		else if (vDeltaSM >= 0.0) then		tNBurNH4M = sNH4SM *(vDeltaSM / cDepthSM) 		else		tNBurNH4M = cNH4Ground *(bPorSM * vDeltaSM) 		endif
! 	burial_flux_of_dissolved_NO3_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNBurNO3M = 0.0 		else if (vDeltaSM >= 0.0) then		tNBurNO3M = sNO3SM *(vDeltaSM / cDepthSM) 		else		tNBurNO3M = cNO3Ground *(bPorSM * vDeltaSM) 		endif
! 	total_N_burial_flux_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tNBurTotM = 0.0 		else if (vDeltaSM >= 0.0) then		tNBurTotM = tNBurDetM + tNBurHumM + tNBurNH4M + tNBurNO3M 		else		tNBurTotM = tNBurHumM + tNBurNH4M + tNBurNO3M 		endif
! 	burial_flux_of_Si_in_detritus_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tSiBurDetM = 0.0 		else if (vDeltaSM >= 0.0) then		tSiBurDetM = rSiDDetSM * tDBurDetM 		else		tSiBurDetM = 0.0 		endif
! 	total_Si_burial_flux_in_marsh
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		tSiBurTotM = 0.0 		else if (vDeltaSM >= 0.0) then		tSiBurTotM = tSiBurDetM 		else		tSiBurTotM = 0.0 		endif
! 	marsh_water_depth_change
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		vDeltaWM = 0.0 		else if (ConstDepth == 1) then		vDeltaWM = 0.0 		else		vDeltaWM = - vDeltaSM 		endif
! 	relative_marsh_water_depth_change
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		aRelDeltaWM = vDeltaWM / sDepthWM 		else		aRelDeltaWM = 0.0 		endif
! 	total_settling
		tDSetTot = tDSetIM + tDSetDet + tDSetPhyt
! 	total_settling
		tPSetTot = tPSetAIM + tPSetDet + tPSetPhyt
! 	total_settling
		tNSetTot = tNSetDet + tNSetPhyt
! 	total_resuspension
		tDResusTot = tDResusDead + tDResusPhytTot
! 	total_P_resuspension_flux
		tPResusTot = tPResusDet + tPResusAIM + tPResusPhyt + tPResusPO4
! 	total_N_resuspension_flux
		tNResusTot = tNResusDet + tNResusNH4 + tNResusNO3 + tNResusPhyt
! 	dredging_time(every_nth_year)
		if (sTime >= cDredStart * DaysPerYear) then
		bTimeDred = (floor(TimeYears/cDredInterval) * cDredInterval) * DaysPerYear 		else		bTimeDred = -9999.999 		endif
! 	update_dredget_layer
		if (sTime == bTimeDred) then
		aDepthStart = sDepthW 		else		aDepthStart = aDepthStart 		endif
! 	rate_constant_of_deepening
		if ((sTime >= bTimeDred) .and. (sTime < bTimeDred + cLengDred) .and. (aDepthStar&
		&t <= cDepthRef - cDepthS)) then
		akDredDepth = (log(cDepthRef / aDepthStart)) / cLengDred 		else		akDredDepth = 0.0 		endif
! 	rate_constant_of_dredging(exponential_function)
		if ((sTime >= bTimeDred) .and. (sTime < bTimeDred + cLengDred) .and. (aDepthStar&
		&t <= cDepthRef - cDepthS)) then
		akDred = (- log(1.0 - fEffDred)) / cLengDred 		else		akDred = 0.0 		endif
! 	rate_constant_of_dredging_for_zoobenthos
		if ((sTime >= bTimeDred) .and. (sTime < bTimeDred + cLengDred) .and. (aDepthStar&
		&t <= cDepthRef - cDepthS)) then
		akDredBent = (- log(1.0 - fEffDredBent)) / cLengDred 		else		akDredBent = 0.0 		endif
! 	change_in_water_depth_due_to_dredging
		vDredDepthW = akDredDepth * sDepthW
! 	dredging_flux_of_DW_Detritus_in_lake_sediment
		tDDredDetS = akDred * sDDetS
! 	dredging_flux_of_P_Detritus_in_lake_sediment
		tPDredDetS = akDred * sPDetS
! 	dredging_flux_of_N_Detritus_in_lake_sediment
		tNDredDetS = akDred * sNDetS
! 	dredging_flux_of_Si_Det_in_lake_sediment
		tSiDredDetS = akDred * sSiDetS
! 	dredging_flux_of_P_absorbed_onto_inorganic_matter_in_lake_sediment
		tPDredAIMS = akDred * sPAIMS
! 	average_solid_density_of_soil_material
		bRhoSolidSoil = fDOrgSoil * cRhoOM +(1.0 - fDOrgSoil) * cRhoIM
! 	dredging_flux_of_DW_NetSoil_in_lake_sediment
		tDDredNetSoil = -(tDDredDetS / cRhoOM) * bRhoSolidSoil
! 	dredging_flux_of_DW_NetIMS_in_lake_sediment
		tDDredNetIMS = (1.0 - fDOrgSoil) * tDDredNetSoil
! 	dredging_flux_of_DW_NetHum_in_lake_sediment
		tDDredNetHumS = fDOrgSoil * tDDredNetSoil
! 	dredging_flux_of_P_NetHum_in_lake_sediment
		tPDredNetHumS = cPDSoilOM * tDDredNetHumS
! 	dredging_flux_of_N_NetHum_in_lake_sediment
		tNDredNetHumS = cNDSoilOM * tDDredNetHumS
! 	dredging_flux_of_DW_Diat_on_lake_sediment
		tDDredDiatS = akDred * sDDiatS
! 	dredging_flux_of_P_Diat_on_lake_sediment
		tPDredDiatS = akDred * sPDiatS
! 	dredging_flux_of_N_Diat_on_lake_sediment
		tNDredDiatS = akDred * sNDiatS
! 	dredging_flux_of_DW_Gren_on_lake_sediment
		tDDredGrenS = akDred * sDGrenS
! 	dredging_flux_of_P_Gren_on_lake_sediment
		tPDredGrenS = akDred * sPGrenS
! 	dredging_flux_of_N_Gren_on_lake_sediment
		tNDredGrenS = akDred * sNGrenS
! 	dredging_flux_of_DW_Blue_on_lake_sediment
		tDDredBlueS = akDred * sDBlueS
! 	dredging_flux_of_P_Blue_on_lake_sediment
		tPDredBlueS = akDred * sPBlueS
! 	dredging_flux_of_N_Blue_on_lake_sediment
		tNDredBlueS = akDred * sNBlueS
! 	dredging_flux_of_DW_Phyt_on_lake_sediment
		tDDredPhytS = tDDredDiatS+tDDredGrenS+tDDredBlueS
! 	dredging_flux_of_P_Phyt_on_lake_sediment
		tPDredPhytS = tPDredDiatS+tPDredGrenS+tPDredBlueS
! 	dredging_flux_of_N_Phyt_on_lake_sediment
		tNDredPhytS = tNDredDiatS+tNDredGrenS+tNDredBlueS
! 	dredging_flux_of_DW_Bent_on_lake_sediment
		if (InclWeb == 1) then
		tDDredBent = akDredBent * sDBent 		else		tDDredBent = 0.0 		endif
! 	dredging_flux_of_P_Bent_on_lake_sediment
		if (InclWeb == 1) then
		tPDredBent = akDredBent * sPBent 		else		tPDredBent = 0.0 		endif
! 	dredging_flux_of_N_Bent_on_lake_sediment
		if (InclWeb == 1) then
		tNDredBent = akDredBent * sNBent 		else		tNDredBent = 0.0 		endif
! 	dredging_flux_of_DW_Veg_on_lake_sediment
		tDDredVeg = akDred * sDVeg
! 	dredging_flux_of_P_Veg_on_lake_sediment
		tPDredVeg = akDred * sPVeg
! 	dredging_flux_of_N_Veg_on_lake_sediment
		tNDredVeg = akDred * sNVeg
! 	total_DW_dredging_flux
		tDDredNetTot = tDDredDetS - tDDredNetSoil + tDDredPhytS + tDDredBent + tDDredVeg
! 	total_P_dredging_flux
		tPDredNetTot = tPDredDetS - tPDredNetHumS + tPDredAIMS + tPDredPhytS + tPDredBen&
		&t + tPDredVeg
! 	total_N_dredging_flux
		tNDredNetTot = tNDredDetS - tNDredNetHumS + tNDredPhytS + tNDredBent + tNDredVeg
! 	total_Si_dredging_flux
		tSiDredTot = tSiDredDetS + cSiDDiat * tDDredDiatS
! 	increase_in_inorganic_matter_in_sediment
		tDIMS = tDAbioIMS
! 	increase_in_sediment_humus_in_lake
		tDHumS = tDAbioHumS
! 	increase_in_sediment_detritus_in_lake
		tDDetS = tDAbioDetS + tDPrimDetS + tDWebDetS + tDBedDetS
! 	turnover_depth_in_lake
		vDeltaS = (tDIMS / cRhoIM +(tDHumS + tDDetS) / cRhoOM)/(1.0 - bPorS)
! 	burial_flux_of_DW_in_inorganic_matter_in_lake
		if (vDeltaS >= 0.0) then
		tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / ((sDHumS + sDDetS) / &
		&sDIMS + cRhoOM / cRhoIM) 		else		tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / (fDOrgSoil /(1.0 - fD&
		&OrgSoil) + cRhoOM / cRhoIM) 		endif
! 	burial_flux_of_DW_in_organic_matter_in_lake
		if (vDeltaS >= 0.0) then
		tDBurOM = (sDHumS + sDDetS) / sDIMS * tDBurIM 		else		tDBurOM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIM 		endif
! 	burial_flux_of_DW_in_detritus_in_lake
		if (vDeltaS >= 0.0) then
		tDBurDet = sDDetS /(sDHumS + sDDetS) * tDBurOM 		else		tDBurDet = 0.0 		endif
! 	burial_flux_of_DW_in_humus_in_lake
		if (vDeltaS >= 0.0) then
		tDBurHum = tDBurOM - tDBurDet 		else		tDBurHum = tDBurOM 		endif
! 	total_DW_burial_flux_in_lake
		if (vDeltaS >= 0.0) then
		tDBurTot = tDBurIM + tDBurOM 		else		tDBurTot = tDBurIM + tDBurOM 		endif
! 	burial_flux_of_P_in_humus_in_lake
		if (vDeltaS >= 0.0) then
		tPBurHum = rPDHumS * tDBurHum 		else		tPBurHum = cPDSoilOM * tDBurHum 		endif
! 	burial_flux_of_P_in_detritus_in_lake
		if (vDeltaS >= 0.0) then
		tPBurDet = rPDDetS * tDBurDet 		else		tPBurDet = 0.0 		endif
! 	burial_flux_of_P_absorbed_onto_inorganic_matter_in_lake
		if (vDeltaS >= 0.0) then
		tPBurAIM = sPAIMS / sDIMS * tDBurIM 		else		tPBurAIM = 0.0 		endif
! 	burial_flux_of_dissolved_P_in_lake
		if (vDeltaS >= 0.0) then
		tPBurPO4 = sPO4S *(vDeltaS / cDepthS) 		else		tPBurPO4 = cPO4Ground *(bPorS * vDeltaS) 		endif
! 	total_P_burial_flux_in_lake
		if (vDeltaS >= 0.0) then
		tPBurTot = tPBurDet + tPBurHum + tPBurAIM + tPBurPO4 		else		tPBurTot = tPBurHum + tPBurAIM + tPBurPO4 		endif
! 	burial_flux_of_N_in_humus_in_lake
		if (vDeltaS >= 0.0) then
		tNBurHum = rNDHumS * tDBurHum 		else		tNBurHum = cNDSoilOM * tDBurHum 		endif
! 	burial_flux_of_N_in_detritus_in_lake
		if (vDeltaS >= 0.0) then
		tNBurDet = rNDDetS * tDBurDet 		else		tNBurDet = 0.0 		endif
! 	burial_flux_of_dissolved_NH4_in_lake
		if (vDeltaS >= 0.0) then
		tNBurNH4 = sNH4S *(vDeltaS / cDepthS) 		else		tNBurNH4 = cNH4Ground *(bPorS * vDeltaS) 		endif
! 	burial_flux_of_dissolved_NO3_in_lake
		if (vDeltaS >= 0.0) then
		tNBurNO3 = sNO3S *(vDeltaS / cDepthS) 		else		tNBurNO3 = cNO3Ground *(bPorS * vDeltaS) 		endif
! 	total_N_burial_flux_in_lake
		if (vDeltaS >= 0.0) then
		tNBurTot = tNBurDet + tNBurHum + tNBurNH4 + tNBurNO3 		else		tNBurTot = tNBurHum + tNBurNH4 + tNBurNO3 		endif
! 	burial_flux_of_Si_in_detritus_in_lake
		if (vDeltaS >= 0.0) then
		tSiBurDet = rSiDDetS * tDBurDet 		else		tSiBurDet = 0.0 		endif
! 	total_Si_burial_flux_in_lake
		if (vDeltaS >= 0.0) then
		tSiBurTot = tSiBurDet 		else		tSiBurTot = 0.0 		endif
! 	lake_water_depth_change
		if (ConstDepth == 1) then
		vDeltaW = 0.0 		else		vDeltaW = - vDeltaS 		endif
! 	relative_water_depth_change_due_to_sediment_turnover_AND_dredging
		aRelDeltaW = (vDeltaW + vDredDepthW) / sDepthW
! 	Mass_balance_totals_of_DW_marsh_water_and_vegetation_module
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tDMarsTotT = (- tDBurTotM - wDMinDetWM * sDepthWM -(1.0 - fRefrDetS) * tDMinDetS&
		&M - tDMinHumSM + tDProdPhra - tDRespShootPhra - tDRespRootPhra - tDManShootPhra)&
		& * fMarsh 		else		tDMarsTotT = 0.0 		endif
! 	Mass_balance_totals_of_P_marsh_water_and_vegetation_module
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tPMarsTotT = (- tPInfPO4SM - tPDifGroundPO4M - tPBurTotM - tPChemPO4M - tPManSho&
		&otPhra) * fMarsh 		else		tPMarsTotT = 0.0 		endif
! 	Mass_balance_totals_of_N_marsh_water_and_vegetation_module
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tNMarsTotT = (- tNInfNH4SM - tNInfNO3SM - tNDifGroundNO3M - tNDifGroundNH4M - tN&
		&BurTotM - wNDenitWM * sDepthWM - tNDenitSM - tNManShootPhra) * fMarsh 		else		tNMarsTotT = 0.0 		endif
! 	Mass_balance_totals_of_SI_marsh_water_and_vegetation_module
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		tSiMarsTotT = - tSiBurDetM * fMarsh 		else		tSiMarsTotT = 0.0 		endif
! 	total_DW_in_system
		aDTotT = (sDIMW+sDDetW+sDDiatW+sDGrenW+sDBlueW+sDZoo) * sDepthW +aDFish+sDPisc+a&
		&DVeg +sDIMS+sDHumS+sDDetS+sDDiatS+sDGrenS+sDBlueS+sDBent + aDTotM
! 	total_N_in_system
		aNTotT = (sNH4W+sNO3W+sNDetW+sNDiatW+sNGrenW+sNBlueW +sNZoo)*sDepthW +aNFish+aNP&
		&isc+aNVeg +sNH4S+sNO3S+sNDetS+sNHumS+sNDiatS+sNGrenS+sNBlueS +sNBent + aNTotM
! 	total_P_in_system
		aPTotT = (sPO4W+sPDetW+sPDiatW+sPGrenW+sPBlueW +sPZoo+sPAIMW) * sDepthW +aPFish+&
		&aPPisc+aPVeg +sPO4S+sPDetS+sPHumS+sPDiatS+sPGrenS+sPBlueS +sPAIMS+sPBent + aPTot&
		&M
! 	total_Si_in_system
		aSiTotT = (sSiO2W + sSiDetW + cSiDDiat*sDDiatW) *sDepthW + sSiDetS + cSiDDiat*sD&
		&DiatS + aSiTotM
! 	DW_mass_balance_error
		aDError = aDTotT - sDExtTotT
! 	N_mass_balance_error
		aNError = aNTotT - sNExtTotT
! 	P_mass_balance_error
		aPError = aPTotT - sPExtTotT
! 	Si_mass_balance_error
		aSiError = aSiTotT - sSiExtTotT
! 	derivative_for_N_ammonium_in_water_in_lake_water
		dNH4W = wNTranNH4W + wNAbioNH4W + wNPrimNH4W + wNBedNH4W + wNWebNH4W + cNBackLoa&
		&d / sDepthW - aRelDeltaW * sNH4W - wNExchNH4
! 	derivative_for_N_nitrate_in_water_in_lake_water
		dNO3W = wNTranNO3W + wNAbioNO3W + wNPrimNO3W + wNBedNO3W + wNWebNO3W - aRelDelta&
		&W * sNO3W - wNExchNO3
! 	derivative_for_P_SRP_in_water_in_lake_water
		dPO4W = wPTranPO4W + wPAbioPO4W + wPPrimPO4W + wPBedPO4W + wPWebPO4W + cPBackLoa&
		&d / sDepthW - aRelDeltaW * sPO4W - wPExchPO4
! 	derivative_for_P_P-adsorbed_onto_IM_in_water_in_lake_water
		dPAIMW = wPTranAIMW + wPAbioAIMW - aRelDeltaW * sPAIMW - wPExchAIM
! 	derivative_for_Si_dissolved_silica_in_water_in_lake_water
		dSiO2W = wSiTranSiO2 + wSiAbioSiO2W + wSiPrimSiO2W - aRelDeltaW * sSiO2W - wSiEx&
		&chSiO2
! 	derivative_for_O2_oxygen_in_water_in_lake_water
		dO2W = wO2TranW + wO2AbioW + wO2PrimW + tO2BedW / sDepthW - aRelDeltaW * sO2W - &
		&wO2Exch
! 	derivative_for_DW_Detritus_in_lake_water
		dDDetW = wDTranDetW + wDAbioDetW + wDPrimDetW + wDBedDetW + wDWebDetW - aRelDelt&
		&aW * sDDetW - wDExchDet
! 	derivative_for_N_Detritus_in_lake_water
		dNDetW = wNTranDetW + wNAbioDetW + wNPrimDetW + wNBedDetW + wNWebDetW - aRelDelt&
		&aW * sNDetW - wNExchDet
! 	derivative_for_P_Detritus_in_lake_water
		dPDetW = wPTranDetW + wPAbioDetW + wPPrimDetW + wPBedDetW + wPWebDetW - aRelDelt&
		&aW * sPDetW - wPExchDet
! 	derivative_for_Si_Detritus_in_lake_water
		dSiDetW = wSiTranDetW + wSiAbioDetW + wSiPrimDetW + wSiWebDetW - aRelDeltaW * sS&
		&iDetW - wSiExchDet
! 	derivative_for_DW_inorg_matter_in_water_in_lake_water
		dDIMW = wDTranIMW + wDAbioIMW - aRelDeltaW * sDIMW - wDExchIM
! 	derivative_for_DW_Diatoms_in_lake_water
		dDDiatW = wDTranDiat + wDPrimDiatW + wDWebDiatW - aRelDeltaW * sDDiatW - wDExchD&
		&iat
! 	derivative_for_N_Diatoms_in_lake_water
		dNDiatW = wNTranDiat + wNPrimDiatW + wNWebDiatW - aRelDeltaW * sNDiatW - wNExchD&
		&iat
! 	derivative_for_P_Diatoms_in_lake_water
		dPDiatW = wPTranDiat + wPPrimDiatW + wPWebDiatW - aRelDeltaW * sPDiatW - wPExchD&
		&iat
! 	derivative_for_DW_Greens_in_lake_water
		dDGrenW = wDTranGren + wDPrimGrenW + wDWebGrenW - aRelDeltaW * sDGrenW - wDExchG&
		&ren
! 	derivative_for_N_Greens_in_lake_water
		dNGrenW = wNTranGren + wNPrimGrenW + wNWebGrenW - aRelDeltaW * sNGrenW - wNExchG&
		&ren
! 	derivative_for_P_Greens_in_lake_water
		dPGrenW = wPTranGren + wPPrimGrenW + wPWebGrenW - aRelDeltaW * sPGrenW - wPExchG&
		&ren
! 	derivative_for_DW_Bluegreens_in_lake_water
		dDBlueW = wDTranBlue + wDPrimBlueW + wDWebBlueW - aRelDeltaW * sDBlueW - wDExchB&
		&lue
! 	derivative_for_N_Bluegreens_in_lake_water
		dNBlueW = wNTranBlue + wNPrimBlueW + wNWebBlueW - aRelDeltaW * sNBlueW - wNExchB&
		&lue
! 	derivative_for_P_Bluegreens_in_lake_water
		dPBlueW = wPTranBlue + wPPrimBlueW + wPWebBlueW - aRelDeltaW * sPBlueW - wPExchB&
		&lue
! 	derivative_for_DW_Zooplankton_in_lake_water
		dDZoo = wDTranZoo + wDWebZoo - aRelDeltaW * sDZoo - wDExchZoo
! 	derivative_for_N_Zooplankton_in_lake_water
		dNZoo = wNTranZoo + wNWebZoo - aRelDeltaW * sNZoo - wNExchZoo
! 	derivative_for_P_Zooplankton_in_lake_water
		dPZoo = wPTranZoo + wPWebZoo - aRelDeltaW * sPZoo - wPExchZoo
! 	derivative_for_DW_Adult_whitefish_in_lake_water
		dDFiAd = tDWebFiAd
! 	derivative_for_DW_Juvenile_whitefish_in_lake_water
		dDFiJv = tDWebFiJv
! 	derivative_for_N_Adult_whitefish_in_lake_water
		dNFiAd = tNWebFiAd
! 	derivative_for_N_Juvenile_whitefish_in_lake_water
		dNFiJv = tNWebFiJv
! 	derivative_for_P_Adult_whitefish_in_lake_water
		dPFiAd = tPWebFiAd
! 	derivative_for_P_Juvenile_whitefish_in_lake_water
		dPFiJv = tPWebFiJv
! 	derivative_for_DW_predatory_fish_in_lake_water
		dDPisc = tDWebPisc
! 	derivative_for_N_Pore_water_ammonium_in_lake_water
		dNH4S = tNAbioNH4S - tNBurNH4 + tNPrimNH4S + tNBedNH4S + tNWebNH4S
! 	derivative_for_N_Pore_water_nitrate_in_lake_water
		dNO3S = tNAbioNO3S - tNBurNO3 + tNPrimNO3S + tNBedNO3S + tNWebNO3S
! 	derivative_for_P_Pore_water_SRP_in_lake_water
		dPO4S = tPAbioPO4S - tPBurPO4 + tPPrimPO4S + tPBedPO4S + tPWebPO4S
! 	derivative_for_P_P-adsorbed_onto_IM_in_sediment_in_lake_sediment
		dPAIMS = tPAbioAIMS - tPBurAIM - tPDredAIMS
! 	derivative_for_DW_Sediment_detritus_in_lake_sediment
		dDDetS = tDAbioDetS - tDBurDet + tDPrimDetS + tDBedDetS + tDWebDetS - tDDredDetS
! 	derivative_for_N_Sediment_detritus_N_in_lake_sediment
		dNDetS = tNAbioDetS - tNBurDet + tNPrimDetS + tNBedDetS + tNWebDetS - tNDredDetS
! 	derivative_for_P_Sediment_detritus_P_in_lake_sediment
		dPDetS = tPAbioDetS - tPBurDet + tPPrimDetS + tPBedDetS + tPWebDetS - tPDredDetS
! 	derivative_for_Si_Sediment_detritus_Si_in_lake_sediment
		dSiDetS = tSiAbioDetS - tSiBurDet + tSiPrimDetS + tSiWebDetS - tSiDredDetS
! 	derivative_for_DW_humus_in_lake_sediment
		dDHumS = tDAbioHumS - tDBurHum - tDDredNetHumS
! 	derivative_for_N_humus_in_lake_sediment
		dNHumS = tNAbioHumS - tNBurHum - tNDredNetHumS
! 	derivative_for_P_humus_in_lake_sediment
		dPHumS = tPAbioHumS - tPBurHum - tPDredNetHumS
! 	derivative_for_DW_inorg_matter_in_sediment_in_lake_sediment
		dDIMS = tDAbioIMS - tDBurIM - tDDredNetIMS
! 	derivative_for_DW_Sed_Diatoms_in_lake_sediment
		dDDiatS = tDPrimDiatS + tDWebDiatS - tDDredDiatS
! 	derivative_for_N_Sediment_diatoms_in_lake_sediment
		dNDiatS = tNPrimDiatS + tNWebDiatS - tNDredDiatS
! 	derivative_for_P_Sediment_diatoms_in_lake_sediment
		dPDiatS = tPPrimDiatS + tPWebDiatS - tPDredDiatS
! 	derivative_for_DW_Sed_Greens_in_lake_sediment
		dDGrenS = tDPrimGrenS + tDWebGrenS - tDDredGrenS
! 	derivative_for_N_Sediment_green_algae_in_lake_sediment
		dNGrenS = tNPrimGrenS + tNWebGrenS - tPDredGrenS
! 	derivative_for_P_Sediment_green_algae_in_lake_sediment
		dPGrenS = tPPrimGrenS + tPWebGrenS - tPDredGrenS
! 	derivative_for_DW_Sed_Blue-greens_in_lake_sediment
		dDBlueS = tDPrimBlueS + tDWebBlueS - tDDredBlueS
! 	derivative_for_N_Sediment_blue-greens_in_lake_sediment
		dNBlueS = tNPrimBlueS + tNWebBlueS - tNDredBlueS
! 	derivative_for_P_Sediment_blue-greens_in_lake_sediment
		dPBlueS = tPPrimBlueS + tPWebBlueS - tPDredBlueS
! 	derivative_for_DW_Vegetation_in_lake_sediment
		dDVeg = tDBedVeg - tDDredVeg
! 	derivative_for_N_Vegetation_in_lake_sediment
		dNVeg = tNBedVeg - tNDredVeg
! 	derivative_for_P_Vegetation_in_lake_sediment
		dPVeg = tPBedVeg - tPDredVeg
! 	derivative_for_DW_Zoobenthos_in_lake_sediment
		dDBent = tDWebBent - tDDredBent
! 	derivative_for_N_Zoobenthos_in_lake_sediment
		dNBent = tNWebBent - tNDredBent
! 	derivative_for_P_Zoobenthos_in_lake_sediment
		dPBent = tPWebBent - tPDredBent
! 	derivative_for_water_depth_change_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDepthWM = vTranDepthW + vDeltaWM 		else		dDepthWM = 0.0 		endif
! 	derivative_for_N_NH4_in_water_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNH4WM = tNDifNH4M/sDepthWM - wNNitrWM + wNMinDetWM - tNEvNH4WM/sDepthWM - tNInf&
		&NH4WM/sDepthWM + wNExchNH4M - aRelDeltaWM * sNH4WM 		else		dNH4WM = 0.0 		endif
! 	derivative_for_N_NO3_in_water_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNO3WM = tNDifNO3M/sDepthWM + wNNitrWM - wNDenitWM - tNEvNO3WM/sDepthWM - tNInfN&
		&O3WM/sDepthWM + wNExchNO3M - aRelDeltaWM * sNO3WM 		else		dNO3WM = 0.0 		endif
! 	derivative_for_P_PO4_in_water_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPO4WM = - tPInfPO4WM / sDepthWM + tPDifPO4M / sDepthWM + wPMinDetWM - tPEvPO4WM&
		& / sDepthWM - wPSorpIMWM + wPExchPO4M - aRelDeltaWM * sPO4WM 		else		dPO4WM = 0.0 		endif
! 	derivative_for_P_P_adsorbed_onto_IM_in_water_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPAIMWM = - tPSetAIMM / sDepthWM + wPSorpIMWM + wPExchAIMM - aRelDeltaWM * sPAIM&
		&WM 		else		dPAIMWM = 0.0 		endif
! 	derivative_for_Si_SiO2_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dSiO2WM = wSiMinDetWM + tSiMinDetSM / sDepthWM + wSiExchSiO2M - aRelDeltaWM * sS&
		&iO2WM 		else		dSiO2WM = 0.0 		endif
! 	derivative_for_O2_O2_in_water_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dO2WM = tO2AerM / sDepthWM - wO2MinDetWM - wO2NitrWM -(tO2MinDetSM + tO2NitrSM) &
		&/ sDepthWM + wO2ExchM - aRelDeltaWM * sO2WM 		else		dO2WM = 0.0 		endif
! 	derivative_for_DW_Detritus_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDDetWM = tDMortShootPhra/sDepthWM - tDSetDetM/sDepthWM - wDMinDetWM + wDExchDet&
		&M - aRelDeltaWM * sDDetWM 		else		dDDetWM = 0.0 		endif
! 	derivative_for_N_detritus_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNDetWM = tNMortShootPhra / sDepthWM - tNSetDetM / sDepthWM - wNMinDetWM + wNExc&
		&hDetM - aRelDeltaWM * sNDetWM 		else		dNDetWM = 0.0 		endif
! 	derivative_for_P_detritus_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPDetWM = tPMortShootPhra / sDepthWM - tPSetDetM / sDepthWM - wPMinDetWM + wPExc&
		&hDetM - aRelDeltaWM * sPDetWM 		else		dPDetWM = 0.0 		endif
! 	derivative_for_Si_detritus_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dSiDetWM = - tSiSetDetM / sDepthWM - wSiMinDetWM + wSiExchDetM - aRelDeltaWM * s&
		&SiDetWM 		else		dSiDetWM = 0.0 		endif
! 	derivative_for_DW_Inorg_matter_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDIMWM = - tDSetIMM/sDepthWM + wDExchIMM - aRelDeltaWM * sDIMWM 		else		dDIMWM = 0.0 		endif
! 	derivative_for_DW_diatoms_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDDiatWM = wDExchDiatM - tDSetDiatM / sDepthWM - aRelDeltaWM * sDDiatWM 		else		dDDiatWM = 0.0 		endif
! 	derivative_for_N_diatoms_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNDiatWM = wNExchDiatM - tNSetDiatM / sDepthWM - aRelDeltaWM * sNDiatWM 		else		dNDiatWM = 0.0 		endif
! 	derivative_for_P_diatoms_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPDiatWM = wPExchDiatM - tPSetDiatM / sDepthWM - aRelDeltaWM * sPDiatWM 		else		dPDiatWM = 0.0 		endif
! 	derivative_for_DW_greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDGrenWM = wDExchGrenM - tDSetGrenM / sDepthWM - aRelDeltaWM * sDGrenWM 		else		dDGrenWM = 0.0 		endif
! 	derivative_for_N_greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNGrenWM = wNExchGrenM - tNSetGrenM / sDepthWM - aRelDeltaWM * sNGrenWM 		else		dNGrenWM = 0.0 		endif
! 	derivative_for_P_greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPGrenWM = wPExchGrenM - tPSetGrenM / sDepthWM - aRelDeltaWM * sPGrenWM 		else		dPGrenWM = 0.0 		endif
! 	derivative_for_DW_blue-greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDBlueWM = wDExchBlueM - tDSetBlueM / sDepthWM - aRelDeltaWM * sDBlueWM 		else		dDBlueWM = 0.0 		endif
! 	derivative_for_N_blue-greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNBlueWM = wNExchBlueM - tNSetBlueM / sDepthWM - aRelDeltaWM * sNBlueWM 		else		dNBlueWM = 0.0 		endif
! 	derivative_for_P_blue-greens_in_marsh_water
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPBlueWM = wPExchBlueM - tPSetBlueM / sDepthWM - aRelDeltaWM * sPBlueWM 		else		dPBlueWM = 0.0 		endif
! 	derivative_for_DW_zooplankton_in_marsh_water
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		dDZooM = 0.0 		else if (InclWeb == 1) then		dDZooM = wDExchZooM - aRelDeltaWM * sDZooM 		else		dDZooM = 0.0 		endif
! 	derivative_for_N_zooplankton_in_marsh_water
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		dNZooM = 0.0 		else if (InclWeb == 1) then		dNZooM = wNExchZooM - aRelDeltaWM * sNZooM 		else		dNZooM = 0.0 		endif
! 	derivative_for_P_zooplankton_in_marsh_water
		if (0 == InclMarsh .or. fMarsh <= NearZero) then
		dPZooM = 0.0 		else if (InclWeb == 1) then		dPZooM = wPExchZooM - aRelDeltaWM * sPZooM 		else		dPZooM = 0.0 		endif
! 	derivative_for_N_NH4_in_water_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNH4SM = tNInfNH4WM - tNInfNH4SM +(1.0-fRefrDetS) * tNMinDetSM + tNMinHumSM - tN&
		&DifNH4M - tNDifGroundNH4M - tNNitrSM - tNBurNH4M - tNUptNH4PhraS + tNEvNH4WM 		else		dNH4SM = 0.0 		endif
! 	derivative_for_N_NO3_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNO3SM = tNInfNO3WM - tNInfNO3SM + tNNitrSM - tNDenitSM - tNDifNO3M - tNDifGroun&
		&dNO3M - tNBurNO3M - tNUptNO3PhraS + tNEvNO3WM 		else		dNO3SM = 0.0 		endif
! 	derivative_for_P_PO4_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPO4SM = tPInfPO4WM - tPInfPO4SM + tPEvPO4WM +(1.0-fRefrDetS) * tPMinDetSM + tPM&
		&inHumSM - tPSorpIMSM - tPDifPO4M - tPDifGroundPO4M - tPChemPO4M - tPUptPhraS - t&
		&PBurPO4M 		else		dPO4SM = 0.0 		endif
! 	derivative_for_P_P_adsorbed_onto_IM_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPAIMSM = tPSetAIMM - tPBurAIMM + tPSorpIMSM 		else		dPAIMSM = 0.0 		endif
! 	derivative_for_DW_Detritus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDDetSM = tDMortRootPhra + tDSetDetM - tDMinDetSM + tDSetPhytM - tDBurDetM 		else		dDDetSM = 0.0 		endif
! 	derivative_for_N_detritus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNDetSM = tNMortRootPhra + tNSetDetM - tNMinDetSM + tNSetPhytM - tNBurDetM 		else		dNDetSM = 0.0 		endif
! 	derivative_for_P_detritus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPDetSM = tPMortRootPhra + tPSetDetM - tPMinDetSM + tPSetPhytM - tPBurDetM 		else		dPDetSM = 0.0 		endif
! 	derivative_for_P_detritus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dSiDetSM = tSiSetDetM - tSiMinDetSM + cSiDDiat * tDSetDiatM - tSiBurDetM 		else		dSiDetSM = 0.0 		endif
! 	derivative_for_DW_sediment_humus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDHumSM = fRefrDetS * tDMinDetSM - tDMinHumSM - tDBurHumM 		else		dDHumSM = 0.0 		endif
! 	derivative_for_N_sediment_humus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNHumSM = fRefrDetS * tNMinDetSM - tNMinHumSM - tNBurHumM 		else		dNHumSM = 0.0 		endif
! 	derivative_for_P_sediment_humus_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPHumSM = fRefrDetS * tPMinDetSM - tPMinHumSM - tPBurHumM 		else		dPHumSM = 0.0 		endif
! 	derivative_for_DW_Inorg_matter_in_sediment_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDIMSM = tDSetIMM - tDBurIMM 		else		dDIMSM = 0.0 		endif
! 	derivative_for_DW_biomass_root_reed_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDRootPhra = tDProdRootPhra - tDRespRootPhra - tDMortRootPhra - tDAllPhra + tDRe&
		&alPhra 		else		dDRootPhra = 0.0 		endif
! 	derivative_for_DW_biomass_shoot_reed_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dDShootPhra = tDProdShootPhra - tDRespShootPhra - tDMortShootPhra + tDAllPhra - &
		&tDRealPhra - tDManShootPhra 		else		dDShootPhra = 0.0 		endif
! 	derivative_for_N_N_in_root_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNRootPhra = tNUptRootPhra - tNMortRootPhra - tNTransPhra + tNRetrPhra 		else		dNRootPhra = 0.0 		endif
! 	derivative_for_N_N_in_shoot_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dNShootPhra = tNUptShootPhra - tNMortShootPhra + tNTransPhra - tNRetrPhra - tNMa&
		&nShootPhra 		else		dNShootPhra = 0.0 		endif
! 	derivative_for_P_P_in_root_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPRootPhra = tPUptRootPhra - tPMortRootPhra - tPTransPhra + tPRetrPhra 		else		dPRootPhra = 0.0 		endif
! 	derivative_for_P_P_in_shoot_in_marsh_sediment
		if (InclMarsh == 1 .and. fMarsh > NearZero) then
		dPShootPhra = tPUptShootPhra - tPMortShootPhra + tPTransPhra - tPRetrPhra - tPMa&
		&nShootPhra 		else		dPShootPhra = 0.0 		endif
! 	derivative_for_total_external_DW_flux_
		dDExtTotT = uDLoad - wDOutflTot*sDepthW + wDTranZoo * sDepthW - tDBurTot + tDAbi&
		&oTotT + tDPrimTotT + tDBedTotT + tDWebTotT + tDMarsTotT - tDDredNetTot
! 	derivative_for_total_external_N_flux_
		dNExtTotT = uNLoad - wNOutflTot * sDepthW + wNTranZoo * sDepthW + cNBackLoad - t&
		&NBurTot + tNAbioTotT + tNPrimTotT + tNBedTotT + tNWebTotT + tNMarsTotT - tNDredN&
		&etTot
! 	derivative_for_total_external_P_flux_
		dPExtTotT = uPLoad - wPOutflTot * sDepthW + wPTranZoo * sDepthW + cPBackLoad - t&
		&PBurTot + tPAbioTotT + tPPrimTotT + tPBedTotT + tPWebTotT + tPMarsTotT - tPDredN&
		&etTot
! 	derivative_for_total_external_Si_flux_
		dSiExtTotT = uSiLoad - wSiDilTot*sDepthW + tSiAbioTotT - tSiBurTot + tSiPrimTotT&
		& + tSiMarsTotT - tSiDredTot
dDFiAd=dDFiAd/sDepthW
dDFiJv=dDFiJv/sDepthW
dNFiAd=dNFiAd/sDepthW
dNFiJv=dNFiJv/sDepthW
dPFiAd=dPFiAd/sDepthW
dPFiJv=dPFiJv/sDepthW
dDPisc=dDPisc/sDepthW
dNH4S=dNH4S/sDepthW
dNO3S=dNO3S/sDepthW
dPO4S=dPO4S/sDepthW
dPAIMS=dPAIMS/sDepthW
dDDetS=dDDetS/sDepthW
dNDetS=dNDetS/sDepthW
dPDetS=dPDetS/sDepthW
dSiDetS=dSiDetS/sDepthW
dDHumS=dDHumS/sDepthW
dNHumS=dNHumS/sDepthW
dPHumS=dPHumS/sDepthW
dDIMS=dDIMS/sDepthW
dDDiatS=dDDiatS/sDepthW
dNDiatS=dNDiatS/sDepthW
dPDiatS=dPDiatS/sDepthW
dDGrenS=dDGrenS/sDepthW
dNGrenS=dNGrenS/sDepthW
dPGrenS=dPGrenS/sDepthW
dDBlueS=dDBlueS/sDepthW
dNBlueS=dNBlueS/sDepthW
dPBlueS=dPBlueS/sDepthW
dDVeg=dDVeg/sDepthW
dNVeg=dNVeg/sDepthW
dPVeg=dPVeg/sDepthW
dDBent=dDBent/sDepthW
dNBent=dNBent/sDepthW
dPBent=dPBent/sDepthW
dDepthWM=dDepthWM/sDepthW
dNH4WM=dNH4WM/sDepthW
dNO3WM=dNO3WM/sDepthW
dPO4WM=dPO4WM/sDepthW
dPAIMWM=dPAIMWM/sDepthW
dSiO2WM=dSiO2WM/sDepthW
dO2WM=dO2WM/sDepthW
dDDetWM=dDDetWM/sDepthW
dNDetWM=dNDetWM/sDepthW
dPDetWM=dPDetWM/sDepthW
dSiDetWM=dSiDetWM/sDepthW
dDIMWM=dDIMWM/sDepthW
dDDiatWM=dDDiatWM/sDepthW
dNDiatWM=dNDiatWM/sDepthW
dPDiatWM=dPDiatWM/sDepthW
dDGrenWM=dDGrenWM/sDepthW
dNGrenWM=dNGrenWM/sDepthW
dPGrenWM=dPGrenWM/sDepthW
dDBlueWM=dDBlueWM/sDepthW
dNBlueWM=dNBlueWM/sDepthW
dPBlueWM=dPBlueWM/sDepthW
dDZooM=dDZooM/sDepthW
dNZooM=dNZooM/sDepthW
dPZooM=dPZooM/sDepthW
dNH4SM=dNH4SM/sDepthW
dNO3SM=dNO3SM/sDepthW
dPO4SM=dPO4SM/sDepthW
dPAIMSM=dPAIMSM/sDepthW
dDDetSM=dDDetSM/sDepthW
dNDetSM=dNDetSM/sDepthW
dPDetSM=dPDetSM/sDepthW
dSiDetSM=dSiDetSM/sDepthW
dDHumSM=dDHumSM/sDepthW
dNHumSM=dNHumSM/sDepthW
dPHumSM=dPHumSM/sDepthW
dDIMSM=dDIMSM/sDepthW
dDRootPhra=dDRootPhra/sDepthW
dDShootPhra=dDShootPhra/sDepthW
dNRootPhra=dNRootPhra/sDepthW
dNShootPhra=dNShootPhra/sDepthW
dPRootPhra=dPRootPhra/sDepthW
dPShootPhra=dPShootPhra/sDepthW
dDExtTotT=dDExtTotT/sDepthW
dNExtTotT=dNExtTotT/sDepthW
dPExtTotT=dPExtTotT/sDepthW
dSiExtTotT=dSiExtTotT/sDepthW
if (counter1 == 0) then
open (unit=1,file="initrep.txt",action="write",status="replace")
write (1,*)"Type Id Name Set0 Set1 Set2 Set3 Rel0-1 Rel0-2 Rel0-3 Dif0-1 Dif0-2 Dif0-3"
write (1,*)" Parameter  0 _InitCalc_", InitCalc
write (1,*)" Parameter  1 _ConstDepth_", ConstDepth
write (1,*)" Parameter  2 _InclTran_", InclTran
write (1,*)" Parameter  3 _InclPhytS_", InclPhytS
write (1,*)" Parameter  4 _InclBed_", InclBed
write (1,*)" Parameter  5 _InclWeb_", InclWeb
write (1,*)" Parameter  6 _InclMarsh_", InclMarsh
write (1,*)" Parameter  7 _InclSeason_", InclSeason
write (1,*)" Parameter  8 _ReadTemp_", ReadTemp
write (1,*)" Parameter  9 _ReadLOut_", ReadLOut
write (1,*)" Parameter  10 _ReadVWind_", ReadVWind
write (1,*)" Parameter  11 _ReadQIn_", ReadQIn
write (1,*)" Parameter  12 _ReadQOut_", ReadQOut
write (1,*)" Parameter  13 _ReadQEv_", ReadQEv
write (1,*)" Parameter  14 _ReadPLoad_", ReadPLoad
write (1,*)" Parameter  15 _ReadNLoad_", ReadNLoad
write (1,*)" Parameter  16 _ReadNutFrac_", ReadNutFrac
write (1,*)" Parameter  17 _ReadPLoadPhyt_", ReadPLoadPhyt
write (1,*)" Parameter  18 _ReadDLoadDet_", ReadDLoadDet
write (1,*)" Parameter  19 _ReadDLoadIM_", ReadDLoadIM
write (1,*)" Parameter  20 _UseSeasonLoad_", UseSeasonLoad
write (1,*)" Parameter  21 _UsePulseLoad_", UsePulseLoad
write (1,*)" Parameter  22 _mTemp_", mTemp
write (1,*)" Parameter  23 _mLOut_", mLOut
write (1,*)" Parameter  24 _mVWind_", mVWind
write (1,*)" Parameter  25 _mQIn_", mQIn
write (1,*)" Parameter  26 _mQOut_", mQOut
write (1,*)" Parameter  27 _mQEv_", mQEv
write (1,*)" Parameter  28 _mPLoad_", mPLoad
write (1,*)" Parameter  29 _mPLoadPO4_", mPLoadPO4
write (1,*)" Parameter  30 _mPLoadOrg_", mPLoadOrg
write (1,*)" Parameter  31 _mPLoadPhytTot_", mPLoadPhytTot
write (1,*)" Parameter  32 _mNLoad_", mNLoad
write (1,*)" Parameter  33 _mNLoadNH4_", mNLoadNH4
write (1,*)" Parameter  34 _mNLoadNO3_", mNLoadNO3
write (1,*)" Parameter  35 _mNLoadOrg_", mNLoadOrg
write (1,*)" Parameter  36 _mDLoadDet_", mDLoadDet
write (1,*)" Parameter  37 _mDLoadIM_", mDLoadIM
write (1,*)" Parameter  38 _BeginTime_", BeginTime
write (1,*)" Parameter  39 _EndTime_", EndTime
write (1,*)" Parameter  40 _YearZero_", YearZero
write (1,*)" Parameter  41 _cFetch_", cFetch
write (1,*)" Parameter  42 _fMarsh_", fMarsh
write (1,*)" Parameter  43 _fLutum_", fLutum
write (1,*)" Parameter  44 _fFeDIM_", fFeDIM
write (1,*)" Parameter  45 _fAlDIM_", fAlDIM
write (1,*)" Parameter  46 _cTmAve_", cTmAve
write (1,*)" Parameter  47 _cTmVar_", cTmVar
write (1,*)" Parameter  48 _cTimeLag_", cTimeLag
write (1,*)" Parameter  49 _cVWind_", cVWind
write (1,*)" Parameter  50 _cQInf_", cQInf
write (1,*)" Parameter  51 _cPBackLoad_", cPBackLoad
write (1,*)" Parameter  52 _cNBackLoad_", cNBackLoad
write (1,*)" Parameter  53 _cLDayAve_", cLDayAve
write (1,*)" Parameter  54 _cLDayVar_", cLDayVar
write (1,*)" Parameter  55 _cfDayAve_", cfDayAve
write (1,*)" Parameter  56 _cfDayVar_", cfDayVar
write (1,*)" Parameter  57 _fRefl_", fRefl
write (1,*)" Parameter  58 _cExtWat_", cExtWat
write (1,*)" Parameter  59 _cDredInterval_", cDredInterval
write (1,*)" Parameter  60 _cDredStart_", cDredStart
write (1,*)" Parameter  61 _cDepthRef_", cDepthRef
write (1,*)" Parameter  62 _cLengDred_", cLengDred
write (1,*)" Parameter  63 _fEffDred_", fEffDred
write (1,*)" Parameter  64 _fEffDredBent_", fEffDredBent
write (1,*)" Parameter  65 _fPAR_", fPAR
write (1,*)" Parameter  66 _cExtSpDet_", cExtSpDet
write (1,*)" Parameter  67 _cExtSpIM_", cExtSpIM
write (1,*)" Parameter  68 _fDTotS0_", fDTotS0
write (1,*)" Parameter  69 _fDOrgS0_", fDOrgS0
write (1,*)" Parameter  70 _fDDetS0_", fDDetS0
write (1,*)" Parameter  71 _fSedPhyt0_", fSedPhyt0
write (1,*)" Parameter  72 _fPInorgS0_", fPInorgS0
write (1,*)" Parameter  73 _fPAdsS0_", fPAdsS0
write (1,*)" Parameter  74 _cPDDet0_", cPDDet0
write (1,*)" Parameter  75 _cNDDet0_", cNDDet0
write (1,*)" Parameter  76 _cSiDDet0_", cSiDDet0
write (1,*)" Parameter  77 _cPDHum0_", cPDHum0
write (1,*)" Parameter  78 _cNDHum0_", cNDHum0
write (1,*)" Parameter  79 _cPDPhyt0_", cPDPhyt0
write (1,*)" Parameter  80 _cNDPhyt0_", cNDPhyt0
write (1,*)" Parameter  81 _cPDDiat0_", cPDDiat0
write (1,*)" Parameter  82 _cNDDiat0_", cNDDiat0
write (1,*)" Parameter  83 _cPDGren0_", cPDGren0
write (1,*)" Parameter  84 _cNDGren0_", cNDGren0
write (1,*)" Parameter  85 _cPDBlue0_", cPDBlue0
write (1,*)" Parameter  86 _cNDBlue0_", cNDBlue0
write (1,*)" Parameter  87 _cPDVeg0_", cPDVeg0
write (1,*)" Parameter  88 _cNDVeg0_", cNDVeg0
write (1,*)" Parameter  89 _cSiDDiat_", cSiDDiat
write (1,*)" Parameter  90 _cPDZooRef_", cPDZooRef
write (1,*)" Parameter  91 _cNDZooRef_", cNDZooRef
write (1,*)" Parameter  92 _cPDBentRef_", cPDBentRef
write (1,*)" Parameter  93 _cNDBentRef_", cNDBentRef
write (1,*)" Parameter  94 _cPDFishRef_", cPDFishRef
write (1,*)" Parameter  95 _cNDFishRef_", cNDFishRef
write (1,*)" Parameter  96 _cPDPisc_", cPDPisc
write (1,*)" Parameter  97 _cNDPisc_", cNDPisc
write (1,*)" Parameter  98 _cQIn_", cQIn
write (1,*)" Parameter  99 _cQInSum_", cQInSum
write (1,*)" Parameter  100 _cQInWin_", cQInWin
write (1,*)" Parameter  101 _cDepthWMax_", cDepthWMax
write (1,*)" Parameter  102 _cQInExtraApril1_", cQInExtraApril1
write (1,*)" Parameter  103 _cQInExtraOct1_", cQInExtraOct1
write (1,*)" Parameter  104 _cQOutExtraApril1_", cQOutExtraApril1
write (1,*)" Parameter  105 _cQOutExtraOct1_", cQOutExtraOct1
write (1,*)" Parameter  106 _cQEvAve_", cQEvAve
write (1,*)" Parameter  107 _cQEvVar_", cQEvVar
write (1,*)" Parameter  108 _cPLoad_", cPLoad
write (1,*)" Parameter  109 _cPLoadSum_", cPLoadSum
write (1,*)" Parameter  110 _cPLoadWin_", cPLoadWin
write (1,*)" Parameter  111 _fPO4In_", fPO4In
write (1,*)" Parameter  112 _fPhytInWin_", fPhytInWin
write (1,*)" Parameter  113 _fPhytInSum_", fPhytInSum
write (1,*)" Parameter  114 _fDiatPhytIn_", fDiatPhytIn
write (1,*)" Parameter  115 _fGrenPhytIn_", fGrenPhytIn
write (1,*)" Parameter  116 _fBluePhytIn_", fBluePhytIn
write (1,*)" Parameter  117 _cNLoad_", cNLoad
write (1,*)" Parameter  118 _cNLoadSum_", cNLoadSum
write (1,*)" Parameter  119 _cNLoadWin_", cNLoadWin
write (1,*)" Parameter  120 _cNPLoadMeas_", cNPLoadMeas
write (1,*)" Parameter  121 _cNPPhytIn_", cNPPhytIn
write (1,*)" Parameter  122 _cNPDetIn_", cNPDetIn
write (1,*)" Parameter  123 _fNH4DissIn_", fNH4DissIn
write (1,*)" Parameter  124 _cNDPhytIn_", cNDPhytIn
write (1,*)" Parameter  125 _cNDDetIn_", cNDDetIn
write (1,*)" Parameter  126 _cDIMIn_", cDIMIn
write (1,*)" Parameter  127 _cO2In_", cO2In
write (1,*)" Parameter  128 _cSiO2In_", cSiO2In
write (1,*)" Parameter  129 _cSiDDetIn_", cSiDDetIn
write (1,*)" Parameter  130 _cDZooIn_", cDZooIn
write (1,*)" Parameter  131 _cDayApril1_", cDayApril1
write (1,*)" Parameter  132 _cDayOct1_", cDayOct1
write (1,*)" Parameter  133 _cLengChange_", cLengChange
write (1,*)" Parameter  134 _cNLoadS_", cNLoadS
write (1,*)" Parameter  135 _fNH4LoadS_", fNH4LoadS
write (1,*)" Parameter  136 _cDErosTot_", cDErosTot
write (1,*)" Parameter  137 _fSedErosIM_", fSedErosIM
write (1,*)" Parameter  138 _fDOrgSoil_", fDOrgSoil
write (1,*)" Parameter  139 _cPDSoilOM_", cPDSoilOM
write (1,*)" Parameter  140 _cNDSoilOM_", cNDSoilOM
write (1,*)" Parameter  141 _cPO4Ground_", cPO4Ground
write (1,*)" Parameter  142 _cNH4Ground_", cNH4Ground
write (1,*)" Parameter  143 _cNO3Ground_", cNO3Ground
write (1,*)" Parameter  144 _cDepthS_", cDepthS
write (1,*)" Parameter  145 _cCPerDW_", cCPerDW
write (1,*)" Parameter  146 _cRhoIM_", cRhoIM
write (1,*)" Parameter  147 _cRhoOM_", cRhoOM
write (1,*)" Parameter  148 _cTmRef_", cTmRef
write (1,*)" Parameter  149 _cAerRoot_", cAerRoot
write (1,*)" Parameter  150 _cAerLin_", cAerLin
write (1,*)" Parameter  151 _cAerSquare_", cAerSquare
write (1,*)" Parameter  152 _cThetaAer_", cThetaAer
write (1,*)" Parameter  153 _cVSetIM_", cVSetIM
write (1,*)" Parameter  154 _cVSetDet_", cVSetDet
write (1,*)" Parameter  155 _cThetaSet_", cThetaSet
write (1,*)" Parameter  156 _cSuspMin_", cSuspMin
write (1,*)" Parameter  157 _cSuspMax_", cSuspMax
write (1,*)" Parameter  158 _cSuspSlope_", cSuspSlope
write (1,*)" Parameter  159 _hDepthSusp_", hDepthSusp
write (1,*)" Parameter  160 _cFetchRef_", cFetchRef
write (1,*)" Parameter  161 _fLutumRef_", fLutumRef
write (1,*)" Parameter  162 _cSuspRef_", cSuspRef
write (1,*)" Parameter  163 _kVegResus_", kVegResus
write (1,*)" Parameter  164 _kTurbFish_", kTurbFish
write (1,*)" Parameter  165 _kResusPhytMax_", kResusPhytMax
write (1,*)" Parameter  166 _cResusPhytExp_", cResusPhytExp
write (1,*)" Parameter  167 _cThetaMinW_", cThetaMinW
write (1,*)" Parameter  168 _kDMinDetW_", kDMinDetW
write (1,*)" Parameter  169 _hO2BOD_", hO2BOD
write (1,*)" Parameter  170 _O2PerNO3_", O2PerNO3
write (1,*)" Parameter  171 _cThetaMinS_", cThetaMinS
write (1,*)" Parameter  172 _kDMinDetS_", kDMinDetS
write (1,*)" Parameter  173 _fRefrDetS_", fRefrDetS
write (1,*)" Parameter  174 _hNO3Denit_", hNO3Denit
write (1,*)" Parameter  175 _NO3PerC_", NO3PerC
write (1,*)" Parameter  176 _kDMinHum_", kDMinHum
write (1,*)" Parameter  177 _kNitrW_", kNitrW
write (1,*)" Parameter  178 _kNitrS_", kNitrS
write (1,*)" Parameter  179 _cThetaNitr_", cThetaNitr
write (1,*)" Parameter  180 _O2PerNH4_", O2PerNH4
write (1,*)" Parameter  181 _hO2Nitr_", hO2Nitr
write (1,*)" Parameter  182 _kPDifPO4_", kPDifPO4
write (1,*)" Parameter  183 _kNDifNO3_", kNDifNO3
write (1,*)" Parameter  184 _kNDifNH4_", kNDifNH4
write (1,*)" Parameter  185 _kO2Dif_", kO2Dif
write (1,*)" Parameter  186 _cThetaDif_", cThetaDif
write (1,*)" Parameter  187 _fDepthDifS_", fDepthDifS
write (1,*)" Parameter  188 _cTurbDifNut_", cTurbDifNut
write (1,*)" Parameter  189 _cTurbDifO2_", cTurbDifO2
write (1,*)" Parameter  190 _kPSorp_", kPSorp
write (1,*)" Parameter  191 _cRelPAdsD_", cRelPAdsD
write (1,*)" Parameter  192 _cRelPAdsFe_", cRelPAdsFe
write (1,*)" Parameter  193 _cRelPAdsAl_", cRelPAdsAl
write (1,*)" Parameter  194 _cKPAdsOx_", cKPAdsOx
write (1,*)" Parameter  195 _fRedMax_", fRedMax
write (1,*)" Parameter  196 _coPO4Max_", coPO4Max
write (1,*)" Parameter  197 _kPChemPO4_", kPChemPO4
write (1,*)" Parameter  198 _cDayManVeg1_", cDayManVeg1
write (1,*)" Parameter  199 _cDayManVeg2_", cDayManVeg2
write (1,*)" Parameter  200 _fManVeg_", fManVeg
write (1,*)" Parameter  201 _cLengMan_", cLengMan
write (1,*)" Parameter  202 _cYearStartBirds_", cYearStartBirds
write (1,*)" Parameter  203 _cDayStartBirds_", cDayStartBirds
write (1,*)" Parameter  204 _cDayEndBirds_", cDayEndBirds
write (1,*)" Parameter  205 _cBirdsPerha_", cBirdsPerha
write (1,*)" Parameter  206 _cDGrazPerBird_", cDGrazPerBird
write (1,*)" Parameter  207 _hDVegBird_", hDVegBird
write (1,*)" Parameter  208 _fDAssBird_", fDAssBird
write (1,*)" Parameter  209 _fDissEgesBird_", fDissEgesBird
write (1,*)" Parameter  210 _fDissMortVeg_", fDissMortVeg
write (1,*)" Parameter  211 _cLengAllo_", cLengAllo
write (1,*)" Parameter  212 _cLengMort_", cLengMort
write (1,*)" Parameter  213 _UseEmpUpt_", UseEmpUpt
write (1,*)" Parameter  214 _fSedUptVegMax_", fSedUptVegMax
write (1,*)" Parameter  215 _fSedUptVegCoef_", fSedUptVegCoef
write (1,*)" Parameter  216 _fSedUptVegExp_", fSedUptVegExp
write (1,*)" Parameter  217 _fRootVegSum_", fRootVegSum
write (1,*)" Parameter  218 _fRootVegWin_", fRootVegWin
write (1,*)" Parameter  219 _fFloatVeg_", fFloatVeg
write (1,*)" Parameter  220 _fEmergVeg_", fEmergVeg
write (1,*)" Parameter  221 _fDepth1Veg_", fDepth1Veg
write (1,*)" Parameter  222 _fDepth2Veg_", fDepth2Veg
write (1,*)" Parameter  223 _cDLayerVeg_", cDLayerVeg
write (1,*)" Parameter  224 _cCovSpVeg_", cCovSpVeg
write (1,*)" Parameter  225 _kMigrVeg_", kMigrVeg
write (1,*)" Parameter  226 _cDVegIn_", cDVegIn
write (1,*)" Parameter  227 _cTmInitVeg_", cTmInitVeg
write (1,*)" Parameter  228 _cDCarrVeg_", cDCarrVeg
write (1,*)" Parameter  229 _cMuMaxVeg_", cMuMaxVeg
write (1,*)" Parameter  230 _cQ10ProdVeg_", cQ10ProdVeg
write (1,*)" Parameter  231 _hLRefVeg_", hLRefVeg
write (1,*)" Parameter  232 _cExtSpVeg_", cExtSpVeg
write (1,*)" Parameter  233 _kDRespVeg_", kDRespVeg
write (1,*)" Parameter  234 _cQ10RespVeg_", cQ10RespVeg
write (1,*)" Parameter  235 _kMortVegSum_", kMortVegSum
write (1,*)" Parameter  236 _fWinVeg_", fWinVeg
write (1,*)" Parameter  237 _cDayWinVeg_", cDayWinVeg
write (1,*)" Parameter  238 _fDetWMortVeg_", fDetWMortVeg
write (1,*)" Parameter  239 _cPrefVegBird_", cPrefVegBird
write (1,*)" Parameter  240 _cVPUptMaxVeg_", cVPUptMaxVeg
write (1,*)" Parameter  241 _cAffPUptVeg_", cAffPUptVeg
write (1,*)" Parameter  242 _cPDVegMin_", cPDVegMin
write (1,*)" Parameter  243 _cPDVegMax_", cPDVegMax
write (1,*)" Parameter  244 _cVNUptMaxVeg_", cVNUptMaxVeg
write (1,*)" Parameter  245 _cAffNUptVeg_", cAffNUptVeg
write (1,*)" Parameter  246 _cNDVegMin_", cNDVegMin
write (1,*)" Parameter  247 _cNDVegMax_", cNDVegMax
write (1,*)" Parameter  248 _cPACoefMin_", cPACoefMin
write (1,*)" Parameter  249 _cPACoefMax_", cPACoefMax
write (1,*)" Parameter  250 _hPACoef_", hPACoef
write (1,*)" Parameter  251 _cSecchiPlus_", cSecchiPlus
write (1,*)" Parameter  252 _cEuph_", cEuph
write (1,*)" Parameter  253 _cCovSpPhyt_", cCovSpPhyt
write (1,*)" Parameter  254 _cTmOptLoss_", cTmOptLoss
write (1,*)" Parameter  255 _cSigTmLoss_", cSigTmLoss
write (1,*)" Parameter  256 _fDissMortPhyt_", fDissMortPhyt
write (1,*)" Parameter  257 _fDissLoss_", fDissLoss
write (1,*)" Parameter  258 _cMuMaxDiat_", cMuMaxDiat
write (1,*)" Parameter  259 _cTmOptDiat_", cTmOptDiat
write (1,*)" Parameter  260 _cSigTmDiat_", cSigTmDiat
write (1,*)" Parameter  261 _cExtSpDiat_", cExtSpDiat
write (1,*)" Parameter  262 _UseSteeleDiat_", UseSteeleDiat
write (1,*)" Parameter  263 _cLOptRefDiat_", cLOptRefDiat
write (1,*)" Parameter  264 _hLRefDiat_", hLRefDiat
write (1,*)" Parameter  265 _cChDDiatMin_", cChDDiatMin
write (1,*)" Parameter  266 _cChDDiatMax_", cChDDiatMax
write (1,*)" Parameter  267 _kDRespDiat_", kDRespDiat
write (1,*)" Parameter  268 _kLossDiat_", kLossDiat
write (1,*)" Parameter  269 _kMortDiatW_", kMortDiatW
write (1,*)" Parameter  270 _kMortDiatS_", kMortDiatS
write (1,*)" Parameter  271 _cVSetDiat_", cVSetDiat
write (1,*)" Parameter  272 _cVPUptMaxDiat_", cVPUptMaxDiat
write (1,*)" Parameter  273 _cAffPUptDiat_", cAffPUptDiat
write (1,*)" Parameter  274 _cPDDiatMin_", cPDDiatMin
write (1,*)" Parameter  275 _cPDDiatMax_", cPDDiatMax
write (1,*)" Parameter  276 _cVNUptMaxDiat_", cVNUptMaxDiat
write (1,*)" Parameter  277 _cAffNUptDiat_", cAffNUptDiat
write (1,*)" Parameter  278 _cNDDiatMin_", cNDDiatMin
write (1,*)" Parameter  279 _cNDDiatMax_", cNDDiatMax
write (1,*)" Parameter  280 _hSiAssDiat_", hSiAssDiat
write (1,*)" Parameter  281 _cMuMaxGren_", cMuMaxGren
write (1,*)" Parameter  282 _cTmOptGren_", cTmOptGren
write (1,*)" Parameter  283 _cSigTmGren_", cSigTmGren
write (1,*)" Parameter  284 _cExtSpGren_", cExtSpGren
write (1,*)" Parameter  285 _UseSteeleGren_", UseSteeleGren
write (1,*)" Parameter  286 _hLRefGren_", hLRefGren
write (1,*)" Parameter  287 _cLOptRefGren_", cLOptRefGren
write (1,*)" Parameter  288 _cChDGrenMin_", cChDGrenMin
write (1,*)" Parameter  289 _cChDGrenMax_", cChDGrenMax
write (1,*)" Parameter  290 _kDRespGren_", kDRespGren
write (1,*)" Parameter  291 _kLossGren_", kLossGren
write (1,*)" Parameter  292 _kMortGrenW_", kMortGrenW
write (1,*)" Parameter  293 _kMortGrenS_", kMortGrenS
write (1,*)" Parameter  294 _cVSetGren_", cVSetGren
write (1,*)" Parameter  295 _cVPUptMaxGren_", cVPUptMaxGren
write (1,*)" Parameter  296 _cAffPUptGren_", cAffPUptGren
write (1,*)" Parameter  297 _cPDGrenMin_", cPDGrenMin
write (1,*)" Parameter  298 _cPDGrenMax_", cPDGrenMax
write (1,*)" Parameter  299 _cVNUptMaxGren_", cVNUptMaxGren
write (1,*)" Parameter  300 _cAffNUptGren_", cAffNUptGren
write (1,*)" Parameter  301 _cNDGrenMin_", cNDGrenMin
write (1,*)" Parameter  302 _cNDGrenMax_", cNDGrenMax
write (1,*)" Parameter  303 _hSiAssGren_", hSiAssGren
write (1,*)" Parameter  304 _cMuMaxBlue_", cMuMaxBlue
write (1,*)" Parameter  305 _cTmOptBlue_", cTmOptBlue
write (1,*)" Parameter  306 _cSigTmBlue_", cSigTmBlue
write (1,*)" Parameter  307 _cExtSpBlue_", cExtSpBlue
write (1,*)" Parameter  308 _UseSteeleBlue_", UseSteeleBlue
write (1,*)" Parameter  309 _cLOptRefBlue_", cLOptRefBlue
write (1,*)" Parameter  310 _hLRefBlue_", hLRefBlue
write (1,*)" Parameter  311 _cChDBlueMin_", cChDBlueMin
write (1,*)" Parameter  312 _cChDBlueMax_", cChDBlueMax
write (1,*)" Parameter  313 _cCyDBlueMin_", cCyDBlueMin
write (1,*)" Parameter  314 _cCyDBlueMax_", cCyDBlueMax
write (1,*)" Parameter  315 _kDRespBlue_", kDRespBlue
write (1,*)" Parameter  316 _kLossBlue_", kLossBlue
write (1,*)" Parameter  317 _kMortBlueW_", kMortBlueW
write (1,*)" Parameter  318 _kMortBlueS_", kMortBlueS
write (1,*)" Parameter  319 _cVSetBlue_", cVSetBlue
write (1,*)" Parameter  320 _cVPUptMaxBlue_", cVPUptMaxBlue
write (1,*)" Parameter  321 _cAffPUptBlue_", cAffPUptBlue
write (1,*)" Parameter  322 _cPDBlueMin_", cPDBlueMin
write (1,*)" Parameter  323 _cPDBlueMax_", cPDBlueMax
write (1,*)" Parameter  324 _cVNUptMaxBlue_", cVNUptMaxBlue
write (1,*)" Parameter  325 _cAffNUptBlue_", cAffNUptBlue
write (1,*)" Parameter  326 _cNDBlueMin_", cNDBlueMin
write (1,*)" Parameter  327 _cNDBlueMax_", cNDBlueMax
write (1,*)" Parameter  328 _hSiAssBlue_", hSiAssBlue
write (1,*)" Parameter  329 _cDBentIn_", cDBentIn
write (1,*)" Parameter  330 _kMigrBent_", kMigrBent
write (1,*)" Parameter  331 _kMigrFish_", kMigrFish
write (1,*)" Parameter  332 _cDFiJvIn_", cDFiJvIn
write (1,*)" Parameter  333 _cDFiAdIn_", cDFiAdIn
write (1,*)" Parameter  334 _kHarvFishWin_", kHarvFishWin
write (1,*)" Parameter  335 _kHarvFishSum_", kHarvFishSum
write (1,*)" Parameter  336 _cDPiscIn_", cDPiscIn
write (1,*)" Parameter  337 _kMigrPisc_", kMigrPisc
write (1,*)" Parameter  338 _kHarvPiscWin_", kHarvPiscWin
write (1,*)" Parameter  339 _kHarvPiscSum_", kHarvPiscSum
write (1,*)" Parameter  340 _cFiltMax_", cFiltMax
write (1,*)" Parameter  341 _hFilt_", hFilt
write (1,*)" Parameter  342 _cDCarrZoo_", cDCarrZoo
write (1,*)" Parameter  343 _cPrefDiat_", cPrefDiat
write (1,*)" Parameter  344 _cPrefGren_", cPrefGren
write (1,*)" Parameter  345 _cPrefBlue_", cPrefBlue
write (1,*)" Parameter  346 _cPrefDet_", cPrefDet
write (1,*)" Parameter  347 _fDAssZoo_", fDAssZoo
write (1,*)" Parameter  348 _fDissEgesZoo_", fDissEgesZoo
write (1,*)" Parameter  349 _kDRespZoo_", kDRespZoo
write (1,*)" Parameter  350 _kMortZoo_", kMortZoo
write (1,*)" Parameter  351 _fDissMortZoo_", fDissMortZoo
write (1,*)" Parameter  352 _cTmOptZoo_", cTmOptZoo
write (1,*)" Parameter  353 _cSigTmZoo_", cSigTmZoo
write (1,*)" Parameter  354 _cDCarrBent_", cDCarrBent
write (1,*)" Parameter  355 _kDAssBent_", kDAssBent
write (1,*)" Parameter  356 _hDFoodBent_", hDFoodBent
write (1,*)" Parameter  357 _fDAssBent_", fDAssBent
write (1,*)" Parameter  358 _fDissEgesBent_", fDissEgesBent
write (1,*)" Parameter  359 _kDRespBent_", kDRespBent
write (1,*)" Parameter  360 _kMortBent_", kMortBent
write (1,*)" Parameter  361 _fDissMortBent_", fDissMortBent
write (1,*)" Parameter  362 _cTmOptBent_", cTmOptBent
write (1,*)" Parameter  363 _cSigTmBent_", cSigTmBent
write (1,*)" Parameter  364 _fDBone_", fDBone
write (1,*)" Parameter  365 _fPBone_", fPBone
write (1,*)" Parameter  366 _cDCarrFish_", cDCarrFish
write (1,*)" Parameter  367 _fDissEgesFish_", fDissEgesFish
write (1,*)" Parameter  368 _fDissMortFish_", fDissMortFish
write (1,*)" Parameter  369 _cTmOptFish_", cTmOptFish
write (1,*)" Parameter  370 _cSigTmFish_", cSigTmFish
write (1,*)" Parameter  371 _cDayReprFish_", cDayReprFish
write (1,*)" Parameter  372 _fReprFish_", fReprFish
write (1,*)" Parameter  373 _fAgeFish_", fAgeFish
write (1,*)" Parameter  374 _cRelVegFish_", cRelVegFish
write (1,*)" Parameter  375 _kDAssFiJv_", kDAssFiJv
write (1,*)" Parameter  376 _hDZooFiJv_", hDZooFiJv
write (1,*)" Parameter  377 _fDAssFiJv_", fDAssFiJv
write (1,*)" Parameter  378 _kDRespFiJv_", kDRespFiJv
write (1,*)" Parameter  379 _kMortFiJv_", kMortFiJv
write (1,*)" Parameter  380 _kDAssFiAd_", kDAssFiAd
write (1,*)" Parameter  381 _hDBentFiAd_", hDBentFiAd
write (1,*)" Parameter  382 _fDAssFiAd_", fDAssFiAd
write (1,*)" Parameter  383 _kDRespFiAd_", kDRespFiAd
write (1,*)" Parameter  384 _kMortFiAd_", kMortFiAd
write (1,*)" Parameter  385 _cDCarrPiscMax_", cDCarrPiscMax
write (1,*)" Parameter  386 _cDCarrPiscMin_", cDCarrPiscMin
write (1,*)" Parameter  387 _cDCarrPiscBare_", cDCarrPiscBare
write (1,*)" Parameter  388 _cDPhraMinPisc_", cDPhraMinPisc
write (1,*)" Parameter  389 _cCovVegMin_", cCovVegMin
write (1,*)" Parameter  390 _cRelPhraPisc_", cRelPhraPisc
write (1,*)" Parameter  391 _cRelVegPisc_", cRelVegPisc
write (1,*)" Parameter  392 _kDAssPisc_", kDAssPisc
write (1,*)" Parameter  393 _hDVegPisc_", hDVegPisc
write (1,*)" Parameter  394 _hDFishPisc_", hDFishPisc
write (1,*)" Parameter  395 _fDAssPisc_", fDAssPisc
write (1,*)" Parameter  396 _fDissEgesPisc_", fDissEgesPisc
write (1,*)" Parameter  397 _kDRespPisc_", kDRespPisc
write (1,*)" Parameter  398 _kMortPisc_", kMortPisc
write (1,*)" Parameter  399 _fDissMortPisc_", fDissMortPisc
write (1,*)" Parameter  400 _cTmOptPisc_", cTmOptPisc
write (1,*)" Parameter  401 _cSigTmPisc_", cSigTmPisc
write (1,*)" Parameter  402 _cDepthSM_", cDepthSM
write (1,*)" Parameter  403 _kExchMaxM_", kExchMaxM
write (1,*)" Parameter  404 _hfMarsh_", hfMarsh
write (1,*)" Parameter  405 _fDTotSM0_", fDTotSM0
write (1,*)" Parameter  406 _fDOrgSM0_", fDOrgSM0
write (1,*)" Parameter  407 _fDDetSM0_", fDDetSM0
write (1,*)" Parameter  408 _fPInorgSM0_", fPInorgSM0
write (1,*)" Parameter  409 _cPDPhra0_", cPDPhra0
write (1,*)" Parameter  410 _cNDPhra0_", cNDPhra0
write (1,*)" Parameter  411 _cDensStemPhra_", cDensStemPhra
write (1,*)" Parameter  412 _cTmInitPhra_", cTmInitPhra
write (1,*)" Parameter  413 _fDAllPhra_", fDAllPhra
write (1,*)" Parameter  414 _kDAllPhra_", kDAllPhra
write (1,*)" Parameter  415 _cDStemPhra_", cDStemPhra
write (1,*)" Parameter  416 _cQ10ProdPhra_", cQ10ProdPhra
write (1,*)" Parameter  417 _cMuPhraMax_", cMuPhraMax
write (1,*)" Parameter  418 _cDShootPhraMax_", cDShootPhraMax
write (1,*)" Parameter  419 _cCovSpPhra_", cCovSpPhra
write (1,*)" Parameter  420 _cPDPhraMin_", cPDPhraMin
write (1,*)" Parameter  421 _cPDPhraMax_", cPDPhraMax
write (1,*)" Parameter  422 _cNDPhraMin_", cNDPhraMin
write (1,*)" Parameter  423 _cNDPhraMax_", cNDPhraMax
write (1,*)" Parameter  424 _cAffNUptPhra_", cAffNUptPhra
write (1,*)" Parameter  425 _cAffPUptPhra_", cAffPUptPhra
write (1,*)" Parameter  426 _cVNUptPhraMax_", cVNUptPhraMax
write (1,*)" Parameter  427 _cVPUptPhraMax_", cVPUptPhraMax
write (1,*)" Parameter  428 _kDRespPhra_", kDRespPhra
write (1,*)" Parameter  429 _cQ10RespPhra_", cQ10RespPhra
write (1,*)" Parameter  430 _fDayWin_", fDayWin
write (1,*)" Parameter  431 _fDRealPhra_", fDRealPhra
write (1,*)" Parameter  432 _kDRealPhra_", kDRealPhra
write (1,*)" Parameter  433 _kDMortShootPhra_", kDMortShootPhra
write (1,*)" Parameter  434 _kDMortRootPhra_", kDMortRootPhra
write (1,*)" Parameter  435 _cDayWinPhra_", cDayWinPhra
write (1,*)" Parameter  436 _cDayManPhra_", cDayManPhra
write (1,*)" Parameter  437 _fManPhra_", fManPhra
write (1,*)" Parameter  438 _kDManShootPhra_", kDManShootPhra
write (1,*)" Parameter  439 _DaysPerYear_", DaysPerYear
write (1,*)" Parameter  440 _TenDays_", TenDays
write (1,*)" Parameter  441 _HoursPerDay_", HoursPerDay
write (1,*)" Parameter  442 _SecsPerDay_", SecsPerDay
write (1,*)" Parameter  443 _mmPerm_", mmPerm
write (1,*)" Parameter  444 _m2Perha_", m2Perha
write (1,*)" Parameter  445 _mgPerg_", mgPerg
write (1,*)" Parameter  446 _gPerkg_", gPerkg
write (1,*)" Parameter  447 _gPerton_", gPerton
write (1,*)" Parameter  448 _PerCent_", PerCent
write (1,*)" Parameter  449 _NearZero_", NearZero
write (1,*)" Parameter  450 _molO2molC_", molO2molC
write (1,*)" Parameter  451 _molO2molN_", molO2molN
write (1,*)" Parameter  452 _molNmolC_", molNmolC
write (1,*)" Parameter  453 _cRhoWat_", cRhoWat
write (1,*)" Parameter  454 _Pi_", Pi
write (1,*)" InitState  0 _cDepthW0_", sDepthW
write (1,*)" InitState  1 _cNH4W0_", sNH4W
write (1,*)" InitState  2 _cNO3W0_", sNO3W
write (1,*)" InitState  3 _cPO4W0_", sPO4W
write (1,*)" InitState  4 _cPAIMW0_", sPAIMW
write (1,*)" InitState  5 _cSiO2W0_", sSiO2W
write (1,*)" InitState  6 _cO2W0_", sO2W
write (1,*)" InitState  7 _cDDetW0_", sDDetW
write (1,*)" InitState  8 _cNDetW0_", sNDetW
write (1,*)" InitState  9 _cPDetW0_", sPDetW
write (1,*)" InitState  10 _cSiDetW0_", sSiDetW
write (1,*)" InitState  11 _cDIMW0_", sDIMW
write (1,*)" InitState  12 _cDDiatW0_", sDDiatW
write (1,*)" InitState  13 _cNDiatW0_", sNDiatW
write (1,*)" InitState  14 _cPDiatW0_", sPDiatW
write (1,*)" InitState  15 _cDGrenW0_", sDGrenW
write (1,*)" InitState  16 _cNGrenW0_", sNGrenW
write (1,*)" InitState  17 _cPGrenW0_", sPGrenW
write (1,*)" InitState  18 _cDBlueW0_", sDBlueW
write (1,*)" InitState  19 _cNBlueW0_", sNBlueW
write (1,*)" InitState  20 _cPBlueW0_", sPBlueW
write (1,*)" InitState  21 _cDZoo0_", sDZoo
write (1,*)" InitState  22 _cNZoo0_", sNZoo
write (1,*)" InitState  23 _cPZoo0_", sPZoo
write (1,*)" InitState  24 _cDFiAd0_", sDFiAd
write (1,*)" InitState  25 _cDFiJv0_", sDFiJv
write (1,*)" InitState  26 _cNFiAd0_", sNFiAd
write (1,*)" InitState  27 _cNFiJv0_", sNFiJv
write (1,*)" InitState  28 _cPFiAd0_", sPFiAd
write (1,*)" InitState  29 _cPFiJv0_", sPFiJv
write (1,*)" InitState  30 _cDPisc0_", sDPisc
write (1,*)" InitState  31 _cNH4S0_", sNH4S
write (1,*)" InitState  32 _cNO3S0_", sNO3S
write (1,*)" InitState  33 _cPO4S0_", sPO4S
write (1,*)" InitState  34 _cPAIMS0_", sPAIMS
write (1,*)" InitState  35 _cDDetS0_", sDDetS
write (1,*)" InitState  36 _cNDetS0_", sNDetS
write (1,*)" InitState  37 _cPDetS0_", sPDetS
write (1,*)" InitState  38 _cSiDetS0_", sSiDetS
write (1,*)" InitState  39 _cDHumS0_", sDHumS
write (1,*)" InitState  40 _cNHumS0_", sNHumS
write (1,*)" InitState  41 _cPHumS0_", sPHumS
write (1,*)" InitState  42 _cDIMS0_", sDIMS
write (1,*)" InitState  43 _cDDiatS0_", sDDiatS
write (1,*)" InitState  44 _cNDiatS0_", sNDiatS
write (1,*)" InitState  45 _cPDiatS0_", sPDiatS
write (1,*)" InitState  46 _cDGrenS0_", sDGrenS
write (1,*)" InitState  47 _cNGrenS0_", sNGrenS
write (1,*)" InitState  48 _cPGrenS0_", sPGrenS
write (1,*)" InitState  49 _cDBlueS0_", sDBlueS
write (1,*)" InitState  50 _cNBlueS0_", sNBlueS
write (1,*)" InitState  51 _cPBlueS0_", sPBlueS
write (1,*)" InitState  52 _cDVeg0_", sDVeg
write (1,*)" InitState  53 _cNVeg0_", sNVeg
write (1,*)" InitState  54 _cPVeg0_", sPVeg
write (1,*)" InitState  55 _cDBent0_", sDBent
write (1,*)" InitState  56 _cNBent0_", sNBent
write (1,*)" InitState  57 _cPBent0_", sPBent
write (1,*)" InitState  58 _cDepthWM0_", sDepthWM
write (1,*)" InitState  59 _cNH4WM0_", sNH4WM
write (1,*)" InitState  60 _cNO3WM0_", sNO3WM
write (1,*)" InitState  61 _cPO4WM0_", sPO4WM
write (1,*)" InitState  62 _cPAIMWM0_", sPAIMWM
write (1,*)" InitState  63 _cSiO2WM0_", sSiO2WM
write (1,*)" InitState  64 _cO2WM0_", sO2WM
write (1,*)" InitState  65 _cDDetWM0_", sDDetWM
write (1,*)" InitState  66 _cNDetWM0_", sNDetWM
write (1,*)" InitState  67 _cPDetWM0_", sPDetWM
write (1,*)" InitState  68 _cSiDetWM0_", sSiDetWM
write (1,*)" InitState  69 _cDIMWM0_", sDIMWM
write (1,*)" InitState  70 _cDDiatWM0_", sDDiatWM
write (1,*)" InitState  71 _cNDiatWM0_", sNDiatWM
write (1,*)" InitState  72 _cPDiatWM0_", sPDiatWM
write (1,*)" InitState  73 _cDGrenWM0_", sDGrenWM
write (1,*)" InitState  74 _cNGrenWM0_", sNGrenWM
write (1,*)" InitState  75 _cPGrenWM0_", sPGrenWM
write (1,*)" InitState  76 _cDBlueWM0_", sDBlueWM
write (1,*)" InitState  77 _cNBlueWM0_", sNBlueWM
write (1,*)" InitState  78 _cPBlueWM0_", sPBlueWM
write (1,*)" InitState  79 _cDZooM0_", sDZooM
write (1,*)" InitState  80 _cNZooM0_", sNZooM
write (1,*)" InitState  81 _cPZooM0_", sPZooM
write (1,*)" InitState  82 _cNH4SM0_", sNH4SM
write (1,*)" InitState  83 _cNO3SM0_", sNO3SM
write (1,*)" InitState  84 _cPO4SM0_", sPO4SM
write (1,*)" InitState  85 _cPAIMSM0_", sPAIMSM
write (1,*)" InitState  86 _cDDetSM0_", sDDetSM
write (1,*)" InitState  87 _cNDetSM0_", sNDetSM
write (1,*)" InitState  88 _cPDetSM0_", sPDetSM
write (1,*)" InitState  89 _cSiDetSM0_", sSiDetSM
write (1,*)" InitState  90 _cDHumSM0_", sDHumSM
write (1,*)" InitState  91 _cNHumSM0_", sNHumSM
write (1,*)" InitState  92 _cPHumSM0_", sPHumSM
write (1,*)" InitState  93 _cDIMSM0_", sDIMSM
write (1,*)" InitState  94 _cDRootPhra0_", sDRootPhra
write (1,*)" InitState  95 _cDShootPhra0_", sDShootPhra
write (1,*)" InitState  96 _cNRootPhra0_", sNRootPhra
write (1,*)" InitState  97 _cNShootPhra0_", sNShootPhra
write (1,*)" InitState  98 _cPRootPhra0_", sPRootPhra
write (1,*)" InitState  99 _cPShootPhra0_", sPShootPhra
write (1,*)" InitState  100 _cDExtTotT0_", sDExtTotT
write (1,*)" InitState  101 _cNExtTotT0_", sNExtTotT
write (1,*)" InitState  102 _cPExtTotT0_", sPExtTotT
write (1,*)" InitState  103 _cSiExtTotT0_", sSiExtTotT
write (1,*)" InitState  0 _sDepthW0_ 0"
write (1,*)" InitState  1 _sNH4W0_ 0"
write (1,*)" InitState  2 _sNO3W0_ 0"
write (1,*)" InitState  3 _sPO4W0_ 0"
write (1,*)" InitState  4 _sPAIMW0_ 0"
write (1,*)" InitState  5 _sSiO2W0_ 0"
write (1,*)" InitState  6 _sO2W0_ 0"
write (1,*)" InitState  7 _sDDetW0_ 0"
write (1,*)" InitState  8 _sNDetW0_ 0"
write (1,*)" InitState  9 _sPDetW0_ 0"
write (1,*)" InitState  10 _sSiDetW0_ 0"
write (1,*)" InitState  11 _sDIMW0_ 0"
write (1,*)" InitState  12 _sDDiatW0_ 0"
write (1,*)" InitState  13 _sNDiatW0_ 0"
write (1,*)" InitState  14 _sPDiatW0_ 0"
write (1,*)" InitState  15 _sDGrenW0_ 0"
write (1,*)" InitState  16 _sNGrenW0_ 0"
write (1,*)" InitState  17 _sPGrenW0_ 0"
write (1,*)" InitState  18 _sDBlueW0_ 0"
write (1,*)" InitState  19 _sNBlueW0_ 0"
write (1,*)" InitState  20 _sPBlueW0_ 0"
write (1,*)" InitState  21 _sDZoo0_ 0"
write (1,*)" InitState  22 _sNZoo0_ 0"
write (1,*)" InitState  23 _sPZoo0_ 0"
write (1,*)" InitState  24 _sDFiAd0_ 0"
write (1,*)" InitState  25 _sDFiJv0_ 0"
write (1,*)" InitState  26 _sNFiAd0_ 0"
write (1,*)" InitState  27 _sNFiJv0_ 0"
write (1,*)" InitState  28 _sPFiAd0_ 0"
write (1,*)" InitState  29 _sPFiJv0_ 0"
write (1,*)" InitState  30 _sDPisc0_ 0"
write (1,*)" InitState  31 _sNH4S0_ 0"
write (1,*)" InitState  32 _sNO3S0_ 0"
write (1,*)" InitState  33 _bRhoSolidS0_ 0"
write (1,*)" InitState  34 _bPorS0_ 0"
write (1,*)" InitState  35 _bRhoTotS0_ 0"
write (1,*)" InitState  36 _bDTotS0_ 0"
write (1,*)" InitState  37 _sPO4S0_ 0"
write (1,*)" InitState  38 _sPAIMS0_ 0"
write (1,*)" InitState  39 _sDDetS0_ 0"
write (1,*)" InitState  40 _sNDetS0_ 0"
write (1,*)" InitState  41 _sPDetS0_ 0"
write (1,*)" InitState  42 _sSiDetS0_ 0"
write (1,*)" InitState  43 _sDHumS0_ 0"
write (1,*)" InitState  44 _sNHumS0_ 0"
write (1,*)" InitState  45 _sPHumS0_ 0"
write (1,*)" InitState  46 _sDIMS0_ 0"
write (1,*)" InitState  47 _sDDiatS0_ 0"
write (1,*)" InitState  48 _sNDiatS0_ 0"
write (1,*)" InitState  49 _sPDiatS0_ 0"
write (1,*)" InitState  50 _sDGrenS0_ 0"
write (1,*)" InitState  51 _sNGrenS0_ 0"
write (1,*)" InitState  52 _sPGrenS0_ 0"
write (1,*)" InitState  53 _sDBlueS0_ 0"
write (1,*)" InitState  54 _sNBlueS0_ 0"
write (1,*)" InitState  55 _sPBlueS0_ 0"
write (1,*)" InitState  56 _sDVeg0_ 0"
write (1,*)" InitState  57 _sNVeg0_ 0"
write (1,*)" InitState  58 _sPVeg0_ 0"
write (1,*)" InitState  59 _sDBent0_ 0"
write (1,*)" InitState  60 _sNBent0_ 0"
write (1,*)" InitState  61 _sPBent0_ 0"
write (1,*)" InitState  62 _sDepthWM0_ 0"
write (1,*)" InitState  63 _sNH4WM0_ 0"
write (1,*)" InitState  64 _sNO3WM0_ 0"
write (1,*)" InitState  65 _sPO4WM0_ 0"
write (1,*)" InitState  66 _sPAIMWM0_ 0"
write (1,*)" InitState  67 _sSiO2WM0_ 0"
write (1,*)" InitState  68 _sO2WM0_ 0"
write (1,*)" InitState  69 _sDDetWM0_ 0"
write (1,*)" InitState  70 _sNDetWM0_ 0"
write (1,*)" InitState  71 _sPDetWM0_ 0"
write (1,*)" InitState  72 _sSiDetWM0_ 0"
write (1,*)" InitState  73 _sDIMWM0_ 0"
write (1,*)" InitState  74 _sDDiatWM0_ 0"
write (1,*)" InitState  75 _sNDiatWM0_ 0"
write (1,*)" InitState  76 _sPDiatWM0_ 0"
write (1,*)" InitState  77 _sDGrenWM0_ 0"
write (1,*)" InitState  78 _sNGrenWM0_ 0"
write (1,*)" InitState  79 _sPGrenWM0_ 0"
write (1,*)" InitState  80 _sDBlueWM0_ 0"
write (1,*)" InitState  81 _sNBlueWM0_ 0"
write (1,*)" InitState  82 _sPBlueWM0_ 0"
write (1,*)" InitState  83 _sDZooM0_ 0"
write (1,*)" InitState  84 _sNZooM0_ 0"
write (1,*)" InitState  85 _sPZooM0_ 0"
write (1,*)" InitState  86 _sNH4SM0_ 0"
write (1,*)" InitState  87 _sNO3SM0_ 0"
write (1,*)" InitState  88 _bRhoSolidSM0_ 0"
write (1,*)" InitState  89 _bPorSM0_ 0"
write (1,*)" InitState  90 _bRhoTotSM0_ 0"
write (1,*)" InitState  91 _bDTotSM0_ 0"
write (1,*)" InitState  92 _fPAdsSM0_ 0"
write (1,*)" InitState  93 _sPO4SM0_ 0"
write (1,*)" InitState  94 _sPAIMSM0_ 0"
write (1,*)" InitState  95 _sDDetSM0_ 0"
write (1,*)" InitState  96 _sNDetSM0_ 0"
write (1,*)" InitState  97 _sPDetSM0_ 0"
write (1,*)" InitState  98 _sSiDetSM0_ 0"
write (1,*)" InitState  99 _sDHumSM0_ 0"
write (1,*)" InitState  100 _sNHumSM0_ 0"
write (1,*)" InitState  101 _sPHumSM0_ 0"
write (1,*)" InitState  102 _sDIMSM0_ 0"
write (1,*)" InitState  103 _sDRootPhra0_ 0"
write (1,*)" InitState  104 _sDShootPhra0_ 0"
write (1,*)" InitState  105 _sNRootPhra0_ 0"
write (1,*)" InitState  106 _sNShootPhra0_ 0"
write (1,*)" InitState  107 _sPRootPhra0_ 0"
write (1,*)" InitState  108 _sPShootPhra0_ 0"
write (1,*)" InitState  109 _uDPhytW0_ 0"
write (1,*)" InitState  110 _uPPhytW0_ 0"
write (1,*)" InitState  111 _uNPhytW0_ 0"
write (1,*)" InitState  112 _uDPhytS0_ 0"
write (1,*)" InitState  113 _uPPhytS0_ 0"
write (1,*)" InitState  114 _uNPhytS0_ 0"
write (1,*)" InitState  115 _uDPhytWM0_ 0"
write (1,*)" InitState  116 _uPPhytWM0_ 0"
write (1,*)" InitState  117 _uNPhytWM0_ 0"
write (1,*)" InitState  118 _uSiDiatWM0_ 0"
write (1,*)" InitState  119 _uDVeg0_ 0"
write (1,*)" InitState  120 _uPVeg0_ 0"
write (1,*)" InitState  121 _uNVeg0_ 0"
write (1,*)" InitState  122 _uDFish0_ 0"
write (1,*)" InitState  123 _uPFish0_ 0"
write (1,*)" InitState  124 _uNFish0_ 0"
write (1,*)" InitState  125 _uPPisc0_ 0"
write (1,*)" InitState  126 _uNPisc0_ 0"
write (1,*)" InitState  127 _uDTotM0_ 0"
write (1,*)" InitState  128 _uPTotM0_ 0"
write (1,*)" InitState  129 _uNTotM0_ 0"
write (1,*)" InitState  130 _uSiTotM0_ 0"
write (1,*)" InitState  131 _uDTotT0_ 0"
write (1,*)" InitState  132 _uPTotT0_ 0"
write (1,*)" InitState  133 _uNTotT0_ 0"
write (1,*)" InitState  134 _uSiTotT0_ 0"
write (1,*)" InitState  135 _sDExtTotT0_ 0"
write (1,*)" InitState  136 _sNExtTotT0_ 0"
write (1,*)" InitState  137 _sPExtTotT0_ 0"
write (1,*)" InitState  138 _sSiExtTotT0_ 0"
write (1,*)" state  0 _sDepthW_", sDepthW
write (1,*)" state  1 _sNH4W_", sNH4W
write (1,*)" state  2 _sNO3W_", sNO3W
write (1,*)" state  3 _sPO4W_", sPO4W
write (1,*)" state  4 _sPAIMW_", sPAIMW
write (1,*)" state  5 _sSiO2W_", sSiO2W
write (1,*)" state  6 _sO2W_", sO2W
write (1,*)" state  7 _sDDetW_", sDDetW
write (1,*)" state  8 _sNDetW_", sNDetW
write (1,*)" state  9 _sPDetW_", sPDetW
write (1,*)" state  10 _sSiDetW_", sSiDetW
write (1,*)" state  11 _sDIMW_", sDIMW
write (1,*)" state  12 _sDDiatW_", sDDiatW
write (1,*)" state  13 _sNDiatW_", sNDiatW
write (1,*)" state  14 _sPDiatW_", sPDiatW
write (1,*)" state  15 _sDGrenW_", sDGrenW
write (1,*)" state  16 _sNGrenW_", sNGrenW
write (1,*)" state  17 _sPGrenW_", sPGrenW
write (1,*)" state  18 _sDBlueW_", sDBlueW
write (1,*)" state  19 _sNBlueW_", sNBlueW
write (1,*)" state  20 _sPBlueW_", sPBlueW
write (1,*)" state  21 _sDZoo_", sDZoo
write (1,*)" state  22 _sNZoo_", sNZoo
write (1,*)" state  23 _sPZoo_", sPZoo
write (1,*)" state  24 _sDFiAd_", sDFiAd
write (1,*)" state  25 _sDFiJv_", sDFiJv
write (1,*)" state  26 _sNFiAd_", sNFiAd
write (1,*)" state  27 _sNFiJv_", sNFiJv
write (1,*)" state  28 _sPFiAd_", sPFiAd
write (1,*)" state  29 _sPFiJv_", sPFiJv
write (1,*)" state  30 _sDPisc_", sDPisc
write (1,*)" state  31 _sNH4S_", sNH4S
write (1,*)" state  32 _sNO3S_", sNO3S
write (1,*)" state  33 _sPO4S_", sPO4S
write (1,*)" state  34 _sPAIMS_", sPAIMS
write (1,*)" state  35 _sDDetS_", sDDetS
write (1,*)" state  36 _sNDetS_", sNDetS
write (1,*)" state  37 _sPDetS_", sPDetS
write (1,*)" state  38 _sSiDetS_", sSiDetS
write (1,*)" state  39 _sDHumS_", sDHumS
write (1,*)" state  40 _sNHumS_", sNHumS
write (1,*)" state  41 _sPHumS_", sPHumS
write (1,*)" state  42 _sDIMS_", sDIMS
write (1,*)" state  43 _sDDiatS_", sDDiatS
write (1,*)" state  44 _sNDiatS_", sNDiatS
write (1,*)" state  45 _sPDiatS_", sPDiatS
write (1,*)" state  46 _sDGrenS_", sDGrenS
write (1,*)" state  47 _sNGrenS_", sNGrenS
write (1,*)" state  48 _sPGrenS_", sPGrenS
write (1,*)" state  49 _sDBlueS_", sDBlueS
write (1,*)" state  50 _sNBlueS_", sNBlueS
write (1,*)" state  51 _sPBlueS_", sPBlueS
write (1,*)" state  52 _sDVeg_", sDVeg
write (1,*)" state  53 _sNVeg_", sNVeg
write (1,*)" state  54 _sPVeg_", sPVeg
write (1,*)" state  55 _sDBent_", sDBent
write (1,*)" state  56 _sNBent_", sNBent
write (1,*)" state  57 _sPBent_", sPBent
write (1,*)" state  58 _sDepthWM_", sDepthWM
write (1,*)" state  59 _sNH4WM_", sNH4WM
write (1,*)" state  60 _sNO3WM_", sNO3WM
write (1,*)" state  61 _sPO4WM_", sPO4WM
write (1,*)" state  62 _sPAIMWM_", sPAIMWM
write (1,*)" state  63 _sSiO2WM_", sSiO2WM
write (1,*)" state  64 _sO2WM_", sO2WM
write (1,*)" state  65 _sDDetWM_", sDDetWM
write (1,*)" state  66 _sNDetWM_", sNDetWM
write (1,*)" state  67 _sPDetWM_", sPDetWM
write (1,*)" state  68 _sSiDetWM_", sSiDetWM
write (1,*)" state  69 _sDIMWM_", sDIMWM
write (1,*)" state  70 _sDDiatWM_", sDDiatWM
write (1,*)" state  71 _sNDiatWM_", sNDiatWM
write (1,*)" state  72 _sPDiatWM_", sPDiatWM
write (1,*)" state  73 _sDGrenWM_", sDGrenWM
write (1,*)" state  74 _sNGrenWM_", sNGrenWM
write (1,*)" state  75 _sPGrenWM_", sPGrenWM
write (1,*)" state  76 _sDBlueWM_", sDBlueWM
write (1,*)" state  77 _sNBlueWM_", sNBlueWM
write (1,*)" state  78 _sPBlueWM_", sPBlueWM
write (1,*)" state  79 _sDZooM_", sDZooM
write (1,*)" state  80 _sNZooM_", sNZooM
write (1,*)" state  81 _sPZooM_", sPZooM
write (1,*)" state  82 _sNH4SM_", sNH4SM
write (1,*)" state  83 _sNO3SM_", sNO3SM
write (1,*)" state  84 _sPO4SM_", sPO4SM
write (1,*)" state  85 _sPAIMSM_", sPAIMSM
write (1,*)" state  86 _sDDetSM_", sDDetSM
write (1,*)" state  87 _sNDetSM_", sNDetSM
write (1,*)" state  88 _sPDetSM_", sPDetSM
write (1,*)" state  89 _sSiDetSM_", sSiDetSM
write (1,*)" state  90 _sDHumSM_", sDHumSM
write (1,*)" state  91 _sNHumSM_", sNHumSM
write (1,*)" state  92 _sPHumSM_", sPHumSM
write (1,*)" state  93 _sDIMSM_", sDIMSM
write (1,*)" state  94 _sDRootPhra_", sDRootPhra
write (1,*)" state  95 _sDShootPhra_", sDShootPhra
write (1,*)" state  96 _sNRootPhra_", sNRootPhra
write (1,*)" state  97 _sNShootPhra_", sNShootPhra
write (1,*)" state  98 _sPRootPhra_", sPRootPhra
write (1,*)" state  99 _sPShootPhra_", sPShootPhra
write (1,*)" state  100 _sDExtTotT_", sDExtTotT
write (1,*)" state  101 _sNExtTotT_", sNExtTotT
write (1,*)" state  102 _sPExtTotT_", sPExtTotT
write (1,*)" state  103 _sSiExtTotT_", sSiExtTotT
write (1,*)" Auxiliary  0 _sTime_", sTime
write (1,*)" Auxiliary  1 _TimeYears_", TimeYears
write (1,*)" Auxiliary  2 _Day_", Day
write (1,*)" Auxiliary  3 _Years_", Years
write (1,*)" Auxiliary  4 _uTm_", uTm
write (1,*)" Auxiliary  5 _uVWind_", uVWind
write (1,*)" Auxiliary  6 _ufDay_", ufDay
write (1,*)" Auxiliary  7 _uLDay_", uLDay
write (1,*)" Auxiliary  8 _uLOut_", uLOut
write (1,*)" Auxiliary  9 _uLPARSurf_", uLPARSurf
write (1,*)" Auxiliary  10 _aExtPhyt_", aExtPhyt
write (1,*)" Auxiliary  11 _aExtDet_", aExtDet
write (1,*)" Auxiliary  12 _aExtIM_", aExtIM
write (1,*)" Auxiliary  13 _aExtCoefOpen_", aExtCoefOpen
write (1,*)" Auxiliary  14 _uQInSeason_", uQInSeason
write (1,*)" Auxiliary  15 _uQEvSinus_", uQEvSinus
write (1,*)" Auxiliary  16 _uQEv_", uQEv
write (1,*)" Auxiliary  17 _uQInExtra_", uQInExtra
write (1,*)" Auxiliary  18 _uQOutExtra_", uQOutExtra
write (1,*)" Auxiliary  19 _uQIn_", uQIn
write (1,*)" Auxiliary  20 _uQOut_", uQOut
write (1,*)" Auxiliary  21 _uQDil_", uQDil
write (1,*)" Auxiliary  22 _ukDil_", ukDil
write (1,*)" Auxiliary  23 _ukDilWat_", ukDilWat
write (1,*)" Auxiliary  24 _ukOut_", ukOut
write (1,*)" Auxiliary  25 _uTauWat_", uTauWat
write (1,*)" Auxiliary  26 _uTauSubst_", uTauSubst
write (1,*)" Auxiliary  27 _vTranDepthW_", vTranDepthW
write (1,*)" Auxiliary  28 _akExchM_", akExchM
write (1,*)" Auxiliary  29 _afVolMarsh_", afVolMarsh
write (1,*)" Auxiliary  30 _akExchL_", akExchL
write (1,*)" Auxiliary  31 _oDPhytW_", oDPhytW
write (1,*)" Auxiliary  32 _oPPhytW_", oPPhytW
write (1,*)" Auxiliary  33 _oNPhytW_", oNPhytW
write (1,*)" Auxiliary  34 _aDPhytS_", aDPhytS
write (1,*)" Auxiliary  35 _aPPhytS_", aPPhytS
write (1,*)" Auxiliary  36 _aNPhytS_", aNPhytS
write (1,*)" Auxiliary  37 _oDOMW_", oDOMW
write (1,*)" Auxiliary  38 _oDSestW_", oDSestW
write (1,*)" Auxiliary  39 _oPOMW_", oPOMW
write (1,*)" Auxiliary  40 _oPSestW_", oPSestW
write (1,*)" Auxiliary  41 _oPInorgW_", oPInorgW
write (1,*)" Auxiliary  42 _oPTotW_", oPTotW
write (1,*)" Auxiliary  43 _oNDissW_", oNDissW
write (1,*)" Auxiliary  44 _oNOMW_", oNOMW
write (1,*)" Auxiliary  45 _oNSestW_", oNSestW
write (1,*)" Auxiliary  46 _oNkjW_", oNkjW
write (1,*)" Auxiliary  47 _oNTotW_", oNTotW
write (1,*)" Auxiliary  48 _bPorS_", bPorS
write (1,*)" Auxiliary  49 _bPorCorS_", bPorCorS
write (1,*)" Auxiliary  50 _aDTotS_", aDTotS
write (1,*)" Auxiliary  51 _aRhoTotS_", aRhoTotS
write (1,*)" Auxiliary  52 _aRhoSolidS_", aRhoSolidS
write (1,*)" Auxiliary  53 _afDTotS_", afDTotS
write (1,*)" Auxiliary  54 _afDOrgS_", afDOrgS
write (1,*)" Auxiliary  55 _afDetS_", afDetS
write (1,*)" Auxiliary  56 _afDetTotS_", afDetTotS
write (1,*)" Auxiliary  57 _aPInorgS_", aPInorgS
write (1,*)" Auxiliary  58 _aPTotAvailS_", aPTotAvailS
write (1,*)" Auxiliary  59 _aPTotS_", aPTotS
write (1,*)" Auxiliary  60 _afPInorgS_", afPInorgS
write (1,*)" Auxiliary  61 _afPTotS_", afPTotS
write (1,*)" Auxiliary  62 _afPO4S_", afPO4S
write (1,*)" Auxiliary  63 _oPO4S_", oPO4S
write (1,*)" Auxiliary  64 _aNDissS_", aNDissS
write (1,*)" Auxiliary  65 _aNkjAvailS_", aNkjAvailS
write (1,*)" Auxiliary  66 _aNkjS_", aNkjS
write (1,*)" Auxiliary  67 _aNTotAvailS_", aNTotAvailS
write (1,*)" Auxiliary  68 _aNTotS_", aNTotS
write (1,*)" Auxiliary  69 _afNInorgS_", afNInorgS
write (1,*)" Auxiliary  70 _afNTotS_", afNTotS
write (1,*)" Auxiliary  71 _oNO3S_", oNO3S
write (1,*)" Auxiliary  72 _oNH4S_", oNH4S
write (1,*)" Auxiliary  73 _oNDissS_", oNDissS
write (1,*)" Auxiliary  74 _rPDIMW_", rPDIMW
write (1,*)" Auxiliary  75 _rPDIMS_", rPDIMS
write (1,*)" Auxiliary  76 _rPDDetW_", rPDDetW
write (1,*)" Auxiliary  77 _rNDDetW_", rNDDetW
write (1,*)" Auxiliary  78 _rSiDDetW_", rSiDDetW
write (1,*)" Auxiliary  79 _rPDHumS_", rPDHumS
write (1,*)" Auxiliary  80 _rNDHumS_", rNDHumS
write (1,*)" Auxiliary  81 _rPDDetS_", rPDDetS
write (1,*)" Auxiliary  82 _rNDDetS_", rNDDetS
write (1,*)" Auxiliary  83 _rSiDDetS_", rSiDDetS
write (1,*)" Auxiliary  84 _oDPhytWM_", oDPhytWM
write (1,*)" Auxiliary  85 _oPPhytWM_", oPPhytWM
write (1,*)" Auxiliary  86 _oNPhytWM_", oNPhytWM
write (1,*)" Auxiliary  87 _oSiDiatWM_", oSiDiatWM
write (1,*)" Auxiliary  88 _oDOMWM_", oDOMWM
write (1,*)" Auxiliary  89 _oDSestWM_", oDSestWM
write (1,*)" Auxiliary  90 _oPOMWM_", oPOMWM
write (1,*)" Auxiliary  91 _oPSestWM_", oPSestWM
write (1,*)" Auxiliary  92 _oPInorgWM_", oPInorgWM
write (1,*)" Auxiliary  93 _oPTotWM_", oPTotWM
write (1,*)" Auxiliary  94 _oNDissWM_", oNDissWM
write (1,*)" Auxiliary  95 _oNOMWM_", oNOMWM
write (1,*)" Auxiliary  96 _oNSestWM_", oNSestWM
write (1,*)" Auxiliary  97 _oNkjWM_", oNkjWM
write (1,*)" Auxiliary  98 _oNTotWM_", oNTotWM
write (1,*)" Auxiliary  99 _bPorSM_", bPorSM
write (1,*)" Auxiliary  100 _bPorCorSM_", bPorCorSM
write (1,*)" Auxiliary  101 _aDTotSM_", aDTotSM
write (1,*)" Auxiliary  102 _aRhoTotSM_", aRhoTotSM
write (1,*)" Auxiliary  103 _aRhoSolidSM_", aRhoSolidSM
write (1,*)" Auxiliary  104 _afDTotSM_", afDTotSM
write (1,*)" Auxiliary  105 _afDOrgSM_", afDOrgSM
write (1,*)" Auxiliary  106 _afDetSM_", afDetSM
write (1,*)" Auxiliary  107 _afDetTotSM_", afDetTotSM
write (1,*)" Auxiliary  108 _aPInorgSM_", aPInorgSM
write (1,*)" Auxiliary  109 _aPTotAvailSM_", aPTotAvailSM
write (1,*)" Auxiliary  110 _aPTotSM_", aPTotSM
write (1,*)" Auxiliary  111 _afPInorgSM_", afPInorgSM
write (1,*)" Auxiliary  112 _afPTotSM_", afPTotSM
write (1,*)" Auxiliary  113 _afPO4SM_", afPO4SM
write (1,*)" Auxiliary  114 _oPO4SM_", oPO4SM
write (1,*)" Auxiliary  115 _aNDissSM_", aNDissSM
write (1,*)" Auxiliary  116 _aNkjAvailSM_", aNkjAvailSM
write (1,*)" Auxiliary  117 _aNkjSM_", aNkjSM
write (1,*)" Auxiliary  118 _aNTotAvailSM_", aNTotAvailSM
write (1,*)" Auxiliary  119 _aNTotSM_", aNTotSM
write (1,*)" Auxiliary  120 _afNInorgSM_", afNInorgSM
write (1,*)" Auxiliary  121 _afNTotSM_", afNTotSM
write (1,*)" Auxiliary  122 _oNO3SM_", oNO3SM
write (1,*)" Auxiliary  123 _oNH4SM_", oNH4SM
write (1,*)" Auxiliary  124 _oNDissSM_", oNDissSM
write (1,*)" Auxiliary  125 _rPDIMWM_", rPDIMWM
write (1,*)" Auxiliary  126 _rPDIMSM_", rPDIMSM
write (1,*)" Auxiliary  127 _rPDDetWM_", rPDDetWM
write (1,*)" Auxiliary  128 _rNDDetWM_", rNDDetWM
write (1,*)" Auxiliary  129 _rSiDDetWM_", rSiDDetWM
write (1,*)" Auxiliary  130 _rPDHumSM_", rPDHumSM
write (1,*)" Auxiliary  131 _rNDHumSM_", rNDHumSM
write (1,*)" Auxiliary  132 _rPDDetSM_", rPDDetSM
write (1,*)" Auxiliary  133 _rNDDetSM_", rNDDetSM
write (1,*)" Auxiliary  134 _rSiDDetSM_", rSiDDetSM
write (1,*)" Auxiliary  135 _aDTotM_", aDTotM
write (1,*)" Auxiliary  136 _aPTotM_", aPTotM
write (1,*)" Auxiliary  137 _aNTotM_", aNTotM
write (1,*)" Auxiliary  138 _aSiTotM_", aSiTotM
write (1,*)" Auxiliary  139 _iPPulse_", iPPulse
write (1,*)" Auxiliary  140 _uPLoadSeason_", uPLoadSeason
write (1,*)" Auxiliary  141 _uPLoad_", uPLoad
write (1,*)" Auxiliary  142 _uPLoadPO4_", uPLoadPO4
write (1,*)" Auxiliary  143 _uPLoadOrg_", uPLoadOrg
write (1,*)" Auxiliary  144 _uPLoadPhytTot_", uPLoadPhytTot
write (1,*)" Auxiliary  145 _uPLoadDet_", uPLoadDet
write (1,*)" Auxiliary  146 _uPLoadAIM_", uPLoadAIM
write (1,*)" Auxiliary  147 _iNPulse_", iNPulse
write (1,*)" Auxiliary  148 _uNLoadSeason_", uNLoadSeason
write (1,*)" Auxiliary  149 _uNLoadPhytTot_", uNLoadPhytTot
write (1,*)" Auxiliary  150 _uNLoad_", uNLoad
write (1,*)" Auxiliary  151 _uNLoadDet_", uNLoadDet
write (1,*)" Auxiliary  152 _uNLoadOrg_", uNLoadOrg
write (1,*)" Auxiliary  153 _uNLoadDiss_", uNLoadDiss
write (1,*)" Auxiliary  154 _uNLoadNH4_", uNLoadNH4
write (1,*)" Auxiliary  155 _uNLoadNO3_", uNLoadNO3
write (1,*)" Auxiliary  156 _uNTotIn_", uNTotIn
write (1,*)" Auxiliary  157 _uDLoadDet_", uDLoadDet
write (1,*)" Auxiliary  158 _uDLoadPhytTot_", uDLoadPhytTot
write (1,*)" Auxiliary  159 _uDLoadIM_", uDLoadIM
write (1,*)" Auxiliary  160 _uDLoad_", uDLoad
write (1,*)" Auxiliary  161 _uPTotIn_", uPTotIn
write (1,*)" Auxiliary  162 _uDLoadDiat_", uDLoadDiat
write (1,*)" Auxiliary  163 _uPLoadDiat_", uPLoadDiat
write (1,*)" Auxiliary  164 _uNLoadDiat_", uNLoadDiat
write (1,*)" Auxiliary  165 _uDLoadGren_", uDLoadGren
write (1,*)" Auxiliary  166 _uPLoadGren_", uPLoadGren
write (1,*)" Auxiliary  167 _uNLoadGren_", uNLoadGren
write (1,*)" Auxiliary  168 _uDLoadBlue_", uDLoadBlue
write (1,*)" Auxiliary  169 _uPLoadBlue_", uPLoadBlue
write (1,*)" Auxiliary  170 _uNLoadBlue_", uNLoadBlue
write (1,*)" Auxiliary  171 _wDDilIM_", wDDilIM
write (1,*)" Auxiliary  172 _wDDilDet_", wDDilDet
write (1,*)" Auxiliary  173 _wPDilPO4_", wPDilPO4
write (1,*)" Auxiliary  174 _wPDilDet_", wPDilDet
write (1,*)" Auxiliary  175 _wPDilAIM_", wPDilAIM
write (1,*)" Auxiliary  176 _wNDilNH4_", wNDilNH4
write (1,*)" Auxiliary  177 _wNDilNO3_", wNDilNO3
write (1,*)" Auxiliary  178 _wNDilDet_", wNDilDet
write (1,*)" Auxiliary  179 _wO2Inflow_", wO2Inflow
write (1,*)" Auxiliary  180 _wO2Outfl_", wO2Outfl
write (1,*)" Auxiliary  181 _wDDilDiat_", wDDilDiat
write (1,*)" Auxiliary  182 _wPDilDiat_", wPDilDiat
write (1,*)" Auxiliary  183 _wNDilDiat_", wNDilDiat
write (1,*)" Auxiliary  184 _wDDilGren_", wDDilGren
write (1,*)" Auxiliary  185 _wPDilGren_", wPDilGren
write (1,*)" Auxiliary  186 _wNDilGren_", wNDilGren
write (1,*)" Auxiliary  187 _wDDilBlue_", wDDilBlue
write (1,*)" Auxiliary  188 _wPDilBlue_", wPDilBlue
write (1,*)" Auxiliary  189 _wNDilBlue_", wNDilBlue
write (1,*)" Auxiliary  190 _wDDilPhyt_", wDDilPhyt
write (1,*)" Auxiliary  191 _wPDilPhyt_", wPDilPhyt
write (1,*)" Auxiliary  192 _wNDilPhyt_", wNDilPhyt
write (1,*)" Auxiliary  193 _wDOutflTot_", wDOutflTot
write (1,*)" Auxiliary  194 _wPOutflTot_", wPOutflTot
write (1,*)" Auxiliary  195 _wNOutflTot_", wNOutflTot
write (1,*)" Auxiliary  196 _wDTranDiat_", wDTranDiat
write (1,*)" Auxiliary  197 _wPTranDiat_", wPTranDiat
write (1,*)" Auxiliary  198 _wNTranDiat_", wNTranDiat
write (1,*)" Auxiliary  199 _wDTranGren_", wDTranGren
write (1,*)" Auxiliary  200 _wPTranGren_", wPTranGren
write (1,*)" Auxiliary  201 _wNTranGren_", wNTranGren
write (1,*)" Auxiliary  202 _wDTranBlue_", wDTranBlue
write (1,*)" Auxiliary  203 _wPTranBlue_", wPTranBlue
write (1,*)" Auxiliary  204 _wNTranBlue_", wNTranBlue
write (1,*)" Auxiliary  205 _wDTranPhyt_", wDTranPhyt
write (1,*)" Auxiliary  206 _wPTranPhyt_", wPTranPhyt
write (1,*)" Auxiliary  207 _wNTranPhyt_", wNTranPhyt
write (1,*)" Auxiliary  208 _uSiLoadSiO2_", uSiLoadSiO2
write (1,*)" Auxiliary  209 _uSiLoadDet_", uSiLoadDet
write (1,*)" Auxiliary  210 _uSiLoadDiat_", uSiLoadDiat
write (1,*)" Auxiliary  211 _uSiLoad_", uSiLoad
write (1,*)" Auxiliary  212 _wSiDilSiO2_", wSiDilSiO2
write (1,*)" Auxiliary  213 _wSiDilDet_", wSiDilDet
write (1,*)" Auxiliary  214 _wSiDilDiat_", wSiDilDiat
write (1,*)" Auxiliary  215 _wSiOutflTot_", wSiOutflTot
write (1,*)" Auxiliary  216 _wSiTranSiO2_", wSiTranSiO2
write (1,*)" Auxiliary  217 _wSiTranDetW_", wSiTranDetW
write (1,*)" Auxiliary  218 _tSiTranTotT_", tSiTranTotT
write (1,*)" Auxiliary  219 _wDTranZoo_", wDTranZoo
write (1,*)" Auxiliary  220 _wPTranZoo_", wPTranZoo
write (1,*)" Auxiliary  221 _wNTranZoo_", wNTranZoo
write (1,*)" Auxiliary  222 _wDTranIMW_", wDTranIMW
write (1,*)" Auxiliary  223 _wDTranDetW_", wDTranDetW
write (1,*)" Auxiliary  224 _wO2TranW_", wO2TranW
write (1,*)" Auxiliary  225 _wPTranPO4W_", wPTranPO4W
write (1,*)" Auxiliary  226 _wPTranAIMW_", wPTranAIMW
write (1,*)" Auxiliary  227 _wPTranDetW_", wPTranDetW
write (1,*)" Auxiliary  228 _wNTranNH4W_", wNTranNH4W
write (1,*)" Auxiliary  229 _wNTranNO3W_", wNTranNO3W
write (1,*)" Auxiliary  230 _wNTranDetW_", wNTranDetW
write (1,*)" Auxiliary  231 _wDDilTot_", wDDilTot
write (1,*)" Auxiliary  232 _wPDilTot_", wPDilTot
write (1,*)" Auxiliary  233 _wNDilTot_", wNDilTot
write (1,*)" Auxiliary  234 _wSiDilTot_", wSiDilTot
write (1,*)" Auxiliary  235 _tDTranTotT_", tDTranTotT
write (1,*)" Auxiliary  236 _tPTranTotT_", tPTranTotT
write (1,*)" Auxiliary  237 _tNTranTotT_", tNTranTotT
write (1,*)" Auxiliary  238 _wDExchIMM_", wDExchIMM
write (1,*)" Auxiliary  239 _wPExchPO4M_", wPExchPO4M
write (1,*)" Auxiliary  240 _wPExchAIMM_", wPExchAIMM
write (1,*)" Auxiliary  241 _wNExchNH4M_", wNExchNH4M
write (1,*)" Auxiliary  242 _wNExchNO3M_", wNExchNO3M
write (1,*)" Auxiliary  243 _wSiExchSiO2M_", wSiExchSiO2M
write (1,*)" Auxiliary  244 _wO2ExchM_", wO2ExchM
write (1,*)" Auxiliary  245 _wDExchDetM_", wDExchDetM
write (1,*)" Auxiliary  246 _wPExchDetM_", wPExchDetM
write (1,*)" Auxiliary  247 _wNExchDetM_", wNExchDetM
write (1,*)" Auxiliary  248 _wSiExchDetM_", wSiExchDetM
write (1,*)" Auxiliary  249 _wDExchDiatM_", wDExchDiatM
write (1,*)" Auxiliary  250 _wPExchDiatM_", wPExchDiatM
write (1,*)" Auxiliary  251 _wNExchDiatM_", wNExchDiatM
write (1,*)" Auxiliary  252 _wSiExchDiatM_", wSiExchDiatM
write (1,*)" Auxiliary  253 _wDExchGrenM_", wDExchGrenM
write (1,*)" Auxiliary  254 _wPExchGrenM_", wPExchGrenM
write (1,*)" Auxiliary  255 _wNExchGrenM_", wNExchGrenM
write (1,*)" Auxiliary  256 _wDExchBlueM_", wDExchBlueM
write (1,*)" Auxiliary  257 _wPExchBlueM_", wPExchBlueM
write (1,*)" Auxiliary  258 _wNExchBlueM_", wNExchBlueM
write (1,*)" Auxiliary  259 _wDExchZooM_", wDExchZooM
write (1,*)" Auxiliary  260 _wPExchZooM_", wPExchZooM
write (1,*)" Auxiliary  261 _wNExchZooM_", wNExchZooM
write (1,*)" Auxiliary  262 _wDExchIM_", wDExchIM
write (1,*)" Auxiliary  263 _wPExchPO4_", wPExchPO4
write (1,*)" Auxiliary  264 _wPExchAIM_", wPExchAIM
write (1,*)" Auxiliary  265 _wNExchNH4_", wNExchNH4
write (1,*)" Auxiliary  266 _wNExchNO3_", wNExchNO3
write (1,*)" Auxiliary  267 _wSiExchSiO2_", wSiExchSiO2
write (1,*)" Auxiliary  268 _wO2Exch_", wO2Exch
write (1,*)" Auxiliary  269 _wDExchDet_", wDExchDet
write (1,*)" Auxiliary  270 _wPExchDet_", wPExchDet
write (1,*)" Auxiliary  271 _wNExchDet_", wNExchDet
write (1,*)" Auxiliary  272 _wSiExchDet_", wSiExchDet
write (1,*)" Auxiliary  273 _wDExchDiat_", wDExchDiat
write (1,*)" Auxiliary  274 _wPExchDiat_", wPExchDiat
write (1,*)" Auxiliary  275 _wNExchDiat_", wNExchDiat
write (1,*)" Auxiliary  276 _wSiExchDiat_", wSiExchDiat
write (1,*)" Auxiliary  277 _wDExchGren_", wDExchGren
write (1,*)" Auxiliary  278 _wPExchGren_", wPExchGren
write (1,*)" Auxiliary  279 _wNExchGren_", wNExchGren
write (1,*)" Auxiliary  280 _wDExchBlue_", wDExchBlue
write (1,*)" Auxiliary  281 _wPExchBlue_", wPExchBlue
write (1,*)" Auxiliary  282 _wNExchBlue_", wNExchBlue
write (1,*)" Auxiliary  283 _wDExchZoo_", wDExchZoo
write (1,*)" Auxiliary  284 _wPExchZoo_", wPExchZoo
write (1,*)" Auxiliary  285 _wNExchZoo_", wNExchZoo
write (1,*)" Auxiliary  286 _tPInfPO4W_", tPInfPO4W
write (1,*)" Auxiliary  287 _tNInfNH4W_", tNInfNH4W
write (1,*)" Auxiliary  288 _tNInfNO3W_", tNInfNO3W
write (1,*)" Auxiliary  289 _tPInfPO4S_", tPInfPO4S
write (1,*)" Auxiliary  290 _tNInfNH4S_", tNInfNH4S
write (1,*)" Auxiliary  291 _tNInfNO3S_", tNInfNO3S
write (1,*)" Auxiliary  292 _tNH4LoadS_", tNH4LoadS
write (1,*)" Auxiliary  293 _tNO3LoadS_", tNO3LoadS
write (1,*)" Auxiliary  294 _uDErosIM_", uDErosIM
write (1,*)" Auxiliary  295 _uDErosIMS_", uDErosIMS
write (1,*)" Auxiliary  296 _uDErosIMW_", uDErosIMW
write (1,*)" Auxiliary  297 _uDErosOM_", uDErosOM
write (1,*)" Auxiliary  298 _uPErosOM_", uPErosOM
write (1,*)" Auxiliary  299 _uNErosOM_", uNErosOM
write (1,*)" Auxiliary  300 _uO2Sat_", uO2Sat
write (1,*)" Auxiliary  301 _kAer_", kAer
write (1,*)" Auxiliary  302 _uFunTmAer_", uFunTmAer
write (1,*)" Auxiliary  303 _aFunLemnAer_", aFunLemnAer
write (1,*)" Auxiliary  304 _tO2Aer_", tO2Aer
write (1,*)" Auxiliary  305 _uFunTmFish_", uFunTmFish
write (1,*)" Auxiliary  306 _tDTurbFish_", tDTurbFish
write (1,*)" Auxiliary  307 _tDTurbFishIM_", tDTurbFishIM
write (1,*)" Auxiliary  308 _aFunVegResus_", aFunVegResus
write (1,*)" Auxiliary  309 _aFunDimSusp_", aFunDimSusp
write (1,*)" Auxiliary  310 _tDResusTauDead_", tDResusTauDead
write (1,*)" Auxiliary  311 _tDResusBareDead_", tDResusBareDead
write (1,*)" Auxiliary  312 _tDResusDead_", tDResusDead
write (1,*)" Auxiliary  313 _tDResusIM_", tDResusIM
write (1,*)" Auxiliary  314 _tDResusDet_", tDResusDet
write (1,*)" Auxiliary  315 _akResusPhytRef_", akResusPhytRef
write (1,*)" Auxiliary  316 _tDResusPhytTot_", tDResusPhytTot
write (1,*)" Auxiliary  317 _tPResusDet_", tPResusDet
write (1,*)" Auxiliary  318 _tPResusPO4_", tPResusPO4
write (1,*)" Auxiliary  319 _tPResusAIM_", tPResusAIM
write (1,*)" Auxiliary  320 _tNResusNO3_", tNResusNO3
write (1,*)" Auxiliary  321 _tNResusNH4_", tNResusNH4
write (1,*)" Auxiliary  322 _tNResusDet_", tNResusDet
write (1,*)" Auxiliary  323 _tSiResusDet_", tSiResusDet
write (1,*)" Auxiliary  324 _aFunTauSetOM_", aFunTauSetOM
write (1,*)" Auxiliary  325 _aFunTauSetIM_", aFunTauSetIM
write (1,*)" Auxiliary  326 _uFunTmSet_", uFunTmSet
write (1,*)" Auxiliary  327 _uCorVSetIM_", uCorVSetIM
write (1,*)" Auxiliary  328 _tDSetIM_", tDSetIM
write (1,*)" Auxiliary  329 _tPSetAIM_", tPSetAIM
write (1,*)" Auxiliary  330 _uCorVSetDet_", uCorVSetDet
write (1,*)" Auxiliary  331 _tDSetDet_", tDSetDet
write (1,*)" Auxiliary  332 _tPSetDet_", tPSetDet
write (1,*)" Auxiliary  333 _tNSetDet_", tNSetDet
write (1,*)" Auxiliary  334 _tSiSetDet_", tSiSetDet
write (1,*)" Auxiliary  335 _kPMinDetW_", kPMinDetW
write (1,*)" Auxiliary  336 _kNMinDetW_", kNMinDetW
write (1,*)" Auxiliary  337 _kSiMinDetW_", kSiMinDetW
write (1,*)" Auxiliary  338 _uFunTmMinW_", uFunTmMinW
write (1,*)" Auxiliary  339 _wDMinDetW_", wDMinDetW
write (1,*)" Auxiliary  340 _wPMinDetW_", wPMinDetW
write (1,*)" Auxiliary  341 _wNMinDetW_", wNMinDetW
write (1,*)" Auxiliary  342 _wSiMinDetW_", wSiMinDetW
write (1,*)" Auxiliary  343 _aCorO2BOD_", aCorO2BOD
write (1,*)" Auxiliary  344 _wO2MinDetW_", wO2MinDetW
write (1,*)" Auxiliary  345 _wDDenitW_", wDDenitW
write (1,*)" Auxiliary  346 _wNDenitW_", wNDenitW
write (1,*)" Auxiliary  347 _uFunTmNitr_", uFunTmNitr
write (1,*)" Auxiliary  348 _aCorO2NitrW_", aCorO2NitrW
write (1,*)" Auxiliary  349 _wNNitrW_", wNNitrW
write (1,*)" Auxiliary  350 _wO2NitrW_", wO2NitrW
write (1,*)" Auxiliary  351 _kPMinDetS_", kPMinDetS
write (1,*)" Auxiliary  352 _kNMinDetS_", kNMinDetS
write (1,*)" Auxiliary  353 _kSiMinDetS_", kSiMinDetS
write (1,*)" Auxiliary  354 _uFunTmMinS_", uFunTmMinS
write (1,*)" Auxiliary  355 _tDMinDetS_", tDMinDetS
write (1,*)" Auxiliary  356 _tPMinDetS_", tPMinDetS
write (1,*)" Auxiliary  357 _tNMinDetS_", tNMinDetS
write (1,*)" Auxiliary  358 _tSiMinDetS_", tSiMinDetS
write (1,*)" Auxiliary  359 _uFunTmDif_", uFunTmDif
write (1,*)" Auxiliary  360 _akO2DifCor_", akO2DifCor
write (1,*)" Auxiliary  361 _tSOD_", tSOD
write (1,*)" Auxiliary  362 _aDepthOxySed_", aDepthOxySed
write (1,*)" Auxiliary  363 _afOxySed_", afOxySed
write (1,*)" Auxiliary  364 _tDMinOxyDetS_", tDMinOxyDetS
write (1,*)" Auxiliary  365 _tO2MinDetS_", tO2MinDetS
write (1,*)" Auxiliary  366 _tDDenitS_", tDDenitS
write (1,*)" Auxiliary  367 _tNDenitS_", tNDenitS
write (1,*)" Auxiliary  368 _tNNitrS_", tNNitrS
write (1,*)" Auxiliary  369 _tO2NitrS_", tO2NitrS
write (1,*)" Auxiliary  370 _tDMinHumS_", tDMinHumS
write (1,*)" Auxiliary  371 _tPMinHumS_", tPMinHumS
write (1,*)" Auxiliary  372 _tNMinHumS_", tNMinHumS
write (1,*)" Auxiliary  373 _aDepthDif_", aDepthDif
write (1,*)" Auxiliary  374 _tPDifPO4_", tPDifPO4
write (1,*)" Auxiliary  375 _tNDifNO3_", tNDifNO3
write (1,*)" Auxiliary  376 _tNDifNH4_", tNDifNH4
write (1,*)" Auxiliary  377 _tO2Dif_", tO2Dif
write (1,*)" Auxiliary  378 _tPDifGroundPO4_", tPDifGroundPO4
write (1,*)" Auxiliary  379 _tNDifGroundNO3_", tNDifGroundNO3
write (1,*)" Auxiliary  380 _tNDifGroundNH4_", tNDifGroundNH4
write (1,*)" Auxiliary  381 _aPAdsMaxW_", aPAdsMaxW
write (1,*)" Auxiliary  382 _aKPAdsW_", aKPAdsW
write (1,*)" Auxiliary  383 _aPIsoAdsW_", aPIsoAdsW
write (1,*)" Auxiliary  384 _aPEqIMW_", aPEqIMW
write (1,*)" Auxiliary  385 _wPSorpIMW_", wPSorpIMW
write (1,*)" Auxiliary  386 _aPAdsMaxS_", aPAdsMaxS
write (1,*)" Auxiliary  387 _aKPAdsS_", aKPAdsS
write (1,*)" Auxiliary  388 _aPIsoAdsS_", aPIsoAdsS
write (1,*)" Auxiliary  389 _aPEqIMS_", aPEqIMS
write (1,*)" Auxiliary  390 _tPSorpIMS_", tPSorpIMS
write (1,*)" Auxiliary  391 _tPChemPO4_", tPChemPO4
write (1,*)" Auxiliary  392 _wDAbioIMW_", wDAbioIMW
write (1,*)" Auxiliary  393 _wDAbioDetW_", wDAbioDetW
write (1,*)" Auxiliary  394 _tDAbioIMS_", tDAbioIMS
write (1,*)" Auxiliary  395 _tDAbioDetS_", tDAbioDetS
write (1,*)" Auxiliary  396 _tDAbioHumS_", tDAbioHumS
write (1,*)" Auxiliary  397 _tDAbioTotT_", tDAbioTotT
write (1,*)" Auxiliary  398 _wO2AbioW_", wO2AbioW
write (1,*)" Auxiliary  399 _wPAbioDetW_", wPAbioDetW
write (1,*)" Auxiliary  400 _wPAbioPO4W_", wPAbioPO4W
write (1,*)" Auxiliary  401 _wPAbioAIMW_", wPAbioAIMW
write (1,*)" Auxiliary  402 _tPAbioDetS_", tPAbioDetS
write (1,*)" Auxiliary  403 _tPAbioHumS_", tPAbioHumS
write (1,*)" Auxiliary  404 _tPAbioPO4S_", tPAbioPO4S
write (1,*)" Auxiliary  405 _tPAbioAIMS_", tPAbioAIMS
write (1,*)" Auxiliary  406 _tPAbioTotT_", tPAbioTotT
write (1,*)" Auxiliary  407 _wNAbioNH4W_", wNAbioNH4W
write (1,*)" Auxiliary  408 _wNAbioNO3W_", wNAbioNO3W
write (1,*)" Auxiliary  409 _wNAbioDetW_", wNAbioDetW
write (1,*)" Auxiliary  410 _tNAbioNH4S_", tNAbioNH4S
write (1,*)" Auxiliary  411 _tNAbioNO3S_", tNAbioNO3S
write (1,*)" Auxiliary  412 _tNAbioDetS_", tNAbioDetS
write (1,*)" Auxiliary  413 _tNAbioHumS_", tNAbioHumS
write (1,*)" Auxiliary  414 _tNAbioTotT_", tNAbioTotT
write (1,*)" Auxiliary  415 _wSiAbioSiO2W_", wSiAbioSiO2W
write (1,*)" Auxiliary  416 _wSiAbioDetW_", wSiAbioDetW
write (1,*)" Auxiliary  417 _tSiAbioDetS_", tSiAbioDetS
write (1,*)" Auxiliary  418 _tSiAbioTotT_", tSiAbioTotT
write (1,*)" Auxiliary  419 _uQEvPhra_", uQEvPhra
write (1,*)" Auxiliary  420 _tPEvPO4WM_", tPEvPO4WM
write (1,*)" Auxiliary  421 _tNEvNH4WM_", tNEvNH4WM
write (1,*)" Auxiliary  422 _tNEvNO3WM_", tNEvNO3WM
write (1,*)" Auxiliary  423 _tPInfPO4WM_", tPInfPO4WM
write (1,*)" Auxiliary  424 _tNInfNH4WM_", tNInfNH4WM
write (1,*)" Auxiliary  425 _tNInfNO3WM_", tNInfNO3WM
write (1,*)" Auxiliary  426 _tPInfPO4SM_", tPInfPO4SM
write (1,*)" Auxiliary  427 _tNInfNH4SM_", tNInfNH4SM
write (1,*)" Auxiliary  428 _tNInfNO3SM_", tNInfNO3SM
write (1,*)" Auxiliary  429 _tO2AerM_", tO2AerM
write (1,*)" Auxiliary  430 _tDSetIMM_", tDSetIMM
write (1,*)" Auxiliary  431 _tPSetAIMM_", tPSetAIMM
write (1,*)" Auxiliary  432 _tDSetDetM_", tDSetDetM
write (1,*)" Auxiliary  433 _tPSetDetM_", tPSetDetM
write (1,*)" Auxiliary  434 _tNSetDetM_", tNSetDetM
write (1,*)" Auxiliary  435 _tSiSetDetM_", tSiSetDetM
write (1,*)" Auxiliary  436 _tDSetDiatM_", tDSetDiatM
write (1,*)" Auxiliary  437 _tPSetDiatM_", tPSetDiatM
write (1,*)" Auxiliary  438 _tNSetDiatM_", tNSetDiatM
write (1,*)" Auxiliary  439 _tSiSetDiatM_", tSiSetDiatM
write (1,*)" Auxiliary  440 _tDSetGrenM_", tDSetGrenM
write (1,*)" Auxiliary  441 _tPSetGrenM_", tPSetGrenM
write (1,*)" Auxiliary  442 _tNSetGrenM_", tNSetGrenM
write (1,*)" Auxiliary  443 _tDSetBlueM_", tDSetBlueM
write (1,*)" Auxiliary  444 _tPSetBlueM_", tPSetBlueM
write (1,*)" Auxiliary  445 _tNSetBlueM_", tNSetBlueM
write (1,*)" Auxiliary  446 _tDSetPhytM_", tDSetPhytM
write (1,*)" Auxiliary  447 _tPSetPhytM_", tPSetPhytM
write (1,*)" Auxiliary  448 _tNSetPhytM_", tNSetPhytM
write (1,*)" Auxiliary  449 _tDSetTotM_", tDSetTotM
write (1,*)" Auxiliary  450 _wDMinDetWM_", wDMinDetWM
write (1,*)" Auxiliary  451 _wPMinDetWM_", wPMinDetWM
write (1,*)" Auxiliary  452 _wNMinDetWM_", wNMinDetWM
write (1,*)" Auxiliary  453 _wSiMinDetWM_", wSiMinDetWM
write (1,*)" Auxiliary  454 _aCorO2BODM_", aCorO2BODM
write (1,*)" Auxiliary  455 _wO2MinDetWM_", wO2MinDetWM
write (1,*)" Auxiliary  456 _wDDenitWM_", wDDenitWM
write (1,*)" Auxiliary  457 _wNDenitWM_", wNDenitWM
write (1,*)" Auxiliary  458 _aCorO2NitrWM_", aCorO2NitrWM
write (1,*)" Auxiliary  459 _wNNitrWM_", wNNitrWM
write (1,*)" Auxiliary  460 _wO2NitrWM_", wO2NitrWM
write (1,*)" Auxiliary  461 _tDMinDetSM_", tDMinDetSM
write (1,*)" Auxiliary  462 _tPMinDetSM_", tPMinDetSM
write (1,*)" Auxiliary  463 _tNMinDetSM_", tNMinDetSM
write (1,*)" Auxiliary  464 _tSiMinDetSM_", tSiMinDetSM
write (1,*)" Auxiliary  465 _akO2DifCorM_", akO2DifCorM
write (1,*)" Auxiliary  466 _tSODM_", tSODM
write (1,*)" Auxiliary  467 _aDepthOxySedM_", aDepthOxySedM
write (1,*)" Auxiliary  468 _afOxySedM_", afOxySedM
write (1,*)" Auxiliary  469 _tDMinOxyDetSM_", tDMinOxyDetSM
write (1,*)" Auxiliary  470 _tO2MinDetSM_", tO2MinDetSM
write (1,*)" Auxiliary  471 _tDDenitSM_", tDDenitSM
write (1,*)" Auxiliary  472 _tNDenitSM_", tNDenitSM
write (1,*)" Auxiliary  473 _tNNitrSM_", tNNitrSM
write (1,*)" Auxiliary  474 _tO2NitrSM_", tO2NitrSM
write (1,*)" Auxiliary  475 _tDMinHumSM_", tDMinHumSM
write (1,*)" Auxiliary  476 _tPMinHumSM_", tPMinHumSM
write (1,*)" Auxiliary  477 _tNMinHumSM_", tNMinHumSM
write (1,*)" Auxiliary  478 _aDepthDifM_", aDepthDifM
write (1,*)" Auxiliary  479 _tPDifPO4M_", tPDifPO4M
write (1,*)" Auxiliary  480 _tNDifNO3M_", tNDifNO3M
write (1,*)" Auxiliary  481 _tNDifNH4M_", tNDifNH4M
write (1,*)" Auxiliary  482 _tO2DifM_", tO2DifM
write (1,*)" Auxiliary  483 _tPDifGroundPO4M_", tPDifGroundPO4M
write (1,*)" Auxiliary  484 _tNDifGroundNO3M_", tNDifGroundNO3M
write (1,*)" Auxiliary  485 _tNDifGroundNH4M_", tNDifGroundNH4M
write (1,*)" Auxiliary  486 _aPAdsMaxWM_", aPAdsMaxWM
write (1,*)" Auxiliary  487 _aKPAdsWM_", aKPAdsWM
write (1,*)" Auxiliary  488 _aPIsoAdsWM_", aPIsoAdsWM
write (1,*)" Auxiliary  489 _aPEqIMWM_", aPEqIMWM
write (1,*)" Auxiliary  490 _wPSorpIMWM_", wPSorpIMWM
write (1,*)" Auxiliary  491 _aPAdsMaxSM_", aPAdsMaxSM
write (1,*)" Auxiliary  492 _aKPAdsSM_", aKPAdsSM
write (1,*)" Auxiliary  493 _aPIsoAdsSM_", aPIsoAdsSM
write (1,*)" Auxiliary  494 _aPEqIMSM_", aPEqIMSM
write (1,*)" Auxiliary  495 _tPSorpIMSM_", tPSorpIMSM
write (1,*)" Auxiliary  496 _tPChemPO4M_", tPChemPO4M
write (1,*)" Auxiliary  497 _aDayInitVeg_", aDayInitVeg
write (1,*)" Auxiliary  498 _bfRootVeg_", bfRootVeg
write (1,*)" Auxiliary  499 _bfShootVeg_", bfShootVeg
write (1,*)" Auxiliary  500 _aDRootVeg_", aDRootVeg
write (1,*)" Auxiliary  501 _aDShootVeg_", aDShootVeg
write (1,*)" Auxiliary  502 _aDEmergVeg_", aDEmergVeg
write (1,*)" Auxiliary  503 _aDFloatVeg_", aDFloatVeg
write (1,*)" Auxiliary  504 _bfSubVeg_", bfSubVeg
write (1,*)" Auxiliary  505 _aDSubVeg_", aDSubVeg
write (1,*)" Auxiliary  506 _aExtVeg_", aExtVeg
write (1,*)" Auxiliary  507 _aDepth1Veg_", aDepth1Veg
write (1,*)" Auxiliary  508 _aDepth2Veg_", aDepth2Veg
write (1,*)" Auxiliary  509 _afCovSurfVeg_", afCovSurfVeg
write (1,*)" Auxiliary  510 _afCovEmergVeg_", afCovEmergVeg
write (1,*)" Auxiliary  511 _aCovVeg_", aCovVeg
write (1,*)" Auxiliary  512 _aDVeg_", aDVeg
write (1,*)" Auxiliary  513 _aPVeg_", aPVeg
write (1,*)" Auxiliary  514 _aNVeg_", aNVeg
write (1,*)" Auxiliary  515 _aExtCoef_", aExtCoef
write (1,*)" Auxiliary  516 _aLPARBot_", aLPARBot
write (1,*)" Auxiliary  517 _rPDVeg_", rPDVeg
write (1,*)" Auxiliary  518 _rNDVeg_", rNDVeg
write (1,*)" Auxiliary  519 _tDMigrVeg_", tDMigrVeg
write (1,*)" Auxiliary  520 _tPMigrVeg_", tPMigrVeg
write (1,*)" Auxiliary  521 _tNMigrVeg_", tNMigrVeg
write (1,*)" Auxiliary  522 _uFunTmProdVeg_", uFunTmProdVeg
write (1,*)" Auxiliary  523 _uFunTmRespVeg_", uFunTmRespVeg
write (1,*)" Auxiliary  524 _afPUptVegS_", afPUptVegS
write (1,*)" Auxiliary  525 _afNUptVegS_", afNUptVegS
write (1,*)" Auxiliary  526 _aVPUptMaxCrVeg_", aVPUptMaxCrVeg
write (1,*)" Auxiliary  527 _aVPUptVegW_", aVPUptVegW
write (1,*)" Auxiliary  528 _aVPUptVegS_", aVPUptVegS
write (1,*)" Auxiliary  529 _tPUptVegW_", tPUptVegW
write (1,*)" Auxiliary  530 _tPUptVegS_", tPUptVegS
write (1,*)" Auxiliary  531 _tPUptVeg_", tPUptVeg
write (1,*)" Auxiliary  532 _aVNUptMaxCrVeg_", aVNUptMaxCrVeg
write (1,*)" Auxiliary  533 _ahNUptVeg_", ahNUptVeg
write (1,*)" Auxiliary  534 _aVNUptVegW_", aVNUptVegW
write (1,*)" Auxiliary  535 _afNH4UptVegW_", afNH4UptVegW
write (1,*)" Auxiliary  536 _tNUptVegW_", tNUptVegW
write (1,*)" Auxiliary  537 _tNUptNH4VegW_", tNUptNH4VegW
write (1,*)" Auxiliary  538 _tNUptNO3VegW_", tNUptNO3VegW
write (1,*)" Auxiliary  539 _aVNUptVegS_", aVNUptVegS
write (1,*)" Auxiliary  540 _tNUptVegS_", tNUptVegS
write (1,*)" Auxiliary  541 _afNH4UptVegS_", afNH4UptVegS
write (1,*)" Auxiliary  542 _tNUptNH4VegS_", tNUptNH4VegS
write (1,*)" Auxiliary  543 _tNUptNO3VegS_", tNUptNO3VegS
write (1,*)" Auxiliary  544 _tNUptVeg_", tNUptVeg
write (1,*)" Auxiliary  545 _aLPAR1Veg_", aLPAR1Veg
write (1,*)" Auxiliary  546 _aLPAR2Veg_", aLPAR2Veg
write (1,*)" Auxiliary  547 _uhLVeg_", uhLVeg
write (1,*)" Auxiliary  548 _aLLimShootVeg_", aLLimShootVeg
write (1,*)" Auxiliary  549 _aMuTmLVeg_", aMuTmLVeg
write (1,*)" Auxiliary  550 _aPLimVeg_", aPLimVeg
write (1,*)" Auxiliary  551 _aNLimVeg_", aNLimVeg
write (1,*)" Auxiliary  552 _aNutLimVeg_", aNutLimVeg
write (1,*)" Auxiliary  553 _aMuVeg_", aMuVeg
write (1,*)" Auxiliary  554 _bkMortVeg_", bkMortVeg
write (1,*)" Auxiliary  555 _akDIncrVeg_", akDIncrVeg
write (1,*)" Auxiliary  556 _tDEnvVeg_", tDEnvVeg
write (1,*)" Auxiliary  557 _tDEnvProdVeg_", tDEnvProdVeg
write (1,*)" Auxiliary  558 _tDProdVeg_", tDProdVeg
write (1,*)" Auxiliary  559 _tDProdSubVeg_", tDProdSubVeg
write (1,*)" Auxiliary  560 _tDRespVeg_", tDRespVeg
write (1,*)" Auxiliary  561 _tDEnvMortVeg_", tDEnvMortVeg
write (1,*)" Auxiliary  562 _tDMortVeg_", tDMortVeg
write (1,*)" Auxiliary  563 _tDMortVegW_", tDMortVegW
write (1,*)" Auxiliary  564 _tDMortVegS_", tDMortVegS
write (1,*)" Auxiliary  565 _tDGrazVegBird_", tDGrazVegBird
write (1,*)" Auxiliary  566 _bkManVeg_", bkManVeg
write (1,*)" Auxiliary  567 _tDManVeg_", tDManVeg
write (1,*)" Auxiliary  568 _tPManVeg_", tPManVeg
write (1,*)" Auxiliary  569 _tNManVeg_", tNManVeg
write (1,*)" Auxiliary  570 _tDBedVeg_", tDBedVeg
write (1,*)" Auxiliary  571 _tO2ProdVeg_", tO2ProdVeg
write (1,*)" Auxiliary  572 _tO2RespVegW_", tO2RespVegW
write (1,*)" Auxiliary  573 _tO2RespVegS_", tO2RespVegS
write (1,*)" Auxiliary  574 _tO2ProdVegS_", tO2ProdVegS
write (1,*)" Auxiliary  575 _tO2ProdVegW_", tO2ProdVegW
write (1,*)" Auxiliary  576 _tO2UptNO3VegW_", tO2UptNO3VegW
write (1,*)" Auxiliary  577 _tO2UptNO3VegS_", tO2UptNO3VegS
write (1,*)" Auxiliary  578 _tPExcrVeg_", tPExcrVeg
write (1,*)" Auxiliary  579 _tPExcrVegS_", tPExcrVegS
write (1,*)" Auxiliary  580 _tPExcrVegW_", tPExcrVegW
write (1,*)" Auxiliary  581 _tPMortVeg_", tPMortVeg
write (1,*)" Auxiliary  582 _tPMortVegPO4_", tPMortVegPO4
write (1,*)" Auxiliary  583 _tPMortVegPO4S_", tPMortVegPO4S
write (1,*)" Auxiliary  584 _tPMortVegPO4W_", tPMortVegPO4W
write (1,*)" Auxiliary  585 _tPMortVegDet_", tPMortVegDet
write (1,*)" Auxiliary  586 _tPMortVegDetW_", tPMortVegDetW
write (1,*)" Auxiliary  587 _tPMortVegDetS_", tPMortVegDetS
write (1,*)" Auxiliary  588 _tPGrazVegBird_", tPGrazVegBird
write (1,*)" Auxiliary  589 _tPBedVeg_", tPBedVeg
write (1,*)" Auxiliary  590 _tNExcrVeg_", tNExcrVeg
write (1,*)" Auxiliary  591 _tNExcrVegS_", tNExcrVegS
write (1,*)" Auxiliary  592 _tNExcrVegW_", tNExcrVegW
write (1,*)" Auxiliary  593 _tNMortVeg_", tNMortVeg
write (1,*)" Auxiliary  594 _tNMortVegNH4_", tNMortVegNH4
write (1,*)" Auxiliary  595 _tNMortVegNH4S_", tNMortVegNH4S
write (1,*)" Auxiliary  596 _tNMortVegNH4W_", tNMortVegNH4W
write (1,*)" Auxiliary  597 _tNMortVegDet_", tNMortVegDet
write (1,*)" Auxiliary  598 _tNMortVegDetW_", tNMortVegDetW
write (1,*)" Auxiliary  599 _tNMortVegDetS_", tNMortVegDetS
write (1,*)" Auxiliary  600 _tNGrazVegBird_", tNGrazVegBird
write (1,*)" Auxiliary  601 _tNBedVeg_", tNBedVeg
write (1,*)" Auxiliary  602 _tDAssVegBird_", tDAssVegBird
write (1,*)" Auxiliary  603 _tDEgesBird_", tDEgesBird
write (1,*)" Auxiliary  604 _tPAssVegBird_", tPAssVegBird
write (1,*)" Auxiliary  605 _tPEgesBird_", tPEgesBird
write (1,*)" Auxiliary  606 _tPEgesBirdPO4_", tPEgesBirdPO4
write (1,*)" Auxiliary  607 _tPEgesBirdDet_", tPEgesBirdDet
write (1,*)" Auxiliary  608 _tNAssVegBird_", tNAssVegBird
write (1,*)" Auxiliary  609 _tNEgesBird_", tNEgesBird
write (1,*)" Auxiliary  610 _tNEgesBirdNH4_", tNEgesBirdNH4
write (1,*)" Auxiliary  611 _tNEgesBirdDet_", tNEgesBirdDet
write (1,*)" Auxiliary  612 _wDBedDetW_", wDBedDetW
write (1,*)" Auxiliary  613 _tDBedDetS_", tDBedDetS
write (1,*)" Auxiliary  614 _tDBedTotT_", tDBedTotT
write (1,*)" Auxiliary  615 _wPBedPO4W_", wPBedPO4W
write (1,*)" Auxiliary  616 _wPBedDetW_", wPBedDetW
write (1,*)" Auxiliary  617 _tPBedPO4S_", tPBedPO4S
write (1,*)" Auxiliary  618 _tPBedDetS_", tPBedDetS
write (1,*)" Auxiliary  619 _tPBedTotT_", tPBedTotT
write (1,*)" Auxiliary  620 _wNBedNH4W_", wNBedNH4W
write (1,*)" Auxiliary  621 _wNBedNO3W_", wNBedNO3W
write (1,*)" Auxiliary  622 _wNBedDetW_", wNBedDetW
write (1,*)" Auxiliary  623 _tNBedNH4S_", tNBedNH4S
write (1,*)" Auxiliary  624 _tNBedNO3S_", tNBedNO3S
write (1,*)" Auxiliary  625 _tNBedDetS_", tNBedDetS
write (1,*)" Auxiliary  626 _tNBedTotT_", tNBedTotT
write (1,*)" Auxiliary  627 _tO2BedW_", tO2BedW
write (1,*)" Auxiliary  628 _tO2BedS_", tO2BedS
write (1,*)" Auxiliary  629 _UseLoss_", UseLoss
write (1,*)" Auxiliary  630 _uFunTmLoss_", uFunTmLoss
write (1,*)" Auxiliary  631 _rPDBlueW_", rPDBlueW
write (1,*)" Auxiliary  632 _rNDBlueW_", rNDBlueW
write (1,*)" Auxiliary  633 _rPDBlueS_", rPDBlueS
write (1,*)" Auxiliary  634 _rNDBlueS_", rNDBlueS
write (1,*)" Auxiliary  635 _uFunTmBlue_", uFunTmBlue
write (1,*)" Auxiliary  636 _uFunTmProdBlue_", uFunTmProdBlue
write (1,*)" Auxiliary  637 _uFunTmRespBlue_", uFunTmRespBlue
write (1,*)" Auxiliary  638 _aVPUptMaxCrBlue_", aVPUptMaxCrBlue
write (1,*)" Auxiliary  639 _aVPUptBlue_", aVPUptBlue
write (1,*)" Auxiliary  640 _wPUptBlue_", wPUptBlue
write (1,*)" Auxiliary  641 _aVNUptMaxCrBlue_", aVNUptMaxCrBlue
write (1,*)" Auxiliary  642 _ahNUptBlue_", ahNUptBlue
write (1,*)" Auxiliary  643 _aVNUptBlue_", aVNUptBlue
write (1,*)" Auxiliary  644 _wNUptBlue_", wNUptBlue
write (1,*)" Auxiliary  645 _afNH4UptBlue_", afNH4UptBlue
write (1,*)" Auxiliary  646 _wNUptNH4Blue_", wNUptNH4Blue
write (1,*)" Auxiliary  647 _wNUptNO3Blue_", wNUptNO3Blue
write (1,*)" Auxiliary  648 _uMuMaxTmBlue_", uMuMaxTmBlue
write (1,*)" Auxiliary  649 _aPLimBlue_", aPLimBlue
write (1,*)" Auxiliary  650 _aNLimBlue_", aNLimBlue
write (1,*)" Auxiliary  651 _aSiLimBlue_", aSiLimBlue
write (1,*)" Auxiliary  652 _aLLimBlue_", aLLimBlue
write (1,*)" Auxiliary  653 _aMuTmLBlue_", aMuTmLBlue
write (1,*)" Auxiliary  654 _aNutLimBlue_", aNutLimBlue
write (1,*)" Auxiliary  655 _aMuBlue_", aMuBlue
write (1,*)" Auxiliary  656 _wDAssBlue_", wDAssBlue
write (1,*)" Auxiliary  657 _rChDBlue_", rChDBlue
write (1,*)" Auxiliary  658 _oChlaBlue_", oChlaBlue
write (1,*)" Auxiliary  659 _aExtChBlue_", aExtChBlue
write (1,*)" Auxiliary  660 _ukDRespTmBlue_", ukDRespTmBlue
write (1,*)" Auxiliary  661 _wDRespBlueW_", wDRespBlueW
write (1,*)" Auxiliary  662 _ukLossTmBlue_", ukLossTmBlue
write (1,*)" Auxiliary  663 _wDLossBlue_", wDLossBlue
write (1,*)" Auxiliary  664 _wDMortBlueW_", wDMortBlueW
write (1,*)" Auxiliary  665 _uCorVSetBlue_", uCorVSetBlue
write (1,*)" Auxiliary  666 _tDSetBlue_", tDSetBlue
write (1,*)" Auxiliary  667 _tDResusBlue_", tDResusBlue
write (1,*)" Auxiliary  668 _tDRespBlueS_", tDRespBlueS
write (1,*)" Auxiliary  669 _tDMortBlueS_", tDMortBlueS
write (1,*)" Auxiliary  670 _ukDDecBlue_", ukDDecBlue
write (1,*)" Auxiliary  671 _wPExcrBlueW_", wPExcrBlueW
write (1,*)" Auxiliary  672 _wPLossBlue_", wPLossBlue
write (1,*)" Auxiliary  673 _wPMortBlueW_", wPMortBlueW
write (1,*)" Auxiliary  674 _tPSetBlue_", tPSetBlue
write (1,*)" Auxiliary  675 _tPResusBlue_", tPResusBlue
write (1,*)" Auxiliary  676 _tPExcrBlueS_", tPExcrBlueS
write (1,*)" Auxiliary  677 _tPMortBlueS_", tPMortBlueS
write (1,*)" Auxiliary  678 _wNExcrBlueW_", wNExcrBlueW
write (1,*)" Auxiliary  679 _wNLossBlue_", wNLossBlue
write (1,*)" Auxiliary  680 _wNMortBlueW_", wNMortBlueW
write (1,*)" Auxiliary  681 _tNSetBlue_", tNSetBlue
write (1,*)" Auxiliary  682 _tNResusBlue_", tNResusBlue
write (1,*)" Auxiliary  683 _tNExcrBlueS_", tNExcrBlueS
write (1,*)" Auxiliary  684 _tNMortBlueS_", tNMortBlueS
write (1,*)" Auxiliary  685 _wDPrimBlueW_", wDPrimBlueW
write (1,*)" Auxiliary  686 _wPPrimBlueW_", wPPrimBlueW
write (1,*)" Auxiliary  687 _wNPrimBlueW_", wNPrimBlueW
write (1,*)" Auxiliary  688 _tDPrimBlueS_", tDPrimBlueS
write (1,*)" Auxiliary  689 _tPPrimBlueS_", tPPrimBlueS
write (1,*)" Auxiliary  690 _tNPrimBlueS_", tNPrimBlueS
write (1,*)" Auxiliary  691 _rPDGrenW_", rPDGrenW
write (1,*)" Auxiliary  692 _rNDGrenW_", rNDGrenW
write (1,*)" Auxiliary  693 _rPDGrenS_", rPDGrenS
write (1,*)" Auxiliary  694 _rNDGrenS_", rNDGrenS
write (1,*)" Auxiliary  695 _uFunTmGren_", uFunTmGren
write (1,*)" Auxiliary  696 _uFunTmProdGren_", uFunTmProdGren
write (1,*)" Auxiliary  697 _uFunTmRespGren_", uFunTmRespGren
write (1,*)" Auxiliary  698 _aVPUptMaxCrGren_", aVPUptMaxCrGren
write (1,*)" Auxiliary  699 _aVPUptGren_", aVPUptGren
write (1,*)" Auxiliary  700 _wPUptGren_", wPUptGren
write (1,*)" Auxiliary  701 _aVNUptMaxCrGren_", aVNUptMaxCrGren
write (1,*)" Auxiliary  702 _ahNUptGren_", ahNUptGren
write (1,*)" Auxiliary  703 _aVNUptGren_", aVNUptGren
write (1,*)" Auxiliary  704 _wNUptGren_", wNUptGren
write (1,*)" Auxiliary  705 _afNH4UptGren_", afNH4UptGren
write (1,*)" Auxiliary  706 _wNUptNH4Gren_", wNUptNH4Gren
write (1,*)" Auxiliary  707 _wNUptNO3Gren_", wNUptNO3Gren
write (1,*)" Auxiliary  708 _uMuMaxTmGren_", uMuMaxTmGren
write (1,*)" Auxiliary  709 _aPLimGren_", aPLimGren
write (1,*)" Auxiliary  710 _aNLimGren_", aNLimGren
write (1,*)" Auxiliary  711 _aSiLimGren_", aSiLimGren
write (1,*)" Auxiliary  712 _aLLimGren_", aLLimGren
write (1,*)" Auxiliary  713 _aMuTmLGren_", aMuTmLGren
write (1,*)" Auxiliary  714 _aNutLimGren_", aNutLimGren
write (1,*)" Auxiliary  715 _aMuGren_", aMuGren
write (1,*)" Auxiliary  716 _wDAssGren_", wDAssGren
write (1,*)" Auxiliary  717 _rChDGren_", rChDGren
write (1,*)" Auxiliary  718 _oChlaGren_", oChlaGren
write (1,*)" Auxiliary  719 _aExtChGren_", aExtChGren
write (1,*)" Auxiliary  720 _ukDRespTmGren_", ukDRespTmGren
write (1,*)" Auxiliary  721 _wDRespGrenW_", wDRespGrenW
write (1,*)" Auxiliary  722 _ukLossTmGren_", ukLossTmGren
write (1,*)" Auxiliary  723 _wDLossGren_", wDLossGren
write (1,*)" Auxiliary  724 _wDMortGrenW_", wDMortGrenW
write (1,*)" Auxiliary  725 _uCorVSetGren_", uCorVSetGren
write (1,*)" Auxiliary  726 _tDSetGren_", tDSetGren
write (1,*)" Auxiliary  727 _tDResusGren_", tDResusGren
write (1,*)" Auxiliary  728 _tDRespGrenS_", tDRespGrenS
write (1,*)" Auxiliary  729 _tDMortGrenS_", tDMortGrenS
write (1,*)" Auxiliary  730 _ukDDecGren_", ukDDecGren
write (1,*)" Auxiliary  731 _wPExcrGrenW_", wPExcrGrenW
write (1,*)" Auxiliary  732 _wPLossGren_", wPLossGren
write (1,*)" Auxiliary  733 _wPMortGrenW_", wPMortGrenW
write (1,*)" Auxiliary  734 _tPSetGren_", tPSetGren
write (1,*)" Auxiliary  735 _tPResusGren_", tPResusGren
write (1,*)" Auxiliary  736 _tPExcrGrenS_", tPExcrGrenS
write (1,*)" Auxiliary  737 _tPMortGrenS_", tPMortGrenS
write (1,*)" Auxiliary  738 _wNExcrGrenW_", wNExcrGrenW
write (1,*)" Auxiliary  739 _wNLossGren_", wNLossGren
write (1,*)" Auxiliary  740 _wNMortGrenW_", wNMortGrenW
write (1,*)" Auxiliary  741 _tNSetGren_", tNSetGren
write (1,*)" Auxiliary  742 _tNResusGren_", tNResusGren
write (1,*)" Auxiliary  743 _tNExcrGrenS_", tNExcrGrenS
write (1,*)" Auxiliary  744 _tNMortGrenS_", tNMortGrenS
write (1,*)" Auxiliary  745 _wDPrimGrenW_", wDPrimGrenW
write (1,*)" Auxiliary  746 _wPPrimGrenW_", wPPrimGrenW
write (1,*)" Auxiliary  747 _wNPrimGrenW_", wNPrimGrenW
write (1,*)" Auxiliary  748 _tDPrimGrenS_", tDPrimGrenS
write (1,*)" Auxiliary  749 _tPPrimGrenS_", tPPrimGrenS
write (1,*)" Auxiliary  750 _tNPrimGrenS_", tNPrimGrenS
write (1,*)" Auxiliary  751 _rPDDiatW_", rPDDiatW
write (1,*)" Auxiliary  752 _rNDDiatW_", rNDDiatW
write (1,*)" Auxiliary  753 _rPDDiatS_", rPDDiatS
write (1,*)" Auxiliary  754 _rNDDiatS_", rNDDiatS
write (1,*)" Auxiliary  755 _uFunTmDiat_", uFunTmDiat
write (1,*)" Auxiliary  756 _uFunTmProdDiat_", uFunTmProdDiat
write (1,*)" Auxiliary  757 _uFunTmRespDiat_", uFunTmRespDiat
write (1,*)" Auxiliary  758 _aVPUptMaxCrDiat_", aVPUptMaxCrDiat
write (1,*)" Auxiliary  759 _aVPUptDiat_", aVPUptDiat
write (1,*)" Auxiliary  760 _wPUptDiat_", wPUptDiat
write (1,*)" Auxiliary  761 _aVNUptMaxCrDiat_", aVNUptMaxCrDiat
write (1,*)" Auxiliary  762 _ahNUptDiat_", ahNUptDiat
write (1,*)" Auxiliary  763 _aVNUptDiat_", aVNUptDiat
write (1,*)" Auxiliary  764 _wNUptDiat_", wNUptDiat
write (1,*)" Auxiliary  765 _afNH4UptDiat_", afNH4UptDiat
write (1,*)" Auxiliary  766 _wNUptNH4Diat_", wNUptNH4Diat
write (1,*)" Auxiliary  767 _wNUptNO3Diat_", wNUptNO3Diat
write (1,*)" Auxiliary  768 _uMuMaxTmDiat_", uMuMaxTmDiat
write (1,*)" Auxiliary  769 _aPLimDiat_", aPLimDiat
write (1,*)" Auxiliary  770 _aNLimDiat_", aNLimDiat
write (1,*)" Auxiliary  771 _aSiLimDiat_", aSiLimDiat
write (1,*)" Auxiliary  772 _aLLimDiat_", aLLimDiat
write (1,*)" Auxiliary  773 _aMuTmLDiat_", aMuTmLDiat
write (1,*)" Auxiliary  774 _aNutLimDiat_", aNutLimDiat
write (1,*)" Auxiliary  775 _aMuDiat_", aMuDiat
write (1,*)" Auxiliary  776 _wDAssDiat_", wDAssDiat
write (1,*)" Auxiliary  777 _rChDDiat_", rChDDiat
write (1,*)" Auxiliary  778 _oChlaDiat_", oChlaDiat
write (1,*)" Auxiliary  779 _aExtChDiat_", aExtChDiat
write (1,*)" Auxiliary  780 _ukDRespTmDiat_", ukDRespTmDiat
write (1,*)" Auxiliary  781 _wDRespDiatW_", wDRespDiatW
write (1,*)" Auxiliary  782 _ukLossTmDiat_", ukLossTmDiat
write (1,*)" Auxiliary  783 _wDLossDiat_", wDLossDiat
write (1,*)" Auxiliary  784 _wDMortDiatW_", wDMortDiatW
write (1,*)" Auxiliary  785 _uCorVSetDiat_", uCorVSetDiat
write (1,*)" Auxiliary  786 _tDSetDiat_", tDSetDiat
write (1,*)" Auxiliary  787 _tDResusDiat_", tDResusDiat
write (1,*)" Auxiliary  788 _tDRespDiatS_", tDRespDiatS
write (1,*)" Auxiliary  789 _tDMortDiatS_", tDMortDiatS
write (1,*)" Auxiliary  790 _ukDDecDiat_", ukDDecDiat
write (1,*)" Auxiliary  791 _wPExcrDiatW_", wPExcrDiatW
write (1,*)" Auxiliary  792 _wPLossDiat_", wPLossDiat
write (1,*)" Auxiliary  793 _wPMortDiatW_", wPMortDiatW
write (1,*)" Auxiliary  794 _tPSetDiat_", tPSetDiat
write (1,*)" Auxiliary  795 _tPResusDiat_", tPResusDiat
write (1,*)" Auxiliary  796 _tPExcrDiatS_", tPExcrDiatS
write (1,*)" Auxiliary  797 _tPMortDiatS_", tPMortDiatS
write (1,*)" Auxiliary  798 _wNExcrDiatW_", wNExcrDiatW
write (1,*)" Auxiliary  799 _wNLossDiat_", wNLossDiat
write (1,*)" Auxiliary  800 _wNMortDiatW_", wNMortDiatW
write (1,*)" Auxiliary  801 _tNSetDiat_", tNSetDiat
write (1,*)" Auxiliary  802 _tNResusDiat_", tNResusDiat
write (1,*)" Auxiliary  803 _tNExcrDiatS_", tNExcrDiatS
write (1,*)" Auxiliary  804 _tNMortDiatS_", tNMortDiatS
write (1,*)" Auxiliary  805 _wDPrimDiatW_", wDPrimDiatW
write (1,*)" Auxiliary  806 _wPPrimDiatW_", wPPrimDiatW
write (1,*)" Auxiliary  807 _wNPrimDiatW_", wNPrimDiatW
write (1,*)" Auxiliary  808 _tDPrimDiatS_", tDPrimDiatS
write (1,*)" Auxiliary  809 _tPPrimDiatS_", tPPrimDiatS
write (1,*)" Auxiliary  810 _tNPrimDiatS_", tNPrimDiatS
write (1,*)" Auxiliary  811 _oChla_", oChla
write (1,*)" Auxiliary  812 _wDAssPhyt_", wDAssPhyt
write (1,*)" Auxiliary  813 _wDRespPhytW_", wDRespPhytW
write (1,*)" Auxiliary  814 _wDMortPhytW_", wDMortPhytW
write (1,*)" Auxiliary  815 _tDSetPhyt_", tDSetPhyt
write (1,*)" Auxiliary  816 _wDLossPhyt_", wDLossPhyt
write (1,*)" Auxiliary  817 _wDPrimPhytW_", wDPrimPhytW
write (1,*)" Auxiliary  818 _wPUptPhyt_", wPUptPhyt
write (1,*)" Auxiliary  819 _wPExcrPhytW_", wPExcrPhytW
write (1,*)" Auxiliary  820 _wPMortPhytW_", wPMortPhytW
write (1,*)" Auxiliary  821 _tPSetPhyt_", tPSetPhyt
write (1,*)" Auxiliary  822 _tPResusPhyt_", tPResusPhyt
write (1,*)" Auxiliary  823 _wPLossPhyt_", wPLossPhyt
write (1,*)" Auxiliary  824 _wPPrimPhytW_", wPPrimPhytW
write (1,*)" Auxiliary  825 _wNUptPhyt_", wNUptPhyt
write (1,*)" Auxiliary  826 _wNUptNH4Phyt_", wNUptNH4Phyt
write (1,*)" Auxiliary  827 _wNUptNO3Phyt_", wNUptNO3Phyt
write (1,*)" Auxiliary  828 _wNExcrPhytW_", wNExcrPhytW
write (1,*)" Auxiliary  829 _wNMortPhytW_", wNMortPhytW
write (1,*)" Auxiliary  830 _tNSetPhyt_", tNSetPhyt
write (1,*)" Auxiliary  831 _tNResusPhyt_", tNResusPhyt
write (1,*)" Auxiliary  832 _wNLossPhyt_", wNLossPhyt
write (1,*)" Auxiliary  833 _wNPrimPhytW_", wNPrimPhytW
write (1,*)" Auxiliary  834 _tDRespPhytS_", tDRespPhytS
write (1,*)" Auxiliary  835 _tDMortPhytS_", tDMortPhytS
write (1,*)" Auxiliary  836 _tDPrimPhytS_", tDPrimPhytS
write (1,*)" Auxiliary  837 _tPExcrPhytS_", tPExcrPhytS
write (1,*)" Auxiliary  838 _tPMortPhytS_", tPMortPhytS
write (1,*)" Auxiliary  839 _tPPrimPhytS_", tPPrimPhytS
write (1,*)" Auxiliary  840 _tNExcrPhytS_", tNExcrPhytS
write (1,*)" Auxiliary  841 _tNMortPhytS_", tNMortPhytS
write (1,*)" Auxiliary  842 _tNPrimPhytS_", tNPrimPhytS
write (1,*)" Auxiliary  843 _wSiUptDiat_", wSiUptDiat
write (1,*)" Auxiliary  844 _wSiExcrDiatW_", wSiExcrDiatW
write (1,*)" Auxiliary  845 _wSiLossDiat_", wSiLossDiat
write (1,*)" Auxiliary  846 _wSiMortDiatW_", wSiMortDiatW
write (1,*)" Auxiliary  847 _tSiSetDiat_", tSiSetDiat
write (1,*)" Auxiliary  848 _tSiResusDiat_", tSiResusDiat
write (1,*)" Auxiliary  849 _wSiPrimDiatW_", wSiPrimDiatW
write (1,*)" Auxiliary  850 _rCyDBlue_", rCyDBlue
write (1,*)" Auxiliary  851 _oCyan_", oCyan
write (1,*)" Auxiliary  852 _fDDiat_", fDDiat
write (1,*)" Auxiliary  853 _wDPrimDetW_", wDPrimDetW
write (1,*)" Auxiliary  854 _tDPrimDetS_", tDPrimDetS
write (1,*)" Auxiliary  855 _tDPrimTotT_", tDPrimTotT
write (1,*)" Auxiliary  856 _wO2ProdPhyt_", wO2ProdPhyt
write (1,*)" Auxiliary  857 _wO2RespPhytW_", wO2RespPhytW
write (1,*)" Auxiliary  858 _wO2UptNO3Phyt_", wO2UptNO3Phyt
write (1,*)" Auxiliary  859 _wO2PrimW_", wO2PrimW
write (1,*)" Auxiliary  860 _tO2RespPhytS_", tO2RespPhytS
write (1,*)" Auxiliary  861 _tO2PrimS_", tO2PrimS
write (1,*)" Auxiliary  862 _wPMortPhytPO4W_", wPMortPhytPO4W
write (1,*)" Auxiliary  863 _wPMortPhytDetW_", wPMortPhytDetW
write (1,*)" Auxiliary  864 _wPLossPhytPO4_", wPLossPhytPO4
write (1,*)" Auxiliary  865 _wPLossPhytDet_", wPLossPhytDet
write (1,*)" Auxiliary  866 _wPPrimPO4W_", wPPrimPO4W
write (1,*)" Auxiliary  867 _wPPrimDetW_", wPPrimDetW
write (1,*)" Auxiliary  868 _tPMortPhytPO4S_", tPMortPhytPO4S
write (1,*)" Auxiliary  869 _tPMortPhytDetS_", tPMortPhytDetS
write (1,*)" Auxiliary  870 _tPPrimDetS_", tPPrimDetS
write (1,*)" Auxiliary  871 _tPPrimPO4S_", tPPrimPO4S
write (1,*)" Auxiliary  872 _tPPrimTotT_", tPPrimTotT
write (1,*)" Auxiliary  873 _wNMortPhytNH4W_", wNMortPhytNH4W
write (1,*)" Auxiliary  874 _wNMortPhytDetW_", wNMortPhytDetW
write (1,*)" Auxiliary  875 _wNLossPhytNH4_", wNLossPhytNH4
write (1,*)" Auxiliary  876 _wNLossPhytDet_", wNLossPhytDet
write (1,*)" Auxiliary  877 _wNPrimNH4W_", wNPrimNH4W
write (1,*)" Auxiliary  878 _wNPrimNO3W_", wNPrimNO3W
write (1,*)" Auxiliary  879 _wNPrimDetW_", wNPrimDetW
write (1,*)" Auxiliary  880 _tNMortPhytNH4S_", tNMortPhytNH4S
write (1,*)" Auxiliary  881 _tNMortPhytDetS_", tNMortPhytDetS
write (1,*)" Auxiliary  882 _tNPrimNH4S_", tNPrimNH4S
write (1,*)" Auxiliary  883 _tNPrimNO3S_", tNPrimNO3S
write (1,*)" Auxiliary  884 _tNPrimDetS_", tNPrimDetS
write (1,*)" Auxiliary  885 _tNPrimTotT_", tNPrimTotT
write (1,*)" Auxiliary  886 _tSiExcrDiatS_", tSiExcrDiatS
write (1,*)" Auxiliary  887 _tSiMortDiatS_", tSiMortDiatS
write (1,*)" Auxiliary  888 _wSiPrimSiO2W_", wSiPrimSiO2W
write (1,*)" Auxiliary  889 _wSiPrimDetW_", wSiPrimDetW
write (1,*)" Auxiliary  890 _tSiPrimDiatS_", tSiPrimDiatS
write (1,*)" Auxiliary  891 _tSiPrimDetS_", tSiPrimDetS
write (1,*)" Auxiliary  892 _tSiPrimTotT_", tSiPrimTotT
write (1,*)" Auxiliary  893 _aPACoef_", aPACoef
write (1,*)" Auxiliary  894 _bSecchiMax_", bSecchiMax
write (1,*)" Auxiliary  895 _aSecchi_", aSecchi
write (1,*)" Auxiliary  896 _aTransparency_", aTransparency
write (1,*)" Auxiliary  897 _aDepthEuph_", aDepthEuph
write (1,*)" Auxiliary  898 _aRelDepthEuph_", aRelDepthEuph
write (1,*)" Auxiliary  899 _aChlaH_", aChlaH
write (1,*)" Auxiliary  900 _aCovPhytW_", aCovPhytW
write (1,*)" Auxiliary  901 _rExtChPhyt_", rExtChPhyt
write (1,*)" Auxiliary  902 _uFunTmZoo_", uFunTmZoo
write (1,*)" Auxiliary  903 _rPDZoo_", rPDZoo
write (1,*)" Auxiliary  904 _rNDZoo_", rNDZoo
write (1,*)" Auxiliary  905 _oDFoodZoo_", oDFoodZoo
write (1,*)" Auxiliary  906 _aFilt_", aFilt
write (1,*)" Auxiliary  907 _ukDAssTmZoo_", ukDAssTmZoo
write (1,*)" Auxiliary  908 _aDSatZoo_", aDSatZoo
write (1,*)" Auxiliary  909 _ukDRespTmZoo_", ukDRespTmZoo
write (1,*)" Auxiliary  910 _ukDIncrZoo_", ukDIncrZoo
write (1,*)" Auxiliary  911 _wDEnvZoo_", wDEnvZoo
write (1,*)" Auxiliary  912 _wDAssZoo_", wDAssZoo
write (1,*)" Auxiliary  913 _wDConsZoo_", wDConsZoo
write (1,*)" Auxiliary  914 _wDConsDetZoo_", wDConsDetZoo
write (1,*)" Auxiliary  915 _wDConsDiatZoo_", wDConsDiatZoo
write (1,*)" Auxiliary  916 _wDConsGrenZoo_", wDConsGrenZoo
write (1,*)" Auxiliary  917 _wDConsBlueZoo_", wDConsBlueZoo
write (1,*)" Auxiliary  918 _wDConsPhytZoo_", wDConsPhytZoo
write (1,*)" Auxiliary  919 _wDEgesZoo_", wDEgesZoo
write (1,*)" Auxiliary  920 _aCorDRespZoo_", aCorDRespZoo
write (1,*)" Auxiliary  921 _wDRespZoo_", wDRespZoo
write (1,*)" Auxiliary  922 _wDMortZoo_", wDMortZoo
write (1,*)" Auxiliary  923 _oPFoodZoo_", oPFoodZoo
write (1,*)" Auxiliary  924 _rPDFoodZoo_", rPDFoodZoo
write (1,*)" Auxiliary  925 _wPConsDiatZoo_", wPConsDiatZoo
write (1,*)" Auxiliary  926 _wPConsGrenZoo_", wPConsGrenZoo
write (1,*)" Auxiliary  927 _wPConsBlueZoo_", wPConsBlueZoo
write (1,*)" Auxiliary  928 _wPConsPhytZoo_", wPConsPhytZoo
write (1,*)" Auxiliary  929 _wPConsDetZoo_", wPConsDetZoo
write (1,*)" Auxiliary  930 _wPConsZoo_", wPConsZoo
write (1,*)" Auxiliary  931 _afPAssZoo_", afPAssZoo
write (1,*)" Auxiliary  932 _wPAssZoo_", wPAssZoo
write (1,*)" Auxiliary  933 _wPEgesZoo_", wPEgesZoo
write (1,*)" Auxiliary  934 _wPEgesZooPO4_", wPEgesZooPO4
write (1,*)" Auxiliary  935 _wPEgesZooDet_", wPEgesZooDet
write (1,*)" Auxiliary  936 _akPExcrZoo_", akPExcrZoo
write (1,*)" Auxiliary  937 _wPExcrZoo_", wPExcrZoo
write (1,*)" Auxiliary  938 _wPMortZoo_", wPMortZoo
write (1,*)" Auxiliary  939 _wPMortZooPO4_", wPMortZooPO4
write (1,*)" Auxiliary  940 _wPMortZooDet_", wPMortZooDet
write (1,*)" Auxiliary  941 _oNFoodZoo_", oNFoodZoo
write (1,*)" Auxiliary  942 _rNDFoodZoo_", rNDFoodZoo
write (1,*)" Auxiliary  943 _wNConsDiatZoo_", wNConsDiatZoo
write (1,*)" Auxiliary  944 _wNConsGrenZoo_", wNConsGrenZoo
write (1,*)" Auxiliary  945 _wNConsBlueZoo_", wNConsBlueZoo
write (1,*)" Auxiliary  946 _wNConsPhytZoo_", wNConsPhytZoo
write (1,*)" Auxiliary  947 _wNConsDetZoo_", wNConsDetZoo
write (1,*)" Auxiliary  948 _wNConsZoo_", wNConsZoo
write (1,*)" Auxiliary  949 _afNAssZoo_", afNAssZoo
write (1,*)" Auxiliary  950 _wNAssZoo_", wNAssZoo
write (1,*)" Auxiliary  951 _wNEgesZoo_", wNEgesZoo
write (1,*)" Auxiliary  952 _wNEgesZooNH4_", wNEgesZooNH4
write (1,*)" Auxiliary  953 _wNEgesZooDet_", wNEgesZooDet
write (1,*)" Auxiliary  954 _kNExcrZoo_", kNExcrZoo
write (1,*)" Auxiliary  955 _wNExcrZoo_", wNExcrZoo
write (1,*)" Auxiliary  956 _wNMortZoo_", wNMortZoo
write (1,*)" Auxiliary  957 _wNMortZooNH4_", wNMortZooNH4
write (1,*)" Auxiliary  958 _wNMortZooDet_", wNMortZooDet
write (1,*)" Auxiliary  959 _wSiConsDiatZoo_", wSiConsDiatZoo
write (1,*)" Auxiliary  960 _uFunTmBent_", uFunTmBent
write (1,*)" Auxiliary  961 _aDFoodBent_", aDFoodBent
write (1,*)" Auxiliary  962 _rPDBent_", rPDBent
write (1,*)" Auxiliary  963 _rNDBent_", rNDBent
write (1,*)" Auxiliary  964 _tDMigrBent_", tDMigrBent
write (1,*)" Auxiliary  965 _aDSatBent_", aDSatBent
write (1,*)" Auxiliary  966 _ukDIncrBent_", ukDIncrBent
write (1,*)" Auxiliary  967 _tDEnvBent_", tDEnvBent
write (1,*)" Auxiliary  968 _tDAssBent_", tDAssBent
write (1,*)" Auxiliary  969 _aDAssBentSp_", aDAssBentSp
write (1,*)" Auxiliary  970 _tDConsBent_", tDConsBent
write (1,*)" Auxiliary  971 _tDConsDetBent_", tDConsDetBent
write (1,*)" Auxiliary  972 _tDConsDiatBent_", tDConsDiatBent
write (1,*)" Auxiliary  973 _tDConsGrenBent_", tDConsGrenBent
write (1,*)" Auxiliary  974 _tDConsBlueBent_", tDConsBlueBent
write (1,*)" Auxiliary  975 _tDConsPhytBent_", tDConsPhytBent
write (1,*)" Auxiliary  976 _tDEgesBent_", tDEgesBent
write (1,*)" Auxiliary  977 _tDRespBent_", tDRespBent
write (1,*)" Auxiliary  978 _tDMortBent_", tDMortBent
write (1,*)" Auxiliary  979 _aPFoodBent_", aPFoodBent
write (1,*)" Auxiliary  980 _rPDFoodBent_", rPDFoodBent
write (1,*)" Auxiliary  981 _tPConsDetBent_", tPConsDetBent
write (1,*)" Auxiliary  982 _tPConsDiatBent_", tPConsDiatBent
write (1,*)" Auxiliary  983 _tPConsGrenBent_", tPConsGrenBent
write (1,*)" Auxiliary  984 _tPConsBlueBent_", tPConsBlueBent
write (1,*)" Auxiliary  985 _tPConsPhytBent_", tPConsPhytBent
write (1,*)" Auxiliary  986 _tPConsBent_", tPConsBent
write (1,*)" Auxiliary  987 _afPAssBent_", afPAssBent
write (1,*)" Auxiliary  988 _tPAssBent_", tPAssBent
write (1,*)" Auxiliary  989 _tPEgesBent_", tPEgesBent
write (1,*)" Auxiliary  990 _tPEgesBentPO4_", tPEgesBentPO4
write (1,*)" Auxiliary  991 _tPEgesBentDet_", tPEgesBentDet
write (1,*)" Auxiliary  992 _tPExcrBent_", tPExcrBent
write (1,*)" Auxiliary  993 _tPMortBent_", tPMortBent
write (1,*)" Auxiliary  994 _tPMortBentPO4_", tPMortBentPO4
write (1,*)" Auxiliary  995 _tPMortBentDet_", tPMortBentDet
write (1,*)" Auxiliary  996 _tPMigrBent_", tPMigrBent
write (1,*)" Auxiliary  997 _aNFoodBent_", aNFoodBent
write (1,*)" Auxiliary  998 _rNDFoodBent_", rNDFoodBent
write (1,*)" Auxiliary  999 _tNMigrBent_", tNMigrBent
write (1,*)" Auxiliary  1000 _tNConsDetBent_", tNConsDetBent
write (1,*)" Auxiliary  1001 _tNConsDiatBent_", tNConsDiatBent
write (1,*)" Auxiliary  1002 _tNConsGrenBent_", tNConsGrenBent
write (1,*)" Auxiliary  1003 _tNConsBlueBent_", tNConsBlueBent
write (1,*)" Auxiliary  1004 _tNConsPhytBent_", tNConsPhytBent
write (1,*)" Auxiliary  1005 _tNConsBent_", tNConsBent
write (1,*)" Auxiliary  1006 _afNAssBent_", afNAssBent
write (1,*)" Auxiliary  1007 _tNAssBent_", tNAssBent
write (1,*)" Auxiliary  1008 _tNEgesBent_", tNEgesBent
write (1,*)" Auxiliary  1009 _tNEgesBentNH4_", tNEgesBentNH4
write (1,*)" Auxiliary  1010 _tNEgesBentDet_", tNEgesBentDet
write (1,*)" Auxiliary  1011 _tNExcrBent_", tNExcrBent
write (1,*)" Auxiliary  1012 _tNMortBent_", tNMortBent
write (1,*)" Auxiliary  1013 _tNMortBentNH4_", tNMortBentNH4
write (1,*)" Auxiliary  1014 _tNMortBentDet_", tNMortBentDet
write (1,*)" Auxiliary  1015 _tSiConsDiatBent_", tSiConsDiatBent
write (1,*)" Auxiliary  1016 _aDFish_", aDFish
write (1,*)" Auxiliary  1017 _aPFish_", aPFish
write (1,*)" Auxiliary  1018 _aNFish_", aNFish
write (1,*)" Auxiliary  1019 _rPDFiJv_", rPDFiJv
write (1,*)" Auxiliary  1020 _rPDFiAd_", rPDFiAd
write (1,*)" Auxiliary  1021 _rNDFiJv_", rNDFiJv
write (1,*)" Auxiliary  1022 _rNDFiAd_", rNDFiAd
write (1,*)" Auxiliary  1023 _tDReprFish_", tDReprFish
write (1,*)" Auxiliary  1024 _tDAgeFish_", tDAgeFish
write (1,*)" Auxiliary  1025 _aFunVegFish_", aFunVegFish
write (1,*)" Auxiliary  1026 _aDSatFiJv_", aDSatFiJv
write (1,*)" Auxiliary  1027 _ukDIncrFiJv_", ukDIncrFiJv
write (1,*)" Auxiliary  1028 _tDEnvFiJv_", tDEnvFiJv
write (1,*)" Auxiliary  1029 _tDAssFiJv_", tDAssFiJv
write (1,*)" Auxiliary  1030 _tDConsFiJv_", tDConsFiJv
write (1,*)" Auxiliary  1031 _tDEgesFiJv_", tDEgesFiJv
write (1,*)" Auxiliary  1032 _tDRespFiJv_", tDRespFiJv
write (1,*)" Auxiliary  1033 _tDMortFiJv_", tDMortFiJv
write (1,*)" Auxiliary  1034 _tDMigrFiJv_", tDMigrFiJv
write (1,*)" Auxiliary  1035 _aDSatFiAd_", aDSatFiAd
write (1,*)" Auxiliary  1036 _ukDIncrFiAd_", ukDIncrFiAd
write (1,*)" Auxiliary  1037 _tDEnvFiAd_", tDEnvFiAd
write (1,*)" Auxiliary  1038 _tDAssFiAd_", tDAssFiAd
write (1,*)" Auxiliary  1039 _tDConsFiAd_", tDConsFiAd
write (1,*)" Auxiliary  1040 _tDEgesFiAd_", tDEgesFiAd
write (1,*)" Auxiliary  1041 _tDRespFiAd_", tDRespFiAd
write (1,*)" Auxiliary  1042 _tDMortFiAd_", tDMortFiAd
write (1,*)" Auxiliary  1043 _ukHarvFish_", ukHarvFish
write (1,*)" Auxiliary  1044 _tDHarvFish_", tDHarvFish
write (1,*)" Auxiliary  1045 _tDMigrFiAd_", tDMigrFiAd
write (1,*)" Auxiliary  1046 _tDMortFish_", tDMortFish
write (1,*)" Auxiliary  1047 _tDMortFishBot_", tDMortFishBot
write (1,*)" Auxiliary  1048 _tDMortFishDet_", tDMortFishDet
write (1,*)" Auxiliary  1049 _tPReprFish_", tPReprFish
write (1,*)" Auxiliary  1050 _tPAgeFish_", tPAgeFish
write (1,*)" Auxiliary  1051 _tPMigrFiJv_", tPMigrFiJv
write (1,*)" Auxiliary  1052 _tPConsFiJv_", tPConsFiJv
write (1,*)" Auxiliary  1053 _afPAssFiJv_", afPAssFiJv
write (1,*)" Auxiliary  1054 _tPAssFiJv_", tPAssFiJv
write (1,*)" Auxiliary  1055 _tPEgesFiJv_", tPEgesFiJv
write (1,*)" Auxiliary  1056 _tPExcrFiJv_", tPExcrFiJv
write (1,*)" Auxiliary  1057 _tPMortFiJv_", tPMortFiJv
write (1,*)" Auxiliary  1058 _tPMigrFiAd_", tPMigrFiAd
write (1,*)" Auxiliary  1059 _tPConsFiAd_", tPConsFiAd
write (1,*)" Auxiliary  1060 _afPAssFiAd_", afPAssFiAd
write (1,*)" Auxiliary  1061 _tPAssFiAd_", tPAssFiAd
write (1,*)" Auxiliary  1062 _tPEgesFiAd_", tPEgesFiAd
write (1,*)" Auxiliary  1063 _tPExcrFiAd_", tPExcrFiAd
write (1,*)" Auxiliary  1064 _tPMortFiAd_", tPMortFiAd
write (1,*)" Auxiliary  1065 _tPHarvFish_", tPHarvFish
write (1,*)" Auxiliary  1066 _tPMortFish_", tPMortFish
write (1,*)" Auxiliary  1067 _tPMortFishBot_", tPMortFishBot
write (1,*)" Auxiliary  1068 _tPMortFishPO4_", tPMortFishPO4
write (1,*)" Auxiliary  1069 _tPMortFishDet_", tPMortFishDet
write (1,*)" Auxiliary  1070 _tPEgesFish_", tPEgesFish
write (1,*)" Auxiliary  1071 _tPEgesFishPO4_", tPEgesFishPO4
write (1,*)" Auxiliary  1072 _tPEgesFishDet_", tPEgesFishDet
write (1,*)" Auxiliary  1073 _tNReprFish_", tNReprFish
write (1,*)" Auxiliary  1074 _tNAgeFish_", tNAgeFish
write (1,*)" Auxiliary  1075 _tNMigrFiJv_", tNMigrFiJv
write (1,*)" Auxiliary  1076 _tNConsFiJv_", tNConsFiJv
write (1,*)" Auxiliary  1077 _afNAssFiJv_", afNAssFiJv
write (1,*)" Auxiliary  1078 _tNAssFiJv_", tNAssFiJv
write (1,*)" Auxiliary  1079 _tNEgesFiJv_", tNEgesFiJv
write (1,*)" Auxiliary  1080 _tNExcrFiJv_", tNExcrFiJv
write (1,*)" Auxiliary  1081 _tNMortFiJv_", tNMortFiJv
write (1,*)" Auxiliary  1082 _tNMigrFiAd_", tNMigrFiAd
write (1,*)" Auxiliary  1083 _tNConsFiAd_", tNConsFiAd
write (1,*)" Auxiliary  1084 _afNAssFiAd_", afNAssFiAd
write (1,*)" Auxiliary  1085 _tNAssFiAd_", tNAssFiAd
write (1,*)" Auxiliary  1086 _tNEgesFiAd_", tNEgesFiAd
write (1,*)" Auxiliary  1087 _tNExcrFiAd_", tNExcrFiAd
write (1,*)" Auxiliary  1088 _tNMortFiAd_", tNMortFiAd
write (1,*)" Auxiliary  1089 _tNHarvFish_", tNHarvFish
write (1,*)" Auxiliary  1090 _tNMortFish_", tNMortFish
write (1,*)" Auxiliary  1091 _tNMortFishBot_", tNMortFishBot
write (1,*)" Auxiliary  1092 _tNMortFishNH4_", tNMortFishNH4
write (1,*)" Auxiliary  1093 _tNMortFishDet_", tNMortFishDet
write (1,*)" Auxiliary  1094 _tNEgesFish_", tNEgesFish
write (1,*)" Auxiliary  1095 _tNEgesFishNH4_", tNEgesFishNH4
write (1,*)" Auxiliary  1096 _tNEgesFishDet_", tNEgesFishDet
write (1,*)" Auxiliary  1097 _uFunTmPisc_", uFunTmPisc
write (1,*)" Auxiliary  1098 _tDMigrPisc_", tDMigrPisc
write (1,*)" Auxiliary  1099 _aDCarrPisc_", aDCarrPisc
write (1,*)" Auxiliary  1100 _aFunVegPisc_", aFunVegPisc
write (1,*)" Auxiliary  1101 _aDSatPisc_", aDSatPisc
write (1,*)" Auxiliary  1102 _akDIncrPisc_", akDIncrPisc
write (1,*)" Auxiliary  1103 _tDEnvPisc_", tDEnvPisc
write (1,*)" Auxiliary  1104 _tDAssPisc_", tDAssPisc
write (1,*)" Auxiliary  1105 _tDConsPisc_", tDConsPisc
write (1,*)" Auxiliary  1106 _tDEgesPisc_", tDEgesPisc
write (1,*)" Auxiliary  1107 _tDConsFiJvPisc_", tDConsFiJvPisc
write (1,*)" Auxiliary  1108 _tDConsFiAdPisc_", tDConsFiAdPisc
write (1,*)" Auxiliary  1109 _tDRespPisc_", tDRespPisc
write (1,*)" Auxiliary  1110 _tDMortPisc_", tDMortPisc
write (1,*)" Auxiliary  1111 _tDMortPiscBot_", tDMortPiscBot
write (1,*)" Auxiliary  1112 _tDMortPiscDet_", tDMortPiscDet
write (1,*)" Auxiliary  1113 _ukHarvPisc_", ukHarvPisc
write (1,*)" Auxiliary  1114 _tDHarvPisc_", tDHarvPisc
write (1,*)" Auxiliary  1115 _aPPisc_", aPPisc
write (1,*)" Auxiliary  1116 _tPConsFiJvPisc_", tPConsFiJvPisc
write (1,*)" Auxiliary  1117 _tPConsFiAdPisc_", tPConsFiAdPisc
write (1,*)" Auxiliary  1118 _tPConsPisc_", tPConsPisc
write (1,*)" Auxiliary  1119 _rPDFoodPisc_", rPDFoodPisc
write (1,*)" Auxiliary  1120 _afPAssPisc_", afPAssPisc
write (1,*)" Auxiliary  1121 _tPAssPisc_", tPAssPisc
write (1,*)" Auxiliary  1122 _tPEgesPisc_", tPEgesPisc
write (1,*)" Auxiliary  1123 _tPEgesPiscPO4_", tPEgesPiscPO4
write (1,*)" Auxiliary  1124 _tPEgesPiscDet_", tPEgesPiscDet
write (1,*)" Auxiliary  1125 _tPExcrPisc_", tPExcrPisc
write (1,*)" Auxiliary  1126 _tPMortPisc_", tPMortPisc
write (1,*)" Auxiliary  1127 _tPMortPiscBot_", tPMortPiscBot
write (1,*)" Auxiliary  1128 _tPMortPiscPO4_", tPMortPiscPO4
write (1,*)" Auxiliary  1129 _tPMortPiscDet_", tPMortPiscDet
write (1,*)" Auxiliary  1130 _tPMigrPisc_", tPMigrPisc
write (1,*)" Auxiliary  1131 _tPHarvPisc_", tPHarvPisc
write (1,*)" Auxiliary  1132 _aNPisc_", aNPisc
write (1,*)" Auxiliary  1133 _tNConsFiJvPisc_", tNConsFiJvPisc
write (1,*)" Auxiliary  1134 _tNConsFiAdPisc_", tNConsFiAdPisc
write (1,*)" Auxiliary  1135 _tNConsPisc_", tNConsPisc
write (1,*)" Auxiliary  1136 _rNDFoodPisc_", rNDFoodPisc
write (1,*)" Auxiliary  1137 _afNAssPisc_", afNAssPisc
write (1,*)" Auxiliary  1138 _tNAssPisc_", tNAssPisc
write (1,*)" Auxiliary  1139 _tNEgesPisc_", tNEgesPisc
write (1,*)" Auxiliary  1140 _tNEgesPiscNH4_", tNEgesPiscNH4
write (1,*)" Auxiliary  1141 _tNEgesPiscDet_", tNEgesPiscDet
write (1,*)" Auxiliary  1142 _tNExcrPisc_", tNExcrPisc
write (1,*)" Auxiliary  1143 _tNMortPisc_", tNMortPisc
write (1,*)" Auxiliary  1144 _tNMortPiscBot_", tNMortPiscBot
write (1,*)" Auxiliary  1145 _tNMortPiscNH4_", tNMortPiscNH4
write (1,*)" Auxiliary  1146 _tNMortPiscDet_", tNMortPiscDet
write (1,*)" Auxiliary  1147 _tNMigrPisc_", tNMigrPisc
write (1,*)" Auxiliary  1148 _tNHarvPisc_", tNHarvPisc
write (1,*)" Auxiliary  1149 _wDWebZoo_", wDWebZoo
write (1,*)" Auxiliary  1150 _wPWebZoo_", wPWebZoo
write (1,*)" Auxiliary  1151 _wNWebZoo_", wNWebZoo
write (1,*)" Auxiliary  1152 _tDWebBent_", tDWebBent
write (1,*)" Auxiliary  1153 _tPWebBent_", tPWebBent
write (1,*)" Auxiliary  1154 _tNWebBent_", tNWebBent
write (1,*)" Auxiliary  1155 _tDWebFiJv_", tDWebFiJv
write (1,*)" Auxiliary  1156 _tPWebFiJv_", tPWebFiJv
write (1,*)" Auxiliary  1157 _tNWebFiJv_", tNWebFiJv
write (1,*)" Auxiliary  1158 _tDWebFiAd_", tDWebFiAd
write (1,*)" Auxiliary  1159 _tPWebFiAd_", tPWebFiAd
write (1,*)" Auxiliary  1160 _tNWebFiAd_", tNWebFiAd
write (1,*)" Auxiliary  1161 _tDWebPisc_", tDWebPisc
write (1,*)" Auxiliary  1162 _wDWebDetW_", wDWebDetW
write (1,*)" Auxiliary  1163 _wDWebDiatW_", wDWebDiatW
write (1,*)" Auxiliary  1164 _wDWebGrenW_", wDWebGrenW
write (1,*)" Auxiliary  1165 _wDWebBlueW_", wDWebBlueW
write (1,*)" Auxiliary  1166 _tDWebDetS_", tDWebDetS
write (1,*)" Auxiliary  1167 _tDWebDiatS_", tDWebDiatS
write (1,*)" Auxiliary  1168 _tDWebGrenS_", tDWebGrenS
write (1,*)" Auxiliary  1169 _tDWebBlueS_", tDWebBlueS
write (1,*)" Auxiliary  1170 _tDWebPhytS_", tDWebPhytS
write (1,*)" Auxiliary  1171 _tDWebTotT_", tDWebTotT
write (1,*)" Auxiliary  1172 _wPWebPO4W_", wPWebPO4W
write (1,*)" Auxiliary  1173 _wPWebDetW_", wPWebDetW
write (1,*)" Auxiliary  1174 _wPWebDiatW_", wPWebDiatW
write (1,*)" Auxiliary  1175 _wPWebGrenW_", wPWebGrenW
write (1,*)" Auxiliary  1176 _wPWebBlueW_", wPWebBlueW
write (1,*)" Auxiliary  1177 _tPWebPO4S_", tPWebPO4S
write (1,*)" Auxiliary  1178 _tPWebDetS_", tPWebDetS
write (1,*)" Auxiliary  1179 _tPWebDiatS_", tPWebDiatS
write (1,*)" Auxiliary  1180 _tPWebGrenS_", tPWebGrenS
write (1,*)" Auxiliary  1181 _tPWebBlueS_", tPWebBlueS
write (1,*)" Auxiliary  1182 _tPWebPhytS_", tPWebPhytS
write (1,*)" Auxiliary  1183 _tPWebTotT_", tPWebTotT
write (1,*)" Auxiliary  1184 _wNWebNH4W_", wNWebNH4W
write (1,*)" Auxiliary  1185 _wNWebNO3W_", wNWebNO3W
write (1,*)" Auxiliary  1186 _wNWebDetW_", wNWebDetW
write (1,*)" Auxiliary  1187 _wNWebDiatW_", wNWebDiatW
write (1,*)" Auxiliary  1188 _wNWebGrenW_", wNWebGrenW
write (1,*)" Auxiliary  1189 _wNWebBlueW_", wNWebBlueW
write (1,*)" Auxiliary  1190 _tNWebNH4S_", tNWebNH4S
write (1,*)" Auxiliary  1191 _tNWebNO3S_", tNWebNO3S
write (1,*)" Auxiliary  1192 _tNWebDetS_", tNWebDetS
write (1,*)" Auxiliary  1193 _tNWebDiatS_", tNWebDiatS
write (1,*)" Auxiliary  1194 _tNWebGrenS_", tNWebGrenS
write (1,*)" Auxiliary  1195 _tNWebBlueS_", tNWebBlueS
write (1,*)" Auxiliary  1196 _tNWebPhytS_", tNWebPhytS
write (1,*)" Auxiliary  1197 _tNWebTotT_", tNWebTotT
write (1,*)" Auxiliary  1198 _wSiWebSiO2W_", wSiWebSiO2W
write (1,*)" Auxiliary  1199 _wSiWebDetW_", wSiWebDetW
write (1,*)" Auxiliary  1200 _tSiWebDetS_", tSiWebDetS
write (1,*)" Auxiliary  1201 _tSiWebTotT_", tSiWebTotT
write (1,*)" Auxiliary  1202 _aPrefAve_", aPrefAve
write (1,*)" Auxiliary  1203 _wDConsZoo2_", wDConsZoo2
write (1,*)" Auxiliary  1204 _aDConsZooSp_", aDConsZooSp
write (1,*)" Auxiliary  1205 _aDAssZooSp_", aDAssZooSp
write (1,*)" Auxiliary  1206 _aDGrazSp_", aDGrazSp
write (1,*)" Auxiliary  1207 _aPConsZooSp_", aPConsZooSp
write (1,*)" Auxiliary  1208 _aPGrazSp_", aPGrazSp
write (1,*)" Auxiliary  1209 _aNConsZooSp_", aNConsZooSp
write (1,*)" Auxiliary  1210 _aNGrazSp_", aNGrazSp
write (1,*)" Auxiliary  1211 _afDShootPhra_", afDShootPhra
write (1,*)" Auxiliary  1212 _rDSRPhra_", rDSRPhra
write (1,*)" Auxiliary  1213 _rPDShootPhra_", rPDShootPhra
write (1,*)" Auxiliary  1214 _rNDShootPhra_", rNDShootPhra
write (1,*)" Auxiliary  1215 _rPDRootPhra_", rPDRootPhra
write (1,*)" Auxiliary  1216 _rNDRootPhra_", rNDRootPhra
write (1,*)" Auxiliary  1217 _aLengShootPhra_", aLengShootPhra
write (1,*)" Auxiliary  1218 _bDayInitPhra_", bDayInitPhra
write (1,*)" Auxiliary  1219 _aDAllPhra_", aDAllPhra
write (1,*)" Auxiliary  1220 _tDAllPhra_", tDAllPhra
write (1,*)" Auxiliary  1221 _tNTransPhra_", tNTransPhra
write (1,*)" Auxiliary  1222 _tPTransPhra_", tPTransPhra
write (1,*)" Auxiliary  1223 _aVNUptPhraMaxCr_", aVNUptPhraMaxCr
write (1,*)" Auxiliary  1224 _ahNUptPhraS_", ahNUptPhraS
write (1,*)" Auxiliary  1225 _aVNUptPhraS_", aVNUptPhraS
write (1,*)" Auxiliary  1226 _tNUptPhraS_", tNUptPhraS
write (1,*)" Auxiliary  1227 _tNUptNH4PhraS_", tNUptNH4PhraS
write (1,*)" Auxiliary  1228 _tNUptNO3PhraS_", tNUptNO3PhraS
write (1,*)" Auxiliary  1229 _tNUptShootPhra_", tNUptShootPhra
write (1,*)" Auxiliary  1230 _tNUptRootPhra_", tNUptRootPhra
write (1,*)" Auxiliary  1231 _aVPUptPhraMaxCr_", aVPUptPhraMaxCr
write (1,*)" Auxiliary  1232 _ahPUptPhraS_", ahPUptPhraS
write (1,*)" Auxiliary  1233 _aVPUptPhraS_", aVPUptPhraS
write (1,*)" Auxiliary  1234 _tPUptPhraS_", tPUptPhraS
write (1,*)" Auxiliary  1235 _tPUptShootPhra_", tPUptShootPhra
write (1,*)" Auxiliary  1236 _tPUptRootPhra_", tPUptRootPhra
write (1,*)" Auxiliary  1237 _uFunTmProdPhra_", uFunTmProdPhra
write (1,*)" Auxiliary  1238 _ukDRespTmPhra_", ukDRespTmPhra
write (1,*)" Auxiliary  1239 _aMuPhotPhra_", aMuPhotPhra
write (1,*)" Auxiliary  1240 _aNLimProdPhra_", aNLimProdPhra
write (1,*)" Auxiliary  1241 _aPLimProdPhra_", aPLimProdPhra
write (1,*)" Auxiliary  1242 _aNutLimPhra_", aNutLimPhra
write (1,*)" Auxiliary  1243 _aMuPhra_", aMuPhra
write (1,*)" Auxiliary  1244 _akDIncrPhra_", akDIncrPhra
write (1,*)" Auxiliary  1245 _tDDensPhra_", tDDensPhra
write (1,*)" Auxiliary  1246 _tDDensProdPhra_", tDDensProdPhra
write (1,*)" Auxiliary  1247 _tDProdPhra_", tDProdPhra
write (1,*)" Auxiliary  1248 _tDProdShootPhra_", tDProdShootPhra
write (1,*)" Auxiliary  1249 _tDProdRootPhra_", tDProdRootPhra
write (1,*)" Auxiliary  1250 _tDRespShootPhra_", tDRespShootPhra
write (1,*)" Auxiliary  1251 _tDRespRootPhra_", tDRespRootPhra
write (1,*)" Auxiliary  1252 _tO2RespRootPhra_", tO2RespRootPhra
write (1,*)" Auxiliary  1253 _tO2FlowPhra_", tO2FlowPhra
write (1,*)" Auxiliary  1254 _bDayRealPhra_", bDayRealPhra
write (1,*)" Auxiliary  1255 _aDRealPhra_", aDRealPhra
write (1,*)" Auxiliary  1256 _tDRealPhra_", tDRealPhra
write (1,*)" Auxiliary  1257 _tNRetrPhra_", tNRetrPhra
write (1,*)" Auxiliary  1258 _tPRetrPhra_", tPRetrPhra
write (1,*)" Auxiliary  1259 _tDMortShootPhra_", tDMortShootPhra
write (1,*)" Auxiliary  1260 _tNMortShootPhra_", tNMortShootPhra
write (1,*)" Auxiliary  1261 _tPMortShootPhra_", tPMortShootPhra
write (1,*)" Auxiliary  1262 _tDMortRootPhra_", tDMortRootPhra
write (1,*)" Auxiliary  1263 _tNMortRootPhra_", tNMortRootPhra
write (1,*)" Auxiliary  1264 _tPMortRootPhra_", tPMortRootPhra
write (1,*)" Auxiliary  1265 _tDManShootPhra_", tDManShootPhra
write (1,*)" Auxiliary  1266 _tNManShootPhra_", tNManShootPhra
write (1,*)" Auxiliary  1267 _tPManShootPhra_", tPManShootPhra
write (1,*)" Auxiliary  1268 _tDIMSM_", tDIMSM
write (1,*)" Auxiliary  1269 _tDHumSM_", tDHumSM
write (1,*)" Auxiliary  1270 _tDDetSM_", tDDetSM
write (1,*)" Auxiliary  1271 _vDeltaSM_", vDeltaSM
write (1,*)" Auxiliary  1272 _tDBurIMM_", tDBurIMM
write (1,*)" Auxiliary  1273 _tDBurOMM_", tDBurOMM
write (1,*)" Auxiliary  1274 _tDBurDetM_", tDBurDetM
write (1,*)" Auxiliary  1275 _tDBurHumM_", tDBurHumM
write (1,*)" Auxiliary  1276 _tDBurTotM_", tDBurTotM
write (1,*)" Auxiliary  1277 _tPBurHumM_", tPBurHumM
write (1,*)" Auxiliary  1278 _tPBurDetM_", tPBurDetM
write (1,*)" Auxiliary  1279 _tPBurAIMM_", tPBurAIMM
write (1,*)" Auxiliary  1280 _tPBurPO4M_", tPBurPO4M
write (1,*)" Auxiliary  1281 _tPBurTotM_", tPBurTotM
write (1,*)" Auxiliary  1282 _tNBurHumM_", tNBurHumM
write (1,*)" Auxiliary  1283 _tNBurDetM_", tNBurDetM
write (1,*)" Auxiliary  1284 _tNBurNH4M_", tNBurNH4M
write (1,*)" Auxiliary  1285 _tNBurNO3M_", tNBurNO3M
write (1,*)" Auxiliary  1286 _tNBurTotM_", tNBurTotM
write (1,*)" Auxiliary  1287 _tSiBurDetM_", tSiBurDetM
write (1,*)" Auxiliary  1288 _tSiBurTotM_", tSiBurTotM
write (1,*)" Auxiliary  1289 _vDeltaWM_", vDeltaWM
write (1,*)" Auxiliary  1290 _aRelDeltaWM_", aRelDeltaWM
write (1,*)" Auxiliary  1291 _tDSetTot_", tDSetTot
write (1,*)" Auxiliary  1292 _tPSetTot_", tPSetTot
write (1,*)" Auxiliary  1293 _tNSetTot_", tNSetTot
write (1,*)" Auxiliary  1294 _tDResusTot_", tDResusTot
write (1,*)" Auxiliary  1295 _tPResusTot_", tPResusTot
write (1,*)" Auxiliary  1296 _tNResusTot_", tNResusTot
write (1,*)" Auxiliary  1297 _bTimeDred_", bTimeDred
write (1,*)" Auxiliary  1298 _aDepthStart_", aDepthStart
write (1,*)" Auxiliary  1299 _akDredDepth_", akDredDepth
write (1,*)" Auxiliary  1300 _akDred_", akDred
write (1,*)" Auxiliary  1301 _akDredBent_", akDredBent
write (1,*)" Auxiliary  1302 _vDredDepthW_", vDredDepthW
write (1,*)" Auxiliary  1303 _tDDredDetS_", tDDredDetS
write (1,*)" Auxiliary  1304 _tPDredDetS_", tPDredDetS
write (1,*)" Auxiliary  1305 _tNDredDetS_", tNDredDetS
write (1,*)" Auxiliary  1306 _tSiDredDetS_", tSiDredDetS
write (1,*)" Auxiliary  1307 _tPDredAIMS_", tPDredAIMS
write (1,*)" Auxiliary  1308 _bRhoSolidSoil_", bRhoSolidSoil
write (1,*)" Auxiliary  1309 _tDDredNetSoil_", tDDredNetSoil
write (1,*)" Auxiliary  1310 _tDDredNetIMS_", tDDredNetIMS
write (1,*)" Auxiliary  1311 _tDDredNetHumS_", tDDredNetHumS
write (1,*)" Auxiliary  1312 _tPDredNetHumS_", tPDredNetHumS
write (1,*)" Auxiliary  1313 _tNDredNetHumS_", tNDredNetHumS
write (1,*)" Auxiliary  1314 _tDDredDiatS_", tDDredDiatS
write (1,*)" Auxiliary  1315 _tPDredDiatS_", tPDredDiatS
write (1,*)" Auxiliary  1316 _tNDredDiatS_", tNDredDiatS
write (1,*)" Auxiliary  1317 _tDDredGrenS_", tDDredGrenS
write (1,*)" Auxiliary  1318 _tPDredGrenS_", tPDredGrenS
write (1,*)" Auxiliary  1319 _tNDredGrenS_", tNDredGrenS
write (1,*)" Auxiliary  1320 _tDDredBlueS_", tDDredBlueS
write (1,*)" Auxiliary  1321 _tPDredBlueS_", tPDredBlueS
write (1,*)" Auxiliary  1322 _tNDredBlueS_", tNDredBlueS
write (1,*)" Auxiliary  1323 _tDDredPhytS_", tDDredPhytS
write (1,*)" Auxiliary  1324 _tPDredPhytS_", tPDredPhytS
write (1,*)" Auxiliary  1325 _tNDredPhytS_", tNDredPhytS
write (1,*)" Auxiliary  1326 _tDDredBent_", tDDredBent
write (1,*)" Auxiliary  1327 _tPDredBent_", tPDredBent
write (1,*)" Auxiliary  1328 _tNDredBent_", tNDredBent
write (1,*)" Auxiliary  1329 _tDDredVeg_", tDDredVeg
write (1,*)" Auxiliary  1330 _tPDredVeg_", tPDredVeg
write (1,*)" Auxiliary  1331 _tNDredVeg_", tNDredVeg
write (1,*)" Auxiliary  1332 _tDDredNetTot_", tDDredNetTot
write (1,*)" Auxiliary  1333 _tPDredNetTot_", tPDredNetTot
write (1,*)" Auxiliary  1334 _tNDredNetTot_", tNDredNetTot
write (1,*)" Auxiliary  1335 _tSiDredTot_", tSiDredTot
write (1,*)" Auxiliary  1336 _tDIMS_", tDIMS
write (1,*)" Auxiliary  1337 _tDHumS_", tDHumS
write (1,*)" Auxiliary  1338 _tDDetS_", tDDetS
write (1,*)" Auxiliary  1339 _vDeltaS_", vDeltaS
write (1,*)" Auxiliary  1340 _tDBurIM_", tDBurIM
write (1,*)" Auxiliary  1341 _tDBurOM_", tDBurOM
write (1,*)" Auxiliary  1342 _tDBurDet_", tDBurDet
write (1,*)" Auxiliary  1343 _tDBurHum_", tDBurHum
write (1,*)" Auxiliary  1344 _tDBurTot_", tDBurTot
write (1,*)" Auxiliary  1345 _tPBurHum_", tPBurHum
write (1,*)" Auxiliary  1346 _tPBurDet_", tPBurDet
write (1,*)" Auxiliary  1347 _tPBurAIM_", tPBurAIM
write (1,*)" Auxiliary  1348 _tPBurPO4_", tPBurPO4
write (1,*)" Auxiliary  1349 _tPBurTot_", tPBurTot
write (1,*)" Auxiliary  1350 _tNBurHum_", tNBurHum
write (1,*)" Auxiliary  1351 _tNBurDet_", tNBurDet
write (1,*)" Auxiliary  1352 _tNBurNH4_", tNBurNH4
write (1,*)" Auxiliary  1353 _tNBurNO3_", tNBurNO3
write (1,*)" Auxiliary  1354 _tNBurTot_", tNBurTot
write (1,*)" Auxiliary  1355 _tSiBurDet_", tSiBurDet
write (1,*)" Auxiliary  1356 _tSiBurTot_", tSiBurTot
write (1,*)" Auxiliary  1357 _vDeltaW_", vDeltaW
write (1,*)" Auxiliary  1358 _aRelDeltaW_", aRelDeltaW
write (1,*)" Auxiliary  1359 _tDMarsTotT_", tDMarsTotT
write (1,*)" Auxiliary  1360 _tPMarsTotT_", tPMarsTotT
write (1,*)" Auxiliary  1361 _tNMarsTotT_", tNMarsTotT
write (1,*)" Auxiliary  1362 _tSiMarsTotT_", tSiMarsTotT
write (1,*)" Auxiliary  1363 _aDTotT_", aDTotT
write (1,*)" Auxiliary  1364 _aNTotT_", aNTotT
write (1,*)" Auxiliary  1365 _aPTotT_", aPTotT
write (1,*)" Auxiliary  1366 _aSiTotT_", aSiTotT
write (1,*)" Auxiliary  1367 _aDError_", aDError
write (1,*)" Auxiliary  1368 _aNError_", aNError
write (1,*)" Auxiliary  1369 _aPError_", aPError
write (1,*)" Auxiliary  1370 _aSiError_", aSiError
write (1,*)" Derivative  0 _dDepthW_ -999"
write (1,*)" Derivative  1 _dNH4W_", dNH4W
write (1,*)" Derivative  2 _dNO3W_", dNO3W
write (1,*)" Derivative  3 _dPO4W_", dPO4W
write (1,*)" Derivative  4 _dPAIMW_", dPAIMW
write (1,*)" Derivative  5 _dSiO2W_", dSiO2W
write (1,*)" Derivative  6 _dO2W_", dO2W
write (1,*)" Derivative  7 _dDDetW_", dDDetW
write (1,*)" Derivative  8 _dNDetW_", dNDetW
write (1,*)" Derivative  9 _dPDetW_", dPDetW
write (1,*)" Derivative  10 _dSiDetW_", dSiDetW
write (1,*)" Derivative  11 _dDIMW_", dDIMW
write (1,*)" Derivative  12 _dDDiatW_", dDDiatW
write (1,*)" Derivative  13 _dNDiatW_", dNDiatW
write (1,*)" Derivative  14 _dPDiatW_", dPDiatW
write (1,*)" Derivative  15 _dDGrenW_", dDGrenW
write (1,*)" Derivative  16 _dNGrenW_", dNGrenW
write (1,*)" Derivative  17 _dPGrenW_", dPGrenW
write (1,*)" Derivative  18 _dDBlueW_", dDBlueW
write (1,*)" Derivative  19 _dNBlueW_", dNBlueW
write (1,*)" Derivative  20 _dPBlueW_", dPBlueW
write (1,*)" Derivative  21 _dDZoo_", dDZoo
write (1,*)" Derivative  22 _dNZoo_", dNZoo
write (1,*)" Derivative  23 _dPZoo_", dPZoo
write (1,*)" Derivative  24 _dDFiAd_", dDFiAd
write (1,*)" Derivative  25 _dDFiJv_", dDFiJv
write (1,*)" Derivative  26 _dNFiAd_", dNFiAd
write (1,*)" Derivative  27 _dNFiJv_", dNFiJv
write (1,*)" Derivative  28 _dPFiAd_", dPFiAd
write (1,*)" Derivative  29 _dPFiJv_", dPFiJv
write (1,*)" Derivative  30 _dDPisc_", dDPisc
write (1,*)" Derivative  31 _dNH4S_", dNH4S
write (1,*)" Derivative  32 _dNO3S_", dNO3S
write (1,*)" Derivative  33 _dPO4S_", dPO4S
write (1,*)" Derivative  34 _dPAIMS_", dPAIMS
write (1,*)" Derivative  35 _dDDetS_", dDDetS
write (1,*)" Derivative  36 _dNDetS_", dNDetS
write (1,*)" Derivative  37 _dPDetS_", dPDetS
write (1,*)" Derivative  38 _dSiDetS_", dSiDetS
write (1,*)" Derivative  39 _dDHumS_", dDHumS
write (1,*)" Derivative  40 _dNHumS_", dNHumS
write (1,*)" Derivative  41 _dPHumS_", dPHumS
write (1,*)" Derivative  42 _dDIMS_", dDIMS
write (1,*)" Derivative  43 _dDDiatS_", dDDiatS
write (1,*)" Derivative  44 _dNDiatS_", dNDiatS
write (1,*)" Derivative  45 _dPDiatS_", dPDiatS
write (1,*)" Derivative  46 _dDGrenS_", dDGrenS
write (1,*)" Derivative  47 _dNGrenS_", dNGrenS
write (1,*)" Derivative  48 _dPGrenS_", dPGrenS
write (1,*)" Derivative  49 _dDBlueS_", dDBlueS
write (1,*)" Derivative  50 _dNBlueS_", dNBlueS
write (1,*)" Derivative  51 _dPBlueS_", dPBlueS
write (1,*)" Derivative  52 _dDVeg_", dDVeg
write (1,*)" Derivative  53 _dNVeg_", dNVeg
write (1,*)" Derivative  54 _dPVeg_", dPVeg
write (1,*)" Derivative  55 _dDBent_", dDBent
write (1,*)" Derivative  56 _dNBent_", dNBent
write (1,*)" Derivative  57 _dPBent_", dPBent
write (1,*)" Derivative  58 _dDepthWM_", dDepthWM
write (1,*)" Derivative  59 _dNH4WM_", dNH4WM
write (1,*)" Derivative  60 _dNO3WM_", dNO3WM
write (1,*)" Derivative  61 _dPO4WM_", dPO4WM
write (1,*)" Derivative  62 _dPAIMWM_", dPAIMWM
write (1,*)" Derivative  63 _dSiO2WM_", dSiO2WM
write (1,*)" Derivative  64 _dO2WM_", dO2WM
write (1,*)" Derivative  65 _dDDetWM_", dDDetWM
write (1,*)" Derivative  66 _dNDetWM_", dNDetWM
write (1,*)" Derivative  67 _dPDetWM_", dPDetWM
write (1,*)" Derivative  68 _dSiDetWM_", dSiDetWM
write (1,*)" Derivative  69 _dDIMWM_", dDIMWM
write (1,*)" Derivative  70 _dDDiatWM_", dDDiatWM
write (1,*)" Derivative  71 _dNDiatWM_", dNDiatWM
write (1,*)" Derivative  72 _dPDiatWM_", dPDiatWM
write (1,*)" Derivative  73 _dDGrenWM_", dDGrenWM
write (1,*)" Derivative  74 _dNGrenWM_", dNGrenWM
write (1,*)" Derivative  75 _dPGrenWM_", dPGrenWM
write (1,*)" Derivative  76 _dDBlueWM_", dDBlueWM
write (1,*)" Derivative  77 _dNBlueWM_", dNBlueWM
write (1,*)" Derivative  78 _dPBlueWM_", dPBlueWM
write (1,*)" Derivative  79 _dDZooM_", dDZooM
write (1,*)" Derivative  80 _dNZooM_", dNZooM
write (1,*)" Derivative  81 _dPZooM_", dPZooM
write (1,*)" Derivative  82 _dNH4SM_", dNH4SM
write (1,*)" Derivative  83 _dNO3SM_", dNO3SM
write (1,*)" Derivative  84 _dPO4SM_", dPO4SM
write (1,*)" Derivative  85 _dPAIMSM_", dPAIMSM
write (1,*)" Derivative  86 _dDDetSM_", dDDetSM
write (1,*)" Derivative  87 _dNDetSM_", dNDetSM
write (1,*)" Derivative  88 _dPDetSM_", dPDetSM
write (1,*)" Derivative  89 _dSiDetSM_", dSiDetSM
write (1,*)" Derivative  90 _dDHumSM_", dDHumSM
write (1,*)" Derivative  91 _dNHumSM_", dNHumSM
write (1,*)" Derivative  92 _dPHumSM_", dPHumSM
write (1,*)" Derivative  93 _dDIMSM_", dDIMSM
write (1,*)" Derivative  94 _dDRootPhra_", dDRootPhra
write (1,*)" Derivative  95 _dDShootPhra_", dDShootPhra
write (1,*)" Derivative  96 _dNRootPhra_", dNRootPhra
write (1,*)" Derivative  97 _dNShootPhra_", dNShootPhra
write (1,*)" Derivative  98 _dPRootPhra_", dPRootPhra
write (1,*)" Derivative  99 _dPShootPhra_", dPShootPhra
write (1,*)" Derivative  100 _dDExtTotT_", dDExtTotT
write (1,*)" Derivative  101 _dNExtTotT_", dNExtTotT
write (1,*)" Derivative  102 _dPExtTotT_", dPExtTotT
write (1,*)" Derivative  103 _dSiExtTotT_", dSiExtTotT
     counter1 = 1
Close (1)
End If
!
if (counter2 == 0) then
open (unit=2,file="staterep.txt",action="write",status="replace")
write (2,*) "STEP ", &
&"sDepthW ", &
&"sNH4W ", &
&"sNO3W ", &
&"sPO4W ", &
&"sPAIMW ", &
&"sSiO2W ", &
&"sO2W ", &
&"sDDetW ", &
&"sNDetW ", &
&"sPDetW ", &
&"sSiDetW ", &
&"sDIMW ", &
&"sDDiatW ", &
&"sNDiatW ", &
&"sPDiatW ", &
&"sDGrenW ", &
&"sNGrenW ", &
&"sPGrenW ", &
&"sDBlueW ", &
&"sNBlueW ", &
&"sPBlueW ", &
&"sDZoo ", &
&"sNZoo ", &
&"sPZoo ", &
&"sDFiAd ", &
&"sDFiJv ", &
&"sNFiAd ", &
&"sNFiJv ", &
&"sPFiAd ", &
&"sPFiJv ", &
&"sDPisc ", &
&"sNH4S ", &
&"sNO3S ", &
&"sPO4S ", &
&"sPAIMS ", &
&"sDDetS ", &
&"sNDetS ", &
&"sPDetS ", &
&"sSiDetS ", &
&"sDHumS ", &
&"sNHumS ", &
&"sPHumS ", &
&"sDIMS ", &
&"sDDiatS ", &
&"sNDiatS ", &
&"sPDiatS ", &
&"sDGrenS ", &
&"sNGrenS ", &
&"sPGrenS ", &
&"sDBlueS ", &
&"sNBlueS ", &
&"sPBlueS ", &
&"sDVeg ", &
&"sNVeg ", &
&"sPVeg ", &
&"sDBent ", &
&"sNBent ", &
&"sPBent ", &
&"sDepthWM ", &
&"sNH4WM ", &
&"sNO3WM ", &
&"sPO4WM ", &
&"sPAIMWM ", &
&"sSiO2WM ", &
&"sO2WM ", &
&"sDDetWM ", &
&"sNDetWM ", &
&"sPDetWM ", &
&"sSiDetWM ", &
&"sDIMWM ", &
&"sDDiatWM ", &
&"sNDiatWM ", &
&"sPDiatWM ", &
&"sDGrenWM ", &
&"sNGrenWM ", &
&"sPGrenWM ", &
&"sDBlueWM ", &
&"sNBlueWM ", &
&"sPBlueWM ", &
&"sDZooM ", &
&"sNZooM ", &
&"sPZooM ", &
&"sNH4SM ", &
&"sNO3SM ", &
&"sPO4SM ", &
&"sPAIMSM ", &
&"sDDetSM ", &
&"sNDetSM ", &
&"sPDetSM ", &
&"sSiDetSM ", &
&"sDHumSM ", &
&"sNHumSM ", &
&"sPHumSM ", &
&"sDIMSM ", &
&"sDRootPhra ", &
&"sDShootPhra ", &
&"sNRootPhra ", &
&"sNShootPhra ", &
&"sPRootPhra ", &
&"sPShootPhra ", &
&"sDExtTotT ", &
&"sNExtTotT ", &
&"sPExtTotT ", &
&"sSiExtTotT "
     counter2 = 1
Close (2)
End If
!
if (sTime > counter3) then
OPEN (3, FILE = 'staterep.txt', ACCESS = 'APPEND')
write(3,*) sTime, &
&sDepthW , &
&sNH4W , &
&sNO3W , &
&sPO4W , &
&sPAIMW , &
&sSiO2W , &
&sO2W , &
&sDDetW , &
&sNDetW , &
&sPDetW , &
&sSiDetW , &
&sDIMW , &
&sDDiatW , &
&sNDiatW , &
&sPDiatW , &
&sDGrenW , &
&sNGrenW , &
&sPGrenW , &
&sDBlueW , &
&sNBlueW , &
&sPBlueW , &
&sDZoo , &
&sNZoo , &
&sPZoo , &
&sDFiAd , &
&sDFiJv , &
&sNFiAd , &
&sNFiJv , &
&sPFiAd , &
&sPFiJv , &
&sDPisc , &
&sNH4S , &
&sNO3S , &
&sPO4S , &
&sPAIMS , &
&sDDetS , &
&sNDetS , &
&sPDetS , &
&sSiDetS , &
&sDHumS , &
&sNHumS , &
&sPHumS , &
&sDIMS , &
&sDDiatS , &
&sNDiatS , &
&sPDiatS , &
&sDGrenS , &
&sNGrenS , &
&sPGrenS , &
&sDBlueS , &
&sNBlueS , &
&sPBlueS , &
&sDVeg , &
&sNVeg , &
&sPVeg , &
&sDBent , &
&sNBent , &
&sPBent , &
&sDepthWM , &
&sNH4WM , &
&sNO3WM , &
&sPO4WM , &
&sPAIMWM , &
&sSiO2WM , &
&sO2WM , &
&sDDetWM , &
&sNDetWM , &
&sPDetWM , &
&sSiDetWM , &
&sDIMWM , &
&sDDiatWM , &
&sNDiatWM , &
&sPDiatWM , &
&sDGrenWM , &
&sNGrenWM , &
&sPGrenWM , &
&sDBlueWM , &
&sNBlueWM , &
&sPBlueWM , &
&sDZooM , &
&sNZooM , &
&sPZooM , &
&sNH4SM , &
&sNO3SM , &
&sPO4SM , &
&sPAIMSM , &
&sDDetSM , &
&sNDetSM , &
&sPDetSM , &
&sSiDetSM , &
&sDHumSM , &
&sNHumSM , &
&sPHumSM , &
&sDIMSM , &
&sDRootPhra , &
&sDShootPhra , &
&sNRootPhra , &
&sNShootPhra , &
&sPRootPhra , &
&sPShootPhra , &
&sDExtTotT , &
&sNExtTotT , &
&sPExtTotT , &
&sSiExtTotT 
Close (3)
     counter3 = counter3 +  1 
End If
!
!
!     /* ==============================  */
!     /* integration calls               */
!     /* ==============================  */
      D0sNH4W          	 =  dNH4W
      D0sNO3W          	 =  dNO3W
      D0sPO4W          	 =  dPO4W
      D0sPAIMW         	 =  dPAIMW
      D0sSiO2W         	 =  dSiO2W
      D0sO2W           	 =  dO2W
      D0sDDetW         	 =  dDDetW
      D0sNDetW         	 =  dNDetW
      D0sPDetW         	 =  dPDetW
      D0sSiDetW        	 =  dSiDetW
      D0sDIMW          	 =  dDIMW
      D0sDDiatW        	 =  dDDiatW
      D0sNDiatW        	 =  dNDiatW
      D0sPDiatW        	 =  dPDiatW
      D0sDGrenW        	 =  dDGrenW
      D0sNGrenW        	 =  dNGrenW
      D0sPGrenW        	 =  dPGrenW
      D0sDBlueW        	 =  dDBlueW
      D0sNBlueW        	 =  dNBlueW
      D0sPBlueW        	 =  dPBlueW
      D0sDZoo          	 =  dDZoo
      D0sNZoo          	 =  dNZoo
      D0sPZoo          	 =  dPZoo
      D0sDFiAd         	 =  dDFiAd
      D0sDFiJv         	 =  dDFiJv
      D0sNFiAd         	 =  dNFiAd
      D0sNFiJv         	 =  dNFiJv
      D0sPFiAd         	 =  dPFiAd
      D0sPFiJv         	 =  dPFiJv
      D0sDPisc         	 =  dDPisc
      D0sNH4S          	 =  dNH4S
      D0sNO3S          	 =  dNO3S
      D0sPO4S          	 =  dPO4S
      D0sPAIMS         	 =  dPAIMS
      D0sDDetS         	 =  dDDetS
      D0sNDetS         	 =  dNDetS
      D0sPDetS         	 =  dPDetS
      D0sSiDetS        	 =  dSiDetS
      D0sDHumS         	 =  dDHumS
      D0sNHumS         	 =  dNHumS
      D0sPHumS         	 =  dPHumS
      D0sDIMS          	 =  dDIMS
      D0sDDiatS        	 =  dDDiatS
      D0sNDiatS        	 =  dNDiatS
      D0sPDiatS        	 =  dPDiatS
      D0sDGrenS        	 =  dDGrenS
      D0sNGrenS        	 =  dNGrenS
      D0sPGrenS        	 =  dPGrenS
      D0sDBlueS        	 =  dDBlueS
      D0sNBlueS        	 =  dNBlueS
      D0sPBlueS        	 =  dPBlueS
      D0sDVeg          	 =  dDVeg
      D0sNVeg          	 =  dNVeg
      D0sPVeg          	 =  dPVeg
      D0sDBent         	 =  dDBent
      D0sNBent         	 =  dNBent
      D0sPBent         	 =  dPBent
      D0sDepthWM       	 =  dDepthWM
      D0sNH4WM         	 =  dNH4WM
      D0sNO3WM         	 =  dNO3WM
      D0sPO4WM         	 =  dPO4WM
      D0sPAIMWM        	 =  dPAIMWM
      D0sSiO2WM        	 =  dSiO2WM
      D0sO2WM          	 =  dO2WM
      D0sDDetWM        	 =  dDDetWM
      D0sNDetWM        	 =  dNDetWM
      D0sPDetWM        	 =  dPDetWM
      D0sSiDetWM       	 =  dSiDetWM
      D0sDIMWM         	 =  dDIMWM
      D0sDDiatWM       	 =  dDDiatWM
      D0sNDiatWM       	 =  dNDiatWM
      D0sPDiatWM       	 =  dPDiatWM
      D0sDGrenWM       	 =  dDGrenWM
      D0sNGrenWM       	 =  dNGrenWM
      D0sPGrenWM       	 =  dPGrenWM
      D0sDBlueWM       	 =  dDBlueWM
      D0sNBlueWM       	 =  dNBlueWM
      D0sPBlueWM       	 =  dPBlueWM
      D0sDZooM         	 =  dDZooM
      D0sNZooM         	 =  dNZooM
      D0sPZooM         	 =  dPZooM
      D0sNH4SM         	 =  dNH4SM
      D0sNO3SM         	 =  dNO3SM
      D0sPO4SM         	 =  dPO4SM
      D0sPAIMSM        	 =  dPAIMSM
      D0sDDetSM        	 =  dDDetSM
      D0sNDetSM        	 =  dNDetSM
      D0sPDetSM        	 =  dPDetSM
      D0sSiDetSM       	 =  dSiDetSM
      D0sDHumSM        	 =  dDHumSM
      D0sNHumSM        	 =  dNHumSM
      D0sPHumSM        	 =  dPHumSM
      D0sDIMSM         	 =  dDIMSM
      D0sDRootPhra     	 =  dDRootPhra
      D0sDShootPhra    	 =  dDShootPhra
      D0sNRootPhra     	 =  dNRootPhra
      D0sNShootPhra    	 =  dNShootPhra
      D0sPRootPhra     	 =  dPRootPhra
      D0sPShootPhra    	 =  dPShootPhra
      D0sDExtTotT      	 =  dDExtTotT
      D0sNExtTotT      	 =  dNExtTotT
      D0sPExtTotT      	 =  dPExtTotT
      D0sSiExtTotT     	 =  dSiExtTotT

!   *****     DUPROL code ends here    *****

         fl  ( ID0sNH4W               	) = D0sNH4W               
         fl  ( ID0sNO3W               	) = D0sNO3W               
         fl  ( ID0sPO4W               	) = D0sPO4W               
         fl  ( ID0sPAIMW              	) = D0sPAIMW              
         fl  ( ID0sSiO2W              	) = D0sSiO2W              
         fl  ( ID0sO2W                	) = D0sO2W                
         fl  ( ID0sDDetW              	) = D0sDDetW              
         fl  ( ID0sNDetW              	) = D0sNDetW              
         fl  ( ID0sPDetW              	) = D0sPDetW              
         fl  ( ID0sSiDetW             	) = D0sSiDetW             
         fl  ( ID0sDIMW               	) = D0sDIMW               
         fl  ( ID0sDDiatW             	) = D0sDDiatW             
         fl  ( ID0sNDiatW             	) = D0sNDiatW             
         fl  ( ID0sPDiatW             	) = D0sPDiatW             
         fl  ( ID0sDGrenW             	) = D0sDGrenW             
         fl  ( ID0sNGrenW             	) = D0sNGrenW             
         fl  ( ID0sPGrenW             	) = D0sPGrenW             
         fl  ( ID0sDBlueW             	) = D0sDBlueW             
         fl  ( ID0sNBlueW             	) = D0sNBlueW             
         fl  ( ID0sPBlueW             	) = D0sPBlueW             
         fl  ( ID0sDZoo               	) = D0sDZoo               
         fl  ( ID0sNZoo               	) = D0sNZoo               
         fl  ( ID0sPZoo               	) = D0sPZoo               
         fl  ( ID0sDFiAd              	) = D0sDFiAd              
         fl  ( ID0sDFiJv              	) = D0sDFiJv              
         fl  ( ID0sNFiAd              	) = D0sNFiAd              
         fl  ( ID0sNFiJv              	) = D0sNFiJv              
         fl  ( ID0sPFiAd              	) = D0sPFiAd              
         fl  ( ID0sPFiJv              	) = D0sPFiJv              
         fl  ( ID0sDPisc              	) = D0sDPisc              
         fl  ( ID0sNH4S               	) = D0sNH4S               
         fl  ( ID0sNO3S               	) = D0sNO3S               
         fl  ( ID0sPO4S               	) = D0sPO4S               
         fl  ( ID0sPAIMS              	) = D0sPAIMS              
         fl  ( ID0sDDetS              	) = D0sDDetS              
         fl  ( ID0sNDetS              	) = D0sNDetS              
         fl  ( ID0sPDetS              	) = D0sPDetS              
         fl  ( ID0sSiDetS             	) = D0sSiDetS             
         fl  ( ID0sDHumS              	) = D0sDHumS              
         fl  ( ID0sNHumS              	) = D0sNHumS              
         fl  ( ID0sPHumS              	) = D0sPHumS              
         fl  ( ID0sDIMS               	) = D0sDIMS               
         fl  ( ID0sDDiatS             	) = D0sDDiatS             
         fl  ( ID0sNDiatS             	) = D0sNDiatS             
         fl  ( ID0sPDiatS             	) = D0sPDiatS             
         fl  ( ID0sDGrenS             	) = D0sDGrenS             
         fl  ( ID0sNGrenS             	) = D0sNGrenS             
         fl  ( ID0sPGrenS             	) = D0sPGrenS             
         fl  ( ID0sDBlueS             	) = D0sDBlueS             
         fl  ( ID0sNBlueS             	) = D0sNBlueS             
         fl  ( ID0sPBlueS             	) = D0sPBlueS             
         fl  ( ID0sDVeg               	) = D0sDVeg               
         fl  ( ID0sNVeg               	) = D0sNVeg               
         fl  ( ID0sPVeg               	) = D0sPVeg               
         fl  ( ID0sDBent              	) = D0sDBent              
         fl  ( ID0sNBent              	) = D0sNBent              
         fl  ( ID0sPBent              	) = D0sPBent              
         fl  ( ID0sDepthWM            	) = D0sDepthWM            
         fl  ( ID0sNH4WM              	) = D0sNH4WM              
         fl  ( ID0sNO3WM              	) = D0sNO3WM              
         fl  ( ID0sPO4WM              	) = D0sPO4WM              
         fl  ( ID0sPAIMWM             	) = D0sPAIMWM             
         fl  ( ID0sSiO2WM             	) = D0sSiO2WM             
         fl  ( ID0sO2WM               	) = D0sO2WM               
         fl  ( ID0sDDetWM             	) = D0sDDetWM             
         fl  ( ID0sNDetWM             	) = D0sNDetWM             
         fl  ( ID0sPDetWM             	) = D0sPDetWM             
         fl  ( ID0sSiDetWM            	) = D0sSiDetWM            
         fl  ( ID0sDIMWM              	) = D0sDIMWM              
         fl  ( ID0sDDiatWM            	) = D0sDDiatWM            
         fl  ( ID0sNDiatWM            	) = D0sNDiatWM            
         fl  ( ID0sPDiatWM            	) = D0sPDiatWM            
         fl  ( ID0sDGrenWM            	) = D0sDGrenWM            
         fl  ( ID0sNGrenWM            	) = D0sNGrenWM            
         fl  ( ID0sPGrenWM            	) = D0sPGrenWM            
         fl  ( ID0sDBlueWM            	) = D0sDBlueWM            
         fl  ( ID0sNBlueWM            	) = D0sNBlueWM            
         fl  ( ID0sPBlueWM            	) = D0sPBlueWM            
         fl  ( ID0sDZooM              	) = D0sDZooM              
         fl  ( ID0sNZooM              	) = D0sNZooM              
         fl  ( ID0sPZooM              	) = D0sPZooM              
         fl  ( ID0sNH4SM              	) = D0sNH4SM              
         fl  ( ID0sNO3SM              	) = D0sNO3SM              
         fl  ( ID0sPO4SM              	) = D0sPO4SM              
         fl  ( ID0sPAIMSM             	) = D0sPAIMSM             
         fl  ( ID0sDDetSM             	) = D0sDDetSM             
         fl  ( ID0sNDetSM             	) = D0sNDetSM             
         fl  ( ID0sPDetSM             	) = D0sPDetSM             
         fl  ( ID0sSiDetSM            	) = D0sSiDetSM            
         fl  ( ID0sDHumSM             	) = D0sDHumSM             
         fl  ( ID0sNHumSM             	) = D0sNHumSM             
         fl  ( ID0sPHumSM             	) = D0sPHumSM             
         fl  ( ID0sDIMSM              	) = D0sDIMSM              
         fl  ( ID0sDRootPhra          	) = D0sDRootPhra          
         fl  ( ID0sDShootPhra         	) = D0sDShootPhra         
         fl  ( ID0sNRootPhra          	) = D0sNRootPhra          
         fl  ( ID0sNShootPhra         	) = D0sNShootPhra         
         fl  ( ID0sPRootPhra          	) = D0sPRootPhra          
         fl  ( ID0sPShootPhra         	) = D0sPShootPhra         
         fl  ( ID0sDExtTotT           	) = D0sDExtTotT           
         fl  ( ID0sNExtTotT           	) = D0sNExtTotT           
         fl  ( ID0sPExtTotT           	) = D0sPExtTotT           
         fl  ( ID0sSiExtTotT          	) = D0sSiExtTotT          
         pmsa( ipnt( 561) ) = sDepthW
         pmsa( ipnt( 562) ) = sTime
         pmsa( ipnt( 563) ) = TimeYears
         pmsa( ipnt( 564) ) = Day
         pmsa( ipnt( 565) ) = Years
         pmsa( ipnt( 566) ) = uTm
         pmsa( ipnt( 567) ) = uVWind
         pmsa( ipnt( 568) ) = ufDay
         pmsa( ipnt( 569) ) = uLDay
         pmsa( ipnt( 570) ) = uLOut
         pmsa( ipnt( 571) ) = uLPARSurf
         pmsa( ipnt( 572) ) = aExtPhyt
         pmsa( ipnt( 573) ) = aExtDet
         pmsa( ipnt( 574) ) = aExtIM
         pmsa( ipnt( 575) ) = aExtCoefOpen
         pmsa( ipnt( 576) ) = uQInSeason
         pmsa( ipnt( 577) ) = uQEvSinus
         pmsa( ipnt( 578) ) = uQEv
         pmsa( ipnt( 579) ) = uQInExtra
         pmsa( ipnt( 580) ) = uQOutExtra
         pmsa( ipnt( 581) ) = uQIn
         pmsa( ipnt( 582) ) = uQOut
         pmsa( ipnt( 583) ) = uQDil
         pmsa( ipnt( 584) ) = ukDil
         pmsa( ipnt( 585) ) = ukDilWat
         pmsa( ipnt( 586) ) = ukOut
         pmsa( ipnt( 587) ) = uTauWat
         pmsa( ipnt( 588) ) = uTauSubst
         pmsa( ipnt( 589) ) = vTranDepthW
         pmsa( ipnt( 590) ) = akExchM
         pmsa( ipnt( 591) ) = afVolMarsh
         pmsa( ipnt( 592) ) = akExchL
         pmsa( ipnt( 593) ) = oDPhytW
         pmsa( ipnt( 594) ) = oPPhytW
         pmsa( ipnt( 595) ) = oNPhytW
         pmsa( ipnt( 596) ) = aDPhytS
         pmsa( ipnt( 597) ) = aPPhytS
         pmsa( ipnt( 598) ) = aNPhytS
         pmsa( ipnt( 599) ) = oDOMW
         pmsa( ipnt( 600) ) = oDSestW
         pmsa( ipnt( 601) ) = oPOMW
         pmsa( ipnt( 602) ) = oPSestW
         pmsa( ipnt( 603) ) = oPInorgW
         pmsa( ipnt( 604) ) = oPTotW
         pmsa( ipnt( 605) ) = oNDissW
         pmsa( ipnt( 606) ) = oNOMW
         pmsa( ipnt( 607) ) = oNSestW
         pmsa( ipnt( 608) ) = oNkjW
         pmsa( ipnt( 609) ) = oNTotW
         pmsa( ipnt( 610) ) = bPorS
         pmsa( ipnt( 611) ) = bPorCorS
         pmsa( ipnt( 612) ) = aDTotS
         pmsa( ipnt( 613) ) = aRhoTotS
         pmsa( ipnt( 614) ) = aRhoSolidS
         pmsa( ipnt( 615) ) = afDTotS
         pmsa( ipnt( 616) ) = afDOrgS
         pmsa( ipnt( 617) ) = afDetS
         pmsa( ipnt( 618) ) = afDetTotS
         pmsa( ipnt( 619) ) = aPInorgS
         pmsa( ipnt( 620) ) = aPTotAvailS
         pmsa( ipnt( 621) ) = aPTotS
         pmsa( ipnt( 622) ) = afPInorgS
         pmsa( ipnt( 623) ) = afPTotS
         pmsa( ipnt( 624) ) = afPO4S
         pmsa( ipnt( 625) ) = oPO4S
         pmsa( ipnt( 626) ) = aNDissS
         pmsa( ipnt( 627) ) = aNkjAvailS
         pmsa( ipnt( 628) ) = aNkjS
         pmsa( ipnt( 629) ) = aNTotAvailS
         pmsa( ipnt( 630) ) = aNTotS
         pmsa( ipnt( 631) ) = afNInorgS
         pmsa( ipnt( 632) ) = afNTotS
         pmsa( ipnt( 633) ) = oNO3S
         pmsa( ipnt( 634) ) = oNH4S
         pmsa( ipnt( 635) ) = oNDissS
         pmsa( ipnt( 636) ) = rPDIMW
         pmsa( ipnt( 637) ) = rPDIMS
         pmsa( ipnt( 638) ) = rPDDetW
         pmsa( ipnt( 639) ) = rNDDetW
         pmsa( ipnt( 640) ) = rSiDDetW
         pmsa( ipnt( 641) ) = rPDHumS
         pmsa( ipnt( 642) ) = rNDHumS
         pmsa( ipnt( 643) ) = rPDDetS
         pmsa( ipnt( 644) ) = rNDDetS
         pmsa( ipnt( 645) ) = rSiDDetS
         pmsa( ipnt( 646) ) = oDPhytWM
         pmsa( ipnt( 647) ) = oPPhytWM
         pmsa( ipnt( 648) ) = oNPhytWM
         pmsa( ipnt( 649) ) = oSiDiatWM
         pmsa( ipnt( 650) ) = oDOMWM
         pmsa( ipnt( 651) ) = oDSestWM
         pmsa( ipnt( 652) ) = oPOMWM
         pmsa( ipnt( 653) ) = oPSestWM
         pmsa( ipnt( 654) ) = oPInorgWM
         pmsa( ipnt( 655) ) = oPTotWM
         pmsa( ipnt( 656) ) = oNDissWM
         pmsa( ipnt( 657) ) = oNOMWM
         pmsa( ipnt( 658) ) = oNSestWM
         pmsa( ipnt( 659) ) = oNkjWM
         pmsa( ipnt( 660) ) = oNTotWM
         pmsa( ipnt( 661) ) = bPorSM
         pmsa( ipnt( 662) ) = bPorCorSM
         pmsa( ipnt( 663) ) = aDTotSM
         pmsa( ipnt( 664) ) = aRhoTotSM
         pmsa( ipnt( 665) ) = aRhoSolidSM
         pmsa( ipnt( 666) ) = afDTotSM
         pmsa( ipnt( 667) ) = afDOrgSM
         pmsa( ipnt( 668) ) = afDetSM
         pmsa( ipnt( 669) ) = afDetTotSM
         pmsa( ipnt( 670) ) = aPInorgSM
         pmsa( ipnt( 671) ) = aPTotAvailSM
         pmsa( ipnt( 672) ) = aPTotSM
         pmsa( ipnt( 673) ) = afPInorgSM
         pmsa( ipnt( 674) ) = afPTotSM
         pmsa( ipnt( 675) ) = afPO4SM
         pmsa( ipnt( 676) ) = oPO4SM
         pmsa( ipnt( 677) ) = aNDissSM
         pmsa( ipnt( 678) ) = aNkjAvailSM
         pmsa( ipnt( 679) ) = aNkjSM
         pmsa( ipnt( 680) ) = aNTotAvailSM
         pmsa( ipnt( 681) ) = aNTotSM
         pmsa( ipnt( 682) ) = afNInorgSM
         pmsa( ipnt( 683) ) = afNTotSM
         pmsa( ipnt( 684) ) = oNO3SM
         pmsa( ipnt( 685) ) = oNH4SM
         pmsa( ipnt( 686) ) = oNDissSM
         pmsa( ipnt( 687) ) = rPDIMWM
         pmsa( ipnt( 688) ) = rPDIMSM
         pmsa( ipnt( 689) ) = rPDDetWM
         pmsa( ipnt( 690) ) = rNDDetWM
         pmsa( ipnt( 691) ) = rSiDDetWM
         pmsa( ipnt( 692) ) = rPDHumSM
         pmsa( ipnt( 693) ) = rNDHumSM
         pmsa( ipnt( 694) ) = rPDDetSM
         pmsa( ipnt( 695) ) = rNDDetSM
         pmsa( ipnt( 696) ) = rSiDDetSM
         pmsa( ipnt( 697) ) = aDTotM
         pmsa( ipnt( 698) ) = aPTotM
         pmsa( ipnt( 699) ) = aNTotM
         pmsa( ipnt( 700) ) = aSiTotM
         pmsa( ipnt( 701) ) = iPPulse
         pmsa( ipnt( 702) ) = uPLoadSeason
         pmsa( ipnt( 703) ) = uPLoad
         pmsa( ipnt( 704) ) = uPLoadPO4
         pmsa( ipnt( 705) ) = uPLoadOrg
         pmsa( ipnt( 706) ) = uPLoadPhytTot
         pmsa( ipnt( 707) ) = uPLoadDet
         pmsa( ipnt( 708) ) = uPLoadAIM
         pmsa( ipnt( 709) ) = iNPulse
         pmsa( ipnt( 710) ) = uNLoadSeason
         pmsa( ipnt( 711) ) = uNLoadPhytTot
         pmsa( ipnt( 712) ) = uNLoad
         pmsa( ipnt( 713) ) = uNLoadDet
         pmsa( ipnt( 714) ) = uNLoadOrg
         pmsa( ipnt( 715) ) = uNLoadDiss
         pmsa( ipnt( 716) ) = uNLoadNH4
         pmsa( ipnt( 717) ) = uNLoadNO3
         pmsa( ipnt( 718) ) = uNTotIn
         pmsa( ipnt( 719) ) = uDLoadDet
         pmsa( ipnt( 720) ) = uDLoadPhytTot
         pmsa( ipnt( 721) ) = uDLoadIM
         pmsa( ipnt( 722) ) = uDLoad
         pmsa( ipnt( 723) ) = uPTotIn
         pmsa( ipnt( 724) ) = uDLoadDiat
         pmsa( ipnt( 725) ) = uPLoadDiat
         pmsa( ipnt( 726) ) = uNLoadDiat
         pmsa( ipnt( 727) ) = uDLoadGren
         pmsa( ipnt( 728) ) = uPLoadGren
         pmsa( ipnt( 729) ) = uNLoadGren
         pmsa( ipnt( 730) ) = uDLoadBlue
         pmsa( ipnt( 731) ) = uPLoadBlue
         pmsa( ipnt( 732) ) = uNLoadBlue
         pmsa( ipnt( 733) ) = wDDilIM
         pmsa( ipnt( 734) ) = wDDilDet
         pmsa( ipnt( 735) ) = wPDilPO4
         pmsa( ipnt( 736) ) = wPDilDet
         pmsa( ipnt( 737) ) = wPDilAIM
         pmsa( ipnt( 738) ) = wNDilNH4
         pmsa( ipnt( 739) ) = wNDilNO3
         pmsa( ipnt( 740) ) = wNDilDet
         pmsa( ipnt( 741) ) = wO2Inflow
         pmsa( ipnt( 742) ) = wO2Outfl
         pmsa( ipnt( 743) ) = wDDilDiat
         pmsa( ipnt( 744) ) = wPDilDiat
         pmsa( ipnt( 745) ) = wNDilDiat
         pmsa( ipnt( 746) ) = wDDilGren
         pmsa( ipnt( 747) ) = wPDilGren
         pmsa( ipnt( 748) ) = wNDilGren
         pmsa( ipnt( 749) ) = wDDilBlue
         pmsa( ipnt( 750) ) = wPDilBlue
         pmsa( ipnt( 751) ) = wNDilBlue
         pmsa( ipnt( 752) ) = wDDilPhyt
         pmsa( ipnt( 753) ) = wPDilPhyt
         pmsa( ipnt( 754) ) = wNDilPhyt
         pmsa( ipnt( 755) ) = wDOutflTot
         pmsa( ipnt( 756) ) = wPOutflTot
         pmsa( ipnt( 757) ) = wNOutflTot
         pmsa( ipnt( 758) ) = wDTranDiat
         pmsa( ipnt( 759) ) = wPTranDiat
         pmsa( ipnt( 760) ) = wNTranDiat
         pmsa( ipnt( 761) ) = wDTranGren
         pmsa( ipnt( 762) ) = wPTranGren
         pmsa( ipnt( 763) ) = wNTranGren
         pmsa( ipnt( 764) ) = wDTranBlue
         pmsa( ipnt( 765) ) = wPTranBlue
         pmsa( ipnt( 766) ) = wNTranBlue
         pmsa( ipnt( 767) ) = wDTranPhyt
         pmsa( ipnt( 768) ) = wPTranPhyt
         pmsa( ipnt( 769) ) = wNTranPhyt
         pmsa( ipnt( 770) ) = uSiLoadSiO2
         pmsa( ipnt( 771) ) = uSiLoadDet
         pmsa( ipnt( 772) ) = uSiLoadDiat
         pmsa( ipnt( 773) ) = uSiLoad
         pmsa( ipnt( 774) ) = wSiDilSiO2
         pmsa( ipnt( 775) ) = wSiDilDet
         pmsa( ipnt( 776) ) = wSiDilDiat
         pmsa( ipnt( 777) ) = wSiOutflTot
         pmsa( ipnt( 778) ) = wSiTranSiO2
         pmsa( ipnt( 779) ) = wSiTranDetW
         pmsa( ipnt( 780) ) = tSiTranTotT
         pmsa( ipnt( 781) ) = wDTranZoo
         pmsa( ipnt( 782) ) = wPTranZoo
         pmsa( ipnt( 783) ) = wNTranZoo
         pmsa( ipnt( 784) ) = wDTranIMW
         pmsa( ipnt( 785) ) = wDTranDetW
         pmsa( ipnt( 786) ) = wO2TranW
         pmsa( ipnt( 787) ) = wPTranPO4W
         pmsa( ipnt( 788) ) = wPTranAIMW
         pmsa( ipnt( 789) ) = wPTranDetW
         pmsa( ipnt( 790) ) = wNTranNH4W
         pmsa( ipnt( 791) ) = wNTranNO3W
         pmsa( ipnt( 792) ) = wNTranDetW
         pmsa( ipnt( 793) ) = wDDilTot
         pmsa( ipnt( 794) ) = wPDilTot
         pmsa( ipnt( 795) ) = wNDilTot
         pmsa( ipnt( 796) ) = wSiDilTot
         pmsa( ipnt( 797) ) = tDTranTotT
         pmsa( ipnt( 798) ) = tPTranTotT
         pmsa( ipnt( 799) ) = tNTranTotT
         pmsa( ipnt( 800) ) = wDExchIMM
         pmsa( ipnt( 801) ) = wPExchPO4M
         pmsa( ipnt( 802) ) = wPExchAIMM
         pmsa( ipnt( 803) ) = wNExchNH4M
         pmsa( ipnt( 804) ) = wNExchNO3M
         pmsa( ipnt( 805) ) = wSiExchSiO2M
         pmsa( ipnt( 806) ) = wO2ExchM
         pmsa( ipnt( 807) ) = wDExchDetM
         pmsa( ipnt( 808) ) = wPExchDetM
         pmsa( ipnt( 809) ) = wNExchDetM
         pmsa( ipnt( 810) ) = wSiExchDetM
         pmsa( ipnt( 811) ) = wDExchDiatM
         pmsa( ipnt( 812) ) = wPExchDiatM
         pmsa( ipnt( 813) ) = wNExchDiatM
         pmsa( ipnt( 814) ) = wSiExchDiatM
         pmsa( ipnt( 815) ) = wDExchGrenM
         pmsa( ipnt( 816) ) = wPExchGrenM
         pmsa( ipnt( 817) ) = wNExchGrenM
         pmsa( ipnt( 818) ) = wDExchBlueM
         pmsa( ipnt( 819) ) = wPExchBlueM
         pmsa( ipnt( 820) ) = wNExchBlueM
         pmsa( ipnt( 821) ) = wDExchZooM
         pmsa( ipnt( 822) ) = wPExchZooM
         pmsa( ipnt( 823) ) = wNExchZooM
         pmsa( ipnt( 824) ) = wDExchIM
         pmsa( ipnt( 825) ) = wPExchPO4
         pmsa( ipnt( 826) ) = wPExchAIM
         pmsa( ipnt( 827) ) = wNExchNH4
         pmsa( ipnt( 828) ) = wNExchNO3
         pmsa( ipnt( 829) ) = wSiExchSiO2
         pmsa( ipnt( 830) ) = wO2Exch
         pmsa( ipnt( 831) ) = wDExchDet
         pmsa( ipnt( 832) ) = wPExchDet
         pmsa( ipnt( 833) ) = wNExchDet
         pmsa( ipnt( 834) ) = wSiExchDet
         pmsa( ipnt( 835) ) = wDExchDiat
         pmsa( ipnt( 836) ) = wPExchDiat
         pmsa( ipnt( 837) ) = wNExchDiat
         pmsa( ipnt( 838) ) = wSiExchDiat
         pmsa( ipnt( 839) ) = wDExchGren
         pmsa( ipnt( 840) ) = wPExchGren
         pmsa( ipnt( 841) ) = wNExchGren
         pmsa( ipnt( 842) ) = wDExchBlue
         pmsa( ipnt( 843) ) = wPExchBlue
         pmsa( ipnt( 844) ) = wNExchBlue
         pmsa( ipnt( 845) ) = wDExchZoo
         pmsa( ipnt( 846) ) = wPExchZoo
         pmsa( ipnt( 847) ) = wNExchZoo
         pmsa( ipnt( 848) ) = tPInfPO4W
         pmsa( ipnt( 849) ) = tNInfNH4W
         pmsa( ipnt( 850) ) = tNInfNO3W
         pmsa( ipnt( 851) ) = tPInfPO4S
         pmsa( ipnt( 852) ) = tNInfNH4S
         pmsa( ipnt( 853) ) = tNInfNO3S
         pmsa( ipnt( 854) ) = tNH4LoadS
         pmsa( ipnt( 855) ) = tNO3LoadS
         pmsa( ipnt( 856) ) = uDErosIM
         pmsa( ipnt( 857) ) = uDErosIMS
         pmsa( ipnt( 858) ) = uDErosIMW
         pmsa( ipnt( 859) ) = uDErosOM
         pmsa( ipnt( 860) ) = uPErosOM
         pmsa( ipnt( 861) ) = uNErosOM
         pmsa( ipnt( 862) ) = uO2Sat
         pmsa( ipnt( 863) ) = kAer
         pmsa( ipnt( 864) ) = uFunTmAer
         pmsa( ipnt( 865) ) = aFunLemnAer
         pmsa( ipnt( 866) ) = tO2Aer
         pmsa( ipnt( 867) ) = uFunTmFish
         pmsa( ipnt( 868) ) = tDTurbFish
         pmsa( ipnt( 869) ) = tDTurbFishIM
         pmsa( ipnt( 870) ) = aFunVegResus
         pmsa( ipnt( 871) ) = aFunDimSusp
         pmsa( ipnt( 872) ) = tDResusTauDead
         pmsa( ipnt( 873) ) = tDResusBareDead
         pmsa( ipnt( 874) ) = tDResusDead
         pmsa( ipnt( 875) ) = tDResusIM
         pmsa( ipnt( 876) ) = tDResusDet
         pmsa( ipnt( 877) ) = akResusPhytRef
         pmsa( ipnt( 878) ) = tDResusPhytTot
         pmsa( ipnt( 879) ) = tPResusDet
         pmsa( ipnt( 880) ) = tPResusPO4
         pmsa( ipnt( 881) ) = tPResusAIM
         pmsa( ipnt( 882) ) = tNResusNO3
         pmsa( ipnt( 883) ) = tNResusNH4
         pmsa( ipnt( 884) ) = tNResusDet
         pmsa( ipnt( 885) ) = tSiResusDet
         pmsa( ipnt( 886) ) = aFunTauSetOM
         pmsa( ipnt( 887) ) = aFunTauSetIM
         pmsa( ipnt( 888) ) = uFunTmSet
         pmsa( ipnt( 889) ) = uCorVSetIM
         pmsa( ipnt( 890) ) = tDSetIM
         pmsa( ipnt( 891) ) = tPSetAIM
         pmsa( ipnt( 892) ) = uCorVSetDet
         pmsa( ipnt( 893) ) = tDSetDet
         pmsa( ipnt( 894) ) = tPSetDet
         pmsa( ipnt( 895) ) = tNSetDet
         pmsa( ipnt( 896) ) = tSiSetDet
         pmsa( ipnt( 897) ) = kPMinDetW
         pmsa( ipnt( 898) ) = kNMinDetW
         pmsa( ipnt( 899) ) = kSiMinDetW
         pmsa( ipnt( 900) ) = uFunTmMinW
         pmsa( ipnt( 901) ) = wDMinDetW
         pmsa( ipnt( 902) ) = wPMinDetW
         pmsa( ipnt( 903) ) = wNMinDetW
         pmsa( ipnt( 904) ) = wSiMinDetW
         pmsa( ipnt( 905) ) = aCorO2BOD
         pmsa( ipnt( 906) ) = wO2MinDetW
         pmsa( ipnt( 907) ) = wDDenitW
         pmsa( ipnt( 908) ) = wNDenitW
         pmsa( ipnt( 909) ) = uFunTmNitr
         pmsa( ipnt( 910) ) = aCorO2NitrW
         pmsa( ipnt( 911) ) = wNNitrW
         pmsa( ipnt( 912) ) = wO2NitrW
         pmsa( ipnt( 913) ) = kPMinDetS
         pmsa( ipnt( 914) ) = kNMinDetS
         pmsa( ipnt( 915) ) = kSiMinDetS
         pmsa( ipnt( 916) ) = uFunTmMinS
         pmsa( ipnt( 917) ) = tDMinDetS
         pmsa( ipnt( 918) ) = tPMinDetS
         pmsa( ipnt( 919) ) = tNMinDetS
         pmsa( ipnt( 920) ) = tSiMinDetS
         pmsa( ipnt( 921) ) = uFunTmDif
         pmsa( ipnt( 922) ) = akO2DifCor
         pmsa( ipnt( 923) ) = tSOD
         pmsa( ipnt( 924) ) = aDepthOxySed
         pmsa( ipnt( 925) ) = afOxySed
         pmsa( ipnt( 926) ) = tDMinOxyDetS
         pmsa( ipnt( 927) ) = tO2MinDetS
         pmsa( ipnt( 928) ) = tDDenitS
         pmsa( ipnt( 929) ) = tNDenitS
         pmsa( ipnt( 930) ) = tNNitrS
         pmsa( ipnt( 931) ) = tO2NitrS
         pmsa( ipnt( 932) ) = tDMinHumS
         pmsa( ipnt( 933) ) = tPMinHumS
         pmsa( ipnt( 934) ) = tNMinHumS
         pmsa( ipnt( 935) ) = aDepthDif
         pmsa( ipnt( 936) ) = tPDifPO4
         pmsa( ipnt( 937) ) = tNDifNO3
         pmsa( ipnt( 938) ) = tNDifNH4
         pmsa( ipnt( 939) ) = tO2Dif
         pmsa( ipnt( 940) ) = tPDifGroundPO4
         pmsa( ipnt( 941) ) = tNDifGroundNO3
         pmsa( ipnt( 942) ) = tNDifGroundNH4
         pmsa( ipnt( 943) ) = aPAdsMaxW
         pmsa( ipnt( 944) ) = aKPAdsW
         pmsa( ipnt( 945) ) = aPIsoAdsW
         pmsa( ipnt( 946) ) = aPEqIMW
         pmsa( ipnt( 947) ) = wPSorpIMW
         pmsa( ipnt( 948) ) = aPAdsMaxS
         pmsa( ipnt( 949) ) = aKPAdsS
         pmsa( ipnt( 950) ) = aPIsoAdsS
         pmsa( ipnt( 951) ) = aPEqIMS
         pmsa( ipnt( 952) ) = tPSorpIMS
         pmsa( ipnt( 953) ) = tPChemPO4
         pmsa( ipnt( 954) ) = wDAbioIMW
         pmsa( ipnt( 955) ) = wDAbioDetW
         pmsa( ipnt( 956) ) = tDAbioIMS
         pmsa( ipnt( 957) ) = tDAbioDetS
         pmsa( ipnt( 958) ) = tDAbioHumS
         pmsa( ipnt( 959) ) = tDAbioTotT
         pmsa( ipnt( 960) ) = wO2AbioW
         pmsa( ipnt( 961) ) = wPAbioDetW
         pmsa( ipnt( 962) ) = wPAbioPO4W
         pmsa( ipnt( 963) ) = wPAbioAIMW
         pmsa( ipnt( 964) ) = tPAbioDetS
         pmsa( ipnt( 965) ) = tPAbioHumS
         pmsa( ipnt( 966) ) = tPAbioPO4S
         pmsa( ipnt( 967) ) = tPAbioAIMS
         pmsa( ipnt( 968) ) = tPAbioTotT
         pmsa( ipnt( 969) ) = wNAbioNH4W
         pmsa( ipnt( 970) ) = wNAbioNO3W
         pmsa( ipnt( 971) ) = wNAbioDetW
         pmsa( ipnt( 972) ) = tNAbioNH4S
         pmsa( ipnt( 973) ) = tNAbioNO3S
         pmsa( ipnt( 974) ) = tNAbioDetS
         pmsa( ipnt( 975) ) = tNAbioHumS
         pmsa( ipnt( 976) ) = tNAbioTotT
         pmsa( ipnt( 977) ) = wSiAbioSiO2W
         pmsa( ipnt( 978) ) = wSiAbioDetW
         pmsa( ipnt( 979) ) = tSiAbioDetS
         pmsa( ipnt( 980) ) = tSiAbioTotT
         pmsa( ipnt( 981) ) = uQEvPhra
         pmsa( ipnt( 982) ) = tPEvPO4WM
         pmsa( ipnt( 983) ) = tNEvNH4WM
         pmsa( ipnt( 984) ) = tNEvNO3WM
         pmsa( ipnt( 985) ) = tPInfPO4WM
         pmsa( ipnt( 986) ) = tNInfNH4WM
         pmsa( ipnt( 987) ) = tNInfNO3WM
         pmsa( ipnt( 988) ) = tPInfPO4SM
         pmsa( ipnt( 989) ) = tNInfNH4SM
         pmsa( ipnt( 990) ) = tNInfNO3SM
         pmsa( ipnt( 991) ) = tO2AerM
         pmsa( ipnt( 992) ) = tDSetIMM
         pmsa( ipnt( 993) ) = tPSetAIMM
         pmsa( ipnt( 994) ) = tDSetDetM
         pmsa( ipnt( 995) ) = tPSetDetM
         pmsa( ipnt( 996) ) = tNSetDetM
         pmsa( ipnt( 997) ) = tSiSetDetM
         pmsa( ipnt( 998) ) = tDSetDiatM
         pmsa( ipnt( 999) ) = tPSetDiatM
         pmsa( ipnt( 1000) ) = tNSetDiatM
         pmsa( ipnt( 1001) ) = tSiSetDiatM
         pmsa( ipnt( 1002) ) = tDSetGrenM
         pmsa( ipnt( 1003) ) = tPSetGrenM
         pmsa( ipnt( 1004) ) = tNSetGrenM
         pmsa( ipnt( 1005) ) = tDSetBlueM
         pmsa( ipnt( 1006) ) = tPSetBlueM
         pmsa( ipnt( 1007) ) = tNSetBlueM
         pmsa( ipnt( 1008) ) = tDSetPhytM
         pmsa( ipnt( 1009) ) = tPSetPhytM
         pmsa( ipnt( 1010) ) = tNSetPhytM
         pmsa( ipnt( 1011) ) = tDSetTotM
         pmsa( ipnt( 1012) ) = wDMinDetWM
         pmsa( ipnt( 1013) ) = wPMinDetWM
         pmsa( ipnt( 1014) ) = wNMinDetWM
         pmsa( ipnt( 1015) ) = wSiMinDetWM
         pmsa( ipnt( 1016) ) = aCorO2BODM
         pmsa( ipnt( 1017) ) = wO2MinDetWM
         pmsa( ipnt( 1018) ) = wDDenitWM
         pmsa( ipnt( 1019) ) = wNDenitWM
         pmsa( ipnt( 1020) ) = aCorO2NitrWM
         pmsa( ipnt( 1021) ) = wNNitrWM
         pmsa( ipnt( 1022) ) = wO2NitrWM
         pmsa( ipnt( 1023) ) = tDMinDetSM
         pmsa( ipnt( 1024) ) = tPMinDetSM
         pmsa( ipnt( 1025) ) = tNMinDetSM
         pmsa( ipnt( 1026) ) = tSiMinDetSM
         pmsa( ipnt( 1027) ) = akO2DifCorM
         pmsa( ipnt( 1028) ) = tSODM
         pmsa( ipnt( 1029) ) = aDepthOxySedM
         pmsa( ipnt( 1030) ) = afOxySedM
         pmsa( ipnt( 1031) ) = tDMinOxyDetSM
         pmsa( ipnt( 1032) ) = tO2MinDetSM
         pmsa( ipnt( 1033) ) = tDDenitSM
         pmsa( ipnt( 1034) ) = tNDenitSM
         pmsa( ipnt( 1035) ) = tNNitrSM
         pmsa( ipnt( 1036) ) = tO2NitrSM
         pmsa( ipnt( 1037) ) = tDMinHumSM
         pmsa( ipnt( 1038) ) = tPMinHumSM
         pmsa( ipnt( 1039) ) = tNMinHumSM
         pmsa( ipnt( 1040) ) = aDepthDifM
         pmsa( ipnt( 1041) ) = tPDifPO4M
         pmsa( ipnt( 1042) ) = tNDifNO3M
         pmsa( ipnt( 1043) ) = tNDifNH4M
         pmsa( ipnt( 1044) ) = tO2DifM
         pmsa( ipnt( 1045) ) = tPDifGroundPO4M
         pmsa( ipnt( 1046) ) = tNDifGroundNO3M
         pmsa( ipnt( 1047) ) = tNDifGroundNH4M
         pmsa( ipnt( 1048) ) = aPAdsMaxWM
         pmsa( ipnt( 1049) ) = aKPAdsWM
         pmsa( ipnt( 1050) ) = aPIsoAdsWM
         pmsa( ipnt( 1051) ) = aPEqIMWM
         pmsa( ipnt( 1052) ) = wPSorpIMWM
         pmsa( ipnt( 1053) ) = aPAdsMaxSM
         pmsa( ipnt( 1054) ) = aKPAdsSM
         pmsa( ipnt( 1055) ) = aPIsoAdsSM
         pmsa( ipnt( 1056) ) = aPEqIMSM
         pmsa( ipnt( 1057) ) = tPSorpIMSM
         pmsa( ipnt( 1058) ) = tPChemPO4M
         pmsa( ipnt( 1059) ) = aDayInitVeg
         pmsa( ipnt( 1060) ) = bfRootVeg
         pmsa( ipnt( 1061) ) = bfShootVeg
         pmsa( ipnt( 1062) ) = aDRootVeg
         pmsa( ipnt( 1063) ) = aDShootVeg
         pmsa( ipnt( 1064) ) = aDEmergVeg
         pmsa( ipnt( 1065) ) = aDFloatVeg
         pmsa( ipnt( 1066) ) = bfSubVeg
         pmsa( ipnt( 1067) ) = aDSubVeg
         pmsa( ipnt( 1068) ) = aExtVeg
         pmsa( ipnt( 1069) ) = aDepth1Veg
         pmsa( ipnt( 1070) ) = aDepth2Veg
         pmsa( ipnt( 1071) ) = afCovSurfVeg
         pmsa( ipnt( 1072) ) = afCovEmergVeg
         pmsa( ipnt( 1073) ) = aCovVeg
         pmsa( ipnt( 1074) ) = aDVeg
         pmsa( ipnt( 1075) ) = aPVeg
         pmsa( ipnt( 1076) ) = aNVeg
         pmsa( ipnt( 1077) ) = aExtCoef
         pmsa( ipnt( 1078) ) = aLPARBot
         pmsa( ipnt( 1079) ) = rPDVeg
         pmsa( ipnt( 1080) ) = rNDVeg
         pmsa( ipnt( 1081) ) = tDMigrVeg
         pmsa( ipnt( 1082) ) = tPMigrVeg
         pmsa( ipnt( 1083) ) = tNMigrVeg
         pmsa( ipnt( 1084) ) = uFunTmProdVeg
         pmsa( ipnt( 1085) ) = uFunTmRespVeg
         pmsa( ipnt( 1086) ) = afPUptVegS
         pmsa( ipnt( 1087) ) = afNUptVegS
         pmsa( ipnt( 1088) ) = aVPUptMaxCrVeg
         pmsa( ipnt( 1089) ) = aVPUptVegW
         pmsa( ipnt( 1090) ) = aVPUptVegS
         pmsa( ipnt( 1091) ) = tPUptVegW
         pmsa( ipnt( 1092) ) = tPUptVegS
         pmsa( ipnt( 1093) ) = tPUptVeg
         pmsa( ipnt( 1094) ) = aVNUptMaxCrVeg
         pmsa( ipnt( 1095) ) = ahNUptVeg
         pmsa( ipnt( 1096) ) = aVNUptVegW
         pmsa( ipnt( 1097) ) = afNH4UptVegW
         pmsa( ipnt( 1098) ) = tNUptVegW
         pmsa( ipnt( 1099) ) = tNUptNH4VegW
         pmsa( ipnt( 1100) ) = tNUptNO3VegW
         pmsa( ipnt( 1101) ) = aVNUptVegS
         pmsa( ipnt( 1102) ) = tNUptVegS
         pmsa( ipnt( 1103) ) = afNH4UptVegS
         pmsa( ipnt( 1104) ) = tNUptNH4VegS
         pmsa( ipnt( 1105) ) = tNUptNO3VegS
         pmsa( ipnt( 1106) ) = tNUptVeg
         pmsa( ipnt( 1107) ) = aLPAR1Veg
         pmsa( ipnt( 1108) ) = aLPAR2Veg
         pmsa( ipnt( 1109) ) = uhLVeg
         pmsa( ipnt( 1110) ) = aLLimShootVeg
         pmsa( ipnt( 1111) ) = aMuTmLVeg
         pmsa( ipnt( 1112) ) = aPLimVeg
         pmsa( ipnt( 1113) ) = aNLimVeg
         pmsa( ipnt( 1114) ) = aNutLimVeg
         pmsa( ipnt( 1115) ) = aMuVeg
         pmsa( ipnt( 1116) ) = bkMortVeg
         pmsa( ipnt( 1117) ) = akDIncrVeg
         pmsa( ipnt( 1118) ) = tDEnvVeg
         pmsa( ipnt( 1119) ) = tDEnvProdVeg
         pmsa( ipnt( 1120) ) = tDProdVeg
         pmsa( ipnt( 1121) ) = tDProdSubVeg
         pmsa( ipnt( 1122) ) = tDRespVeg
         pmsa( ipnt( 1123) ) = tDEnvMortVeg
         pmsa( ipnt( 1124) ) = tDMortVeg
         pmsa( ipnt( 1125) ) = tDMortVegW
         pmsa( ipnt( 1126) ) = tDMortVegS
         pmsa( ipnt( 1127) ) = tDGrazVegBird
         pmsa( ipnt( 1128) ) = bkManVeg
         pmsa( ipnt( 1129) ) = tDManVeg
         pmsa( ipnt( 1130) ) = tPManVeg
         pmsa( ipnt( 1131) ) = tNManVeg
         pmsa( ipnt( 1132) ) = tDBedVeg
         pmsa( ipnt( 1133) ) = tO2ProdVeg
         pmsa( ipnt( 1134) ) = tO2RespVegW
         pmsa( ipnt( 1135) ) = tO2RespVegS
         pmsa( ipnt( 1136) ) = tO2ProdVegS
         pmsa( ipnt( 1137) ) = tO2ProdVegW
         pmsa( ipnt( 1138) ) = tO2UptNO3VegW
         pmsa( ipnt( 1139) ) = tO2UptNO3VegS
         pmsa( ipnt( 1140) ) = tPExcrVeg
         pmsa( ipnt( 1141) ) = tPExcrVegS
         pmsa( ipnt( 1142) ) = tPExcrVegW
         pmsa( ipnt( 1143) ) = tPMortVeg
         pmsa( ipnt( 1144) ) = tPMortVegPO4
         pmsa( ipnt( 1145) ) = tPMortVegPO4S
         pmsa( ipnt( 1146) ) = tPMortVegPO4W
         pmsa( ipnt( 1147) ) = tPMortVegDet
         pmsa( ipnt( 1148) ) = tPMortVegDetW
         pmsa( ipnt( 1149) ) = tPMortVegDetS
         pmsa( ipnt( 1150) ) = tPGrazVegBird
         pmsa( ipnt( 1151) ) = tPBedVeg
         pmsa( ipnt( 1152) ) = tNExcrVeg
         pmsa( ipnt( 1153) ) = tNExcrVegS
         pmsa( ipnt( 1154) ) = tNExcrVegW
         pmsa( ipnt( 1155) ) = tNMortVeg
         pmsa( ipnt( 1156) ) = tNMortVegNH4
         pmsa( ipnt( 1157) ) = tNMortVegNH4S
         pmsa( ipnt( 1158) ) = tNMortVegNH4W
         pmsa( ipnt( 1159) ) = tNMortVegDet
         pmsa( ipnt( 1160) ) = tNMortVegDetW
         pmsa( ipnt( 1161) ) = tNMortVegDetS
         pmsa( ipnt( 1162) ) = tNGrazVegBird
         pmsa( ipnt( 1163) ) = tNBedVeg
         pmsa( ipnt( 1164) ) = tDAssVegBird
         pmsa( ipnt( 1165) ) = tDEgesBird
         pmsa( ipnt( 1166) ) = tPAssVegBird
         pmsa( ipnt( 1167) ) = tPEgesBird
         pmsa( ipnt( 1168) ) = tPEgesBirdPO4
         pmsa( ipnt( 1169) ) = tPEgesBirdDet
         pmsa( ipnt( 1170) ) = tNAssVegBird
         pmsa( ipnt( 1171) ) = tNEgesBird
         pmsa( ipnt( 1172) ) = tNEgesBirdNH4
         pmsa( ipnt( 1173) ) = tNEgesBirdDet
         pmsa( ipnt( 1174) ) = wDBedDetW
         pmsa( ipnt( 1175) ) = tDBedDetS
         pmsa( ipnt( 1176) ) = tDBedTotT
         pmsa( ipnt( 1177) ) = wPBedPO4W
         pmsa( ipnt( 1178) ) = wPBedDetW
         pmsa( ipnt( 1179) ) = tPBedPO4S
         pmsa( ipnt( 1180) ) = tPBedDetS
         pmsa( ipnt( 1181) ) = tPBedTotT
         pmsa( ipnt( 1182) ) = wNBedNH4W
         pmsa( ipnt( 1183) ) = wNBedNO3W
         pmsa( ipnt( 1184) ) = wNBedDetW
         pmsa( ipnt( 1185) ) = tNBedNH4S
         pmsa( ipnt( 1186) ) = tNBedNO3S
         pmsa( ipnt( 1187) ) = tNBedDetS
         pmsa( ipnt( 1188) ) = tNBedTotT
         pmsa( ipnt( 1189) ) = tO2BedW
         pmsa( ipnt( 1190) ) = tO2BedS
         pmsa( ipnt( 1191) ) = UseLoss
         pmsa( ipnt( 1192) ) = uFunTmLoss
         pmsa( ipnt( 1193) ) = rPDBlueW
         pmsa( ipnt( 1194) ) = rNDBlueW
         pmsa( ipnt( 1195) ) = rPDBlueS
         pmsa( ipnt( 1196) ) = rNDBlueS
         pmsa( ipnt( 1197) ) = uFunTmBlue
         pmsa( ipnt( 1198) ) = uFunTmProdBlue
         pmsa( ipnt( 1199) ) = uFunTmRespBlue
         pmsa( ipnt( 1200) ) = aVPUptMaxCrBlue
         pmsa( ipnt( 1201) ) = aVPUptBlue
         pmsa( ipnt( 1202) ) = wPUptBlue
         pmsa( ipnt( 1203) ) = aVNUptMaxCrBlue
         pmsa( ipnt( 1204) ) = ahNUptBlue
         pmsa( ipnt( 1205) ) = aVNUptBlue
         pmsa( ipnt( 1206) ) = wNUptBlue
         pmsa( ipnt( 1207) ) = afNH4UptBlue
         pmsa( ipnt( 1208) ) = wNUptNH4Blue
         pmsa( ipnt( 1209) ) = wNUptNO3Blue
         pmsa( ipnt( 1210) ) = uMuMaxTmBlue
         pmsa( ipnt( 1211) ) = aPLimBlue
         pmsa( ipnt( 1212) ) = aNLimBlue
         pmsa( ipnt( 1213) ) = aSiLimBlue
         pmsa( ipnt( 1214) ) = aLLimBlue
         pmsa( ipnt( 1215) ) = aMuTmLBlue
         pmsa( ipnt( 1216) ) = aNutLimBlue
         pmsa( ipnt( 1217) ) = aMuBlue
         pmsa( ipnt( 1218) ) = wDAssBlue
         pmsa( ipnt( 1219) ) = rChDBlue
         pmsa( ipnt( 1220) ) = oChlaBlue
         pmsa( ipnt( 1221) ) = aExtChBlue
         pmsa( ipnt( 1222) ) = ukDRespTmBlue
         pmsa( ipnt( 1223) ) = wDRespBlueW
         pmsa( ipnt( 1224) ) = ukLossTmBlue
         pmsa( ipnt( 1225) ) = wDLossBlue
         pmsa( ipnt( 1226) ) = wDMortBlueW
         pmsa( ipnt( 1227) ) = uCorVSetBlue
         pmsa( ipnt( 1228) ) = tDSetBlue
         pmsa( ipnt( 1229) ) = tDResusBlue
         pmsa( ipnt( 1230) ) = tDRespBlueS
         pmsa( ipnt( 1231) ) = tDMortBlueS
         pmsa( ipnt( 1232) ) = ukDDecBlue
         pmsa( ipnt( 1233) ) = wPExcrBlueW
         pmsa( ipnt( 1234) ) = wPLossBlue
         pmsa( ipnt( 1235) ) = wPMortBlueW
         pmsa( ipnt( 1236) ) = tPSetBlue
         pmsa( ipnt( 1237) ) = tPResusBlue
         pmsa( ipnt( 1238) ) = tPExcrBlueS
         pmsa( ipnt( 1239) ) = tPMortBlueS
         pmsa( ipnt( 1240) ) = wNExcrBlueW
         pmsa( ipnt( 1241) ) = wNLossBlue
         pmsa( ipnt( 1242) ) = wNMortBlueW
         pmsa( ipnt( 1243) ) = tNSetBlue
         pmsa( ipnt( 1244) ) = tNResusBlue
         pmsa( ipnt( 1245) ) = tNExcrBlueS
         pmsa( ipnt( 1246) ) = tNMortBlueS
         pmsa( ipnt( 1247) ) = wDPrimBlueW
         pmsa( ipnt( 1248) ) = wPPrimBlueW
         pmsa( ipnt( 1249) ) = wNPrimBlueW
         pmsa( ipnt( 1250) ) = tDPrimBlueS
         pmsa( ipnt( 1251) ) = tPPrimBlueS
         pmsa( ipnt( 1252) ) = tNPrimBlueS
         pmsa( ipnt( 1253) ) = rPDGrenW
         pmsa( ipnt( 1254) ) = rNDGrenW
         pmsa( ipnt( 1255) ) = rPDGrenS
         pmsa( ipnt( 1256) ) = rNDGrenS
         pmsa( ipnt( 1257) ) = uFunTmGren
         pmsa( ipnt( 1258) ) = uFunTmProdGren
         pmsa( ipnt( 1259) ) = uFunTmRespGren
         pmsa( ipnt( 1260) ) = aVPUptMaxCrGren
         pmsa( ipnt( 1261) ) = aVPUptGren
         pmsa( ipnt( 1262) ) = wPUptGren
         pmsa( ipnt( 1263) ) = aVNUptMaxCrGren
         pmsa( ipnt( 1264) ) = ahNUptGren
         pmsa( ipnt( 1265) ) = aVNUptGren
         pmsa( ipnt( 1266) ) = wNUptGren
         pmsa( ipnt( 1267) ) = afNH4UptGren
         pmsa( ipnt( 1268) ) = wNUptNH4Gren
         pmsa( ipnt( 1269) ) = wNUptNO3Gren
         pmsa( ipnt( 1270) ) = uMuMaxTmGren
         pmsa( ipnt( 1271) ) = aPLimGren
         pmsa( ipnt( 1272) ) = aNLimGren
         pmsa( ipnt( 1273) ) = aSiLimGren
         pmsa( ipnt( 1274) ) = aLLimGren
         pmsa( ipnt( 1275) ) = aMuTmLGren
         pmsa( ipnt( 1276) ) = aNutLimGren
         pmsa( ipnt( 1277) ) = aMuGren
         pmsa( ipnt( 1278) ) = wDAssGren
         pmsa( ipnt( 1279) ) = rChDGren
         pmsa( ipnt( 1280) ) = oChlaGren
         pmsa( ipnt( 1281) ) = aExtChGren
         pmsa( ipnt( 1282) ) = ukDRespTmGren
         pmsa( ipnt( 1283) ) = wDRespGrenW
         pmsa( ipnt( 1284) ) = ukLossTmGren
         pmsa( ipnt( 1285) ) = wDLossGren
         pmsa( ipnt( 1286) ) = wDMortGrenW
         pmsa( ipnt( 1287) ) = uCorVSetGren
         pmsa( ipnt( 1288) ) = tDSetGren
         pmsa( ipnt( 1289) ) = tDResusGren
         pmsa( ipnt( 1290) ) = tDRespGrenS
         pmsa( ipnt( 1291) ) = tDMortGrenS
         pmsa( ipnt( 1292) ) = ukDDecGren
         pmsa( ipnt( 1293) ) = wPExcrGrenW
         pmsa( ipnt( 1294) ) = wPLossGren
         pmsa( ipnt( 1295) ) = wPMortGrenW
         pmsa( ipnt( 1296) ) = tPSetGren
         pmsa( ipnt( 1297) ) = tPResusGren
         pmsa( ipnt( 1298) ) = tPExcrGrenS
         pmsa( ipnt( 1299) ) = tPMortGrenS
         pmsa( ipnt( 1300) ) = wNExcrGrenW
         pmsa( ipnt( 1301) ) = wNLossGren
         pmsa( ipnt( 1302) ) = wNMortGrenW
         pmsa( ipnt( 1303) ) = tNSetGren
         pmsa( ipnt( 1304) ) = tNResusGren
         pmsa( ipnt( 1305) ) = tNExcrGrenS
         pmsa( ipnt( 1306) ) = tNMortGrenS
         pmsa( ipnt( 1307) ) = wDPrimGrenW
         pmsa( ipnt( 1308) ) = wPPrimGrenW
         pmsa( ipnt( 1309) ) = wNPrimGrenW
         pmsa( ipnt( 1310) ) = tDPrimGrenS
         pmsa( ipnt( 1311) ) = tPPrimGrenS
         pmsa( ipnt( 1312) ) = tNPrimGrenS
         pmsa( ipnt( 1313) ) = rPDDiatW
         pmsa( ipnt( 1314) ) = rNDDiatW
         pmsa( ipnt( 1315) ) = rPDDiatS
         pmsa( ipnt( 1316) ) = rNDDiatS
         pmsa( ipnt( 1317) ) = uFunTmDiat
         pmsa( ipnt( 1318) ) = uFunTmProdDiat
         pmsa( ipnt( 1319) ) = uFunTmRespDiat
         pmsa( ipnt( 1320) ) = aVPUptMaxCrDiat
         pmsa( ipnt( 1321) ) = aVPUptDiat
         pmsa( ipnt( 1322) ) = wPUptDiat
         pmsa( ipnt( 1323) ) = aVNUptMaxCrDiat
         pmsa( ipnt( 1324) ) = ahNUptDiat
         pmsa( ipnt( 1325) ) = aVNUptDiat
         pmsa( ipnt( 1326) ) = wNUptDiat
         pmsa( ipnt( 1327) ) = afNH4UptDiat
         pmsa( ipnt( 1328) ) = wNUptNH4Diat
         pmsa( ipnt( 1329) ) = wNUptNO3Diat
         pmsa( ipnt( 1330) ) = uMuMaxTmDiat
         pmsa( ipnt( 1331) ) = aPLimDiat
         pmsa( ipnt( 1332) ) = aNLimDiat
         pmsa( ipnt( 1333) ) = aSiLimDiat
         pmsa( ipnt( 1334) ) = aLLimDiat
         pmsa( ipnt( 1335) ) = aMuTmLDiat
         pmsa( ipnt( 1336) ) = aNutLimDiat
         pmsa( ipnt( 1337) ) = aMuDiat
         pmsa( ipnt( 1338) ) = wDAssDiat
         pmsa( ipnt( 1339) ) = rChDDiat
         pmsa( ipnt( 1340) ) = oChlaDiat
         pmsa( ipnt( 1341) ) = aExtChDiat
         pmsa( ipnt( 1342) ) = ukDRespTmDiat
         pmsa( ipnt( 1343) ) = wDRespDiatW
         pmsa( ipnt( 1344) ) = ukLossTmDiat
         pmsa( ipnt( 1345) ) = wDLossDiat
         pmsa( ipnt( 1346) ) = wDMortDiatW
         pmsa( ipnt( 1347) ) = uCorVSetDiat
         pmsa( ipnt( 1348) ) = tDSetDiat
         pmsa( ipnt( 1349) ) = tDResusDiat
         pmsa( ipnt( 1350) ) = tDRespDiatS
         pmsa( ipnt( 1351) ) = tDMortDiatS
         pmsa( ipnt( 1352) ) = ukDDecDiat
         pmsa( ipnt( 1353) ) = wPExcrDiatW
         pmsa( ipnt( 1354) ) = wPLossDiat
         pmsa( ipnt( 1355) ) = wPMortDiatW
         pmsa( ipnt( 1356) ) = tPSetDiat
         pmsa( ipnt( 1357) ) = tPResusDiat
         pmsa( ipnt( 1358) ) = tPExcrDiatS
         pmsa( ipnt( 1359) ) = tPMortDiatS
         pmsa( ipnt( 1360) ) = wNExcrDiatW
         pmsa( ipnt( 1361) ) = wNLossDiat
         pmsa( ipnt( 1362) ) = wNMortDiatW
         pmsa( ipnt( 1363) ) = tNSetDiat
         pmsa( ipnt( 1364) ) = tNResusDiat
         pmsa( ipnt( 1365) ) = tNExcrDiatS
         pmsa( ipnt( 1366) ) = tNMortDiatS
         pmsa( ipnt( 1367) ) = wDPrimDiatW
         pmsa( ipnt( 1368) ) = wPPrimDiatW
         pmsa( ipnt( 1369) ) = wNPrimDiatW
         pmsa( ipnt( 1370) ) = tDPrimDiatS
         pmsa( ipnt( 1371) ) = tPPrimDiatS
         pmsa( ipnt( 1372) ) = tNPrimDiatS
         pmsa( ipnt( 1373) ) = oChla
         pmsa( ipnt( 1374) ) = wDAssPhyt
         pmsa( ipnt( 1375) ) = wDRespPhytW
         pmsa( ipnt( 1376) ) = wDMortPhytW
         pmsa( ipnt( 1377) ) = tDSetPhyt
         pmsa( ipnt( 1378) ) = wDLossPhyt
         pmsa( ipnt( 1379) ) = wDPrimPhytW
         pmsa( ipnt( 1380) ) = wPUptPhyt
         pmsa( ipnt( 1381) ) = wPExcrPhytW
         pmsa( ipnt( 1382) ) = wPMortPhytW
         pmsa( ipnt( 1383) ) = tPSetPhyt
         pmsa( ipnt( 1384) ) = tPResusPhyt
         pmsa( ipnt( 1385) ) = wPLossPhyt
         pmsa( ipnt( 1386) ) = wPPrimPhytW
         pmsa( ipnt( 1387) ) = wNUptPhyt
         pmsa( ipnt( 1388) ) = wNUptNH4Phyt
         pmsa( ipnt( 1389) ) = wNUptNO3Phyt
         pmsa( ipnt( 1390) ) = wNExcrPhytW
         pmsa( ipnt( 1391) ) = wNMortPhytW
         pmsa( ipnt( 1392) ) = tNSetPhyt
         pmsa( ipnt( 1393) ) = tNResusPhyt
         pmsa( ipnt( 1394) ) = wNLossPhyt
         pmsa( ipnt( 1395) ) = wNPrimPhytW
         pmsa( ipnt( 1396) ) = tDRespPhytS
         pmsa( ipnt( 1397) ) = tDMortPhytS
         pmsa( ipnt( 1398) ) = tDPrimPhytS
         pmsa( ipnt( 1399) ) = tPExcrPhytS
         pmsa( ipnt( 1400) ) = tPMortPhytS
         pmsa( ipnt( 1401) ) = tPPrimPhytS
         pmsa( ipnt( 1402) ) = tNExcrPhytS
         pmsa( ipnt( 1403) ) = tNMortPhytS
         pmsa( ipnt( 1404) ) = tNPrimPhytS
         pmsa( ipnt( 1405) ) = wSiUptDiat
         pmsa( ipnt( 1406) ) = wSiExcrDiatW
         pmsa( ipnt( 1407) ) = wSiLossDiat
         pmsa( ipnt( 1408) ) = wSiMortDiatW
         pmsa( ipnt( 1409) ) = tSiSetDiat
         pmsa( ipnt( 1410) ) = tSiResusDiat
         pmsa( ipnt( 1411) ) = wSiPrimDiatW
         pmsa( ipnt( 1412) ) = rCyDBlue
         pmsa( ipnt( 1413) ) = oCyan
         pmsa( ipnt( 1414) ) = fDDiat
         pmsa( ipnt( 1415) ) = wDPrimDetW
         pmsa( ipnt( 1416) ) = tDPrimDetS
         pmsa( ipnt( 1417) ) = tDPrimTotT
         pmsa( ipnt( 1418) ) = wO2ProdPhyt
         pmsa( ipnt( 1419) ) = wO2RespPhytW
         pmsa( ipnt( 1420) ) = wO2UptNO3Phyt
         pmsa( ipnt( 1421) ) = wO2PrimW
         pmsa( ipnt( 1422) ) = tO2RespPhytS
         pmsa( ipnt( 1423) ) = tO2PrimS
         pmsa( ipnt( 1424) ) = wPMortPhytPO4W
         pmsa( ipnt( 1425) ) = wPMortPhytDetW
         pmsa( ipnt( 1426) ) = wPLossPhytPO4
         pmsa( ipnt( 1427) ) = wPLossPhytDet
         pmsa( ipnt( 1428) ) = wPPrimPO4W
         pmsa( ipnt( 1429) ) = wPPrimDetW
         pmsa( ipnt( 1430) ) = tPMortPhytPO4S
         pmsa( ipnt( 1431) ) = tPMortPhytDetS
         pmsa( ipnt( 1432) ) = tPPrimDetS
         pmsa( ipnt( 1433) ) = tPPrimPO4S
         pmsa( ipnt( 1434) ) = tPPrimTotT
         pmsa( ipnt( 1435) ) = wNMortPhytNH4W
         pmsa( ipnt( 1436) ) = wNMortPhytDetW
         pmsa( ipnt( 1437) ) = wNLossPhytNH4
         pmsa( ipnt( 1438) ) = wNLossPhytDet
         pmsa( ipnt( 1439) ) = wNPrimNH4W
         pmsa( ipnt( 1440) ) = wNPrimNO3W
         pmsa( ipnt( 1441) ) = wNPrimDetW
         pmsa( ipnt( 1442) ) = tNMortPhytNH4S
         pmsa( ipnt( 1443) ) = tNMortPhytDetS
         pmsa( ipnt( 1444) ) = tNPrimNH4S
         pmsa( ipnt( 1445) ) = tNPrimNO3S
         pmsa( ipnt( 1446) ) = tNPrimDetS
         pmsa( ipnt( 1447) ) = tNPrimTotT
         pmsa( ipnt( 1448) ) = tSiExcrDiatS
         pmsa( ipnt( 1449) ) = tSiMortDiatS
         pmsa( ipnt( 1450) ) = wSiPrimSiO2W
         pmsa( ipnt( 1451) ) = wSiPrimDetW
         pmsa( ipnt( 1452) ) = tSiPrimDiatS
         pmsa( ipnt( 1453) ) = tSiPrimDetS
         pmsa( ipnt( 1454) ) = tSiPrimTotT
         pmsa( ipnt( 1455) ) = aPACoef
         pmsa( ipnt( 1456) ) = bSecchiMax
         pmsa( ipnt( 1457) ) = aSecchi
         pmsa( ipnt( 1458) ) = aTransparency
         pmsa( ipnt( 1459) ) = aDepthEuph
         pmsa( ipnt( 1460) ) = aRelDepthEuph
         pmsa( ipnt( 1461) ) = aChlaH
         pmsa( ipnt( 1462) ) = aCovPhytW
         pmsa( ipnt( 1463) ) = rExtChPhyt
         pmsa( ipnt( 1464) ) = uFunTmZoo
         pmsa( ipnt( 1465) ) = rPDZoo
         pmsa( ipnt( 1466) ) = rNDZoo
         pmsa( ipnt( 1467) ) = oDFoodZoo
         pmsa( ipnt( 1468) ) = aFilt
         pmsa( ipnt( 1469) ) = ukDAssTmZoo
         pmsa( ipnt( 1470) ) = aDSatZoo
         pmsa( ipnt( 1471) ) = ukDRespTmZoo
         pmsa( ipnt( 1472) ) = ukDIncrZoo
         pmsa( ipnt( 1473) ) = wDEnvZoo
         pmsa( ipnt( 1474) ) = wDAssZoo
         pmsa( ipnt( 1475) ) = wDConsZoo
         pmsa( ipnt( 1476) ) = wDConsDetZoo
         pmsa( ipnt( 1477) ) = wDConsDiatZoo
         pmsa( ipnt( 1478) ) = wDConsGrenZoo
         pmsa( ipnt( 1479) ) = wDConsBlueZoo
         pmsa( ipnt( 1480) ) = wDConsPhytZoo
         pmsa( ipnt( 1481) ) = wDEgesZoo
         pmsa( ipnt( 1482) ) = aCorDRespZoo
         pmsa( ipnt( 1483) ) = wDRespZoo
         pmsa( ipnt( 1484) ) = wDMortZoo
         pmsa( ipnt( 1485) ) = oPFoodZoo
         pmsa( ipnt( 1486) ) = rPDFoodZoo
         pmsa( ipnt( 1487) ) = wPConsDiatZoo
         pmsa( ipnt( 1488) ) = wPConsGrenZoo
         pmsa( ipnt( 1489) ) = wPConsBlueZoo
         pmsa( ipnt( 1490) ) = wPConsPhytZoo
         pmsa( ipnt( 1491) ) = wPConsDetZoo
         pmsa( ipnt( 1492) ) = wPConsZoo
         pmsa( ipnt( 1493) ) = afPAssZoo
         pmsa( ipnt( 1494) ) = wPAssZoo
         pmsa( ipnt( 1495) ) = wPEgesZoo
         pmsa( ipnt( 1496) ) = wPEgesZooPO4
         pmsa( ipnt( 1497) ) = wPEgesZooDet
         pmsa( ipnt( 1498) ) = akPExcrZoo
         pmsa( ipnt( 1499) ) = wPExcrZoo
         pmsa( ipnt( 1500) ) = wPMortZoo
         pmsa( ipnt( 1501) ) = wPMortZooPO4
         pmsa( ipnt( 1502) ) = wPMortZooDet
         pmsa( ipnt( 1503) ) = oNFoodZoo
         pmsa( ipnt( 1504) ) = rNDFoodZoo
         pmsa( ipnt( 1505) ) = wNConsDiatZoo
         pmsa( ipnt( 1506) ) = wNConsGrenZoo
         pmsa( ipnt( 1507) ) = wNConsBlueZoo
         pmsa( ipnt( 1508) ) = wNConsPhytZoo
         pmsa( ipnt( 1509) ) = wNConsDetZoo
         pmsa( ipnt( 1510) ) = wNConsZoo
         pmsa( ipnt( 1511) ) = afNAssZoo
         pmsa( ipnt( 1512) ) = wNAssZoo
         pmsa( ipnt( 1513) ) = wNEgesZoo
         pmsa( ipnt( 1514) ) = wNEgesZooNH4
         pmsa( ipnt( 1515) ) = wNEgesZooDet
         pmsa( ipnt( 1516) ) = kNExcrZoo
         pmsa( ipnt( 1517) ) = wNExcrZoo
         pmsa( ipnt( 1518) ) = wNMortZoo
         pmsa( ipnt( 1519) ) = wNMortZooNH4
         pmsa( ipnt( 1520) ) = wNMortZooDet
         pmsa( ipnt( 1521) ) = wSiConsDiatZoo
         pmsa( ipnt( 1522) ) = uFunTmBent
         pmsa( ipnt( 1523) ) = aDFoodBent
         pmsa( ipnt( 1524) ) = rPDBent
         pmsa( ipnt( 1525) ) = rNDBent
         pmsa( ipnt( 1526) ) = tDMigrBent
         pmsa( ipnt( 1527) ) = aDSatBent
         pmsa( ipnt( 1528) ) = ukDIncrBent
         pmsa( ipnt( 1529) ) = tDEnvBent
         pmsa( ipnt( 1530) ) = tDAssBent
         pmsa( ipnt( 1531) ) = aDAssBentSp
         pmsa( ipnt( 1532) ) = tDConsBent
         pmsa( ipnt( 1533) ) = tDConsDetBent
         pmsa( ipnt( 1534) ) = tDConsDiatBent
         pmsa( ipnt( 1535) ) = tDConsGrenBent
         pmsa( ipnt( 1536) ) = tDConsBlueBent
         pmsa( ipnt( 1537) ) = tDConsPhytBent
         pmsa( ipnt( 1538) ) = tDEgesBent
         pmsa( ipnt( 1539) ) = tDRespBent
         pmsa( ipnt( 1540) ) = tDMortBent
         pmsa( ipnt( 1541) ) = aPFoodBent
         pmsa( ipnt( 1542) ) = rPDFoodBent
         pmsa( ipnt( 1543) ) = tPConsDetBent
         pmsa( ipnt( 1544) ) = tPConsDiatBent
         pmsa( ipnt( 1545) ) = tPConsGrenBent
         pmsa( ipnt( 1546) ) = tPConsBlueBent
         pmsa( ipnt( 1547) ) = tPConsPhytBent
         pmsa( ipnt( 1548) ) = tPConsBent
         pmsa( ipnt( 1549) ) = afPAssBent
         pmsa( ipnt( 1550) ) = tPAssBent
         pmsa( ipnt( 1551) ) = tPEgesBent
         pmsa( ipnt( 1552) ) = tPEgesBentPO4
         pmsa( ipnt( 1553) ) = tPEgesBentDet
         pmsa( ipnt( 1554) ) = tPExcrBent
         pmsa( ipnt( 1555) ) = tPMortBent
         pmsa( ipnt( 1556) ) = tPMortBentPO4
         pmsa( ipnt( 1557) ) = tPMortBentDet
         pmsa( ipnt( 1558) ) = tPMigrBent
         pmsa( ipnt( 1559) ) = aNFoodBent
         pmsa( ipnt( 1560) ) = rNDFoodBent
         pmsa( ipnt( 1561) ) = tNMigrBent
         pmsa( ipnt( 1562) ) = tNConsDetBent
         pmsa( ipnt( 1563) ) = tNConsDiatBent
         pmsa( ipnt( 1564) ) = tNConsGrenBent
         pmsa( ipnt( 1565) ) = tNConsBlueBent
         pmsa( ipnt( 1566) ) = tNConsPhytBent
         pmsa( ipnt( 1567) ) = tNConsBent
         pmsa( ipnt( 1568) ) = afNAssBent
         pmsa( ipnt( 1569) ) = tNAssBent
         pmsa( ipnt( 1570) ) = tNEgesBent
         pmsa( ipnt( 1571) ) = tNEgesBentNH4
         pmsa( ipnt( 1572) ) = tNEgesBentDet
         pmsa( ipnt( 1573) ) = tNExcrBent
         pmsa( ipnt( 1574) ) = tNMortBent
         pmsa( ipnt( 1575) ) = tNMortBentNH4
         pmsa( ipnt( 1576) ) = tNMortBentDet
         pmsa( ipnt( 1577) ) = tSiConsDiatBent
         pmsa( ipnt( 1578) ) = aDFish
         pmsa( ipnt( 1579) ) = aPFish
         pmsa( ipnt( 1580) ) = aNFish
         pmsa( ipnt( 1581) ) = rPDFiJv
         pmsa( ipnt( 1582) ) = rPDFiAd
         pmsa( ipnt( 1583) ) = rNDFiJv
         pmsa( ipnt( 1584) ) = rNDFiAd
         pmsa( ipnt( 1585) ) = tDReprFish
         pmsa( ipnt( 1586) ) = tDAgeFish
         pmsa( ipnt( 1587) ) = aFunVegFish
         pmsa( ipnt( 1588) ) = aDSatFiJv
         pmsa( ipnt( 1589) ) = ukDIncrFiJv
         pmsa( ipnt( 1590) ) = tDEnvFiJv
         pmsa( ipnt( 1591) ) = tDAssFiJv
         pmsa( ipnt( 1592) ) = tDConsFiJv
         pmsa( ipnt( 1593) ) = tDEgesFiJv
         pmsa( ipnt( 1594) ) = tDRespFiJv
         pmsa( ipnt( 1595) ) = tDMortFiJv
         pmsa( ipnt( 1596) ) = tDMigrFiJv
         pmsa( ipnt( 1597) ) = aDSatFiAd
         pmsa( ipnt( 1598) ) = ukDIncrFiAd
         pmsa( ipnt( 1599) ) = tDEnvFiAd
         pmsa( ipnt( 1600) ) = tDAssFiAd
         pmsa( ipnt( 1601) ) = tDConsFiAd
         pmsa( ipnt( 1602) ) = tDEgesFiAd
         pmsa( ipnt( 1603) ) = tDRespFiAd
         pmsa( ipnt( 1604) ) = tDMortFiAd
         pmsa( ipnt( 1605) ) = ukHarvFish
         pmsa( ipnt( 1606) ) = tDHarvFish
         pmsa( ipnt( 1607) ) = tDMigrFiAd
         pmsa( ipnt( 1608) ) = tDMortFish
         pmsa( ipnt( 1609) ) = tDMortFishBot
         pmsa( ipnt( 1610) ) = tDMortFishDet
         pmsa( ipnt( 1611) ) = tPReprFish
         pmsa( ipnt( 1612) ) = tPAgeFish
         pmsa( ipnt( 1613) ) = tPMigrFiJv
         pmsa( ipnt( 1614) ) = tPConsFiJv
         pmsa( ipnt( 1615) ) = afPAssFiJv
         pmsa( ipnt( 1616) ) = tPAssFiJv
         pmsa( ipnt( 1617) ) = tPEgesFiJv
         pmsa( ipnt( 1618) ) = tPExcrFiJv
         pmsa( ipnt( 1619) ) = tPMortFiJv
         pmsa( ipnt( 1620) ) = tPMigrFiAd
         pmsa( ipnt( 1621) ) = tPConsFiAd
         pmsa( ipnt( 1622) ) = afPAssFiAd
         pmsa( ipnt( 1623) ) = tPAssFiAd
         pmsa( ipnt( 1624) ) = tPEgesFiAd
         pmsa( ipnt( 1625) ) = tPExcrFiAd
         pmsa( ipnt( 1626) ) = tPMortFiAd
         pmsa( ipnt( 1627) ) = tPHarvFish
         pmsa( ipnt( 1628) ) = tPMortFish
         pmsa( ipnt( 1629) ) = tPMortFishBot
         pmsa( ipnt( 1630) ) = tPMortFishPO4
         pmsa( ipnt( 1631) ) = tPMortFishDet
         pmsa( ipnt( 1632) ) = tPEgesFish
         pmsa( ipnt( 1633) ) = tPEgesFishPO4
         pmsa( ipnt( 1634) ) = tPEgesFishDet
         pmsa( ipnt( 1635) ) = tNReprFish
         pmsa( ipnt( 1636) ) = tNAgeFish
         pmsa( ipnt( 1637) ) = tNMigrFiJv
         pmsa( ipnt( 1638) ) = tNConsFiJv
         pmsa( ipnt( 1639) ) = afNAssFiJv
         pmsa( ipnt( 1640) ) = tNAssFiJv
         pmsa( ipnt( 1641) ) = tNEgesFiJv
         pmsa( ipnt( 1642) ) = tNExcrFiJv
         pmsa( ipnt( 1643) ) = tNMortFiJv
         pmsa( ipnt( 1644) ) = tNMigrFiAd
         pmsa( ipnt( 1645) ) = tNConsFiAd
         pmsa( ipnt( 1646) ) = afNAssFiAd
         pmsa( ipnt( 1647) ) = tNAssFiAd
         pmsa( ipnt( 1648) ) = tNEgesFiAd
         pmsa( ipnt( 1649) ) = tNExcrFiAd
         pmsa( ipnt( 1650) ) = tNMortFiAd
         pmsa( ipnt( 1651) ) = tNHarvFish
         pmsa( ipnt( 1652) ) = tNMortFish
         pmsa( ipnt( 1653) ) = tNMortFishBot
         pmsa( ipnt( 1654) ) = tNMortFishNH4
         pmsa( ipnt( 1655) ) = tNMortFishDet
         pmsa( ipnt( 1656) ) = tNEgesFish
         pmsa( ipnt( 1657) ) = tNEgesFishNH4
         pmsa( ipnt( 1658) ) = tNEgesFishDet
         pmsa( ipnt( 1659) ) = uFunTmPisc
         pmsa( ipnt( 1660) ) = tDMigrPisc
         pmsa( ipnt( 1661) ) = aDCarrPisc
         pmsa( ipnt( 1662) ) = aFunVegPisc
         pmsa( ipnt( 1663) ) = aDSatPisc
         pmsa( ipnt( 1664) ) = akDIncrPisc
         pmsa( ipnt( 1665) ) = tDEnvPisc
         pmsa( ipnt( 1666) ) = tDAssPisc
         pmsa( ipnt( 1667) ) = tDConsPisc
         pmsa( ipnt( 1668) ) = tDEgesPisc
         pmsa( ipnt( 1669) ) = tDConsFiJvPisc
         pmsa( ipnt( 1670) ) = tDConsFiAdPisc
         pmsa( ipnt( 1671) ) = tDRespPisc
         pmsa( ipnt( 1672) ) = tDMortPisc
         pmsa( ipnt( 1673) ) = tDMortPiscBot
         pmsa( ipnt( 1674) ) = tDMortPiscDet
         pmsa( ipnt( 1675) ) = ukHarvPisc
         pmsa( ipnt( 1676) ) = tDHarvPisc
         pmsa( ipnt( 1677) ) = aPPisc
         pmsa( ipnt( 1678) ) = tPConsFiJvPisc
         pmsa( ipnt( 1679) ) = tPConsFiAdPisc
         pmsa( ipnt( 1680) ) = tPConsPisc
         pmsa( ipnt( 1681) ) = rPDFoodPisc
         pmsa( ipnt( 1682) ) = afPAssPisc
         pmsa( ipnt( 1683) ) = tPAssPisc
         pmsa( ipnt( 1684) ) = tPEgesPisc
         pmsa( ipnt( 1685) ) = tPEgesPiscPO4
         pmsa( ipnt( 1686) ) = tPEgesPiscDet
         pmsa( ipnt( 1687) ) = tPExcrPisc
         pmsa( ipnt( 1688) ) = tPMortPisc
         pmsa( ipnt( 1689) ) = tPMortPiscBot
         pmsa( ipnt( 1690) ) = tPMortPiscPO4
         pmsa( ipnt( 1691) ) = tPMortPiscDet
         pmsa( ipnt( 1692) ) = tPMigrPisc
         pmsa( ipnt( 1693) ) = tPHarvPisc
         pmsa( ipnt( 1694) ) = aNPisc
         pmsa( ipnt( 1695) ) = tNConsFiJvPisc
         pmsa( ipnt( 1696) ) = tNConsFiAdPisc
         pmsa( ipnt( 1697) ) = tNConsPisc
         pmsa( ipnt( 1698) ) = rNDFoodPisc
         pmsa( ipnt( 1699) ) = afNAssPisc
         pmsa( ipnt( 1700) ) = tNAssPisc
         pmsa( ipnt( 1701) ) = tNEgesPisc
         pmsa( ipnt( 1702) ) = tNEgesPiscNH4
         pmsa( ipnt( 1703) ) = tNEgesPiscDet
         pmsa( ipnt( 1704) ) = tNExcrPisc
         pmsa( ipnt( 1705) ) = tNMortPisc
         pmsa( ipnt( 1706) ) = tNMortPiscBot
         pmsa( ipnt( 1707) ) = tNMortPiscNH4
         pmsa( ipnt( 1708) ) = tNMortPiscDet
         pmsa( ipnt( 1709) ) = tNMigrPisc
         pmsa( ipnt( 1710) ) = tNHarvPisc
         pmsa( ipnt( 1711) ) = wDWebZoo
         pmsa( ipnt( 1712) ) = wPWebZoo
         pmsa( ipnt( 1713) ) = wNWebZoo
         pmsa( ipnt( 1714) ) = tDWebBent
         pmsa( ipnt( 1715) ) = tPWebBent
         pmsa( ipnt( 1716) ) = tNWebBent
         pmsa( ipnt( 1717) ) = tDWebFiJv
         pmsa( ipnt( 1718) ) = tPWebFiJv
         pmsa( ipnt( 1719) ) = tNWebFiJv
         pmsa( ipnt( 1720) ) = tDWebFiAd
         pmsa( ipnt( 1721) ) = tPWebFiAd
         pmsa( ipnt( 1722) ) = tNWebFiAd
         pmsa( ipnt( 1723) ) = tDWebPisc
         pmsa( ipnt( 1724) ) = wDWebDetW
         pmsa( ipnt( 1725) ) = wDWebDiatW
         pmsa( ipnt( 1726) ) = wDWebGrenW
         pmsa( ipnt( 1727) ) = wDWebBlueW
         pmsa( ipnt( 1728) ) = tDWebDetS
         pmsa( ipnt( 1729) ) = tDWebDiatS
         pmsa( ipnt( 1730) ) = tDWebGrenS
         pmsa( ipnt( 1731) ) = tDWebBlueS
         pmsa( ipnt( 1732) ) = tDWebPhytS
         pmsa( ipnt( 1733) ) = tDWebTotT
         pmsa( ipnt( 1734) ) = wPWebPO4W
         pmsa( ipnt( 1735) ) = wPWebDetW
         pmsa( ipnt( 1736) ) = wPWebDiatW
         pmsa( ipnt( 1737) ) = wPWebGrenW
         pmsa( ipnt( 1738) ) = wPWebBlueW
         pmsa( ipnt( 1739) ) = tPWebPO4S
         pmsa( ipnt( 1740) ) = tPWebDetS
         pmsa( ipnt( 1741) ) = tPWebDiatS
         pmsa( ipnt( 1742) ) = tPWebGrenS
         pmsa( ipnt( 1743) ) = tPWebBlueS
         pmsa( ipnt( 1744) ) = tPWebPhytS
         pmsa( ipnt( 1745) ) = tPWebTotT
         pmsa( ipnt( 1746) ) = wNWebNH4W
         pmsa( ipnt( 1747) ) = wNWebNO3W
         pmsa( ipnt( 1748) ) = wNWebDetW
         pmsa( ipnt( 1749) ) = wNWebDiatW
         pmsa( ipnt( 1750) ) = wNWebGrenW
         pmsa( ipnt( 1751) ) = wNWebBlueW
         pmsa( ipnt( 1752) ) = tNWebNH4S
         pmsa( ipnt( 1753) ) = tNWebNO3S
         pmsa( ipnt( 1754) ) = tNWebDetS
         pmsa( ipnt( 1755) ) = tNWebDiatS
         pmsa( ipnt( 1756) ) = tNWebGrenS
         pmsa( ipnt( 1757) ) = tNWebBlueS
         pmsa( ipnt( 1758) ) = tNWebPhytS
         pmsa( ipnt( 1759) ) = tNWebTotT
         pmsa( ipnt( 1760) ) = wSiWebSiO2W
         pmsa( ipnt( 1761) ) = wSiWebDetW
         pmsa( ipnt( 1762) ) = tSiWebDetS
         pmsa( ipnt( 1763) ) = tSiWebTotT
         pmsa( ipnt( 1764) ) = aPrefAve
         pmsa( ipnt( 1765) ) = wDConsZoo2
         pmsa( ipnt( 1766) ) = aDConsZooSp
         pmsa( ipnt( 1767) ) = aDAssZooSp
         pmsa( ipnt( 1768) ) = aDGrazSp
         pmsa( ipnt( 1769) ) = aPConsZooSp
         pmsa( ipnt( 1770) ) = aPGrazSp
         pmsa( ipnt( 1771) ) = aNConsZooSp
         pmsa( ipnt( 1772) ) = aNGrazSp
         pmsa( ipnt( 1773) ) = afDShootPhra
         pmsa( ipnt( 1774) ) = rDSRPhra
         pmsa( ipnt( 1775) ) = rPDShootPhra
         pmsa( ipnt( 1776) ) = rNDShootPhra
         pmsa( ipnt( 1777) ) = rPDRootPhra
         pmsa( ipnt( 1778) ) = rNDRootPhra
         pmsa( ipnt( 1779) ) = aLengShootPhra
         pmsa( ipnt( 1780) ) = bDayInitPhra
         pmsa( ipnt( 1781) ) = aDAllPhra
         pmsa( ipnt( 1782) ) = tDAllPhra
         pmsa( ipnt( 1783) ) = tNTransPhra
         pmsa( ipnt( 1784) ) = tPTransPhra
         pmsa( ipnt( 1785) ) = aVNUptPhraMaxCr
         pmsa( ipnt( 1786) ) = ahNUptPhraS
         pmsa( ipnt( 1787) ) = aVNUptPhraS
         pmsa( ipnt( 1788) ) = tNUptPhraS
         pmsa( ipnt( 1789) ) = tNUptNH4PhraS
         pmsa( ipnt( 1790) ) = tNUptNO3PhraS
         pmsa( ipnt( 1791) ) = tNUptShootPhra
         pmsa( ipnt( 1792) ) = tNUptRootPhra
         pmsa( ipnt( 1793) ) = aVPUptPhraMaxCr
         pmsa( ipnt( 1794) ) = ahPUptPhraS
         pmsa( ipnt( 1795) ) = aVPUptPhraS
         pmsa( ipnt( 1796) ) = tPUptPhraS
         pmsa( ipnt( 1797) ) = tPUptShootPhra
         pmsa( ipnt( 1798) ) = tPUptRootPhra
         pmsa( ipnt( 1799) ) = uFunTmProdPhra
         pmsa( ipnt( 1800) ) = ukDRespTmPhra
         pmsa( ipnt( 1801) ) = aMuPhotPhra
         pmsa( ipnt( 1802) ) = aNLimProdPhra
         pmsa( ipnt( 1803) ) = aPLimProdPhra
         pmsa( ipnt( 1804) ) = aNutLimPhra
         pmsa( ipnt( 1805) ) = aMuPhra
         pmsa( ipnt( 1806) ) = akDIncrPhra
         pmsa( ipnt( 1807) ) = tDDensPhra
         pmsa( ipnt( 1808) ) = tDDensProdPhra
         pmsa( ipnt( 1809) ) = tDProdPhra
         pmsa( ipnt( 1810) ) = tDProdShootPhra
         pmsa( ipnt( 1811) ) = tDProdRootPhra
         pmsa( ipnt( 1812) ) = tDRespShootPhra
         pmsa( ipnt( 1813) ) = tDRespRootPhra
         pmsa( ipnt( 1814) ) = tO2RespRootPhra
         pmsa( ipnt( 1815) ) = tO2FlowPhra
         pmsa( ipnt( 1816) ) = bDayRealPhra
         pmsa( ipnt( 1817) ) = aDRealPhra
         pmsa( ipnt( 1818) ) = tDRealPhra
         pmsa( ipnt( 1819) ) = tNRetrPhra
         pmsa( ipnt( 1820) ) = tPRetrPhra
         pmsa( ipnt( 1821) ) = tDMortShootPhra
         pmsa( ipnt( 1822) ) = tNMortShootPhra
         pmsa( ipnt( 1823) ) = tPMortShootPhra
         pmsa( ipnt( 1824) ) = tDMortRootPhra
         pmsa( ipnt( 1825) ) = tNMortRootPhra
         pmsa( ipnt( 1826) ) = tPMortRootPhra
         pmsa( ipnt( 1827) ) = tDManShootPhra
         pmsa( ipnt( 1828) ) = tNManShootPhra
         pmsa( ipnt( 1829) ) = tPManShootPhra
         pmsa( ipnt( 1830) ) = tDIMSM
         pmsa( ipnt( 1831) ) = tDHumSM
         pmsa( ipnt( 1832) ) = tDDetSM
         pmsa( ipnt( 1833) ) = vDeltaSM
         pmsa( ipnt( 1834) ) = tDBurIMM
         pmsa( ipnt( 1835) ) = tDBurOMM
         pmsa( ipnt( 1836) ) = tDBurDetM
         pmsa( ipnt( 1837) ) = tDBurHumM
         pmsa( ipnt( 1838) ) = tDBurTotM
         pmsa( ipnt( 1839) ) = tPBurHumM
         pmsa( ipnt( 1840) ) = tPBurDetM
         pmsa( ipnt( 1841) ) = tPBurAIMM
         pmsa( ipnt( 1842) ) = tPBurPO4M
         pmsa( ipnt( 1843) ) = tPBurTotM
         pmsa( ipnt( 1844) ) = tNBurHumM
         pmsa( ipnt( 1845) ) = tNBurDetM
         pmsa( ipnt( 1846) ) = tNBurNH4M
         pmsa( ipnt( 1847) ) = tNBurNO3M
         pmsa( ipnt( 1848) ) = tNBurTotM
         pmsa( ipnt( 1849) ) = tSiBurDetM
         pmsa( ipnt( 1850) ) = tSiBurTotM
         pmsa( ipnt( 1851) ) = vDeltaWM
         pmsa( ipnt( 1852) ) = aRelDeltaWM
         pmsa( ipnt( 1853) ) = tDSetTot
         pmsa( ipnt( 1854) ) = tPSetTot
         pmsa( ipnt( 1855) ) = tNSetTot
         pmsa( ipnt( 1856) ) = tDResusTot
         pmsa( ipnt( 1857) ) = tPResusTot
         pmsa( ipnt( 1858) ) = tNResusTot
         pmsa( ipnt( 1859) ) = bTimeDred
         pmsa( ipnt( 1860) ) = aDepthStart
         pmsa( ipnt( 1861) ) = akDredDepth
         pmsa( ipnt( 1862) ) = akDred
         pmsa( ipnt( 1863) ) = akDredBent
         pmsa( ipnt( 1864) ) = vDredDepthW
         pmsa( ipnt( 1865) ) = tDDredDetS
         pmsa( ipnt( 1866) ) = tPDredDetS
         pmsa( ipnt( 1867) ) = tNDredDetS
         pmsa( ipnt( 1868) ) = tSiDredDetS
         pmsa( ipnt( 1869) ) = tPDredAIMS
         pmsa( ipnt( 1870) ) = bRhoSolidSoil
         pmsa( ipnt( 1871) ) = tDDredNetSoil
         pmsa( ipnt( 1872) ) = tDDredNetIMS
         pmsa( ipnt( 1873) ) = tDDredNetHumS
         pmsa( ipnt( 1874) ) = tPDredNetHumS
         pmsa( ipnt( 1875) ) = tNDredNetHumS
         pmsa( ipnt( 1876) ) = tDDredDiatS
         pmsa( ipnt( 1877) ) = tPDredDiatS
         pmsa( ipnt( 1878) ) = tNDredDiatS
         pmsa( ipnt( 1879) ) = tDDredGrenS
         pmsa( ipnt( 1880) ) = tPDredGrenS
         pmsa( ipnt( 1881) ) = tNDredGrenS
         pmsa( ipnt( 1882) ) = tDDredBlueS
         pmsa( ipnt( 1883) ) = tPDredBlueS
         pmsa( ipnt( 1884) ) = tNDredBlueS
         pmsa( ipnt( 1885) ) = tDDredPhytS
         pmsa( ipnt( 1886) ) = tPDredPhytS
         pmsa( ipnt( 1887) ) = tNDredPhytS
         pmsa( ipnt( 1888) ) = tDDredBent
         pmsa( ipnt( 1889) ) = tPDredBent
         pmsa( ipnt( 1890) ) = tNDredBent
         pmsa( ipnt( 1891) ) = tDDredVeg
         pmsa( ipnt( 1892) ) = tPDredVeg
         pmsa( ipnt( 1893) ) = tNDredVeg
         pmsa( ipnt( 1894) ) = tDDredNetTot
         pmsa( ipnt( 1895) ) = tPDredNetTot
         pmsa( ipnt( 1896) ) = tNDredNetTot
         pmsa( ipnt( 1897) ) = tSiDredTot
         pmsa( ipnt( 1898) ) = tDIMS
         pmsa( ipnt( 1899) ) = tDHumS
         pmsa( ipnt( 1900) ) = tDDetS
         pmsa( ipnt( 1901) ) = vDeltaS
         pmsa( ipnt( 1902) ) = tDBurIM
         pmsa( ipnt( 1903) ) = tDBurOM
         pmsa( ipnt( 1904) ) = tDBurDet
         pmsa( ipnt( 1905) ) = tDBurHum
         pmsa( ipnt( 1906) ) = tDBurTot
         pmsa( ipnt( 1907) ) = tPBurHum
         pmsa( ipnt( 1908) ) = tPBurDet
         pmsa( ipnt( 1909) ) = tPBurAIM
         pmsa( ipnt( 1910) ) = tPBurPO4
         pmsa( ipnt( 1911) ) = tPBurTot
         pmsa( ipnt( 1912) ) = tNBurHum
         pmsa( ipnt( 1913) ) = tNBurDet
         pmsa( ipnt( 1914) ) = tNBurNH4
         pmsa( ipnt( 1915) ) = tNBurNO3
         pmsa( ipnt( 1916) ) = tNBurTot
         pmsa( ipnt( 1917) ) = tSiBurDet
         pmsa( ipnt( 1918) ) = tSiBurTot
         pmsa( ipnt( 1919) ) = vDeltaW
         pmsa( ipnt( 1920) ) = aRelDeltaW
         pmsa( ipnt( 1921) ) = tDMarsTotT
         pmsa( ipnt( 1922) ) = tPMarsTotT
         pmsa( ipnt( 1923) ) = tNMarsTotT
         pmsa( ipnt( 1924) ) = tSiMarsTotT
         pmsa( ipnt( 1925) ) = aDTotT
         pmsa( ipnt( 1926) ) = aNTotT
         pmsa( ipnt( 1927) ) = aPTotT
         pmsa( ipnt( 1928) ) = aSiTotT
         pmsa( ipnt( 1929) ) = aDError
         pmsa( ipnt( 1930) ) = aNError
         pmsa( ipnt( 1931) ) = aPError
         pmsa( ipnt( 1932) ) = aSiError
         pmsa( ipnt( 1933) ) = dNH4W
         pmsa( ipnt( 1934) ) = dNO3W
         pmsa( ipnt( 1935) ) = dPO4W
         pmsa( ipnt( 1936) ) = dPAIMW
         pmsa( ipnt( 1937) ) = dSiO2W
         pmsa( ipnt( 1938) ) = dO2W
         pmsa( ipnt( 1939) ) = dDDetW
         pmsa( ipnt( 1940) ) = dNDetW
         pmsa( ipnt( 1941) ) = dPDetW
         pmsa( ipnt( 1942) ) = dSiDetW
         pmsa( ipnt( 1943) ) = dDIMW
         pmsa( ipnt( 1944) ) = dDDiatW
         pmsa( ipnt( 1945) ) = dNDiatW
         pmsa( ipnt( 1946) ) = dPDiatW
         pmsa( ipnt( 1947) ) = dDGrenW
         pmsa( ipnt( 1948) ) = dNGrenW
         pmsa( ipnt( 1949) ) = dPGrenW
         pmsa( ipnt( 1950) ) = dDBlueW
         pmsa( ipnt( 1951) ) = dNBlueW
         pmsa( ipnt( 1952) ) = dPBlueW
         pmsa( ipnt( 1953) ) = dDZoo
         pmsa( ipnt( 1954) ) = dNZoo
         pmsa( ipnt( 1955) ) = dPZoo
         pmsa( ipnt( 1956) ) = dDFiAd
         pmsa( ipnt( 1957) ) = dDFiJv
         pmsa( ipnt( 1958) ) = dNFiAd
         pmsa( ipnt( 1959) ) = dNFiJv
         pmsa( ipnt( 1960) ) = dPFiAd
         pmsa( ipnt( 1961) ) = dPFiJv
         pmsa( ipnt( 1962) ) = dDPisc
         pmsa( ipnt( 1963) ) = dNH4S
         pmsa( ipnt( 1964) ) = dNO3S
         pmsa( ipnt( 1965) ) = dPO4S
         pmsa( ipnt( 1966) ) = dPAIMS
         pmsa( ipnt( 1967) ) = dDDetS
         pmsa( ipnt( 1968) ) = dNDetS
         pmsa( ipnt( 1969) ) = dPDetS
         pmsa( ipnt( 1970) ) = dSiDetS
         pmsa( ipnt( 1971) ) = dDHumS
         pmsa( ipnt( 1972) ) = dNHumS
         pmsa( ipnt( 1973) ) = dPHumS
         pmsa( ipnt( 1974) ) = dDIMS
         pmsa( ipnt( 1975) ) = dDDiatS
         pmsa( ipnt( 1976) ) = dNDiatS
         pmsa( ipnt( 1977) ) = dPDiatS
         pmsa( ipnt( 1978) ) = dDGrenS
         pmsa( ipnt( 1979) ) = dNGrenS
         pmsa( ipnt( 1980) ) = dPGrenS
         pmsa( ipnt( 1981) ) = dDBlueS
         pmsa( ipnt( 1982) ) = dNBlueS
         pmsa( ipnt( 1983) ) = dPBlueS
         pmsa( ipnt( 1984) ) = dDVeg
         pmsa( ipnt( 1985) ) = dNVeg
         pmsa( ipnt( 1986) ) = dPVeg
         pmsa( ipnt( 1987) ) = dDBent
         pmsa( ipnt( 1988) ) = dNBent
         pmsa( ipnt( 1989) ) = dPBent
         pmsa( ipnt( 1990) ) = dDepthWM
         pmsa( ipnt( 1991) ) = dNH4WM
         pmsa( ipnt( 1992) ) = dNO3WM
         pmsa( ipnt( 1993) ) = dPO4WM
         pmsa( ipnt( 1994) ) = dPAIMWM
         pmsa( ipnt( 1995) ) = dSiO2WM
         pmsa( ipnt( 1996) ) = dO2WM
         pmsa( ipnt( 1997) ) = dDDetWM
         pmsa( ipnt( 1998) ) = dNDetWM
         pmsa( ipnt( 1999) ) = dPDetWM
         pmsa( ipnt( 2000) ) = dSiDetWM
         pmsa( ipnt( 2001) ) = dDIMWM
         pmsa( ipnt( 2002) ) = dDDiatWM
         pmsa( ipnt( 2003) ) = dNDiatWM
         pmsa( ipnt( 2004) ) = dPDiatWM
         pmsa( ipnt( 2005) ) = dDGrenWM
         pmsa( ipnt( 2006) ) = dNGrenWM
         pmsa( ipnt( 2007) ) = dPGrenWM
         pmsa( ipnt( 2008) ) = dDBlueWM
         pmsa( ipnt( 2009) ) = dNBlueWM
         pmsa( ipnt( 2010) ) = dPBlueWM
         pmsa( ipnt( 2011) ) = dDZooM
         pmsa( ipnt( 2012) ) = dNZooM
         pmsa( ipnt( 2013) ) = dPZooM
         pmsa( ipnt( 2014) ) = dNH4SM
         pmsa( ipnt( 2015) ) = dNO3SM
         pmsa( ipnt( 2016) ) = dPO4SM
         pmsa( ipnt( 2017) ) = dPAIMSM
         pmsa( ipnt( 2018) ) = dDDetSM
         pmsa( ipnt( 2019) ) = dNDetSM
         pmsa( ipnt( 2020) ) = dPDetSM
         pmsa( ipnt( 2021) ) = dSiDetSM
         pmsa( ipnt( 2022) ) = dDHumSM
         pmsa( ipnt( 2023) ) = dNHumSM
         pmsa( ipnt( 2024) ) = dPHumSM
         pmsa( ipnt( 2025) ) = dDIMSM
         pmsa( ipnt( 2026) ) = dDRootPhra
         pmsa( ipnt( 2027) ) = dDShootPhra
         pmsa( ipnt( 2028) ) = dNRootPhra
         pmsa( ipnt( 2029) ) = dNShootPhra
         pmsa( ipnt( 2030) ) = dPRootPhra
         pmsa( ipnt( 2031) ) = dPShootPhra
         pmsa( ipnt( 2032) ) = dDExtTotT
         pmsa( ipnt( 2033) ) = dNExtTotT
         pmsa( ipnt( 2034) ) = dPExtTotT
         pmsa( ipnt( 2035) ) = dSiExtTotT
         ID0sNH4W                = ID0sNH4W                 + noflux
         ID0sNO3W                = ID0sNO3W                 + noflux
         ID0sPO4W                = ID0sPO4W                 + noflux
         ID0sPAIMW               = ID0sPAIMW                + noflux
         ID0sSiO2W               = ID0sSiO2W                + noflux
         ID0sO2W                 = ID0sO2W                  + noflux
         ID0sDDetW               = ID0sDDetW                + noflux
         ID0sNDetW               = ID0sNDetW                + noflux
         ID0sPDetW               = ID0sPDetW                + noflux
         ID0sSiDetW              = ID0sSiDetW               + noflux
         ID0sDIMW                = ID0sDIMW                 + noflux
         ID0sDDiatW              = ID0sDDiatW               + noflux
         ID0sNDiatW              = ID0sNDiatW               + noflux
         ID0sPDiatW              = ID0sPDiatW               + noflux
         ID0sDGrenW              = ID0sDGrenW               + noflux
         ID0sNGrenW              = ID0sNGrenW               + noflux
         ID0sPGrenW              = ID0sPGrenW               + noflux
         ID0sDBlueW              = ID0sDBlueW               + noflux
         ID0sNBlueW              = ID0sNBlueW               + noflux
         ID0sPBlueW              = ID0sPBlueW               + noflux
         ID0sDZoo                = ID0sDZoo                 + noflux
         ID0sNZoo                = ID0sNZoo                 + noflux
         ID0sPZoo                = ID0sPZoo                 + noflux
         ID0sDFiAd               = ID0sDFiAd                + noflux
         ID0sDFiJv               = ID0sDFiJv                + noflux
         ID0sNFiAd               = ID0sNFiAd                + noflux
         ID0sNFiJv               = ID0sNFiJv                + noflux
         ID0sPFiAd               = ID0sPFiAd                + noflux
         ID0sPFiJv               = ID0sPFiJv                + noflux
         ID0sDPisc               = ID0sDPisc                + noflux
         ID0sNH4S                = ID0sNH4S                 + noflux
         ID0sNO3S                = ID0sNO3S                 + noflux
         ID0sPO4S                = ID0sPO4S                 + noflux
         ID0sPAIMS               = ID0sPAIMS                + noflux
         ID0sDDetS               = ID0sDDetS                + noflux
         ID0sNDetS               = ID0sNDetS                + noflux
         ID0sPDetS               = ID0sPDetS                + noflux
         ID0sSiDetS              = ID0sSiDetS               + noflux
         ID0sDHumS               = ID0sDHumS                + noflux
         ID0sNHumS               = ID0sNHumS                + noflux
         ID0sPHumS               = ID0sPHumS                + noflux
         ID0sDIMS                = ID0sDIMS                 + noflux
         ID0sDDiatS              = ID0sDDiatS               + noflux
         ID0sNDiatS              = ID0sNDiatS               + noflux
         ID0sPDiatS              = ID0sPDiatS               + noflux
         ID0sDGrenS              = ID0sDGrenS               + noflux
         ID0sNGrenS              = ID0sNGrenS               + noflux
         ID0sPGrenS              = ID0sPGrenS               + noflux
         ID0sDBlueS              = ID0sDBlueS               + noflux
         ID0sNBlueS              = ID0sNBlueS               + noflux
         ID0sPBlueS              = ID0sPBlueS               + noflux
         ID0sDVeg                = ID0sDVeg                 + noflux
         ID0sNVeg                = ID0sNVeg                 + noflux
         ID0sPVeg                = ID0sPVeg                 + noflux
         ID0sDBent               = ID0sDBent                + noflux
         ID0sNBent               = ID0sNBent                + noflux
         ID0sPBent               = ID0sPBent                + noflux
         ID0sDepthWM             = ID0sDepthWM              + noflux
         ID0sNH4WM               = ID0sNH4WM                + noflux
         ID0sNO3WM               = ID0sNO3WM                + noflux
         ID0sPO4WM               = ID0sPO4WM                + noflux
         ID0sPAIMWM              = ID0sPAIMWM               + noflux
         ID0sSiO2WM              = ID0sSiO2WM               + noflux
         ID0sO2WM                = ID0sO2WM                 + noflux
         ID0sDDetWM              = ID0sDDetWM               + noflux
         ID0sNDetWM              = ID0sNDetWM               + noflux
         ID0sPDetWM              = ID0sPDetWM               + noflux
         ID0sSiDetWM             = ID0sSiDetWM              + noflux
         ID0sDIMWM               = ID0sDIMWM                + noflux
         ID0sDDiatWM             = ID0sDDiatWM              + noflux
         ID0sNDiatWM             = ID0sNDiatWM              + noflux
         ID0sPDiatWM             = ID0sPDiatWM              + noflux
         ID0sDGrenWM             = ID0sDGrenWM              + noflux
         ID0sNGrenWM             = ID0sNGrenWM              + noflux
         ID0sPGrenWM             = ID0sPGrenWM              + noflux
         ID0sDBlueWM             = ID0sDBlueWM              + noflux
         ID0sNBlueWM             = ID0sNBlueWM              + noflux
         ID0sPBlueWM             = ID0sPBlueWM              + noflux
         ID0sDZooM               = ID0sDZooM                + noflux
         ID0sNZooM               = ID0sNZooM                + noflux
         ID0sPZooM               = ID0sPZooM                + noflux
         ID0sNH4SM               = ID0sNH4SM                + noflux
         ID0sNO3SM               = ID0sNO3SM                + noflux
         ID0sPO4SM               = ID0sPO4SM                + noflux
         ID0sPAIMSM              = ID0sPAIMSM               + noflux
         ID0sDDetSM              = ID0sDDetSM               + noflux
         ID0sNDetSM              = ID0sNDetSM               + noflux
         ID0sPDetSM              = ID0sPDetSM               + noflux
         ID0sSiDetSM             = ID0sSiDetSM              + noflux
         ID0sDHumSM              = ID0sDHumSM               + noflux
         ID0sNHumSM              = ID0sNHumSM               + noflux
         ID0sPHumSM              = ID0sPHumSM               + noflux
         ID0sDIMSM               = ID0sDIMSM                + noflux
         ID0sDRootPhra           = ID0sDRootPhra            + noflux
         ID0sDShootPhra          = ID0sDShootPhra           + noflux
         ID0sNRootPhra           = ID0sNRootPhra            + noflux
         ID0sNShootPhra          = ID0sNShootPhra           + noflux
         ID0sPRootPhra           = ID0sPRootPhra            + noflux
         ID0sPShootPhra          = ID0sPShootPhra           + noflux
         ID0sDExtTotT            = ID0sDExtTotT             + noflux
         ID0sNExtTotT            = ID0sNExtTotT             + noflux
         ID0sPExtTotT            = ID0sPExtTotT             + noflux
         ID0sSiExtTotT           = ID0sSiExtTotT            + noflux

         ipnt        = ipnt        + increm 
   !   
    9000 continue  
   !   
         return    
         end subroutine    
