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
         integer ipoint(1624) ! I  Array of pointers in pmsa to get and store the data 
         integer increm(1624) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying  
         integer noseg        ! I  Number of computational elements in the whole model schematisation  
         integer noflux       ! I  Number of fluxes, increment in the fl array 
         integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces  
         integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use   
         integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh) 
         integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid   
         integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward 
         integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)  
         integer ipnt(1624)   !    Local work array for the pointering 
         integer iseg         !    Local loop counter for computational element loop   
!
!*******************************************************************************
!
!     Type    Name          I/O Description                                        Unit
!
!     /******************************************************************************/
!     /*                                                                            */
!     /* Eutrophication model pcditch pd21316 for DELWAQ                            */
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
      real(4) sDDetS         	! [      3.63E+02 ]	gDW/m2		Detritus_DW_in_ditch_sediment
      real(4) sDDetW         	! [             2 ]	gDW/m3		Detritus_DW_in_ditch_water
      real(4) sDHumS         	! [      3.27E+03 ]	gDW/m2		Humus_DW_in_ditch_sediment
      real(4) sDIMS          	! [      3.27E+04 ]	gDW/m2		Inorganic_matter_in_ditch_sediment
      real(4) sDIMW          	! [             5 ]	gDW/m3		Inorganic_matter_in_ditch_water
      real(4) sNDetS         	! [      9.09E+00 ]	gN/m2		Detritus_N_in_ditch_sediment
      real(4) sNDetW         	! [          0.05 ]	gN/m3		Detritus_N_in_ditch_water
      real(4) sNH4S          	! [          0.02 ]	gN/m2		N_in_NH4_in_ditch_sediment_interstitial_water
      real(4) sNH4W          	! [           0.1 ]	gN/m3		N_in_NH4_in_ditch_water
      real(4) sNHumS         	! [      1.64E+02 ]	gN/m2		Humus_N_in_ditch_sediment
      real(4) sNO3S          	! [         0.002 ]	gN/m2		N_in_NO3_in_ditch_sediment_interstitial_water
      real(4) sNO3W          	! [           0.1 ]	gN/m3		N_in_NO3_in_ditch_water
      real(4) sO2W           	! [            10 ]	gO2/m3		Oxygen_in_ditch_water
      real(4) sPAIMS         	! [      1.80E+01 ]	gP/m2		P_adsorbed_onto_IM_in_ditch_sediment
      real(4) sPAIMW         	! [             0 ]	gP/m3		P_adsorbed_onto_IM_in_ditch_water
      real(4) sPDetS         	! [      9.09E-01 ]	gP/m2		Detritus_P_in_ditch_sediment
      real(4) sPDetW         	! [         0.005 ]	gP/m3		Detritus_P_in_ditch_water
      real(4) sPHumS         	! [      1.64E+01 ]	gP/m2		Humus_P_in_ditch_sediment
      real(4) sPO4S          	! [      1.82E-01 ]	gP/m2		P_in_PO4_in_ditch_sediment_interstitial_water
      real(4) sPO4W          	! [          0.01 ]	gP/m3		P_in_PO4_in_ditch_water
      real(4) sDPhytW        	! [             1 ]	gDW/m3  		DW_of_Phytoplankton
      real(4) sPPhytW        	! [          0.01 ]	gP/m3  		P_in_Phytoplankton
      real(4) sNPhytW        	! [           0.1 ]	gN/m3  		N_in_Phytoplankton
      real(4) sDPhytS        	! [         0.001 ]	gDW/m2		DW_of_settled_phytoplankton
      real(4) sPPhytS        	! [       0.00001 ]	gP/m2  		P_in_settled_phytoplankton
      real(4) sNPhytS        	! [        0.0001 ]	gN/m2  		N_in_settled_phytoplankton
      real(4) sDElod         	! [             1 ]	gDW/m2  		DW_of_Elodeids
      real(4) sDChar         	! [             1 ]	gDW/m2  		DW_of_Characeans
      real(4) sDCera         	! [             1 ]	gDW/m2  		DW_of_Ceratophyllids
      real(4) sDLemn         	! [             1 ]	gDW/m2  		DW_of_Lemnacaea
      real(4) sDNymp         	! [             1 ]	gDW/m2  		DW_of_Nymphaeids
      real(4) sDHelo         	! [             1 ]	gDW/m2  		DW_of_helophytes
      real(4) sPElod         	! [         0.002 ]	gP/m2  		P_in_Elodeids
      real(4) sPChar         	! [         0.002 ]	gP/m2  		P_in_Characeans
      real(4) sPCera         	! [         0.002 ]	gP/m2  		P_in_Ceratophyllids
      real(4) sPLemn         	! [         0.005 ]	gP/m2  		P_in_Lemnacaea
      real(4) sPNymp         	! [         0.002 ]	gP/m2  		P_in_Nymphaeids
      real(4) sPHelo         	! [         0.002 ]	gP/m2  		P_in_helophytes
      real(4) sNElod         	! [          0.02 ]	gN/m2  		N_in_Elodeids
      real(4) sNChar         	! [          0.02 ]	gN/m2  		N_in_Characeans
      real(4) sNCera         	! [          0.02 ]	gN/m2  		N_in_Ceratophyllids
      real(4) sNLemn         	! [          0.05 ]	gN/m2  		N_in_Lemnacaea
      real(4) sNNymp         	! [          0.02 ]	gN/m2  		N_in_Nymphaeids
      real(4) sNHelo         	! [          0.02 ]	gN/m2  		N_in_helophytes
      real(4) sDExtTotT      	! [             0 ]	gDW/m2		Total_amount_of_DW_moved_into_or_out_from_the_system
      real(4) sNExtTotT      	! [             0 ]	gN/m2		Total_amount_of_N_moved_into_or_out_from_the_system
      real(4) sPExtTotT      	! [             0 ]	gP/m2		Total_amount_of_P_moved_into_or_out_from_the_system
!
!
!     /* ==============================  */
!     /* declaration parameters          */
!     /* ==============================  */
      real(4) ITIME                     ! [           0]	sec                     Time of DELWAQ
      real(4) TotalDepth                ! [           0]	m                       depth of DELWAQ
      real(4) BeginTime           	! [           0]	day                 	begintime
      real(4) ConstDepth          	! [           1]	-                   	if_1_water_depth_kept_constant_by_daily_dredging
      real(4) ReadTemp            	! [           0]	-                   	if_1_use_measured_time-series_of_temperature_otherwise_sinus
      real(4) ReadLOut            	! [           0]	-                   	if_1_use_measured_time-series_of_light_otherwise_sinus
      real(4) ReadVWind           	! [           0]	-                   	if_1_use_measured_time-series_of_wind_speed_otherwise_constant
      real(4) InitCalc            	! [           0]	-                   	if_1_calculate_initial_values
      real(4) InclTran            	! [           0]	-                   	if_1_use_transportmodule_(in_Duflow_transport_module_can_not_be_used
      real(4) ReadQIn             	! [           0]	-                   	if_1_use_measured_time-series_of_water_inflow_otherwise_constant
      real(4) ReadQOut            	! [           0]	-                   	if_1_use_measured_time-series_of_water_outflow_otherwise_constant
      real(4) ReadQEv             	! [           0]	-                   	if_1_use_measured_time-series_of_evaporation_otherwise_constant
      real(4) ReadPLoad           	! [           0]	-                   	if_1_use_measured_time-series_of_P_loading_otherwise_constant
      real(4) ReadNLoad           	! [           0]	-                   	if_1_use_measured_time-series_of_N_loading_otherwise_constant
      real(4) ReadNutFrac         	! [           0]	-                   	if_1_use_measured_time-series_of_loading_with_diff_nutrient_fractions
      real(4) ReadPLoadPhyt       	! [           0]	-                   	if_1_use_measured_time-series_of_P_phytoplankton_loading_otherwise_constant
      real(4) ReadDLoadDet        	! [           0]	-                   	if_1_use_measured_time-series_of_DDet_loading_otherwise_constant
      real(4) ReadDLoadIM         	! [           0]	-                   	if_1_use_measured_time-series_of_DIM_loading_otherwise_constant
      real(4) UseSeasonLoad       	! [           0]	-                   	if_1_use_different_inflow_and_loading_for_summer_and_winter_periods
      real(4) UsePulseLoad        	! [           0]	-                   	if_1_use_a_pulse-wise_nutrient_loading
      real(4) mTemp               	! [           0]	oC                  	measured_time-series_of_temperature
      real(4) mLOut               	! [           0]	W/m2                	measured_time-series_of_light
      real(4) mVWind              	! [           0]	m/s                 	measured_time-series_of_wind_speed
      real(4) mQIn                	! [           0]	mm/day              	use_measured_time-series_of_inflow
      real(4) mQOut               	! [           0]	mm/day              	use_measured_time-series_of_outflow
      real(4) mQEv                	! [           0]	mm/day              	use_measured_time-series_of_evaporation
      real(4) mPLoad              	! [           0]	gP/m2/day           	use_measured_time-series_of_P_loading
      real(4) mPLoadPO4           	! [           0]	gP/m2/day           	use_measured_time-series_of_PO4_loading
      real(4) mPLoadOrg           	! [           0]	gP/m2/day           	use_measured_time-series_of_loading_P_bound_to_organic_matter
      real(4) mPLoadPhytTot       	! [           0]	gP/m2/day           	use_measured_time-series_of_P_loading_algal_input
      real(4) mNLoad              	! [           0]	gN/m2/day           	use_measured_time-series_of_N_loading
      real(4) mNLoadNH4           	! [           0]	gN/m2/day           	use_measured_time-series_of_NH4_loading
      real(4) mNLoadNO3           	! [           0]	gN/m2/day           	use_measured_time-series_of_NO3_loading
      real(4) mNLoadOrg           	! [           0]	gN/m2/day           	use_measured_time-series_of_loading_N_bound_to_organic_matter
      real(4) mDLoadDet           	! [           0]	gDW/m2/day          	use_measured_time-series_of_Detritus_loading
      real(4) mDLoadIM            	! [           0]	gDW/m2/day          	use_measured_time-series_of_loading_of_DW_of_inorganic_matter
      real(4) fDTotS0             	! [         0.3]	gDW/g_sediment      	initial_dry-weight_fraction_in_sediment
      real(4) fDOrgS0             	! [         0.1]	gOM/gDW             	initial_organic_fraction_of_sediment_DW
      real(4) fDDetS0             	! [         0.1]	dDet/gOM            	initial_detritus_fraction_of_sediment_organic_matter
      real(4) fPInorgS0           	! [      0.0005]	gP/gDW              	initial_inorganic_P_fraction_in_sediment
      real(4) fPAdsS0             	! [        0.99]	gP_Ads/gP_IM        	initial_adsorbed_fraction_of_inorganic_P_in_sediment
      real(4) cPDDet0             	! [      0.0025]	gP/gDW              	initial_P_fraction_in_detritus
      real(4) cNDDet0             	! [       0.025]	gN/gDW              	initial_N_fraction_in_detritus
      real(4) cPDHum0             	! [       0.005]	gP/gDW              	initial_P_fraction_in_humus
      real(4) cNDHum0             	! [        0.05]	gN/gDW              	initial_N_fraction_in_humus
      real(4) cPDPhyt0            	! [        0.01]	gP/gDW              	initial_P_fraction_in_phytoplankton
      real(4) cNDPhyt0            	! [         0.1]	gN/gDW              	initial_N_fraction_in_phytoplankton
      real(4) cPDElod0            	! [       0.002]	gP/gDW              	initial_P_fraction_in_Elodeids
      real(4) cNDElod0            	! [        0.02]	gN/gDW              	initial_N_fraction_in_Elodeids
      real(4) cPDChar0            	! [       0.002]	gP/gDW              	initial_P_fraction_in_Characeans
      real(4) cNDChar0            	! [        0.02]	gN/gDW              	initial_N_fraction_in_Characeans
      real(4) cPDCera0            	! [       0.002]	gP/gDW              	initial_P_fraction_in_Ceratophyllids
      real(4) cNDCera0            	! [        0.02]	gN/gDW              	initial_N_fraction_in_Ceraphyllids
      real(4) cPDLemn0            	! [       0.005]	gP/gDW              	initial_P_fraction_in_Lemnacaea
      real(4) cNDLemn0            	! [        0.05]	gN/gDW              	initial_N_fraction_in_Lemnacaea
      real(4) cPDNymp0            	! [       0.002]	gP/gDW              	initial_P_fraction_in_Nymphaeids
      real(4) cNDNymp0            	! [        0.02]	gN/gDW              	initial_N_fraction_in_Nymphaeids
      real(4) cPDHelo0            	! [       0.002]	gP/gDW              	initial_P_fraction_in_helophytes
      real(4) cNDHelo0            	! [        0.02]	gN/gDW              	initial_N_fraction_in_helophytes
      real(4) cQInf               	! [           0]	mm/day              	infiltration_rate
      real(4) cPBackLoad          	! [           0]	gP/m2/day           	Background_PO4_loading
      real(4) cNBackLoad          	! [           0]	gP/m2/day           	Background_NH4_loading
      real(4) cNLoadS             	! [           0]	gN/m2/day           	N_fertilizer_to_sediment
      real(4) fNH4LoadS           	! [         0.5]	-                   	NH4_fraction_of_N_fertilizer_to_sediment
      real(4) cPO4Ground          	! [         0.1]	gP/m3               	PO4_concentration_in_groundwater
      real(4) cNH4Ground          	! [           1]	gN/m3               	NH4_concentration_in_groundwater
      real(4) cNO3Ground          	! [         0.1]	gN/m3               	NO3_concentration_in_groundwater
      real(4) cQIn                	! [          30]	mm/day              	standard_water_inflow_if_not_measured
      real(4) cQInSum             	! [          30]	mm/day              	summer_water_inflow_if_not_measured
      real(4) cQInWin             	! [          30]	mm/day              	winter_water_inflow_if_not_measured
      real(4) cDepthWMax          	! [           3]	m                   	maximum_water_depth
      real(4) cQInExtraApril1     	! [           0]	mm/day              	extra_water_inflow_at_start_of_summer
      real(4) cQInExtraOct1       	! [           0]	mm/day              	extra_water_inflow_at_start_of_winter
      real(4) cQOutExtraApril1    	! [           0]	mm/day              	extra_water_outflow_at_start_of_summer
      real(4) cQOutExtraOct1      	! [           0]	mm/day              	extra_water_outflow_at_start_of_winter
      real(4) cQEvAve             	! [         1.5]	mm/day              	average_evaporation
      real(4) cQEvVar             	! [         1.3]	mm/day              	variation_in_evaporation
      real(4) cPLoad              	! [        0.02]	gP/m2/day           	P_loading_if_not_measured
      real(4) cPLoadSum           	! [        0.02]	gP/m2/day           	summer_P_loading_if_not_measured
      real(4) cPLoadWin           	! [        0.02]	gP/m2/day           	winter_P_loading_if_not_measured
      real(4) fPO4In              	! [         0.8]	-                   	fraction_PO4_in_input_(if_PO4_input_not_measured)
      real(4) fPhytInWin          	! [       0.001]	-                   	minimum_algal_fraction_in_organic_P_input
      real(4) fPhytInSum          	! [       0.002]	-                   	maximum_algal_fraction_in_organic_P_input
      real(4) cNLoad              	! [         0.2]	gN/m2/day           	N_loading
      real(4) cNLoadSum           	! [         0.2]	gN/m2/day           	summer_N_loading
      real(4) cNLoadWin           	! [         0.2]	gN/m2/day           	winter_N_loading
      real(4) cNPLoadMeas         	! [           7]	gN/gP               	N/P_loading_if_P_is_measured_and_N_not
      real(4) cNPPhytIn           	! [           7]	gP/gDW              	N/P_ratio_of_algal_input
      real(4) cNPDetIn            	! [           7]	gP/gDW              	N/P_ratio_of_detrital_input
      real(4) fNH4DissIn          	! [         0.5]	-                   	NH4_fraction_of_dissolved_N_load_(if_NH4_not_measured)
      real(4) cNDPhytIn           	! [        0.07]	gN/gDW              	N/D_ratio_of_algal_input
      real(4) cNDDetIn            	! [        0.07]	gN/gDW              	N/D_ratio_of_detrital_input
      real(4) cDIMIn              	! [          10]	gDW/m3              	inorganic_matter_concentration_in_inflow_water
      real(4) cO2In               	! [           5]	gO2/m3              	O2_concentration_in_inflow_water
      real(4) cDredInterval       	! [     9999000]	year                	dredging_interval
      real(4) cDredStart          	! [     9999000]	year                	first_dredging_year_(should_be_n_times_cDredInterval_)
      real(4) cDepthRef           	! [       1E-28]	m                   	reference_water_depth_for_dredging
      real(4) cLengDred           	! [          10]	day                 	length_of_dredging_period
      real(4) fEffDred            	! [        0.95]	-                   	dredging_efficiency_(<10)
      real(4) fEffDredLemn        	! [         0.5]	-                   	dredging_efficiency_for_duckweed_(<10)
      real(4) cFetch              	! [           0]	m                   	wind_fetch
      real(4) cTmAve              	! [          12]	oC                  	average_water_temperature
      real(4) cTmVar              	! [          10]	oC                  	annual_temperature_variation
      real(4) cTimeLag            	! [          40]	day                 	time_lag_for_temperature
      real(4) cVWind              	! [           5]	m/s                 	average_wind_speed_needed_to_calculate_reaeration
      real(4) cLDayAve            	! [    1.00E+07]	J/m2/day            	annual_average_radiation
      real(4) cLDayVar            	! [     8000000]	J/m2/day            	annual_variation_in_radiation
      real(4) cfDayAve            	! [         0.5]	-                   	average_day_length
      real(4) cfDayVar            	! [         0.2]	-                   	annual_variation_in_day_length
      real(4) fRefl               	! [         0.2]	-                   	the_fraction_photosynthetically_active_radiation_reflected_at_the_surface
      real(4) fPAR                	! [        0.48]	-                   	fraction_photosynthetically_active_radiation_(PAR)
      real(4) cDayManVeg1         	! [   -1.00E+07 ]	day                 	first_mowing_day_(default_non-existent)
      real(4) cDayManVeg2         	! [   -1.00E+07 ]	day                 	second_mowing_day_(_259_=_16_Sep)
      real(4) fManVeg             	! [         0.8]	-                   	fraction_removed_by_management_for_submerged_plants
      real(4) fManLemn            	! [         0.4]	-                   	fraction_of_Lemnacae_removed_by_management
      real(4) fManHelo            	! [         0.8]	-                   	fraction_of_helophytes_and_Nymphaeids_removed_by_management
      real(4) cLengMan            	! [          10]	day                 	length_of_mowing_period
      real(4) cYearStartBirds     	! [           0]	year                	first_year_of_birds_presence
      real(4) cDayStartBirds      	! [          46]	day                 	yearly_first_day_of_birds_presence
      real(4) cDayEndBirds        	! [         288]	day                 	yearly_last_day_of_birds_presence
      real(4) cBirdsPerha         	! [           0]	n/ha                	number_of_birds_per_ha
      real(4) cDGrazPerBird       	! [          45]	gDW/coot/day        	daily_grazing_of_birds
      real(4) hDVegBird           	! [           5]	gDW/m2              	half-sat_vegetation_biomass_for_birds_grazing
      real(4) fDAssBird           	! [         0.5]	-                   	birds_assim_efficiency
      real(4) fDissEgesBird       	! [        0.25]	-                   	fraction_dissolved_nutrient_of_coot_egestion
      real(4) cDErosTot           	! [          10]	gDW/m2/day          	DW_input_by_erosion
      real(4) cExtWat             	! [         0.5]	m-1                 	background_extinction
      real(4) cExtSpDet           	! [        0.15]	m2/gDW              	specific_extinction_detritus
      real(4) cExtSpIM            	! [        0.05]	m2/gDW              	specific_extinction_inert_matter
      real(4) cExtSpPhyt          	! [        0.25]	m2/gDW              	specific_extinction_phytoplankton
      real(4) fDOrgSoil           	! [         0.1]	-                   	fraction_soil_organic_matter
      real(4) cPDSoilOM           	! [       0.001]	gP/gDW              	P/DW_ratio_of_soil_organic_matter
      real(4) cNDSoilOM           	! [        0.01]	gN/gDW              	N/DW_ratio_of_soil_organic_matter
      real(4) cDepthS             	! [         0.1]	m                   	sediment_depth
      real(4) fLutum              	! [         0.1]	-                   	lutum_content_of_inorganic_matter
      real(4) fFeDIM              	! [        0.01]	gFe/gDW             	Fe_content_of_inorganic_matter
      real(4) fAlDIM              	! [        0.01]	gAl/gDW             	Al_content_of_inorganic_matter
      real(4) cCPerDW             	! [         0.4]	gC/gDW              	C_content_of_organic_matter
      real(4) cRhoIM              	! [     2500000]	g/m3_solid          	density_of_sediment_inorganic_matter
      real(4) cRhoOM              	! [     1400000]	g/m3                	density_of_sediment_detritus
      real(4) cAerRoot            	! [       0.727]	-                   	reaeration_coefficient_for_sqrt(VWind)
      real(4) cAerLin             	! [      -0.371 ]	s/day               	reaeration_coefficient_for_Vwind
      real(4) cAerSquare          	! [      0.0376]	-                   	reaeration_coefficient_for_Vwind^2
      real(4) cThetaAer           	! [       1.024]	1/e^oC              	temperature_coeff_for_reaeration_(Downing_&_Truesdale_1955)
      real(4) kLemnAer            	! [        0.01]	m2/gDW              	Reaeration_reduction_coeff_for_duckweed
      real(4) fSedErosIM          	! [        0.95]	-                   	instantly_sedimentating_fraction_of_inorganic_matter
      real(4) cVSetIM             	! [           1]	m/day               	maximum_sedimentation_velocity_of_inert_organic_matter
      real(4) cVSetDet            	! [        0.25]	m/day               	maximum_sedimentation_velocity_of_detritus
      real(4) cThetaSet           	! [        1.01]	1/e^oC              	temperature_parameter_of_sedimentation
      real(4) cSuspRef            	! [         0.5]	-                   	reference_suspended_matter_function
      real(4) cSuspMin            	! [         6.1]	-                   	minimum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSuspMax            	! [        25.2]	-                   	maximum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSuspSlope          	! [         2.1]	-                   	slope_of_logistic_empirical_suspended_matter_function
      real(4) hDepthSusp          	! [           2]	-                   	half_sat_value_of_depth_in_logistic_empirical_suspended_matter_function
      real(4) cFetchRef           	! [        1000]	m                   	reference_fetch
      real(4) fLutumRef           	! [         0.2]	-                   	reference_lutum_fraction_of_sediment
      real(4) kVegResus           	! [        0.01]	m2/gDW              	resuspension_reduction_per_g_vegetation
      real(4) kResusPhytMax       	! [        0.25]	day-1               	maximum_resuspension_of_phytoplankton
      real(4) cResusPhytExp       	! [      -0.379 ]	(gDW/m2/day)-1      	coefficient_for_phytoplankton_resuspension
      real(4) kPDifPO4            	! [    7.20E-05]	m2/day              	mol_PO4_diffusion_constant
      real(4) kNDifNO3            	! [    8.60E-05]	m2/day              	mol_NO3_diffusion_constant
      real(4) kNDifNH4            	! [    1.12E-04]	m2/day              	mol_NH4_diffusion_constant
      real(4) kO2Dif              	! [    2.60E-05]	m2/day              	mol_O2_diffusion_constant
      real(4) cThetaDif           	! [        1.02]	1/eoC               	Temperature_coefficient_for_diffusion
      real(4) fDepthDifS          	! [         0.5]	-                   	nutrient_diffusion_distance_as_fraction_of_sediment_depth
      real(4) cTurbDifNut         	! [           5]	-                   	bioturbation_factor_for_diffusion
      real(4) cTurbDifO2          	! [           5]	-                   	bioturbation_factor_for_diffusion
      real(4) kPSorp              	! [        0.05]	day-1               	P_sorption_rate_constant_(not_too_high_->_model_speed)
      real(4) cRelPAdsD           	! [     0.00003]	gP/gDW              	maximum_P_adsorption_per_g_DW
      real(4) cRelPAdsFe          	! [       0.065]	gP/gFe              	maximum_P_adsorption_per_g_Fe
      real(4) cRelPAdsAl          	! [       0.134]	gP/gAl              	maximum_P_adsorption_per_g_Al
      real(4) cKPAdsOx            	! [         0.6]	m3/gP               	P_adsorption_affinity_at_oxidized_conditions
      real(4) fRedMax             	! [         0.9]	-                   	maximum_reduction_factor_of_P_adsorption_affinity
      real(4) coPO4Max            	! [           2]	gP/m3               	maximum_SRP_concentration_in_pore_water
      real(4) kPChemPO4           	! [        0.03]	day-1               	chemical_PO4_loss_rate_by_precipitation
      real(4) cTmRef              	! [          20]	oC                  	reference_temperature
      real(4) fRefrDetS           	! [        0.15]	-                   	refractory_fraction_of_sediment_detritus
      real(4) cThetaMinW          	! [        1.07]	-                   	exponential_temperature_constant_of_mineralization_in_water
      real(4) kDMinDetW           	! [        0.01]	day-1               	decomposition_constant_of_detritus
      real(4) hO2BOD              	! [           1]	gO2/m3              	half-sat_oxygen_conc_for_BOD
      real(4) cThetaMinS          	! [        1.07]	-                   	exponential_temperature_constant_of_sediment_mineralization
      real(4) kDMinDetS           	! [       0.002]	day-1               	decomposition_constant_of_sediment_detritus
      real(4) kDMinHum            	! [     0.00001]	day-1               	maximum_decomposition_constant_of_humic_material
      real(4) hNO3Denit           	! [           2]	gN/m3               	quadratic_half-sat_NO3_concentration_for_denitrification
      real(4) NO3PerC             	! [         0.8]	-                   	mol_NO3_denitrified_per_mol_C_mineralised
      real(4) kNitrW              	! [         0.1]	day-1               	nitrification_rate_constant_in_water
      real(4) kNitrS              	! [           1]	day-1               	nitrification_rate_constant_in_sediment
      real(4) cThetaNitr          	! [        1.08]	1/eoC               	temperature_coefficient_of_nitrification
      real(4) O2PerNH4            	! [           2]	-                   	mol_O2_used_per_mol_NH4+_nitrified
      real(4) hO2Nitr             	! [           2]	gO2/m3              	half-sat_O2_concentration_for_nitrification_in_water
      real(4) fDissMortVeg        	! [        0.25]	-                   	fraction_dissolved_nutrients_from_died_plants
      real(4) cLengAllo           	! [          15]	day                 	duration_of_allocation_and_reallocation_phase
      real(4) cLengMort           	! [          15]	day                 	duration_of_autumn_mortality_period
      real(4) fRootElodSum        	! [         0.1]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootElodWin        	! [         0.6]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatElod          	! [           0]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergElod          	! [           0]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Elod         	! [           0]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Elod         	! [           1]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerElod         	! [           0]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpElod          	! [         0.5]	%_cover_per_gDW_Shoo	specific_cover
      real(4) kMigrElod           	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDElodIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitElod         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrElod          	! [         500]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxElod          	! [        0.32]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdElod        	! [         1.2]	-                   	temperature_quotient_of_production
      real(4) hLRefElod           	! [          32]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpElod          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespElod          	! [       0.024]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespElod        	! [         1.5]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinElod         	! [         259]	day                 	end_of_growing_season_(259_=_16_Sep)
      real(4) kMortElodSum        	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinElod            	! [         0.2]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortElod       	! [         0.5]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefElodBird       	! [           1]	-                   	edibility_for_birds
      real(4) cVPUptMaxElod       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptElod        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDElodMin          	! [      0.0008]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDElodMax          	! [      0.0035]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxElod       	! [         0.1]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptElod        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDElodMin          	! [        0.01]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDElodMax          	! [       0.035]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) fRootCharSum        	! [        0.05]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootCharWin        	! [         0.1]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatChar          	! [           0]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergChar          	! [           0]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Char         	! [         0.5]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Char         	! [           1]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerChar         	! [           0]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpChar          	! [         0.5]	%_cover_per_gDW_Shoo	specific_cover
      real(4) kMigrChar           	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDCharIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitChar         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrChar          	! [         500]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxChar          	! [        0.22]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdChar        	! [         1.2]	-                   	temperature_quotient_of_production
      real(4) hLRefChar           	! [          19]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpChar          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespChar          	! [       0.025]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespChar        	! [         1.2]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinChar         	! [         259]	day                 	end_of_growing_season_(259_=_16_Sep)
      real(4) kMortCharSum        	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinChar            	! [         0.9]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortChar       	! [         0.5]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefCharBird       	! [         0.5]	-                   	edibility_for_birds
      real(4) cVPUptMaxChar       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptChar        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDCharMin          	! [      0.0012]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDCharMax          	! [      0.0035]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxChar       	! [         0.1]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptChar        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDCharMin          	! [        0.01]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDCharMax          	! [       0.035]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) fRootCeraSum        	! [           0]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootCeraWin        	! [           0]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatCera          	! [           0]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergCera          	! [           0]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Cera         	! [           0]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Cera         	! [         0.5]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerCera         	! [           0]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpCera          	! [         0.5]	%_cover_per_gDW_Shoo	specific_cover
      real(4) kMigrCera           	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDCeraIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitCera         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrCera          	! [         500]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxCera          	! [        0.21]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdCera        	! [         1.5]	-                   	temperature_quotient_of_production
      real(4) hLRefCera           	! [          25]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpCera          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespCera          	! [       0.024]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespCera        	! [           2]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinCera         	! [         259]	day                 	end_of_growing_season_(259_=_16_Sep)
      real(4) kMortCeraSum        	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinCera            	! [         0.1]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortCera       	! [         0.5]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefCeraBird       	! [           0]	-                   	edibility_for_birds
      real(4) cVPUptMaxCera       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptCera        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDCeraMin          	! [      0.0012]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDCeraMax          	! [      0.0035]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxCera       	! [         0.1]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptCera        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDCeraMin          	! [        0.01]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDCeraMax          	! [       0.033]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) fObstrLemn          	! [           1]	-                   	obstructed_fraction_of_Lemnacaea_outflow
      real(4) fRootLemnSum        	! [           0]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootLemnWin        	! [           0]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatLemn          	! [           1]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergLemn          	! [           0]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Lemn         	! [           0]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Lemn         	! [           0]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerLemn         	! [         100]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpLemn          	! [           1]	%_cover_per_gDW_Shoo	specific_cover
      real(4) ckMigrLemn          	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDLemnIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitLemn         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrLemn          	! [         575]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxLemn          	! [         0.4]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdLemn        	! [         2.5]	-                   	temperature_quotient_of_production
      real(4) hLRefLemn           	! [         7.5]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpLemn          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespLemn          	! [        0.03]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespLemn        	! [           3]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinLemn         	! [         289]	day                 	end_of_growing_season_(289_=_16_Oct)
      real(4) kMortLemnSum        	! [        0.02]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinLemn            	! [         0.2]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortLemn       	! [         0.8]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefLemnBird       	! [           0]	-                   	edibility_for_birds
      real(4) cVPUptMaxLemn       	! [       0.005]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptLemn        	! [        0.02]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDLemnMin          	! [       0.004]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDLemnMax          	! [       0.026]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxLemn       	! [        0.05]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptLemn        	! [        0.02]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDLemnMin          	! [        0.04]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDLemnMax          	! [         0.1]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) fRootNympSum        	! [        0.75]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootNympWin        	! [        0.95]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatNymp          	! [           1]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergNymp          	! [           0]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Nymp         	! [           0]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Nymp         	! [           0]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerNymp         	! [         100]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpNymp          	! [         0.5]	%_cover_per_gDW_Shoo	specific_cover
      real(4) kMigrNymp           	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDNympIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitNymp         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrNymp          	! [         500]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxNymp          	! [         0.1]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdNymp        	! [         1.5]	-                   	temperature_quotient_of_production
      real(4) hLRefNymp           	! [          25]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpNymp          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespNymp          	! [        0.01]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespNymp        	! [           2]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinNymp         	! [         259]	day                 	end_of_growing_season_(259_=_16_Sep)
      real(4) kMortNympSum        	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinNymp            	! [       0.333]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortNymp       	! [        0.25]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefNympBird       	! [           0]	-                   	edibility_for_birds
      real(4) cVPUptMaxNymp       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptNymp        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDNympMin          	! [       0.001]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDNympMax          	! [      0.0075]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxNymp       	! [         0.1]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptNymp        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDNympMin          	! [        0.01]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDNympMax          	! [        0.03]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) fRootHeloSum        	! [         0.5]	gDW_Root/gDW        	root_fraction_in_growing_season
      real(4) fRootHeloWin        	! [         0.8]	gDW_Root/gDW        	root_fraction_outside_growing_season
      real(4) fFloatHelo          	! [           0]	gDW_Floating/gDW_Sho	floating_fraction_of_shoot
      real(4) fEmergHelo          	! [           1]	gDW_Emergent/gDW_Sho	emergent_fraction_of_shoot
      real(4) fDepth1Helo         	! [           0]	-                   	maximum_upper_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) fDepth2Helo         	! [           0]	-                   	maximum_lower_depth_of_submerged_vegetation_layer_as_fraction_of_water_depth
      real(4) cDLayerHelo         	! [           0]	gDW/m2              	biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpHelo          	! [        0.05]	%_cover_per_gDW_Shoo	specific_cover
      real(4) kMigrHelo           	! [     0.00001]	day-1               	vegetation_migration_rate
      real(4) cDHeloIn            	! [         0.1]	gDW/m2              	external_vegetation_density
      real(4) cTmInitHelo         	! [          10]	oC                  	temperature_for_start_of_growing_season
      real(4) cDCarrHelo          	! [        2000]	gDW/m2              	maximum_vegetation_biomass
      real(4) cMuMaxHelo          	! [         0.1]	gDW/gDW_Shoot/day   	maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10ProdHelo        	! [         1.5]	-                   	temperature_quotient_of_production
      real(4) hLRefHelo           	! [          25]	W/m2_PAR            	half-sat_light_intensity_at_20_oC
      real(4) cExtSpHelo          	! [        0.01]	m2/gDW              	specific_ligth_extinction
      real(4) kDRespHelo          	! [        0.01]	day-1               	dark_respiration_rate_of_vegetation
      real(4) cQ10RespHelo        	! [           2]	-                   	temperature_quotient_of_respiration
      real(4) cDayWinHelo         	! [         259]	day                 	end_of_growing_season_(259_=_16_Sep)
      real(4) kMortHeloSum        	! [       0.005]	day-1               	vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinHelo            	! [       0.333]	-                   	fraction_of_vegetation_DW_surviving_in_winter
      real(4) fDetWMortHelo       	! [        0.25]	-                   	fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrefHeloBird       	! [           0]	-                   	edibility_for_birds
      real(4) cVPUptMaxHelo       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_vegetation_at_20_oC
      real(4) cAffPUptHelo        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_vegetation
      real(4) cPDHeloMin          	! [       0.001]	gP/gDW              	minimum_P/D_ratio_vegetation
      real(4) cPDHeloMax          	! [      0.0075]	gP/gDW              	maximum_P/D_ratio_vegetation
      real(4) cVNUptMaxHelo       	! [         0.1]	gN/gDW/day          	maximum_N_uptake_capacity_of_vegetation_at_20oC
      real(4) cAffNUptHelo        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_vegetation
      real(4) cNDHeloMin          	! [        0.01]	gN/gDW              	minimum_N/D_ratio_vegetation
      real(4) cNDHeloMax          	! [        0.03]	gN/gDW              	maximum_N/D_ratio_vegetation
      real(4) cPACoefMin          	! [         1.5]	-                   	minimum_Poole-Atkins_coefficient
      real(4) cPACoefMax          	! [         2.5]	-                   	maximum_Poole-Atkins_coefficient
      real(4) hPACoef             	! [           3]	g/m2                	decrease_constant_for_PA_coefficient_with_DOMW
      real(4) cSecchiPlus         	! [           0]	m                   	maximum_Secchi_depth_above_water_depth
      real(4) cEuph               	! [         1.7]	-                   	constant_to_convert_Secchi_depth_to_euphotic_depth
      real(4) cCovSpPhyt          	! [           2]	%/gDW/m2            	specific_coverage
      real(4) cTmOptLoss          	! [          25]	oC                  	optimum_tempearture_for_grazing
      real(4) cSigTmLoss          	! [          13]	oC                  	temperature_constant_of_grazing(sigma_in_Gaussian_curve)
      real(4) fDissMortPhyt       	! [         0.2]	-                   	soluble_nutrient_fraction_of_died_phytoplankton
      real(4) fDissLoss           	! [        0.25]	-                   	dissolved_nutrient_fraction_of_grazing_loss
      real(4) cMuMaxPhyt          	! [         1.9]	day-1               	maximum_growth_rate_phytoplankton
      real(4) cTmOptPhyt          	! [          25]	oC                  	optimum_temperature_of_phytoplankton
      real(4) cSigTmPhyt          	! [          15]	oC                  	temperature_constant_phytoplankton(sigma_in_Gaussian_curve)
      real(4) UseSteelePhyt       	! [           0]	-                   	if_1_use_Steele_function_(light_inhibition)_if_use_Lehman_function_(no_light_inhibition)
      real(4) hLRefPhyt           	! [        10.2]	W/m2                	half-sat_PAR_for_phytoplankton_at_20_oC(Lehman_function)
      real(4) cLOptRefPhyt        	! [        1000]	W/m2                	optimum_PAR_at_20_oC_(Steele_function)
      real(4) cChDPhytMin         	! [        0.01]	gChl/gDW            	minimum_chlorophyll/C_ratio_phytoplankton
      real(4) cChDPhytMax         	! [        0.02]	gChl/gDW            	maximum_chlorophyll/C_ratio_phytoplankton
      real(4) kDRespPhyt          	! [         0.1]	day-1               	maintenance_respiration_constant_phytoplankton
      real(4) kLossPhyt           	! [           0]	-                   	grazing_loss_rate_for_phytoplankton
      real(4) kMortPhytW          	! [        0.01]	day-1               	mortality_constant_of_phytoplankton_in_water
      real(4) cVSetPhyt           	! [         0.1]	m/day               	sedimentation_velocity_phytoplankton
      real(4) kMortPhytS          	! [        0.05]	day-1               	mortality_constant_sed_phytoplankton
      real(4) cVPUptMaxPhyt       	! [        0.01]	gP/gDW/day          	maximum_P_uptake_capacity_of_phytoplankton_at_20_oC
      real(4) cAffPUptPhyt        	! [         0.2]	m3/gDW              	initial_P_uptake_affinity_phytoplankton
      real(4) cPDPhytMin          	! [       0.002]	gP/gDW              	minimum_P/D_ratio_phytoplankton
      real(4) cPDPhytMax          	! [       0.015]	gP/gDW              	maximum_P/D_ratio_phytoplankton
      real(4) cVNUptMaxPhyt       	! [        0.07]	gN/gDW/day          	maximum_N_uptake_capacity_of_phytoplankton_at_20_oC
      real(4) cAffNUptPhyt        	! [         0.2]	m3/gDW              	initial_N_uptake_affinity_phytoplankton
      real(4) cNDPhytMin          	! [        0.02]	gN/gDW              	minimum_N/D_ratio_phytoplankton
      real(4) cNDPhytMax          	! [         0.1]	gN/gDW              	maximum_N/D_ratio_phytoplankton
      real(4) O2PerNO3            	! [         1.5]	-                   	mol_O2_formed_per_mol_NO3-_ammonified
      real(4) cDayApril1          	! [          91]	day                 	April_1
      real(4) cDayOct1            	! [         273]	day                 	October_1
      real(4) cLengChange         	! [          10]	day                 	length_of_season_change
      real(4) DaysPerYear         	! [         365]	day/y               	days_per_year
      real(4) TenDays             	! [          10]	day                 	ten_days
      real(4) SecsPerDay          	! [       86400]	s/day               	secs_per_day
      real(4) mmPerm              	! [        1000]	mm/m                	mm_per_m
      real(4) m2Perha             	! [       10000]	m2/ha               	m2_per_ha
      real(4) mgPerg              	! [        1000]	mg/g                	mg_per_g
      real(4) PerCent             	! [        0.01]	%                   	per_cent
      real(4) NearZero            	! [       1E-28]	-                   	very_small_number_used_to_avoid_dividing_by_zero
      real(4) molO2molC           	! [      2.6667]	gO2/gC              	ratio_of_molar_weights
      real(4) molO2molN           	! [      2.2857]	gO2/gN              	ratio_of_molar_weights
      real(4) molNmolC            	! [      1.1667]	gN/gC               	ratio_of_molar_weights
      real(4) cRhoWat             	! [     1000000]	g/m3                	density_of_water
      real(4) Pi                  	! [    3.14E+00]	-                   	Pi
!
!
!     /* ==============================  */
!     /* declaration auxiliaries         */
!     /* ==============================  */
      real(4) sDepthW              	! Water depth
      real(4) bPorS               	! bPorS
      real(4) bPorCorS            	! bPorCorS
      real(4) sTime               	! sTime
      real(4) TimeYears           	! TimeYears
      real(4) Day                 	! Day
      real(4) uTm                 	! uTm
      real(4) uVWind              	! uVWind
      real(4) ufDay               	! ufDay
      real(4) uLDay               	! uLDay
      real(4) uLOut               	! uLOut
      real(4) uLPAR0              	! uLPAR0
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
      real(4) rPDHumS             	! rPDHumS
      real(4) rNDHumS             	! rNDHumS
      real(4) rPDDetS             	! rPDDetS
      real(4) rNDDetS             	! rNDDetS
      real(4) uPLoadSeason        	! uPLoadSeason
      real(4) uPLoad              	! uPLoad
      real(4) uPLoadPO4           	! uPLoadPO4
      real(4) uPLoadOrg           	! uPLoadOrg
      real(4) uPLoadPhytTot       	! uPLoadPhytTot
      real(4) uPLoadDet           	! uPLoadDet
      real(4) uPLoadAIM           	! uPLoadAIM
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
      real(4) wDDilPhyt           	! wDDilPhyt
      real(4) wPDilPhyt           	! wPDilPhyt
      real(4) wNDilPhyt           	! wNDilPhyt
      real(4) wDOutflTot          	! wDOutflTot
      real(4) wPOutflTot          	! wPOutflTot
      real(4) wNOutflTot          	! wNOutflTot
      real(4) uDLoadPhyt          	! uDLoadPhyt
      real(4) uPLoadPhyt          	! uPLoadPhyt
      real(4) uNLoadPhyt          	! uNLoadPhyt
      real(4) wDTranPhyt          	! wDTranPhyt
      real(4) wPTranPhyt          	! wPTranPhyt
      real(4) wNTranPhyt          	! wNTranPhyt
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
      real(4) tDTurbFish          	! tDTurbFish
      real(4) tDTurbFishIM        	! tDTurbFishIM
      real(4) aDVeg               	! aDVeg
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
      real(4) kPMinDetW           	! kPMinDetW
      real(4) kNMinDetW           	! kNMinDetW
      real(4) uFunTmMinW          	! uFunTmMinW
      real(4) wDMinDetW           	! wDMinDetW
      real(4) wPMinDetW           	! wPMinDetW
      real(4) wNMinDetW           	! wNMinDetW
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
      real(4) uFunTmMinS          	! uFunTmMinS
      real(4) tDMinDetS           	! tDMinDetS
      real(4) tPMinDetS           	! tPMinDetS
      real(4) tNMinDetS           	! tNMinDetS
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
      real(4) fManElod            	! fManElod
      real(4) fManCera            	! fManCera
      real(4) fManChar            	! fManChar
      real(4) fManNymp            	! fManNymp
      real(4) kMigrLemn           	! kMigrLemn
      real(4), save :: aDayInitElod
      real(4) bfRootElod          	! bfRootElod
      real(4) bfShootElod         	! bfShootElod
      real(4) aDRootElod          	! aDRootElod
      real(4) aDShootElod         	! aDShootElod
      real(4) aDEmergElod         	! aDEmergElod
      real(4) aDFloatElod         	! aDFloatElod
      real(4) bfSubElod           	! bfSubElod
      real(4) aDSubElod           	! aDSubElod
      real(4) aExtElod            	! aExtElod
      real(4) aDepth1Elod         	! aDepth1Elod
      real(4) aDepth2Elod         	! aDepth2Elod
      real(4) afCovSurfElod       	! afCovSurfElod
      real(4) afCovEmergElod      	! afCovEmergElod
      real(4) aCovElod            	! aCovElod
      real(4), save :: aDayInitChar
      real(4) bfRootChar          	! bfRootChar
      real(4) bfShootChar         	! bfShootChar
      real(4) aDRootChar          	! aDRootChar
      real(4) aDShootChar         	! aDShootChar
      real(4) aDEmergChar         	! aDEmergChar
      real(4) aDFloatChar         	! aDFloatChar
      real(4) bfSubChar           	! bfSubChar
      real(4) aDSubChar           	! aDSubChar
      real(4) aExtChar            	! aExtChar
      real(4) aDepth1Char         	! aDepth1Char
      real(4) aDepth2Char         	! aDepth2Char
      real(4) afCovSurfChar       	! afCovSurfChar
      real(4) afCovEmergChar      	! afCovEmergChar
      real(4) aCovChar            	! aCovChar
      real(4), save :: aDayInitCera
      real(4) bfRootCera          	! bfRootCera
      real(4) bfShootCera         	! bfShootCera
      real(4) aDRootCera          	! aDRootCera
      real(4) aDShootCera         	! aDShootCera
      real(4) aDEmergCera         	! aDEmergCera
      real(4) aDFloatCera         	! aDFloatCera
      real(4) bfSubCera           	! bfSubCera
      real(4) aDSubCera           	! aDSubCera
      real(4) aExtCera            	! aExtCera
      real(4) aDepth1Cera         	! aDepth1Cera
      real(4) aDepth2Cera         	! aDepth2Cera
      real(4) afCovSurfCera       	! afCovSurfCera
      real(4) afCovEmergCera      	! afCovEmergCera
      real(4) aCovCera            	! aCovCera
      real(4), save :: aDayInitLemn
      real(4) bfRootLemn          	! bfRootLemn
      real(4) bfShootLemn         	! bfShootLemn
      real(4) aDRootLemn          	! aDRootLemn
      real(4) aDShootLemn         	! aDShootLemn
      real(4) aDEmergLemn         	! aDEmergLemn
      real(4) aDFloatLemn         	! aDFloatLemn
      real(4) bfSubLemn           	! bfSubLemn
      real(4) aDSubLemn           	! aDSubLemn
      real(4) aExtLemn            	! aExtLemn
      real(4) aDepth1Lemn         	! aDepth1Lemn
      real(4) aDepth2Lemn         	! aDepth2Lemn
      real(4) afCovSurfLemn       	! afCovSurfLemn
      real(4) afCovEmergLemn      	! afCovEmergLemn
      real(4) aCovLemn            	! aCovLemn
      real(4), save :: aDayInitNymp
      real(4) bfRootNymp          	! bfRootNymp
      real(4) bfShootNymp         	! bfShootNymp
      real(4) aDRootNymp          	! aDRootNymp
      real(4) aDShootNymp         	! aDShootNymp
      real(4) aDEmergNymp         	! aDEmergNymp
      real(4) aDFloatNymp         	! aDFloatNymp
      real(4) bfSubNymp           	! bfSubNymp
      real(4) aDSubNymp           	! aDSubNymp
      real(4) aExtNymp            	! aExtNymp
      real(4) aDepth1Nymp         	! aDepth1Nymp
      real(4) aDepth2Nymp         	! aDepth2Nymp
      real(4) afCovSurfNymp       	! afCovSurfNymp
      real(4) afCovEmergNymp      	! afCovEmergNymp
      real(4) aCovNymp            	! aCovNymp
      real(4), save :: aDayInitHelo
      real(4) bfRootHelo          	! bfRootHelo
      real(4) bfShootHelo         	! bfShootHelo
      real(4) aDRootHelo          	! aDRootHelo
      real(4) aDShootHelo         	! aDShootHelo
      real(4) aDEmergHelo         	! aDEmergHelo
      real(4) aDFloatHelo         	! aDFloatHelo
      real(4) bfSubHelo           	! bfSubHelo
      real(4) aDSubHelo           	! aDSubHelo
      real(4) aExtHelo            	! aExtHelo
      real(4) aDepth1Helo         	! aDepth1Helo
      real(4) aDepth2Helo         	! aDepth2Helo
      real(4) afCovSurfHelo       	! afCovSurfHelo
      real(4) afCovEmergHelo      	! afCovEmergHelo
      real(4) aCovHelo            	! aCovHelo
      real(4) aPVeg               	! aPVeg
      real(4) aNVeg               	! aNVeg
      real(4) afCovSurfVeg        	! afCovSurfVeg
      real(4) afCovEmergVeg       	! afCovEmergVeg
      real(4) aExtVeg             	! aExtVeg
      real(4) aExtCoef            	! aExtCoef
      real(4) aLPARBot            	! aLPARBot
      real(4) rPDElod             	! rPDElod
      real(4) rNDElod             	! rNDElod
      real(4) tDMigrElod          	! tDMigrElod
      real(4) tPMigrElod          	! tPMigrElod
      real(4) tNMigrElod          	! tNMigrElod
      real(4) uFunTmProdElod      	! uFunTmProdElod
      real(4) uFunTmRespElod      	! uFunTmRespElod
      real(4) aVPUptMaxCrElod     	! aVPUptMaxCrElod
      real(4) aVPUptElodW         	! aVPUptElodW
      real(4) tPUptElodW          	! tPUptElodW
      real(4) aVPUptElodS         	! aVPUptElodS
      real(4) tPUptElodS          	! tPUptElodS
      real(4) tPUptElod           	! tPUptElod
      real(4) aVNUptMaxCrElod     	! aVNUptMaxCrElod
      real(4) ahNUptElod          	! ahNUptElod
      real(4) aVNUptElodW         	! aVNUptElodW
      real(4) tNUptElodW          	! tNUptElodW
      real(4) afNH4UptElodW       	! afNH4UptElodW
      real(4) tNUptNH4ElodW       	! tNUptNH4ElodW
      real(4) tNUptNO3ElodW       	! tNUptNO3ElodW
      real(4) aVNUptElodS         	! aVNUptElodS
      real(4) tNUptElodS          	! tNUptElodS
      real(4) afNH4UptElodS       	! afNH4UptElodS
      real(4) tNUptNH4ElodS       	! tNUptNH4ElodS
      real(4) tNUptNO3ElodS       	! tNUptNO3ElodS
      real(4) tNUptElod           	! tNUptElod
      real(4) aLPAR1Elod          	! aLPAR1Elod
      real(4) aLPAR2Elod          	! aLPAR2Elod
      real(4) uhLElod             	! uhLElod
      real(4) aLLimShootElod      	! aLLimShootElod
      real(4) aMuTmLElod          	! aMuTmLElod
      real(4) aPLimElod           	! aPLimElod
      real(4) aNLimElod           	! aNLimElod
      real(4) aNutLimElod         	! aNutLimElod
      real(4) aMuElod             	! aMuElod
      real(4) bkMortElod          	! bkMortElod
      real(4) akDIncrElod         	! akDIncrElod
      real(4) tDEnvElod           	! tDEnvElod
      real(4) tDEnvProdElod       	! tDEnvProdElod
      real(4) tDProdElod          	! tDProdElod
      real(4) tDProdSubElod       	! tDProdSubElod
      real(4) tDRespElod          	! tDRespElod
      real(4) tDEnvMortElod       	! tDEnvMortElod
      real(4) tDMortElod          	! tDMortElod
      real(4) tDMortElodW         	! tDMortElodW
      real(4) tDMortElodS         	! tDMortElodS
      real(4) tDGrazElodBird      	! tDGrazElodBird
      real(4) bkManElod           	! bkManElod
      real(4) tDManElod           	! tDManElod
      real(4) tPManElod           	! tPManElod
      real(4) tNManElod           	! tNManElod
      real(4) tDBedElod           	! tDBedElod
      real(4) tO2ProdElod         	! tO2ProdElod
      real(4) tO2RespElodW        	! tO2RespElodW
      real(4) tO2RespElodS        	! tO2RespElodS
      real(4) tO2ProdElodS        	! tO2ProdElodS
      real(4) tO2ProdElodW        	! tO2ProdElodW
      real(4) tO2UptNO3ElodW      	! tO2UptNO3ElodW
      real(4) tO2UptNO3ElodS      	! tO2UptNO3ElodS
      real(4) tPExcrElod          	! tPExcrElod
      real(4) tPExcrElodS         	! tPExcrElodS
      real(4) tPExcrElodW         	! tPExcrElodW
      real(4) tPMortElod          	! tPMortElod
      real(4) tPMortElodPO4       	! tPMortElodPO4
      real(4) tPMortElodPO4S      	! tPMortElodPO4S
      real(4) tPMortElodPO4W      	! tPMortElodPO4W
      real(4) tPMortElodDet       	! tPMortElodDet
      real(4) tPMortElodDetW      	! tPMortElodDetW
      real(4) tPMortElodDetS      	! tPMortElodDetS
      real(4) tPGrazElodBird      	! tPGrazElodBird
      real(4) tPBedElod           	! tPBedElod
      real(4) tNExcrElod          	! tNExcrElod
      real(4) tNExcrElodS         	! tNExcrElodS
      real(4) tNExcrElodW         	! tNExcrElodW
      real(4) tNMortElod          	! tNMortElod
      real(4) tNMortElodNH4       	! tNMortElodNH4
      real(4) tNMortElodNH4S      	! tNMortElodNH4S
      real(4) tNMortElodNH4W      	! tNMortElodNH4W
      real(4) tNMortElodDet       	! tNMortElodDet
      real(4) tNMortElodDetW      	! tNMortElodDetW
      real(4) tNMortElodDetS      	! tNMortElodDetS
      real(4) tNGrazElodBird      	! tNGrazElodBird
      real(4) tNBedElod           	! tNBedElod
      real(4) rPDChar             	! rPDChar
      real(4) rNDChar             	! rNDChar
      real(4) tDMigrChar          	! tDMigrChar
      real(4) tPMigrChar          	! tPMigrChar
      real(4) tNMigrChar          	! tNMigrChar
      real(4) uFunTmProdChar      	! uFunTmProdChar
      real(4) uFunTmRespChar      	! uFunTmRespChar
      real(4) aVPUptMaxCrChar     	! aVPUptMaxCrChar
      real(4) aVPUptCharW         	! aVPUptCharW
      real(4) tPUptCharW          	! tPUptCharW
      real(4) aVPUptCharS         	! aVPUptCharS
      real(4) tPUptCharS          	! tPUptCharS
      real(4) tPUptChar           	! tPUptChar
      real(4) aVNUptMaxCrChar     	! aVNUptMaxCrChar
      real(4) ahNUptChar          	! ahNUptChar
      real(4) aVNUptCharW         	! aVNUptCharW
      real(4) tNUptCharW          	! tNUptCharW
      real(4) afNH4UptCharW       	! afNH4UptCharW
      real(4) tNUptNH4CharW       	! tNUptNH4CharW
      real(4) tNUptNO3CharW       	! tNUptNO3CharW
      real(4) aVNUptCharS         	! aVNUptCharS
      real(4) tNUptCharS          	! tNUptCharS
      real(4) afNH4UptCharS       	! afNH4UptCharS
      real(4) tNUptNH4CharS       	! tNUptNH4CharS
      real(4) tNUptNO3CharS       	! tNUptNO3CharS
      real(4) tNUptChar           	! tNUptChar
      real(4) aLPAR1Char          	! aLPAR1Char
      real(4) aLPAR2Char          	! aLPAR2Char
      real(4) uhLChar             	! uhLChar
      real(4) aLLimShootChar      	! aLLimShootChar
      real(4) aMuTmLChar          	! aMuTmLChar
      real(4) aPLimChar           	! aPLimChar
      real(4) aNLimChar           	! aNLimChar
      real(4) aNutLimChar         	! aNutLimChar
      real(4) aMuChar             	! aMuChar
      real(4) bkMortChar          	! bkMortChar
      real(4) akDIncrChar         	! akDIncrChar
      real(4) tDEnvChar           	! tDEnvChar
      real(4) tDEnvProdChar       	! tDEnvProdChar
      real(4) tDProdChar          	! tDProdChar
      real(4) tDProdSubChar       	! tDProdSubChar
      real(4) tDRespChar          	! tDRespChar
      real(4) tDEnvMortChar       	! tDEnvMortChar
      real(4) tDMortChar          	! tDMortChar
      real(4) tDMortCharW         	! tDMortCharW
      real(4) tDMortCharS         	! tDMortCharS
      real(4) tDGrazCharBird      	! tDGrazCharBird
      real(4) bkManChar           	! bkManChar
      real(4) tDManChar           	! tDManChar
      real(4) tPManChar           	! tPManChar
      real(4) tNManChar           	! tNManChar
      real(4) tDBedChar           	! tDBedChar
      real(4) tO2ProdChar         	! tO2ProdChar
      real(4) tO2RespCharW        	! tO2RespCharW
      real(4) tO2RespCharS        	! tO2RespCharS
      real(4) tO2ProdCharS        	! tO2ProdCharS
      real(4) tO2ProdCharW        	! tO2ProdCharW
      real(4) tO2UptNO3CharW      	! tO2UptNO3CharW
      real(4) tO2UptNO3CharS      	! tO2UptNO3CharS
      real(4) tPExcrChar          	! tPExcrChar
      real(4) tPExcrCharS         	! tPExcrCharS
      real(4) tPExcrCharW         	! tPExcrCharW
      real(4) tPMortChar          	! tPMortChar
      real(4) tPMortCharPO4       	! tPMortCharPO4
      real(4) tPMortCharPO4S      	! tPMortCharPO4S
      real(4) tPMortCharPO4W      	! tPMortCharPO4W
      real(4) tPMortCharDet       	! tPMortCharDet
      real(4) tPMortCharDetW      	! tPMortCharDetW
      real(4) tPMortCharDetS      	! tPMortCharDetS
      real(4) tPGrazCharBird      	! tPGrazCharBird
      real(4) tPBedChar           	! tPBedChar
      real(4) tNExcrChar          	! tNExcrChar
      real(4) tNExcrCharS         	! tNExcrCharS
      real(4) tNExcrCharW         	! tNExcrCharW
      real(4) tNMortChar          	! tNMortChar
      real(4) tNMortCharNH4       	! tNMortCharNH4
      real(4) tNMortCharNH4S      	! tNMortCharNH4S
      real(4) tNMortCharNH4W      	! tNMortCharNH4W
      real(4) tNMortCharDet       	! tNMortCharDet
      real(4) tNMortCharDetW      	! tNMortCharDetW
      real(4) tNMortCharDetS      	! tNMortCharDetS
      real(4) tNGrazCharBird      	! tNGrazCharBird
      real(4) tNBedChar           	! tNBedChar
      real(4) rPDCera             	! rPDCera
      real(4) rNDCera             	! rNDCera
      real(4) tDMigrCera          	! tDMigrCera
      real(4) tPMigrCera          	! tPMigrCera
      real(4) tNMigrCera          	! tNMigrCera
      real(4) uFunTmProdCera      	! uFunTmProdCera
      real(4) uFunTmRespCera      	! uFunTmRespCera
      real(4) aVPUptMaxCrCera     	! aVPUptMaxCrCera
      real(4) aVPUptCeraW         	! aVPUptCeraW
      real(4) tPUptCeraW          	! tPUptCeraW
      real(4) aVPUptCeraS         	! aVPUptCeraS
      real(4) tPUptCeraS          	! tPUptCeraS
      real(4) tPUptCera           	! tPUptCera
      real(4) aVNUptMaxCrCera     	! aVNUptMaxCrCera
      real(4) ahNUptCera          	! ahNUptCera
      real(4) aVNUptCeraW         	! aVNUptCeraW
      real(4) tNUptCeraW          	! tNUptCeraW
      real(4) afNH4UptCeraW       	! afNH4UptCeraW
      real(4) tNUptNH4CeraW       	! tNUptNH4CeraW
      real(4) tNUptNO3CeraW       	! tNUptNO3CeraW
      real(4) aVNUptCeraS         	! aVNUptCeraS
      real(4) tNUptCeraS          	! tNUptCeraS
      real(4) afNH4UptCeraS       	! afNH4UptCeraS
      real(4) tNUptNH4CeraS       	! tNUptNH4CeraS
      real(4) tNUptNO3CeraS       	! tNUptNO3CeraS
      real(4) tNUptCera           	! tNUptCera
      real(4) aLPAR1Cera          	! aLPAR1Cera
      real(4) aLPAR2Cera          	! aLPAR2Cera
      real(4) uhLCera             	! uhLCera
      real(4) aLLimShootCera      	! aLLimShootCera
      real(4) aMuTmLCera          	! aMuTmLCera
      real(4) aPLimCera           	! aPLimCera
      real(4) aNLimCera           	! aNLimCera
      real(4) aNutLimCera         	! aNutLimCera
      real(4) aMuCera             	! aMuCera
      real(4) bkMortCera          	! bkMortCera
      real(4) akDIncrCera         	! akDIncrCera
      real(4) tDEnvCera           	! tDEnvCera
      real(4) tDEnvProdCera       	! tDEnvProdCera
      real(4) tDProdCera          	! tDProdCera
      real(4) tDProdSubCera       	! tDProdSubCera
      real(4) tDRespCera          	! tDRespCera
      real(4) tDEnvMortCera       	! tDEnvMortCera
      real(4) tDMortCera          	! tDMortCera
      real(4) tDMortCeraW         	! tDMortCeraW
      real(4) tDMortCeraS         	! tDMortCeraS
      real(4) tDGrazCeraBird      	! tDGrazCeraBird
      real(4) bkManCera           	! bkManCera
      real(4) tDManCera           	! tDManCera
      real(4) tPManCera           	! tPManCera
      real(4) tNManCera           	! tNManCera
      real(4) tDBedCera           	! tDBedCera
      real(4) tO2ProdCera         	! tO2ProdCera
      real(4) tO2RespCeraW        	! tO2RespCeraW
      real(4) tO2RespCeraS        	! tO2RespCeraS
      real(4) tO2ProdCeraS        	! tO2ProdCeraS
      real(4) tO2ProdCeraW        	! tO2ProdCeraW
      real(4) tO2UptNO3CeraW      	! tO2UptNO3CeraW
      real(4) tO2UptNO3CeraS      	! tO2UptNO3CeraS
      real(4) tPExcrCera          	! tPExcrCera
      real(4) tPExcrCeraS         	! tPExcrCeraS
      real(4) tPExcrCeraW         	! tPExcrCeraW
      real(4) tPMortCera          	! tPMortCera
      real(4) tPMortCeraPO4       	! tPMortCeraPO4
      real(4) tPMortCeraPO4S      	! tPMortCeraPO4S
      real(4) tPMortCeraPO4W      	! tPMortCeraPO4W
      real(4) tPMortCeraDet       	! tPMortCeraDet
      real(4) tPMortCeraDetW      	! tPMortCeraDetW
      real(4) tPMortCeraDetS      	! tPMortCeraDetS
      real(4) tPGrazCeraBird      	! tPGrazCeraBird
      real(4) tPBedCera           	! tPBedCera
      real(4) tNExcrCera          	! tNExcrCera
      real(4) tNExcrCeraS         	! tNExcrCeraS
      real(4) tNExcrCeraW         	! tNExcrCeraW
      real(4) tNMortCera          	! tNMortCera
      real(4) tNMortCeraNH4       	! tNMortCeraNH4
      real(4) tNMortCeraNH4S      	! tNMortCeraNH4S
      real(4) tNMortCeraNH4W      	! tNMortCeraNH4W
      real(4) tNMortCeraDet       	! tNMortCeraDet
      real(4) tNMortCeraDetW      	! tNMortCeraDetW
      real(4) tNMortCeraDetS      	! tNMortCeraDetS
      real(4) tNGrazCeraBird      	! tNGrazCeraBird
      real(4) tNBedCera           	! tNBedCera
      real(4) rPDLemn             	! rPDLemn
      real(4) rNDLemn             	! rNDLemn
      real(4) tDMigrLemn          	! tDMigrLemn
      real(4) tPMigrLemn          	! tPMigrLemn
      real(4) tNMigrLemn          	! tNMigrLemn
      real(4) uFunTmProdLemn      	! uFunTmProdLemn
      real(4) uFunTmRespLemn      	! uFunTmRespLemn
      real(4) aVPUptMaxCrLemn     	! aVPUptMaxCrLemn
      real(4) aVPUptLemnW         	! aVPUptLemnW
      real(4) tPUptLemnW          	! tPUptLemnW
      real(4) aVPUptLemnS         	! aVPUptLemnS
      real(4) tPUptLemnS          	! tPUptLemnS
      real(4) tPUptLemn           	! tPUptLemn
      real(4) aVNUptMaxCrLemn     	! aVNUptMaxCrLemn
      real(4) ahNUptLemn          	! ahNUptLemn
      real(4) aVNUptLemnW         	! aVNUptLemnW
      real(4) tNUptLemnW          	! tNUptLemnW
      real(4) afNH4UptLemnW       	! afNH4UptLemnW
      real(4) tNUptNH4LemnW       	! tNUptNH4LemnW
      real(4) tNUptNO3LemnW       	! tNUptNO3LemnW
      real(4) aVNUptLemnS         	! aVNUptLemnS
      real(4) tNUptLemnS          	! tNUptLemnS
      real(4) afNH4UptLemnS       	! afNH4UptLemnS
      real(4) tNUptNH4LemnS       	! tNUptNH4LemnS
      real(4) tNUptNO3LemnS       	! tNUptNO3LemnS
      real(4) tNUptLemn           	! tNUptLemn
      real(4) aLPAR1Lemn          	! aLPAR1Lemn
      real(4) aLPAR2Lemn          	! aLPAR2Lemn
      real(4) uhLLemn             	! uhLLemn
      real(4) aLLimShootLemn      	! aLLimShootLemn
      real(4) aMuTmLLemn          	! aMuTmLLemn
      real(4) aPLimLemn           	! aPLimLemn
      real(4) aNLimLemn           	! aNLimLemn
      real(4) aNutLimLemn         	! aNutLimLemn
      real(4) aMuLemn             	! aMuLemn
      real(4) bkMortLemn          	! bkMortLemn
      real(4) akDIncrLemn         	! akDIncrLemn
      real(4) tDEnvLemn           	! tDEnvLemn
      real(4) tDEnvProdLemn       	! tDEnvProdLemn
      real(4) tDProdLemn          	! tDProdLemn
      real(4) tDProdSubLemn       	! tDProdSubLemn
      real(4) tDRespLemn          	! tDRespLemn
      real(4) tDEnvMortLemn       	! tDEnvMortLemn
      real(4) tDMortLemn          	! tDMortLemn
      real(4) tDMortLemnW         	! tDMortLemnW
      real(4) tDMortLemnS         	! tDMortLemnS
      real(4) tDGrazLemnBird      	! tDGrazLemnBird
      real(4) bkManLemn           	! bkManLemn
      real(4) tDManLemn           	! tDManLemn
      real(4) tPManLemn           	! tPManLemn
      real(4) tNManLemn           	! tNManLemn
      real(4) tDBedLemn           	! tDBedLemn
      real(4) tO2ProdLemn         	! tO2ProdLemn
      real(4) tO2RespLemnW        	! tO2RespLemnW
      real(4) tO2RespLemnS        	! tO2RespLemnS
      real(4) tO2ProdLemnS        	! tO2ProdLemnS
      real(4) tO2ProdLemnW        	! tO2ProdLemnW
      real(4) tO2UptNO3LemnW      	! tO2UptNO3LemnW
      real(4) tO2UptNO3LemnS      	! tO2UptNO3LemnS
      real(4) tPExcrLemn          	! tPExcrLemn
      real(4) tPExcrLemnS         	! tPExcrLemnS
      real(4) tPExcrLemnW         	! tPExcrLemnW
      real(4) tPMortLemn          	! tPMortLemn
      real(4) tPMortLemnPO4       	! tPMortLemnPO4
      real(4) tPMortLemnPO4S      	! tPMortLemnPO4S
      real(4) tPMortLemnPO4W      	! tPMortLemnPO4W
      real(4) tPMortLemnDet       	! tPMortLemnDet
      real(4) tPMortLemnDetW      	! tPMortLemnDetW
      real(4) tPMortLemnDetS      	! tPMortLemnDetS
      real(4) tPGrazLemnBird      	! tPGrazLemnBird
      real(4) tPBedLemn           	! tPBedLemn
      real(4) tNExcrLemn          	! tNExcrLemn
      real(4) tNExcrLemnS         	! tNExcrLemnS
      real(4) tNExcrLemnW         	! tNExcrLemnW
      real(4) tNMortLemn          	! tNMortLemn
      real(4) tNMortLemnNH4       	! tNMortLemnNH4
      real(4) tNMortLemnNH4S      	! tNMortLemnNH4S
      real(4) tNMortLemnNH4W      	! tNMortLemnNH4W
      real(4) tNMortLemnDet       	! tNMortLemnDet
      real(4) tNMortLemnDetW      	! tNMortLemnDetW
      real(4) tNMortLemnDetS      	! tNMortLemnDetS
      real(4) tNGrazLemnBird      	! tNGrazLemnBird
      real(4) tNBedLemn           	! tNBedLemn
      real(4) rPDNymp             	! rPDNymp
      real(4) rNDNymp             	! rNDNymp
      real(4) tDMigrNymp          	! tDMigrNymp
      real(4) tPMigrNymp          	! tPMigrNymp
      real(4) tNMigrNymp          	! tNMigrNymp
      real(4) uFunTmProdNymp      	! uFunTmProdNymp
      real(4) uFunTmRespNymp      	! uFunTmRespNymp
      real(4) aVPUptMaxCrNymp     	! aVPUptMaxCrNymp
      real(4) aVPUptNympW         	! aVPUptNympW
      real(4) tPUptNympW          	! tPUptNympW
      real(4) aVPUptNympS         	! aVPUptNympS
      real(4) tPUptNympS          	! tPUptNympS
      real(4) tPUptNymp           	! tPUptNymp
      real(4) aVNUptMaxCrNymp     	! aVNUptMaxCrNymp
      real(4) ahNUptNymp          	! ahNUptNymp
      real(4) aVNUptNympW         	! aVNUptNympW
      real(4) tNUptNympW          	! tNUptNympW
      real(4) afNH4UptNympW       	! afNH4UptNympW
      real(4) tNUptNH4NympW       	! tNUptNH4NympW
      real(4) tNUptNO3NympW       	! tNUptNO3NympW
      real(4) aVNUptNympS         	! aVNUptNympS
      real(4) tNUptNympS          	! tNUptNympS
      real(4) afNH4UptNympS       	! afNH4UptNympS
      real(4) tNUptNH4NympS       	! tNUptNH4NympS
      real(4) tNUptNO3NympS       	! tNUptNO3NympS
      real(4) tNUptNymp           	! tNUptNymp
      real(4) aLPAR1Nymp          	! aLPAR1Nymp
      real(4) aLPAR2Nymp          	! aLPAR2Nymp
      real(4) uhLNymp             	! uhLNymp
      real(4) aLLimShootNymp      	! aLLimShootNymp
      real(4) aMuTmLNymp          	! aMuTmLNymp
      real(4) aPLimNymp           	! aPLimNymp
      real(4) aNLimNymp           	! aNLimNymp
      real(4) aNutLimNymp         	! aNutLimNymp
      real(4) aMuNymp             	! aMuNymp
      real(4) bkMortNymp          	! bkMortNymp
      real(4) akDIncrNymp         	! akDIncrNymp
      real(4) tDEnvNymp           	! tDEnvNymp
      real(4) tDEnvProdNymp       	! tDEnvProdNymp
      real(4) tDProdNymp          	! tDProdNymp
      real(4) tDProdSubNymp       	! tDProdSubNymp
      real(4) tDRespNymp          	! tDRespNymp
      real(4) tDEnvMortNymp       	! tDEnvMortNymp
      real(4) tDMortNymp          	! tDMortNymp
      real(4) tDMortNympW         	! tDMortNympW
      real(4) tDMortNympS         	! tDMortNympS
      real(4) tDGrazNympBird      	! tDGrazNympBird
      real(4) bkManNymp           	! bkManNymp
      real(4) tDManNymp           	! tDManNymp
      real(4) tPManNymp           	! tPManNymp
      real(4) tNManNymp           	! tNManNymp
      real(4) tDBedNymp           	! tDBedNymp
      real(4) tO2ProdNymp         	! tO2ProdNymp
      real(4) tO2RespNympW        	! tO2RespNympW
      real(4) tO2RespNympS        	! tO2RespNympS
      real(4) tO2ProdNympS        	! tO2ProdNympS
      real(4) tO2ProdNympW        	! tO2ProdNympW
      real(4) tO2UptNO3NympW      	! tO2UptNO3NympW
      real(4) tO2UptNO3NympS      	! tO2UptNO3NympS
      real(4) tPExcrNymp          	! tPExcrNymp
      real(4) tPExcrNympS         	! tPExcrNympS
      real(4) tPExcrNympW         	! tPExcrNympW
      real(4) tPMortNymp          	! tPMortNymp
      real(4) tPMortNympPO4       	! tPMortNympPO4
      real(4) tPMortNympPO4S      	! tPMortNympPO4S
      real(4) tPMortNympPO4W      	! tPMortNympPO4W
      real(4) tPMortNympDet       	! tPMortNympDet
      real(4) tPMortNympDetW      	! tPMortNympDetW
      real(4) tPMortNympDetS      	! tPMortNympDetS
      real(4) tPGrazNympBird      	! tPGrazNympBird
      real(4) tPBedNymp           	! tPBedNymp
      real(4) tNExcrNymp          	! tNExcrNymp
      real(4) tNExcrNympS         	! tNExcrNympS
      real(4) tNExcrNympW         	! tNExcrNympW
      real(4) tNMortNymp          	! tNMortNymp
      real(4) tNMortNympNH4       	! tNMortNympNH4
      real(4) tNMortNympNH4S      	! tNMortNympNH4S
      real(4) tNMortNympNH4W      	! tNMortNympNH4W
      real(4) tNMortNympDet       	! tNMortNympDet
      real(4) tNMortNympDetW      	! tNMortNympDetW
      real(4) tNMortNympDetS      	! tNMortNympDetS
      real(4) tNGrazNympBird      	! tNGrazNympBird
      real(4) tNBedNymp           	! tNBedNymp
      real(4) rPDHelo             	! rPDHelo
      real(4) rNDHelo             	! rNDHelo
      real(4) tDMigrHelo          	! tDMigrHelo
      real(4) tPMigrHelo          	! tPMigrHelo
      real(4) tNMigrHelo          	! tNMigrHelo
      real(4) uFunTmProdHelo      	! uFunTmProdHelo
      real(4) uFunTmRespHelo      	! uFunTmRespHelo
      real(4) aVPUptMaxCrHelo     	! aVPUptMaxCrHelo
      real(4) aVPUptHeloW         	! aVPUptHeloW
      real(4) tPUptHeloW          	! tPUptHeloW
      real(4) aVPUptHeloS         	! aVPUptHeloS
      real(4) tPUptHeloS          	! tPUptHeloS
      real(4) tPUptHelo           	! tPUptHelo
      real(4) aVNUptMaxCrHelo     	! aVNUptMaxCrHelo
      real(4) ahNUptHelo          	! ahNUptHelo
      real(4) aVNUptHeloW         	! aVNUptHeloW
      real(4) tNUptHeloW          	! tNUptHeloW
      real(4) afNH4UptHeloW       	! afNH4UptHeloW
      real(4) tNUptNH4HeloW       	! tNUptNH4HeloW
      real(4) tNUptNO3HeloW       	! tNUptNO3HeloW
      real(4) aVNUptHeloS         	! aVNUptHeloS
      real(4) tNUptHeloS          	! tNUptHeloS
      real(4) afNH4UptHeloS       	! afNH4UptHeloS
      real(4) tNUptNH4HeloS       	! tNUptNH4HeloS
      real(4) tNUptNO3HeloS       	! tNUptNO3HeloS
      real(4) tNUptHelo           	! tNUptHelo
      real(4) aLPAR1Helo          	! aLPAR1Helo
      real(4) aLPAR2Helo          	! aLPAR2Helo
      real(4) uhLHelo             	! uhLHelo
      real(4) aLLimShootHelo      	! aLLimShootHelo
      real(4) aMuTmLHelo          	! aMuTmLHelo
      real(4) aPLimHelo           	! aPLimHelo
      real(4) aNLimHelo           	! aNLimHelo
      real(4) aNutLimHelo         	! aNutLimHelo
      real(4) aMuHelo             	! aMuHelo
      real(4) bkMortHelo          	! bkMortHelo
      real(4) akDIncrHelo         	! akDIncrHelo
      real(4) tDEnvHelo           	! tDEnvHelo
      real(4) tDEnvProdHelo       	! tDEnvProdHelo
      real(4) tDProdHelo          	! tDProdHelo
      real(4) tDProdSubHelo       	! tDProdSubHelo
      real(4) tDRespHelo          	! tDRespHelo
      real(4) tDEnvMortHelo       	! tDEnvMortHelo
      real(4) tDMortHelo          	! tDMortHelo
      real(4) tDMortHeloW         	! tDMortHeloW
      real(4) tDMortHeloS         	! tDMortHeloS
      real(4) tDGrazHeloBird      	! tDGrazHeloBird
      real(4) bkManHelo           	! bkManHelo
      real(4) tDManHelo           	! tDManHelo
      real(4) tPManHelo           	! tPManHelo
      real(4) tNManHelo           	! tNManHelo
      real(4) tDBedHelo           	! tDBedHelo
      real(4) tO2ProdHelo         	! tO2ProdHelo
      real(4) tO2RespHeloW        	! tO2RespHeloW
      real(4) tO2RespHeloS        	! tO2RespHeloS
      real(4) tO2ProdHeloS        	! tO2ProdHeloS
      real(4) tO2ProdHeloW        	! tO2ProdHeloW
      real(4) tO2UptNO3HeloW      	! tO2UptNO3HeloW
      real(4) tO2UptNO3HeloS      	! tO2UptNO3HeloS
      real(4) tPExcrHelo          	! tPExcrHelo
      real(4) tPExcrHeloS         	! tPExcrHeloS
      real(4) tPExcrHeloW         	! tPExcrHeloW
      real(4) tPMortHelo          	! tPMortHelo
      real(4) tPMortHeloPO4       	! tPMortHeloPO4
      real(4) tPMortHeloPO4S      	! tPMortHeloPO4S
      real(4) tPMortHeloPO4W      	! tPMortHeloPO4W
      real(4) tPMortHeloDet       	! tPMortHeloDet
      real(4) tPMortHeloDetW      	! tPMortHeloDetW
      real(4) tPMortHeloDetS      	! tPMortHeloDetS
      real(4) tPGrazHeloBird      	! tPGrazHeloBird
      real(4) tPBedHelo           	! tPBedHelo
      real(4) tNExcrHelo          	! tNExcrHelo
      real(4) tNExcrHeloS         	! tNExcrHeloS
      real(4) tNExcrHeloW         	! tNExcrHeloW
      real(4) tNMortHelo          	! tNMortHelo
      real(4) tNMortHeloNH4       	! tNMortHeloNH4
      real(4) tNMortHeloNH4S      	! tNMortHeloNH4S
      real(4) tNMortHeloNH4W      	! tNMortHeloNH4W
      real(4) tNMortHeloDet       	! tNMortHeloDet
      real(4) tNMortHeloDetW      	! tNMortHeloDetW
      real(4) tNMortHeloDetS      	! tNMortHeloDetS
      real(4) tNGrazHeloBird      	! tNGrazHeloBird
      real(4) tNBedHelo           	! tNBedHelo
      real(4) tDMigrVeg           	! tDMigrVeg
      real(4) tPMigrVeg           	! tPMigrVeg
      real(4) tNMigrVeg           	! tNMigrVeg
      real(4) tDProdVeg           	! tDProdVeg
      real(4) tPUptVegW           	! tPUptVegW
      real(4) tPUptVegS           	! tPUptVegS
      real(4) tNUptNH4VegW        	! tNUptNH4VegW
      real(4) tNUptNH4VegS        	! tNUptNH4VegS
      real(4) tNUptNO3VegW        	! tNUptNO3VegW
      real(4) tNUptNO3VegS        	! tNUptNO3VegS
      real(4) tDRespVeg           	! tDRespVeg
      real(4) tPExcrVegW          	! tPExcrVegW
      real(4) tPExcrVegS          	! tPExcrVegS
      real(4) tNExcrVegW          	! tNExcrVegW
      real(4) tNExcrVegS          	! tNExcrVegS
      real(4) tO2ProdVeg          	! tO2ProdVeg
      real(4) tO2ProdVegW         	! tO2ProdVegW
      real(4) tO2ProdVegS         	! tO2ProdVegS
      real(4) tO2RespVegW         	! tO2RespVegW
      real(4) tO2RespVegS         	! tO2RespVegS
      real(4) tO2UptNO3VegW       	! tO2UptNO3VegW
      real(4) tO2UptNO3VegS       	! tO2UptNO3VegS
      real(4) tDMortVegW          	! tDMortVegW
      real(4) tDMortVegS          	! tDMortVegS
      real(4) tPMortVegPO4W       	! tPMortVegPO4W
      real(4) tPMortVegDetW       	! tPMortVegDetW
      real(4) tPMortVegPO4S       	! tPMortVegPO4S
      real(4) tPMortVegDetS       	! tPMortVegDetS
      real(4) tNMortVegNH4W       	! tNMortVegNH4W
      real(4) tNMortVegDetW       	! tNMortVegDetW
      real(4) tNMortVegNH4S       	! tNMortVegNH4S
      real(4) tNMortVegDetS       	! tNMortVegDetS
      real(4) tDGrazVegBird       	! tDGrazVegBird
      real(4) tPGrazVegBird       	! tPGrazVegBird
      real(4) tNGrazVegBird       	! tNGrazVegBird
      real(4) tDManVeg            	! tDManVeg
      real(4) tPManVeg            	! tPManVeg
      real(4) tNManVeg            	! tNManVeg
      real(4) aCovSub             	! aCovSub
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
      real(4) rPDPhytW            	! rPDPhytW
      real(4) rNDPhytW            	! rNDPhytW
      real(4) rPDPhytS            	! rPDPhytS
      real(4) rNDPhytS            	! rNDPhytS
      real(4) uFunTmPhyt          	! uFunTmPhyt
      real(4) uFunTmProdPhyt      	! uFunTmProdPhyt
      real(4) uFunTmRespPhyt      	! uFunTmRespPhyt
      real(4) aVPUptMaxCrPhyt     	! aVPUptMaxCrPhyt
      real(4) aVPUptPhyt          	! aVPUptPhyt
      real(4) wPUptPhyt           	! wPUptPhyt
      real(4) aVNUptMaxCrPhyt     	! aVNUptMaxCrPhyt
      real(4) ahNUptPhyt          	! ahNUptPhyt
      real(4) aVNUptPhyt          	! aVNUptPhyt
      real(4) wNUptPhyt           	! wNUptPhyt
      real(4) afNH4UptPhyt        	! afNH4UptPhyt
      real(4) wNUptNH4Phyt        	! wNUptNH4Phyt
      real(4) wNUptNO3Phyt        	! wNUptNO3Phyt
      real(4) uMuMaxTmPhyt        	! uMuMaxTmPhyt
      real(4) aPLimPhyt           	! aPLimPhyt
      real(4) aNLimPhyt           	! aNLimPhyt
      real(4) aLLimPhyt           	! aLLimPhyt
      real(4) aMuTmLPhyt          	! aMuTmLPhyt
      real(4) aNutLimPhyt         	! aNutLimPhyt
      real(4) aMuPhyt             	! aMuPhyt
      real(4) wDAssPhyt           	! wDAssPhyt
      real(4) rChDPhyt            	! rChDPhyt
      real(4) oChlaPhyt           	! oChlaPhyt
      real(4) aExtChPhyt          	! aExtChPhyt
      real(4) ukDRespTmPhyt       	! ukDRespTmPhyt
      real(4) wDRespPhytW         	! wDRespPhytW
      real(4) ukLossTmPhyt        	! ukLossTmPhyt
      real(4) wDLossPhyt          	! wDLossPhyt
      real(4) wDMortPhytW         	! wDMortPhytW
      real(4) uCorVSetPhyt        	! uCorVSetPhyt
      real(4) tDSetPhyt           	! tDSetPhyt
      real(4) tDResusPhyt         	! tDResusPhyt
      real(4) tDRespPhytS         	! tDRespPhytS
      real(4) tDMortPhytS         	! tDMortPhytS
      real(4) ukDDecPhyt          	! ukDDecPhyt
      real(4) wPExcrPhytW         	! wPExcrPhytW
      real(4) wPLossPhyt          	! wPLossPhyt
      real(4) wPMortPhytW         	! wPMortPhytW
      real(4) tPSetPhyt           	! tPSetPhyt
      real(4) tPResusPhyt         	! tPResusPhyt
      real(4) tPExcrPhytS         	! tPExcrPhytS
      real(4) tPMortPhytS         	! tPMortPhytS
      real(4) wNExcrPhytW         	! wNExcrPhytW
      real(4) wNLossPhyt          	! wNLossPhyt
      real(4) wNMortPhytW         	! wNMortPhytW
      real(4) tNSetPhyt           	! tNSetPhyt
      real(4) tNResusPhyt         	! tNResusPhyt
      real(4) tNExcrPhytS         	! tNExcrPhytS
      real(4) tNMortPhytS         	! tNMortPhytS
      real(4) wDPrimPhytW         	! wDPrimPhytW
      real(4) wPPrimPhytW         	! wPPrimPhytW
      real(4) wNPrimPhytW         	! wNPrimPhytW
      real(4) tDPrimPhytS         	! tDPrimPhytS
      real(4) tPPrimPhytS         	! tPPrimPhytS
      real(4) tNPrimPhytS         	! tNPrimPhytS
      real(4) oChla               	! oChla
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
      real(4) aPACoef             	! aPACoef
      real(4) bSecchiMax          	! bSecchiMax
      real(4) aSecchi             	! aSecchi
      real(4) aDepthEuph          	! aDepthEuph
      real(4) aRelDepthEuph       	! aRelDepthEuph
      real(4) aChlaH              	! aChlaH
      real(4) aCovPhytW           	! aCovPhytW
      real(4) rExtChPhyt          	! rExtChPhyt
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
      real(4) akDredLemn          	! akDredLemn
      real(4) vDredDepthW         	! vDredDepthW
      real(4) tDDredDetS          	! tDDredDetS
      real(4) tPDredDetS          	! tPDredDetS
      real(4) tNDredDetS          	! tNDredDetS
      real(4) tPDredAIMS          	! tPDredAIMS
      real(4) bRhoSolidSoil       	! bRhoSolidSoil
      real(4) tDDredNetSoil       	! tDDredNetSoil
      real(4) tDDredNetIMS        	! tDDredNetIMS
      real(4) tDDredNetHumS       	! tDDredNetHumS
      real(4) tPDredNetHumS       	! tPDredNetHumS
      real(4) tNDredNetHumS       	! tNDredNetHumS
      real(4) tDDredPhytS         	! tDDredPhytS
      real(4) tPDredPhytS         	! tPDredPhytS
      real(4) tNDredPhytS         	! tNDredPhytS
      real(4) tDDredElod          	! tDDredElod
      real(4) tPDredElod          	! tPDredElod
      real(4) tNDredElod          	! tNDredElod
      real(4) tDDredCera          	! tDDredCera
      real(4) tPDredCera          	! tPDredCera
      real(4) tNDredCera          	! tNDredCera
      real(4) tDDredChar          	! tDDredChar
      real(4) tPDredChar          	! tPDredChar
      real(4) tNDredChar          	! tNDredChar
      real(4) tDDredLemn          	! tDDredLemn
      real(4) tPDredLemn          	! tPDredLemn
      real(4) tNDredLemn          	! tNDredLemn
      real(4) tDDredNymp          	! tDDredNymp
      real(4) tPDredNymp          	! tPDredNymp
      real(4) tNDredNymp          	! tNDredNymp
      real(4) tDDredHelo          	! tDDredHelo
      real(4) tPDredHelo          	! tPDredHelo
      real(4) tNDredHelo          	! tNDredHelo
      real(4) tDDredVeg           	! tDDredVeg
      real(4) tPDredVeg           	! tPDredVeg
      real(4) tNDredVeg           	! tNDredVeg
      real(4) tDDredNetTot        	! tDDredNetTot
      real(4) tPDredNetTot        	! tPDredNetTot
      real(4) tNDredNetTot        	! tNDredNetTot
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
      real(4) vDeltaW             	! vDeltaW
      real(4) aRelDeltaW          	! aRelDeltaW
      real(4) aDTotT              	! aDTotT
      real(4) aNTotT              	! aNTotT
      real(4) aPTotT              	! aPTotT
      real(4) aDError             	! aDError
      real(4) aNError             	! aNError
      real(4) aPError             	! aPError
      real(4) dPO4W               	! dPO4W
      real(4) dPAIMW              	! dPAIMW
      real(4) dNH4W               	! dNH4W
      real(4) dNO3W               	! dNO3W
      real(4) dO2W                	! dO2W
      real(4) dDIMW               	! dDIMW
      real(4) dDDetW              	! dDDetW
      real(4) dPDetW              	! dPDetW
      real(4) dNDetW              	! dNDetW
      real(4) dDPhytW             	! dDPhytW
      real(4) dPPhytW             	! dPPhytW
      real(4) dNPhytW             	! dNPhytW
      real(4) dPO4S               	! dPO4S
      real(4) dPAIMS              	! dPAIMS
      real(4) dNH4S               	! dNH4S
      real(4) dNO3S               	! dNO3S
      real(4) dDIMS               	! dDIMS
      real(4) dDHumS              	! dDHumS
      real(4) dPHumS              	! dPHumS
      real(4) dNHumS              	! dNHumS
      real(4) dDDetS              	! dDDetS
      real(4) dPDetS              	! dPDetS
      real(4) dNDetS              	! dNDetS
      real(4) dDPhytS             	! dDPhytS
      real(4) dPPhytS             	! dPPhytS
      real(4) dNPhytS             	! dNPhytS
      real(4) dDElod              	! dDElod
      real(4) dPElod              	! dPElod
      real(4) dNElod              	! dNElod
      real(4) dDChar              	! dDChar
      real(4) dPChar              	! dPChar
      real(4) dNChar              	! dNChar
      real(4) dDCera              	! dDCera
      real(4) dPCera              	! dPCera
      real(4) dNCera              	! dNCera
      real(4) dDLemn              	! dDLemn
      real(4) dPLemn              	! dPLemn
      real(4) dNLemn              	! dNLemn
      real(4) dDNymp              	! dDNymp
      real(4) dPNymp              	! dPNymp
      real(4) dNNymp              	! dNNymp
      real(4) dDHelo              	! dDHelo
      real(4) dPHelo              	! dPHelo
      real(4) dNHelo              	! dNHelo
      real(4) dDExtTotT           	! dDExtTotT
      real(4) dNExtTotT           	! dNExtTotT
      real(4) dPExtTotT           	! dPExtTotT
!
!
!     /* ==============================  */
!     /* declaration fluxes              */
!     /* ==============================  */
      real(4) D0sDDetS         	 ! flux of sDDetS
      real(4) D0sDDetW         	 ! flux of sDDetW
      real(4) D0sDHumS         	 ! flux of sDHumS
      real(4) D0sDIMS          	 ! flux of sDIMS
      real(4) D0sDIMW          	 ! flux of sDIMW
      real(4) D0sNDetS         	 ! flux of sNDetS
      real(4) D0sNDetW         	 ! flux of sNDetW
      real(4) D0sNH4S          	 ! flux of sNH4S
      real(4) D0sNH4W          	 ! flux of sNH4W
      real(4) D0sNHumS         	 ! flux of sNHumS
      real(4) D0sNO3S          	 ! flux of sNO3S
      real(4) D0sNO3W          	 ! flux of sNO3W
      real(4) D0sO2W           	 ! flux of sO2W
      real(4) D0sPAIMS         	 ! flux of sPAIMS
      real(4) D0sPAIMW         	 ! flux of sPAIMW
      real(4) D0sPDetS         	 ! flux of sPDetS
      real(4) D0sPDetW         	 ! flux of sPDetW
      real(4) D0sPHumS         	 ! flux of sPHumS
      real(4) D0sPO4S          	 ! flux of sPO4S
      real(4) D0sPO4W          	 ! flux of sPO4W
      real(4) D0sDPhytW        	 ! flux of sDPhytW
      real(4) D0sPPhytW        	 ! flux of sPPhytW
      real(4) D0sNPhytW        	 ! flux of sNPhytW
      real(4) D0sDPhytS        	 ! flux of sDPhytS
      real(4) D0sPPhytS        	 ! flux of sPPhytS
      real(4) D0sNPhytS        	 ! flux of sNPhytS
      real(4) D0sDElod         	 ! flux of sDElod
      real(4) D0sDChar         	 ! flux of sDChar
      real(4) D0sDCera         	 ! flux of sDCera
      real(4) D0sDLemn         	 ! flux of sDLemn
      real(4) D0sDNymp         	 ! flux of sDNymp
      real(4) D0sDHelo         	 ! flux of sDHelo
      real(4) D0sPElod         	 ! flux of sPElod
      real(4) D0sPChar         	 ! flux of sPChar
      real(4) D0sPCera         	 ! flux of sPCera
      real(4) D0sPLemn         	 ! flux of sPLemn
      real(4) D0sPNymp         	 ! flux of sPNymp
      real(4) D0sPHelo         	 ! flux of sPHelo
      real(4) D0sNElod         	 ! flux of sNElod
      real(4) D0sNChar         	 ! flux of sNChar
      real(4) D0sNCera         	 ! flux of sNCera
      real(4) D0sNLemn         	 ! flux of sNLemn
      real(4) D0sNNymp         	 ! flux of sNNymp
      real(4) D0sNHelo         	 ! flux of sNHelo
      real(4) D0sDExtTotT      	 ! flux of sDExtTotT
      real(4) D0sNExtTotT      	 ! flux of sNExtTotT
      real(4) D0sPExtTotT      	 ! flux of sPExtTotT
!
!
!     /* ==============================  */
!     /* declaration pointer to flux     */
!     /* ==============================  */
      integer ID0sDDetS         	 ! pointer to flux variable 
      integer ID0sDDetW         	 ! pointer to flux variable 
      integer ID0sDHumS         	 ! pointer to flux variable 
      integer ID0sDIMS          	 ! pointer to flux variable 
      integer ID0sDIMW          	 ! pointer to flux variable 
      integer ID0sNDetS         	 ! pointer to flux variable 
      integer ID0sNDetW         	 ! pointer to flux variable 
      integer ID0sNH4S          	 ! pointer to flux variable 
      integer ID0sNH4W          	 ! pointer to flux variable 
      integer ID0sNHumS         	 ! pointer to flux variable 
      integer ID0sNO3S          	 ! pointer to flux variable 
      integer ID0sNO3W          	 ! pointer to flux variable 
      integer ID0sO2W           	 ! pointer to flux variable 
      integer ID0sPAIMS         	 ! pointer to flux variable 
      integer ID0sPAIMW         	 ! pointer to flux variable 
      integer ID0sPDetS         	 ! pointer to flux variable 
      integer ID0sPDetW         	 ! pointer to flux variable 
      integer ID0sPHumS         	 ! pointer to flux variable 
      integer ID0sPO4S          	 ! pointer to flux variable 
      integer ID0sPO4W          	 ! pointer to flux variable 
      integer ID0sDPhytW        	 ! pointer to flux variable 
      integer ID0sPPhytW        	 ! pointer to flux variable 
      integer ID0sNPhytW        	 ! pointer to flux variable 
      integer ID0sDPhytS        	 ! pointer to flux variable 
      integer ID0sPPhytS        	 ! pointer to flux variable 
      integer ID0sNPhytS        	 ! pointer to flux variable 
      integer ID0sDElod         	 ! pointer to flux variable 
      integer ID0sDChar         	 ! pointer to flux variable 
      integer ID0sDCera         	 ! pointer to flux variable 
      integer ID0sDLemn         	 ! pointer to flux variable 
      integer ID0sDNymp         	 ! pointer to flux variable 
      integer ID0sDHelo         	 ! pointer to flux variable 
      integer ID0sPElod         	 ! pointer to flux variable 
      integer ID0sPChar         	 ! pointer to flux variable 
      integer ID0sPCera         	 ! pointer to flux variable 
      integer ID0sPLemn         	 ! pointer to flux variable 
      integer ID0sPNymp         	 ! pointer to flux variable 
      integer ID0sPHelo         	 ! pointer to flux variable 
      integer ID0sNElod         	 ! pointer to flux variable 
      integer ID0sNChar         	 ! pointer to flux variable 
      integer ID0sNCera         	 ! pointer to flux variable 
      integer ID0sNLemn         	 ! pointer to flux variable 
      integer ID0sNNymp         	 ! pointer to flux variable 
      integer ID0sNHelo         	 ! pointer to flux variable 
      integer ID0sDExtTotT      	 ! pointer to flux variable 
      integer ID0sNExtTotT      	 ! pointer to flux variable 
      integer ID0sPExtTotT      	 ! pointer to flux variable 
integer, save :: counter1 = 0
integer, save :: counter2 = 0
integer, save :: counter3 = 0
CHARACTER :: textfile
 !   
 !*******************************************************************************    
 !   
      ipnt        = ipoint
      ID0sDDetS         	= 1 
      ID0sDDetW         	= 2 
      ID0sDHumS         	= 3 
      ID0sDIMS          	= 4 
      ID0sDIMW          	= 5 
      ID0sNDetS         	= 6 
      ID0sNDetW         	= 7 
      ID0sNH4S          	= 8 
      ID0sNH4W          	= 9 
      ID0sNHumS         	= 10 
      ID0sNO3S          	= 11 
      ID0sNO3W          	= 12 
      ID0sO2W           	= 13 
      ID0sPAIMS         	= 14 
      ID0sPAIMW         	= 15 
      ID0sPDetS         	= 16 
      ID0sPDetW         	= 17 
      ID0sPHumS         	= 18 
      ID0sPO4S          	= 19 
      ID0sPO4W          	= 20 
      ID0sDPhytW        	= 21 
      ID0sPPhytW        	= 22 
      ID0sNPhytW        	= 23 
      ID0sDPhytS        	= 24 
      ID0sPPhytS        	= 25 
      ID0sNPhytS        	= 26 
      ID0sDElod         	= 27 
      ID0sDChar         	= 28 
      ID0sDCera         	= 29 
      ID0sDLemn         	= 30 
      ID0sDNymp         	= 31 
      ID0sDHelo         	= 32 
      ID0sPElod         	= 33 
      ID0sPChar         	= 34 
      ID0sPCera         	= 35 
      ID0sPLemn         	= 36 
      ID0sPNymp         	= 37 
      ID0sPHelo         	= 38 
      ID0sNElod         	= 39 
      ID0sNChar         	= 40 
      ID0sNCera         	= 41 
      ID0sNLemn         	= 42 
      ID0sNNymp         	= 43 
      ID0sNHelo         	= 44 
      ID0sDExtTotT      	= 45 
      ID0sNExtTotT      	= 46 
      ID0sPExtTotT      	= 47 
 !   
       do 9000 iseg = 1 , noseg
 !   
         sDDetS              	= pmsa( ipnt( 1) )
         sDDetW              	= pmsa( ipnt( 2) )
         sDHumS              	= pmsa( ipnt( 3) )
         sDIMS               	= pmsa( ipnt( 4) )
         sDIMW               	= pmsa( ipnt( 5) )
         sNDetS              	= pmsa( ipnt( 6) )
         sNDetW              	= pmsa( ipnt( 7) )
         sNH4S               	= pmsa( ipnt( 8) )
         sNH4W               	= pmsa( ipnt( 9) )
         sNHumS              	= pmsa( ipnt( 10) )
         sNO3S               	= pmsa( ipnt( 11) )
         sNO3W               	= pmsa( ipnt( 12) )
         sO2W                	= pmsa( ipnt( 13) )
         sPAIMS              	= pmsa( ipnt( 14) )
         sPAIMW              	= pmsa( ipnt( 15) )
         sPDetS              	= pmsa( ipnt( 16) )
         sPDetW              	= pmsa( ipnt( 17) )
         sPHumS              	= pmsa( ipnt( 18) )
         sPO4S               	= pmsa( ipnt( 19) )
         sPO4W               	= pmsa( ipnt( 20) )
         sDPhytW             	= pmsa( ipnt( 21) )
         sPPhytW             	= pmsa( ipnt( 22) )
         sNPhytW             	= pmsa( ipnt( 23) )
         sDPhytS             	= pmsa( ipnt( 24) )
         sPPhytS             	= pmsa( ipnt( 25) )
         sNPhytS             	= pmsa( ipnt( 26) )
         sDElod              	= pmsa( ipnt( 27) )
         sDChar              	= pmsa( ipnt( 28) )
         sDCera              	= pmsa( ipnt( 29) )
         sDLemn              	= pmsa( ipnt( 30) )
         sDNymp              	= pmsa( ipnt( 31) )
         sDHelo              	= pmsa( ipnt( 32) )
         sPElod              	= pmsa( ipnt( 33) )
         sPChar              	= pmsa( ipnt( 34) )
         sPCera              	= pmsa( ipnt( 35) )
         sPLemn              	= pmsa( ipnt( 36) )
         sPNymp              	= pmsa( ipnt( 37) )
         sPHelo              	= pmsa( ipnt( 38) )
         sNElod              	= pmsa( ipnt( 39) )
         sNChar              	= pmsa( ipnt( 40) )
         sNCera              	= pmsa( ipnt( 41) )
         sNLemn              	= pmsa( ipnt( 42) )
         sNNymp              	= pmsa( ipnt( 43) )
         sNHelo              	= pmsa( ipnt( 44) )
         sDExtTotT           	= pmsa( ipnt( 45) )
         sNExtTotT           	= pmsa( ipnt( 46) )
         sPExtTotT           	= pmsa( ipnt( 47) )
         ITIME               	= pmsa( ipnt( 48) )
         TotalDepth        	= pmsa( ipnt( 49) )
         BeginTime           	= pmsa( ipnt( 50) )
         ConstDepth          	= pmsa( ipnt( 51) )
         ReadTemp            	= pmsa( ipnt( 52) )
         ReadLOut            	= pmsa( ipnt( 53) )
         ReadVWind           	= pmsa( ipnt( 54) )
         InitCalc            	= pmsa( ipnt( 55) )
         InclTran            	= pmsa( ipnt( 56) )
         ReadQIn             	= pmsa( ipnt( 57) )
         ReadQOut            	= pmsa( ipnt( 58) )
         ReadQEv             	= pmsa( ipnt( 59) )
         ReadPLoad           	= pmsa( ipnt( 60) )
         ReadNLoad           	= pmsa( ipnt( 61) )
         ReadNutFrac         	= pmsa( ipnt( 62) )
         ReadPLoadPhyt       	= pmsa( ipnt( 63) )
         ReadDLoadDet        	= pmsa( ipnt( 64) )
         ReadDLoadIM         	= pmsa( ipnt( 65) )
         UseSeasonLoad       	= pmsa( ipnt( 66) )
         UsePulseLoad        	= pmsa( ipnt( 67) )
         mTemp               	= pmsa( ipnt( 68) )
         mLOut               	= pmsa( ipnt( 69) )
         mVWind              	= pmsa( ipnt( 70) )
         mQIn                	= pmsa( ipnt( 71) )
         mQOut               	= pmsa( ipnt( 72) )
         mQEv                	= pmsa( ipnt( 73) )
         mPLoad              	= pmsa( ipnt( 74) )
         mPLoadPO4           	= pmsa( ipnt( 75) )
         mPLoadOrg           	= pmsa( ipnt( 76) )
         mPLoadPhytTot       	= pmsa( ipnt( 77) )
         mNLoad              	= pmsa( ipnt( 78) )
         mNLoadNH4           	= pmsa( ipnt( 79) )
         mNLoadNO3           	= pmsa( ipnt( 80) )
         mNLoadOrg           	= pmsa( ipnt( 81) )
         mDLoadDet           	= pmsa( ipnt( 82) )
         mDLoadIM            	= pmsa( ipnt( 83) )
         fDTotS0             	= pmsa( ipnt( 84) )
         fDOrgS0             	= pmsa( ipnt( 85) )
         fDDetS0             	= pmsa( ipnt( 86) )
         fPInorgS0           	= pmsa( ipnt( 87) )
         fPAdsS0             	= pmsa( ipnt( 88) )
         cPDDet0             	= pmsa( ipnt( 89) )
         cNDDet0             	= pmsa( ipnt( 90) )
         cPDHum0             	= pmsa( ipnt( 91) )
         cNDHum0             	= pmsa( ipnt( 92) )
         cPDPhyt0            	= pmsa( ipnt( 93) )
         cNDPhyt0            	= pmsa( ipnt( 94) )
         cPDElod0            	= pmsa( ipnt( 95) )
         cNDElod0            	= pmsa( ipnt( 96) )
         cPDChar0            	= pmsa( ipnt( 97) )
         cNDChar0            	= pmsa( ipnt( 98) )
         cPDCera0            	= pmsa( ipnt( 99) )
         cNDCera0            	= pmsa( ipnt( 100) )
         cPDLemn0            	= pmsa( ipnt( 101) )
         cNDLemn0            	= pmsa( ipnt( 102) )
         cPDNymp0            	= pmsa( ipnt( 103) )
         cNDNymp0            	= pmsa( ipnt( 104) )
         cPDHelo0            	= pmsa( ipnt( 105) )
         cNDHelo0            	= pmsa( ipnt( 106) )
         cQInf               	= pmsa( ipnt( 107) )
         cPBackLoad          	= pmsa( ipnt( 108) )
         cNBackLoad          	= pmsa( ipnt( 109) )
         cNLoadS             	= pmsa( ipnt( 110) )
         fNH4LoadS           	= pmsa( ipnt( 111) )
         cPO4Ground          	= pmsa( ipnt( 112) )
         cNH4Ground          	= pmsa( ipnt( 113) )
         cNO3Ground          	= pmsa( ipnt( 114) )
         cQIn                	= pmsa( ipnt( 115) )
         cQInSum             	= pmsa( ipnt( 116) )
         cQInWin             	= pmsa( ipnt( 117) )
         cDepthWMax          	= pmsa( ipnt( 118) )
         cQInExtraApril1     	= pmsa( ipnt( 119) )
         cQInExtraOct1       	= pmsa( ipnt( 120) )
         cQOutExtraApril1    	= pmsa( ipnt( 121) )
         cQOutExtraOct1      	= pmsa( ipnt( 122) )
         cQEvAve             	= pmsa( ipnt( 123) )
         cQEvVar             	= pmsa( ipnt( 124) )
         cPLoad              	= pmsa( ipnt( 125) )
         cPLoadSum           	= pmsa( ipnt( 126) )
         cPLoadWin           	= pmsa( ipnt( 127) )
         fPO4In              	= pmsa( ipnt( 128) )
         fPhytInWin          	= pmsa( ipnt( 129) )
         fPhytInSum          	= pmsa( ipnt( 130) )
         cNLoad              	= pmsa( ipnt( 131) )
         cNLoadSum           	= pmsa( ipnt( 132) )
         cNLoadWin           	= pmsa( ipnt( 133) )
         cNPLoadMeas         	= pmsa( ipnt( 134) )
         cNPPhytIn           	= pmsa( ipnt( 135) )
         cNPDetIn            	= pmsa( ipnt( 136) )
         fNH4DissIn          	= pmsa( ipnt( 137) )
         cNDPhytIn           	= pmsa( ipnt( 138) )
         cNDDetIn            	= pmsa( ipnt( 139) )
         cDIMIn              	= pmsa( ipnt( 140) )
         cO2In               	= pmsa( ipnt( 141) )
         cDredInterval       	= pmsa( ipnt( 142) )
         cDredStart          	= pmsa( ipnt( 143) )
         cDepthRef           	= pmsa( ipnt( 144) )
         cLengDred           	= pmsa( ipnt( 145) )
         fEffDred            	= pmsa( ipnt( 146) )
         fEffDredLemn        	= pmsa( ipnt( 147) )
         cFetch              	= pmsa( ipnt( 148) )
         cTmAve              	= pmsa( ipnt( 149) )
         cTmVar              	= pmsa( ipnt( 150) )
         cTimeLag            	= pmsa( ipnt( 151) )
         cVWind              	= pmsa( ipnt( 152) )
         cLDayAve            	= pmsa( ipnt( 153) )
         cLDayVar            	= pmsa( ipnt( 154) )
         cfDayAve            	= pmsa( ipnt( 155) )
         cfDayVar            	= pmsa( ipnt( 156) )
         fRefl               	= pmsa( ipnt( 157) )
         fPAR                	= pmsa( ipnt( 158) )
         cDayManVeg1         	= pmsa( ipnt( 159) )
         cDayManVeg2         	= pmsa( ipnt( 160) )
         fManVeg             	= pmsa( ipnt( 161) )
         fManLemn            	= pmsa( ipnt( 162) )
         fManHelo            	= pmsa( ipnt( 163) )
         cLengMan            	= pmsa( ipnt( 164) )
         cYearStartBirds     	= pmsa( ipnt( 165) )
         cDayStartBirds      	= pmsa( ipnt( 166) )
         cDayEndBirds        	= pmsa( ipnt( 167) )
         cBirdsPerha         	= pmsa( ipnt( 168) )
         cDGrazPerBird       	= pmsa( ipnt( 169) )
         hDVegBird           	= pmsa( ipnt( 170) )
         fDAssBird           	= pmsa( ipnt( 171) )
         fDissEgesBird       	= pmsa( ipnt( 172) )
         cDErosTot           	= pmsa( ipnt( 173) )
         cExtWat             	= pmsa( ipnt( 174) )
         cExtSpDet           	= pmsa( ipnt( 175) )
         cExtSpIM            	= pmsa( ipnt( 176) )
         cExtSpPhyt          	= pmsa( ipnt( 177) )
         fDOrgSoil           	= pmsa( ipnt( 178) )
         cPDSoilOM           	= pmsa( ipnt( 179) )
         cNDSoilOM           	= pmsa( ipnt( 180) )
         cDepthS             	= pmsa( ipnt( 181) )
         fLutum              	= pmsa( ipnt( 182) )
         fFeDIM              	= pmsa( ipnt( 183) )
         fAlDIM              	= pmsa( ipnt( 184) )
         cCPerDW             	= pmsa( ipnt( 185) )
         cRhoIM              	= pmsa( ipnt( 186) )
         cRhoOM              	= pmsa( ipnt( 187) )
         cAerRoot            	= pmsa( ipnt( 188) )
         cAerLin             	= pmsa( ipnt( 189) )
         cAerSquare          	= pmsa( ipnt( 190) )
         cThetaAer           	= pmsa( ipnt( 191) )
         kLemnAer            	= pmsa( ipnt( 192) )
         fSedErosIM          	= pmsa( ipnt( 193) )
         cVSetIM             	= pmsa( ipnt( 194) )
         cVSetDet            	= pmsa( ipnt( 195) )
         cThetaSet           	= pmsa( ipnt( 196) )
         cSuspRef            	= pmsa( ipnt( 197) )
         cSuspMin            	= pmsa( ipnt( 198) )
         cSuspMax            	= pmsa( ipnt( 199) )
         cSuspSlope          	= pmsa( ipnt( 200) )
         hDepthSusp          	= pmsa( ipnt( 201) )
         cFetchRef           	= pmsa( ipnt( 202) )
         fLutumRef           	= pmsa( ipnt( 203) )
         kVegResus           	= pmsa( ipnt( 204) )
         kResusPhytMax       	= pmsa( ipnt( 205) )
         cResusPhytExp       	= pmsa( ipnt( 206) )
         kPDifPO4            	= pmsa( ipnt( 207) )
         kNDifNO3            	= pmsa( ipnt( 208) )
         kNDifNH4            	= pmsa( ipnt( 209) )
         kO2Dif              	= pmsa( ipnt( 210) )
         cThetaDif           	= pmsa( ipnt( 211) )
         fDepthDifS          	= pmsa( ipnt( 212) )
         cTurbDifNut         	= pmsa( ipnt( 213) )
         cTurbDifO2          	= pmsa( ipnt( 214) )
         kPSorp              	= pmsa( ipnt( 215) )
         cRelPAdsD           	= pmsa( ipnt( 216) )
         cRelPAdsFe          	= pmsa( ipnt( 217) )
         cRelPAdsAl          	= pmsa( ipnt( 218) )
         cKPAdsOx            	= pmsa( ipnt( 219) )
         fRedMax             	= pmsa( ipnt( 220) )
         coPO4Max            	= pmsa( ipnt( 221) )
         kPChemPO4           	= pmsa( ipnt( 222) )
         cTmRef              	= pmsa( ipnt( 223) )
         fRefrDetS           	= pmsa( ipnt( 224) )
         cThetaMinW          	= pmsa( ipnt( 225) )
         kDMinDetW           	= pmsa( ipnt( 226) )
         hO2BOD              	= pmsa( ipnt( 227) )
         cThetaMinS          	= pmsa( ipnt( 228) )
         kDMinDetS           	= pmsa( ipnt( 229) )
         kDMinHum            	= pmsa( ipnt( 230) )
         hNO3Denit           	= pmsa( ipnt( 231) )
         NO3PerC             	= pmsa( ipnt( 232) )
         kNitrW              	= pmsa( ipnt( 233) )
         kNitrS              	= pmsa( ipnt( 234) )
         cThetaNitr          	= pmsa( ipnt( 235) )
         O2PerNH4            	= pmsa( ipnt( 236) )
         hO2Nitr             	= pmsa( ipnt( 237) )
         fDissMortVeg        	= pmsa( ipnt( 238) )
         cLengAllo           	= pmsa( ipnt( 239) )
         cLengMort           	= pmsa( ipnt( 240) )
         fRootElodSum        	= pmsa( ipnt( 241) )
         fRootElodWin        	= pmsa( ipnt( 242) )
         fFloatElod          	= pmsa( ipnt( 243) )
         fEmergElod          	= pmsa( ipnt( 244) )
         fDepth1Elod         	= pmsa( ipnt( 245) )
         fDepth2Elod         	= pmsa( ipnt( 246) )
         cDLayerElod         	= pmsa( ipnt( 247) )
         cCovSpElod          	= pmsa( ipnt( 248) )
         kMigrElod           	= pmsa( ipnt( 249) )
         cDElodIn            	= pmsa( ipnt( 250) )
         cTmInitElod         	= pmsa( ipnt( 251) )
         cDCarrElod          	= pmsa( ipnt( 252) )
         cMuMaxElod          	= pmsa( ipnt( 253) )
         cQ10ProdElod        	= pmsa( ipnt( 254) )
         hLRefElod           	= pmsa( ipnt( 255) )
         cExtSpElod          	= pmsa( ipnt( 256) )
         kDRespElod          	= pmsa( ipnt( 257) )
         cQ10RespElod        	= pmsa( ipnt( 258) )
         cDayWinElod         	= pmsa( ipnt( 259) )
         kMortElodSum        	= pmsa( ipnt( 260) )
         fWinElod            	= pmsa( ipnt( 261) )
         fDetWMortElod       	= pmsa( ipnt( 262) )
         cPrefElodBird       	= pmsa( ipnt( 263) )
         cVPUptMaxElod       	= pmsa( ipnt( 264) )
         cAffPUptElod        	= pmsa( ipnt( 265) )
         cPDElodMin          	= pmsa( ipnt( 266) )
         cPDElodMax          	= pmsa( ipnt( 267) )
         cVNUptMaxElod       	= pmsa( ipnt( 268) )
         cAffNUptElod        	= pmsa( ipnt( 269) )
         cNDElodMin          	= pmsa( ipnt( 270) )
         cNDElodMax          	= pmsa( ipnt( 271) )
         fRootCharSum        	= pmsa( ipnt( 272) )
         fRootCharWin        	= pmsa( ipnt( 273) )
         fFloatChar          	= pmsa( ipnt( 274) )
         fEmergChar          	= pmsa( ipnt( 275) )
         fDepth1Char         	= pmsa( ipnt( 276) )
         fDepth2Char         	= pmsa( ipnt( 277) )
         cDLayerChar         	= pmsa( ipnt( 278) )
         cCovSpChar          	= pmsa( ipnt( 279) )
         kMigrChar           	= pmsa( ipnt( 280) )
         cDCharIn            	= pmsa( ipnt( 281) )
         cTmInitChar         	= pmsa( ipnt( 282) )
         cDCarrChar          	= pmsa( ipnt( 283) )
         cMuMaxChar          	= pmsa( ipnt( 284) )
         cQ10ProdChar        	= pmsa( ipnt( 285) )
         hLRefChar           	= pmsa( ipnt( 286) )
         cExtSpChar          	= pmsa( ipnt( 287) )
         kDRespChar          	= pmsa( ipnt( 288) )
         cQ10RespChar        	= pmsa( ipnt( 289) )
         cDayWinChar         	= pmsa( ipnt( 290) )
         kMortCharSum        	= pmsa( ipnt( 291) )
         fWinChar            	= pmsa( ipnt( 292) )
         fDetWMortChar       	= pmsa( ipnt( 293) )
         cPrefCharBird       	= pmsa( ipnt( 294) )
         cVPUptMaxChar       	= pmsa( ipnt( 295) )
         cAffPUptChar        	= pmsa( ipnt( 296) )
         cPDCharMin          	= pmsa( ipnt( 297) )
         cPDCharMax          	= pmsa( ipnt( 298) )
         cVNUptMaxChar       	= pmsa( ipnt( 299) )
         cAffNUptChar        	= pmsa( ipnt( 300) )
         cNDCharMin          	= pmsa( ipnt( 301) )
         cNDCharMax          	= pmsa( ipnt( 302) )
         fRootCeraSum        	= pmsa( ipnt( 303) )
         fRootCeraWin        	= pmsa( ipnt( 304) )
         fFloatCera          	= pmsa( ipnt( 305) )
         fEmergCera          	= pmsa( ipnt( 306) )
         fDepth1Cera         	= pmsa( ipnt( 307) )
         fDepth2Cera         	= pmsa( ipnt( 308) )
         cDLayerCera         	= pmsa( ipnt( 309) )
         cCovSpCera          	= pmsa( ipnt( 310) )
         kMigrCera           	= pmsa( ipnt( 311) )
         cDCeraIn            	= pmsa( ipnt( 312) )
         cTmInitCera         	= pmsa( ipnt( 313) )
         cDCarrCera          	= pmsa( ipnt( 314) )
         cMuMaxCera          	= pmsa( ipnt( 315) )
         cQ10ProdCera        	= pmsa( ipnt( 316) )
         hLRefCera           	= pmsa( ipnt( 317) )
         cExtSpCera          	= pmsa( ipnt( 318) )
         kDRespCera          	= pmsa( ipnt( 319) )
         cQ10RespCera        	= pmsa( ipnt( 320) )
         cDayWinCera         	= pmsa( ipnt( 321) )
         kMortCeraSum        	= pmsa( ipnt( 322) )
         fWinCera            	= pmsa( ipnt( 323) )
         fDetWMortCera       	= pmsa( ipnt( 324) )
         cPrefCeraBird       	= pmsa( ipnt( 325) )
         cVPUptMaxCera       	= pmsa( ipnt( 326) )
         cAffPUptCera        	= pmsa( ipnt( 327) )
         cPDCeraMin          	= pmsa( ipnt( 328) )
         cPDCeraMax          	= pmsa( ipnt( 329) )
         cVNUptMaxCera       	= pmsa( ipnt( 330) )
         cAffNUptCera        	= pmsa( ipnt( 331) )
         cNDCeraMin          	= pmsa( ipnt( 332) )
         cNDCeraMax          	= pmsa( ipnt( 333) )
         fObstrLemn          	= pmsa( ipnt( 334) )
         fRootLemnSum        	= pmsa( ipnt( 335) )
         fRootLemnWin        	= pmsa( ipnt( 336) )
         fFloatLemn          	= pmsa( ipnt( 337) )
         fEmergLemn          	= pmsa( ipnt( 338) )
         fDepth1Lemn         	= pmsa( ipnt( 339) )
         fDepth2Lemn         	= pmsa( ipnt( 340) )
         cDLayerLemn         	= pmsa( ipnt( 341) )
         cCovSpLemn          	= pmsa( ipnt( 342) )
         ckMigrLemn          	= pmsa( ipnt( 343) )
         cDLemnIn            	= pmsa( ipnt( 344) )
         cTmInitLemn         	= pmsa( ipnt( 345) )
         cDCarrLemn          	= pmsa( ipnt( 346) )
         cMuMaxLemn          	= pmsa( ipnt( 347) )
         cQ10ProdLemn        	= pmsa( ipnt( 348) )
         hLRefLemn           	= pmsa( ipnt( 349) )
         cExtSpLemn          	= pmsa( ipnt( 350) )
         kDRespLemn          	= pmsa( ipnt( 351) )
         cQ10RespLemn        	= pmsa( ipnt( 352) )
         cDayWinLemn         	= pmsa( ipnt( 353) )
         kMortLemnSum        	= pmsa( ipnt( 354) )
         fWinLemn            	= pmsa( ipnt( 355) )
         fDetWMortLemn       	= pmsa( ipnt( 356) )
         cPrefLemnBird       	= pmsa( ipnt( 357) )
         cVPUptMaxLemn       	= pmsa( ipnt( 358) )
         cAffPUptLemn        	= pmsa( ipnt( 359) )
         cPDLemnMin          	= pmsa( ipnt( 360) )
         cPDLemnMax          	= pmsa( ipnt( 361) )
         cVNUptMaxLemn       	= pmsa( ipnt( 362) )
         cAffNUptLemn        	= pmsa( ipnt( 363) )
         cNDLemnMin          	= pmsa( ipnt( 364) )
         cNDLemnMax          	= pmsa( ipnt( 365) )
         fRootNympSum        	= pmsa( ipnt( 366) )
         fRootNympWin        	= pmsa( ipnt( 367) )
         fFloatNymp          	= pmsa( ipnt( 368) )
         fEmergNymp          	= pmsa( ipnt( 369) )
         fDepth1Nymp         	= pmsa( ipnt( 370) )
         fDepth2Nymp         	= pmsa( ipnt( 371) )
         cDLayerNymp         	= pmsa( ipnt( 372) )
         cCovSpNymp          	= pmsa( ipnt( 373) )
         kMigrNymp           	= pmsa( ipnt( 374) )
         cDNympIn            	= pmsa( ipnt( 375) )
         cTmInitNymp         	= pmsa( ipnt( 376) )
         cDCarrNymp          	= pmsa( ipnt( 377) )
         cMuMaxNymp          	= pmsa( ipnt( 378) )
         cQ10ProdNymp        	= pmsa( ipnt( 379) )
         hLRefNymp           	= pmsa( ipnt( 380) )
         cExtSpNymp          	= pmsa( ipnt( 381) )
         kDRespNymp          	= pmsa( ipnt( 382) )
         cQ10RespNymp        	= pmsa( ipnt( 383) )
         cDayWinNymp         	= pmsa( ipnt( 384) )
         kMortNympSum        	= pmsa( ipnt( 385) )
         fWinNymp            	= pmsa( ipnt( 386) )
         fDetWMortNymp       	= pmsa( ipnt( 387) )
         cPrefNympBird       	= pmsa( ipnt( 388) )
         cVPUptMaxNymp       	= pmsa( ipnt( 389) )
         cAffPUptNymp        	= pmsa( ipnt( 390) )
         cPDNympMin          	= pmsa( ipnt( 391) )
         cPDNympMax          	= pmsa( ipnt( 392) )
         cVNUptMaxNymp       	= pmsa( ipnt( 393) )
         cAffNUptNymp        	= pmsa( ipnt( 394) )
         cNDNympMin          	= pmsa( ipnt( 395) )
         cNDNympMax          	= pmsa( ipnt( 396) )
         fRootHeloSum        	= pmsa( ipnt( 397) )
         fRootHeloWin        	= pmsa( ipnt( 398) )
         fFloatHelo          	= pmsa( ipnt( 399) )
         fEmergHelo          	= pmsa( ipnt( 400) )
         fDepth1Helo         	= pmsa( ipnt( 401) )
         fDepth2Helo         	= pmsa( ipnt( 402) )
         cDLayerHelo         	= pmsa( ipnt( 403) )
         cCovSpHelo          	= pmsa( ipnt( 404) )
         kMigrHelo           	= pmsa( ipnt( 405) )
         cDHeloIn            	= pmsa( ipnt( 406) )
         cTmInitHelo         	= pmsa( ipnt( 407) )
         cDCarrHelo          	= pmsa( ipnt( 408) )
         cMuMaxHelo          	= pmsa( ipnt( 409) )
         cQ10ProdHelo        	= pmsa( ipnt( 410) )
         hLRefHelo           	= pmsa( ipnt( 411) )
         cExtSpHelo          	= pmsa( ipnt( 412) )
         kDRespHelo          	= pmsa( ipnt( 413) )
         cQ10RespHelo        	= pmsa( ipnt( 414) )
         cDayWinHelo         	= pmsa( ipnt( 415) )
         kMortHeloSum        	= pmsa( ipnt( 416) )
         fWinHelo            	= pmsa( ipnt( 417) )
         fDetWMortHelo       	= pmsa( ipnt( 418) )
         cPrefHeloBird       	= pmsa( ipnt( 419) )
         cVPUptMaxHelo       	= pmsa( ipnt( 420) )
         cAffPUptHelo        	= pmsa( ipnt( 421) )
         cPDHeloMin          	= pmsa( ipnt( 422) )
         cPDHeloMax          	= pmsa( ipnt( 423) )
         cVNUptMaxHelo       	= pmsa( ipnt( 424) )
         cAffNUptHelo        	= pmsa( ipnt( 425) )
         cNDHeloMin          	= pmsa( ipnt( 426) )
         cNDHeloMax          	= pmsa( ipnt( 427) )
         cPACoefMin          	= pmsa( ipnt( 428) )
         cPACoefMax          	= pmsa( ipnt( 429) )
         hPACoef             	= pmsa( ipnt( 430) )
         cSecchiPlus         	= pmsa( ipnt( 431) )
         cEuph               	= pmsa( ipnt( 432) )
         cCovSpPhyt          	= pmsa( ipnt( 433) )
         cTmOptLoss          	= pmsa( ipnt( 434) )
         cSigTmLoss          	= pmsa( ipnt( 435) )
         fDissMortPhyt       	= pmsa( ipnt( 436) )
         fDissLoss           	= pmsa( ipnt( 437) )
         cMuMaxPhyt          	= pmsa( ipnt( 438) )
         cTmOptPhyt          	= pmsa( ipnt( 439) )
         cSigTmPhyt          	= pmsa( ipnt( 440) )
         UseSteelePhyt       	= pmsa( ipnt( 441) )
         hLRefPhyt           	= pmsa( ipnt( 442) )
         cLOptRefPhyt        	= pmsa( ipnt( 443) )
         cChDPhytMin         	= pmsa( ipnt( 444) )
         cChDPhytMax         	= pmsa( ipnt( 445) )
         kDRespPhyt          	= pmsa( ipnt( 446) )
         kLossPhyt           	= pmsa( ipnt( 447) )
         kMortPhytW          	= pmsa( ipnt( 448) )
         cVSetPhyt           	= pmsa( ipnt( 449) )
         kMortPhytS          	= pmsa( ipnt( 450) )
         cVPUptMaxPhyt       	= pmsa( ipnt( 451) )
         cAffPUptPhyt        	= pmsa( ipnt( 452) )
         cPDPhytMin          	= pmsa( ipnt( 453) )
         cPDPhytMax          	= pmsa( ipnt( 454) )
         cVNUptMaxPhyt       	= pmsa( ipnt( 455) )
         cAffNUptPhyt        	= pmsa( ipnt( 456) )
         cNDPhytMin          	= pmsa( ipnt( 457) )
         cNDPhytMax          	= pmsa( ipnt( 458) )
         O2PerNO3            	= pmsa( ipnt( 459) )
         cDayApril1          	= pmsa( ipnt( 460) )
         cDayOct1            	= pmsa( ipnt( 461) )
         cLengChange         	= pmsa( ipnt( 462) )
         DaysPerYear         	= pmsa( ipnt( 463) )
         TenDays             	= pmsa( ipnt( 464) )
         SecsPerDay          	= pmsa( ipnt( 465) )
         mmPerm              	= pmsa( ipnt( 466) )
         m2Perha             	= pmsa( ipnt( 467) )
         mgPerg              	= pmsa( ipnt( 468) )
         PerCent             	= pmsa( ipnt( 469) )
         NearZero            	= pmsa( ipnt( 470) )
         molO2molC           	= pmsa( ipnt( 471) )
         molO2molN           	= pmsa( ipnt( 472) )
         molNmolC            	= pmsa( ipnt( 473) )
         cRhoWat             	= pmsa( ipnt( 474) )
         Pi                  	= pmsa( ipnt( 475) )

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
! 	porosity
		bPorS = (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat &
		&/ ( fDTotS0 + (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cR&
		&hoWat )
! 	sediment_porosity_corrected_for_tortuosity
		bPorCorS = ( (bPorS )** (bPorS + 1.0))
! 	time_in_years
		TimeYears = sTime / DaysPerYear
! 	time_(daynumber)_within_the_year_
		Day = sTime - floor(TimeYears) * DaysPerYear
! 	forcing_function_temperature
		if (ReadTemp == 1 ) then
		uTm = mTemp 		else		uTm = cTmAve - cTmVar * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysPerYear) 		endif
! 	forcing_function_wind_speed
		if (ReadVWind == 1)then
		uVWind = mVWind 		else		uVWind = cVWind 		endif
! 	day_length
		ufDay = cfDayAve - cfDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear)
! 	total_daily_radiation
		if (ReadLOut == 1 ) then
		uLDay = 0.0 		else		uLDay = cLDayAve - cLDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysPerYear) 		endif
! 	average_light_intensity_during_daytime
		if (ReadLOut == 1 ) then
		uLOut = mLOut / ufDay 		else		uLOut = uLDay / SecsPerDay / ufDay 		endif
! 	average_PAR_at_zero_depth
		uLPAR0 = fPAR * (1.0 - fRefl) * uLOut
! 	contribution_of_phytoplankton_to_extinction
		aExtPhyt = cExtSpPhyt * sDPhytW
! 	detrital_contribution_to_extinction
		aExtDet = cExtSpDet * sDDetW
! 	contribution_of_inorganic_matter_to_extinction
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
		if (ReadQEv == 1 ) then
		uQEv = mQEv 		else		uQEv = uQEvSinus 		endif
! 	extra_inflow_for_periodic_water_level_regulation
		if ((Day >= cDayApril1 - 0.5*cLengChange) .and. (Day < cDayApril1 + 0.5*cLengCha&
		&nge)) then
		uQInExtra = cQInExtraApril1 		else if ( (Day >= cDayOct1 - 0.5*cLengChange) .and. (Day < cDayOct1 + 0.5*cLengC&
		&hange) ) then		uQInExtra = cQInExtraOct1 		else		uQInExtra = 0.0 		endif
! 	extra_outflow_for_periodic_water_level_regulation
		if ((Day >= cDayApril1 - 0.5*cLengChange) .and. (Day < cDayApril1 + 0.5*cLengCha&
		&nge)) then
		uQOutExtra = cQOutExtraApril1 		else if ( (Day >= cDayOct1 - 0.5*cLengChange) .and. (Day < cDayOct1 + 0.5*cLengC&
		&hange) ) then		uQOutExtra = cQOutExtraOct1 		else		uQOutExtra = 0.0 		endif
! 	inflow
		if (ReadQIn == 1 ) then
		uQIn = mQIn 		else if (UseSeasonLoad == 1 ) then		uQIn = uQInSeason + uQInExtra 		else		uQIn = cQIn + uQInExtra 		endif
! 	outflow
		if (ReadQOut == 1 ) then
		uQOut = max(mQOut, (sDepthW - cDepthWMax) * mmPerm) 		else		uQOut = max(0.0, (uQIn - uQInExtra) - uQEv - cQInf) + uQOutExtra 		endif
! 	inflow_minus_evaporation
		uQDil = uQIn - uQEv
! 	dilution_rate_of_substances
		ukDil = uQDil / mmPerm / sDepthW
! 	dilution_rate_of_water
		ukDilWat = uQIn / mmPerm / sDepthW
! 	outflow_rate
		if (InclTran == 1 ) then
		ukOut = uQOut / mmPerm / sDepthW 		else		ukOut = 0.0 		endif
! 	water_residence_time
		uTauWat = 1.0 / (ukDilWat+NearZero)
! 	residence_time_of_substances
		uTauSubst = 1.0 / (ukDil+NearZero)
! 	change_in_water_depth
		if (InclTran == 1 ) then
		vTranDepthW = (uQIn - uQEv - cQInf - uQOut) / mmPerm 		else		vTranDepthW = 0.0 		endif
! 	total_DW_phytoplankton_in_ditch_water
		oDPhytW = sDPhytW
! 	total_P_phytoplankton_in_ditch_water
		oPPhytW = sPPhytW
! 	total_N_phytoplankton_in_ditch_water
		oNPhytW = sNPhytW
! 	total_DW_phytoplankton_on_ditch_sediment
		aDPhytS = sDPhytS
! 	total_P_phytoplankton_on_ditch_sediment
		aPPhytS = sPPhytS
! 	total_N_phytoplankton_on_ditch_sediment
		aNPhytS = sNPhytS
! 	organic_seston
		oDOMW = sDDetW + oDPhytW
! 	total_seston
		oDSestW = oDOMW + sDIMW
! 	organic_P_in_water
		oPOMW = oPPhytW + sPDetW
! 	total_seston_P_including_adsorbed_P
		oPSestW = oPPhytW + sPDetW + sPAIMW
! 	inorganic_P_in_water
		oPInorgW = sPO4W + sPAIMW
! 	total_P_in_water_(without_vegetation)
		oPTotW = oPSestW + sPO4W
! 	SRN_in_water
		oNDissW = sNO3W + sNH4W
! 	organic_seston_N
		oNOMW = oNPhytW + sNDetW
! 	total_seston_N
		oNSestW = oNOMW
! 	kjeldahl_N_in_water
		oNkjW = oNSestW + sNH4W
! 	total_N_in_water_(without_vegetation)
		oNTotW = oNkjW + sNO3W
! 	total_sediment_DW_(excluding_biota)
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
! 	total_P_in_sediment_(without_humus_and_vegetation)
		aPTotAvailS = sPDetS + aPInorgS + aPPhytS
! 	total_P_in_sediment_(without_vegetation)
		aPTotS = aPTotAvailS + sPHumS
! 	fraction_inorganic_P_in_sediment
		afPInorgS = aPInorgS / aDTotS
! 	total_P_fraction_in_sediment
		afPTotS = aPTotS / aDTotS
! 	fraction_dissolved_P_in_sediment
		afPO4S = sPO4S / (aPTotAvailS + NearZero)
! 	concentration_dissolved_P_in_interstitial_water
		oPO4S = sPO4S / cDepthS / bPorS
! 	total_dissolved_N_in_interstitial_water
		aNDissS = sNH4S + sNO3S
! 	kjeldahl_N_in_sediment_excluding_humus
		aNkjAvailS = sNDetS + aNPhytS + sNH4S
! 	kjeldahl_N_in_sediment
		aNkjS = aNkjAvailS + sNHumS
! 	total_N_in_sediment_excluding_humus
		aNTotAvailS = aNkjAvailS + sNO3S
! 	total_N_in_sediment
		aNTotS = aNkjS + sNO3S
! 	fraction_inorganic_N_in_sediment
		afNInorgS = aNDissS / aDTotS
! 	total_N_fraction_in_sediment
		afNTotS = aNTotS / aDTotS
! 	concentration_dissolved_N-NO3_in_interstitial_water
		oNO3S = sNO3S / cDepthS / bPorS
! 	concentration_dissolved_N-NH4_in_interstitial_water
		oNH4S = sNH4S / cDepthS / bPorS
! 	SRN_concentration_in_interstitial_water
		oNDissS = aNDissS / cDepthS / bPorS
! 	P/DW_ratio_of_water_DIM
		rPDIMW = sPAIMW / sDIMW
! 	P/DW_ratio_of_sediment_DIM
		rPDIMS = sPAIMS / sDIMS
! 	P/DW_ratio_of_water_detritus
		rPDDetW = sPDetW / (sDDetW+NearZero)
! 	N/DW_ratio_of_water_detritus
		rNDDetW = sNDetW / (sDDetW+NearZero)
! 	P/DW_ratio_of_sediment_OM
		rPDHumS = sPHumS / (sDHumS+NearZero)
! 	N/DW_ratio_of_sediment_OM
		rNDHumS = sNHumS / (sDHumS+NearZero)
! 	P/DW_ratio_of_sediment_detritus
		rPDDetS = sPDetS / (sDDetS+NearZero)
! 	N/DW_ratio_of_sediment_detritus
		rNDDetS = sNDetS / (sDDetS+NearZero)
! 	seasonal_P_load
		if (0 == UseSeasonLoad) then
		uPLoadSeason = 0.0 		else if (Day < cDayApril1 - 0.5*cLengChange) then		uPLoadSeason = cPLoadWin 		else if (Day < cDayApril1 + 0.5*cLengChange) then		uPLoadSeason = 0.5*(cPLoadWin + cPLoadSum) + 0.5*(cPLoadWin - cPLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayApril1)) 		else if (Day < cDayOct1 - 0.5*cLengChange) then		uPLoadSeason = cPLoadSum 		else if (Day < cDayOct1 + 0.5*cLengChange) then		uPLoadSeason = 0.5*(cPLoadWin + cPLoadSum) - 0.5*(cPLoadWin - cPLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayOct1)) 		else		uPLoadSeason = cPLoadWin 		endif
! 	P_load
		if (ReadPLoad == 1 .and. ReadNutFrac == 1 ) then
		uPLoad = mPLoadPO4 + mPLoadOrg 		else if (ReadPLoad == 1 .and. 0 == ReadNutFrac) then		uPLoad = mPLoad 		else if (UsePulseLoad == 1 ) then		uPLoad = 0.0 		else if (UseSeasonLoad == 1 ) then		uPLoad = uPLoadSeason 		else		uPLoad = cPLoad 		endif
! 	P_load_PO4
		if (0 == ReadPLoad) then
		uPLoadPO4 = fPO4In * uPLoad 		else if (ReadNutFrac == 1 ) then		uPLoadPO4 = mPLoadPO4 		else		uPLoadPO4 = fPO4In * mPLoad 		endif
! 	P_load_bound_to_organic_matter
		if (0 == ReadPLoad) then
		uPLoadOrg = (1.0 - fPO4In) * uPLoad 		else if (ReadNutFrac == 1 ) then		uPLoadOrg = mPLoadOrg 		else		uPLoadOrg = (1.0 - fPO4In) * mPLoad 		endif
! 	(total)_phytoplankton_P_input
		if (ReadPLoadPhyt == 1 ) then
		uPLoadPhytTot = mPLoadPhytTot 		else		uPLoadPhytTot = ((fPhytInSum+fPhytInWin)/2.0 - (fPhytInSum-fPhytInWin)/2.0 * cos&
		&(2.0*Pi * (sTime + TenDays - cTimeLag) / DaysPerYear)) * uPLoadOrg 		endif
! 	detrital_P_input
		uPLoadDet = uPLoadOrg - uPLoadPhytTot
! 	Adsorbed_P_loading
		uPLoadAIM = 0.0
! 	seasonal_N_load
		if (0 == UseSeasonLoad) then
		uNLoadSeason = 0.0 		else if (Day < cDayApril1 - 0.5*cLengChange) then		uNLoadSeason = cNLoadWin 		else if (Day < cDayApril1 + 0.5*cLengChange) then		uNLoadSeason = 0.5*(cNLoadWin + cNLoadSum) + 0.5*(cNLoadWin - cNLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayApril1)) 		else if (Day < cDayOct1 - 0.5*cLengChange) then		uNLoadSeason = cNLoadSum 		else if (Day < cDayOct1 + 0.5*cLengChange) then		uNLoadSeason = 0.5*(cNLoadWin + cNLoadSum) - 0.5*(cNLoadWin - cNLoadSum) * cos(P&
		&i/cLengChange * (Day - cDayOct1)) 		else		uNLoadSeason = cNLoadWin 		endif
! 	total_phytoplankton_N_input
		uNLoadPhytTot = cNPPhytIn * uPLoadPhytTot
! 	N_load
		if (ReadNLoad == 1 .and. ReadNutFrac == 1 ) then
		uNLoad = mNLoadNH4 + mNLoadNO3 + mNLoadOrg 		else if (ReadNLoad == 1 .and. 0 == ReadNutFrac) then		uNLoad = mNLoad 		else if (UsePulseLoad == 1 ) then		uNLoad = 0.0 		else if (ReadPLoad == 1 ) then		uNLoad = cNPLoadMeas * uPLoad 		else if (UseSeasonLoad == 1 ) then		uNLoad = uNLoadSeason 		else		uNLoad = cNLoad 		endif
! 	N_load_detritus
		if (0 == ReadNLoad) then
		uNLoadDet = min(cNPDetIn * uPLoadDet, uNLoad - uNLoadPhytTot) 		else if (ReadNutFrac == 1 ) then		uNLoadDet = 0.0 		else		uNLoadDet = min(cNPDetIn * uPLoadDet, uNLoad - uNLoadPhytTot) 		endif
! 	loading_N_bound_to_organic_matter
		if (0 == ReadNLoad) then
		uNLoadOrg = uNLoadPhytTot + uNLoadDet 		else if (ReadNutFrac == 1 ) then		uNLoadOrg = mNLoadOrg 		else		uNLoadOrg = uNLoadPhytTot + uNLoadDet 		endif
! 	N_loading_dissolved_(sum_of_NO3_and_NH4)
		if (0 == ReadNLoad) then
		uNLoadDiss = uNLoad - uNLoadOrg 		else if (ReadNutFrac == 1 ) then		uNLoadDiss = 0.0 		else		uNLoadDiss = uNLoad - uNLoadOrg 		endif
! 	NH4_loading
		if (0 == ReadNLoad) then
		uNLoadNH4 = fNH4DissIn * uNLoadDiss 		else if (ReadNutFrac == 1 ) then		uNLoadNH4 = mNLoadNH4 		else		uNLoadNH4 = fNH4DissIn * uNLoadDiss 		endif
! 	NO3_loading
		if (0 == ReadNLoad) then
		uNLoadNO3 = (1.0 - fNH4DissIn) * uNLoadDiss 		else if (ReadNutFrac == 1 ) then		uNLoadNO3 = mNLoadNO3 		else		uNLoadNO3 = (1.0 - fNH4DissIn) * uNLoadDiss 		endif
! 	external_N_concentration
		uNTotIn = uNLoad / (uQIn / mmPerm + NearZero)
! 	detrital_DW_loading
		if (ReadDLoadDet == 1 ) then
		uDLoadDet = mDLoadDet 		else		uDLoadDet = uNLoadDet / cNDDetIn 		endif
! 	total_phytoplankton_DW_input
		uDLoadPhytTot = uNLoadPhytTot / cNDPhytIn
! 	loading_of_DW_of_inorganic_matter
		if (ReadDLoadIM == 1 ) then
		uDLoadIM = mDLoadIM 		else		uDLoadIM = cDIMIn * uQIn / mmPerm 		endif
! 	total_DW_input
		uDLoad = uDLoadIM + uDLoadDet + uDLoadPhytTot
! 	external_P_concentration
		uPTotIn = uPLoad / (uQIn / mmPerm + NearZero)
! 	dilution_of_DW_IM
		wDDilIM = ukDil * sDIMW
! 	dilllution_of_detritus
		wDDilDet = ukDil * sDDetW
! 	dilution_of_SRP
		wPDilPO4 = ukDil * sPO4W
! 	dilution_of_detritus
		wPDilDet = ukDil*sPDetW
! 	dilution_of_IM-adsorption_P
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
! 	total_phytoplankton_dilution
		wDDilPhyt = ukDil * sDPhytW
! 	total_phytoplankton_dilution
		wPDilPhyt = ukDil * sPPhytW
! 	total_phytoplankton_dilution
		wNDilPhyt = ukDil * sNPhytW
! 	total_DW_outflow
		wDOutflTot = ukOut * oDSestW
! 	total_P_outflow
		wPOutflTot = ukOut * oPTotW
! 	total_N_outflow
		wNOutflTot = ukOut * oNTotW
! 	phytoplankton_input
		uDLoadPhyt = uDLoadPhytTot
! 	phytoplankton_input
		uPLoadPhyt = uPLoadPhytTot
! 	phytoplankton_input
		uNLoadPhyt = uNLoadPhytTot
! 	total_transport_flux_of_DW_in_phytoplankton
		if (InclTran == 1 ) then
		wDTranPhyt = uDLoadPhyt / sDepthW - wDDilPhyt 		else		wDTranPhyt = 0.0 		endif
! 	total_transport_flux_of_P_in_phytoplankton
		if (InclTran == 1 ) then
		wPTranPhyt = uPLoadPhyt / sDepthW - wPDilPhyt 		else		wPTranPhyt = 0.0 		endif
! 	total_transport_flux_of_N_in_phytoplankton
		if (InclTran == 1 ) then
		wNTranPhyt = uNLoadPhyt / sDepthW - wNDilPhyt 		else		wNTranPhyt = 0.0 		endif
! 	transport_flux_DW_in_IM
		if (InclTran == 1 ) then
		wDTranIMW = uDLoadIM / sDepthW - wDDilIM 		else		wDTranIMW = 0.0 		endif
! 	transport_flux_DW_in_detritus
		if (InclTran == 1 ) then
		wDTranDetW = uDLoadDet / sDepthW - wDDilDet 		else		wDTranDetW = 0.0 		endif
! 	transport_flux_O2
		if (InclTran == 1 ) then
		wO2TranW = wO2Inflow - wO2Outfl 		else		wO2TranW = 0.0 		endif
! 	transport_flux_of_P_in_PO4
		if (InclTran == 1 ) then
		wPTranPO4W = uPLoadPO4 / sDepthW - wPDilPO4 		else		wPTranPO4W = 0.0 		endif
! 	transport_flux_of_P_in_AIM
		if (InclTran == 1 ) then
		wPTranAIMW = uPLoadAIM / sDepthW - wPDilAIM 		else		wPTranAIMW = 0.0 		endif
! 	transport_flux_of_P_in_detritus
		if (InclTran == 1 ) then
		wPTranDetW = uPLoadDet / sDepthW - wPDilDet 		else		wPTranDetW = 0.0 		endif
! 	transport_flux_of_N_in_NH4
		if (InclTran == 1 ) then
		wNTranNH4W = uNLoadNH4 / sDepthW - wNDilNH4 		else		wNTranNH4W = 0.0 		endif
! 	transport_flux_of_N_in_NO3
		if (InclTran == 1 ) then
		wNTranNO3W = uNLoadNO3 / sDepthW - wNDilNO3 		else		wNTranNO3W = 0.0 		endif
! 	transport_flux_of_N_in_detritus
		if (InclTran == 1 ) then
		wNTranDetW = uNLoadDet / sDepthW - wNDilDet 		else		wNTranDetW = 0.0 		endif
! 	Total_DW_dilution_fluxes
		wDDilTot = wDDilIM + wDDilDet + wDDilPhyt
! 	Total_P_dilution_fluxes
		wPDilTot = wPDilDet + wPDilPO4 + wPDilAIM + wPDilPhyt
! 	Total_N_dilution_fluxes
		wNDilTot = wNDilDet + wNDilNO3 + wNDilNH4 + wNDilPhyt
! 	infiltration_of_SRP
		if (0 == InclTran) then
		tPInfPO4W = 0.0 		else if (cQInf >= 0.0) then		tPInfPO4W = cQInf / mmPerm * sPO4W 		else		tPInfPO4W = cQInf / mmPerm * oPO4S 		endif
! 	infiltration_of_ammonium
		if (0 == InclTran) then
		tNInfNH4W = 0.0 		else if (cQInf >= 0.0) then		tNInfNH4W = cQInf / mmPerm * sNH4W 		else		tNInfNH4W = cQInf / mmPerm * oNH4S 		endif
! 	infiltration_of_nitrate
		if (0 == InclTran) then
		tNInfNO3W = 0.0 		else if (cQInf >= 0.0) then		tNInfNO3W = cQInf / mmPerm * sNO3W 		else		tNInfNO3W = cQInf / mmPerm * oNO3S 		endif
! 	infiltration_of_interstitial_PO4
		if (0 == InclTran) then
		tPInfPO4S = 0.0 		else if (cQInf >= 0.0) then		tPInfPO4S = cQInf / mmPerm * oPO4S 		else		tPInfPO4S = cQInf / mmPerm * cPO4Ground 		endif
! 	infiltration_of_interstitial_NH4
		if (0 == InclTran) then
		tNInfNH4S = 0.0 		else if (cQInf >= 0.0) then		tNInfNH4S = cQInf / mmPerm * oNH4S 		else		tNInfNH4S = cQInf / mmPerm * cNH4Ground 		endif
! 	infiltration_of_interstitial_NO3
		if (0 == InclTran) then
		tNInfNO3S = 0.0 		else if (cQInf >= 0.0) then		tNInfNO3S = cQInf / mmPerm * oNO3S 		else		tNInfNO3S = cQInf / mmPerm * cNO3Ground 		endif
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
		uFunTmAer = ( (cThetaAer )** (uTm-cTmRef))
! 	duckweed_function_of_reaeration
		aFunLemnAer = max(0.0, 1.0 - (kLemnAer * sDLemn))
! 	reaeration_flux_of_O2_into_the_water
		tO2Aer = kAer * uFunTmAer * (uO2Sat - sO2W) * aFunLemnAer
! 	bioturbation_by_fish
		tDTurbFish = 0.0
! 	IM_bioturbation_by_fish
		tDTurbFishIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDTurbFish
! 	total_plant_biomass
		aDVeg = sDElod + sDChar + sDCera + sDLemn + sDNymp + sDHelo
! 	vegetation_dependence_of_resuspension
		aFunVegResus = max(1.0 - kVegResus * aDVeg, 0.0)
! 	Empirical_suspended_matter_function_(logistic_fit_to_data)
		if (uTm >= 0.1) then
		aFunDimSusp = cSuspRef * ((cSuspMin + cSuspMax / (1.0 + exp(cSuspSlope * (sDepth&
		&W - hDepthSusp)))) * ((((cFetch +NearZero)/ cFetchRef))**(0.5))) 		else		aFunDimSusp = 0.0 		endif
! 	resuspension_due_to_shear_stress
		tDResusTauDead = min(aFunDimSusp, (((aFunDimSusp +NearZero))**(0.5))) * (((fLutu&
		&m / fLutumRef))**(0.5)) * bPorS
! 	resuspension_due_to_shear_stress_and_fish
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
! 	resuspension_flux_of_P_adsorbed_onto_inorganic_matter
		tPResusAIM = sPAIMS / sDIMS * tDResusIM
! 	resuspension_flux_of_nitrate
		tNResusNO3 = sNO3S / sDDetS * tDResusDet
! 	resuspension_flux_of_ammonium
		tNResusNH4 = sNH4S / sDDetS * tDResusDet
! 	resuspension_flux_of_detrital_N
		tNResusDet = rNDDetS * tDResusDet
! 	correction_factor_for_IM_settling_rate_(<=_1)
		aFunTauSetOM = min(1.0 / (((aFunDimSusp +NearZero))**(0.5)), 1.0)
! 	correction_factor_for_OM_settling_rate_(<=_1)
		aFunTauSetIM = aFunTauSetOM
! 	temperature_correction_of_sedimentation
		uFunTmSet = ( (cThetaSet )** (uTm-cTmRef))
! 	corrected_sedimentation_velocity_of_IM
		uCorVSetIM = aFunTauSetIM * (((fLutumRef/fLutum))**(0.5)) * uFunTmSet * cVSetIM
! 	sedimentation_flux_of_inorganic_matter
		tDSetIM = uCorVSetIM * sDIMW
! 	sedimentation_flux_of_P_adsorbed_onto_inorganic_matter
		tPSetAIM = sPAIMW / sDIMW * tDSetIM
! 	corrected_sedimentation_velocity_of_detritus
		uCorVSetDet = cVSetDet * aFunTauSetOM * uFunTmSet
! 	sedimentation_flux_of_detritus
		tDSetDet = uCorVSetDet * sDDetW
! 	sedimentation_flux_of_detrital_P
		tPSetDet = uCorVSetDet * sPDetW
! 	sedimentation_flux_of_detrital_N
		tNSetDet = uCorVSetDet * sNDetW
! 	P_mineralization_constant_in_water
		kPMinDetW = kDMinDetW
! 	N_mineralization_constant_in_water
		kNMinDetW = kDMinDetW
! 	temperature_function_of_mineralization_in_water
		uFunTmMinW = ( (cThetaMinW )** (uTm-cTmRef))
! 	decomposition
		wDMinDetW = kDMinDetW * uFunTmMinW * sDDetW
! 	mineralization
		wPMinDetW = kPMinDetW * uFunTmMinW * sPDetW
! 	mineralization
		wNMinDetW = kNMinDetW * uFunTmMinW * sNDetW
! 	correction_of_O2_demand_in_water_at_low_oxygen_concentration
		aCorO2BOD = sO2W / (hO2BOD + sO2W)
! 	O2_flux_due_to_mineralization_of_detritus
		wO2MinDetW = molO2molC * cCPerDW * aCorO2BOD * wDMinDetW
! 	mineralization_flux_by_denitrification
		wDDenitW = sNO3W*sNO3W / (hNO3Denit*hNO3Denit + sNO3W*sNO3W) * (1.0 - aCorO2BOD)&
		& * wDMinDetW
! 	Denitrification_flux
		wNDenitW = NO3PerC * molNmolC * cCPerDW * wDDenitW
! 	Temperature_dependence_for_nitrification
		uFunTmNitr = ( (cThetaNitr )** (uTm-cTmRef))
! 	oxygen_dependence_of_nitrification
		aCorO2NitrW = sO2W*sO2W / (hO2Nitr*hO2Nitr + sO2W*sO2W)
! 	nitrification_flux
		wNNitrW = kNitrW * uFunTmNitr * aCorO2NitrW * sNH4W
! 	O2_flux_due_to_nitrification
		wO2NitrW = O2PerNH4 * molO2molN * wNNitrW
! 	P_mineralization_constant_in_sediment
		kPMinDetS = kDMinDetS
! 	N_mineralization_constant_in_sediment
		kNMinDetS = kDMinDetS
! 	temperature_function
		uFunTmMinS = ( (cThetaMinS )** (uTm-cTmRef))
! 	decomposition_of_upper_sediment
		tDMinDetS = kDMinDetS * uFunTmMinS * sDDetS
! 	mineralization_of_P_in_upper_sediment
		tPMinDetS = kPMinDetS * uFunTmMinS * sPDetS
! 	mineralization_of_N_in_upper_sediment
		tNMinDetS = kNMinDetS * uFunTmMinS * sNDetS
! 	temperature_function_of_diffusion
		uFunTmDif = ( (cThetaDif )** (uTm-cTmRef))
! 	corrected_O2_diffusion_coefficient
		akO2DifCor = kO2Dif * uFunTmDif * cTurbDifO2 * bPorCorS
! 	sediment_oxygen_demand
		tSOD = (molO2molC * cCPerDW * (1.0 - fRefrDetS) * tDMinDetS + O2PerNH4 * molO2mo&
		&lN * kNitrS * uFunTmNitr * sNH4S) / cDepthS
! 	oxygen_penetration_depth
		aDepthOxySed = (((2.0 * sO2W * akO2DifCor / tSOD))**(0.5))
! 	fraction_aerobic_sediment
		afOxySed = aDepthOxySed / cDepthS
! 	aerobic_mineralization
		tDMinOxyDetS = afOxySed * (1.0 - fRefrDetS) * tDMinDetS
! 	sediment_oxygen_demand
		tO2MinDetS = molO2molC * cCPerDW * tDMinOxyDetS
! 	mineralization_flux_by_denitrification
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
! 	O2_diffusion_from_water_to_sediment
		tO2Dif = kO2Dif / aDepthDif * uFunTmDif * cTurbDifO2 * bPorCorS * sO2W
! 	diffusion_flux_of_dissolved_P_from_interstitial_water_to_ground_water
		tPDifGroundPO4 = 0.0
! 	diffusion_flux_of_dissolved_NO3_from_interstitial_water_to_ground_water
		tNDifGroundNO3 = 0.0
! 	diffusion_flux_of_dissolved_NH4_from_interstitial_water_to_ground_water
		tNDifGroundNH4 = 0.0
! 	maximum_P_adsorption_per_g_inorganic_matter_in_water
		aPAdsMaxW = cRelPAdsD + aCorO2BOD * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsW = (1.0 - fRedMax * (1.0-aCorO2BOD)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorganic_matter_in_sediment
		aPIsoAdsW = aPAdsMaxW * aKPAdsW * sPO4W / (1.0 + aKPAdsW * sPO4W)
! 	equilibrium_concentration
		aPEqIMW = aPIsoAdsW * sDIMW
! 	sorption_flux_in_water
		wPSorpIMW = kPSorp * (aPEqIMW - sPAIMW)
! 	maximum_P_adsorption_per_g_inorganic_matter_in_sediment
		aPAdsMaxS = cRelPAdsD + afOxySed * cRelPAdsFe * fFeDIM + cRelPAdsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsS = (1.0 - fRedMax * (1.0-afOxySed)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorganic_matter_in_sediment
		aPIsoAdsS = aPAdsMaxS * aKPAdsS * oPO4S / (1.0 + aKPAdsS * oPO4S)
! 	equilibrium_amount
		aPEqIMS = aPIsoAdsS * sDIMS
! 	sorption
		tPSorpIMS = kPSorp * (aPEqIMS - sPAIMS)
! 	chemical_loss_of_dissolved_P_from_interstitial_water_by_precipitation
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
! 	fraction_of_DW_biomass_removed_by_management
		fManElod = fManVeg
! 	fraction_of_DW_biomass_removed_by_management
		fManCera = fManVeg
! 	fraction_of_DW_biomass_removed_by_management
		fManChar = fManVeg
! 	fraction_of_DW_biomass_removed_by_management
		fManNymp = fManHelo
! 	migration_constant_for_Lemnacaea
		kMigrLemn = ckMigrLemn + (1.0 - fObstrLemn) * ukOut
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitElod = 367 		else if (uTm >= cTmInitElod .and. aDayInitElod > 366) then		aDayInitElod = Day 		else		aDayInitElod = aDayInitElod 		endif
! 	root_fraction
		if (Day < aDayInitElod) then
		bfRootElod = fRootElodWin 		else if (Day < aDayInitElod + cLengAllo) then		bfRootElod = 0.5*(fRootElodWin + fRootElodSum) + 0.5*(fRootElodWin - fRootElodSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitElod)) 		else if (Day < cDayWinElod) then		bfRootElod = fRootElodSum 		else if (Day < cDayWinElod + cLengAllo) then		bfRootElod = 0.5*(fRootElodWin + fRootElodSum) - 0.5*(fRootElodWin - fRootElodSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinElod)) 		else		bfRootElod = fRootElodWin 		endif
! 	shoot_fraction
		bfShootElod = 1.0 - bfRootElod
! 	root_biomass
		aDRootElod = bfRootElod * sDElod
! 	shoot_biomass
		aDShootElod = bfShootElod * sDElod
! 	emergent_biomass
		aDEmergElod = fEmergElod * aDShootElod
! 	floating_biomass
		aDFloatElod = fFloatElod * aDShootElod
! 	submerged_fraction_of_shoot
		bfSubElod = 1.0 - fFloatElod - fEmergElod
! 	submerged_biomass
		aDSubElod = bfSubElod * aDShootElod
! 	contribution_of_plant_species_to_extinction
		aExtElod = cExtSpElod * aDSubElod / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Elod = fDepth1Elod * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Elod = fDepth2Elod * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfElod = min(1.0, max(aDFloatElod / (cDLayerElod + NearZero), aDEmergElod&
		& / (fEmergElod * cDCarrElod + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergElod = min(1.0, PerCent * cCovSpElod * aDEmergElod)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovElod = min(100.0, cCovSpElod * aDShootElod)
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitChar = 367 		else if (uTm >= cTmInitChar .and. aDayInitChar > 366) then		aDayInitChar = Day 		else		aDayInitChar = aDayInitChar 		endif
! 	root_fraction
		if (Day < aDayInitChar) then
		bfRootChar = fRootCharWin 		else if (Day < aDayInitChar + cLengAllo) then		bfRootChar = 0.5*(fRootCharWin + fRootCharSum) + 0.5*(fRootCharWin - fRootCharSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitChar)) 		else if (Day < cDayWinChar) then		bfRootChar = fRootCharSum 		else if (Day < cDayWinChar + cLengAllo) then		bfRootChar = 0.5*(fRootCharWin + fRootCharSum) - 0.5*(fRootCharWin - fRootCharSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinChar)) 		else		bfRootChar = fRootCharWin 		endif
! 	shoot_fraction
		bfShootChar = 1.0 - bfRootChar
! 	root_biomass
		aDRootChar = bfRootChar * sDChar
! 	shoot_biomass
		aDShootChar = bfShootChar * sDChar
! 	emergent_biomass
		aDEmergChar = fEmergChar * aDShootChar
! 	floating_biomass
		aDFloatChar = fFloatChar * aDShootChar
! 	submerged_fraction_of_shoot
		bfSubChar = 1.0 - fFloatChar - fEmergChar
! 	submerged_biomass
		aDSubChar = bfSubChar * aDShootChar
! 	contribution_of_plant_species_to_extinction
		aExtChar = cExtSpChar * aDSubChar / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Char = fDepth1Char * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Char = fDepth2Char * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfChar = min(1.0, max(aDFloatChar / (cDLayerChar + NearZero), aDEmergChar&
		& / (fEmergChar * cDCarrChar + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergChar = min(1.0, PerCent * cCovSpChar * aDEmergChar)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovChar = min(100.0, cCovSpChar * aDShootChar)
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitCera = 367 		else if (uTm >= cTmInitCera .and. aDayInitCera > 366) then		aDayInitCera = Day 		else		aDayInitCera = aDayInitCera 		endif
! 	root_fraction
		if (Day < aDayInitCera) then
		bfRootCera = fRootCeraWin 		else if (Day < aDayInitCera + cLengAllo) then		bfRootCera = 0.5*(fRootCeraWin + fRootCeraSum) + 0.5*(fRootCeraWin - fRootCeraSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitCera)) 		else if (Day < cDayWinCera) then		bfRootCera = fRootCeraSum 		else if (Day < cDayWinCera + cLengAllo) then		bfRootCera = 0.5*(fRootCeraWin + fRootCeraSum) - 0.5*(fRootCeraWin - fRootCeraSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinCera)) 		else		bfRootCera = fRootCeraWin 		endif
! 	shoot_fraction
		bfShootCera = 1.0 - bfRootCera
! 	root_biomass
		aDRootCera = bfRootCera * sDCera
! 	shoot_biomass
		aDShootCera = bfShootCera * sDCera
! 	emergent_biomass
		aDEmergCera = fEmergCera * aDShootCera
! 	floating_biomass
		aDFloatCera = fFloatCera * aDShootCera
! 	submerged_fraction_of_shoot
		bfSubCera = 1.0 - fFloatCera - fEmergCera
! 	submerged_biomass
		aDSubCera = bfSubCera * aDShootCera
! 	contribution_of_plant_species_to_extinction
		aExtCera = cExtSpCera * aDSubCera / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Cera = fDepth1Cera * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Cera = fDepth2Cera * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfCera = min(1.0, max(aDFloatCera / (cDLayerCera + NearZero), aDEmergCera&
		& / (fEmergCera * cDCarrCera + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergCera = min(1.0, PerCent * cCovSpCera * aDEmergCera)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovCera = min(100.0, cCovSpCera * aDShootCera)
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitLemn = 367 		else if (uTm >= cTmInitLemn .and. aDayInitLemn > 366) then		aDayInitLemn = Day 		else		aDayInitLemn = aDayInitLemn 		endif
! 	root_fraction
		if (Day < aDayInitLemn) then
		bfRootLemn = fRootLemnWin 		else if (Day < aDayInitLemn + cLengAllo) then		bfRootLemn = 0.5*(fRootLemnWin + fRootLemnSum) + 0.5*(fRootLemnWin - fRootLemnSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitLemn)) 		else if (Day < cDayWinLemn) then		bfRootLemn = fRootLemnSum 		else if (Day < cDayWinLemn + cLengAllo) then		bfRootLemn = 0.5*(fRootLemnWin + fRootLemnSum) - 0.5*(fRootLemnWin - fRootLemnSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinLemn)) 		else		bfRootLemn = fRootLemnWin 		endif
! 	shoot_fraction
		bfShootLemn = 1.0 - bfRootLemn
! 	root_biomass
		aDRootLemn = bfRootLemn * sDLemn
! 	shoot_biomass
		aDShootLemn = bfShootLemn * sDLemn
! 	emergent_biomass
		aDEmergLemn = fEmergLemn * aDShootLemn
! 	floating_biomass
		aDFloatLemn = fFloatLemn * aDShootLemn
! 	submerged_fraction_of_shoot
		bfSubLemn = 1.0 - fFloatLemn - fEmergLemn
! 	submerged_biomass
		aDSubLemn = bfSubLemn * aDShootLemn
! 	contribution_of_plant_species_to_extinction
		aExtLemn = cExtSpLemn * aDSubLemn / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Lemn = fDepth1Lemn * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Lemn = fDepth2Lemn * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfLemn = min(1.0, max(aDFloatLemn / (cDLayerLemn + NearZero), aDEmergLemn&
		& / (fEmergLemn * cDCarrLemn + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergLemn = min(1.0, PerCent * cCovSpLemn * aDEmergLemn)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovLemn = min(100.0, cCovSpLemn * aDShootLemn)
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitNymp = 367 		else if (uTm >= cTmInitNymp .and. aDayInitNymp > 366) then		aDayInitNymp = Day 		else		aDayInitNymp = aDayInitNymp 		endif
! 	root_fraction
		if (Day < aDayInitNymp) then
		bfRootNymp = fRootNympWin 		else if (Day < aDayInitNymp + cLengAllo) then		bfRootNymp = 0.5*(fRootNympWin + fRootNympSum) + 0.5*(fRootNympWin - fRootNympSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitNymp)) 		else if (Day < cDayWinNymp) then		bfRootNymp = fRootNympSum 		else if (Day < cDayWinNymp + cLengAllo) then		bfRootNymp = 0.5*(fRootNympWin + fRootNympSum) - 0.5*(fRootNympWin - fRootNympSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinNymp)) 		else		bfRootNymp = fRootNympWin 		endif
! 	shoot_fraction
		bfShootNymp = 1.0 - bfRootNymp
! 	root_biomass
		aDRootNymp = bfRootNymp * sDNymp
! 	shoot_biomass
		aDShootNymp = bfShootNymp * sDNymp
! 	emergent_biomass
		aDEmergNymp = fEmergNymp * aDShootNymp
! 	floating_biomass
		aDFloatNymp = fFloatNymp * aDShootNymp
! 	submerged_fraction_of_shoot
		bfSubNymp = 1.0 - fFloatNymp - fEmergNymp
! 	submerged_biomass
		aDSubNymp = bfSubNymp * aDShootNymp
! 	contribution_of_plant_species_to_extinction
		aExtNymp = cExtSpNymp * aDSubNymp / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Nymp = fDepth1Nymp * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Nymp = fDepth2Nymp * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfNymp = min(1.0, max(aDFloatNymp / (cDLayerNymp + NearZero), aDEmergNymp&
		& / (fEmergNymp * cDCarrNymp + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergNymp = min(1.0, PerCent * cCovSpNymp * aDEmergNymp)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovNymp = min(100.0, cCovSpNymp * aDShootNymp)
! 	day_of_the_year_at_which_growing_season_starts
		if (Day < 1.0 .or. sTime < BeginTime + 1.0) then
		aDayInitHelo = 367 		else if (uTm >= cTmInitHelo .and. aDayInitHelo > 366) then		aDayInitHelo = Day 		else		aDayInitHelo = aDayInitHelo 		endif
! 	root_fraction
		if (Day < aDayInitHelo) then
		bfRootHelo = fRootHeloWin 		else if (Day < aDayInitHelo + cLengAllo) then		bfRootHelo = 0.5*(fRootHeloWin + fRootHeloSum) + 0.5*(fRootHeloWin - fRootHeloSu&
		&m) * cos(Pi/cLengAllo * (Day - aDayInitHelo)) 		else if (Day < cDayWinHelo) then		bfRootHelo = fRootHeloSum 		else if (Day < cDayWinHelo + cLengAllo) then		bfRootHelo = 0.5*(fRootHeloWin + fRootHeloSum) - 0.5*(fRootHeloWin - fRootHeloSu&
		&m) * cos(Pi/cLengAllo * (Day - cDayWinHelo)) 		else		bfRootHelo = fRootHeloWin 		endif
! 	shoot_fraction
		bfShootHelo = 1.0 - bfRootHelo
! 	root_biomass
		aDRootHelo = bfRootHelo * sDHelo
! 	shoot_biomass
		aDShootHelo = bfShootHelo * sDHelo
! 	emergent_biomass
		aDEmergHelo = fEmergHelo * aDShootHelo
! 	floating_biomass
		aDFloatHelo = fFloatHelo * aDShootHelo
! 	submerged_fraction_of_shoot
		bfSubHelo = 1.0 - fFloatHelo - fEmergHelo
! 	submerged_biomass
		aDSubHelo = bfSubHelo * aDShootHelo
! 	contribution_of_plant_species_to_extinction
		aExtHelo = cExtSpHelo * aDSubHelo / sDepthW
! 	upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		aDepth1Helo = fDepth1Helo * sDepthW
! 	lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		aDepth2Helo = fDepth2Helo * sDepthW
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfHelo = min(1.0, max(aDFloatHelo / (cDLayerHelo + NearZero), aDEmergHelo&
		& / (fEmergHelo * cDCarrHelo + NearZero) ) )
! 	fraction_of_water_surface_covered_by_emergent_biomass
		afCovEmergHelo = min(1.0, PerCent * cCovSpHelo * aDEmergHelo)
! 	percentage_of_water_surface_covered_by_plant_species
		aCovHelo = min(100.0, cCovSpHelo * aDShootHelo)
! 	total_P_in_vegetation
		aPVeg = sPElod + sPChar + sPCera + sPLemn + sPNymp + sPHelo
! 	total_N_in_vegetation
		aNVeg = sNElod + sNChar + sNCera + sNLemn + sNNymp + sNHelo
! 	fraction_of_water_surface_covered_by_plant_species
		afCovSurfVeg = min(1.0, afCovSurfElod + afCovSurfChar + afCovSurfCera + afCovSur&
		&fLemn + afCovSurfNymp + afCovSurfHelo)
! 	fraction_emergent_coverage
		afCovEmergVeg = min(1.0, afCovEmergElod + afCovEmergChar + afCovEmergCera + afCo&
		&vEmergLemn + afCovEmergNymp + afCovEmergHelo)
! 	extinction_coefficient_of_total_vegetation
		aExtVeg = aExtElod + aExtChar + aExtCera + aExtLemn + aExtNymp + aExtHelo
! 	extinction_coefficient_of_water_colum_including_vegetation
		aExtCoef = aExtCoefOpen + aExtVeg
! 	light_at_the_bottom
		aLPARBot = uLPAR0 * exp(- aExtCoef * sDepthW)
! 	P/DW_ratio_of_vegetation
		rPDElod = sPElod / (sDElod+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDElod = sNElod / (sDElod+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrElod = kMigrElod * (cDElodIn - sDElod)
! 	migration_flux_of_vegetation_P
		tPMigrElod = kMigrElod * (cPDElod0* cDElodIn - sPElod)
! 	migration_flux_of_vegetation_N
		tNMigrElod = kMigrElod * (cNDElod0* cDElodIn - sNElod)
! 	temperature_function_of_vegetation_production
		uFunTmProdElod = ( (cQ10ProdElod )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespElod = ( (cQ10RespElod )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrElod = max( 0.0, cVPUptMaxElod * uFunTmProdElod * (cPDElodMax-rPDElod&
		&) / (cPDElodMax-cPDElodMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptElodW = sPO4W * aVPUptMaxCrElod / (aVPUptMaxCrElod / cAffPUptElod + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptElodW = aVPUptElodW * (aDSubElod + aDFloatElod)
! 	P_uptake_rate_by_roots
		aVPUptElodS = oPO4S * aVPUptMaxCrElod / (aVPUptMaxCrElod / cAffPUptElod + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptElodS = aVPUptElodS * aDRootElod
! 	total_P_uptake_by_vegetation
		tPUptElod = tPUptElodW + tPUptElodS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrElod = max( 0.0, cVNUptMaxElod * uFunTmProdElod * (cNDElodMax - rNDEl&
		&od) / (cNDElodMax - cNDElodMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptElod = aVNUptMaxCrElod / cAffNUptElod
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptElodW = oNDissW * aVNUptMaxCrElod / (ahNUptElod + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptElodW = aVNUptElodW * (aDSubElod + aDFloatElod)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptElodW = sNH4W * sNO3W / ((ahNUptElod + sNH4W) * (ahNUptElod + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptElod / ((sNH4W + sNO3W +NearZero) * (ahNUptElod + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4ElodW = afNH4UptElodW * tNUptElodW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3ElodW = tNUptElodW - tNUptNH4ElodW
! 	N_uptake_rate_by_roots
		aVNUptElodS = oNDissS * aVNUptMaxCrElod / (ahNUptElod + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptElodS = aVNUptElodS * aDRootElod
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptElodS = oNH4S * oNO3S / ((ahNUptElod + oNH4S +NearZero) * (ahNUptElod + &
		&oNO3S +NearZero)) + oNH4S * ahNUptElod / ((oNH4S + oNO3S+NearZero) * (ahNUptElod&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4ElodS = afNH4UptElodS * tNUptElodS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3ElodS = tNUptElodS - tNUptNH4ElodS
! 	total_N_uptake_by_vegetation
		tNUptElod = tNUptElodW + tNUptElodS
! 	light_at_top_of_vegetation_layer
		aLPAR1Elod = uLPAR0 * exp(- aExtCoefOpen * aDepth1Elod)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Elod = aLPAR1Elod * exp(- aExtCoef * (aDepth2Elod - aDepth1Elod))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLElod = hLRefElod * uFunTmProdElod
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootElod = fEmergElod + fFloatElod * (1.0 - afCovEmergVeg) + bfSubElod * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Elod / uhLE&
		&lod) / (1.0 + aLPAR2Elod / uhLElod))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLElod = ufDay * bfShootElod * aLLimShootElod * uFunTmProdElod * cMuMaxElod
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimElod = max(0.0, (1.0 - cPDElodMin / (rPDElod + NearZero)) * cPDElodMax / (c&
		&PDElodMax - cPDElodMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimElod = max(0.0, (1.0 - cNDElodMin / (rNDElod + NearZero)) * cNDElodMax / (c&
		&NDElodMax - cNDElodMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimElod = min( aPLimElod, aNLimElod)
! 	actual_growth_rate_of_vegetation
		aMuElod = aMuTmLElod * aNutLimElod
! 	mortality_of_plant_species
		if (Day < cDayWinElod) then
		bkMortElod = kMortElodSum 		else if (Day < cDayWinElod + cLengMort) then		bkMortElod = - log(fWinElod) / cLengMort 		else		bkMortElod = kMortElodSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrElod = aMuTmLElod - kDRespElod * uFunTmRespElod - bkMortElod
! 	logistic_correction_of_vegetation_biomass
		tDEnvElod = max(0.0, akDIncrElod / (cDCarrElod+NearZero) * sDElod*sDElod)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdElod = aMuElod / cMuMaxElod * tDEnvElod
! 	vegetation_growth_rate
		tDProdElod = max(0.0, aMuElod * sDElod - tDEnvProdElod)
! 	growth_rate_of_submerged_parts
		tDProdSubElod = bfSubElod * tDProdElod
! 	dark_respiration_rate_of_vegetation
		tDRespElod = kDRespElod * uFunTmRespElod * sDElod
! 	logistic_correction_of_mortality
		tDEnvMortElod = tDEnvElod - tDEnvProdElod
! 	vegetation_mortality_rate
		tDMortElod = bkMortElod * sDElod + tDEnvMortElod
! 	mortality_flux_becoming_water_detritus
		tDMortElodW = fDetWMortElod * (1.0 - bfRootElod) * tDMortElod
! 	mortality_flux_becoming_sediment_detritus
		tDMortElodS = tDMortElod - tDMortElodW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazElodBird = cPrefElodBird * sDElod / (hDVegBird + sDElod) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazElodBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManElod = -log(1.0 - fManElod) / cLengMan 		else		bkManElod = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManElod = bkManElod * sDElod
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManElod = rPDElod * tDManElod
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManElod = rNDElod * tDManElod
! 	derivative_of_vegetation_DW
		tDBedElod = tDMigrElod + tDProdElod - tDRespElod - tDMortElod - tDGrazElodBird -&
		& tDManElod
! 	O2_production_by_vegetation
		tO2ProdElod = molO2molC * cCPerDW * tDProdElod
! 	O2_respiration_of_submerged_part
		tO2RespElodW = molO2molC * cCPerDW * bfSubElod * tDRespElod * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespElodS = molO2molC * cCPerDW * bfRootElod * tDRespElod * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdElodS = min (tO2RespElodS, tO2ProdElod)
! 	O2_production_by_submerged_part
		tO2ProdElodW = min( tO2ProdElod - tO2ProdElodS, bfSubElod * tO2ProdElod)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3ElodW = O2PerNO3 * molO2molN * bfSubElod * tNUptNO3ElodW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3ElodS = O2PerNO3 * molO2molN * tNUptNO3ElodS
! 	P_excretion_by_vegetation
		tPExcrElod = (2.0*rPDElod) / (cPDElodMax + rPDElod) * rPDElod * tDRespElod
! 	P_excretion_by_rooted_part
		tPExcrElodS = bfRootElod * tPExcrElod
! 	P_excretion_by_submerged_part
		tPExcrElodW = tPExcrElod - tPExcrElodS
! 	P_associated_with_mortality_of_vegetation
		tPMortElod = rPDElod * tDMortElod
! 	PO4_associated_with_mortality_of_vegetation
		tPMortElodPO4 = fDissMortVeg * tPMortElod
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortElodPO4S = bfRootElod * tPMortElodPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortElodPO4W = tPMortElodPO4 - tPMortElodPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortElodDet = tPMortElod - tPMortElodPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortElodDetW = fDetWMortElod * (1.0 - bfRootElod) * tPMortElodDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortElodDetS = tPMortElodDet - tPMortElodDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazElodBird = rPDElod * tDGrazElodBird
! 	derivative_of_vegetation_P
		tPBedElod = tPMigrElod + tPUptElod - tPExcrElod - tPMortElod - tPGrazElodBird - &
		&tPManElod
! 	N_excretion_by_vegetation
		tNExcrElod = (2.0*rNDElod) / (cNDElodMax + rNDElod) * rNDElod * tDRespElod
! 	N_excretion_by_rooted_part
		tNExcrElodS = bfRootElod * tNExcrElod
! 	N_excretion_by_submerged_part
		tNExcrElodW = tNExcrElod - tNExcrElodS
! 	N_associated_with_mortality_of_vegetation
		tNMortElod = rNDElod * tDMortElod
! 	NH4_associated_with_mortality_of_vegetation
		tNMortElodNH4 = fDissMortVeg * tNMortElod
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortElodNH4S = bfRootElod * tNMortElodNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortElodNH4W = tNMortElodNH4 - tNMortElodNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortElodDet = tNMortElod - tNMortElodNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortElodDetW = fDetWMortElod * (1.0 - bfRootElod) * tNMortElodDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortElodDetS = tNMortElodDet - tNMortElodDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazElodBird = rNDElod * tDGrazElodBird
! 	derivative_of_vegetation_N
		tNBedElod = tNMigrElod + tNUptElod - tNExcrElod - tNMortElod - tNGrazElodBird - &
		&tNManElod
! 	P/DW_ratio_of_vegetation
		rPDChar = sPChar / (sDChar+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDChar = sNChar / (sDChar+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrChar = kMigrChar * (cDCharIn - sDChar)
! 	migration_flux_of_vegetation_P
		tPMigrChar = kMigrChar * (cPDChar0* cDCharIn - sPChar)
! 	migration_flux_of_vegetation_N
		tNMigrChar = kMigrChar * (cNDChar0* cDCharIn - sNChar)
! 	temperature_function_of_vegetation_production
		uFunTmProdChar = ( (cQ10ProdChar )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespChar = ( (cQ10RespChar )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrChar = max( 0.0, cVPUptMaxChar * uFunTmProdChar * (cPDCharMax-rPDChar&
		&) / (cPDCharMax-cPDCharMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptCharW = sPO4W * aVPUptMaxCrChar / (aVPUptMaxCrChar / cAffPUptChar + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptCharW = aVPUptCharW * (aDSubChar + aDFloatChar)
! 	P_uptake_rate_by_roots
		aVPUptCharS = oPO4S * aVPUptMaxCrChar / (aVPUptMaxCrChar / cAffPUptChar + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptCharS = aVPUptCharS * aDRootChar
! 	total_P_uptake_by_vegetation
		tPUptChar = tPUptCharW + tPUptCharS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrChar = max( 0.0, cVNUptMaxChar * uFunTmProdChar * (cNDCharMax - rNDCh&
		&ar) / (cNDCharMax - cNDCharMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptChar = aVNUptMaxCrChar / cAffNUptChar
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptCharW = oNDissW * aVNUptMaxCrChar / (ahNUptChar + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptCharW = aVNUptCharW * (aDSubChar + aDFloatChar)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptCharW = sNH4W * sNO3W / ((ahNUptChar + sNH4W) * (ahNUptChar + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptChar / ((sNH4W + sNO3W +NearZero) * (ahNUptChar + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4CharW = afNH4UptCharW * tNUptCharW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3CharW = tNUptCharW - tNUptNH4CharW
! 	N_uptake_rate_by_roots
		aVNUptCharS = oNDissS * aVNUptMaxCrChar / (ahNUptChar + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptCharS = aVNUptCharS * aDRootChar
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptCharS = oNH4S * oNO3S / ((ahNUptChar + oNH4S +NearZero) * (ahNUptChar + &
		&oNO3S +NearZero)) + oNH4S * ahNUptChar / ((oNH4S + oNO3S+NearZero) * (ahNUptChar&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4CharS = afNH4UptCharS * tNUptCharS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3CharS = tNUptCharS - tNUptNH4CharS
! 	total_N_uptake_by_vegetation
		tNUptChar = tNUptCharW + tNUptCharS
! 	light_at_top_of_vegetation_layer
		aLPAR1Char = uLPAR0 * exp(- aExtCoefOpen * aDepth1Char)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Char = aLPAR1Char * exp(- aExtCoef * (aDepth2Char - aDepth1Char))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLChar = hLRefChar * uFunTmProdChar
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootChar = fEmergChar + fFloatChar * (1.0 - afCovEmergVeg) + bfSubChar * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Char / uhLC&
		&har) / (1.0 + aLPAR2Char / uhLChar))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLChar = ufDay * bfShootChar * aLLimShootChar * uFunTmProdChar * cMuMaxChar
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimChar = max(0.0, (1.0 - cPDCharMin / (rPDChar + NearZero)) * cPDCharMax / (c&
		&PDCharMax - cPDCharMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimChar = max(0.0, (1.0 - cNDCharMin / (rNDChar + NearZero)) * cNDCharMax / (c&
		&NDCharMax - cNDCharMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimChar = min( aPLimChar, aNLimChar)
! 	actual_growth_rate_of_vegetation
		aMuChar = aMuTmLChar * aNutLimChar
! 	mortality_of_plant_species
		if (Day < cDayWinChar) then
		bkMortChar = kMortCharSum 		else if (Day < cDayWinChar + cLengMort) then		bkMortChar = - log(fWinChar) / cLengMort 		else		bkMortChar = kMortCharSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrChar = aMuTmLChar - kDRespChar * uFunTmRespChar - bkMortChar
! 	logistic_correction_of_vegetation_biomass
		tDEnvChar = max(0.0, akDIncrChar / (cDCarrChar+NearZero) * sDChar*sDChar)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdChar = aMuChar / cMuMaxChar * tDEnvChar
! 	vegetation_growth_rate
		tDProdChar = max(0.0, aMuChar * sDChar - tDEnvProdChar)
! 	growth_rate_of_submerged_parts
		tDProdSubChar = bfSubChar * tDProdChar
! 	dark_respiration_rate_of_vegetation
		tDRespChar = kDRespChar * uFunTmRespChar * sDChar
! 	logistic_correction_of_mortality
		tDEnvMortChar = tDEnvChar - tDEnvProdChar
! 	vegetation_mortality_rate
		tDMortChar = bkMortChar * sDChar + tDEnvMortChar
! 	mortality_flux_becoming_water_detritus
		tDMortCharW = fDetWMortChar * (1.0 - bfRootChar) * tDMortChar
! 	mortality_flux_becoming_sediment_detritus
		tDMortCharS = tDMortChar - tDMortCharW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazCharBird = cPrefCharBird * sDChar / (hDVegBird + sDChar) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazCharBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManChar = -log(1.0 - fManChar) / cLengMan 		else		bkManChar = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManChar = bkManChar * sDChar
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManChar = rPDChar * tDManChar
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManChar = rNDChar * tDManChar
! 	derivative_of_vegetation_DW
		tDBedChar = tDMigrChar + tDProdChar - tDRespChar - tDMortChar - tDGrazCharBird -&
		& tDManChar
! 	O2_production_by_vegetation
		tO2ProdChar = molO2molC * cCPerDW * tDProdChar
! 	O2_respiration_of_submerged_part
		tO2RespCharW = molO2molC * cCPerDW * bfSubChar * tDRespChar * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespCharS = molO2molC * cCPerDW * bfRootChar * tDRespChar * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdCharS = min (tO2RespCharS, tO2ProdChar)
! 	O2_production_by_submerged_part
		tO2ProdCharW = min( tO2ProdChar - tO2ProdCharS, bfSubChar * tO2ProdChar)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3CharW = O2PerNO3 * molO2molN * bfSubChar * tNUptNO3CharW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3CharS = O2PerNO3 * molO2molN * tNUptNO3CharS
! 	P_excretion_by_vegetation
		tPExcrChar = (2.0*rPDChar) / (cPDCharMax + rPDChar) * rPDChar * tDRespChar
! 	P_excretion_by_rooted_part
		tPExcrCharS = bfRootChar * tPExcrChar
! 	P_excretion_by_submerged_part
		tPExcrCharW = tPExcrChar - tPExcrCharS
! 	P_associated_with_mortality_of_vegetation
		tPMortChar = rPDChar * tDMortChar
! 	PO4_associated_with_mortality_of_vegetation
		tPMortCharPO4 = fDissMortVeg * tPMortChar
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortCharPO4S = bfRootChar * tPMortCharPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortCharPO4W = tPMortCharPO4 - tPMortCharPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortCharDet = tPMortChar - tPMortCharPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortCharDetW = fDetWMortChar * (1.0 - bfRootChar) * tPMortCharDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortCharDetS = tPMortCharDet - tPMortCharDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazCharBird = rPDChar * tDGrazCharBird
! 	derivative_of_vegetation_P
		tPBedChar = tPMigrChar + tPUptChar - tPExcrChar - tPMortChar - tPGrazCharBird - &
		&tPManChar
! 	N_excretion_by_vegetation
		tNExcrChar = (2.0*rNDChar) / (cNDCharMax + rNDChar) * rNDChar * tDRespChar
! 	N_excretion_by_rooted_part
		tNExcrCharS = bfRootChar * tNExcrChar
! 	N_excretion_by_submerged_part
		tNExcrCharW = tNExcrChar - tNExcrCharS
! 	N_associated_with_mortality_of_vegetation
		tNMortChar = rNDChar * tDMortChar
! 	NH4_associated_with_mortality_of_vegetation
		tNMortCharNH4 = fDissMortVeg * tNMortChar
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortCharNH4S = bfRootChar * tNMortCharNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortCharNH4W = tNMortCharNH4 - tNMortCharNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortCharDet = tNMortChar - tNMortCharNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortCharDetW = fDetWMortChar * (1.0 - bfRootChar) * tNMortCharDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortCharDetS = tNMortCharDet - tNMortCharDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazCharBird = rNDChar * tDGrazCharBird
! 	derivative_of_vegetation_N
		tNBedChar = tNMigrChar + tNUptChar - tNExcrChar - tNMortChar - tNGrazCharBird - &
		&tNManChar
! 	P/DW_ratio_of_vegetation
		rPDCera = sPCera / (sDCera+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDCera = sNCera / (sDCera+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrCera = kMigrCera * (cDCeraIn - sDCera)
! 	migration_flux_of_vegetation_P
		tPMigrCera = kMigrCera * (cPDCera0* cDCeraIn - sPCera)
! 	migration_flux_of_vegetation_N
		tNMigrCera = kMigrCera * (cNDCera0* cDCeraIn - sNCera)
! 	temperature_function_of_vegetation_production
		uFunTmProdCera = ( (cQ10ProdCera )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespCera = ( (cQ10RespCera )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrCera = max( 0.0, cVPUptMaxCera * uFunTmProdCera * (cPDCeraMax-rPDCera&
		&) / (cPDCeraMax-cPDCeraMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptCeraW = sPO4W * aVPUptMaxCrCera / (aVPUptMaxCrCera / cAffPUptCera + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptCeraW = aVPUptCeraW * (aDSubCera + aDFloatCera)
! 	P_uptake_rate_by_roots
		aVPUptCeraS = oPO4S * aVPUptMaxCrCera / (aVPUptMaxCrCera / cAffPUptCera + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptCeraS = aVPUptCeraS * aDRootCera
! 	total_P_uptake_by_vegetation
		tPUptCera = tPUptCeraW + tPUptCeraS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrCera = max( 0.0, cVNUptMaxCera * uFunTmProdCera * (cNDCeraMax - rNDCe&
		&ra) / (cNDCeraMax - cNDCeraMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptCera = aVNUptMaxCrCera / cAffNUptCera
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptCeraW = oNDissW * aVNUptMaxCrCera / (ahNUptCera + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptCeraW = aVNUptCeraW * (aDSubCera + aDFloatCera)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptCeraW = sNH4W * sNO3W / ((ahNUptCera + sNH4W) * (ahNUptCera + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptCera / ((sNH4W + sNO3W +NearZero) * (ahNUptCera + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4CeraW = afNH4UptCeraW * tNUptCeraW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3CeraW = tNUptCeraW - tNUptNH4CeraW
! 	N_uptake_rate_by_roots
		aVNUptCeraS = oNDissS * aVNUptMaxCrCera / (ahNUptCera + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptCeraS = aVNUptCeraS * aDRootCera
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptCeraS = oNH4S * oNO3S / ((ahNUptCera + oNH4S +NearZero) * (ahNUptCera + &
		&oNO3S +NearZero)) + oNH4S * ahNUptCera / ((oNH4S + oNO3S+NearZero) * (ahNUptCera&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4CeraS = afNH4UptCeraS * tNUptCeraS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3CeraS = tNUptCeraS - tNUptNH4CeraS
! 	total_N_uptake_by_vegetation
		tNUptCera = tNUptCeraW + tNUptCeraS
! 	light_at_top_of_vegetation_layer
		aLPAR1Cera = uLPAR0 * exp(- aExtCoefOpen * aDepth1Cera)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Cera = aLPAR1Cera * exp(- aExtCoef * (aDepth2Cera - aDepth1Cera))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLCera = hLRefCera * uFunTmProdCera
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootCera = fEmergCera + fFloatCera * (1.0 - afCovEmergVeg) + bfSubCera * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Cera / uhLC&
		&era) / (1.0 + aLPAR2Cera / uhLCera))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLCera = ufDay * bfShootCera * aLLimShootCera * uFunTmProdCera * cMuMaxCera
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimCera = max(0.0, (1.0 - cPDCeraMin / (rPDCera + NearZero)) * cPDCeraMax / (c&
		&PDCeraMax - cPDCeraMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimCera = max(0.0, (1.0 - cNDCeraMin / (rNDCera + NearZero)) * cNDCeraMax / (c&
		&NDCeraMax - cNDCeraMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimCera = min( aPLimCera, aNLimCera)
! 	actual_growth_rate_of_vegetation
		aMuCera = aMuTmLCera * aNutLimCera
! 	mortality_of_plant_species
		if (Day < cDayWinCera) then
		bkMortCera = kMortCeraSum 		else if (Day < cDayWinCera + cLengMort) then		bkMortCera = - log(fWinCera) / cLengMort 		else		bkMortCera = kMortCeraSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrCera = aMuTmLCera - kDRespCera * uFunTmRespCera - bkMortCera
! 	logistic_correction_of_vegetation_biomass
		tDEnvCera = max(0.0, akDIncrCera / (cDCarrCera+NearZero) * sDCera*sDCera)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdCera = aMuCera / cMuMaxCera * tDEnvCera
! 	vegetation_growth_rate
		tDProdCera = max(0.0, aMuCera * sDCera - tDEnvProdCera)
! 	growth_rate_of_submerged_parts
		tDProdSubCera = bfSubCera * tDProdCera
! 	dark_respiration_rate_of_vegetation
		tDRespCera = kDRespCera * uFunTmRespCera * sDCera
! 	logistic_correction_of_mortality
		tDEnvMortCera = tDEnvCera - tDEnvProdCera
! 	vegetation_mortality_rate
		tDMortCera = bkMortCera * sDCera + tDEnvMortCera
! 	mortality_flux_becoming_water_detritus
		tDMortCeraW = fDetWMortCera * (1.0 - bfRootCera) * tDMortCera
! 	mortality_flux_becoming_sediment_detritus
		tDMortCeraS = tDMortCera - tDMortCeraW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazCeraBird = cPrefCeraBird * sDCera / (hDVegBird + sDCera) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazCeraBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManCera = -log(1.0 - fManCera) / cLengMan 		else		bkManCera = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManCera = bkManCera * sDCera
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManCera = rPDCera * tDManCera
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManCera = rNDCera * tDManCera
! 	derivative_of_vegetation_DW
		tDBedCera = tDMigrCera + tDProdCera - tDRespCera - tDMortCera - tDGrazCeraBird -&
		& tDManCera
! 	O2_production_by_vegetation
		tO2ProdCera = molO2molC * cCPerDW * tDProdCera
! 	O2_respiration_of_submerged_part
		tO2RespCeraW = molO2molC * cCPerDW * bfSubCera * tDRespCera * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespCeraS = molO2molC * cCPerDW * bfRootCera * tDRespCera * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdCeraS = min (tO2RespCeraS, tO2ProdCera)
! 	O2_production_by_submerged_part
		tO2ProdCeraW = min( tO2ProdCera - tO2ProdCeraS, bfSubCera * tO2ProdCera)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3CeraW = O2PerNO3 * molO2molN * bfSubCera * tNUptNO3CeraW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3CeraS = O2PerNO3 * molO2molN * tNUptNO3CeraS
! 	P_excretion_by_vegetation
		tPExcrCera = (2.0*rPDCera) / (cPDCeraMax + rPDCera) * rPDCera * tDRespCera
! 	P_excretion_by_rooted_part
		tPExcrCeraS = bfRootCera * tPExcrCera
! 	P_excretion_by_submerged_part
		tPExcrCeraW = tPExcrCera - tPExcrCeraS
! 	P_associated_with_mortality_of_vegetation
		tPMortCera = rPDCera * tDMortCera
! 	PO4_associated_with_mortality_of_vegetation
		tPMortCeraPO4 = fDissMortVeg * tPMortCera
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortCeraPO4S = bfRootCera * tPMortCeraPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortCeraPO4W = tPMortCeraPO4 - tPMortCeraPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortCeraDet = tPMortCera - tPMortCeraPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortCeraDetW = fDetWMortCera * (1.0 - bfRootCera) * tPMortCeraDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortCeraDetS = tPMortCeraDet - tPMortCeraDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazCeraBird = rPDCera * tDGrazCeraBird
! 	derivative_of_vegetation_P
		tPBedCera = tPMigrCera + tPUptCera - tPExcrCera - tPMortCera - tPGrazCeraBird - &
		&tPManCera
! 	N_excretion_by_vegetation
		tNExcrCera = (2.0*rNDCera) / (cNDCeraMax + rNDCera) * rNDCera * tDRespCera
! 	N_excretion_by_rooted_part
		tNExcrCeraS = bfRootCera * tNExcrCera
! 	N_excretion_by_submerged_part
		tNExcrCeraW = tNExcrCera - tNExcrCeraS
! 	N_associated_with_mortality_of_vegetation
		tNMortCera = rNDCera * tDMortCera
! 	NH4_associated_with_mortality_of_vegetation
		tNMortCeraNH4 = fDissMortVeg * tNMortCera
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortCeraNH4S = bfRootCera * tNMortCeraNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortCeraNH4W = tNMortCeraNH4 - tNMortCeraNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortCeraDet = tNMortCera - tNMortCeraNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortCeraDetW = fDetWMortCera * (1.0 - bfRootCera) * tNMortCeraDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortCeraDetS = tNMortCeraDet - tNMortCeraDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazCeraBird = rNDCera * tDGrazCeraBird
! 	derivative_of_vegetation_N
		tNBedCera = tNMigrCera + tNUptCera - tNExcrCera - tNMortCera - tNGrazCeraBird - &
		&tNManCera
! 	P/DW_ratio_of_vegetation
		rPDLemn = sPLemn / (sDLemn+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDLemn = sNLemn / (sDLemn+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrLemn = kMigrLemn * (cDLemnIn - sDLemn)
! 	migration_flux_of_vegetation_P
		tPMigrLemn = kMigrLemn * (cPDLemn0* cDLemnIn - sPLemn)
! 	migration_flux_of_vegetation_N
		tNMigrLemn = kMigrLemn * (cNDLemn0* cDLemnIn - sNLemn)
! 	temperature_function_of_vegetation_production
		uFunTmProdLemn = ( (cQ10ProdLemn )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespLemn = ( (cQ10RespLemn )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrLemn = max( 0.0, cVPUptMaxLemn * uFunTmProdLemn * (cPDLemnMax-rPDLemn&
		&) / (cPDLemnMax-cPDLemnMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptLemnW = sPO4W * aVPUptMaxCrLemn / (aVPUptMaxCrLemn / cAffPUptLemn + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptLemnW = aVPUptLemnW * (aDSubLemn + aDFloatLemn)
! 	P_uptake_rate_by_roots
		aVPUptLemnS = oPO4S * aVPUptMaxCrLemn / (aVPUptMaxCrLemn / cAffPUptLemn + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptLemnS = aVPUptLemnS * aDRootLemn
! 	total_P_uptake_by_vegetation
		tPUptLemn = tPUptLemnW + tPUptLemnS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrLemn = max( 0.0, cVNUptMaxLemn * uFunTmProdLemn * (cNDLemnMax - rNDLe&
		&mn) / (cNDLemnMax - cNDLemnMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptLemn = aVNUptMaxCrLemn / cAffNUptLemn
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptLemnW = oNDissW * aVNUptMaxCrLemn / (ahNUptLemn + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptLemnW = aVNUptLemnW * (aDSubLemn + aDFloatLemn)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptLemnW = sNH4W * sNO3W / ((ahNUptLemn + sNH4W) * (ahNUptLemn + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptLemn / ((sNH4W + sNO3W +NearZero) * (ahNUptLemn + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4LemnW = afNH4UptLemnW * tNUptLemnW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3LemnW = tNUptLemnW - tNUptNH4LemnW
! 	N_uptake_rate_by_roots
		aVNUptLemnS = oNDissS * aVNUptMaxCrLemn / (ahNUptLemn + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptLemnS = aVNUptLemnS * aDRootLemn
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptLemnS = oNH4S * oNO3S / ((ahNUptLemn + oNH4S +NearZero) * (ahNUptLemn + &
		&oNO3S +NearZero)) + oNH4S * ahNUptLemn / ((oNH4S + oNO3S+NearZero) * (ahNUptLemn&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4LemnS = afNH4UptLemnS * tNUptLemnS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3LemnS = tNUptLemnS - tNUptNH4LemnS
! 	total_N_uptake_by_vegetation
		tNUptLemn = tNUptLemnW + tNUptLemnS
! 	light_at_top_of_vegetation_layer
		aLPAR1Lemn = uLPAR0 * exp(- aExtCoefOpen * aDepth1Lemn)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Lemn = aLPAR1Lemn * exp(- aExtCoef * (aDepth2Lemn - aDepth1Lemn))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLLemn = hLRefLemn * uFunTmProdLemn
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootLemn = fEmergLemn + fFloatLemn * (1.0 - afCovEmergVeg) + bfSubLemn * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Lemn / uhLL&
		&emn) / (1.0 + aLPAR2Lemn / uhLLemn))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLLemn = ufDay * bfShootLemn * aLLimShootLemn * uFunTmProdLemn * cMuMaxLemn
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimLemn = max(0.0, (1.0 - cPDLemnMin / (rPDLemn + NearZero)) * cPDLemnMax / (c&
		&PDLemnMax - cPDLemnMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimLemn = max(0.0, (1.0 - cNDLemnMin / (rNDLemn + NearZero)) * cNDLemnMax / (c&
		&NDLemnMax - cNDLemnMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimLemn = min( aPLimLemn, aNLimLemn)
! 	actual_growth_rate_of_vegetation
		aMuLemn = aMuTmLLemn * aNutLimLemn
! 	mortality_of_plant_species
		if (Day < cDayWinLemn) then
		bkMortLemn = kMortLemnSum 		else if (Day < cDayWinLemn + cLengMort) then		bkMortLemn = - log(fWinLemn) / cLengMort 		else		bkMortLemn = kMortLemnSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrLemn = aMuTmLLemn - kDRespLemn * uFunTmRespLemn - bkMortLemn
! 	logistic_correction_of_vegetation_biomass
		tDEnvLemn = max(0.0, akDIncrLemn / (cDCarrLemn+NearZero) * sDLemn*sDLemn)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdLemn = aMuLemn / cMuMaxLemn * tDEnvLemn
! 	vegetation_growth_rate
		tDProdLemn = max(0.0, aMuLemn * sDLemn - tDEnvProdLemn)
! 	growth_rate_of_submerged_parts
		tDProdSubLemn = bfSubLemn * tDProdLemn
! 	dark_respiration_rate_of_vegetation
		tDRespLemn = kDRespLemn * uFunTmRespLemn * sDLemn
! 	logistic_correction_of_mortality
		tDEnvMortLemn = tDEnvLemn - tDEnvProdLemn
! 	vegetation_mortality_rate
		tDMortLemn = bkMortLemn * sDLemn + tDEnvMortLemn
! 	mortality_flux_becoming_water_detritus
		tDMortLemnW = fDetWMortLemn * (1.0 - bfRootLemn) * tDMortLemn
! 	mortality_flux_becoming_sediment_detritus
		tDMortLemnS = tDMortLemn - tDMortLemnW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazLemnBird = cPrefLemnBird * sDLemn / (hDVegBird + sDLemn) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazLemnBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManLemn = -log(1.0 - fManLemn) / cLengMan 		else		bkManLemn = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManLemn = bkManLemn * sDLemn
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManLemn = rPDLemn * tDManLemn
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManLemn = rNDLemn * tDManLemn
! 	derivative_of_vegetation_DW
		tDBedLemn = tDMigrLemn + tDProdLemn - tDRespLemn - tDMortLemn - tDGrazLemnBird -&
		& tDManLemn
! 	O2_production_by_vegetation
		tO2ProdLemn = molO2molC * cCPerDW * tDProdLemn
! 	O2_respiration_of_submerged_part
		tO2RespLemnW = molO2molC * cCPerDW * bfSubLemn * tDRespLemn * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespLemnS = molO2molC * cCPerDW * bfRootLemn * tDRespLemn * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdLemnS = min (tO2RespLemnS, tO2ProdLemn)
! 	O2_production_by_submerged_part
		tO2ProdLemnW = min( tO2ProdLemn - tO2ProdLemnS, bfSubLemn * tO2ProdLemn)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3LemnW = O2PerNO3 * molO2molN * bfSubLemn * tNUptNO3LemnW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3LemnS = O2PerNO3 * molO2molN * tNUptNO3LemnS
! 	P_excretion_by_vegetation
		tPExcrLemn = (2.0*rPDLemn) / (cPDLemnMax + rPDLemn) * rPDLemn * tDRespLemn
! 	P_excretion_by_rooted_part
		tPExcrLemnS = bfRootLemn * tPExcrLemn
! 	P_excretion_by_submerged_part
		tPExcrLemnW = tPExcrLemn - tPExcrLemnS
! 	P_associated_with_mortality_of_vegetation
		tPMortLemn = rPDLemn * tDMortLemn
! 	PO4_associated_with_mortality_of_vegetation
		tPMortLemnPO4 = fDissMortVeg * tPMortLemn
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortLemnPO4S = bfRootLemn * tPMortLemnPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortLemnPO4W = tPMortLemnPO4 - tPMortLemnPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortLemnDet = tPMortLemn - tPMortLemnPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortLemnDetW = fDetWMortLemn * (1.0 - bfRootLemn) * tPMortLemnDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortLemnDetS = tPMortLemnDet - tPMortLemnDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazLemnBird = rPDLemn * tDGrazLemnBird
! 	derivative_of_vegetation_P
		tPBedLemn = tPMigrLemn + tPUptLemn - tPExcrLemn - tPMortLemn - tPGrazLemnBird - &
		&tPManLemn
! 	N_excretion_by_vegetation
		tNExcrLemn = (2.0*rNDLemn) / (cNDLemnMax + rNDLemn) * rNDLemn * tDRespLemn
! 	N_excretion_by_rooted_part
		tNExcrLemnS = bfRootLemn * tNExcrLemn
! 	N_excretion_by_submerged_part
		tNExcrLemnW = tNExcrLemn - tNExcrLemnS
! 	N_associated_with_mortality_of_vegetation
		tNMortLemn = rNDLemn * tDMortLemn
! 	NH4_associated_with_mortality_of_vegetation
		tNMortLemnNH4 = fDissMortVeg * tNMortLemn
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortLemnNH4S = bfRootLemn * tNMortLemnNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortLemnNH4W = tNMortLemnNH4 - tNMortLemnNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortLemnDet = tNMortLemn - tNMortLemnNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortLemnDetW = fDetWMortLemn * (1.0 - bfRootLemn) * tNMortLemnDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortLemnDetS = tNMortLemnDet - tNMortLemnDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazLemnBird = rNDLemn * tDGrazLemnBird
! 	derivative_of_vegetation_N
		tNBedLemn = tNMigrLemn + tNUptLemn - tNExcrLemn - tNMortLemn - tNGrazLemnBird - &
		&tNManLemn
! 	P/DW_ratio_of_vegetation
		rPDNymp = sPNymp / (sDNymp+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDNymp = sNNymp / (sDNymp+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrNymp = kMigrNymp * (cDNympIn - sDNymp)
! 	migration_flux_of_vegetation_P
		tPMigrNymp = kMigrNymp * (cPDNymp0* cDNympIn - sPNymp)
! 	migration_flux_of_vegetation_N
		tNMigrNymp = kMigrNymp * (cNDNymp0* cDNympIn - sNNymp)
! 	temperature_function_of_vegetation_production
		uFunTmProdNymp = ( (cQ10ProdNymp )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespNymp = ( (cQ10RespNymp )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrNymp = max( 0.0, cVPUptMaxNymp * uFunTmProdNymp * (cPDNympMax-rPDNymp&
		&) / (cPDNympMax-cPDNympMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptNympW = sPO4W * aVPUptMaxCrNymp / (aVPUptMaxCrNymp / cAffPUptNymp + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptNympW = aVPUptNympW * (aDSubNymp + aDFloatNymp)
! 	P_uptake_rate_by_roots
		aVPUptNympS = oPO4S * aVPUptMaxCrNymp / (aVPUptMaxCrNymp / cAffPUptNymp + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptNympS = aVPUptNympS * aDRootNymp
! 	total_P_uptake_by_vegetation
		tPUptNymp = tPUptNympW + tPUptNympS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrNymp = max( 0.0, cVNUptMaxNymp * uFunTmProdNymp * (cNDNympMax - rNDNy&
		&mp) / (cNDNympMax - cNDNympMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptNymp = aVNUptMaxCrNymp / cAffNUptNymp
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptNympW = oNDissW * aVNUptMaxCrNymp / (ahNUptNymp + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptNympW = aVNUptNympW * (aDSubNymp + aDFloatNymp)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptNympW = sNH4W * sNO3W / ((ahNUptNymp + sNH4W) * (ahNUptNymp + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptNymp / ((sNH4W + sNO3W +NearZero) * (ahNUptNymp + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4NympW = afNH4UptNympW * tNUptNympW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3NympW = tNUptNympW - tNUptNH4NympW
! 	N_uptake_rate_by_roots
		aVNUptNympS = oNDissS * aVNUptMaxCrNymp / (ahNUptNymp + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptNympS = aVNUptNympS * aDRootNymp
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptNympS = oNH4S * oNO3S / ((ahNUptNymp + oNH4S +NearZero) * (ahNUptNymp + &
		&oNO3S +NearZero)) + oNH4S * ahNUptNymp / ((oNH4S + oNO3S+NearZero) * (ahNUptNymp&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4NympS = afNH4UptNympS * tNUptNympS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3NympS = tNUptNympS - tNUptNH4NympS
! 	total_N_uptake_by_vegetation
		tNUptNymp = tNUptNympW + tNUptNympS
! 	light_at_top_of_vegetation_layer
		aLPAR1Nymp = uLPAR0 * exp(- aExtCoefOpen * aDepth1Nymp)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Nymp = aLPAR1Nymp * exp(- aExtCoef * (aDepth2Nymp - aDepth1Nymp))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLNymp = hLRefNymp * uFunTmProdNymp
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootNymp = fEmergNymp + fFloatNymp * (1.0 - afCovEmergVeg) + bfSubNymp * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Nymp / uhLN&
		&ymp) / (1.0 + aLPAR2Nymp / uhLNymp))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLNymp = ufDay * bfShootNymp * aLLimShootNymp * uFunTmProdNymp * cMuMaxNymp
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimNymp = max(0.0, (1.0 - cPDNympMin / (rPDNymp + NearZero)) * cPDNympMax / (c&
		&PDNympMax - cPDNympMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimNymp = max(0.0, (1.0 - cNDNympMin / (rNDNymp + NearZero)) * cNDNympMax / (c&
		&NDNympMax - cNDNympMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimNymp = min( aPLimNymp, aNLimNymp)
! 	actual_growth_rate_of_vegetation
		aMuNymp = aMuTmLNymp * aNutLimNymp
! 	mortality_of_plant_species
		if (Day < cDayWinNymp) then
		bkMortNymp = kMortNympSum 		else if (Day < cDayWinNymp + cLengMort) then		bkMortNymp = - log(fWinNymp) / cLengMort 		else		bkMortNymp = kMortNympSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrNymp = aMuTmLNymp - kDRespNymp * uFunTmRespNymp - bkMortNymp
! 	logistic_correction_of_vegetation_biomass
		tDEnvNymp = max(0.0, akDIncrNymp / (cDCarrNymp+NearZero) * sDNymp*sDNymp)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdNymp = aMuNymp / cMuMaxNymp * tDEnvNymp
! 	vegetation_growth_rate
		tDProdNymp = max(0.0, aMuNymp * sDNymp - tDEnvProdNymp)
! 	growth_rate_of_submerged_parts
		tDProdSubNymp = bfSubNymp * tDProdNymp
! 	dark_respiration_rate_of_vegetation
		tDRespNymp = kDRespNymp * uFunTmRespNymp * sDNymp
! 	logistic_correction_of_mortality
		tDEnvMortNymp = tDEnvNymp - tDEnvProdNymp
! 	vegetation_mortality_rate
		tDMortNymp = bkMortNymp * sDNymp + tDEnvMortNymp
! 	mortality_flux_becoming_water_detritus
		tDMortNympW = fDetWMortNymp * (1.0 - bfRootNymp) * tDMortNymp
! 	mortality_flux_becoming_sediment_detritus
		tDMortNympS = tDMortNymp - tDMortNympW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazNympBird = cPrefNympBird * sDNymp / (hDVegBird + sDNymp) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazNympBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManNymp = -log(1.0 - fManNymp) / cLengMan 		else		bkManNymp = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManNymp = bkManNymp * sDNymp
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManNymp = rPDNymp * tDManNymp
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManNymp = rNDNymp * tDManNymp
! 	derivative_of_vegetation_DW
		tDBedNymp = tDMigrNymp + tDProdNymp - tDRespNymp - tDMortNymp - tDGrazNympBird -&
		& tDManNymp
! 	O2_production_by_vegetation
		tO2ProdNymp = molO2molC * cCPerDW * tDProdNymp
! 	O2_respiration_of_submerged_part
		tO2RespNympW = molO2molC * cCPerDW * bfSubNymp * tDRespNymp * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespNympS = molO2molC * cCPerDW * bfRootNymp * tDRespNymp * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdNympS = min (tO2RespNympS, tO2ProdNymp)
! 	O2_production_by_submerged_part
		tO2ProdNympW = min( tO2ProdNymp - tO2ProdNympS, bfSubNymp * tO2ProdNymp)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3NympW = O2PerNO3 * molO2molN * bfSubNymp * tNUptNO3NympW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3NympS = O2PerNO3 * molO2molN * tNUptNO3NympS
! 	P_excretion_by_vegetation
		tPExcrNymp = (2.0*rPDNymp) / (cPDNympMax + rPDNymp) * rPDNymp * tDRespNymp
! 	P_excretion_by_rooted_part
		tPExcrNympS = bfRootNymp * tPExcrNymp
! 	P_excretion_by_submerged_part
		tPExcrNympW = tPExcrNymp - tPExcrNympS
! 	P_associated_with_mortality_of_vegetation
		tPMortNymp = rPDNymp * tDMortNymp
! 	PO4_associated_with_mortality_of_vegetation
		tPMortNympPO4 = fDissMortVeg * tPMortNymp
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortNympPO4S = bfRootNymp * tPMortNympPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortNympPO4W = tPMortNympPO4 - tPMortNympPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortNympDet = tPMortNymp - tPMortNympPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortNympDetW = fDetWMortNymp * (1.0 - bfRootNymp) * tPMortNympDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortNympDetS = tPMortNympDet - tPMortNympDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazNympBird = rPDNymp * tDGrazNympBird
! 	derivative_of_vegetation_P
		tPBedNymp = tPMigrNymp + tPUptNymp - tPExcrNymp - tPMortNymp - tPGrazNympBird - &
		&tPManNymp
! 	N_excretion_by_vegetation
		tNExcrNymp = (2.0*rNDNymp) / (cNDNympMax + rNDNymp) * rNDNymp * tDRespNymp
! 	N_excretion_by_rooted_part
		tNExcrNympS = bfRootNymp * tNExcrNymp
! 	N_excretion_by_submerged_part
		tNExcrNympW = tNExcrNymp - tNExcrNympS
! 	N_associated_with_mortality_of_vegetation
		tNMortNymp = rNDNymp * tDMortNymp
! 	NH4_associated_with_mortality_of_vegetation
		tNMortNympNH4 = fDissMortVeg * tNMortNymp
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortNympNH4S = bfRootNymp * tNMortNympNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortNympNH4W = tNMortNympNH4 - tNMortNympNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortNympDet = tNMortNymp - tNMortNympNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortNympDetW = fDetWMortNymp * (1.0 - bfRootNymp) * tNMortNympDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortNympDetS = tNMortNympDet - tNMortNympDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazNympBird = rNDNymp * tDGrazNympBird
! 	derivative_of_vegetation_N
		tNBedNymp = tNMigrNymp + tNUptNymp - tNExcrNymp - tNMortNymp - tNGrazNympBird - &
		&tNManNymp
! 	P/DW_ratio_of_vegetation
		rPDHelo = sPHelo / (sDHelo+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDHelo = sNHelo / (sDHelo+NearZero)
! 	migration_flux_of_vegetation_DW
		tDMigrHelo = kMigrHelo * (cDHeloIn - sDHelo)
! 	migration_flux_of_vegetation_P
		tPMigrHelo = kMigrHelo * (cPDHelo0* cDHeloIn - sPHelo)
! 	migration_flux_of_vegetation_N
		tNMigrHelo = kMigrHelo * (cNDHelo0* cDHeloIn - sNHelo)
! 	temperature_function_of_vegetation_production
		uFunTmProdHelo = ( (cQ10ProdHelo )** (0.1 * (uTm - cTmRef)))
! 	temperature_function_of_vegetation_respiration
		uFunTmRespHelo = ( (cQ10RespHelo )** (0.1 * (uTm - cTmRef)))
! 	maximum_P_uptake_rate_of_vegetation_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrHelo = max( 0.0, cVPUptMaxHelo * uFunTmProdHelo * (cPDHeloMax-rPDHelo&
		&) / (cPDHeloMax-cPDHeloMin) )
! 	P_uptake_rate_by_submerged_and_floating_parts
		aVPUptHeloW = sPO4W * aVPUptMaxCrHelo / (aVPUptMaxCrHelo / cAffPUptHelo + sPO4W)
! 	P_uptake_from_water_by_submerged_and_floating_parts
		tPUptHeloW = aVPUptHeloW * (aDSubHelo + aDFloatHelo)
! 	P_uptake_rate_by_roots
		aVPUptHeloS = oPO4S * aVPUptMaxCrHelo / (aVPUptMaxCrHelo / cAffPUptHelo + oPO4S)
! 	P_uptake_from_interstitial_water_by_rooted_part
		tPUptHeloS = aVPUptHeloS * aDRootHelo
! 	total_P_uptake_by_vegetation
		tPUptHelo = tPUptHeloW + tPUptHeloS
! 	maximum_N_uptake_rate_of_vegetation_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrHelo = max( 0.0, cVNUptMaxHelo * uFunTmProdHelo * (cNDHeloMax - rNDHe&
		&lo) / (cNDHeloMax - cNDHeloMin))
! 	half-saturation_concentration_for_N_uptake
		ahNUptHelo = aVNUptMaxCrHelo / cAffNUptHelo
! 	N_uptake_rate_by_submerged_and_floating_parts
		aVNUptHeloW = oNDissW * aVNUptMaxCrHelo / (ahNUptHelo + oNDissW)
! 	N_uptake_from_water_by_submerged_and_floating_parts
		tNUptHeloW = aVNUptHeloW * (aDSubHelo + aDFloatHelo)
! 	fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		afNH4UptHeloW = sNH4W * sNO3W / ((ahNUptHelo + sNH4W) * (ahNUptHelo + sNO3W +Nea&
		&rZero)) + sNH4W * ahNUptHelo / ((sNH4W + sNO3W +NearZero) * (ahNUptHelo + sNO3W &
		&+NearZero))
! 	NH4_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNH4HeloW = afNH4UptHeloW * tNUptHeloW
! 	NO3_uptake_of_vegetation_from_water_by_submerged_and_floating_parts
		tNUptNO3HeloW = tNUptHeloW - tNUptNH4HeloW
! 	N_uptake_rate_by_roots
		aVNUptHeloS = oNDissS * aVNUptMaxCrHelo / (ahNUptHelo + oNDissS)
! 	N_uptake_from_interstitial_water_by_rooted_part
		tNUptHeloS = aVNUptHeloS * aDRootHelo
! 	fraction_ammonium_uptake_from_interstitial_water_(from_WASP_model__EPA)
		afNH4UptHeloS = oNH4S * oNO3S / ((ahNUptHelo + oNH4S +NearZero) * (ahNUptHelo + &
		&oNO3S +NearZero)) + oNH4S * ahNUptHelo / ((oNH4S + oNO3S+NearZero) * (ahNUptHelo&
		& + oNO3S+NearZero))
! 	NH4_uptake_from_interstitial_water_by_rooted_part
		tNUptNH4HeloS = afNH4UptHeloS * tNUptHeloS
! 	NO3_uptake_from_interstitial_water_by_rooted_part
		tNUptNO3HeloS = tNUptHeloS - tNUptNH4HeloS
! 	total_N_uptake_by_vegetation
		tNUptHelo = tNUptHeloW + tNUptHeloS
! 	light_at_top_of_vegetation_layer
		aLPAR1Helo = uLPAR0 * exp(- aExtCoefOpen * aDepth1Helo)
! 	light_at_bottom_of_vegetation_layer
		aLPAR2Helo = aLPAR1Helo * exp(- aExtCoef * (aDepth2Helo - aDepth1Helo))
! 	half-saturation_light_intensity_for_veg_production_at_current_temperature
		uhLHelo = hLRefHelo * uFunTmProdHelo
! 	light_function_of_growth_based_on_shoot_fraction
		aLLimShootHelo = fEmergHelo + fFloatHelo * (1.0 - afCovEmergVeg) + bfSubHelo * (&
		&1.0 - afCovSurfVeg) * 1.0 / (aExtCoef * sDepthW) * log( (1.0 + aLPAR1Helo / uhLH&
		&elo) / (1.0 + aLPAR2Helo / uhLHelo))
! 	maximum_growth_rate_at_current_temperature_and_light
		aMuTmLHelo = ufDay * bfShootHelo * aLLimShootHelo * uFunTmProdHelo * cMuMaxHelo
! 	P_limitation_on_vegetation_growth_according_to_Droop_function
		aPLimHelo = max(0.0, (1.0 - cPDHeloMin / (rPDHelo + NearZero)) * cPDHeloMax / (c&
		&PDHeloMax - cPDHeloMin))
! 	N_limitation_on_vegetation_growth_according_to_Droop_function
		aNLimHelo = max(0.0, (1.0 - cNDHeloMin / (rNDHelo + NearZero)) * cNDHeloMax / (c&
		&NDHeloMax - cNDHeloMin))
! 	nutrient_limitation_on_veg_growth_according_to_Liebigs_law_of_the_minimum
		aNutLimHelo = min( aPLimHelo, aNLimHelo)
! 	actual_growth_rate_of_vegetation
		aMuHelo = aMuTmLHelo * aNutLimHelo
! 	mortality_of_plant_species
		if (Day < cDayWinHelo) then
		bkMortHelo = kMortHeloSum 		else if (Day < cDayWinHelo + cLengMort) then		bkMortHelo = - log(fWinHelo) / cLengMort 		else		bkMortHelo = kMortHeloSum 		endif
! 	net_growth_rate_of_vegetation_(growth_minus_respiration_minus_mortality)
		akDIncrHelo = aMuTmLHelo - kDRespHelo * uFunTmRespHelo - bkMortHelo
! 	logistic_correction_of_vegetation_biomass
		tDEnvHelo = max(0.0, akDIncrHelo / (cDCarrHelo+NearZero) * sDHelo*sDHelo)
! 	logistic_correction_of_vegetation_growth
		tDEnvProdHelo = aMuHelo / cMuMaxHelo * tDEnvHelo
! 	vegetation_growth_rate
		tDProdHelo = max(0.0, aMuHelo * sDHelo - tDEnvProdHelo)
! 	growth_rate_of_submerged_parts
		tDProdSubHelo = bfSubHelo * tDProdHelo
! 	dark_respiration_rate_of_vegetation
		tDRespHelo = kDRespHelo * uFunTmRespHelo * sDHelo
! 	logistic_correction_of_mortality
		tDEnvMortHelo = tDEnvHelo - tDEnvProdHelo
! 	vegetation_mortality_rate
		tDMortHelo = bkMortHelo * sDHelo + tDEnvMortHelo
! 	mortality_flux_becoming_water_detritus
		tDMortHeloW = fDetWMortHelo * (1.0 - bfRootHelo) * tDMortHelo
! 	mortality_flux_becoming_sediment_detritus
		tDMortHeloS = tDMortHelo - tDMortHeloW
! 	biomass_loss_due_to_grazing_of_birds
		if ( (sTime >= cYearStartBirds * DaysPerYear) .and. (Day >= cDayStartBirds) .and&
		&. (Day <= cDayEndBirds) ) then
		tDGrazHeloBird = cPrefHeloBird * sDHelo / (hDVegBird + sDHelo) * cBirdsPerha / m&
		&2Perha * cDGrazPerBird 		else		tDGrazHeloBird = 0.0 		endif
! 	loss_rate_by_mowing_during_mowing_period
		if ((Day >= cDayManVeg1 .and. Day < cDayManVeg1 + cLengMan) .or. (Day >= cDayMan&
		&Veg2 .and. Day < cDayManVeg2 + cLengMan)) then
		bkManHelo = -log(1.0 - fManHelo) / cLengMan 		else		bkManHelo = 0.0 		endif
! 	vegetation_loss_of_vegetation_DW_by_mowing
		tDManHelo = bkManHelo * sDHelo
! 	vegetation_loss_of_vegetation_P_by_mowing
		tPManHelo = rPDHelo * tDManHelo
! 	vegetation_loss_of_vegetation_N_by_mowing
		tNManHelo = rNDHelo * tDManHelo
! 	derivative_of_vegetation_DW
		tDBedHelo = tDMigrHelo + tDProdHelo - tDRespHelo - tDMortHelo - tDGrazHeloBird -&
		& tDManHelo
! 	O2_production_by_vegetation
		tO2ProdHelo = molO2molC * cCPerDW * tDProdHelo
! 	O2_respiration_of_submerged_part
		tO2RespHeloW = molO2molC * cCPerDW * bfSubHelo * tDRespHelo * aCorO2BOD
! 	O2_respiration_of_rooted_part
		tO2RespHeloS = molO2molC * cCPerDW * bfRootHelo * tDRespHelo * afOxySed
! 	O2_production_by_rooted_part
		tO2ProdHeloS = min (tO2RespHeloS, tO2ProdHelo)
! 	O2_production_by_submerged_part
		tO2ProdHeloW = min( tO2ProdHelo - tO2ProdHeloS, bfSubHelo * tO2ProdHelo)
! 	O2_production_due_to_NO3_uptake_by_submerged_part
		tO2UptNO3HeloW = O2PerNO3 * molO2molN * bfSubHelo * tNUptNO3HeloW
! 	O2_production_due_to_NO3_uptake_by_rooted_part
		tO2UptNO3HeloS = O2PerNO3 * molO2molN * tNUptNO3HeloS
! 	P_excretion_by_vegetation
		tPExcrHelo = (2.0*rPDHelo) / (cPDHeloMax + rPDHelo) * rPDHelo * tDRespHelo
! 	P_excretion_by_rooted_part
		tPExcrHeloS = bfRootHelo * tPExcrHelo
! 	P_excretion_by_submerged_part
		tPExcrHeloW = tPExcrHelo - tPExcrHeloS
! 	P_associated_with_mortality_of_vegetation
		tPMortHelo = rPDHelo * tDMortHelo
! 	PO4_associated_with_mortality_of_vegetation
		tPMortHeloPO4 = fDissMortVeg * tPMortHelo
! 	PO4_associated_with_mortality_of_rooted_part
		tPMortHeloPO4S = bfRootHelo * tPMortHeloPO4
! 	PO4_associated_with_mortality_of_submerged_part
		tPMortHeloPO4W = tPMortHeloPO4 - tPMortHeloPO4S
! 	Detritus_P_associated_with_mortality_of_vegetation
		tPMortHeloDet = tPMortHelo - tPMortHeloPO4
! 	Detritus_P_associated_with_mortality_of_rooted_part
		tPMortHeloDetW = fDetWMortHelo * (1.0 - bfRootHelo) * tPMortHeloDet
! 	Detritus_P_associated_with_mortality_of_submerged_part
		tPMortHeloDetS = tPMortHeloDet - tPMortHeloDetW
! 	P_associated_with_loss_of_vegetation_by_herbivorous_birds
		tPGrazHeloBird = rPDHelo * tDGrazHeloBird
! 	derivative_of_vegetation_P
		tPBedHelo = tPMigrHelo + tPUptHelo - tPExcrHelo - tPMortHelo - tPGrazHeloBird - &
		&tPManHelo
! 	N_excretion_by_vegetation
		tNExcrHelo = (2.0*rNDHelo) / (cNDHeloMax + rNDHelo) * rNDHelo * tDRespHelo
! 	N_excretion_by_rooted_part
		tNExcrHeloS = bfRootHelo * tNExcrHelo
! 	N_excretion_by_submerged_part
		tNExcrHeloW = tNExcrHelo - tNExcrHeloS
! 	N_associated_with_mortality_of_vegetation
		tNMortHelo = rNDHelo * tDMortHelo
! 	NH4_associated_with_mortality_of_vegetation
		tNMortHeloNH4 = fDissMortVeg * tNMortHelo
! 	NH4_associated_with_mortality_of_rooted_part
		tNMortHeloNH4S = bfRootHelo * tNMortHeloNH4
! 	NH4_associated_with_mortality_of_submerged_part
		tNMortHeloNH4W = tNMortHeloNH4 - tNMortHeloNH4S
! 	Detritus_N_associated_with_mortality_of_vegetation
		tNMortHeloDet = tNMortHelo - tNMortHeloNH4
! 	Detritus_N_associated_with_mortality_of_rooted_part
		tNMortHeloDetW = fDetWMortHelo * (1.0 - bfRootHelo) * tNMortHeloDet
! 	Detritus_N_associated_with_mortality_of_submerged_part
		tNMortHeloDetS = tNMortHeloDet - tNMortHeloDetW
! 	N_associated_with_loss_of_vegetation_by_herbivorous_birds
		tNGrazHeloBird = rNDHelo * tDGrazHeloBird
! 	derivative_of_vegetation_N
		tNBedHelo = tNMigrHelo + tNUptHelo - tNExcrHelo - tNMortHelo - tNGrazHeloBird - &
		&tNManHelo
! 	total_vegetation_migration_flux
		tDMigrVeg = tDMigrElod + tDMigrChar + tDMigrCera + tDMigrLemn + tDMigrNymp + tDM&
		&igrHelo
! 	total_vegetation_net_migration_flux
		tPMigrVeg = tPMigrElod + tPMigrChar + tPMigrCera + tPMigrLemn + tPMigrNymp + tPM&
		&igrHelo
! 	total_vegetation_net_migration_flux
		tNMigrVeg = tNMigrElod + tNMigrChar + tNMigrCera + tNMigrLemn + tNMigrNymp + tNM&
		&igrHelo
! 	total_vegetation_production
		tDProdVeg = tDProdElod + tDProdChar + tDProdCera + tDProdLemn + tDProdNymp + tDP&
		&rodHelo
! 	total_vegetation_P_uptake_from_water
		tPUptVegW = tPUptElodW + tPUptCharW + tPUptCeraW + tPUptLemnW + tPUptNympW + tPU&
		&ptHeloW
! 	total_vegetation_P_uptake_from_interstitial_water_by_rooted_part
		tPUptVegS = tPUptElodS + tPUptCharS + tPUptCeraS + tPUptLemnS + tPUptNympS + tPU&
		&ptHeloS
! 	total_vegetation_NH4_uptake_from_water
		tNUptNH4VegW = tNUptNH4ElodW + tNUptNH4CharW + tNUptNH4CeraW + tNUptNH4LemnW + t&
		&NUptNH4NympW + tNUptNH4HeloW
! 	total_vegetation_NH4_uptake_from_sediment
		tNUptNH4VegS = tNUptNH4ElodS + tNUptNH4CharS + tNUptNH4CeraS + tNUptNH4LemnS + t&
		&NUptNH4NympS + tNUptNH4HeloS
! 	total_vegetation_NO3_uptake_from_water
		tNUptNO3VegW = tNUptNO3ElodW + tNUptNO3CharW + tNUptNO3CeraW + tNUptNO3LemnW + t&
		&NUptNO3NympW + tNUptNO3HeloW
! 	total_vegetation_NO3_uptake_from_sediment
		tNUptNO3VegS = tNUptNO3ElodS + tNUptNO3CharS + tNUptNO3CeraS + tNUptNO3LemnS + t&
		&NUptNO3NympS + tNUptNO3HeloS
! 	total_vegetation_dark_respiration
		tDRespVeg = tDRespElod + tDRespChar + tDRespCera + tDRespLemn + tDRespNymp + tDR&
		&espHelo
! 	total_vegetation_P_excretion_in_water
		tPExcrVegW = tPExcrElodW + tPExcrCharW + tPExcrCeraW + tPExcrLemnW + tPExcrNympW&
		& + tPExcrHeloW
! 	total_vegetation_P_excretion_in_sediment
		tPExcrVegS = tPExcrElodS + tPExcrCharS + tPExcrCeraS + tPExcrLemnS + tPExcrNympS&
		& + tPExcrHeloS
! 	total_vegetation_N_excretion_to_water
		tNExcrVegW = tNExcrElodW + tNExcrCharW + tNExcrCeraW + tNExcrLemnW + tNExcrNympW&
		& + tNExcrHeloW
! 	total_vegetation_N_excretion_to_sediment
		tNExcrVegS = tNExcrElodS + tNExcrCharS + tNExcrCeraS + tNExcrLemnS + tNExcrNympS&
		& + tNExcrHeloS
! 	total_vegetation_O2_production
		tO2ProdVeg = tO2ProdElod + tO2ProdChar + tO2ProdCera + tO2ProdLemn + tO2ProdNymp&
		& + tO2ProdHelo
! 	total_vegetation_O2_used_for_production
		tO2ProdVegW = tO2ProdElodW + tO2ProdCharW + tO2ProdCeraW + tO2ProdLemnW + tO2Pro&
		&dNympW + tO2ProdHeloW
! 	total_vegetation_O2_transport_to_roots
		tO2ProdVegS = tO2ProdElodS + tO2ProdCharS + tO2ProdCeraS + tO2ProdLemnS + tO2Pro&
		&dNympS + tO2ProdHeloS
! 	total_vegetation_submerged_O2_respiration
		tO2RespVegW = tO2RespElodW + tO2RespCharW + tO2RespCeraW + tO2RespLemnW + tO2Res&
		&pNympW + tO2RespHeloW
! 	total_vegetation_root_O2_respiration
		tO2RespVegS = tO2RespElodS + tO2RespCharS + tO2RespCeraS + tO2RespLemnS + tO2Res&
		&pNympS + tO2RespHeloS
! 	total_vegetation_O2_production_to_water_due_to_NO3_uptake
		tO2UptNO3VegW = tO2UptNO3ElodW + tO2UptNO3CharW + tO2UptNO3CeraW + tO2UptNO3Lemn&
		&W + tO2UptNO3NympW + tO2UptNO3HeloW
! 	total_vegetation_O2_production_due_to_NO3_uptake_from_sediment
		tO2UptNO3VegS = tO2UptNO3ElodS + tO2UptNO3CharS + tO2UptNO3CeraS + tO2UptNO3Lemn&
		&S + tO2UptNO3NympS + tO2UptNO3HeloS
! 	total_vegetation_mortality_flux_becoming_water_detritus
		tDMortVegW = tDMortElodW + tDMortCharW + tDMortCeraW + tDMortLemnW + tDMortNympW&
		& + tDMortHeloW
! 	total_vegetation_mortality_flux_becoming_sediment_detritus
		tDMortVegS = tDMortElodS + tDMortCharS + tDMortCeraS + tDMortLemnS + tDMortNympS&
		& + tDMortHeloS
! 	total_vegetation_mortality_flux_becoming_dissolved_P_in_water
		tPMortVegPO4W = tPMortElodPO4W + tPMortCharPO4W + tPMortCeraPO4W + tPMortLemnPO4&
		&W + tPMortNympPO4W + tPMortHeloPO4W
! 	total_vegetation_mortality_flux_becoming_detritus_P_in_water
		tPMortVegDetW = tPMortElodDetW + tPMortCharDetW + tPMortCeraDetW + tPMortLemnDet&
		&W + tPMortNympDetW + tPMortHeloDetW
! 	total_vegetation_mortality_flux_becoming_dissolved_P_in_sediment
		tPMortVegPO4S = tPMortElodPO4S + tPMortCharPO4S + tPMortCeraPO4S + tPMortLemnPO4&
		&S + tPMortNympPO4S + tPMortHeloPO4S
! 	total_vegetation_mortality_flux_becoming_detritus_P_in_sediment
		tPMortVegDetS = tPMortElodDetS + tPMortCharDetS + tPMortCeraDetS + tPMortLemnDet&
		&S + tPMortNympDetS + tPMortHeloDetS
! 	total_vegetation_mortality_flux_becoming_dissolved_N_in_water
		tNMortVegNH4W = tNMortElodNH4W + tNMortCharNH4W + tNMortCeraNH4W + tNMortLemnNH4&
		&W + tNMortNympNH4W + tNMortHeloNH4W
! 	total_vegetation_mortality_flux_becoming_detritus_N_in_water
		tNMortVegDetW = tNMortElodDetW + tNMortCharDetW + tNMortCeraDetW + tNMortLemnDet&
		&W + tNMortNympDetW + tNMortHeloDetW
! 	total_vegetation_mortality_flux_becoming_dissolved_N_in_sediment
		tNMortVegNH4S = tNMortElodNH4S + tNMortCharNH4S + tNMortCeraNH4S + tNMortLemnNH4&
		&S + tNMortNympNH4S + tNMortHeloNH4S
! 	total_vegetation_mortality_flux_becoming_detritus_N_in_sediment
		tNMortVegDetS = tNMortElodDetS + tNMortCharDetS + tNMortCeraDetS + tNMortLemnDet&
		&S + tNMortNympDetS + tNMortHeloDetS
! 	total_vegetation_loss_due_to_herbivorous_birds
		tDGrazVegBird = tDGrazElodBird + tDGrazCharBird + tDGrazCeraBird + tDGrazLemnBir&
		&d + tDGrazNympBird + tDGrazHeloBird
! 	total_vegetation_P_mortality_flux_by_herbivorous_birds
		tPGrazVegBird = tPGrazElodBird + tPGrazCharBird + tPGrazCeraBird + tPGrazLemnBir&
		&d + tPGrazNympBird + tPGrazHeloBird
! 	total_vegetation_N_mortality_flux_by_herbivorous_birds
		tNGrazVegBird = tNGrazElodBird + tNGrazCharBird + tNGrazCeraBird + tNGrazLemnBir&
		&d + tNGrazNympBird + tNGrazHeloBird
! 	total_vegetation_Mowing_DW
		tDManVeg = tDManElod + tDManChar + tDManCera + tDManLemn + tDManNymp + tDManHelo
! 	total_vegetation_Mowing_P
		tPManVeg = tPManElod + tPManChar + tPManCera + tPManLemn + tPManNymp + tPManHelo
! 	total_vegetation_Mowing_N
		tNManVeg = tNManElod + tNManChar + tNManCera + tNManLemn + tNManNymp + tNManHelo
! 	total_vegetation_submerged_coverage
		aCovSub = min(100.0,aCovElod + aCovChar + aCovCera)
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
! 	total_vegetation_DW_flux_to_water_detritus
		wDBedDetW = (tDMortVegW + tDEgesBird) / sDepthW
! 	total_vegetation_DW_flux_to_sediment_detritus
		tDBedDetS = tDMortVegS
! 	total_vegetation_DW_flux
		tDBedTotT = tDMigrVeg + tDProdVeg - tDRespVeg - tDManVeg - tDAssVegBird
! 	total_vegetation_P_flux_to_PO4_in_water
		wPBedPO4W = (- tPUptVegW + tPExcrVegW + tPMortVegPO4W + tPEgesBirdPO4) /sDepthW
! 	total_vegetation_P_flux_to_water_detritus
		wPBedDetW = (tPMortVegDetW + tPEgesBirdDet) / sDepthW
! 	total_vegetation_P_flux_to_interstitial_water_PO4
		tPBedPO4S = - tPUptVegS + tPExcrVegS + tPMortVegPO4S
! 	total_vegetation_P_flux_to_sediment_detritus
		tPBedDetS = tPMortVegDetS
! 	total_vegetation_P_flux
		tPBedTotT = tPMigrVeg - tPManVeg - tPAssVegBird
! 	total_vegetation_N_flux_to_NH4_in_water
		wNBedNH4W = (- tNUptNH4VegW + tNExcrVegW + tNMortVegNH4W + tNEgesBirdNH4) / sDep&
		&thW
! 	total_vegetation_N_flux_to_NO3_in_water
		wNBedNO3W = - tNUptNO3VegW / sDepthW
! 	total_vegetation_N_flux_to_water_detritus
		wNBedDetW = (tNMortVegDetW + tNEgesBirdDet) / sDepthW
! 	total_vegetation_N_flux_to_NH4_in_interstitial_water
		tNBedNH4S = - tNUptNH4VegS + tNExcrVegS + tNMortVegNH4S
! 	total_vegetation_N_flux_to_NO3_in_interstitial_water
		tNBedNO3S = - tNUptNO3VegS
! 	total_vegetation_N_flux_to_sediment_detritus
		tNBedDetS = tNMortVegDetS
! 	total_vegetation_N_flux
		tNBedTotT = tNMigrVeg - tNManVeg - tNAssVegBird
! 	total_vegetation_O2_flux_to_water
		tO2BedW = tO2ProdVegW - tO2RespVegW + tO2UptNO3VegW
! 	total_vegetation_O2_flux_to_sediment
		tO2BedS = tO2ProdVegS - tO2RespVegS + tO2UptNO3VegS
! 	switch_to_indicate_if_phytoplankton_is_affected_by_grazing_(1=yes_and_0=no)
		UseLoss = 1.0
! 	temperature_function_of_grazing
		uFunTmLoss = exp(-0.5/(cSigTmLoss*cSigTmLoss) *(((uTm-cTmOptLoss)*(uTm-cTmOptLos&
		&s)) -(cTmRef-cTmOptLoss)*(cTmRef-cTmOptLoss)))
! 	P/DW_ratio_of_phytoplankton
		rPDPhytW = sPPhytW /(sDPhytW+NearZero)
! 	N/DW_ratio_of_phytoplankton
		rNDPhytW = sNPhytW /(sDPhytW+NearZero)
! 	P/DW_ratio_of_settled_phytoplankton
		rPDPhytS = sPPhytS /(sDPhytS+NearZero)
! 	N/DW_ratio_of_settled_phytoplankton
		rNDPhytS = sNPhytS /(sDPhytS+NearZero)
! 	temperature_function_of_phytoplankton_growth_and_respiration
		uFunTmPhyt = exp(-0.5/(cSigTmPhyt*cSigTmPhyt) *((uTm-cTmOptPhyt)*(uTm-cTmOptPhyt&
		&) - (cTmRef-cTmOptPhyt)*(cTmRef-cTmOptPhyt)))
! 	temperature_function_of_phytoplankton_growth
		uFunTmProdPhyt = uFunTmPhyt
! 	temperature_function_of_phytoplankton_respiration
		uFunTmRespPhyt = uFunTmPhyt
! 	maximum_P_uptake_rate_phytoplankton_corrected_for_P/DW_ratio_and_temperature
		aVPUptMaxCrPhyt = max(0.0,cVPUptMaxPhyt * uFunTmProdPhyt *(cPDPhytMax - rPDPhytW&
		&) /(cPDPhytMax - cPDPhytMin))
! 	actual_P_uptake_rate_of_phytoplankton
		aVPUptPhyt = sPO4W * aVPUptMaxCrPhyt /(aVPUptMaxCrPhyt / cAffPUptPhyt + sPO4W)
! 	total_P_uptake_by_phytoplankton
		wPUptPhyt = aVPUptPhyt * sDPhytW
! 	maximum_N_uptake_rate_ophytoplankton_corrected_for_N/DW_ratio_and_temperature
		aVNUptMaxCrPhyt = max(0.0,cVNUptMaxPhyt * uFunTmProdPhyt * (cNDPhytMax - rNDPhyt&
		&W) /(cNDPhytMax - cNDPhytMin))
! 	half-saturation_SRN_concentration_for_uptake_by_phytoplankton
		ahNUptPhyt = aVNUptMaxCrPhyt / cAffNUptPhyt
! 	actual_N_uptake_rate_of_phytoplankton
		aVNUptPhyt = oNDissW * aVNUptMaxCrPhyt /(ahNUptPhyt + oNDissW)
! 	total_N_uptake_by_phytoplankton
		wNUptPhyt = aVNUptPhyt * sDPhytW
! 	fraction_ammonium_uptake_by_phytoplankton_(from_EPA)
		afNH4UptPhyt = sNH4W * sNO3W /((ahNUptPhyt + sNH4W) *(ahNUptPhyt + sNO3W)) + sNH&
		&4W * ahNUptPhyt /((sNH4W + sNO3W) *(ahNUptPhyt + sNO3W))
! 	total_NH4-N_uptake_by_phytoplankton
		wNUptNH4Phyt = afNH4UptPhyt * wNUptPhyt
! 	total_NO3-N_uptake_by_phytoplankton
		wNUptNO3Phyt = wNUptPhyt - wNUptNH4Phyt
! 	maximum_growth_rate_of_phytoplankton_at_current_temperature
		uMuMaxTmPhyt = cMuMaxPhyt * uFunTmProdPhyt
! 	P_limitation_on_phytoplankton_growth_according_to_Droop_function
		aPLimPhyt = max(0.0,(1.0 - cPDPhytMin / (rPDPhytW + NearZero)) * cPDPhytMax /(cP&
		&DPhytMax - cPDPhytMin))
! 	N_limitation_on_phytoplankton_growth_according_to_Droop_function
		aNLimPhyt = max(0.0,(1.0 - cNDPhytMin / (rNDPhytW + NearZero)) * cNDPhytMax /(cN&
		&DPhytMax - cNDPhytMin))
! 	light_limitation_phytoplankton_growth_according_to_Steele_or_Lehman_function
		aLLimPhyt = UseSteelePhyt *(exp(1.0) /(aExtCoef * sDepthW) *(exp(- aLPARBot /(cL&
		&OptRefPhyt * uFunTmProdPhyt)) - exp(- uLPAR0 /(cLOptRefPhyt * uFunTmProdPhyt))))&
		& +(1.0 - UseSteelePhyt) *(1.0 /(aExtCoef * sDepthW) * log((1.0 + uLPAR0 / (hLRef&
		&Phyt * uFunTmProdPhyt)) / (1.0 + aLPARBot /(hLRefPhyt * uFunTmProdPhyt))))
! 	phytoplankton_growth_rate_at_current_light_and_temperature
		aMuTmLPhyt = ufDay *(1.0 - afCovSurfVeg) * aLLimPhyt * uMuMaxTmPhyt
! 	nutrient_limitation_phytoplankton_growth_(Liebigs_law_of_the_minimum)
		aNutLimPhyt = min(aPLimPhyt,aNLimPhyt)
! 	phytoplankton_growth_rate
		aMuPhyt = aNutLimPhyt * aMuTmLPhyt
! 	total_phytoplankton_growth
		wDAssPhyt = aMuPhyt*sDPhytW
! 	chlorophyll-a/DW_ratio_phytoplankton
		rChDPhyt = cChDPhytMax -(cChDPhytMax - cChDPhytMin) * aLLimPhyt
! 	chlorophyll-a_concentration
		oChlaPhyt = mgPerg * rChDPhyt * sDPhytW
! 	specific_light_extinction_per_unit_chlorophyll-a
		aExtChPhyt = cExtSpPhyt / rChDPhyt
! 	temperature_corrected_respiration_constant_of_phytoplankton
		ukDRespTmPhyt = kDRespPhyt * uFunTmRespPhyt
! 	total_phytoplankton_respiration_in_water
		wDRespPhytW = ukDRespTmPhyt * sDPhytW
! 	daily_grazing_on_phytoplankton
		ukLossTmPhyt = UseLoss * kLossPhyt * uFunTmLoss
! 	total_phytoplankton_grazing_loss
		wDLossPhyt = ukLossTmPhyt * sDPhytW
! 	total_phytoplankton_mortality_in_water
		wDMortPhytW = kMortPhytW * sDPhytW
! 	corrected_settling_velocity_of_phytoplankton
		uCorVSetPhyt = cVSetPhyt * aFunTauSetOM * uFunTmSet
! 	total_phytoplankton_settling
		tDSetPhyt = uCorVSetPhyt * sDPhytW
! 	resuspension_of_settled_phytoplankton
		tDResusPhyt = sDPhytS /(aDPhytS+NearZero) * tDResusPhytTot
! 	respiration_of_settled_phytoplankton
		tDRespPhytS = ukDRespTmPhyt * sDPhytS
! 	mortality_of_settled_phytoplankton
		tDMortPhytS = kMortPhytS * sDPhytS
! 	total_loss_rate_of_phytoplankton_in_water_(without_dilution)
		ukDDecPhyt = ukDRespTmPhyt + ukLossTmPhyt + kMortPhytW +(uCorVSetPhyt * uFunTmSe&
		&t) / sDepthW
! 	total_P_excretion_phytoplankton_in_water
		wPExcrPhytW = (2.0*rPDPhytW) /(cPDPhytMax + rPDPhytW) * rPDPhytW * wDRespPhytW
! 	total_P_loss_by_grazing_on_phytoplankton
		wPLossPhyt = rPDPhytW * wDLossPhyt
! 	total_P_mortality_phytoplankton_in_water
		wPMortPhytW = wDMortPhytW * rPDPhytW
! 	total_P_associated_with_settling_of_phytoplankton
		tPSetPhyt = rPDPhytW * tDSetPhyt
! 	total_P_resuspension_of_settled_phytoplankton
		tPResusPhyt = rPDPhytS * tDResusPhyt
! 	total_P_excretion_by_settled_phytoplankton
		tPExcrPhytS = (2.0*rPDPhytS) /(cPDPhytMax + rPDPhytS) * rPDPhytS * tDRespPhytS
! 	total_P_phytoplankton_mortality
		tPMortPhytS = tDMortPhytS * rPDPhytS
! 	total_N_excretion_phytoplankton_in_water
		wNExcrPhytW = (2.0*rNDPhytW) /(cNDPhytMax + rNDPhytW) * rNDPhytW * wDRespPhytW
! 	total_N_loss_by_grazing_on_phytoplankton
		wNLossPhyt = rNDPhytW * wDLossPhyt
! 	total_N_mortality_phytoplankton_in_water
		wNMortPhytW = wDMortPhytW * rNDPhytW
! 	total_N_associated_with_settling_of_phytoplankton
		tNSetPhyt = rNDPhytW * tDSetPhyt
! 	total_N_resuspension_of_settled_phytoplankton
		tNResusPhyt = rNDPhytS * tDResusPhyt
! 	total_N_excretion_by_settled_phytoplankton
		tNExcrPhytS = (2.0*rNDPhytS) /(cNDPhytMax + rNDPhytS) * rNDPhytS * tDRespPhytS
! 	total_N_phytoplankton_mortality
		tNMortPhytS = tDMortPhytS * rNDPhytS
! 	total_of_phytoplankton_DW_processes_in_water
		wDPrimPhytW = wDAssPhyt - wDRespPhytW - wDLossPhyt - wDMortPhytW -(tDSetPhyt - t&
		&DResusPhyt) / sDepthW
! 	total_of_phytoplankton_P_processes_in_water
		wPPrimPhytW = wPUptPhyt - wPExcrPhytW - wPLossPhyt - wPMortPhytW -(tPSetPhyt - t&
		&PResusPhyt) / sDepthW
! 	total_of_phytoplankton_N_processes_in_water
		wNPrimPhytW = wNUptPhyt - wNExcrPhytW - wNLossPhyt - wNMortPhytW -(tNSetPhyt - t&
		&NResusPhyt) / sDepthW
! 	total_of_settled_phytoplankton_DW_processes
		tDPrimPhytS = tDSetPhyt - tDResusPhyt - tDMortPhytS - tDRespPhytS
! 	total_of_settled_phytoplankton_P_processes
		tPPrimPhytS = tPSetPhyt - tPResusPhyt - tPMortPhytS - tPExcrPhytS
! 	total_of_settled_phytoplankton_N_processes
		tNPrimPhytS = tNSetPhyt - tNResusPhyt - tNMortPhytS - tNExcrPhytS
! 	total_chlorophyll-a_concentration
		oChla = oChlaPhyt
! 	total_detritus_flux_to_water_by_phytoplankton
		wDPrimDetW = wDMortPhytW + wDLossPhyt
! 	total_detritus_flux_to_sediment_by_settled_phytoplankton
		tDPrimDetS = tDMortPhytS
! 	total_DW_flux_associated_with_phytoplankton
		tDPrimTotT = (wDAssPhyt - wDRespPhytW) * sDepthW - tDRespPhytS
! 	O2_production_by_phytoplankton
		wO2ProdPhyt = molO2molC * cCPerDW * wDAssPhyt
! 	O2_respiration_by_water_phytoplankton
		wO2RespPhytW = molO2molC * cCPerDW * wDRespPhytW * aCorO2BOD
! 	O2_production_due_to_NO3_uptake_by_phytoplankton
		wO2UptNO3Phyt = O2PerNO3 * molO2molN * wNUptNO3Phyt
! 	O2_flux_by_water_phytoplankton
		wO2PrimW = wO2ProdPhyt - wO2RespPhytW + wO2UptNO3Phyt
! 	O2_respiration_by_settled_phytoplankton
		tO2RespPhytS = molO2molC * cCPerDW * tDRespPhytS * afOxySed
! 	O2_flux_by_settled_phytoplankton
		tO2PrimS = tO2RespPhytS
! 	soluble_P_flux_due_to_water_phytoplankton_mortality
		wPMortPhytPO4W = fDissMortPhyt * wPMortPhytW
! 	detrital_P_flux_due_to_water_phytoplankton_mortality
		wPMortPhytDetW = wPMortPhytW - wPMortPhytPO4W
! 	soluble_P_flux_due_to_grazing_on_phytoplankton
		wPLossPhytPO4 = fDissLoss * wPLossPhyt
! 	detrital_P_flux_due_to_grazing_on_phytoplankton
		wPLossPhytDet = wPLossPhyt - wPLossPhytPO4
! 	SRP_flux_in_water_due_to_phytoplankton
		wPPrimPO4W = - wPUptPhyt + wPExcrPhytW + wPLossPhytPO4 + wPMortPhytPO4W
! 	detrital_P_flux_in_water_due_to_phytoplankton
		wPPrimDetW = wPLossPhytDet + wPMortPhytDetW
! 	pore_water_SRP_flux_due_to_settled_phytoplankton_mortality
		tPMortPhytPO4S = fDissMortPhyt * tPMortPhytS
! 	detrital_P_flux_due_to_settled_phytoplankton_mortality
		tPMortPhytDetS = tPMortPhytS - tPMortPhytPO4S
! 	detrital_P_flux_in_sediment_due_to_phytoplankton
		tPPrimDetS = tPMortPhytDetS
! 	pore_water_SRP_flux_due_to_phytoplankton
		tPPrimPO4S = tPExcrPhytS + tPMortPhytPO4S
! 	total_P_flux_associated_with_phytoplankton
		tPPrimTotT = 0.0
! 	ammonium_flux_due_to_water_phytoplankton_mortality
		wNMortPhytNH4W = fDissMortPhyt * wNMortPhytW
! 	detrital_N_flux_due_to_water_phytoplankton_mortality
		wNMortPhytDetW = wNMortPhytW - wNMortPhytNH4W
! 	ammonium_flux_due_to_grazing_on_phytoplankton
		wNLossPhytNH4 = fDissLoss * wNLossPhyt
! 	detrital_N_flux_due_to_grazing_on_phytoplankton
		wNLossPhytDet = wNLossPhyt - wNLossPhytNH4
! 	ammonium_flux_in_water_due_to_phytoplankton
		wNPrimNH4W = - wNUptNH4Phyt + wNExcrPhytW + wNLossPhytNH4 + wNMortPhytNH4W
! 	nitrate_flux_in_water_due_to_phytoplankton
		wNPrimNO3W = - wNUptNO3Phyt
! 	detrital_N_flux_in_water_due_to_phytoplankton
		wNPrimDetW = wNLossPhytDet + wNMortPhytDetW
! 	pore_water_ammonium_flux_due_to_settled_phytoplankton_mortality
		tNMortPhytNH4S = fDissMortPhyt * tNMortPhytS
! 	sediment_detrital_N_flux_due_to_settled_phytoplankton_mortality
		tNMortPhytDetS = tNMortPhytS - tNMortPhytNH4S
! 	pore_water_ammonium_flux_due_to_phytoplankton
		tNPrimNH4S = tNExcrPhytS + tNMortPhytNH4S
! 	pore_water_nitrate_flux_due_to_phytoplankton
		tNPrimNO3S = 0.0
! 	sediment_detrital_N_flux_due_to_phytoplankton
		tNPrimDetS = tNMortPhytDetS
! 	total_N_flux_associated_with_phytoplankton
		tNPrimTotT = 0.0
! 	Poole-Atkins_coefficient_to_calculate_Secchi_depth
		aPACoef = cPACoefMin +(cPACoefMax - cPACoefMin) * hPACoef / (hPACoef + oDOMW)
! 	maximum_Secchi_depth
		bSecchiMax = sDepthW + cSecchiPlus
! 	Secchi_depth
		aSecchi = min(bSecchiMax,aPACoef / aExtCoefOpen)
! 	euphotic_depth
		aDepthEuph = cEuph * aSecchi
! 	relative_euphotic_depth
		aRelDepthEuph = aDepthEuph / sDepthW
! 	mg_Chla_per_m2
		aChlaH = oChla * sDepthW
! 	%_horizontal_coverage_with_phytoplankton
		aCovPhytW = cCovSpPhyt *(oDPhytW * sDepthW)
! 	average_specific_light_extinction_of_phytoplankton_per_unit_chlorophyl-a
		rExtChPhyt = aExtPhyt /(oChla / mgPerg + NearZero)
! 	total_settling_DW_flux
		tDSetTot = tDSetIM + tDSetDet + tDSetPhyt
! 	total_settling_P_flux
		tPSetTot = tPSetAIM + tPSetDet + tPSetPhyt
! 	total_settling_N_flux
		tNSetTot = tNSetDet + tNSetPhyt
! 	total_resuspension_DW_flux
		tDResusTot = tDResusDead + tDResusPhytTot
! 	total_resuspension_P_flux
		tPResusTot = tPResusDet + tPResusAIM + tPResusPhyt + tPResusPO4
! 	total_resuspension_N_flux
		tNResusTot = tNResusDet + tNResusNH4 + tNResusNO3 + tNResusPhyt
! 	start_of_dredging_period
		if (sTime >= cDredStart * DaysPerYear) then
		bTimeDred = (floor(TimeYears/cDredInterval) * cDredInterval) * DaysPerYear 		else		bTimeDred = -9999.999 		endif
! 	water_depth_at_the_beginning_of_dredging_period
		if (sTime == bTimeDred) then
		aDepthStart = sDepthW 		else		aDepthStart = aDepthStart 		endif
! 	deepening_rate_constant_during_dredging_period
		if ((sTime >= bTimeDred) .and. (sTime < bTimeDred + cLengDred) .and. (aDepthStar&
		&t <= cDepthRef - cDepthS)) then
		akDredDepth = (log(cDepthRef / aDepthStart)) / cLengDred 		else		akDredDepth = 0.0 		endif
! 	dredging_rate_constant_during_dredging_period
		if ((sTime >= bTimeDred) .and.(sTime < bTimeDred + cLengDred) .and.(aDepthStart &
		&<= cDepthRef - cDepthS)) then
		akDred = (- log(1.0 - fEffDred)) / cLengDred 		else		akDred = 0.0 		endif
! 	dredging_rate_constant_during_dredging_period_for_Lemnacaea
		if ((sTime >= bTimeDred) .and.(sTime < bTimeDred + cLengDred) .and.(aDepthStart &
		&<= cDepthRef - cDepthS)) then
		akDredLemn = (- log(1.0 - fEffDredLemn)) / cLengDred 		else		akDredLemn = 0.0 		endif
! 	change_in_water_depth_due_to_dredging
		vDredDepthW = akDredDepth * sDepthW
! 	dredging_flux_of_DW_Detritus_in_ditch_sediment
		tDDredDetS = akDred * sDDetS
! 	dredging_flux_of_P_Detritus_in_ditch_sediment
		tPDredDetS = akDred * sPDetS
! 	dredging_flux_of_N_Detritus_in_ditch_sediment
		tNDredDetS = akDred * sNDetS
! 	dredging_flux_of_P_absorbed_onto_inorganic_matter_in_ditch_sediment
		tPDredAIMS = akDred * sPAIMS
! 	average_solid_density_of_soil_material
		bRhoSolidSoil = fDOrgSoil * cRhoOM +(1 - fDOrgSoil) * cRhoIM
! 	dredging_flux_of_DW_NetSoil_in_ditch_sediment
		tDDredNetSoil = -(tDDredDetS / cRhoOM) * bRhoSolidSoil
! 	dredging_flux_of_DW_NetIMS_in_ditch_sediment
		tDDredNetIMS = (1 - fDOrgSoil) * tDDredNetSoil
! 	dredging_flux_of_DW_NetHum_in_ditch_sediment
		tDDredNetHumS = fDOrgSoil * tDDredNetSoil
! 	dredging_flux_of_P_NetHum_in_ditch_sediment
		tPDredNetHumS = cPDSoilOM * tDDredNetHumS
! 	dredging_flux_of_N_NetHum_in_ditch_sediment
		tNDredNetHumS = cNDSoilOM * tDDredNetHumS
! 	dredging_flux_of_DW_phytoplankton_on_ditch_sediment
		tDDredPhytS = akDred * sDPhytS
! 	dredging_flux_of_P_phytoplankton_on_ditch_sediment
		tPDredPhytS = akDred * sPPhytS
! 	dredging_flux_of_N_phytoplankton_on_ditch_sediment
		tNDredPhytS = akDred * sNPhytS
! 	dredging_flux_of_DW_Elodeids_on_ditch_sediment
		tDDredElod = akDred * sDElod
! 	dredging_flux_of_P_Elodeids_on_ditch_sediment
		tPDredElod = akDred * sPElod
! 	dredging_flux_of_N_Elodeids_on_ditch_sediment
		tNDredElod = akDred * sNElod
! 	dredging_flux_of_DW_Ceratophyllids_on_ditch_sediment
		tDDredCera = akDred * sDCera
! 	dredging_flux_of_P_Ceratophyllids_on_ditch_sediment
		tPDredCera = akDred * sPCera
! 	dredging_flux_of_N_Ceratophyllids_on_ditch_sediment
		tNDredCera = akDred * sNCera
! 	dredging_flux_of_DW_Characeans_on_ditch_sediment
		tDDredChar = akDred * sDChar
! 	dredging_flux_of_P_Characeans_on_ditch_sediment
		tPDredChar = akDred * sPChar
! 	dredging_flux_of_N_Characeans_on_ditch_sediment
		tNDredChar = akDred * sNChar
! 	dredging_flux_of_DW_Lemnacaea_on_ditch_sediment
		tDDredLemn = akDredLemn * sDLemn
! 	dredging_flux_of_P_Lemnacaea_on_ditch_sediment
		tPDredLemn = akDredLemn * sPLemn
! 	dredging_flux_of_N_Lemnacaea_on_ditch_sediment
		tNDredLemn = akDredLemn * sNLemn
! 	dredging_flux_of_DW_Nymphaeids_on_ditch_sediment
		tDDredNymp = akDred * sDNymp
! 	dredging_flux_of_P_Nymphaeids_on_ditch_sediment
		tPDredNymp = akDred * sPNymp
! 	dredging_flux_of_N_Nymphaeids_on_ditch_sediment
		tNDredNymp = akDred * sNNymp
! 	dredging_flux_of_DW_helophytes_on_ditch_sediment
		tDDredHelo = akDred * sDHelo
! 	dredging_flux_of_P_helophytes_on_ditch_sediment
		tPDredHelo = akDred * sPHelo
! 	dredging_flux_of_N_helophytes_on_ditch_sediment
		tNDredHelo = akDred * sNHelo
! 	dredging_flux_of_DW_total_vegetation_on_ditch_sediment
		tDDredVeg = tDDredElod+tDDredCera+tDDredChar+tDDredLemn+tDDredNymp+tDDredHelo
! 	dredging_flux_of_P_total_vegetation_on_ditch_sediment
		tPDredVeg = tPDredElod+tPDredCera+tPDredChar+tPDredLemn+tPDredNymp+tPDredHelo
! 	dredging_flux_of_N_total_vegetation_on_ditch_sediment
		tNDredVeg = tNDredElod+tNDredCera+tNDredChar+tNDredLemn+tNDredNymp+tNDredHelo
! 	total_DW_dredging_flux
		tDDredNetTot = tDDredDetS - tDDredNetSoil + tDDredPhytS + tDDredVeg
! 	total_P_dredging_flux
		tPDredNetTot = tPDredDetS - tPDredNetHumS + tPDredAIMS + tPDredPhytS + tPDredVeg
! 	total_N_dredging_flux
		tNDredNetTot = tNDredDetS - tNDredNetHumS + tNDredPhytS + tNDredVeg
! 	increase_in_DW_inorganic_matter_in_sediment
		tDIMS = tDAbioIMS
! 	increase_in_DW_sediment_humus_in_ditch
		tDHumS = tDAbioHumS
! 	increase_in_DW_sediment_detritus_in_ditch
		tDDetS = tDAbioDetS + tDPrimDetS + tDBedDetS
! 	turnover_depth_in_ditch
		vDeltaS = (tDIMS / cRhoIM +(tDHumS + tDDetS) / cRhoOM) /(1.0 - bPorS)
! 	burial_flux_of_DW_in_inorganic_matter_in_ditch
		if (vDeltaS >= 0.0) then
		tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / ((sDHumS + sDDetS) / &
		&sDIMS + cRhoOM / cRhoIM) 		else		tDBurIM = ((tDHumS + tDDetS) +(cRhoOM / cRhoIM) * tDIMS) / (fDOrgSoil /(1.0 - fD&
		&OrgSoil) + cRhoOM / cRhoIM) 		endif
! 	burial_flux_of_DW_in_organic_matter_in_ditch
		if (vDeltaS >= 0.0) then
		tDBurOM = (sDHumS + sDDetS) / sDIMS * tDBurIM 		else		tDBurOM = fDOrgSoil /(1.0 - fDOrgSoil) * tDBurIM 		endif
! 	burial_flux_of_DW_in_detritus_in_ditch
		if (vDeltaS >= 0.0) then
		tDBurDet = sDDetS /(sDHumS + sDDetS) * tDBurOM 		else		tDBurDet = 0.0 		endif
! 	burial_flux_of_DW_in_humus_in_ditch
		if (vDeltaS >= 0.0) then
		tDBurHum = tDBurOM - tDBurDet 		else		tDBurHum = tDBurOM 		endif
! 	total_burial_flux_of_DW_in_ditch
		if (vDeltaS >= 0.0) then
		tDBurTot = tDBurIM + tDBurOM 		else		tDBurTot = tDBurIM + tDBurOM 		endif
! 	burial_flux_of_P_in_humus_in_ditch
		if (vDeltaS >= 0.0) then
		tPBurHum = rPDHumS * tDBurHum 		else		tPBurHum = cPDSoilOM * tDBurHum 		endif
! 	burial_flux_of_P_in_detritus_in_ditch
		if (vDeltaS >= 0.0) then
		tPBurDet = rPDDetS * tDBurDet 		else		tPBurDet = 0.0 		endif
! 	burial_flux_of_P_absorbed_onto_inorganic_matter_in_ditch
		if (vDeltaS >= 0.0) then
		tPBurAIM = sPAIMS / sDIMS * tDBurIM 		else		tPBurAIM = 0.0 		endif
! 	burial_flux_of_SRP_in_ditch
		if (vDeltaS >= 0.0) then
		tPBurPO4 = sPO4S *(vDeltaS / cDepthS) 		else		tPBurPO4 = cPO4Ground *(bPorS * vDeltaS) 		endif
! 	total_burial_flux_of_P_in_ditch
		if (vDeltaS >= 0.0) then
		tPBurTot = tPBurDet + tPBurHum + tPBurAIM + tPBurPO4 		else		tPBurTot = tPBurHum + tPBurAIM + tPBurPO4 		endif
! 	burial_flux_of_N_in_humus_in_ditch
		if (vDeltaS >= 0.0) then
		tNBurHum = rNDHumS * tDBurHum 		else		tNBurHum = cNDSoilOM * tDBurHum 		endif
! 	burial_flux_of_N_in_detritus_in_ditch
		if (vDeltaS >= 0.0) then
		tNBurDet = rNDDetS * tDBurDet 		else		tNBurDet = 0.0 		endif
! 	burial_flux_of_dissolved_NH4_in_ditch
		if (vDeltaS >= 0.0) then
		tNBurNH4 = sNH4S *(vDeltaS / cDepthS) 		else		tNBurNH4 = cNH4Ground *(bPorS * vDeltaS) 		endif
! 	burial_flux_of_dissolved_NO3_in_ditch
		if (vDeltaS >= 0.0) then
		tNBurNO3 = sNO3S *(vDeltaS / cDepthS) 		else		tNBurNO3 = cNO3Ground *(bPorS * vDeltaS) 		endif
! 	total_burial_flux_of_N_in_ditch
		if (vDeltaS >= 0.0) then
		tNBurTot = tNBurDet + tNBurHum + tNBurNH4 + tNBurNO3 		else		tNBurTot = tNBurHum + tNBurNH4 + tNBurNO3 		endif
! 	rate_of_change_in_ditch_water_depth
		if (ConstDepth == 1 ) then
		vDeltaW = 0.0 		else		vDeltaW = - vDeltaS 		endif
! 	relative_water_depth_change_due_to_sediment_turnover_and_dredging
		aRelDeltaW = (vDeltaW + vDredDepthW) / sDepthW
! 	total_DW_in_system
		aDTotT =(sDIMW + sDDetW + sDPhytW) * sDepthW + aDVeg + sDIMS + sDHumS + sDDetS +&
		& aDPhytS
! 	total_N_in_system
		aNTotT =(sNH4W + sNO3W + sNDetW + sNPhytW) * sDepthW + aNVeg + sNH4S + sNO3S + s&
		&NDetS + sNHumS + sNPhytS
! 	total_P_in_system
		aPTotT =(sPO4W + sPDetW + sPPhytW + sPAIMW) * sDepthW + aPVeg + sPO4S + sPDetS +&
		& sPHumS + sPPhytS + sPAIMS
! 	DW_mass_balance_error
		aDError = aDTotT - sDExtTotT
! 	N_mass_balance_error
		aNError = aNTotT - sNExtTotT
! 	P_mass_balance_error
		aPError = aPTotT - sPExtTotT
! 	derivative_of_P_in_PO4_in_ditch_water
		dPO4W = wPAbioPO4W + wPPrimPO4W + wPBedPO4W + cPBackLoad/sDepthW - aRelDeltaW * &
		&sPO4W + wPTranPO4W
! 	derivative_of_P_adsorbed_onto_IM_in_ditch_water
		dPAIMW = wPAbioAIMW - aRelDeltaW * sPAIMW +wPTranAIMW
! 	derivative_of_N_in_NH4_in_ditch_water
		dNH4W = wNAbioNH4W + wNPrimNH4W + wNBedNH4W + cNBackLoad/sDepthW - aRelDeltaW * &
		&sNH4W+wNTranNH4W
! 	derivative_of_N_in_NO3_in_ditch_water
		dNO3W = wNAbioNO3W + wNPrimNO3W + wNBedNO3W - aRelDeltaW * sNO3W +wNTranNO3W
! 	derivative_of_Oxygen_in_ditch_water
		dO2W = wO2AbioW + wO2PrimW + tO2BedW / sDepthW - aRelDeltaW * sO2W +wO2TranW
! 	derivative_of_Inorganic_matter_in_ditch_water
		dDIMW = wDAbioIMW - aRelDeltaW * sDIMW +wDTranIMW
! 	derivative_of_Detritus_DW_in_ditch_water
		dDDetW = wDAbioDetW + wDPrimDetW + wDBedDetW - aRelDeltaW * sDDetW +wDTranDetW
! 	derivative_of_Detritus_P_in_ditch_water
		dPDetW = wPAbioDetW + wPPrimDetW + wPBedDetW - aRelDeltaW * sPDetW +wPTranDetW
! 	derivative_of_Detritus_N_in_ditch_water
		dNDetW = wNAbioDetW + wNPrimDetW + wNBedDetW - aRelDeltaW * sNDetW +wNTranDetW
! 	derivative_of_DW_of_Phytoplankton
		dDPhytW = wDPrimPhytW - aRelDeltaW * sDPhytW +wDTranPhyt
! 	derivative_of_P_in_Phytoplankton
		dPPhytW = wPPrimPhytW - aRelDeltaW * sPPhytW +wPTranPhyt
! 	derivative_of_N_in_Phytoplankton
		dNPhytW = wNPrimPhytW - aRelDeltaW * sNPhytW +wNTranPhyt
! 	derivative_of_P_in_PO4_in_ditch_sediment_interstitial_water
		dPO4S = tPAbioPO4S - tPBurPO4 + tPPrimPO4S + tPBedPO4S
! 	derivative_of_P_adsorbed_onto_IM_in_ditch_sediment
		dPAIMS = tPAbioAIMS - tPBurAIM - tPDredAIMS
! 	derivative_of_N_in_NH4_in_ditch_sediment_interstitial_water
		dNH4S = tNAbioNH4S - tNBurNH4 + tNPrimNH4S + tNBedNH4S
! 	derivative_of_N_in_NO3_in_ditch_sediment_interstitial_water
		dNO3S = tNAbioNO3S - tNBurNO3 + tNPrimNO3S + tNBedNO3S
! 	derivative_of_Inorganic_matter_in_ditch_sediment
		dDIMS = tDAbioIMS - tDBurIM - tDDredNetIMS
! 	derivative_of_Humus_DW_in_ditch_sediment
		dDHumS = tDAbioHumS - tDBurHum - tDDredNetHumS
! 	derivative_of_Humus_P_in_ditch_sediment
		dPHumS = tPAbioHumS - tPBurHum - tPDredNetHumS
! 	derivative_of_Humus_N_in_ditch_sediment
		dNHumS = tNAbioHumS - tNBurHum - tNDredNetHumS
! 	derivative_of_Detritus_DW_in_ditch_sediment
		dDDetS = tDAbioDetS - tDBurDet + tDPrimDetS + tDBedDetS - tDDredDetS
! 	derivative_of_Detritus_P_in_ditch_sediment
		dPDetS = tPAbioDetS - tPBurDet + tPPrimDetS + tPBedDetS - tPDredDetS
! 	derivative_of_Detritus_N_in_ditch_sediment
		dNDetS = tNAbioDetS - tNBurDet + tNPrimDetS + tNBedDetS - tNDredDetS
! 	derivative_of_DW_of_settled_phytoplankton
		dDPhytS = tDPrimPhytS - tDDredPhytS
! 	derivative_of_P_in_settled_phytoplankton
		dPPhytS = tPPrimPhytS - tPDredPhytS
! 	derivative_of_N_in_settled_phytoplankton
		dNPhytS = tNPrimPhytS - tNDredPhytS
! 	derivative_of_DW_of_Elodeids
		dDElod = tDBedElod - tDDredElod
! 	derivative_of_P_in_Elodeids
		dPElod = tPBedElod - tPDredElod
! 	derivative_of_N_in_Elodeids
		dNElod = tNBedElod - tNDredElod
! 	derivative_of_DW_of_Characeans
		dDChar = tDBedChar - tDDredChar
! 	derivative_of_P_in_Characeans
		dPChar = tPBedChar - tPDredChar
! 	derivative_of_N_in_Characeans
		dNChar = tNBedChar - tNDredChar
! 	derivative_of_DW_of_Ceratophyllids
		dDCera = tDBedCera - tDDredCera
! 	derivative_of_P_in_Ceratophyllids
		dPCera = tPBedCera - tPDredCera
! 	derivative_of_N_in_Ceratophyllids
		dNCera = tNBedCera - tNDredCera
! 	derivative_of_DW_of_Lemnacaea
		dDLemn = tDBedLemn - tDDredLemn
! 	derivative_of_P_in_Lemnacaea
		dPLemn = tPBedLemn - tPDredLemn
! 	derivative_of_N_in_Lemnacaea
		dNLemn = tNBedLemn - tNDredLemn
! 	derivative_of_DW_of_Nymphaeids
		dDNymp = tDBedNymp - tDDredNymp
! 	derivative_of_P_in_Nymphaeids
		dPNymp = tPBedNymp - tPDredNymp
! 	derivative_of_N_in_Nymphaeids
		dNNymp = tNBedNymp - tNDredNymp
! 	derivative_of_DW_of_helophytes
		dDHelo = tDBedHelo - tDDredHelo
! 	derivative_of_P_in_helophytes
		dPHelo = tPBedHelo - tPDredHelo
! 	derivative_of_N_in_helophytes
		dNHelo = tNBedHelo - tNDredHelo
! 	derivative_of_Total_amount_of_DW_moved_into_or_out_the_system
		dDExtTotT = uDLoad - wDOutflTot*sDepthW - tDBurTot + tDAbioTotT + tDPrimTotT + t&
		&DBedTotT - tDDredNetTot
! 	derivative_of_Total_amount_of_N_moved_into_or_out_the_system
		dNExtTotT = uNLoad - wNOutflTot*sDepthW + cNBackLoad - tNBurTot - tNDredNetTot +&
		& tNAbioTotT + tNPrimTotT + tNBedTotT
! 	derivative_of_Total_amount_of_P_moved_into_or_out_the_system
		dPExtTotT = uPLoad - wPOutflTot*sDepthW + cPBackLoad - tPBurTot - tPDredNetTot +&
		& tPAbioTotT + tPPrimTotT + tPBedTotT
dDDetS=dDDetS/sDepthW
dDHumS=dDHumS/sDepthW
dDIMS=dDIMS/sDepthW
dNDetS=dNDetS/sDepthW
dNH4S=dNH4S/sDepthW
dNHumS=dNHumS/sDepthW
dNO3S=dNO3S/sDepthW
dPAIMS=dPAIMS/sDepthW
dPDetS=dPDetS/sDepthW
dPHumS=dPHumS/sDepthW
dPO4S=dPO4S/sDepthW
dDPhytS=dDPhytS/sDepthW
dPPhytS=dPPhytS/sDepthW
dNPhytS=dNPhytS/sDepthW
dDElod=dDElod/sDepthW
dDChar=dDChar/sDepthW
dDCera=dDCera/sDepthW
dDLemn=dDLemn/sDepthW
dDNymp=dDNymp/sDepthW
dDHelo=dDHelo/sDepthW
dPElod=dPElod/sDepthW
dPChar=dPChar/sDepthW
dPCera=dPCera/sDepthW
dPLemn=dPLemn/sDepthW
dPNymp=dPNymp/sDepthW
dPHelo=dPHelo/sDepthW
dNElod=dNElod/sDepthW
dNChar=dNChar/sDepthW
dNCera=dNCera/sDepthW
dNLemn=dNLemn/sDepthW
dNNymp=dNNymp/sDepthW
dNHelo=dNHelo/sDepthW
dDExtTotT=dDExtTotT/sDepthW
dNExtTotT=dNExtTotT/sDepthW
dPExtTotT=dPExtTotT/sDepthW
if (counter1 == 0) then
open (unit=1,file="initrep.txt",action="write",status="replace")
write (1,*)"Type Id Name Set0 Set1 Set2 Set3 Rel0-1 Rel0-2 Rel0-3 Dif0-1 Dif0-2 Dif0-3"
write (1,*)" Parameter  0 _BeginTime_", BeginTime
write (1,*)" Parameter  1 _ConstDepth_", ConstDepth
write (1,*)" Parameter  2 _ReadTemp_", ReadTemp
write (1,*)" Parameter  3 _ReadLOut_", ReadLOut
write (1,*)" Parameter  4 _ReadVWind_", ReadVWind
write (1,*)" Parameter  5 _InitCalc_", InitCalc
write (1,*)" Parameter  6 _InclTran_", InclTran
write (1,*)" Parameter  7 _ReadQIn_", ReadQIn
write (1,*)" Parameter  8 _ReadQOut_", ReadQOut
write (1,*)" Parameter  9 _ReadQEv_", ReadQEv
write (1,*)" Parameter  10 _ReadPLoad_", ReadPLoad
write (1,*)" Parameter  11 _ReadNLoad_", ReadNLoad
write (1,*)" Parameter  12 _ReadNutFrac_", ReadNutFrac
write (1,*)" Parameter  13 _ReadPLoadPhyt_", ReadPLoadPhyt
write (1,*)" Parameter  14 _ReadDLoadDet_", ReadDLoadDet
write (1,*)" Parameter  15 _ReadDLoadIM_", ReadDLoadIM
write (1,*)" Parameter  16 _UseSeasonLoad_", UseSeasonLoad
write (1,*)" Parameter  17 _UsePulseLoad_", UsePulseLoad
write (1,*)" Parameter  18 _mTemp_", mTemp
write (1,*)" Parameter  19 _mLOut_", mLOut
write (1,*)" Parameter  20 _mVWind_", mVWind
write (1,*)" Parameter  21 _mQIn_", mQIn
write (1,*)" Parameter  22 _mQOut_", mQOut
write (1,*)" Parameter  23 _mQEv_", mQEv
write (1,*)" Parameter  24 _mPLoad_", mPLoad
write (1,*)" Parameter  25 _mPLoadPO4_", mPLoadPO4
write (1,*)" Parameter  26 _mPLoadOrg_", mPLoadOrg
write (1,*)" Parameter  27 _mPLoadPhytTot_", mPLoadPhytTot
write (1,*)" Parameter  28 _mNLoad_", mNLoad
write (1,*)" Parameter  29 _mNLoadNH4_", mNLoadNH4
write (1,*)" Parameter  30 _mNLoadNO3_", mNLoadNO3
write (1,*)" Parameter  31 _mNLoadOrg_", mNLoadOrg
write (1,*)" Parameter  32 _mDLoadDet_", mDLoadDet
write (1,*)" Parameter  33 _mDLoadIM_", mDLoadIM
write (1,*)" Parameter  34 _fDTotS0_", fDTotS0
write (1,*)" Parameter  35 _fDOrgS0_", fDOrgS0
write (1,*)" Parameter  36 _fDDetS0_", fDDetS0
write (1,*)" Parameter  37 _fPInorgS0_", fPInorgS0
write (1,*)" Parameter  38 _fPAdsS0_", fPAdsS0
write (1,*)" Parameter  39 _cPDDet0_", cPDDet0
write (1,*)" Parameter  40 _cNDDet0_", cNDDet0
write (1,*)" Parameter  41 _cPDHum0_", cPDHum0
write (1,*)" Parameter  42 _cNDHum0_", cNDHum0
write (1,*)" Parameter  43 _cPDPhyt0_", cPDPhyt0
write (1,*)" Parameter  44 _cNDPhyt0_", cNDPhyt0
write (1,*)" Parameter  45 _cPDElod0_", cPDElod0
write (1,*)" Parameter  46 _cNDElod0_", cNDElod0
write (1,*)" Parameter  47 _cPDChar0_", cPDChar0
write (1,*)" Parameter  48 _cNDChar0_", cNDChar0
write (1,*)" Parameter  49 _cPDCera0_", cPDCera0
write (1,*)" Parameter  50 _cNDCera0_", cNDCera0
write (1,*)" Parameter  51 _cPDLemn0_", cPDLemn0
write (1,*)" Parameter  52 _cNDLemn0_", cNDLemn0
write (1,*)" Parameter  53 _cPDNymp0_", cPDNymp0
write (1,*)" Parameter  54 _cNDNymp0_", cNDNymp0
write (1,*)" Parameter  55 _cPDHelo0_", cPDHelo0
write (1,*)" Parameter  56 _cNDHelo0_", cNDHelo0
write (1,*)" Parameter  57 _cQInf_", cQInf
write (1,*)" Parameter  58 _cPBackLoad_", cPBackLoad
write (1,*)" Parameter  59 _cNBackLoad_", cNBackLoad
write (1,*)" Parameter  60 _cNLoadS_", cNLoadS
write (1,*)" Parameter  61 _fNH4LoadS_", fNH4LoadS
write (1,*)" Parameter  62 _cPO4Ground_", cPO4Ground
write (1,*)" Parameter  63 _cNH4Ground_", cNH4Ground
write (1,*)" Parameter  64 _cNO3Ground_", cNO3Ground
write (1,*)" Parameter  65 _cQIn_", cQIn
write (1,*)" Parameter  66 _cQInSum_", cQInSum
write (1,*)" Parameter  67 _cQInWin_", cQInWin
write (1,*)" Parameter  68 _cDepthWMax_", cDepthWMax
write (1,*)" Parameter  69 _cQInExtraApril1_", cQInExtraApril1
write (1,*)" Parameter  70 _cQInExtraOct1_", cQInExtraOct1
write (1,*)" Parameter  71 _cQOutExtraApril1_", cQOutExtraApril1
write (1,*)" Parameter  72 _cQOutExtraOct1_", cQOutExtraOct1
write (1,*)" Parameter  73 _cQEvAve_", cQEvAve
write (1,*)" Parameter  74 _cQEvVar_", cQEvVar
write (1,*)" Parameter  75 _cPLoad_", cPLoad
write (1,*)" Parameter  76 _cPLoadSum_", cPLoadSum
write (1,*)" Parameter  77 _cPLoadWin_", cPLoadWin
write (1,*)" Parameter  78 _fPO4In_", fPO4In
write (1,*)" Parameter  79 _fPhytInWin_", fPhytInWin
write (1,*)" Parameter  80 _fPhytInSum_", fPhytInSum
write (1,*)" Parameter  81 _cNLoad_", cNLoad
write (1,*)" Parameter  82 _cNLoadSum_", cNLoadSum
write (1,*)" Parameter  83 _cNLoadWin_", cNLoadWin
write (1,*)" Parameter  84 _cNPLoadMeas_", cNPLoadMeas
write (1,*)" Parameter  85 _cNPPhytIn_", cNPPhytIn
write (1,*)" Parameter  86 _cNPDetIn_", cNPDetIn
write (1,*)" Parameter  87 _fNH4DissIn_", fNH4DissIn
write (1,*)" Parameter  88 _cNDPhytIn_", cNDPhytIn
write (1,*)" Parameter  89 _cNDDetIn_", cNDDetIn
write (1,*)" Parameter  90 _cDIMIn_", cDIMIn
write (1,*)" Parameter  91 _cO2In_", cO2In
write (1,*)" Parameter  92 _cDredInterval_", cDredInterval
write (1,*)" Parameter  93 _cDredStart_", cDredStart
write (1,*)" Parameter  94 _cDepthRef_", cDepthRef
write (1,*)" Parameter  95 _cLengDred_", cLengDred
write (1,*)" Parameter  96 _fEffDred_", fEffDred
write (1,*)" Parameter  97 _fEffDredLemn_", fEffDredLemn
write (1,*)" Parameter  98 _cFetch_", cFetch
write (1,*)" Parameter  99 _cTmAve_", cTmAve
write (1,*)" Parameter  100 _cTmVar_", cTmVar
write (1,*)" Parameter  101 _cTimeLag_", cTimeLag
write (1,*)" Parameter  102 _cVWind_", cVWind
write (1,*)" Parameter  103 _cLDayAve_", cLDayAve
write (1,*)" Parameter  104 _cLDayVar_", cLDayVar
write (1,*)" Parameter  105 _cfDayAve_", cfDayAve
write (1,*)" Parameter  106 _cfDayVar_", cfDayVar
write (1,*)" Parameter  107 _fRefl_", fRefl
write (1,*)" Parameter  108 _fPAR_", fPAR
write (1,*)" Parameter  109 _cDayManVeg1_", cDayManVeg1
write (1,*)" Parameter  110 _cDayManVeg2_", cDayManVeg2
write (1,*)" Parameter  111 _fManVeg_", fManVeg
write (1,*)" Parameter  112 _fManLemn_", fManLemn
write (1,*)" Parameter  113 _fManHelo_", fManHelo
write (1,*)" Parameter  114 _cLengMan_", cLengMan
write (1,*)" Parameter  115 _cYearStartBirds_", cYearStartBirds
write (1,*)" Parameter  116 _cDayStartBirds_", cDayStartBirds
write (1,*)" Parameter  117 _cDayEndBirds_", cDayEndBirds
write (1,*)" Parameter  118 _cBirdsPerha_", cBirdsPerha
write (1,*)" Parameter  119 _cDGrazPerBird_", cDGrazPerBird
write (1,*)" Parameter  120 _hDVegBird_", hDVegBird
write (1,*)" Parameter  121 _fDAssBird_", fDAssBird
write (1,*)" Parameter  122 _fDissEgesBird_", fDissEgesBird
write (1,*)" Parameter  123 _cDErosTot_", cDErosTot
write (1,*)" Parameter  124 _cExtWat_", cExtWat
write (1,*)" Parameter  125 _cExtSpDet_", cExtSpDet
write (1,*)" Parameter  126 _cExtSpIM_", cExtSpIM
write (1,*)" Parameter  127 _cExtSpPhyt_", cExtSpPhyt
write (1,*)" Parameter  128 _fDOrgSoil_", fDOrgSoil
write (1,*)" Parameter  129 _cPDSoilOM_", cPDSoilOM
write (1,*)" Parameter  130 _cNDSoilOM_", cNDSoilOM
write (1,*)" Parameter  131 _cDepthS_", cDepthS
write (1,*)" Parameter  132 _fLutum_", fLutum
write (1,*)" Parameter  133 _fFeDIM_", fFeDIM
write (1,*)" Parameter  134 _fAlDIM_", fAlDIM
write (1,*)" Parameter  135 _cCPerDW_", cCPerDW
write (1,*)" Parameter  136 _cRhoIM_", cRhoIM
write (1,*)" Parameter  137 _cRhoOM_", cRhoOM
write (1,*)" Parameter  138 _cAerRoot_", cAerRoot
write (1,*)" Parameter  139 _cAerLin_", cAerLin
write (1,*)" Parameter  140 _cAerSquare_", cAerSquare
write (1,*)" Parameter  141 _cThetaAer_", cThetaAer
write (1,*)" Parameter  142 _kLemnAer_", kLemnAer
write (1,*)" Parameter  143 _fSedErosIM_", fSedErosIM
write (1,*)" Parameter  144 _cVSetIM_", cVSetIM
write (1,*)" Parameter  145 _cVSetDet_", cVSetDet
write (1,*)" Parameter  146 _cThetaSet_", cThetaSet
write (1,*)" Parameter  147 _cSuspRef_", cSuspRef
write (1,*)" Parameter  148 _cSuspMin_", cSuspMin
write (1,*)" Parameter  149 _cSuspMax_", cSuspMax
write (1,*)" Parameter  150 _cSuspSlope_", cSuspSlope
write (1,*)" Parameter  151 _hDepthSusp_", hDepthSusp
write (1,*)" Parameter  152 _cFetchRef_", cFetchRef
write (1,*)" Parameter  153 _fLutumRef_", fLutumRef
write (1,*)" Parameter  154 _kVegResus_", kVegResus
write (1,*)" Parameter  155 _kResusPhytMax_", kResusPhytMax
write (1,*)" Parameter  156 _cResusPhytExp_", cResusPhytExp
write (1,*)" Parameter  157 _kPDifPO4_", kPDifPO4
write (1,*)" Parameter  158 _kNDifNO3_", kNDifNO3
write (1,*)" Parameter  159 _kNDifNH4_", kNDifNH4
write (1,*)" Parameter  160 _kO2Dif_", kO2Dif
write (1,*)" Parameter  161 _cThetaDif_", cThetaDif
write (1,*)" Parameter  162 _fDepthDifS_", fDepthDifS
write (1,*)" Parameter  163 _cTurbDifNut_", cTurbDifNut
write (1,*)" Parameter  164 _cTurbDifO2_", cTurbDifO2
write (1,*)" Parameter  165 _kPSorp_", kPSorp
write (1,*)" Parameter  166 _cRelPAdsD_", cRelPAdsD
write (1,*)" Parameter  167 _cRelPAdsFe_", cRelPAdsFe
write (1,*)" Parameter  168 _cRelPAdsAl_", cRelPAdsAl
write (1,*)" Parameter  169 _cKPAdsOx_", cKPAdsOx
write (1,*)" Parameter  170 _fRedMax_", fRedMax
write (1,*)" Parameter  171 _coPO4Max_", coPO4Max
write (1,*)" Parameter  172 _kPChemPO4_", kPChemPO4
write (1,*)" Parameter  173 _cTmRef_", cTmRef
write (1,*)" Parameter  174 _fRefrDetS_", fRefrDetS
write (1,*)" Parameter  175 _cThetaMinW_", cThetaMinW
write (1,*)" Parameter  176 _kDMinDetW_", kDMinDetW
write (1,*)" Parameter  177 _hO2BOD_", hO2BOD
write (1,*)" Parameter  178 _cThetaMinS_", cThetaMinS
write (1,*)" Parameter  179 _kDMinDetS_", kDMinDetS
write (1,*)" Parameter  180 _kDMinHum_", kDMinHum
write (1,*)" Parameter  181 _hNO3Denit_", hNO3Denit
write (1,*)" Parameter  182 _NO3PerC_", NO3PerC
write (1,*)" Parameter  183 _kNitrW_", kNitrW
write (1,*)" Parameter  184 _kNitrS_", kNitrS
write (1,*)" Parameter  185 _cThetaNitr_", cThetaNitr
write (1,*)" Parameter  186 _O2PerNH4_", O2PerNH4
write (1,*)" Parameter  187 _hO2Nitr_", hO2Nitr
write (1,*)" Parameter  188 _fDissMortVeg_", fDissMortVeg
write (1,*)" Parameter  189 _cLengAllo_", cLengAllo
write (1,*)" Parameter  190 _cLengMort_", cLengMort
write (1,*)" Parameter  191 _fRootElodSum_", fRootElodSum
write (1,*)" Parameter  192 _fRootElodWin_", fRootElodWin
write (1,*)" Parameter  193 _fFloatElod_", fFloatElod
write (1,*)" Parameter  194 _fEmergElod_", fEmergElod
write (1,*)" Parameter  195 _fDepth1Elod_", fDepth1Elod
write (1,*)" Parameter  196 _fDepth2Elod_", fDepth2Elod
write (1,*)" Parameter  197 _cDLayerElod_", cDLayerElod
write (1,*)" Parameter  198 _cCovSpElod_", cCovSpElod
write (1,*)" Parameter  199 _kMigrElod_", kMigrElod
write (1,*)" Parameter  200 _cDElodIn_", cDElodIn
write (1,*)" Parameter  201 _cTmInitElod_", cTmInitElod
write (1,*)" Parameter  202 _cDCarrElod_", cDCarrElod
write (1,*)" Parameter  203 _cMuMaxElod_", cMuMaxElod
write (1,*)" Parameter  204 _cQ10ProdElod_", cQ10ProdElod
write (1,*)" Parameter  205 _hLRefElod_", hLRefElod
write (1,*)" Parameter  206 _cExtSpElod_", cExtSpElod
write (1,*)" Parameter  207 _kDRespElod_", kDRespElod
write (1,*)" Parameter  208 _cQ10RespElod_", cQ10RespElod
write (1,*)" Parameter  209 _cDayWinElod_", cDayWinElod
write (1,*)" Parameter  210 _kMortElodSum_", kMortElodSum
write (1,*)" Parameter  211 _fWinElod_", fWinElod
write (1,*)" Parameter  212 _fDetWMortElod_", fDetWMortElod
write (1,*)" Parameter  213 _cPrefElodBird_", cPrefElodBird
write (1,*)" Parameter  214 _cVPUptMaxElod_", cVPUptMaxElod
write (1,*)" Parameter  215 _cAffPUptElod_", cAffPUptElod
write (1,*)" Parameter  216 _cPDElodMin_", cPDElodMin
write (1,*)" Parameter  217 _cPDElodMax_", cPDElodMax
write (1,*)" Parameter  218 _cVNUptMaxElod_", cVNUptMaxElod
write (1,*)" Parameter  219 _cAffNUptElod_", cAffNUptElod
write (1,*)" Parameter  220 _cNDElodMin_", cNDElodMin
write (1,*)" Parameter  221 _cNDElodMax_", cNDElodMax
write (1,*)" Parameter  222 _fRootCharSum_", fRootCharSum
write (1,*)" Parameter  223 _fRootCharWin_", fRootCharWin
write (1,*)" Parameter  224 _fFloatChar_", fFloatChar
write (1,*)" Parameter  225 _fEmergChar_", fEmergChar
write (1,*)" Parameter  226 _fDepth1Char_", fDepth1Char
write (1,*)" Parameter  227 _fDepth2Char_", fDepth2Char
write (1,*)" Parameter  228 _cDLayerChar_", cDLayerChar
write (1,*)" Parameter  229 _cCovSpChar_", cCovSpChar
write (1,*)" Parameter  230 _kMigrChar_", kMigrChar
write (1,*)" Parameter  231 _cDCharIn_", cDCharIn
write (1,*)" Parameter  232 _cTmInitChar_", cTmInitChar
write (1,*)" Parameter  233 _cDCarrChar_", cDCarrChar
write (1,*)" Parameter  234 _cMuMaxChar_", cMuMaxChar
write (1,*)" Parameter  235 _cQ10ProdChar_", cQ10ProdChar
write (1,*)" Parameter  236 _hLRefChar_", hLRefChar
write (1,*)" Parameter  237 _cExtSpChar_", cExtSpChar
write (1,*)" Parameter  238 _kDRespChar_", kDRespChar
write (1,*)" Parameter  239 _cQ10RespChar_", cQ10RespChar
write (1,*)" Parameter  240 _cDayWinChar_", cDayWinChar
write (1,*)" Parameter  241 _kMortCharSum_", kMortCharSum
write (1,*)" Parameter  242 _fWinChar_", fWinChar
write (1,*)" Parameter  243 _fDetWMortChar_", fDetWMortChar
write (1,*)" Parameter  244 _cPrefCharBird_", cPrefCharBird
write (1,*)" Parameter  245 _cVPUptMaxChar_", cVPUptMaxChar
write (1,*)" Parameter  246 _cAffPUptChar_", cAffPUptChar
write (1,*)" Parameter  247 _cPDCharMin_", cPDCharMin
write (1,*)" Parameter  248 _cPDCharMax_", cPDCharMax
write (1,*)" Parameter  249 _cVNUptMaxChar_", cVNUptMaxChar
write (1,*)" Parameter  250 _cAffNUptChar_", cAffNUptChar
write (1,*)" Parameter  251 _cNDCharMin_", cNDCharMin
write (1,*)" Parameter  252 _cNDCharMax_", cNDCharMax
write (1,*)" Parameter  253 _fRootCeraSum_", fRootCeraSum
write (1,*)" Parameter  254 _fRootCeraWin_", fRootCeraWin
write (1,*)" Parameter  255 _fFloatCera_", fFloatCera
write (1,*)" Parameter  256 _fEmergCera_", fEmergCera
write (1,*)" Parameter  257 _fDepth1Cera_", fDepth1Cera
write (1,*)" Parameter  258 _fDepth2Cera_", fDepth2Cera
write (1,*)" Parameter  259 _cDLayerCera_", cDLayerCera
write (1,*)" Parameter  260 _cCovSpCera_", cCovSpCera
write (1,*)" Parameter  261 _kMigrCera_", kMigrCera
write (1,*)" Parameter  262 _cDCeraIn_", cDCeraIn
write (1,*)" Parameter  263 _cTmInitCera_", cTmInitCera
write (1,*)" Parameter  264 _cDCarrCera_", cDCarrCera
write (1,*)" Parameter  265 _cMuMaxCera_", cMuMaxCera
write (1,*)" Parameter  266 _cQ10ProdCera_", cQ10ProdCera
write (1,*)" Parameter  267 _hLRefCera_", hLRefCera
write (1,*)" Parameter  268 _cExtSpCera_", cExtSpCera
write (1,*)" Parameter  269 _kDRespCera_", kDRespCera
write (1,*)" Parameter  270 _cQ10RespCera_", cQ10RespCera
write (1,*)" Parameter  271 _cDayWinCera_", cDayWinCera
write (1,*)" Parameter  272 _kMortCeraSum_", kMortCeraSum
write (1,*)" Parameter  273 _fWinCera_", fWinCera
write (1,*)" Parameter  274 _fDetWMortCera_", fDetWMortCera
write (1,*)" Parameter  275 _cPrefCeraBird_", cPrefCeraBird
write (1,*)" Parameter  276 _cVPUptMaxCera_", cVPUptMaxCera
write (1,*)" Parameter  277 _cAffPUptCera_", cAffPUptCera
write (1,*)" Parameter  278 _cPDCeraMin_", cPDCeraMin
write (1,*)" Parameter  279 _cPDCeraMax_", cPDCeraMax
write (1,*)" Parameter  280 _cVNUptMaxCera_", cVNUptMaxCera
write (1,*)" Parameter  281 _cAffNUptCera_", cAffNUptCera
write (1,*)" Parameter  282 _cNDCeraMin_", cNDCeraMin
write (1,*)" Parameter  283 _cNDCeraMax_", cNDCeraMax
write (1,*)" Parameter  284 _fObstrLemn_", fObstrLemn
write (1,*)" Parameter  285 _fRootLemnSum_", fRootLemnSum
write (1,*)" Parameter  286 _fRootLemnWin_", fRootLemnWin
write (1,*)" Parameter  287 _fFloatLemn_", fFloatLemn
write (1,*)" Parameter  288 _fEmergLemn_", fEmergLemn
write (1,*)" Parameter  289 _fDepth1Lemn_", fDepth1Lemn
write (1,*)" Parameter  290 _fDepth2Lemn_", fDepth2Lemn
write (1,*)" Parameter  291 _cDLayerLemn_", cDLayerLemn
write (1,*)" Parameter  292 _cCovSpLemn_", cCovSpLemn
write (1,*)" Parameter  293 _ckMigrLemn_", ckMigrLemn
write (1,*)" Parameter  294 _cDLemnIn_", cDLemnIn
write (1,*)" Parameter  295 _cTmInitLemn_", cTmInitLemn
write (1,*)" Parameter  296 _cDCarrLemn_", cDCarrLemn
write (1,*)" Parameter  297 _cMuMaxLemn_", cMuMaxLemn
write (1,*)" Parameter  298 _cQ10ProdLemn_", cQ10ProdLemn
write (1,*)" Parameter  299 _hLRefLemn_", hLRefLemn
write (1,*)" Parameter  300 _cExtSpLemn_", cExtSpLemn
write (1,*)" Parameter  301 _kDRespLemn_", kDRespLemn
write (1,*)" Parameter  302 _cQ10RespLemn_", cQ10RespLemn
write (1,*)" Parameter  303 _cDayWinLemn_", cDayWinLemn
write (1,*)" Parameter  304 _kMortLemnSum_", kMortLemnSum
write (1,*)" Parameter  305 _fWinLemn_", fWinLemn
write (1,*)" Parameter  306 _fDetWMortLemn_", fDetWMortLemn
write (1,*)" Parameter  307 _cPrefLemnBird_", cPrefLemnBird
write (1,*)" Parameter  308 _cVPUptMaxLemn_", cVPUptMaxLemn
write (1,*)" Parameter  309 _cAffPUptLemn_", cAffPUptLemn
write (1,*)" Parameter  310 _cPDLemnMin_", cPDLemnMin
write (1,*)" Parameter  311 _cPDLemnMax_", cPDLemnMax
write (1,*)" Parameter  312 _cVNUptMaxLemn_", cVNUptMaxLemn
write (1,*)" Parameter  313 _cAffNUptLemn_", cAffNUptLemn
write (1,*)" Parameter  314 _cNDLemnMin_", cNDLemnMin
write (1,*)" Parameter  315 _cNDLemnMax_", cNDLemnMax
write (1,*)" Parameter  316 _fRootNympSum_", fRootNympSum
write (1,*)" Parameter  317 _fRootNympWin_", fRootNympWin
write (1,*)" Parameter  318 _fFloatNymp_", fFloatNymp
write (1,*)" Parameter  319 _fEmergNymp_", fEmergNymp
write (1,*)" Parameter  320 _fDepth1Nymp_", fDepth1Nymp
write (1,*)" Parameter  321 _fDepth2Nymp_", fDepth2Nymp
write (1,*)" Parameter  322 _cDLayerNymp_", cDLayerNymp
write (1,*)" Parameter  323 _cCovSpNymp_", cCovSpNymp
write (1,*)" Parameter  324 _kMigrNymp_", kMigrNymp
write (1,*)" Parameter  325 _cDNympIn_", cDNympIn
write (1,*)" Parameter  326 _cTmInitNymp_", cTmInitNymp
write (1,*)" Parameter  327 _cDCarrNymp_", cDCarrNymp
write (1,*)" Parameter  328 _cMuMaxNymp_", cMuMaxNymp
write (1,*)" Parameter  329 _cQ10ProdNymp_", cQ10ProdNymp
write (1,*)" Parameter  330 _hLRefNymp_", hLRefNymp
write (1,*)" Parameter  331 _cExtSpNymp_", cExtSpNymp
write (1,*)" Parameter  332 _kDRespNymp_", kDRespNymp
write (1,*)" Parameter  333 _cQ10RespNymp_", cQ10RespNymp
write (1,*)" Parameter  334 _cDayWinNymp_", cDayWinNymp
write (1,*)" Parameter  335 _kMortNympSum_", kMortNympSum
write (1,*)" Parameter  336 _fWinNymp_", fWinNymp
write (1,*)" Parameter  337 _fDetWMortNymp_", fDetWMortNymp
write (1,*)" Parameter  338 _cPrefNympBird_", cPrefNympBird
write (1,*)" Parameter  339 _cVPUptMaxNymp_", cVPUptMaxNymp
write (1,*)" Parameter  340 _cAffPUptNymp_", cAffPUptNymp
write (1,*)" Parameter  341 _cPDNympMin_", cPDNympMin
write (1,*)" Parameter  342 _cPDNympMax_", cPDNympMax
write (1,*)" Parameter  343 _cVNUptMaxNymp_", cVNUptMaxNymp
write (1,*)" Parameter  344 _cAffNUptNymp_", cAffNUptNymp
write (1,*)" Parameter  345 _cNDNympMin_", cNDNympMin
write (1,*)" Parameter  346 _cNDNympMax_", cNDNympMax
write (1,*)" Parameter  347 _fRootHeloSum_", fRootHeloSum
write (1,*)" Parameter  348 _fRootHeloWin_", fRootHeloWin
write (1,*)" Parameter  349 _fFloatHelo_", fFloatHelo
write (1,*)" Parameter  350 _fEmergHelo_", fEmergHelo
write (1,*)" Parameter  351 _fDepth1Helo_", fDepth1Helo
write (1,*)" Parameter  352 _fDepth2Helo_", fDepth2Helo
write (1,*)" Parameter  353 _cDLayerHelo_", cDLayerHelo
write (1,*)" Parameter  354 _cCovSpHelo_", cCovSpHelo
write (1,*)" Parameter  355 _kMigrHelo_", kMigrHelo
write (1,*)" Parameter  356 _cDHeloIn_", cDHeloIn
write (1,*)" Parameter  357 _cTmInitHelo_", cTmInitHelo
write (1,*)" Parameter  358 _cDCarrHelo_", cDCarrHelo
write (1,*)" Parameter  359 _cMuMaxHelo_", cMuMaxHelo
write (1,*)" Parameter  360 _cQ10ProdHelo_", cQ10ProdHelo
write (1,*)" Parameter  361 _hLRefHelo_", hLRefHelo
write (1,*)" Parameter  362 _cExtSpHelo_", cExtSpHelo
write (1,*)" Parameter  363 _kDRespHelo_", kDRespHelo
write (1,*)" Parameter  364 _cQ10RespHelo_", cQ10RespHelo
write (1,*)" Parameter  365 _cDayWinHelo_", cDayWinHelo
write (1,*)" Parameter  366 _kMortHeloSum_", kMortHeloSum
write (1,*)" Parameter  367 _fWinHelo_", fWinHelo
write (1,*)" Parameter  368 _fDetWMortHelo_", fDetWMortHelo
write (1,*)" Parameter  369 _cPrefHeloBird_", cPrefHeloBird
write (1,*)" Parameter  370 _cVPUptMaxHelo_", cVPUptMaxHelo
write (1,*)" Parameter  371 _cAffPUptHelo_", cAffPUptHelo
write (1,*)" Parameter  372 _cPDHeloMin_", cPDHeloMin
write (1,*)" Parameter  373 _cPDHeloMax_", cPDHeloMax
write (1,*)" Parameter  374 _cVNUptMaxHelo_", cVNUptMaxHelo
write (1,*)" Parameter  375 _cAffNUptHelo_", cAffNUptHelo
write (1,*)" Parameter  376 _cNDHeloMin_", cNDHeloMin
write (1,*)" Parameter  377 _cNDHeloMax_", cNDHeloMax
write (1,*)" Parameter  378 _cPACoefMin_", cPACoefMin
write (1,*)" Parameter  379 _cPACoefMax_", cPACoefMax
write (1,*)" Parameter  380 _hPACoef_", hPACoef
write (1,*)" Parameter  381 _cSecchiPlus_", cSecchiPlus
write (1,*)" Parameter  382 _cEuph_", cEuph
write (1,*)" Parameter  383 _cCovSpPhyt_", cCovSpPhyt
write (1,*)" Parameter  384 _cTmOptLoss_", cTmOptLoss
write (1,*)" Parameter  385 _cSigTmLoss_", cSigTmLoss
write (1,*)" Parameter  386 _fDissMortPhyt_", fDissMortPhyt
write (1,*)" Parameter  387 _fDissLoss_", fDissLoss
write (1,*)" Parameter  388 _cMuMaxPhyt_", cMuMaxPhyt
write (1,*)" Parameter  389 _cTmOptPhyt_", cTmOptPhyt
write (1,*)" Parameter  390 _cSigTmPhyt_", cSigTmPhyt
write (1,*)" Parameter  391 _UseSteelePhyt_", UseSteelePhyt
write (1,*)" Parameter  392 _hLRefPhyt_", hLRefPhyt
write (1,*)" Parameter  393 _cLOptRefPhyt_", cLOptRefPhyt
write (1,*)" Parameter  394 _cChDPhytMin_", cChDPhytMin
write (1,*)" Parameter  395 _cChDPhytMax_", cChDPhytMax
write (1,*)" Parameter  396 _kDRespPhyt_", kDRespPhyt
write (1,*)" Parameter  397 _kLossPhyt_", kLossPhyt
write (1,*)" Parameter  398 _kMortPhytW_", kMortPhytW
write (1,*)" Parameter  399 _cVSetPhyt_", cVSetPhyt
write (1,*)" Parameter  400 _kMortPhytS_", kMortPhytS
write (1,*)" Parameter  401 _cVPUptMaxPhyt_", cVPUptMaxPhyt
write (1,*)" Parameter  402 _cAffPUptPhyt_", cAffPUptPhyt
write (1,*)" Parameter  403 _cPDPhytMin_", cPDPhytMin
write (1,*)" Parameter  404 _cPDPhytMax_", cPDPhytMax
write (1,*)" Parameter  405 _cVNUptMaxPhyt_", cVNUptMaxPhyt
write (1,*)" Parameter  406 _cAffNUptPhyt_", cAffNUptPhyt
write (1,*)" Parameter  407 _cNDPhytMin_", cNDPhytMin
write (1,*)" Parameter  408 _cNDPhytMax_", cNDPhytMax
write (1,*)" Parameter  409 _O2PerNO3_", O2PerNO3
write (1,*)" Parameter  410 _cDayApril1_", cDayApril1
write (1,*)" Parameter  411 _cDayOct1_", cDayOct1
write (1,*)" Parameter  412 _cLengChange_", cLengChange
write (1,*)" Parameter  413 _DaysPerYear_", DaysPerYear
write (1,*)" Parameter  414 _TenDays_", TenDays
write (1,*)" Parameter  415 _SecsPerDay_", SecsPerDay
write (1,*)" Parameter  416 _mmPerm_", mmPerm
write (1,*)" Parameter  417 _m2Perha_", m2Perha
write (1,*)" Parameter  418 _mgPerg_", mgPerg
write (1,*)" Parameter  419 _PerCent_", PerCent
write (1,*)" Parameter  420 _NearZero_", NearZero
write (1,*)" Parameter  421 _molO2molC_", molO2molC
write (1,*)" Parameter  422 _molO2molN_", molO2molN
write (1,*)" Parameter  423 _molNmolC_", molNmolC
write (1,*)" Parameter  424 _cRhoWat_", cRhoWat
write (1,*)" Parameter  425 _Pi_", Pi
write (1,*)" InitState  0 _cDDetS0_", sDDetS
write (1,*)" InitState  1 _cDDetW0_", sDDetW
write (1,*)" InitState  2 _cDepthW0_", sDepthW
write (1,*)" InitState  3 _cDHumS0_", sDHumS
write (1,*)" InitState  4 _cDIMS0_", sDIMS
write (1,*)" InitState  5 _cDIMW0_", sDIMW
write (1,*)" InitState  6 _cNDetS0_", sNDetS
write (1,*)" InitState  7 _cNDetW0_", sNDetW
write (1,*)" InitState  8 _cNH4S0_", sNH4S
write (1,*)" InitState  9 _cNH4W0_", sNH4W
write (1,*)" InitState  10 _cNHumS0_", sNHumS
write (1,*)" InitState  11 _cNO3S0_", sNO3S
write (1,*)" InitState  12 _cNO3W0_", sNO3W
write (1,*)" InitState  13 _cO2W0_", sO2W
write (1,*)" InitState  14 _cPAIMS0_", sPAIMS
write (1,*)" InitState  15 _cPAIMW0_", sPAIMW
write (1,*)" InitState  16 _cPDetS0_", sPDetS
write (1,*)" InitState  17 _cPDetW0_", sPDetW
write (1,*)" InitState  18 _cPHumS0_", sPHumS
write (1,*)" InitState  19 _cPO4S0_", sPO4S
write (1,*)" InitState  20 _cPO4W0_", sPO4W
write (1,*)" InitState  21 _cDPhytW0_", sDPhytW
write (1,*)" InitState  22 _cPPhytW0_", sPPhytW
write (1,*)" InitState  23 _cNPhytW0_", sNPhytW
write (1,*)" InitState  24 _cDPhytS0_", sDPhytS
write (1,*)" InitState  25 _cPPhytS0_", sPPhytS
write (1,*)" InitState  26 _cNPhytS0_", sNPhytS
write (1,*)" InitState  27 _cDElod0_", sDElod
write (1,*)" InitState  28 _cDChar0_", sDChar
write (1,*)" InitState  29 _cDCera0_", sDCera
write (1,*)" InitState  30 _cDLemn0_", sDLemn
write (1,*)" InitState  31 _cDNymp0_", sDNymp
write (1,*)" InitState  32 _cDHelo0_", sDHelo
write (1,*)" InitState  33 _cPElod0_", sPElod
write (1,*)" InitState  34 _cPChar0_", sPChar
write (1,*)" InitState  35 _cPCera0_", sPCera
write (1,*)" InitState  36 _cPLemn0_", sPLemn
write (1,*)" InitState  37 _cPNymp0_", sPNymp
write (1,*)" InitState  38 _cPHelo0_", sPHelo
write (1,*)" InitState  39 _cNElod0_", sNElod
write (1,*)" InitState  40 _cNChar0_", sNChar
write (1,*)" InitState  41 _cNCera0_", sNCera
write (1,*)" InitState  42 _cNLemn0_", sNLemn
write (1,*)" InitState  43 _cNNymp0_", sNNymp
write (1,*)" InitState  44 _cNHelo0_", sNHelo
write (1,*)" InitState  45 _cDExtTotT0_", sDExtTotT
write (1,*)" InitState  46 _cNExtTotT0_", sNExtTotT
write (1,*)" InitState  47 _cPExtTotT0_", sPExtTotT
write (1,*)" InitState  0 _bRhoSolidS0_ 0"
write (1,*)" InitState  1 _bPorS0_ 0"
write (1,*)" InitState  2 _bRhoTotS0_ 0"
write (1,*)" InitState  3 _bDTotS0_ 0"
write (1,*)" InitState  4 _sDepthW0_ 0"
write (1,*)" InitState  5 _sDIMW0_ 0"
write (1,*)" InitState  6 _sDDetW0_ 0"
write (1,*)" InitState  7 _sPDetW0_ 0"
write (1,*)" InitState  8 _sNDetW0_ 0"
write (1,*)" InitState  9 _sNH4W0_ 0"
write (1,*)" InitState  10 _sNO3W0_ 0"
write (1,*)" InitState  11 _sO2W0_ 0"
write (1,*)" InitState  12 _sPO4W0_ 0"
write (1,*)" InitState  13 _sPAIMW0_ 0"
write (1,*)" InitState  14 _sNH4S0_ 0"
write (1,*)" InitState  15 _sNO3S0_ 0"
write (1,*)" InitState  16 _sDHumS0_ 0"
write (1,*)" InitState  17 _sDDetS0_ 0"
write (1,*)" InitState  18 _sDIMS0_ 0"
write (1,*)" InitState  19 _sPHumS0_ 0"
write (1,*)" InitState  20 _sNHumS0_ 0"
write (1,*)" InitState  21 _sPDetS0_ 0"
write (1,*)" InitState  22 _sNDetS0_ 0"
write (1,*)" InitState  23 _sPAIMS0_ 0"
write (1,*)" InitState  24 _sPO4S0_ 0"
write (1,*)" InitState  25 _sDPhytW0_ 0"
write (1,*)" InitState  26 _sPPhytW0_ 0"
write (1,*)" InitState  27 _sNPhytW0_ 0"
write (1,*)" InitState  28 _sDPhytS0_ 0"
write (1,*)" InitState  29 _sPPhytS0_ 0"
write (1,*)" InitState  30 _sNPhytS0_ 0"
write (1,*)" InitState  31 _sDChar0_ 0"
write (1,*)" InitState  32 _sPChar0_ 0"
write (1,*)" InitState  33 _sNChar0_ 0"
write (1,*)" InitState  34 _sDElod0_ 0"
write (1,*)" InitState  35 _sPElod0_ 0"
write (1,*)" InitState  36 _sNElod0_ 0"
write (1,*)" InitState  37 _sDCera0_ 0"
write (1,*)" InitState  38 _sPCera0_ 0"
write (1,*)" InitState  39 _sNCera0_ 0"
write (1,*)" InitState  40 _sDLemn0_ 0"
write (1,*)" InitState  41 _sPLemn0_ 0"
write (1,*)" InitState  42 _sNLemn0_ 0"
write (1,*)" InitState  43 _sDNymp0_ 0"
write (1,*)" InitState  44 _sPNymp0_ 0"
write (1,*)" InitState  45 _sNNymp0_ 0"
write (1,*)" InitState  46 _sDHelo0_ 0"
write (1,*)" InitState  47 _sPHelo0_ 0"
write (1,*)" InitState  48 _sNHelo0_ 0"
write (1,*)" InitState  49 _uDPhytW0_ 0"
write (1,*)" InitState  50 _uPPhytW0_ 0"
write (1,*)" InitState  51 _uNPhytW0_ 0"
write (1,*)" InitState  52 _uDPhytS0_ 0"
write (1,*)" InitState  53 _uPPhytS0_ 0"
write (1,*)" InitState  54 _uNPhytS0_ 0"
write (1,*)" InitState  55 _uDVeg0_ 0"
write (1,*)" InitState  56 _uPVeg0_ 0"
write (1,*)" InitState  57 _uNVeg0_ 0"
write (1,*)" InitState  58 _uDTotT0_ 0"
write (1,*)" InitState  59 _uPTotT0_ 0"
write (1,*)" InitState  60 _uNTotT0_ 0"
write (1,*)" InitState  61 _sDDetS_ 0"
write (1,*)" InitState  62 _sDDetW_ 0"
write (1,*)" InitState  63 _sDepthW_ 0"
write (1,*)" InitState  64 _sDHumS_ 0"
write (1,*)" InitState  65 _sDIMS_ 0"
write (1,*)" InitState  66 _sDIMW_ 0"
write (1,*)" InitState  67 _sNDetS_ 0"
write (1,*)" InitState  68 _sNDetW_ 0"
write (1,*)" InitState  69 _sNH4S_ 0"
write (1,*)" InitState  70 _sNH4W_ 0"
write (1,*)" InitState  71 _sNHumS_ 0"
write (1,*)" InitState  72 _sNO3S_ 0"
write (1,*)" InitState  73 _sNO3W_ 0"
write (1,*)" InitState  74 _sO2W_ 0"
write (1,*)" InitState  75 _sPAIMS_ 0"
write (1,*)" InitState  76 _sPAIMW_ 0"
write (1,*)" InitState  77 _sPDetS_ 0"
write (1,*)" InitState  78 _sPDetW_ 0"
write (1,*)" InitState  79 _sPHumS_ 0"
write (1,*)" InitState  80 _sPO4S_ 0"
write (1,*)" InitState  81 _sPO4W_ 0"
write (1,*)" InitState  82 _sDPhytW_ 0"
write (1,*)" InitState  83 _sPPhytW_ 0"
write (1,*)" InitState  84 _sNPhytW_ 0"
write (1,*)" InitState  85 _sDPhytS_ 0"
write (1,*)" InitState  86 _sPPhytS_ 0"
write (1,*)" InitState  87 _sNPhytS_ 0"
write (1,*)" InitState  88 _sDElod_ 0"
write (1,*)" InitState  89 _sDChar_ 0"
write (1,*)" InitState  90 _sDCera_ 0"
write (1,*)" InitState  91 _sDLemn_ 0"
write (1,*)" InitState  92 _sDNymp_ 0"
write (1,*)" InitState  93 _sDHelo_ 0"
write (1,*)" InitState  94 _sPElod_ 0"
write (1,*)" InitState  95 _sPChar_ 0"
write (1,*)" InitState  96 _sPCera_ 0"
write (1,*)" InitState  97 _sPLemn_ 0"
write (1,*)" InitState  98 _sPNymp_ 0"
write (1,*)" InitState  99 _sPHelo_ 0"
write (1,*)" InitState  100 _sNElod_ 0"
write (1,*)" InitState  101 _sNChar_ 0"
write (1,*)" InitState  102 _sNCera_ 0"
write (1,*)" InitState  103 _sNLemn_ 0"
write (1,*)" InitState  104 _sNNymp_ 0"
write (1,*)" InitState  105 _sNHelo_ 0"
write (1,*)" InitState  106 _sDExtTotT0_ 0"
write (1,*)" InitState  107 _sNExtTotT0_ 0"
write (1,*)" InitState  108 _sPExtTotT0_ 0"
write (1,*)" state  0 _sDDetS_", sDDetS
write (1,*)" state  1 _sDDetW_", sDDetW
write (1,*)" state  2 _sDepthW_", sDepthW
write (1,*)" state  3 _sDHumS_", sDHumS
write (1,*)" state  4 _sDIMS_", sDIMS
write (1,*)" state  5 _sDIMW_", sDIMW
write (1,*)" state  6 _sNDetS_", sNDetS
write (1,*)" state  7 _sNDetW_", sNDetW
write (1,*)" state  8 _sNH4S_", sNH4S
write (1,*)" state  9 _sNH4W_", sNH4W
write (1,*)" state  10 _sNHumS_", sNHumS
write (1,*)" state  11 _sNO3S_", sNO3S
write (1,*)" state  12 _sNO3W_", sNO3W
write (1,*)" state  13 _sO2W_", sO2W
write (1,*)" state  14 _sPAIMS_", sPAIMS
write (1,*)" state  15 _sPAIMW_", sPAIMW
write (1,*)" state  16 _sPDetS_", sPDetS
write (1,*)" state  17 _sPDetW_", sPDetW
write (1,*)" state  18 _sPHumS_", sPHumS
write (1,*)" state  19 _sPO4S_", sPO4S
write (1,*)" state  20 _sPO4W_", sPO4W
write (1,*)" state  21 _sDPhytW_", sDPhytW
write (1,*)" state  22 _sPPhytW_", sPPhytW
write (1,*)" state  23 _sNPhytW_", sNPhytW
write (1,*)" state  24 _sDPhytS_", sDPhytS
write (1,*)" state  25 _sPPhytS_", sPPhytS
write (1,*)" state  26 _sNPhytS_", sNPhytS
write (1,*)" state  27 _sDElod_", sDElod
write (1,*)" state  28 _sDChar_", sDChar
write (1,*)" state  29 _sDCera_", sDCera
write (1,*)" state  30 _sDLemn_", sDLemn
write (1,*)" state  31 _sDNymp_", sDNymp
write (1,*)" state  32 _sDHelo_", sDHelo
write (1,*)" state  33 _sPElod_", sPElod
write (1,*)" state  34 _sPChar_", sPChar
write (1,*)" state  35 _sPCera_", sPCera
write (1,*)" state  36 _sPLemn_", sPLemn
write (1,*)" state  37 _sPNymp_", sPNymp
write (1,*)" state  38 _sPHelo_", sPHelo
write (1,*)" state  39 _sNElod_", sNElod
write (1,*)" state  40 _sNChar_", sNChar
write (1,*)" state  41 _sNCera_", sNCera
write (1,*)" state  42 _sNLemn_", sNLemn
write (1,*)" state  43 _sNNymp_", sNNymp
write (1,*)" state  44 _sNHelo_", sNHelo
write (1,*)" state  45 _sDExtTotT_", sDExtTotT
write (1,*)" state  46 _sNExtTotT_", sNExtTotT
write (1,*)" state  47 _sPExtTotT_", sPExtTotT
write (1,*)" Auxiliary  0 _bPorS_", bPorS
write (1,*)" Auxiliary  1 _bPorCorS_", bPorCorS
write (1,*)" Auxiliary  2 _sTime_", sTime
write (1,*)" Auxiliary  3 _TimeYears_", TimeYears
write (1,*)" Auxiliary  4 _Day_", Day
write (1,*)" Auxiliary  5 _uTm_", uTm
write (1,*)" Auxiliary  6 _uVWind_", uVWind
write (1,*)" Auxiliary  7 _ufDay_", ufDay
write (1,*)" Auxiliary  8 _uLDay_", uLDay
write (1,*)" Auxiliary  9 _uLOut_", uLOut
write (1,*)" Auxiliary  10 _uLPAR0_", uLPAR0
write (1,*)" Auxiliary  11 _aExtPhyt_", aExtPhyt
write (1,*)" Auxiliary  12 _aExtDet_", aExtDet
write (1,*)" Auxiliary  13 _aExtIM_", aExtIM
write (1,*)" Auxiliary  14 _aExtCoefOpen_", aExtCoefOpen
write (1,*)" Auxiliary  15 _uQInSeason_", uQInSeason
write (1,*)" Auxiliary  16 _uQEvSinus_", uQEvSinus
write (1,*)" Auxiliary  17 _uQEv_", uQEv
write (1,*)" Auxiliary  18 _uQInExtra_", uQInExtra
write (1,*)" Auxiliary  19 _uQOutExtra_", uQOutExtra
write (1,*)" Auxiliary  20 _uQIn_", uQIn
write (1,*)" Auxiliary  21 _uQOut_", uQOut
write (1,*)" Auxiliary  22 _uQDil_", uQDil
write (1,*)" Auxiliary  23 _ukDil_", ukDil
write (1,*)" Auxiliary  24 _ukDilWat_", ukDilWat
write (1,*)" Auxiliary  25 _ukOut_", ukOut
write (1,*)" Auxiliary  26 _uTauWat_", uTauWat
write (1,*)" Auxiliary  27 _uTauSubst_", uTauSubst
write (1,*)" Auxiliary  28 _vTranDepthW_", vTranDepthW
write (1,*)" Auxiliary  29 _oDPhytW_", oDPhytW
write (1,*)" Auxiliary  30 _oPPhytW_", oPPhytW
write (1,*)" Auxiliary  31 _oNPhytW_", oNPhytW
write (1,*)" Auxiliary  32 _aDPhytS_", aDPhytS
write (1,*)" Auxiliary  33 _aPPhytS_", aPPhytS
write (1,*)" Auxiliary  34 _aNPhytS_", aNPhytS
write (1,*)" Auxiliary  35 _oDOMW_", oDOMW
write (1,*)" Auxiliary  36 _oDSestW_", oDSestW
write (1,*)" Auxiliary  37 _oPOMW_", oPOMW
write (1,*)" Auxiliary  38 _oPSestW_", oPSestW
write (1,*)" Auxiliary  39 _oPInorgW_", oPInorgW
write (1,*)" Auxiliary  40 _oPTotW_", oPTotW
write (1,*)" Auxiliary  41 _oNDissW_", oNDissW
write (1,*)" Auxiliary  42 _oNOMW_", oNOMW
write (1,*)" Auxiliary  43 _oNSestW_", oNSestW
write (1,*)" Auxiliary  44 _oNkjW_", oNkjW
write (1,*)" Auxiliary  45 _oNTotW_", oNTotW
write (1,*)" Auxiliary  46 _aDTotS_", aDTotS
write (1,*)" Auxiliary  47 _aRhoTotS_", aRhoTotS
write (1,*)" Auxiliary  48 _aRhoSolidS_", aRhoSolidS
write (1,*)" Auxiliary  49 _afDTotS_", afDTotS
write (1,*)" Auxiliary  50 _afDOrgS_", afDOrgS
write (1,*)" Auxiliary  51 _afDetS_", afDetS
write (1,*)" Auxiliary  52 _afDetTotS_", afDetTotS
write (1,*)" Auxiliary  53 _aPInorgS_", aPInorgS
write (1,*)" Auxiliary  54 _aPTotAvailS_", aPTotAvailS
write (1,*)" Auxiliary  55 _aPTotS_", aPTotS
write (1,*)" Auxiliary  56 _afPInorgS_", afPInorgS
write (1,*)" Auxiliary  57 _afPTotS_", afPTotS
write (1,*)" Auxiliary  58 _afPO4S_", afPO4S
write (1,*)" Auxiliary  59 _oPO4S_", oPO4S
write (1,*)" Auxiliary  60 _aNDissS_", aNDissS
write (1,*)" Auxiliary  61 _aNkjAvailS_", aNkjAvailS
write (1,*)" Auxiliary  62 _aNkjS_", aNkjS
write (1,*)" Auxiliary  63 _aNTotAvailS_", aNTotAvailS
write (1,*)" Auxiliary  64 _aNTotS_", aNTotS
write (1,*)" Auxiliary  65 _afNInorgS_", afNInorgS
write (1,*)" Auxiliary  66 _afNTotS_", afNTotS
write (1,*)" Auxiliary  67 _oNO3S_", oNO3S
write (1,*)" Auxiliary  68 _oNH4S_", oNH4S
write (1,*)" Auxiliary  69 _oNDissS_", oNDissS
write (1,*)" Auxiliary  70 _rPDIMW_", rPDIMW
write (1,*)" Auxiliary  71 _rPDIMS_", rPDIMS
write (1,*)" Auxiliary  72 _rPDDetW_", rPDDetW
write (1,*)" Auxiliary  73 _rNDDetW_", rNDDetW
write (1,*)" Auxiliary  74 _rPDHumS_", rPDHumS
write (1,*)" Auxiliary  75 _rNDHumS_", rNDHumS
write (1,*)" Auxiliary  76 _rPDDetS_", rPDDetS
write (1,*)" Auxiliary  77 _rNDDetS_", rNDDetS
write (1,*)" Auxiliary  78 _uPLoadSeason_", uPLoadSeason
write (1,*)" Auxiliary  79 _uPLoad_", uPLoad
write (1,*)" Auxiliary  80 _uPLoadPO4_", uPLoadPO4
write (1,*)" Auxiliary  81 _uPLoadOrg_", uPLoadOrg
write (1,*)" Auxiliary  82 _uPLoadPhytTot_", uPLoadPhytTot
write (1,*)" Auxiliary  83 _uPLoadDet_", uPLoadDet
write (1,*)" Auxiliary  84 _uPLoadAIM_", uPLoadAIM
write (1,*)" Auxiliary  85 _uNLoadSeason_", uNLoadSeason
write (1,*)" Auxiliary  86 _uNLoadPhytTot_", uNLoadPhytTot
write (1,*)" Auxiliary  87 _uNLoad_", uNLoad
write (1,*)" Auxiliary  88 _uNLoadDet_", uNLoadDet
write (1,*)" Auxiliary  89 _uNLoadOrg_", uNLoadOrg
write (1,*)" Auxiliary  90 _uNLoadDiss_", uNLoadDiss
write (1,*)" Auxiliary  91 _uNLoadNH4_", uNLoadNH4
write (1,*)" Auxiliary  92 _uNLoadNO3_", uNLoadNO3
write (1,*)" Auxiliary  93 _uNTotIn_", uNTotIn
write (1,*)" Auxiliary  94 _uDLoadDet_", uDLoadDet
write (1,*)" Auxiliary  95 _uDLoadPhytTot_", uDLoadPhytTot
write (1,*)" Auxiliary  96 _uDLoadIM_", uDLoadIM
write (1,*)" Auxiliary  97 _uDLoad_", uDLoad
write (1,*)" Auxiliary  98 _uPTotIn_", uPTotIn
write (1,*)" Auxiliary  99 _wDDilIM_", wDDilIM
write (1,*)" Auxiliary  100 _wDDilDet_", wDDilDet
write (1,*)" Auxiliary  101 _wPDilPO4_", wPDilPO4
write (1,*)" Auxiliary  102 _wPDilDet_", wPDilDet
write (1,*)" Auxiliary  103 _wPDilAIM_", wPDilAIM
write (1,*)" Auxiliary  104 _wNDilNH4_", wNDilNH4
write (1,*)" Auxiliary  105 _wNDilNO3_", wNDilNO3
write (1,*)" Auxiliary  106 _wNDilDet_", wNDilDet
write (1,*)" Auxiliary  107 _wO2Inflow_", wO2Inflow
write (1,*)" Auxiliary  108 _wO2Outfl_", wO2Outfl
write (1,*)" Auxiliary  109 _wDDilPhyt_", wDDilPhyt
write (1,*)" Auxiliary  110 _wPDilPhyt_", wPDilPhyt
write (1,*)" Auxiliary  111 _wNDilPhyt_", wNDilPhyt
write (1,*)" Auxiliary  112 _wDOutflTot_", wDOutflTot
write (1,*)" Auxiliary  113 _wPOutflTot_", wPOutflTot
write (1,*)" Auxiliary  114 _wNOutflTot_", wNOutflTot
write (1,*)" Auxiliary  115 _uDLoadPhyt_", uDLoadPhyt
write (1,*)" Auxiliary  116 _uPLoadPhyt_", uPLoadPhyt
write (1,*)" Auxiliary  117 _uNLoadPhyt_", uNLoadPhyt
write (1,*)" Auxiliary  118 _wDTranPhyt_", wDTranPhyt
write (1,*)" Auxiliary  119 _wPTranPhyt_", wPTranPhyt
write (1,*)" Auxiliary  120 _wNTranPhyt_", wNTranPhyt
write (1,*)" Auxiliary  121 _wDTranIMW_", wDTranIMW
write (1,*)" Auxiliary  122 _wDTranDetW_", wDTranDetW
write (1,*)" Auxiliary  123 _wO2TranW_", wO2TranW
write (1,*)" Auxiliary  124 _wPTranPO4W_", wPTranPO4W
write (1,*)" Auxiliary  125 _wPTranAIMW_", wPTranAIMW
write (1,*)" Auxiliary  126 _wPTranDetW_", wPTranDetW
write (1,*)" Auxiliary  127 _wNTranNH4W_", wNTranNH4W
write (1,*)" Auxiliary  128 _wNTranNO3W_", wNTranNO3W
write (1,*)" Auxiliary  129 _wNTranDetW_", wNTranDetW
write (1,*)" Auxiliary  130 _wDDilTot_", wDDilTot
write (1,*)" Auxiliary  131 _wPDilTot_", wPDilTot
write (1,*)" Auxiliary  132 _wNDilTot_", wNDilTot
write (1,*)" Auxiliary  133 _tPInfPO4W_", tPInfPO4W
write (1,*)" Auxiliary  134 _tNInfNH4W_", tNInfNH4W
write (1,*)" Auxiliary  135 _tNInfNO3W_", tNInfNO3W
write (1,*)" Auxiliary  136 _tPInfPO4S_", tPInfPO4S
write (1,*)" Auxiliary  137 _tNInfNH4S_", tNInfNH4S
write (1,*)" Auxiliary  138 _tNInfNO3S_", tNInfNO3S
write (1,*)" Auxiliary  139 _tNH4LoadS_", tNH4LoadS
write (1,*)" Auxiliary  140 _tNO3LoadS_", tNO3LoadS
write (1,*)" Auxiliary  141 _uDErosIM_", uDErosIM
write (1,*)" Auxiliary  142 _uDErosIMS_", uDErosIMS
write (1,*)" Auxiliary  143 _uDErosIMW_", uDErosIMW
write (1,*)" Auxiliary  144 _uDErosOM_", uDErosOM
write (1,*)" Auxiliary  145 _uPErosOM_", uPErosOM
write (1,*)" Auxiliary  146 _uNErosOM_", uNErosOM
write (1,*)" Auxiliary  147 _uO2Sat_", uO2Sat
write (1,*)" Auxiliary  148 _kAer_", kAer
write (1,*)" Auxiliary  149 _uFunTmAer_", uFunTmAer
write (1,*)" Auxiliary  150 _aFunLemnAer_", aFunLemnAer
write (1,*)" Auxiliary  151 _tO2Aer_", tO2Aer
write (1,*)" Auxiliary  152 _tDTurbFish_", tDTurbFish
write (1,*)" Auxiliary  153 _tDTurbFishIM_", tDTurbFishIM
write (1,*)" Auxiliary  154 _aDVeg_", aDVeg
write (1,*)" Auxiliary  155 _aFunVegResus_", aFunVegResus
write (1,*)" Auxiliary  156 _aFunDimSusp_", aFunDimSusp
write (1,*)" Auxiliary  157 _tDResusTauDead_", tDResusTauDead
write (1,*)" Auxiliary  158 _tDResusBareDead_", tDResusBareDead
write (1,*)" Auxiliary  159 _tDResusDead_", tDResusDead
write (1,*)" Auxiliary  160 _tDResusIM_", tDResusIM
write (1,*)" Auxiliary  161 _tDResusDet_", tDResusDet
write (1,*)" Auxiliary  162 _akResusPhytRef_", akResusPhytRef
write (1,*)" Auxiliary  163 _tDResusPhytTot_", tDResusPhytTot
write (1,*)" Auxiliary  164 _tPResusDet_", tPResusDet
write (1,*)" Auxiliary  165 _tPResusPO4_", tPResusPO4
write (1,*)" Auxiliary  166 _tPResusAIM_", tPResusAIM
write (1,*)" Auxiliary  167 _tNResusNO3_", tNResusNO3
write (1,*)" Auxiliary  168 _tNResusNH4_", tNResusNH4
write (1,*)" Auxiliary  169 _tNResusDet_", tNResusDet
write (1,*)" Auxiliary  170 _aFunTauSetOM_", aFunTauSetOM
write (1,*)" Auxiliary  171 _aFunTauSetIM_", aFunTauSetIM
write (1,*)" Auxiliary  172 _uFunTmSet_", uFunTmSet
write (1,*)" Auxiliary  173 _uCorVSetIM_", uCorVSetIM
write (1,*)" Auxiliary  174 _tDSetIM_", tDSetIM
write (1,*)" Auxiliary  175 _tPSetAIM_", tPSetAIM
write (1,*)" Auxiliary  176 _uCorVSetDet_", uCorVSetDet
write (1,*)" Auxiliary  177 _tDSetDet_", tDSetDet
write (1,*)" Auxiliary  178 _tPSetDet_", tPSetDet
write (1,*)" Auxiliary  179 _tNSetDet_", tNSetDet
write (1,*)" Auxiliary  180 _kPMinDetW_", kPMinDetW
write (1,*)" Auxiliary  181 _kNMinDetW_", kNMinDetW
write (1,*)" Auxiliary  182 _uFunTmMinW_", uFunTmMinW
write (1,*)" Auxiliary  183 _wDMinDetW_", wDMinDetW
write (1,*)" Auxiliary  184 _wPMinDetW_", wPMinDetW
write (1,*)" Auxiliary  185 _wNMinDetW_", wNMinDetW
write (1,*)" Auxiliary  186 _aCorO2BOD_", aCorO2BOD
write (1,*)" Auxiliary  187 _wO2MinDetW_", wO2MinDetW
write (1,*)" Auxiliary  188 _wDDenitW_", wDDenitW
write (1,*)" Auxiliary  189 _wNDenitW_", wNDenitW
write (1,*)" Auxiliary  190 _uFunTmNitr_", uFunTmNitr
write (1,*)" Auxiliary  191 _aCorO2NitrW_", aCorO2NitrW
write (1,*)" Auxiliary  192 _wNNitrW_", wNNitrW
write (1,*)" Auxiliary  193 _wO2NitrW_", wO2NitrW
write (1,*)" Auxiliary  194 _kPMinDetS_", kPMinDetS
write (1,*)" Auxiliary  195 _kNMinDetS_", kNMinDetS
write (1,*)" Auxiliary  196 _uFunTmMinS_", uFunTmMinS
write (1,*)" Auxiliary  197 _tDMinDetS_", tDMinDetS
write (1,*)" Auxiliary  198 _tPMinDetS_", tPMinDetS
write (1,*)" Auxiliary  199 _tNMinDetS_", tNMinDetS
write (1,*)" Auxiliary  200 _uFunTmDif_", uFunTmDif
write (1,*)" Auxiliary  201 _akO2DifCor_", akO2DifCor
write (1,*)" Auxiliary  202 _tSOD_", tSOD
write (1,*)" Auxiliary  203 _aDepthOxySed_", aDepthOxySed
write (1,*)" Auxiliary  204 _afOxySed_", afOxySed
write (1,*)" Auxiliary  205 _tDMinOxyDetS_", tDMinOxyDetS
write (1,*)" Auxiliary  206 _tO2MinDetS_", tO2MinDetS
write (1,*)" Auxiliary  207 _tDDenitS_", tDDenitS
write (1,*)" Auxiliary  208 _tNDenitS_", tNDenitS
write (1,*)" Auxiliary  209 _tNNitrS_", tNNitrS
write (1,*)" Auxiliary  210 _tO2NitrS_", tO2NitrS
write (1,*)" Auxiliary  211 _tDMinHumS_", tDMinHumS
write (1,*)" Auxiliary  212 _tPMinHumS_", tPMinHumS
write (1,*)" Auxiliary  213 _tNMinHumS_", tNMinHumS
write (1,*)" Auxiliary  214 _aDepthDif_", aDepthDif
write (1,*)" Auxiliary  215 _tPDifPO4_", tPDifPO4
write (1,*)" Auxiliary  216 _tNDifNO3_", tNDifNO3
write (1,*)" Auxiliary  217 _tNDifNH4_", tNDifNH4
write (1,*)" Auxiliary  218 _tO2Dif_", tO2Dif
write (1,*)" Auxiliary  219 _tPDifGroundPO4_", tPDifGroundPO4
write (1,*)" Auxiliary  220 _tNDifGroundNO3_", tNDifGroundNO3
write (1,*)" Auxiliary  221 _tNDifGroundNH4_", tNDifGroundNH4
write (1,*)" Auxiliary  222 _aPAdsMaxW_", aPAdsMaxW
write (1,*)" Auxiliary  223 _aKPAdsW_", aKPAdsW
write (1,*)" Auxiliary  224 _aPIsoAdsW_", aPIsoAdsW
write (1,*)" Auxiliary  225 _aPEqIMW_", aPEqIMW
write (1,*)" Auxiliary  226 _wPSorpIMW_", wPSorpIMW
write (1,*)" Auxiliary  227 _aPAdsMaxS_", aPAdsMaxS
write (1,*)" Auxiliary  228 _aKPAdsS_", aKPAdsS
write (1,*)" Auxiliary  229 _aPIsoAdsS_", aPIsoAdsS
write (1,*)" Auxiliary  230 _aPEqIMS_", aPEqIMS
write (1,*)" Auxiliary  231 _tPSorpIMS_", tPSorpIMS
write (1,*)" Auxiliary  232 _tPChemPO4_", tPChemPO4
write (1,*)" Auxiliary  233 _wDAbioIMW_", wDAbioIMW
write (1,*)" Auxiliary  234 _wDAbioDetW_", wDAbioDetW
write (1,*)" Auxiliary  235 _tDAbioIMS_", tDAbioIMS
write (1,*)" Auxiliary  236 _tDAbioDetS_", tDAbioDetS
write (1,*)" Auxiliary  237 _tDAbioHumS_", tDAbioHumS
write (1,*)" Auxiliary  238 _tDAbioTotT_", tDAbioTotT
write (1,*)" Auxiliary  239 _wO2AbioW_", wO2AbioW
write (1,*)" Auxiliary  240 _wPAbioDetW_", wPAbioDetW
write (1,*)" Auxiliary  241 _wPAbioPO4W_", wPAbioPO4W
write (1,*)" Auxiliary  242 _wPAbioAIMW_", wPAbioAIMW
write (1,*)" Auxiliary  243 _tPAbioDetS_", tPAbioDetS
write (1,*)" Auxiliary  244 _tPAbioHumS_", tPAbioHumS
write (1,*)" Auxiliary  245 _tPAbioPO4S_", tPAbioPO4S
write (1,*)" Auxiliary  246 _tPAbioAIMS_", tPAbioAIMS
write (1,*)" Auxiliary  247 _tPAbioTotT_", tPAbioTotT
write (1,*)" Auxiliary  248 _wNAbioNH4W_", wNAbioNH4W
write (1,*)" Auxiliary  249 _wNAbioNO3W_", wNAbioNO3W
write (1,*)" Auxiliary  250 _wNAbioDetW_", wNAbioDetW
write (1,*)" Auxiliary  251 _tNAbioNH4S_", tNAbioNH4S
write (1,*)" Auxiliary  252 _tNAbioNO3S_", tNAbioNO3S
write (1,*)" Auxiliary  253 _tNAbioDetS_", tNAbioDetS
write (1,*)" Auxiliary  254 _tNAbioHumS_", tNAbioHumS
write (1,*)" Auxiliary  255 _tNAbioTotT_", tNAbioTotT
write (1,*)" Auxiliary  256 _fManElod_", fManElod
write (1,*)" Auxiliary  257 _fManCera_", fManCera
write (1,*)" Auxiliary  258 _fManChar_", fManChar
write (1,*)" Auxiliary  259 _fManNymp_", fManNymp
write (1,*)" Auxiliary  260 _kMigrLemn_", kMigrLemn
write (1,*)" Auxiliary  261 _aDayInitElod_", aDayInitElod
write (1,*)" Auxiliary  262 _bfRootElod_", bfRootElod
write (1,*)" Auxiliary  263 _bfShootElod_", bfShootElod
write (1,*)" Auxiliary  264 _aDRootElod_", aDRootElod
write (1,*)" Auxiliary  265 _aDShootElod_", aDShootElod
write (1,*)" Auxiliary  266 _aDEmergElod_", aDEmergElod
write (1,*)" Auxiliary  267 _aDFloatElod_", aDFloatElod
write (1,*)" Auxiliary  268 _bfSubElod_", bfSubElod
write (1,*)" Auxiliary  269 _aDSubElod_", aDSubElod
write (1,*)" Auxiliary  270 _aExtElod_", aExtElod
write (1,*)" Auxiliary  271 _aDepth1Elod_", aDepth1Elod
write (1,*)" Auxiliary  272 _aDepth2Elod_", aDepth2Elod
write (1,*)" Auxiliary  273 _afCovSurfElod_", afCovSurfElod
write (1,*)" Auxiliary  274 _afCovEmergElod_", afCovEmergElod
write (1,*)" Auxiliary  275 _aCovElod_", aCovElod
write (1,*)" Auxiliary  276 _aDayInitChar_", aDayInitChar
write (1,*)" Auxiliary  277 _bfRootChar_", bfRootChar
write (1,*)" Auxiliary  278 _bfShootChar_", bfShootChar
write (1,*)" Auxiliary  279 _aDRootChar_", aDRootChar
write (1,*)" Auxiliary  280 _aDShootChar_", aDShootChar
write (1,*)" Auxiliary  281 _aDEmergChar_", aDEmergChar
write (1,*)" Auxiliary  282 _aDFloatChar_", aDFloatChar
write (1,*)" Auxiliary  283 _bfSubChar_", bfSubChar
write (1,*)" Auxiliary  284 _aDSubChar_", aDSubChar
write (1,*)" Auxiliary  285 _aExtChar_", aExtChar
write (1,*)" Auxiliary  286 _aDepth1Char_", aDepth1Char
write (1,*)" Auxiliary  287 _aDepth2Char_", aDepth2Char
write (1,*)" Auxiliary  288 _afCovSurfChar_", afCovSurfChar
write (1,*)" Auxiliary  289 _afCovEmergChar_", afCovEmergChar
write (1,*)" Auxiliary  290 _aCovChar_", aCovChar
write (1,*)" Auxiliary  291 _aDayInitCera_", aDayInitCera
write (1,*)" Auxiliary  292 _bfRootCera_", bfRootCera
write (1,*)" Auxiliary  293 _bfShootCera_", bfShootCera
write (1,*)" Auxiliary  294 _aDRootCera_", aDRootCera
write (1,*)" Auxiliary  295 _aDShootCera_", aDShootCera
write (1,*)" Auxiliary  296 _aDEmergCera_", aDEmergCera
write (1,*)" Auxiliary  297 _aDFloatCera_", aDFloatCera
write (1,*)" Auxiliary  298 _bfSubCera_", bfSubCera
write (1,*)" Auxiliary  299 _aDSubCera_", aDSubCera
write (1,*)" Auxiliary  300 _aExtCera_", aExtCera
write (1,*)" Auxiliary  301 _aDepth1Cera_", aDepth1Cera
write (1,*)" Auxiliary  302 _aDepth2Cera_", aDepth2Cera
write (1,*)" Auxiliary  303 _afCovSurfCera_", afCovSurfCera
write (1,*)" Auxiliary  304 _afCovEmergCera_", afCovEmergCera
write (1,*)" Auxiliary  305 _aCovCera_", aCovCera
write (1,*)" Auxiliary  306 _aDayInitLemn_", aDayInitLemn
write (1,*)" Auxiliary  307 _bfRootLemn_", bfRootLemn
write (1,*)" Auxiliary  308 _bfShootLemn_", bfShootLemn
write (1,*)" Auxiliary  309 _aDRootLemn_", aDRootLemn
write (1,*)" Auxiliary  310 _aDShootLemn_", aDShootLemn
write (1,*)" Auxiliary  311 _aDEmergLemn_", aDEmergLemn
write (1,*)" Auxiliary  312 _aDFloatLemn_", aDFloatLemn
write (1,*)" Auxiliary  313 _bfSubLemn_", bfSubLemn
write (1,*)" Auxiliary  314 _aDSubLemn_", aDSubLemn
write (1,*)" Auxiliary  315 _aExtLemn_", aExtLemn
write (1,*)" Auxiliary  316 _aDepth1Lemn_", aDepth1Lemn
write (1,*)" Auxiliary  317 _aDepth2Lemn_", aDepth2Lemn
write (1,*)" Auxiliary  318 _afCovSurfLemn_", afCovSurfLemn
write (1,*)" Auxiliary  319 _afCovEmergLemn_", afCovEmergLemn
write (1,*)" Auxiliary  320 _aCovLemn_", aCovLemn
write (1,*)" Auxiliary  321 _aDayInitNymp_", aDayInitNymp
write (1,*)" Auxiliary  322 _bfRootNymp_", bfRootNymp
write (1,*)" Auxiliary  323 _bfShootNymp_", bfShootNymp
write (1,*)" Auxiliary  324 _aDRootNymp_", aDRootNymp
write (1,*)" Auxiliary  325 _aDShootNymp_", aDShootNymp
write (1,*)" Auxiliary  326 _aDEmergNymp_", aDEmergNymp
write (1,*)" Auxiliary  327 _aDFloatNymp_", aDFloatNymp
write (1,*)" Auxiliary  328 _bfSubNymp_", bfSubNymp
write (1,*)" Auxiliary  329 _aDSubNymp_", aDSubNymp
write (1,*)" Auxiliary  330 _aExtNymp_", aExtNymp
write (1,*)" Auxiliary  331 _aDepth1Nymp_", aDepth1Nymp
write (1,*)" Auxiliary  332 _aDepth2Nymp_", aDepth2Nymp
write (1,*)" Auxiliary  333 _afCovSurfNymp_", afCovSurfNymp
write (1,*)" Auxiliary  334 _afCovEmergNymp_", afCovEmergNymp
write (1,*)" Auxiliary  335 _aCovNymp_", aCovNymp
write (1,*)" Auxiliary  336 _aDayInitHelo_", aDayInitHelo
write (1,*)" Auxiliary  337 _bfRootHelo_", bfRootHelo
write (1,*)" Auxiliary  338 _bfShootHelo_", bfShootHelo
write (1,*)" Auxiliary  339 _aDRootHelo_", aDRootHelo
write (1,*)" Auxiliary  340 _aDShootHelo_", aDShootHelo
write (1,*)" Auxiliary  341 _aDEmergHelo_", aDEmergHelo
write (1,*)" Auxiliary  342 _aDFloatHelo_", aDFloatHelo
write (1,*)" Auxiliary  343 _bfSubHelo_", bfSubHelo
write (1,*)" Auxiliary  344 _aDSubHelo_", aDSubHelo
write (1,*)" Auxiliary  345 _aExtHelo_", aExtHelo
write (1,*)" Auxiliary  346 _aDepth1Helo_", aDepth1Helo
write (1,*)" Auxiliary  347 _aDepth2Helo_", aDepth2Helo
write (1,*)" Auxiliary  348 _afCovSurfHelo_", afCovSurfHelo
write (1,*)" Auxiliary  349 _afCovEmergHelo_", afCovEmergHelo
write (1,*)" Auxiliary  350 _aCovHelo_", aCovHelo
write (1,*)" Auxiliary  351 _aPVeg_", aPVeg
write (1,*)" Auxiliary  352 _aNVeg_", aNVeg
write (1,*)" Auxiliary  353 _afCovSurfVeg_", afCovSurfVeg
write (1,*)" Auxiliary  354 _afCovEmergVeg_", afCovEmergVeg
write (1,*)" Auxiliary  355 _aExtVeg_", aExtVeg
write (1,*)" Auxiliary  356 _aExtCoef_", aExtCoef
write (1,*)" Auxiliary  357 _aLPARBot_", aLPARBot
write (1,*)" Auxiliary  358 _rPDElod_", rPDElod
write (1,*)" Auxiliary  359 _rNDElod_", rNDElod
write (1,*)" Auxiliary  360 _tDMigrElod_", tDMigrElod
write (1,*)" Auxiliary  361 _tPMigrElod_", tPMigrElod
write (1,*)" Auxiliary  362 _tNMigrElod_", tNMigrElod
write (1,*)" Auxiliary  363 _uFunTmProdElod_", uFunTmProdElod
write (1,*)" Auxiliary  364 _uFunTmRespElod_", uFunTmRespElod
write (1,*)" Auxiliary  365 _aVPUptMaxCrElod_", aVPUptMaxCrElod
write (1,*)" Auxiliary  366 _aVPUptElodW_", aVPUptElodW
write (1,*)" Auxiliary  367 _tPUptElodW_", tPUptElodW
write (1,*)" Auxiliary  368 _aVPUptElodS_", aVPUptElodS
write (1,*)" Auxiliary  369 _tPUptElodS_", tPUptElodS
write (1,*)" Auxiliary  370 _tPUptElod_", tPUptElod
write (1,*)" Auxiliary  371 _aVNUptMaxCrElod_", aVNUptMaxCrElod
write (1,*)" Auxiliary  372 _ahNUptElod_", ahNUptElod
write (1,*)" Auxiliary  373 _aVNUptElodW_", aVNUptElodW
write (1,*)" Auxiliary  374 _tNUptElodW_", tNUptElodW
write (1,*)" Auxiliary  375 _afNH4UptElodW_", afNH4UptElodW
write (1,*)" Auxiliary  376 _tNUptNH4ElodW_", tNUptNH4ElodW
write (1,*)" Auxiliary  377 _tNUptNO3ElodW_", tNUptNO3ElodW
write (1,*)" Auxiliary  378 _aVNUptElodS_", aVNUptElodS
write (1,*)" Auxiliary  379 _tNUptElodS_", tNUptElodS
write (1,*)" Auxiliary  380 _afNH4UptElodS_", afNH4UptElodS
write (1,*)" Auxiliary  381 _tNUptNH4ElodS_", tNUptNH4ElodS
write (1,*)" Auxiliary  382 _tNUptNO3ElodS_", tNUptNO3ElodS
write (1,*)" Auxiliary  383 _tNUptElod_", tNUptElod
write (1,*)" Auxiliary  384 _aLPAR1Elod_", aLPAR1Elod
write (1,*)" Auxiliary  385 _aLPAR2Elod_", aLPAR2Elod
write (1,*)" Auxiliary  386 _uhLElod_", uhLElod
write (1,*)" Auxiliary  387 _aLLimShootElod_", aLLimShootElod
write (1,*)" Auxiliary  388 _aMuTmLElod_", aMuTmLElod
write (1,*)" Auxiliary  389 _aPLimElod_", aPLimElod
write (1,*)" Auxiliary  390 _aNLimElod_", aNLimElod
write (1,*)" Auxiliary  391 _aNutLimElod_", aNutLimElod
write (1,*)" Auxiliary  392 _aMuElod_", aMuElod
write (1,*)" Auxiliary  393 _bkMortElod_", bkMortElod
write (1,*)" Auxiliary  394 _akDIncrElod_", akDIncrElod
write (1,*)" Auxiliary  395 _tDEnvElod_", tDEnvElod
write (1,*)" Auxiliary  396 _tDEnvProdElod_", tDEnvProdElod
write (1,*)" Auxiliary  397 _tDProdElod_", tDProdElod
write (1,*)" Auxiliary  398 _tDProdSubElod_", tDProdSubElod
write (1,*)" Auxiliary  399 _tDRespElod_", tDRespElod
write (1,*)" Auxiliary  400 _tDEnvMortElod_", tDEnvMortElod
write (1,*)" Auxiliary  401 _tDMortElod_", tDMortElod
write (1,*)" Auxiliary  402 _tDMortElodW_", tDMortElodW
write (1,*)" Auxiliary  403 _tDMortElodS_", tDMortElodS
write (1,*)" Auxiliary  404 _tDGrazElodBird_", tDGrazElodBird
write (1,*)" Auxiliary  405 _bkManElod_", bkManElod
write (1,*)" Auxiliary  406 _tDManElod_", tDManElod
write (1,*)" Auxiliary  407 _tPManElod_", tPManElod
write (1,*)" Auxiliary  408 _tNManElod_", tNManElod
write (1,*)" Auxiliary  409 _tDBedElod_", tDBedElod
write (1,*)" Auxiliary  410 _tO2ProdElod_", tO2ProdElod
write (1,*)" Auxiliary  411 _tO2RespElodW_", tO2RespElodW
write (1,*)" Auxiliary  412 _tO2RespElodS_", tO2RespElodS
write (1,*)" Auxiliary  413 _tO2ProdElodS_", tO2ProdElodS
write (1,*)" Auxiliary  414 _tO2ProdElodW_", tO2ProdElodW
write (1,*)" Auxiliary  415 _tO2UptNO3ElodW_", tO2UptNO3ElodW
write (1,*)" Auxiliary  416 _tO2UptNO3ElodS_", tO2UptNO3ElodS
write (1,*)" Auxiliary  417 _tPExcrElod_", tPExcrElod
write (1,*)" Auxiliary  418 _tPExcrElodS_", tPExcrElodS
write (1,*)" Auxiliary  419 _tPExcrElodW_", tPExcrElodW
write (1,*)" Auxiliary  420 _tPMortElod_", tPMortElod
write (1,*)" Auxiliary  421 _tPMortElodPO4_", tPMortElodPO4
write (1,*)" Auxiliary  422 _tPMortElodPO4S_", tPMortElodPO4S
write (1,*)" Auxiliary  423 _tPMortElodPO4W_", tPMortElodPO4W
write (1,*)" Auxiliary  424 _tPMortElodDet_", tPMortElodDet
write (1,*)" Auxiliary  425 _tPMortElodDetW_", tPMortElodDetW
write (1,*)" Auxiliary  426 _tPMortElodDetS_", tPMortElodDetS
write (1,*)" Auxiliary  427 _tPGrazElodBird_", tPGrazElodBird
write (1,*)" Auxiliary  428 _tPBedElod_", tPBedElod
write (1,*)" Auxiliary  429 _tNExcrElod_", tNExcrElod
write (1,*)" Auxiliary  430 _tNExcrElodS_", tNExcrElodS
write (1,*)" Auxiliary  431 _tNExcrElodW_", tNExcrElodW
write (1,*)" Auxiliary  432 _tNMortElod_", tNMortElod
write (1,*)" Auxiliary  433 _tNMortElodNH4_", tNMortElodNH4
write (1,*)" Auxiliary  434 _tNMortElodNH4S_", tNMortElodNH4S
write (1,*)" Auxiliary  435 _tNMortElodNH4W_", tNMortElodNH4W
write (1,*)" Auxiliary  436 _tNMortElodDet_", tNMortElodDet
write (1,*)" Auxiliary  437 _tNMortElodDetW_", tNMortElodDetW
write (1,*)" Auxiliary  438 _tNMortElodDetS_", tNMortElodDetS
write (1,*)" Auxiliary  439 _tNGrazElodBird_", tNGrazElodBird
write (1,*)" Auxiliary  440 _tNBedElod_", tNBedElod
write (1,*)" Auxiliary  441 _rPDChar_", rPDChar
write (1,*)" Auxiliary  442 _rNDChar_", rNDChar
write (1,*)" Auxiliary  443 _tDMigrChar_", tDMigrChar
write (1,*)" Auxiliary  444 _tPMigrChar_", tPMigrChar
write (1,*)" Auxiliary  445 _tNMigrChar_", tNMigrChar
write (1,*)" Auxiliary  446 _uFunTmProdChar_", uFunTmProdChar
write (1,*)" Auxiliary  447 _uFunTmRespChar_", uFunTmRespChar
write (1,*)" Auxiliary  448 _aVPUptMaxCrChar_", aVPUptMaxCrChar
write (1,*)" Auxiliary  449 _aVPUptCharW_", aVPUptCharW
write (1,*)" Auxiliary  450 _tPUptCharW_", tPUptCharW
write (1,*)" Auxiliary  451 _aVPUptCharS_", aVPUptCharS
write (1,*)" Auxiliary  452 _tPUptCharS_", tPUptCharS
write (1,*)" Auxiliary  453 _tPUptChar_", tPUptChar
write (1,*)" Auxiliary  454 _aVNUptMaxCrChar_", aVNUptMaxCrChar
write (1,*)" Auxiliary  455 _ahNUptChar_", ahNUptChar
write (1,*)" Auxiliary  456 _aVNUptCharW_", aVNUptCharW
write (1,*)" Auxiliary  457 _tNUptCharW_", tNUptCharW
write (1,*)" Auxiliary  458 _afNH4UptCharW_", afNH4UptCharW
write (1,*)" Auxiliary  459 _tNUptNH4CharW_", tNUptNH4CharW
write (1,*)" Auxiliary  460 _tNUptNO3CharW_", tNUptNO3CharW
write (1,*)" Auxiliary  461 _aVNUptCharS_", aVNUptCharS
write (1,*)" Auxiliary  462 _tNUptCharS_", tNUptCharS
write (1,*)" Auxiliary  463 _afNH4UptCharS_", afNH4UptCharS
write (1,*)" Auxiliary  464 _tNUptNH4CharS_", tNUptNH4CharS
write (1,*)" Auxiliary  465 _tNUptNO3CharS_", tNUptNO3CharS
write (1,*)" Auxiliary  466 _tNUptChar_", tNUptChar
write (1,*)" Auxiliary  467 _aLPAR1Char_", aLPAR1Char
write (1,*)" Auxiliary  468 _aLPAR2Char_", aLPAR2Char
write (1,*)" Auxiliary  469 _uhLChar_", uhLChar
write (1,*)" Auxiliary  470 _aLLimShootChar_", aLLimShootChar
write (1,*)" Auxiliary  471 _aMuTmLChar_", aMuTmLChar
write (1,*)" Auxiliary  472 _aPLimChar_", aPLimChar
write (1,*)" Auxiliary  473 _aNLimChar_", aNLimChar
write (1,*)" Auxiliary  474 _aNutLimChar_", aNutLimChar
write (1,*)" Auxiliary  475 _aMuChar_", aMuChar
write (1,*)" Auxiliary  476 _bkMortChar_", bkMortChar
write (1,*)" Auxiliary  477 _akDIncrChar_", akDIncrChar
write (1,*)" Auxiliary  478 _tDEnvChar_", tDEnvChar
write (1,*)" Auxiliary  479 _tDEnvProdChar_", tDEnvProdChar
write (1,*)" Auxiliary  480 _tDProdChar_", tDProdChar
write (1,*)" Auxiliary  481 _tDProdSubChar_", tDProdSubChar
write (1,*)" Auxiliary  482 _tDRespChar_", tDRespChar
write (1,*)" Auxiliary  483 _tDEnvMortChar_", tDEnvMortChar
write (1,*)" Auxiliary  484 _tDMortChar_", tDMortChar
write (1,*)" Auxiliary  485 _tDMortCharW_", tDMortCharW
write (1,*)" Auxiliary  486 _tDMortCharS_", tDMortCharS
write (1,*)" Auxiliary  487 _tDGrazCharBird_", tDGrazCharBird
write (1,*)" Auxiliary  488 _bkManChar_", bkManChar
write (1,*)" Auxiliary  489 _tDManChar_", tDManChar
write (1,*)" Auxiliary  490 _tPManChar_", tPManChar
write (1,*)" Auxiliary  491 _tNManChar_", tNManChar
write (1,*)" Auxiliary  492 _tDBedChar_", tDBedChar
write (1,*)" Auxiliary  493 _tO2ProdChar_", tO2ProdChar
write (1,*)" Auxiliary  494 _tO2RespCharW_", tO2RespCharW
write (1,*)" Auxiliary  495 _tO2RespCharS_", tO2RespCharS
write (1,*)" Auxiliary  496 _tO2ProdCharS_", tO2ProdCharS
write (1,*)" Auxiliary  497 _tO2ProdCharW_", tO2ProdCharW
write (1,*)" Auxiliary  498 _tO2UptNO3CharW_", tO2UptNO3CharW
write (1,*)" Auxiliary  499 _tO2UptNO3CharS_", tO2UptNO3CharS
write (1,*)" Auxiliary  500 _tPExcrChar_", tPExcrChar
write (1,*)" Auxiliary  501 _tPExcrCharS_", tPExcrCharS
write (1,*)" Auxiliary  502 _tPExcrCharW_", tPExcrCharW
write (1,*)" Auxiliary  503 _tPMortChar_", tPMortChar
write (1,*)" Auxiliary  504 _tPMortCharPO4_", tPMortCharPO4
write (1,*)" Auxiliary  505 _tPMortCharPO4S_", tPMortCharPO4S
write (1,*)" Auxiliary  506 _tPMortCharPO4W_", tPMortCharPO4W
write (1,*)" Auxiliary  507 _tPMortCharDet_", tPMortCharDet
write (1,*)" Auxiliary  508 _tPMortCharDetW_", tPMortCharDetW
write (1,*)" Auxiliary  509 _tPMortCharDetS_", tPMortCharDetS
write (1,*)" Auxiliary  510 _tPGrazCharBird_", tPGrazCharBird
write (1,*)" Auxiliary  511 _tPBedChar_", tPBedChar
write (1,*)" Auxiliary  512 _tNExcrChar_", tNExcrChar
write (1,*)" Auxiliary  513 _tNExcrCharS_", tNExcrCharS
write (1,*)" Auxiliary  514 _tNExcrCharW_", tNExcrCharW
write (1,*)" Auxiliary  515 _tNMortChar_", tNMortChar
write (1,*)" Auxiliary  516 _tNMortCharNH4_", tNMortCharNH4
write (1,*)" Auxiliary  517 _tNMortCharNH4S_", tNMortCharNH4S
write (1,*)" Auxiliary  518 _tNMortCharNH4W_", tNMortCharNH4W
write (1,*)" Auxiliary  519 _tNMortCharDet_", tNMortCharDet
write (1,*)" Auxiliary  520 _tNMortCharDetW_", tNMortCharDetW
write (1,*)" Auxiliary  521 _tNMortCharDetS_", tNMortCharDetS
write (1,*)" Auxiliary  522 _tNGrazCharBird_", tNGrazCharBird
write (1,*)" Auxiliary  523 _tNBedChar_", tNBedChar
write (1,*)" Auxiliary  524 _rPDCera_", rPDCera
write (1,*)" Auxiliary  525 _rNDCera_", rNDCera
write (1,*)" Auxiliary  526 _tDMigrCera_", tDMigrCera
write (1,*)" Auxiliary  527 _tPMigrCera_", tPMigrCera
write (1,*)" Auxiliary  528 _tNMigrCera_", tNMigrCera
write (1,*)" Auxiliary  529 _uFunTmProdCera_", uFunTmProdCera
write (1,*)" Auxiliary  530 _uFunTmRespCera_", uFunTmRespCera
write (1,*)" Auxiliary  531 _aVPUptMaxCrCera_", aVPUptMaxCrCera
write (1,*)" Auxiliary  532 _aVPUptCeraW_", aVPUptCeraW
write (1,*)" Auxiliary  533 _tPUptCeraW_", tPUptCeraW
write (1,*)" Auxiliary  534 _aVPUptCeraS_", aVPUptCeraS
write (1,*)" Auxiliary  535 _tPUptCeraS_", tPUptCeraS
write (1,*)" Auxiliary  536 _tPUptCera_", tPUptCera
write (1,*)" Auxiliary  537 _aVNUptMaxCrCera_", aVNUptMaxCrCera
write (1,*)" Auxiliary  538 _ahNUptCera_", ahNUptCera
write (1,*)" Auxiliary  539 _aVNUptCeraW_", aVNUptCeraW
write (1,*)" Auxiliary  540 _tNUptCeraW_", tNUptCeraW
write (1,*)" Auxiliary  541 _afNH4UptCeraW_", afNH4UptCeraW
write (1,*)" Auxiliary  542 _tNUptNH4CeraW_", tNUptNH4CeraW
write (1,*)" Auxiliary  543 _tNUptNO3CeraW_", tNUptNO3CeraW
write (1,*)" Auxiliary  544 _aVNUptCeraS_", aVNUptCeraS
write (1,*)" Auxiliary  545 _tNUptCeraS_", tNUptCeraS
write (1,*)" Auxiliary  546 _afNH4UptCeraS_", afNH4UptCeraS
write (1,*)" Auxiliary  547 _tNUptNH4CeraS_", tNUptNH4CeraS
write (1,*)" Auxiliary  548 _tNUptNO3CeraS_", tNUptNO3CeraS
write (1,*)" Auxiliary  549 _tNUptCera_", tNUptCera
write (1,*)" Auxiliary  550 _aLPAR1Cera_", aLPAR1Cera
write (1,*)" Auxiliary  551 _aLPAR2Cera_", aLPAR2Cera
write (1,*)" Auxiliary  552 _uhLCera_", uhLCera
write (1,*)" Auxiliary  553 _aLLimShootCera_", aLLimShootCera
write (1,*)" Auxiliary  554 _aMuTmLCera_", aMuTmLCera
write (1,*)" Auxiliary  555 _aPLimCera_", aPLimCera
write (1,*)" Auxiliary  556 _aNLimCera_", aNLimCera
write (1,*)" Auxiliary  557 _aNutLimCera_", aNutLimCera
write (1,*)" Auxiliary  558 _aMuCera_", aMuCera
write (1,*)" Auxiliary  559 _bkMortCera_", bkMortCera
write (1,*)" Auxiliary  560 _akDIncrCera_", akDIncrCera
write (1,*)" Auxiliary  561 _tDEnvCera_", tDEnvCera
write (1,*)" Auxiliary  562 _tDEnvProdCera_", tDEnvProdCera
write (1,*)" Auxiliary  563 _tDProdCera_", tDProdCera
write (1,*)" Auxiliary  564 _tDProdSubCera_", tDProdSubCera
write (1,*)" Auxiliary  565 _tDRespCera_", tDRespCera
write (1,*)" Auxiliary  566 _tDEnvMortCera_", tDEnvMortCera
write (1,*)" Auxiliary  567 _tDMortCera_", tDMortCera
write (1,*)" Auxiliary  568 _tDMortCeraW_", tDMortCeraW
write (1,*)" Auxiliary  569 _tDMortCeraS_", tDMortCeraS
write (1,*)" Auxiliary  570 _tDGrazCeraBird_", tDGrazCeraBird
write (1,*)" Auxiliary  571 _bkManCera_", bkManCera
write (1,*)" Auxiliary  572 _tDManCera_", tDManCera
write (1,*)" Auxiliary  573 _tPManCera_", tPManCera
write (1,*)" Auxiliary  574 _tNManCera_", tNManCera
write (1,*)" Auxiliary  575 _tDBedCera_", tDBedCera
write (1,*)" Auxiliary  576 _tO2ProdCera_", tO2ProdCera
write (1,*)" Auxiliary  577 _tO2RespCeraW_", tO2RespCeraW
write (1,*)" Auxiliary  578 _tO2RespCeraS_", tO2RespCeraS
write (1,*)" Auxiliary  579 _tO2ProdCeraS_", tO2ProdCeraS
write (1,*)" Auxiliary  580 _tO2ProdCeraW_", tO2ProdCeraW
write (1,*)" Auxiliary  581 _tO2UptNO3CeraW_", tO2UptNO3CeraW
write (1,*)" Auxiliary  582 _tO2UptNO3CeraS_", tO2UptNO3CeraS
write (1,*)" Auxiliary  583 _tPExcrCera_", tPExcrCera
write (1,*)" Auxiliary  584 _tPExcrCeraS_", tPExcrCeraS
write (1,*)" Auxiliary  585 _tPExcrCeraW_", tPExcrCeraW
write (1,*)" Auxiliary  586 _tPMortCera_", tPMortCera
write (1,*)" Auxiliary  587 _tPMortCeraPO4_", tPMortCeraPO4
write (1,*)" Auxiliary  588 _tPMortCeraPO4S_", tPMortCeraPO4S
write (1,*)" Auxiliary  589 _tPMortCeraPO4W_", tPMortCeraPO4W
write (1,*)" Auxiliary  590 _tPMortCeraDet_", tPMortCeraDet
write (1,*)" Auxiliary  591 _tPMortCeraDetW_", tPMortCeraDetW
write (1,*)" Auxiliary  592 _tPMortCeraDetS_", tPMortCeraDetS
write (1,*)" Auxiliary  593 _tPGrazCeraBird_", tPGrazCeraBird
write (1,*)" Auxiliary  594 _tPBedCera_", tPBedCera
write (1,*)" Auxiliary  595 _tNExcrCera_", tNExcrCera
write (1,*)" Auxiliary  596 _tNExcrCeraS_", tNExcrCeraS
write (1,*)" Auxiliary  597 _tNExcrCeraW_", tNExcrCeraW
write (1,*)" Auxiliary  598 _tNMortCera_", tNMortCera
write (1,*)" Auxiliary  599 _tNMortCeraNH4_", tNMortCeraNH4
write (1,*)" Auxiliary  600 _tNMortCeraNH4S_", tNMortCeraNH4S
write (1,*)" Auxiliary  601 _tNMortCeraNH4W_", tNMortCeraNH4W
write (1,*)" Auxiliary  602 _tNMortCeraDet_", tNMortCeraDet
write (1,*)" Auxiliary  603 _tNMortCeraDetW_", tNMortCeraDetW
write (1,*)" Auxiliary  604 _tNMortCeraDetS_", tNMortCeraDetS
write (1,*)" Auxiliary  605 _tNGrazCeraBird_", tNGrazCeraBird
write (1,*)" Auxiliary  606 _tNBedCera_", tNBedCera
write (1,*)" Auxiliary  607 _rPDLemn_", rPDLemn
write (1,*)" Auxiliary  608 _rNDLemn_", rNDLemn
write (1,*)" Auxiliary  609 _tDMigrLemn_", tDMigrLemn
write (1,*)" Auxiliary  610 _tPMigrLemn_", tPMigrLemn
write (1,*)" Auxiliary  611 _tNMigrLemn_", tNMigrLemn
write (1,*)" Auxiliary  612 _uFunTmProdLemn_", uFunTmProdLemn
write (1,*)" Auxiliary  613 _uFunTmRespLemn_", uFunTmRespLemn
write (1,*)" Auxiliary  614 _aVPUptMaxCrLemn_", aVPUptMaxCrLemn
write (1,*)" Auxiliary  615 _aVPUptLemnW_", aVPUptLemnW
write (1,*)" Auxiliary  616 _tPUptLemnW_", tPUptLemnW
write (1,*)" Auxiliary  617 _aVPUptLemnS_", aVPUptLemnS
write (1,*)" Auxiliary  618 _tPUptLemnS_", tPUptLemnS
write (1,*)" Auxiliary  619 _tPUptLemn_", tPUptLemn
write (1,*)" Auxiliary  620 _aVNUptMaxCrLemn_", aVNUptMaxCrLemn
write (1,*)" Auxiliary  621 _ahNUptLemn_", ahNUptLemn
write (1,*)" Auxiliary  622 _aVNUptLemnW_", aVNUptLemnW
write (1,*)" Auxiliary  623 _tNUptLemnW_", tNUptLemnW
write (1,*)" Auxiliary  624 _afNH4UptLemnW_", afNH4UptLemnW
write (1,*)" Auxiliary  625 _tNUptNH4LemnW_", tNUptNH4LemnW
write (1,*)" Auxiliary  626 _tNUptNO3LemnW_", tNUptNO3LemnW
write (1,*)" Auxiliary  627 _aVNUptLemnS_", aVNUptLemnS
write (1,*)" Auxiliary  628 _tNUptLemnS_", tNUptLemnS
write (1,*)" Auxiliary  629 _afNH4UptLemnS_", afNH4UptLemnS
write (1,*)" Auxiliary  630 _tNUptNH4LemnS_", tNUptNH4LemnS
write (1,*)" Auxiliary  631 _tNUptNO3LemnS_", tNUptNO3LemnS
write (1,*)" Auxiliary  632 _tNUptLemn_", tNUptLemn
write (1,*)" Auxiliary  633 _aLPAR1Lemn_", aLPAR1Lemn
write (1,*)" Auxiliary  634 _aLPAR2Lemn_", aLPAR2Lemn
write (1,*)" Auxiliary  635 _uhLLemn_", uhLLemn
write (1,*)" Auxiliary  636 _aLLimShootLemn_", aLLimShootLemn
write (1,*)" Auxiliary  637 _aMuTmLLemn_", aMuTmLLemn
write (1,*)" Auxiliary  638 _aPLimLemn_", aPLimLemn
write (1,*)" Auxiliary  639 _aNLimLemn_", aNLimLemn
write (1,*)" Auxiliary  640 _aNutLimLemn_", aNutLimLemn
write (1,*)" Auxiliary  641 _aMuLemn_", aMuLemn
write (1,*)" Auxiliary  642 _bkMortLemn_", bkMortLemn
write (1,*)" Auxiliary  643 _akDIncrLemn_", akDIncrLemn
write (1,*)" Auxiliary  644 _tDEnvLemn_", tDEnvLemn
write (1,*)" Auxiliary  645 _tDEnvProdLemn_", tDEnvProdLemn
write (1,*)" Auxiliary  646 _tDProdLemn_", tDProdLemn
write (1,*)" Auxiliary  647 _tDProdSubLemn_", tDProdSubLemn
write (1,*)" Auxiliary  648 _tDRespLemn_", tDRespLemn
write (1,*)" Auxiliary  649 _tDEnvMortLemn_", tDEnvMortLemn
write (1,*)" Auxiliary  650 _tDMortLemn_", tDMortLemn
write (1,*)" Auxiliary  651 _tDMortLemnW_", tDMortLemnW
write (1,*)" Auxiliary  652 _tDMortLemnS_", tDMortLemnS
write (1,*)" Auxiliary  653 _tDGrazLemnBird_", tDGrazLemnBird
write (1,*)" Auxiliary  654 _bkManLemn_", bkManLemn
write (1,*)" Auxiliary  655 _tDManLemn_", tDManLemn
write (1,*)" Auxiliary  656 _tPManLemn_", tPManLemn
write (1,*)" Auxiliary  657 _tNManLemn_", tNManLemn
write (1,*)" Auxiliary  658 _tDBedLemn_", tDBedLemn
write (1,*)" Auxiliary  659 _tO2ProdLemn_", tO2ProdLemn
write (1,*)" Auxiliary  660 _tO2RespLemnW_", tO2RespLemnW
write (1,*)" Auxiliary  661 _tO2RespLemnS_", tO2RespLemnS
write (1,*)" Auxiliary  662 _tO2ProdLemnS_", tO2ProdLemnS
write (1,*)" Auxiliary  663 _tO2ProdLemnW_", tO2ProdLemnW
write (1,*)" Auxiliary  664 _tO2UptNO3LemnW_", tO2UptNO3LemnW
write (1,*)" Auxiliary  665 _tO2UptNO3LemnS_", tO2UptNO3LemnS
write (1,*)" Auxiliary  666 _tPExcrLemn_", tPExcrLemn
write (1,*)" Auxiliary  667 _tPExcrLemnS_", tPExcrLemnS
write (1,*)" Auxiliary  668 _tPExcrLemnW_", tPExcrLemnW
write (1,*)" Auxiliary  669 _tPMortLemn_", tPMortLemn
write (1,*)" Auxiliary  670 _tPMortLemnPO4_", tPMortLemnPO4
write (1,*)" Auxiliary  671 _tPMortLemnPO4S_", tPMortLemnPO4S
write (1,*)" Auxiliary  672 _tPMortLemnPO4W_", tPMortLemnPO4W
write (1,*)" Auxiliary  673 _tPMortLemnDet_", tPMortLemnDet
write (1,*)" Auxiliary  674 _tPMortLemnDetW_", tPMortLemnDetW
write (1,*)" Auxiliary  675 _tPMortLemnDetS_", tPMortLemnDetS
write (1,*)" Auxiliary  676 _tPGrazLemnBird_", tPGrazLemnBird
write (1,*)" Auxiliary  677 _tPBedLemn_", tPBedLemn
write (1,*)" Auxiliary  678 _tNExcrLemn_", tNExcrLemn
write (1,*)" Auxiliary  679 _tNExcrLemnS_", tNExcrLemnS
write (1,*)" Auxiliary  680 _tNExcrLemnW_", tNExcrLemnW
write (1,*)" Auxiliary  681 _tNMortLemn_", tNMortLemn
write (1,*)" Auxiliary  682 _tNMortLemnNH4_", tNMortLemnNH4
write (1,*)" Auxiliary  683 _tNMortLemnNH4S_", tNMortLemnNH4S
write (1,*)" Auxiliary  684 _tNMortLemnNH4W_", tNMortLemnNH4W
write (1,*)" Auxiliary  685 _tNMortLemnDet_", tNMortLemnDet
write (1,*)" Auxiliary  686 _tNMortLemnDetW_", tNMortLemnDetW
write (1,*)" Auxiliary  687 _tNMortLemnDetS_", tNMortLemnDetS
write (1,*)" Auxiliary  688 _tNGrazLemnBird_", tNGrazLemnBird
write (1,*)" Auxiliary  689 _tNBedLemn_", tNBedLemn
write (1,*)" Auxiliary  690 _rPDNymp_", rPDNymp
write (1,*)" Auxiliary  691 _rNDNymp_", rNDNymp
write (1,*)" Auxiliary  692 _tDMigrNymp_", tDMigrNymp
write (1,*)" Auxiliary  693 _tPMigrNymp_", tPMigrNymp
write (1,*)" Auxiliary  694 _tNMigrNymp_", tNMigrNymp
write (1,*)" Auxiliary  695 _uFunTmProdNymp_", uFunTmProdNymp
write (1,*)" Auxiliary  696 _uFunTmRespNymp_", uFunTmRespNymp
write (1,*)" Auxiliary  697 _aVPUptMaxCrNymp_", aVPUptMaxCrNymp
write (1,*)" Auxiliary  698 _aVPUptNympW_", aVPUptNympW
write (1,*)" Auxiliary  699 _tPUptNympW_", tPUptNympW
write (1,*)" Auxiliary  700 _aVPUptNympS_", aVPUptNympS
write (1,*)" Auxiliary  701 _tPUptNympS_", tPUptNympS
write (1,*)" Auxiliary  702 _tPUptNymp_", tPUptNymp
write (1,*)" Auxiliary  703 _aVNUptMaxCrNymp_", aVNUptMaxCrNymp
write (1,*)" Auxiliary  704 _ahNUptNymp_", ahNUptNymp
write (1,*)" Auxiliary  705 _aVNUptNympW_", aVNUptNympW
write (1,*)" Auxiliary  706 _tNUptNympW_", tNUptNympW
write (1,*)" Auxiliary  707 _afNH4UptNympW_", afNH4UptNympW
write (1,*)" Auxiliary  708 _tNUptNH4NympW_", tNUptNH4NympW
write (1,*)" Auxiliary  709 _tNUptNO3NympW_", tNUptNO3NympW
write (1,*)" Auxiliary  710 _aVNUptNympS_", aVNUptNympS
write (1,*)" Auxiliary  711 _tNUptNympS_", tNUptNympS
write (1,*)" Auxiliary  712 _afNH4UptNympS_", afNH4UptNympS
write (1,*)" Auxiliary  713 _tNUptNH4NympS_", tNUptNH4NympS
write (1,*)" Auxiliary  714 _tNUptNO3NympS_", tNUptNO3NympS
write (1,*)" Auxiliary  715 _tNUptNymp_", tNUptNymp
write (1,*)" Auxiliary  716 _aLPAR1Nymp_", aLPAR1Nymp
write (1,*)" Auxiliary  717 _aLPAR2Nymp_", aLPAR2Nymp
write (1,*)" Auxiliary  718 _uhLNymp_", uhLNymp
write (1,*)" Auxiliary  719 _aLLimShootNymp_", aLLimShootNymp
write (1,*)" Auxiliary  720 _aMuTmLNymp_", aMuTmLNymp
write (1,*)" Auxiliary  721 _aPLimNymp_", aPLimNymp
write (1,*)" Auxiliary  722 _aNLimNymp_", aNLimNymp
write (1,*)" Auxiliary  723 _aNutLimNymp_", aNutLimNymp
write (1,*)" Auxiliary  724 _aMuNymp_", aMuNymp
write (1,*)" Auxiliary  725 _bkMortNymp_", bkMortNymp
write (1,*)" Auxiliary  726 _akDIncrNymp_", akDIncrNymp
write (1,*)" Auxiliary  727 _tDEnvNymp_", tDEnvNymp
write (1,*)" Auxiliary  728 _tDEnvProdNymp_", tDEnvProdNymp
write (1,*)" Auxiliary  729 _tDProdNymp_", tDProdNymp
write (1,*)" Auxiliary  730 _tDProdSubNymp_", tDProdSubNymp
write (1,*)" Auxiliary  731 _tDRespNymp_", tDRespNymp
write (1,*)" Auxiliary  732 _tDEnvMortNymp_", tDEnvMortNymp
write (1,*)" Auxiliary  733 _tDMortNymp_", tDMortNymp
write (1,*)" Auxiliary  734 _tDMortNympW_", tDMortNympW
write (1,*)" Auxiliary  735 _tDMortNympS_", tDMortNympS
write (1,*)" Auxiliary  736 _tDGrazNympBird_", tDGrazNympBird
write (1,*)" Auxiliary  737 _bkManNymp_", bkManNymp
write (1,*)" Auxiliary  738 _tDManNymp_", tDManNymp
write (1,*)" Auxiliary  739 _tPManNymp_", tPManNymp
write (1,*)" Auxiliary  740 _tNManNymp_", tNManNymp
write (1,*)" Auxiliary  741 _tDBedNymp_", tDBedNymp
write (1,*)" Auxiliary  742 _tO2ProdNymp_", tO2ProdNymp
write (1,*)" Auxiliary  743 _tO2RespNympW_", tO2RespNympW
write (1,*)" Auxiliary  744 _tO2RespNympS_", tO2RespNympS
write (1,*)" Auxiliary  745 _tO2ProdNympS_", tO2ProdNympS
write (1,*)" Auxiliary  746 _tO2ProdNympW_", tO2ProdNympW
write (1,*)" Auxiliary  747 _tO2UptNO3NympW_", tO2UptNO3NympW
write (1,*)" Auxiliary  748 _tO2UptNO3NympS_", tO2UptNO3NympS
write (1,*)" Auxiliary  749 _tPExcrNymp_", tPExcrNymp
write (1,*)" Auxiliary  750 _tPExcrNympS_", tPExcrNympS
write (1,*)" Auxiliary  751 _tPExcrNympW_", tPExcrNympW
write (1,*)" Auxiliary  752 _tPMortNymp_", tPMortNymp
write (1,*)" Auxiliary  753 _tPMortNympPO4_", tPMortNympPO4
write (1,*)" Auxiliary  754 _tPMortNympPO4S_", tPMortNympPO4S
write (1,*)" Auxiliary  755 _tPMortNympPO4W_", tPMortNympPO4W
write (1,*)" Auxiliary  756 _tPMortNympDet_", tPMortNympDet
write (1,*)" Auxiliary  757 _tPMortNympDetW_", tPMortNympDetW
write (1,*)" Auxiliary  758 _tPMortNympDetS_", tPMortNympDetS
write (1,*)" Auxiliary  759 _tPGrazNympBird_", tPGrazNympBird
write (1,*)" Auxiliary  760 _tPBedNymp_", tPBedNymp
write (1,*)" Auxiliary  761 _tNExcrNymp_", tNExcrNymp
write (1,*)" Auxiliary  762 _tNExcrNympS_", tNExcrNympS
write (1,*)" Auxiliary  763 _tNExcrNympW_", tNExcrNympW
write (1,*)" Auxiliary  764 _tNMortNymp_", tNMortNymp
write (1,*)" Auxiliary  765 _tNMortNympNH4_", tNMortNympNH4
write (1,*)" Auxiliary  766 _tNMortNympNH4S_", tNMortNympNH4S
write (1,*)" Auxiliary  767 _tNMortNympNH4W_", tNMortNympNH4W
write (1,*)" Auxiliary  768 _tNMortNympDet_", tNMortNympDet
write (1,*)" Auxiliary  769 _tNMortNympDetW_", tNMortNympDetW
write (1,*)" Auxiliary  770 _tNMortNympDetS_", tNMortNympDetS
write (1,*)" Auxiliary  771 _tNGrazNympBird_", tNGrazNympBird
write (1,*)" Auxiliary  772 _tNBedNymp_", tNBedNymp
write (1,*)" Auxiliary  773 _rPDHelo_", rPDHelo
write (1,*)" Auxiliary  774 _rNDHelo_", rNDHelo
write (1,*)" Auxiliary  775 _tDMigrHelo_", tDMigrHelo
write (1,*)" Auxiliary  776 _tPMigrHelo_", tPMigrHelo
write (1,*)" Auxiliary  777 _tNMigrHelo_", tNMigrHelo
write (1,*)" Auxiliary  778 _uFunTmProdHelo_", uFunTmProdHelo
write (1,*)" Auxiliary  779 _uFunTmRespHelo_", uFunTmRespHelo
write (1,*)" Auxiliary  780 _aVPUptMaxCrHelo_", aVPUptMaxCrHelo
write (1,*)" Auxiliary  781 _aVPUptHeloW_", aVPUptHeloW
write (1,*)" Auxiliary  782 _tPUptHeloW_", tPUptHeloW
write (1,*)" Auxiliary  783 _aVPUptHeloS_", aVPUptHeloS
write (1,*)" Auxiliary  784 _tPUptHeloS_", tPUptHeloS
write (1,*)" Auxiliary  785 _tPUptHelo_", tPUptHelo
write (1,*)" Auxiliary  786 _aVNUptMaxCrHelo_", aVNUptMaxCrHelo
write (1,*)" Auxiliary  787 _ahNUptHelo_", ahNUptHelo
write (1,*)" Auxiliary  788 _aVNUptHeloW_", aVNUptHeloW
write (1,*)" Auxiliary  789 _tNUptHeloW_", tNUptHeloW
write (1,*)" Auxiliary  790 _afNH4UptHeloW_", afNH4UptHeloW
write (1,*)" Auxiliary  791 _tNUptNH4HeloW_", tNUptNH4HeloW
write (1,*)" Auxiliary  792 _tNUptNO3HeloW_", tNUptNO3HeloW
write (1,*)" Auxiliary  793 _aVNUptHeloS_", aVNUptHeloS
write (1,*)" Auxiliary  794 _tNUptHeloS_", tNUptHeloS
write (1,*)" Auxiliary  795 _afNH4UptHeloS_", afNH4UptHeloS
write (1,*)" Auxiliary  796 _tNUptNH4HeloS_", tNUptNH4HeloS
write (1,*)" Auxiliary  797 _tNUptNO3HeloS_", tNUptNO3HeloS
write (1,*)" Auxiliary  798 _tNUptHelo_", tNUptHelo
write (1,*)" Auxiliary  799 _aLPAR1Helo_", aLPAR1Helo
write (1,*)" Auxiliary  800 _aLPAR2Helo_", aLPAR2Helo
write (1,*)" Auxiliary  801 _uhLHelo_", uhLHelo
write (1,*)" Auxiliary  802 _aLLimShootHelo_", aLLimShootHelo
write (1,*)" Auxiliary  803 _aMuTmLHelo_", aMuTmLHelo
write (1,*)" Auxiliary  804 _aPLimHelo_", aPLimHelo
write (1,*)" Auxiliary  805 _aNLimHelo_", aNLimHelo
write (1,*)" Auxiliary  806 _aNutLimHelo_", aNutLimHelo
write (1,*)" Auxiliary  807 _aMuHelo_", aMuHelo
write (1,*)" Auxiliary  808 _bkMortHelo_", bkMortHelo
write (1,*)" Auxiliary  809 _akDIncrHelo_", akDIncrHelo
write (1,*)" Auxiliary  810 _tDEnvHelo_", tDEnvHelo
write (1,*)" Auxiliary  811 _tDEnvProdHelo_", tDEnvProdHelo
write (1,*)" Auxiliary  812 _tDProdHelo_", tDProdHelo
write (1,*)" Auxiliary  813 _tDProdSubHelo_", tDProdSubHelo
write (1,*)" Auxiliary  814 _tDRespHelo_", tDRespHelo
write (1,*)" Auxiliary  815 _tDEnvMortHelo_", tDEnvMortHelo
write (1,*)" Auxiliary  816 _tDMortHelo_", tDMortHelo
write (1,*)" Auxiliary  817 _tDMortHeloW_", tDMortHeloW
write (1,*)" Auxiliary  818 _tDMortHeloS_", tDMortHeloS
write (1,*)" Auxiliary  819 _tDGrazHeloBird_", tDGrazHeloBird
write (1,*)" Auxiliary  820 _bkManHelo_", bkManHelo
write (1,*)" Auxiliary  821 _tDManHelo_", tDManHelo
write (1,*)" Auxiliary  822 _tPManHelo_", tPManHelo
write (1,*)" Auxiliary  823 _tNManHelo_", tNManHelo
write (1,*)" Auxiliary  824 _tDBedHelo_", tDBedHelo
write (1,*)" Auxiliary  825 _tO2ProdHelo_", tO2ProdHelo
write (1,*)" Auxiliary  826 _tO2RespHeloW_", tO2RespHeloW
write (1,*)" Auxiliary  827 _tO2RespHeloS_", tO2RespHeloS
write (1,*)" Auxiliary  828 _tO2ProdHeloS_", tO2ProdHeloS
write (1,*)" Auxiliary  829 _tO2ProdHeloW_", tO2ProdHeloW
write (1,*)" Auxiliary  830 _tO2UptNO3HeloW_", tO2UptNO3HeloW
write (1,*)" Auxiliary  831 _tO2UptNO3HeloS_", tO2UptNO3HeloS
write (1,*)" Auxiliary  832 _tPExcrHelo_", tPExcrHelo
write (1,*)" Auxiliary  833 _tPExcrHeloS_", tPExcrHeloS
write (1,*)" Auxiliary  834 _tPExcrHeloW_", tPExcrHeloW
write (1,*)" Auxiliary  835 _tPMortHelo_", tPMortHelo
write (1,*)" Auxiliary  836 _tPMortHeloPO4_", tPMortHeloPO4
write (1,*)" Auxiliary  837 _tPMortHeloPO4S_", tPMortHeloPO4S
write (1,*)" Auxiliary  838 _tPMortHeloPO4W_", tPMortHeloPO4W
write (1,*)" Auxiliary  839 _tPMortHeloDet_", tPMortHeloDet
write (1,*)" Auxiliary  840 _tPMortHeloDetW_", tPMortHeloDetW
write (1,*)" Auxiliary  841 _tPMortHeloDetS_", tPMortHeloDetS
write (1,*)" Auxiliary  842 _tPGrazHeloBird_", tPGrazHeloBird
write (1,*)" Auxiliary  843 _tPBedHelo_", tPBedHelo
write (1,*)" Auxiliary  844 _tNExcrHelo_", tNExcrHelo
write (1,*)" Auxiliary  845 _tNExcrHeloS_", tNExcrHeloS
write (1,*)" Auxiliary  846 _tNExcrHeloW_", tNExcrHeloW
write (1,*)" Auxiliary  847 _tNMortHelo_", tNMortHelo
write (1,*)" Auxiliary  848 _tNMortHeloNH4_", tNMortHeloNH4
write (1,*)" Auxiliary  849 _tNMortHeloNH4S_", tNMortHeloNH4S
write (1,*)" Auxiliary  850 _tNMortHeloNH4W_", tNMortHeloNH4W
write (1,*)" Auxiliary  851 _tNMortHeloDet_", tNMortHeloDet
write (1,*)" Auxiliary  852 _tNMortHeloDetW_", tNMortHeloDetW
write (1,*)" Auxiliary  853 _tNMortHeloDetS_", tNMortHeloDetS
write (1,*)" Auxiliary  854 _tNGrazHeloBird_", tNGrazHeloBird
write (1,*)" Auxiliary  855 _tNBedHelo_", tNBedHelo
write (1,*)" Auxiliary  856 _tDMigrVeg_", tDMigrVeg
write (1,*)" Auxiliary  857 _tPMigrVeg_", tPMigrVeg
write (1,*)" Auxiliary  858 _tNMigrVeg_", tNMigrVeg
write (1,*)" Auxiliary  859 _tDProdVeg_", tDProdVeg
write (1,*)" Auxiliary  860 _tPUptVegW_", tPUptVegW
write (1,*)" Auxiliary  861 _tPUptVegS_", tPUptVegS
write (1,*)" Auxiliary  862 _tNUptNH4VegW_", tNUptNH4VegW
write (1,*)" Auxiliary  863 _tNUptNH4VegS_", tNUptNH4VegS
write (1,*)" Auxiliary  864 _tNUptNO3VegW_", tNUptNO3VegW
write (1,*)" Auxiliary  865 _tNUptNO3VegS_", tNUptNO3VegS
write (1,*)" Auxiliary  866 _tDRespVeg_", tDRespVeg
write (1,*)" Auxiliary  867 _tPExcrVegW_", tPExcrVegW
write (1,*)" Auxiliary  868 _tPExcrVegS_", tPExcrVegS
write (1,*)" Auxiliary  869 _tNExcrVegW_", tNExcrVegW
write (1,*)" Auxiliary  870 _tNExcrVegS_", tNExcrVegS
write (1,*)" Auxiliary  871 _tO2ProdVeg_", tO2ProdVeg
write (1,*)" Auxiliary  872 _tO2ProdVegW_", tO2ProdVegW
write (1,*)" Auxiliary  873 _tO2ProdVegS_", tO2ProdVegS
write (1,*)" Auxiliary  874 _tO2RespVegW_", tO2RespVegW
write (1,*)" Auxiliary  875 _tO2RespVegS_", tO2RespVegS
write (1,*)" Auxiliary  876 _tO2UptNO3VegW_", tO2UptNO3VegW
write (1,*)" Auxiliary  877 _tO2UptNO3VegS_", tO2UptNO3VegS
write (1,*)" Auxiliary  878 _tDMortVegW_", tDMortVegW
write (1,*)" Auxiliary  879 _tDMortVegS_", tDMortVegS
write (1,*)" Auxiliary  880 _tPMortVegPO4W_", tPMortVegPO4W
write (1,*)" Auxiliary  881 _tPMortVegDetW_", tPMortVegDetW
write (1,*)" Auxiliary  882 _tPMortVegPO4S_", tPMortVegPO4S
write (1,*)" Auxiliary  883 _tPMortVegDetS_", tPMortVegDetS
write (1,*)" Auxiliary  884 _tNMortVegNH4W_", tNMortVegNH4W
write (1,*)" Auxiliary  885 _tNMortVegDetW_", tNMortVegDetW
write (1,*)" Auxiliary  886 _tNMortVegNH4S_", tNMortVegNH4S
write (1,*)" Auxiliary  887 _tNMortVegDetS_", tNMortVegDetS
write (1,*)" Auxiliary  888 _tDGrazVegBird_", tDGrazVegBird
write (1,*)" Auxiliary  889 _tPGrazVegBird_", tPGrazVegBird
write (1,*)" Auxiliary  890 _tNGrazVegBird_", tNGrazVegBird
write (1,*)" Auxiliary  891 _tDManVeg_", tDManVeg
write (1,*)" Auxiliary  892 _tPManVeg_", tPManVeg
write (1,*)" Auxiliary  893 _tNManVeg_", tNManVeg
write (1,*)" Auxiliary  894 _aCovSub_", aCovSub
write (1,*)" Auxiliary  895 _tDAssVegBird_", tDAssVegBird
write (1,*)" Auxiliary  896 _tDEgesBird_", tDEgesBird
write (1,*)" Auxiliary  897 _tPAssVegBird_", tPAssVegBird
write (1,*)" Auxiliary  898 _tPEgesBird_", tPEgesBird
write (1,*)" Auxiliary  899 _tPEgesBirdPO4_", tPEgesBirdPO4
write (1,*)" Auxiliary  900 _tPEgesBirdDet_", tPEgesBirdDet
write (1,*)" Auxiliary  901 _tNAssVegBird_", tNAssVegBird
write (1,*)" Auxiliary  902 _tNEgesBird_", tNEgesBird
write (1,*)" Auxiliary  903 _tNEgesBirdNH4_", tNEgesBirdNH4
write (1,*)" Auxiliary  904 _tNEgesBirdDet_", tNEgesBirdDet
write (1,*)" Auxiliary  905 _wDBedDetW_", wDBedDetW
write (1,*)" Auxiliary  906 _tDBedDetS_", tDBedDetS
write (1,*)" Auxiliary  907 _tDBedTotT_", tDBedTotT
write (1,*)" Auxiliary  908 _wPBedPO4W_", wPBedPO4W
write (1,*)" Auxiliary  909 _wPBedDetW_", wPBedDetW
write (1,*)" Auxiliary  910 _tPBedPO4S_", tPBedPO4S
write (1,*)" Auxiliary  911 _tPBedDetS_", tPBedDetS
write (1,*)" Auxiliary  912 _tPBedTotT_", tPBedTotT
write (1,*)" Auxiliary  913 _wNBedNH4W_", wNBedNH4W
write (1,*)" Auxiliary  914 _wNBedNO3W_", wNBedNO3W
write (1,*)" Auxiliary  915 _wNBedDetW_", wNBedDetW
write (1,*)" Auxiliary  916 _tNBedNH4S_", tNBedNH4S
write (1,*)" Auxiliary  917 _tNBedNO3S_", tNBedNO3S
write (1,*)" Auxiliary  918 _tNBedDetS_", tNBedDetS
write (1,*)" Auxiliary  919 _tNBedTotT_", tNBedTotT
write (1,*)" Auxiliary  920 _tO2BedW_", tO2BedW
write (1,*)" Auxiliary  921 _tO2BedS_", tO2BedS
write (1,*)" Auxiliary  922 _UseLoss_", UseLoss
write (1,*)" Auxiliary  923 _uFunTmLoss_", uFunTmLoss
write (1,*)" Auxiliary  924 _rPDPhytW_", rPDPhytW
write (1,*)" Auxiliary  925 _rNDPhytW_", rNDPhytW
write (1,*)" Auxiliary  926 _rPDPhytS_", rPDPhytS
write (1,*)" Auxiliary  927 _rNDPhytS_", rNDPhytS
write (1,*)" Auxiliary  928 _uFunTmPhyt_", uFunTmPhyt
write (1,*)" Auxiliary  929 _uFunTmProdPhyt_", uFunTmProdPhyt
write (1,*)" Auxiliary  930 _uFunTmRespPhyt_", uFunTmRespPhyt
write (1,*)" Auxiliary  931 _aVPUptMaxCrPhyt_", aVPUptMaxCrPhyt
write (1,*)" Auxiliary  932 _aVPUptPhyt_", aVPUptPhyt
write (1,*)" Auxiliary  933 _wPUptPhyt_", wPUptPhyt
write (1,*)" Auxiliary  934 _aVNUptMaxCrPhyt_", aVNUptMaxCrPhyt
write (1,*)" Auxiliary  935 _ahNUptPhyt_", ahNUptPhyt
write (1,*)" Auxiliary  936 _aVNUptPhyt_", aVNUptPhyt
write (1,*)" Auxiliary  937 _wNUptPhyt_", wNUptPhyt
write (1,*)" Auxiliary  938 _afNH4UptPhyt_", afNH4UptPhyt
write (1,*)" Auxiliary  939 _wNUptNH4Phyt_", wNUptNH4Phyt
write (1,*)" Auxiliary  940 _wNUptNO3Phyt_", wNUptNO3Phyt
write (1,*)" Auxiliary  941 _uMuMaxTmPhyt_", uMuMaxTmPhyt
write (1,*)" Auxiliary  942 _aPLimPhyt_", aPLimPhyt
write (1,*)" Auxiliary  943 _aNLimPhyt_", aNLimPhyt
write (1,*)" Auxiliary  944 _aLLimPhyt_", aLLimPhyt
write (1,*)" Auxiliary  945 _aMuTmLPhyt_", aMuTmLPhyt
write (1,*)" Auxiliary  946 _aNutLimPhyt_", aNutLimPhyt
write (1,*)" Auxiliary  947 _aMuPhyt_", aMuPhyt
write (1,*)" Auxiliary  948 _wDAssPhyt_", wDAssPhyt
write (1,*)" Auxiliary  949 _rChDPhyt_", rChDPhyt
write (1,*)" Auxiliary  950 _oChlaPhyt_", oChlaPhyt
write (1,*)" Auxiliary  951 _aExtChPhyt_", aExtChPhyt
write (1,*)" Auxiliary  952 _ukDRespTmPhyt_", ukDRespTmPhyt
write (1,*)" Auxiliary  953 _wDRespPhytW_", wDRespPhytW
write (1,*)" Auxiliary  954 _ukLossTmPhyt_", ukLossTmPhyt
write (1,*)" Auxiliary  955 _wDLossPhyt_", wDLossPhyt
write (1,*)" Auxiliary  956 _wDMortPhytW_", wDMortPhytW
write (1,*)" Auxiliary  957 _uCorVSetPhyt_", uCorVSetPhyt
write (1,*)" Auxiliary  958 _tDSetPhyt_", tDSetPhyt
write (1,*)" Auxiliary  959 _tDResusPhyt_", tDResusPhyt
write (1,*)" Auxiliary  960 _tDRespPhytS_", tDRespPhytS
write (1,*)" Auxiliary  961 _tDMortPhytS_", tDMortPhytS
write (1,*)" Auxiliary  962 _ukDDecPhyt_", ukDDecPhyt
write (1,*)" Auxiliary  963 _wPExcrPhytW_", wPExcrPhytW
write (1,*)" Auxiliary  964 _wPLossPhyt_", wPLossPhyt
write (1,*)" Auxiliary  965 _wPMortPhytW_", wPMortPhytW
write (1,*)" Auxiliary  966 _tPSetPhyt_", tPSetPhyt
write (1,*)" Auxiliary  967 _tPResusPhyt_", tPResusPhyt
write (1,*)" Auxiliary  968 _tPExcrPhytS_", tPExcrPhytS
write (1,*)" Auxiliary  969 _tPMortPhytS_", tPMortPhytS
write (1,*)" Auxiliary  970 _wNExcrPhytW_", wNExcrPhytW
write (1,*)" Auxiliary  971 _wNLossPhyt_", wNLossPhyt
write (1,*)" Auxiliary  972 _wNMortPhytW_", wNMortPhytW
write (1,*)" Auxiliary  973 _tNSetPhyt_", tNSetPhyt
write (1,*)" Auxiliary  974 _tNResusPhyt_", tNResusPhyt
write (1,*)" Auxiliary  975 _tNExcrPhytS_", tNExcrPhytS
write (1,*)" Auxiliary  976 _tNMortPhytS_", tNMortPhytS
write (1,*)" Auxiliary  977 _wDPrimPhytW_", wDPrimPhytW
write (1,*)" Auxiliary  978 _wPPrimPhytW_", wPPrimPhytW
write (1,*)" Auxiliary  979 _wNPrimPhytW_", wNPrimPhytW
write (1,*)" Auxiliary  980 _tDPrimPhytS_", tDPrimPhytS
write (1,*)" Auxiliary  981 _tPPrimPhytS_", tPPrimPhytS
write (1,*)" Auxiliary  982 _tNPrimPhytS_", tNPrimPhytS
write (1,*)" Auxiliary  983 _oChla_", oChla
write (1,*)" Auxiliary  984 _wDPrimDetW_", wDPrimDetW
write (1,*)" Auxiliary  985 _tDPrimDetS_", tDPrimDetS
write (1,*)" Auxiliary  986 _tDPrimTotT_", tDPrimTotT
write (1,*)" Auxiliary  987 _wO2ProdPhyt_", wO2ProdPhyt
write (1,*)" Auxiliary  988 _wO2RespPhytW_", wO2RespPhytW
write (1,*)" Auxiliary  989 _wO2UptNO3Phyt_", wO2UptNO3Phyt
write (1,*)" Auxiliary  990 _wO2PrimW_", wO2PrimW
write (1,*)" Auxiliary  991 _tO2RespPhytS_", tO2RespPhytS
write (1,*)" Auxiliary  992 _tO2PrimS_", tO2PrimS
write (1,*)" Auxiliary  993 _wPMortPhytPO4W_", wPMortPhytPO4W
write (1,*)" Auxiliary  994 _wPMortPhytDetW_", wPMortPhytDetW
write (1,*)" Auxiliary  995 _wPLossPhytPO4_", wPLossPhytPO4
write (1,*)" Auxiliary  996 _wPLossPhytDet_", wPLossPhytDet
write (1,*)" Auxiliary  997 _wPPrimPO4W_", wPPrimPO4W
write (1,*)" Auxiliary  998 _wPPrimDetW_", wPPrimDetW
write (1,*)" Auxiliary  999 _tPMortPhytPO4S_", tPMortPhytPO4S
write (1,*)" Auxiliary  1000 _tPMortPhytDetS_", tPMortPhytDetS
write (1,*)" Auxiliary  1001 _tPPrimDetS_", tPPrimDetS
write (1,*)" Auxiliary  1002 _tPPrimPO4S_", tPPrimPO4S
write (1,*)" Auxiliary  1003 _tPPrimTotT_", tPPrimTotT
write (1,*)" Auxiliary  1004 _wNMortPhytNH4W_", wNMortPhytNH4W
write (1,*)" Auxiliary  1005 _wNMortPhytDetW_", wNMortPhytDetW
write (1,*)" Auxiliary  1006 _wNLossPhytNH4_", wNLossPhytNH4
write (1,*)" Auxiliary  1007 _wNLossPhytDet_", wNLossPhytDet
write (1,*)" Auxiliary  1008 _wNPrimNH4W_", wNPrimNH4W
write (1,*)" Auxiliary  1009 _wNPrimNO3W_", wNPrimNO3W
write (1,*)" Auxiliary  1010 _wNPrimDetW_", wNPrimDetW
write (1,*)" Auxiliary  1011 _tNMortPhytNH4S_", tNMortPhytNH4S
write (1,*)" Auxiliary  1012 _tNMortPhytDetS_", tNMortPhytDetS
write (1,*)" Auxiliary  1013 _tNPrimNH4S_", tNPrimNH4S
write (1,*)" Auxiliary  1014 _tNPrimNO3S_", tNPrimNO3S
write (1,*)" Auxiliary  1015 _tNPrimDetS_", tNPrimDetS
write (1,*)" Auxiliary  1016 _tNPrimTotT_", tNPrimTotT
write (1,*)" Auxiliary  1017 _aPACoef_", aPACoef
write (1,*)" Auxiliary  1018 _bSecchiMax_", bSecchiMax
write (1,*)" Auxiliary  1019 _aSecchi_", aSecchi
write (1,*)" Auxiliary  1020 _aDepthEuph_", aDepthEuph
write (1,*)" Auxiliary  1021 _aRelDepthEuph_", aRelDepthEuph
write (1,*)" Auxiliary  1022 _aChlaH_", aChlaH
write (1,*)" Auxiliary  1023 _aCovPhytW_", aCovPhytW
write (1,*)" Auxiliary  1024 _rExtChPhyt_", rExtChPhyt
write (1,*)" Auxiliary  1025 _tDSetTot_", tDSetTot
write (1,*)" Auxiliary  1026 _tPSetTot_", tPSetTot
write (1,*)" Auxiliary  1027 _tNSetTot_", tNSetTot
write (1,*)" Auxiliary  1028 _tDResusTot_", tDResusTot
write (1,*)" Auxiliary  1029 _tPResusTot_", tPResusTot
write (1,*)" Auxiliary  1030 _tNResusTot_", tNResusTot
write (1,*)" Auxiliary  1031 _bTimeDred_", bTimeDred
write (1,*)" Auxiliary  1032 _aDepthStart_", aDepthStart
write (1,*)" Auxiliary  1033 _akDredDepth_", akDredDepth
write (1,*)" Auxiliary  1034 _akDred_", akDred
write (1,*)" Auxiliary  1035 _akDredLemn_", akDredLemn
write (1,*)" Auxiliary  1036 _vDredDepthW_", vDredDepthW
write (1,*)" Auxiliary  1037 _tDDredDetS_", tDDredDetS
write (1,*)" Auxiliary  1038 _tPDredDetS_", tPDredDetS
write (1,*)" Auxiliary  1039 _tNDredDetS_", tNDredDetS
write (1,*)" Auxiliary  1040 _tPDredAIMS_", tPDredAIMS
write (1,*)" Auxiliary  1041 _bRhoSolidSoil_", bRhoSolidSoil
write (1,*)" Auxiliary  1042 _tDDredNetSoil_", tDDredNetSoil
write (1,*)" Auxiliary  1043 _tDDredNetIMS_", tDDredNetIMS
write (1,*)" Auxiliary  1044 _tDDredNetHumS_", tDDredNetHumS
write (1,*)" Auxiliary  1045 _tPDredNetHumS_", tPDredNetHumS
write (1,*)" Auxiliary  1046 _tNDredNetHumS_", tNDredNetHumS
write (1,*)" Auxiliary  1047 _tDDredPhytS_", tDDredPhytS
write (1,*)" Auxiliary  1048 _tPDredPhytS_", tPDredPhytS
write (1,*)" Auxiliary  1049 _tNDredPhytS_", tNDredPhytS
write (1,*)" Auxiliary  1050 _tDDredElod_", tDDredElod
write (1,*)" Auxiliary  1051 _tPDredElod_", tPDredElod
write (1,*)" Auxiliary  1052 _tNDredElod_", tNDredElod
write (1,*)" Auxiliary  1053 _tDDredCera_", tDDredCera
write (1,*)" Auxiliary  1054 _tPDredCera_", tPDredCera
write (1,*)" Auxiliary  1055 _tNDredCera_", tNDredCera
write (1,*)" Auxiliary  1056 _tDDredChar_", tDDredChar
write (1,*)" Auxiliary  1057 _tPDredChar_", tPDredChar
write (1,*)" Auxiliary  1058 _tNDredChar_", tNDredChar
write (1,*)" Auxiliary  1059 _tDDredLemn_", tDDredLemn
write (1,*)" Auxiliary  1060 _tPDredLemn_", tPDredLemn
write (1,*)" Auxiliary  1061 _tNDredLemn_", tNDredLemn
write (1,*)" Auxiliary  1062 _tDDredNymp_", tDDredNymp
write (1,*)" Auxiliary  1063 _tPDredNymp_", tPDredNymp
write (1,*)" Auxiliary  1064 _tNDredNymp_", tNDredNymp
write (1,*)" Auxiliary  1065 _tDDredHelo_", tDDredHelo
write (1,*)" Auxiliary  1066 _tPDredHelo_", tPDredHelo
write (1,*)" Auxiliary  1067 _tNDredHelo_", tNDredHelo
write (1,*)" Auxiliary  1068 _tDDredVeg_", tDDredVeg
write (1,*)" Auxiliary  1069 _tPDredVeg_", tPDredVeg
write (1,*)" Auxiliary  1070 _tNDredVeg_", tNDredVeg
write (1,*)" Auxiliary  1071 _tDDredNetTot_", tDDredNetTot
write (1,*)" Auxiliary  1072 _tPDredNetTot_", tPDredNetTot
write (1,*)" Auxiliary  1073 _tNDredNetTot_", tNDredNetTot
write (1,*)" Auxiliary  1074 _tDIMS_", tDIMS
write (1,*)" Auxiliary  1075 _tDHumS_", tDHumS
write (1,*)" Auxiliary  1076 _tDDetS_", tDDetS
write (1,*)" Auxiliary  1077 _vDeltaS_", vDeltaS
write (1,*)" Auxiliary  1078 _tDBurIM_", tDBurIM
write (1,*)" Auxiliary  1079 _tDBurOM_", tDBurOM
write (1,*)" Auxiliary  1080 _tDBurDet_", tDBurDet
write (1,*)" Auxiliary  1081 _tDBurHum_", tDBurHum
write (1,*)" Auxiliary  1082 _tDBurTot_", tDBurTot
write (1,*)" Auxiliary  1083 _tPBurHum_", tPBurHum
write (1,*)" Auxiliary  1084 _tPBurDet_", tPBurDet
write (1,*)" Auxiliary  1085 _tPBurAIM_", tPBurAIM
write (1,*)" Auxiliary  1086 _tPBurPO4_", tPBurPO4
write (1,*)" Auxiliary  1087 _tPBurTot_", tPBurTot
write (1,*)" Auxiliary  1088 _tNBurHum_", tNBurHum
write (1,*)" Auxiliary  1089 _tNBurDet_", tNBurDet
write (1,*)" Auxiliary  1090 _tNBurNH4_", tNBurNH4
write (1,*)" Auxiliary  1091 _tNBurNO3_", tNBurNO3
write (1,*)" Auxiliary  1092 _tNBurTot_", tNBurTot
write (1,*)" Auxiliary  1093 _vDeltaW_", vDeltaW
write (1,*)" Auxiliary  1094 _aRelDeltaW_", aRelDeltaW
write (1,*)" Auxiliary  1095 _aDTotT_", aDTotT
write (1,*)" Auxiliary  1096 _aNTotT_", aNTotT
write (1,*)" Auxiliary  1097 _aPTotT_", aPTotT
write (1,*)" Auxiliary  1098 _aDError_", aDError
write (1,*)" Auxiliary  1099 _aNError_", aNError
write (1,*)" Auxiliary  1100 _aPError_", aPError
write (1,*)" Derivative  0 _dDDetS_", dDDetS
write (1,*)" Derivative  1 _dDDetW_", dDDetW
write (1,*)" Derivative  2 _dDepthW_ -999"
write (1,*)" Derivative  3 _dDHumS_", dDHumS
write (1,*)" Derivative  4 _dDIMS_", dDIMS
write (1,*)" Derivative  5 _dDIMW_", dDIMW
write (1,*)" Derivative  6 _dNDetS_", dNDetS
write (1,*)" Derivative  7 _dNDetW_", dNDetW
write (1,*)" Derivative  8 _dNH4S_", dNH4S
write (1,*)" Derivative  9 _dNH4W_", dNH4W
write (1,*)" Derivative  10 _dNHumS_", dNHumS
write (1,*)" Derivative  11 _dNO3S_", dNO3S
write (1,*)" Derivative  12 _dNO3W_", dNO3W
write (1,*)" Derivative  13 _dO2W_", dO2W
write (1,*)" Derivative  14 _dPAIMS_", dPAIMS
write (1,*)" Derivative  15 _dPAIMW_", dPAIMW
write (1,*)" Derivative  16 _dPDetS_", dPDetS
write (1,*)" Derivative  17 _dPDetW_", dPDetW
write (1,*)" Derivative  18 _dPHumS_", dPHumS
write (1,*)" Derivative  19 _dPO4S_", dPO4S
write (1,*)" Derivative  20 _dPO4W_", dPO4W
write (1,*)" Derivative  21 _dDPhytW_", dDPhytW
write (1,*)" Derivative  22 _dPPhytW_", dPPhytW
write (1,*)" Derivative  23 _dNPhytW_", dNPhytW
write (1,*)" Derivative  24 _dDPhytS_", dDPhytS
write (1,*)" Derivative  25 _dPPhytS_", dPPhytS
write (1,*)" Derivative  26 _dNPhytS_", dNPhytS
write (1,*)" Derivative  27 _dDElod_", dDElod
write (1,*)" Derivative  28 _dDChar_", dDChar
write (1,*)" Derivative  29 _dDCera_", dDCera
write (1,*)" Derivative  30 _dDLemn_", dDLemn
write (1,*)" Derivative  31 _dDNymp_", dDNymp
write (1,*)" Derivative  32 _dDHelo_", dDHelo
write (1,*)" Derivative  33 _dPElod_", dPElod
write (1,*)" Derivative  34 _dPChar_", dPChar
write (1,*)" Derivative  35 _dPCera_", dPCera
write (1,*)" Derivative  36 _dPLemn_", dPLemn
write (1,*)" Derivative  37 _dPNymp_", dPNymp
write (1,*)" Derivative  38 _dPHelo_", dPHelo
write (1,*)" Derivative  39 _dNElod_", dNElod
write (1,*)" Derivative  40 _dNChar_", dNChar
write (1,*)" Derivative  41 _dNCera_", dNCera
write (1,*)" Derivative  42 _dNLemn_", dNLemn
write (1,*)" Derivative  43 _dNNymp_", dNNymp
write (1,*)" Derivative  44 _dNHelo_", dNHelo
write (1,*)" Derivative  45 _dDExtTotT_", dDExtTotT
write (1,*)" Derivative  46 _dNExtTotT_", dNExtTotT
write (1,*)" Derivative  47 _dPExtTotT_", dPExtTotT
     counter1 = 1
Close (1)
End If
!
if (counter2 == 0) then
open (unit=2,file="staterep.txt",action="write",status="replace")
write (2,*) "STEP ", &
&"sDDetS ", &
&"sDDetW ", &
&"sDepthW ", &
&"sDHumS ", &
&"sDIMS ", &
&"sDIMW ", &
&"sNDetS ", &
&"sNDetW ", &
&"sNH4S ", &
&"sNH4W ", &
&"sNHumS ", &
&"sNO3S ", &
&"sNO3W ", &
&"sO2W ", &
&"sPAIMS ", &
&"sPAIMW ", &
&"sPDetS ", &
&"sPDetW ", &
&"sPHumS ", &
&"sPO4S ", &
&"sPO4W ", &
&"sDPhytW ", &
&"sPPhytW ", &
&"sNPhytW ", &
&"sDPhytS ", &
&"sPPhytS ", &
&"sNPhytS ", &
&"sDElod ", &
&"sDChar ", &
&"sDCera ", &
&"sDLemn ", &
&"sDNymp ", &
&"sDHelo ", &
&"sPElod ", &
&"sPChar ", &
&"sPCera ", &
&"sPLemn ", &
&"sPNymp ", &
&"sPHelo ", &
&"sNElod ", &
&"sNChar ", &
&"sNCera ", &
&"sNLemn ", &
&"sNNymp ", &
&"sNHelo ", &
&"sDExtTotT ", &
&"sNExtTotT ", &
&"sPExtTotT "
     counter2 = 1
Close (2)
End If
!
if (sTime > counter3) then
OPEN (3, FILE = 'staterep.txt', ACCESS = 'APPEND')
write(3,*) sTime, &
&sDDetS , &
&sDDetW , &
&sDepthW , &
&sDHumS , &
&sDIMS , &
&sDIMW , &
&sNDetS , &
&sNDetW , &
&sNH4S , &
&sNH4W , &
&sNHumS , &
&sNO3S , &
&sNO3W , &
&sO2W , &
&sPAIMS , &
&sPAIMW , &
&sPDetS , &
&sPDetW , &
&sPHumS , &
&sPO4S , &
&sPO4W , &
&sDPhytW , &
&sPPhytW , &
&sNPhytW , &
&sDPhytS , &
&sPPhytS , &
&sNPhytS , &
&sDElod , &
&sDChar , &
&sDCera , &
&sDLemn , &
&sDNymp , &
&sDHelo , &
&sPElod , &
&sPChar , &
&sPCera , &
&sPLemn , &
&sPNymp , &
&sPHelo , &
&sNElod , &
&sNChar , &
&sNCera , &
&sNLemn , &
&sNNymp , &
&sNHelo , &
&sDExtTotT , &
&sNExtTotT , &
&sPExtTotT 
Close (3)
     counter3 = counter3 +  1 
End If
!
!
!     /* ==============================  */
!     /* integration calls               */
!     /* ==============================  */
      D0sDDetS         	 =  dDDetS
      D0sDDetW         	 =  dDDetW
      D0sDHumS         	 =  dDHumS
      D0sDIMS          	 =  dDIMS
      D0sDIMW          	 =  dDIMW
      D0sNDetS         	 =  dNDetS
      D0sNDetW         	 =  dNDetW
      D0sNH4S          	 =  dNH4S
      D0sNH4W          	 =  dNH4W
      D0sNHumS         	 =  dNHumS
      D0sNO3S          	 =  dNO3S
      D0sNO3W          	 =  dNO3W
      D0sO2W           	 =  dO2W
      D0sPAIMS         	 =  dPAIMS
      D0sPAIMW         	 =  dPAIMW
      D0sPDetS         	 =  dPDetS
      D0sPDetW         	 =  dPDetW
      D0sPHumS         	 =  dPHumS
      D0sPO4S          	 =  dPO4S
      D0sPO4W          	 =  dPO4W
      D0sDPhytW        	 =  dDPhytW
      D0sPPhytW        	 =  dPPhytW
      D0sNPhytW        	 =  dNPhytW
      D0sDPhytS        	 =  dDPhytS
      D0sPPhytS        	 =  dPPhytS
      D0sNPhytS        	 =  dNPhytS
      D0sDElod         	 =  dDElod
      D0sDChar         	 =  dDChar
      D0sDCera         	 =  dDCera
      D0sDLemn         	 =  dDLemn
      D0sDNymp         	 =  dDNymp
      D0sDHelo         	 =  dDHelo
      D0sPElod         	 =  dPElod
      D0sPChar         	 =  dPChar
      D0sPCera         	 =  dPCera
      D0sPLemn         	 =  dPLemn
      D0sPNymp         	 =  dPNymp
      D0sPHelo         	 =  dPHelo
      D0sNElod         	 =  dNElod
      D0sNChar         	 =  dNChar
      D0sNCera         	 =  dNCera
      D0sNLemn         	 =  dNLemn
      D0sNNymp         	 =  dNNymp
      D0sNHelo         	 =  dNHelo
      D0sDExtTotT      	 =  dDExtTotT
      D0sNExtTotT      	 =  dNExtTotT
      D0sPExtTotT      	 =  dPExtTotT

!   *****     DUPROL code ends here    *****

         fl  ( ID0sDDetS              	) = D0sDDetS              
         fl  ( ID0sDDetW              	) = D0sDDetW              
         fl  ( ID0sDHumS              	) = D0sDHumS              
         fl  ( ID0sDIMS               	) = D0sDIMS               
         fl  ( ID0sDIMW               	) = D0sDIMW               
         fl  ( ID0sNDetS              	) = D0sNDetS              
         fl  ( ID0sNDetW              	) = D0sNDetW              
         fl  ( ID0sNH4S               	) = D0sNH4S               
         fl  ( ID0sNH4W               	) = D0sNH4W               
         fl  ( ID0sNHumS              	) = D0sNHumS              
         fl  ( ID0sNO3S               	) = D0sNO3S               
         fl  ( ID0sNO3W               	) = D0sNO3W               
         fl  ( ID0sO2W                	) = D0sO2W                
         fl  ( ID0sPAIMS              	) = D0sPAIMS              
         fl  ( ID0sPAIMW              	) = D0sPAIMW              
         fl  ( ID0sPDetS              	) = D0sPDetS              
         fl  ( ID0sPDetW              	) = D0sPDetW              
         fl  ( ID0sPHumS              	) = D0sPHumS              
         fl  ( ID0sPO4S               	) = D0sPO4S               
         fl  ( ID0sPO4W               	) = D0sPO4W               
         fl  ( ID0sDPhytW             	) = D0sDPhytW             
         fl  ( ID0sPPhytW             	) = D0sPPhytW             
         fl  ( ID0sNPhytW             	) = D0sNPhytW             
         fl  ( ID0sDPhytS             	) = D0sDPhytS             
         fl  ( ID0sPPhytS             	) = D0sPPhytS             
         fl  ( ID0sNPhytS             	) = D0sNPhytS             
         fl  ( ID0sDElod              	) = D0sDElod              
         fl  ( ID0sDChar              	) = D0sDChar              
         fl  ( ID0sDCera              	) = D0sDCera              
         fl  ( ID0sDLemn              	) = D0sDLemn              
         fl  ( ID0sDNymp              	) = D0sDNymp              
         fl  ( ID0sDHelo              	) = D0sDHelo              
         fl  ( ID0sPElod              	) = D0sPElod              
         fl  ( ID0sPChar              	) = D0sPChar              
         fl  ( ID0sPCera              	) = D0sPCera              
         fl  ( ID0sPLemn              	) = D0sPLemn              
         fl  ( ID0sPNymp              	) = D0sPNymp              
         fl  ( ID0sPHelo              	) = D0sPHelo              
         fl  ( ID0sNElod              	) = D0sNElod              
         fl  ( ID0sNChar              	) = D0sNChar              
         fl  ( ID0sNCera              	) = D0sNCera              
         fl  ( ID0sNLemn              	) = D0sNLemn              
         fl  ( ID0sNNymp              	) = D0sNNymp              
         fl  ( ID0sNHelo              	) = D0sNHelo              
         fl  ( ID0sDExtTotT           	) = D0sDExtTotT           
         fl  ( ID0sNExtTotT           	) = D0sNExtTotT           
         fl  ( ID0sPExtTotT           	) = D0sPExtTotT           
         pmsa( ipnt( 476) ) = sDepthW
         pmsa( ipnt( 477) ) = bPorS
         pmsa( ipnt( 478) ) = bPorCorS
         pmsa( ipnt( 479) ) = sTime
         pmsa( ipnt( 480) ) = TimeYears
         pmsa( ipnt( 481) ) = Day
         pmsa( ipnt( 482) ) = uTm
         pmsa( ipnt( 483) ) = uVWind
         pmsa( ipnt( 484) ) = ufDay
         pmsa( ipnt( 485) ) = uLDay
         pmsa( ipnt( 486) ) = uLOut
         pmsa( ipnt( 487) ) = uLPAR0
         pmsa( ipnt( 488) ) = aExtPhyt
         pmsa( ipnt( 489) ) = aExtDet
         pmsa( ipnt( 490) ) = aExtIM
         pmsa( ipnt( 491) ) = aExtCoefOpen
         pmsa( ipnt( 492) ) = uQInSeason
         pmsa( ipnt( 493) ) = uQEvSinus
         pmsa( ipnt( 494) ) = uQEv
         pmsa( ipnt( 495) ) = uQInExtra
         pmsa( ipnt( 496) ) = uQOutExtra
         pmsa( ipnt( 497) ) = uQIn
         pmsa( ipnt( 498) ) = uQOut
         pmsa( ipnt( 499) ) = uQDil
         pmsa( ipnt( 500) ) = ukDil
         pmsa( ipnt( 501) ) = ukDilWat
         pmsa( ipnt( 502) ) = ukOut
         pmsa( ipnt( 503) ) = uTauWat
         pmsa( ipnt( 504) ) = uTauSubst
         pmsa( ipnt( 505) ) = vTranDepthW
         pmsa( ipnt( 506) ) = oDPhytW
         pmsa( ipnt( 507) ) = oPPhytW
         pmsa( ipnt( 508) ) = oNPhytW
         pmsa( ipnt( 509) ) = aDPhytS
         pmsa( ipnt( 510) ) = aPPhytS
         pmsa( ipnt( 511) ) = aNPhytS
         pmsa( ipnt( 512) ) = oDOMW
         pmsa( ipnt( 513) ) = oDSestW
         pmsa( ipnt( 514) ) = oPOMW
         pmsa( ipnt( 515) ) = oPSestW
         pmsa( ipnt( 516) ) = oPInorgW
         pmsa( ipnt( 517) ) = oPTotW
         pmsa( ipnt( 518) ) = oNDissW
         pmsa( ipnt( 519) ) = oNOMW
         pmsa( ipnt( 520) ) = oNSestW
         pmsa( ipnt( 521) ) = oNkjW
         pmsa( ipnt( 522) ) = oNTotW
         pmsa( ipnt( 523) ) = aDTotS
         pmsa( ipnt( 524) ) = aRhoTotS
         pmsa( ipnt( 525) ) = aRhoSolidS
         pmsa( ipnt( 526) ) = afDTotS
         pmsa( ipnt( 527) ) = afDOrgS
         pmsa( ipnt( 528) ) = afDetS
         pmsa( ipnt( 529) ) = afDetTotS
         pmsa( ipnt( 530) ) = aPInorgS
         pmsa( ipnt( 531) ) = aPTotAvailS
         pmsa( ipnt( 532) ) = aPTotS
         pmsa( ipnt( 533) ) = afPInorgS
         pmsa( ipnt( 534) ) = afPTotS
         pmsa( ipnt( 535) ) = afPO4S
         pmsa( ipnt( 536) ) = oPO4S
         pmsa( ipnt( 537) ) = aNDissS
         pmsa( ipnt( 538) ) = aNkjAvailS
         pmsa( ipnt( 539) ) = aNkjS
         pmsa( ipnt( 540) ) = aNTotAvailS
         pmsa( ipnt( 541) ) = aNTotS
         pmsa( ipnt( 542) ) = afNInorgS
         pmsa( ipnt( 543) ) = afNTotS
         pmsa( ipnt( 544) ) = oNO3S
         pmsa( ipnt( 545) ) = oNH4S
         pmsa( ipnt( 546) ) = oNDissS
         pmsa( ipnt( 547) ) = rPDIMW
         pmsa( ipnt( 548) ) = rPDIMS
         pmsa( ipnt( 549) ) = rPDDetW
         pmsa( ipnt( 550) ) = rNDDetW
         pmsa( ipnt( 551) ) = rPDHumS
         pmsa( ipnt( 552) ) = rNDHumS
         pmsa( ipnt( 553) ) = rPDDetS
         pmsa( ipnt( 554) ) = rNDDetS
         pmsa( ipnt( 555) ) = uPLoadSeason
         pmsa( ipnt( 556) ) = uPLoad
         pmsa( ipnt( 557) ) = uPLoadPO4
         pmsa( ipnt( 558) ) = uPLoadOrg
         pmsa( ipnt( 559) ) = uPLoadPhytTot
         pmsa( ipnt( 560) ) = uPLoadDet
         pmsa( ipnt( 561) ) = uPLoadAIM
         pmsa( ipnt( 562) ) = uNLoadSeason
         pmsa( ipnt( 563) ) = uNLoadPhytTot
         pmsa( ipnt( 564) ) = uNLoad
         pmsa( ipnt( 565) ) = uNLoadDet
         pmsa( ipnt( 566) ) = uNLoadOrg
         pmsa( ipnt( 567) ) = uNLoadDiss
         pmsa( ipnt( 568) ) = uNLoadNH4
         pmsa( ipnt( 569) ) = uNLoadNO3
         pmsa( ipnt( 570) ) = uNTotIn
         pmsa( ipnt( 571) ) = uDLoadDet
         pmsa( ipnt( 572) ) = uDLoadPhytTot
         pmsa( ipnt( 573) ) = uDLoadIM
         pmsa( ipnt( 574) ) = uDLoad
         pmsa( ipnt( 575) ) = uPTotIn
         pmsa( ipnt( 576) ) = wDDilIM
         pmsa( ipnt( 577) ) = wDDilDet
         pmsa( ipnt( 578) ) = wPDilPO4
         pmsa( ipnt( 579) ) = wPDilDet
         pmsa( ipnt( 580) ) = wPDilAIM
         pmsa( ipnt( 581) ) = wNDilNH4
         pmsa( ipnt( 582) ) = wNDilNO3
         pmsa( ipnt( 583) ) = wNDilDet
         pmsa( ipnt( 584) ) = wO2Inflow
         pmsa( ipnt( 585) ) = wO2Outfl
         pmsa( ipnt( 586) ) = wDDilPhyt
         pmsa( ipnt( 587) ) = wPDilPhyt
         pmsa( ipnt( 588) ) = wNDilPhyt
         pmsa( ipnt( 589) ) = wDOutflTot
         pmsa( ipnt( 590) ) = wPOutflTot
         pmsa( ipnt( 591) ) = wNOutflTot
         pmsa( ipnt( 592) ) = uDLoadPhyt
         pmsa( ipnt( 593) ) = uPLoadPhyt
         pmsa( ipnt( 594) ) = uNLoadPhyt
         pmsa( ipnt( 595) ) = wDTranPhyt
         pmsa( ipnt( 596) ) = wPTranPhyt
         pmsa( ipnt( 597) ) = wNTranPhyt
         pmsa( ipnt( 598) ) = wDTranIMW
         pmsa( ipnt( 599) ) = wDTranDetW
         pmsa( ipnt( 600) ) = wO2TranW
         pmsa( ipnt( 601) ) = wPTranPO4W
         pmsa( ipnt( 602) ) = wPTranAIMW
         pmsa( ipnt( 603) ) = wPTranDetW
         pmsa( ipnt( 604) ) = wNTranNH4W
         pmsa( ipnt( 605) ) = wNTranNO3W
         pmsa( ipnt( 606) ) = wNTranDetW
         pmsa( ipnt( 607) ) = wDDilTot
         pmsa( ipnt( 608) ) = wPDilTot
         pmsa( ipnt( 609) ) = wNDilTot
         pmsa( ipnt( 610) ) = tPInfPO4W
         pmsa( ipnt( 611) ) = tNInfNH4W
         pmsa( ipnt( 612) ) = tNInfNO3W
         pmsa( ipnt( 613) ) = tPInfPO4S
         pmsa( ipnt( 614) ) = tNInfNH4S
         pmsa( ipnt( 615) ) = tNInfNO3S
         pmsa( ipnt( 616) ) = tNH4LoadS
         pmsa( ipnt( 617) ) = tNO3LoadS
         pmsa( ipnt( 618) ) = uDErosIM
         pmsa( ipnt( 619) ) = uDErosIMS
         pmsa( ipnt( 620) ) = uDErosIMW
         pmsa( ipnt( 621) ) = uDErosOM
         pmsa( ipnt( 622) ) = uPErosOM
         pmsa( ipnt( 623) ) = uNErosOM
         pmsa( ipnt( 624) ) = uO2Sat
         pmsa( ipnt( 625) ) = kAer
         pmsa( ipnt( 626) ) = uFunTmAer
         pmsa( ipnt( 627) ) = aFunLemnAer
         pmsa( ipnt( 628) ) = tO2Aer
         pmsa( ipnt( 629) ) = tDTurbFish
         pmsa( ipnt( 630) ) = tDTurbFishIM
         pmsa( ipnt( 631) ) = aDVeg
         pmsa( ipnt( 632) ) = aFunVegResus
         pmsa( ipnt( 633) ) = aFunDimSusp
         pmsa( ipnt( 634) ) = tDResusTauDead
         pmsa( ipnt( 635) ) = tDResusBareDead
         pmsa( ipnt( 636) ) = tDResusDead
         pmsa( ipnt( 637) ) = tDResusIM
         pmsa( ipnt( 638) ) = tDResusDet
         pmsa( ipnt( 639) ) = akResusPhytRef
         pmsa( ipnt( 640) ) = tDResusPhytTot
         pmsa( ipnt( 641) ) = tPResusDet
         pmsa( ipnt( 642) ) = tPResusPO4
         pmsa( ipnt( 643) ) = tPResusAIM
         pmsa( ipnt( 644) ) = tNResusNO3
         pmsa( ipnt( 645) ) = tNResusNH4
         pmsa( ipnt( 646) ) = tNResusDet
         pmsa( ipnt( 647) ) = aFunTauSetOM
         pmsa( ipnt( 648) ) = aFunTauSetIM
         pmsa( ipnt( 649) ) = uFunTmSet
         pmsa( ipnt( 650) ) = uCorVSetIM
         pmsa( ipnt( 651) ) = tDSetIM
         pmsa( ipnt( 652) ) = tPSetAIM
         pmsa( ipnt( 653) ) = uCorVSetDet
         pmsa( ipnt( 654) ) = tDSetDet
         pmsa( ipnt( 655) ) = tPSetDet
         pmsa( ipnt( 656) ) = tNSetDet
         pmsa( ipnt( 657) ) = kPMinDetW
         pmsa( ipnt( 658) ) = kNMinDetW
         pmsa( ipnt( 659) ) = uFunTmMinW
         pmsa( ipnt( 660) ) = wDMinDetW
         pmsa( ipnt( 661) ) = wPMinDetW
         pmsa( ipnt( 662) ) = wNMinDetW
         pmsa( ipnt( 663) ) = aCorO2BOD
         pmsa( ipnt( 664) ) = wO2MinDetW
         pmsa( ipnt( 665) ) = wDDenitW
         pmsa( ipnt( 666) ) = wNDenitW
         pmsa( ipnt( 667) ) = uFunTmNitr
         pmsa( ipnt( 668) ) = aCorO2NitrW
         pmsa( ipnt( 669) ) = wNNitrW
         pmsa( ipnt( 670) ) = wO2NitrW
         pmsa( ipnt( 671) ) = kPMinDetS
         pmsa( ipnt( 672) ) = kNMinDetS
         pmsa( ipnt( 673) ) = uFunTmMinS
         pmsa( ipnt( 674) ) = tDMinDetS
         pmsa( ipnt( 675) ) = tPMinDetS
         pmsa( ipnt( 676) ) = tNMinDetS
         pmsa( ipnt( 677) ) = uFunTmDif
         pmsa( ipnt( 678) ) = akO2DifCor
         pmsa( ipnt( 679) ) = tSOD
         pmsa( ipnt( 680) ) = aDepthOxySed
         pmsa( ipnt( 681) ) = afOxySed
         pmsa( ipnt( 682) ) = tDMinOxyDetS
         pmsa( ipnt( 683) ) = tO2MinDetS
         pmsa( ipnt( 684) ) = tDDenitS
         pmsa( ipnt( 685) ) = tNDenitS
         pmsa( ipnt( 686) ) = tNNitrS
         pmsa( ipnt( 687) ) = tO2NitrS
         pmsa( ipnt( 688) ) = tDMinHumS
         pmsa( ipnt( 689) ) = tPMinHumS
         pmsa( ipnt( 690) ) = tNMinHumS
         pmsa( ipnt( 691) ) = aDepthDif
         pmsa( ipnt( 692) ) = tPDifPO4
         pmsa( ipnt( 693) ) = tNDifNO3
         pmsa( ipnt( 694) ) = tNDifNH4
         pmsa( ipnt( 695) ) = tO2Dif
         pmsa( ipnt( 696) ) = tPDifGroundPO4
         pmsa( ipnt( 697) ) = tNDifGroundNO3
         pmsa( ipnt( 698) ) = tNDifGroundNH4
         pmsa( ipnt( 699) ) = aPAdsMaxW
         pmsa( ipnt( 700) ) = aKPAdsW
         pmsa( ipnt( 701) ) = aPIsoAdsW
         pmsa( ipnt( 702) ) = aPEqIMW
         pmsa( ipnt( 703) ) = wPSorpIMW
         pmsa( ipnt( 704) ) = aPAdsMaxS
         pmsa( ipnt( 705) ) = aKPAdsS
         pmsa( ipnt( 706) ) = aPIsoAdsS
         pmsa( ipnt( 707) ) = aPEqIMS
         pmsa( ipnt( 708) ) = tPSorpIMS
         pmsa( ipnt( 709) ) = tPChemPO4
         pmsa( ipnt( 710) ) = wDAbioIMW
         pmsa( ipnt( 711) ) = wDAbioDetW
         pmsa( ipnt( 712) ) = tDAbioIMS
         pmsa( ipnt( 713) ) = tDAbioDetS
         pmsa( ipnt( 714) ) = tDAbioHumS
         pmsa( ipnt( 715) ) = tDAbioTotT
         pmsa( ipnt( 716) ) = wO2AbioW
         pmsa( ipnt( 717) ) = wPAbioDetW
         pmsa( ipnt( 718) ) = wPAbioPO4W
         pmsa( ipnt( 719) ) = wPAbioAIMW
         pmsa( ipnt( 720) ) = tPAbioDetS
         pmsa( ipnt( 721) ) = tPAbioHumS
         pmsa( ipnt( 722) ) = tPAbioPO4S
         pmsa( ipnt( 723) ) = tPAbioAIMS
         pmsa( ipnt( 724) ) = tPAbioTotT
         pmsa( ipnt( 725) ) = wNAbioNH4W
         pmsa( ipnt( 726) ) = wNAbioNO3W
         pmsa( ipnt( 727) ) = wNAbioDetW
         pmsa( ipnt( 728) ) = tNAbioNH4S
         pmsa( ipnt( 729) ) = tNAbioNO3S
         pmsa( ipnt( 730) ) = tNAbioDetS
         pmsa( ipnt( 731) ) = tNAbioHumS
         pmsa( ipnt( 732) ) = tNAbioTotT
         pmsa( ipnt( 733) ) = fManElod
         pmsa( ipnt( 734) ) = fManCera
         pmsa( ipnt( 735) ) = fManChar
         pmsa( ipnt( 736) ) = fManNymp
         pmsa( ipnt( 737) ) = kMigrLemn
         pmsa( ipnt( 738) ) = aDayInitElod
         pmsa( ipnt( 739) ) = bfRootElod
         pmsa( ipnt( 740) ) = bfShootElod
         pmsa( ipnt( 741) ) = aDRootElod
         pmsa( ipnt( 742) ) = aDShootElod
         pmsa( ipnt( 743) ) = aDEmergElod
         pmsa( ipnt( 744) ) = aDFloatElod
         pmsa( ipnt( 745) ) = bfSubElod
         pmsa( ipnt( 746) ) = aDSubElod
         pmsa( ipnt( 747) ) = aExtElod
         pmsa( ipnt( 748) ) = aDepth1Elod
         pmsa( ipnt( 749) ) = aDepth2Elod
         pmsa( ipnt( 750) ) = afCovSurfElod
         pmsa( ipnt( 751) ) = afCovEmergElod
         pmsa( ipnt( 752) ) = aCovElod
         pmsa( ipnt( 753) ) = aDayInitChar
         pmsa( ipnt( 754) ) = bfRootChar
         pmsa( ipnt( 755) ) = bfShootChar
         pmsa( ipnt( 756) ) = aDRootChar
         pmsa( ipnt( 757) ) = aDShootChar
         pmsa( ipnt( 758) ) = aDEmergChar
         pmsa( ipnt( 759) ) = aDFloatChar
         pmsa( ipnt( 760) ) = bfSubChar
         pmsa( ipnt( 761) ) = aDSubChar
         pmsa( ipnt( 762) ) = aExtChar
         pmsa( ipnt( 763) ) = aDepth1Char
         pmsa( ipnt( 764) ) = aDepth2Char
         pmsa( ipnt( 765) ) = afCovSurfChar
         pmsa( ipnt( 766) ) = afCovEmergChar
         pmsa( ipnt( 767) ) = aCovChar
         pmsa( ipnt( 768) ) = aDayInitCera
         pmsa( ipnt( 769) ) = bfRootCera
         pmsa( ipnt( 770) ) = bfShootCera
         pmsa( ipnt( 771) ) = aDRootCera
         pmsa( ipnt( 772) ) = aDShootCera
         pmsa( ipnt( 773) ) = aDEmergCera
         pmsa( ipnt( 774) ) = aDFloatCera
         pmsa( ipnt( 775) ) = bfSubCera
         pmsa( ipnt( 776) ) = aDSubCera
         pmsa( ipnt( 777) ) = aExtCera
         pmsa( ipnt( 778) ) = aDepth1Cera
         pmsa( ipnt( 779) ) = aDepth2Cera
         pmsa( ipnt( 780) ) = afCovSurfCera
         pmsa( ipnt( 781) ) = afCovEmergCera
         pmsa( ipnt( 782) ) = aCovCera
         pmsa( ipnt( 783) ) = aDayInitLemn
         pmsa( ipnt( 784) ) = bfRootLemn
         pmsa( ipnt( 785) ) = bfShootLemn
         pmsa( ipnt( 786) ) = aDRootLemn
         pmsa( ipnt( 787) ) = aDShootLemn
         pmsa( ipnt( 788) ) = aDEmergLemn
         pmsa( ipnt( 789) ) = aDFloatLemn
         pmsa( ipnt( 790) ) = bfSubLemn
         pmsa( ipnt( 791) ) = aDSubLemn
         pmsa( ipnt( 792) ) = aExtLemn
         pmsa( ipnt( 793) ) = aDepth1Lemn
         pmsa( ipnt( 794) ) = aDepth2Lemn
         pmsa( ipnt( 795) ) = afCovSurfLemn
         pmsa( ipnt( 796) ) = afCovEmergLemn
         pmsa( ipnt( 797) ) = aCovLemn
         pmsa( ipnt( 798) ) = aDayInitNymp
         pmsa( ipnt( 799) ) = bfRootNymp
         pmsa( ipnt( 800) ) = bfShootNymp
         pmsa( ipnt( 801) ) = aDRootNymp
         pmsa( ipnt( 802) ) = aDShootNymp
         pmsa( ipnt( 803) ) = aDEmergNymp
         pmsa( ipnt( 804) ) = aDFloatNymp
         pmsa( ipnt( 805) ) = bfSubNymp
         pmsa( ipnt( 806) ) = aDSubNymp
         pmsa( ipnt( 807) ) = aExtNymp
         pmsa( ipnt( 808) ) = aDepth1Nymp
         pmsa( ipnt( 809) ) = aDepth2Nymp
         pmsa( ipnt( 810) ) = afCovSurfNymp
         pmsa( ipnt( 811) ) = afCovEmergNymp
         pmsa( ipnt( 812) ) = aCovNymp
         pmsa( ipnt( 813) ) = aDayInitHelo
         pmsa( ipnt( 814) ) = bfRootHelo
         pmsa( ipnt( 815) ) = bfShootHelo
         pmsa( ipnt( 816) ) = aDRootHelo
         pmsa( ipnt( 817) ) = aDShootHelo
         pmsa( ipnt( 818) ) = aDEmergHelo
         pmsa( ipnt( 819) ) = aDFloatHelo
         pmsa( ipnt( 820) ) = bfSubHelo
         pmsa( ipnt( 821) ) = aDSubHelo
         pmsa( ipnt( 822) ) = aExtHelo
         pmsa( ipnt( 823) ) = aDepth1Helo
         pmsa( ipnt( 824) ) = aDepth2Helo
         pmsa( ipnt( 825) ) = afCovSurfHelo
         pmsa( ipnt( 826) ) = afCovEmergHelo
         pmsa( ipnt( 827) ) = aCovHelo
         pmsa( ipnt( 828) ) = aPVeg
         pmsa( ipnt( 829) ) = aNVeg
         pmsa( ipnt( 830) ) = afCovSurfVeg
         pmsa( ipnt( 831) ) = afCovEmergVeg
         pmsa( ipnt( 832) ) = aExtVeg
         pmsa( ipnt( 833) ) = aExtCoef
         pmsa( ipnt( 834) ) = aLPARBot
         pmsa( ipnt( 835) ) = rPDElod
         pmsa( ipnt( 836) ) = rNDElod
         pmsa( ipnt( 837) ) = tDMigrElod
         pmsa( ipnt( 838) ) = tPMigrElod
         pmsa( ipnt( 839) ) = tNMigrElod
         pmsa( ipnt( 840) ) = uFunTmProdElod
         pmsa( ipnt( 841) ) = uFunTmRespElod
         pmsa( ipnt( 842) ) = aVPUptMaxCrElod
         pmsa( ipnt( 843) ) = aVPUptElodW
         pmsa( ipnt( 844) ) = tPUptElodW
         pmsa( ipnt( 845) ) = aVPUptElodS
         pmsa( ipnt( 846) ) = tPUptElodS
         pmsa( ipnt( 847) ) = tPUptElod
         pmsa( ipnt( 848) ) = aVNUptMaxCrElod
         pmsa( ipnt( 849) ) = ahNUptElod
         pmsa( ipnt( 850) ) = aVNUptElodW
         pmsa( ipnt( 851) ) = tNUptElodW
         pmsa( ipnt( 852) ) = afNH4UptElodW
         pmsa( ipnt( 853) ) = tNUptNH4ElodW
         pmsa( ipnt( 854) ) = tNUptNO3ElodW
         pmsa( ipnt( 855) ) = aVNUptElodS
         pmsa( ipnt( 856) ) = tNUptElodS
         pmsa( ipnt( 857) ) = afNH4UptElodS
         pmsa( ipnt( 858) ) = tNUptNH4ElodS
         pmsa( ipnt( 859) ) = tNUptNO3ElodS
         pmsa( ipnt( 860) ) = tNUptElod
         pmsa( ipnt( 861) ) = aLPAR1Elod
         pmsa( ipnt( 862) ) = aLPAR2Elod
         pmsa( ipnt( 863) ) = uhLElod
         pmsa( ipnt( 864) ) = aLLimShootElod
         pmsa( ipnt( 865) ) = aMuTmLElod
         pmsa( ipnt( 866) ) = aPLimElod
         pmsa( ipnt( 867) ) = aNLimElod
         pmsa( ipnt( 868) ) = aNutLimElod
         pmsa( ipnt( 869) ) = aMuElod
         pmsa( ipnt( 870) ) = bkMortElod
         pmsa( ipnt( 871) ) = akDIncrElod
         pmsa( ipnt( 872) ) = tDEnvElod
         pmsa( ipnt( 873) ) = tDEnvProdElod
         pmsa( ipnt( 874) ) = tDProdElod
         pmsa( ipnt( 875) ) = tDProdSubElod
         pmsa( ipnt( 876) ) = tDRespElod
         pmsa( ipnt( 877) ) = tDEnvMortElod
         pmsa( ipnt( 878) ) = tDMortElod
         pmsa( ipnt( 879) ) = tDMortElodW
         pmsa( ipnt( 880) ) = tDMortElodS
         pmsa( ipnt( 881) ) = tDGrazElodBird
         pmsa( ipnt( 882) ) = bkManElod
         pmsa( ipnt( 883) ) = tDManElod
         pmsa( ipnt( 884) ) = tPManElod
         pmsa( ipnt( 885) ) = tNManElod
         pmsa( ipnt( 886) ) = tDBedElod
         pmsa( ipnt( 887) ) = tO2ProdElod
         pmsa( ipnt( 888) ) = tO2RespElodW
         pmsa( ipnt( 889) ) = tO2RespElodS
         pmsa( ipnt( 890) ) = tO2ProdElodS
         pmsa( ipnt( 891) ) = tO2ProdElodW
         pmsa( ipnt( 892) ) = tO2UptNO3ElodW
         pmsa( ipnt( 893) ) = tO2UptNO3ElodS
         pmsa( ipnt( 894) ) = tPExcrElod
         pmsa( ipnt( 895) ) = tPExcrElodS
         pmsa( ipnt( 896) ) = tPExcrElodW
         pmsa( ipnt( 897) ) = tPMortElod
         pmsa( ipnt( 898) ) = tPMortElodPO4
         pmsa( ipnt( 899) ) = tPMortElodPO4S
         pmsa( ipnt( 900) ) = tPMortElodPO4W
         pmsa( ipnt( 901) ) = tPMortElodDet
         pmsa( ipnt( 902) ) = tPMortElodDetW
         pmsa( ipnt( 903) ) = tPMortElodDetS
         pmsa( ipnt( 904) ) = tPGrazElodBird
         pmsa( ipnt( 905) ) = tPBedElod
         pmsa( ipnt( 906) ) = tNExcrElod
         pmsa( ipnt( 907) ) = tNExcrElodS
         pmsa( ipnt( 908) ) = tNExcrElodW
         pmsa( ipnt( 909) ) = tNMortElod
         pmsa( ipnt( 910) ) = tNMortElodNH4
         pmsa( ipnt( 911) ) = tNMortElodNH4S
         pmsa( ipnt( 912) ) = tNMortElodNH4W
         pmsa( ipnt( 913) ) = tNMortElodDet
         pmsa( ipnt( 914) ) = tNMortElodDetW
         pmsa( ipnt( 915) ) = tNMortElodDetS
         pmsa( ipnt( 916) ) = tNGrazElodBird
         pmsa( ipnt( 917) ) = tNBedElod
         pmsa( ipnt( 918) ) = rPDChar
         pmsa( ipnt( 919) ) = rNDChar
         pmsa( ipnt( 920) ) = tDMigrChar
         pmsa( ipnt( 921) ) = tPMigrChar
         pmsa( ipnt( 922) ) = tNMigrChar
         pmsa( ipnt( 923) ) = uFunTmProdChar
         pmsa( ipnt( 924) ) = uFunTmRespChar
         pmsa( ipnt( 925) ) = aVPUptMaxCrChar
         pmsa( ipnt( 926) ) = aVPUptCharW
         pmsa( ipnt( 927) ) = tPUptCharW
         pmsa( ipnt( 928) ) = aVPUptCharS
         pmsa( ipnt( 929) ) = tPUptCharS
         pmsa( ipnt( 930) ) = tPUptChar
         pmsa( ipnt( 931) ) = aVNUptMaxCrChar
         pmsa( ipnt( 932) ) = ahNUptChar
         pmsa( ipnt( 933) ) = aVNUptCharW
         pmsa( ipnt( 934) ) = tNUptCharW
         pmsa( ipnt( 935) ) = afNH4UptCharW
         pmsa( ipnt( 936) ) = tNUptNH4CharW
         pmsa( ipnt( 937) ) = tNUptNO3CharW
         pmsa( ipnt( 938) ) = aVNUptCharS
         pmsa( ipnt( 939) ) = tNUptCharS
         pmsa( ipnt( 940) ) = afNH4UptCharS
         pmsa( ipnt( 941) ) = tNUptNH4CharS
         pmsa( ipnt( 942) ) = tNUptNO3CharS
         pmsa( ipnt( 943) ) = tNUptChar
         pmsa( ipnt( 944) ) = aLPAR1Char
         pmsa( ipnt( 945) ) = aLPAR2Char
         pmsa( ipnt( 946) ) = uhLChar
         pmsa( ipnt( 947) ) = aLLimShootChar
         pmsa( ipnt( 948) ) = aMuTmLChar
         pmsa( ipnt( 949) ) = aPLimChar
         pmsa( ipnt( 950) ) = aNLimChar
         pmsa( ipnt( 951) ) = aNutLimChar
         pmsa( ipnt( 952) ) = aMuChar
         pmsa( ipnt( 953) ) = bkMortChar
         pmsa( ipnt( 954) ) = akDIncrChar
         pmsa( ipnt( 955) ) = tDEnvChar
         pmsa( ipnt( 956) ) = tDEnvProdChar
         pmsa( ipnt( 957) ) = tDProdChar
         pmsa( ipnt( 958) ) = tDProdSubChar
         pmsa( ipnt( 959) ) = tDRespChar
         pmsa( ipnt( 960) ) = tDEnvMortChar
         pmsa( ipnt( 961) ) = tDMortChar
         pmsa( ipnt( 962) ) = tDMortCharW
         pmsa( ipnt( 963) ) = tDMortCharS
         pmsa( ipnt( 964) ) = tDGrazCharBird
         pmsa( ipnt( 965) ) = bkManChar
         pmsa( ipnt( 966) ) = tDManChar
         pmsa( ipnt( 967) ) = tPManChar
         pmsa( ipnt( 968) ) = tNManChar
         pmsa( ipnt( 969) ) = tDBedChar
         pmsa( ipnt( 970) ) = tO2ProdChar
         pmsa( ipnt( 971) ) = tO2RespCharW
         pmsa( ipnt( 972) ) = tO2RespCharS
         pmsa( ipnt( 973) ) = tO2ProdCharS
         pmsa( ipnt( 974) ) = tO2ProdCharW
         pmsa( ipnt( 975) ) = tO2UptNO3CharW
         pmsa( ipnt( 976) ) = tO2UptNO3CharS
         pmsa( ipnt( 977) ) = tPExcrChar
         pmsa( ipnt( 978) ) = tPExcrCharS
         pmsa( ipnt( 979) ) = tPExcrCharW
         pmsa( ipnt( 980) ) = tPMortChar
         pmsa( ipnt( 981) ) = tPMortCharPO4
         pmsa( ipnt( 982) ) = tPMortCharPO4S
         pmsa( ipnt( 983) ) = tPMortCharPO4W
         pmsa( ipnt( 984) ) = tPMortCharDet
         pmsa( ipnt( 985) ) = tPMortCharDetW
         pmsa( ipnt( 986) ) = tPMortCharDetS
         pmsa( ipnt( 987) ) = tPGrazCharBird
         pmsa( ipnt( 988) ) = tPBedChar
         pmsa( ipnt( 989) ) = tNExcrChar
         pmsa( ipnt( 990) ) = tNExcrCharS
         pmsa( ipnt( 991) ) = tNExcrCharW
         pmsa( ipnt( 992) ) = tNMortChar
         pmsa( ipnt( 993) ) = tNMortCharNH4
         pmsa( ipnt( 994) ) = tNMortCharNH4S
         pmsa( ipnt( 995) ) = tNMortCharNH4W
         pmsa( ipnt( 996) ) = tNMortCharDet
         pmsa( ipnt( 997) ) = tNMortCharDetW
         pmsa( ipnt( 998) ) = tNMortCharDetS
         pmsa( ipnt( 999) ) = tNGrazCharBird
         pmsa( ipnt( 1000) ) = tNBedChar
         pmsa( ipnt( 1001) ) = rPDCera
         pmsa( ipnt( 1002) ) = rNDCera
         pmsa( ipnt( 1003) ) = tDMigrCera
         pmsa( ipnt( 1004) ) = tPMigrCera
         pmsa( ipnt( 1005) ) = tNMigrCera
         pmsa( ipnt( 1006) ) = uFunTmProdCera
         pmsa( ipnt( 1007) ) = uFunTmRespCera
         pmsa( ipnt( 1008) ) = aVPUptMaxCrCera
         pmsa( ipnt( 1009) ) = aVPUptCeraW
         pmsa( ipnt( 1010) ) = tPUptCeraW
         pmsa( ipnt( 1011) ) = aVPUptCeraS
         pmsa( ipnt( 1012) ) = tPUptCeraS
         pmsa( ipnt( 1013) ) = tPUptCera
         pmsa( ipnt( 1014) ) = aVNUptMaxCrCera
         pmsa( ipnt( 1015) ) = ahNUptCera
         pmsa( ipnt( 1016) ) = aVNUptCeraW
         pmsa( ipnt( 1017) ) = tNUptCeraW
         pmsa( ipnt( 1018) ) = afNH4UptCeraW
         pmsa( ipnt( 1019) ) = tNUptNH4CeraW
         pmsa( ipnt( 1020) ) = tNUptNO3CeraW
         pmsa( ipnt( 1021) ) = aVNUptCeraS
         pmsa( ipnt( 1022) ) = tNUptCeraS
         pmsa( ipnt( 1023) ) = afNH4UptCeraS
         pmsa( ipnt( 1024) ) = tNUptNH4CeraS
         pmsa( ipnt( 1025) ) = tNUptNO3CeraS
         pmsa( ipnt( 1026) ) = tNUptCera
         pmsa( ipnt( 1027) ) = aLPAR1Cera
         pmsa( ipnt( 1028) ) = aLPAR2Cera
         pmsa( ipnt( 1029) ) = uhLCera
         pmsa( ipnt( 1030) ) = aLLimShootCera
         pmsa( ipnt( 1031) ) = aMuTmLCera
         pmsa( ipnt( 1032) ) = aPLimCera
         pmsa( ipnt( 1033) ) = aNLimCera
         pmsa( ipnt( 1034) ) = aNutLimCera
         pmsa( ipnt( 1035) ) = aMuCera
         pmsa( ipnt( 1036) ) = bkMortCera
         pmsa( ipnt( 1037) ) = akDIncrCera
         pmsa( ipnt( 1038) ) = tDEnvCera
         pmsa( ipnt( 1039) ) = tDEnvProdCera
         pmsa( ipnt( 1040) ) = tDProdCera
         pmsa( ipnt( 1041) ) = tDProdSubCera
         pmsa( ipnt( 1042) ) = tDRespCera
         pmsa( ipnt( 1043) ) = tDEnvMortCera
         pmsa( ipnt( 1044) ) = tDMortCera
         pmsa( ipnt( 1045) ) = tDMortCeraW
         pmsa( ipnt( 1046) ) = tDMortCeraS
         pmsa( ipnt( 1047) ) = tDGrazCeraBird
         pmsa( ipnt( 1048) ) = bkManCera
         pmsa( ipnt( 1049) ) = tDManCera
         pmsa( ipnt( 1050) ) = tPManCera
         pmsa( ipnt( 1051) ) = tNManCera
         pmsa( ipnt( 1052) ) = tDBedCera
         pmsa( ipnt( 1053) ) = tO2ProdCera
         pmsa( ipnt( 1054) ) = tO2RespCeraW
         pmsa( ipnt( 1055) ) = tO2RespCeraS
         pmsa( ipnt( 1056) ) = tO2ProdCeraS
         pmsa( ipnt( 1057) ) = tO2ProdCeraW
         pmsa( ipnt( 1058) ) = tO2UptNO3CeraW
         pmsa( ipnt( 1059) ) = tO2UptNO3CeraS
         pmsa( ipnt( 1060) ) = tPExcrCera
         pmsa( ipnt( 1061) ) = tPExcrCeraS
         pmsa( ipnt( 1062) ) = tPExcrCeraW
         pmsa( ipnt( 1063) ) = tPMortCera
         pmsa( ipnt( 1064) ) = tPMortCeraPO4
         pmsa( ipnt( 1065) ) = tPMortCeraPO4S
         pmsa( ipnt( 1066) ) = tPMortCeraPO4W
         pmsa( ipnt( 1067) ) = tPMortCeraDet
         pmsa( ipnt( 1068) ) = tPMortCeraDetW
         pmsa( ipnt( 1069) ) = tPMortCeraDetS
         pmsa( ipnt( 1070) ) = tPGrazCeraBird
         pmsa( ipnt( 1071) ) = tPBedCera
         pmsa( ipnt( 1072) ) = tNExcrCera
         pmsa( ipnt( 1073) ) = tNExcrCeraS
         pmsa( ipnt( 1074) ) = tNExcrCeraW
         pmsa( ipnt( 1075) ) = tNMortCera
         pmsa( ipnt( 1076) ) = tNMortCeraNH4
         pmsa( ipnt( 1077) ) = tNMortCeraNH4S
         pmsa( ipnt( 1078) ) = tNMortCeraNH4W
         pmsa( ipnt( 1079) ) = tNMortCeraDet
         pmsa( ipnt( 1080) ) = tNMortCeraDetW
         pmsa( ipnt( 1081) ) = tNMortCeraDetS
         pmsa( ipnt( 1082) ) = tNGrazCeraBird
         pmsa( ipnt( 1083) ) = tNBedCera
         pmsa( ipnt( 1084) ) = rPDLemn
         pmsa( ipnt( 1085) ) = rNDLemn
         pmsa( ipnt( 1086) ) = tDMigrLemn
         pmsa( ipnt( 1087) ) = tPMigrLemn
         pmsa( ipnt( 1088) ) = tNMigrLemn
         pmsa( ipnt( 1089) ) = uFunTmProdLemn
         pmsa( ipnt( 1090) ) = uFunTmRespLemn
         pmsa( ipnt( 1091) ) = aVPUptMaxCrLemn
         pmsa( ipnt( 1092) ) = aVPUptLemnW
         pmsa( ipnt( 1093) ) = tPUptLemnW
         pmsa( ipnt( 1094) ) = aVPUptLemnS
         pmsa( ipnt( 1095) ) = tPUptLemnS
         pmsa( ipnt( 1096) ) = tPUptLemn
         pmsa( ipnt( 1097) ) = aVNUptMaxCrLemn
         pmsa( ipnt( 1098) ) = ahNUptLemn
         pmsa( ipnt( 1099) ) = aVNUptLemnW
         pmsa( ipnt( 1100) ) = tNUptLemnW
         pmsa( ipnt( 1101) ) = afNH4UptLemnW
         pmsa( ipnt( 1102) ) = tNUptNH4LemnW
         pmsa( ipnt( 1103) ) = tNUptNO3LemnW
         pmsa( ipnt( 1104) ) = aVNUptLemnS
         pmsa( ipnt( 1105) ) = tNUptLemnS
         pmsa( ipnt( 1106) ) = afNH4UptLemnS
         pmsa( ipnt( 1107) ) = tNUptNH4LemnS
         pmsa( ipnt( 1108) ) = tNUptNO3LemnS
         pmsa( ipnt( 1109) ) = tNUptLemn
         pmsa( ipnt( 1110) ) = aLPAR1Lemn
         pmsa( ipnt( 1111) ) = aLPAR2Lemn
         pmsa( ipnt( 1112) ) = uhLLemn
         pmsa( ipnt( 1113) ) = aLLimShootLemn
         pmsa( ipnt( 1114) ) = aMuTmLLemn
         pmsa( ipnt( 1115) ) = aPLimLemn
         pmsa( ipnt( 1116) ) = aNLimLemn
         pmsa( ipnt( 1117) ) = aNutLimLemn
         pmsa( ipnt( 1118) ) = aMuLemn
         pmsa( ipnt( 1119) ) = bkMortLemn
         pmsa( ipnt( 1120) ) = akDIncrLemn
         pmsa( ipnt( 1121) ) = tDEnvLemn
         pmsa( ipnt( 1122) ) = tDEnvProdLemn
         pmsa( ipnt( 1123) ) = tDProdLemn
         pmsa( ipnt( 1124) ) = tDProdSubLemn
         pmsa( ipnt( 1125) ) = tDRespLemn
         pmsa( ipnt( 1126) ) = tDEnvMortLemn
         pmsa( ipnt( 1127) ) = tDMortLemn
         pmsa( ipnt( 1128) ) = tDMortLemnW
         pmsa( ipnt( 1129) ) = tDMortLemnS
         pmsa( ipnt( 1130) ) = tDGrazLemnBird
         pmsa( ipnt( 1131) ) = bkManLemn
         pmsa( ipnt( 1132) ) = tDManLemn
         pmsa( ipnt( 1133) ) = tPManLemn
         pmsa( ipnt( 1134) ) = tNManLemn
         pmsa( ipnt( 1135) ) = tDBedLemn
         pmsa( ipnt( 1136) ) = tO2ProdLemn
         pmsa( ipnt( 1137) ) = tO2RespLemnW
         pmsa( ipnt( 1138) ) = tO2RespLemnS
         pmsa( ipnt( 1139) ) = tO2ProdLemnS
         pmsa( ipnt( 1140) ) = tO2ProdLemnW
         pmsa( ipnt( 1141) ) = tO2UptNO3LemnW
         pmsa( ipnt( 1142) ) = tO2UptNO3LemnS
         pmsa( ipnt( 1143) ) = tPExcrLemn
         pmsa( ipnt( 1144) ) = tPExcrLemnS
         pmsa( ipnt( 1145) ) = tPExcrLemnW
         pmsa( ipnt( 1146) ) = tPMortLemn
         pmsa( ipnt( 1147) ) = tPMortLemnPO4
         pmsa( ipnt( 1148) ) = tPMortLemnPO4S
         pmsa( ipnt( 1149) ) = tPMortLemnPO4W
         pmsa( ipnt( 1150) ) = tPMortLemnDet
         pmsa( ipnt( 1151) ) = tPMortLemnDetW
         pmsa( ipnt( 1152) ) = tPMortLemnDetS
         pmsa( ipnt( 1153) ) = tPGrazLemnBird
         pmsa( ipnt( 1154) ) = tPBedLemn
         pmsa( ipnt( 1155) ) = tNExcrLemn
         pmsa( ipnt( 1156) ) = tNExcrLemnS
         pmsa( ipnt( 1157) ) = tNExcrLemnW
         pmsa( ipnt( 1158) ) = tNMortLemn
         pmsa( ipnt( 1159) ) = tNMortLemnNH4
         pmsa( ipnt( 1160) ) = tNMortLemnNH4S
         pmsa( ipnt( 1161) ) = tNMortLemnNH4W
         pmsa( ipnt( 1162) ) = tNMortLemnDet
         pmsa( ipnt( 1163) ) = tNMortLemnDetW
         pmsa( ipnt( 1164) ) = tNMortLemnDetS
         pmsa( ipnt( 1165) ) = tNGrazLemnBird
         pmsa( ipnt( 1166) ) = tNBedLemn
         pmsa( ipnt( 1167) ) = rPDNymp
         pmsa( ipnt( 1168) ) = rNDNymp
         pmsa( ipnt( 1169) ) = tDMigrNymp
         pmsa( ipnt( 1170) ) = tPMigrNymp
         pmsa( ipnt( 1171) ) = tNMigrNymp
         pmsa( ipnt( 1172) ) = uFunTmProdNymp
         pmsa( ipnt( 1173) ) = uFunTmRespNymp
         pmsa( ipnt( 1174) ) = aVPUptMaxCrNymp
         pmsa( ipnt( 1175) ) = aVPUptNympW
         pmsa( ipnt( 1176) ) = tPUptNympW
         pmsa( ipnt( 1177) ) = aVPUptNympS
         pmsa( ipnt( 1178) ) = tPUptNympS
         pmsa( ipnt( 1179) ) = tPUptNymp
         pmsa( ipnt( 1180) ) = aVNUptMaxCrNymp
         pmsa( ipnt( 1181) ) = ahNUptNymp
         pmsa( ipnt( 1182) ) = aVNUptNympW
         pmsa( ipnt( 1183) ) = tNUptNympW
         pmsa( ipnt( 1184) ) = afNH4UptNympW
         pmsa( ipnt( 1185) ) = tNUptNH4NympW
         pmsa( ipnt( 1186) ) = tNUptNO3NympW
         pmsa( ipnt( 1187) ) = aVNUptNympS
         pmsa( ipnt( 1188) ) = tNUptNympS
         pmsa( ipnt( 1189) ) = afNH4UptNympS
         pmsa( ipnt( 1190) ) = tNUptNH4NympS
         pmsa( ipnt( 1191) ) = tNUptNO3NympS
         pmsa( ipnt( 1192) ) = tNUptNymp
         pmsa( ipnt( 1193) ) = aLPAR1Nymp
         pmsa( ipnt( 1194) ) = aLPAR2Nymp
         pmsa( ipnt( 1195) ) = uhLNymp
         pmsa( ipnt( 1196) ) = aLLimShootNymp
         pmsa( ipnt( 1197) ) = aMuTmLNymp
         pmsa( ipnt( 1198) ) = aPLimNymp
         pmsa( ipnt( 1199) ) = aNLimNymp
         pmsa( ipnt( 1200) ) = aNutLimNymp
         pmsa( ipnt( 1201) ) = aMuNymp
         pmsa( ipnt( 1202) ) = bkMortNymp
         pmsa( ipnt( 1203) ) = akDIncrNymp
         pmsa( ipnt( 1204) ) = tDEnvNymp
         pmsa( ipnt( 1205) ) = tDEnvProdNymp
         pmsa( ipnt( 1206) ) = tDProdNymp
         pmsa( ipnt( 1207) ) = tDProdSubNymp
         pmsa( ipnt( 1208) ) = tDRespNymp
         pmsa( ipnt( 1209) ) = tDEnvMortNymp
         pmsa( ipnt( 1210) ) = tDMortNymp
         pmsa( ipnt( 1211) ) = tDMortNympW
         pmsa( ipnt( 1212) ) = tDMortNympS
         pmsa( ipnt( 1213) ) = tDGrazNympBird
         pmsa( ipnt( 1214) ) = bkManNymp
         pmsa( ipnt( 1215) ) = tDManNymp
         pmsa( ipnt( 1216) ) = tPManNymp
         pmsa( ipnt( 1217) ) = tNManNymp
         pmsa( ipnt( 1218) ) = tDBedNymp
         pmsa( ipnt( 1219) ) = tO2ProdNymp
         pmsa( ipnt( 1220) ) = tO2RespNympW
         pmsa( ipnt( 1221) ) = tO2RespNympS
         pmsa( ipnt( 1222) ) = tO2ProdNympS
         pmsa( ipnt( 1223) ) = tO2ProdNympW
         pmsa( ipnt( 1224) ) = tO2UptNO3NympW
         pmsa( ipnt( 1225) ) = tO2UptNO3NympS
         pmsa( ipnt( 1226) ) = tPExcrNymp
         pmsa( ipnt( 1227) ) = tPExcrNympS
         pmsa( ipnt( 1228) ) = tPExcrNympW
         pmsa( ipnt( 1229) ) = tPMortNymp
         pmsa( ipnt( 1230) ) = tPMortNympPO4
         pmsa( ipnt( 1231) ) = tPMortNympPO4S
         pmsa( ipnt( 1232) ) = tPMortNympPO4W
         pmsa( ipnt( 1233) ) = tPMortNympDet
         pmsa( ipnt( 1234) ) = tPMortNympDetW
         pmsa( ipnt( 1235) ) = tPMortNympDetS
         pmsa( ipnt( 1236) ) = tPGrazNympBird
         pmsa( ipnt( 1237) ) = tPBedNymp
         pmsa( ipnt( 1238) ) = tNExcrNymp
         pmsa( ipnt( 1239) ) = tNExcrNympS
         pmsa( ipnt( 1240) ) = tNExcrNympW
         pmsa( ipnt( 1241) ) = tNMortNymp
         pmsa( ipnt( 1242) ) = tNMortNympNH4
         pmsa( ipnt( 1243) ) = tNMortNympNH4S
         pmsa( ipnt( 1244) ) = tNMortNympNH4W
         pmsa( ipnt( 1245) ) = tNMortNympDet
         pmsa( ipnt( 1246) ) = tNMortNympDetW
         pmsa( ipnt( 1247) ) = tNMortNympDetS
         pmsa( ipnt( 1248) ) = tNGrazNympBird
         pmsa( ipnt( 1249) ) = tNBedNymp
         pmsa( ipnt( 1250) ) = rPDHelo
         pmsa( ipnt( 1251) ) = rNDHelo
         pmsa( ipnt( 1252) ) = tDMigrHelo
         pmsa( ipnt( 1253) ) = tPMigrHelo
         pmsa( ipnt( 1254) ) = tNMigrHelo
         pmsa( ipnt( 1255) ) = uFunTmProdHelo
         pmsa( ipnt( 1256) ) = uFunTmRespHelo
         pmsa( ipnt( 1257) ) = aVPUptMaxCrHelo
         pmsa( ipnt( 1258) ) = aVPUptHeloW
         pmsa( ipnt( 1259) ) = tPUptHeloW
         pmsa( ipnt( 1260) ) = aVPUptHeloS
         pmsa( ipnt( 1261) ) = tPUptHeloS
         pmsa( ipnt( 1262) ) = tPUptHelo
         pmsa( ipnt( 1263) ) = aVNUptMaxCrHelo
         pmsa( ipnt( 1264) ) = ahNUptHelo
         pmsa( ipnt( 1265) ) = aVNUptHeloW
         pmsa( ipnt( 1266) ) = tNUptHeloW
         pmsa( ipnt( 1267) ) = afNH4UptHeloW
         pmsa( ipnt( 1268) ) = tNUptNH4HeloW
         pmsa( ipnt( 1269) ) = tNUptNO3HeloW
         pmsa( ipnt( 1270) ) = aVNUptHeloS
         pmsa( ipnt( 1271) ) = tNUptHeloS
         pmsa( ipnt( 1272) ) = afNH4UptHeloS
         pmsa( ipnt( 1273) ) = tNUptNH4HeloS
         pmsa( ipnt( 1274) ) = tNUptNO3HeloS
         pmsa( ipnt( 1275) ) = tNUptHelo
         pmsa( ipnt( 1276) ) = aLPAR1Helo
         pmsa( ipnt( 1277) ) = aLPAR2Helo
         pmsa( ipnt( 1278) ) = uhLHelo
         pmsa( ipnt( 1279) ) = aLLimShootHelo
         pmsa( ipnt( 1280) ) = aMuTmLHelo
         pmsa( ipnt( 1281) ) = aPLimHelo
         pmsa( ipnt( 1282) ) = aNLimHelo
         pmsa( ipnt( 1283) ) = aNutLimHelo
         pmsa( ipnt( 1284) ) = aMuHelo
         pmsa( ipnt( 1285) ) = bkMortHelo
         pmsa( ipnt( 1286) ) = akDIncrHelo
         pmsa( ipnt( 1287) ) = tDEnvHelo
         pmsa( ipnt( 1288) ) = tDEnvProdHelo
         pmsa( ipnt( 1289) ) = tDProdHelo
         pmsa( ipnt( 1290) ) = tDProdSubHelo
         pmsa( ipnt( 1291) ) = tDRespHelo
         pmsa( ipnt( 1292) ) = tDEnvMortHelo
         pmsa( ipnt( 1293) ) = tDMortHelo
         pmsa( ipnt( 1294) ) = tDMortHeloW
         pmsa( ipnt( 1295) ) = tDMortHeloS
         pmsa( ipnt( 1296) ) = tDGrazHeloBird
         pmsa( ipnt( 1297) ) = bkManHelo
         pmsa( ipnt( 1298) ) = tDManHelo
         pmsa( ipnt( 1299) ) = tPManHelo
         pmsa( ipnt( 1300) ) = tNManHelo
         pmsa( ipnt( 1301) ) = tDBedHelo
         pmsa( ipnt( 1302) ) = tO2ProdHelo
         pmsa( ipnt( 1303) ) = tO2RespHeloW
         pmsa( ipnt( 1304) ) = tO2RespHeloS
         pmsa( ipnt( 1305) ) = tO2ProdHeloS
         pmsa( ipnt( 1306) ) = tO2ProdHeloW
         pmsa( ipnt( 1307) ) = tO2UptNO3HeloW
         pmsa( ipnt( 1308) ) = tO2UptNO3HeloS
         pmsa( ipnt( 1309) ) = tPExcrHelo
         pmsa( ipnt( 1310) ) = tPExcrHeloS
         pmsa( ipnt( 1311) ) = tPExcrHeloW
         pmsa( ipnt( 1312) ) = tPMortHelo
         pmsa( ipnt( 1313) ) = tPMortHeloPO4
         pmsa( ipnt( 1314) ) = tPMortHeloPO4S
         pmsa( ipnt( 1315) ) = tPMortHeloPO4W
         pmsa( ipnt( 1316) ) = tPMortHeloDet
         pmsa( ipnt( 1317) ) = tPMortHeloDetW
         pmsa( ipnt( 1318) ) = tPMortHeloDetS
         pmsa( ipnt( 1319) ) = tPGrazHeloBird
         pmsa( ipnt( 1320) ) = tPBedHelo
         pmsa( ipnt( 1321) ) = tNExcrHelo
         pmsa( ipnt( 1322) ) = tNExcrHeloS
         pmsa( ipnt( 1323) ) = tNExcrHeloW
         pmsa( ipnt( 1324) ) = tNMortHelo
         pmsa( ipnt( 1325) ) = tNMortHeloNH4
         pmsa( ipnt( 1326) ) = tNMortHeloNH4S
         pmsa( ipnt( 1327) ) = tNMortHeloNH4W
         pmsa( ipnt( 1328) ) = tNMortHeloDet
         pmsa( ipnt( 1329) ) = tNMortHeloDetW
         pmsa( ipnt( 1330) ) = tNMortHeloDetS
         pmsa( ipnt( 1331) ) = tNGrazHeloBird
         pmsa( ipnt( 1332) ) = tNBedHelo
         pmsa( ipnt( 1333) ) = tDMigrVeg
         pmsa( ipnt( 1334) ) = tPMigrVeg
         pmsa( ipnt( 1335) ) = tNMigrVeg
         pmsa( ipnt( 1336) ) = tDProdVeg
         pmsa( ipnt( 1337) ) = tPUptVegW
         pmsa( ipnt( 1338) ) = tPUptVegS
         pmsa( ipnt( 1339) ) = tNUptNH4VegW
         pmsa( ipnt( 1340) ) = tNUptNH4VegS
         pmsa( ipnt( 1341) ) = tNUptNO3VegW
         pmsa( ipnt( 1342) ) = tNUptNO3VegS
         pmsa( ipnt( 1343) ) = tDRespVeg
         pmsa( ipnt( 1344) ) = tPExcrVegW
         pmsa( ipnt( 1345) ) = tPExcrVegS
         pmsa( ipnt( 1346) ) = tNExcrVegW
         pmsa( ipnt( 1347) ) = tNExcrVegS
         pmsa( ipnt( 1348) ) = tO2ProdVeg
         pmsa( ipnt( 1349) ) = tO2ProdVegW
         pmsa( ipnt( 1350) ) = tO2ProdVegS
         pmsa( ipnt( 1351) ) = tO2RespVegW
         pmsa( ipnt( 1352) ) = tO2RespVegS
         pmsa( ipnt( 1353) ) = tO2UptNO3VegW
         pmsa( ipnt( 1354) ) = tO2UptNO3VegS
         pmsa( ipnt( 1355) ) = tDMortVegW
         pmsa( ipnt( 1356) ) = tDMortVegS
         pmsa( ipnt( 1357) ) = tPMortVegPO4W
         pmsa( ipnt( 1358) ) = tPMortVegDetW
         pmsa( ipnt( 1359) ) = tPMortVegPO4S
         pmsa( ipnt( 1360) ) = tPMortVegDetS
         pmsa( ipnt( 1361) ) = tNMortVegNH4W
         pmsa( ipnt( 1362) ) = tNMortVegDetW
         pmsa( ipnt( 1363) ) = tNMortVegNH4S
         pmsa( ipnt( 1364) ) = tNMortVegDetS
         pmsa( ipnt( 1365) ) = tDGrazVegBird
         pmsa( ipnt( 1366) ) = tPGrazVegBird
         pmsa( ipnt( 1367) ) = tNGrazVegBird
         pmsa( ipnt( 1368) ) = tDManVeg
         pmsa( ipnt( 1369) ) = tPManVeg
         pmsa( ipnt( 1370) ) = tNManVeg
         pmsa( ipnt( 1371) ) = aCovSub
         pmsa( ipnt( 1372) ) = tDAssVegBird
         pmsa( ipnt( 1373) ) = tDEgesBird
         pmsa( ipnt( 1374) ) = tPAssVegBird
         pmsa( ipnt( 1375) ) = tPEgesBird
         pmsa( ipnt( 1376) ) = tPEgesBirdPO4
         pmsa( ipnt( 1377) ) = tPEgesBirdDet
         pmsa( ipnt( 1378) ) = tNAssVegBird
         pmsa( ipnt( 1379) ) = tNEgesBird
         pmsa( ipnt( 1380) ) = tNEgesBirdNH4
         pmsa( ipnt( 1381) ) = tNEgesBirdDet
         pmsa( ipnt( 1382) ) = wDBedDetW
         pmsa( ipnt( 1383) ) = tDBedDetS
         pmsa( ipnt( 1384) ) = tDBedTotT
         pmsa( ipnt( 1385) ) = wPBedPO4W
         pmsa( ipnt( 1386) ) = wPBedDetW
         pmsa( ipnt( 1387) ) = tPBedPO4S
         pmsa( ipnt( 1388) ) = tPBedDetS
         pmsa( ipnt( 1389) ) = tPBedTotT
         pmsa( ipnt( 1390) ) = wNBedNH4W
         pmsa( ipnt( 1391) ) = wNBedNO3W
         pmsa( ipnt( 1392) ) = wNBedDetW
         pmsa( ipnt( 1393) ) = tNBedNH4S
         pmsa( ipnt( 1394) ) = tNBedNO3S
         pmsa( ipnt( 1395) ) = tNBedDetS
         pmsa( ipnt( 1396) ) = tNBedTotT
         pmsa( ipnt( 1397) ) = tO2BedW
         pmsa( ipnt( 1398) ) = tO2BedS
         pmsa( ipnt( 1399) ) = UseLoss
         pmsa( ipnt( 1400) ) = uFunTmLoss
         pmsa( ipnt( 1401) ) = rPDPhytW
         pmsa( ipnt( 1402) ) = rNDPhytW
         pmsa( ipnt( 1403) ) = rPDPhytS
         pmsa( ipnt( 1404) ) = rNDPhytS
         pmsa( ipnt( 1405) ) = uFunTmPhyt
         pmsa( ipnt( 1406) ) = uFunTmProdPhyt
         pmsa( ipnt( 1407) ) = uFunTmRespPhyt
         pmsa( ipnt( 1408) ) = aVPUptMaxCrPhyt
         pmsa( ipnt( 1409) ) = aVPUptPhyt
         pmsa( ipnt( 1410) ) = wPUptPhyt
         pmsa( ipnt( 1411) ) = aVNUptMaxCrPhyt
         pmsa( ipnt( 1412) ) = ahNUptPhyt
         pmsa( ipnt( 1413) ) = aVNUptPhyt
         pmsa( ipnt( 1414) ) = wNUptPhyt
         pmsa( ipnt( 1415) ) = afNH4UptPhyt
         pmsa( ipnt( 1416) ) = wNUptNH4Phyt
         pmsa( ipnt( 1417) ) = wNUptNO3Phyt
         pmsa( ipnt( 1418) ) = uMuMaxTmPhyt
         pmsa( ipnt( 1419) ) = aPLimPhyt
         pmsa( ipnt( 1420) ) = aNLimPhyt
         pmsa( ipnt( 1421) ) = aLLimPhyt
         pmsa( ipnt( 1422) ) = aMuTmLPhyt
         pmsa( ipnt( 1423) ) = aNutLimPhyt
         pmsa( ipnt( 1424) ) = aMuPhyt
         pmsa( ipnt( 1425) ) = wDAssPhyt
         pmsa( ipnt( 1426) ) = rChDPhyt
         pmsa( ipnt( 1427) ) = oChlaPhyt
         pmsa( ipnt( 1428) ) = aExtChPhyt
         pmsa( ipnt( 1429) ) = ukDRespTmPhyt
         pmsa( ipnt( 1430) ) = wDRespPhytW
         pmsa( ipnt( 1431) ) = ukLossTmPhyt
         pmsa( ipnt( 1432) ) = wDLossPhyt
         pmsa( ipnt( 1433) ) = wDMortPhytW
         pmsa( ipnt( 1434) ) = uCorVSetPhyt
         pmsa( ipnt( 1435) ) = tDSetPhyt
         pmsa( ipnt( 1436) ) = tDResusPhyt
         pmsa( ipnt( 1437) ) = tDRespPhytS
         pmsa( ipnt( 1438) ) = tDMortPhytS
         pmsa( ipnt( 1439) ) = ukDDecPhyt
         pmsa( ipnt( 1440) ) = wPExcrPhytW
         pmsa( ipnt( 1441) ) = wPLossPhyt
         pmsa( ipnt( 1442) ) = wPMortPhytW
         pmsa( ipnt( 1443) ) = tPSetPhyt
         pmsa( ipnt( 1444) ) = tPResusPhyt
         pmsa( ipnt( 1445) ) = tPExcrPhytS
         pmsa( ipnt( 1446) ) = tPMortPhytS
         pmsa( ipnt( 1447) ) = wNExcrPhytW
         pmsa( ipnt( 1448) ) = wNLossPhyt
         pmsa( ipnt( 1449) ) = wNMortPhytW
         pmsa( ipnt( 1450) ) = tNSetPhyt
         pmsa( ipnt( 1451) ) = tNResusPhyt
         pmsa( ipnt( 1452) ) = tNExcrPhytS
         pmsa( ipnt( 1453) ) = tNMortPhytS
         pmsa( ipnt( 1454) ) = wDPrimPhytW
         pmsa( ipnt( 1455) ) = wPPrimPhytW
         pmsa( ipnt( 1456) ) = wNPrimPhytW
         pmsa( ipnt( 1457) ) = tDPrimPhytS
         pmsa( ipnt( 1458) ) = tPPrimPhytS
         pmsa( ipnt( 1459) ) = tNPrimPhytS
         pmsa( ipnt( 1460) ) = oChla
         pmsa( ipnt( 1461) ) = wDPrimDetW
         pmsa( ipnt( 1462) ) = tDPrimDetS
         pmsa( ipnt( 1463) ) = tDPrimTotT
         pmsa( ipnt( 1464) ) = wO2ProdPhyt
         pmsa( ipnt( 1465) ) = wO2RespPhytW
         pmsa( ipnt( 1466) ) = wO2UptNO3Phyt
         pmsa( ipnt( 1467) ) = wO2PrimW
         pmsa( ipnt( 1468) ) = tO2RespPhytS
         pmsa( ipnt( 1469) ) = tO2PrimS
         pmsa( ipnt( 1470) ) = wPMortPhytPO4W
         pmsa( ipnt( 1471) ) = wPMortPhytDetW
         pmsa( ipnt( 1472) ) = wPLossPhytPO4
         pmsa( ipnt( 1473) ) = wPLossPhytDet
         pmsa( ipnt( 1474) ) = wPPrimPO4W
         pmsa( ipnt( 1475) ) = wPPrimDetW
         pmsa( ipnt( 1476) ) = tPMortPhytPO4S
         pmsa( ipnt( 1477) ) = tPMortPhytDetS
         pmsa( ipnt( 1478) ) = tPPrimDetS
         pmsa( ipnt( 1479) ) = tPPrimPO4S
         pmsa( ipnt( 1480) ) = tPPrimTotT
         pmsa( ipnt( 1481) ) = wNMortPhytNH4W
         pmsa( ipnt( 1482) ) = wNMortPhytDetW
         pmsa( ipnt( 1483) ) = wNLossPhytNH4
         pmsa( ipnt( 1484) ) = wNLossPhytDet
         pmsa( ipnt( 1485) ) = wNPrimNH4W
         pmsa( ipnt( 1486) ) = wNPrimNO3W
         pmsa( ipnt( 1487) ) = wNPrimDetW
         pmsa( ipnt( 1488) ) = tNMortPhytNH4S
         pmsa( ipnt( 1489) ) = tNMortPhytDetS
         pmsa( ipnt( 1490) ) = tNPrimNH4S
         pmsa( ipnt( 1491) ) = tNPrimNO3S
         pmsa( ipnt( 1492) ) = tNPrimDetS
         pmsa( ipnt( 1493) ) = tNPrimTotT
         pmsa( ipnt( 1494) ) = aPACoef
         pmsa( ipnt( 1495) ) = bSecchiMax
         pmsa( ipnt( 1496) ) = aSecchi
         pmsa( ipnt( 1497) ) = aDepthEuph
         pmsa( ipnt( 1498) ) = aRelDepthEuph
         pmsa( ipnt( 1499) ) = aChlaH
         pmsa( ipnt( 1500) ) = aCovPhytW
         pmsa( ipnt( 1501) ) = rExtChPhyt
         pmsa( ipnt( 1502) ) = tDSetTot
         pmsa( ipnt( 1503) ) = tPSetTot
         pmsa( ipnt( 1504) ) = tNSetTot
         pmsa( ipnt( 1505) ) = tDResusTot
         pmsa( ipnt( 1506) ) = tPResusTot
         pmsa( ipnt( 1507) ) = tNResusTot
         pmsa( ipnt( 1508) ) = bTimeDred
         pmsa( ipnt( 1509) ) = aDepthStart
         pmsa( ipnt( 1510) ) = akDredDepth
         pmsa( ipnt( 1511) ) = akDred
         pmsa( ipnt( 1512) ) = akDredLemn
         pmsa( ipnt( 1513) ) = vDredDepthW
         pmsa( ipnt( 1514) ) = tDDredDetS
         pmsa( ipnt( 1515) ) = tPDredDetS
         pmsa( ipnt( 1516) ) = tNDredDetS
         pmsa( ipnt( 1517) ) = tPDredAIMS
         pmsa( ipnt( 1518) ) = bRhoSolidSoil
         pmsa( ipnt( 1519) ) = tDDredNetSoil
         pmsa( ipnt( 1520) ) = tDDredNetIMS
         pmsa( ipnt( 1521) ) = tDDredNetHumS
         pmsa( ipnt( 1522) ) = tPDredNetHumS
         pmsa( ipnt( 1523) ) = tNDredNetHumS
         pmsa( ipnt( 1524) ) = tDDredPhytS
         pmsa( ipnt( 1525) ) = tPDredPhytS
         pmsa( ipnt( 1526) ) = tNDredPhytS
         pmsa( ipnt( 1527) ) = tDDredElod
         pmsa( ipnt( 1528) ) = tPDredElod
         pmsa( ipnt( 1529) ) = tNDredElod
         pmsa( ipnt( 1530) ) = tDDredCera
         pmsa( ipnt( 1531) ) = tPDredCera
         pmsa( ipnt( 1532) ) = tNDredCera
         pmsa( ipnt( 1533) ) = tDDredChar
         pmsa( ipnt( 1534) ) = tPDredChar
         pmsa( ipnt( 1535) ) = tNDredChar
         pmsa( ipnt( 1536) ) = tDDredLemn
         pmsa( ipnt( 1537) ) = tPDredLemn
         pmsa( ipnt( 1538) ) = tNDredLemn
         pmsa( ipnt( 1539) ) = tDDredNymp
         pmsa( ipnt( 1540) ) = tPDredNymp
         pmsa( ipnt( 1541) ) = tNDredNymp
         pmsa( ipnt( 1542) ) = tDDredHelo
         pmsa( ipnt( 1543) ) = tPDredHelo
         pmsa( ipnt( 1544) ) = tNDredHelo
         pmsa( ipnt( 1545) ) = tDDredVeg
         pmsa( ipnt( 1546) ) = tPDredVeg
         pmsa( ipnt( 1547) ) = tNDredVeg
         pmsa( ipnt( 1548) ) = tDDredNetTot
         pmsa( ipnt( 1549) ) = tPDredNetTot
         pmsa( ipnt( 1550) ) = tNDredNetTot
         pmsa( ipnt( 1551) ) = tDIMS
         pmsa( ipnt( 1552) ) = tDHumS
         pmsa( ipnt( 1553) ) = tDDetS
         pmsa( ipnt( 1554) ) = vDeltaS
         pmsa( ipnt( 1555) ) = tDBurIM
         pmsa( ipnt( 1556) ) = tDBurOM
         pmsa( ipnt( 1557) ) = tDBurDet
         pmsa( ipnt( 1558) ) = tDBurHum
         pmsa( ipnt( 1559) ) = tDBurTot
         pmsa( ipnt( 1560) ) = tPBurHum
         pmsa( ipnt( 1561) ) = tPBurDet
         pmsa( ipnt( 1562) ) = tPBurAIM
         pmsa( ipnt( 1563) ) = tPBurPO4
         pmsa( ipnt( 1564) ) = tPBurTot
         pmsa( ipnt( 1565) ) = tNBurHum
         pmsa( ipnt( 1566) ) = tNBurDet
         pmsa( ipnt( 1567) ) = tNBurNH4
         pmsa( ipnt( 1568) ) = tNBurNO3
         pmsa( ipnt( 1569) ) = tNBurTot
         pmsa( ipnt( 1570) ) = vDeltaW
         pmsa( ipnt( 1571) ) = aRelDeltaW
         pmsa( ipnt( 1572) ) = aDTotT
         pmsa( ipnt( 1573) ) = aNTotT
         pmsa( ipnt( 1574) ) = aPTotT
         pmsa( ipnt( 1575) ) = aDError
         pmsa( ipnt( 1576) ) = aNError
         pmsa( ipnt( 1577) ) = aPError
         pmsa( ipnt( 1578) ) = dPO4W
         pmsa( ipnt( 1579) ) = dPAIMW
         pmsa( ipnt( 1580) ) = dNH4W
         pmsa( ipnt( 1581) ) = dNO3W
         pmsa( ipnt( 1582) ) = dO2W
         pmsa( ipnt( 1583) ) = dDIMW
         pmsa( ipnt( 1584) ) = dDDetW
         pmsa( ipnt( 1585) ) = dPDetW
         pmsa( ipnt( 1586) ) = dNDetW
         pmsa( ipnt( 1587) ) = dDPhytW
         pmsa( ipnt( 1588) ) = dPPhytW
         pmsa( ipnt( 1589) ) = dNPhytW
         pmsa( ipnt( 1590) ) = dPO4S
         pmsa( ipnt( 1591) ) = dPAIMS
         pmsa( ipnt( 1592) ) = dNH4S
         pmsa( ipnt( 1593) ) = dNO3S
         pmsa( ipnt( 1594) ) = dDIMS
         pmsa( ipnt( 1595) ) = dDHumS
         pmsa( ipnt( 1596) ) = dPHumS
         pmsa( ipnt( 1597) ) = dNHumS
         pmsa( ipnt( 1598) ) = dDDetS
         pmsa( ipnt( 1599) ) = dPDetS
         pmsa( ipnt( 1600) ) = dNDetS
         pmsa( ipnt( 1601) ) = dDPhytS
         pmsa( ipnt( 1602) ) = dPPhytS
         pmsa( ipnt( 1603) ) = dNPhytS
         pmsa( ipnt( 1604) ) = dDElod
         pmsa( ipnt( 1605) ) = dPElod
         pmsa( ipnt( 1606) ) = dNElod
         pmsa( ipnt( 1607) ) = dDChar
         pmsa( ipnt( 1608) ) = dPChar
         pmsa( ipnt( 1609) ) = dNChar
         pmsa( ipnt( 1610) ) = dDCera
         pmsa( ipnt( 1611) ) = dPCera
         pmsa( ipnt( 1612) ) = dNCera
         pmsa( ipnt( 1613) ) = dDLemn
         pmsa( ipnt( 1614) ) = dPLemn
         pmsa( ipnt( 1615) ) = dNLemn
         pmsa( ipnt( 1616) ) = dDNymp
         pmsa( ipnt( 1617) ) = dPNymp
         pmsa( ipnt( 1618) ) = dNNymp
         pmsa( ipnt( 1619) ) = dDHelo
         pmsa( ipnt( 1620) ) = dPHelo
         pmsa( ipnt( 1621) ) = dNHelo
         pmsa( ipnt( 1622) ) = dDExtTotT
         pmsa( ipnt( 1623) ) = dNExtTotT
         pmsa( ipnt( 1624) ) = dPExtTotT
         ID0sDDetS               = ID0sDDetS                + noflux
         ID0sDDetW               = ID0sDDetW                + noflux
         ID0sDHumS               = ID0sDHumS                + noflux
         ID0sDIMS                = ID0sDIMS                 + noflux
         ID0sDIMW                = ID0sDIMW                 + noflux
         ID0sNDetS               = ID0sNDetS                + noflux
         ID0sNDetW               = ID0sNDetW                + noflux
         ID0sNH4S                = ID0sNH4S                 + noflux
         ID0sNH4W                = ID0sNH4W                 + noflux
         ID0sNHumS               = ID0sNHumS                + noflux
         ID0sNO3S                = ID0sNO3S                 + noflux
         ID0sNO3W                = ID0sNO3W                 + noflux
         ID0sO2W                 = ID0sO2W                  + noflux
         ID0sPAIMS               = ID0sPAIMS                + noflux
         ID0sPAIMW               = ID0sPAIMW                + noflux
         ID0sPDetS               = ID0sPDetS                + noflux
         ID0sPDetW               = ID0sPDetW                + noflux
         ID0sPHumS               = ID0sPHumS                + noflux
         ID0sPO4S                = ID0sPO4S                 + noflux
         ID0sPO4W                = ID0sPO4W                 + noflux
         ID0sDPhytW              = ID0sDPhytW               + noflux
         ID0sPPhytW              = ID0sPPhytW               + noflux
         ID0sNPhytW              = ID0sNPhytW               + noflux
         ID0sDPhytS              = ID0sDPhytS               + noflux
         ID0sPPhytS              = ID0sPPhytS               + noflux
         ID0sNPhytS              = ID0sNPhytS               + noflux
         ID0sDElod               = ID0sDElod                + noflux
         ID0sDChar               = ID0sDChar                + noflux
         ID0sDCera               = ID0sDCera                + noflux
         ID0sDLemn               = ID0sDLemn                + noflux
         ID0sDNymp               = ID0sDNymp                + noflux
         ID0sDHelo               = ID0sDHelo                + noflux
         ID0sPElod               = ID0sPElod                + noflux
         ID0sPChar               = ID0sPChar                + noflux
         ID0sPCera               = ID0sPCera                + noflux
         ID0sPLemn               = ID0sPLemn                + noflux
         ID0sPNymp               = ID0sPNymp                + noflux
         ID0sPHelo               = ID0sPHelo                + noflux
         ID0sNElod               = ID0sNElod                + noflux
         ID0sNChar               = ID0sNChar                + noflux
         ID0sNCera               = ID0sNCera                + noflux
         ID0sNLemn               = ID0sNLemn                + noflux
         ID0sNNymp               = ID0sNNymp                + noflux
         ID0sNHelo               = ID0sNHelo                + noflux
         ID0sDExtTotT            = ID0sDExtTotT             + noflux
         ID0sNExtTotT            = ID0sNExtTotT             + noflux
         ID0sPExtTotT            = ID0sPExtTotT             + noflux

         ipnt        = ipnt        + increm 
   !   
    9000 continue  
   !   
         return    
         end subroutine    
