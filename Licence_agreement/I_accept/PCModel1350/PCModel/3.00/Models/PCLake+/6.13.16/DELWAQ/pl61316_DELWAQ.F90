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
         integer ipoint(2983) ! I  Array of pointers in pmsa to get and store the data 
         integer increm(2983) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying  
         integer noseg        ! I  Number of computational elements in the whole model schematisation  
         integer noflux       ! I  Number of fluxes, increment in the fl array 
         integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces  
         integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use   
         integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh) 
         integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid   
         integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward 
         integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)  
         integer ipnt(2983)   !    Local work array for the pointering 
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
      real(4) sNH4WH         	! [      3.66E-01 ]	_gN_*_m_^-2*_d_^-1		N_in_NH4_in_hypolimnion_lake_water
      real(4) sNO3WH         	! [       4.10222 ]	_gN_*_m_^-2*_d_^-1		N_in_NO3_in_hypolimnion_lake_water
      real(4) sPO4WH         	! [      8.48E-01 ]	_gP_*_m_^-2*_d_^-1		P_in_PO4_in_hypolimnion_lake_water
      real(4) sPAIMWH        	! [      6.91E-04 ]	_gP_*_m_^-2*_d_^-1		P_adsorbed_onto_IM_in_hypolimnion_lake_water
      real(4) sSiO2WH        	! [        6.6665 ]	_gSi_*_m_^-2*_d_^-1		Dissolved_Si_in_hypolimnion_lake_water
      real(4) sO2WH          	! [       6.91704 ]	_gO2_*_m_^-2*_d_^-1		Oxygen_in_hypolimnion_lake_water
      real(4) sDDetWH        	! [       6.16687 ]	_gDW_*_m_^-2*_d_^-1		Detritus_DW_in_hypolimnion_lake_water
      real(4) sNDetWH        	! [      4.20E-01 ]	_gN_*_m_^-2*_d_^-1		Detritus_N_in_hypolimnion_lake_water
      real(4) sPDetWH        	! [      6.13E-02 ]	_gP_*_m_^-2*_d_^-1		Detritus_P_in_hypolimnion_lake_water
      real(4) sSiDetWH       	! [      3.37E-01 ]	_gSi_*_m_^-2*_d_^-1		Detritus_Si_in_hypolimnion_lake_water
      real(4) sDIMWH         	! [       2.02593 ]	_gDW_*_m_^-2*_d_^-1		Inorganic_matter_in_hypolimnion_lake_water
      real(4) sDDiatWH       	! [      6.18E-03 ]	_gDW_*_m_^-2*_d_^-1		Diatoms_DW_in_hypolimnion_lake_water
      real(4) sNDiatWH       	! [      3.09E-04 ]	_gN_*_m_^-2*_d_^-1		Diatoms_N_in_hypolimnion_lake_water
      real(4) sPDiatWH       	! [      3.61E-05 ]	_gP_*_m_^-2*_d_^-1		Diatoms_P_in_hypolimnion_lake_water
      real(4) sDGrenWH       	! [       0.01658 ]	_gDW_*_m_^-2*_d_^-1		Green_algae_DW_in_hypolimnion_lake_water
      real(4) sNGrenWH       	! [      1.65E-03 ]	_gN_*_m_^-2*_d_^-1		Green_algae_N_in_hypolimnion_lake_water
      real(4) sPGrenWH       	! [      2.46E-04 ]	_gP_*_m_^-2*_d_^-1		Green_algae_P_in_hypolimnion_lake_water
      real(4) sDBlueWH       	! [      4.20E-01 ]	_gDW_*_m_^-2*_d_^-1		Blue-greens_DW_in_hypolimnion_lake_water
      real(4) sNBlueWH       	! [      6.22E-02 ]	_gN_*_m_^-2*_d_^-1		Blue-greens_N_in_hypolimnion_lake_water
      real(4) sPBlueWH       	! [      1.05E-02 ]	_gP_*_m_^-2*_d_^-1		Blue-greens_P_in_hypolimnion_lake_water
      real(4) sDZooH         	! [      4.39E-02 ]	_gDW_*_m_^-2*_d_^-1		Zooplankton_DW_in_hypolimnion_lake_water
      real(4) sNZooH         	! [      3.07E-03 ]	_gN_*_m_^-2*_d_^-1		Zooplankton_N_in_hypolimnion_lake_water
      real(4) sPZooH         	! [      4.39E-04 ]	_gP_*_m_^-2*_d_^-1		Zooplankton_P_in_hypolimnion_lake_water
      real(4) sDFiAd         	! [       2.22476 ]	_gDW_*_m_^-2*_d_^-1		Adult_fish_DW_in_lake_water
      real(4) sDFiJv         	! [      9.02E-01 ]	_gDW_*_m_^-2*_d_^-1		Young_fish_DW_in_lake_water
      real(4) sNFiAd         	! [      2.22E-01 ]	_gN_*_m_^-2*_d_^-1		Adult_fish_N_in_lake_water
      real(4) sNFiJv         	! [      9.02E-02 ]	_gN_*_m_^-2*_d_^-1		Young_fish_N_in_lake_water
      real(4) sPFiAd         	! [      4.89E-02 ]	_gP_*_m_^-2*_d_^-1		Adult_fish_P_in_lake_water
      real(4) sPFiJv         	! [      1.99E-02 ]	_gP_*_m_^-2*_d_^-1		Young_fish_P_in_lake_water
      real(4) sDPisc         	! [      2.48E-04 ]	_gDW_*_m_^-2*_d_^-1		Predatory_fish_DW_in_lake_water
      real(4) sNH4S          	! [       0.20993 ]	_gN_*_m_^-2*_d_^-1		N_in_NH4_in_lake_sediment_pore_water
      real(4) sNO3S          	! [      9.52E-02 ]	_gN_*_m_^-2*_d_^-1		N_in_NO3_in_lake_sediment_pore_water
      real(4) sPO4S          	! [      5.18E-02 ]	_gP_*_m_^-2*_d_^-1		P_in_PO4_in_lake_sediment_pore_water
      real(4) sPAIMS         	! [       2.72036 ]	_gP_*_m_^-2*_d_^-1		P_adsorbed_onto_IM_in_lake_sediment
      real(4) sDDetS         	! [        481.77 ]	_gDW_*_m_^-2*_d_^-1		Detritus_DW_in_lake_sediment
      real(4) sNDetS         	! [       28.6447 ]	_gN_*_m_^-2*_d_^-1		Detritus_N_in_lake_sediment
      real(4) sPDetS         	! [       4.05956 ]	_gP_*_m_^-2*_d_^-1		Detritus_P_in_lake_sediment
      real(4) sSiDetS        	! [       32.8693 ]	_gSi_*_m_^-2*_d_^-1		Detritus_Si_in_lake_sediment
      real(4) sDHumS         	! [       6811.77 ]	_gDW_*_m_^-2*_d_^-1		Humus_DW_in_lake_sediment
      real(4) sNHumS         	! [       340.739 ]	_gN_*_m_^-2*_d_^-1		Humus_N_in_lake_sediment
      real(4) sPHumS         	! [        45.916 ]	_gP_*_m_^-2*_d_^-1		Humus_P_in_lake_sediment
      real(4) sDIMS          	! [       26171.9 ]	_gDW_*_m_^-2*_d_^-1		Inorganic_matter_in_lake_sediment
      real(4) sDDiatS        	! [      8.47E-03 ]	_gDW_*_m_^-2*_d_^-1		Diatoms_DW_on_lake_sediment
      real(4) sNDiatS        	! [      4.23E-04 ]	_gN_*_m_^-2*_d_^-1		Diatoms_N_on_lake_sediment
      real(4) sPDiatS        	! [      4.87E-05 ]	_gP_*_m_^-2*_d_^-1		Diatoms_P_on_lake_sediment
      real(4) sDGrenS        	! [      1.08E-02 ]	_gDW_*_m_^-2*_d_^-1		Green_algae_DW_on_lake_sediment
      real(4) sNGrenS        	! [      1.07E-03 ]	_gN_*_m_^-2*_d_^-1		Green_algae_N_on_lake_sediment
      real(4) sPGrenS        	! [      1.59E-04 ]	_gP_*_m_^-2*_d_^-1		Green_algae_P_on_lake_sediment
      real(4) sDBlueS        	! [      4.30E-02 ]	_gDW_*_m_^-2*_d_^-1		Blue-greens_DW_on_lake_sediment
      real(4) sNBlueS        	! [      6.33E-03 ]	_gN_*_m_^-2*_d_^-1		Blue-greens_N_on_lake_sediment
      real(4) sPBlueS        	! [      1.07E-03 ]	_gP_*_m_^-2*_d_^-1		Blue-greens_P_on_lake_sediment
      real(4) sDVeg          	! [      4.70E-02 ]	_gDW_*_m_^-2*_d_^-1		Vegetation_DW_in_lake_water
      real(4) sNVeg          	! [      1.64E-03 ]	_gN_*_m_^-2*_d_^-1		Vegetation_N_in_lake_water
      real(4) sPVeg          	! [      1.64E-04 ]	_gP_*_m_^-2*_d_^-1		Vegetation_P_in_lake_water
      real(4) sVegHe         	! [             1 ]	_m_		Height_of_vegetation
      real(4) sDBent         	! [       2.91398 ]	_gDW_*_m_^-2*_d_^-1		Zoobenthos_DW_in_lake_sediment
      real(4) sNBent         	! [      2.04E-01 ]	_gN_*_m_^-2*_d_^-1		Zoobenthos_N_in_lake_sediment
      real(4) sPBent         	! [      2.91E-02 ]	_gP_*_m_^-2*_d_^-1		Zoobenthos_P_in_lake_sediment
      real(4) sDepthWM       	! [           0.5 ]	_m_		Depth_of_marsh_water
      real(4) sNH4WM         	! [           0.1 ]	_gN_*_m_^-2*_d_^-1		N_in_NH4_in_marsh_water
      real(4) sNO3WM         	! [           0.1 ]	_gN_*_m_^-2*_d_^-1		N_in_NO3_in_marsh_water
      real(4) sPO4WM         	! [          0.01 ]	_gP_*_m_^-2*_d_^-1		P_in_PO4_in_marsh_water
      real(4) sPAIMWM        	! [             0 ]	_gP_*_m_^-2*_d_^-1		P_adsorbed_onto_IM_in_marsh_water
      real(4) sSiO2WM        	! [             3 ]	_gSi_*_m_^-2*_d_^-1		Dissolved_Si_in_marsh_water
      real(4) sO2WM          	! [            10 ]	_gO2_*_m_^-2*_d_^-1		Oxygen_in_marsh_water
      real(4) sDDetWM        	! [             2 ]	_gDW_*_m_^-2*_d_^-1		Detritus_DW_in_marsh_water
      real(4) sNDetWM        	! [          0.05 ]	_gN_*_m_^-2*_d_^-1		Detritus_N_in_marsh_water
      real(4) sPDetWM        	! [         0.005 ]	_gP_*_m_^-2*_d_^-1		Detritus_P_in_marsh_water
      real(4) sSiDetWM       	! [          0.02 ]	_gSi_*_m_^-2*_d_^-1		Detritus_Si_in_marsh_water
      real(4) sDIMWM         	! [             5 ]	_gDW_*_m_^-2*_d_^-1		Inorganic_matter_in_marsh_water
      real(4) sDDiatWM       	! [           0.5 ]	_gDW_*_m_^-2*_d_^-1		Diatoms_DW_in_marsh_water
      real(4) sNDiatWM       	! [          0.05 ]	_gN_*_m_^-2*_d_^-1		Diatoms_N_in_marsh_water
      real(4) sPDiatWM       	! [         0.005 ]	_gP_*_m_^-2*_d_^-1		Diatoms_P_in_marsh_water
      real(4) sDGrenWM       	! [           0.5 ]	_gDW_*_m_^-2*_d_^-1		Green_algae_DW_in_marsh_water
      real(4) sNGrenWM       	! [          0.05 ]	_gN_*_m_^-2*_d_^-1		Green_algae_N_in_marsh_water
      real(4) sPGrenWM       	! [         0.005 ]	_gP_*_m_^-2*_d_^-1		Green_algae_P_in_marsh_water
      real(4) sDBlueWM       	! [             3 ]	_gDW_*_m_^-2*_d_^-1		Blue-greens_DW_in_marsh_water
      real(4) sNBlueWM       	! [           0.3 ]	_gN_*_m_^-2*_d_^-1		Blue-greens_N_in_marsh_water
      real(4) sPBlueWM       	! [          0.03 ]	_gP_*_m_^-2*_d_^-1		Blue-greens_P_in_marsh_water
      real(4) sDZooM         	! [          0.05 ]	_gDW_*_m_^-2*_d_^-1		Zooplankton_DW_in_marsh_water
      real(4) sNZooM         	! [        0.0035 ]	_gN_*_m_^-2*_d_^-1		Zooplankton_N_in_marsh_water
      real(4) sPZooM         	! [        0.0005 ]	_gP_*_m_^-2*_d_^-1		Zooplankton_P_in_marsh_water
      real(4) sNH4SM         	! [             1 ]	_gN_*_m_^-2*_d_^-1		N_in_NH4_in_marsh_sediment_pore_water
      real(4) sNO3SM         	! [          0.01 ]	_gN_*_m_^-2*_d_^-1		N_in_NO3_in_marsh_sediment_pore_water
      real(4) sPO4SM         	! [      1.82E-01 ]	_gP_*_m_^-2*_d_^-1		P_in_PO4_in_marsh_sediment_pore_water
      real(4) sPAIMSM        	! [       17.9886 ]	_gP_*_m_^-2*_d_^-1		P_adsorbed_onto_IM_in_marsh_sediment
      real(4) sDDetSM        	! [       181.703 ]	_gDW_*_m_^-2*_d_^-1		Detritus_DW_in_marsh_sediment
      real(4) sNDetSM        	! [       4.54258 ]	_gN_*_m_^-2*_d_^-1		Detritus_N_in_marsh_sediment
      real(4) sPDetSM        	! [      4.54E-01 ]	_gP_*_m_^-2*_d_^-1		Detritus_P_in_marsh_sediment
      real(4) sSiDetSM       	! [       1.81703 ]	_gSi_*_m_^-2*_d_^-1		Detritus_Si_in_marsh_sediment
      real(4) sDHumSM        	! [       3452.36 ]	_gDW_*_m_^-2*_d_^-1		Humus_DW_in_marsh_sediment
      real(4) sNHumSM        	! [       172.618 ]	_gN_*_m_^-2*_d_^-1		Humus_N_in_marsh_sediment
      real(4) sPHumSM        	! [       17.2618 ]	_gP_*_m_^-2*_d_^-1		Humus_Pin_marsh_sediment
      real(4) sDIMSM         	! [       32706.5 ]	_gDW_*_m_^-2*_d_^-1		Inorganic_matter_in_marsh_sediment
      real(4) sDRoPhra       	! [          5000 ]	_gDW_*_m_^-2*_d_^-1		Root_biomass_DW_in_marsh_sediment
      real(4) sDShPhra       	! [          1000 ]	_gDW_*_m_^-2*_d_^-1		Shoot_biomass_DW_in_marsh_water
      real(4) sNRoPhra       	! [           100 ]	_gN_*_m_^-2*_d_^-1		Root_biomass_N_in_marsh_sediment
      real(4) sNShPhra       	! [            20 ]	_gN_*_m_^-2*_d_^-1		Shoot_biomass_N_in_marsh_water
      real(4) sPRoPhra       	! [            10 ]	_gP_*_m_^-2*_d_^-1		Root_biomass_P_in_marsh_sediment
      real(4) sPShPhra       	! [             2 ]	_gP_*_m_^-2*_d_^-1		Shoot_biomass_P_in_marsh_water
      real(4) sNH4WE         	! [       0.42609 ]	_gN_*_m_^-2*_d_^-1		N_in_NH4_in_epilimnion_lake_water
      real(4) sNO3WE         	! [       4.10144 ]	_gN_*_m_^-2*_d_^-1		N_in_NO3_in_epilimnion_lake_water
      real(4) sPO4WE         	! [      8.39E-01 ]	_gP_*_m_^-2*_d_^-1		P_in_PO4_in_epilimnion_lake_water
      real(4) sPAIMWE        	! [      2.57E-04 ]	_gP_*_m_^-2*_d_^-1		P_adsorbed_onto_IM_in_epilimnion_lake_water
      real(4) sSiO2WE        	! [       6.54808 ]	_gSi_*_m_^-2*_d_^-1		Dissolved_Si_in_epilimnion_lake_water
      real(4) sO2WE          	! [       9.94226 ]	_gO2_*_m_^-2*_d_^-1		Oxygen_in_epilimnion_lake_water
      real(4) sDDetWE        	! [       6.15446 ]	_gDW_*_m_^-2*_d_^-1		Detritus_DW_in_epilimnion_lake_water
      real(4) sNDetWE        	! [      4.23E-01 ]	_gN_*_m_^-2*_d_^-1		Detritus_N_in_epilimnion_lake_water
      real(4) sPDetWE        	! [      6.15E-02 ]	_gP_*_m_^-2*_d_^-1		Detritus_P_in_epilimnion_lake_water
      real(4) sSiDetWE       	! [      3.28E-01 ]	_gSi_*_m_^-2*_d_^-1		Detritus_Si_in_epilimnion_lake_water
      real(4) sDIMWE         	! [      6.65E-01 ]	_gDW_*_m_^-2*_d_^-1		Inorganic_matter_in_epilimnion_lake_water
      real(4) sDDiatWE       	! [      1.53E-02 ]	_gDW_*_m_^-2*_d_^-1		Diatoms_DW_in_epilimnion_lake_water
      real(4) sNDiatWE       	! [      7.50E-04 ]	_gN_*_m_^-2*_d_^-1		Diatoms_N_in_epilimnion_lake_water
      real(4) sPDiatWE       	! [      9.12E-05 ]	_gP_*_m_^-2*_d_^-1		Diatoms_P_in_epilimnion_lake_water
      real(4) sDGrenWE       	! [      2.82E-02 ]	_gDW_*_m_^-2*_d_^-1		Green_algae_DW_in_epilimnion_lake_water
      real(4) sNGrenWE       	! [      2.51E-03 ]	_gN_*_m_^-2*_d_^-1		Green_algae_N_in_epilimnion_lake_water
      real(4) sPGrenWE       	! [      3.69E-04 ]	_gP_*_m_^-2*_d_^-1		Green_algae_P_in_epilimnion_lake_water
      real(4) sDBlueWE       	! [      4.40E-01 ]	_gDW_*_m_^-2*_d_^-1		Blue-greens_DW_in_epilimnion_lake_water
      real(4) sNBlueWE       	! [      6.11E-02 ]	_gN_*_m_^-2*_d_^-1		Blue-greens_N_in_epilimnion_lake_water
      real(4) sPBlueWE       	! [      1.06E-02 ]	_gP_*_m_^-2*_d_^-1		Blue-greens_P_in_epilimnion_lake_water
      real(4) sDZooE         	! [      4.48E-02 ]	_gDW_*_m_^-2*_d_^-1		Zooplankton_DW_in_epilimnion_lake_water
      real(4) sNZooE         	! [      3.14E-03 ]	_gN_*_m_^-2*_d_^-1		Zooplankton_N_in_epilimnion_lake_water
      real(4) sPZooE         	! [      4.48E-04 ]	_gP_*_m_^-2*_d_^-1		Zooplankton_P_in_epilimnion_lake_water
      real(4) sDExTotT       	! [         33594 ]	_gDW_*_m_^-2*_d_^-1		Total_amount_of_DW_moved_into_or_out_from_the_system
      real(4) sNExTotT       	! [       444.908 ]	_gN_*_m_^-2*_d_^-1		Total_amount_of_N_moved_into_or_out_from_the_system
      real(4) sPExTotT       	! [       66.6182 ]	_gP_*_m_^-2*_d_^-1		Total_amount_of_P_moved_into_or_out_from_the_system
      real(4) sSiETotT       	! [       137.204 ]	_gSi_*_m_^-2*_d_^-1		Total_amount_of_Si_moved_into_or_out_from_the_system
      real(4) sO2ETotT       	! [       121.292 ]	_gO2_*m_^-2		Total_amount_of_O2_moved_into_or_out_from_the_system
!
!
!     /* ==============================  */
!     /* declaration parameters          */
!     /* ==============================  */
      real(4) ITIME                     ! [           0]	sec                     Time of DELWAQ
      real(4) TotalDepth                ! [           0]	m                       depth of DELWAQ
      real(4) InitCalc            	! [           0]	0                   	If_T_skip_calculation_of_initial_values_used_in_case_of_REINIT_command
      real(4) CalcMass            	! [           1]	0                   	If_T_calculation_of_model_in_mass_if_F_calculation_of_model_in_concentration
      real(4) ContDpth            	! [           1]	0                   	If_T_water_depth_kept_constant_by_daily_dredging
      real(4) InitDpth            	! [           1]	0                   	If_T_calculate_epilimnion_and_hypolimnion_depth_based_on_cDepthW0_
      real(4) ContTans            	! [           1]	0                   	If_T_calculate_with_constant_atmospheric_transmissivity_in_latitude_module
      real(4) InclTran            	! [           0]	0                   	Include_transport_processes_of_substances_module
      real(4) InclV               	! [           1]	0                   	Include_vegetation_module
      real(4) InclBur             	! [           1]	0                   	Include_burial_module
      real(4) InclWeb             	! [           1]	0                   	Include_food_web_module
      real(4) InclMrsh            	! [           0]	0                   	Include_marsh_zone_module
      real(4) IncSeson            	! [           1]	0                   	Include_season_module
      real(4) InclSrat            	! [           0]	0                   	Include_Stratification
      real(4) calixpth            	! [           0]	0                   	If_T_calculate_mixing_depth_if_F_use_constant
      real(4) InclLat             	! [           1]	0                   	If_T_include_latitude_module
      real(4) calcQEv             	! [           0]	0                   	If_T_calculate_evaporation_based_on_Thornthwaite_1984
      real(4) DayStart            	! [           0]	_d_                 	Day_of_year_to_start_model
      real(4) ReadTemp            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_water_temperature_otherwise_sinus
      real(4) ReadSrat            	! [           0]	0                   	If_TRUE_use_external_time-series_of_stratification_period
      real(4) ReadLOut            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_light_otherwise_sinus
      real(4) ReadVind            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_wind_otherwise_constant
      real(4) ReadQIn             	! [           1]	0                   	If_TRUE_use_measured_time-series_of_inflow_otherwise_constant
      real(4) ReadQOut            	! [           1]	0                   	If_TRUE_use_measured_time-series_of_inflow_otherwise_constant
      real(4) ReadQEv             	! [           1]	0                   	If_TRUE_use_measured_time-series_of_evaporation_otherwise_constant
      real(4) ReadPoad            	! [           1]	0                   	If_TRUE_use_measured_time-series_of_P_loading_otherwise_constant
      real(4) ReadNoad            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_N_loading_otherwise_constant
      real(4) ReaNurac            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_loading_with_diff_nutrient_fractions
      real(4) ReaPLadP            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_P_loading_algal_input_otherwise_constant
      real(4) ReaDLdDe            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_DDet_loading_otherwise_constant
      real(4) ReaVSade            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_shading_percentage_of_plants_otherwise_constant
      real(4) UsePoerV            	! [           0]	0                   	If_TRUE_use_a_power_function_for_vegetation_else_use_exponential_function
      real(4) UsePhotV            	! [           1]	0                   	If_TRUE_use_a_photoperiod_cue_to_determine_reallocation_from_winter_to_summer_mode_if_FALSE_use_temperature
      real(4) UseLWinV            	! [           1]	0                   	If_TRUE_use_a_light_integration_cue_to_initiate_overwintering_of_vegetation_otherwise_use_cDayWinVeg
      real(4) UseVHe              	! [           0]	0                   	If_TRUE_use_a_dynamic_estimation_for_vegetation_height_based_on_stem_densities_and_biomass_otherwise_use_cVegHeight_ONLY_USE_IN_SPATIALLY_HETEROGENEOUS_MODELLING
      real(4) ReaDLdIM            	! [           0]	0                   	If_TRUE_use_measured_time-series_of_DIM_loading_otherwise_constant
      real(4) Useasoad            	! [           0]	0                   	If_TRUE_use_different_inflow_and_loading_for_summer_and_winter_periods
      real(4) Uselsoad            	! [           0]	0                   	If_TRUE_use_a_pulse-wise_nutrient_loading
      real(4) mTempE              	! [           0]	_oC_                	Measured_time-series_of_water_temperature_Epilimion
      real(4) mTempH              	! [           0]	_oC_                	Measured_time-series_of_water_temperature_Hypolimnion
      real(4) mStrat              	! [           0]	0                   	External_time-series_of_stratification_period_mstrat_equals_1_is_stratification_and_mstrat_equals_0_is_mixing
      real(4) mLOut               	! [           0]	_J_*_s_^-1*_m_^-2   	Measured_time-series_of_light
      real(4) mVShade             	! [           0]	0                   	Measured_time-series_of_fraction_shading_on_plants
      real(4) mVWind              	! [           0]	_m_*_s^-1_          	Measured_time-series_of_wind
      real(4) mQIn                	! [          20]	_mm_*_d_^-1         	Measured_time-series_of_inflow
      real(4) mQOut               	! [           0]	_mm_*_d_^-1         	Measured_time-series_of_outflow
      real(4) mQEv                	! [           0]	_mm_*_d_^-1         	Measured_time-series_of_evaporation
      real(4) mPLoad              	! [           0]	_gP_*_m_^-2*_d_^-1  	Measured_time-series_of_P_loading
      real(4) mPLoaPO4            	! [           0]	_gP_*_m_^-2*_d_^-1  	Measured_time-series_of_PO4_loading
      real(4) mPLoaOrg            	! [           0]	_gP_*_m_^-2*_d_^-1  	Measured_time-series_of_loading_P_bound_to_org_matter
      real(4) mPLadTot            	! [           0]	_gP_*_m_^-2*_d_^-1  	Measured_time-series_of_P_loading_algal_input
      real(4) mNLoad              	! [           0]	_gN_*_m_^-2*_d_^-1  	Measured_time-series_of_N_loading
      real(4) mNLoaNH4            	! [           0]	_gN_*_m_^-2*_d_^-1  	Measured_time-series_of_NH4_loading
      real(4) mNLoaNO3            	! [           0]	_gN_*_m_^-2*_d_^-1  	Measured_time-series_of_NO3_loading
      real(4) mNLoaOrg            	! [           0]	_gN_*_m_^-2*_d_^-1  	Measured_time-series_of_loading_N_bound_to_org_matter
      real(4) mDLoadDe            	! [           0]	_gDW_*_m_^-2*_d_^-1 	Measured_time-series_of_Detritus_loading
      real(4) mDLoadIM            	! [           0]	_gDW_*_m_^-2*_d_^-1 	Measured_time-series_of_loading_of_DW_of_inorg_matter
      real(4) StTime              	! [           0]	_d_                 	Day_of_the_year_that_the_model_run_will_start
      real(4) EndTime             	! [         365]	_d_                 	(=1_year)
      real(4) YearZero            	! [           0]	_y_                 	Year_number_of_start_of_calculation_Note_also_Day_no_1_=_1_Jan_of_this_year
      real(4) cDepthW0            	! [         1.5]	_m_                 	Initial_average_depth_of_lake
      real(4) cMnephHE            	! [        0.01]	_m_                 	Minimum_hypolimnion_depth
      real(4) cFetch              	! [    2.63E+02]	_m_                 	Wind_fetch
      real(4) fMarsh              	! [           0]	_m_^2*_m_^-2        	Relative_marsh_area_per_lake_area
      real(4) fLutum              	! [        0.03]	0                   	Lutum_content_of_inorg_matter
      real(4) kExchW              	! [           1]	_d_^-1              	Rate_of_water_exchange_between_epilimnion_and_hypolimnion_in_case_of_water_level_difference_from_mixing_depth
      real(4) kExchMxW            	! [    1.73E-04]	_m_^2*_d_^-1        	Maximum_diffusive_exchange_rate_of_water_between_epilimnion_and_hypolimnion
      real(4) cDeptMix            	! [         4.5]	_m_                 	Mixing_depth_between_epilimnion_and_hypolymnion
      real(4) cAMix               	! [       0.569]	_m_^-_cBMix_*_m_    	Factor_for_mixing_depth_between_epilimnion_and_hypolymnion
      real(4) cBMix               	! [       0.336]	0                   	Factor_for_mixing_depth_between_epilimnion_and_hypolymnion
      real(4) cfH                 	! [           1]	_m_^2*_m_^-2        	Relative_hypolimnion_area_where_exchange_is_50_percent
      real(4) fFeDIM              	! [       0.003]	_gFe_*_gDW_^-1      	Fe_content_of_inorg_matter
      real(4) fAlDIM              	! [       0.003]	_gAl_*_gDW_^-1      	Al_content_of_inorg_matter
      real(4) fVShade             	! [           0]	0                   	Fraction_reduction_of_light_reaching_plants
      real(4) cTmAveE             	! [          15]	_oC_                	Average_water_temperature_Epilimnion
      real(4) cTmVarE             	! [          10]	_oC_                	Annual_water_temperature_variation_Epilimnion
      real(4) cTmAveH             	! [           5]	_oC_                	Average_water_temperature_Hypolimnion
      real(4) cTmVarH             	! [           4]	_oC_                	Annual_water_temperature_variation_Hypolimnion
      real(4) cStratTm            	! [           1]	_oC_                	Temperature_difference_for_stratification
      real(4) cTimeLag            	! [          40]	_d_                 	Time_lag_for_temperature
      real(4) cVWind              	! [           5]	_m_*_s^-1_          	Average_wind_speed
      real(4) cQInf               	! [           0]	_mm_*_d_^-1         	Infiltration_rate
      real(4) cPBckoad            	! [           0]	_gP_*_m_^-2*_d_^-1  	Background_P_loading
      real(4) cNBckoad            	! [           0]	_gP_*_m_^-2*_d_^-1  	Background_N_loading
      real(4) cLDayAve            	! [    1.00E+07]	_J_*_m_^-2*_d_^-1   	Annual_average_radiation
      real(4) cLDayVar            	! [     8000000]	_J_*_m_^-2*_d_^-1   	Annual_variation_in_radiation
      real(4) cfDayAve            	! [         0.5]	_h_*_h_^-1          	Average_day_length
      real(4) cfDayVar            	! [         0.2]	0                   	Annual_variation_in_day_length
      real(4) fRefl               	! [         0.2]	0                   	The_fraction_photosynthetically_active_radiation_reflected_at_the_surface
      real(4) cExtWat             	! [         0.5]	_m_^-1              	Background_extinction
      real(4) cDrInval            	! [     9999000]	_y_                 	Dredging_interval
      real(4) cDrdSart            	! [     9999000]	_y_                 	First_dredging_year_(should_be_n_times_cDredInterval_)
      real(4) cDeptRef            	! [       1E-28]	_m_                 	Reference_water_depth_for_dredging
      real(4) cLengred            	! [          10]	_d_                 	Length_of_dredging_period
      real(4) fEffDred            	! [        0.95]	0                   	Dredging_efficiency_(<10)
      real(4) fEfreent            	! [         0.5]	0                   	Dredging_efficiency_for_zoobenthos_(<10)
      real(4) fPAR                	! [        0.48]	0                   	Fraction_photosynthetically_active_radiation_(PAR)
      real(4) cLAlloV             	! [        50.1]	_J_*_s_^-1*_m_^-1   	Integrated_relativized_light_level_at_which_the_plant_will_start_summer_regrowth_strategy
      real(4) cVIniLen            	! [          22]	_d_                 	Number_of_days_until_plant_is_allowed_to_break_from_summer_mode_and_redo_initial_allocation_given_summer_has_ended_and_restarted
      real(4) cVWinLen            	! [          22]	_d_                 	Number_of_days_until_plant_is_allowed_to_break_from_winter_mode_and_redo_winter_allocation_given_winter_has_ended_and_restarted
      real(4) cExtSpDe            	! [        0.15]	_m_^2*_gDW_^-1      	Specific_extinction_detritus
      real(4) cExtSpIM            	! [        0.05]	_m_^2*_gDW_^-1      	Specific_extinction_inert_matter
      real(4) fDTotS0             	! [         0.5]	_gDW_*_gDW_^-1      	Initial_dry-weight_fraction_in_sediment
      real(4) fDOrgS0             	! [        0.08]	_gDW_*_gDW_^-1      	Initial_organic_fraction_of_sediment_DW
      real(4) fDDeS0              	! [        0.05]	_gDW_*_gDW_^-1      	Initial_detritus_fraction_of_sediment_organic_matter
      real(4) fSedP0              	! [        0.01]	_gDWDiat_*_gDW_^-1  	Fraction_diatoms_DW_on_lake_sediment
      real(4) fPInogS0            	! [      0.0005]	_gP_*_gDW_^-1       	Initial_inorg_P_fraction_in_sed
      real(4) fPAdsS0             	! [        0.99]	0                   	Initial_adsorbed_fraction_of_inorg_P_in_sed
      real(4) cPDDe0              	! [      0.0025]	_gP_*_gDW_^-1       	Initial_P_fraction_in_detritus
      real(4) cNDDe0              	! [       0.025]	_gN_*_gDW_^-1       	Initial_N_fraction_in_detritus
      real(4) cSiDDe0             	! [        0.01]	_gSi_*_gDW_^-1      	Initial_Si_fraction_in_detritus_Tentative
      real(4) cPDHum0             	! [       0.005]	_gP_*_gDW_^-1       	Initial_P_fraction_in_humus
      real(4) cNDHum0             	! [        0.05]	_gN_*_gDW_^-1       	Initial_N_fraction_in_humus
      real(4) cPDP0               	! [        0.01]	_gP_*_gDW_^-1       	Initial_P_fraction_in_algae
      real(4) cNDP0               	! [         0.1]	_gN_*_gDW_^-1       	Initial_N_fraction_in_algae
      real(4) cPDDi0              	! [        0.01]	_gP_*_gDW_^-1       	Initial_P_fraction_in_diatoms
      real(4) cNDDi0              	! [         0.1]	_gN_*_gDW_^-1       	Initial_N_fraction_in_diatoms
      real(4) cPDGren0            	! [        0.01]	_gP_*_gDW_^-1       	Initial_P_fraction_in_green_algae
      real(4) cNDGren0            	! [         0.1]	_gN_*_gDW_^-1       	Initial_N_fraction_in_green_algae
      real(4) cPDBlue0            	! [        0.01]	_gP_*_gDW_^-1       	Initial_P_fraction_in_blue-green_algae
      real(4) cNDBlue0            	! [         0.1]	_gN_*_gDW_^-1       	Initial_N_fraction_in_blue-green_algae
      real(4) cPDV0               	! [       0.002]	_gP_*_gDW_^-1       	Initial_P_fraction_in_veg
      real(4) cNDV0               	! [        0.02]	_gN_*_gDW_^-1       	Initial_N_fraction_in_veg
      real(4) cSiDDi              	! [        0.15]	_gSi_*_gDW_^-1      	Si/DW_ratio_of_diatoms
      real(4) cPDZoRef            	! [        0.01]	_gP_*_gDW_^-1       	Reference_P/C-ratio_herb_zooplankton
      real(4) cNDZoRef            	! [        0.07]	_gN_*_gDW_^-1       	Reference_N/C-ratio_herb_zooplankton
      real(4) cPDenRef            	! [        0.01]	_gP_*_gDW_^-1       	Reference_P/C_ratio_of_zoobenthos
      real(4) cNDenRef            	! [        0.07]	_gN_*_gDW_^-1       	Reference_N/C_ratio_of_zoobenthos
      real(4) cPDisRef            	! [       0.022]	_gP_*_gDW_^-1       	Reference_P/C_ratio_of_Fish
      real(4) cNDisRef            	! [         0.1]	_gN_*_gDW_^-1       	Reference_N/C_ratio_of_Fish
      real(4) cPDPisc             	! [       0.022]	_gP_*_gDW_^-1       	Reference_P/C_ratio_of_Pi_sc
      real(4) cNDPisc             	! [         0.1]	_gN_*_gDW_^-1       	Reference_N/C_ratio_of_Pi_sc
      real(4) cQIn                	! [          20]	_mm_*_d_^-1         	Standard_water_inflow_if_not_measured
      real(4) cQInSum             	! [          20]	_mm_*_d_^-1         	Summer_water_inflow_if_not_measured
      real(4) cQInWin             	! [          20]	_mm_*_d_^-1         	Winter_water_inflow_if_not_measured
      real(4) cMxWOut             	! [           1]	_d_^-1              	Maximum_water_outflow_rate_in_case_maximum_water_depth_is_reached
      real(4) cDeptWMx            	! [          10]	_m_                 	Maximum_water_depth
      real(4) cQIExpr1            	! [           0]	_mm_*_d_^-1         	Extra_inflow_at_start_of_summer
      real(4) cQIExct1            	! [           0]	_mm_*_d_^-1         	Extra_inflow_at_start_of_winter
      real(4) cQOtEpr1            	! [           0]	_mm_*_d_^-1         	Extra_outflow_at_start_of_summer
      real(4) cQOtEct1            	! [           0]	_mm_*_d_^-1         	Extra_outflow_at_start_of_winter
      real(4) cQEvAve             	! [         1.5]	_mm_*_d_^-1         	Standard_average_evaporation
      real(4) cQEvVar             	! [         1.3]	_mm_*_d_^-1         	Standard_variation_in_evaporation
      real(4) cPLoad              	! [       0.005]	_gP_*_m_^-2*_d_^-1  	Standard_P_loading_if_not_measured
      real(4) cPLoaSum            	! [       0.005]	_gP_*_m_^-2*_d_^-1  	Summer_P_loading_if_not_measured
      real(4) cPLoaWin            	! [       0.005]	_gP_*_m_^-2*_d_^-1  	Winter_P_loading_if_not_measured
      real(4) fPO4In              	! [         0.5]	0                   	Fraction_PO4_in_input_(if_PO4_input_not_measured)
      real(4) fPInWin             	! [        0.02]	0                   	Minimum_algal_fraction_in_organic_P_input
      real(4) fPInSum             	! [         0.1]	0                   	Maximum_algal_fraction_in_organic_P_input
      real(4) fDiPIn              	! [        0.33]	0                   	Diatoms_fraction_of_algal_input
      real(4) fGrenPIn            	! [        0.34]	0                   	Greens_fraction_of_algal_input
      real(4) fBluEn              	! [        0.33]	0                   	Blue-greens_fraction_of_algal_input
      real(4) cNLoad              	! [        0.05]	_gN_*_m_^-2*_d_^-1  	Standard_N_loading
      real(4) cNLoaSum            	! [        0.05]	_gN_*_m_^-2*_d_^-1  	Summer_N_loading
      real(4) cNLoaWin            	! [        0.05]	_gN_*_m_^-2*_d_^-1  	Winter_N_loading
      real(4) cNPoaeas            	! [          10]	_gN_*_gP_^-1        	N/P_loading_if_P_is_measured_and_N_not
      real(4) cNPPIn              	! [           7]	_gP_*_gDW_^-1       	N/P_ratio_of_algal_input
      real(4) cNPDeIn             	! [           7]	_gP_*_gDW_^-1       	N/P_ratio_of_detrital_input
      real(4) fNHDisIn            	! [         0.5]	0                   	NH4_fraction_of_dissolved_N_load_(if_NH4_not_measured)
      real(4) cNDPIn              	! [        0.07]	_gN_*_gDW_^-1       	N/day_ratio_of_algal_input
      real(4) cNDDeIn             	! [        0.07]	_gN_*_gDW_^-1       	N/P_ratio_of_detrital_input
      real(4) cDIMn               	! [           5]	_gDW_*_m_^-3        	IM_conc_in_inflow
      real(4) cO2In               	! [           5]	_gO2_*_m_^-3        	O2_conc_in_inflow
      real(4) cSiO2In             	! [           3]	_gSi_*_m_^-3        	SiO2_conc_in_inflow
      real(4) cSiDDeIn            	! [        0.05]	_gSi_*_gDW_^-1      	Si_content_of_sediment_detritus
      real(4) cDZooIn             	! [         0.1]	_gDW_*_m_^-3        	Zoopl_conc_in_inflowing_water
      real(4) cDayApr1            	! [          91]	_d_                 	April_1
      real(4) cDayOct1            	! [         273]	_d_                 	October_1
      real(4) cLegCnge            	! [          10]	_d_                 	Length_of_season_change
      real(4) cNLoadS             	! [           0]	_gN_*_m_^-2*_d_^-1  	N_fertilizer_to_sediment
      real(4) fNH4LadS            	! [         0.5]	0                   	NH4_fraction_of_N_fertilizer_to_sediment
      real(4) cDEroTot            	! [           0]	_gDW_*_m_^-2*_d_^-1 	Erosion_input_(tentative)
      real(4) fSeErsIM            	! [        0.95]	0                   	Instantly_sedimenting_fraction_of_IM
      real(4) fHErosIM            	! [           0]	0                   	Fraction_of_IM_from_banks_to_hypolymnion
      real(4) fDOrgoil            	! [         0.1]	0                   	Fraction_soil_organic_matter
      real(4) cPDSolOM            	! [       0.001]	_gP_*_gDW_^-1       	P/day_ratio_of_soil_organic_matter
      real(4) cNDSolOM            	! [        0.01]	_gN_*_gDW_^-1       	N/day_ratio_of_soil_organic_matter
      real(4) cPO4Gr              	! [         0.1]	_gP_*_m_^-3         	PO4_cone_in_groundwater
      real(4) cNH4Gr              	! [           1]	_gN_*_m_^-3         	NH4_cone_in_groundwater
      real(4) cNO3Gr              	! [         0.1]	_gN_*_m_^-3         	NO3_cone_in_groundwater
      real(4) cDepthS             	! [         0.1]	_m_                 	Sediment_depth
      real(4) cCPerDW             	! [         0.4]	_gC_*_gDW_^-1       	C_content_of_organic_matter
      real(4) cRhoIM              	! [     2500000]	_gDW_*_m_^-3        	Density_of_sediment_IM
      real(4) cRhoOM              	! [     1400000]	_gDW_*_m_^-3        	Density_of_sediment_detritus
      real(4) cTmRef              	! [          20]	_oC_                	Reference_temperature
      real(4) cAerR               	! [       0.727]	_m_^(1.5)*_s_^(0.5)*	Coefficient_for_VWind^05
      real(4) cAerLin             	! [      -0.371 ]	_s_*_d_^-1          	Coefficient_for_VWind_(is_negative)
      real(4) cAeSqare            	! [      0.0376]	_s_^2*_m_^-1*_d^-1_ 	Coefficient_for_VWind^2
      real(4) cThetAer            	! [       1.024]	1^[_oC_^-1]         	Temperature_coeff_for_reaeration_(Downing_&_Truesdale_1955)
      real(4) kFloAer             	! [        0.01]	_m_^2*_gDW_^-1      	Reaeration_reduction_coeff_for_floating_plant_biomass
      real(4) cVSetIM             	! [           1]	_m_*_d_^-1          	Max_sedimentation_velocity_of_inert_org_matter_(10)
      real(4) cVSetDe             	! [        0.25]	_m_*_d_^-1          	Max_sedimentation_velocity_of_detritus
      real(4) cThetSet            	! [        1.01]	1^[_oC_^-1]         	Temp_parameter_of_sedimentation
      real(4) cSuspMn             	! [         6.1]	0                   	Minimum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSuspMx             	! [        25.2]	0                   	Maximum_value_of_logistic_empirical_suspended_matter_function
      real(4) cSupSope            	! [         2.1]	_m_                 	Slope_of_logistic_empirical_suspended_matter_function
      real(4) hDethusp            	! [           2]	_m_                 	Half_sat_value_of_depth_in_logistic_empirical_suspended_matter_function
      real(4) cFetcRef            	! [        1000]	_m_                 	Reference_fetch
      real(4) fLutuRef            	! [         0.2]	0                   	Reference_lutum_fraction_(of_sandy_clay_soils)
      real(4) cSuspRef            	! [         0.5]	_gDW_*_m_^-2*_d_^-1 	Reference_suspended_matter_function
      real(4) kVResus             	! [        0.01]	_m_^2*_gDW_^-1      	Rel_resuspension_reduction_per_g_vegetation
      real(4) kTurbish            	! [           1]	_gDW_*_gDW_^-1*_d_^-	Relative_resuspension_by_adult_fish_browsing
      real(4) kResuPMx            	! [        0.25]	_d_^-1              	Max_phytopl_resuspension
      real(4) cReusExp            	! [      -0.379 ]	_gDW_^-1*_m_^2*_d_^1	Exp_par_for_phytopl_resuspension
      real(4) cThetMnW            	! [        1.07]	0                   	Expon_temp_constant_of_mineralization_in_water
      real(4) kDMnDeW             	! [        0.01]	_d_^-1              	Decomposition_constant_of_detritus
      real(4) hO2BOD              	! [           1]	_gO2_*_m_^-3        	Half-sat_oxygen_conc_for_BOD
      real(4) O2PerNO3            	! [         1.5]	0                   	Mol_O2_formed_per_mol_NO3-_ammonified
      real(4) cThetMnS            	! [        1.07]	0                   	Expon_temp_constant_of_sediment_mineralization
      real(4) kDMnDeS             	! [       0.002]	_d_^-1              	Decomposition_constant_of_sediment_detritus
      real(4) fRefrDeS            	! [        0.15]	0                   	Refractory_fraction_of_sed_detritus
      real(4) hNO3Dnit            	! [           2]	_gN_*_m_^-3         	Quadratic_half-sat_NO3_conc_for_denitrification
      real(4) NO3PerC             	! [         0.8]	0                   	Mol_NO3_denitrified_per_mol_C_mineralised
      real(4) kDMnHum             	! [     0.00001]	_d_^-1              	Maximum_decomposition_constant_of_humic_material_(1D-5)
      real(4) kNitrW              	! [         0.1]	_d_^-1              	Nitrification_rate_constant_in_water
      real(4) kNitrS              	! [           1]	_d_^-1              	Nitrification_rate_constant_in_sediment
      real(4) cThtaitr            	! [        1.08]	_oC_                	Temperature_coefficient_of_nitrification
      real(4) O2PerNH4            	! [           2]	0                   	Mol_O2_used_per_mol_NH4+_nitrified
      real(4) hO2Nitr             	! [           2]	_gO2_*_m_^-3        	Half-sat_O2_conc_for_nitrification_in_water
      real(4) kPDifPO4            	! [    7.20E-05]	_m_^2*_d_^-1        	Mol_PO4_diffusion_constant
      real(4) kNDifNO3            	! [    8.60E-05]	_m_^2*_d_^-1        	Mol_NO3_diffusion_constant
      real(4) kNDifNH4            	! [    1.12E-04]	_m_^2*_d_^-1        	Mol_NH4_diffusion_constant
      real(4) kO2Dif              	! [    2.60E-05]	_m_^2*_d_^-1        	Mol_O2_diffusion_constant
      real(4) cThetDif            	! [        1.02]	_oC_                	Temperature_coefficient_for_diffusion
      real(4) fDethifS            	! [         0.5]	0                   	Nutrient_diffusion_distance_as_fraction_of_sediment_depth
      real(4) cTubDNut            	! [           5]	0                   	Bioturbation_factor_for_diffusion
      real(4) cTubDfO2            	! [           5]	0                   	Bioturbation_factor_for_diffusion
      real(4) kPSorp              	! [        0.05]	_d_^-1              	P_sorption_rate_constant_not_too_high_->_model_speed
      real(4) cRelPdsD            	! [     0.00003]	_gP_*_gDW_^-1       	Max_P_adsorption_per_g_DW
      real(4) cRePAsFe            	! [       0.065]	_gP_*_gFe_^-1       	Max_P_adsorption_per_g_Fe
      real(4) cRePAsAl            	! [       0.134]	_gP_*_gAl_^-1       	Max_P_adsorption_per_g_Al
      real(4) cKPAdsOx            	! [         0.6]	_m_^3*_gP_^-1       	P_adsorption_affinity_at_oxidized_conditions
      real(4) fRedMx              	! [         0.9]	0                   	Max_reduction_factor_of_P_adsorption_affinity
      real(4) coPO4Mx             	! [           1]	_gP_*_m_^-3         	Max_SRP_conc_in_pore_water
      real(4) kPChePO4            	! [        0.03]	_m_*_d_^-1          	Chem_PO4_loss_rate
      real(4) cDayMnV1            	! [   -1.00E+07 ]	_d_                 	First_mowing_day_(default_non-existent)
      real(4) cDayMnV2            	! [   -1.00E+07 ]	_d_                 	Second_mowing_day_(Note_259_=_16_Sep)
      real(4) fManV               	! [           0]	0                   	Fraction_removed_by_management_for_submerged_plants
      real(4) cLengMan            	! [          10]	_d_                 	Length_of_mowing_period
      real(4) cYetards            	! [           0]	_y_                 	First_year_of_birds_presence
      real(4) cDaarrds            	! [          46]	_d_                 	Yearly_first_day_of_birds_presence
      real(4) cDandrds            	! [         288]	_d_                 	Yearly_last_day_of_birds_presence
      real(4) cBidsrha            	! [           0]	_coot_*_ha_^-1      	Number_of_birds_per_ha_vegetated_lake_(Default_=_0)
      real(4) cDGzPird            	! [          45]	_gDW_*_coot_^-1*_d_^	Daily_grazing_of_birds
      real(4) hDVBird             	! [           5]	_gDW_*_m_^-2        	Half-sat_vegetation_biomass_for_birds_grazing
      real(4) fDAssird            	! [         0.5]	0                   	Birds_assim_efficiency
      real(4) fDiEgird            	! [        0.25]	0                   	Fraction_dissolved_nutrient_of_coot_egestion
      real(4) fDissMV             	! [        0.25]	0                   	Fraction_dissolved_nutrients_from_died_plants
      real(4) cLengllo            	! [          15]	_d_                 	Duration_of_allocation_and_reallocation_phase
      real(4) cLengM              	! [          15]	_d_                 	Duration_of_autumn_mortality_period
      real(4) UseEmpU             	! [           0]	0                   	False_=_do_not_use_this_empirical_relation
      real(4) fSedUVMx            	! [       0.998]	0                   	Maximum_sediment_fraction_of_nutrient_uptake
      real(4) fSeUVoef            	! [        2.66]	0                   	Sigm_regr_coeff_for_sediment_fraction_of_nutrient_uptake
      real(4) fSedUExp            	! [       -0.83 ]	0                   	Exponent_in_sigm_regr_for_sediment_fraction_of_nutrient_uptake
      real(4) fRVSum              	! [         0.1]	_gDW_*_gDW_^-1      	Root_fraction_outside_growing_season
      real(4) fRVWin              	! [         0.6]	_gDW_*_gDW_^-1      	Root_fraction_outside_growing_season
      real(4) fFloatV             	! [           0]	_gDW_*_gDW_^-1      	Floating_fraction_of_shoot
      real(4) fEmergV             	! [           0]	_gDW_*_gDW_^-1      	Emergent_fraction_of_shoot
      real(4) cVHeight            	! [           3]	_m_                 	Height_of_vegetation_in_summer
      real(4) cVHeW               	! [           2]	_m_                 	Height_of_vegetation_in_Winter
      real(4) cVMHe               	! [           3]	_m_                 	Maximum_height_of_vegetation
      real(4) cDStemV             	! [        1.97]	_gDW_*_m_^-1        	Weight_of_stem_per_m_of_length_grown
      real(4) cDensV              	! [          10]	_m_^-2              	Density_of_vegetation_stems_per_m2
      real(4) fDepth1V            	! [           0]	0                   	Max_upper_depth_of_submerged_veget_layer_as_fraction_of_water_depth
      real(4) fDepth2V            	! [           1]	0                   	Max_lower_depth_of_submerged_veget_layer_as_fraction_of_water_depth
      real(4) cDLayerV            	! [           0]	_gDW_*_m_^-2        	Biomass_of_a_single_layer_floating_leaves
      real(4) cCovSpV             	! [         0.5]	_gDW_^-1*_m_^2      	Specific_cover
      real(4) kMigrV              	! [       0.001]	_d_^-1              	Vegetation_migration_rate
      real(4) cDVIn               	! [           1]	_gDW_*_m_^-2        	External_vegetation_density
      real(4) cTMInitV            	! [           9]	_oC_                	Temperature_for_initial_growth
      real(4) cPhInitV            	! [          12]	_h_                 	Hours_in_day_for_initial_growth__photoperiod_
      real(4) cDCarrV             	! [         400]	_gDW_*_m_^-2        	Max_vegetation_standing_crop
      real(4) cMuMxV              	! [         0.2]	_gDW_*_gDW_^-1*_d_^-	Maximum_growth_rate_of_vegetation_at_20oC
      real(4) cQ10PodV            	! [         1.2]	0                   	Temperature_quotient_of_production
      real(4) hLRefV              	! [          13]	_J_*_s_^-1*_m_^-2   	Half-sat_light_at_20_oC
      real(4) cExtSpV             	! [        0.01]	_m_^2*_gDW_^-1      	Specific_extinction_of_vegetation
      real(4) kDRespV             	! [       0.016]	_d_^-1              	Dark_respiration_rate_of_vegetation
      real(4) cQ10RspV            	! [           2]	0                   	Temperature_quotient_of_respiration
      real(4) kMVSum              	! [       0.005]	_d_^-1              	Vegetation_mortality_rate_in_Spring_and_Summer_(low)
      real(4) fWinV               	! [         0.3]	0                   	Fraction_surviving_in_winter
      real(4) cDayWinV            	! [         259]	_d_                 	End_of_growing_season_=_16_Sep
      real(4) fDeWMV              	! [         0.1]	0                   	Fraction_of_shoot_mortality_becoming_water_detritus
      real(4) cPrfVird            	! [           1]	0                   	Edibility_for_birds
      real(4) cVPUMxV             	! [        0.01]	_gP_*_gDW_^-1*_d_^-1	Maximum_P_uptake_capacity_of_vegetation
      real(4) cAfPUV              	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_P_uptake_affinity_vegetation
      real(4) cPDVMn              	! [      0.0008]	_gP_*_gDW_^-1       	Minimum_P/day_ratio_vegetation
      real(4) cPDVMx              	! [      0.0035]	_gP_*_gDW_^-1       	Maximum_P/day_ratio_vegetation
      real(4) cVNUMxV             	! [         0.1]	_gN_*_gDW_^-1*_d_^-1	Maximum_N_uptake_capacity_of_vegetation
      real(4) cAfNUV              	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_N_uptake_affinity_vegetation
      real(4) cNDVMn              	! [        0.01]	_gN_*_gDW_^-1       	Minimum_N/day_ratio_vegetation
      real(4) cNDVMx              	! [       0.035]	_gN_*_gDW_^-1       	Maximum_N/day_ratio_vegetation
      real(4) cPACofMn            	! [         1.5]	0                   	Minimum_Poole-Atkins_coefficient
      real(4) cPACofMx            	! [         2.5]	0                   	Maximum_Poole-Atkins_coefficient
      real(4) hPACoef             	! [           3]	_gDW_*_m_^-3        	Decrease_constant_for_PA_coeff_with_DOMW
      real(4) cSechlus            	! [           0]	_m_                 	Maximum_Secchi_depth_above_water_depth
      real(4) cEuph               	! [         1.7]	0                   	Conversion_constant_Secchi_depth_->_euphotic_depth
      real(4) cCovSpP             	! [           2]	_gDW_^-1*_m_^2      	Specific_coverage_Tentative
      real(4) cTmptoss            	! [          25]	_oC_                	Optimum_temp_for_grazing
      real(4) cSiTmoss            	! [          13]	_oC_                	Temperature_constant_of_grazing(sigma_in_Gaussian_curve)
      real(4) fDissMP             	! [         0.2]	0                   	Soluble_nutrient_fraction_of_died_Algae
      real(4) fDissoss            	! [        0.25]	0                   	Dissolved_nutrient_fraction_of_grazing_loss
      real(4) cMuMxDi             	! [           2]	_d_^-1              	Maximum_growth_rate_Diatoms
      real(4) cTmOptDi            	! [          18]	_oC_                	Optimum_temp_diatoms
      real(4) cSigTmDi            	! [          20]	_oC_                	Temperature_constant_diatoms(sigma_in_Gaussian_curve)
      real(4) cExtSpDi            	! [        0.25]	_m_^2*_gDW_^-1      	Specific_extinction_Diatoms
      real(4) UseteeDi            	! [           1]	0                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) cLOtRfDi            	! [          54]	_J_*_s_^-1*_m_^-2   	Optimum_PAR_for_Diatoms_at_20_oC(Steele_function)
      real(4) hLRefDi             	! [        1000]	_J_*_s_^-1*_m_^-2   	Half-sat_PAR_at_20_oC(Lehmann_function)_Fake_value
      real(4) cChDDiMn            	! [       0.004]	_gChl_*_gDW_^-1     	Min_chlorophyll/C_ratio_Diatoms
      real(4) cChDDiMx            	! [       0.012]	_gChl_*_gDW_^-1     	Max_chlorophyll/C_ratio_Diatoms
      real(4) kDRespDi            	! [         0.1]	_d_^-1              	Maintenance_respiration_constant_diatoms(=_005_*_MuMax)
      real(4) kLossDi             	! [        0.25]	_d_^-1              	Grazing_loss_rate_for_Diatoms
      real(4) kMDiW               	! [        0.01]	_d_^-1              	Mortality_constant_of_Diatoms_in_water
      real(4) kMDiS               	! [        0.05]	_d_^-1              	Mortality_constant_of_sed_Diatoms
      real(4) cVSetDi             	! [         0.5]	_m_*_d_^-1          	Sedimentation_velocity_Diatoms
      real(4) cVPUMxDi            	! [        0.01]	_gP_*_gDW_^-1*_d_^-1	Maximum_P_uptake_capacity_of_Diatoms
      real(4) cAfPUDi             	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_P_uptake_affinity_Diatoms
      real(4) cPDDiMn             	! [      0.0005]	_gP_*_gDW_^-1       	Minimum_P/day_ratio_Diatoms
      real(4) cPDDiMx             	! [       0.005]	_gP_*_gDW_^-1       	Max_P/day_ratio_Diatoms
      real(4) cVNUMxDi            	! [        0.07]	_gN_*_gDW_^-1*_d_^-1	Maximum_N_uptake_capacity_of_Diatoms
      real(4) cAfNUDi             	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_N_uptake_affinity_Diatoms
      real(4) cNDDiMn             	! [        0.01]	_gN_*_gDW_^-1       	Minimum_N/day_ratio_Diatoms
      real(4) cNDDiMx             	! [        0.05]	_gN_*_gDW_^-1       	Max_N/day_ratio_Diatoms
      real(4) hSiAssDi            	! [        0.09]	_gSi_*_m_^-3        	Half_sat_Si_for_diatoms
      real(4) cMuMxren            	! [         1.5]	_d_^-1              	Maximum_growth_rate_greens
      real(4) cTmptren            	! [          25]	_oC_                	Optimum_temp_of_greens
      real(4) cSiTmren            	! [          15]	_oC_                	Temperature_constant_greens(sigma_in_Gaussian_curve)
      real(4) cExSpren            	! [        0.25]	_m_^2*_gDW_^-1      	Specific_extinction_greens
      real(4) Useeeren            	! [           0]	0                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) hLRefren            	! [          17]	_J_*_s_^-1*_m_^-2   	Half-sat_PAR_for_green_algae_at_20_oC(Lehmann_function)
      real(4) cLOReren            	! [        1000]	_J_*_s_^-1*_m_^-2   	Optimum_PAR_at_20_oC(Steele_function)_Fake_value
      real(4) cChGrnMn            	! [        0.01]	_gChl_*_gDW_^-1     	Min_chlorophyll/C_ratio_greens
      real(4) cChGrnMx            	! [        0.02]	_gChl_*_gDW_^-1     	Max_chlorophyll/C_ratio_greens
      real(4) kDRspren            	! [       0.075]	_d_^-1              	Maintenance_respiration_constant_greens(=_005_*_MuMax)
      real(4) kLossren            	! [        0.25]	_d_^-1              	Grazing_loss_rate_for_greens
      real(4) kMGrenW             	! [        0.01]	_d_^-1              	Mortality_constant_of_Diatoms_in_water
      real(4) kMGrenS             	! [        0.05]	_d_^-1              	Mortality_constant_greens
      real(4) cVSetren            	! [         0.2]	_m_*_d_^-1          	Sedimentation_velocity_of_greens
      real(4) cVPMxren            	! [        0.01]	_gP_*_gDW_^-1*_d_^-1	Maximum_P_uptake_capacity_of_greens
      real(4) cAfPUren            	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_P_uptake_affinity_greens
      real(4) cPDGrnMn            	! [      0.0015]	_gP_*_gDW_^-1       	Minimum_P/day_ratio_greens
      real(4) cPDGrnMx            	! [       0.015]	_gP_*_gDW_^-1       	Max_P/day_ratio_greens
      real(4) cVNMxren            	! [        0.07]	_gN_*_gDW_^-1*_d_^-1	Maximum_N_uptake_capacity_of_greens
      real(4) cAfNUren            	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_N_uptake_affinity_greens
      real(4) cNDGrnMn            	! [        0.02]	_gN_*_gDW_^-1       	Minimum_N/day_ratio_greens
      real(4) cNDGrnMx            	! [         0.1]	_gN_*_gDW_^-1       	Max_N/day_ratio_greens
      real(4) hSissren            	! [           0]	_gSi_*_m_^-3        	Half-sat_Si_conc_for_growth_of_green_algae_=_0
      real(4) cMuMxlue            	! [         0.6]	_d_^-1              	Maximum_growth_rate_Bluegreens
      real(4) cTmptlue            	! [          25]	_oC_                	Optimum_temp_blue-greens
      real(4) cSiTmlue            	! [          12]	_oC_                	Temperature_constant_blue-greens(sigma_in_Gaussian_curve)
      real(4) cExSplue            	! [        0.35]	_m_^2*_gDW_^-1      	Specific_extinction_Bluegreens
      real(4) Useeelue            	! [           1]	0                   	Flag_1_=_use_Steele_function0_=_use_Lehman_function
      real(4) cLORelue            	! [        13.6]	_J_*_s_^-1*_m_^-2   	Optimum_PAR_for_blue-greens_at_20_oC(Steele_function)
      real(4) hLReflue            	! [        1000]	_J_*_s_^-1*_m_^-2   	Half-sat_PAR_at_20_oC(Lehmann_function)_Fake_value
      real(4) cChBleMn            	! [       0.005]	_gChl_*_gDW_^-1     	Min_chlorophyll/C_ratio_Bluegreens
      real(4) cChBleMx            	! [       0.015]	_gChl_*_gDW_^-1     	Max_chlorophyll/C_ratio_Bluegreens
      real(4) cCyBleMn            	! [       0.004]	_gChl_*_gDW_^-1     	Min_c-phycocyanin/C_ratio_Bluegreens
      real(4) cCyBleMx            	! [        0.06]	_gChl_*_gDW_^-1     	Max_c-phycocyanin/C_ratio_Bluegreens
      real(4) kDRsplue            	! [        0.03]	_d_^-1              	Maintenance_respiration_constant_blue-greens(=_005_*_MuMax)
      real(4) kLosslue            	! [        0.03]	_d_^-1              	Grazing_loss_rate_for_Blue-greens
      real(4) kMBlueW             	! [        0.01]	_d_^-1              	Mortality_constant_of_blue-greens_in_water
      real(4) kMBlueS             	! [         0.2]	_d_^-1              	Mortality_constant_Bluegreens
      real(4) cVSetlue            	! [        0.06]	_m_*_d_^-1          	Sedimentation_velocity_Blue-greens
      real(4) cVPMxlue            	! [        0.04]	_gP_*_gDW_^-1*_d_^-1	Maximum_P_uptake_capacity_of_Bluegreens
      real(4) cAfPUlue            	! [         0.8]	_m_^3*_gDW_^-1*_d_^-	Initial_P_uptake_affinity_Bluegreens
      real(4) cPDBleMn            	! [      0.0025]	_gP_*_gDW_^-1       	Minimum_P/day_ratio_Bluegreens
      real(4) cPDBleMx            	! [       0.025]	_gP_*_gDW_^-1       	Max_P/day_ratio_blue-greens
      real(4) cVNMxlue            	! [        0.07]	_gN_*_gDW_^-1*_d_^-1	Maximum_N_uptake_capacity_of_Bluegreens
      real(4) cAfNUlue            	! [         0.2]	_m_^3*_gDW_^-1*_d_^-	Initial_N_uptake_affinity_Bluegreens
      real(4) cNDBleMn            	! [        0.03]	_gN_*_gDW_^-1       	Minimum_N/day_ratio_Bluegreens
      real(4) cNDBleMx            	! [        0.15]	_gN_*_gDW_^-1       	Max_N/DW_ratio_blue-greens
      real(4) fFiJvMH             	! [           0]	0                   	Fraction_of_mortality_FiJv_to_hypolimnion
      real(4) fFiJVesH            	! [         0.1]	0                   	Fraction_of_egested_FiJv_to_hypolimnion
      real(4) fFivEcrH            	! [         0.1]	0                   	Fraction_of_excreted_FiJv_to_hypolimnion
      real(4) fFiAdMH             	! [           0]	0                   	Fraction_of_mortality_FiAd_to_hypolimnion
      real(4) fFidEesH            	! [         0.9]	0                   	Fraction_of_egested_FiAd_to_hypolimnion
      real(4) fFidEcrH            	! [         0.9]	0                   	Fraction_of_excreted_FiAd_to_hypolimnion
      real(4) fPiscMH             	! [           0]	0                   	Fraction_of_mortality_Pisc_to_hypolimnion
      real(4) fPicEesH            	! [         0.5]	0                   	Fraction_of_egested_Pisc_to_hypolimnion
      real(4) fPicEcrH            	! [         0.5]	0                   	Fraction_of_excreted_Pisc_to_hypolimnion
      real(4) hSisslue            	! [           0]	_gSi_*_m_^-3        	Half-sat_Si_conc_for_growth_of_blue-greens_=_0
      real(4) cDBentIn            	! [        0.01]	_gDW_*_m_^-2        	External_zoobenthos_density
      real(4) kMigrent            	! [       0.001]	_d_^-1              	Zoobenthos_migration_rate
      real(4) kMigrish            	! [       0.001]	_d_^-1              	Fish_migration_rate
      real(4) cDFiJvIn            	! [       0.005]	_gDW_*_m_^-2        	External_fish_density
      real(4) cDFiAdIn            	! [       0.005]	_gDW_*_m_^-2        	External_fish_density
      real(4) kHaFiWin            	! [           0]	_d_^-1              	Fish_harvesting_fraction_in_winter
      real(4) kHaFiSum            	! [           0]	_d_^-1              	Fish_harvesting_fraction_in_summer
      real(4) cDPiscIn            	! [       0.001]	_gDW_*_m_^-2        	External_Pi_sc_density
      real(4) kMigrisc            	! [       0.001]	_d_^-1              	_Pi_sc_migration_rate
      real(4) kHaPiWin            	! [           0]	_d_^-1              	_Pi_sc_harvesting_fraction_in_winter
      real(4) kHaPiSum            	! [           0]	_d_^-1              	_Pi_sc_harvesting_fraction_in_summer
      real(4) cFiltMx             	! [         4.5]	_m_^3*_gDW_^-1*_d_^-	Maximum_filtering_rate(when_DOMW=0)
      real(4) hFilt               	! [           1]	_gDW_*_m_^-3        	Half-sat_food_conc_for_filtering
      real(4) cDCarZoo            	! [          25]	_gSi_*_m_^-3        	Carrying_capacity_of_zooplankton
      real(4) cPrefDi             	! [        0.75]	0                   	Selection_factor_for_Diatoms
      real(4) cPrefren            	! [        0.75]	0                   	Selection_factor_for_Greens
      real(4) cPreflue            	! [       0.125]	0                   	Selection_factor_for_Bluegreens_Cal
      real(4) cPrefDe             	! [        0.25]	0                   	Selection_factor_for_detritus
      real(4) fDAssZoo            	! [        0.35]	0                   	DW-assimilation_efficiency_of_herb_zooplankton
      real(4) fDiEgZoo            	! [        0.25]	0                   	Soluble_nutrient_fraction_of_by_herb_zoopl_egested_food
      real(4) kDResZoo            	! [        0.15]	_d_^-1              	Maintenance_respiration_constant_herb_zooplankton
      real(4) kMZoo               	! [        0.04]	_d_^-1              	Mortality_constant_herb_zooplankton
      real(4) fDissZoo            	! [         0.1]	0                   	Soluble_nutrient_fraction_of_died_zooplankton
      real(4) cTmOpZoo            	! [          25]	_oC_                	Optimum_temp_zooplankton
      real(4) cSigTZoo            	! [          13]	_oC_                	Temperature_constant_zooplankton(sigma_in_Gaussian_curve)
      real(4) cDCrrent            	! [          10]	_gDW_*_m_^-2        	Carrying_capacity_of_zoobenthos
      real(4) kDAssent            	! [         0.1]	_d_^-1              	Maximum_assimilation_rate
      real(4) hDFodent            	! [         200]	_gDW_*_m_^-2        	Half-saturating_food_for_zoobenthos
      real(4) fDAssent            	! [         0.3]	0                   	C_ass_efficiency_of_zoobenthos
      real(4) fDiEgent            	! [        0.25]	0                   	Soluble_nutrient_fraction_of_by_zoobenthos_egested_food
      real(4) kDRspent            	! [       0.005]	_d_^-1              	Maint_respiration_constant_of_zoobenthos
      real(4) kMBent              	! [       0.005]	_d_^-1              	Mortality_constant_of_zoobenthos
      real(4) fDisMent            	! [         0.1]	0                   	Soluble_P_fraction_of_died_zoobenthos_P
      real(4) cTmptent            	! [          25]	_oC_                	Optimum_temp_of_zoobenthos
      real(4) cSiTment            	! [          16]	_oC_                	Temperature_constant_of_zoobenthos(sigma_in_Gaussian_curve)
      real(4) fDBone              	! [        0.35]	0                   	Fraction_of_fish_C_fixed_in_bones_and_scales
      real(4) fPBone              	! [         0.5]	0                   	Fraction_of_fish_P_fixed_in_bones_and_scales
      real(4) cDCrrish            	! [          15]	_gDW_*_m_^-2        	Carrying_capacity_of_fish(=_100_gFW/m2Grimm_1983)
      real(4) fDiEgish            	! [        0.25]	0                   	Soluble_nutrient_fraction_of_by_fish_egested_food
      real(4) fDisMish            	! [         0.1]	0                   	Soluble_nutrient_fraction_of_died_fish(excl_bones_and_scales
      real(4) cTmptish            	! [          25]	_oC_                	Optimum_temp_of_fish
      real(4) cSiTmish            	! [          10]	_oC_                	Temperature_constant_of_fish(sigma_in_Gaussian_curve)
      real(4) cDaepish            	! [         120]	0                   	Reproduction_date_of_fish_=_1_May
      real(4) fReprish            	! [        0.02]	0                   	Yearly_reproduction_fraction_of_adult_fish
      real(4) fAgeFish            	! [         0.5]	0                   	Yearly_ageing_fraction_of_young_fish
      real(4) cRelVish            	! [       0.009]	0                   	Decrease_of_fish_feeding_per_%_vegetation_cover(max_001)
      real(4) kDAssiJv            	! [        0.12]	_d_^-1              	Maximum_assimilation_rate_of_young_fish
      real(4) hDZooiJv            	! [        1.25]	_gDW_*_m_^-2        	Half-saturating_zooplankton_biomass_for_young_fish_predation
      real(4) fDAssiJv            	! [         0.4]	0                   	C_assimilation_efficiency_of_young_fish
      real(4) kDRspiJv            	! [        0.01]	_d_^-1              	Maintenance_respiration_constant_of_young_fish
      real(4) kMFiJv              	! [     0.00137]	_d_^-1              	Specific_mortality_of_young_fish(=_01_y-1)
      real(4) kDAssiAd            	! [        0.06]	_d_^-1              	Maximum_assimilation_rate_of_adult_fish
      real(4) hDBntiAd            	! [         2.5]	_gDW_*_m_^-2        	Half-saturating_zoobenthos_biomass_for_adult_fish_predation
      real(4) fDAssiAd            	! [         0.4]	0                   	C_assimilation_efficiency_of_adult_fish
      real(4) kDRspiAd            	! [       0.004]	_d_^-1              	Maintenance_respiration_constant_of_adult_fish
      real(4) kMFiAd              	! [     0.00027]	_d_^-1              	Specific_mortality_of_adult_fish(=_01_y-1)
      real(4) cDCrPcMx            	! [         1.2]	_gDW_*_m_^-2        	Maximum_carrying_capacity_of_Pi_sc(=75_kg/ha)
      real(4) cDCrPcMn            	! [         0.1]	_gDW_*_m_^-2        	Minimum_carrying_capacity_of_Pi_sc(=6_kg/ha)
      real(4) cDCPiare            	! [         0.1]	_gDW_*_m_^-2        	Carrying_capacity_of_Pi_sc_for_lake_without_marsh_zone
      real(4) cDPaMisc            	! [          50]	_gDW_*_m_^-2        	Min_reed_biomass_for_Pi_sc
      real(4) cCovVMn             	! [          40]	0                   	Min_submveg_coverage_for_Pi_sc
      real(4) cTmOptV             	! [          20]	_oC_                	Optimum_temperature_of_vegetation
      real(4) cSigTmV             	! [          20]	_oC_                	Temperature_constant_vegetation(sigma_in_Gaussian_curve)
      real(4) cRehrisc            	! [       0.075]	_gDW_*_m_^-2        	Rel_Pi_sc_density_per_%_reed_if_subm_veg_absent
      real(4) cRelVisc            	! [        0.03]	_gDW_*_m_^-2        	Extra_rel_Pi_sc_density_per_%_reed_if_aCovVeg_>_cCovVegMin
      real(4) kDAssisc            	! [       0.025]	_d_^-1              	Maximum_assimilation_rate
      real(4) hDVPisc             	! [           5]	_gDW_*_m_^-2        	Half-sat_vegetation_biomass_for_Pi_sc_growth
      real(4) hDFshisc            	! [           1]	_gDW_*_m_^-2        	Half-saturating_DFish_for_Pi_sc_predation
      real(4) fDAssisc            	! [         0.4]	0                   	C_ass_efficiency_of_Pi_sc
      real(4) fDiEgisc            	! [        0.25]	0                   	Soluble_P_fraction_of_by_fish_egested_food
      real(4) kDRspisc            	! [       0.005]	_d_^-1              	Maint_respiration_constant_of_Pi_sc
      real(4) kMPisc              	! [     0.00027]	_d_^-1              	Specific_mortality_of_Pi_sc_=_01_y-1
      real(4) fDisMisc            	! [         0.1]	0                   	Soluble_nutrient_fraction_of_died_Pi_sc(excl_bones_and_scales
      real(4) cTmptisc            	! [          25]	_oC_                	Optimum_temp_of_Pi_sc
      real(4) cSiTmisc            	! [          10]	_oC_                	Temperature_constant_of_Pi_sc(sigma_in_Gaussian_curve)
      real(4) cDepthSM            	! [         0.1]	_m_                 	Sediment_depth
      real(4) kExchMxM            	! [           1]	_m_^3*_m_^-3*_d_^-1 	Maximum_dispersive_marsh_water_exchange_coefficient
      real(4) hfMarsh             	! [         0.1]	_m_^2*_m_^-2        	Rel_marsh_area_where_exchange_is_50%
      real(4) fDTotSM0            	! [         0.3]	_gDW_*_gDW_^-1      	Initial_dry-weight_fraction_in_sediment
      real(4) fDOrgSM0            	! [         0.1]	_gDW_*_gDW_^-1      	Initial_organic_fraction_of_sed
      real(4) fDDeSM0             	! [        0.05]	_gDW_*_gDW_^-1      	Initial_detritus_fraction_of_sediment_organic_matter
      real(4) fPIorSM0            	! [      0.0005]	_gP_*_gDW_^-1       	Initial_inorg_P_fraction_in_sed
      real(4) cPDPhra0            	! [       0.002]	_gP_*_gDW_^-1       	Initial_P/day_ratio_of_reed
      real(4) cNDPhra0            	! [        0.02]	_gN_*_gDW_^-1       	Initial_N/day_ratio_of_reed
      real(4) cDeSthra            	! [        61.5]	_m_^-2              	Density_stem(+/-_139)
      real(4) cTMithra            	! [           8]	_oC_                	Temp_start_initial_growth
      real(4) fDAllhra            	! [         0.3]	0                   	Allocation_fraction
      real(4) kDAllhra            	! [        0.05]	_d_^-1              	Allocation_rate
      real(4) cDSemhra            	! [           6]	_gDW_*_m_^-1        	Average_stem_weight
      real(4) cQ1rohra            	! [           2]	0                   	Temp_quotient_of_production
      real(4) cMuPhaMx            	! [        0.03]	_d_^-1              	Maximum_growth_rate_reed
      real(4) cDSPhaMx            	! [        3500]	_gDW_*_m_^-2        	Max_shoot_biomass_of_reed
      real(4) cCoSphra            	! [         0.1]	_gDW_^-1*_m_^-2     	Specific_coverage
      real(4) cPDPhaMn            	! [      0.0008]	0                   	MinPhra_P/day_-ratio
      real(4) cPDPhaMx            	! [       0.003]	0                   	MaxPhra_P/day_-ratio
      real(4) cNDPhaMn            	! [       0.008]	0                   	MinPhra_N/day_-ratio
      real(4) cNDPhaMx            	! [        0.03]	0                   	MaxPhra_N/day_-ratio
      real(4) cAfNUhra            	! [      0.0002]	_m_^3*_gDW_^-1*_d_^-	N_uptake_affinity_reed
      real(4) cAfPUhra            	! [      0.0002]	_m_^3*_gDW_^-1*_d_^-	P_uptake_affinity_reed
      real(4) cVNPhaMx            	! [         0.1]	_gN_*_gDW_^-1*_d_^-1	Max_uptake_rate_N_001
      real(4) cVPPhaMx            	! [        0.01]	_gP_*_gDW_^-1       	Max_uptake_rate_P_0001
      real(4) kDRsphra            	! [       0.001]	_d_^-1              	Respiration_rate_of_reed
      real(4) cQ1eshra            	! [         2.5]	1^[_oC_^-1]         	Temp_quotient_of_respiration
      real(4) fDayWin             	! [        0.52]	0                   	Start_autumn
      real(4) fDRalhra            	! [        0.85]	0                   	Reallocated_fraction_day
      real(4) kDRalhra            	! [        0.05]	_d_^-1              	Reallocation_rate_day
      real(4) kDMShhra            	! [           0]	_d_^-1              	Mortality_rate_shoots
      real(4) kDMRPhra            	! [    3.91E-04]	_d_^-1              	Mortality_rate_roots
      real(4) cDaWihra            	! [         259]	_d_                 	Begin_autumn(16_Sept)
      real(4) cDaMahra            	! [         255]	_d_                 	Time_of_management
      real(4) fManPhra            	! [           0]	_d_^-1              	Fraction_biomass_loss_by_management
      real(4) kDMnShra            	! [           1]	_d_^-1              	Rate_of_management
      real(4) DayPeear            	! [         365]	_d_*_y_^-1          	Days_Per_Year
      real(4) TenDays             	! [          10]	_d_                 	Ten_Days
      real(4) HousIDay            	! [          24]	_h_                 	Hours_in_day
      real(4) HousPDay            	! [          24]	_h_*_d_^-1          	Hours_per_day
      real(4) SecPeDay            	! [       86400]	_s_*_d_^-1          	SecsPerDay
      real(4) mmPerm              	! [        1000]	_mm_*_m_^-1         	MmPerm
      real(4) m2Perha             	! [       10000]	_m_^2*_ha_^-1       	M2Perha
      real(4) mgPerg              	! [        1000]	_mgChl_*_gChl_^-1   	MgPerg
      real(4) gPerkg              	! [        1000]	_g_*_kg_            	GPerkg
      real(4) gPerton             	! [     1000000]	_g_*_ton_^-1        	GPerton
      real(4) PerDay              	! [           1]	_d_^-1              	Per_day
      real(4) PerCent             	! [        0.01]	0                   	PerCent
      real(4) NearZero            	! [       1E-28]	0                   	Very_small_number_used_to_avoid_dividing_by_zero_
      real(4) molO2olC            	! [      2.6667]	_gO2_*_gC_^-1       	Ratio_of_mol_weights
      real(4) molO2olN            	! [      2.2857]	_gO2_*_gN_^-1       	Ratio_of_mol_weights
      real(4) molNmolC            	! [      1.1667]	_gN_*_gC_^-1        	Ratio_of_mol_weights
      real(4) cRhoWat             	! [     1000000]	_gDW_*_m_^-3        	Density_of_water
      real(4) cSolar              	! [        1368]	_J_*_s_^-1*_m_^-2   	Solar_constant_incl_latitude_module
      real(4) cLAT                	! [          52]	_deg_               	Latitude_incl_latitude_module
      real(4) cPhiR               	! [       0.409]	_rad_               	Tilt_of_the_earths_axis_incl_latitude_module
      real(4) cDayR               	! [         173]	_d_                 	Day_of_summer_solstice_in_the_northern_hemisphere_incl_latitude_module
      real(4) fCloudH             	! [        0.15]	0                   	Fraction_high_clouds_incl_latitude_module
      real(4) fCloudM             	! [        0.15]	0                   	Fraction_middle_clouds_incl_latitude_module
      real(4) fCloudL             	! [        0.15]	0                   	Fraction_low_clouds_incl_latitude_module
      real(4) cTrans              	! [        0.41]	0                   	Atmospheric_transmisivity_incl_latitude_module
      real(4) Pi                  	! [    3.14E+00]	0                   	Pi_(approx_314159)
      real(4) cHeath              	! [         100]	0                   	Evaporation_heath_index_Thornthwaite
      real(4) cAlpha              	! [           2]	0                   	Alpha_evaporation_Thornthwaite
      real(4) cDeMeChl            	! [         0.3]	_m_                 	Depth_measurement_chlorophyll
      real(4) cPMx                	! [         1.3]	0                   	Maximum_PI
      real(4) cDehMaMn            	! [      0.0001]	_m_                 	Minimum_Metaliminon_Depth
      real(4) fMeta               	! [        0.13]	0                   	Metalimnion
      real(4) kBub                	! [         0.4]	_m_*_d_^-1          	Metalimnion
      real(4) cVegHP1             	! [      0.0191]	0                   	Vegetation_height_power_multiplier_for_Haga_Function
      real(4) cVegHP2             	! [      0.7969]	0                   	Vegetation_height_power_operator_for_Haga_Function
      real(4) cVegL               	! [           1]	0                   	Vegetation_light_correction
!
!
!     /* ==============================  */
!     /* declaration auxiliaries         */
!     /* ==============================  */
      real(4) sDepthW              	! Water depth
      real(4) sTime               	! sTime
      real(4) DaysInY             	! DaysInYear
      real(4) aInlSrat            	! aInclStrat
      real(4) uDptMix             	! uDepthMix
      real(4) uDptWE              	! uDepthWEpi
      real(4) uDptWH              	! uDepthWHyp
      real(4) MassH               	! MassHyp
      real(4) MassE               	! MassEpi
      real(4) MassWM              	! MassWM
      real(4) oNH4WH              	! oNH4WHyp
      real(4) oNO3WH              	! oNO3WHyp
      real(4) oPO4WH              	! oPO4WHyp
      real(4) oPAIMWH             	! oPAIMWHyp
      real(4) oSiO2WH             	! oSiO2WHyp
      real(4) oO2WH               	! oO2WHyp
      real(4) oDDtWH              	! oDDetWHyp
      real(4) oNDtWH              	! oNDetWHyp
      real(4) oPDtWH              	! oPDetWHyp
      real(4) oSiDtWH             	! oSiDetWHyp
      real(4) oDIMWH              	! oDIMWHyp
      real(4) oDDiWH              	! oDDiatWHyp
      real(4) oNDiWH              	! oNDiatWHyp
      real(4) oPDiWH              	! oPDiatWHyp
      real(4) oDGrenWH            	! oDGrenWHyp
      real(4) oNGrenWH            	! oNGrenWHyp
      real(4) oPGrenWH            	! oPGrenWHyp
      real(4) oDBlueWH            	! oDBlueWHyp
      real(4) oNBlueWH            	! oNBlueWHyp
      real(4) oPBlueWH            	! oPBlueWHyp
      real(4) oDZooH              	! oDZooHyp
      real(4) oNZooH              	! oNZooHyp
      real(4) oPZooH              	! oPZooHyp
      real(4) oNH4WM              	! oNH4WM
      real(4) oNO3WM              	! oNO3WM
      real(4) oPO4WM              	! oPO4WM
      real(4) oPAIMWM             	! oPAIMWM
      real(4) oSiO2WM             	! oSiO2WM
      real(4) oO2WM               	! oO2WM
      real(4) oDDtWM              	! oDDetWM
      real(4) oNDtWM              	! oNDetWM
      real(4) oPDtWM              	! oPDetWM
      real(4) oSiDtWM             	! oSiDetWM
      real(4) oDIMWM              	! oDIMWM
      real(4) oDDiWM              	! oDDiatWM
      real(4) oNDiWM              	! oNDiatWM
      real(4) oPDiWM              	! oPDiatWM
      real(4) oDGrenWM            	! oDGrenWM
      real(4) oNGrenWM            	! oNGrenWM
      real(4) oPGrenWM            	! oPGrenWM
      real(4) oDBlueWM            	! oDBlueWM
      real(4) oNBlueWM            	! oNBlueWM
      real(4) oPBlueWM            	! oPBlueWM
      real(4) oDZooM              	! oDZooM
      real(4) oNZooM              	! oNZooM
      real(4) oPZooM              	! oPZooM
      real(4) oNH4WE              	! oNH4WEpi
      real(4) oNO3WE              	! oNO3WEpi
      real(4) oPO4WE              	! oPO4WEpi
      real(4) oPAIMWE             	! oPAIMWEpi
      real(4) oSiO2WE             	! oSiO2WEpi
      real(4) oO2WE               	! oO2WEpi
      real(4) oDDtWE              	! oDDetWEpi
      real(4) oNDtWE              	! oNDetWEpi
      real(4) oPDtWE              	! oPDetWEpi
      real(4) oSiDtWE             	! oSiDetWEpi
      real(4) oDIMWE              	! oDIMWEpi
      real(4) oDDiWE              	! oDDiatWEpi
      real(4) oNDiWE              	! oNDiatWEpi
      real(4) oPDiWE              	! oPDiatWEpi
      real(4) oDGrenWE            	! oDGrenWEpi
      real(4) oNGrenWE            	! oNGrenWEpi
      real(4) oPGrenWE            	! oPGrenWEpi
      real(4) oDBlueWE            	! oDBlueWEpi
      real(4) oNBlueWE            	! oNBlueWEpi
      real(4) oPBlueWE            	! oPBlueWEpi
      real(4) oDZooE              	! oDZooEpi
      real(4) oNZooE              	! oNZooEpi
      real(4) oPZooE              	! oPZooEpi
      real(4) TimeYars            	! TimeYears
      real(4) Day                 	! Day
      real(4) Years               	! Years
      real(4) uVeHeght            	! uVegHeight
      real(4) aVeHeght            	! aVegHeight
      real(4) uVeHehtH            	! uVegHeightHyp
      real(4) uVeHehtE            	! uVegHeightEpi
      real(4) aDpt2egH            	! aDepth2VegHyp
      real(4) aDpt1egH            	! aDepth1VegHyp
      real(4) aDpt2egE            	! aDepth2VegEpi
      real(4) aDpt1egE            	! aDepth1VegEpi
      real(4) uVegHL              	! uVegHeightLight
      real(4) uVegHLS             	! uVegHeightLightSum
      real(4) uVeghhtE            	! uVegHeightLightEpi
      real(4) uVeghhtH            	! uVegHeightLightHyp
      real(4) uDVegH              	! uDVegHyp
      real(4) uDVegE              	! uDVegEpi
      real(4) uVegSade            	! uVegShade
      real(4) aExtPytH            	! aExtPhytHyp
      real(4) aExtDtH             	! aExtDetHyp
      real(4) aExtIMH             	! aExtIMHyp
      real(4) aExCofOH            	! aExtCoefOpenHyp
      real(4) aExtPytE            	! aExtPhytEpi
      real(4) aExtDtE             	! aExtDetEpi
      real(4) aExtIME             	! aExtIMEpi
      real(4) aExCofOE            	! aExtCoefOpenEpi
      real(4) aTmE                	! aTmEpi
      real(4) aTmH                	! aTmHyp
      real(4) uTmE                	! uTmEpi
      real(4) uTmH                	! uTmHyp
      real(4) uTmBot              	! uTmBot
      real(4) uLAT                	! uLAT
      real(4) uSoDeAng            	! uSolDecAng
      real(4) uSunRise            	! uSunRise
      real(4) uSunSet             	! uSunSet
      real(4) uSunHour            	! uSunHours
      real(4) uSunPath            	! uSunPath
      real(4) uTrans              	! uTrans
      real(4) ufDay               	! ufDay
      real(4) uLDay               	! uLDay
      real(4) uLOut               	! uLOut
      real(4) uLPARurf            	! uLPARSurf
      real(4) aLPRBtEO            	! aLPARBotEpiOpen
      real(4) aLPRBtHO            	! aLPARBotHypOpen
      real(4) aLPR1egE            	! aLPAR1VegEpi
      real(4) aLPR1egH            	! aLPAR1VegHyp
      real(4) aLPVeght            	! aLPARVegLight
      real(4) uTmVeAve            	! uTmVegAve
      real(4) uFunTVeg            	! uFunTmVeg
      real(4) uFumPVeg            	! uFunTmProdVeg
      real(4) uhLVeg              	! uhLVeg
      real(4) aLPIVeg             	! aLPIVeg
      real(4) aSpring             	! aSpring
      real(4), save :: aTiWiVeg
      real(4), save :: aTiInVeg
      real(4) aVSum               	! aVegSum
      real(4) aVWin               	! aVegWin
      real(4) aDaysSuV            	! aDaysVegSum
      real(4) aDaysWiV            	! aDaysVegWin
      real(4), save :: uDaWiVeg
      real(4), save :: aDaInVeg
      real(4) bfRtVeg             	! bfRootVeg
      real(4) bfShVeg             	! bfShootVeg
      real(4) aDShVeg             	! aDShootVeg
      real(4) aDEerVeg            	! aDEmergVeg
      real(4) aDFoaVeg            	! aDFloatVeg
      real(4) bfSubVeg            	! bfSubVeg
      real(4) aDSubVeg            	! aDSubVeg
      real(4) aExtVegH            	! aExtVegHyp
      real(4) aExtCefH            	! aExtCoefHyp
      real(4) aExtVegE            	! aExtVegEpi
      real(4) aExtCefE            	! aExtCoefEpi
      real(4) aLPARotE            	! aLPARBotEpi
      real(4) uLPRSrfH            	! uLPARSurfHyp
      real(4) aLPR2egE            	! aLPAR2VegEpi
      real(4) aLPR2egH            	! aLPAR2VegHyp
      real(4) uVWind              	! uVWind
      real(4) aStrat              	! aStrat
      real(4) uQISeson            	! uQInSeason
      real(4) uQEvSnus            	! uQEvSinus
      real(4) aQEv                	! aQEv
      real(4) uQEv                	! uQEv
      real(4) uQInEtra            	! uQInExtra
      real(4) uQIn                	! uQIn
      real(4) uQOtEtra            	! uQOutExtra
      real(4) uQOutE              	! uQOutEpi
      real(4) uQOutH              	! uQOutHyp
      real(4) uQDilE              	! uQDilEpi
      real(4) uQDilH              	! uQDilHyp
      real(4) ukDilE              	! ukDilEpi
      real(4) ukDilH              	! ukDilHyp
      real(4) ukDilatE            	! ukDilWatEpi
      real(4) ukDilatH            	! ukDilWatHyp
      real(4) ukOutE              	! ukOutEpi
      real(4) ukOutH              	! ukOutHyp
      real(4) uTauWatE            	! uTauWatEpi
      real(4) uTauWatH            	! uTauWatHyp
      real(4) uTauWat             	! uTauWat
      real(4) uTaSustH            	! uTauSubstHyp
      real(4) uFiJvMH             	! uFiJvMortHyp
      real(4) uFiJvEH             	! uFiJvEgesHyp
      real(4) uFivEcrH            	! uFiJvExcrHyp
      real(4) uFiAdMH             	! uFiAdMortHyp
      real(4) uFiAdEH             	! uFiAdEgesHyp
      real(4) uFidEcrH            	! uFiAdExcrHyp
      real(4) uPiscMH             	! uPiscMortHyp
      real(4) uPiscEH             	! uPiscEgesHyp
      real(4) uPicEcrH            	! uPiscExcrHyp
      real(4) vTranptW            	! vTranDepthW
      real(4) vTranHEW            	! vTranHypEpiW
      real(4) vDeltaWH            	! vDeltaWHyp
      real(4) vDeltaWE            	! vDeltaWEpi
      real(4) afVolHE             	! afVolHypEpi
      real(4) wDAdvIMW            	! wDAdvIMW
      real(4) wPAdvO4W            	! wPAdvPO4W
      real(4) wPAdvIMW            	! wPAdvAIMW
      real(4) wNAdvH4W            	! wNAdvNH4W
      real(4) wNAdvO3W            	! wNAdvNO3W
      real(4) wSidvO2W            	! wSiAdvSiO2W
      real(4) wO2AdvW             	! wO2AdvW
      real(4) wDAdvDtW            	! wDAdvDetW
      real(4) wPAdvDtW            	! wPAdvDetW
      real(4) wNAdvDtW            	! wNAdvDetW
      real(4) wSiAdDtW            	! wSiAdvDetW
      real(4) wDAdvDiW            	! wDAdvDiatW
      real(4) wPAdvDiW            	! wPAdvDiatW
      real(4) wNAdvDiW            	! wNAdvDiatW
      real(4) wSiAdDiW            	! wSiAdvDiatW
      real(4) wDAvGenW            	! wDAdvGrenW
      real(4) wPAvGenW            	! wPAdvGrenW
      real(4) wNAvGenW            	! wNAdvGrenW
      real(4) wDAvBueW            	! wDAdvBlueW
      real(4) wPAvBueW            	! wPAdvBlueW
      real(4) wNAvBueW            	! wNAdvBlueW
      real(4) wDAdvooW            	! wDAdvZooW
      real(4) wPAdvooW            	! wPAdvZooW
      real(4) wNAdvooW            	! wNAdvZooW
      real(4) wDAIMWM             	! wDAdvIMWM
      real(4) wPAPO4WM            	! wPAdvPO4WM
      real(4) wPAAIMWM            	! wPAdvAIMWM
      real(4) wNANH4WM            	! wNAdvNH4WM
      real(4) wNANO3WM            	! wNAdvNO3WM
      real(4) wSiAO2WM            	! wSiAdvSiO2WM
      real(4) wO2AWM              	! wO2AdvWM
      real(4) wDADeWM             	! wDAdvDetWM
      real(4) wPADeWM             	! wPAdvDetWM
      real(4) wNADeWM             	! wNAdvDetWM
      real(4) wSiADeWM            	! wSiAdvDetWM
      real(4) wDADiWM             	! wDAdvDiatWM
      real(4) wPADiWM             	! wPAdvDiatWM
      real(4) wNADiWM             	! wNAdvDiatWM
      real(4) wSiADiWM            	! wSiAdvDiatWM
      real(4) wDAGWM              	! wDAdvGrenWM
      real(4) wPAGWM              	! wPAdvGrenWM
      real(4) wNAGWM              	! wNAdvGrenWM
      real(4) wDABWM              	! wDAdvBlueWM
      real(4) wPABWM              	! wPAdvBlueWM
      real(4) wNABWM              	! wNAdvBlueWM
      real(4) wDAZWM              	! wDAdvZooWM
      real(4) wPAZWM              	! wPAdvZooWM
      real(4) wNAZWM              	! wNAdvZooWM
      real(4) uExMaxW             	! uExchMaxW
      real(4) akExWH              	! akExchWHyp
      real(4) akExWE              	! akExchWEpi
      real(4) wDExIMWH            	! wDExchIMWHyp
      real(4) wPExP4WH            	! wPExchPO4WHyp
      real(4) wPExAMWH            	! wPExchAIMWHyp
      real(4) wNExN4WH            	! wNExchNH4WHyp
      real(4) wNExN3WH            	! wNExchNO3WHyp
      real(4) wSixS2WH            	! wSiExchSiO2WHyp
      real(4) wO2ExWH             	! wO2ExchWHyp
      real(4) wDExDtWH            	! wDExchDetWHyp
      real(4) wPExDtWH            	! wPExchDetWHyp
      real(4) wNExDtWH            	! wNExchDetWHyp
      real(4) wSiExtWH            	! wSiExchDetWHyp
      real(4) wDExDiWH            	! wDExchDiatWHyp
      real(4) wPExDiWH            	! wPExchDiatWHyp
      real(4) wNExDiWH            	! wNExchDiatWHyp
      real(4) wSiExiWH            	! wSiExchDiatWHyp
      real(4) wDEGrnWH            	! wDExchGrenWHyp
      real(4) wPEGrnWH            	! wPExchGrenWHyp
      real(4) wNEGrnWH            	! wNExchGrenWHyp
      real(4) wDEBleWH            	! wDExchBlueWHyp
      real(4) wPEBleWH            	! wPExchBlueWHyp
      real(4) wNEBleWH            	! wNExchBlueWHyp
      real(4) wDExZoWH            	! wDExchZooWHyp
      real(4) wPExZoWH            	! wPExchZooWHyp
      real(4) wNExZoWH            	! wNExchZooWHyp
      real(4) wDExIMWE            	! wDExchIMWEpi
      real(4) wPExP4WE            	! wPExchPO4WEpi
      real(4) wPExAMWE            	! wPExchAIMWEpi
      real(4) wNExN4WE            	! wNExchNH4WEpi
      real(4) wNExN3WE            	! wNExchNO3WEpi
      real(4) wSixS2WE            	! wSiExchSiO2WEpi
      real(4) wO2ExWE             	! wO2ExchWEpi
      real(4) wDExDtWE            	! wDExchDetWEpi
      real(4) wPExDtWE            	! wPExchDetWEpi
      real(4) wNExDtWE            	! wNExchDetWEpi
      real(4) wSiExtWE            	! wSiExchDetWEpi
      real(4) wDExDiWE            	! wDExchDiatWEpi
      real(4) wPExDiWE            	! wPExchDiatWEpi
      real(4) wNExDiWE            	! wNExchDiatWEpi
      real(4) wSiExiWE            	! wSiExchDiatWEpi
      real(4) wDEGrnWE            	! wDExchGrenWEpi
      real(4) wPEGrnWE            	! wPExchGrenWEpi
      real(4) wNEGrnWE            	! wNExchGrenWEpi
      real(4) wDEBleWE            	! wDExchBlueWEpi
      real(4) wPEBleWE            	! wPExchBlueWEpi
      real(4) wNEBleWE            	! wNExchBlueWEpi
      real(4) wDExZoWE            	! wDExchZooWEpi
      real(4) wPExZoWE            	! wPExchZooWEpi
      real(4) wNExZoWE            	! wNExchZooWEpi
      real(4) akExMH              	! akExchMHyp
      real(4) akExLH              	! akExchLHyp
      real(4) oDPhytWH            	! oDPhytWHyp
      real(4) oPPhytWH            	! oPPhytWHyp
      real(4) oNPhytWH            	! oNPhytWHyp
      real(4) aDPhytS             	! aDPhytS
      real(4) aPPhytS             	! aPPhytS
      real(4) aNPhytS             	! aNPhytS
      real(4) oDPhytWE            	! oDPhytWEpi
      real(4) oDOMWE              	! oDOMWEpi
      real(4) oDOMWH              	! oDOMWHyp
      real(4) oTDOMW              	! oTDOMW
      real(4) oDSestWH            	! oDSestWHyp
      real(4) oPOMWH              	! oPOMWHyp
      real(4) oPSestWH            	! oPSestWHyp
      real(4) oPInogWH            	! oPInorgWHyp
      real(4) oPTotWH             	! oPTotWHyp
      real(4) oNDissWE            	! oNDissWEpi
      real(4) oNDissWH            	! oNDissWHyp
      real(4) oNOMWH              	! oNOMWHyp
      real(4) oNSestWH            	! oNSestWHyp
      real(4) oNkjWH              	! oNkjWHyp
      real(4) oNTotWH             	! oNTotWHyp
      real(4) bPorS               	! bPorS
      real(4) bPorCorS            	! bPorCorS
      real(4) aDTotS              	! aDTotS
      real(4) aRhoTotS            	! aRhoTotS
      real(4) aRhSoidS            	! aRhoSolidS
      real(4) afDTotS             	! afDTotS
      real(4) afDOrgS             	! afDOrgS
      real(4) afDtS               	! afDetS
      real(4) afDtTotS            	! afDetTotS
      real(4) aPInorgS            	! aPInorgS
      real(4) aPTtAilS            	! aPTotAvailS
      real(4) aPTotS              	! aPTotS
      real(4) afPInrgS            	! afPInorgS
      real(4) afPTotS             	! afPTotS
      real(4) afPO4S              	! afPO4S
      real(4) oPO4S               	! oPO4S
      real(4) aNDissS             	! aNDissS
      real(4) aNkAvilS            	! aNkjAvailS
      real(4) aNkjS               	! aNkjS
      real(4) aNTtAilS            	! aNTotAvailS
      real(4) aNTotS              	! aNTotS
      real(4) afNInrgS            	! afNInorgS
      real(4) afNTotS             	! afNTotS
      real(4) oNO3S               	! oNO3S
      real(4) oNH4S               	! oNH4S
      real(4) oNDissS             	! oNDissS
      real(4) rPDIMWH             	! rPDIMWHyp
      real(4) rPDIMS              	! rPDIMS
      real(4) rPDDtWH             	! rPDDetWHyp
      real(4) rNDDtWH             	! rNDDetWHyp
      real(4) rSiDDtWH            	! rSiDDetWHyp
      real(4) rPDHumS             	! rPDHumS
      real(4) rNDHumS             	! rNDHumS
      real(4) rPDDtS              	! rPDDetS
      real(4) rNDDtS              	! rNDDetS
      real(4) rSiDDtS             	! rSiDDetS
      real(4) oDPhytWM            	! oDPhytWM
      real(4) oPPhytWM            	! oPPhytWM
      real(4) oNPhytWM            	! oNPhytWM
      real(4) oSiDiWM             	! oSiDiatWM
      real(4) oDOMWM              	! oDOMWM
      real(4) oDSestWM            	! oDSestWM
      real(4) oPOMWM              	! oPOMWM
      real(4) oPSestWM            	! oPSestWM
      real(4) oPInogWM            	! oPInorgWM
      real(4) oPTotWM             	! oPTotWM
      real(4) oNDissWM            	! oNDissWM
      real(4) oNOMWM              	! oNOMWM
      real(4) oNSestWM            	! oNSestWM
      real(4) oNkjWM              	! oNkjWM
      real(4) oNTotWM             	! oNTotWM
      real(4) bPorSM              	! bPorSM
      real(4) bPorCrSM            	! bPorCorSM
      real(4) aDTotSM             	! aDTotSM
      real(4) aRhoTtSM            	! aRhoTotSM
      real(4) aRhSodSM            	! aRhoSolidSM
      real(4) afDTotSM            	! afDTotSM
      real(4) afDOrgSM            	! afDOrgSM
      real(4) afDtSM              	! afDetSM
      real(4) afDtTtSM            	! afDetTotSM
      real(4) aPInogSM            	! aPInorgSM
      real(4) aPTAvlSM            	! aPTotAvailSM
      real(4) aPTotSM             	! aPTotSM
      real(4) afPnogSM            	! afPInorgSM
      real(4) afPTotSM            	! afPTotSM
      real(4) afPO4SM             	! afPO4SM
      real(4) oPO4SM              	! oPO4SM
      real(4) aNDissSM            	! aNDissSM
      real(4) aNkAvlSM            	! aNkjAvailSM
      real(4) aNkjSM              	! aNkjSM
      real(4) aNTAvlSM            	! aNTotAvailSM
      real(4) aNTotSM             	! aNTotSM
      real(4) afNnogSM            	! afNInorgSM
      real(4) afNTotSM            	! afNTotSM
      real(4) oNO3SM              	! oNO3SM
      real(4) oNH4SM              	! oNH4SM
      real(4) oNDissSM            	! oNDissSM
      real(4) rPDIMWM             	! rPDIMWM
      real(4) rPDIMSM             	! rPDIMSM
      real(4) rPDDtWM             	! rPDDetWM
      real(4) rNDDtWM             	! rNDDetWM
      real(4) rSiDDtWM            	! rSiDDetWM
      real(4) rPDHumSM            	! rPDHumSM
      real(4) rNDHumSM            	! rNDHumSM
      real(4) rPDDtSM             	! rPDDetSM
      real(4) rNDDtSM             	! rNDDetSM
      real(4) rSiDDtSM            	! rSiDDetSM
      real(4) aDTotM              	! aDTotM
      real(4) aPTotM              	! aPTotM
      real(4) aNTotM              	! aNTotM
      real(4) aSiTotM             	! aSiTotM
      real(4) aO2TotM             	! aO2TotM
      real(4) uPLdSson            	! uPLoadSeason
      real(4) uPLoad              	! uPLoad
      real(4) uPLoaPO4            	! uPLoadPO4
      real(4) uPLoaOrg            	! uPLoadOrg
      real(4) uPLdPTot            	! uPLoadPhytTot
      real(4) uPLoadDt            	! uPLoadDet
      real(4) uPLoaAIM            	! uPLoadAIM
      real(4) uNLdSson            	! uNLoadSeason
      real(4) uNLdPTot            	! uNLoadPhytTot
      real(4) uNLoad              	! uNLoad
      real(4) uNLoadDt            	! uNLoadDet
      real(4) uNLoaOrg            	! uNLoadOrg
      real(4) uNLadiss            	! uNLoadDiss
      real(4) uNLoaNH4            	! uNLoadNH4
      real(4) uNLoaNO3            	! uNLoadNO3
      real(4) uNBckadH            	! uNBackLoadHyp
      real(4) uPBckadH            	! uPBackLoadHyp
      real(4) uNTotIn             	! uNTotIn
      real(4) uDLoadDt            	! uDLoadDet
      real(4) uDLdPTot            	! uDLoadPhytTot
      real(4) uDLoadIM            	! uDLoadIM
      real(4) uDLoad              	! uDLoad
      real(4) UotIn               	! uPTotIn
      real(4) uDLoadDi            	! uDLoadDiat
      real(4) uPLoadDi            	! uPLoadDiat
      real(4) uNLoadDi            	! uNLoadDiat
      real(4) uDLadren            	! uDLoadGren
      real(4) uPLadren            	! uPLoadGren
      real(4) uNLadren            	! uNLoadGren
      real(4) uDLadlue            	! uDLoadBlue
      real(4) uPLadlue            	! uPLoadBlue
      real(4) uNLadlue            	! uNLoadBlue
      real(4) wDDilIMH            	! wDDilIMHyp
      real(4) wDDilDtH            	! wDDilDetHyp
      real(4) wDDlGenH            	! wDDilGrenHyp
      real(4) wDDlBueH            	! wDDilBlueHyp
      real(4) wDDilDiH            	! wDDilDiatHyp
      real(4) wDDilooH            	! wDDilZooHyp
      real(4) wDDlPytH            	! wDDilPhytHyp
      real(4) wPDilooH            	! wPDilZooHyp
      real(4) wNDilooH            	! wNDilZooHyp
      real(4) wPDilO4H            	! wPDilPO4Hyp
      real(4) wPDilDtH            	! wPDilDetHyp
      real(4) wPDilIMH            	! wPDilAIMHyp
      real(4) wNDilH4H            	! wNDilNH4Hyp
      real(4) wNDilO3H            	! wNDilNO3Hyp
      real(4) wNDilDtH            	! wNDilDetHyp
      real(4) wO2nfowH            	! wO2InflowHyp
      real(4) wO2OuflH            	! wO2OutflHyp
      real(4) wPDilDiH            	! wPDilDiatHyp
      real(4) wNDilDiH            	! wNDilDiatHyp
      real(4) wPDlGenH            	! wPDilGrenHyp
      real(4) wNDlGenH            	! wNDilGrenHyp
      real(4) wPDlBueH            	! wPDilBlueHyp
      real(4) wNDlBueH            	! wNDilBlueHyp
      real(4) wPDlPytH            	! wPDilPhytHyp
      real(4) wNDlPytH            	! wNDilPhytHyp
      real(4) wDOtfotH            	! wDOutflTotHyp
      real(4) wPOtfotH            	! wPOutflTotHyp
      real(4) wNOtfotH            	! wNOutflTotHyp
      real(4) wDTraDiH            	! wDTranDiatHyp
      real(4) wPTraDiH            	! wPTranDiatHyp
      real(4) wNTraDiH            	! wNTranDiatHyp
      real(4) wDTanenH            	! wDTranGrenHyp
      real(4) wPTanenH            	! wPTranGrenHyp
      real(4) wNTanenH            	! wNTranGrenHyp
      real(4) wDTanueH            	! wDTranBlueHyp
      real(4) wPTanueH            	! wPTranBlueHyp
      real(4) wNTanueH            	! wNTranBlueHyp
      real(4) wDTanytH            	! wDTranPhytHyp
      real(4) wPTanytH            	! wPTranPhytHyp
      real(4) wNTanytH            	! wNTranPhytHyp
      real(4) uSioaiO2            	! uSiLoadSiO2
      real(4) uSiLodDt            	! uSiLoadDet
      real(4) uSiLodDi            	! uSiLoadDiat
      real(4) uSiLoad             	! uSiLoad
      real(4) wSiilO2H            	! wSiDilSiO2Hyp
      real(4) wSiDiDtH            	! wSiDilDetHyp
      real(4) wSiDiDiH            	! wSiDilDiatHyp
      real(4) wSitfotH            	! wSiOutflTotHyp
      real(4) wSianO2H            	! wSiTranSiO2Hyp
      real(4) wSiratWH            	! wSiTranDetWHyp
      real(4) tSiantTH            	! tSiTranTotTHyp
      real(4) wDTanooH            	! wDTranZooHyp
      real(4) wPTanooH            	! wPTranZooHyp
      real(4) wNTanooH            	! wNTranZooHyp
      real(4) wDTanMWH            	! wDTranIMWHyp
      real(4) wDTantWH            	! wDTranDetWHyp
      real(4) wO2TrnWH            	! wO2TranWHyp
      real(4) wPTan4WH            	! wPTranPO4WHyp
      real(4) wPTanMWH            	! wPTranAIMWHyp
      real(4) wPTantWH            	! wPTranDetWHyp
      real(4) wNTan4WH            	! wNTranNH4WHyp
      real(4) wNTan3WH            	! wNTranNO3WHyp
      real(4) wNTantWH            	! wNTranDetWHyp
      real(4) wDDilotH            	! wDDilTotHyp
      real(4) wPDilotH            	! wPDilTotHyp
      real(4) wNDilotH            	! wNDilTotHyp
      real(4) wSiilotH            	! wSiDilTotHyp
      real(4) tDTantTH            	! tDTranTotTHyp
      real(4) tPTantTH            	! tPTranTotTHyp
      real(4) tNTantTH            	! tNTranTotTHyp
      real(4) wDExIMMH            	! wDExchIMMHyp
      real(4) wPExP4MH            	! wPExchPO4MHyp
      real(4) wPExAMMH            	! wPExchAIMMHyp
      real(4) wNExN4MH            	! wNExchNH4MHyp
      real(4) wNExN3MH            	! wNExchNO3MHyp
      real(4) wSixS2MH            	! wSiExchSiO2MHyp
      real(4) wO2ExMH             	! wO2ExchMHyp
      real(4) wDExDtMH            	! wDExchDetMHyp
      real(4) wPExDtMH            	! wPExchDetMHyp
      real(4) wNExDtMH            	! wNExchDetMHyp
      real(4) wSiExtMH            	! wSiExchDetMHyp
      real(4) wDExDiMH            	! wDExchDiatMHyp
      real(4) wPExDiMH            	! wPExchDiatMHyp
      real(4) wNExDiMH            	! wNExchDiatMHyp
      real(4) wSiExiMH            	! wSiExchDiatMHyp
      real(4) wDEGrnMH            	! wDExchGrenMHyp
      real(4) wPEGrnMH            	! wPExchGrenMHyp
      real(4) wNEGrnMH            	! wNExchGrenMHyp
      real(4) wDEBleMH            	! wDExchBlueMHyp
      real(4) wPEBleMH            	! wPExchBlueMHyp
      real(4) wNEBleMH            	! wNExchBlueMHyp
      real(4) wDExZoMH            	! wDExchZooMHyp
      real(4) wPExZoMH            	! wPExchZooMHyp
      real(4) wNExZoMH            	! wNExchZooMHyp
      real(4) wDExIMH             	! wDExchIMHyp
      real(4) wPExPO4H            	! wPExchPO4Hyp
      real(4) wPExAIMH            	! wPExchAIMHyp
      real(4) wNExNH4H            	! wNExchNH4Hyp
      real(4) wNExNO3H            	! wNExchNO3Hyp
      real(4) wSixSO2H            	! wSiExchSiO2Hyp
      real(4) wO2ExH              	! wO2ExchHyp
      real(4) wDExDtH             	! wDExchDetHyp
      real(4) wPExDtH             	! wPExchDetHyp
      real(4) wNExDtH             	! wNExchDetHyp
      real(4) wSiExDtH            	! wSiExchDetHyp
      real(4) wDExDiH             	! wDExchDiatHyp
      real(4) wPExDiH             	! wPExchDiatHyp
      real(4) wNExDiH             	! wNExchDiatHyp
      real(4) wSiExDiH            	! wSiExchDiatHyp
      real(4) wDExGenH            	! wDExchGrenHyp
      real(4) wPExGenH            	! wPExchGrenHyp
      real(4) wNExGenH            	! wNExchGrenHyp
      real(4) wDExBueH            	! wDExchBlueHyp
      real(4) wPExBueH            	! wPExchBlueHyp
      real(4) wNExBueH            	! wNExchBlueHyp
      real(4) wDExZooH            	! wDExchZooHyp
      real(4) wPExZooH            	! wPExchZooHyp
      real(4) wNExZooH            	! wNExchZooHyp
      real(4) tPIfP4WH            	! tPInfPO4WHyp
      real(4) tNIfN4WH            	! tNInfNH4WHyp
      real(4) tNIfN3WH            	! tNInfNO3WHyp
      real(4) tPInfO4S            	! tPInfPO4S
      real(4) tNInfH4S            	! tNInfNH4S
      real(4) tNInfO3S            	! tNInfNO3S
      real(4) tNH4LadS            	! tNH4LoadS
      real(4) tNO3LadS            	! tNO3LoadS
      real(4) uDErosIM            	! uDErosIM
      real(4) uDEroIMS            	! uDErosIMS
      real(4) uDEroIMW            	! uDErosIMW
      real(4) uDErosOM            	! uDErosOM
      real(4) uPErosOM            	! uPErosOM
      real(4) uNErosOM            	! uNErosOM
      real(4) uO2SatE             	! uO2SatEpi
      real(4) uO2SatH             	! uO2SatHyp
      real(4) kAer                	! kAer
      real(4) uFuTmerE            	! uFunTmAerEpi
      real(4) uFuTmerH            	! uFunTmAerHyp
      real(4) aFuLeAer            	! aFunFloatAer
      real(4) tO2BubH             	! tO2BubHyp
      real(4) uFuTmish            	! uFunTmFish
      real(4) tDTrbish            	! tDTurbFish
      real(4) aFuegsus            	! aFunVegResus
      real(4) aFuDiusp            	! aFunDimSusp
      real(4) tDRsTead            	! tDResusTauDead
      real(4) tDRsBead            	! tDResusBareDead
      real(4) tDRsuead            	! tDResusDead
      real(4) tDRessIM            	! tDResusIM
      real(4) tDRessDt            	! tDResusDet
      real(4) akRsPRef            	! akResusPhytRef
      real(4) tDRsPTot            	! tDResusPhytTot
      real(4) tPRessDt            	! tPResusDet
      real(4) tPRsuPO4            	! tPResusPO4
      real(4) tPRsuAIM            	! tPResusAIM
      real(4) tNRsuNO3            	! tNResusNO3
      real(4) tNRsuNH4            	! tNResusNH4
      real(4) tNRessDt            	! tNResusDet
      real(4) tSiessDt            	! tSiResusDet
      real(4) aFuauOMH            	! aFunTauSetOMHyp
      real(4) aFuauIMH            	! aFunTauSetIMHyp
      real(4) uFuTmetE            	! uFunTmSetEpi
      real(4) uFuTmetH            	! uFunTmSetHyp
      real(4) uCoVSIMH            	! uCorVSetIMHyp
      real(4) tDSetIMH            	! tDSetIMHyp
      real(4) tPSetIMH            	! tPSetAIMHyp
      real(4) uCoVSDtH            	! uCorVSetDetHyp
      real(4) tDSetDtH            	! tDSetDetHyp
      real(4) tPSetDtH            	! tPSetDetHyp
      real(4) tNSetDtH            	! tNSetDetHyp
      real(4) tSiSeDtH            	! tSiSetDetHyp
      real(4) kPMinDtW            	! kPMinDetW
      real(4) kNMinDtW            	! kNMinDetW
      real(4) kSiMiDtW            	! kSiMinDetW
      real(4) uFuTmnWE            	! uFunTmMinWEpi
      real(4) uFuTmnWH            	! uFunTmMinWHyp
      real(4) wDMintWH            	! wDMinDetWHyp
      real(4) wPMintWH            	! wPMinDetWHyp
      real(4) wNMintWH            	! wNMinDetWHyp
      real(4) wSiintWH            	! wSiMinDetWHyp
      real(4) aCoO2ODH            	! aCorO2BODHyp
      real(4) wO2intWH            	! wO2MinDetWHyp
      real(4) wDDentWH            	! wDDenitWHyp
      real(4) wNDentWH            	! wNDenitWHyp
      real(4) uFuTmtrH            	! uFunTmNitrHyp
      real(4) uFuTmtrE            	! uFunTmNitrEpi
      real(4) uFuTmtrS            	! uFunTmNitrS
      real(4) aCo2NrWH            	! aCorO2NitrWHyp
      real(4) wNNitrWH            	! wNNitrWHyp
      real(4) wO2NirWH            	! wO2NitrWHyp
      real(4) kPMinDtS            	! kPMinDetS
      real(4) kNMinDtS            	! kNMinDetS
      real(4) kSiMiDtS            	! kSiMinDetS
      real(4) uFuTminS            	! uFunTmMinS
      real(4) tDMinDtS            	! tDMinDetS
      real(4) tPMinDtS            	! tPMinDetS
      real(4) tNMinDtS            	! tNMinDetS
      real(4) tSiMiDtS            	! tSiMinDetS
      real(4) uFunTDif            	! uFunTmDif
      real(4) akODiCor            	! akO2DifCor
      real(4) tSOD                	! tSOD
      real(4) aDptDif             	! aDepthDif
      real(4) tPDifO4H            	! tPDifPO4Hyp
      real(4) tNDifO3H            	! tNDifNO3Hyp
      real(4) tNDifH4H            	! tNDifNH4Hyp
      real(4) tO2DifH             	! tO2DifHyp
      real(4) tPDroPO4            	! tPDifGroundPO4
      real(4) tNDroNO3            	! tNDifGroundNO3
      real(4) tNDroNH4            	! tNDifGroundNH4
      real(4) aPAsMxWH            	! aPAdsMaxWHyp
      real(4) aKPAdsWH            	! aKPAdsWHyp
      real(4) aPIoAsWH            	! aPIsoAdsWHyp
      real(4) aPEqIMWH            	! aPEqIMWHyp
      real(4) wPSrpMWH            	! wPSorpIMWHyp
      real(4) tPChePO4            	! tPChemPO4
      real(4) tSibiotT            	! tSiAbioTotT
      real(4) uQEvPhra            	! uQEvPhra
      real(4) tPEvP4WM            	! tPEvPO4WM
      real(4) tNEvN4WM            	! tNEvNH4WM
      real(4) tNEvN3WM            	! tNEvNO3WM
      real(4) tPIfP4WM            	! tPInfPO4WM
      real(4) tNIfN4WM            	! tNInfNH4WM
      real(4) tNIfN3WM            	! tNInfNO3WM
      real(4) tPIfP4SM            	! tPInfPO4SM
      real(4) tNIfN4SM            	! tNInfNH4SM
      real(4) tNIfN3SM            	! tNInfNO3SM
      real(4) tO2AerM             	! tO2AerM
      real(4) tDSetIMM            	! tDSetIMM
      real(4) tPSetIMM            	! tPSetAIMM
      real(4) tDSetDtM            	! tDSetDetM
      real(4) tPSetDtM            	! tPSetDetM
      real(4) tNSetDtM            	! tNSetDetM
      real(4) tSiSeDtM            	! tSiSetDetM
      real(4) tDSetDiM            	! tDSetDiatM
      real(4) tPSetDiM            	! tPSetDiatM
      real(4) tNSetDiM            	! tNSetDiatM
      real(4) tSiSeDiM            	! tSiSetDiatM
      real(4) tDStGenM            	! tDSetGrenM
      real(4) tPStGenM            	! tPSetGrenM
      real(4) tNStGenM            	! tNSetGrenM
      real(4) tDStBueM            	! tDSetBlueM
      real(4) tPStBueM            	! tPSetBlueM
      real(4) tNStBueM            	! tNSetBlueM
      real(4) tDStPytM            	! tDSetPhytM
      real(4) tPStPytM            	! tPSetPhytM
      real(4) tNStPytM            	! tNSetPhytM
      real(4) tDSetotM            	! tDSetTotM
      real(4) wDMintWM            	! wDMinDetWM
      real(4) wPMintWM            	! wPMinDetWM
      real(4) wNMintWM            	! wNMinDetWM
      real(4) wSiintWM            	! wSiMinDetWM
      real(4) aCoO2ODM            	! aCorO2BODM
      real(4) wO2intWM            	! wO2MinDetWM
      real(4) wDDentWM            	! wDDenitWM
      real(4) wNDentWM            	! wNDenitWM
      real(4) aCo2NrWM            	! aCorO2NitrWM
      real(4) wNNitrWM            	! wNNitrWM
      real(4) wO2NirWM            	! wO2NitrWM
      real(4) tDMintSM            	! tDMinDetSM
      real(4) tPMintSM            	! tPMinDetSM
      real(4) tNMintSM            	! tNMinDetSM
      real(4) tSiintSM            	! tSiMinDetSM
      real(4) akODiorM            	! akO2DifCorM
      real(4) tSODM               	! tSODM
      real(4) aDpOxedM            	! aDepthOxySedM
      real(4) afOxyedM            	! afOxySedM
      real(4) tDMOxtSM            	! tDMinOxyDetSM
      real(4) tO2intSM            	! tO2MinDetSM
      real(4) tDDentSM            	! tDDenitSM
      real(4) tNDentSM            	! tNDenitSM
      real(4) tNNitrSM            	! tNNitrSM
      real(4) tO2NirSM            	! tO2NitrSM
      real(4) tDMnHmSM            	! tDMinHumSM
      real(4) tPMnHmSM            	! tPMinHumSM
      real(4) tNMnHmSM            	! tNMinHumSM
      real(4) aDptDifM            	! aDepthDifM
      real(4) tPDifO4M            	! tPDifPO4M
      real(4) tNDifO3M            	! tNDifNO3M
      real(4) tNDifH4M            	! tNDifNH4M
      real(4) tO2DifM             	! tO2DifM
      real(4) tPDroO4M            	! tPDifGroundPO4M
      real(4) tNDroO3M            	! tNDifGroundNO3M
      real(4) tNDroH4M            	! tNDifGroundNH4M
      real(4) aPAsMxWM            	! aPAdsMaxWM
      real(4) aKPAdsWM            	! aKPAdsWM
      real(4) aPIoAsWM            	! aPIsoAdsWM
      real(4) aPEqIMWM            	! aPEqIMWM
      real(4) wPSrpMWM            	! wPSorpIMWM
      real(4) aPAsMxSM            	! aPAdsMaxSM
      real(4) aKPAdsSM            	! aKPAdsSM
      real(4) aPIoAsSM            	! aPIsoAdsSM
      real(4) aPEqIMSM            	! aPEqIMSM
      real(4) tPSrpMSM            	! tPSorpIMSM
      real(4) tPCemO4M            	! tPChemPO4M
      real(4) aDRtVeg             	! aDRootVeg
      real(4) bfSubegE            	! bfSubVegEpi
      real(4) bfSubegH            	! bfSubVegHyp
      real(4) afCSuVeg            	! afCovSurfVeg
      real(4) afCEmVeg            	! afCovEmergVeg
      real(4) aCovVeg             	! aCovVeg
      real(4) aDVeg               	! aDVeg
      real(4) aPVeg               	! aPVeg
      real(4) aNVeg               	! aNVeg
      real(4) aLPARotH            	! aLPARBotHyp
      real(4) rPDVeg              	! rPDVeg
      real(4) rNDVeg              	! rNDVeg
      real(4) tDMigVeg            	! tDMigrVeg
      real(4) tPMigVeg            	! tPMigrVeg
      real(4) tNMigVeg            	! tNMigrVeg
      real(4) uFumRVeg            	! uFunTmRespVeg
      real(4) afPUVegS            	! afPUptVegS
      real(4) afNUVegS            	! afNUptVegS
      real(4) aVPaxVeg            	! aVPUptMaxCrVeg
      real(4) aVPUVgWH            	! aVPUptVegWHyp
      real(4) aVPUVegS            	! aVPUptVegS
      real(4) aVPUVgWE            	! aVPUptVegWEpi
      real(4) tPUVegWE            	! tPUptVegWEpi
      real(4) tPUVegWH            	! tPUptVegWHyp
      real(4) tPUVegS             	! tPUptVegS
      real(4) tPUVegE             	! tPUptVegEpi
      real(4) tPUVegH             	! tPUptVegHyp
      real(4) aVNaxVeg            	! aVNUptMaxCrVeg
      real(4) ahNUVeg             	! ahNUptVeg
      real(4) aVNUVgWE            	! aVNUptVegWEpi
      real(4) aVNUVgWH            	! aVNUptVegWHyp
      real(4) afN4UgWE            	! afNH4UptVegWEpi
      real(4) afN4UgWH            	! afNH4UptVegWHyp
      real(4) tNUVegWE            	! tNUptVegWEpi
      real(4) tNUVegWH            	! tNUptVegWHyp
      real(4) tNUH4gWE            	! tNUptNH4VegWEpi
      real(4) tNUO3gWE            	! tNUptNO3VegWEpi
      real(4) tNUH4gWH            	! tNUptNH4VegWHyp
      real(4) tNUO3gWH            	! tNUptNO3VegWHyp
      real(4) aVNUVegS            	! aVNUptVegS
      real(4) tNUVegS             	! tNUptVegS
      real(4) afN4UegS            	! afNH4UptVegS
      real(4) tNUH4egS            	! tNUptNH4VegS
      real(4) tNUVegSE            	! tNUptVegSEpi
      real(4) tNUO3egS            	! tNUptNO3VegS
      real(4) tNUVeg              	! tNUptVeg
      real(4) aLLmSegE            	! aLLimShootVegEpi
      real(4) aLLmSegH            	! aLLimShootVegHyp
      real(4) aMumLegH            	! aMuTmLVegHyp
      real(4) aPLimVeg            	! aPLimVeg
      real(4) aNLimVeg            	! aNLimVeg
      real(4) aNuLiVeg            	! aNutLimVeg
      real(4) aMuVegH             	! aMuVegHyp
      real(4) bkMVeg              	! bkMortVeg
      real(4) akDncegH            	! akDIncrVegHyp
      real(4) tDEnvegH            	! tDEnvVegHyp
      real(4) tDEPregH            	! tDEnvProdVegHyp
      real(4) tDPodegH            	! tDProdVegHyp
      real(4) tDPdSegH            	! tDProdSubVegHyp
      real(4) tDResVeg            	! tDRespVeg
      real(4) tDEvMegH            	! tDEnvMortVegHyp
      real(4) tDMVegH             	! tDMortVegHyp
      real(4) tDMVegWH            	! tDMortVegWHyp
      real(4) tDGazgBi            	! tDGrazVegBird
      real(4) bkManVeg            	! bkManVeg
      real(4) tDManVeg            	! tDManVeg
      real(4) tPManVeg            	! tPManVeg
      real(4) tNManVeg            	! tNManVeg
      real(4) tO2roegH            	! tO2ProdVegHyp
      real(4) tO2spgWH            	! tO2RespVegWHyp
      real(4) aDpOxSed            	! aDepthOxySed
      real(4) afOxySed            	! afOxySed
      real(4) tO2esegS            	! tO2RespVegS
      real(4) tO2odgSH            	! tO2ProdVegSHyp
      real(4) tO2odgWH            	! tO2ProdVegWHyp
      real(4) tO2O3gWH            	! tO2UptNO3VegWHyp
      real(4) tO2NOegS            	! tO2UptNO3VegS
      real(4) tPExcVeg            	! tPExcrVeg
      real(4) tPEcregS            	! tPExcrVegS
      real(4) tPEcregW            	! tPExcrVegW
      real(4) tPMVegH             	! tPMortVegHyp
      real(4) tPMegO4H            	! tPMortVegPO4Hyp
      real(4) tPMeg4SH            	! tPMortVegPO4SHyp
      real(4) tPMeg4WH            	! tPMortVegPO4WHyp
      real(4) tPMVeDtH            	! tPMortVegDetHyp
      real(4) tPMegtWH            	! tPMortVegDetWHyp
      real(4) tPMegtSH            	! tPMortVegDetSHyp
      real(4) tPGazgBi            	! tPGrazVegBird
      real(4) tNExcVeg            	! tNExcrVeg
      real(4) tNEcregS            	! tNExcrVegS
      real(4) tNEcregW            	! tNExcrVegW
      real(4) tNMVegH             	! tNMortVegHyp
      real(4) tNMegH4H            	! tNMortVegNH4Hyp
      real(4) tNMeg4SH            	! tNMortVegNH4SHyp
      real(4) tNMeg4WH            	! tNMortVegNH4WHyp
      real(4) tNMVeDtH            	! tNMortVegDetHyp
      real(4) tNMegtWH            	! tNMortVegDetWHyp
      real(4) tNMegtSH            	! tNMortVegDetSHyp
      real(4) tNGazgBi            	! tNGrazVegBird
      real(4) tDAsVgBi            	! tDAssVegBird
      real(4) tDEBi               	! tDEgesBird
      real(4) tPAsVgBi            	! tPAssVegBird
      real(4) tPEBi               	! tPEgesBird
      real(4) tPEBiPO4            	! tPEgesBirdPO4
      real(4) tPEBiDt             	! tPEgesBirdDet
      real(4) tNAsVgBi            	! tNAssVegBird
      real(4) tNEBi               	! tNEgesBird
      real(4) tNEBiNH4            	! tNEgesBirdNH4
      real(4) tNEBiDt             	! tNEgesBirdDet
      real(4) wDBedtWH            	! wDBedDetWHyp
      real(4) aMumLegE            	! aMuTmLVegEpi
      real(4) akDncegE            	! akDIncrVegEpi
      real(4) tDEnvegE            	! tDEnvVegEpi
      real(4) aMuVegE             	! aMuVegEpi
      real(4) tDEPregE            	! tDEnvProdVegEpi
      real(4) tDPodegE            	! tDProdVegEpi
      real(4) tDEvMegE            	! tDEnvMortVegEpi
      real(4) tDMVegE             	! tDMortVegEpi
      real(4) tNMVegE             	! tNMortVegEpi
      real(4) tNMegH4E            	! tNMortVegNH4Epi
      real(4) tNMeg4SE            	! tNMortVegNH4SEpi
      real(4) wPBdP4WH            	! wPBedPO4WHyp
      real(4) wPBedtWH            	! wPBedDetWHyp
      real(4) wNBdN4WH            	! wNBedNH4WHyp
      real(4) wNBdN3WH            	! wNBedNO3WHyp
      real(4) wNBedtWH            	! wNBedDetWHyp
      real(4) tDMVegSH            	! tDMortVegSHyp
      real(4) tDBedtSH            	! tDBedDetSHyp
      real(4) tO2BedWH            	! tO2BedWHyp
      real(4) UseLoss             	! UseLoss
      real(4) uFuTmssH            	! uFunTmLossHyp
      real(4) uFuTmssE            	! uFunTmLossEpi
      real(4) rPDBleWH            	! rPDBlueWHyp
      real(4) rNDBleWH            	! rNDBlueWHyp
      real(4) rPDBlueS            	! rPDBlueS
      real(4) rNDBlueS            	! rNDBlueS
      real(4) uFuTmueH            	! uFunTmBlueHyp
      real(4) uFuPrueH            	! uFunTmProdBlueHyp
      real(4) uFuReueH            	! uFunTmRespBlueHyp
      real(4) uFuTmueE            	! uFunTmBlueEpi
      real(4) uFuPrueE            	! uFunTmProdBlueEpi
      real(4) uFuReueE            	! uFunTmRespBlueEpi
      real(4) uFuTmueS            	! uFunTmBlueS
      real(4) uFuReueS            	! uFunTmRespBlueS
      real(4) aVPxCueH            	! aVPUptMaxCrBlueHyp
      real(4) aVPUBueH            	! aVPUptBlueHyp
      real(4) wPUBlueH            	! wPUptBlueHyp
      real(4) aVNxCueH            	! aVNUptMaxCrBlueHyp
      real(4) ahNUBueH            	! ahNUptBlueHyp
      real(4) aVNUBueH            	! aVNUptBlueHyp
      real(4) wNUBlueH            	! wNUptBlueHyp
      real(4) afN4UueH            	! afNH4UptBlueHyp
      real(4) wNUH4ueH            	! wNUptNH4BlueHyp
      real(4) wNUO3ueH            	! wNUptNO3BlueHyp
      real(4) uMuxTueH            	! uMuMaxTmBlueHyp
      real(4) uMuxTueE            	! uMuMaxTmBlueEpi
      real(4) aPLmBueH            	! aPLimBlueHyp
      real(4) aNLmBueH            	! aNLimBlueHyp
      real(4) aSiimueH            	! aSiLimBlueHyp
      real(4) aLLmBueH            	! aLLimBlueHyp
      real(4) aMumLueH            	! aMuTmLBlueHyp
      real(4) aNuimueH            	! aNutLimBlueHyp
      real(4) aMuBlueH            	! aMuBlueHyp
      real(4) wDAsBueH            	! wDAssBlueHyp
      real(4) rChDBueH            	! rChDBlueHyp
      real(4) oChlBlH             	! oChlaBlueHyp
      real(4) aExChueH            	! aExtChBlueHyp
      real(4) ukDpTueH            	! ukDRespTmBlueHyp
      real(4) ukDpTueE            	! ukDRespTmBlueEpi
      real(4) ukDpTueS            	! ukDRespTmBlueS
      real(4) wDRpBeWH            	! wDRespBlueWHyp
      real(4) ukLsTueH            	! ukLossTmBlueHyp
      real(4) ukLsTueE            	! ukLossTmBlueEpi
      real(4) wDLssueH            	! wDLossBlueHyp
      real(4) wDMBleWH            	! wDMortBlueWHyp
      real(4) uCoSeueH            	! uCorVSetBlueHyp
      real(4) tDStBueH            	! tDSetBlueHyp
      real(4) tDRsulue            	! tDResusBlue
      real(4) tDRspueS            	! tDRespBlueS
      real(4) tDMBlueS            	! tDMortBlueS
      real(4) ukDecueH            	! ukDDecBlueHyp
      real(4) wPErBeWH            	! wPExcrBlueWHyp
      real(4) wPLssueH            	! wPLossBlueHyp
      real(4) wPMBleWH            	! wPMortBlueWHyp
      real(4) tPStBueH            	! tPSetBlueHyp
      real(4) tPRsulue            	! tPResusBlue
      real(4) tPEcrueS            	! tPExcrBlueS
      real(4) tPMBlueS            	! tPMortBlueS
      real(4) wNErBeWH            	! wNExcrBlueWHyp
      real(4) wNLssueH            	! wNLossBlueHyp
      real(4) wNMBleWH            	! wNMortBlueWHyp
      real(4) tNStBueH            	! tNSetBlueHyp
      real(4) tNRsulue            	! tNResusBlue
      real(4) tNEcrueS            	! tNExcrBlueS
      real(4) tNMBlueS            	! tNMortBlueS
      real(4) aFuauOME            	! aFunTauSetOMEpi
      real(4) aFuauIME            	! aFunTauSetIMEpi
      real(4) uCoSeueE            	! uCorVSetBlueEpi
      real(4) tDStBueE            	! tDSetBlueEpi
      real(4) rNDBleWE            	! rNDBlueWEpi
      real(4) tNStBueE            	! tNSetBlueEpi
      real(4) rPDBleWE            	! rPDBlueWEpi
      real(4) tPStBueE            	! tPSetBlueEpi
      real(4) wDPmBeWH            	! wDPrimBlueWHyp
      real(4) wPPmBeWH            	! wPPrimBlueWHyp
      real(4) wNPmBeWH            	! wNPrimBlueWHyp
      real(4) rPDGrnWH            	! rPDGrenWHyp
      real(4) rNDGrnWH            	! rNDGrenWHyp
      real(4) rPDGrenS            	! rPDGrenS
      real(4) rNDGrenS            	! rNDGrenS
      real(4) uFuTmenH            	! uFunTmGrenHyp
      real(4) uFuTmenE            	! uFunTmGrenEpi
      real(4) uFuTmenS            	! uFunTmGrenS
      real(4) uFuPrenH            	! uFunTmProdGrenHyp
      real(4) uFuReenH            	! uFunTmRespGrenHyp
      real(4) uFuPrenE            	! uFunTmProdGrenEpi
      real(4) uFuReenE            	! uFunTmRespGrenEpi
      real(4) uFuReenS            	! uFunTmRespGrenS
      real(4) aVPxCenH            	! aVPUptMaxCrGrenHyp
      real(4) aVPUGenH            	! aVPUptGrenHyp
      real(4) wPUGrenH            	! wPUptGrenHyp
      real(4) aVNxCenH            	! aVNUptMaxCrGrenHyp
      real(4) ahNUGenH            	! ahNUptGrenHyp
      real(4) aVNUGenH            	! aVNUptGrenHyp
      real(4) wNUGrenH            	! wNUptGrenHyp
      real(4) afN4UenH            	! afNH4UptGrenHyp
      real(4) wNUH4enH            	! wNUptNH4GrenHyp
      real(4) wNUO3enH            	! wNUptNO3GrenHyp
      real(4) uMuxTenH            	! uMuMaxTmGrenHyp
      real(4) uMuxTenE            	! uMuMaxTmGrenEpi
      real(4) aPLmGenH            	! aPLimGrenHyp
      real(4) aNLmGenH            	! aNLimGrenHyp
      real(4) aSiimenH            	! aSiLimGrenHyp
      real(4) aLLmGenH            	! aLLimGrenHyp
      real(4) aMumLenH            	! aMuTmLGrenHyp
      real(4) aNuimenH            	! aNutLimGrenHyp
      real(4) aMuGrenH            	! aMuGrenHyp
      real(4) wDAsGenH            	! wDAssGrenHyp
      real(4) rChDGenH            	! rChDGrenHyp
      real(4) oChlGrH             	! oChlaGrenHyp
      real(4) aExChenH            	! aExtChGrenHyp
      real(4) ukDpTenH            	! ukDRespTmGrenHyp
      real(4) ukDpTenE            	! ukDRespTmGrenEpi
      real(4) ukDpTenS            	! ukDRespTmGrenS
      real(4) wDRpGnWH            	! wDRespGrenWHyp
      real(4) ukLsTenH            	! ukLossTmGrenHyp
      real(4) ukLsTenE            	! ukLossTmGrenEpi
      real(4) wDLssenH            	! wDLossGrenHyp
      real(4) wDMGrnWH            	! wDMortGrenWHyp
      real(4) uCoSeenH            	! uCorVSetGrenHyp
      real(4) tDStGenH            	! tDSetGrenHyp
      real(4) tDRsuren            	! tDResusGren
      real(4) tDRspenS            	! tDRespGrenS
      real(4) tDMGrenS            	! tDMortGrenS
      real(4) ukDecenH            	! ukDDecGrenHyp
      real(4) wPErGnWH            	! wPExcrGrenWHyp
      real(4) wPLssenH            	! wPLossGrenHyp
      real(4) wPMGrnWH            	! wPMortGrenWHyp
      real(4) tPStGenH            	! tPSetGrenHyp
      real(4) tPRsuren            	! tPResusGren
      real(4) tPEcrenS            	! tPExcrGrenS
      real(4) tPMGrenS            	! tPMortGrenS
      real(4) wNErGnWH            	! wNExcrGrenWHyp
      real(4) wNLssenH            	! wNLossGrenHyp
      real(4) wNMGrnWH            	! wNMortGrenWHyp
      real(4) tNStGenH            	! tNSetGrenHyp
      real(4) tNRsuren            	! tNResusGren
      real(4) tNEcrenS            	! tNExcrGrenS
      real(4) tNMGrenS            	! tNMortGrenS
      real(4) uCoSeenE            	! uCorVSetGrenEpi
      real(4) tDStGenE            	! tDSetGrenEpi
      real(4) rNDGrnWE            	! rNDGrenWEpi
      real(4) tNStGenE            	! tNSetGrenEpi
      real(4) rPDGrnWE            	! rPDGrenWEpi
      real(4) tPStGenE            	! tPSetGrenEpi
      real(4) wDPmGnWH            	! wDPrimGrenWHyp
      real(4) wPPmGnWH            	! wPPrimGrenWHyp
      real(4) wNPmGnWH            	! wNPrimGrenWHyp
      real(4) rPDDiWH             	! rPDDiatWHyp
      real(4) rNDDiWH             	! rNDDiatWHyp
      real(4) rPDDiS              	! rPDDiatS
      real(4) uFunTDiH            	! uFunTmDiatHyp
      real(4) rNDDiS              	! rNDDiatS
      real(4) uFunTDiE            	! uFunTmDiatEpi
      real(4) uFunTDiS            	! uFunTmDiatS
      real(4) uFumPDiH            	! uFunTmProdDiatHyp
      real(4) uFumRDiH            	! uFunTmRespDiatHyp
      real(4) uFumPDiE            	! uFunTmProdDiatEpi
      real(4) uFumRDiE            	! uFunTmRespDiatEpi
      real(4) uFumRDiS            	! uFunTmRespDiatS
      real(4) aVPaxDiH            	! aVPUptMaxCrDiatHyp
      real(4) aVPUDiH             	! aVPUptDiatHyp
      real(4) wPUDiH              	! wPUptDiatHyp
      real(4) aVNaxDiH            	! aVNUptMaxCrDiatHyp
      real(4) ahNUDiH             	! ahNUptDiatHyp
      real(4) aVNUDiH             	! aVNUptDiatHyp
      real(4) wNUDiH              	! wNUptDiatHyp
      real(4) afNH4DiH            	! afNH4UptDiatHyp
      real(4) wNUNHDiH            	! wNUptNH4DiatHyp
      real(4) wNUNODiH            	! wNUptNO3DiatHyp
      real(4) uMuaxDiH            	! uMuMaxTmDiatHyp
      real(4) uMuaxDiE            	! uMuMaxTmDiatEpi
      real(4) aPLimDiH            	! aPLimDiatHyp
      real(4) aNLimDiH            	! aNLimDiatHyp
      real(4) aSiLiDiH            	! aSiLimDiatHyp
      real(4) aLLimDiH            	! aLLimDiatHyp
      real(4) aMuTmDiH            	! aMuTmLDiatHyp
      real(4) aNuLiDiH            	! aNutLimDiatHyp
      real(4) aMuDiH              	! aMuDiatHyp
      real(4) wDAssDiH            	! wDAssDiatHyp
      real(4) rChDDiH             	! rChDDiatHyp
      real(4) oChlDiH             	! oChlaDiatHyp
      real(4) aExtCDiH            	! aExtChDiatHyp
      real(4) ukDspDiH            	! ukDRespTmDiatHyp
      real(4) ukDspDiE            	! ukDRespTmDiatEpi
      real(4) ukDspDiS            	! ukDRespTmDiatS
      real(4) wDRspiWH            	! wDRespDiatWHyp
      real(4) ukLssDiH            	! ukLossTmDiatHyp
      real(4) ukLssDiE            	! ukLossTmDiatEpi
      real(4) wDLosDiH            	! wDLossDiatHyp
      real(4) wDMDiWH             	! wDMortDiatWHyp
      real(4) uCoVSDiH            	! uCorVSetDiatHyp
      real(4) tDSetDiH            	! tDSetDiatHyp
      real(4) tDRessDi            	! tDResusDiat
      real(4) tDResDiS            	! tDRespDiatS
      real(4) tDMDiS              	! tDMortDiatS
      real(4) ukDDeDiH            	! ukDDecDiatHyp
      real(4) wPEcriWH            	! wPExcrDiatWHyp
      real(4) wPLosDiH            	! wPLossDiatHyp
      real(4) wPMDiWH             	! wPMortDiatWHyp
      real(4) tPSetDiH            	! tPSetDiatHyp
      real(4) tPRessDi            	! tPResusDiat
      real(4) tPExcDiS            	! tPExcrDiatS
      real(4) tPMDiS              	! tPMortDiatS
      real(4) wNEcriWH            	! wNExcrDiatWHyp
      real(4) wNLosDiH            	! wNLossDiatHyp
      real(4) wNMDiWH             	! wNMortDiatWHyp
      real(4) tNSetDiH            	! tNSetDiatHyp
      real(4) tNRessDi            	! tNResusDiat
      real(4) tNExcDiS            	! tNExcrDiatS
      real(4) tNMDiS              	! tNMortDiatS
      real(4) uCoVSDiE            	! uCorVSetDiatEpi
      real(4) tDSetDiE            	! tDSetDiatEpi
      real(4) rPDDiWE             	! rPDDiatWEpi
      real(4) tPSetDiE            	! tPSetDiatEpi
      real(4) rNDDiWE             	! rNDDiatWEpi
      real(4) tNSetDiE            	! tNSetDiatEpi
      real(4) wDPimiWH            	! wDPrimDiatWHyp
      real(4) wPPimiWH            	! wPPrimDiatWHyp
      real(4) wNPimiWH            	! wNPrimDiatWHyp
      real(4) oChlaH              	! oChlaHyp
      real(4) wDAsPytH            	! wDAssPhytHyp
      real(4) wDRpPtWH            	! wDRespPhytWHyp
      real(4) wDMPhtWH            	! wDMortPhytWHyp
      real(4) tDStPytH            	! tDSetPhytHyp
      real(4) wDLssytH            	! wDLossPhytHyp
      real(4) wDPmPtWH            	! wDPrimPhytWHyp
      real(4) wPUPhytH            	! wPUptPhytHyp
      real(4) wPErPtWH            	! wPExcrPhytWHyp
      real(4) wPMPhtWH            	! wPMortPhytWHyp
      real(4) tPStPytH            	! tPSetPhytHyp
      real(4) tPRsuhyt            	! tPResusPhyt
      real(4) wPLssytH            	! wPLossPhytHyp
      real(4) wPPmPtWH            	! wPPrimPhytWHyp
      real(4) wNUPhytH            	! wNUptPhytHyp
      real(4) wNUH4ytH            	! wNUptNH4PhytHyp
      real(4) wNUO3ytH            	! wNUptNO3PhytHyp
      real(4) wNErPtWH            	! wNExcrPhytWHyp
      real(4) wNMPhtWH            	! wNMortPhytWHyp
      real(4) tNStPytH            	! tNSetPhytHyp
      real(4) tNRsuhyt            	! tNResusPhyt
      real(4) wNLssytH            	! wNLossPhytHyp
      real(4) wNPmPtWH            	! wNPrimPhytWHyp
      real(4) tDRspytS            	! tDRespPhytS
      real(4) tDMPhytS            	! tDMortPhytS
      real(4) tPEcrytS            	! tPExcrPhytS
      real(4) tPMPhytS            	! tPMortPhytS
      real(4) tNEcrytS            	! tNExcrPhytS
      real(4) tNMPhytS            	! tNMortPhytS
      real(4) wSiUDiH             	! wSiUptDiatHyp
      real(4) wSixciWH            	! wSiExcrDiatWHyp
      real(4) wSiosDiH            	! wSiLossDiatHyp
      real(4) wSiMDiWH            	! wSiMortDiatWHyp
      real(4) tSiSeDiH            	! tSiSetDiatHyp
      real(4) tSiessDi            	! tSiResusDiat
      real(4) rCyDBueH            	! rCyDBlueHyp
      real(4) oCyanH              	! oCyanHyp
      real(4) fDDiH               	! fDDiatHyp
      real(4) wDPimtWH            	! wDPrimDetWHyp
      real(4) wO2odytH            	! wO2ProdPhytHyp
      real(4) wO2sptWH            	! wO2RespPhytWHyp
      real(4) wO2O3ytH            	! wO2UptNO3PhytHyp
      real(4) wO2PrmWH            	! wO2PrimWHyp
      real(4) wPMyt4WH            	! wPMortPhytPO4WHyp
      real(4) wPMhytWH            	! wPMortPhytDetWHyp
      real(4) wPLPhO4H            	! wPLossPhytPO4Hyp
      real(4) wPLsPDtH            	! wPLossPhytDetHyp
      real(4) wPPim4WH            	! wPPrimPO4WHyp
      real(4) wPPimtWH            	! wPPrimDetWHyp
      real(4) tPMhyO4S            	! tPMortPhytPO4S
      real(4) tPMhyDtS            	! tPMortPhytDetS
      real(4) tPPimO4S            	! tPPrimPO4S
      real(4) tPPimotT            	! tPPrimTotT
      real(4) wNMyt4WH            	! wNMortPhytNH4WHyp
      real(4) wNMhytWH            	! wNMortPhytDetWHyp
      real(4) wNLPhH4H            	! wNLossPhytNH4Hyp
      real(4) wNLsPDtH            	! wNLossPhytDetHyp
      real(4) wNPim4WH            	! wNPrimNH4WHyp
      real(4) wNPim3WH            	! wNPrimNO3WHyp
      real(4) wNPimtWH            	! wNPrimDetWHyp
      real(4) tNMhyH4S            	! tNMortPhytNH4S
      real(4) tNMhyDtS            	! tNMortPhytDetS
      real(4) tNPimH4S            	! tNPrimNH4S
      real(4) tNPimO3S            	! tNPrimNO3S
      real(4) tNPimotT            	! tNPrimTotT
      real(4) tSixcDiS            	! tSiExcrDiatS
      real(4) tSiMDiS             	! tSiMortDiatS
      real(4) wSiim2WH            	! wSiPrimSiO2WHyp
      real(4) wSiritWH            	! wSiPrimDetWHyp
      real(4) tSiriotT            	! tSiPrimTotT
      real(4) aPACoefE            	! aPACoefEpi
      real(4) aPACoefH            	! aPACoefHyp
      real(4) bSechaxE            	! bSecchiMaxEpi
      real(4) aSecchiE            	! aSecchiEpi
      real(4) aTrpacyE            	! aTransparencyEpi
      real(4) bSechaxH            	! bSecchiMaxHyp
      real(4) aSecchiH            	! aSecchiHyp
      real(4) aSecchiT            	! aSecchiT
      real(4) aTrpacyH            	! aTransparencyHyp
      real(4) aTrpacyT            	! aTransparencyT
      real(4) aDptEphH            	! aDepthEuphHyp
      real(4) aReptphH            	! aRelDepthEuphHyp
      real(4) aChlaHH             	! aChlaHHyp
      real(4) aCoPhtWH            	! aCovPhytWHyp
      real(4) rExChytH            	! rExtChPhytHyp
      real(4) uFuTmooH            	! uFunTmZooHyp
      real(4) uFuTmooE            	! uFunTmZooEpi
      real(4) rPDZooH             	! rPDZooHyp
      real(4) rNDZooH             	! rNDZooHyp
      real(4) oDFodooH            	! oDFoodZooHyp
      real(4) aFiltH              	! aFiltHyp
      real(4) ukDsTooH            	! ukDAssTmZooHyp
      real(4) ukDsTooE            	! ukDAssTmZooEpi
      real(4) aDSatooH            	! aDSatZooHyp
      real(4) ukDspooH            	! ukDRespTmZooHyp
      real(4) ukDspooE            	! ukDRespTmZooEpi
      real(4) ukDncooH            	! ukDIncrZooHyp
      real(4) ukDncooE            	! ukDIncrZooEpi
      real(4) wDEnvooH            	! wDEnvZooHyp
      real(4) wDAssooH            	! wDAssZooHyp
      real(4) wDCZooH             	! wDConsZooHyp
      real(4) wDCDtooH            	! wDConsDetZooHyp
      real(4) wDCDiooH            	! wDConsDiatZooHyp
      real(4) wDCreooH            	! wDConsGrenZooHyp
      real(4) wDCluooH            	! wDConsBlueZooHyp
      real(4) wDChyooH            	! wDConsPhytZooHyp
      real(4) wDEZooH             	! wDEgesZooHyp
      real(4) aCoReooH            	! aCorDRespZooHyp
      real(4) wDRspooH            	! wDRespZooHyp
      real(4) wDMZooH             	! wDMortZooHyp
      real(4) oPFodooH            	! oPFoodZooHyp
      real(4) rPDooooH            	! rPDFoodZooHyp
      real(4) wPCDiooH            	! wPConsDiatZooHyp
      real(4) wPCreooH            	! wPConsGrenZooHyp
      real(4) wPCluooH            	! wPConsBlueZooHyp
      real(4) wPChyooH            	! wPConsPhytZooHyp
      real(4) wPCDtooH            	! wPConsDetZooHyp
      real(4) wPCZooH             	! wPConsZooHyp
      real(4) afPssooH            	! afPAssZooHyp
      real(4) wPAssooH            	! wPAssZooHyp
      real(4) wPEZooH             	! wPEgesZooHyp
      real(4) wPEooO4H            	! wPEgesZooPO4Hyp
      real(4) wPEZoDtH            	! wPEgesZooDetHyp
      real(4) akPxcooH            	! akPExcrZooHyp
      real(4) wPEcrooH            	! wPExcrZooHyp
      real(4) wPMZooH             	! wPMortZooHyp
      real(4) wPMooO4H            	! wPMortZooPO4Hyp
      real(4) wPMZoDtH            	! wPMortZooDetHyp
      real(4) oNFodooH            	! oNFoodZooHyp
      real(4) rNDooooH            	! rNDFoodZooHyp
      real(4) wNCDiooH            	! wNConsDiatZooHyp
      real(4) wNCreooH            	! wNConsGrenZooHyp
      real(4) wNCluooH            	! wNConsBlueZooHyp
      real(4) wNChyooH            	! wNConsPhytZooHyp
      real(4) wNCDtooH            	! wNConsDetZooHyp
      real(4) wNCZooH             	! wNConsZooHyp
      real(4) afNssooH            	! afNAssZooHyp
      real(4) wNAssooH            	! wNAssZooHyp
      real(4) wNEZooH             	! wNEgesZooHyp
      real(4) wNEooH4H            	! wNEgesZooNH4Hyp
      real(4) wNEZoDtH            	! wNEgesZooDetHyp
      real(4) kNEcrooH            	! kNExcrZooHyp
      real(4) wNEcrooH            	! wNExcrZooHyp
      real(4) wNMZooH             	! wNMortZooHyp
      real(4) wNMooH4H            	! wNMortZooNH4Hyp
      real(4) wNMZoDtH            	! wNMortZooDetHyp
      real(4) wSiDiooH            	! wSiConsDiatZooHyp
      real(4) uFunTBnt            	! uFunTmBent
      real(4) aDFooBnt            	! aDFoodBent
      real(4) rPDBnt              	! rPDBent
      real(4) rNDBnt              	! rNDBent
      real(4) tDMigBnt            	! tDMigrBent
      real(4) aDSatBnt            	! aDSatBent
      real(4) ukDncBnt            	! ukDIncrBent
      real(4) tDEnvBnt            	! tDEnvBent
      real(4) tDAssBnt            	! tDAssBent
      real(4) aDAsBtSp            	! aDAssBentSp
      real(4) tDCBnt              	! tDConsBent
      real(4) tDCDtBnt            	! tDConsDetBent
      real(4) tDCDiBnt            	! tDConsDiatBent
      real(4) tDCreBnt            	! tDConsGrenBent
      real(4) tDCluBnt            	! tDConsBlueBent
      real(4) tDChyBnt            	! tDConsPhytBent
      real(4) tDEBnt              	! tDEgesBent
      real(4) tDResBnt            	! tDRespBent
      real(4) tDMBnt              	! tDMortBent
      real(4) aPFooBnt            	! aPFoodBent
      real(4) rPDooBnt            	! rPDFoodBent
      real(4) tPCDtBnt            	! tPConsDetBent
      real(4) tPCDiBnt            	! tPConsDiatBent
      real(4) tPCreBnt            	! tPConsGrenBent
      real(4) tPCluBnt            	! tPConsBlueBent
      real(4) tPChyBnt            	! tPConsPhytBent
      real(4) tPCBnt              	! tPConsBent
      real(4) afPAsBnt            	! afPAssBent
      real(4) tPAssBnt            	! tPAssBent
      real(4) tPEBnt              	! tPEgesBent
      real(4) tPEBnPO4            	! tPEgesBentPO4
      real(4) tPEBntDt            	! tPEgesBentDet
      real(4) tPExcBnt            	! tPExcrBent
      real(4) tPMBnt              	! tPMortBent
      real(4) tPMBnPO4            	! tPMortBentPO4
      real(4) tPMBntDt            	! tPMortBentDet
      real(4) tPMigBnt            	! tPMigrBent
      real(4) aNFooBnt            	! aNFoodBent
      real(4) rNDooBnt            	! rNDFoodBent
      real(4) tNMigBnt            	! tNMigrBent
      real(4) tNCDtBnt            	! tNConsDetBent
      real(4) tNCDiBnt            	! tNConsDiatBent
      real(4) tNCreBnt            	! tNConsGrenBent
      real(4) tNCluBnt            	! tNConsBlueBent
      real(4) tNChyBnt            	! tNConsPhytBent
      real(4) tNCBnt              	! tNConsBent
      real(4) afNAsBnt            	! afNAssBent
      real(4) tNAssBnt            	! tNAssBent
      real(4) tNEBnt              	! tNEgesBent
      real(4) tNEBnNH4            	! tNEgesBentNH4
      real(4) tNEBntDt            	! tNEgesBentDet
      real(4) tNExcBnt            	! tNExcrBent
      real(4) tNMBnt              	! tNMortBent
      real(4) tNMBnNH4            	! tNMortBentNH4
      real(4) tNMBntDt            	! tNMortBentDet
      real(4) tSiCDBnt            	! tSiConsDiatBent
      real(4) aFuVeish            	! aFunVegFish
      real(4) ukDnciJv            	! ukDIncrFiJv
      real(4) aDSatiAd            	! aDSatFiAd
      real(4) ukDnciAd            	! ukDIncrFiAd
      real(4) ukHrvish            	! ukHarvFish
      real(4) afPssiAd            	! afPAssFiAd
      real(4) afNssiAd            	! afNAssFiAd
      real(4) uFuTmisc            	! uFunTmPisc
      real(4) aDCrrisc            	! aDCarrPisc
      real(4) aFuVeisc            	! aFunVegPisc
      real(4) akDncisc            	! akDIncrPisc
      real(4) ukHrvisc            	! ukHarvPisc
      real(4) tDWebDtS            	! tDWebDetS
      real(4) tDWebDiS            	! tDWebDiatS
      real(4) tDWbGenS            	! tDWebGrenS
      real(4) tDWbBueS            	! tDWebBlueS
      real(4) tDWbPytS            	! tDWebPhytS
      real(4) tPWebO4S            	! tPWebPO4S
      real(4) tPWebDtS            	! tPWebDetS
      real(4) tPWebDiS            	! tPWebDiatS
      real(4) tPWbGenS            	! tPWebGrenS
      real(4) tPWbBueS            	! tPWebBlueS
      real(4) tPWbPytS            	! tPWebPhytS
      real(4) tNWebH4S            	! tNWebNH4S
      real(4) tNWebO3S            	! tNWebNO3S
      real(4) tNWebDtS            	! tNWebDetS
      real(4) tNWebDiS            	! tNWebDiatS
      real(4) tNWbGenS            	! tNWebGrenS
      real(4) tNWbBueS            	! tNWebBlueS
      real(4) tNWbPytS            	! tNWebPhytS
      real(4) wSiebO2W            	! wSiWebSiO2W
      real(4) wSiebtWH            	! wSiWebDetWHyp
      real(4) tSiWeDtS            	! tSiWebDetS
      real(4) tSiebotT            	! tSiWebTotT
      real(4) aPrefveH            	! aPrefAveHyp
      real(4) wDCZoo2H            	! wDConsZoo2Hyp
      real(4) aDCZoSpH            	! aDConsZooSpHyp
      real(4) aDAsZSpH            	! aDAssZooSpHyp
      real(4) aDGraSpH            	! aDGrazSpHyp
      real(4) aPCZoSpH            	! aPConsZooSpHyp
      real(4) aPGraSpH            	! aPGrazSpHyp
      real(4) aNCZoSpH            	! aNConsZooSpHyp
      real(4) aNGraSpH            	! aNGrazSpHyp
      real(4) afDShhra            	! afDShootPhra
      real(4) rDSRPhra            	! rDSRPhra
      real(4) rPDShhra            	! rPDShootPhra
      real(4) rNDShhra            	! rNDShootPhra
      real(4) rPDRthra            	! rPDRootPhra
      real(4) rNDRthra            	! rNDRootPhra
      real(4) aLegShra            	! aLengShootPhra
      real(4), save :: bDanihra
      real(4) aDAllhra            	! aDAllPhra
      real(4) tDAllhra            	! tDAllPhra
      real(4) tNTanhra            	! tNTransPhra
      real(4) tPTanhra            	! tPTransPhra
      real(4) aVNhrxCr            	! aVNUptPhraMaxCr
      real(4) ahNUPraS            	! ahNUptPhraS
      real(4) aVNUPraS            	! aVNUptPhraS
      real(4) tNUPhraS            	! tNUptPhraS
      real(4) tNUH4raS            	! tNUptNH4PhraS
      real(4) tNUO3raS            	! tNUptNO3PhraS
      real(4) tNUShhra            	! tNUptShootPhra
      real(4) tNURthra            	! tNUptRootPhra
      real(4) aVPhrxCr            	! aVPUptPhraMaxCr
      real(4) ahPUPraS            	! ahPUptPhraS
      real(4) aVPUPraS            	! aVPUptPhraS
      real(4) tPUPhraS            	! tPUptPhraS
      real(4) tPUShhra            	! tPUptShootPhra
      real(4) tPURthra            	! tPUptRootPhra
      real(4) uFuPrhra            	! uFunTmProdPhra
      real(4) ukDsphra            	! ukDRespTmPhra
      real(4) aMuhohra            	! aMuPhotPhra
      real(4) aNLPrhra            	! aNLimProdPhra
      real(4) aPLPrhra            	! aPLimProdPhra
      real(4) aNuLihra            	! aNutLimPhra
      real(4) aMuPhra             	! aMuPhra
      real(4) akDnchra            	! akDIncrPhra
      real(4) tDDnshra            	! tDDensPhra
      real(4) tDDPrhra            	! tDDensProdPhra
      real(4) tDPodhra            	! tDProdPhra
      real(4) tDPdShra            	! tDProdShootPhra
      real(4) tDPdRhra            	! tDProdRootPhra
      real(4) tDRpShra            	! tDRespShootPhra
      real(4) tDRpRhra            	! tDRespRootPhra
      real(4) tO2sphra            	! tO2RespRootPhra
      real(4) tO2lohra            	! tO2FlowPhra
      real(4), save :: bDaeahra
      real(4) aDRalhra            	! aDRealPhra
      real(4) tDRalhra            	! tDRealPhra
      real(4) tNRtrhra            	! tNRetrPhra
      real(4) tPRtrhra            	! tPRetrPhra
      real(4) tDMShhra            	! tDMortShootPhra
      real(4) tNMShhra            	! tNMortShootPhra
      real(4) tPMShhra            	! tPMortShootPhra
      real(4) tDMRthra            	! tDMortRootPhra
      real(4) tNMRthra            	! tNMortRootPhra
      real(4) tPMRthra            	! tPMortRootPhra
      real(4) tDMnShra            	! tDManShootPhra
      real(4) tNMnShra            	! tNManShootPhra
      real(4) tPMnShra            	! tPManShootPhra
      real(4) tDIMSM              	! tDIMSM
      real(4) tDHumSM             	! tDHumSM
      real(4) tDDtSM              	! tDDetSM
      real(4) vDeltaSM            	! vDeltaSM
      real(4) tDBurIMM            	! tDBurIMM
      real(4) tDBurOMM            	! tDBurOMM
      real(4) tDBurDtM            	! tDBurDetM
      real(4) tDBurumM            	! tDBurHumM
      real(4) tDBurotM            	! tDBurTotM
      real(4) tPBurumM            	! tPBurHumM
      real(4) tPBurDtM            	! tPBurDetM
      real(4) tPBurIMM            	! tPBurAIMM
      real(4) tPBurO4M            	! tPBurPO4M
      real(4) tPBurotM            	! tPBurTotM
      real(4) tNBurumM            	! tNBurHumM
      real(4) tNBurDtM            	! tNBurDetM
      real(4) tNBurH4M            	! tNBurNH4M
      real(4) tNBurO3M            	! tNBurNO3M
      real(4) tNBurotM            	! tNBurTotM
      real(4) tSiBuDtM            	! tSiBurDetM
      real(4) tSiurotM            	! tSiBurTotM
      real(4) vDeltaWM            	! vDeltaWM
      real(4) aReDeaWM            	! aRelDeltaWM
      real(4) tDSetotH            	! tDSetTotHyp
      real(4) tPSetotH            	! tPSetTotHyp
      real(4) tNSetotH            	! tNSetTotHyp
      real(4) bTimered            	! bTimeDred
      real(4) bRholoil            	! bRhoSolidSoil
      real(4) tDMrsotT            	! tDMarsTotT
      real(4) tPMrsotT            	! tPMarsTotT
      real(4) tNMrsotT            	! tNMarsTotT
      real(4) tSiarotT            	! tSiMarsTotT
      real(4) uTaSustE            	! uTauSubstEpi
      real(4) akExME              	! akExchMEpi
      real(4) afVlMshE            	! afVolMarshEpi
      real(4) akExLE              	! akExchLEpi
      real(4) oPPhytWE            	! oPPhytWEpi
      real(4) oNPhytWE            	! oNPhytWEpi
      real(4) oDSestWE            	! oDSestWEpi
      real(4) oPOMWE              	! oPOMWEpi
      real(4) oPSestWE            	! oPSestWEpi
      real(4) oPInogWE            	! oPInorgWEpi
      real(4) oPTotWE             	! oPTotWEpi
      real(4) oNOMWE              	! oNOMWEpi
      real(4) oNSestWE            	! oNSestWEpi
      real(4) oNkjWE              	! oNkjWEpi
      real(4) oNTotWE             	! oNTotWEpi
      real(4) rPDIMWE             	! rPDIMWEpi
      real(4) rPDDtWE             	! rPDDetWEpi
      real(4) rNDDtWE             	! rNDDetWEpi
      real(4) rSiDDtWE            	! rSiDDetWEpi
      real(4) wDDilIME            	! wDDilIMEpi
      real(4) wDDilDtE            	! wDDilDetEpi
      real(4) wDDilDiE            	! wDDilDiatEpi
      real(4) wDDlGenE            	! wDDilGrenEpi
      real(4) wDDlBueE            	! wDDilBlueEpi
      real(4) wDDlPytE            	! wDDilPhytEpi
      real(4) wDDilooE            	! wDDilZooEpi
      real(4) wPDilooE            	! wPDilZooEpi
      real(4) wNDilooE            	! wNDilZooEpi
      real(4) wPDilO4E            	! wPDilPO4Epi
      real(4) wPDilDtE            	! wPDilDetEpi
      real(4) wPDilIME            	! wPDilAIMEpi
      real(4) wNDilH4E            	! wNDilNH4Epi
      real(4) wNDilO3E            	! wNDilNO3Epi
      real(4) wNDilDtE            	! wNDilDetEpi
      real(4) wO2nfowE            	! wO2InflowEpi
      real(4) wO2OuflE            	! wO2OutflEpi
      real(4) wPDilDiE            	! wPDilDiatEpi
      real(4) wNDilDiE            	! wNDilDiatEpi
      real(4) wPDlGenE            	! wPDilGrenEpi
      real(4) wNDlGenE            	! wNDilGrenEpi
      real(4) wPDlBueE            	! wPDilBlueEpi
      real(4) wNDlBueE            	! wNDilBlueEpi
      real(4) wPDlPytE            	! wPDilPhytEpi
      real(4) wNDlPytE            	! wNDilPhytEpi
      real(4) wDOtfotE            	! wDOutflTotEpi
      real(4) wPOtfotE            	! wPOutflTotEpi
      real(4) wNOtfotE            	! wNOutflTotEpi
      real(4) wDTraDiE            	! wDTranDiatEpi
      real(4) wPTraDiE            	! wPTranDiatEpi
      real(4) wNTraDiE            	! wNTranDiatEpi
      real(4) wDTanenE            	! wDTranGrenEpi
      real(4) wPTanenE            	! wPTranGrenEpi
      real(4) wNTanenE            	! wNTranGrenEpi
      real(4) wDTanueE            	! wDTranBlueEpi
      real(4) wPTanueE            	! wPTranBlueEpi
      real(4) wNTanueE            	! wNTranBlueEpi
      real(4) wDTanytE            	! wDTranPhytEpi
      real(4) wPTanytE            	! wPTranPhytEpi
      real(4) wNTanytE            	! wNTranPhytEpi
      real(4) wSiilO2E            	! wSiDilSiO2Epi
      real(4) wSiDiDtE            	! wSiDilDetEpi
      real(4) wSiDiDiE            	! wSiDilDiatEpi
      real(4) wSitfotE            	! wSiOutflTotEpi
      real(4) wSianO2E            	! wSiTranSiO2Epi
      real(4) wSiratWE            	! wSiTranDetWEpi
      real(4) tSiantTE            	! tSiTranTotTEpi
      real(4) wDTanooE            	! wDTranZooEpi
      real(4) wPTanooE            	! wPTranZooEpi
      real(4) wNTanooE            	! wNTranZooEpi
      real(4) wDTanMWE            	! wDTranIMWEpi
      real(4) wDTantWE            	! wDTranDetWEpi
      real(4) wO2TrnWE            	! wO2TranWEpi
      real(4) wPTan4WE            	! wPTranPO4WEpi
      real(4) wPTanMWE            	! wPTranAIMWEpi
      real(4) wPTantWE            	! wPTranDetWEpi
      real(4) wNTan4WE            	! wNTranNH4WEpi
      real(4) wNTan3WE            	! wNTranNO3WEpi
      real(4) wNTantWE            	! wNTranDetWEpi
      real(4) wDDilotE            	! wDDilTotEpi
      real(4) wPDilotE            	! wPDilTotEpi
      real(4) wNDilotE            	! wNDilTotEpi
      real(4) wSiilotE            	! wSiDilTotEpi
      real(4) tDTantTE            	! tDTranTotTEpi
      real(4) tPTantTE            	! tPTranTotTEpi
      real(4) tNTantTE            	! tNTranTotTEpi
      real(4) wDExIMME            	! wDExchIMMEpi
      real(4) wPExP4ME            	! wPExchPO4MEpi
      real(4) wPExAMME            	! wPExchAIMMEpi
      real(4) wNExN4ME            	! wNExchNH4MEpi
      real(4) wNExN3ME            	! wNExchNO3MEpi
      real(4) wSixS2ME            	! wSiExchSiO2MEpi
      real(4) wO2ExME             	! wO2ExchMEpi
      real(4) wDExDtME            	! wDExchDetMEpi
      real(4) wPExDtME            	! wPExchDetMEpi
      real(4) wNExDtME            	! wNExchDetMEpi
      real(4) wSiExtME            	! wSiExchDetMEpi
      real(4) wDExDiME            	! wDExchDiatMEpi
      real(4) wPExDiME            	! wPExchDiatMEpi
      real(4) wNExDiME            	! wNExchDiatMEpi
      real(4) wSiExiME            	! wSiExchDiatMEpi
      real(4) wDEGrnME            	! wDExchGrenMEpi
      real(4) wPEGrnME            	! wPExchGrenMEpi
      real(4) wNEGrnME            	! wNExchGrenMEpi
      real(4) wDEBleME            	! wDExchBlueMEpi
      real(4) wPEBleME            	! wPExchBlueMEpi
      real(4) wNEBleME            	! wNExchBlueMEpi
      real(4) wDExZoME            	! wDExchZooMEpi
      real(4) wPExZoME            	! wPExchZooMEpi
      real(4) wNExZoME            	! wNExchZooMEpi
      real(4) wDExIME             	! wDExchIMEpi
      real(4) wPExPO4E            	! wPExchPO4Epi
      real(4) wPExAIME            	! wPExchAIMEpi
      real(4) wNExNH4E            	! wNExchNH4Epi
      real(4) wNExNO3E            	! wNExchNO3Epi
      real(4) wSixSO2E            	! wSiExchSiO2Epi
      real(4) wO2ExE              	! wO2ExchEpi
      real(4) wDExDtE             	! wDExchDetEpi
      real(4) wPExDtE             	! wPExchDetEpi
      real(4) wNExDtE             	! wNExchDetEpi
      real(4) wSiExDtE            	! wSiExchDetEpi
      real(4) wDExDiE             	! wDExchDiatEpi
      real(4) wPExDiE             	! wPExchDiatEpi
      real(4) wNExDiE             	! wNExchDiatEpi
      real(4) wSiExDiE            	! wSiExchDiatEpi
      real(4) wDExGenE            	! wDExchGrenEpi
      real(4) wPExGenE            	! wPExchGrenEpi
      real(4) wNExGenE            	! wNExchGrenEpi
      real(4) wDExBueE            	! wDExchBlueEpi
      real(4) wPExBueE            	! wPExchBlueEpi
      real(4) wNExBueE            	! wNExchBlueEpi
      real(4) wDExZooE            	! wDExchZooEpi
      real(4) wPExZooE            	! wPExchZooEpi
      real(4) wNExZooE            	! wNExchZooEpi
      real(4) tPIfP4WE            	! tPInfPO4WEpi
      real(4) tNIfN4WE            	! tNInfNH4WEpi
      real(4) tNIfN3WE            	! tNInfNO3WEpi
      real(4) tO2AerE             	! tO2AerEpi
      real(4) tDTbFhIM            	! tDTurbFishIM
      real(4) uCoVSIME            	! uCorVSetIMEpi
      real(4) tDSetIME            	! tDSetIMEpi
      real(4) tPSetIME            	! tPSetAIMEpi
      real(4) uCoVSDtE            	! uCorVSetDetEpi
      real(4) tDSetDtE            	! tDSetDetEpi
      real(4) tPSetDtE            	! tPSetDetEpi
      real(4) tNSetDtE            	! tNSetDetEpi
      real(4) tSiSeDtE            	! tSiSetDetEpi
      real(4) wDMintWE            	! wDMinDetWEpi
      real(4) wPMintWE            	! wPMinDetWEpi
      real(4) wNMintWE            	! wNMinDetWEpi
      real(4) wSiintWE            	! wSiMinDetWEpi
      real(4) aCoO2ODE            	! aCorO2BODEpi
      real(4) wO2intWE            	! wO2MinDetWEpi
      real(4) wDDentWE            	! wDDenitWEpi
      real(4) wNDentWE            	! wNDenitWEpi
      real(4) aCo2NrWE            	! aCorO2NitrWEpi
      real(4) wNNitrWE            	! wNNitrWEpi
      real(4) wO2NirWE            	! wO2NitrWEpi
      real(4) tDMnODtS            	! tDMinOxyDetS
      real(4) tO2MiDtS            	! tO2MinDetS
      real(4) tDDenitS            	! tDDenitS
      real(4) tNDenitS            	! tNDenitS
      real(4) tNNitrS             	! tNNitrS
      real(4) tO2NitrS            	! tO2NitrS
      real(4) tDMinumS            	! tDMinHumS
      real(4) tPMinumS            	! tPMinHumS
      real(4) tNMinumS            	! tNMinHumS
      real(4) tPDifO4E            	! tPDifPO4Epi
      real(4) tNDifO3E            	! tNDifNO3Epi
      real(4) tNDifH4E            	! tNDifNH4Epi
      real(4) tO2DifE             	! tO2DifEpi
      real(4) aPAsMxWE            	! aPAdsMaxWEpi
      real(4) aKPAdsWE            	! aKPAdsWEpi
      real(4) aPIoAsWE            	! aPIsoAdsWEpi
      real(4) aPEqIMWE            	! aPEqIMWEpi
      real(4) wPSrpMWE            	! wPSorpIMWEpi
      real(4) aPAdsaxS            	! aPAdsMaxS
      real(4) aKPAdsS             	! aKPAdsS
      real(4) aPIsodsS            	! aPIsoAdsS
      real(4) aPEqIMS             	! aPEqIMS
      real(4) tPSorIMS            	! tPSorpIMS
      real(4) wO2AboWH            	! wO2AbioWHyp
      real(4) wPAiotWH            	! wPAbioDetWHyp
      real(4) wPAio4WH            	! wPAbioPO4WHyp
      real(4) wPAioMWH            	! wPAbioAIMWHyp
      real(4) tPAiootT            	! tPAbioTotT
      real(4) wNAio4WH            	! wNAbioNH4WHyp
      real(4) wNAio3WH            	! wNAbioNO3WHyp
      real(4) wNAiotWH            	! wNAbioDetWHyp
      real(4) wSiio2WH            	! wSiAbioSiO2WHyp
      real(4) wSibitWH            	! wSiAbioDetWHyp
      real(4) wDAiotWH            	! wDAbioDetWHyp
      real(4) wDAioMWH            	! wDAbioIMWHyp
      real(4) wDAioMWE            	! wDAbioIMWEpi
      real(4) wDAiotWE            	! wDAbioDetWEpi
      real(4) tDAbiIMS            	! tDAbioIMS
      real(4) tDAbiDtS            	! tDAbioDetS
      real(4) tDAioumS            	! tDAbioHumS
      real(4) tDAiootT            	! tDAbioTotT
      real(4) wO2AboWE            	! wO2AbioWEpi
      real(4) wO2AbioM            	! wO2AbioM
      real(4) wPAiotWE            	! wPAbioDetWEpi
      real(4) wPAio4WE            	! wPAbioPO4WEpi
      real(4) wPAioMWE            	! wPAbioAIMWEpi
      real(4) tPAbiDtS            	! tPAbioDetS
      real(4) tPAioumS            	! tPAbioHumS
      real(4) tPAioO4S            	! tPAbioPO4S
      real(4) tPAioIMS            	! tPAbioAIMS
      real(4) wNAio4WE            	! wNAbioNH4WEpi
      real(4) wNAio3WE            	! wNAbioNO3WEpi
      real(4) wNAiotWE            	! wNAbioDetWEpi
      real(4) tNAioH4S            	! tNAbioNH4S
      real(4) tNAioO3S            	! tNAbioNO3S
      real(4) tNAbiDtS            	! tNAbioDetS
      real(4) tNAioumS            	! tNAbioHumS
      real(4) tNAiootT            	! tNAbioTotT
      real(4) wSiio2WE            	! wSiAbioSiO2WEpi
      real(4) wSibitWE            	! wSiAbioDetWEpi
      real(4) tSibiDtS            	! tSiAbioDetS
      real(4) tDPdSegE            	! tDProdSubVegEpi
      real(4) tDMVegWE            	! tDMortVegWEpi
      real(4) tDMVegSE            	! tDMortVegSEpi
      real(4) tDBedVeg            	! tDBedVeg
      real(4) tO2roegE            	! tO2ProdVegEpi
      real(4) tO2spgWE            	! tO2RespVegWEpi
      real(4) tO2odgSE            	! tO2ProdVegSEpi
      real(4) tO2odgWE            	! tO2ProdVegWEpi
      real(4) tO2BedS             	! tO2BedS
      real(4) tO2O3gWE            	! tO2UptNO3VegWEpi
      real(4) tPMVegE             	! tPMortVegEpi
      real(4) tPMegO4E            	! tPMortVegPO4Epi
      real(4) tPMeg4SE            	! tPMortVegPO4SEpi
      real(4) tPMeg4WE            	! tPMortVegPO4WEpi
      real(4) tPMVeDtE            	! tPMortVegDetEpi
      real(4) tPMegtWE            	! tPMortVegDetWEpi
      real(4) tPMegtSE            	! tPMortVegDetSEpi
      real(4) tPBedVeg            	! tPBedVeg
      real(4) tNMeg4WE            	! tNMortVegNH4WEpi
      real(4) tNMVeDtE            	! tNMortVegDetEpi
      real(4) tNMegtWE            	! tNMortVegDetWEpi
      real(4) tNMegtSE            	! tNMortVegDetSEpi
      real(4) tNBedVeg            	! tNBedVeg
      real(4) wDBedtWE            	! wDBedDetWEpi
      real(4) tDBedtSE            	! tDBedDetSEpi
      real(4) wPBdP4WE            	! wPBedPO4WEpi
      real(4) wPBedtWE            	! wPBedDetWEpi
      real(4) tPBedO4S            	! tPBedPO4S
      real(4) tPBedDtS            	! tPBedDetS
      real(4) wNBdN4WE            	! wNBedNH4WEpi
      real(4) wNBdN3WE            	! wNBedNO3WEpi
      real(4) wNBedtWE            	! wNBedDetWEpi
      real(4) tNBedH4S            	! tNBedNH4S
      real(4) tNBedO3S            	! tNBedNO3S
      real(4) tNBedDtS            	! tNBedDetS
      real(4) tO2BedWE            	! tO2BedWEpi
      real(4) aVPxCueE            	! aVPUptMaxCrBlueEpi
      real(4) aVPUBueE            	! aVPUptBlueEpi
      real(4) wPUBlueE            	! wPUptBlueEpi
      real(4) aVNxCueE            	! aVNUptMaxCrBlueEpi
      real(4) ahNUBueE            	! ahNUptBlueEpi
      real(4) aVNUBueE            	! aVNUptBlueEpi
      real(4) wNUBlueE            	! wNUptBlueEpi
      real(4) afN4UueE            	! afNH4UptBlueEpi
      real(4) wNUH4ueE            	! wNUptNH4BlueEpi
      real(4) wNUO3ueE            	! wNUptNO3BlueEpi
      real(4) aPLmBueE            	! aPLimBlueEpi
      real(4) aNLmBueE            	! aNLimBlueEpi
      real(4) aSiimueE            	! aSiLimBlueEpi
      real(4) aLLmBueE            	! aLLimBlueEpi
      real(4) aMumLueE            	! aMuTmLBlueEpi
      real(4) aNuimueE            	! aNutLimBlueEpi
      real(4) aMuBlueE            	! aMuBlueEpi
      real(4) wDAsBueE            	! wDAssBlueEpi
      real(4) rChDBueE            	! rChDBlueEpi
      real(4) oChlBlE             	! oChlaBlueEpi
      real(4) aExChueE            	! aExtChBlueEpi
      real(4) wDRpBeWE            	! wDRespBlueWEpi
      real(4) wDLssueE            	! wDLossBlueEpi
      real(4) wDMBleWE            	! wDMortBlueWEpi
      real(4) ukDecueE            	! ukDDecBlueEpi
      real(4) wPErBeWE            	! wPExcrBlueWEpi
      real(4) wPLssueE            	! wPLossBlueEpi
      real(4) wPMBleWE            	! wPMortBlueWEpi
      real(4) wNErBeWE            	! wNExcrBlueWEpi
      real(4) wNLssueE            	! wNLossBlueEpi
      real(4) wNMBleWE            	! wNMortBlueWEpi
      real(4) wDPmBeWE            	! wDPrimBlueWEpi
      real(4) wPPmBeWE            	! wPPrimBlueWEpi
      real(4) wNPmBeWE            	! wNPrimBlueWEpi
      real(4) tDPimueS            	! tDPrimBlueS
      real(4) tPPimueS            	! tPPrimBlueS
      real(4) tNPimueS            	! tNPrimBlueS
      real(4) aVPxCenE            	! aVPUptMaxCrGrenEpi
      real(4) aVPUGenE            	! aVPUptGrenEpi
      real(4) wPUGrenE            	! wPUptGrenEpi
      real(4) aVNxCenE            	! aVNUptMaxCrGrenEpi
      real(4) ahNUGenE            	! ahNUptGrenEpi
      real(4) aVNUGenE            	! aVNUptGrenEpi
      real(4) wNUGrenE            	! wNUptGrenEpi
      real(4) afN4UenE            	! afNH4UptGrenEpi
      real(4) wNUH4enE            	! wNUptNH4GrenEpi
      real(4) wNUO3enE            	! wNUptNO3GrenEpi
      real(4) aPLmGenE            	! aPLimGrenEpi
      real(4) aNLmGenE            	! aNLimGrenEpi
      real(4) aSiimenE            	! aSiLimGrenEpi
      real(4) aLLmGenE            	! aLLimGrenEpi
      real(4) aMumLenE            	! aMuTmLGrenEpi
      real(4) aNuimenE            	! aNutLimGrenEpi
      real(4) aMuGrenE            	! aMuGrenEpi
      real(4) wDAsGenE            	! wDAssGrenEpi
      real(4) rChDGenE            	! rChDGrenEpi
      real(4) oChlGrE             	! oChlaGrenEpi
      real(4) aExChenE            	! aExtChGrenEpi
      real(4) wDRpGnWE            	! wDRespGrenWEpi
      real(4) wDLssenE            	! wDLossGrenEpi
      real(4) wDMGrnWE            	! wDMortGrenWEpi
      real(4) ukDecenE            	! ukDDecGrenEpi
      real(4) wPErGnWE            	! wPExcrGrenWEpi
      real(4) wPLssenE            	! wPLossGrenEpi
      real(4) wPMGrnWE            	! wPMortGrenWEpi
      real(4) wNErGnWE            	! wNExcrGrenWEpi
      real(4) wNLssenE            	! wNLossGrenEpi
      real(4) wNMGrnWE            	! wNMortGrenWEpi
      real(4) wDPmGnWE            	! wDPrimGrenWEpi
      real(4) wPPmGnWE            	! wPPrimGrenWEpi
      real(4) wNPmGnWE            	! wNPrimGrenWEpi
      real(4) tDPimenS            	! tDPrimGrenS
      real(4) tPPimenS            	! tPPrimGrenS
      real(4) tNPimenS            	! tNPrimGrenS
      real(4) aVPaxDiE            	! aVPUptMaxCrDiatEpi
      real(4) aVPUDiE             	! aVPUptDiatEpi
      real(4) wPUDiE              	! wPUptDiatEpi
      real(4) aVNaxDiE            	! aVNUptMaxCrDiatEpi
      real(4) ahNUDiE             	! ahNUptDiatEpi
      real(4) aVNUDiE             	! aVNUptDiatEpi
      real(4) wNUDiE              	! wNUptDiatEpi
      real(4) afNH4DiE            	! afNH4UptDiatEpi
      real(4) wNUNHDiE            	! wNUptNH4DiatEpi
      real(4) wNUNODiE            	! wNUptNO3DiatEpi
      real(4) aPLimDiE            	! aPLimDiatEpi
      real(4) aNLimDiE            	! aNLimDiatEpi
      real(4) aSiLiDiE            	! aSiLimDiatEpi
      real(4) aLLimDiE            	! aLLimDiatEpi
      real(4) aMuTmDiE            	! aMuTmLDiatEpi
      real(4) aNuLiDiE            	! aNutLimDiatEpi
      real(4) aMuDiE              	! aMuDiatEpi
      real(4) wDAssDiE            	! wDAssDiatEpi
      real(4) rChDDiE             	! rChDDiatEpi
      real(4) oChlDiE             	! oChlaDiatEpi
      real(4) aExtCDiE            	! aExtChDiatEpi
      real(4) wDRspiWE            	! wDRespDiatWEpi
      real(4) wDLosDiE            	! wDLossDiatEpi
      real(4) wDMDiWE             	! wDMortDiatWEpi
      real(4) ukDDeDiE            	! ukDDecDiatEpi
      real(4) wPEcriWE            	! wPExcrDiatWEpi
      real(4) wPLosDiE            	! wPLossDiatEpi
      real(4) wPMDiWE             	! wPMortDiatWEpi
      real(4) wNEcriWE            	! wNExcrDiatWEpi
      real(4) wNLosDiE            	! wNLossDiatEpi
      real(4) wNMDiWE             	! wNMortDiatWEpi
      real(4) wDPimiWE            	! wDPrimDiatWEpi
      real(4) wPPimiWE            	! wPPrimDiatWEpi
      real(4) wNPimiWE            	! wNPrimDiatWEpi
      real(4) tDPriDiS            	! tDPrimDiatS
      real(4) tPPriDiS            	! tPPrimDiatS
      real(4) tNPriDiS            	! tNPrimDiatS
      real(4) oChlaE              	! oChlaEpi
      real(4) oChla               	! oChla
      real(4) aLPIE               	! aLPIEpi
      real(4) aLMeasE             	! aLMeasEpi
      real(4) oChlMeE             	! oChlaMeasEpi
      real(4) wDAsPytE            	! wDAssPhytEpi
      real(4) wDRpPtWE            	! wDRespPhytWEpi
      real(4) wDMPhtWE            	! wDMortPhytWEpi
      real(4) tDStPytE            	! tDSetPhytEpi
      real(4) wDLssytE            	! wDLossPhytEpi
      real(4) wDPmPtWE            	! wDPrimPhytWEpi
      real(4) wPUPhytE            	! wPUptPhytEpi
      real(4) wPErPtWE            	! wPExcrPhytWEpi
      real(4) wPMPhtWE            	! wPMortPhytWEpi
      real(4) tPStPytE            	! tPSetPhytEpi
      real(4) wPLssytE            	! wPLossPhytEpi
      real(4) wPPmPtWE            	! wPPrimPhytWEpi
      real(4) wNUPhytE            	! wNUptPhytEpi
      real(4) wNUH4ytE            	! wNUptNH4PhytEpi
      real(4) wNUO3ytE            	! wNUptNO3PhytEpi
      real(4) wNErPtWE            	! wNExcrPhytWEpi
      real(4) wNMPhtWE            	! wNMortPhytWEpi
      real(4) tNStPytE            	! tNSetPhytEpi
      real(4) wNLssytE            	! wNLossPhytEpi
      real(4) wNPmPtWE            	! wNPrimPhytWEpi
      real(4) tDPimytS            	! tDPrimPhytS
      real(4) tPPimytS            	! tPPrimPhytS
      real(4) tNPimytS            	! tNPrimPhytS
      real(4) wSiUDiE             	! wSiUptDiatEpi
      real(4) wSixciWE            	! wSiExcrDiatWEpi
      real(4) wSiosDiE            	! wSiLossDiatEpi
      real(4) wSiMDiWE            	! wSiMortDiatWEpi
      real(4) tSiSeDiE            	! tSiSetDiatEpi
      real(4) wSiriiWH            	! wSiPrimDiatWHyp
      real(4) wSiriiWE            	! wSiPrimDiatWEpi
      real(4) rCyDBueE            	! rCyDBlueEpi
      real(4) oCyanE              	! oCyanEpi
      real(4) fDDiE               	! fDDiatEpi
      real(4) wDPimtWE            	! wDPrimDetWEpi
      real(4) tDPriDtS            	! tDPrimDetS
      real(4) tDPimotT            	! tDPrimTotT
      real(4) wO2odytE            	! wO2ProdPhytEpi
      real(4) wO2sptWE            	! wO2RespPhytWEpi
      real(4) wO2O3ytE            	! wO2UptNO3PhytEpi
      real(4) wO2PrmWE            	! wO2PrimWEpi
      real(4) tO2spytS            	! tO2RespPhytS
      real(4) tO2PrimS            	! tO2PrimS
      real(4) tO2PrmSH            	! tO2PrimSHyp
      real(4) tO2PrmSE            	! tO2PrimSEpi
      real(4) wPMyt4WE            	! wPMortPhytPO4WEpi
      real(4) wPMhytWE            	! wPMortPhytDetWEpi
      real(4) wPLPhO4E            	! wPLossPhytPO4Epi
      real(4) wPLsPDtE            	! wPLossPhytDetEpi
      real(4) wPPim4WE            	! wPPrimPO4WEpi
      real(4) wPPimtWE            	! wPPrimDetWEpi
      real(4) tPPriDtS            	! tPPrimDetS
      real(4) wNMyt4WE            	! wNMortPhytNH4WEpi
      real(4) wNMhytWE            	! wNMortPhytDetWEpi
      real(4) wNLPhH4E            	! wNLossPhytNH4Epi
      real(4) wNLsPDtE            	! wNLossPhytDetEpi
      real(4) wNPim4WE            	! wNPrimNH4WEpi
      real(4) wNPim3WE            	! wNPrimNO3WEpi
      real(4) wNPimtWE            	! wNPrimDetWEpi
      real(4) tNPriDtS            	! tNPrimDetS
      real(4) wSiim2WE            	! wSiPrimSiO2WEpi
      real(4) wSiritWE            	! wSiPrimDetWEpi
      real(4) tSiriDiS            	! tSiPrimDiatS
      real(4) tSiriDtS            	! tSiPrimDetS
      real(4) aDptEphE            	! aDepthEuphEpi
      real(4) aReptphE            	! aRelDepthEuphEpi
      real(4) aChlaHE             	! aChlaHEpi
      real(4) aCoPhtWE            	! aCovPhytWEpi
      real(4) rExChytE            	! rExtChPhytEpi
      real(4) rPDZooE             	! rPDZooEpi
      real(4) rNDZooE             	! rNDZooEpi
      real(4) oDFodooE            	! oDFoodZooEpi
      real(4) aFiltE              	! aFiltEpi
      real(4) aDSatooE            	! aDSatZooEpi
      real(4) wDEnvooE            	! wDEnvZooEpi
      real(4) wDAssooE            	! wDAssZooEpi
      real(4) wDCZooE             	! wDConsZooEpi
      real(4) wDCDtooE            	! wDConsDetZooEpi
      real(4) wDCDiooE            	! wDConsDiatZooEpi
      real(4) wDCreooE            	! wDConsGrenZooEpi
      real(4) wDCluooE            	! wDConsBlueZooEpi
      real(4) wDChyooE            	! wDConsPhytZooEpi
      real(4) wDEZooE             	! wDEgesZooEpi
      real(4) aCoReooE            	! aCorDRespZooEpi
      real(4) wDRspooE            	! wDRespZooEpi
      real(4) wDMZooE             	! wDMortZooEpi
      real(4) oPFodooE            	! oPFoodZooEpi
      real(4) rPDooooE            	! rPDFoodZooEpi
      real(4) wPCDiooE            	! wPConsDiatZooEpi
      real(4) wPCreooE            	! wPConsGrenZooEpi
      real(4) wPCluooE            	! wPConsBlueZooEpi
      real(4) wPChyooE            	! wPConsPhytZooEpi
      real(4) wPCDtooE            	! wPConsDetZooEpi
      real(4) wPCZooE             	! wPConsZooEpi
      real(4) afPssooE            	! afPAssZooEpi
      real(4) wPAssooE            	! wPAssZooEpi
      real(4) wPEZooE             	! wPEgesZooEpi
      real(4) wPEooO4E            	! wPEgesZooPO4Epi
      real(4) wPEZoDtE            	! wPEgesZooDetEpi
      real(4) akPxcooE            	! akPExcrZooEpi
      real(4) wPEcrooE            	! wPExcrZooEpi
      real(4) wPMZooE             	! wPMortZooEpi
      real(4) wPMooO4E            	! wPMortZooPO4Epi
      real(4) wPMZoDtE            	! wPMortZooDetEpi
      real(4) oNFodooE            	! oNFoodZooEpi
      real(4) rNDooooE            	! rNDFoodZooEpi
      real(4) wNCDiooE            	! wNConsDiatZooEpi
      real(4) wNCreooE            	! wNConsGrenZooEpi
      real(4) wNCluooE            	! wNConsBlueZooEpi
      real(4) wNChyooE            	! wNConsPhytZooEpi
      real(4) wNCDtooE            	! wNConsDetZooEpi
      real(4) wNCZooE             	! wNConsZooEpi
      real(4) afNssooE            	! afNAssZooEpi
      real(4) wNAssooE            	! wNAssZooEpi
      real(4) wNEZooE             	! wNEgesZooEpi
      real(4) wNEooH4E            	! wNEgesZooNH4Epi
      real(4) wNEZoDtE            	! wNEgesZooDetEpi
      real(4) kNEcrooE            	! kNExcrZooEpi
      real(4) wNEcrooE            	! wNExcrZooEpi
      real(4) wNMZooE             	! wNMortZooEpi
      real(4) wNMooH4E            	! wNMortZooNH4Epi
      real(4) wNMZoDtE            	! wNMortZooDetEpi
      real(4) wSiDiooE            	! wSiConsDiatZooEpi
      real(4) rPDFiJv             	! rPDFiJv
      real(4) rPDFiAd             	! rPDFiAd
      real(4) rNDFiJv             	! rNDFiJv
      real(4) rNDFiAd             	! rNDFiAd
      real(4) tDRprish            	! tDReprFish
      real(4) tDAgeish            	! tDAgeFish
      real(4) aDTotZoo            	! aDTotZoo
      real(4) aPTotZoo            	! aPTotZoo
      real(4) aNTotZoo            	! aNTotZoo
      real(4) aDSatiJv            	! aDSatFiJv
      real(4) tDEnviJv            	! tDEnvFiJv
      real(4) tDAssiJv            	! tDAssFiJv
      real(4) tDCFiJv             	! tDConsFiJv
      real(4) tDEFiJv             	! tDEgesFiJv
      real(4) tDRspiJv            	! tDRespFiJv
      real(4) tDMFiJv             	! tDMortFiJv
      real(4) tDMgriJv            	! tDMigrFiJv
      real(4) tDEnviAd            	! tDEnvFiAd
      real(4) tDAssiAd            	! tDAssFiAd
      real(4) tDCFiAd             	! tDConsFiAd
      real(4) tDEFiAd             	! tDEgesFiAd
      real(4) tDRspiAd            	! tDRespFiAd
      real(4) tDMFiAd             	! tDMortFiAd
      real(4) tDHrvish            	! tDHarvFish
      real(4) tDMgriAd            	! tDMigrFiAd
      real(4) tDMiJBot            	! tDMortFiJvBot
      real(4) tDMFivDt            	! tDMortFiJvDet
      real(4) tDMiABot            	! tDMortFiAdBot
      real(4) tDMFidDt            	! tDMortFiAdDet
      real(4) tPRprish            	! tPReprFish
      real(4) tPAgeish            	! tPAgeFish
      real(4) tPMgriJv            	! tPMigrFiJv
      real(4) tPCFiJv             	! tPConsFiJv
      real(4) afPssiJv            	! afPAssFiJv
      real(4) tPAssiJv            	! tPAssFiJv
      real(4) tPEFiJv             	! tPEgesFiJv
      real(4) tPEcriJv            	! tPExcrFiJv
      real(4) tPMFiJv             	! tPMortFiJv
      real(4) tPMgriAd            	! tPMigrFiAd
      real(4) tPCFiAd             	! tPConsFiAd
      real(4) tPAssiAd            	! tPAssFiAd
      real(4) tPEFiAd             	! tPEgesFiAd
      real(4) tPEcriAd            	! tPExcrFiAd
      real(4) tPMFiAd             	! tPMortFiAd
      real(4) tPHrvish            	! tPHarvFish
      real(4) tPMiJBot            	! tPMortFiJvBot
      real(4) tPMiJPO4            	! tPMortFiJvPO4
      real(4) tPMFivDt            	! tPMortFiJvDet
      real(4) tPMiABot            	! tPMortFiAdBot
      real(4) tPMiAPO4            	! tPMortFiAdPO4
      real(4) tPMFidDt            	! tPMortFiAdDet
      real(4) tPEiJPO4            	! tPEgesFiJvPO4
      real(4) tPEFivDt            	! tPEgesFiJvDet
      real(4) tPEiAPO4            	! tPEgesFiAdPO4
      real(4) tPEFidDt            	! tPEgesFiAdDet
      real(4) tNRprish            	! tNReprFish
      real(4) tNAgeish            	! tNAgeFish
      real(4) tNMgriJv            	! tNMigrFiJv
      real(4) tNCFiJv             	! tNConsFiJv
      real(4) afNssiJv            	! afNAssFiJv
      real(4) tNAssiJv            	! tNAssFiJv
      real(4) tNEFiJv             	! tNEgesFiJv
      real(4) tNEcriJv            	! tNExcrFiJv
      real(4) tNMFiJv             	! tNMortFiJv
      real(4) tNMgriAd            	! tNMigrFiAd
      real(4) tNCFiAd             	! tNConsFiAd
      real(4) tNAssiAd            	! tNAssFiAd
      real(4) tNEFiAd             	! tNEgesFiAd
      real(4) tNEcriAd            	! tNExcrFiAd
      real(4) tNMFiAd             	! tNMortFiAd
      real(4) tNHrvish            	! tNHarvFish
      real(4) tNMiABot            	! tNMortFiAdBot
      real(4) tNMiANH4            	! tNMortFiAdNH4
      real(4) tNMFidDt            	! tNMortFiAdDet
      real(4) tNMiJBot            	! tNMortFiJvBot
      real(4) tNMiJNH4            	! tNMortFiJvNH4
      real(4) tNMFivDt            	! tNMortFiJvDet
      real(4) tNEiJNH4            	! tNEgesFiJvNH4
      real(4) tNEFivDt            	! tNEgesFiJvDet
      real(4) tDMgrisc            	! tDMigrPisc
      real(4) aDFish              	! aDFish
      real(4) aDSatisc            	! aDSatPisc
      real(4) tDEnvisc            	! tDEnvPisc
      real(4) tDAssisc            	! tDAssPisc
      real(4) tDCPisc             	! tDConsPisc
      real(4) tDEPisc             	! tDEgesPisc
      real(4) tDCiJisc            	! tDConsFiJvPisc
      real(4) tDCiAisc            	! tDConsFiAdPisc
      real(4) tDMPisc             	! tDMortPisc
      real(4) tDMisBot            	! tDMortPiscBot
      real(4) tDMPicDt            	! tDMortPiscDet
      real(4) tDHrvisc            	! tDHarvPisc
      real(4) aPPisc              	! aPPisc
      real(4) tPCiJisc            	! tPConsFiJvPisc
      real(4) tPCiAisc            	! tPConsFiAdPisc
      real(4) tPCPisc             	! tPConsPisc
      real(4) rPDooisc            	! rPDFoodPisc
      real(4) aNPisc              	! aNPisc
      real(4) tNCiJisc            	! tNConsFiJvPisc
      real(4) tNCiAisc            	! tNConsFiAdPisc
      real(4) tNCPisc             	! tNConsPisc
      real(4) rNDooisc            	! rNDFoodPisc
      real(4) tDCPiscN            	! tDConsPiscNut
      real(4) tDEPiscN            	! tDEgesPiscNut
      real(4) tDCFJPNu            	! tDConsFiJvPiscNut
      real(4) tDCFAPNu            	! tDConsFiAdPiscNut
      real(4) tPCFJPNu            	! tPConsFiJvPiscNut
      real(4) tPCFAPNu            	! tPConsFiAdPiscNut
      real(4) tPCPiscN            	! tPConsPiscNut
      real(4) tNCFJPNu            	! tNConsFiJvPiscNut
      real(4) tNCFAPNu            	! tNConsFiAdPiscNut
      real(4) tNCPiscN            	! tNConsPiscNut
      real(4) afPssisc            	! afPAssPisc
      real(4) tPAssisc            	! tPAssPisc
      real(4) tPEPisc             	! tPEgesPisc
      real(4) tPEisPO4            	! tPEgesPiscPO4
      real(4) tPEPicDt            	! tPEgesPiscDet
      real(4) tDRspisc            	! tDRespPisc
      real(4) tPEcrisc            	! tPExcrPisc
      real(4) tPMPisc             	! tPMortPisc
      real(4) tPMisBot            	! tPMortPiscBot
      real(4) tPMisPO4            	! tPMortPiscPO4
      real(4) tPMPicDt            	! tPMortPiscDet
      real(4) tPMgrisc            	! tPMigrPisc
      real(4) tPHrvisc            	! tPHarvPisc
      real(4) afNssisc            	! afNAssPisc
      real(4) tNAssisc            	! tNAssPisc
      real(4) tNEPisc             	! tNEgesPisc
      real(4) tNEisNH4            	! tNEgesPiscNH4
      real(4) tNEPicDt            	! tNEgesPiscDet
      real(4) tNEcrisc            	! tNExcrPisc
      real(4) tNMPisc             	! tNMortPisc
      real(4) tNMisBot            	! tNMortPiscBot
      real(4) tNMisNH4            	! tNMortPiscNH4
      real(4) tNMPicDt            	! tNMortPiscDet
      real(4) tNMgrisc            	! tNMigrPisc
      real(4) tNHrvisc            	! tNHarvPisc
      real(4) tDWebBnt            	! tDWebBent
      real(4) tPWebBnt            	! tPWebBent
      real(4) tNWebBnt            	! tNWebBent
      real(4) tDWebiJv            	! tDWebFiJv
      real(4) tPWebiJv            	! tPWebFiJv
      real(4) tNWebiJv            	! tNWebFiJv
      real(4) tDWebiAd            	! tDWebFiAd
      real(4) tPWebiAd            	! tPWebFiAd
      real(4) tNWebiAd            	! tNWebFiAd
      real(4) tDWebisc            	! tDWebPisc
      real(4) wDWebooH            	! wDWebZooHyp
      real(4) wDWebtWH            	! wDWebDetWHyp
      real(4) wDWebiWH            	! wDWebDiatWHyp
      real(4) wDWbGnWH            	! wDWebGrenWHyp
      real(4) wDWbBeWH            	! wDWebBlueWHyp
      real(4) wDWebooE            	! wDWebZooEpi
      real(4) wDWebtWE            	! wDWebDetWEpi
      real(4) wDWebiWE            	! wDWebDiatWEpi
      real(4) wDWbGnWE            	! wDWebGrenWEpi
      real(4) wDWbBeWE            	! wDWebBlueWEpi
      real(4) tDWebotT            	! tDWebTotT
      real(4) wPWebooH            	! wPWebZooHyp
      real(4) wPWbP4WH            	! wPWebPO4WHyp
      real(4) wPWebtWH            	! wPWebDetWHyp
      real(4) wPWebiWH            	! wPWebDiatWHyp
      real(4) wPWbGnWH            	! wPWebGrenWHyp
      real(4) wPWbBeWH            	! wPWebBlueWHyp
      real(4) wPWebooE            	! wPWebZooEpi
      real(4) wPWbP4WE            	! wPWebPO4WEpi
      real(4) wPWebtWE            	! wPWebDetWEpi
      real(4) wPWebiWE            	! wPWebDiatWEpi
      real(4) wPWbGnWE            	! wPWebGrenWEpi
      real(4) wPWbBeWE            	! wPWebBlueWEpi
      real(4) tPWebotT            	! tPWebTotT
      real(4) tNEiANH4            	! tNEgesFiAdNH4
      real(4) tNEFidDt            	! tNEgesFiAdDet
      real(4) wNWbN3WH            	! wNWebNO3WHyp
      real(4) wNWbN3WE            	! wNWebNO3WEpi
      real(4) wNWebooH            	! wNWebZooHyp
      real(4) wNWbN4WH            	! wNWebNH4WHyp
      real(4) wNWebtWH            	! wNWebDetWHyp
      real(4) wNWebiWH            	! wNWebDiatWHyp
      real(4) wNWbGnWH            	! wNWebGrenWHyp
      real(4) wNWbBeWH            	! wNWebBlueWHyp
      real(4) wNWebooE            	! wNWebZooEpi
      real(4) wNWbN4WE            	! wNWebNH4WEpi
      real(4) wNWebtWE            	! wNWebDetWEpi
      real(4) wNWebiWE            	! wNWebDiatWEpi
      real(4) wNWbGnWE            	! wNWebGrenWEpi
      real(4) wNWbBeWE            	! wNWebBlueWEpi
      real(4) tNWebotT            	! tNWebTotT
      real(4) wSiebtWE            	! wSiWebDetWEpi
      real(4) aPrefveE            	! aPrefAveEpi
      real(4) wDCZoo2E            	! wDConsZoo2Epi
      real(4) aDCZoSpE            	! aDConsZooSpEpi
      real(4) aDAsZSpE            	! aDAssZooSpEpi
      real(4) aDGraSpE            	! aDGrazSpEpi
      real(4) aPCZoSpE            	! aPConsZooSpEpi
      real(4) aPGraSpE            	! aPGrazSpEpi
      real(4) aNCZoSpE            	! aNConsZooSpEpi
      real(4) aNGraSpE            	! aNGrazSpEpi
      real(4) tDSetotE            	! tDSetTotEpi
      real(4) tPSetotE            	! tPSetTotEpi
      real(4) tNSetotE            	! tNSetTotEpi
      real(4) tDRsuTot            	! tDResusTot
      real(4) tPRsuTot            	! tPResusTot
      real(4) tNRsuTot            	! tNResusTot
      real(4), save :: aDptSart
      real(4) akDreDpt            	! akDredDepth
      real(4) akDred              	! akDred
      real(4) akDreBnt            	! akDredBent
      real(4) vDredptW            	! vDredDepthW
      real(4) tDDreDtS            	! tDDredDetS
      real(4) tPDreDtS            	! tPDredDetS
      real(4) tNDreDtS            	! tNDredDetS
      real(4) tSireDtS            	! tSiDredDetS
      real(4) tPDedIMS            	! tPDredAIMS
      real(4) tDDdNoil            	! tDDredNetSoil
      real(4) tDDdNIMS            	! tDDredNetIMS
      real(4) tDDdNumS            	! tDDredNetHumS
      real(4) tPDdNumS            	! tPDredNetHumS
      real(4) tNDdNumS            	! tNDredNetHumS
      real(4) tDDreDiS            	! tDDredDiatS
      real(4) tPDreDiS            	! tPDredDiatS
      real(4) tNDreDiS            	! tNDredDiatS
      real(4) tDDedenS            	! tDDredGrenS
      real(4) tPDedenS            	! tPDredGrenS
      real(4) tNDedenS            	! tNDredGrenS
      real(4) tDDedueS            	! tDDredBlueS
      real(4) tPDedueS            	! tPDredBlueS
      real(4) tNDedueS            	! tNDredBlueS
      real(4) tDDedytS            	! tDDredPhytS
      real(4) tPDedytS            	! tPDredPhytS
      real(4) tNDedytS            	! tNDredPhytS
      real(4) tDDreBnt            	! tDDredBent
      real(4) tPDreBnt            	! tPDredBent
      real(4) tNDreBnt            	! tNDredBent
      real(4) tDDreVeg            	! tDDredVeg
      real(4) tPDreVeg            	! tPDredVeg
      real(4) tNDreVeg            	! tNDredVeg
      real(4) tDDdNTot            	! tDDredNetTot
      real(4) tPDdNTot            	! tPDredNetTot
      real(4) tNDdNTot            	! tNDredNetTot
      real(4) tSireTot            	! tSiDredTot
      real(4) tDIMS               	! tDIMS
      real(4) tDHumS              	! tDHumS
      real(4) tDDtS               	! tDDetS
      real(4) vDeltaS             	! vDeltaS
      real(4) vDeltaW             	! vDeltaW
      real(4) tDBurIM             	! tDBurIM
      real(4) tDBurOM             	! tDBurOM
      real(4) tDBurDt             	! tDBurDet
      real(4) tDBurHum            	! tDBurHum
      real(4) tDBurTot            	! tDBurTot
      real(4) tPBurHum            	! tPBurHum
      real(4) tPBurDt             	! tPBurDet
      real(4) tPBurAIM            	! tPBurAIM
      real(4) tPBurPO4            	! tPBurPO4
      real(4) tPBurTot            	! tPBurTot
      real(4) tNBurHum            	! tNBurHum
      real(4) tNBurDt             	! tNBurDet
      real(4) tNBurNH4            	! tNBurNH4
      real(4) tNBurNO3            	! tNBurNO3
      real(4) tNBurTot            	! tNBurTot
      real(4) tSiBurDt            	! tSiBurDet
      real(4) tSiBuTot            	! tSiBurTot
      real(4) aReDeaWE            	! aRelDeltaWEpi
      real(4) aReDeaWH            	! aRelDeltaWHyp
      real(4) aPFish              	! aPFish
      real(4) aNFish              	! aNFish
      real(4) aDRelotT            	! aDRelTotT
      real(4) aNRelotT            	! aNRelTotT
      real(4) aPRelotT            	! aPRelTotT
      real(4) aSielotT            	! aSiRelTotT
      real(4) aO2elotT            	! aO2RelTotT
      real(4) aDTotTH             	! aDTotTHyp
      real(4) aNTotTH             	! aNTotTHyp
      real(4) aPTotTH             	! aPTotTHyp
      real(4) aSiTotTH            	! aSiTotTHyp
      real(4) aO2TotTH            	! aO2TotTHyp
      real(4) aDTotT              	! aDTotT
      real(4) aNTotT              	! aNTotT
      real(4) aPTotT              	! aPTotT
      real(4) aSiTotT             	! aSiTotT
      real(4) aO2TotT             	! aO2TotT
      real(4) tDBedotT            	! tDBedTotT
      real(4) tPBedotT            	! tPBedTotT
      real(4) tNBedotT            	! tNBedTotT
      real(4) aDError             	! aDError
      real(4) aNError             	! aNError
      real(4) aPError             	! aPError
      real(4) aSiError            	! aSiError
      real(4) aO2Error            	! aO2Error
      real(4) cChVegH             	! cCheckVegHeight
      real(4) dNH4WH              	! dNH4WHyp
      real(4) dNO3WH              	! dNO3WHyp
      real(4) dPO4WH              	! dPO4WHyp
      real(4) dPAIMWH             	! dPAIMWHyp
      real(4) dSiO2WH             	! dSiO2WHyp
      real(4) dO2WH               	! dO2WHyp
      real(4) dDDetWH             	! dDDetWHyp
      real(4) dNDetWH             	! dNDetWHyp
      real(4) dPDetWH             	! dPDetWHyp
      real(4) dSiDetWH            	! dSiDetWHyp
      real(4) dDIMWH              	! dDIMWHyp
      real(4) dDDiatWH            	! dDDiatWHyp
      real(4) dNDiatWH            	! dNDiatWHyp
      real(4) dPDiatWH            	! dPDiatWHyp
      real(4) dDGrenWH            	! dDGrenWHyp
      real(4) dNGrenWH            	! dNGrenWHyp
      real(4) dPGrenWH            	! dPGrenWHyp
      real(4) dDBlueWH            	! dDBlueWHyp
      real(4) dNBlueWH            	! dNBlueWHyp
      real(4) dPBlueWH            	! dPBlueWHyp
      real(4) dDZooH              	! dDZooHyp
      real(4) dNZooH              	! dNZooHyp
      real(4) dPZooH              	! dPZooHyp
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
      real(4) dVegHe              	! dVegHeight
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
      real(4) dDRoPhra            	! dDRootPhra
      real(4) dDShPhra            	! dDShootPhra
      real(4) dNRoPhra            	! dNRootPhra
      real(4) dNShPhra            	! dNShootPhra
      real(4) dPRoPhra            	! dPRootPhra
      real(4) dPShPhra            	! dPShootPhra
      real(4) dNH4WE              	! dNH4WEpi
      real(4) dNO3WE              	! dNO3WEpi
      real(4) dPO4WE              	! dPO4WEpi
      real(4) dPAIMWE             	! dPAIMWEpi
      real(4) dSiO2WE             	! dSiO2WEpi
      real(4) dO2WE               	! dO2WEpi
      real(4) dDDetWE             	! dDDetWEpi
      real(4) dNDetWE             	! dNDetWEpi
      real(4) dPDetWE             	! dPDetWEpi
      real(4) dSiDetWE            	! dSiDetWEpi
      real(4) dDIMWE              	! dDIMWEpi
      real(4) dDDiatWE            	! dDDiatWEpi
      real(4) dNDiatWE            	! dNDiatWEpi
      real(4) dPDiatWE            	! dPDiatWEpi
      real(4) dDGrenWE            	! dDGrenWEpi
      real(4) dNGrenWE            	! dNGrenWEpi
      real(4) dPGrenWE            	! dPGrenWEpi
      real(4) dDBlueWE            	! dDBlueWEpi
      real(4) dNBlueWE            	! dNBlueWEpi
      real(4) dPBlueWE            	! dPBlueWEpi
      real(4) dDZooE              	! dDZooEpi
      real(4) dNZooE              	! dNZooEpi
      real(4) dPZooE              	! dPZooEpi
      real(4) dDExTotT            	! dDExtTotT
      real(4) dNExTotT            	! dNExtTotT
      real(4) dPExTotT            	! dPExtTotT
      real(4) dSiETotT            	! dSiExtTotT
      real(4) dO2ETotT            	! dO2ExtTotT
!
!
!     /* ==============================  */
!     /* declaration fluxes              */
!     /* ==============================  */
      real(4) D0sNH4WH         	 ! flux of sNH4WH
      real(4) D0sNO3WH         	 ! flux of sNO3WH
      real(4) D0sPO4WH         	 ! flux of sPO4WH
      real(4) D0sPAIMWH        	 ! flux of sPAIMWH
      real(4) D0sSiO2WH        	 ! flux of sSiO2WH
      real(4) D0sO2WH          	 ! flux of sO2WH
      real(4) D0sDDetWH        	 ! flux of sDDetWH
      real(4) D0sNDetWH        	 ! flux of sNDetWH
      real(4) D0sPDetWH        	 ! flux of sPDetWH
      real(4) D0sSiDetWH       	 ! flux of sSiDetWH
      real(4) D0sDIMWH         	 ! flux of sDIMWH
      real(4) D0sDDiatWH       	 ! flux of sDDiatWH
      real(4) D0sNDiatWH       	 ! flux of sNDiatWH
      real(4) D0sPDiatWH       	 ! flux of sPDiatWH
      real(4) D0sDGrenWH       	 ! flux of sDGrenWH
      real(4) D0sNGrenWH       	 ! flux of sNGrenWH
      real(4) D0sPGrenWH       	 ! flux of sPGrenWH
      real(4) D0sDBlueWH       	 ! flux of sDBlueWH
      real(4) D0sNBlueWH       	 ! flux of sNBlueWH
      real(4) D0sPBlueWH       	 ! flux of sPBlueWH
      real(4) D0sDZooH         	 ! flux of sDZooH
      real(4) D0sNZooH         	 ! flux of sNZooH
      real(4) D0sPZooH         	 ! flux of sPZooH
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
      real(4) D0sVegHe         	 ! flux of sVegHe
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
      real(4) D0sDRoPhra       	 ! flux of sDRoPhra
      real(4) D0sDShPhra       	 ! flux of sDShPhra
      real(4) D0sNRoPhra       	 ! flux of sNRoPhra
      real(4) D0sNShPhra       	 ! flux of sNShPhra
      real(4) D0sPRoPhra       	 ! flux of sPRoPhra
      real(4) D0sPShPhra       	 ! flux of sPShPhra
      real(4) D0sNH4WE         	 ! flux of sNH4WE
      real(4) D0sNO3WE         	 ! flux of sNO3WE
      real(4) D0sPO4WE         	 ! flux of sPO4WE
      real(4) D0sPAIMWE        	 ! flux of sPAIMWE
      real(4) D0sSiO2WE        	 ! flux of sSiO2WE
      real(4) D0sO2WE          	 ! flux of sO2WE
      real(4) D0sDDetWE        	 ! flux of sDDetWE
      real(4) D0sNDetWE        	 ! flux of sNDetWE
      real(4) D0sPDetWE        	 ! flux of sPDetWE
      real(4) D0sSiDetWE       	 ! flux of sSiDetWE
      real(4) D0sDIMWE         	 ! flux of sDIMWE
      real(4) D0sDDiatWE       	 ! flux of sDDiatWE
      real(4) D0sNDiatWE       	 ! flux of sNDiatWE
      real(4) D0sPDiatWE       	 ! flux of sPDiatWE
      real(4) D0sDGrenWE       	 ! flux of sDGrenWE
      real(4) D0sNGrenWE       	 ! flux of sNGrenWE
      real(4) D0sPGrenWE       	 ! flux of sPGrenWE
      real(4) D0sDBlueWE       	 ! flux of sDBlueWE
      real(4) D0sNBlueWE       	 ! flux of sNBlueWE
      real(4) D0sPBlueWE       	 ! flux of sPBlueWE
      real(4) D0sDZooE         	 ! flux of sDZooE
      real(4) D0sNZooE         	 ! flux of sNZooE
      real(4) D0sPZooE         	 ! flux of sPZooE
      real(4) D0sDExTotT       	 ! flux of sDExTotT
      real(4) D0sNExTotT       	 ! flux of sNExTotT
      real(4) D0sPExTotT       	 ! flux of sPExTotT
      real(4) D0sSiETotT       	 ! flux of sSiETotT
      real(4) D0sO2ETotT       	 ! flux of sO2ETotT
!
!
!     /* ==============================  */
!     /* declaration pointer to flux     */
!     /* ==============================  */
      integer ID0sNH4WH         	 ! pointer to flux variable 
      integer ID0sNO3WH         	 ! pointer to flux variable 
      integer ID0sPO4WH         	 ! pointer to flux variable 
      integer ID0sPAIMWH        	 ! pointer to flux variable 
      integer ID0sSiO2WH        	 ! pointer to flux variable 
      integer ID0sO2WH          	 ! pointer to flux variable 
      integer ID0sDDetWH        	 ! pointer to flux variable 
      integer ID0sNDetWH        	 ! pointer to flux variable 
      integer ID0sPDetWH        	 ! pointer to flux variable 
      integer ID0sSiDetWH       	 ! pointer to flux variable 
      integer ID0sDIMWH         	 ! pointer to flux variable 
      integer ID0sDDiatWH       	 ! pointer to flux variable 
      integer ID0sNDiatWH       	 ! pointer to flux variable 
      integer ID0sPDiatWH       	 ! pointer to flux variable 
      integer ID0sDGrenWH       	 ! pointer to flux variable 
      integer ID0sNGrenWH       	 ! pointer to flux variable 
      integer ID0sPGrenWH       	 ! pointer to flux variable 
      integer ID0sDBlueWH       	 ! pointer to flux variable 
      integer ID0sNBlueWH       	 ! pointer to flux variable 
      integer ID0sPBlueWH       	 ! pointer to flux variable 
      integer ID0sDZooH         	 ! pointer to flux variable 
      integer ID0sNZooH         	 ! pointer to flux variable 
      integer ID0sPZooH         	 ! pointer to flux variable 
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
      integer ID0sVegHe         	 ! pointer to flux variable 
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
      integer ID0sDRoPhra       	 ! pointer to flux variable 
      integer ID0sDShPhra       	 ! pointer to flux variable 
      integer ID0sNRoPhra       	 ! pointer to flux variable 
      integer ID0sNShPhra       	 ! pointer to flux variable 
      integer ID0sPRoPhra       	 ! pointer to flux variable 
      integer ID0sPShPhra       	 ! pointer to flux variable 
      integer ID0sNH4WE         	 ! pointer to flux variable 
      integer ID0sNO3WE         	 ! pointer to flux variable 
      integer ID0sPO4WE         	 ! pointer to flux variable 
      integer ID0sPAIMWE        	 ! pointer to flux variable 
      integer ID0sSiO2WE        	 ! pointer to flux variable 
      integer ID0sO2WE          	 ! pointer to flux variable 
      integer ID0sDDetWE        	 ! pointer to flux variable 
      integer ID0sNDetWE        	 ! pointer to flux variable 
      integer ID0sPDetWE        	 ! pointer to flux variable 
      integer ID0sSiDetWE       	 ! pointer to flux variable 
      integer ID0sDIMWE         	 ! pointer to flux variable 
      integer ID0sDDiatWE       	 ! pointer to flux variable 
      integer ID0sNDiatWE       	 ! pointer to flux variable 
      integer ID0sPDiatWE       	 ! pointer to flux variable 
      integer ID0sDGrenWE       	 ! pointer to flux variable 
      integer ID0sNGrenWE       	 ! pointer to flux variable 
      integer ID0sPGrenWE       	 ! pointer to flux variable 
      integer ID0sDBlueWE       	 ! pointer to flux variable 
      integer ID0sNBlueWE       	 ! pointer to flux variable 
      integer ID0sPBlueWE       	 ! pointer to flux variable 
      integer ID0sDZooE         	 ! pointer to flux variable 
      integer ID0sNZooE         	 ! pointer to flux variable 
      integer ID0sPZooE         	 ! pointer to flux variable 
      integer ID0sDExTotT       	 ! pointer to flux variable 
      integer ID0sNExTotT       	 ! pointer to flux variable 
      integer ID0sPExTotT       	 ! pointer to flux variable 
      integer ID0sSiETotT       	 ! pointer to flux variable 
      integer ID0sO2ETotT       	 ! pointer to flux variable 
integer, save :: counter1 = 0
integer, save :: counter2 = 0
integer, save :: counter3 = 0
CHARACTER :: textfile
 !   
 !*******************************************************************************    
 !   
      ipnt        = ipoint
      ID0sNH4WH         	= 1 
      ID0sNO3WH         	= 2 
      ID0sPO4WH         	= 3 
      ID0sPAIMWH        	= 4 
      ID0sSiO2WH        	= 5 
      ID0sO2WH          	= 6 
      ID0sDDetWH        	= 7 
      ID0sNDetWH        	= 8 
      ID0sPDetWH        	= 9 
      ID0sSiDetWH       	= 10 
      ID0sDIMWH         	= 11 
      ID0sDDiatWH       	= 12 
      ID0sNDiatWH       	= 13 
      ID0sPDiatWH       	= 14 
      ID0sDGrenWH       	= 15 
      ID0sNGrenWH       	= 16 
      ID0sPGrenWH       	= 17 
      ID0sDBlueWH       	= 18 
      ID0sNBlueWH       	= 19 
      ID0sPBlueWH       	= 20 
      ID0sDZooH         	= 21 
      ID0sNZooH         	= 22 
      ID0sPZooH         	= 23 
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
      ID0sVegHe         	= 55 
      ID0sDBent         	= 56 
      ID0sNBent         	= 57 
      ID0sPBent         	= 58 
      ID0sDepthWM       	= 59 
      ID0sNH4WM         	= 60 
      ID0sNO3WM         	= 61 
      ID0sPO4WM         	= 62 
      ID0sPAIMWM        	= 63 
      ID0sSiO2WM        	= 64 
      ID0sO2WM          	= 65 
      ID0sDDetWM        	= 66 
      ID0sNDetWM        	= 67 
      ID0sPDetWM        	= 68 
      ID0sSiDetWM       	= 69 
      ID0sDIMWM         	= 70 
      ID0sDDiatWM       	= 71 
      ID0sNDiatWM       	= 72 
      ID0sPDiatWM       	= 73 
      ID0sDGrenWM       	= 74 
      ID0sNGrenWM       	= 75 
      ID0sPGrenWM       	= 76 
      ID0sDBlueWM       	= 77 
      ID0sNBlueWM       	= 78 
      ID0sPBlueWM       	= 79 
      ID0sDZooM         	= 80 
      ID0sNZooM         	= 81 
      ID0sPZooM         	= 82 
      ID0sNH4SM         	= 83 
      ID0sNO3SM         	= 84 
      ID0sPO4SM         	= 85 
      ID0sPAIMSM        	= 86 
      ID0sDDetSM        	= 87 
      ID0sNDetSM        	= 88 
      ID0sPDetSM        	= 89 
      ID0sSiDetSM       	= 90 
      ID0sDHumSM        	= 91 
      ID0sNHumSM        	= 92 
      ID0sPHumSM        	= 93 
      ID0sDIMSM         	= 94 
      ID0sDRoPhra       	= 95 
      ID0sDShPhra       	= 96 
      ID0sNRoPhra       	= 97 
      ID0sNShPhra       	= 98 
      ID0sPRoPhra       	= 99 
      ID0sPShPhra       	= 100 
      ID0sNH4WE         	= 101 
      ID0sNO3WE         	= 102 
      ID0sPO4WE         	= 103 
      ID0sPAIMWE        	= 104 
      ID0sSiO2WE        	= 105 
      ID0sO2WE          	= 106 
      ID0sDDetWE        	= 107 
      ID0sNDetWE        	= 108 
      ID0sPDetWE        	= 109 
      ID0sSiDetWE       	= 110 
      ID0sDIMWE         	= 111 
      ID0sDDiatWE       	= 112 
      ID0sNDiatWE       	= 113 
      ID0sPDiatWE       	= 114 
      ID0sDGrenWE       	= 115 
      ID0sNGrenWE       	= 116 
      ID0sPGrenWE       	= 117 
      ID0sDBlueWE       	= 118 
      ID0sNBlueWE       	= 119 
      ID0sPBlueWE       	= 120 
      ID0sDZooE         	= 121 
      ID0sNZooE         	= 122 
      ID0sPZooE         	= 123 
      ID0sDExTotT       	= 124 
      ID0sNExTotT       	= 125 
      ID0sPExTotT       	= 126 
      ID0sSiETotT       	= 127 
      ID0sO2ETotT       	= 128 
 !   
       do 9000 iseg = 1 , noseg
 !   
         sNH4WH              	= pmsa( ipnt( 1) )
         sNO3WH              	= pmsa( ipnt( 2) )
         sPO4WH              	= pmsa( ipnt( 3) )
         sPAIMWH             	= pmsa( ipnt( 4) )
         sSiO2WH             	= pmsa( ipnt( 5) )
         sO2WH               	= pmsa( ipnt( 6) )
         sDDetWH             	= pmsa( ipnt( 7) )
         sNDetWH             	= pmsa( ipnt( 8) )
         sPDetWH             	= pmsa( ipnt( 9) )
         sSiDetWH            	= pmsa( ipnt( 10) )
         sDIMWH              	= pmsa( ipnt( 11) )
         sDDiatWH            	= pmsa( ipnt( 12) )
         sNDiatWH            	= pmsa( ipnt( 13) )
         sPDiatWH            	= pmsa( ipnt( 14) )
         sDGrenWH            	= pmsa( ipnt( 15) )
         sNGrenWH            	= pmsa( ipnt( 16) )
         sPGrenWH            	= pmsa( ipnt( 17) )
         sDBlueWH            	= pmsa( ipnt( 18) )
         sNBlueWH            	= pmsa( ipnt( 19) )
         sPBlueWH            	= pmsa( ipnt( 20) )
         sDZooH              	= pmsa( ipnt( 21) )
         sNZooH              	= pmsa( ipnt( 22) )
         sPZooH              	= pmsa( ipnt( 23) )
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
         sVegHe              	= pmsa( ipnt( 55) )
         sDBent              	= pmsa( ipnt( 56) )
         sNBent              	= pmsa( ipnt( 57) )
         sPBent              	= pmsa( ipnt( 58) )
         sDepthWM            	= pmsa( ipnt( 59) )
         sNH4WM              	= pmsa( ipnt( 60) )
         sNO3WM              	= pmsa( ipnt( 61) )
         sPO4WM              	= pmsa( ipnt( 62) )
         sPAIMWM             	= pmsa( ipnt( 63) )
         sSiO2WM             	= pmsa( ipnt( 64) )
         sO2WM               	= pmsa( ipnt( 65) )
         sDDetWM             	= pmsa( ipnt( 66) )
         sNDetWM             	= pmsa( ipnt( 67) )
         sPDetWM             	= pmsa( ipnt( 68) )
         sSiDetWM            	= pmsa( ipnt( 69) )
         sDIMWM              	= pmsa( ipnt( 70) )
         sDDiatWM            	= pmsa( ipnt( 71) )
         sNDiatWM            	= pmsa( ipnt( 72) )
         sPDiatWM            	= pmsa( ipnt( 73) )
         sDGrenWM            	= pmsa( ipnt( 74) )
         sNGrenWM            	= pmsa( ipnt( 75) )
         sPGrenWM            	= pmsa( ipnt( 76) )
         sDBlueWM            	= pmsa( ipnt( 77) )
         sNBlueWM            	= pmsa( ipnt( 78) )
         sPBlueWM            	= pmsa( ipnt( 79) )
         sDZooM              	= pmsa( ipnt( 80) )
         sNZooM              	= pmsa( ipnt( 81) )
         sPZooM              	= pmsa( ipnt( 82) )
         sNH4SM              	= pmsa( ipnt( 83) )
         sNO3SM              	= pmsa( ipnt( 84) )
         sPO4SM              	= pmsa( ipnt( 85) )
         sPAIMSM             	= pmsa( ipnt( 86) )
         sDDetSM             	= pmsa( ipnt( 87) )
         sNDetSM             	= pmsa( ipnt( 88) )
         sPDetSM             	= pmsa( ipnt( 89) )
         sSiDetSM            	= pmsa( ipnt( 90) )
         sDHumSM             	= pmsa( ipnt( 91) )
         sNHumSM             	= pmsa( ipnt( 92) )
         sPHumSM             	= pmsa( ipnt( 93) )
         sDIMSM              	= pmsa( ipnt( 94) )
         sDRoPhra            	= pmsa( ipnt( 95) )
         sDShPhra            	= pmsa( ipnt( 96) )
         sNRoPhra            	= pmsa( ipnt( 97) )
         sNShPhra            	= pmsa( ipnt( 98) )
         sPRoPhra            	= pmsa( ipnt( 99) )
         sPShPhra            	= pmsa( ipnt( 100) )
         sNH4WE              	= pmsa( ipnt( 101) )
         sNO3WE              	= pmsa( ipnt( 102) )
         sPO4WE              	= pmsa( ipnt( 103) )
         sPAIMWE             	= pmsa( ipnt( 104) )
         sSiO2WE             	= pmsa( ipnt( 105) )
         sO2WE               	= pmsa( ipnt( 106) )
         sDDetWE             	= pmsa( ipnt( 107) )
         sNDetWE             	= pmsa( ipnt( 108) )
         sPDetWE             	= pmsa( ipnt( 109) )
         sSiDetWE            	= pmsa( ipnt( 110) )
         sDIMWE              	= pmsa( ipnt( 111) )
         sDDiatWE            	= pmsa( ipnt( 112) )
         sNDiatWE            	= pmsa( ipnt( 113) )
         sPDiatWE            	= pmsa( ipnt( 114) )
         sDGrenWE            	= pmsa( ipnt( 115) )
         sNGrenWE            	= pmsa( ipnt( 116) )
         sPGrenWE            	= pmsa( ipnt( 117) )
         sDBlueWE            	= pmsa( ipnt( 118) )
         sNBlueWE            	= pmsa( ipnt( 119) )
         sPBlueWE            	= pmsa( ipnt( 120) )
         sDZooE              	= pmsa( ipnt( 121) )
         sNZooE              	= pmsa( ipnt( 122) )
         sPZooE              	= pmsa( ipnt( 123) )
         sDExTotT            	= pmsa( ipnt( 124) )
         sNExTotT            	= pmsa( ipnt( 125) )
         sPExTotT            	= pmsa( ipnt( 126) )
         sSiETotT            	= pmsa( ipnt( 127) )
         sO2ETotT            	= pmsa( ipnt( 128) )
         ITIME               	= pmsa( ipnt( 129) )
         TotalDepth        	= pmsa( ipnt( 130) )
         InitCalc            	= pmsa( ipnt( 131) )
         CalcMass            	= pmsa( ipnt( 132) )
         ContDpth            	= pmsa( ipnt( 133) )
         InitDpth            	= pmsa( ipnt( 134) )
         ContTans            	= pmsa( ipnt( 135) )
         InclTran            	= pmsa( ipnt( 136) )
         InclV               	= pmsa( ipnt( 137) )
         InclBur             	= pmsa( ipnt( 138) )
         InclWeb             	= pmsa( ipnt( 139) )
         InclMrsh            	= pmsa( ipnt( 140) )
         IncSeson            	= pmsa( ipnt( 141) )
         InclSrat            	= pmsa( ipnt( 142) )
         calixpth            	= pmsa( ipnt( 143) )
         InclLat             	= pmsa( ipnt( 144) )
         calcQEv             	= pmsa( ipnt( 145) )
         DayStart            	= pmsa( ipnt( 146) )
         ReadTemp            	= pmsa( ipnt( 147) )
         ReadSrat            	= pmsa( ipnt( 148) )
         ReadLOut            	= pmsa( ipnt( 149) )
         ReadVind            	= pmsa( ipnt( 150) )
         ReadQIn             	= pmsa( ipnt( 151) )
         ReadQOut            	= pmsa( ipnt( 152) )
         ReadQEv             	= pmsa( ipnt( 153) )
         ReadPoad            	= pmsa( ipnt( 154) )
         ReadNoad            	= pmsa( ipnt( 155) )
         ReaNurac            	= pmsa( ipnt( 156) )
         ReaPLadP            	= pmsa( ipnt( 157) )
         ReaDLdDe            	= pmsa( ipnt( 158) )
         ReaVSade            	= pmsa( ipnt( 159) )
         UsePoerV            	= pmsa( ipnt( 160) )
         UsePhotV            	= pmsa( ipnt( 161) )
         UseLWinV            	= pmsa( ipnt( 162) )
         UseVHe              	= pmsa( ipnt( 163) )
         ReaDLdIM            	= pmsa( ipnt( 164) )
         Useasoad            	= pmsa( ipnt( 165) )
         Uselsoad            	= pmsa( ipnt( 166) )
         mTempE              	= pmsa( ipnt( 167) )
         mTempH              	= pmsa( ipnt( 168) )
         mStrat              	= pmsa( ipnt( 169) )
         mLOut               	= pmsa( ipnt( 170) )
         mVShade             	= pmsa( ipnt( 171) )
         mVWind              	= pmsa( ipnt( 172) )
         mQIn                	= pmsa( ipnt( 173) )
         mQOut               	= pmsa( ipnt( 174) )
         mQEv                	= pmsa( ipnt( 175) )
         mPLoad              	= pmsa( ipnt( 176) )
         mPLoaPO4            	= pmsa( ipnt( 177) )
         mPLoaOrg            	= pmsa( ipnt( 178) )
         mPLadTot            	= pmsa( ipnt( 179) )
         mNLoad              	= pmsa( ipnt( 180) )
         mNLoaNH4            	= pmsa( ipnt( 181) )
         mNLoaNO3            	= pmsa( ipnt( 182) )
         mNLoaOrg            	= pmsa( ipnt( 183) )
         mDLoadDe            	= pmsa( ipnt( 184) )
         mDLoadIM            	= pmsa( ipnt( 185) )
         StTime              	= pmsa( ipnt( 186) )
         EndTime             	= pmsa( ipnt( 187) )
         YearZero            	= pmsa( ipnt( 188) )
         cDepthW0            	= pmsa( ipnt( 189) )
         cMnephHE            	= pmsa( ipnt( 190) )
         cFetch              	= pmsa( ipnt( 191) )
         fMarsh              	= pmsa( ipnt( 192) )
         fLutum              	= pmsa( ipnt( 193) )
         kExchW              	= pmsa( ipnt( 194) )
         kExchMxW            	= pmsa( ipnt( 195) )
         cDeptMix            	= pmsa( ipnt( 196) )
         cAMix               	= pmsa( ipnt( 197) )
         cBMix               	= pmsa( ipnt( 198) )
         cfH                 	= pmsa( ipnt( 199) )
         fFeDIM              	= pmsa( ipnt( 200) )
         fAlDIM              	= pmsa( ipnt( 201) )
         fVShade             	= pmsa( ipnt( 202) )
         cTmAveE             	= pmsa( ipnt( 203) )
         cTmVarE             	= pmsa( ipnt( 204) )
         cTmAveH             	= pmsa( ipnt( 205) )
         cTmVarH             	= pmsa( ipnt( 206) )
         cStratTm            	= pmsa( ipnt( 207) )
         cTimeLag            	= pmsa( ipnt( 208) )
         cVWind              	= pmsa( ipnt( 209) )
         cQInf               	= pmsa( ipnt( 210) )
         cPBckoad            	= pmsa( ipnt( 211) )
         cNBckoad            	= pmsa( ipnt( 212) )
         cLDayAve            	= pmsa( ipnt( 213) )
         cLDayVar            	= pmsa( ipnt( 214) )
         cfDayAve            	= pmsa( ipnt( 215) )
         cfDayVar            	= pmsa( ipnt( 216) )
         fRefl               	= pmsa( ipnt( 217) )
         cExtWat             	= pmsa( ipnt( 218) )
         cDrInval            	= pmsa( ipnt( 219) )
         cDrdSart            	= pmsa( ipnt( 220) )
         cDeptRef            	= pmsa( ipnt( 221) )
         cLengred            	= pmsa( ipnt( 222) )
         fEffDred            	= pmsa( ipnt( 223) )
         fEfreent            	= pmsa( ipnt( 224) )
         fPAR                	= pmsa( ipnt( 225) )
         cLAlloV             	= pmsa( ipnt( 226) )
         cVIniLen            	= pmsa( ipnt( 227) )
         cVWinLen            	= pmsa( ipnt( 228) )
         cExtSpDe            	= pmsa( ipnt( 229) )
         cExtSpIM            	= pmsa( ipnt( 230) )
         fDTotS0             	= pmsa( ipnt( 231) )
         fDOrgS0             	= pmsa( ipnt( 232) )
         fDDeS0              	= pmsa( ipnt( 233) )
         fSedP0              	= pmsa( ipnt( 234) )
         fPInogS0            	= pmsa( ipnt( 235) )
         fPAdsS0             	= pmsa( ipnt( 236) )
         cPDDe0              	= pmsa( ipnt( 237) )
         cNDDe0              	= pmsa( ipnt( 238) )
         cSiDDe0             	= pmsa( ipnt( 239) )
         cPDHum0             	= pmsa( ipnt( 240) )
         cNDHum0             	= pmsa( ipnt( 241) )
         cPDP0               	= pmsa( ipnt( 242) )
         cNDP0               	= pmsa( ipnt( 243) )
         cPDDi0              	= pmsa( ipnt( 244) )
         cNDDi0              	= pmsa( ipnt( 245) )
         cPDGren0            	= pmsa( ipnt( 246) )
         cNDGren0            	= pmsa( ipnt( 247) )
         cPDBlue0            	= pmsa( ipnt( 248) )
         cNDBlue0            	= pmsa( ipnt( 249) )
         cPDV0               	= pmsa( ipnt( 250) )
         cNDV0               	= pmsa( ipnt( 251) )
         cSiDDi              	= pmsa( ipnt( 252) )
         cPDZoRef            	= pmsa( ipnt( 253) )
         cNDZoRef            	= pmsa( ipnt( 254) )
         cPDenRef            	= pmsa( ipnt( 255) )
         cNDenRef            	= pmsa( ipnt( 256) )
         cPDisRef            	= pmsa( ipnt( 257) )
         cNDisRef            	= pmsa( ipnt( 258) )
         cPDPisc             	= pmsa( ipnt( 259) )
         cNDPisc             	= pmsa( ipnt( 260) )
         cQIn                	= pmsa( ipnt( 261) )
         cQInSum             	= pmsa( ipnt( 262) )
         cQInWin             	= pmsa( ipnt( 263) )
         cMxWOut             	= pmsa( ipnt( 264) )
         cDeptWMx            	= pmsa( ipnt( 265) )
         cQIExpr1            	= pmsa( ipnt( 266) )
         cQIExct1            	= pmsa( ipnt( 267) )
         cQOtEpr1            	= pmsa( ipnt( 268) )
         cQOtEct1            	= pmsa( ipnt( 269) )
         cQEvAve             	= pmsa( ipnt( 270) )
         cQEvVar             	= pmsa( ipnt( 271) )
         cPLoad              	= pmsa( ipnt( 272) )
         cPLoaSum            	= pmsa( ipnt( 273) )
         cPLoaWin            	= pmsa( ipnt( 274) )
         fPO4In              	= pmsa( ipnt( 275) )
         fPInWin             	= pmsa( ipnt( 276) )
         fPInSum             	= pmsa( ipnt( 277) )
         fDiPIn              	= pmsa( ipnt( 278) )
         fGrenPIn            	= pmsa( ipnt( 279) )
         fBluEn              	= pmsa( ipnt( 280) )
         cNLoad              	= pmsa( ipnt( 281) )
         cNLoaSum            	= pmsa( ipnt( 282) )
         cNLoaWin            	= pmsa( ipnt( 283) )
         cNPoaeas            	= pmsa( ipnt( 284) )
         cNPPIn              	= pmsa( ipnt( 285) )
         cNPDeIn             	= pmsa( ipnt( 286) )
         fNHDisIn            	= pmsa( ipnt( 287) )
         cNDPIn              	= pmsa( ipnt( 288) )
         cNDDeIn             	= pmsa( ipnt( 289) )
         cDIMn               	= pmsa( ipnt( 290) )
         cO2In               	= pmsa( ipnt( 291) )
         cSiO2In             	= pmsa( ipnt( 292) )
         cSiDDeIn            	= pmsa( ipnt( 293) )
         cDZooIn             	= pmsa( ipnt( 294) )
         cDayApr1            	= pmsa( ipnt( 295) )
         cDayOct1            	= pmsa( ipnt( 296) )
         cLegCnge            	= pmsa( ipnt( 297) )
         cNLoadS             	= pmsa( ipnt( 298) )
         fNH4LadS            	= pmsa( ipnt( 299) )
         cDEroTot            	= pmsa( ipnt( 300) )
         fSeErsIM            	= pmsa( ipnt( 301) )
         fHErosIM            	= pmsa( ipnt( 302) )
         fDOrgoil            	= pmsa( ipnt( 303) )
         cPDSolOM            	= pmsa( ipnt( 304) )
         cNDSolOM            	= pmsa( ipnt( 305) )
         cPO4Gr              	= pmsa( ipnt( 306) )
         cNH4Gr              	= pmsa( ipnt( 307) )
         cNO3Gr              	= pmsa( ipnt( 308) )
         cDepthS             	= pmsa( ipnt( 309) )
         cCPerDW             	= pmsa( ipnt( 310) )
         cRhoIM              	= pmsa( ipnt( 311) )
         cRhoOM              	= pmsa( ipnt( 312) )
         cTmRef              	= pmsa( ipnt( 313) )
         cAerR               	= pmsa( ipnt( 314) )
         cAerLin             	= pmsa( ipnt( 315) )
         cAeSqare            	= pmsa( ipnt( 316) )
         cThetAer            	= pmsa( ipnt( 317) )
         kFloAer             	= pmsa( ipnt( 318) )
         cVSetIM             	= pmsa( ipnt( 319) )
         cVSetDe             	= pmsa( ipnt( 320) )
         cThetSet            	= pmsa( ipnt( 321) )
         cSuspMn             	= pmsa( ipnt( 322) )
         cSuspMx             	= pmsa( ipnt( 323) )
         cSupSope            	= pmsa( ipnt( 324) )
         hDethusp            	= pmsa( ipnt( 325) )
         cFetcRef            	= pmsa( ipnt( 326) )
         fLutuRef            	= pmsa( ipnt( 327) )
         cSuspRef            	= pmsa( ipnt( 328) )
         kVResus             	= pmsa( ipnt( 329) )
         kTurbish            	= pmsa( ipnt( 330) )
         kResuPMx            	= pmsa( ipnt( 331) )
         cReusExp            	= pmsa( ipnt( 332) )
         cThetMnW            	= pmsa( ipnt( 333) )
         kDMnDeW             	= pmsa( ipnt( 334) )
         hO2BOD              	= pmsa( ipnt( 335) )
         O2PerNO3            	= pmsa( ipnt( 336) )
         cThetMnS            	= pmsa( ipnt( 337) )
         kDMnDeS             	= pmsa( ipnt( 338) )
         fRefrDeS            	= pmsa( ipnt( 339) )
         hNO3Dnit            	= pmsa( ipnt( 340) )
         NO3PerC             	= pmsa( ipnt( 341) )
         kDMnHum             	= pmsa( ipnt( 342) )
         kNitrW              	= pmsa( ipnt( 343) )
         kNitrS              	= pmsa( ipnt( 344) )
         cThtaitr            	= pmsa( ipnt( 345) )
         O2PerNH4            	= pmsa( ipnt( 346) )
         hO2Nitr             	= pmsa( ipnt( 347) )
         kPDifPO4            	= pmsa( ipnt( 348) )
         kNDifNO3            	= pmsa( ipnt( 349) )
         kNDifNH4            	= pmsa( ipnt( 350) )
         kO2Dif              	= pmsa( ipnt( 351) )
         cThetDif            	= pmsa( ipnt( 352) )
         fDethifS            	= pmsa( ipnt( 353) )
         cTubDNut            	= pmsa( ipnt( 354) )
         cTubDfO2            	= pmsa( ipnt( 355) )
         kPSorp              	= pmsa( ipnt( 356) )
         cRelPdsD            	= pmsa( ipnt( 357) )
         cRePAsFe            	= pmsa( ipnt( 358) )
         cRePAsAl            	= pmsa( ipnt( 359) )
         cKPAdsOx            	= pmsa( ipnt( 360) )
         fRedMx              	= pmsa( ipnt( 361) )
         coPO4Mx             	= pmsa( ipnt( 362) )
         kPChePO4            	= pmsa( ipnt( 363) )
         cDayMnV1            	= pmsa( ipnt( 364) )
         cDayMnV2            	= pmsa( ipnt( 365) )
         fManV               	= pmsa( ipnt( 366) )
         cLengMan            	= pmsa( ipnt( 367) )
         cYetards            	= pmsa( ipnt( 368) )
         cDaarrds            	= pmsa( ipnt( 369) )
         cDandrds            	= pmsa( ipnt( 370) )
         cBidsrha            	= pmsa( ipnt( 371) )
         cDGzPird            	= pmsa( ipnt( 372) )
         hDVBird             	= pmsa( ipnt( 373) )
         fDAssird            	= pmsa( ipnt( 374) )
         fDiEgird            	= pmsa( ipnt( 375) )
         fDissMV             	= pmsa( ipnt( 376) )
         cLengllo            	= pmsa( ipnt( 377) )
         cLengM              	= pmsa( ipnt( 378) )
         UseEmpU             	= pmsa( ipnt( 379) )
         fSedUVMx            	= pmsa( ipnt( 380) )
         fSeUVoef            	= pmsa( ipnt( 381) )
         fSedUExp            	= pmsa( ipnt( 382) )
         fRVSum              	= pmsa( ipnt( 383) )
         fRVWin              	= pmsa( ipnt( 384) )
         fFloatV             	= pmsa( ipnt( 385) )
         fEmergV             	= pmsa( ipnt( 386) )
         cVHeight            	= pmsa( ipnt( 387) )
         cVHeW               	= pmsa( ipnt( 388) )
         cVMHe               	= pmsa( ipnt( 389) )
         cDStemV             	= pmsa( ipnt( 390) )
         cDensV              	= pmsa( ipnt( 391) )
         fDepth1V            	= pmsa( ipnt( 392) )
         fDepth2V            	= pmsa( ipnt( 393) )
         cDLayerV            	= pmsa( ipnt( 394) )
         cCovSpV             	= pmsa( ipnt( 395) )
         kMigrV              	= pmsa( ipnt( 396) )
         cDVIn               	= pmsa( ipnt( 397) )
         cTMInitV            	= pmsa( ipnt( 398) )
         cPhInitV            	= pmsa( ipnt( 399) )
         cDCarrV             	= pmsa( ipnt( 400) )
         cMuMxV              	= pmsa( ipnt( 401) )
         cQ10PodV            	= pmsa( ipnt( 402) )
         hLRefV              	= pmsa( ipnt( 403) )
         cExtSpV             	= pmsa( ipnt( 404) )
         kDRespV             	= pmsa( ipnt( 405) )
         cQ10RspV            	= pmsa( ipnt( 406) )
         kMVSum              	= pmsa( ipnt( 407) )
         fWinV               	= pmsa( ipnt( 408) )
         cDayWinV            	= pmsa( ipnt( 409) )
         fDeWMV              	= pmsa( ipnt( 410) )
         cPrfVird            	= pmsa( ipnt( 411) )
         cVPUMxV             	= pmsa( ipnt( 412) )
         cAfPUV              	= pmsa( ipnt( 413) )
         cPDVMn              	= pmsa( ipnt( 414) )
         cPDVMx              	= pmsa( ipnt( 415) )
         cVNUMxV             	= pmsa( ipnt( 416) )
         cAfNUV              	= pmsa( ipnt( 417) )
         cNDVMn              	= pmsa( ipnt( 418) )
         cNDVMx              	= pmsa( ipnt( 419) )
         cPACofMn            	= pmsa( ipnt( 420) )
         cPACofMx            	= pmsa( ipnt( 421) )
         hPACoef             	= pmsa( ipnt( 422) )
         cSechlus            	= pmsa( ipnt( 423) )
         cEuph               	= pmsa( ipnt( 424) )
         cCovSpP             	= pmsa( ipnt( 425) )
         cTmptoss            	= pmsa( ipnt( 426) )
         cSiTmoss            	= pmsa( ipnt( 427) )
         fDissMP             	= pmsa( ipnt( 428) )
         fDissoss            	= pmsa( ipnt( 429) )
         cMuMxDi             	= pmsa( ipnt( 430) )
         cTmOptDi            	= pmsa( ipnt( 431) )
         cSigTmDi            	= pmsa( ipnt( 432) )
         cExtSpDi            	= pmsa( ipnt( 433) )
         UseteeDi            	= pmsa( ipnt( 434) )
         cLOtRfDi            	= pmsa( ipnt( 435) )
         hLRefDi             	= pmsa( ipnt( 436) )
         cChDDiMn            	= pmsa( ipnt( 437) )
         cChDDiMx            	= pmsa( ipnt( 438) )
         kDRespDi            	= pmsa( ipnt( 439) )
         kLossDi             	= pmsa( ipnt( 440) )
         kMDiW               	= pmsa( ipnt( 441) )
         kMDiS               	= pmsa( ipnt( 442) )
         cVSetDi             	= pmsa( ipnt( 443) )
         cVPUMxDi            	= pmsa( ipnt( 444) )
         cAfPUDi             	= pmsa( ipnt( 445) )
         cPDDiMn             	= pmsa( ipnt( 446) )
         cPDDiMx             	= pmsa( ipnt( 447) )
         cVNUMxDi            	= pmsa( ipnt( 448) )
         cAfNUDi             	= pmsa( ipnt( 449) )
         cNDDiMn             	= pmsa( ipnt( 450) )
         cNDDiMx             	= pmsa( ipnt( 451) )
         hSiAssDi            	= pmsa( ipnt( 452) )
         cMuMxren            	= pmsa( ipnt( 453) )
         cTmptren            	= pmsa( ipnt( 454) )
         cSiTmren            	= pmsa( ipnt( 455) )
         cExSpren            	= pmsa( ipnt( 456) )
         Useeeren            	= pmsa( ipnt( 457) )
         hLRefren            	= pmsa( ipnt( 458) )
         cLOReren            	= pmsa( ipnt( 459) )
         cChGrnMn            	= pmsa( ipnt( 460) )
         cChGrnMx            	= pmsa( ipnt( 461) )
         kDRspren            	= pmsa( ipnt( 462) )
         kLossren            	= pmsa( ipnt( 463) )
         kMGrenW             	= pmsa( ipnt( 464) )
         kMGrenS             	= pmsa( ipnt( 465) )
         cVSetren            	= pmsa( ipnt( 466) )
         cVPMxren            	= pmsa( ipnt( 467) )
         cAfPUren            	= pmsa( ipnt( 468) )
         cPDGrnMn            	= pmsa( ipnt( 469) )
         cPDGrnMx            	= pmsa( ipnt( 470) )
         cVNMxren            	= pmsa( ipnt( 471) )
         cAfNUren            	= pmsa( ipnt( 472) )
         cNDGrnMn            	= pmsa( ipnt( 473) )
         cNDGrnMx            	= pmsa( ipnt( 474) )
         hSissren            	= pmsa( ipnt( 475) )
         cMuMxlue            	= pmsa( ipnt( 476) )
         cTmptlue            	= pmsa( ipnt( 477) )
         cSiTmlue            	= pmsa( ipnt( 478) )
         cExSplue            	= pmsa( ipnt( 479) )
         Useeelue            	= pmsa( ipnt( 480) )
         cLORelue            	= pmsa( ipnt( 481) )
         hLReflue            	= pmsa( ipnt( 482) )
         cChBleMn            	= pmsa( ipnt( 483) )
         cChBleMx            	= pmsa( ipnt( 484) )
         cCyBleMn            	= pmsa( ipnt( 485) )
         cCyBleMx            	= pmsa( ipnt( 486) )
         kDRsplue            	= pmsa( ipnt( 487) )
         kLosslue            	= pmsa( ipnt( 488) )
         kMBlueW             	= pmsa( ipnt( 489) )
         kMBlueS             	= pmsa( ipnt( 490) )
         cVSetlue            	= pmsa( ipnt( 491) )
         cVPMxlue            	= pmsa( ipnt( 492) )
         cAfPUlue            	= pmsa( ipnt( 493) )
         cPDBleMn            	= pmsa( ipnt( 494) )
         cPDBleMx            	= pmsa( ipnt( 495) )
         cVNMxlue            	= pmsa( ipnt( 496) )
         cAfNUlue            	= pmsa( ipnt( 497) )
         cNDBleMn            	= pmsa( ipnt( 498) )
         cNDBleMx            	= pmsa( ipnt( 499) )
         fFiJvMH             	= pmsa( ipnt( 500) )
         fFiJVesH            	= pmsa( ipnt( 501) )
         fFivEcrH            	= pmsa( ipnt( 502) )
         fFiAdMH             	= pmsa( ipnt( 503) )
         fFidEesH            	= pmsa( ipnt( 504) )
         fFidEcrH            	= pmsa( ipnt( 505) )
         fPiscMH             	= pmsa( ipnt( 506) )
         fPicEesH            	= pmsa( ipnt( 507) )
         fPicEcrH            	= pmsa( ipnt( 508) )
         hSisslue            	= pmsa( ipnt( 509) )
         cDBentIn            	= pmsa( ipnt( 510) )
         kMigrent            	= pmsa( ipnt( 511) )
         kMigrish            	= pmsa( ipnt( 512) )
         cDFiJvIn            	= pmsa( ipnt( 513) )
         cDFiAdIn            	= pmsa( ipnt( 514) )
         kHaFiWin            	= pmsa( ipnt( 515) )
         kHaFiSum            	= pmsa( ipnt( 516) )
         cDPiscIn            	= pmsa( ipnt( 517) )
         kMigrisc            	= pmsa( ipnt( 518) )
         kHaPiWin            	= pmsa( ipnt( 519) )
         kHaPiSum            	= pmsa( ipnt( 520) )
         cFiltMx             	= pmsa( ipnt( 521) )
         hFilt               	= pmsa( ipnt( 522) )
         cDCarZoo            	= pmsa( ipnt( 523) )
         cPrefDi             	= pmsa( ipnt( 524) )
         cPrefren            	= pmsa( ipnt( 525) )
         cPreflue            	= pmsa( ipnt( 526) )
         cPrefDe             	= pmsa( ipnt( 527) )
         fDAssZoo            	= pmsa( ipnt( 528) )
         fDiEgZoo            	= pmsa( ipnt( 529) )
         kDResZoo            	= pmsa( ipnt( 530) )
         kMZoo               	= pmsa( ipnt( 531) )
         fDissZoo            	= pmsa( ipnt( 532) )
         cTmOpZoo            	= pmsa( ipnt( 533) )
         cSigTZoo            	= pmsa( ipnt( 534) )
         cDCrrent            	= pmsa( ipnt( 535) )
         kDAssent            	= pmsa( ipnt( 536) )
         hDFodent            	= pmsa( ipnt( 537) )
         fDAssent            	= pmsa( ipnt( 538) )
         fDiEgent            	= pmsa( ipnt( 539) )
         kDRspent            	= pmsa( ipnt( 540) )
         kMBent              	= pmsa( ipnt( 541) )
         fDisMent            	= pmsa( ipnt( 542) )
         cTmptent            	= pmsa( ipnt( 543) )
         cSiTment            	= pmsa( ipnt( 544) )
         fDBone              	= pmsa( ipnt( 545) )
         fPBone              	= pmsa( ipnt( 546) )
         cDCrrish            	= pmsa( ipnt( 547) )
         fDiEgish            	= pmsa( ipnt( 548) )
         fDisMish            	= pmsa( ipnt( 549) )
         cTmptish            	= pmsa( ipnt( 550) )
         cSiTmish            	= pmsa( ipnt( 551) )
         cDaepish            	= pmsa( ipnt( 552) )
         fReprish            	= pmsa( ipnt( 553) )
         fAgeFish            	= pmsa( ipnt( 554) )
         cRelVish            	= pmsa( ipnt( 555) )
         kDAssiJv            	= pmsa( ipnt( 556) )
         hDZooiJv            	= pmsa( ipnt( 557) )
         fDAssiJv            	= pmsa( ipnt( 558) )
         kDRspiJv            	= pmsa( ipnt( 559) )
         kMFiJv              	= pmsa( ipnt( 560) )
         kDAssiAd            	= pmsa( ipnt( 561) )
         hDBntiAd            	= pmsa( ipnt( 562) )
         fDAssiAd            	= pmsa( ipnt( 563) )
         kDRspiAd            	= pmsa( ipnt( 564) )
         kMFiAd              	= pmsa( ipnt( 565) )
         cDCrPcMx            	= pmsa( ipnt( 566) )
         cDCrPcMn            	= pmsa( ipnt( 567) )
         cDCPiare            	= pmsa( ipnt( 568) )
         cDPaMisc            	= pmsa( ipnt( 569) )
         cCovVMn             	= pmsa( ipnt( 570) )
         cTmOptV             	= pmsa( ipnt( 571) )
         cSigTmV             	= pmsa( ipnt( 572) )
         cRehrisc            	= pmsa( ipnt( 573) )
         cRelVisc            	= pmsa( ipnt( 574) )
         kDAssisc            	= pmsa( ipnt( 575) )
         hDVPisc             	= pmsa( ipnt( 576) )
         hDFshisc            	= pmsa( ipnt( 577) )
         fDAssisc            	= pmsa( ipnt( 578) )
         fDiEgisc            	= pmsa( ipnt( 579) )
         kDRspisc            	= pmsa( ipnt( 580) )
         kMPisc              	= pmsa( ipnt( 581) )
         fDisMisc            	= pmsa( ipnt( 582) )
         cTmptisc            	= pmsa( ipnt( 583) )
         cSiTmisc            	= pmsa( ipnt( 584) )
         cDepthSM            	= pmsa( ipnt( 585) )
         kExchMxM            	= pmsa( ipnt( 586) )
         hfMarsh             	= pmsa( ipnt( 587) )
         fDTotSM0            	= pmsa( ipnt( 588) )
         fDOrgSM0            	= pmsa( ipnt( 589) )
         fDDeSM0             	= pmsa( ipnt( 590) )
         fPIorSM0            	= pmsa( ipnt( 591) )
         cPDPhra0            	= pmsa( ipnt( 592) )
         cNDPhra0            	= pmsa( ipnt( 593) )
         cDeSthra            	= pmsa( ipnt( 594) )
         cTMithra            	= pmsa( ipnt( 595) )
         fDAllhra            	= pmsa( ipnt( 596) )
         kDAllhra            	= pmsa( ipnt( 597) )
         cDSemhra            	= pmsa( ipnt( 598) )
         cQ1rohra            	= pmsa( ipnt( 599) )
         cMuPhaMx            	= pmsa( ipnt( 600) )
         cDSPhaMx            	= pmsa( ipnt( 601) )
         cCoSphra            	= pmsa( ipnt( 602) )
         cPDPhaMn            	= pmsa( ipnt( 603) )
         cPDPhaMx            	= pmsa( ipnt( 604) )
         cNDPhaMn            	= pmsa( ipnt( 605) )
         cNDPhaMx            	= pmsa( ipnt( 606) )
         cAfNUhra            	= pmsa( ipnt( 607) )
         cAfPUhra            	= pmsa( ipnt( 608) )
         cVNPhaMx            	= pmsa( ipnt( 609) )
         cVPPhaMx            	= pmsa( ipnt( 610) )
         kDRsphra            	= pmsa( ipnt( 611) )
         cQ1eshra            	= pmsa( ipnt( 612) )
         fDayWin             	= pmsa( ipnt( 613) )
         fDRalhra            	= pmsa( ipnt( 614) )
         kDRalhra            	= pmsa( ipnt( 615) )
         kDMShhra            	= pmsa( ipnt( 616) )
         kDMRPhra            	= pmsa( ipnt( 617) )
         cDaWihra            	= pmsa( ipnt( 618) )
         cDaMahra            	= pmsa( ipnt( 619) )
         fManPhra            	= pmsa( ipnt( 620) )
         kDMnShra            	= pmsa( ipnt( 621) )
         DayPeear            	= pmsa( ipnt( 622) )
         TenDays             	= pmsa( ipnt( 623) )
         HousIDay            	= pmsa( ipnt( 624) )
         HousPDay            	= pmsa( ipnt( 625) )
         SecPeDay            	= pmsa( ipnt( 626) )
         mmPerm              	= pmsa( ipnt( 627) )
         m2Perha             	= pmsa( ipnt( 628) )
         mgPerg              	= pmsa( ipnt( 629) )
         gPerkg              	= pmsa( ipnt( 630) )
         gPerton             	= pmsa( ipnt( 631) )
         PerDay              	= pmsa( ipnt( 632) )
         PerCent             	= pmsa( ipnt( 633) )
         NearZero            	= pmsa( ipnt( 634) )
         molO2olC            	= pmsa( ipnt( 635) )
         molO2olN            	= pmsa( ipnt( 636) )
         molNmolC            	= pmsa( ipnt( 637) )
         cRhoWat             	= pmsa( ipnt( 638) )
         cSolar              	= pmsa( ipnt( 639) )
         cLAT                	= pmsa( ipnt( 640) )
         cPhiR               	= pmsa( ipnt( 641) )
         cDayR               	= pmsa( ipnt( 642) )
         fCloudH             	= pmsa( ipnt( 643) )
         fCloudM             	= pmsa( ipnt( 644) )
         fCloudL             	= pmsa( ipnt( 645) )
         cTrans              	= pmsa( ipnt( 646) )
         Pi                  	= pmsa( ipnt( 647) )
         cHeath              	= pmsa( ipnt( 648) )
         cAlpha              	= pmsa( ipnt( 649) )
         cDeMeChl            	= pmsa( ipnt( 650) )
         cPMx                	= pmsa( ipnt( 651) )
         cDehMaMn            	= pmsa( ipnt( 652) )
         fMeta               	= pmsa( ipnt( 653) )
         kBub                	= pmsa( ipnt( 654) )
         cVegHP1             	= pmsa( ipnt( 655) )
         cVegHP2             	= pmsa( ipnt( 656) )
         cVegL               	= pmsa( ipnt( 657) )

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
! 	Days_In_A_Year
		DaysInY = DayPeear*1
! 	If_T_Include_stratification
		if ( ( InclSrat == 0 ) ) then
		aInlSrat = 0.0 		else		aInlSrat = 1.0 		endif
! 	Mixing_depth
		if ( ( calixpth == 1 ) ) then
		uDptMix = cAMix * ( ( cFetch )** (cBMix) ) 		else		uDptMix = cDeptMix 		endif
! 	Total_Depth
		if ( ( aInlSrat == 1 ) ) then
		uDptWE = uDptMix 		else		uDptWE = sDepthW 		endif
! 	Total_Depth
		if ( ( aInlSrat == 1 ) ) then
		uDptWH = sDepthW - uDptMix 		else		uDptWH = cMnephHE 		endif
! 	Conversion_factor_from_concentration_to_mass
		if ( CalcMass == 1 ) then
		MassH = uDptWH 		else		MassH = 1.0 		endif
! 	Conversion_factor_from_concentration_to_mass
		if ( CalcMass == 1 ) then
		MassE = uDptWE 		else		MassE = 1.0 		endif
! 	Conversion_factor_from_concentration_to_mass
		if ( CalcMass == 1 ) then
		MassWM = sDepthWM 		else		MassWM = 1.0 		endif
! 	Mass_to_concentration
		oNH4WH = sNH4WH / MassH
! 	Mass_to_concentration
		oNO3WH = sNO3WH / MassH
! 	Mass_to_concentration
		oPO4WH = sPO4WH / MassH
! 	Mass_to_concentration
		oPAIMWH = sPAIMWH / MassH
! 	Mass_to_concentration
		oSiO2WH = sSiO2WH / MassH
! 	Mass_to_concentration
		oO2WH = sO2WH / MassH
! 	Mass_to_concentration
		oDDtWH = sDDetWH / MassH
! 	Mass_to_concentration
		oNDtWH = sNDetWH / MassH
! 	Mass_to_concentration
		oPDtWH = sPDetWH / MassH
! 	Mass_to_concentration
		oSiDtWH = sSiDetWH / MassH
! 	Mass_to_concentration
		oDIMWH = sDIMWH / MassH
! 	Mass_to_concentration
		oDDiWH = sDDiatWH / MassH
! 	Mass_to_concentration
		oNDiWH = sNDiatWH / MassH
! 	Mass_to_concentration
		oPDiWH = sPDiatWH / MassH
! 	Mass_to_concentration
		oDGrenWH = sDGrenWH / MassH
! 	Mass_to_concentration
		oNGrenWH = sNGrenWH / MassH
! 	Mass_to_concentration
		oPGrenWH = sPGrenWH / MassH
! 	Mass_to_concentration
		oDBlueWH = sDBlueWH / MassH
! 	Mass_to_concentration
		oNBlueWH = sNBlueWH / MassH
! 	Mass_to_concentration
		oPBlueWH = sPBlueWH / MassH
! 	Mass_to_concentration
		oDZooH = sDZooH / MassH
! 	Mass_to_concentration
		oNZooH = sNZooH / MassH
! 	Mass_to_concentration
		oPZooH = sPZooH / MassH
! 	Mass_to_concentration
		oNH4WM = sNH4WM / MassWM
! 	Mass_to_concentration
		oNO3WM = sNO3WM / MassWM
! 	Mass_to_concentration
		oPO4WM = sPO4WM / MassWM
! 	Mass_to_concentration
		oPAIMWM = sPAIMWM / MassWM
! 	Mass_to_concentration
		oSiO2WM = sSiO2WM / MassWM
! 	Mass_to_concentration
		oO2WM = sO2WM / MassWM
! 	Mass_to_concentration
		oDDtWM = sDDetWM / MassWM
! 	Mass_to_concentration
		oNDtWM = sNDetWM / MassWM
! 	Mass_to_concentration
		oPDtWM = sPDetWM / MassWM
! 	Mass_to_concentration
		oSiDtWM = sSiDetWM / MassWM
! 	Mass_to_concentration
		oDIMWM = sDIMWM / MassWM
! 	Mass_to_concentration
		oDDiWM = sDDiatWM / MassWM
! 	Mass_to_concentration
		oNDiWM = sNDiatWM / MassWM
! 	Mass_to_concentration
		oPDiWM = sPDiatWM / MassWM
! 	Mass_to_concentration
		oDGrenWM = sDGrenWM / MassWM
! 	Mass_to_concentration
		oNGrenWM = sNGrenWM / MassWM
! 	Mass_to_concentration
		oPGrenWM = sPGrenWM / MassWM
! 	Mass_to_concentration
		oDBlueWM = sDBlueWM / MassWM
! 	Mass_to_concentration
		oNBlueWM = sNBlueWM / MassWM
! 	Mass_to_concentration
		oPBlueWM = sPBlueWM / MassWM
! 	Mass_to_concentration
		oDZooM = sDZooM / MassWM
! 	Mass_to_concentration
		oNZooM = sNZooM / MassWM
! 	Mass_to_concentration
		oPZooM = sPZooM / MassWM
! 	Mass_to_concentration
		oNH4WE = sNH4WE / MassE
! 	Mass_to_concentration
		oNO3WE = sNO3WE / MassE
! 	Mass_to_concentration
		oPO4WE = sPO4WE / MassE
! 	Mass_to_concentration
		oPAIMWE = sPAIMWE / MassE
! 	Mass_to_concentration
		oSiO2WE = sSiO2WE / MassE
! 	Mass_to_concentration
		oO2WE = sO2WE / MassE
! 	Mass_to_concentration
		oDDtWE = sDDetWE / MassE
! 	Mass_to_concentration
		oNDtWE = sNDetWE / MassE
! 	Mass_to_concentration
		oPDtWE = sPDetWE / MassE
! 	Mass_to_concentration
		oSiDtWE = sSiDetWE / MassE
! 	Mass_to_concentration
		oDIMWE = sDIMWE / MassE
! 	Mass_to_concentration
		oDDiWE = sDDiatWE / MassE
! 	Mass_to_concentration
		oNDiWE = sNDiatWE / MassE
! 	Mass_to_concentration
		oPDiWE = sPDiatWE / MassE
! 	Mass_to_concentration
		oDGrenWE = sDGrenWE / MassE
! 	Mass_to_concentration
		oNGrenWE = sNGrenWE / MassE
! 	Mass_to_concentration
		oPGrenWE = sPGrenWE / MassE
! 	Mass_to_concentration
		oDBlueWE = sDBlueWE / MassE
! 	Mass_to_concentration
		oNBlueWE = sNBlueWE / MassE
! 	Mass_to_concentration
		oPBlueWE = sPBlueWE / MassE
! 	Mass_to_concentration
		oDZooE = sDZooE / MassE
! 	Mass_to_concentration
		oNZooE = sNZooE / MassE
! 	Mass_to_concentration
		oPZooE = sPZooE / MassE
! 	Time_in_years
		TimeYars = sTime / DayPeear
! 	Time_(day_number)_within_the_year_(0-365)
		Day = sTime - floor(TimeYars) * DayPeear
! 	Time_in_calendar_years
		Years = YearZero + TimeYars
! 	Vegetation_height
		if ( ( UseVHe == 0 .and.  sVegHe < sDepthW ) ) then
		uVeHeght = sVegHe 		else if ( ( UseVHe == 0 ) ) then		uVeHeght = ( sDepthW ) 		else		uVeHeght = min ( sVegHe , sDepthW , ( ( cVegHP1*sDVeg )** cVegHP2 ) ) 		endif
! 	Vegetation_height_based_on_empirical_biomass_length_relationship_of_Haga_2007
		aVeHeght =   ( ( cVegHP1*sDVeg )** cVegHP2 ) 
! 	Vegetation_height_hyp
		if ( ( aInlSrat == 0 ) ) then
		uVeHehtH = 0.0 		else if ( (uVeHeght > uDptWH ) ) then		uVeHehtH = uDptWH 		else		uVeHehtH = uVeHeght 		endif
! 	Vegetation_height_epi
		if ( ( aInlSrat == 0 .and. uVeHeght < uDptWE ) ) then
		uVeHehtE = uVeHeght 		else if ( ( aInlSrat == 0 .and. uVeHeght >= uDptWE ) ) then		uVeHehtE = uDptWE 		else if ( (uVeHeght > uDptWH ) ) then		uVeHehtE = uVeHeght -uDptWH 		else		uVeHehtE = 0.0 		endif
! 	Lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		if ( ( aInlSrat == 1 .and. fDepth2V >= 1 ) ) then
		aDpt2egH = uDptWH  		else if ( ( aInlSrat == 1 .and. fDepth2V < 1 ) ) then		aDpt2egH = max (0.0, uVeHeght - uDptWE ) 		else		aDpt2egH = 0.0 		endif
! 	Upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		if ( ( aInlSrat == 1 .and. fDepth2V >= 1 ) ) then
		aDpt1egH = uDptWH - uVeHehtH  		else		aDpt1egH = 0.0 		endif
! 	Lower_depth_of_vegetation_layer_(maximum_=_water_depth)
		if ( (fDepth2V >= 1 ) ) then
		aDpt2egE = uDptWE 		else		aDpt2egE = uDptWE - uVeHehtE 		endif
! 	Upper_depth_of_vegetation_layer_(minimum_=_0_m_=_surface)
		if ( (fDepth2V >= 1 ) ) then
		aDpt1egE = uDptWE - uVeHehtE  		else		aDpt1egE = 0.0 		endif
! 	Vegetation_height_epi
		uVegHL = cVegL *log( 1.0 + uVeHeght )
! 	Vegetation_height_epi
		uVegHLS = cVegL *log( 1.0 + max (uVeHeght, min (sDepthW, cVHeight) ) )
! 	Vegetation_Light
		if ( (  uVegHL > uVeHehtE ) ) then
		uVeghhtE = uVeHehtE 		else		uVeghhtE = uVegHL 		endif
! 	Vegetation_Light
		uVeghhtH =  uVegHL - uVeghhtE  
! 	Vegetation_biomass_hyp
		uDVegH = uVeHehtH / uVeHeght * sDVeg
! 	Vegetation_biomass_epi
		uDVegE = uVeHehtE / uVeHeght * sDVeg
! 	Vegetation_shading_factor
		if ( ( ReaVSade  ==  1) ) then
		uVegSade = mVShade 		else		uVegSade = fVShade 		endif
! 	Contribution_of_algae_to_extinction
		aExtPytH = cExtSpDi * oDDiWH + cExSpren * oDGrenWH + cExSplue * oDBlueWH
! 	Detrital_contribution_to_extinction
		aExtDtH = cExtSpDe * oDDtWH
! 	Contribution_of_inert_matter_to_extinction
		aExtIMH = cExtSpIM * oDIMWH
! 	Extinction_coefficient_without_vegetation
		aExCofOH = cExtWat + aExtIMH + aExtDtH + aExtPytH
! 	Contribution_of_algae_to_extinction
		aExtPytE = cExtSpDi * oDDiWE + cExSpren * oDGrenWE + cExSplue * oDBlueWE
! 	Detrital_contribution_to_extinction
		aExtDtE = cExtSpDe * oDDtWE
! 	Contribution_of_inert_matter_to_extinction
		aExtIME = cExtSpIM * oDIMWE
! 	Extinction_coefficient_without_vegetation
		aExCofOE = cExtWat + aExtIME + aExtDtE + aExtPytE
! 	Forcing_function_temperature
		aTmE =  cTmAveE - cTmVarE * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysInY) 
! 	Forcing_function_temperature
		aTmH =  cTmAveH - cTmVarH * cos(2.0*Pi*(sTime + TenDays - cTimeLag) / DaysInY) 
! 	Forcing_function_temperature
		if ( (0 == IncSeson) ) then
		uTmE = cTmAveE 		else if ( (ReadTemp == 1) ) then		uTmE = mTempE 		else if ( ( aTmE > 0.0  ) ) then		uTmE = aTmE 		else		uTmE =  0.0 		endif
! 	Forcing_function_temperature
		if ( (0 == IncSeson) ) then
		uTmH = cTmAveH 		else if ( (ReadTemp == 1) ) then		uTmH = mTempH 		else if ( ( aTmH > 0.0  ) ) then		uTmH = aTmH 		else		uTmH =  0.0 		endif
! 	Forcing_function_temperature
		if ( (aInlSrat == 1 ) ) then
		uTmBot = uTmH 		else		uTmBot = uTmE 		endif
! 	Latitude_conversion_to_radians
		uLAT= cLAT*2*Pi/360
! 	Solar_Declination_angle
		if ((InclLat == 1) ) then
		uSoDeAng = cPhiR*cos(2*Pi*(Day-cDayR)/DaysInY) 		else		uSoDeAng = 0.0 		endif
! 	Time_of_sun_rise
		uSunRise = HousIDay/(2*Pi)*acos((sin(uLAT)* sin(uSoDeAng))/(cos(uLAT)*cos(uSoDeA&
		&ng))) 
! 	Time_of_sun_set
		uSunSet=HousIDay-uSunRise
! 	Hours_of_sun_per_day
		uSunHour=uSunSet-uSunRise
! 	Integration_of_the_sunpath_over_the_local_rise_and_falls
		if ((InclLat == 1) ) then
		uSunPath = sin(uLAT)*sin( uSoDeAng )*( uSunSet-uSunRise)- cos(uLAT) * cos (uSoDe&
		&Ang) * HousIDay / (2*Pi) * ( sin (2*Pi*uSunSet/HousIDay ) -sin(2*Pi*uSunRise /Ho&
		&usIDay )) 		else		uSunPath = 0.0 		endif
! 	Transmissivity_atmosphere
		if (((InclLat == 1) .and. (ContTans == 0)) ) then
		uTrans = (0.6-0.2*(uSunPath/(uSunSet - uSunRise)))*(1-0.4*fCloudH) *(1-0.7* fClo&
		&udM) *(1-0.4*fCloudL)		else if ( ((InclLat == 1) .and. (ContTans == 1)) ) then		uTrans = cTrans 		else		uTrans = 0.0 		endif
! 	Day_length
		if ( (0 == IncSeson) ) then
		ufDay = cfDayAve 		else if ( (InclLat == 1) ) then		ufDay = (uSunSet -uSunRise )/ HousIDay 		else		ufDay = cfDayAve - cfDayVar * cos( 2.0 *Pi*(sTime+TenDays) / DaysInY) 		endif
! 	Total_daily_radiation
		if ( (0 == IncSeson) ) then
		uLDay = cLDayAve 		else if ( (ReadLOut == 1) ) then		uLDay = 0.0 		else if ( (InclLat == 1) ) then		uLDay = cSolar*uTrans*uSunPath * SecPeDay /HousIDay 		else		uLDay = cLDayAve - cLDayVar * cos(2.0*Pi*(sTime+TenDays) / DaysInY) 		endif
! 	Average_light_intensity_during_daytime
		if ( (0 == IncSeson) ) then
		uLOut = uLDay / SecPeDay / ufDay 		else if ( (ReadLOut == 1) ) then		uLOut = mLOut / ufDay 		else		uLOut = uLDay / SecPeDay / ufDay 		endif
! 	Average_PAR_at_zero_depth
		uLPARurf = fPAR * (1.0 - fRefl) * uLOut
! 	Light_at_the_bottom_of_Epi
		aLPRBtEO = uLPARurf * exp(- aExCofOE * uDptWE)
! 	Light_at_the_bottom_of_Epi
		aLPRBtHO = aLPRBtEO * exp(- aExCofOH * uDptWE)
! 	Light_at_top_of_vegetation_layer
		aLPR1egE = uLPARurf * exp(- aExCofOE * aDpt1egE) * ( 1 - uVegSade )
! 	Light_at_top_of_vegetation_layer
		aLPR1egH = aLPR1egE * exp(- aExCofOH * aDpt1egH)
! 	Light_at_the_bottom_of_Epi
		if (  ( aInlSrat == 1 .and.  uVeHeght <= uDptWH ) ) then
		aLPVeght = aLPR1egH *   exp(- aExCofOH * uVeghhtE) 		else		aLPVeght = aLPR1egE *  exp(- aExCofOE * uVeghhtE)  *  exp(- aExCofOH * uVeghhtE)&
		& 		endif
! 	Average_temperature_for_vegetation
		uTmVeAve = ( uVeHehtH * uTmH + uVeHehtE * uTmE ) / uVeHeght
! 	temperature_function_of_growth_vegetation
		uFunTVeg = exp(-0.5/(cSigTmV*cSigTmV) *((uTmVeAve-cTmOptV)*(uTmVeAve-cTmOptV) - &
		&(cTmRef-cTmOptV)*(cTmRef-cTmOptV)))
! 	Temperature_function_of_vegetation_production
		if ( ( UsePoerV < NearZero ) ) then
		uFumPVeg = uFunTVeg 		else		uFumPVeg = ( ( cQ10PodV )** (0.1 * (uTmVeAve - cTmRef))) 		endif
! 	Half-sat_light_for_vegetation_production_at_current_temp
		uhLVeg = hLRefV * uFumPVeg
! 	integrated_PI_curve_over_depth_for_vegetation
		if ( ( aInlSrat == 1 .and.  uVeHeght > uDptWH ) ) then
		aLPIVeg = cPMx*(aLPR1egE- aLPVeght + uhLVeg*log((1.0 +aLPVeght/ uhLVeg)  /(1.0 +&
		&aLPR1egE  / uhLVeg) + NearZero  ) )		else if (  ( aInlSrat == 1 .and.  uVeHeght <= uDptWH ) ) then		aLPIVeg = cPMx*( aLPR1egH- aLPVeght + uhLVeg* log((1.0 +aLPVeght/ uhLVeg)  / (1.&
		&0 +aLPR1egH  / uhLVeg) ) ) 		else		aLPIVeg = cPMx * (aLPR1egE- aLPVeght + uhLVeg*log((1.0 +aLPVeght/ uhLVeg)  /(1.0&
		& +aLPR1egE  / uhLVeg) + NearZero  ) )		endif
! 	day_at_which_plants_change_strategy_towards_overwintering
		if ( ( cLAT < 0 .and. Day >= 265 .and. Day <= 356 ) ) then
		aSpring = 1 		else if ( ( cLAT > 0 .and. Day >= 80 .and. Day <= 173) ) then		aSpring = 1 		else if ( ( cLAT == 0) ) then		aSpring = 1 		else		aSpring = 0 		endif
! 	day_at_which_plants_change_strategy_towards_overwintering
		if ( ( sTime < StTime + 1.0 ) ) then
		aTiWiVeg = -999 		else if ( ( UsePhotV == 0  .and.  ( uTmBot >= cTMInitV )  .and. aSpring == 1 ) )&
		& then		aTiWiVeg = sTime  		else if ( (UsePhotV == 1  .and.  uSunHour >= cPhInitV   .and. aSpring == 1 ) ) t&
		&hen		aTiWiVeg = sTime 		else if ( (( aLPIVeg / uVegHL )<= cLAlloV ) ) then		aTiWiVeg = aTiWiVeg 		else if ( (floor ( sTime+ 0.3 ) - floor (aTiWiVeg + 0.3) > 1 .and. floor ( sTime&
		&+ 0.3 ) - floor (aTiWiVeg + 0.3) < cVWinLen ) ) then		aTiWiVeg = aTiWiVeg 		else		aTiWiVeg = sTime 		endif
! 	Initial_growth_only_once_a_year
		if ( ( sTime < StTime + 1.0 ) ) then
		aTiInVeg = -999 		else if ( ( floor ( sTime+ 0.3 ) - floor (aTiWiVeg + 0.3) > 1 ) ) then		aTiInVeg = sTime 		else if ( ( UsePhotV == 0  .and.  uTmBot >= cTMInitV ) ) then		aTiInVeg = aTiInVeg 		else if ( ( UsePhotV == 1  .and.    uSunHour >= cPhInitV  ) ) then		aTiInVeg = aTiInVeg 		else if ( (UsePhotV == 0  .and.  uTmBot < cTMInitV .and. ( ( aLPIVeg / uVegHL ) &
		&>= cLAlloV ) .and. floor ( sTime+ 0.3 ) - floor (aTiInVeg + 0.3) > 1) ) then		aTiInVeg = aTiInVeg 		else if ( (UsePhotV == 1  .and.   uSunHour < cPhInitV .and. ( ( aLPIVeg / uVegHL&
		& ) >= cLAlloV ) .and. floor ( sTime+ 0.3 ) - floor (aTiInVeg + 0.3) > 1) ) then		aTiInVeg = aTiInVeg 		else if ( (floor ( sTime+ 0.3 ) - floor (aTiInVeg + 0.3) > 1 .and. floor ( sTime&
		&+ 0.3 ) - floor (aTiInVeg + 0.3) < cVIniLen ) ) then		aTiInVeg = aTiInVeg 		else		aTiInVeg = sTime 		endif
! 	day_at_which_plants_change_strategy_towards_overwintering
		if ( ( floor ( aTiInVeg + 0.3) < floor (  sTime + 0.3 ) .and. floor ( aTiWiVeg +&
		& 0.3 ) == floor (  sTime  + 0.3 ) ) ) then
		aVSum = 1 		else		aVSum = 0 		endif
! 	day_at_which_plants_change_strategy_towards_overwintering
		aVWin =  1 - aVSum
! 	day_at_which_plants_change_strategy_towards_overwintering
		aDaysSuV = ( floor ( sTime+ 0.3) - floor (aTiInVeg + 0.3) ) * aVSum
! 	day_at_which_plants_change_strategy_towards_overwintering
		aDaysWiV = ( floor ( sTime + 0.3) - floor ( aTiWiVeg  + 0.3 )  ) * aVWin
! 	day_at_which_plants_change_strategy_towards_overwintering
		if ( UseLWinV <= NearZero ) then
		uDaWiVeg = cDayWinV 		else if ( (Day < 1.0 .or. sTime < StTime + 1.0) ) then		uDaWiVeg = 367 		else if ( (( aLPIVeg / uVegHL )<= cLAlloV .and. uDaWiVeg > 366 .and. Day > 1 ) )&
		& then		uDaWiVeg = Day 		else		uDaWiVeg = uDaWiVeg 		endif
! 	Initial_growth_only_once_a_year
		if ( (Day < 1.0 .or. sTime < StTime + 1.0) ) then
		aDaInVeg = 367 		else if ( (uTmBot >= cTMInitV .and. aDaInVeg > 366) ) then		aDaInVeg = Day 		else		aDaInVeg = aDaInVeg 		endif
! 	Setting_root_fraction
		if ( (0 == IncSeson) ) then
		bfRtVeg = fRVSum 		else if ( (aVWin == 1 .and. aDaysWiV > cLengllo) ) then		bfRtVeg = fRVWin 		else if ( (aVSum == 1 .and. aDaysSuV <= cLengllo) ) then		bfRtVeg = 0.5*(fRVWin + fRVSum) + 0.5*(fRVWin - fRVSum) * cos ( Pi /cLengllo * (&
		& sTime - aTiInVeg )) 		else if ( ( aVWin == 1 .and. aDaysWiV == 0) ) then		bfRtVeg = fRVWin 		else if ( ( aVWin == 1) ) then		bfRtVeg = 0.5*( fRVWin + fRVSum) - 0.5*( fRVWin - fRVSum) * cos ( Pi / cLengllo &
		&* ( sTime - aTiWiVeg )) 		else		bfRtVeg = fRVSum 		endif
! 	Shoot_fraction
		bfShVeg = 1.0 - bfRtVeg
! 	Shoot_biomass
		aDShVeg = bfShVeg * sDVeg
! 	Emergent_biomass
		aDEerVeg = fEmergV * aDShVeg
! 	Floating_biomass
		if (  ( aDpt1egE < 0.005 ) ) then
		aDFoaVeg = fFloatV * aDShVeg 		else		aDFoaVeg = 0 		endif
! 	Submerged_fraction_of_shoot
		bfSubVeg = 1.0 - fFloatV - fEmergV
! 	Submerged_biomass
		aDSubVeg = bfSubVeg * aDShVeg
! 	Contribution_of_plant_species_to_extinction_(submerged)
		aExtVegH = cExtSpV * aDSubVeg / uDptWH * (uVeHehtH /sDepthW )
! 	Extinction_coefficient_incl_vegetation
		aExtCefH = aExCofOH + aExtVegH
! 	Contribution_of_plant_species_to_extinction_(submerged)
		aExtVegE = cExtSpV * aDSubVeg / uDptWE * (uVeHehtE /sDepthW ) 
! 	Extinction_coefficient_incl_vegetation
		aExtCefE = aExCofOE + aExtVegE
! 	Light_at_the_bottom_of_Epi
		aLPARotE = uLPARurf * exp(- aExtCefE * uDptWE)
! 	Light_at_the_top_of_Hyp
		if ( ( aInlSrat == 1 ) ) then
		uLPRSrfH = aLPARotE 		else		uLPRSrfH = 0.0 		endif
! 	Light_at_bottom_of_vegetation_layer
		aLPR2egE = aLPR1egE * exp(- aExtCefE * (aDpt2egE - aDpt1egE))
! 	Light_at_bottom_of_vegetation_layer
		aLPR2egH = aLPR1egH * exp(- aExtCefH * (aDpt2egH - aDpt1egH))
! 	Forcing_function_wind_speed
		if ( (ReadVind == 1)) then
		uVWind = mVWind 		else		uVWind = cVWind 		endif
! 	Presence_of_stratification
		if ( (ReadSrat == 1) ) then
		aStrat = mStrat 		else if ( ((uTmE -uTmH) >  cStratTm .or. (uTmH -uTmE) >  cStratTm) ) then		aStrat = 1.0  		else		aStrat = 0.0 		endif
! 	Seasonal_inflow
		if ( (0 == Useasoad) ) then
		uQISeson = 0.0 		else if ( (Day < cDayApr1 - 0.5*cLegCnge) ) then		uQISeson = cQInWin 		else if ( (Day < cDayApr1 + 0.5*cLegCnge) ) then		uQISeson = 0.5*(cQInWin + cQInSum) + 0.5*(cQInWin - cQInSum) * cos(Pi/cLegCnge *&
		& (Day - cDayApr1)) 		else if ( (Day < cDayOct1 - 0.5*cLegCnge) ) then		uQISeson = cQInSum 		else if ( (Day < cDayOct1 + 0.5*cLegCnge) ) then		uQISeson = 0.5*(cQInWin + cQInSum) - 0.5*(cQInWin - cQInSum) * cos(Pi/cLegCnge *&
		& (Day - cDayOct1)) 		else		uQISeson = cQInWin 		endif
! 	Sinusoid_evaporation
		uQEvSnus = cQEvAve - cQEvVar * cos(2.0*Pi * (sTime + TenDays - cTimeLag) / DaysI&
		&nY)
! 	Evaporation_from_heath_based_on_Thornthwaite
		if ( (10 *uTmE /cHeath) <= NearZero ) then
		aQEv = 16 * 2*ufDay * 1 / 30 * ( ( NearZero )** (cAlpha) ) 		else		aQEv = 16 * 2*ufDay * 1 / 30 * ( ( 10 *uTmE /cHeath )** (cAlpha) ) 		endif
! 	Evaporation
		if ( (0 == IncSeson) ) then
		uQEv = cQEvAve 		else if ( (ReadQEv == 1) ) then		uQEv = mQEv 		else if ( (calcQEv == 1 ) ) then		uQEv = aQEv 		else		uQEv = uQEvSnus 		endif
! 	Extra_inflow_(for_periodic_water_level_regulation
		if ( ((Day >= cDayApr1 - 0.5*cLegCnge) .and. (Day < cDayApr1 + 0.5*cLegCnge)) ) &
		&then
		uQInEtra = cQIExpr1 		else if ( ( (Day >= cDayOct1 - 0.5*cLegCnge) .and. (Day < cDayOct1 + 0.5*cLegCng&
		&e) ) ) then		uQInEtra = cQIExct1 		else		uQInEtra = 0.0 		endif
! 	Inflow
		if ( (ReadQIn == 1) ) then
		uQIn = mQIn 		else if ( (Useasoad == 1) ) then		uQIn = uQISeson + uQInEtra 		else		uQIn = cQIn + uQInEtra 		endif
! 	Extra_outflow_(for_periodic_water_level_regulation
		if ( ((Day >= cDayApr1 - 0.5*cLegCnge) .and. (Day < cDayApr1 + 0.5*cLegCnge)) ) &
		&then
		uQOtEtra = cQOtEpr1 		else if ( ( (Day >= cDayOct1 - 0.5*cLegCnge) .and. (Day < cDayOct1 + 0.5*cLegCng&
		&e) ) ) then		uQOtEtra = cQOtEct1 		else		uQOtEtra = 0.0 		endif
! 	Outflow
		if ( (ReadQOut == 1) ) then
		uQOutE = max(mQOut, (uDptWE - cDeptWMx) * cMxWOut * mmPerm) 		else if ( (0 == aInlSrat) ) then		uQOutE = max(0.0, (uQIn - uQInEtra) - uQEv - cQInf ) + uQOtEtra 		else		uQOutE = max(0.0, (uQIn - uQInEtra) - uQEv - cQInf ) + uQOtEtra 		endif
! 	Outflow
		uQOutH = 0.0
! 	Inflow_minus_evaporation
		uQDilE = uQOutE
! 	Dilution_in_hypolimnion
		uQDilH = uQOutH
! 	Dilution_rate_of_substances
		if ( (0 == InclTran) ) then
		ukDilE = 0.0 		else		ukDilE = uQDilE / mmPerm / uDptWE 		endif
! 	Dilution_rate_of_substances
		if ( (0 == InclTran) ) then
		ukDilH = 0.0 		else		ukDilH = uQDilH / mmPerm / uDptWH 		endif
! 	Dilution_rate_of_inflow_water_excluding_infiltration
		ukDilatE = uQIn / mmPerm / uDptWE
! 	Dilution_rate_of_inflow_water_excluding_infiltration
		ukDilatH = 0.0
! 	Outflow_rate_of_substances
		if ( (0 == InclTran) ) then
		ukOutE = 0.0 		else		ukOutE = uQOutE / mmPerm / uDptWE 		endif
! 	Outflow_rate_of_substances
		if ( (0 == InclTran) ) then
		ukOutH = 0.0 		else		ukOutH = uQOutH / mmPerm / uDptWH 		endif
! 	Water_residence_time
		if ( ( aInlSrat == 0 .and. cQInf < 0.0 ) ) then
		uTauWatE = 1.0 / ((uQIn - cQInf) / mmPerm / uDptWE +NearZero) 		else		uTauWatE = 1.0 / ((uQIn ) / mmPerm / uDptWE +NearZero) 		endif
! 	Water_residence_time
		if ( ( aInlSrat == 0 )) then
		uTauWatH = -9999 		else if ( (cQInf < 0.0 ) ) then		uTauWatH = 1.0 / ((- cQInf ) / mmPerm / uDptWH +NearZero) 		else		uTauWatH = 1.0 / ( cQInf / mmPerm / uDptWH +NearZero) 		endif
! 	Water_residence_time
		if ( ( cQInf > 0.0 ) ) then
		uTauWat = 1.0 / (uQIn / mmPerm / sDepthW +NearZero) 		else		uTauWat = 1.0 / ((uQIn - cQInf ) / mmPerm / sDepthW +NearZero) 		endif
! 	Residence_time_of_substances
		uTaSustH = 1.0 / (ukDilH+NearZero)
! 	Fijv_fraction
		if ( (aInlSrat == 1 ) ) then
		uFiJvMH = fFiJvMH 		else		uFiJvMH = 0.0 		endif
! 	Fijv_fraction
		if ( (aInlSrat == 1 ) ) then
		uFiJvEH = fFiJVesH 		else		uFiJvEH = 0.0 		endif
! 	Fijv_fraction
		if ( (aInlSrat == 1 ) ) then
		uFivEcrH = fFivEcrH 		else		uFivEcrH = 0.0 		endif
! 	Fiad_fraction
		if ( (aInlSrat == 1 ) ) then
		uFiAdMH = fFiAdMH 		else		uFiAdMH = 0.0 		endif
! 	Fiad_fraction
		if ( (aInlSrat == 1 ) ) then
		uFiAdEH = fFidEesH 		else		uFiAdEH = 0.0 		endif
! 	Fiad_fraction
		if ( (aInlSrat == 1 ) ) then
		uFidEcrH = fFidEcrH 		else		uFidEcrH = 0.0 		endif
! 	Pisc_fraction
		if ( (aInlSrat == 1 ) ) then
		uPiscMH = fPiscMH 		else		uPiscMH = 0.0 		endif
! 	Pisc_fraction
		if ( (aInlSrat == 1 ) ) then
		uPiscEH = fPicEesH 		else		uPiscEH = 0.0 		endif
! 	Pisc_fraction
		if ( (aInlSrat == 1 ) ) then
		uPicEcrH = fPicEcrH 		else		uPicEcrH = 0.0 		endif
! 	Change_in_water_depth
		if ( ( ContDpth == 1) ) then
		vTranptW = 0.0 		else if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then		vTranptW = (uQIn - uQEv - cQInf - uQOutH - uQOutE) / mmPerm 		else		vTranptW = ((uQIn - uQEv - cQInf - uQOutH - uQOutE) / mmPerm) / (1.0 + fMarsh) 		endif
! 	Exchange_flux_of_water_over_the_vertical_water_layers
		if ( (InclSrat == 0) ) then
		vTranHEW = 0.0 		else if ( ( sDepthW -uDptMix < cMnephHE ) ) then		vTranHEW = ( sDepthW - cMnephHE -uDptWE ) * kExchW 		else if ( ( uDptMix < cMnephHE ) ) then		vTranHEW = ( cMnephHE -uDptWE ) * kExchW 		else		vTranHEW = ( uDptMix -uDptWE ) * kExchW 		endif
! 	Change_in_water_depth
		if ( ( ContDpth == 1 .or. 0 == aInlSrat ) ) then
		vDeltaWH = 0.0 		else if ( (0 == InclMrsh .or. fMarsh <= NearZero ) ) then		vDeltaWH = ( - cQInf - uQOutH) / mmPerm 		else		vDeltaWH = ( - cQInf - uQOutH) / mmPerm / (1.0 + fMarsh ) 		endif
! 	Change_in_water_depth
		if ( (ContDpth == 1 ) ) then
		vDeltaWE = 0.0 		else if ( ( 0 == InclMrsh .or. fMarsh <= NearZero ) ) then		vDeltaWE = (uQIn - uQEv - uQOutE - cQInf ) / mmPerm 		else		vDeltaWE = (uQIn - uQEv - uQOutE - cQInf ) / mmPerm / (1.0 + fMarsh ) 		endif
! 	Fraction_change_in_hypolimnion_to_epilimnion_volume
		if ( (aInlSrat == 1 .and. uDptWH > NearZero) ) then
		afVolHE =  uDptWH / uDptWE 		else		afVolHE = 0.0 		endif
! 	Advective_exchange_flux_of_inorganic_matter_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAdvIMW = vTranHEW * oDIMWH 		else		wDAdvIMW = vTranHEW * oDIMWE 		endif
! 	Advective_exchange_flux_of_PO4_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAdvO4W = vTranHEW * oPO4WH 		else		wPAdvO4W = vTranHEW * oPO4WE 		endif
! 	Advective_exchange_flux_of_P_adsorbed_to_inorganic_matter_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAdvIMW = vTranHEW * oPAIMWH 		else		wPAdvIMW = vTranHEW * oPAIMWE 		endif
! 	Advective_exchange_flux_of_NH4_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAdvH4W = vTranHEW * oNH4WH 		else		wNAdvH4W = vTranHEW * oNH4WE 		endif
! 	Advective_exchange_flux_of_NO3_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAdvO3W = vTranHEW * oNO3WH 		else		wNAdvO3W = vTranHEW * oNO3WE 		endif
! 	Advective_exchange_flux_of_SiO2_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wSidvO2W = vTranHEW * oSiO2WH 		else		wSidvO2W = vTranHEW * oSiO2WE 		endif
! 	Advective_exchange_flux_of_O2_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wO2AdvW = vTranHEW * oO2WH 		else		wO2AdvW = vTranHEW * oO2WE 		endif
! 	Advective_exchange_flux_of_DW_of_detritus_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAdvDtW = vTranHEW * oDDtWH 		else		wDAdvDtW = vTranHEW * oDDtWE 		endif
! 	Advective_exchange_flux_of_P_in_detritus_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAdvDtW = vTranHEW * oPDtWH 		else		wPAdvDtW = vTranHEW * oPDtWE 		endif
! 	Advective_exchange_flux_of_N_in_detritus_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAdvDtW = vTranHEW * oNDtWH 		else		wNAdvDtW = vTranHEW * oNDtWE 		endif
! 	Advective_exchange_flux_of_Si_in_detritus_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wSiAdDtW = vTranHEW * oSiDtWH 		else		wSiAdDtW = vTranHEW * oSiDtWE 		endif
! 	Advective_exchange_flux_of_DW_in_diatoms_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAdvDiW = vTranHEW * oDDiWH 		else		wDAdvDiW = vTranHEW * oDDiWE 		endif
! 	Advective_exchange_flux_of_P_in_diatoms_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAdvDiW = vTranHEW * oPDiWH 		else		wPAdvDiW = vTranHEW * oPDiWE 		endif
! 	Advective_exchange_flux_of_N_in_diatoms_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAdvDiW = vTranHEW * oNDiWH 		else		wNAdvDiW = vTranHEW * oNDiWE 		endif
! 	Advective_exchange_flux_of_Si_in_diatoms_between_hypolimnion_and_epilimnion
		wSiAdDiW = vTranHEW * cSiDDi
! 	Advective_exchange_flux_of_DW_in_green_algae_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAvGenW = vTranHEW * oDGrenWH 		else		wDAvGenW = vTranHEW * oDGrenWE 		endif
! 	Advective_exchange_flux_of_P_in_green_algae_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAvGenW = vTranHEW * oPGrenWH 		else		wPAvGenW = vTranHEW * oPGrenWE 		endif
! 	Advective_exchange_flux_of_N_in_green_algae_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAvGenW = vTranHEW * oNGrenWH 		else		wNAvGenW = vTranHEW * oNGrenWE 		endif
! 	Advective_exchange_flux_of_DW_in_blue_greens_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAvBueW = vTranHEW * oDBlueWH 		else		wDAvBueW = vTranHEW * oDBlueWE 		endif
! 	Advective_exchange_flux_of_P_in_blue_greens_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAvBueW = vTranHEW * oPBlueWH 		else		wPAvBueW = vTranHEW * oPBlueWE 		endif
! 	Advective_exchange_flux_of_N_in_blue_greens_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAvBueW = vTranHEW * oNBlueWH 		else		wNAvBueW = vTranHEW * oNBlueWE 		endif
! 	Advective_exchange_flux_of_DW_in_zooplankton_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wDAdvooW = vTranHEW * oDZooH 		else		wDAdvooW = vTranHEW * oDZooE 		endif
! 	Advective_exchange_flux_of_P_in_zooplankton_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wPAdvooW = vTranHEW * oPZooH 		else		wPAdvooW = vTranHEW * oPZooE 		endif
! 	Advective_exchange_flux_of_N_in_zooplankton_between_hypolimnion_and_epilimnion
		if ( ( vTranHEW > 0.0) ) then
		wNAdvooW = vTranHEW * oNZooH 		else		wNAdvooW = vTranHEW * oNZooE 		endif
! 	Advective_exchange_flux_of_inorganic_matter_between_hypolimnion_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDAIMWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDAIMWM = vTranptW * oDIMWE 		else		wDAIMWM = vTranptW *  oDIMWM 		endif
! 	Advective_exchange_flux_of_PO4_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPAPO4WM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPAPO4WM = vTranptW * oPO4WE 		else		wPAPO4WM = vTranptW *  oPO4WM 		endif
! 	Advective_exchange_flux_of_P_adsorbed_to_inorganic_matter_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPAAIMWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPAAIMWM = vTranptW * oPAIMWE 		else		wPAAIMWM = vTranptW *  oPAIMWM 		endif
! 	Advective_exchange_flux_of_NH4_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNANH4WM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNANH4WM = vTranptW * oNH4WE 		else		wNANH4WM = vTranptW *  oNH4WM 		endif
! 	Advective_exchange_flux_of_NO3_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNANO3WM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNANO3WM = vTranptW * oNO3WE 		else		wNANO3WM = vTranptW *  oNO3WM 		endif
! 	Advective_exchange_flux_of_SiO2_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wSiAO2WM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wSiAO2WM = vTranptW * oSiO2WE 		else		wSiAO2WM = vTranptW *  oSiO2WM 		endif
! 	Advective_exchange_flux_of_O2_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wO2AWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wO2AWM = vTranptW * oO2WE 		else		wO2AWM = vTranptW *  oO2WM 		endif
! 	Advective_exchange_flux_of_DW_of_detritus_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDADeWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDADeWM = vTranptW * oDDtWE 		else		wDADeWM = vTranptW *  oDDtWM 		endif
! 	Advective_exchange_flux_of_P_in_detritus_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPADeWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPADeWM = vTranptW * oPDtWE 		else		wPADeWM = vTranptW *  oPDtWM 		endif
! 	Advective_exchange_flux_of_N_in_detritus_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNADeWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNADeWM = vTranptW * oNDtWE 		else		wNADeWM = vTranptW *  oNDtWM 		endif
! 	Advective_exchange_flux_of_Si_in_detritus_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wSiADeWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wSiADeWM = vTranptW * oSiDtWE 		else		wSiADeWM = vTranptW *  oSiDtWM 		endif
! 	Advective_exchange_flux_of_DW_in_diatoms_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDADiWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDADiWM = vTranptW * oDDiWE 		else		wDADiWM = vTranptW *  oDDiWM 		endif
! 	Advective_exchange_flux_of_P_in_diatoms_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPADiWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPADiWM = vTranptW * oPDiWE 		else		wPADiWM = vTranptW *  oPDiWM 		endif
! 	Advective_exchange_flux_of_N_in_diatoms_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNADiWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNADiWM = vTranptW * oNDiWE 		else		wNADiWM = vTranptW *  oNDiWM 		endif
! 	Advective_exchange_flux_of_Si_in_diatoms_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wSiADiWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wSiADiWM = vTranptW * oDDiWE * cSiDDi 		else		wSiADiWM = vTranptW *  oDDiWM  * cSiDDi 		endif
! 	Advective_exchange_flux_of_DW_in_green_algae_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDAGWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDAGWM = vTranptW * oDGrenWE 		else		wDAGWM = vTranptW *  oDGrenWM 		endif
! 	Advective_exchange_flux_of_P_in_green_algae_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPAGWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPAGWM = vTranptW * oPGrenWE 		else		wPAGWM = vTranptW *  oPGrenWM 		endif
! 	Advective_exchange_flux_of_N_in_green_algae_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNAGWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNAGWM = vTranptW * oNGrenWE 		else		wNAGWM = vTranptW *  oNGrenWM 		endif
! 	Advective_exchange_flux_of_DW_in_blue_greens_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDABWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDABWM = vTranptW * oDBlueWE 		else		wDABWM = vTranptW *  oDBlueWM 		endif
! 	Advective_exchange_flux_of_P_in_blue_greens_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPABWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPABWM = vTranptW * oPBlueWE 		else		wPABWM = vTranptW *  oPBlueWM 		endif
! 	Advective_exchange_flux_of_N_in_blue_greens_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNABWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNABWM = vTranptW * oNBlueWE 		else		wNABWM = vTranptW *  oNBlueWM 		endif
! 	Advective_exchange_flux_of_DW_in_zooplankton_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wDAZWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wDAZWM = vTranptW * oDZooE 		else		wDAZWM = vTranptW *  oDZooM 		endif
! 	Advective_exchange_flux_of_P_in_zooplankton_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wPAZWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wPAZWM = vTranptW * oPZooE 		else		wPAZWM = vTranptW *  oPZooM 		endif
! 	Advective_exchange_flux_of_N_in_zooplankton_between_marsh_and_epilimnion
		if ( (InclMrsh == 0 .or. fMarsh < NearZero) ) then
		wNAZWM = 0.0 		else if ( ( vTranptW > 0.0) ) then		wNAZWM = vTranptW * oNZooE 		else		wNAZWM = vTranptW *  oNZooM 		endif
! 	Diffusive_exchange_rate_between_epilimnion_and_hypolimnion
		if ( ( aInlSrat == 1) ) then
		uExMaxW = kExchMxW /( aStrat* fMeta * sDepthW + cDehMaMn ) 		else		uExMaxW = 0.0 		endif
! 	Diffusive_exchange_rate_of_hypolimnion
		if ( (aInlSrat == 1 .and. uDptWH > NearZero) ) then
		akExWH =  ( uExMaxW ) / uDptWH 		else		akExWH = 0.0 		endif
! 	Diffusive_exchange_rate_of_epilimnion
		if ( (aInlSrat == 1 ) ) then
		akExWE = akExWH * afVolHE 		else		akExWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_inorganic_matter_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDExIMWH = akExWH *(oDIMWH - oDIMWE) 		else		wDExIMWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_PO4_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPExP4WH = akExWH *(oPO4WH - oPO4WE) 		else		wPExP4WH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_adsorbed_to_inorganic_matter_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPExAMWH = akExWH *(oPAIMWH - oPAIMWE) 		else		wPExAMWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_NH4_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNExN4WH = akExWH *(oNH4WH - oNH4WE) 		else		wNExN4WH = 0.0 		endif
! 	Diffusive_exchange_flux_of_NO3_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNExN3WH = akExWH *(oNO3WH - oNO3WE) 		else		wNExN3WH = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wSixS2WH = akExWH *(oSiO2WH - oSiO2WE) 		else		wSixS2WH = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wO2ExWH = akExWH *(oO2WH - oO2WE) 		else		wO2ExWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_of_detritus_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDExDtWH = akExWH *(oDDtWH - oDDtWE) 		else		wDExDtWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPExDtWH = akExWH *(oPDtWH - oPDtWE) 		else		wPExDtWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNExDtWH = akExWH *(oNDtWH - oNDtWE) 		else		wNExDtWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wSiExtWH = akExWH *(oSiDtWH - oSiDtWE) 		else		wSiExtWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDExDiWH = akExWH *(oDDiWH - oDDiWE) 		else		wDExDiWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPExDiWH = akExWH *(oPDiWH - oPDiWE) 		else		wPExDiWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNExDiWH = akExWH *(oNDiWH - oNDiWE) 		else		wNExDiWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wSiExiWH = cSiDDi * wDExDiWH 		else		wSiExiWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDEGrnWH = akExWH *(oDGrenWH - oDGrenWE) 		else		wDEGrnWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPEGrnWH = akExWH *(oPGrenWH - oPGrenWE) 		else		wPEGrnWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNEGrnWH = akExWH *(oNGrenWH - oNGrenWE) 		else		wNEGrnWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDEBleWH = akExWH *(oDBlueWH - oDBlueWE) 		else		wDEBleWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPEBleWH = akExWH *(oPBlueWH - oPBlueWE) 		else		wPEBleWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNEBleWH = akExWH *(oNBlueWH - oNBlueWE) 		else		wNEBleWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wDExZoWH = akExWH *(oDZooH - oDZooE) 		else		wDExZoWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wPExZoWH = akExWH *(oPZooH - oPZooE) 		else		wPExZoWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_and_to_hypolimnion
		if ( (aInlSrat == 1 ) ) then
		wNExZoWH = akExWH *(oNZooH - oNZooE) 		else		wNExZoWH = 0.0 		endif
! 	Diffusive_exchange_flux_of_inorganic_matter_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDExIMWE = akExWE *(oDIMWH - oDIMWE) 		else		wDExIMWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_PO4_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPExP4WE = akExWE *(oPO4WH - oPO4WE) 		else		wPExP4WE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_adsorbed_to_inorganic_matter_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPExAMWE = akExWE *(oPAIMWH - oPAIMWE) 		else		wPExAMWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_NH4_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNExN4WE = akExWE *(oNH4WH - oNH4WE) 		else		wNExN4WE = 0.0 		endif
! 	Diffusive_exchange_flux_of_NO3_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNExN3WE = akExWE *(oNO3WH - oNO3WE) 		else		wNExN3WE = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wSixS2WE = akExWE *(oSiO2WH - oSiO2WE) 		else		wSixS2WE = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wO2ExWE = akExWE *(oO2WH - oO2WE) 		else		wO2ExWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_of_detritus_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDExDtWE = akExWE *(oDDtWH - oDDtWE) 		else		wDExDtWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPExDtWE = akExWE *(oPDtWH - oPDtWE) 		else		wPExDtWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNExDtWE = akExWE *(oNDtWH - oNDtWE) 		else		wNExDtWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wSiExtWE = akExWE *(oSiDtWH - oSiDtWE) 		else		wSiExtWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDExDiWE = akExWE *(oDDiWH - oDDiWE) 		else		wDExDiWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPExDiWE = akExWE *(oPDiWH - oPDiWE) 		else		wPExDiWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNExDiWE = akExWE *(oNDiWH - oNDiWE) 		else		wNExDiWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wSiExiWE = cSiDDi * wDExDiWE 		else		wSiExiWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDEGrnWE = akExWE *(oDGrenWH - oDGrenWE) 		else		wDEGrnWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPEGrnWE = akExWE *(oPGrenWH - oPGrenWE) 		else		wPEGrnWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNEGrnWE = akExWE *(oNGrenWH - oNGrenWE) 		else		wNEGrnWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDEBleWE = akExWE *(oDBlueWH - oDBlueWE) 		else		wDEBleWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPEBleWE = akExWE *(oPBlueWH - oPBlueWE) 		else		wPEBleWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNEBleWE = akExWE *(oNBlueWH - oNBlueWE) 		else		wNEBleWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wDExZoWE = akExWE *(oDZooH - oDZooE) 		else		wDExZoWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wPExZoWE = akExWE *(oPZooH - oPZooE) 		else		wPExZoWE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_and_to_epilimnion
		if ( (aInlSrat == 1 ) ) then
		wNExZoWE = akExWE *(oNZooH - oNZooE) 		else		wNExZoWE = 0.0 		endif
! 	Marsh_water_exchange_coefficient
		akExMH = 0.0
! 	Lake_water_exchange_coefficient
		akExLH = 0.0
! 	Total_DW_phytoplankton_in_lake_water
		oDPhytWH = oDDiWH + oDGrenWH + oDBlueWH
! 	Total_P_phytoplankton_in_lake_water
		oPPhytWH = oPDiWH + oPGrenWH + oPBlueWH
! 	Total_N_phytoplankton_in_lake_water
		oNPhytWH = oNDiWH + oNGrenWH + oNBlueWH
! 	Total_DW_phytoplankton_on_lake_sediment
		aDPhytS = sDDiatS + sDGrenS + sDBlueS 
! 	Total_P_phytoplankton_on_lake_sediment
		aPPhytS = sPDiatS + sPGrenS + sPBlueS 
! 	Total_N_phytoplankton_on_lake_sediment
		aNPhytS = sNDiatS + sNGrenS + sNBlueS
! 	Total_DW_phytoplankton_in_lake_water
		oDPhytWE = oDDiWE + oDGrenWE + oDBlueWE
! 	Organic_seston
		oDOMWE = oDDtWE + oDPhytWE
! 	Organic_seston
		oDOMWH = oDDtWH + oDPhytWH
! 	Organic_seston
		oTDOMW = oDOMWE + oDOMWH
! 	Total_seston
		oDSestWH = oDOMWH + oDIMWH
! 	Organic_P_in_water
		oPOMWH = oPPhytWH + oPDtWH
! 	Total_seston_P_(incl_adsorbed
		oPSestWH = oPPhytWH + oPDtWH + oPAIMWH
! 	Inorganic_P_in_water
		oPInogWH = oPO4WH + oPAIMWH
! 	Total_P_in_water_(excl_animals_AND_vegetation)
		oPTotWH = oPSestWH + oPO4WH
! 	SRN_in_water
		oNDissWE = oNO3WE + oNH4WE
! 	SRN_in_water
		oNDissWH = oNO3WH + oNH4WH
! 	Org_seston_N
		oNOMWH = oNPhytWH + oNDtWH
! 	Total_seston_N
		oNSestWH = oNOMWH
! 	Kjeldahl_N_in_water
		oNkjWH = oNSestWH + oNH4WH
! 	Total_N_in_water_(without_animals_AND_vegetation)
		oNTotWH = oNkjWH + oNO3WH
! 	Porosity
		bPorS = (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cRhoWat &
		&/ ( fDTotS0 + (1.0 - fDTotS0) * (fDOrgS0 * cRhoOM + (1 - fDOrgS0) * cRhoIM) / cR&
		&hoWat )
! 	Sediment_porosity_corrected_for_tortuosity
		if ( ((bPorS )** (bPorS + 1.0)) <= NearZero ) then
		bPorCorS = 1 		else		bPorCorS = ((bPorS )** (bPorS + 1.0)) 		endif
! 	Total_sediment_(excl_biota)
		aDTotS = sDIMS + sDHumS + sDDetS
! 	(apparent)_bulk_density_of_sediment
		aRhoTotS = aDTotS / cDepthS
! 	Average_solid_density
		aRhSoidS = (sDIMS * cRhoIM + (sDHumS + sDDetS) * cRhoOM) / aDTotS
! 	Sediment_dry-weight_fraction
		afDTotS = 1.0 / (1.0 + bPorS/(1.0-bPorS) * cRhoWat / aRhSoidS)
! 	Total_organic_fraction_of_sediment_DW
		afDOrgS = (sDHumS + sDDetS) / aDTotS
! 	Detrital_fraction_of_sediment_organic_DW
		afDtS = sDDetS / (sDHumS + sDDetS)
! 	Detrital_fraction_of_total_sediment_DW
		afDtTotS = sDDetS / (sDIMS + sDHumS + sDDetS)
! 	Inorganic_P_in_sediment
		aPInorgS = sPO4S + sPAIMS
! 	Total_P_in_sediment_(excl_humus_animals_AND_vegetation)
		aPTtAilS = sPDetS + aPInorgS + aPPhytS
! 	Total_P_in_sediment_(excl_animals_AND_vegetation)
		aPTotS = aPTtAilS + sPHumS
! 	Fraction_inorganic_P_in_sediment
		afPInrgS = aPInorgS / aDTotS
! 	Total_P_fraction_in_sediment
		afPTotS = aPTotS / aDTotS
! 	Fraction_dissolved_P_in_sediment
		afPO4S = sPO4S / (aPTtAilS + NearZero)
! 	Conc_dissolved_P_in_interstitial_water
		oPO4S = sPO4S / cDepthS / bPorS
! 	Total_dissolved_N_in_pore_water
		aNDissS = sNH4S + sNO3S
! 	Kjeldahl_N_in_sediment_excl_humus
		aNkAvilS = sNDetS + aNPhytS + sNH4S
! 	Kjeldahl_N_in_sediment
		aNkjS = aNkAvilS + sNHumS
! 	Total_N_in_sediment_excl_humus
		aNTtAilS = aNkAvilS + sNO3S
! 	Total_N_in_sediment
		aNTotS = aNkjS + sNO3S
! 	Fraction_inorganic_N_in_sediment
		afNInrgS = aNDissS / aDTotS
! 	Total_N_fraction_in_sediment
		afNTotS = aNTotS / aDTotS
! 	Conc_dissolved_N-NO3_in_interstitial_water
		oNO3S = sNO3S / cDepthS / bPorS
! 	Conc_dissolved_N-NH4_in_interstitial_water
		oNH4S = sNH4S / cDepthS / bPorS
! 	Dissolved_N_conc_in_sediment_needed_for_calc_of_veg_uptake_rate
		oNDissS = aNDissS / cDepthS / bPorS
! 	P/D_ratio_of_water_DIM
		rPDIMWH = oPAIMWH / oDIMWH
! 	P/D_ratio_of_sediment_DIM
		rPDIMS = sPAIMS / sDIMS
! 	P/D_ratio_of_water_detritus
		rPDDtWH = oPDtWH / (oDDtWH+NearZero)
! 	N/D_ratio_of_water_detritus
		rNDDtWH = oNDtWH / (oDDtWH+NearZero)
! 	Si/D_ratio_of_water_detritus
		rSiDDtWH = oSiDtWH / (oDDtWH+NearZero)
! 	P_content_of_sediment_OM
		rPDHumS = sPHumS / (sDHumS+NearZero)
! 	N_content_of_sediment_OM
		rNDHumS = sNHumS / (sDHumS+NearZero)
! 	P_content_of_sediment_detritus
		rPDDtS = sPDetS / (sDDetS+NearZero)
! 	N_content_of_sediment_detritus
		rNDDtS = sNDetS / (sDDetS+NearZero)
! 	Si_content_of_sediment_detritus
		rSiDDtS = sSiDetS / (sDDetS+NearZero)
! 	Total_DW_phytoplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oDPhytWM = oDDiWM + oDGrenWM + oDBlueWM 		else		oDPhytWM = 0.0 		endif
! 	Total_P_phytoplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPPhytWM = oPDiWM + oPGrenWM + oPBlueWM 		else		oPPhytWM = 0.0 		endif
! 	Total_N_phytoplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNPhytWM = oNDiWM + oNGrenWM + oNBlueWM 		else		oNPhytWM = 0.0 		endif
! 	Total_Si_diatoms_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oSiDiWM = cSiDDi * oDDiWM 		else		oSiDiWM = 0.0 		endif
! 	Organic_seston
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oDOMWM = oDDtWM + oDPhytWM 		else		oDOMWM = 0.0 		endif
! 	Total_seston
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oDSestWM = oDOMWM + oDIMWM 		else		oDSestWM = 0.0 		endif
! 	Organic_P_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPOMWM = oPPhytWM + oPDtWM 		else		oPOMWM = 0.0 		endif
! 	Total_seston_P(incl_adsorbed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPSestWM = oPPhytWM + oPDtWM + oPAIMWM 		else		oPSestWM = 0.0 		endif
! 	Inorganic_P_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPInogWM = oPO4WM + oPAIMWM 		else		oPInogWM = 0.0 		endif
! 	Total_P_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPTotWM = oPSestWM + oPO4WM 		else		oPTotWM = 0.0 		endif
! 	SRN_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNDissWM = oNO3WM + oNH4WM 		else		oNDissWM = 0.0 		endif
! 	Org_seston_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNOMWM = oNPhytWM + oNDtWM 		else		oNOMWM = 0.0 		endif
! 	Total_seston_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNSestWM = oNOMWM 		else		oNSestWM = 0.0 		endif
! 	Kjeldahl_N_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNkjWM = oNSestWM + oNH4WM 		else		oNkjWM = 0.0 		endif
! 	Total_N_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNTotWM = oNkjWM + oNO3WM 		else		oNTotWM = 0.0 		endif
! 	Porosity
		bPorSM = (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) / cRhoW&
		&at /(fDTotSM0 + (1.0 - fDTotSM0) * (fDOrgSM0 * cRhoOM +(1 - fDOrgSM0) * cRhoIM) &
		&/ cRhoWat)
! 	Sediment_porosity_corrected_for_tortuosity
		if ( ( ( bPorSM )** (bPorSM + 1.0)) <= NearZero ) then
		bPorCrSM = 1 		else		bPorCrSM = ( ( bPorSM )** (bPorSM + 1.0)) 		endif
! 	Total_sediment(excl_biota)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aDTotSM = sDIMSM + sDHumSM + sDDetSM 		else		aDTotSM = 0.0 		endif
! 	(apparent)_bulk_density_of_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aRhoTtSM = aDTotSM / cDepthSM 		else		aRhoTtSM = 0.0 		endif
! 	Average_solid_density
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aRhSodSM = (sDIMSM * cRhoIM +(sDHumSM + sDDetSM) * cRhoOM) / aDTotSM 		else		aRhSodSM = 0.0 		endif
! 	Sediment_dry-weight_fraction
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afDTotSM = 1.0 /(1.0 + bPorSM/(1.0-bPorSM) * cRhoWat / aRhSodSM) 		else		afDTotSM = 0.0 		endif
! 	Total_organic_fraction_of_sediment_DW
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afDOrgSM = (sDHumSM + sDDetSM) / aDTotSM 		else		afDOrgSM = 0.0 		endif
! 	Detrital_fraction_of_sediment_organic_DW
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afDtSM = sDDetSM /(sDHumSM + sDDetSM) 		else		afDtSM = 0.0 		endif
! 	Detrital_fraction_of_total_sediment_DW
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afDtTtSM = sDDetSM /(sDIMSM + sDHumSM + sDDetSM) 		else		afDtTtSM = 0.0 		endif
! 	Inorganic_P_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPInogSM = sPO4SM + sPAIMSM 		else		aPInogSM = 0.0 		endif
! 	Total_P_in_sediment(excl_humus_animals_AND_vegetation)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPTAvlSM = sPDetSM + aPInogSM 		else		aPTAvlSM = 0.0 		endif
! 	Total_P_in_sediment(excl_animals_AND_vegetation)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPTotSM = aPTAvlSM + sPHumSM 		else		aPTotSM = 0.0 		endif
! 	Fraction_inorganic_P_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afPnogSM = aPInogSM / aDTotSM 		else		afPnogSM = 0.0 		endif
! 	Total_P_fraction_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afPTotSM = aPTotSM / aDTotSM 		else		afPTotSM = 0.0 		endif
! 	Fraction_dissolved_P_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afPO4SM = sPO4SM /(aPTAvlSM + NearZero) 		else		afPO4SM = 0.0 		endif
! 	Conc_dissolved_P_in_interstitial_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oPO4SM = sPO4SM / cDepthSM / bPorSM 		else		oPO4SM = 0.0 		endif
! 	Total_dissolved_N_in_pore_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNDissSM = sNH4SM + sNO3SM 		else		aNDissSM = 0.0 		endif
! 	Kjeldahl_N_in_sediment_excl_humus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNkAvlSM = sNDetSM + sNH4SM 		else		aNkAvlSM = 0.0 		endif
! 	Kjeldahl_N_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNkjSM = aNkAvlSM + sNHumSM 		else		aNkjSM = 0.0 		endif
! 	Total_N_in_sediment_excl_humus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNTAvlSM = aNkAvlSM + sNO3SM 		else		aNTAvlSM = 0.0 		endif
! 	Total_N_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNTotSM = aNkjSM + sNO3SM 		else		aNTotSM = 0.0 		endif
! 	Fraction_inorganic_N_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afNnogSM = aNDissSM / aDTotSM 		else		afNnogSM = 0.0 		endif
! 	Total_N_fraction_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afNTotSM = aNTotSM / aDTotSM 		else		afNTotSM = 0.0 		endif
! 	Conc_dissolved_N-NO3_in_interstitial_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNO3SM = sNO3SM / cDepthSM / bPorSM 		else		oNO3SM = 0.0 		endif
! 	Conc_dissolved_N-NH4_in_interstitial_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNH4SM = sNH4SM / cDepthSM / bPorSM 		else		oNH4SM = 0.0 		endif
! 	Dissolved_N_conc_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		oNDissSM = aNDissSM / cDepthSM / bPorSM 		else		oNDissSM = 0.0 		endif
! 	P/D_ratio_of_DIM_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDIMWM = oPAIMWM / oDIMWM 		else		rPDIMWM = 0.0 		endif
! 	P/D_ratio_of_DIM_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDIMSM = sPAIMSM / sDIMSM 		else		rPDIMSM = 0.0 		endif
! 	P/D_ratio_of_marsh_water_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDDtWM = oPDtWM /(oDDtWM+NearZero) 		else		rPDDtWM = 0.0 		endif
! 	N/D_ratio_of_marsh_water_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rNDDtWM = oNDtWM /(oDDtWM+NearZero) 		else		rNDDtWM = 0.0 		endif
! 	Si/D_ratio_of_marsh_water_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rSiDDtWM = oSiDtWM /(oDDtWM+NearZero) 		else		rSiDDtWM = 0.0 		endif
! 	P_content_of_marsh_sediment_OM
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDHumSM = sPHumSM /(sDHumSM+NearZero) 		else		rPDHumSM = 0.0 		endif
! 	N_content_of_marsh_sediment_OM
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rNDHumSM = sNHumSM /(sDHumSM+NearZero) 		else		rNDHumSM = 0.0 		endif
! 	P_content_of_marsh_sediment_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDDtSM = sPDetSM /(sDDetSM+NearZero) 		else		rPDDtSM = 0.0 		endif
! 	N_content_of_marsh_sediment_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rNDDtSM = sNDetSM /(sDDetSM+NearZero) 		else		rNDDtSM = 0.0 		endif
! 	Si_content_of_marsh_sediment_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rSiDDtSM = sSiDetSM /(sDDetSM+NearZero) 		else		rSiDDtSM = 0.0 		endif
! 	Total_D_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aDTotM = ((oDIMWM + oDDtWM + oDPhytWM + oDZooM) * sDepthWM + sDIMSM + sDHumSM + &
		&sDDetSM + sDShPhra + sDRoPhra) * fMarsh 		else		aDTotM = 0.0 		endif
! 	Total_P_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPTotM = ((oPO4WM + oPDtWM + oPAIMWM + oPPhytWM + oPZooM) * sDepthWM + sPO4SM + &
		&sPHumSM + sPDetSM + sPAIMSM + sPShPhra + sPRoPhra) * fMarsh 		else		aPTotM = 0.0 		endif
! 	Total_N_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNTotM = ((oNH4WM + oNO3WM + oNDtWM + oNPhytWM + oNZooM) * sDepthWM + sNH4SM + s&
		&NO3SM + sNHumSM + sNDetSM + sNShPhra + sNRoPhra) * fMarsh 		else		aNTotM = 0.0 		endif
! 	Total_Si_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aSiTotM = ((oSiO2WM + oSiDtWM + oSiDiWM) * sDepthWM + sSiDetSM) * fMarsh 		else		aSiTotM = 0.0 		endif
! 	Total_Si_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aO2TotM = ((oO2WM) * sDepthWM ) * fMarsh 		else		aO2TotM = 0.0 		endif
! 	Seasonal_P_load
		if ( (0 == Useasoad) ) then
		uPLdSson = 0.0 		else if ( (Day < cDayApr1 - 0.5*cLegCnge) ) then		uPLdSson = cPLoaWin 		else if ( (Day < cDayApr1 + 0.5*cLegCnge) ) then		uPLdSson = 0.5*(cPLoaWin + cPLoaSum) + 0.5*(cPLoaWin - cPLoaSum) * cos(Pi/cLegCn&
		&ge * (Day - cDayApr1)) 		else if ( (Day < cDayOct1 - 0.5*cLegCnge) ) then		uPLdSson = cPLoaSum 		else if ( (Day < cDayOct1 + 0.5*cLegCnge) ) then		uPLdSson = 0.5*(cPLoaWin + cPLoaSum) - 0.5*(cPLoaWin - cPLoaSum) * cos(Pi/cLegCn&
		&ge * (Day - cDayOct1)) 		else		uPLdSson = cPLoaWin 		endif
! 	P_load
		if ( (0 == InclTran) ) then
		uPLoad = 0.0 		else if ( (ReadPoad == 1 .and. ReaNurac == 1) ) then		uPLoad = mPLoaPO4 + mPLoaOrg 		else if ( (ReadPoad == 1 .and. 0 == ReaNurac) ) then		uPLoad = mPLoad 		else if ( (Uselsoad == 1) ) then		uPLoad = 0.0 		else if ( (Useasoad == 1) ) then		uPLoad = uPLdSson 		else		uPLoad = cPLoad 		endif
! 	P_load_PO4
		if ( (0 == ReadPoad) ) then
		uPLoaPO4 = fPO4In * uPLoad 		else if ( (ReaNurac == 1) ) then		uPLoaPO4 = mPLoaPO4 		else		uPLoaPO4 = fPO4In * uPLoad 		endif
! 	P_load_bound_to_org_matter
		if ( (0 == ReadPoad) ) then
		uPLoaOrg = (1.0 - fPO4In) * uPLoad 		else if ( (ReaNurac == 1) ) then		uPLoaOrg = mPLoaOrg 		else		uPLoaOrg = (1.0 - fPO4In) * uPLoad 		endif
! 	(total)_algal_P_input
		if ( (0 == IncSeson) ) then
		uPLdPTot = (((fPInSum + fPInWin)/2)*uPLoaOrg) 		else if ( (ReaPLadP == 1) ) then		uPLdPTot = mPLadTot 		else		uPLdPTot = ((fPInSum+fPInWin)/2.0 - (fPInSum-fPInWin)/2.0 * cos(2.0*Pi * (sTime &
		&+ TenDays - cTimeLag) / DaysInY)) * uPLoaOrg 		endif
! 	Detrital_P_input
		uPLoadDt = uPLoaOrg - uPLdPTot
! 	Adsorbed_P_loading_(=0)
		uPLoaAIM = 0.0
! 	Seasonal_N_load
		if ( (0 == Useasoad) ) then
		uNLdSson = 0.0 		else if ( (Day < cDayApr1 - 0.5*cLegCnge) ) then		uNLdSson = cNLoaWin 		else if ( (Day < cDayApr1 + 0.5*cLegCnge) ) then		uNLdSson = 0.5*(cNLoaWin + cNLoaSum) + 0.5*(cNLoaWin - cNLoaSum) * cos(Pi/cLegCn&
		&ge * (Day - cDayApr1)) 		else if ( (Day < cDayOct1 - 0.5*cLegCnge) ) then		uNLdSson = cNLoaSum 		else if ( (Day < cDayOct1 + 0.5*cLegCnge) ) then		uNLdSson = 0.5*(cNLoaWin + cNLoaSum) - 0.5*(cNLoaWin - cNLoaSum) * cos(Pi/cLegCn&
		&ge * (Day - cDayOct1)) 		else		uNLdSson = cNLoaWin 		endif
! 	(total)_algal_N_input
		uNLdPTot = cNPPIn * uPLdPTot
! 	N_load
		if ( (0 == InclTran) ) then
		uNLoad = 0.0 		else if ( (ReadNoad == 1 .and. ReaNurac == 1) ) then		uNLoad = mNLoaNH4 + mNLoaNO3 + mNLoaOrg 		else if ( (ReadNoad == 1 .and. 0 == ReaNurac) ) then		uNLoad = mNLoad 		else if ( (Uselsoad == 1) ) then		uNLoad = 0.0 		else if ( (ReadPoad == 1) ) then		uNLoad = cNPoaeas * uPLoad 		else if ( (Useasoad == 1) ) then		uNLoad = uNLdSson 		else		uNLoad = cNLoad 		endif
! 	N_load_detritus
		if ( (0 == ReadNoad) ) then
		uNLoadDt = min(cNPDeIn * uPLoadDt, uNLoad - uNLdPTot) 		else if ( (ReaNurac == 1) ) then		uNLoadDt = 0.0 		else		uNLoadDt = min(cNPDeIn * uPLoadDt, uNLoad - uNLdPTot) 		endif
! 	Loading_N_bound_to_org_matter
		if ( (0 == ReadNoad) ) then
		uNLoaOrg = uNLdPTot + uNLoadDt 		else if ( (ReaNurac == 1) ) then		uNLoaOrg = mNLoaOrg 		else		uNLoaOrg = uNLdPTot + uNLoadDt 		endif
! 	N_loading_dissolved_(sum_of_NO2_and_NH4)
		if ( (0 == ReadNoad) ) then
		uNLadiss = uNLoad - uNLoaOrg 		else if ( (ReaNurac == 1) ) then		uNLadiss = 0.0 		else		uNLadiss = uNLoad - uNLoaOrg 		endif
! 	NH4_loading
		if ( (0 == ReadNoad) ) then
		uNLoaNH4 = fNHDisIn * uNLadiss 		else if ( (ReaNurac == 1) ) then		uNLoaNH4 = mNLoaNH4 		else		uNLoaNH4 = fNHDisIn * uNLadiss 		endif
! 	NO3_loading
		if ( (0 == ReadNoad) ) then
		uNLoaNO3 = (1.0 - fNHDisIn) * uNLadiss 		else if ( (ReaNurac == 1) ) then		uNLoaNO3 = mNLoaNO3 		else		uNLoaNO3 = (1.0 - fNHDisIn) * uNLadiss 		endif
! 	NO3_loading
		if ( (0 == aInlSrat) ) then
		uNBckadH = 0.0 		else		uNBckadH = cNBckoad 		endif
! 	NO3_loading
		if ( (0 == aInlSrat) ) then
		uPBckadH = 0.0 		else		uPBckadH = cPBckoad 		endif
! 	External_N_conc
		uNTotIn = uNLoad / (uQIn / mmPerm + NearZero)
! 	Detrital_DW_loading
		if ( (ReaDLdDe == 1) ) then
		uDLoadDt = mDLoadDe 		else		uDLoadDt = uNLoadDt / cNDDeIn 		endif
! 	(total)_algal_DW_input
		uDLdPTot = uNLdPTot / cNDPIn
! 	Loading_of_DW_of_inorg_matter
		if ( (0 == InclTran) ) then
		uDLoadIM = 0.0 		else if ( (ReaDLdIM == 1) ) then		uDLoadIM = mDLoadIM 		else		uDLoadIM = cDIMn * uQIn / mmPerm 		endif
! 	Total_DW_input
		if ( (0 == InclTran) ) then
		uDLoad = 0.0 		else		uDLoad = uDLoadIM + uDLoadDt + uDLdPTot 		endif
! 	External_P_concentration
		UotIn = uPLoad / (uQIn / mmPerm + NearZero)
! 	Diat_input
		uDLoadDi = fDiPIn * uDLdPTot
! 	Diat_input
		uPLoadDi = fDiPIn * uPLdPTot
! 	Diat_input
		uNLoadDi = fDiPIn * uNLdPTot
! 	Gren_input
		uDLadren = fGrenPIn * uDLdPTot
! 	Gren_input
		uPLadren = fGrenPIn * uPLdPTot
! 	Gren_input
		uNLadren = fGrenPIn * uNLdPTot
! 	Blue_input
		uDLadlue = fBluEn * uDLdPTot
! 	Blue_input
		uPLadlue = fBluEn * uPLdPTot
! 	Blue_input
		uNLadlue = fBluEn * uNLdPTot
! 	Dilution_of_DW_IM
		wDDilIMH = ukDilH * oDIMWH
! 	Dilution_of_detritus
		wDDilDtH = ukDilH * oDDtWH
! 	Dilution_of_Gren
		wDDlGenH = ukDilH * oDGrenWH
! 	Dilution_of_Blue
		wDDlBueH = ukDilH * oDBlueWH
! 	Dilution_of_Diat
		wDDilDiH = ukDilH * oDDiWH
! 	Total_zooplankton_dilution
		wDDilooH = ukDilH * oDZooH
! 	Total_algal_dilution
		wDDlPytH = wDDilDiH + wDDlGenH + wDDlBueH
! 	Total_zooplankton_dilution
		wPDilooH = ukDilH * oPZooH
! 	Total_zooplankton_dilution
		wNDilooH = ukDilH * oNZooH
! 	Dilution_of_SRP
		wPDilO4H = ukDilH * oPO4WH
! 	Dilution_of_detritus
		wPDilDtH = ukDilH*oPDtWH
! 	Dilution_of_IM-ads_P
		wPDilIMH = ukDilH * oPAIMWH
! 	Dilution_of_ammonium
		wNDilH4H = ukDilH * oNH4WH
! 	Dilution_of_nitrate
		wNDilO3H = ukDilH * oNO3WH
! 	Dilution_of_detritus
		wNDilDtH = ukDilH * oNDtWH
! 	Oxygen_inflow
		wO2nfowH = ukDilatH * cO2In
! 	Oxygen_outflow
		wO2OuflH = ukDilH * oO2WH
! 	Dilution_of_Diat
		wPDilDiH = ukDilH * oPDiWH
! 	Dilution_of_Diat
		wNDilDiH = ukDilH * oNDiWH
! 	Dilution_of_Gren
		wPDlGenH = ukDilH * oPGrenWH
! 	Dilution_of_Gren
		wNDlGenH = ukDilH * oNGrenWH
! 	Dilution_of_Blue
		wPDlBueH = ukDilH * oPBlueWH
! 	Dilution_of_Blue
		wNDlBueH = ukDilH * oNBlueWH
! 	Total_algal_dilution
		wPDlPytH = wPDilDiH + wPDlGenH + wPDlBueH
! 	Total_algal_dilution
		wNDlPytH = wNDilDiH + wNDlGenH + wNDlBueH
! 	Outflow_of_DW
		wDOtfotH = ukOutH * oDSestWH
! 	Outflow_of_P
		wPOtfotH = ukOutH * oPTotWH
! 	Outflow_of_N
		wNOtfotH = ukOutH * oNTotWH
! 	Transport_flux_of_D_in_Diat
		if ( (0 == InclTran) ) then
		wDTraDiH = 0.0 		else		wDTraDiH = - wDDilDiH 		endif
! 	Transport_flux_of_P_in_Diat
		if ( (0 == InclTran) ) then
		wPTraDiH = 0.0 		else		wPTraDiH = - wPDilDiH 		endif
! 	Transport_flux_of_N_in_Diat
		if ( (0 == InclTran) ) then
		wNTraDiH = 0.0 		else		wNTraDiH = - wNDilDiH 		endif
! 	Transport_flux_of_D_in_Gren
		if ( (0 == InclTran) ) then
		wDTanenH = 0.0 		else		wDTanenH = - wDDlGenH 		endif
! 	Transport_flux_of_P_in_Gren
		if ( (0 == InclTran) ) then
		wPTanenH = 0.0 		else		wPTanenH = - wPDlGenH 		endif
! 	Transport_flux_of_N_in_Gren
		if ( (0 == InclTran) ) then
		wNTanenH = 0.0 		else		wNTanenH = - wNDlGenH 		endif
! 	Transport_flux_of_D_in_Blue
		if ( (0 == InclTran) ) then
		wDTanueH = 0.0 		else		wDTanueH = - wDDlBueH 		endif
! 	Transport_flux_of_P_in_Blue
		if ( (0 == InclTran) ) then
		wPTanueH = 0.0 		else		wPTanueH = - wPDlBueH 		endif
! 	Transport_flux_of_N_in_Blue
		if ( (0 == InclTran) ) then
		wNTanueH = 0.0 		else		wNTanueH = - wNDlBueH 		endif
! 	Total_transport_flux_of_D_in_Phyt
		if ( (0 == InclTran) ) then
		wDTanytH = 0.0 		else		wDTanytH = wDTraDiH + wDTanenH + wDTanueH 		endif
! 	Total_transport_flux_of_P_in_Phyt
		if ( (0 == InclTran) ) then
		wPTanytH = 0.0 		else		wPTanytH = wPTraDiH + wPTanenH + wPTanueH 		endif
! 	Total_transport_flux_of_N_in_Phyt
		if ( (0 == InclTran) ) then
		wNTanytH = 0.0 		else		wNTanytH = wNTraDiH + wNTanenH + wNTanueH 		endif
! 	Total_transport_flux_of_Si_in_sio2
		uSioaiO2 = cSiO2In * uQIn / mmPerm
! 	Total_transport_flux_of_Si_in_Det
		uSiLodDt = cSiDDeIn * uDLoadDt
! 	Total_transport_flux_of_Si_in_Diat
		uSiLodDi = cSiDDi * uDLoadDi
! 	Silica_loading
		if ( (0 == InclTran) ) then
		uSiLoad = 0.0 		else		uSiLoad = uSioaiO2 + uSiLodDt + uSiLodDi 		endif
! 	Dilution_of_Si_in_sio2
		wSiilO2H = ukDilH * oSiO2WH
! 	Dilution_of_Si_in_detritus
		wSiDiDtH = ukDilH * oSiDtWH
! 	Dilution_of_Si_in_diatoms
		wSiDiDiH = cSiDDi * wDDilDiH
! 	Total_Si_surface_outflow
		wSitfotH = ukOutH * (oSiO2WH + oSiDtWH + cSiDDi * oDDiWH)
! 	Transport_flux_of_Si_in_SIO2
		if ( (0 == InclTran) ) then
		wSianO2H = 0.0 		else		wSianO2H = - wSiilO2H 		endif
! 	Transport_flux_of_Si_in_detritus
		if ( (0 == InclTran) ) then
		wSiratWH = 0.0 		else		wSiratWH = - wSiDiDtH 		endif
! 	Total_Si_transport_flux
		if ( (0 == InclTran) ) then
		tSiantTH = 0.0 		else		tSiantTH = - (wSiDiDtH + wSiilO2H + wSiDiDiH) * uDptWH 		endif
! 	Net_migration_flux_of_D_in_Zoo
		if ( (0 == InclTran) ) then
		wDTanooH = 0.0 		else		wDTanooH =( - wDDilooH) 		endif
! 	Net_migration_flux_of_P_in_ZOO
		if ( (0 == InclTran) ) then
		wPTanooH = 0.0 		else		wPTanooH =( - wPDilooH) 		endif
! 	Net_migration_flux_of_N_in_Zoo
		if ( (0 == InclTran) ) then
		wNTanooH = 0.0 		else		wNTanooH =( - wNDilooH) 		endif
! 	Transport_flux_DW_in_IM
		if ( (0 == InclTran) ) then
		wDTanMWH = 0.0 		else		wDTanMWH = - wDDilIMH 		endif
! 	Transport_flux_DW_in_detritus
		if ( (0 == InclTran) ) then
		wDTantWH = 0.0 		else		wDTantWH = - wDDilDtH 		endif
! 	Transport_flux_O2
		if ( (0 == InclTran) ) then
		wO2TrnWH = 0.0 		else		wO2TrnWH = - wO2OuflH 		endif
! 	Transport_flux_of_P_in_PO4
		if ( (0 == InclTran) ) then
		wPTan4WH = 0.0 		else		wPTan4WH = - wPDilO4H 		endif
! 	Transport_flux_of_P_in_AIM
		if ( (0 == InclTran) ) then
		wPTanMWH = 0.0 		else		wPTanMWH = - wPDilIMH 		endif
! 	Transport_flux_of_P_in_detritus
		if ( (0 == InclTran) ) then
		wPTantWH = 0.0 		else		wPTantWH = - wPDilDtH 		endif
! 	Transport_flux_of_N_in_NH4
		if ( (0 == InclTran) ) then
		wNTan4WH = 0.0 		else		wNTan4WH = - wNDilH4H 		endif
! 	Transport_flux_of_N_in_NO3
		if ( (0 == InclTran) ) then
		wNTan3WH = 0.0 		else		wNTan3WH = - wNDilO3H 		endif
! 	Transport_flux_of_N_in_detritus
		if ( (0 == InclTran) ) then
		wNTantWH = 0.0 		else		wNTantWH = - wNDilDtH 		endif
! 	Total_DW_dilution_fluxes
		wDDilotH = wDDilIMH + wDDilDtH + wDDlPytH
! 	Total_P_dilution_fluxes
		wPDilotH = wPDilDtH + wPDilO4H + wPDilIMH + wPDlPytH
! 	Total_N_dilution_fluxes
		wNDilotH = wNDilDtH + wNDilO3H + wNDilH4H + wNDlPytH
! 	Total_SI_dilution_fluxes
		wSiilotH = wSiDiDtH + wSiilO2H + wSiDiDiH
! 	Total_transport_fluxes_of_DW_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tDTantTH = 0.0 		else		tDTantTH = - wDDilotH * uDptWH 		endif
! 	Total_transport_fluxes_of_P_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tPTantTH = 0.0 		else		tPTantTH = uPLoad - wPDilotH * uDptWH 		endif
! 	Total_transport_fluxes_of_N_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tNTantTH = 0.0 		else		tNTantTH = uNLoad - wNDilotH * uDptWH 		endif
! 	Diffusive_exchange_flux_of_DW_in_IM_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExIMMH = akExMH *(oDIMWH - oDIMWM) 		else		wDExIMMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_PO4_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExP4MH = akExMH *(oPO4WH - oPO4WM) 		else		wPExP4MH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_AIM_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExAMMH = akExMH *(oPAIMWH - oPAIMWM) 		else		wPExAMMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NH4_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExN4MH = akExMH *(oNH4WH - oNH4WM) 		else		wNExN4MH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NO3_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExN3MH = akExMH *(oNO3WH - oNO3WM) 		else		wNExN3MH = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSixS2MH = akExMH *(oSiO2WH - oSiO2WM) 		else		wSixS2MH = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2ExMH = akExMH *(oO2WH - oO2WM) 		else		wO2ExMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_detritus_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDtMH = akExMH *(oDDtWH - oDDtWM) 		else		wDExDtMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDtMH = akExMH *(oPDtWH - oPDtWM) 		else		wPExDtMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDtMH = akExMH *(oNDtWH - oNDtWM) 		else		wNExDtMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExtMH = akExMH *(oSiDtWH - oSiDtWM) 		else		wSiExtMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDiMH = akExMH *(oDDiWH - oDDiWM) 		else		wDExDiMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDiMH = akExMH *(oPDiWH - oPDiWM) 		else		wPExDiMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDiMH = akExMH *(oNDiWH - oNDiWM) 		else		wNExDiMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExiMH = cSiDDi * wDExDiMH 		else		wSiExiMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDEGrnMH = akExMH *(oDGrenWH - oDGrenWM) 		else		wDEGrnMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPEGrnMH = akExMH *(oPGrenWH - oPGrenWM) 		else		wPEGrnMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNEGrnMH = akExMH *(oNGrenWH - oNGrenWM) 		else		wNEGrnMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDEBleMH = akExMH *(oDBlueWH - oDBlueWM) 		else		wDEBleMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPEBleMH = akExMH *(oPBlueWH - oPBlueWM) 		else		wPEBleMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNEBleMH = akExMH *(oNBlueWH - oNBlueWM) 		else		wNEBleMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExZoMH = akExMH *(oDZooH - oDZooM) 		else		wDExZoMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExZoMH = akExMH *(oPZooH - oPZooM) 		else		wPExZoMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_hypolimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExZoMH = akExMH *(oNZooH - oNZooM) 		else		wNExZoMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_IM_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExIMH = akExLH *(oDIMWH - oDIMWM) 		else		wDExIMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_PO4_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExPO4H = akExLH *(oPO4WH - oPO4WM) 		else		wPExPO4H = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_AIM_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExAIMH = akExLH *(oPAIMWH - oPAIMWM) 		else		wPExAIMH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NH4_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExNH4H = akExLH *(oNH4WH - oNH4WM) 		else		wNExNH4H = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NO3_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExNO3H = akExLH *(oNO3WH - oNO3WM) 		else		wNExNO3H = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSixSO2H = akExLH *(oSiO2WH - oSiO2WM) 		else		wSixSO2H = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2ExH = akExLH *(oO2WH - oO2WM) 		else		wO2ExH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_detritus_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDtH = akExLH *(oDDtWH - oDDtWM) 		else		wDExDtH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDtH = akExLH *(oPDtWH - oPDtWM) 		else		wPExDtH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDtH = akExLH *(oNDtWH - oNDtWM) 		else		wNExDtH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExDtH = akExLH *(oSiDtWH - oSiDtWM) 		else		wSiExDtH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDiH = akExLH *(oDDiWH - oDDiWM) 		else		wDExDiH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDiH = akExLH *(oPDiWH - oPDiWM) 		else		wPExDiH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDiH = akExLH *(oNDiWH - oNDiWM) 		else		wNExDiH = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExDiH = cSiDDi * wDExDiH 		else		wSiExDiH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExGenH = akExLH *(oDGrenWH - oDGrenWM) 		else		wDExGenH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExGenH = akExLH *(oPGrenWH - oPGrenWM) 		else		wPExGenH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExGenH = akExLH *(oNGrenWH - oNGrenWM) 		else		wNExGenH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExBueH = akExLH *(oDBlueWH - oDBlueWM) 		else		wDExBueH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExBueH = akExLH *(oPBlueWH - oPBlueWM) 		else		wPExBueH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExBueH = akExLH *(oNBlueWH - oNBlueWM) 		else		wNExBueH = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExZooH = akExLH *(oDZooH - oDZooM) 		else		wDExZooH = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExZooH = akExLH *(oPZooH - oPZooM) 		else		wPExZooH = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_marsh_to_hypolimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExZooH = akExLH *(oNZooH - oNZooM) 		else		wNExZooH = 0.0 		endif
! 	Infiltration_of_SRP
		if ( (cQInf >= 0.0) ) then
		tPIfP4WH = cQInf / mmPerm * oPO4WH 		else		tPIfP4WH = cQInf / mmPerm * oPO4S 		endif
! 	Infiltration_of_ammonium
		if ( (cQInf >= 0.0) ) then
		tNIfN4WH = cQInf / mmPerm * oNH4WH 		else		tNIfN4WH = cQInf / mmPerm * oNH4S 		endif
! 	Infiltration_of_nitrate
		if ( (cQInf >= 0.0) ) then
		tNIfN3WH = cQInf / mmPerm * oNO3WH 		else		tNIfN3WH = cQInf / mmPerm * oNO3S 		endif
! 	Infiltration_of_interstitial_PO4
		if ( (cQInf >= 0.0) ) then
		tPInfO4S = cQInf / mmPerm * oPO4S 		else		tPInfO4S = cQInf / mmPerm * cPO4Gr 		endif
! 	Infiltration_of_interstitial_NH4
		if ( (cQInf >= 0.0) ) then
		tNInfH4S = cQInf / mmPerm * oNH4S 		else		tNInfH4S = cQInf / mmPerm * cNH4Gr 		endif
! 	Infiltration_of_interstitial_NO3
		if ( (cQInf >= 0.0) ) then
		tNInfO3S = cQInf / mmPerm * oNO3S 		else		tNInfO3S = cQInf / mmPerm * cNO3Gr 		endif
! 	NH4_load_to_sediment_from_artificial_fertilizer
		tNH4LadS = fNH4LadS * cNLoadS
! 	NO3_load_to_sediment_from_artificial_fertilizer
		tNO3LadS = cNLoadS - tNH4LadS
! 	IM_input_from_banks
		uDErosIM = (1.0 - fDOrgoil) * cDEroTot
! 	IM_input_to_sediment_from_banks
		uDEroIMS = fSeErsIM * uDErosIM
! 	IM_input_to_water_column_from_banks
		uDEroIMW = uDErosIM - uDEroIMS
! 	Organic_matter_input_from_banks
		uDErosOM = fDOrgoil * cDEroTot
! 	Organic_P_input_from_banks
		uPErosOM = cPDSolOM * uDErosOM
! 	Organic_N_input_from_banks
		uNErosOM = cNDSolOM * uDErosOM
! 	Oxygen_saturation_concentration
		uO2SatE = 14.652 - 0.41022 * uTmE + 0.007991 * uTmE*uTmE - 0.000077774 * uTmE*uT&
		&mE*uTmE
! 	Oxygen_saturation_concentration
		uO2SatH = 14.652 - 0.41022 * uTmH + 0.007991 * uTmH*uTmH- 0.000077774 *uTmH*uTmH&
		&*uTmH
! 	Reaeration_coefficient
		if ( ( ( uVWind )** (0.5))  <= NearZero ) then
		kAer = 1 		else		kAer = cAerR * ( ( uVWind )** (0.5)) + cAerLin * uVWind + cAeSqare * uVWind*uVWi&
		&nd 		endif
! 	Temperature_function_of_reaeration
		if (  ( ( cThetAer )** (uTmE-cTmRef)) <= NearZero ) then
		uFuTmerE = 1 		else		uFuTmerE = ( ( cThetAer )** (uTmE-cTmRef)) 		endif
! 	Temperature_function_of_reaeration
		if ( ( ( cThetAer )** (uTmH-cTmRef)) <= NearZero ) then
		uFuTmerH = 1 		else		uFuTmerH = ( ( cThetAer )** (uTmH-cTmRef)) 		endif
! 	Floating_leave_function_of_reaeration
		aFuLeAer =  max( 0.0, 1.0 -  ( kFloAer * aDFoaVeg ) )
! 	Bubble_formation
		if ( (uO2SatH < oO2WH ) ) then
		tO2BubH = kBub * uFuTmerH * (uO2SatH - oO2WH) 		else		tO2BubH = 0 		endif
! 	Temp_function_of_fish
		if ( (InclWeb == 1) ) then
		uFuTmish = exp( -0.5/(cSiTmish*cSiTmish) * (( max (uTmE, uTmH * aInlSrat) - cTmp&
		&tish)*( max (uTmE, uTmH * aInlSrat) - cTmptish) - (cTmRef - cTmptish)*(cTmRef - &
		&cTmptish))) 		else		uFuTmish = 0.0 		endif
! 	Bioturbation_by_fish
		if ( (InclWeb == 1) ) then
		tDTrbish = (kTurbish * uFuTmish * sDFiAd) 		else		tDTrbish = 0.0 		endif
! 	Vegetation_dependence_of_resuspension
		aFuegsus = max(1.0 - kVResus * sDVeg, 0.0)
! 	Empirical_suspended_matter_function_(logistic_fit_to_data)
		if ( (uTmE >= 0.1) .or.  ((((cFetch +NearZero)/ cFetcRef) )** (0.5)) <= NearZero&
		& ) then
		aFuDiusp = cSuspRef * ((cSuspMn + cSuspMx / (1.0 + exp(cSupSope * (sDepthW - hDe&
		&thusp)))) * ((((cFetch +NearZero)/ cFetcRef) )** (0.5))) 		else		aFuDiusp = 0.0 		endif
! 	Resuspension_due_to_shear_stress
		if ( ((aFuDiusp +NearZero )** (0.5)) <= NearZero ) then
		tDRsTead = min(aFuDiusp, (( 1  )** (0.5))) * ((fLutum / fLutuRef )** (0.5)) * bP&
		&orS 		else		tDRsTead = min(aFuDiusp, ((aFuDiusp +NearZero )** (0.5))) * ((fLutum / fLutuRef &
		&)** (0.5)) * bPorS 		endif
! 	Resuspension_due_to_shear_stress_AND_fish
		tDRsBead = tDRsTead + tDTrbish
! 	Resuspension_corrected_for_vegetation_effect
		tDRsuead = tDRsBead * aFuegsus
! 	IM_resuspension
		tDRessIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDRsuead
! 	Detrital_resuspension
		tDRessDt = sDDetS / (fLutum * sDIMS + sDDetS) * tDRsuead
! 	Phytoplankton_resuspension_rate_constant
		akRsPRef = kResuPMx * (1.0 - exp(cReusExp * tDRsuead))
! 	Phytoplankton_resuspension
		tDRsPTot = akRsPRef * aDPhytS
! 	Resuspension_flux_of_detrital_P
		tPRessDt = rPDDtS * tDRessDt
! 	Resuspension_flux_of_dissolved_P
		tPRsuPO4 = sPO4S / sDDetS * tDRessDt
! 	Resuspension_flux_of_P_adsorbed_onto_inert_matter
		tPRsuAIM = sPAIMS / sDIMS * tDRessIM
! 	Resuspension_flux_of_nitrate
		tNRsuNO3 = sNO3S / sDDetS * tDRessDt
! 	Resuspension_flux_of_ammonium
		tNRsuNH4 = sNH4S / sDDetS * tDRessDt
! 	Resuspension_flux_of_detrital_N
		tNRessDt = rNDDtS * tDRessDt
! 	Resuspension_flux_of_detrital_SI
		tSiessDt = rSiDDtS * tDRessDt
! 	Correction_factor_for_IM_settling_rate_(<=_1)
		if ( ((aFuDiusp +NearZero )** (0.5)) < NearZero ) then
		aFuauOMH = 1 		else		aFuauOMH = min(1.0 / ((aFuDiusp +NearZero )** (0.5)), 1.0) 		endif
! 	Correction_factor_for_OM_settling_rate_(<=_1)
		aFuauIMH = aFuauOMH
! 	Temperature_correction_of_sedimentation
		if ( ( ( cThetSet )** (uTmE-cTmRef)) < NearZero ) then
		uFuTmetE = 1 		else		uFuTmetE = ( ( cThetSet )** (uTmE-cTmRef)) 		endif
! 	Temperature_correction_of_sedimentation
		if ( ((cThetSet )** (uTmH-cTmRef))  < NearZero ) then
		uFuTmetH = 1 		else		uFuTmetH = ((cThetSet )** (uTmH-cTmRef)) 		endif
! 	Corrected_sedimentation_velocity_of_IM
		if ( (((fLutuRef/fLutum) )** (0.5))  < NearZero ) then
		uCoVSIMH = 1 		else		uCoVSIMH = aFuauIMH * (((fLutuRef/fLutum) )** (0.5)) * uFuTmetH * cVSetIM 		endif
! 	Sedimentation_flux_of_inert_matter
		tDSetIMH = uCoVSIMH * oDIMWH
! 	Sedimentation_flux_of_P_adsorbed_onto_inert_org_matter
		tPSetIMH = oPAIMWH / oDIMWH * tDSetIMH
! 	Corrected_sedimentation_velocity_of_detritus
		uCoVSDtH = cVSetDe * aFuauOMH * uFuTmetH
! 	Sedimentation_flux_of_detritus
		tDSetDtH = uCoVSDtH * oDDtWH
! 	Sedimentation_flux_of_detrital_P
		tPSetDtH = uCoVSDtH * oPDtWH
! 	Sedimentation_flux_of_detrital_N
		tNSetDtH = uCoVSDtH * oNDtWH
! 	Sedimentation_flux_of_detrital_Si
		tSiSeDtH = uCoVSDtH * oSiDtWH
! 	P_mineralisation_constant_in_water
		kPMinDtW = kDMnDeW
! 	N_mineralisation_constant_in_water
		kNMinDtW = kDMnDeW
! 	Si_mineralisation_constant_in_water
		kSiMiDtW = kDMnDeW
! 	Temp_function_of_mineralization_in_water
		if ( ((cThetMnW )** (uTmE-cTmRef))  < NearZero ) then
		uFuTmnWE = 1 		else		uFuTmnWE = ((cThetMnW )** (uTmE-cTmRef)) 		endif
! 	Temp_function_of_mineralization_in_water
		if ( ((cThetMnW )** (uTmE-cTmRef))  < NearZero ) then
		uFuTmnWH = 1 		else		uFuTmnWH =  ((cThetMnW )** (uTmE-cTmRef)) 		endif
! 	Decomposition
		if ( (aInlSrat == 0) ) then
		wDMintWH = 0.0 		else		wDMintWH = kDMnDeW * uFuTmnWH * oDDtWH 		endif
! 	Mineralization
		if ( (aInlSrat == 0) ) then
		wPMintWH = 0.0 		else		wPMintWH = kPMinDtW * uFuTmnWH * oPDtWH 		endif
! 	Mineralization
		if ( (aInlSrat == 0) ) then
		wNMintWH = 0.0 		else		wNMintWH = kNMinDtW * uFuTmnWH * oNDtWH 		endif
! 	Mineralization
		if ( (aInlSrat == 0) ) then
		wSiintWH = 0.0 		else		wSiintWH = kSiMiDtW * uFuTmnWH * oSiDtWH 		endif
! 	Correction_of_O2_demand_in_water_at_low_oxygen_conc
		aCoO2ODH = oO2WH / (hO2BOD + oO2WH)
! 	O2_flux_due_to_mineralization_of_detritus
		if ( (aInlSrat == 0) ) then
		wO2intWH = 0.0 		else		wO2intWH = molO2olC * cCPerDW * aCoO2ODH * wDMintWH 		endif
! 	Mineralisation_flux_by_denitrification
		if ( (aInlSrat == 0) ) then
		wDDentWH = 0.0 		else		wDDentWH = oNO3WH*oNO3WH / (hNO3Dnit*hNO3Dnit + oNO3WH*oNO3WH) * (1.0 - aCoO2ODH&
		&) * wDMintWH 		endif
! 	Denitrification_flux
		if ( (aInlSrat == 0) ) then
		wNDentWH = 0.0 		else		wNDentWH = NO3PerC * molNmolC * cCPerDW * wDDentWH 		endif
! 	Temperature_dependence_for_nitrification
		if (   ((cThtaitr )** (uTmH-cTmRef))  < NearZero ) then
		uFuTmtrH = 1 		else		uFuTmtrH = ((cThtaitr )** (uTmH-cTmRef)) 		endif
! 	Temperature_dependence_for_nitrification
		if ( ((cThtaitr )** (uTmE-cTmRef))  < NearZero ) then
		uFuTmtrE = 1 		else		uFuTmtrE = ((cThtaitr )** (uTmE-cTmRef)) 		endif
! 	Temperature_dependence_for_nitrification
		if ( ((cThtaitr )** (uTmE-cTmRef))  < NearZero ) then
		uFuTmtrS = 1 		else		uFuTmtrS = ((cThtaitr )** (uTmE-cTmRef)) 		endif
! 	Oxygen_consumption_during_nitrification
		aCo2NrWH = oO2WH*oO2WH / (hO2Nitr*hO2Nitr + oO2WH*oO2WH)
! 	Nitrification_flux
		if ( (aInlSrat == 0) ) then
		wNNitrWH = 0.0 		else		wNNitrWH = kNitrW * uFuTmtrH * aCo2NrWH * oNH4WH 		endif
! 	O2_flux_due_to_nitrification
		if ( (aInlSrat == 0) ) then
		wO2NirWH = 0.0 		else		wO2NirWH = O2PerNH4 * molO2olN * wNNitrWH 		endif
! 	P_mineralisation_constant_in_sed
		kPMinDtS = kDMnDeS
! 	N_mineralisation_constant_in_sed
		kNMinDtS = kDMnDeS
! 	Si_mineralisation_constant_in_sed
		kSiMiDtS = kDMnDeS
! 	Temp_function
		if ( ((cThetMnS )** (uTmBot-cTmRef)) < NearZero ) then
		uFuTminS = 1 		else		uFuTminS =  ((cThetMnS )** (uTmBot-cTmRef)) 		endif
! 	Decomposition_of_upper_sediment
		tDMinDtS = kDMnDeS * uFuTminS * sDDetS
! 	Mineralization_of_P_in_upper_sediment
		tPMinDtS = kPMinDtS * uFuTminS * sPDetS
! 	Mineralization_of_N_in_upper_sediment
		tNMinDtS = kNMinDtS * uFuTminS * sNDetS
! 	Mineralization_of_Si_in_upper_sediment
		tSiMiDtS = kSiMiDtS * uFuTminS * sSiDetS
! 	Temperature_function_of_diffusion
		if ( ((cThetDif )** (uTmE-cTmRef)) < NearZero ) then
		uFunTDif = 1 		else		uFunTDif = ((cThetDif )** (uTmE-cTmRef)) 		endif
! 	Corrected_O2_diffusion_coefficient
		akODiCor = kO2Dif * uFunTDif * cTubDfO2 * bPorCorS
! 	Sediment_oxygen_demand
		tSOD = (molO2olC * cCPerDW * (1.0 - fRefrDeS) * tDMinDtS + O2PerNH4 * molO2olN *&
		& kNitrS * uFuTmtrS * sNH4S) / cDepthS
! 	Average_diffusion_distance
		aDptDif = fDethifS * cDepthS
! 	Diffusion_flux_of_dissolved_P_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tPDifO4H = kPDifPO4 * uFunTDif * cTubDNut * bPorCorS * (oPO4S - oPO4WH ) / aDptD&
		&if 		else		tPDifO4H = 0.0 		endif
! 	Diffusion_flux_of_NO3_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tNDifO3H = kNDifNO3 * uFunTDif * cTubDNut * bPorCorS * (oNO3S - oNO3WH ) / aDptD&
		&if 		else		tNDifO3H = 0.0 		endif
! 	Diffusion_flux_of_NH4_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tNDifH4H = kNDifNH4 * uFunTDif * cTubDNut * bPorCorS * (oNH4S - oNH4WH ) / aDptD&
		&if 		else		tNDifH4H = 0.0 		endif
! 	O2_diffusion_(water_->_sediment)
		if ( ( aInlSrat == 1 ) ) then
		tO2DifH = kO2Dif / aDptDif * uFunTDif * cTubDfO2 * bPorCorS * oO2WH 		else		tO2DifH = 0.0 		endif
! 	Diffusion_flux_of_dissolved_P_from_pore_water_to_groundwater
		tPDroPO4 = 0.0
! 	Diffusion_flux_of_dissolved_NO3_from_pore_water_to_groundwater
		tNDroNO3 = 0.0
! 	Diffusion_flux_of_dissolved_NH4_from_pore_water_to_groundwater
		tNDroNH4 = 0.0
! 	Max_P_adsorption_per_g_inorg_matter_in_water
		aPAsMxWH = cRelPdsD + aCoO2ODH * cRePAsFe * fFeDIM + cRePAsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsWH = (1.0 - fRedMx * (1.0-aCoO2ODH)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		aPIoAsWH = aPAsMxWH * aKPAdsWH * oPO4WH / (1.0 + aKPAdsWH * oPO4WH)
! 	Equilibrium_conc
		aPEqIMWH = aPIoAsWH * oDIMWH
! 	Sorption_flux_in_water
		if ( (aInlSrat == 0) ) then
		wPSrpMWH = 0.0 		else		wPSrpMWH = kPSorp * (aPEqIMWH - oPAIMWH) 		endif
! 	Chemical_loss_of_dissolved_P_from_pore_water
		tPChePO4 = max( 0.0, kPChePO4 * (oPO4S - coPO4Mx) )
! 	Total_abiotic/microbial_Si_flux_for_mass_balance_check
		tSibiotT = - fRefrDeS * tSiMiDtS
! 	Reed_evaporation(set_EQUAL_to_lake_evaporation)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		uQEvPhra = uQEv 		else		uQEvPhra = 0.0 		endif
! 	SRP_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPEvP4WM = uQEvPhra / mmPerm * oPO4WM 		else		tPEvP4WM = 0.0 		endif
! 	Ammonium_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNEvN4WM = uQEvPhra / mmPerm * oNH4WM 		else		tNEvN4WM = 0.0 		endif
! 	Nitrate_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNEvN3WM = uQEvPhra / mmPerm * oNO3WM 		else		tNEvN3WM = 0.0 		endif
! 	Infiltration_of_SRP
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPIfP4WM = 0.0 		else if ( (cQInf >= 0.0) ) then		tPIfP4WM = cQInf / mmPerm * oPO4WM 		else		tPIfP4WM = cQInf / mmPerm * oPO4SM 		endif
! 	Infiltration_of_ammonium
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNIfN4WM = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN4WM = cQInf / mmPerm * oNH4WM 		else		tNIfN4WM = cQInf / mmPerm * oNH4SM 		endif
! 	Infiltration_of_nitrate
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNIfN3WM = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN3WM = cQInf / mmPerm * oNO3WM 		else		tNIfN3WM = cQInf / mmPerm * oNO3SM 		endif
! 	Infiltration_of_interstitial_PO4
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPIfP4SM = 0.0 		else if ( (cQInf >= 0.0) ) then		tPIfP4SM = cQInf / mmPerm * oPO4SM 		else		tPIfP4SM = cQInf / mmPerm * cPO4Gr 		endif
! 	Infiltration_of_interstitial_NH4
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNIfN4SM = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN4SM = cQInf / mmPerm * oNH4SM 		else		tNIfN4SM = cQInf / mmPerm * oNH4SM 		endif
! 	Infiltration_of_interstitial_NO3
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNIfN3SM = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN3SM = cQInf / mmPerm * oNO3SM 		else		tNIfN3SM = cQInf / mmPerm * oNO3SM 		endif
! 	Reaeration_flux_of_O2_into_the_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2AerM = kAer * uFuTmerE *(uO2SatE - oO2WM) 		else		tO2AerM = 0.0 		endif
! 	Sedimentation_flux_of_inert_matter
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDSetIMM = cVSetIM * uFuTmetE * oDIMWM 		else		tDSetIMM = 0.0 		endif
! 	Sedimentation_flux_of_P_adsorbed_onto_inert_org_matter
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPSetIMM = cVSetIM * uFuTmetE * oPAIMWM 		else		tPSetIMM = 0.0 		endif
! 	Sedimentation_flux_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDSetDtM = cVSetDe * uFuTmetE * oDDtWM 		else		tDSetDtM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_P
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPSetDtM = cVSetDe * uFuTmetE* oPDtWM 		else		tPSetDtM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNSetDtM = cVSetDe * uFuTmetE* oNDtWM 		else		tNSetDtM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_Si
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tSiSeDtM = cVSetDe * uFuTmetE * oSiDtWM 		else		tSiSeDtM = 0.0 		endif
! 	Sedimentation_flux_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDSetDiM = cVSetDi * uFuTmetE * oDDiWM 		else		tDSetDiM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_P
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPSetDiM = cVSetDi * uFuTmetE* oPDiWM 		else		tPSetDiM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNSetDiM = cVSetDi * uFuTmetE* oNDiWM 		else		tNSetDiM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_Si
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tSiSeDiM = cVSetDi * uFuTmetE * oSiDiWM 		else		tSiSeDiM = 0.0 		endif
! 	Sedimentation_flux_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDStGenM = cVSetren * uFuTmetE * oDGrenWM 		else		tDStGenM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_P
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPStGenM = cVSetren * uFuTmetE* oPGrenWM 		else		tPStGenM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNStGenM = cVSetren * uFuTmetE* oNGrenWM 		else		tNStGenM = 0.0 		endif
! 	Sedimentation_flux_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDStBueM = cVSetlue * uFuTmetE * oDBlueWM 		else		tDStBueM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_P
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPStBueM = cVSetlue * uFuTmetE* oPBlueWM 		else		tPStBueM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNStBueM = cVSetlue * uFuTmetE* oNBlueWM 		else		tNStBueM = 0.0 		endif
! 	Sedimentation_flux_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDStPytM = tDSetDiM + tDStGenM + tDStBueM 		else		tDStPytM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_P
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPStPytM = tPSetDiM + tPStGenM + tPStBueM 		else		tPStPytM = 0.0 		endif
! 	Sedimentation_flux_of_detrital_N
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNStPytM = tNSetDiM + tNStGenM + tNStBueM 		else		tNStPytM = 0.0 		endif
! 	Total_sedimentation_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDSetotM = tDSetIMM + tDSetDtM + tDStPytM 		else		tDSetotM = 0.0 		endif
! 	Decomposition
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDMintWM = kDMnDeW * uFuTmnWE * oDDtWM 		else		wDMintWM = 0.0 		endif
! 	Mineralization
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPMintWM = kPMinDtW * uFuTmnWE * oPDtWM 		else		wPMintWM = 0.0 		endif
! 	Mineralization
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNMintWM = kNMinDtW * uFuTmnWE * oNDtWM 		else		wNMintWM = 0.0 		endif
! 	Mineralization
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiintWM = kSiMiDtW * uFuTmnWE * oSiDtWM 		else		wSiintWM = 0.0 		endif
! 	Correction_of_O2_demand_in_water_at_low_oxygen_conc
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aCoO2ODM = oO2WM /(hO2BOD + oO2WM) 		else		aCoO2ODM = 0.0 		endif
! 	O2_flux_due_to_mineralization_of_detritus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2intWM = molO2olC * cCPerDW * aCoO2ODM * wDMintWM 		else		wO2intWM = 0.0 		endif
! 	Mineralisation_flux_by_denitrification
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDDentWM = oNO3WM*oNO3WM /(hNO3Dnit*hNO3Dnit + oNO3WM*oNO3WM) *(1.0 - aCoO2ODM) &
		&* wDMintWM 		else		wDDentWM = 0.0 		endif
! 	Denitrification_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNDentWM = NO3PerC * molNmolC * cCPerDW * wDDentWM 		else		wNDentWM = 0.0 		endif
! 	Oxygen_use_for_nitrification_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aCo2NrWM = oO2WM * oO2WM /(hO2Nitr * hO2Nitr + oO2WM * oO2WM) 		else		aCo2NrWM = 0.0 		endif
! 	Nitrification_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNNitrWM = kNitrW * uFuTmtrE * aCo2NrWM * oNH4WM 		else		wNNitrWM = 0.0 		endif
! 	O2_flux_due_to_nitrification
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2NirWM = O2PerNH4 * molO2olN * wNNitrWM 		else		wO2NirWM = 0.0 		endif
! 	Decomposition_of_upper_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMintSM = kDMnDeS * uFuTminS * sDDetSM 		else		tDMintSM = 0.0 		endif
! 	Mineralization_of_P_in_upper_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMintSM = kPMinDtS * uFuTminS * sPDetSM 		else		tPMintSM = 0.0 		endif
! 	Mineralization_of_N_in_upper_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMintSM = kNMinDtS * uFuTminS * sNDetSM 		else		tNMintSM = 0.0 		endif
! 	Mineralization_of_Si_in_upper_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tSiintSM = kSiMiDtS * uFuTminS * sSiDetSM 		else		tSiintSM = 0.0 		endif
! 	Corrected_O2_diffusion_coefficient
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		akODiorM = kO2Dif * uFunTDif * cTubDfO2 * bPorCrSM 		else		akODiorM = 0.0 		endif
! 	Sediment_oxygen_demand
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tSODM =(molO2olC * cCPerDW *(1.0 - fRefrDeS) * tDMintSM + O2PerNH4 * molO2olN * &
		&kNitrS * uFuTmtrE * sNH4SM) / cDepthSM 		else		tSODM = 0.0 		endif
! 	Oxygen_penetration_depth
		if ( (InclMrsh == 0 .or. fMarsh <= NearZero) ) then
		aDpOxedM = 0.0 		else if ( (((2.0 * oO2WM * akODiorM / tSODM) )** (0.5)) <= NearZero ) then		aDpOxedM = 1 		else		aDpOxedM = (((2.0 * oO2WM * akODiorM / tSODM) )** (0.5)) 		endif
! 	Fraction_aerobic_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afOxyedM = aDpOxedM / cDepthSM 		else		afOxyedM = 0.0 		endif
! 	Aerobic_mineralisation
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMOxtSM = afOxyedM *(1.0 - fRefrDeS) * tDMintSM 		else		tDMOxtSM = 0.0 		endif
! 	Sediment_oxygen_demand
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2intSM = molO2olC * cCPerDW * tDMOxtSM 		else		tO2intSM = 0.0 		endif
! 	Mineralisation_flux_by_denitrification
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDDentSM = oNO3SM*oNO3SM /(hNO3Dnit*hNO3Dnit + oNO3SM*oNO3SM) *(1.0 - afOxyedM) &
		&*(1.0 - fRefrDeS) * tDMintSM 		else		tDDentSM = 0.0 		endif
! 	Denitrification_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNDentSM = NO3PerC * molNmolC * cCPerDW * tDDentSM 		else		tNDentSM = 0.0 		endif
! 	Nitrification_flux
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNNitrSM = afOxyedM * kNitrS * uFuTmtrE * sNH4SM 		else		tNNitrSM = 0.0 		endif
! 	O2_flux_due_to_nitrification
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2NirSM = O2PerNH4 * molO2olN * tNNitrSM 		else		tO2NirSM = 0.0 		endif
! 	Decomposition_of_upper_sediment_humus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMnHmSM = kDMnHum * uFuTminS * afOxyedM * sDHumSM 		else		tDMnHmSM = 0.0 		endif
! 	Mineralization_of_P_in_upper_sediment_humus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMnHmSM = kDMnHum * uFuTminS * afOxyedM * sPHumSM 		else		tPMnHmSM = 0.0 		endif
! 	Mineralization_of_N_in_upper_sediment_humus
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMnHmSM = kDMnHum * uFuTminS * afOxyedM * sNHumSM 		else		tNMnHmSM = 0.0 		endif
! 	Average_diffusion_distance
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aDptDifM = fDethifS * cDepthSM 		else		aDptDifM = 0.0 		endif
! 	Diffusion_flux_of_dissolved_P_from_sediment_to_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPDifO4M = kPDifPO4 * uFunTDif * cTubDNut * bPorCrSM * (oPO4SM - oPO4WM) / aDptD&
		&ifM 		else		tPDifO4M = 0.0 		endif
! 	Diffusion_flux_of_NO3_from_sediment_to_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNDifO3M = kNDifNO3 * uFunTDif * cTubDNut * bPorCrSM * (oNO3SM - oNO3WM) / aDptD&
		&ifM 		else		tNDifO3M = 0.0 		endif
! 	Diffusion_flux_of_NH4_from_sediment_to_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNDifH4M = kNDifNH4 * uFunTDif * cTubDNut * bPorCrSM * (oNH4SM - oNH4WM) / aDptD&
		&ifM 		else		tNDifH4M = 0.0 		endif
! 	O2_diffusion(water_->_sediment)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2DifM = kO2Dif / aDptDifM * uFunTDif * cTubDfO2 * bPorCrSM * oO2WM 		else		tO2DifM = 0.0 		endif
! 	Diffusion_flux_of_dissolved_P_from_pore_water_to_groundwater
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPDroO4M = 0.0 		else		tPDroO4M = 0.0 		endif
! 	Diffusion_flux_of_NO3_from_pore_water_to_groundwater
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNDroO3M = 0.0 		else		tNDroO3M = 0.0 		endif
! 	Diffusion_flux_of_NH4_from_pore_water_to_groundwater
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNDroH4M = 0.0 		else		tNDroH4M = 0.0 		endif
! 	Max_P_adsorption_per_g_inorg_matter_in_water_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPAsMxWM = cRelPdsD + aCoO2ODM * cRePAsFe * fFeDIM + cRePAsAl * fAlDIM 		else		aPAsMxWM = 0.0 		endif
! 	P_adsorption_affinity_corrected_for_redox_conditions
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aKPAdsWM =(1.0 - fRedMx *(1.0-aCoO2ODM)) * cKPAdsOx 		else		aKPAdsWM = 0.0 		endif
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPIoAsWM = aPAsMxWM * aKPAdsWM * oPO4WM /(1.0 + aKPAdsWM * oPO4WM) 		else		aPIoAsWM = 0.0 		endif
! 	Equilibrium_conc
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPEqIMWM = aPIoAsWM * oDIMWM 		else		aPEqIMWM = 0.0 		endif
! 	Sorption_flux_in_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPSrpMWM = kPSorp *(aPEqIMWM - oPAIMWM) 		else		wPSrpMWM = 0.0 		endif
! 	Max_P_adsorption_per_g_inorg_matter_in_sediment_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPAsMxSM = cRelPdsD + afOxyedM * cRePAsFe * fFeDIM + cRePAsAl * fAlDIM 		else		aPAsMxSM = 0.0 		endif
! 	P_adsorption_affinity_corrected_for_redox_conditions
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aKPAdsSM =(1.0 - fRedMx *(1.0-afOxyedM)) * cKPAdsOx 		else		aKPAdsSM = 0.0 		endif
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPIoAsSM = aPAsMxSM * aKPAdsSM * oPO4SM /(1.0 + aKPAdsSM * oPO4SM) 		else		aPIoAsSM = 0.0 		endif
! 	Equilibrium_amount
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPEqIMSM = aPIoAsSM * sDIMSM 		else		aPEqIMSM = 0.0 		endif
! 	Sorption
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPSrpMSM = kPSorp *(aPEqIMSM - sPAIMSM) 		else		tPSrpMSM = 0.0 		endif
! 	Chemical_loss_of_dissolved_P_from_pore_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPCemO4M = max(0.0,kPChePO4 *(oPO4SM - coPO4Mx)) 		else		tPCemO4M = 0.0 		endif
! 	Root_biomass
		aDRtVeg = bfRtVeg * sDVeg
! 	Submerged_fraction_of_shoot_in_epilimnion
		bfSubegE = bfSubVeg * ( uVeHehtE / sDepthW )
! 	Submerged_fraction_of_shoot
		bfSubegH = bfSubVeg * ( uVeHehtH / sDepthW )
! 	Fraction_of_water_SURFACE_covered_by_plant_species
		afCSuVeg = min(1.0, max(aDFoaVeg / (cDLayerV + NearZero), aDEerVeg / (fEmergV * &
		&cDCarrV + NearZero) ) ) 
! 	Fraction_emergent_coverage
		afCEmVeg = min(1.0, PerCent * cCovSpV * aDEerVeg)
! 	Percent_cover
		aCovVeg = min(100.0, cCovSpV * aDShVeg)
! 	Total_plant_biomass
		aDVeg = sDVeg
! 	Total_P_in_vegetation
		aPVeg = sPVeg
! 	Total_N_in_vegetation
		aNVeg = sNVeg
! 	Light_at_the_bottom_of_Hyp
		aLPARotH = uLPRSrfH * exp(- aExtCefH * uDptWH)
! 	P/DW_ratio_of_vegetation
		rPDVeg = sPVeg / (sDVeg+NearZero)
! 	N/DW_ratio_of_vegetation
		rNDVeg = sNVeg / (sDVeg+NearZero)
! 	Migration_flux
		tDMigVeg = kMigrV * (cDVIn - sDVeg)
! 	Net_migration_flux
		tPMigVeg = kMigrV * (cPDV0* cDVIn - sPVeg)
! 	Net_migration_flux
		tNMigVeg = kMigrV * (cNDV0* cDVIn - sNVeg)
! 	Temperature_function_of_vegetation_respiration
		if ( ( UsePoerV < NearZero ) ) then
		uFumRVeg = uFunTVeg 		else		uFumRVeg = ((cQ10RspV )** (0.1 * (uTmVeAve - cTmRef))) 		endif
! 	Fraction_of_P_uptake_from_sediment
		if ( (0 == UseEmpU) ) then
		afPUVegS = 0.0 		else if ( (bfRtVeg <= NearZero) ) then		afPUVegS = 0.0 		else if ( (fFloatV + bfSubVeg <= NearZero) ) then		afPUVegS = 1.0 		else if ( (aInlSrat == 1 .and. ((oPO4S+NearZero) / (oPO4WH+NearZero)) > NearZero&
		& ) ) then		afPUVegS = fSedUVMx / (1.0 + fSeUVoef * ((((oPO4S+NearZero) / (oPO4WH+NearZero))&
		& )** fSedUExp)) 		else if ( (aInlSrat == 1 .and. ((oPO4S+NearZero) / (oPO4WH+NearZero)) <= NearZer&
		&o ) ) then		afPUVegS = fSedUVMx / (1.0 + fSeUVoef * (( NearZero )** fSedUExp)) 		else if ( (((oPO4S+NearZero) / (oPO4WE+NearZero)) <= NearZero ) ) then		afPUVegS = fSedUVMx / (1.0 + fSeUVoef * ( (NearZero )** fSedUExp)) 		else		afPUVegS = fSedUVMx / (1.0 + fSeUVoef * ((((oPO4S+NearZero) / (oPO4WE+NearZero))&
		& )** fSedUExp)) 		endif
! 	Fraction_of_N_uptake_from_sediment
		if ( (0 == UseEmpU) ) then
		afNUVegS = 0.0 		else if ( (bfRtVeg <= NearZero) ) then		afNUVegS = 0.0 		else if ( (fFloatV + bfSubVeg <= NearZero) ) then		afNUVegS = 1.0 		else if ( ( aInlSrat == 1 .and. ((oNDissS+NearZero) / (oNDissWH+NearZero)) > Nea&
		&rZero ) ) then		afNUVegS = fSedUVMx / (1.0 + fSeUVoef * ((((oNDissS+NearZero) / (oNDissWH+NearZe&
		&ro)) )** fSedUExp)) 		else if ( ( aInlSrat == 1 .and. ((oNDissS+NearZero) / (oNDissWH+NearZero)) <= Ne&
		&arZero ) ) then		afNUVegS = fSedUVMx / (1.0 + fSeUVoef * ((NearZero )** fSedUExp)) 		else if ( ( ((oNDissS+NearZero) / (oNDissWE+NearZero)) <= NearZero ) ) then		afNUVegS = fSedUVMx / (1.0 + fSeUVoef * ((NearZero )** fSedUExp)) 		else		afNUVegS = fSedUVMx / (1.0 + fSeUVoef * ((((oNDissS+NearZero) / (oNDissWE+NearZe&
		&ro)) )** fSedUExp)) 		endif
! 	Maximum_P_uptake_rate_of_vegetation_corrected_for_P/D_ratio
		if ( (0 == UseEmpU) ) then
		aVPaxVeg = max( 0.0, cVPUMxV * uFumPVeg * (cPDVMx-rPDVeg) / (cPDVMx-cPDVMn) ) 		else		aVPaxVeg = max( 0.0, cVPUMxV * uFumPVeg * (cPDVMx-rPDVeg) / (cPDVMx-cPDVMn) ) 		endif
! 	P_uptake_rate_by_submerged_AND_floating_parts
		if ( (0 == UseEmpU) ) then
		aVPUVgWH = oPO4WH * aVPaxVeg / (aVPaxVeg / cAfPUV + oPO4WH) 		else		aVPUVgWH = 0.0 		endif
! 	P_uptake_rate_by_roots
		if ( (0 == UseEmpU) ) then
		aVPUVegS = oPO4S * aVPaxVeg / (aVPaxVeg / cAfPUV + oPO4S) 		else		aVPUVegS = 0.0 		endif
! 	P_uptake_rate_by_submerged_AND_floating_parts
		if ( (0 == UseEmpU) ) then
		aVPUVgWE = oPO4WE * aVPaxVeg / (aVPaxVeg / cAfPUV + oPO4WE) 		else		aVPUVgWE = 0.0 		endif
! 	P_uptake_from_water
		if ( (0 == UseEmpU) ) then
		tPUVegWE = aVPUVgWE * (aDSubVeg + aDFoaVeg) 		else		tPUVegWE = (1.0 - afPUVegS) * aVPaxVeg * oPO4WE / (aVPaxVeg / cAfPUV + oPO4WE) *&
		& uDVegE 		endif
! 	P_uptake_from_water
		if ( (0 == UseEmpU) ) then
		tPUVegWH = aVPUVgWH * (aDSubVeg + aDFoaVeg) 		else		tPUVegWH = (1.0 - afPUVegS) * aVPaxVeg * oPO4WH / (aVPaxVeg / cAfPUV + oPO4WH) *&
		& uDVegH 		endif
! 	P_uptake_from_pore_water_(by_root_fraction)
		if ( (0 == UseEmpU) ) then
		tPUVegS = aVPUVegS * aDRtVeg 		else		tPUVegS = afPUVegS * aVPaxVeg * oPO4S / (aVPaxVeg / cAfPUV + oPO4S) * sDVeg 		endif
! 	Total_P_uptake_vegetation
		if ( (aInlSrat == 1 ) ) then
		tPUVegE = tPUVegWE + tPUVegS * uDVegE / sDVeg 		else if ( (aInlSrat == 0 .and. 0 == UseEmpU) ) then		tPUVegE = tPUVegWE + tPUVegS 		else		tPUVegE = tPUVegWE + tPUVegS 		endif
! 	Total_P_uptake_vegetation
		if ( (aInlSrat == 0) ) then
		tPUVegH = 0.0 		else if ( (0 == UseEmpU) ) then		tPUVegH = tPUVegWH + tPUVegS * uDVegH / sDVeg 		else		tPUVegH = tPUVegWH + tPUVegS * uDVegH / sDVeg 		endif
! 	Maximum_N_uptake_rate_of_vegetation_corrected_for_N/D_ratio
		if ( (0 == UseEmpU) ) then
		aVNaxVeg = max( 0.0, cVNUMxV * uFumPVeg * (cNDVMx - rNDVeg) / (cNDVMx - cNDVMn))&
		& 		else		aVNaxVeg = max( 0.0, cVNUMxV * uFumPVeg * (cNDVMx - rNDVeg) / (cNDVMx - cNDVMn))&
		& 		endif
! 	Half-sat_constant_for_N_uptake
		if ( (0 == UseEmpU) ) then
		ahNUVeg = aVNaxVeg / cAfNUV 		else		ahNUVeg = aVNaxVeg / cAfNUV 		endif
! 	N_uptake_RATE_by_submerged_AND_floating_parts
		if ( (0 == UseEmpU) ) then
		aVNUVgWE = oNDissWE * aVNaxVeg / (ahNUVeg + oNDissWE) 		else		aVNUVgWE = 0.0 		endif
! 	N_uptake_RATE_by_submerged_AND_floating_parts
		if ( (0 == UseEmpU) ) then
		aVNUVgWH = oNDissWH * aVNaxVeg / (ahNUVeg + oNDissWH) 		else		aVNUVgWH = 0.0 		endif
! 	Fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		if ( (0 == UseEmpU) ) then
		afN4UgWE = oNH4WE * oNO3WE / ((ahNUVeg + oNH4WE) * (ahNUVeg + oNO3WE + NearZero)&
		&) + oNH4WE * ahNUVeg / ((oNH4WE + oNO3WE + NearZero) * (ahNUVeg + oNO3WE + NearZ&
		&ero)) 		else		afN4UgWE = oNH4WE * oNO3WE / ((ahNUVeg + oNH4WE) * (ahNUVeg + oNO3WE + NearZero)&
		&) + oNH4WE * ahNUVeg / ((oNH4WE + oNO3WE + NearZero) * (ahNUVeg + oNO3WE + NearZ&
		&ero)) 		endif
! 	Fraction_ammonium_uptake_from_water_column_(from_WASP_model_EPA)
		if ( (0 == UseEmpU) ) then
		afN4UgWH = oNH4WH * oNO3WH / ((ahNUVeg + oNH4WH) * (ahNUVeg + oNO3WH + NearZero)&
		&) + oNH4WH * ahNUVeg / ((oNH4WH + oNO3WH + NearZero) * (ahNUVeg + oNO3WH + NearZ&
		&ero)) 		else		afN4UgWH = oNH4WH * oNO3WH / ((ahNUVeg + oNH4WH) * (ahNUVeg + oNO3WH + NearZero)&
		&) + oNH4WH * ahNUVeg / ((oNH4WH + oNO3WH + NearZero) * (ahNUVeg + oNO3WH + NearZ&
		&ero)) 		endif
! 	N_uptake_from_water_(by_shoots)
		if ( (0 == UseEmpU) ) then
		tNUVegWE = aVNUVgWE * (aDSubVeg + aDFoaVeg) 		else		tNUVegWE = (1.0 - afNUVegS) * aVNaxVeg * oNDissWE / (aVNaxVeg / cAfNUV + oNDissW&
		&E) * uDVegE 		endif
! 	N_uptake_from_water_(by_shoots)
		if ( (0 == aInlSrat) ) then
		tNUVegWH = 0.0 		else if ( (0 == UseEmpU) ) then		tNUVegWH = aVNUVgWH * (aDSubVeg + aDFoaVeg) 		else		tNUVegWH = (1.0 - afNUVegS) * aVNaxVeg * oNDissWH / (aVNaxVeg / cAfNUV + oNDissW&
		&H) * uDVegH 		endif
! 	NH4_uptake_of_vegetation_from_water
		if ( (0 == UseEmpU) ) then
		tNUH4gWE = afN4UgWE * tNUVegWE 		else		tNUH4gWE = afN4UgWE * tNUVegWE 		endif
! 	NO3_uptake_of_vegetation_from_water
		if ( (0 == UseEmpU) ) then
		tNUO3gWE = tNUVegWE - tNUH4gWE 		else		tNUO3gWE = tNUVegWE - tNUH4gWE 		endif
! 	NH4_uptake_of_vegetation_from_water
		if ( (0 == UseEmpU) ) then
		tNUH4gWH = afN4UgWH * tNUVegWH 		else		tNUH4gWH = afN4UgWH * tNUVegWH 		endif
! 	NO3_uptake_of_vegetation_from_water
		if ( (0 == UseEmpU) ) then
		tNUO3gWH = tNUVegWH - tNUH4gWH 		else		tNUO3gWH = tNUVegWH - tNUH4gWH 		endif
! 	N_uptake_RATE_of_roots
		if ( (0 == UseEmpU) ) then
		aVNUVegS = oNDissS * aVNaxVeg / (ahNUVeg + oNDissS) 		else		aVNUVegS = 0.0 		endif
! 	N_uptake_from_pore_water_(by_roots)
		if ( (0 == UseEmpU) ) then
		tNUVegS = aVNUVegS * aDRtVeg 		else		tNUVegS = afNUVegS * aVNaxVeg * oNDissS / (aVNaxVeg / cAfNUV + oNDissS) * sDVeg &
		&		endif
! 	Fraction_ammonium_uptake_from_pore_water_(from_WASP_model_EPA)
		if ( (0 == UseEmpU) ) then
		afN4UegS = oNH4S * oNO3S / ((ahNUVeg + oNH4S +NearZero) * (ahNUVeg + oNO3S +Near&
		&Zero)) + oNH4S * ahNUVeg / ((oNH4S + oNO3S+NearZero) * (ahNUVeg + oNO3S+NearZero&
		&)) 		else		afN4UegS = oNH4S * oNO3S / ((ahNUVeg + oNH4S +NearZero) * (ahNUVeg + oNO3S +Near&
		&Zero)) + oNH4S * ahNUVeg / ((oNH4S + oNO3S+NearZero) * (ahNUVeg + oNO3S+NearZero&
		&)) 		endif
! 	NH4_uptake_of_vegetation_from_sediment
		if ( (0 == UseEmpU) ) then
		tNUH4egS = afN4UegS * tNUVegS 		else		tNUH4egS = afN4UegS * tNUVegS 		endif
! 	N_uptake_from_pore_water_(by_roots)
		if ( (0 == UseEmpU) ) then
		tNUVegSE = aVNUVegS * aDRtVeg 		else		tNUVegSE = afNUVegS * aVNaxVeg * oNDissS / (aVNaxVeg / cAfNUV + oNDissS) * sDVeg&
		& 		endif
! 	NO3_uptake_of_vegetation_from_sediment
		if ( (0 == UseEmpU) ) then
		tNUO3egS = tNUVegS - tNUH4egS 		else		tNUO3egS = tNUVegS - tNUH4egS 		endif
! 	Total_N_uptake_vegetation
		tNUVeg =tNUVegWE + tNUVegWH + tNUVegS
! 	Light_function_of_growth_based_on_shoot_fraction
		aLLmSegE = fEmergV + fFloatV * (1.0 - afCEmVeg) + bfSubegE * (1.0 - afCSuVeg) * &
		&1.0 / (aExtCefE * uDptWE) * log( (1.0 + aLPR1egE / uhLVeg) / (1.0 + aLPR2egE / u&
		&hLVeg))
! 	Light_function_of_growth_based_on_shoot_fraction
		aLLmSegH = fEmergV + fFloatV * (1.0 - afCEmVeg) + bfSubegH * (1.0 - afCSuVeg) * &
		&1.0 / (aExtCefH * uDptWH) * log( (1.0 + aLPR1egH / uhLVeg) / (1.0 + aLPR2egH / u&
		&hLVeg))
! 	Max_growth_rate_at_current_temp_AND_light
		aMumLegH = ufDay * bfShVeg * aLLmSegH * uFumPVeg * cMuMxV
! 	Droop_function_(P)_for_vegetation
		aPLimVeg = max(0.0, (1.0 - cPDVMn / rPDVeg) * cPDVMx / (cPDVMx - cPDVMn) )
! 	Droop_function_(N)_for_vegetation
		aNLimVeg = max(0.0, (1.0 - cNDVMn / rNDVeg) * cNDVMx / (cNDVMx - cNDVMn) )
! 	Nutrient_limitation_function_of_vegetation
		aNuLiVeg = min( aPLimVeg, aNLimVeg)
! 	Actual_growth_rate_of_vegetation
		aMuVegH = aMumLegH * aNuLiVeg
! 	Mortality_constant
		if ( (0 == IncSeson) ) then
		bkMVeg = kMVSum 		else if ( (aVWin == 1 .and. aDaysWiV <= cLengM .and. aDaysWiV > 0) ) then		bkMVeg = - log(fWinV) / cLengM 		else		bkMVeg = kMVSum 		endif
! 	Intrinsic_net_increase_rate_of_vegetation
		akDncegH = aMumLegH - kDRespV * uFumRVeg - bkMVeg
! 	Logistic_correction_of_vegetation
		tDEnvegH = max(0.0, akDncegH / (cDCarrV+NearZero) * sDVeg*sDVeg)
! 	Logistic_correction_of_production
		tDEPregH = aMuVegH / cMuMxV * tDEnvegH
! 	Vegetation_production
		if ( (aInlSrat == 0) ) then
		tDPodegH = 0.0 		else		tDPodegH = max(0.0, aMuVegH * uDVegH - tDEPregH) 		endif
! 	Submerged_production
		tDPdSegH = bfSubegH * tDPodegH
! 	Dark_respiration_of_vegetation
		tDResVeg = kDRespV * uFumRVeg * sDVeg
! 	Logistic_correction_of_mortality
		tDEvMegH = tDEnvegH - tDEPregH
! 	Total_mortality_flux_DW_vegetation
		if ( (aInlSrat == 0) ) then
		tDMVegH = 0.0 		else		tDMVegH = bkMVeg * uDVegH + tDEvMegH 		endif
! 	Mortality_flux_becoming_water_detritus
		tDMVegWH = fDeWMV * (1.0 - bfRtVeg) * tDMVegH
! 	Biomass_loss_due_to_grazing_of_birds
		if ( ( (sTime >= cYetards * DayPeear) .and. (Day >= cDaarrds) .and. (Day <= cDan&
		&drds) ) ) then
		tDGazgBi = cPrfVird * sDVeg / (hDVBird + sDVeg) * cBidsrha / m2Perha * cDGzPird &
		&		else		tDGazgBi = 0.0 		endif
! 	Rate_constant_during_mowing_period
		if ( ((Day >= cDayMnV1 .and. Day < cDayMnV1 + cLengMan) .or. (Day >= cDayMnV2 .a&
		&nd. Day < cDayMnV2 + cLengMan)) ) then
		bkManVeg = -log(1.0 - fManV) / cLengMan 		else		bkManVeg = 0.0 		endif
! 	Mowing_of_vegetation_DW
		tDManVeg = bkManVeg * sDVeg
! 	Mowing_of_vegetation_P
		tPManVeg = rPDVeg * tDManVeg
! 	Mowing_of_vegetation_N
		tNManVeg = rNDVeg * tDManVeg
! 	Vegetation_O2_production
		tO2roegH = molO2olC * cCPerDW * tDPodegH
! 	Submerged_O2_respiration
		tO2spgWH = molO2olC * cCPerDW * bfSubegH * tDResVeg * aCoO2ODH
! 	Oxygen_penetration_depth
		if ((aInlSrat == 1 .and. (2.0 * oO2WH * akODiCor / tSOD) > NearZero ) ) then
		aDpOxSed = (((2.0 * oO2WH * akODiCor / tSOD) )** (0.5)) 		else if ( ( aInlSrat == 1 .and. (2.0 * oO2WH * akODiCor / tSOD) <= NearZero ) ) &
		&then		aDpOxSed = ( ( NearZero )** (0.5)) 		else if ( ( aInlSrat == 1 .and. ( 2.0 * oO2WE * akODiCor / tSOD ) <= NearZero ) &
		&) then		aDpOxSed = ( ( NearZero )** (0.5)) 		else		aDpOxSed = ( ( ( 2.0 * oO2WE * akODiCor / tSOD ) )** (0.5)) 		endif
! 	Fraction_aerobic_sediment
		afOxySed = aDpOxSed / cDepthS
! 	Root_O2_respiration
		tO2esegS = molO2olC * cCPerDW * bfRtVeg * tDResVeg * afOxySed
! 	O2_transport_to_roots
		tO2odgSH = min (tO2esegS, tO2roegH)
! 	O2_used_for_vegetation_production
		tO2odgWH = min( tO2roegH - tO2odgSH, bfSubVeg * tO2roegH)
! 	O2_production_to_water_due_to_NO3_uptake_by_macrophytes
		tO2O3gWH = O2PerNO3 * molO2olN * bfSubVeg * tNUO3gWH
! 	O2_production_due_to_NO3_uptake_from_sed_by_macrophytes
		tO2NOegS = O2PerNO3 * molO2olN * tNUO3egS
! 	P_excretion_by_vegetation
		tPExcVeg = (rPDVeg *2.0)/ (cPDVMx + rPDVeg) * rPDVeg * tDResVeg
! 	P_excretion_by_vegetation_in_sediment
		tPEcregS = bfRtVeg * tPExcVeg
! 	P_excretion_by_vegetation_in_water
		tPEcregW = tPExcVeg - tPEcregS
! 	P_mortality_flux_of_vegetation
		tPMVegH = rPDVeg * tDMVegH
! 	Mortality_flux_of_vegetation_becoming_dissolved_P
		tPMegO4H = fDissMV * tPMVegH
! 	Mortality_flux_of_vegetation_becoming_dissolved_P_in_sediment
		tPMeg4SH = bfRtVeg * tPMegO4H
! 	Mortality_flux_of_vegetation_becoming_dissolved_P_in_water
		tPMeg4WH = tPMegO4H - tPMeg4SH
! 	Mortality_flux_of_vegetation_becoming_detritus_P
		tPMVeDtH = tPMVegH - tPMegO4H
! 	Mortality_flux_of_vegetation_becoming_detritus_P_in_water
		tPMegtWH = fDeWMV * (1.0 - bfRtVeg) * tPMVeDtH
! 	Mortality_flux_of_vegetation_becoming_detritus_P_in_sediment
		tPMegtSH = tPMVeDtH - tPMegtWH
! 	P_mortality_flux_of_vegetation_by_bird_grazing
		tPGazgBi = rPDVeg * tDGazgBi
! 	N_excretion_by_vegetation
		tNExcVeg = (2.0 * rNDVeg) / (cNDVMx + rNDVeg) * rNDVeg * tDResVeg
! 	N_excretion_by_vegetation_to_sediment
		tNEcregS = bfRtVeg * tNExcVeg
! 	N_excretion_by_vegetation_to_water
		tNEcregW = tNExcVeg - tNEcregS
! 	N_mortality_flux_of_vegetation
		tNMVegH = rNDVeg * tDMVegH
! 	Mortality_flux_of_vegetation_becoming_dissolved_N
		tNMegH4H = fDissMV * tNMVegH
! 	Mortality_flux_of_vegetation_becoming_dissolved_N_in_sediment
		if ( (aInlSrat == 1 ) ) then
		tNMeg4SH = bfRtVeg * tNMegH4H 		else		tNMeg4SH = 0.0 		endif
! 	Mortality_flux_of_vegetation_becoming_dissolved_N_in_water
		tNMeg4WH = tNMegH4H - tNMeg4SH
! 	Mortality_flux_of_vegetation_becoming_detritus_N
		tNMVeDtH = tNMVegH - tNMegH4H
! 	Mortality_flux_of_vegetation_becoming_detritus_N_in_water
		tNMegtWH = fDeWMV * (1.0 - bfRtVeg) * tNMVeDtH
! 	Mortality_flux_of_vegetation_becoming_detritus_N_in_sediment
		tNMegtSH = tNMVeDtH - tNMegtWH
! 	N_mortality_flux_of_vegetation_by_bird_grazing
		tNGazgBi = rNDVeg * tDGazgBi
! 	DW_assimilation_by_herbivorous_birds
		tDAsVgBi = fDAssird * tDGazgBi
! 	DW_egestion_by_herbivorous_birds
		tDEBi = tDGazgBi - tDAsVgBi
! 	P_assimilation_by_herbivorous_birds
		tPAsVgBi = fDAssird * tPGazgBi
! 	P_egestion_by_herbivorous_birds
		tPEBi = tPGazgBi - tPAsVgBi
! 	PO4_egestion_by_herbivorous_birds
		tPEBiPO4 = fDiEgird * tPEBi
! 	P_detritus_egestion_by_herbivorous_birds
		tPEBiDt = tPEBi - tPEBiPO4
! 	N_assimilation_by_herbivorous_birds
		tNAsVgBi = fDAssird * tNGazgBi
! 	N_egestion_by_herbivorous_birds
		tNEBi = tNGazgBi - tNAsVgBi
! 	NH4_egestion_by_herbivorous_birds
		tNEBiNH4 = fDiEgird * tNEBi
! 	N_detritus_egestion_by_herbivorous_birds
		tNEBiDt = tNEBi - tNEBiNH4
! 	Total_DW_flux_from_Vegetation_module_to_water_detritus
		if ( (aInlSrat == 1 .and. InclV == 1) ) then
		wDBedtWH = (tDMVegWH) / uDptWH 		else		wDBedtWH = 0.0 		endif
! 	Max_growth_rate_at_current_temp_AND_light
		aMumLegE = ufDay * bfShVeg * aLLmSegE * uFumPVeg * cMuMxV
! 	Intrinsic_net_increase_rate_of_vegetation
		akDncegE = aMumLegE - kDRespV * uFumRVeg - bkMVeg
! 	Logistic_correction_of_vegetation
		tDEnvegE = max(0.0, akDncegE / (cDCarrV+NearZero) * sDVeg*sDVeg)
! 	Actual_growth_rate_of_vegetation
		aMuVegE = aMumLegE * aNuLiVeg
! 	Logistic_correction_of_production
		tDEPregE = aMuVegE / cMuMxV * tDEnvegE
! 	Vegetation_production
		tDPodegE = max(0.0, aMuVegE * uDVegE - tDEPregE)
! 	Logistic_correction_of_mortality
		tDEvMegE = tDEnvegE - tDEPregE
! 	Total_mortality_flux_DW_vegetation
		tDMVegE = bkMVeg * uDVegE + tDEvMegE
! 	N_mortality_flux_of_vegetation
		tNMVegE = rNDVeg * tDMVegE
! 	Mortality_flux_of_vegetation_becoming_dissolved_N
		tNMegH4E = fDissMV * tNMVegE
! 	Mortality_flux_of_vegetation_becoming_dissolved_N_in_sediment
		tNMeg4SE = bfRtVeg * tNMegH4E
! 	Total_P_flux_from_Vegetation_module_to_PO4_in_water
		if ( (aInlSrat == 1 .and. InclV == 1 ) ) then
		wPBdP4WH = (- tPUVegWH + tPEcregW * uVeHehtH / uVeHeght + tPMeg4WH ) /uDptWH 		else		wPBdP4WH = 0.0 		endif
! 	Total_P_flux_from_Vegetation_module_to_water_detritus
		if ( (aInlSrat == 1 .and. InclV == 1) ) then
		wPBedtWH = (tPMegtWH ) / uDptWH 		else		wPBedtWH = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NH4_in_water
		if ( (aInlSrat == 1 .and. InclV == 1 ) ) then
		wNBdN4WH = (- tNUH4gWH + tNEcregW * (uVeHehtH /uVeHeght ) + tNMeg4WH ) / uDptWH &
		&		else		wNBdN4WH = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NO3_in_water
		if ( (aInlSrat == 1 .and. InclV == 1) ) then
		wNBdN3WH = - tNUO3gWH / uDptWH 		else		wNBdN3WH = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_water_detritus
		if ( (aInlSrat == 1 .and. InclV == 1) ) then
		wNBedtWH = (tNMegtWH ) / uDptWH 		else		wNBedtWH = 0.0 		endif
! 	Mortality_flux_becoming_sediment_detritus
		tDMVegSH = tDMVegH - tDMVegWH
! 	Total_N_flux_from_Vegetation_module_to_NH4_in_pore_water
		if ( (aInlSrat == 1 .and. InclV == 1 ) ) then
		tDBedtSH = tDMVegSH 		else		tDBedtSH = 0.0 		endif
! 	Total_water_O2_flux_in_vegetation_module
		if ( (aInlSrat == 1 .and. InclV == 1 ) ) then
		tO2BedWH = tO2odgWH - tO2spgWH + tO2O3gWH 		else		tO2BedWH = 0.0 		endif
! 	If_T_Use_Loss_function
		if ( (InclWeb == 1) ) then
		UseLoss = 0.0 		else		UseLoss = 1.0 		endif
! 	Temp_function_of_grazing
		uFuTmssH = exp(-0.5/(cSiTmoss*cSiTmoss) *((uTmH-cTmptoss)*(uTmH-cTmptoss) -(cTmR&
		&ef-cTmptoss)*(cTmRef-cTmptoss)))
! 	Temp_function_of_grazing
		uFuTmssE = exp(-0.5/(cSiTmoss*cSiTmoss) *((uTmE-cTmptoss)*(uTmE-cTmptoss) -(cTmR&
		&ef-cTmptoss)*(cTmRef-cTmptoss)))
! 	P/D_ratio_of_Algae
		rPDBleWH = oPBlueWH /(oDBlueWH+NearZero)
! 	N/D_ratio_of_Algae
		rNDBleWH = oNBlueWH /(oDBlueWH+NearZero)
! 	P/D_ratio_of_Algae
		rPDBlueS = sPBlueS /(sDBlueS+NearZero)
! 	N/D_ratio_of_Algae
		rNDBlueS = sNBlueS /(sDBlueS+NearZero)
! 	Temperature_function_of_Algae
		uFuTmueH = exp(-0.5/(cSiTmlue*cSiTmlue) *((uTmH-cTmptlue)*(uTmH-cTmptlue) - (cTm&
		&Ref-cTmptlue)*(cTmRef-cTmptlue)))
! 	Temperature_function_of_Algae
		uFuPrueH = uFuTmueH
! 	Temperature_function_of_Algae
		uFuReueH = uFuTmueH
! 	Temperature_function_of_Algae
		uFuTmueE = exp(-0.5/(cSiTmlue*cSiTmlue) *((uTmE-cTmptlue)*(uTmE-cTmptlue) - (cTm&
		&Ref-cTmptlue)*(cTmRef-cTmptlue)))
! 	Temperature_function_of_Algae
		uFuPrueE = uFuTmueE
! 	Temperature_function_of_Algae
		uFuReueE = uFuTmueE
! 	Temperature_function_of_Algae
		uFuTmueS = exp(-0.5/(cSiTmlue*cSiTmlue) *((uTmBot-cTmptlue)*(uTmBot-cTmptlue) - &
		&(cTmRef-cTmptlue)*(cTmRef-cTmptlue)))
! 	Temperature_function_of_Algae
		uFuReueS = uFuTmueS
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPxCueH = max(0.0,cVPMxlue * uFuPrueH *(cPDBleMx - rPDBleWH) /(cPDBleMx - cPDBl&
		&eMn))
! 	P_uptake_rate_of_Algae
		aVPUBueH = oPO4WH * aVPxCueH /(aVPxCueH / cAfPUlue + oPO4WH)
! 	P_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wPUBlueH = 0.0 		else		wPUBlueH = aVPUBueH * oDBlueWH 		endif
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNxCueH = max(0.0,cVNMxlue * uFuPrueH * (cNDBleMx - rNDBleWH) /(cNDBleMx - cNDB&
		&leMn))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUBueH = aVNxCueH / cAfNUlue
! 	N_uptake_rate_of_Algae
		aVNUBueH = oNDissWH * aVNxCueH /(ahNUBueH + oNDissWH)
! 	N_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wNUBlueH = 0.0 		else		wNUBlueH = aVNUBueH * oDBlueWH 		endif
! 	Fraction_ammonium_uptake_by_Algae
		afN4UueH = oNH4WH * oNO3WH /((ahNUBueH + oNH4WH) *(ahNUBueH + oNO3WH)) + oNH4WH &
		&* ahNUBueH /((oNH4WH + oNO3WH) *(ahNUBueH + oNO3WH))
! 	Ammonium_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUH4ueH = 0.0 		else		wNUH4ueH = afN4UueH * wNUBlueH 		endif
! 	Nitrate_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUO3ueH = 0.0 		else		wNUO3ueH = wNUBlueH - wNUH4ueH 		endif
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuxTueH = cMuMxlue * uFuPrueH
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuxTueE = cMuMxlue * uFuPrueE
! 	Droop_function(P)_for_Algae
		aPLmBueH = max(0.0,(1.0 - cPDBleMn / rPDBleWH) * cPDBleMx /(cPDBleMx - cPDBleMn)&
		&)
! 	Droop_function(N)_for_Algae
		aNLmBueH = max(0.0,(1.0 - cNDBleMn / rNDBleWH) * cNDBleMx /(cNDBleMx - cNDBleMn)&
		&)
! 	Silica_dependence_of_growth_rate
		aSiimueH = oSiO2WH /(hSisslue + oSiO2WH)
! 	Light_function
		aLLmBueH = Useeelue *(exp(1.0) /(aExtCefH * uDptWH) *(exp(- aLPARotH /(cLORelue &
		&* uFuPrueH)) - exp(- uLPRSrfH /(cLORelue * uFuPrueH)))) +(1.0 - Useeelue) *(1.0 &
		&/(aExtCefH * uDptWH) * log((1.0 + uLPRSrfH / (hLReflue * uFuPrueH)) / (1.0 + aLP&
		&ARotH /(hLReflue * uFuPrueH))))
! 	Growth_rate_at_current_light_AND_temp
		aMumLueH = ufDay *(1.0 - afCSuVeg) * aLLmBueH * uMuxTueH
! 	Nutrient_limitation_function_of_Algae
		aNuimueH = min(aPLmBueH,(min(aNLmBueH,aSiimueH)))
! 	Growth_rate
		aMuBlueH = aNuimueH * aMumLueH
! 	Assimilation_Algae
		if ( (aInlSrat == 0) ) then
		wDAsBueH = 0.0 		else		wDAsBueH = aMuBlueH*oDBlueWH 		endif
! 	Chlorophyll-a/DW_ratio_Algae
		rChDBueH = cChBleMx -(cChBleMx - cChBleMn) * aLLmBueH
! 	Chlorophyll-a_conc
		oChlBlH = mgPerg * rChDBueH * oDBlueWH
! 	Specific_extinction_per_unit_chlorophyll-a
		aExChueH = cExSplue / rChDBueH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTueH = kDRsplue * uFuReueH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTueE = kDRsplue * uFuReueE
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTueS = kDRsplue * uFuReueS
! 	Respiration_of_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wDRpBeWH = 0.0 		else		wDRpBeWH = ukDpTueH * oDBlueWH 		endif
! 	Daily_grazing_on_Algae
		ukLsTueH = UseLoss * kLosslue * uFuTmssH
! 	Daily_grazing_on_Algae
		ukLsTueE = UseLoss * kLosslue * uFuTmssE
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wDLssueH = 0.0 		else		wDLssueH = ukLsTueH * oDBlueWH 		endif
! 	Mortality_in_water
		if ( (aInlSrat == 0) ) then
		wDMBleWH = 0.0 		else		wDMBleWH = kMBlueW * oDBlueWH 		endif
! 	Corrected_sedimentation_velocity_of_Algae
		uCoSeueH = cVSetlue * aFuauOMH * uFuTmetH
! 	Sedimentation_flux_of_Algae
		if ( (aInlSrat == 0) ) then
		tDStBueH = 0.0 		else		tDStBueH = uCoSeueH * oDBlueWH 		endif
! 	Resuspension_DW_blue-greens
		tDRsulue = sDBlueS /(aDPhytS+NearZero) * tDRsPTot
! 	Respiration_of_sediment_Algae
		tDRspueS = ukDpTueS * sDBlueS
! 	Mortality_in_sed
		tDMBlueS = kMBlueS * sDBlueS
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDecueH = ukDpTueH + ukLsTueH + kMBlueW +(uCoSeueH * uFuTmetH) / uDptWH
! 	P_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPErBeWH = 0.0 		else		wPErBeWH = (rPDBleWH * 2.0 )/(cPDBleMx + rPDBleWH) * rPDBleWH * wDRpBeWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wPLssueH = 0.0 		else		wPLssueH = rPDBleWH * wDLssueH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPMBleWH = 0.0 		else		wPMBleWH = kMBlueW * oPBlueWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tPStBueH = 0.0 		else		tPStBueH = rPDBleWH * tDStBueH 		endif
! 	Resuspension_of_algae
		tPRsulue = rPDBlueS * tDRsulue
! 	P_excretion_of_algae_in_sediment
		tPEcrueS = (rPDBlueS *2.0)/(cPDBleMx + rPDBlueS) * rPDBlueS * tDRspueS
! 	P_mortality_of_algae_in_sediment
		tPMBlueS = kMBlueS * sPBlueS
! 	N_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNErBeWH = 0.0 		else		wNErBeWH = (rNDBleWH *2.0)/(cNDBleMx + rNDBleWH) * rNDBleWH * wDRpBeWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wNLssueH = 0.0 		else		wNLssueH = rNDBleWH * wDLssueH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNMBleWH = 0.0 		else		wNMBleWH = kMBlueW * oNBlueWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tNStBueH = 0.0 		else		tNStBueH = rNDBleWH * tDStBueH 		endif
! 	Resuspension_of_algae
		tNRsulue = rNDBlueS * tDRsulue
! 	N_excretion_of_algae_in_sediment
		tNEcrueS = (2.0 * rNDBlueS) /(cNDBleMx + rNDBlueS) * rNDBlueS * tDRspueS
! 	N_mortality_of_algae_in_sediment
		tNMBlueS = kMBlueS * sNBlueS
! 	Correction_factor_for_IM_settling_rate_(<=_1)
		aFuauOME = min(1.0 / ((aFuDiusp +NearZero )** (0.5)), 1.0)
! 	Correction_factor_for_OM_settling_rate_(<=_1)
		aFuauIME = aFuauOME
! 	Corrected_sedimentation_velocity_of_Algae
		uCoSeueE = cVSetlue * aFuauOME * uFuTmetE
! 	Sedimentation_flux_of_Algae
		tDStBueE = uCoSeueE * oDBlueWE
! 	N/D_ratio_of_Algae
		rNDBleWE = oNBlueWE /(oDBlueWE+NearZero)
! 	Sedimentation
		tNStBueE = rNDBleWE * tDStBueE
! 	P/D_ratio_of_Algae
		rPDBleWE = oPBlueWE /(oDBlueWE+NearZero)
! 	Sedimentation
		tPStBueE = rPDBleWE * tDStBueE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 0) ) then
		wDPmBeWH = 0.0 		else		wDPmBeWH = wDAsBueH - wDRpBeWH - wDLssueH - wDMBleWH -(tDStBueH - tDStBueE - tDR&
		&sulue) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wPPmBeWH = 0.0 		else		wPPmBeWH = wPUBlueH - wPErBeWH - wPLssueH - wPMBleWH -(tPStBueH - tPStBueE - tPR&
		&sulue) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wNPmBeWH = 0.0 		else		wNPmBeWH = wNUBlueH - wNErBeWH - wNLssueH - wNMBleWH -(tNStBueH - tNStBueE - tNR&
		&sulue) / uDptWH 		endif
! 	P/D_ratio_of_Algae
		rPDGrnWH = oPGrenWH /(oDGrenWH+NearZero)
! 	N/D_ratio_of_Algae
		rNDGrnWH = oNGrenWH /(oDGrenWH+NearZero)
! 	P/D_ratio_of_Algae
		rPDGrenS = sPGrenS /(sDGrenS+NearZero)
! 	N/D_ratio_of_Algae
		rNDGrenS = sNGrenS /(sDGrenS+NearZero)
! 	Temperature_function_of_Algae
		uFuTmenH = exp(-0.5/(cSiTmren*cSiTmren) *((uTmH-cTmptren)*(uTmH-cTmptren) - (cTm&
		&Ref-cTmptren)*(cTmRef-cTmptren)))
! 	Temperature_function_of_Algae
		uFuTmenE = exp(-0.5/(cSiTmren*cSiTmren) *((uTmE-cTmptren)*(uTmE-cTmptren) - (cTm&
		&Ref-cTmptren)*(cTmRef-cTmptren)))
! 	Temperature_function_of_Algae
		uFuTmenS = exp(-0.5/(cSiTmren*cSiTmren) *((uTmBot-cTmptren)*(uTmBot-cTmptren) - &
		&(cTmRef-cTmptren)*(cTmRef-cTmptren)))
! 	Temperature_function_of_Algae
		uFuPrenH = uFuTmenH
! 	Temperature_function_of_Algae
		uFuReenH = uFuTmenH
! 	Temperature_function_of_Algae
		uFuPrenE = uFuTmenE
! 	Temperature_function_of_Algae
		uFuReenE = uFuTmenE
! 	Temperature_function_of_Algae
		uFuReenS = uFuTmenS
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPxCenH = max(0.0,cVPMxren * uFuPrenH *(cPDGrnMx - rPDGrnWH) /(cPDGrnMx - cPDGr&
		&nMn))
! 	P_uptake_rate_of_Algae
		aVPUGenH = oPO4WH * aVPxCenH /(aVPxCenH / cAfPUren + oPO4WH)
! 	P_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wPUGrenH = 0.0 		else		wPUGrenH = aVPUGenH * oDGrenWH 		endif
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNxCenH = max(0.0,cVNMxren * uFuPrenH * (cNDGrnMx - rNDGrnWH) /(cNDGrnMx - cNDG&
		&rnMn))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUGenH = aVNxCenH / cAfNUren
! 	N_uptake_rate_of_Algae
		aVNUGenH = oNDissWH * aVNxCenH /(ahNUGenH + oNDissWH)
! 	N_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wNUGrenH = 0.0 		else		wNUGrenH = aVNUGenH * oDGrenWH 		endif
! 	Fraction_ammonium_uptake_by_Algae
		afN4UenH = oNH4WH * oNO3WH /((ahNUGenH + oNH4WH) *(ahNUGenH + oNO3WH)) + oNH4WH &
		&* ahNUGenH /((oNH4WH + oNO3WH) *(ahNUGenH + oNO3WH))
! 	Ammonium_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUH4enH = 0.0 		else		wNUH4enH = afN4UenH * wNUGrenH 		endif
! 	Nitrate_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUO3enH = 0.0 		else		wNUO3enH = wNUGrenH - wNUH4enH 		endif
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuxTenH = cMuMxren * uFuPrenH
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuxTenE = cMuMxren * uFuPrenE
! 	Droop_function(P)_for_Algae
		aPLmGenH = max(0.0,(1.0 - cPDGrnMn / rPDGrnWH) * cPDGrnMx /(cPDGrnMx - cPDGrnMn)&
		&)
! 	Droop_function(N)_for_Algae
		aNLmGenH = max(0.0,(1.0 - cNDGrnMn / rNDGrnWH) * cNDGrnMx /(cNDGrnMx - cNDGrnMn)&
		&)
! 	Silica_dependence_of_growth_rate
		aSiimenH = oSiO2WH /(hSissren + oSiO2WH)
! 	Light_function
		aLLmGenH = Useeeren *(exp(1.0) /(aExtCefH * uDptWH) *(exp(- aLPARotH /(cLOReren &
		&* uFuPrenH)) - exp(- uLPRSrfH /(cLOReren * uFuPrenH)))) +(1.0 - Useeeren) *(1.0 &
		&/(aExtCefH * uDptWH) * log((1.0 + uLPRSrfH / (hLRefren * uFuPrenH)) / (1.0 + aLP&
		&ARotH /(hLRefren * uFuPrenH))))
! 	Growth_rate_at_current_light_AND_temp
		aMumLenH = ufDay *(1.0 - afCSuVeg) * aLLmGenH * uMuxTenH
! 	Nutrient_limitation_function_of_Algae
		aNuimenH = min(aPLmGenH,(min(aNLmGenH,aSiimenH)))
! 	Growth_rate
		aMuGrenH = aNuimenH * aMumLenH
! 	Assimilation_Algae
		if ( (aInlSrat == 0) ) then
		wDAsGenH = 0.0 		else		wDAsGenH = aMuGrenH*oDGrenWH 		endif
! 	Chlorophyll-a/DW_ratio_Algae
		rChDGenH = cChGrnMx -(cChGrnMx - cChGrnMn) * aLLmGenH
! 	Chlorophyll-a_conc
		oChlGrH = mgPerg * rChDGenH * oDGrenWH
! 	Specific_extinction_per_unit_chlorophyll-a
		aExChenH = cExSpren / rChDGenH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTenH = kDRspren * uFuReenH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTenE = kDRspren * uFuReenE
! 	Temp_corrected_respiration_constant_of_Algae
		ukDpTenS = kDRspren * uFuReenS
! 	Respiration_of_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wDRpGnWH = 0.0 		else		wDRpGnWH = ukDpTenH * oDGrenWH 		endif
! 	Daily_grazing_on_Algae
		ukLsTenH = UseLoss * kLossren * uFuTmssH
! 	Daily_grazing_on_Algae
		ukLsTenE = UseLoss * kLossren * uFuTmssE
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wDLssenH = 0.0 		else		wDLssenH = ukLsTenH * oDGrenWH 		endif
! 	Mortality_in_water
		if ( (aInlSrat == 0) ) then
		wDMGrnWH = 0.0 		else		wDMGrnWH = kMGrenW * oDGrenWH 		endif
! 	Corrected_sedimentation_velocity_of_Algae
		uCoSeenH = cVSetren * aFuauOMH * uFuTmetH
! 	Sedimentation_flux_of_Algae
		if ( (aInlSrat == 0) ) then
		tDStGenH = 0.0 		else		tDStGenH = uCoSeenH * oDGrenWH 		endif
! 	Resuspension_of_Algae
		tDRsuren = sDGrenS /(aDPhytS+NearZero) * tDRsPTot
! 	Respiration_of_sediment_Algae
		tDRspenS = ukDpTenS * sDGrenS
! 	Mortality_in_sed
		tDMGrenS = kMGrenS * sDGrenS
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDecenH = ukDpTenH + ukLsTenH + kMGrenW +(uCoSeenH * uFuTmetH) / uDptWH
! 	P_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPErGnWH = 0.0 		else		wPErGnWH = (2.0 *rPDGrnWH) /(cPDGrnMx + rPDGrnWH) * rPDGrnWH * wDRpGnWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wPLssenH = 0.0 		else		wPLssenH = rPDGrnWH * wDLssenH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPMGrnWH = 0.0 		else		wPMGrnWH = kMGrenW * oPGrenWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tPStGenH = 0.0 		else		tPStGenH = rPDGrnWH * tDStGenH 		endif
! 	Resuspension_of_algae
		tPRsuren = rPDGrenS * tDRsuren
! 	P_excretion_of_algae_in_sediment
		tPEcrenS = (2.0 * rPDGrenS) /(cPDGrnMx + rPDGrenS) * rPDGrenS * tDRspenS
! 	P_mortality_of_algae_in_sediment
		tPMGrenS = kMGrenS * sPGrenS
! 	N_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNErGnWH = 0.0 		else		wNErGnWH = (2.0 * rNDGrnWH) /(cNDGrnMx + rNDGrnWH) * rNDGrnWH * wDRpGnWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wNLssenH = 0.0 		else		wNLssenH = rNDGrnWH * wDLssenH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNMGrnWH = 0.0 		else		wNMGrnWH = kMGrenW * oNGrenWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tNStGenH = 0.0 		else		tNStGenH = rNDGrnWH * tDStGenH 		endif
! 	Resuspension_of_algae
		tNRsuren = rNDGrenS * tDRsuren
! 	N_excretion_of_algae_in_sediment
		tNEcrenS = (2.0 * rNDGrenS) /(cNDGrnMx + rNDGrenS) * rNDGrenS * tDRspenS
! 	N_mortality_of_algae_in_sediment
		tNMGrenS = kMGrenS * sNGrenS
! 	Corrected_sedimentation_velocity_of_Algae
		uCoSeenE = cVSetren * aFuauOME * uFuTmetE
! 	Sedimentation_flux_of_Algae
		tDStGenE = uCoSeenE * oDGrenWE
! 	N/D_ratio_of_Algae
		rNDGrnWE = oNGrenWE /(oDGrenWE+NearZero)
! 	Sedimentation
		tNStGenE = rNDGrnWE * tDStGenE
! 	P/D_ratio_of_Algae
		rPDGrnWE = oPGrenWE /(oDGrenWE+NearZero)
! 	Sedimentation
		tPStGenE = rPDGrnWE * tDStGenE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 0) ) then
		wDPmGnWH = 0.0 		else		wDPmGnWH = wDAsGenH - wDRpGnWH - wDLssenH - wDMGrnWH -(tDStGenH -tDStGenE - tDRs&
		&uren) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wPPmGnWH = 0.0 		else		wPPmGnWH = wPUGrenH - wPErGnWH - wPLssenH - wPMGrnWH -(tPStGenH -tPStGenE - tPRs&
		&uren) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wNPmGnWH = 0.0 		else		wNPmGnWH = wNUGrenH - wNErGnWH - wNLssenH - wNMGrnWH -(tNStGenH -tNStGenE - tNRs&
		&uren) / uDptWH 		endif
! 	P/D_ratio_of_Algae
		rPDDiWH = oPDiWH /(oDDiWH+NearZero)
! 	N/D_ratio_of_Algae
		rNDDiWH = oNDiWH /(oDDiWH+NearZero)
! 	P/D_ratio_of_Algae
		rPDDiS = sPDiatS /(sDDiatS+NearZero)
! 	Temperature_function_of_Algae
		uFunTDiH = exp(-0.5/(cSigTmDi*cSigTmDi) *((uTmH-cTmOptDi)*(uTmH-cTmOptDi) - (cTm&
		&Ref-cTmOptDi)*(cTmRef-cTmOptDi)))
! 	N/D_ratio_of_Algae
		rNDDiS = sNDiatS /(sDDiatS+NearZero)
! 	Temperature_function_of_Algae
		uFunTDiE = exp(-0.5/(cSigTmDi*cSigTmDi) *((uTmE-cTmOptDi)*(uTmE-cTmOptDi) - (cTm&
		&Ref-cTmOptDi)*(cTmRef-cTmOptDi)))
! 	Temperature_function_of_Algae
		uFunTDiS = exp(-0.5/(cSigTmDi*cSigTmDi) *((uTmBot-cTmOptDi)*(uTmBot-cTmOptDi) - &
		&(cTmRef-cTmOptDi)*(cTmRef-cTmOptDi)))
! 	Temperature_function_production_of_Algae
		uFumPDiH = uFunTDiH
! 	Temperature_function_respiration_of_Algae
		uFumRDiH = uFunTDiH
! 	Temperature_function_production_of_Algae
		uFumPDiE = uFunTDiE
! 	Temperature_function_respiration_of_Algae
		uFumRDiE = uFunTDiE
! 	Temperature_function_respiration_of_Algae
		uFumRDiS = uFunTDiS
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPaxDiH = max(0.0,cVPUMxDi * uFumPDiH *(cPDDiMx - rPDDiWH) /(cPDDiMx - cPDDiMn)&
		&)
! 	P_uptake_rate_of_Algae
		aVPUDiH = oPO4WH * aVPaxDiH /(aVPaxDiH / cAfPUDi + oPO4WH)
! 	P_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wPUDiH = 0.0 		else		wPUDiH = aVPUDiH * oDDiWH 		endif
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNaxDiH = max(0.0,cVNUMxDi * uFumPDiH * (cNDDiMx - rNDDiWH) /(cNDDiMx - cNDDiMn&
		&))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUDiH = aVNaxDiH / cAfNUDi
! 	N_uptake_rate_of_Algae
		aVNUDiH = oNDissWH * aVNaxDiH /(ahNUDiH + oNDissWH)
! 	N_uptake_Algae
		if ( (aInlSrat == 0) ) then
		wNUDiH = 0.0 		else		wNUDiH = aVNUDiH * oDDiWH 		endif
! 	Fraction_ammonium_uptake_by_Algae
		afNH4DiH = oNH4WH * oNO3WH /((ahNUDiH + oNH4WH) *(ahNUDiH + oNO3WH)) + oNH4WH * &
		&ahNUDiH /((oNH4WH + oNO3WH) *(ahNUDiH + oNO3WH))
! 	Ammonium_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUNHDiH = 0.0 		else		wNUNHDiH = afNH4DiH * wNUDiH 		endif
! 	Nitrate_uptake_by_Algae
		if ( (aInlSrat == 0) ) then
		wNUNODiH = 0.0 		else		wNUNODiH = wNUDiH - wNUNHDiH 		endif
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuaxDiH = cMuMxDi * uFumPDiH
! 	Max_growth_rate_of_Algae_at_ambient_temperature
		uMuaxDiE = cMuMxDi * uFumPDiE
! 	Droop_function(P)_for_Algae
		aPLimDiH = max(0.0,(1.0 - cPDDiMn / rPDDiWH) * cPDDiMx /(cPDDiMx - cPDDiMn))
! 	Droop_function(N)_for_Algae
		aNLimDiH = max(0.0,(1.0 - cNDDiMn / rNDDiWH) * cNDDiMx /(cNDDiMx - cNDDiMn))
! 	Silica_dependence_of_growth_rate
		aSiLiDiH = oSiO2WH /(hSiAssDi + oSiO2WH)
! 	Light_function
		aLLimDiH = UseteeDi *(exp(1.0) /(aExtCefH * uDptWH) *(exp(- aLPARotH /(cLOtRfDi &
		&* uFumPDiH)) - exp(- uLPRSrfH /(cLOtRfDi * uFumPDiH)))) +(1.0 - UseteeDi) *(1.0 &
		&/(aExtCefH * uDptWH) * log((1.0 + uLPRSrfH / (hLRefDi * uFumPDiH)) / (1.0 + aLPA&
		&RotH /(hLRefDi * uFumPDiH))))
! 	Growth_rate_at_current_light_AND_temp
		aMuTmDiH = ufDay *(1.0 - afCSuVeg) * aLLimDiH * uMuaxDiH
! 	Nutrient_limitation_function_of_Algae
		aNuLiDiH = min(aPLimDiH,(min(aNLimDiH,aSiLiDiH)))
! 	Growth_rate
		aMuDiH = aNuLiDiH * aMuTmDiH
! 	Assimilation_Algae
		if ( (aInlSrat == 0) ) then
		wDAssDiH = 0.0 		else		wDAssDiH = aMuDiH*oDDiWH 		endif
! 	Chlorophyll-a/DW_ratio_Algae
		rChDDiH = cChDDiMx -(cChDDiMx - cChDDiMn) * aLLimDiH
! 	Chlorophyll-a_conc
		oChlDiH = mgPerg * rChDDiH * oDDiWH
! 	Specific_extinction_per_unit_chlorophyll-a
		aExtCDiH = cExtSpDi / rChDDiH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDspDiH = kDRespDi * uFumRDiH
! 	Temp_corrected_respiration_constant_of_Algae
		ukDspDiE = kDRespDi * uFumRDiE
! 	Temp_corrected_respiration_constant_of_Algae
		ukDspDiS = kDRespDi * uFumRDiS
! 	Respiration_of_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wDRspiWH = 0.0 		else		wDRspiWH = ukDspDiH * oDDiWH 		endif
! 	Daily_grazing_on_Algae
		ukLssDiH = UseLoss * kLossDi * uFuTmssH
! 	Daily_grazing_on_Algae
		ukLssDiE = UseLoss * kLossDi * uFuTmssE
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wDLosDiH = 0.0 		else		wDLosDiH = ukLssDiH * oDDiWH 		endif
! 	Mortality_in_water
		if ( (aInlSrat == 0) ) then
		wDMDiWH = 0.0 		else		wDMDiWH = kMDiW * oDDiWH 		endif
! 	Corrected_sedimentation_velocity_of_Algae
		uCoVSDiH = cVSetDi * aFuauOMH * uFuTmetH
! 	Sedimentation_flux_of_Algae
		if ( (aInlSrat == 0) ) then
		tDSetDiH = 0.0 		else		tDSetDiH = uCoVSDiH * oDDiWH 		endif
! 	Resuspension_of_Algae
		tDRessDi = sDDiatS /(aDPhytS+NearZero) * tDRsPTot
! 	Respiration_of_sediment_Algae
		tDResDiS = ukDspDiS * sDDiatS
! 	Mortality_in_sed
		tDMDiS = kMDiS * sDDiatS
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDDeDiH = ukDspDiH + ukLssDiH + kMDiW +(uCoVSDiH * uFuTmetH) / uDptWH
! 	P_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPEcriWH = 0.0 		else		wPEcriWH = (2.0 * rPDDiWH) /(cPDDiMx + rPDDiWH) * rPDDiWH * wDRspiWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wPLosDiH = 0.0 		else		wPLosDiH = rPDDiWH * wDLosDiH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wPMDiWH = 0.0 		else		wPMDiWH = kMDiW * oPDiWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tPSetDiH = 0.0 		else		tPSetDiH = rPDDiWH * tDSetDiH 		endif
! 	Resuspension_of_algae
		tPRessDi = rPDDiS * tDRessDi
! 	P_excretion_of_algae_in_sediment
		tPExcDiS = (2.0 * rPDDiS) /(cPDDiMx + rPDDiS) * rPDDiS * tDResDiS
! 	P_mortality_of_algae_in_sediment
		tPMDiS = kMDiS * sPDiatS
! 	N_excretion_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNEcriWH = 0.0 		else		wNEcriWH = (2.0 * rNDDiWH) /(cNDDiMx + rNDDiWH) * rNDDiWH * wDRspiWH 		endif
! 	Algae_grazing_loss
		if ( (aInlSrat == 0) ) then
		wNLosDiH = 0.0 		else		wNLosDiH = rNDDiWH * wDLosDiH 		endif
! 	Mortality_Algae_in_water
		if ( (aInlSrat == 0) ) then
		wNMDiWH = 0.0 		else		wNMDiWH = kMDiW * oNDiWH 		endif
! 	Sedimentation
		if ( (aInlSrat == 0) ) then
		tNSetDiH = 0.0 		else		tNSetDiH = rNDDiWH * tDSetDiH 		endif
! 	Resuspension_of_algae
		tNRessDi = rNDDiS * tDRessDi
! 	N_excretion_of_algae_in_sediment
		tNExcDiS = (2.0 * rNDDiS) /(cNDDiMx + rNDDiS) * rNDDiS * tDResDiS
! 	N_mortality_of_algae_in_sediment
		tNMDiS = kMDiS * sNDiatS
! 	Corrected_sedimentation_velocity_of_Algae
		uCoVSDiE = cVSetDi * aFuauOME * uFuTmetE
! 	Sedimentation_flux_of_Algae
		tDSetDiE = uCoVSDiE * oDDiWE
! 	P/D_ratio_of_Algae
		rPDDiWE = oPDiWE /(oDDiWE+NearZero)
! 	Sedimentation
		tPSetDiE = rPDDiWE * tDSetDiE
! 	N/D_ratio_of_Algae
		rNDDiWE = oNDiWE /(oDDiWE+NearZero)
! 	Sedimentation
		tNSetDiE = rNDDiWE * tDSetDiE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 0) ) then
		wDPimiWH = 0.0 		else		wDPimiWH = wDAssDiH - wDRspiWH - wDLosDiH - wDMDiWH -(tDSetDiH - tDSetDiE - tDRe&
		&ssDi) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wPPimiWH = 0.0 		else		wPPimiWH = wPUDiH - wPEcriWH - wPLosDiH - wPMDiWH -(tPSetDiH -tPSetDiE - tPRessD&
		&i) / uDptWH 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 0) ) then
		wNPimiWH = 0.0 		else		wNPimiWH = wNUDiH - wNEcriWH - wNLosDiH - wNMDiWH -(tNSetDiH - tNSetDiE - tNRess&
		&Di) / uDptWH 		endif
! 	Total_chlorophyll-a
		oChlaH = oChlDiH + oChlGrH + oChlBlH
! 	Total_algal_growth
		wDAsPytH = wDAssDiH + wDAsGenH + wDAsBueH
! 	Total_algal_respiration_in_water
		wDRpPtWH = wDRspiWH + wDRpGnWH + wDRpBeWH
! 	Total_algal_mortality_in_water
		wDMPhtWH = wDMDiWH + wDMGrnWH + wDMBleWH
! 	Total_phytoplankton_sedimentation
		tDStPytH = tDSetDiH + tDStGenH + tDStBueH
! 	Total_phytoplankton_grazing_loss
		wDLssytH = wDLosDiH + wDLssenH + wDLssueH
! 	Total_of_PRIM_processes_of_algae_in_water
		wDPmPtWH = wDPimiWH + wDPmGnWH + wDPmBeWH
! 	Total_P_uptake_phytoplankton
		wPUPhytH = wPUDiH + wPUGrenH + wPUBlueH
! 	Total_P_excretion_phytoplankton_in_water
		wPErPtWH = wPEcriWH + wPErGnWH + wPErBeWH
! 	Total_P_mortality_phytoplankton_in_water
		wPMPhtWH = wPMDiWH + wPMGrnWH + wPMBleWH
! 	Total_sedimentation_of_algae
		tPStPytH = tPSetDiH + tPStGenH + tPStBueH
! 	-
		tPRsuhyt = tPRessDi + tPRsuren + tPRsulue
! 	Total_grazing_loss
		wPLssytH = wPLosDiH + wPLssenH + wPLssueH
! 	Total_of_PRIM_processes_of_algae_in_water
		wPPmPtWH = wPPimiWH + wPPmGnWH + wPPmBeWH
! 	Total_N_uptake_phytoplankton
		wNUPhytH = wNUDiH + wNUGrenH + wNUBlueH
! 	Total_ammonium-N_uptake_phytoplankton
		wNUH4ytH = wNUNHDiH + wNUH4enH + wNUH4ueH
! 	Total_nitrate-N_uptake_phytoplankton
		wNUO3ytH = wNUNODiH + wNUO3enH + wNUO3ueH
! 	Total_N_excretion_phytoplankton_in_water
		wNErPtWH = wNEcriWH + wNErGnWH + wNErBeWH
! 	Total_N_mortality_phytoplankton_in_water
		wNMPhtWH = wNMDiWH + wNMGrnWH + wNMBleWH
! 	Total_sedimentation_of_algae
		tNStPytH = tNSetDiH + tNStGenH + tNStBueH
! 	-
		tNRsuhyt = tNRessDi + tNRsuren + tNRsulue
! 	Total_grazing_loss
		wNLssytH = wNLosDiH + wNLssenH + wNLssueH
! 	Total_of_PRIM_processes_of_algae_in_water
		wNPmPtWH = wNPimiWH + wNPmGnWH + wNPmBeWH
! 	Respiration_of_algae_on_bottom
		tDRspytS = tDResDiS + tDRspenS + tDRspueS
! 	Mortality_of_algae_on_bottom
		tDMPhytS = tDMDiS + tDMGrenS + tDMBlueS
! 	Total_P_excretion_sediment_phytoplankton
		tPEcrytS = tPExcDiS + tPEcrenS + tPEcrueS
! 	Total_phytoplankton_mortality
		tPMPhytS = tPMDiS + tPMGrenS + tPMBlueS
! 	Total_N_excretion_sediment_phytoplankton
		tNEcrytS = tNExcDiS + tNEcrenS + tNEcrueS
! 	Total_phytoplankton_mortality
		tNMPhytS = tNMDiS + tNMGrenS + tNMBlueS
! 	Diatoms_silica_uptake
		wSiUDiH = cSiDDi * wDAssDiH
! 	Si_excretion
		wSixciWH = cSiDDi * wDRspiWH 
! 	Diatom_grazing_loss
		wSiosDiH = cSiDDi * wDLosDiH
! 	Diatoms_mortality_in_water
		wSiMDiWH = cSiDDi * wDMDiWH
! 	Diatoms_sedimentation
		tSiSeDiH = cSiDDi * tDSetDiH
! 	Diatoms_sedimentation
		tSiessDi = cSiDDi * tDRessDi 
! 	C-phycocyanin/DW-ratio_blue-greens
		rCyDBueH = cCyBleMx -(cCyBleMx - cCyBleMn) * aLLmBueH
! 	C-phycocyanin
		oCyanH = rCyDBueH * oDBlueWH * mgPerg
! 	DW_fraction_of_algal_group_of_total_algae
		fDDiH = oDDiWH /(oDDiWH + oDGrenWH + oDBlueWH + NearZero)
! 	Flux_to_water_detritus
		wDPimtWH = wDMPhtWH + wDLssytH
! 	O2_production_by_phytoplankton
		wO2odytH = molO2olC * cCPerDW * wDAsPytH
! 	O2_production_by_phytoplankton
		wO2sptWH = molO2olC * cCPerDW * wDRpPtWH * aCoO2ODH
! 	O2_production_due_to_NO3_uptake_by_phytoplankton
		wO2O3ytH = O2PerNO3 * molO2olN * wNUO3ytH
! 	O2_flux_by_water_algae
		wO2PrmWH = wO2odytH - wO2sptWH + wO2O3ytH
! 	Soluble_P_flux_from_died_Algae
		wPMyt4WH = fDissMP * wPMPhtWH
! 	Detrital_P_flux_from_died_Algae
		wPMhytWH = wPMPhtWH - wPMyt4WH
! 	Soluble_P_grazing_loss
		wPLPhO4H = fDissoss * wPLssytH
! 	Detrital_P_grazing_loss
		wPLsPDtH = wPLssytH - wPLPhO4H
! 	SRP_in_water
		wPPim4WH = - wPUPhytH + wPErPtWH + wPLPhO4H + wPMyt4WH
! 	Detritus_in_water
		wPPimtWH = wPLsPDtH + wPMhytWH
! 	Soluble_P_flux_from_died_Algae
		tPMhyO4S = fDissMP * tPMPhytS
! 	Detrital_P_flux_from_died_Algae
		tPMhyDtS = tPMPhytS - tPMhyO4S
! 	Pore_water_P
		tPPimO4S = tPEcrytS + tPMhyO4S
! 	Total_P_flux
		tPPimotT = 0.0
! 	Ammonium_flux_from_died_Algae
		wNMyt4WH = fDissMP * wNMPhtWH
! 	Detrital_N_flux_from_died_Algae
		wNMhytWH = wNMPhtWH - wNMyt4WH
! 	NH4-N_grazing_loss
		wNLPhH4H = fDissoss * wNLssytH
! 	Detrital_N_grazing_loss
		wNLsPDtH = wNLssytH - wNLPhH4H
! 	Ammonium_in_water
		wNPim4WH = - wNUH4ytH + wNErPtWH + wNLPhH4H + wNMyt4WH
! 	Nitrate_in_water
		wNPim3WH = - wNUO3ytH
! 	Detritus_in_water
		wNPimtWH = wNLsPDtH + wNMhytWH
! 	Ammonium_flux_from_died_Algae
		tNMhyH4S = fDissMP * tNMPhytS
! 	Detrital_N_flux_from_died_Algae
		tNMhyDtS = tNMPhytS - tNMhyH4S
! 	Pore_water_ammonium
		tNPimH4S = tNEcrytS + tNMhyH4S
! 	Pore_water_nitrate
		tNPimO3S = 0.0
! 	Total_N_flux
		tNPimotT = 0.0
! 	Si_excretion_of_bottom_Algae
		tSixcDiS = cSiDDi * tDResDiS 
! 	Mortality_of_bottom_Algae
		tSiMDiS = cSiDDi * tDMDiS
! 	Total_Si_flux_to_sio2_in_PRIM_module
		if ( (aInlSrat == 0) ) then
		wSiim2WH = 0.0 		else		wSiim2WH = wSixciWH - wSiUDiH + tSixcDiS / uDptWH 		endif
! 	Total_Si_flux_to_sed_detritus_in_PRIM_module
		wSiritWH = wSiMDiWH + wSiosDiH
! 	Total_Si_flux
		tSiriotT = 0.0
! 	Poole-Atkins_coefficient
		aPACoefE = cPACofMn +(cPACofMx - cPACofMn) * hPACoef / (hPACoef + oDOMWE)
! 	Poole-Atkins_coefficient
		aPACoefH = cPACofMn +(cPACofMx - cPACofMn) * hPACoef / (hPACoef + oDOMWH)
! 	Max_Secchi_depth
		bSechaxE = uDptWE
! 	Secchi_depth
		aSecchiE = min(bSechaxE,aPACoefE / aExCofOE)
! 	Transparency
		aTrpacyE = aSecchiE / uDptWE
! 	Max_Secchi_depth
		bSechaxH = uDptWH + cSechlus
! 	Secchi_depth
		if ( ( aInlSrat == 1 .and. uDptWE <= aPACoefE / aExCofOE ) ) then
		aSecchiH = min(bSechaxH,aPACoefH / aExCofOH - aSecchiE) 		else if ( ( aInlSrat == 1 .and. uDptWE > aPACoefE / aExCofOE) ) then		aSecchiH = 0.0 		else		aSecchiH = min(bSechaxH,aPACoefH / aExCofOH) 		endif
! 	Secchi_depth
		if ( ( aInlSrat == 1 ) ) then
		aSecchiT = aSecchiE +aSecchiH 		else		aSecchiT = aSecchiE 		endif
! 	Transparency
		aTrpacyH = aSecchiH / uDptWH
! 	Transparency
		if ( ( aInlSrat == 1 ) ) then
		aTrpacyT = aSecchiT / sDepthW 		else		aTrpacyT = aSecchiT / uDptWH 		endif
! 	Euphotic_depth
		aDptEphH = cEuph * aSecchiH
! 	Relative_euphotic_depth
		aReptphH = aDptEphH / uDptWH
! 	Chla_per_m2
		aChlaHH = oChlaH * uDptWH
! 	%_cover_with_algae
		aCoPhtWH = cCovSpP *(oDPhytWH * uDptWH)
! 	Average_spec_extinction_of_algae_per_unit_chla
		rExChytH = aExtPytH /(oChlaH / mgPerg + NearZero)
! 	Temp_function_of_zooplankton
		uFuTmooH = exp(-0.5/(cSigTZoo*cSigTZoo) *((uTmH-cTmOpZoo)*(uTmH-cTmOpZoo) -(cTmR&
		&ef-cTmOpZoo)*(cTmRef-cTmOpZoo)))
! 	Temp_function_of_zooplankton
		uFuTmooE = exp(-0.5/(cSigTZoo*cSigTZoo) *((uTmE-cTmOpZoo)*(uTmE-cTmOpZoo) -(cTmR&
		&ef-cTmOpZoo)*(cTmRef-cTmOpZoo)))
! 	P/D_ratio_herbaceous_zooplankton
		rPDZooH = oPZooH /(oDZooH+NearZero)
! 	N/C_ratio_herbaceous_zooplankton
		rNDZooH = oNZooH/(oDZooH+NearZero)
! 	Food_for_zooplankton
		oDFodooH = cPrefDi * oDDiWH + cPrefren * oDGrenWH + cPreflue * oDBlueWH + cPrefD&
		&e * oDDtWH
! 	Filtering_rate
		aFiltH = cFiltMx * uFuTmooH * hFilt /(hFilt + oDOMWH)
! 	Max_assimilation_rate_of_zooplankton_temp_corrected
		ukDsTooH = fDAssZoo * cFiltMx * uFuTmooH * hFilt
! 	Max_assimilation_rate_of_corrected
		ukDsTooE = fDAssZoo * cFiltMx * uFuTmooE * hFilt
! 	Food_saturation_function_of_zooplankton
		aDSatooH = oDFodooH /(hFilt + oDOMWH)
! 	Respiration_constant_of_zooplankton
		ukDspooH = kDResZoo * uFuTmooH
! 	Respiration_constant_of_zooplankton
		ukDspooE = kDResZoo * uFuTmooE
! 	Intrinsic_rate_of_increase_of_zooplankton
		ukDncooH = ukDsTooH - ukDspooH - kMZoo
! 	Intrinsic_rate_of_increase_of_zooplankton
		ukDncooE = ukDsTooE - ukDspooE - kMZoo
! 	Environmental_correction_of_zooplankton
		if ( (aInlSrat == 0) ) then
		wDEnvooH = 0.0 		else		wDEnvooH = max(0.0,ukDncooH / cDCarZoo * oDZooH*oDZooH) 		endif
! 	Assimilation_of_zooplankton
		if ( (aInlSrat == 0) ) then
		wDAssooH = 0.0 		else		wDAssooH = aDSatooH *(ukDsTooH * oDZooH - wDEnvooH) 		endif
! 	Consumption_of_zooplankton
		wDCZooH = wDAssooH / fDAssZoo
! 	DW_detritus_consumption_by_zooplankton
		wDCDtooH = cPrefDe*oDDtWH / oDFodooH * wDCZooH
! 	DW_diatoms_consumption_by_zooplankton
		wDCDiooH = cPrefDi*oDDiWH / oDFodooH * wDCZooH
! 	DW_greens_consumption_by_zooplankton
		wDCreooH = cPrefren*oDGrenWH / oDFodooH * wDCZooH
! 	DW_blue-greens_consumption_by_zooplankton
		wDCluooH = cPreflue*oDBlueWH / oDFodooH * wDCZooH
! 	Phytoplankton_consumption_by_zooplankton
		wDChyooH = wDCDiooH + wDCreooH + wDCluooH
! 	Egestion_of_zooplankton
		wDEZooH = wDCZooH - wDAssooH
! 	Correction_factor_of_zooplankton_respiration_for_P_and_N_content
		aCoReooH = max(cPDZoRef / rPDZooH,cNDZoRef / rNDZooH)
! 	Zooplankton_respiration
		if ( (aInlSrat == 0) ) then
		wDRspooH = 0.0 		else		wDRspooH = aCoReooH * ukDspooH * oDZooH 		endif
! 	Zooplankton_mortality_incl_environmental_correction
		if ( (aInlSrat == 0) ) then
		wDMZooH = 0.0 		else		wDMZooH = kMZoo * oDZooH +(1.0 - aDSatooH) * wDEnvooH 		endif
! 	Zooplankton_food
		oPFodooH = cPrefDi*oPDiWH + cPrefren*oPGrenWH + cPreflue*oPBlueWH + cPrefDe * oP&
		&DtWH
! 	P/D_ratio_of_zooplankton_food
		rPDooooH = oPFodooH /(oDFodooH+NearZero)
! 	P_diatom_consumption_by_zooplankton
		wPCDiooH = rPDDiWH * wDCDiooH
! 	P_green_consumption_by_zooplankton
		wPCreooH = rPDGrnWH * wDCreooH
! 	P_blue_green_consumption_by_zooplankton
		wPCluooH = rPDBleWH * wDCluooH
! 	Total_P_phytoplankton_consumption_by_zooplankton
		wPChyooH = wPCDiooH + wPCreooH + wPCluooH
! 	Consumption_of_detrital_P
		wPCDtooH = rPDDtWH * wDCDtooH
! 	Total_P_consumption
		wPCZooH = wPChyooH + wPCDtooH
! 	P_assimilation_efficiency_of_herbivores
		afPssooH = min(1.0,cPDZoRef / rPDooooH * fDAssZoo)
! 	Assimilation_by_herbivores
		wPAssooH = afPssooH * wPCZooH
! 	P_egestion
		wPEZooH = wPCZooH - wPAssooH
! 	Soluble_P_egestion
		wPEooO4H = fDiEgZoo*wPEZooH
! 	Detrital_P_egestion
		wPEZoDtH = wPEZooH - wPEooO4H
! 	P_excretion_rate_of_herbivores
		akPxcooH = rPDZooH / cPDZoRef * kDResZoo * uFuTmooH
! 	P_excretion
		if ( (aInlSrat == 0) ) then
		wPEcrooH = 0.0 		else		wPEcrooH = akPxcooH*oPZooH 		endif
! 	Mortality
		wPMZooH = rPDZooH * wDMZooH
! 	Soluble_P_mortality
		wPMooO4H = fDissZoo * wPMZooH
! 	Detrital_P_mortality
		wPMZoDtH = wPMZooH - wPMooO4H
! 	Zooplankton_food
		oNFodooH = cPrefDi*oNDiWH + cPrefren*oNGrenWH + cPreflue*oNBlueWH + cPrefDe*oNDt&
		&WH
! 	N/C_ratio_of_zooplankton_food
		rNDooooH = oNFodooH /(oDFodooH+NearZero)
! 	N_diatom_consumption_by_zooplankton
		wNCDiooH = rNDDiWH*wDCDiooH
! 	N_green_consumption_by_zooplankton
		wNCreooH = rNDGrnWH*wDCreooH
! 	N_blue_green_consumption_by_zooplankton
		wNCluooH = rNDBleWH*wDCluooH
! 	Total_N_phytoplankton_consumption_by_zooplankton
		wNChyooH = wNCDiooH + wNCreooH + wNCluooH
! 	Consumption_of_detrital_N
		wNCDtooH = rNDDtWH*wDCDtooH
! 	Total_N_consumption
		wNCZooH = wNChyooH + wNCDtooH
! 	N_assimilation_efficiency_of_herbivores
		afNssooH = min(1.0,cNDZoRef / rNDooooH * fDAssZoo)
! 	Assimilation_by_herbivores
		wNAssooH = afNssooH*wNCZooH
! 	N_egestion
		wNEZooH = wNCZooH - wNAssooH
! 	Soluble_N_egestion
		wNEooH4H = fDiEgZoo*wNEZooH
! 	Detrital_N_egestion
		wNEZoDtH = wNEZooH - wNEooH4H
! 	N_excretion_rate_of_herbivores
		kNEcrooH = rNDZooH / cNDZoRef * kDResZoo * uFuTmooH
! 	N_excretion
		if ( (aInlSrat == 0) ) then
		wNEcrooH = 0.0 		else		wNEcrooH = kNEcrooH*oNZooH 		endif
! 	Mortality
		wNMZooH = rNDZooH*wDMZooH
! 	Soluble_N_mortality
		wNMooH4H = fDissZoo*wNMZooH
! 	Detrital_N_mortality
		wNMZoDtH = wNMZooH - wNMooH4H
! 	Consumption_of_diatoms
		wSiDiooH = cSiDDi * wDCDiooH
! 	Temp_function_of_zoobenthos
		uFunTBnt = exp(-0.5/(cSiTment*cSiTment) *((uTmBot-cTmptent)*(uTmBot-cTmptent) - &
		&(cTmRef-cTmptent)*(cTmRef-cTmptent)))
! 	Food_for_zoobenthos
		aDFooBnt = sDDetS + aDPhytS
! 	P/D_ratio_of_zoobenthos
		rPDBnt = sPBent /(sDBent+NearZero)
! 	N/D_ratio_of_zoobenthos
		rNDBnt = sNBent /(sDBent+NearZero)
! 	Migration_flux
		tDMigBnt = kMigrent *(cDBentIn - sDBent)
! 	Food_limitation_function_of_zoobenthos
		aDSatBnt = aDFooBnt /(hDFodent + aDFooBnt)
! 	Intrinsic_net_increase_rate_of_zoobenthos
		ukDncBnt = (kDAssent - kDRspent) * uFunTBnt - kMBent
! 	Environmental_correction_of_zoobenthos
		tDEnvBnt = max(0.0,ukDncBnt / cDCrrent * sDBent*sDBent)
! 	Assimilation_of_zoobenthos
		tDAssBnt = aDSatBnt *(kDAssent * uFunTBnt * sDBent - tDEnvBnt)
! 	Specific_assimilation_rate_of_zoobenthos
		aDAsBtSp = tDAssBnt / sDBent
! 	Consumption_of_zoobenthos
		tDCBnt = tDAssBnt / fDAssent
! 	Detritus_consumption_by_zoobenthos
		tDCDtBnt = sDDetS / aDFooBnt * tDCBnt
! 	Diatoms_consumption_by_zoobenthos
		tDCDiBnt = sDDiatS / aDFooBnt * tDCBnt
! 	Greens_consumption_by_zoobenthos
		tDCreBnt = sDGrenS / aDFooBnt * tDCBnt
! 	Blue-greens_consumption_by_zoobenthos
		tDCluBnt = sDBlueS / aDFooBnt * tDCBnt
! 	Phytoplankton_consumption_by_zoobenthos
		tDChyBnt = tDCDiBnt + tDCreBnt + tDCluBnt
! 	Egestion_of_zoobenthos
		tDEBnt = tDCBnt - tDAssBnt
! 	Respiration_of_zoobenthos
		tDResBnt = (cPDenRef / rPDBnt) * kDRspent * uFunTBnt * sDBent
! 	Zoobenthos_mortality_incl_environmental_correction
		tDMBnt = kMBent*sDBent +(1.0 - aDSatBnt) * tDEnvBnt
! 	Food_for_zoobenthos
		aPFooBnt = sPDetS + aPPhytS
! 	Average_P/D_ratio_of_zoobenthos_food
		rPDooBnt = aPFooBnt /(aDFooBnt+NearZero)
! 	Detrital_P_consumption_by_zoobenthos
		tPCDtBnt = rPDDtS * tDCDtBnt
! 	Diatom_P_consumption_by_zoobenthos
		tPCDiBnt = rPDDiS * tDCDiBnt
! 	Greens_P_consumption_by_zoobenthos
		tPCreBnt = rPDGrenS * tDCreBnt
! 	Blue-greens_P_consumption_by_zoobenthos
		tPCluBnt = rPDBlueS * tDCluBnt
! 	Phytoplankton_P_consumption_by_zoobenthos
		tPChyBnt = tPCDiBnt + tPCreBnt + tPCluBnt
! 	Total_P_consumption_of_zoobenthos
		tPCBnt = tPCDtBnt + tPChyBnt
! 	P_assimilation_efficiency_of_zoobenthos
		afPAsBnt = min(1.0,cPDenRef / rPDooBnt * fDAssent)
! 	P_assimilation_of_zoobenthos
		tPAssBnt = afPAsBnt * tPCBnt
! 	Egestion_of_zoobenthos
		tPEBnt = tPCBnt - tPAssBnt
! 	SRP_egestion_of_zoobenthos
		tPEBnPO4 = fDiEgent * tPEBnt
! 	Detrital_P_egestion_of_zoobenthos
		tPEBntDt = (1.0 - fDiEgent) * tPEBnt
! 	P_excretion_of_zoobenthos
		tPExcBnt = (rPDBnt / cPDenRef) * kDRspent * uFunTBnt * sPBent
! 	Mortality_of_zoobenthos
		tPMBnt = rPDBnt * tDMBnt
! 	Part_of_died_zoobenthos_P_becoming_dissolved_P
		tPMBnPO4 = fDisMent * tPMBnt
! 	Part_of_died_zoobenthos_P_becoming_detrital_P
		tPMBntDt = (1.0-fDisMent)*tPMBnt
! 	Net_migration_flux
		tPMigBnt = kMigrent *(cPDenRef*cDBentIn - sPBent)
! 	Food_for_zoobenthos
		aNFooBnt = sNDetS + aNPhytS
! 	Average_N/D_ratio_of_zoobenthos_food
		rNDooBnt = aNFooBnt /(aDFooBnt+NearZero)
! 	Net_migration_flux
		tNMigBnt = kMigrent *(cNDenRef*cDBentIn - sNBent)
! 	Detrital_N_consumption_by_zoobenthos
		tNCDtBnt = rNDDtS * tDCDtBnt
! 	Diatom_N_consumption_by_zoobenthos
		tNCDiBnt = rNDDiS * tDCDiBnt
! 	Greens_N_consumption_by_zoobenthos
		tNCreBnt = rNDGrenS * tDCreBnt
! 	Blue-greens_N_consumption_by_zoobenthos
		tNCluBnt = rNDBlueS * tDCluBnt
! 	Phytoplankton_N_consumption_by_zoobenthos
		tNChyBnt = tNCDiBnt + tNCreBnt + tNCluBnt
! 	Total_N_consumption_of_zoobenthos
		tNCBnt = tNCDtBnt + tNChyBnt
! 	N_assimilation_efficiency_of_zoobenthos
		afNAsBnt = min(1.0,cNDenRef / rNDooBnt * fDAssent)
! 	N_assimilation_of_zoobenthos
		tNAssBnt = afNAsBnt * tNCBnt
! 	Egestion_of_zoobenthos
		tNEBnt = tNCBnt - tNAssBnt
! 	NH4_egestion_of_zoobenthos
		tNEBnNH4 = fDiEgent * tNEBnt
! 	Detrital_N_egestion_of_zoobenthos
		tNEBntDt = (1.0 - fDiEgent) * tNEBnt
! 	N_excretion_of_zoobenthos
		tNExcBnt = (rNDBnt / cNDenRef) * kDRspent * uFunTBnt * sNBent
! 	Mortality_of_zoobenthos
		tNMBnt = rNDBnt * tDMBnt
! 	Part_of_died_zoobenthos_N_becoming_ammonium-N
		tNMBnNH4 = fDisMent*tNMBnt
! 	Part_of_died_zoobenthos_N_becoming_detrital_N
		tNMBntDt = (1.0-fDisMent)*tNMBnt
! 	Diatom_consumption_by_zoobenthos
		tSiCDBnt = cSiDDi * tDCDiBnt
! 	Vegetation_dependence_of_fish_feeding
		aFuVeish = max(0.0,1.0 - cRelVish * aCovVeg)
! 	Intrinsic_net_increase_rate_of_fish
		ukDnciJv = (kDAssiJv - kDRspiJv) * uFuTmish - kMFiJv
! 	Food_limitation_function_of_adult_fish
		aDSatiAd = (aFuVeish * sDBent) *(aFuVeish * sDBent) /(hDBntiAd * hDBntiAd + (aFu&
		&Veish * sDBent) *(aFuVeish * sDBent))
! 	Intrinsic_net_increase_rate_of_fish
		ukDnciAd = (kDAssiAd - kDRspiAd) * uFuTmish - kMFiAd
! 	Fish_harvesting_constant
		if ( (cos(2.0 * Pi * sTime / DaysInY) > 0.1) ) then
		ukHrvish = kHaFiWin 		else		ukHrvish = kHaFiSum 		endif
! 	P_assimilation_efficiency_of_FiAd
		afPssiAd = min(1.0,cPDisRef / rPDBnt * fDAssiAd)
! 	N_assimilation_efficiency_of_FiAd
		afNssiAd = min(1.0,cNDisRef / rNDBnt * fDAssiAd)
! 	Temp_function_of_Pisc
		uFuTmisc = exp(-0.5 /(cSiTmisc*cSiTmisc) *((uTmE - cTmptisc)*(uTmE - cTmptisc) -&
		& (cTmRef - cTmptisc)*(cTmRef - cTmptisc)))
! 	Carrying_capacity_of_Pisc_for_lake_without_OR_with_marsh_zone_respectively
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		aDCrrisc = max(cDCrPcMn,min(cDCrPcMx,cDCPiare)) 		else if ( (sDShPhra < cDPaMisc) ) then		aDCrrisc = cDCrPcMn 		else if ( (aCovVeg < cCovVMn) ) then		aDCrrisc = max(cDCrPcMn,min(cDCrPcMx,fMarsh *(1.0/PerCent) * cRehrisc)) 		else		aDCrrisc = max(cDCrPcMn,min(cDCrPcMx,fMarsh *(1.0/PerCent) *(cRehrisc + cRelVisc&
		&))) 		endif
! 	Vegetation_dependence_of_Pisc_growth_rate
		aFuVeisc = aDSubVeg /(hDVPisc + aDSubVeg + NearZero)
! 	Intrinsic_net_increase_rate_of_Pisc
		akDncisc = (kDAssisc * aFuVeisc - kDRspisc) * uFuTmisc - kMPisc
! 	Fish_harvesting_constant
		if ( (cos(2.0*Pi * sTime / DaysInY) > 0.1) ) then
		ukHrvisc = kHaPiWin 		else		ukHrvisc = kHaPiSum 		endif
! 	Total_food_web_flux_of_DW_in_Sediment_detritus_in_lake
		if ( (0 == InclWeb) ) then
		tDWebDtS = 0.0 		else		tDWebDtS = - tDCDtBnt + tDEBnt + tDMBnt 		endif
! 	Total_food_web_flux_of_DW_in_sediment_diatoms_in_lake
		if ( (0 == InclWeb) ) then
		tDWebDiS = 0.0 		else		tDWebDiS = - tDCDiBnt 		endif
! 	Total_food_web_flux_of_DW_in_sediment_greens_in_lake
		if ( (0 == InclWeb) ) then
		tDWbGenS = 0.0 		else		tDWbGenS = - tDCreBnt 		endif
! 	Total_food_web_flux_of_DW_in_sediment_blue-greens_in_lake
		if ( (0 == InclWeb) ) then
		tDWbBueS = 0.0 		else		tDWbBueS = - tDCluBnt 		endif
! 	Total_food_web_flux_of_sediment_algae
		tDWbPytS = tDWebDiS + tDWbGenS + tDWbBueS
! 	Total_food_web_flux_of_P_in_pore_water_P_in_lake_sediment
		if ( (0 == InclWeb) ) then
		tPWebO4S = 0.0 		else		tPWebO4S = tPExcBnt + tPEBnPO4 + tPMBnPO4 		endif
! 	Total_food_web_flux_of_P_in_Sediment_P_in_lake
		if ( (0 == InclWeb) ) then
		tPWebDtS = 0.0 		else		tPWebDtS = - tPCDtBnt + tPEBntDt + tPMBntDt 		endif
! 	Total_food_web_flux_of_P_in_sediment_diatoms_in_lake
		if ( (0 == InclWeb) ) then
		tPWebDiS = 0.0 		else		tPWebDiS = - tPCDiBnt 		endif
! 	Total_food_web_flux_of_P_in_sediment_greens_in_lake
		if ( (0 == InclWeb) ) then
		tPWbGenS = 0.0 		else		tPWbGenS = - tPCreBnt 		endif
! 	Total_food_web_flux_of_P_in_sediment_blue-greens_in_lake
		if ( (0 == InclWeb) ) then
		tPWbBueS = 0.0 		else		tPWbBueS = - tPCluBnt 		endif
! 	Total_food_web_flux_of_sediment_algae
		tPWbPytS = tPWebDiS + tPWbGenS + tPWbBueS
! 	Total_food_web_flux_of_N_in_pore_water_ammonium_in_lake_sediment
		if ( (0 == InclWeb) ) then
		tNWebH4S = 0.0 		else		tNWebH4S = tNExcBnt + tNEBnNH4 + tNMBnNH4 		endif
! 	Total_food_web_flux_of_N_in_pore_water_nitrate_in_lake_sediment
		if ( (0 == InclWeb) ) then
		tNWebO3S = 0.0 		else		tNWebO3S = 0.0 		endif
! 	Total_food_web_flux_of_N_in_Sediment_N_in_lake_sediment
		if ( (0 == InclWeb) ) then
		tNWebDtS = 0.0 		else		tNWebDtS = - tNCDtBnt + tNEBntDt + tNMBntDt 		endif
! 	Total_food_web_flux_of_N_in_sediment_diatoms_in_lake
		if ( (0 == InclWeb) ) then
		tNWebDiS = 0.0 		else		tNWebDiS = - tNCDiBnt 		endif
! 	Total_food_web_flux_of_N_in_sediment_greens_in_lake
		if ( (0 == InclWeb) ) then
		tNWbGenS = 0.0 		else		tNWbGenS = - tNCreBnt 		endif
! 	Total_food_web_flux_of_N_in_sediment_blue-greens_in_lake
		if ( (0 == InclWeb) ) then
		tNWbBueS = 0.0 		else		tNWbBueS = - tNCluBnt 		endif
! 	Total_food_web_flux_of_sediment_algae
		tNWbPytS = tNWebDiS + tNWbGenS + tNWbBueS
! 	Total_food_web_flux_of_silica_in_sio2_lake_water
		wSiebO2W = 0.0
! 	Total_food_web_flux_of_silica_in_lake_water_detritus
		if ( (0 == InclWeb) ) then
		wSiebtWH = 0.0 		else		wSiebtWH = wSiDiooH 		endif
! 	Total_food_web_flux_of_silica_in_sediment_detritus
		if ( (0 == InclWeb) ) then
		tSiWeDtS = 0.0 		else		tSiWeDtS = tSiCDBnt 		endif
! 	Total_food_web_flux_of_silica
		if ( (0 == InclWeb) ) then
		tSiebotT = 0.0 		else		tSiebotT = 0.0 		endif
! 	Average_selection_factor
		aPrefveH = (cPrefDi * oDDiWH + cPrefren * oDGrenWH + cPreflue * oDBlueWH + cPref&
		&De * oDDtWH) / oDOMWH
! 	Total_zooplankton_consumption(check)
		if ( (aInlSrat == 0) ) then
		wDCZoo2H = 0.0 		else		wDCZoo2H = aFiltH * aPrefveH * oDOMWH * oDZooH 		endif
! 	Specific_consumption_rate_of_zooplankton(daily_ration)
		aDCZoSpH = wDCZooH / oDZooH
! 	Specific_C_assimilation_of_zooplankton
		aDAsZSpH = wDAssooH / oDZooH
! 	Specific_DW_grazing(daily_grazing)
		aDGraSpH = wDCZooH / oDOMWH
! 	Specific_P_consumption_OR_daily_ration
		aPCZoSpH = wPCZooH / oPZooH
! 	Specific_P_grazing_OR_daily_grazing
		aPGraSpH = wPCZooH / oPOMWH
! 	Specific_N_consumption_OR_daily_ration
		aNCZoSpH = wNCZooH / oNZooH
! 	Specific_N_grazing_OR_daily_grazing
		aNGraSpH = wNCZooH / oNOMWH
! 	Shoot/total_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afDShhra = sDShPhra /(sDRoPhra + sDShPhra) 		else		afDShhra = 0.0 		endif
! 	Shoot/Root_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rDSRPhra = sDShPhra/sDRoPhra 		else		rDSRPhra = 0.0 		endif
! 	Shoot_P/D_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDShhra = sPShPhra/sDShPhra 		else		rPDShhra = 0.0 		endif
! 	Shoot_N/D_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rNDShhra = sNShPhra/sDShPhra 		else		rNDShhra = 0.0 		endif
! 	Root_P/D_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rPDRthra = sPRoPhra/sDRoPhra 		else		rPDRthra = 0.0 		endif
! 	Root_N/D_-ratio
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		rNDRthra = sNRoPhra/sDRoPhra 		else		rNDRthra = 0.0 		endif
! 	-
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aLegShra = sDShPhra / cDSemhra / cDeSthra 		else		aLegShra = 0.0 		endif
! 	Marks_start_of_root_allocation_to_shoot_of_phragmites
		if ( (Day < 1.0 .or. sTime < StTime + 1.0) ) then
		bDanihra = 367 		else if ( (uTmE >= cTMithra .and. bDanihra > 366) ) then		bDanihra = Day 		else		bDanihra = bDanihra 		endif
! 	Root_biomass_available_for_allocation_to_shoot
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		aDAllhra = 0.0 		else if ( (Day < bDanihra) ) then		aDAllhra = 0.0 		else if ( (Day <= bDanihra + 1.0 / kDAllhra) ) then		aDAllhra = fDAllhra * sDRoPhra 		else		aDAllhra = 0.0 		endif
! 	Allocation_flux
		if ( (0 == IncSeson) ) then
		tDAllhra = 0.0 		else if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then		tDAllhra = 0.0 		else if ( (Day < bDanihra) ) then		tDAllhra = 0.0 		else if ( (Day <= bDanihra + 1.0 / kDAllhra) ) then		tDAllhra = kDAllhra * aDAllhra 		else		tDAllhra = 0.0 		endif
! 	Translocation_of_N_initial_growth
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNTanhra = rNDRthra * tDAllhra 		else		tNTanhra = 0.0 		endif
! 	Translocation_of_P_initial_growth
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPTanhra = rPDRthra * tDAllhra 		else		tPTanhra = 0.0 		endif
! 	Max_uptake_rate_at_current_N/D_ratio_AND_temp
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aVNhrxCr = max(0.0,cVNPhaMx * ((cQ1rohra )** (0.1 *(uTmE - cTmRef))) *(cNDPhaMx &
		&- rNDRthra) /(cNDPhaMx - cNDPhaMn)) 		else		aVNhrxCr = 0.0 		endif
! 	Half-saturating_N_concentration
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		ahNUPraS = aVNhrxCr / cAfNUhra 		else		ahNUPraS = 0.0 		endif
! 	N_uptake_rate(by_roots)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aVNUPraS = aVNhrxCr * oNDissSM /(ahNUPraS + oNDissSM) 		else		aVNUPraS = 0.0 		endif
! 	Total_N_uptake_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNUPhraS = aVNUPraS * sDRoPhra 		else		tNUPhraS = 0.0 		endif
! 	NH4_uptake_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNUH4raS = sNH4SM / aNDissSM * tNUPhraS 		else		tNUH4raS = 0.0 		endif
! 	NO3_uptake_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNUO3raS = tNUPhraS - tNUH4raS 		else		tNUO3raS = 0.0 		endif
! 	N_uptake_shoot
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNUShhra = afDShhra * tNUPhraS 		else		tNUShhra = 0.0 		endif
! 	N_uptake_root
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNURthra = tNUPhraS - tNUShhra 		else		tNURthra = 0.0 		endif
! 	Max_uptake_rate_at_current_P/D_ratio_AND_temp
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aVPhrxCr = max(0.0,cVPPhaMx * ((cQ1rohra )** (0.1 *(uTmE - cTmRef))) *(cPDPhaMx &
		&- rPDRthra) /(cPDPhaMx - cPDPhaMn)) 		else		aVPhrxCr = 0.0 		endif
! 	Half-saturating_P_concentration
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		ahPUPraS = aVPhrxCr / cAfPUhra 		else		ahPUPraS = 0.0 		endif
! 	P_uptake_rate(by_roots)
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aVPUPraS = aVPhrxCr * oPO4SM /(ahPUPraS + oPO4SM) 		else		aVPUPraS = 0.0 		endif
! 	Total_P_uptake_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPUPhraS = aVPUPraS * sDRoPhra 		else		tPUPhraS = 0.0 		endif
! 	P_uptake_shoot
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPUShhra = afDShhra * tPUPhraS 		else		tPUShhra = 0.0 		endif
! 	P_uptake_root
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPURthra = tPUPhraS - tPUShhra 		else		tPURthra = 0.0 		endif
! 	Temperature_function_production_vegetation
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		uFuPrhra = ((cQ1rohra )** (0.1 *(uTmE - cTmRef))) 		else		uFuPrhra = 0.0 		endif
! 	Maintenance_respiration_rate_at_current_temperature
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		ukDsphra = kDRsphra * ((cQ1eshra )** (0.1 *(uTmE - cTmRef))) 		else		ukDsphra = 0.0 		endif
! 	Max_photosynthetic_rate_at_current_light_AND_temp
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		aMuhohra = 0.0 		else if ( (aLegShra >= sDepthWM) ) then		aMuhohra = cMuPhaMx * uFuPrhra * ufDay 		else		aMuhohra = 0.0 		endif
! 	Droop_function_N-limitation
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNLPrhra = (1.0 - cNDPhaMn / rNDRthra) * cNDPhaMx /(cNDPhaMx - cNDPhaMn) 		else		aNLPrhra = 0.0 		endif
! 	Droop_function_P-limitation
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aPLPrhra = (1.0 - cPDPhaMn / rPDRthra) * cPDPhaMx /(cPDPhaMx - cPDPhaMn) 		else		aPLPrhra = 0.0 		endif
! 	Nutrient_reduction_function
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aNuLihra = min(aNLPrhra,aPLPrhra) 		else		aNuLihra = 0.0 		endif
! 	Growth_rate
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aMuPhra = aMuhohra * aNuLihra 		else		aMuPhra = 0.0 		endif
! 	Intrinsic_net_increase_rate_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		akDnchra = aMuhohra - ukDsphra - kDMShhra 		else		akDnchra = 0.0 		endif
! 	Density_correction_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDDnshra = max(0.0,akDnchra / cDSPhaMx * sDShPhra * sDShPhra) 		else		tDDnshra = 0.0 		endif
! 	Density_correction_of_production
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDDPrhra = aMuPhra / cMuPhaMx * tDDnshra 		else		tDDPrhra = 0.0 		endif
! 	Production_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDPodhra = aMuPhra * sDShPhra - tDDPrhra 		else		tDPodhra = 0.0 		endif
! 	Production_shoot_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDPdShra = afDShhra * tDPodhra 		else		tDPdShra = 0.0 		endif
! 	Production_root_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDPdRhra = tDPodhra - tDPdShra 		else		tDPdRhra = 0.0 		endif
! 	Maintenance_respiration_shoot_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDRpShra = ukDsphra * sDShPhra 		else		tDRpShra = 0.0 		endif
! 	Maintenance_respiration_root_of_reed
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDRpRhra = ukDsphra * sDRoPhra 		else		tDRpRhra = 0.0 		endif
! 	Root_O2_respiration
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2sphra = molO2olC * cCPerDW * tDRpRhra * afOxyedM 		else		tO2sphra = 0.0 		endif
! 	O2_flux_to_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tO2lohra = tO2sphra 		else		tO2lohra = 0.0 		endif
! 	-
		if ( (Day < 180.0) ) then
		bDaeahra = 367 		else if ( (ufDay <= fDayWin .and. bDaeahra > 366) ) then		bDaeahra = Day 		else		bDaeahra = bDaeahra 		endif
! 	Shoot_biomass_available_for_reallocation_to_root
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		aDRalhra = 0.0 		else if ( (Day < bDaeahra) ) then		aDRalhra = 0.0 		else if ( (Day <= bDaeahra + 1.0 / kDRalhra) ) then		aDRalhra = fDRalhra * sDShPhra 		else		aDRalhra = 0.0 		endif
! 	Reallocation_of_D_per_day_at_end_of_growing_season
		if ( (0 == IncSeson) ) then
		tDRalhra = 0.0 		else if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then		tDRalhra = 0.0 		else if ( (Day < bDaeahra) ) then		tDRalhra = 0.0 		else if ( (Day <= bDaeahra + 1.0 / kDRalhra) ) then		tDRalhra = kDRalhra * aDRalhra 		else		tDRalhra = 0.0 		endif
! 	Retranslocation_of_N_end_growing_season
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNRtrhra = rNDShhra * tDRalhra 		else		tNRtrhra = 0.0 		endif
! 	Retranslocation_of_P_end_growing_season
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPRtrhra = rPDShhra * tDRalhra 		else		tPRtrhra = 0.0 		endif
! 	Mortality_of_shoots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMShhra = kDMShhra * sDShPhra 		else		tDMShhra = 0.0 		endif
! 	Mortality_of_shoots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMShhra = rNDShhra * tDMShhra 		else		tNMShhra = 0.0 		endif
! 	Mortality_of_shoots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMShhra = rPDShhra * tDMShhra 		else		tPMShhra = 0.0 		endif
! 	Mortality_of_roots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMRthra = kDMRPhra * sDRoPhra 		else		tDMRthra = 0.0 		endif
! 	Mortality_of_roots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMRthra = rNDRthra * tDMRthra 		else		tNMRthra = 0.0 		endif
! 	Mortality_of_roots
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMRthra = rPDRthra * tDMRthra 		else		tPMRthra = 0.0 		endif
! 	Loss_flux_of_biomass_by_management
		if ( (0 == InclMrsh .or. fMarsh > NearZero) ) then
		tDMnShra = 0.0 		else if ( ((Day >= cDaMahra) .and. (Day < cDaMahra + 1.0)) ) then		tDMnShra = fManPhra * sDShPhra 		else		tDMnShra = 0.0 		endif
! 	Loss_flux_of_N_through_management
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMnShra = tDMnShra * rNDShhra 		else		tNMnShra = 0.0 		endif
! 	Loss_flux_of_P_through_management
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMnShra = tDMnShra * rPDShhra 		else		tPMnShra = 0.0 		endif
! 	Increase_in_inorganic_matter_in_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDIMSM = tDSetIMM 		else		tDIMSM = 0.0 		endif
! 	Increase_in_sediment_humus_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDHumSM = fRefrDeS * tDMintSM - tDMnHmSM 		else		tDHumSM = 0.0 		endif
! 	Increase_in_sediment_detritus_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDDtSM = tDSetDtM - tDMintSM + tDStPytM + tDMRthra 		else		tDDtSM = 0.0 		endif
! 	Turnover_depth_in_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		vDeltaSM = (tDIMSM / cRhoIM +(tDHumSM + tDDtSM) / cRhoOM)/(1.0 - bPorS) 		else		vDeltaSM = 0.0 		endif
! 	Burial_flux_of_DW_in_inorganic_matter_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tDBurIMM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tDBurIMM = ((tDHumSM + tDDtSM) +(cRhoOM / cRhoIM) * tDIMSM) / ((sDHumSM + sDDetS&
		&M) / sDIMSM + cRhoOM / cRhoIM) 		else		tDBurIMM = ((tDHumSM + tDDtSM) +(cRhoOM / cRhoIM) * tDIMSM) / (fDOrgoil /(1.0 - &
		&fDOrgoil) + cRhoOM / cRhoIM) 		endif
! 	Burial_flux_of_DW_in_organic_matter_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tDBurOMM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tDBurOMM = (sDHumSM + sDDetSM) / sDIMSM * tDBurIMM 		else		tDBurOMM = fDOrgoil /(1.0 - fDOrgoil) * tDBurIMM 		endif
! 	Burial_flux_of_DW_in_detritus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tDBurDtM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tDBurDtM = sDDetSM /(sDHumSM + sDDetSM) * tDBurOMM 		else		tDBurDtM = 0.0 		endif
! 	Burial_flux_of_DW_in_humus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tDBurumM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tDBurumM = tDBurOMM - tDBurDtM 		else		tDBurumM = tDBurOMM 		endif
! 	Total_DW_burial_flux_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tDBurotM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tDBurotM = tDBurIMM + tDBurOMM 		else		tDBurotM = tDBurIMM + tDBurOMM 		endif
! 	Burial_flux_of_P_in_humus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPBurumM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tPBurumM = rPDHumSM * tDBurumM 		else		tPBurumM = cPDSolOM * tDBurumM 		endif
! 	Burial_flux_of_P_in_detritus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPBurDtM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tPBurDtM = rPDDtSM * tDBurDtM 		else		tPBurDtM = 0.0 		endif
! 	Burial_flux_of_P_absorbed_onto_inorganic_matter_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPBurIMM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tPBurIMM = sPAIMSM / sDIMSM * tDBurIMM 		else		tPBurIMM = 0.0 		endif
! 	Burial_flux_of_dissolved_P_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPBurO4M = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tPBurO4M = sPO4SM *(vDeltaSM / cDepthSM) 		else		tPBurO4M = cPO4Gr *(bPorSM * vDeltaSM) 		endif
! 	Total_P_burial_flux_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tPBurotM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tPBurotM = tPBurDtM + tPBurumM + tPBurIMM + tPBurO4M 		else		tPBurotM = tPBurumM + tPBurIMM + tPBurO4M 		endif
! 	Burial_flux_of_N_in_humus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNBurumM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tNBurumM = rNDHumSM * tDBurumM 		else		tNBurumM = cNDSolOM * tDBurumM 		endif
! 	Burial_flux_of_N_in_detritus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNBurDtM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tNBurDtM = rNDDtSM * tDBurDtM 		else		tNBurDtM = 0.0 		endif
! 	Burial_flux_of_dissolved_NH4_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNBurH4M = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tNBurH4M = sNH4SM *(vDeltaSM / cDepthSM) 		else		tNBurH4M = cNH4Gr *(bPorSM * vDeltaSM) 		endif
! 	Burial_flux_of_dissolved_NO3_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNBurO3M = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tNBurO3M = sNO3SM *(vDeltaSM / cDepthSM) 		else		tNBurO3M = cNO3Gr *(bPorSM * vDeltaSM) 		endif
! 	Total_N_burial_flux_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tNBurotM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tNBurotM = tNBurDtM + tNBurumM + tNBurH4M + tNBurO3M 		else		tNBurotM = tNBurumM + tNBurH4M + tNBurO3M 		endif
! 	Burial_flux_of_Si_in_detritus_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tSiBuDtM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tSiBuDtM = rSiDDtSM * tDBurDtM 		else		tSiBuDtM = 0.0 		endif
! 	Total_Si_burial_flux_in_marsh
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		tSiurotM = 0.0 		else if ( (vDeltaSM >= 0.0) ) then		tSiurotM = tSiBuDtM 		else		tSiurotM = 0.0 		endif
! 	Marsh_water_depth_change
		if ( (0 == InclMrsh .or. fMarsh <= NearZero) ) then
		vDeltaWM = 0.0 		else if ( (ContDpth == 1) ) then		vDeltaWM = 0.0 		else		vDeltaWM = - vDeltaSM 		endif
! 	Relative_marsh_water_depth_change
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		aReDeaWM = vDeltaWM / sDepthWM 		else		aReDeaWM = 0.0 		endif
! 	Total_settling
		tDSetotH = tDSetIMH + tDSetDtH + tDStPytH
! 	Total_settling
		tPSetotH = tPSetIMH + tPSetDtH + tPStPytH
! 	Total_settling
		tNSetotH = tNSetDtH + tNStPytH
! 	Dredging_time(every_nth_year)
		if ( (sTime >= cDrdSart * DayPeear) ) then
		bTimered = (floor(TimeYars/cDrInval) * cDrInval) * DayPeear 		else		bTimered = -9999.999 		endif
! 	Average_solid_density_of_soil_material
		bRholoil = fDOrgoil * cRhoOM +(1.0 - fDOrgoil) * cRhoIM
! 	Mass_balance_totals_of_DW_marsh_water_and_vegetation_module
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tDMrsotT = (- tDBurotM - wDMintWM * sDepthWM -(1.0 - fRefrDeS) * tDMintSM - tDMn&
		&HmSM + tDPodhra - tDRpShra - tDRpRhra - tDMnShra) * fMarsh 		else		tDMrsotT = 0.0 		endif
! 	Mass_balance_totals_of_P_marsh_water_and_vegetation_module
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tPMrsotT = (- tPIfP4SM - tPDroO4M - tPBurotM - tPCemO4M - tPMnShra) * fMarsh 		else		tPMrsotT = 0.0 		endif
! 	Mass_balance_totals_of_N_marsh_water_and_vegetation_module
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tNMrsotT = (- tNIfN4SM - tNIfN3SM - tNDroO3M - tNDroH4M - tNBurotM - wNDentWM * &
		&sDepthWM - tNDentSM - tNMnShra) * fMarsh 		else		tNMrsotT = 0.0 		endif
! 	Mass_balance_totals_of_SI_marsh_water_and_vegetation_module
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		tSiarotT = - tSiBuDtM * fMarsh 		else		tSiarotT = 0.0 		endif
! 	Residence_time_of_substances
		uTaSustE = 1.0 / (ukDilE+NearZero)
! 	Marsh_water_exchange_coefficient
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		akExME = kExchMxM * hfMarsh /(hfMarsh + fMarsh)  		else		akExME = 0.0 		endif
! 	Relative_marsh_volume
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		afVlMshE = fMarsh * sDepthWM / uDptWE 		else		afVlMshE = 0.0 		endif
! 	Lake_water_exchange_coefficient
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		akExLE = akExME * afVlMshE 		else		akExLE = 0.0 		endif
! 	Total_P_phytoplankton_in_lake_water
		oPPhytWE = oPDiWE + oPGrenWE + oPBlueWE
! 	Total_N_phytoplankton_in_lake_water
		oNPhytWE = oNDiWE + oNGrenWE + oNBlueWE
! 	Total_seston
		oDSestWE = oDOMWE + oDIMWE
! 	Organic_P_in_water
		oPOMWE = oPPhytWE + oPDtWE
! 	Total_seston_P_(incl_adsorbed
		oPSestWE = oPPhytWE + oPDtWE + oPAIMWE
! 	Inorganic_P_in_water
		oPInogWE = oPO4WE + oPAIMWE
! 	Total_P_in_water_(excl_animals_AND_vegetation)
		oPTotWE = oPSestWE + oPO4WE
! 	Organic_seston_N
		oNOMWE = oNPhytWE + oNDtWE
! 	Total_seston_N
		oNSestWE = oNOMWE
! 	Kjeldahl_N_in_water
		oNkjWE = oNSestWE + oNH4WE
! 	Total_N_in_water_(without_animals_AND_vegetation)
		oNTotWE = oNkjWE + oNO3WE
! 	P/D_ratio_of_water_DIM
		rPDIMWE = oPAIMWE / oDIMWE
! 	P/D_ratio_of_water_detritus
		rPDDtWE = oPDtWE / (oDDtWE+NearZero)
! 	N/D_ratio_of_water_detritus
		rNDDtWE = oNDtWE / (oDDtWE+NearZero)
! 	Si/D_ratio_of_water_detritus
		rSiDDtWE = oSiDtWE / (oDDtWE+NearZero)
! 	Dilution_of_DW_IM
		wDDilIME = ukDilE * oDIMWE
! 	Dilution_of_detritus
		wDDilDtE = ukDilE * oDDtWE
! 	Dilution_of_Diat
		wDDilDiE = ukDilE * oDDiWE
! 	Dilution_of_Gren
		wDDlGenE = ukDilE * oDGrenWE
! 	Dilution_of_Blue
		wDDlBueE = ukDilE * oDBlueWE
! 	Total_algal_dilution
		wDDlPytE = wDDilDiE + wDDlGenE + wDDlBueE
! 	Total_algal_dilution
		wDDilooE = ukDilE * oDZooE
! 	Total_algal_dilution
		wPDilooE = ukDilE * oPZooE
! 	Total_algal_dilution
		wNDilooE = ukDilE * oNZooE
! 	Dilution_of_SRP
		wPDilO4E = ukDilE * oPO4WE
! 	Dilution_of_detritus
		wPDilDtE = ukDilE*oPDtWE
! 	Dilution_of_IM-ads_P
		wPDilIME = ukDilE * oPAIMWE
! 	Dilution_of_ammonium
		wNDilH4E = ukDilE * oNH4WE
! 	Dilution_of_nitrate
		wNDilO3E = ukDilE * oNO3WE
! 	Dilution_of_detritus
		wNDilDtE = ukDilE * oNDtWE
! 	Oxygen_inflow
		wO2nfowE = ukDilatE * cO2In
! 	Oxygen_outflow
		wO2OuflE = ukDilE * oO2WE
! 	Dilution_of_Diat
		wPDilDiE = ukDilE * oPDiWE
! 	Dilution_of_Diat
		wNDilDiE = ukDilE * oNDiWE
! 	Dilution_of_Gren
		wPDlGenE = ukDilE * oPGrenWE
! 	Dilution_of_Gren
		wNDlGenE = ukDilE * oNGrenWE
! 	Dilution_of_Blue
		wPDlBueE = ukDilE * oPBlueWE
! 	Dilution_of_Blue
		wNDlBueE = ukDilE * oNBlueWE
! 	Total_algal_dilution
		wPDlPytE = wPDilDiE + wPDlGenE + wPDlBueE
! 	Total_algal_dilution
		wNDlPytE = wNDilDiE + wNDlGenE + wNDlBueE
! 	Outflow_of_DW
		wDOtfotE = ukOutE * oDSestWE
! 	Outflow_of_P
		wPOtfotE = ukOutE * oPTotWE
! 	Outflow_of_N
		wNOtfotE = ukOutE * oNTotWE
! 	Transport_flux_of_D_in_Diat
		if ( (0 == InclTran) ) then
		wDTraDiE = 0.0 		else		wDTraDiE = uDLoadDi / uDptWE - wDDilDiE 		endif
! 	Transport_flux_of_P_in_Diat
		if ( (0 == InclTran) ) then
		wPTraDiE = 0.0 		else		wPTraDiE = uPLoadDi / uDptWE - wPDilDiE 		endif
! 	Transport_flux_of_N_in_Diat
		if ( (0 == InclTran) ) then
		wNTraDiE = 0.0 		else		wNTraDiE = uNLoadDi / uDptWE - wNDilDiE 		endif
! 	Transport_flux_of_D_in_Gren
		if ( (0 == InclTran) ) then
		wDTanenE = 0.0 		else		wDTanenE = uDLadren / uDptWE - wDDlGenE 		endif
! 	Transport_flux_of_P_in_Gren
		if ( (0 == InclTran) ) then
		wPTanenE = 0.0 		else		wPTanenE = uPLadren / uDptWE - wPDlGenE 		endif
! 	Transport_flux_of_N_in_Gren
		if ( (0 == InclTran) ) then
		wNTanenE = 0.0 		else		wNTanenE = uNLadren / uDptWE - wNDlGenE 		endif
! 	Transport_flux_of_D_in_Blue
		if ( (0 == InclTran) ) then
		wDTanueE = 0.0 		else		wDTanueE = uDLadlue / uDptWE - wDDlBueE 		endif
! 	Transport_flux_of_P_in_Blue
		if ( (0 == InclTran) ) then
		wPTanueE = 0.0 		else		wPTanueE = uPLadlue / uDptWE - wPDlBueE 		endif
! 	Transport_flux_of_N_in_Blue
		if ( (0 == InclTran) ) then
		wNTanueE = 0.0 		else		wNTanueE = uNLadlue / uDptWE - wNDlBueE 		endif
! 	Total_transport_flux_of_D_in_Phyt
		if ( (0 == InclTran) ) then
		wDTanytE = 0.0 		else		wDTanytE = wDTraDiE + wDTanenE + wDTanueE 		endif
! 	Total_transport_flux_of_P_in_Phyt
		if ( (0 == InclTran) ) then
		wPTanytE = 0.0 		else		wPTanytE = wPTraDiE + wPTanenE + wPTanueE 		endif
! 	Total_transport_flux_of_N_in_Phyt
		if ( (0 == InclTran) ) then
		wNTanytE = 0.0 		else		wNTanytE = wNTraDiE + wNTanenE + wNTanueE 		endif
! 	Dilution_of_Si_in_sio2
		wSiilO2E = ukDilE * oSiO2WE
! 	Dilution_of_Si_in_detritus
		wSiDiDtE = ukDilE * oSiDtWE
! 	Dilution_of_Si_in_diatoms
		wSiDiDiE = cSiDDi * wDDilDiE
! 	Total_Si_surface_outflow
		wSitfotE = ukOutE * (oSiO2WE + oSiDtWE + cSiDDi * oDDiWE)
! 	Transport_flux_of_Si_in_SIO2
		if ( (0 == InclTran) ) then
		wSianO2E = 0.0 		else		wSianO2E = uSioaiO2 / uDptWE - wSiilO2E 		endif
! 	Transport_flux_of_Si_in_detritus
		if ( (0 == InclTran) ) then
		wSiratWE = 0.0 		else		wSiratWE = uSiLodDt / uDptWE - wSiDiDtE 		endif
! 	Total_Si_transport_flux
		if ( (0 == InclTran) ) then
		tSiantTE = 0.0 		else		tSiantTE = uSioaiO2 + uSiLodDt + uSiLodDi - (wSiDiDtE + wSiilO2E + wSiDiDiE) * u&
		&DptWE 		endif
! 	Net_migration_flux_of_D_in_Zoo
		if ( (0 == InclTran) ) then
		wDTanooE = 0.0 		else		wDTanooE =( ukDilatE * cDZooIn - wDDilooE) 		endif
! 	Net_migration_flux_of_P_in_ZOO
		if ( (0 == InclTran) ) then
		wPTanooE = 0.0 		else		wPTanooE =(ukDilatE * cPDZoRef*cDZooIn - wPDilooE) 		endif
! 	Net_migration_flux_of_N_in_Zoo
		if ( (0 == InclTran) ) then
		wNTanooE = 0.0 		else		wNTanooE =(ukDilatE * cNDZoRef*cDZooIn - wNDilooE) 		endif
! 	Transport_flux_DW_in_IM
		if ( (0 == InclTran) ) then
		wDTanMWE = 0.0 		else		wDTanMWE = uDLoadIM / uDptWE - wDDilIME 		endif
! 	Transport_flux_DW_in_detritus
		if ( (0 == InclTran) ) then
		wDTantWE = 0.0 		else		wDTantWE = uDLoadDt / uDptWE - wDDilDtE 		endif
! 	Transport_flux_O2
		if ( (0 == InclTran) ) then
		wO2TrnWE = 0.0 		else		wO2TrnWE = wO2nfowE - wO2OuflE 		endif
! 	Transport_flux_of_P_in_PO4
		if ( (0 == InclTran) ) then
		wPTan4WE = 0.0 		else		wPTan4WE = uPLoaPO4 / uDptWE - wPDilO4E 		endif
! 	Transport_flux_of_P_in_AIM
		if ( (0 == InclTran) ) then
		wPTanMWE = 0.0 		else		wPTanMWE = uPLoaAIM / uDptWE - wPDilIME 		endif
! 	Transport_flux_of_P_in_detritus
		if ( (0 == InclTran) ) then
		wPTantWE = 0.0 		else		wPTantWE = uPLoadDt / uDptWE - wPDilDtE 		endif
! 	Transport_flux_of_N_in_NH4
		if ( (0 == InclTran) ) then
		wNTan4WE = 0.0 		else		wNTan4WE = uNLoaNH4 / uDptWE - wNDilH4E 		endif
! 	Transport_flux_of_N_in_NO3
		if ( (0 == InclTran) ) then
		wNTan3WE = 0.0 		else		wNTan3WE = uNLoaNO3 / uDptWE - wNDilO3E 		endif
! 	Transport_flux_of_N_in_detritus
		if ( (0 == InclTran) ) then
		wNTantWE = 0.0 		else		wNTantWE = uNLoadDt / uDptWE - wNDilDtE 		endif
! 	Total_DW_dilution_fluxes
		wDDilotE = wDDilIME + wDDilDtE + wDDlPytE
! 	Total_P_dilution_fluxes
		wPDilotE = wPDilDtE + wPDilO4E + wPDilIME + wPDlPytE
! 	Total_N_dilution_fluxes
		wNDilotE = wNDilDtE + wNDilO3E + wNDilH4E + wNDlPytE
! 	Total_SI_dilution_fluxes
		wSiilotE = wSiDiDtE + wSiilO2E + wSiDiDiE
! 	Total_transport_fluxes_of_DW_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tDTantTE = 0.0 		else		tDTantTE = uDLoad - wDDilotE * uDptWE 		endif
! 	Total_transport_fluxes_of_P_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tPTantTE = 0.0 		else		tPTantTE = uPLoad - wPDilotE * uDptWE 		endif
! 	Total_transport_fluxes_of_N_for_mass_balance_equations
		if ( (0 == InclTran) ) then
		tNTantTE = 0.0 		else		tNTantTE = uNLoad - wNDilotE * uDptWE 		endif
! 	Diffusive_exchange_flux_of_DW_in_IM_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExIMME = akExME *(oDIMWE - oDIMWM) 		else		wDExIMME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_PO4_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExP4ME = akExME *(oPO4WE - oPO4WM) 		else		wPExP4ME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_AIM_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExAMME = akExME *(oPAIMWE - oPAIMWM) 		else		wPExAMME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NH4_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExN4ME = akExME *(oNH4WE - oNH4WM) 		else		wNExN4ME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NO3_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExN3ME = akExME *(oNO3WE - oNO3WM) 		else		wNExN3ME = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSixS2ME = akExME *(oSiO2WE - oSiO2WM) 		else		wSixS2ME = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2ExME = akExME *(oO2WE - oO2WM) 		else		wO2ExME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_detritus_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDtME = akExME *(oDDtWE - oDDtWM) 		else		wDExDtME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDtME = akExME *(oPDtWE - oPDtWM) 		else		wPExDtME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDtME = akExME *(oNDtWE - oNDtWM) 		else		wNExDtME = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExtME = akExME *(oSiDtWE - oSiDtWM) 		else		wSiExtME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDiME = akExME *(oDDiWE - oDDiWM) 		else		wDExDiME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDiME = akExME *(oPDiWE - oPDiWM) 		else		wPExDiME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDiME = akExME *(oNDiWE - oNDiWM) 		else		wNExDiME = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExiME = cSiDDi * wDExDiME 		else		wSiExiME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDEGrnME = akExME *(oDGrenWE - oDGrenWM) 		else		wDEGrnME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPEGrnME = akExME *(oPGrenWE - oPGrenWM) 		else		wPEGrnME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNEGrnME = akExME *(oNGrenWE - oNGrenWM) 		else		wNEGrnME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDEBleME = akExME *(oDBlueWE - oDBlueWM) 		else		wDEBleME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPEBleME = akExME *(oPBlueWE - oPBlueWM) 		else		wPEBleME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNEBleME = akExME *(oNBlueWE - oNBlueWM) 		else		wNEBleME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExZoME = akExME *(oDZooE - oDZooM) 		else		wDExZoME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExZoME = akExME *(oPZooE - oPZooM) 		else		wPExZoME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_epilimnion_to_marsh
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExZoME = akExME *(oNZooE - oNZooM) 		else		wNExZoME = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_IM_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExIME = akExLE *(oDIMWE - oDIMWM) 		else		wDExIME = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_PO4_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExPO4E = akExLE *(oPO4WE - oPO4WM) 		else		wPExPO4E = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_AIM_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExAIME = akExLE *(oPAIMWE - oPAIMWM) 		else		wPExAIME = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NH4_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExNH4E = akExLE *(oNH4WE - oNH4WM) 		else		wNExNH4E = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_NO3_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExNO3E = akExLE *(oNO3WE - oNO3WM) 		else		wNExNO3E = 0.0 		endif
! 	Diffusive_exchange_flux_of_SiO2_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSixSO2E = akExLE *(oSiO2WE - oSiO2WM) 		else		wSixSO2E = 0.0 		endif
! 	Diffusive_exchange_flux_of_O2_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wO2ExE = akExLE *(oO2WE - oO2WM) 		else		wO2ExE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_detritus_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDtE = akExLE *(oDDtWE - oDDtWM) 		else		wDExDtE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_detritus_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDtE = akExLE *(oPDtWE - oPDtWM) 		else		wPExDtE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_detritus_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDtE = akExLE *(oNDtWE - oNDtWM) 		else		wNExDtE = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_detritus_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExDtE = akExLE *(oSiDtWE - oSiDtWM) 		else		wSiExDtE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_diatoms_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExDiE = akExLE *(oDDiWE - oDDiWM) 		else		wDExDiE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_diatoms_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExDiE = akExLE *(oPDiWE - oPDiWM) 		else		wPExDiE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_diatoms_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExDiE = akExLE *(oNDiWE - oNDiWM) 		else		wNExDiE = 0.0 		endif
! 	Diffusive_exchange_flux_of_Si_in_diatoms_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wSiExDiE = cSiDDi * wDExDiE 		else		wSiExDiE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_green_algae_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExGenE = akExLE *(oDGrenWE - oDGrenWM) 		else		wDExGenE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_green_algae_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExGenE = akExLE *(oPGrenWE - oPGrenWM) 		else		wPExGenE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_green_algae_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExGenE = akExLE *(oNGrenWE - oNGrenWM) 		else		wNExGenE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_blue_greens_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExBueE = akExLE *(oDBlueWE - oDBlueWM) 		else		wDExBueE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_blue_greens_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExBueE = akExLE *(oPBlueWE - oPBlueWM) 		else		wPExBueE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_blue_greens_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExBueE = akExLE *(oNBlueWE - oNBlueWM) 		else		wNExBueE = 0.0 		endif
! 	Diffusive_exchange_flux_of_DW_in_zooplankton_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wDExZooE = akExLE *(oDZooE - oDZooM) 		else		wDExZooE = 0.0 		endif
! 	Diffusive_exchange_flux_of_P_in_zooplankton_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wPExZooE = akExLE *(oPZooE - oPZooM) 		else		wPExZooE = 0.0 		endif
! 	Diffusive_exchange_flux_of_N_in_zooplankton_from_marsh_to_epilimnion
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		wNExZooE = akExLE *(oNZooE - oNZooM) 		else		wNExZooE = 0.0 		endif
! 	Infiltration_of_SRP
		if ( (aInlSrat == 1 ) ) then
		tPIfP4WE = 0.0 		else if ( (cQInf >= 0.0) ) then		tPIfP4WE = cQInf / mmPerm * oPO4WE 		else		tPIfP4WE = cQInf / mmPerm * oPO4S 		endif
! 	Infiltration_of_ammonium
		if ( (aInlSrat == 1 ) ) then
		tNIfN4WE = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN4WE = cQInf / mmPerm * oNH4WE 		else		tNIfN4WE = cQInf / mmPerm * oNH4S 		endif
! 	Infiltration_of_nitrate
		if ( (aInlSrat == 1 ) ) then
		tNIfN3WE = 0.0 		else if ( (cQInf >= 0.0) ) then		tNIfN3WE = cQInf / mmPerm * oNO3WE 		else		tNIfN3WE = cQInf / mmPerm * oNO3S 		endif
! 	Reaeration_flux_of_O2_into_the_water
		tO2AerE = kAer * uFuTmerE * (uO2SatE - oO2WE) * aFuLeAer
! 	IM_bioturbation_by_fish
		tDTbFhIM = fLutum * sDIMS / (fLutum * sDIMS + sDDetS) * tDTrbish
! 	Corrected_sedimentation_velocity_of_IM
		uCoVSIME = aFuauIME * (((fLutuRef/fLutum) )** (0.5)) * uFuTmetE * cVSetIM
! 	Sedimentation_flux_of_inert_matter
		tDSetIME = uCoVSIME * oDIMWE
! 	Sedimentation_flux_of_P_adsorbed_onto_inert_org_matter
		tPSetIME = oPAIMWE / oDIMWE * tDSetIME
! 	Corrected_sedimentation_velocity_of_detritus
		uCoVSDtE = cVSetDe * aFuauOME * uFuTmetE
! 	Sedimentation_flux_of_detritus
		tDSetDtE = uCoVSDtE * oDDtWE
! 	Sedimentation_flux_of_detrital_P
		tPSetDtE = uCoVSDtE * oPDtWE
! 	Sedimentation_flux_of_detrital_N
		tNSetDtE = uCoVSDtE * oNDtWE
! 	Sedimentation_flux_of_detrital_Si
		tSiSeDtE = uCoVSDtE * oSiDtWE
! 	Decomposition
		wDMintWE = kDMnDeW * uFuTmnWE * oDDtWE
! 	Mineralization
		wPMintWE = kPMinDtW * uFuTmnWE * oPDtWE
! 	Mineralization
		wNMintWE = kNMinDtW * uFuTmnWE * oNDtWE
! 	Mineralization
		wSiintWE = kSiMiDtW * uFuTmnWE * oSiDtWE
! 	Correction_of_O2_demand_in_water_at_low_oxygen_conc
		aCoO2ODE = oO2WE / (hO2BOD + oO2WE)
! 	O2_flux_due_to_mineralization_of_detritus
		wO2intWE = molO2olC * cCPerDW * aCoO2ODE * wDMintWE
! 	Mineralisation_flux_by_denitrification
		wDDentWE = oNO3WE*oNO3WE / (hNO3Dnit*hNO3Dnit + oNO3WE*oNO3WE) * (1.0 - aCoO2ODE&
		&) * wDMintWE
! 	Denitrification_flux
		wNDentWE = NO3PerC * molNmolC * cCPerDW * wDDentWE
! 	Oxygen_consumption_during_nitrification
		aCo2NrWE = oO2WE*oO2WE / (hO2Nitr*hO2Nitr + oO2WE*oO2WE)
! 	Nitrification_flux
		wNNitrWE = kNitrW * uFuTmtrE * aCo2NrWE * oNH4WE
! 	O2_flux_due_to_nitrification
		wO2NirWE = O2PerNH4 * molO2olN * wNNitrWE
! 	Aerobic_mineralisation
		tDMnODtS = afOxySed * (1.0 - fRefrDeS) * tDMinDtS
! 	Sediment_oxygen_demand
		tO2MiDtS = molO2olC * cCPerDW * tDMnODtS
! 	Mineralisation_flux_by_denitrification
		tDDenitS = oNO3S*oNO3S / (hNO3Dnit*hNO3Dnit + oNO3S*oNO3S) * (1.0 - afOxySed) * &
		&(1.0 - fRefrDeS) * tDMinDtS
! 	Denitrification_flux
		tNDenitS = NO3PerC * molNmolC * cCPerDW * tDDenitS
! 	Nitrification_flux
		tNNitrS = afOxySed * kNitrS * uFuTmtrS * sNH4S
! 	O2_flux_due_to_nitrification
		tO2NitrS = O2PerNH4 * molO2olN * tNNitrS
! 	Decomposition_of_upper_sediment_humus
		tDMinumS = kDMnHum * uFuTminS * afOxySed * sDHumS
! 	Mineralization_of_P_in_upper_sediment_humus
		tPMinumS = kDMnHum * uFuTminS * afOxySed * sPHumS
! 	Mineralization_of_N_in_upper_sediment_humus
		tNMinumS = kDMnHum * uFuTminS * afOxySed * sNHumS
! 	Diffusion_flux_of_dissolved_P_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tPDifO4E = 0.0 		else		tPDifO4E = kPDifPO4 * uFunTDif * cTubDNut * bPorCorS * (oPO4S - oPO4WE ) / aDptD&
		&if 		endif
! 	Diffusion_flux_of_NO3_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tNDifO3E = 0.0 		else		tNDifO3E = kNDifNO3 * uFunTDif * cTubDNut * bPorCorS * (oNO3S - oNO3WE ) / aDptD&
		&if 		endif
! 	Diffusion_flux_of_NH4_from_sediment_to_water
		if ( ( aInlSrat == 1 ) ) then
		tNDifH4E = 0.0 		else		tNDifH4E = kNDifNH4 * uFunTDif * cTubDNut * bPorCorS * (oNH4S - oNH4WE ) / aDptD&
		&if 		endif
! 	O2_diffusion_(water_->_sediment)
		if ( ( aInlSrat == 1 ) ) then
		tO2DifE = 0.0 		else		tO2DifE = kO2Dif / aDptDif * uFunTDif * cTubDfO2 * bPorCorS * oO2WE 		endif
! 	Max_P_adsorption_per_g_inorg_matter_in_water
		aPAsMxWE = cRelPdsD + aCoO2ODE * cRePAsFe * fFeDIM + cRePAsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsWE = (1.0 - fRedMx * (1.0-aCoO2ODE)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		aPIoAsWE = aPAsMxWE * aKPAdsWE * oPO4WE / (1.0 + aKPAdsWE * oPO4WE)
! 	Equilibrium_conc
		aPEqIMWE = aPIoAsWE * oDIMWE
! 	Sorption_flux_in_water
		wPSrpMWE = kPSorp * (aPEqIMWE - oPAIMWE)
! 	Max_P_adsorption_per_g_inorg_matter_in_sediment
		aPAdsaxS = cRelPdsD + afOxySed * cRePAsFe * fFeDIM + cRePAsAl * fAlDIM
! 	P_adsorption_affinity_corrected_for_redox_conditions
		aKPAdsS = (1.0 - fRedMx * (1.0-afOxySed)) * cKPAdsOx
! 	P_adsorption_isotherm_onto_inorg_matter_in_sediment
		aPIsodsS = aPAdsaxS * aKPAdsS * oPO4S / (1.0 + aKPAdsS * oPO4S)
! 	Equilibrium_amount
		aPEqIMS = aPIsodsS * sDIMS
! 	Sorption
		tPSorIMS = kPSorp * (aPEqIMS - sPAIMS)
! 	Total_abiotic/microbial_O2_flux_in_water
		if ( (aInlSrat == 0) ) then
		wO2AboWH = 0.0 		else		wO2AboWH = tO2BubH / uDptWH - wO2intWH - wO2NirWH - (tO2MiDtS + tO2NitrS) / uDpt&
		&WH 		endif
! 	Total_abiotic/microbial_P_detritus_flux_in_water
		if ( (aInlSrat == 0) ) then
		wPAiotWH = 0.0 		else		wPAiotWH = - wPMintWH - (tPSetDtH - tPSetDtE - tPRessDt) / uDptWH 		endif
! 	Total_abiotic/microbial_dissolved_P_flux_in_water
		if ( (aInlSrat == 0) ) then
		wPAio4WH = 0.0 		else		wPAio4WH = wPMintWH - wPSrpMWH - tPIfP4WH / uDptWH + tPDifO4H / uDptWH + tPRsuPO&
		&4 / uDptWH 		endif
! 	Total_abiotic/microbial_P_absorbed_onto_inorganic_matter_flux_in_water
		if ( (aInlSrat == 0) ) then
		wPAioMWH = 0.0 		else		wPAioMWH = (- tPSetIMH +tPSetIME + tPRsuAIM) / uDptWH + wPSrpMWH 		endif
! 	Total_abiotic/microbial_P_flux_for_mass_balance_check
		tPAiootT = uPErosOM - tPChePO4 - tPInfO4S - tPDroPO4
! 	Total_abiotic/microbial_N_NH4_flux_in_water
		if ( (aInlSrat == 0) ) then
		wNAio4WH = 0.0 		else		wNAio4WH = wNMintWH - wNNitrWH - tNIfN4WH / uDptWH + (tNDifH4H + tNRsuNH4) / uDp&
		&tWH 		endif
! 	Total_abiotic/microbial_N_NO3_flux_in_water
		if ( (aInlSrat == 0) ) then
		wNAio3WH = 0.0 		else		wNAio3WH = wNNitrWH - wNDentWH + (tNDifO3H + tNRsuNO3 - tNIfN3WH) / uDptWH 		endif
! 	Total_abiotic/microbial_N_detritus_flux_in_water
		if ( (aInlSrat == 0) ) then
		wNAiotWH = 0.0 		else		wNAiotWH = - wNMintWH - (tNSetDtH -tNSetDtE - tNRessDt ) / uDptWH 		endif
! 	Total_abiotic/microbial_Si_sio2_flux_in_water
		if ( (aInlSrat == 0) ) then
		wSiio2WH = 0.0 		else		wSiio2WH = wSiintWH + (1.0 - fRefrDeS) * tSiMiDtS / uDptWH 		endif
! 	Total_abiotic/microbial_Si_detritus_flux_in_water
		if ( (aInlSrat == 0) ) then
		wSibitWH = 0.0 		else		wSibitWH = - wSiintWH - (tSiSeDtH -tSiSeDtE - tSiessDt) / uDptWH 		endif
! 	Total_abiotic/microbial_DW_detritus_flux_in_water
		if ( (aInlSrat == 0) ) then
		wDAiotWH = 0.0 		else		wDAiotWH = (- tDSetDtH + tDSetDtE + tDRessDt ) / uDptWH - wDMintWH 		endif
! 	Total_abiotic/microbial_DW_inorganic_matter_flux_in_water
		if ( (aInlSrat == 0) ) then
		wDAioMWH = 0.0 		else		wDAioMWH = (uDEroIMW * fHErosIM - tDSetIMH+ tDSetIME + tDRessIM) / uDptWH 		endif
! 	Total_abiotic/microbial_DW_inorganic_matter_flux_in_water
		if ( ( aInlSrat == 1 ) ) then
		wDAioMWE = (uDEroIMW * (1- fHErosIM) - tDSetIME ) / uDptWE 		else		wDAioMWE = (uDEroIMW * (1- fHErosIM) - tDSetIME + tDRessIM) / uDptWE 		endif
! 	Total_abiotic/microbial_DW_detritus_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wDAiotWE = (- tDSetDtE ) / uDptWE - wDMintWE 		else		wDAiotWE = (- tDSetDtE + tDRessDt ) / uDptWE - wDMintWE 		endif
! 	Total_abiotic/microbial_DW_inorganic_matter_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tDAbiIMS = uDEroIMS + tDSetIMH - tDRessIM 		else		tDAbiIMS = uDEroIMS + tDSetIME - tDRessIM 		endif
! 	Total_abiotic/microbial_DW_detritus_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tDAbiDtS = tDSetDtH - tDRessDt - tDMinDtS 		else		tDAbiDtS = tDSetDtE - tDRessDt - tDMinDtS 		endif
! 	Total_abiotic/microbial_DW_humus_flux_in_sediment
		tDAioumS = uDErosOM + fRefrDeS * tDMinDtS - tDMinumS
! 	Total_abiotic/microbial_DW_flux_for_mass_balance_check
		tDAiootT = cDEroTot - wDMintWE * uDptWE - wDMintWH * uDptWH - (1.0 - fRefrDeS) *&
		& tDMinDtS - tDMinumS
! 	Total_abiotic/microbial_O2_flux_in_water
		if ( (aInlSrat == 0) ) then
		wO2AboWE = tO2AerE / uDptWE - wO2intWE - wO2NirWE - (tO2MiDtS + tO2NitrS) / uDpt&
		&WE 		else		wO2AboWE = tO2AerE / uDptWE - wO2intWE - wO2NirWE 		endif
! 	Total_abiotic/microbial_O2_flux_in_water
		wO2AbioM = tO2AerM / sDepthWM - wO2intWM - wO2NirWM - (tO2intSM + tO2NirSM) / sD&
		&epthWM
! 	Total_abiotic/microbial_P_detritus_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wPAiotWE = - wPMintWE - (tPSetDtE ) / uDptWE 		else		wPAiotWE = - wPMintWE - (tPSetDtE - tPRessDt) / uDptWE 		endif
! 	Total_abiotic/microbial_dissolved_P_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wPAio4WE = wPMintWE - wPSrpMWE - tPIfP4WE / uDptWE + tPDifO4E / uDptWE 		else		wPAio4WE = wPMintWE - wPSrpMWE - tPIfP4WE / uDptWE + tPDifO4E / uDptWE + tPRsuPO&
		&4 / uDptWE 		endif
! 	Total_abiotic/microbial_P_absorbed_onto_inorganic_matter_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wPAioMWE = (- tPSetIME ) / uDptWE + wPSrpMWE 		else		wPAioMWE = (- tPSetIME + tPRsuAIM ) / uDptWE + wPSrpMWE 		endif
! 	Total_abiotic/microbial_P_detritus_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tPAbiDtS = - tPMinDtS + tPSetDtH - tPRessDt 		else		tPAbiDtS = - tPMinDtS + tPSetDtE - tPRessDt 		endif
! 	Total_abiotic/microbial_P_humus_flux_in_sediment
		tPAioumS = uPErosOM + fRefrDeS * tPMinDtS - tPMinumS
! 	Total_abiotic/microbial_dissolved_P_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tPAioO4S = tPIfP4WH - tPInfO4S + (1.0-fRefrDeS) * tPMinDtS + tPMinumS - tPSorIMS&
		& - tPRsuPO4 - tPDifO4H - tPDroPO4 - tPChePO4 		else		tPAioO4S = tPIfP4WE - tPInfO4S + (1.0-fRefrDeS) * tPMinDtS + tPMinumS - tPSorIMS&
		& - tPRsuPO4 - tPDifO4E - tPDroPO4 - tPChePO4 		endif
! 	Total_abiotic/microbial_P_absorbed_onto_inorganic_matter_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tPAioIMS = tPSetIMH - tPRsuAIM + tPSorIMS 		else		tPAioIMS = tPSetIME - tPRsuAIM + tPSorIMS 		endif
! 	Total_abiotic/microbial_N_NH4_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wNAio4WE = wNMintWE - wNNitrWE - tNIfN4WE / uDptWE + (tNDifH4E ) / uDptWE 		else		wNAio4WE = wNMintWE - wNNitrWE - tNIfN4WE / uDptWE + (tNDifH4E + tNRsuNH4) / uDp&
		&tWE 		endif
! 	Total_abiotic/microbial_N_NO3_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wNAio3WE = wNNitrWE - wNDentWE + (tNDifO3E - tNIfN3WE) / uDptWE 		else		wNAio3WE = wNNitrWE - wNDentWE + (tNDifO3E + tNRsuNO3 - tNIfN3WE) / uDptWE 		endif
! 	Total_abiotic/microbial_N_detritus_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wNAiotWE = - wNMintWE - (tNSetDtE) / uDptWE 		else		wNAiotWE = - wNMintWE - (tNSetDtE - tNRessDt ) / uDptWE 		endif
! 	Total_abiotic/microbial_N_NH4_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tNAioH4S = tNIfN4WH - tNInfH4S + (1.0-fRefrDeS) * tNMinDtS + tNMinumS - tNRsuNH4&
		& - tNDifH4H - tNDroNH4 - tNNitrS 		else		tNAioH4S = tNIfN4WE - tNInfH4S + (1.0-fRefrDeS) * tNMinDtS + tNMinumS - tNRsuNH4&
		& - tNDifH4E - tNDroNH4 - tNNitrS 		endif
! 	Total_abiotic/microbial_N_NO3_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tNAioO3S = tNIfN3WH - tNInfO3S + tNNitrS - tNDenitS - tNRsuNO3 - tNDifO3H - tNDr&
		&oNO3 		else		tNAioO3S = tNIfN3WE - tNInfO3S + tNNitrS - tNDenitS - tNRsuNO3 - tNDifO3E - tNDr&
		&oNO3 		endif
! 	Total_abiotic/microbial_N_detritus_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tNAbiDtS = - tNMinDtS + tNSetDtH - tNRessDt 		else		tNAbiDtS = - tNMinDtS + tNSetDtE - tNRessDt 		endif
! 	Total_abiotic/microbial_N_humus_flux_in_sediment
		tNAioumS = uNErosOM + fRefrDeS * tNMinDtS - tNMinumS
! 	Total_abiotic/microbial_N_flux_for_mass_balance_check
		tNAiootT = uNErosOM - tNDenitS - wNDentWE * uDptWE - wNDentWH * uDptWH - tNInfH4&
		&S - tNInfO3S - tNDroNO3 - tNDroNH4
! 	Total_abiotic/microbial_Si_sio2_flux_in_water
		if ( (aInlSrat == 0 ) ) then
		wSiio2WE = wSiintWE + (1.0 - fRefrDeS) * tSiMiDtS / uDptWE 		else		wSiio2WE = wSiintWE 		endif
! 	Total_abiotic/microbial_Si_detritus_flux_in_water
		if ( (aInlSrat == 1 ) ) then
		wSibitWE = - wSiintWE - (tSiSeDtE ) / uDptWE 		else		wSibitWE = - wSiintWE - (tSiSeDtE - tSiessDt) / uDptWE 		endif
! 	Total_abiotic/microbial_Si_detritus_flux_in_sediment
		if ( ( aInlSrat == 1 ) ) then
		tSibiDtS = - tSiMiDtS + tSiSeDtH - tSiessDt 		else		tSibiDtS = - tSiMiDtS + tSiSeDtE - tSiessDt 		endif
! 	Submerged_production
		tDPdSegE = bfSubegE * tDPodegE
! 	Mortality_flux_becoming_water_detritus
		tDMVegWE = fDeWMV * (1.0 - bfRtVeg) * tDMVegE
! 	Mortality_flux_becoming_sediment_detritus
		tDMVegSE = tDMVegE - tDMVegWE
! 	Derivative_of_vegetation_biomass
		if ( InclV == 1 ) then
		tDBedVeg = tDMigVeg + tDPodegE + tDPodegH- tDResVeg - tDMVegE - tDMVegH - tDGazg&
		&Bi - tDManVeg 		else		tDBedVeg = 0.0 		endif
! 	Vegetation_O2_production
		tO2roegE = molO2olC * cCPerDW * tDPodegE
! 	Submerged_O2_respiration
		tO2spgWE = molO2olC * cCPerDW * bfSubegE * tDResVeg * aCoO2ODE
! 	O2_transport_to_roots
		tO2odgSE = min (tO2esegS, tO2roegE)
! 	O2_used_for_vegetation_production
		tO2odgWE = min ( tO2roegE - tO2odgSE, bfSubVeg * tO2roegE) 
! 	Total_sediment_O2_flux_in_vegetation_module
		tO2BedS = tO2odgSH + tO2odgSE - tO2esegS + tO2NOegS
! 	O2_production_to_water_due_to_NO3_uptake_by_macrophytes
		tO2O3gWE = O2PerNO3 * molO2olN * bfSubVeg * tNUO3gWE
! 	P_mortality_flux_of_vegetation
		tPMVegE = rPDVeg * tDMVegE
! 	Mortality_flux_of_vegetation_becoming_dissolved_P
		tPMegO4E = fDissMV * tPMVegE
! 	Mortality_flux_of_vegetation_becoming_dissolved_P_in_sediment
		tPMeg4SE = bfRtVeg * tPMegO4E
! 	Mortality_flux_of_vegetation_becoming_dissolved_P_in_water
		tPMeg4WE = tPMegO4E - tPMeg4SE
! 	Mortality_flux_of_vegetation_becoming_detritus_P
		tPMVeDtE = tPMVegE - tPMegO4E
! 	Mortality_flux_of_vegetation_becoming_detritus_P_in_water
		tPMegtWE = fDeWMV * (1.0 - bfRtVeg) * tPMVeDtE
! 	Mortality_flux_of_vegetation_becoming_detritus_P_in_sediment
		tPMegtSE = tPMVeDtE - tPMegtWE
! 	Total_vegetation_P_flux_in_bed_module
		if ( InclV == 1 ) then
		tPBedVeg = tPMigVeg + tPUVegE + tPUVegH - tPExcVeg - tPMVegE - tPMVegH - tPGazgB&
		&i - tPManVeg 		else		tPBedVeg = 0.0 		endif
! 	Mortality_flux_of_vegetation_becoming_dissolved_N_in_water
		tNMeg4WE = tNMegH4E - tNMeg4SE
! 	Mortality_flux_of_vegetation_becoming_detritus_N
		tNMVeDtE = tNMVegE - tNMegH4E
! 	Mortality_flux_of_vegetation_becoming_detritus_N_in_water
		tNMegtWE = fDeWMV * (1.0 - bfRtVeg) * tNMVeDtE
! 	Mortality_flux_of_vegetation_becoming_detritus_N_in_sediment
		tNMegtSE = tNMVeDtE - tNMegtWE
! 	Total_vegetation_N_flux_in_bed_module
		if ( InclV == 1 ) then
		tNBedVeg = tNMigVeg + tNUVeg - tNExcVeg - tNMVegE - tNMVegH - tNGazgBi - tNManVe&
		&g 		else		tNBedVeg = 0.0 		endif
! 	Total_DW_flux_from_Vegetation_module_to_water_detritus
		if ( (InclV == 1) ) then
		wDBedtWE = (tDMVegWE + tDEBi) / uDptWE 		else		wDBedtWE = 0.0 		endif
! 	Total_DW_flux_from_Vegetation_module_to_sediment_detritus
		if ( ( InclV == 1) ) then
		tDBedtSE = tDMVegSE 		else		tDBedtSE = 0.0 		endif
! 	Total_P_flux_from_Vegetation_module_to_PO4_in_water
		if ( (InclV == 0) ) then
		wPBdP4WE = 0.0 		else		wPBdP4WE = (- tPUVegWE + tPEcregW * uVeHehtE / uVeHeght + tPMeg4WE + tPEBiPO4) /&
		&uDptWE 		endif
! 	Total_P_flux_from_Vegetation_module_to_water_detritus
		if ( (InclV == 1) ) then
		wPBedtWE = (tPMegtWE + tPEBiDt) / uDptWE 		else		wPBedtWE = 0.0 		endif
! 	Total_P_flux_from_Vegetation_module_to_pore_water_PO4
		if ( ( InclV == 1) ) then
		tPBedO4S = - tPUVegS + tPEcregS + tPMeg4SE + tPMeg4SH 		else		tPBedO4S = 0.0 		endif
! 	Total_P_flux_from_Vegetation_module_to_sediment_detritus
		if ( (InclV == 1) ) then
		tPBedDtS = tPMegtSE + tPMegtSH 		else		tPBedDtS = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NH4_in_water
		if ( ( InclV == 1 ) ) then
		wNBdN4WE = (- tNUH4gWE + tNEcregW * (uVeHehtE /uVeHeght ) + tNMeg4WE + tNEBiNH4)&
		& / uDptWE 		else		wNBdN4WE = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NO3_in_water
		if ( ( InclV == 1) ) then
		wNBdN3WE = - tNUO3gWE / uDptWE 		else		wNBdN3WE = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_water_detritus
		if ( (InclV == 1) ) then
		wNBedtWE = (tNMegtWE + tNEBiDt) / uDptWE 		else		wNBedtWE = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NH4_in_pore_water
		if ( ( InclV == 1) ) then
		tNBedH4S = - tNUH4egS + tNEcregS + tNMeg4SE + tNMeg4SH 		else		tNBedH4S = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_NO3_in_pore_water
		if ( ( InclV == 1 ) ) then
		tNBedO3S = - tNUO3egS 		else		tNBedO3S = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module_to_sediment_detritus
		if ( ( InclV == 0) ) then
		tNBedDtS = 0.0 		else if ( (aInlSrat == 1) ) then		tNBedDtS = tNMegtSE + tNMegtSH 		else		tNBedDtS = tNMegtSE 		endif
! 	Total_water_O2_flux_in_vegetation_module
		if ( ( InclV == 1 ) ) then
		tO2BedWE = tO2odgWE - tO2spgWE + tO2O3gWE 		else		tO2BedWE = 0.0 		endif
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPxCueE = max(0.0,cVPMxlue * uFuPrueE *(cPDBleMx - rPDBleWE) /(cPDBleMx - cPDBl&
		&eMn))
! 	P_uptake_rate_of_Algae
		aVPUBueE = oPO4WE * aVPxCueE /(aVPxCueE / cAfPUlue + oPO4WE)
! 	P_uptake_Algae
		wPUBlueE = aVPUBueE * oDBlueWE
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNxCueE = max(0.0,cVNMxlue * uFuPrueE * (cNDBleMx - rNDBleWE) /(cNDBleMx - cNDB&
		&leMn))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUBueE = aVNxCueE / cAfNUlue
! 	N_uptake_rate_of_Algae
		aVNUBueE = oNDissWE * aVNxCueE /(ahNUBueE + oNDissWE)
! 	N_uptake_Algae
		wNUBlueE = aVNUBueE * oDBlueWE
! 	Fraction_ammonium_uptake_by_Algae
		afN4UueE = oNH4WE * oNO3WE /((ahNUBueE + oNH4WE) *(ahNUBueE + oNO3WE)) + oNH4WE &
		&* ahNUBueE /((oNH4WE + oNO3WE) *(ahNUBueE + oNO3WE))
! 	Ammonium_uptake_by_Algae
		wNUH4ueE = afN4UueE * wNUBlueE
! 	Nitrate_uptake_by_Algae
		wNUO3ueE = wNUBlueE - wNUH4ueE
! 	Droop_function(P)_for_Algae
		aPLmBueE = max(0.0,(1.0 - cPDBleMn / rPDBleWE) * cPDBleMx /(cPDBleMx - cPDBleMn)&
		&)
! 	Droop_function(N)_for_Algae
		aNLmBueE = max(0.0,(1.0 - cNDBleMn / rNDBleWE) * cNDBleMx /(cNDBleMx - cNDBleMn)&
		&)
! 	Silica_dependence_of_growth_rate
		aSiimueE = oSiO2WE /(hSisslue + oSiO2WE)
! 	Light_function
		aLLmBueE = Useeelue *(exp(1.0) /(aExtCefE * uDptWE) *(exp(- aLPARotE /(cLORelue &
		&* uFuPrueE)) - exp(- uLPARurf /(cLORelue * uFuPrueE)))) +(1.0 - Useeelue) *(1.0 &
		&/(aExtCefE * uDptWE) * log((1.0 + uLPARurf / (hLReflue * uFuPrueE)) / (1.0 + aLP&
		&ARotE /(hLReflue * uFuPrueE))))
! 	Growth_rate_at_current_light_AND_temp
		aMumLueE = ufDay *(1.0 - afCSuVeg) * aLLmBueE * uMuxTueE
! 	Nutrient_limitation_function_of_Algae
		aNuimueE = min(aPLmBueE , (min(aNLmBueE,aSiimueE)))
! 	Growth_rate
		aMuBlueE = aNuimueE * aMumLueE
! 	Assimilation_Algae
		wDAsBueE = aMuBlueE*oDBlueWE
! 	Chlorophyll-a/DW_ratio_Algae
		rChDBueE = cChBleMx -(cChBleMx - cChBleMn) * aLLmBueE
! 	Chlorophyll-a_conc
		oChlBlE = mgPerg * rChDBueE * oDBlueWE
! 	Specific_extinction_per_unit_chlorophyll-a
		aExChueE = cExSplue / rChDBueE
! 	Respiration_of_Algae_in_water
		wDRpBeWE = ukDpTueE * oDBlueWE
! 	Algae_grazing_loss
		wDLssueE = ukLsTueE * oDBlueWE
! 	Mortality_in_water
		wDMBleWE = kMBlueW * oDBlueWE
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDecueE = ukDpTueE + ukLsTueE + kMBlueW +(uCoSeueE * uFuTmetE) / uDptWE
! 	P_excretion_Algae_in_water
		wPErBeWE = (rPDBleWE * 2.0 )/(cPDBleMx + rPDBleWE) * rPDBleWE * wDRpBeWE 
! 	Algae_grazing_loss
		wPLssueE = rPDBleWE * wDLssueE
! 	Mortality_Algae_in_water
		wPMBleWE = kMBlueW * oPBlueWE
! 	N_excretion_Algae_in_water
		wNErBeWE = (rNDBleWE *2.0)/(cNDBleMx + rNDBleWE) * rNDBleWE * wDRpBeWE 
! 	Algae_grazing_loss
		wNLssueE = rNDBleWE * wDLssueE
! 	Mortality_Algae_in_water
		wNMBleWE = kMBlueW * oNBlueWE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 1 ) ) then
		wDPmBeWE = wDAsBueE - wDRpBeWE - wDLssueE - wDMBleWE -(tDStBueE ) / uDptWE 		else		wDPmBeWE = wDAsBueE - wDRpBeWE - wDLssueE - wDMBleWE -(tDStBueE - tDRsulue) / uD&
		&ptWE 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wPPmBeWE = wPUBlueE - wPErBeWE - wPLssueE - wPMBleWE -(tPStBueE ) / uDptWE 		else		wPPmBeWE = wPUBlueE - wPErBeWE - wPLssueE - wPMBleWE -(tPStBueE - tPRsulue) / uD&
		&ptWE 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wNPmBeWE = wNUBlueE - wNErBeWE - wNLssueE - wNMBleWE -(tNStBueE ) / uDptWE 		else		wNPmBeWE = wNUBlueE - wNErBeWE - wNLssueE - wNMBleWE -(tNStBueE - tNRsulue) / uD&
		&ptWE 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tDPimueS = tDStBueE - tDRsulue - tDMBlueS - tDRspueS 		else		tDPimueS = tDStBueH - tDRsulue - tDMBlueS - tDRspueS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tPPimueS = tPStBueE - tPRsulue - tPMBlueS - tPEcrueS 		else		tPPimueS = tPStBueH - tPRsulue - tPMBlueS - tPEcrueS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( (aInlSrat == 0) ) then
		tNPimueS = tNStBueE - tNRsulue - tNMBlueS - tNEcrueS 		else		tNPimueS = tNStBueH - tNRsulue - tNMBlueS - tNEcrueS 		endif
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPxCenE = max(0.0,cVPMxren * uFuPrenE *(cPDGrnMx - rPDGrnWE) /(cPDGrnMx - cPDGr&
		&nMn))
! 	P_uptake_rate_of_Algae
		aVPUGenE = oPO4WE * aVPxCenE /(aVPxCenE / cAfPUren + oPO4WE)
! 	P_uptake_Algae
		wPUGrenE = aVPUGenE * oDGrenWE
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNxCenE = max(0.0,cVNMxren * uFuPrenE * (cNDGrnMx - rNDGrnWE) /(cNDGrnMx - cNDG&
		&rnMn))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUGenE = aVNxCenE / cAfNUren
! 	N_uptake_rate_of_Algae
		aVNUGenE = oNDissWE * aVNxCenE /(ahNUGenE + oNDissWE)
! 	N_uptake_Algae
		wNUGrenE = aVNUGenE * oDGrenWE
! 	Fraction_ammonium_uptake_by_Algae
		afN4UenE = oNH4WE * oNO3WE /((ahNUGenE + oNH4WE) *(ahNUGenE + oNO3WE)) + oNH4WE &
		&* ahNUGenE /((oNH4WE + oNO3WE) *(ahNUGenE + oNO3WE))
! 	Ammonium_uptake_by_Algae
		wNUH4enE = afN4UenE * wNUGrenE
! 	Nitrate_uptake_by_Algae
		wNUO3enE = wNUGrenE - wNUH4enE
! 	Droop_function(P)_for_Algae
		aPLmGenE = max(0.0,(1.0 - cPDGrnMn / rPDGrnWE) * cPDGrnMx /(cPDGrnMx - cPDGrnMn)&
		&)
! 	Droop_function(N)_for_Algae
		aNLmGenE = max(0.0,(1.0 - cNDGrnMn / rNDGrnWE) * cNDGrnMx /(cNDGrnMx - cNDGrnMn)&
		&)
! 	Silica_dependence_of_growth_rate
		aSiimenE = oSiO2WE /(hSissren + oSiO2WE)
! 	Light_function
		aLLmGenE = Useeeren *(exp(1.0) /(aExtCefE * uDptWE) *(exp(- aLPARotE /(cLOReren &
		&* uFuPrenE)) - exp(- uLPARurf /(cLOReren * uFuPrenE)))) +(1.0 - Useeeren) *(1.0 &
		&/(aExtCefE * uDptWE) * log((1.0 + uLPARurf / (hLRefren * uFuPrenE)) / (1.0 + aLP&
		&ARotE /(hLRefren * uFuPrenE))))
! 	Growth_rate_at_current_light_AND_temp
		aMumLenE = ufDay *(1.0 - afCSuVeg) * aLLmGenE * uMuxTenE
! 	Nutrient_limitation_function_of_Algae
		aNuimenE = min(aPLmGenE , (min(aNLmGenE,aSiimenE)))
! 	Growth_rate
		aMuGrenE = aNuimenE * aMumLenE
! 	Assimilation_Algae
		wDAsGenE = aMuGrenE*oDGrenWE
! 	Chlorophyll-a/DW_ratio_Algae
		rChDGenE = cChGrnMx -(cChGrnMx - cChGrnMn) * aLLmGenE
! 	Chlorophyll-a_conc
		oChlGrE = mgPerg * rChDGenE * oDGrenWE
! 	Specific_extinction_per_unit_chlorophyll-a
		aExChenE = cExSpren / rChDGenE
! 	Respiration_of_Algae_in_water
		wDRpGnWE = ukDpTenE * oDGrenWE
! 	Algae_grazing_loss
		wDLssenE = ukLsTenE * oDGrenWE
! 	Mortality_in_water
		wDMGrnWE = kMGrenW * oDGrenWE
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDecenE = ukDpTenE + ukLsTenE + kMGrenW +(uCoSeenE * uFuTmetE) / uDptWE
! 	P_excretion_Algae_in_water
		wPErGnWE = (2.0 *rPDGrnWE) /(cPDGrnMx + rPDGrnWE) * rPDGrnWE * wDRpGnWE 
! 	Algae_grazing_loss
		wPLssenE = rPDGrnWE * wDLssenE
! 	Mortality_Algae_in_water
		wPMGrnWE = kMGrenW * oPGrenWE
! 	N_excretion_Algae_in_water
		wNErGnWE = (2.0 * rNDGrnWE) /(cNDGrnMx + rNDGrnWE) * rNDGrnWE * wDRpGnWE
! 	Algae_grazing_loss
		wNLssenE = rNDGrnWE * wDLssenE
! 	Mortality_Algae_in_water
		wNMGrnWE = kMGrenW * oNGrenWE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 1 ) ) then
		wDPmGnWE = wDAsGenE - wDRpGnWE - wDLssenE - wDMGrnWE -(tDStGenE ) / uDptWE 		else		wDPmGnWE = wDAsGenE - wDRpGnWE - wDLssenE - wDMGrnWE -(tDStGenE - tDRsuren) / uD&
		&ptWE 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wPPmGnWE = wPUGrenE - wPErGnWE - wPLssenE - wPMGrnWE -(tPStGenE ) / uDptWE 		else		wPPmGnWE = wPUGrenE - wPErGnWE - wPLssenE - wPMGrnWE -(tPStGenE - tPRsuren) / uD&
		&ptWE 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wNPmGnWE = wNUGrenE - wNErGnWE - wNLssenE - wNMGrnWE -(tNStGenE ) / uDptWE 		else		wNPmGnWE = wNUGrenE - wNErGnWE - wNLssenE - wNMGrnWE -(tNStGenE - tNRsuren) / uD&
		&ptWE 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tDPimenS = tDStGenE - tDRsuren - tDMGrenS - tDRspenS 		else		tDPimenS = tDStGenH - tDRsuren - tDMGrenS - tDRspenS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tPPimenS = tPStGenE - tPRsuren - tPMGrenS - tPEcrenS 		else		tPPimenS = tPStGenH - tPRsuren - tPMGrenS - tPEcrenS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tNPimenS = tNStGenE - tNRsuren - tNMGrenS - tNEcrenS 		else		tNPimenS = tNStGenH - tNRsuren - tNMGrenS - tNEcrenS 		endif
! 	Maximum_P_uptake_rate_of_Algae_corrected_for_P/D_ratio
		aVPaxDiE = max(0.0,cVPUMxDi * uFumPDiE *(cPDDiMx - rPDDiWE) /(cPDDiMx - cPDDiMn)&
		&)
! 	P_uptake_rate_of_Algae
		aVPUDiE = oPO4WE * aVPaxDiE /(aVPaxDiE / cAfPUDi + oPO4WE)
! 	P_uptake_Algae
		wPUDiE = aVPUDiE * oDDiWE
! 	Maximum_N_uptake_rate_of_Algae_corrected_for_N/D_ratio
		aVNaxDiE = max(0.0,cVNUMxDi * uFumPDiE * (cNDDiMx - rNDDiWE) /(cNDDiMx - cNDDiMn&
		&))
! 	Half-sat_dissolved_nitrogen_in_water_for_uptake_by_Algae
		ahNUDiE = aVNaxDiE / cAfNUDi
! 	N_uptake_rate_of_Algae
		aVNUDiE = oNDissWE * aVNaxDiE /(ahNUDiE + oNDissWE)
! 	N_uptake_Algae
		wNUDiE = aVNUDiE * oDDiWE
! 	Fraction_ammonium_uptake_by_Algae
		afNH4DiE = oNH4WE * oNO3WE /((ahNUDiE + oNH4WE) *(ahNUDiE + oNO3WE)) + oNH4WE * &
		&ahNUDiE /((oNH4WE + oNO3WE) *(ahNUDiE + oNO3WE))
! 	Ammonium_uptake_by_Algae
		wNUNHDiE = afNH4DiE * wNUDiE
! 	Nitrate_uptake_by_Algae
		wNUNODiE = wNUDiE - wNUNHDiE
! 	Droop_function(P)_for_Algae
		aPLimDiE = max(0.0,(1.0 - cPDDiMn / rPDDiWE) * cPDDiMx /(cPDDiMx - cPDDiMn))
! 	Droop_function(N)_for_Algae
		aNLimDiE = max(0.0,(1.0 - cNDDiMn / rNDDiWE) * cNDDiMx /(cNDDiMx - cNDDiMn))
! 	Silica_dependence_of_growth_rate
		aSiLiDiE = oSiO2WE /(hSiAssDi + oSiO2WE)
! 	Light_function
		aLLimDiE = UseteeDi *(exp(1.0) /(aExtCefE * uDptWE) *(exp(- aLPARotE /(cLOtRfDi &
		&* uFumPDiE)) - exp(- uLPARurf /(cLOtRfDi * uFumPDiE)))) +(1.0 - UseteeDi) *(1.0 &
		&/(aExtCefE * uDptWE) * log((1.0 + uLPARurf / (hLRefDi * uFumPDiE)) / (1.0 + aLPA&
		&RotE /(hLRefDi * uFumPDiE))))
! 	Growth_rate_at_current_light_AND_temp
		aMuTmDiE = ufDay *(1.0 - afCSuVeg) * aLLimDiE * uMuaxDiE
! 	Nutrient_limitation_function_of_Algae
		aNuLiDiE = min(aPLimDiE , (min(aNLimDiE,aSiLiDiE)))
! 	Growth_rate
		aMuDiE = aNuLiDiE * aMuTmDiE
! 	Assimilation_Algae
		wDAssDiE = aMuDiE*oDDiWE
! 	Chlorophyll-a/DW_ratio_Algae
		rChDDiE = cChDDiMx -(cChDDiMx - cChDDiMn) * aLLimDiE
! 	Chlorophyll-a_conc
		oChlDiE = mgPerg * rChDDiE * oDDiWE
! 	Specific_extinction_per_unit_chlorophyll-a
		aExtCDiE = cExtSpDi / rChDDiE
! 	Respiration_of_Algae_in_water
		wDRspiWE = ukDspDiE * oDDiWE
! 	Algae_grazing_loss
		wDLosDiE = ukLssDiE * oDDiWE
! 	Mortality_in_water
		wDMDiWE = kMDiW * oDDiWE
! 	Total_loss_rate_of_algae_in_water(excl_dilution)
		ukDDeDiE = ukDspDiE + ukLssDiE + kMDiW +(uCoVSDiE * uFuTmetE) / uDptWE
! 	P_excretion_Algae_in_water
		wPEcriWE = (2.0 * rPDDiWE) /(cPDDiMx + rPDDiWE) * rPDDiWE * wDRspiWE 
! 	Algae_grazing_loss
		wPLosDiE = rPDDiWE * wDLosDiE
! 	Mortality_Algae_in_water
		wPMDiWE = kMDiW * oPDiWE
! 	N_excretion_Algae_in_water
		wNEcriWE = (2.0 * rNDDiWE) /(cNDDiMx + rNDDiWE) * rNDDiWE * wDRspiWE 
! 	Algae_grazing_loss
		wNLosDiE = rNDDiWE * wDLosDiE
! 	Mortality_Algae_in_water
		wNMDiWE = kMDiW * oNDiWE
! 	Total_PRIM_flux_to_algae_in_water
		if ( (aInlSrat == 1 ) ) then
		wDPimiWE = wDAssDiE - wDRspiWE - wDLosDiE - wDMDiWE -(tDSetDiE ) / uDptWE 		else		wDPimiWE = wDAssDiE - wDRspiWE - wDLosDiE - wDMDiWE -(tDSetDiE - tDRessDi) / uDp&
		&tWE 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wPPimiWE = wPUDiE - wPEcriWE - wPLosDiE - wPMDiWE -(tPSetDiE ) / uDptWE 		else		wPPimiWE = wPUDiE - wPEcriWE - wPLosDiE - wPMDiWE -(tPSetDiE - tPRessDi) / uDptW&
		&E 		endif
! 	Total_PRIM_flux_to_Algae
		if ( (aInlSrat == 1 ) ) then
		wNPimiWE = wNUDiE - wNEcriWE - wNLosDiE - wNMDiWE -(tNSetDiE ) / uDptWE 		else		wNPimiWE = wNUDiE - wNEcriWE - wNLosDiE - wNMDiWE -(tNSetDiE - tNRessDi) / uDptW&
		&E 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tDPriDiS = tDSetDiE - tDRessDi - tDMDiS - tDResDiS 		else		tDPriDiS = tDSetDiH - tDRessDi - tDMDiS - tDResDiS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tPPriDiS = tPSetDiE - tPRessDi - tPMDiS - tPExcDiS 		else		tPPriDiS = tPSetDiH - tPRessDi - tPMDiS - tPExcDiS 		endif
! 	Total_flux_from_PRIM_module_to_sediment_Algae
		if ( ( aInlSrat == 0) ) then
		tNPriDiS = tNSetDiE - tNRessDi - tNMDiS - tNExcDiS 		else		tNPriDiS = tNSetDiH - tNRessDi - tNMDiS - tNExcDiS 		endif
! 	Total_chlorophyll-a
		oChlaE = oChlDiE + oChlGrE + oChlBlE
! 	Total_chlorophyll-a
		if ( ( aInlSrat == 1 ) ) then
		oChla = (oChlaE * uDptWE + oChlaH * uDptWH ) / sDepthW 		else		oChla = oChlaE 		endif
! 	Integrated_PI_curve_for_phytoplankton_growth
		aLPIE =  (uLPARurf- aLPRBtEO + uhLVeg* log((1.0 +aLPRBtEO/ uhLVeg)  /(1.0 + uLPA&
		&Rurf  / uhLVeg) + NearZero  ) ) / uDptWE
! 	Light_at_measurement_depth
		aLMeasE = uLPARurf * exp(- aExCofOE *cDeMeChl)
! 	Total_chlorophyll-a_at_measurement_depth
		oChlMeE = (aLMeasE  )/ aLPIE *oChlaE
! 	Total_algal_growth
		wDAsPytE = wDAssDiE + wDAsGenE + wDAsBueE
! 	Total_algal_respiration_in_water
		wDRpPtWE = wDRspiWE + wDRpGnWE + wDRpBeWE
! 	Total_algal_mortality_in_water
		wDMPhtWE = wDMDiWE + wDMGrnWE + wDMBleWE
! 	Total_phytoplankton_sedimentation
		tDStPytE = tDSetDiE + tDStGenE + tDStBueE
! 	Total_phytoplankton_grazing_loss
		wDLssytE = wDLosDiE + wDLssenE + wDLssueE
! 	Total_of_PRIM_processes_of_algae_in_water
		wDPmPtWE = wDPimiWE + wDPmGnWE + wDPmBeWE
! 	Total_P_uptake_phytoplankton
		wPUPhytE = wPUDiE + wPUGrenE + wPUBlueE
! 	Total_P_excretion_phytoplankton_in_water
		wPErPtWE = wPEcriWE + wPErGnWE + wPErBeWE
! 	Total_P_mortality_phytoplankton_in_water
		wPMPhtWE = wPMDiWE + wPMGrnWE + wPMBleWE
! 	Total_sedimentation_of_algae
		tPStPytE = tPSetDiE + tPStGenE + tPStBueE
! 	Total_grazing_loss
		wPLssytE = wPLosDiE + wPLssenE + wPLssueE
! 	Total_of_PRIM_processes_of_algae_in_water
		wPPmPtWE = wPPimiWE + wPPmGnWE + wPPmBeWE
! 	Total_N_uptake_phytoplankton
		wNUPhytE = wNUDiE + wNUGrenE + wNUBlueE
! 	Total_ammonium-N_uptake_phytoplankton
		wNUH4ytE = wNUNHDiE + wNUH4enE + wNUH4ueE
! 	Total_nitrate-N_uptake_phytoplankton
		wNUO3ytE = wNUNODiE + wNUO3enE + wNUO3ueE
! 	Total_N_excretion_phytoplankton_in_water
		wNErPtWE = wNEcriWE + wNErGnWE + wNErBeWE
! 	Total_N_mortality_phytoplankton_in_water
		wNMPhtWE = wNMDiWE + wNMGrnWE + wNMBleWE
! 	Total_sedimentation_of_algae
		tNStPytE = tNSetDiE + tNStGenE + tNStBueE
! 	Total_grazing_loss
		wNLssytE = wNLosDiE + wNLssenE + wNLssueE
! 	Total_of_PRIM_processes_of_algae_in_water
		wNPmPtWE = wNPimiWE + wNPmGnWE + wNPmBeWE
! 	Total_flux_of_algae_on_bottom
		tDPimytS = tDPriDiS + tDPimenS + tDPimueS
! 	Total_flux_of_algae_on_bottom
		tPPimytS = tPPriDiS + tPPimenS + tPPimueS
! 	Total_flux_of_algae_on_bottom
		tNPimytS = tNPriDiS + tNPimenS + tNPimueS
! 	Diatoms_silica_uptake
		wSiUDiE = cSiDDi * wDAssDiE
! 	Si_excretion
		wSixciWE = cSiDDi * wDRspiWE 
! 	Diatom_grazing_loss
		wSiosDiE = cSiDDi * wDLosDiE
! 	Diatoms_mortality_in_water
		wSiMDiWE = cSiDDi * wDMDiWE
! 	Diatoms_sedimentation
		tSiSeDiE = cSiDDi * tDSetDiE
! 	Total_Si_flux_to_sed_diatoms_in_PRIM_module
		if ( (aInlSrat == 1 ) ) then
		wSiriiWH = wSiUDiH - tSiSeDiH / uDptWH - wSixciWH - wSiMDiWH - wSiosDiH 		else		wSiriiWH = 0.0 		endif
! 	Total_Si_flux_to_sed_diatoms_in_PRIM_module
		if ( (aInlSrat == 1 ) ) then
		wSiriiWE = wSiUDiE - tSiSeDiE / uDptWE - wSixciWE - wSiMDiWE - wSiosDiE 		else		wSiriiWE = wSiUDiE - tSiSeDiE / uDptWE - wSixciWE - wSiMDiWE - wSiosDiE + tSiess&
		&Di / uDptWE 		endif
! 	C-phycocyanin/DW-ratio_blue-greens
		rCyDBueE = cCyBleMx -(cCyBleMx - cCyBleMn) * aLLmBueE
! 	C-phycocyanin
		oCyanE = rCyDBueE * oDBlueWE * mgPerg
! 	DW_fraction_of_algal_group_of_total_algae
		fDDiE = oDDiWE /(oDDiWE + oDGrenWE + oDBlueWE + NearZero)
! 	Flux_to_water_detritus
		wDPimtWE = wDMPhtWE + wDLssytE
! 	Flux_to_sediment_detritus
		tDPriDtS = tDMPhytS
! 	Total_DW_flux
		if ( (aInlSrat == 0 ) ) then
		tDPimotT =(wDAsPytE - wDRpPtWE) * uDptWE - tDRspytS 		else		tDPimotT = (wDAsPytE - wDRpPtWE) * uDptWE + (wDAsPytH - wDRpPtWH) * uDptWH - tDR&
		&spytS 		endif
! 	O2_production_by_phytoplankton
		wO2odytE = molO2olC * cCPerDW * wDAsPytE
! 	O2_production_by_phytoplankton
		wO2sptWE = molO2olC * cCPerDW * wDRpPtWE * aCoO2ODE
! 	O2_production_due_to_NO3_uptake_by_phytoplankton
		wO2O3ytE = O2PerNO3 * molO2olN * wNUO3ytE
! 	O2_flux_by_water_algae
		wO2PrmWE = wO2odytE - wO2sptWE + wO2O3ytE
! 	O2_respiration_by_sediment_algae
		tO2spytS = molO2olC * cCPerDW * tDRspytS * afOxySed
! 	O2_flux_by_sediment_algae
		tO2PrimS = tO2spytS
! 	O2_flux_from_sediment_algae_to_Hypolimnion
		if ( aInlSrat == 1 ) then
		tO2PrmSH = tO2PrimS / uDptWH 		else		tO2PrmSH = 0.0 		endif
! 	O2_flux_from_sediment_algae_to_Epilimnion
		if ( aInlSrat == 0 ) then
		tO2PrmSE = tO2PrimS / uDptWE 		else		tO2PrmSE = 0.0 		endif
! 	Soluble_P_flux_from_died_Algae
		wPMyt4WE = fDissMP * wPMPhtWE
! 	Detrital_P_flux_from_died_Algae
		wPMhytWE = wPMPhtWE - wPMyt4WE
! 	Soluble_P_grazing_loss
		wPLPhO4E = fDissoss * wPLssytE
! 	Detrital_P_grazing_loss
		wPLsPDtE = wPLssytE - wPLPhO4E
! 	SRP_in_water
		wPPim4WE = - wPUPhytE + wPErPtWE + wPLPhO4E + wPMyt4WE
! 	Detritus_in_water
		wPPimtWE = wPLsPDtE + wPMhytWE
! 	Sediment_detritus
		tPPriDtS = tPMhyDtS 
! 	Ammonium_flux_from_died_Algae
		wNMyt4WE = fDissMP * wNMPhtWE
! 	Detrital_N_flux_from_died_Algae
		wNMhytWE = wNMPhtWE - wNMyt4WE
! 	NH4-N_grazing_loss
		wNLPhH4E = fDissoss * wNLssytE
! 	Detrital_N_grazing_loss
		wNLsPDtE = wNLssytE - wNLPhH4E
! 	Ammonium_in_water
		wNPim4WE = - wNUH4ytE + wNErPtWE + wNLPhH4E + wNMyt4WE
! 	Nitrate_in_water
		wNPim3WE = - wNUO3ytE
! 	Detritus_in_water
		wNPimtWE = wNLsPDtE + wNMhytWE
! 	Sediment_detritus
		tNPriDtS = tNMhyDtS 
! 	Total_Si_flux_to_sio2_in_PRIM_module
		if ( (aInlSrat == 0) ) then
		wSiim2WE = wSixciWE - wSiUDiE + tSixcDiS / uDptWE 		else		wSiim2WE = wSixciWE - wSiUDiE 		endif
! 	Total_Si_flux_to_sed_detritus_in_PRIM_module
		wSiritWE = wSiMDiWE + wSiosDiE
! 	Total_Si_flux_to_sed_diatoms_in_PRIM_module
		if ( (aInlSrat == 0) ) then
		tSiriDiS = tSiSeDiE - tSiessDi - tSixcDiS - tSiMDiS 		else		tSiriDiS = tSiSeDiH - tSiessDi - tSixcDiS - tSiMDiS 		endif
! 	Sediment_detritus
		tSiriDtS = tSiMDiS 
! 	Euphotic_depth
		aDptEphE = cEuph * aSecchiE
! 	Relative_euphotic_depth
		aReptphE = aDptEphE / uDptWE
! 	Chla_per_m2
		aChlaHE = oChlaE * uDptWE
! 	%_cover_with_algae
		aCoPhtWE = cCovSpP *(oDPhytWE * uDptWE)
! 	Average_spec_extinction_of_algae_per_unit_chla
		rExChytE = aExtPytE /(oChlaE / mgPerg + NearZero)
! 	P/D_ratio_herbaceous_zooplankton
		rPDZooE = oPZooE /(oDZooE+NearZero)
! 	N/C_ratio_herbaceous_zooplankton
		rNDZooE = oNZooE/(oDZooE+NearZero)
! 	Food_for_zooplankton
		oDFodooE = cPrefDi * oDDiWE + cPrefren * oDGrenWE + cPreflue * oDBlueWE + cPrefD&
		&e * oDDtWE
! 	Filtering_rate
		aFiltE = cFiltMx * uFuTmooE * hFilt /(hFilt + oDOMWE)
! 	Food_saturation_function_of_zooplankton
		aDSatooE = oDFodooE /(hFilt + oDOMWE)
! 	Environmental_correction_of_zooplankton
		wDEnvooE = max(0.0,ukDncooE / cDCarZoo * oDZooE*oDZooE)
! 	Assimilation_of_zooplankton
		wDAssooE = aDSatooE *(ukDsTooE * oDZooE - wDEnvooE)
! 	Consumption_of_zooplankton
		wDCZooE = wDAssooE / fDAssZoo
! 	DW_detritus_consumption_by_zooplankton
		wDCDtooE = cPrefDe*oDDtWE / oDFodooE * wDCZooE
! 	DW_diatoms_consumption_by_zooplankton
		wDCDiooE = cPrefDi*oDDiWE / oDFodooE * wDCZooE
! 	DW_greens_consumption_by_zooplankton
		wDCreooE = cPrefren*oDGrenWE / oDFodooE * wDCZooE
! 	DW_blue-greens_consumption_by_zooplankton
		wDCluooE = cPreflue*oDBlueWE / oDFodooE * wDCZooE
! 	Phytoplankton_consumption_by_zooplankton
		wDChyooE = wDCDiooE + wDCreooE + wDCluooE
! 	Egestion_of_zooplankton
		wDEZooE = wDCZooE - wDAssooE
! 	Correction_factor_of_zooplankton_respiration_for_P_and_N_content
		aCoReooE = max(cPDZoRef / rPDZooE,cNDZoRef / rNDZooE)
! 	Zooplankton_respiration
		wDRspooE = aCoReooE * ukDspooE * oDZooE
! 	Zooplankton_mortality_incl_environmental_correction
		wDMZooE = kMZoo * oDZooE +(1.0 - aDSatooE) * wDEnvooE
! 	Zooplankton_food
		oPFodooE = cPrefDi*oPDiWE + cPrefren*oPGrenWE + cPreflue*oPBlueWE + cPrefDe * oP&
		&DtWE
! 	P/D_ratio_of_zooplankton_food
		rPDooooE = oPFodooE /(oDFodooE+NearZero)
! 	P_diatom_consumption_by_zooplankton
		wPCDiooE = rPDDiWE * wDCDiooE
! 	P_green_consumption_by_zooplankton
		wPCreooE = rPDGrnWE * wDCreooE
! 	P_blue_green_consumption_by_zooplankton
		wPCluooE = rPDBleWE * wDCluooE
! 	Total_P_phytoplankton_consumption_by_zooplankton
		wPChyooE = wPCDiooE + wPCreooE + wPCluooE
! 	Consumption_of_detrital_P
		wPCDtooE = rPDDtWE * wDCDtooE
! 	Total_P_consumption
		wPCZooE = wPChyooE + wPCDtooE
! 	P_assimilation_efficiency_of_herbivores
		afPssooE = min(1.0,cPDZoRef / rPDooooE * fDAssZoo)
! 	Assimilation_by_herbivores
		wPAssooE = afPssooE * wPCZooE
! 	P_egestion
		wPEZooE = wPCZooE - wPAssooE
! 	Soluble_P_egestion
		wPEooO4E = fDiEgZoo*wPEZooE
! 	Detrital_P_egestion
		wPEZoDtE = wPEZooE - wPEooO4E
! 	P_excretion_rate_of_herbivores
		akPxcooE = rPDZooE / cPDZoRef * kDResZoo * uFuTmooE
! 	P_excretion
		wPEcrooE = akPxcooE*oPZooE 
! 	Mortality
		wPMZooE = rPDZooE * wDMZooE
! 	Soluble_P_mortality
		wPMooO4E = fDissZoo * wPMZooE
! 	Detrital_P_mortality
		wPMZoDtE = wPMZooE - wPMooO4E
! 	Zooplankton_food
		oNFodooE = cPrefDi*oNDiWE + cPrefren*oNGrenWE + cPreflue*oNBlueWE + cPrefDe*oNDt&
		&WE
! 	N/C_ratio_of_zooplankton_food
		rNDooooE = oNFodooE /(oDFodooE+NearZero)
! 	N_diatom_consumption_by_zooplankton
		wNCDiooE = rNDDiWE*wDCDiooE
! 	N_green_consumption_by_zooplankton
		wNCreooE = rNDGrnWE*wDCreooE
! 	N_blue_green_consumption_by_zooplankton
		wNCluooE = rNDBleWE*wDCluooE
! 	Total_N_phytoplankton_consumption_by_zooplankton
		wNChyooE = wNCDiooE + wNCreooE + wNCluooE
! 	Consumption_of_detrital_N
		wNCDtooE = rNDDtWE*wDCDtooE
! 	Total_N_consumption
		wNCZooE = wNChyooE + wNCDtooE
! 	N_assimilation_efficiency_of_herbivores
		afNssooE = min(1.0,cNDZoRef / rNDooooE * fDAssZoo)
! 	Assimilation_by_herbivores
		wNAssooE = afNssooE*wNCZooE
! 	N_egestion
		wNEZooE = wNCZooE - wNAssooE
! 	Soluble_N_egestion
		wNEooH4E = fDiEgZoo*wNEZooE
! 	Detrital_N_egestion
		wNEZoDtE = wNEZooE - wNEooH4E
! 	N_excretion_rate_of_herbivores
		kNEcrooE = rNDZooE / cNDZoRef * kDResZoo * uFuTmooE
! 	N_excretion
		wNEcrooE = kNEcrooE*oNZooE 
! 	Mortality
		wNMZooE = rNDZooE*wDMZooE
! 	Soluble_N_mortality
		wNMooH4E = fDissZoo*wNMZooE
! 	Detrital_N_mortality
		wNMZoDtE = wNMZooE - wNMooH4E
! 	Consumption_of_diatoms
		wSiDiooE = cSiDDi * wDCDiooE
! 	P/D_ratio_of_young_fish
		rPDFiJv = sPFiJv /(sDFiJv+NearZero)
! 	P/D_ratio_of_adult_fish
		rPDFiAd = sPFiAd /(sDFiAd+NearZero)
! 	N/D_ratio_of_young_fish
		rNDFiJv = sNFiJv /(sDFiJv+NearZero)
! 	N/D_ratio_of_adult_fish
		rNDFiAd = sNFiAd /(sDFiAd+NearZero)
! 	Reproduction_flux
		if ( (0 == IncSeson) ) then
		tDRprish = ((1/DaysInY)*fReprish) * sDFiAd 		else if ( (Day >= cDaepish .and. Day < cDaepish + 1.0) ) then		tDRprish = fReprish * sDFiAd * PerDay 		else		tDRprish = 0.0 		endif
! 	Ageing
		if ( (0 == IncSeson) ) then
		tDAgeish = ((1/DaysInY)*fAgeFish) * sDFiJv 		else if ( (Day >= 364.0) ) then		tDAgeish = fAgeFish * sDFiJv *PerDay 		else		tDAgeish = 0.0 		endif
! 	Food_limitation_function_of_young_fish
		if ( (aInlSrat == 0) ) then
		aDTotZoo = oDZooE * uDptWE 		else		aDTotZoo = oDZooE * uDptWE + oDZooH * uDptWH 		endif
! 	Food_limitation_function_of_young_fish
		if ( (aInlSrat == 0) ) then
		aPTotZoo = oPZooE * uDptWE 		else		aPTotZoo = oPZooE * uDptWE + oPZooH * uDptWH 		endif
! 	Food_limitation_function_of_young_fish
		if ( (aInlSrat == 0) ) then
		aNTotZoo = oNZooE * uDptWE 		else		aNTotZoo = oNZooE * uDptWE + oNZooH * uDptWH 		endif
! 	Food_limitation_function_of_young_fish
		aDSatiJv = aDTotZoo * aDTotZoo /(hDZooiJv * hDZooiJv + aDTotZoo * aDTotZoo)
! 	Environmental_correction_of_fish
		tDEnviJv = max(0.0,ukDnciJv /(cDCrrish - sDFiAd) * sDFiJv*sDFiJv)
! 	Assimilation_of_fish
		tDAssiJv = aDSatiJv *(kDAssiJv * uFuTmish * sDFiJv - tDEnviJv)
! 	Zooplankton_consumption_of_fish
		tDCFiJv = tDAssiJv / fDAssiJv
! 	Egestion_of_fish
		tDEFiJv = tDCFiJv - tDAssiJv
! 	Respiration_of_fish
		tDRspiJv = (cPDisRef / rPDFiJv) * kDRspiJv * uFuTmish * sDFiJv
! 	Fish_mortality_incl_environmental_correction
		tDMFiJv = kMFiJv * sDFiJv +(1.0 - aDSatiJv) * tDEnviJv
! 	Migration_flux
		tDMgriJv = kMigrish *(cDFiJvIn - sDFiJv)
! 	Environmental_correction_of_fish
		tDEnviAd = max(0.0,ukDnciAd /(cDCrrish - sDFiJv) * sDFiAd*sDFiAd)
! 	Assimilation_of_fish
		tDAssiAd = aDSatiAd *(kDAssiAd * uFuTmish * sDFiAd - tDEnviAd)
! 	Zoobenthos_consumption_of_fish
		tDCFiAd = tDAssiAd / fDAssiAd
! 	Egestion_of_fish
		tDEFiAd = tDCFiAd - tDAssiAd
! 	Respiration_of_fish
		tDRspiAd = (cPDisRef / rPDFiAd) * kDRspiAd * uFuTmish * sDFiAd
! 	Fish_mortality_incl_environmental_correction
		tDMFiAd = kMFiAd * sDFiAd +(1.0 - aDSatiAd) * tDEnviAd
! 	Harvesting_of_fish
		tDHrvish = ukHrvish * sDFiAd
! 	Migration_flux
		tDMgriAd = kMigrish *(cDFiAdIn - sDFiAd)
! 	Part_of_dead_fish_DW_fixed_in_bones_and_scales
		tDMiJBot = fDBone * tDMFiJv
! 	Part_of_dead_fish_DW_becoming_detritus
		tDMFivDt = tDMFiJv - tDMiJBot
! 	Part_of_dead_fish_DW_fixed_in_bones_and_scales
		tDMiABot = fDBone * tDMFiAd
! 	Part_of_dead_fish_DW_becoming_detritus
		tDMFidDt = tDMFiAd - tDMiABot
! 	Reproduction_flux
		tPRprish = rPDFiAd * tDRprish
! 	Ageing
		tPAgeish = rPDFiJv * tDAgeish
! 	Net_migration_flux
		tPMgriJv = kMigrish *(cPDisRef * cDFiJvIn - sPFiJv)
! 	(zooplankton)_P_consumption_by_juvenile_fish
		tPCFiJv = rPDZooE * tDCFiJv
! 	P_assimilation_efficiency_of_juvenile_fish
		afPssiJv = min(1.0,cPDisRef / rPDZooE * fDAssiJv)
! 	P_assimilation_of_juvenile_fish
		tPAssiJv = afPssiJv * tPCFiJv
! 	Egestion_of_juvenile_fish
		tPEFiJv = tPCFiJv - tPAssiJv
! 	P_excretion_of_juvenile_fish
		tPEcriJv = (rPDFiJv / cPDisRef) * kDRspiJv * uFuTmish * sPFiJv
! 	Mortality_of_juvenile_fish
		tPMFiJv = rPDFiJv * tDMFiJv
! 	Net_migration_flux
		tPMgriAd = kMigrish *(cPDisRef * cDFiAdIn - sPFiAd)
! 	(zoobenthos)_P_consumption_by_FiAd
		tPCFiAd = rPDBnt * tDCFiAd
! 	P_assimilation_of_FiAd
		tPAssiAd = afPssiAd * tPCFiAd
! 	Egestion_of_FiAd
		tPEFiAd = tPCFiAd - tPAssiAd
! 	P_excretion_of_FiAd
		tPEcriAd = (rPDFiAd / cPDisRef) * kDRspiAd * uFuTmish * sPFiAd
! 	Mortality_of_FiAd
		tPMFiAd = rPDFiAd * tDMFiAd
! 	Harvesting_of_FiAd
		tPHrvish = rPDFiAd * tDHrvish
! 	Part_of_dead_fish_P_fixed_in_bones_AND_scales
		tPMiJBot = fPBone * tPMFiJv
! 	Part_of_dead_fish_P_becoming_dissolved_P
		tPMiJPO4 = fDisMish *(tPMFiJv - tPMiJBot)
! 	Part_of_dead_fish_PW_becoming_detritus
		tPMFivDt = tPMFiJv - tPMiJBot - tPMiJPO4
! 	Part_of_dead_fish_P_fixed_in_bones_AND_scales
		tPMiABot = fPBone * tPMFiAd
! 	Part_of_dead_fish_P_becoming_dissolved_P
		tPMiAPO4 = fDisMish *(tPMFiAd - tPMiABot)
! 	Part_of_dead_fish_PW_becoming_detritus
		tPMFidDt = tPMFiAd - tPMiABot - tPMiAPO4
! 	SRP_egestion_of_fish
		tPEiJPO4 = fDiEgish * tPEFiJv
! 	Detrital_P_egestion_of_fish
		tPEFivDt = tPEFiJv - tPEiJPO4
! 	SRP_egestion_of_fish
		tPEiAPO4 = fDiEgish * tPEFiAd
! 	Detrital_P_egestion_of_fish
		tPEFidDt = tPEFiAd - tPEiAPO4
! 	Reproduction_flux
		tNRprish = rNDFiAd * tDRprish
! 	Ageing
		tNAgeish = rNDFiJv * tDAgeish
! 	Net_migration_flux
		tNMgriJv = kMigrish *(cNDisRef * cDFiJvIn - sNFiJv)
! 	(zooplankton)_N_consumption_by_juvenile_fish
		tNCFiJv = rNDZooE * tDCFiJv
! 	N_assimilation_efficiency_of_juvenile_fish
		afNssiJv = min(1.0,cNDisRef / rNDZooE * fDAssiJv)
! 	N_assimilation_of_juvenile_fish
		tNAssiJv = afNssiJv * tNCFiJv
! 	Egestion_of_juvenile_fish
		tNEFiJv = tNCFiJv - tNAssiJv
! 	N_excretion_of_juvenile_fish
		tNEcriJv = (rNDFiJv / cNDisRef) * kDRspiJv * uFuTmish * sNFiJv
! 	Mortality_of_juvenile_fish
		tNMFiJv = rNDFiJv * tDMFiJv
! 	Net_migration_flux
		tNMgriAd = kMigrish *(cNDisRef * cDFiAdIn - sNFiAd)
! 	(zoobenthos)_N_consumption_by_FiAd
		tNCFiAd = rNDBnt * tDCFiAd
! 	N_assimilation_of_FiAd
		tNAssiAd = afNssiAd * tNCFiAd
! 	Egestion_of_FiAd
		tNEFiAd = tNCFiAd - tNAssiAd
! 	N_excretion_of_FiAd
		tNEcriAd = (rNDFiAd / cNDisRef) * kDRspiAd * uFuTmish * sNFiAd
! 	Mortality_of_FiAd
		tNMFiAd = rNDFiAd * tDMFiAd
! 	Harvesting_of_FiAd
		tNHrvish = rNDFiAd * tDHrvish
! 	Part_of_dead_fish_N_fixed_in_bones_AND_scales
		tNMiABot = fDBone * tNMFiAd
! 	Part_of_dead_fish_N_becoming_dissolved_N
		tNMiANH4 = fDisMish *(tNMFiAd - tNMiABot)
! 	Part_of_dead_fish_NW_becoming_detritus
		tNMFidDt = tNMFiAd - tNMiABot - tNMiANH4
! 	Part_of_dead_fish_N_fixed_in_bones_AND_scales
		tNMiJBot = fDBone * tNMFiJv
! 	Part_of_dead_fish_N_becoming_dissolved_N
		tNMiJNH4 = fDisMish *(tNMFiJv - tNMiJBot)
! 	Part_of_dead_fish_NW_becoming_detritus
		tNMFivDt = tNMFiJv - tNMiJBot - tNMiJNH4
! 	NH4_egestion_of_fish
		tNEiJNH4 = fDiEgish * tNEFiJv
! 	Detrital_egestion_of_fish
		tNEFivDt = tNEFiJv - tNEiJNH4
! 	Migration_flux
		tDMgrisc = kMigrisc *(cDPiscIn - sDPisc)
! 	Total_fish_biomass
		aDFish = sDFiJv + sDFiAd
! 	Food_limitation_function_of_Pisc
		aDSatisc = aDFish*aDFish /(hDFshisc*hDFshisc + aDFish*aDFish)
! 	Environmental_correction_of_Pisc
		tDEnvisc = max(0.0,akDncisc / aDCrrisc * sDPisc*sDPisc)
! 	Assimilation_of_Pisc
		tDAssisc = aDSatisc *(kDAssisc * aFuVeisc * uFuTmisc * sDPisc - tDEnvisc)
! 	Consumption_of_Pisc
		tDCPisc = tDAssisc / fDAssisc
! 	Egestion_of_Pisc
		tDEPisc = tDCPisc - tDAssisc
! 	Young_fish_consumption_by_Pisc
		tDCiJisc = sDFiJv / aDFish * tDCPisc
! 	Adult_fish_consumption_by_Pisc
		tDCiAisc = tDCPisc - tDCiJisc
! 	Mortality_of_Pisc(incl_environmental_correction)
		tDMPisc = kMPisc * sDPisc +(1.0 - aDSatisc) * tDEnvisc
! 	Part_of_dead_fish_DW_fixed_in_bones_AND_scales
		tDMisBot = fDBone * tDMPisc
! 	Part_of_dead_Pisc_DW_becoming_detritus
		tDMPicDt = tDMPisc - tDMisBot
! 	Harvesting_of_Pisc
		tDHrvisc = ukHrvisc * sDPisc
! 	Piscivorous_fish
		aPPisc = cPDPisc * sDPisc
! 	Young_fish_consumption_by_Pisc
		tPCiJisc = rPDFiJv * tDCiJisc
! 	Adult_fish_consumption_by_Pisc
		tPCiAisc = rPDFiAd * tDCiAisc
! 	Total_P_consumption_by_Pisc
		tPCPisc = tPCiJisc + tPCiAisc
! 	Average_P/D_ratio_of_Pisc_food
		rPDooisc = tPCPisc / tDCPisc
! 	Piscivorous_fish
		aNPisc = cNDPisc * sDPisc
! 	Young_fish_consumption_by_Pisc
		tNCiJisc = rNDFiJv * tDCiJisc
! 	Adult_fish_consumption_by_Pisc
		tNCiAisc = rNDFiAd * tDCiAisc
! 	Total_N_consumption_by_Pisc
		tNCPisc = tNCiJisc + tNCiAisc
! 	Average_N/D_ratio_of_Pisc_food
		rNDooisc = tNCPisc / tDCPisc
! 	Consumption_of_Pisc_corrected_for_N_and_P_limitation
		tDCPiscN = (tDAssisc / fDAssisc) * max (1.0, cNDPisc / rNDooisc * fDAssisc, cPDP&
		&isc / rPDooisc * fDAssisc)
! 	Egestion_of_Pisc_corrected_for_N_and_P_limitation
		tDEPiscN = tDCPiscN - tDAssisc
! 	Young_fish_consumption_by_Pisc_corrected_for_N_and_P_limitation
		tDCFJPNu = sDFiJv / aDFish * tDCPiscN
! 	Adult_fish_consumption_by_Pisc_corrected_for_N_and_P_limitation
		tDCFAPNu = tDCPiscN - tDCFJPNu
! 	Young_fish_consumption_by_Pisc
		tPCFJPNu = rPDFiJv * tDCFJPNu
! 	Adult_fish_consumption_by_Pisc
		tPCFAPNu = rPDFiAd * tDCFAPNu
! 	Total_P_consumption_by_Pisc
		tPCPiscN = tPCFJPNu + tPCFAPNu
! 	Young_fish_consumption_by_Pisc
		tNCFJPNu = rNDFiJv * tDCFJPNu
! 	Adult_fish_consumption_by_Pisc
		tNCFAPNu = rNDFiAd * tDCFAPNu
! 	Total_N_consumption_by_Pisc
		tNCPiscN = tNCFJPNu + tNCFAPNu
! 	P_assimilation_efficiency_of_Pisc
		afPssisc = min(1.0, ((cPDPisc / rPDooisc * fDAssisc) / (tDCPiscN / tDCPisc )) )
! 	P_assimilation_of_Pisc_corrected_for_N_and_P_limitation
		tPAssisc = afPssisc * tPCPiscN
! 	Egestion_of_Pisc_corrected_for_N_and_P_limitation
		tPEPisc = tPCPiscN - tPAssisc
! 	SRP_egestion_of_Pisc_corrected_for_N_and_P_limitation
		tPEisPO4 = fDiEgisc * tPEPisc
! 	Detrital_P_egestion_of_Pisc_corrected_for_N_and_P_limitation
		tPEPicDt = tPEPisc - tPEisPO4
! 	Respiration_of_Pisc
		tDRspisc = kDRspisc * uFuTmisc * sDPisc
! 	Respiration_of_Pisc
		tPEcrisc = cPDPisc * tDRspisc
! 	Mortality_of_Pisc
		tPMPisc = cPDPisc * tDMPisc
! 	Part_of_dead_Pisc_P_fixed_in_bones_AND_scales
		tPMisBot = fPBone * tPMPisc
! 	Part_of_dead_fish_P_becoming_dissolved_P
		tPMisPO4 = fDisMisc *(tPMPisc - tPMisBot)
! 	Part_of_dead_Pisc_P_becoming_detrital_P
		tPMPicDt = tPMPisc - tPMisBot - tPMisPO4
! 	Net_migration_flux
		tPMgrisc = kMigrisc *(cPDPisc * cDPiscIn - aPPisc)
! 	Harvesting_of_Pisc
		tPHrvisc = cPDPisc * tDHrvisc
! 	N_assimilation_efficiency_of_Pisc
		afNssisc = min(1.0, ((cNDPisc / rNDooisc * fDAssisc) / (tDCPiscN / tDCPisc )) )
! 	N_assimilation_of_Pisc
		tNAssisc = afNssisc * tNCPiscN
! 	Egestion_of_Pisc
		tNEPisc = tNCPiscN - tNAssisc
! 	SRN_egestion_of_Pisc
		tNEisNH4 = fDiEgisc * tNEPisc
! 	Detrital_N_egestion_of_Pisc
		tNEPicDt = tNEPisc - tNEisNH4
! 	Respiration_of_Pisc
		tNEcrisc = cNDPisc * tDRspisc
! 	Mortality_of_Pisc
		tNMPisc = cNDPisc * tDMPisc
! 	Part_of_dead_Pisc_N_fixed_in_bones_AND_scales
		tNMisBot = fDBone * tNMPisc
! 	Part_of_dead_fish_N_becoming_dissolved_N
		tNMisNH4 = fDisMisc *(tNMPisc - tNMisBot)
! 	Part_of_dead_Pisc_N_becoming_detrital_N
		tNMPicDt = tNMPisc - tNMisBot - tNMisNH4
! 	Net_migration_flux
		tNMgrisc = kMigrisc *(cNDPisc * cDPiscIn - aNPisc)
! 	Harvesting_of_Pisc
		tNHrvisc = cNDPisc * tDHrvisc
! 	Total_food_web_flux_of_DW_in_Zoobenthos
		if ( (0 == InclWeb) ) then
		tDWebBnt = 0.0 		else		tDWebBnt = tDMigBnt + tDAssBnt - tDCFiAd - tDResBnt - tDMBnt 		endif
! 	Total_food_web_flux_of_P_in_Zoobenthos
		if ( (0 == InclWeb) ) then
		tPWebBnt = 0.0 		else		tPWebBnt = tPMigBnt + tPAssBnt - tPCFiAd - tPExcBnt - tPMBnt 		endif
! 	Total_food_web_flux_of_N_in_Zoobenthos
		if ( (0 == InclWeb) ) then
		tNWebBnt = 0.0 		else		tNWebBnt = tNMigBnt + tNAssBnt - tNCFiAd - tNExcBnt - tNMBnt 		endif
! 	Total_food_web_flux_of_DW_in_Young_fish
		if ( (0 == InclWeb) ) then
		tDWebiJv = 0.0 		else		tDWebiJv = tDMgriJv + tDRprish - tDAgeish + tDAssiJv - tDRspiJv - tDMFiJv - tDCF&
		&JPNu 		endif
! 	Total_food_web_flux_of_P_in_Young_fish
		if ( (0 == InclWeb) ) then
		tPWebiJv = 0.0 		else		tPWebiJv = tPMgriJv + tPRprish - tPAgeish + tPAssiJv - tPEcriJv - tPMFiJv - tPCF&
		&JPNu 		endif
! 	Total_food_web_flux_of_N_in_Young_fish
		if ( (0 == InclWeb) ) then
		tNWebiJv = 0.0 		else		tNWebiJv = tNMgriJv + tNRprish - tNAgeish + tNAssiJv - tNEcriJv - tNMFiJv - tNCF&
		&JPNu 		endif
! 	Total_food_web_flux_of_DW_in_Adult_fish
		if ( (0 == InclWeb) ) then
		tDWebiAd = 0.0 		else		tDWebiAd = tDMgriAd + tDAssiAd - tDRspiAd - tDMFiAd - tDRprish + tDAgeish - tDCF&
		&APNu - tDHrvish 		endif
! 	Total_food_web_flux_of_P_in_Adult_fish
		if ( (0 == InclWeb) ) then
		tPWebiAd = 0.0 		else		tPWebiAd = tPMgriAd + tPAssiAd - tPEcriAd - tPMFiAd - tPRprish + tPAgeish - tPCF&
		&APNu - tPHrvish 		endif
! 	Total_food_web_flux_of_N_in_Adult_fish
		if ( (0 == InclWeb) ) then
		tNWebiAd = 0.0 		else		tNWebiAd = tNMgriAd + tNAssiAd - tNEcriAd - tNMFiAd - tNRprish + tNAgeish - tNCF&
		&APNu - tNHrvish 		endif
! 	Total_food_web_flux_of_DW_in_predatory_fish
		if ( (0 == InclWeb) ) then
		tDWebisc = 0.0 		else		tDWebisc = tDMgrisc + tDAssisc - tDRspisc - tDMPisc - tDHrvisc 		endif
! 	Total_food_web_flux_of_DW_in_Herbivorous_zooplankton
		if ( (0 == InclWeb .or. aInlSrat == 0) ) then
		wDWebooH = 0.0 		else		wDWebooH = wDAssooH - wDRspooH - wDMZooH - tDCFiJv * (( aDTotZoo - oDZooE * uDpt&
		&WE) / (aDTotZoo + NearZero)) / uDptWH 		endif
! 	Total_food_web_flux_of_DW_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWebtWH = 0.0 		else		wDWebtWH = - wDCDtooH + wDEZooH + wDMZooH +(tDEFiJv * uFiJvEH + tDEFiAd * uFiAdE&
		&H + tDMFivDt * uFiJvMH + tDMFidDt * uFiAdMH + tDEPiscN * uPiscEH + tDMPicDt * uP&
		&iscMH) / uDptWH 		endif
! 	Total_food_web_flux_of_DW_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWebiWH = 0.0 		else		wDWebiWH = - wDCDiooH 		endif
! 	Total_food_web_flux_of_DW_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWbGnWH = 0.0 		else		wDWbGnWH = - wDCreooH 		endif
! 	Total_food_web_flux_of_DW_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWbBeWH = 0.0 		else		wDWbBeWH = - wDCluooH 		endif
! 	Total_food_web_flux_of_DW_in_Herbivorous_zooplankton
		if ( (0 == InclWeb) ) then
		wDWebooE = 0.0 		else		wDWebooE = wDAssooE - wDRspooE - wDMZooE - tDCFiJv * (oDZooE * uDptWE / (aDTotZo&
		&o + NearZero) ) / uDptWE 		endif
! 	Total_food_web_flux_of_DW_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWebtWE = 0.0 		else		wDWebtWE = - wDCDtooE + wDEZooE + wDMZooE +(tDEFiJv * ( 1.0 - uFiJvEH ) + tDEFiA&
		&d * ( 1.0 - uFiAdEH ) + tDMFivDt * ( 1.0 - uFiJvMH ) + tDMFidDt * ( 1.0 - uFiAdM&
		&H ) + tDEPiscN * ( 1.0 - uPiscEH ) + tDMPicDt * ( 1.0 - uPiscMH)) / uDptWE 		endif
! 	Total_food_web_flux_of_DW_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWebiWE = 0.0 		else		wDWebiWE = - wDCDiooE 		endif
! 	Total_food_web_flux_of_DW_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWbGnWE = 0.0 		else		wDWbGnWE = - wDCreooE 		endif
! 	Total_food_web_flux_of_DW_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wDWbBeWE = 0.0 		else		wDWbBeWE = - wDCluooE 		endif
! 	Total_DW_in_system
		if ( (0 == InclWeb) ) then
		tDWebotT = 0.0 		else		tDWebotT = - wDRspooE * uDptWE - wDRspooH * uDptWH + tDMgriJv + tDMgriAd + tDMgr&
		&isc + tDMigBnt - tDRspiJv - tDRspiAd - tDRspisc - tDResBnt - tDMiJBot - tDMiABot&
		&- tDMisBot - tDHrvish - tDHrvisc 		endif
! 	Total_food_web_flux_of_P_in_Herbivorous_zooplankton
		if ( (0 == InclWeb .or. aInlSrat == 0) ) then
		wPWebooH = 0.0 		else		wPWebooH = wPAssooH - wPEcrooH - wPMZooH - tPCFiJv * (( aPTotZoo - oPZooE * uDpt&
		&WE ) / (aPTotZoo + NearZero) ) / uDptWH 		endif
! 	Total_food_web_flux_of_P_in_SRP_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbP4WH = 0.0 		else		wPWbP4WH = wPEcrooH + wPEooO4H + wPMooO4H +(tPEcriJv *uFivEcrH + tPEcriAd * uFid&
		&EcrH + tPEiJPO4 * uFiJvEH + tPEiAPO4 * uFiAdEH + tPMiJPO4 * uFiJvMH + tPMiAPO4 *&
		& uFiAdMH+ tPEcrisc * uPicEcrH + tPEisPO4 * uPiscEH + tPMisPO4 * uPiscMH ) / uDpt&
		&WH 		endif
! 	Total_food_web_flux_of_P_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWebtWH = 0.0 		else		wPWebtWH = - wPCDtooH + wPEZoDtH + wPMZoDtH +(tPEFivDt * uFiJvEH + tPEFidDt * uF&
		&iAdEH+ tPMFivDt * uFiJvMH + tPMFidDt * uFiAdMH+ tPEPicDt * uPiscEH+ tPMPicDt * u&
		&PiscMH) / uDptWH 		endif
! 	Total_food_web_flux_of_P_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWebiWH = 0.0 		else		wPWebiWH = - wPCDiooH 		endif
! 	Total_food_web_flux_of_P_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbGnWH = 0.0 		else		wPWbGnWH = - wPCreooH 		endif
! 	Total_food_web_flux_of_P_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbBeWH = 0.0 		else		wPWbBeWH = - wPCluooH 		endif
! 	Total_food_web_flux_of_P_in_Herbivorous_zooplankton
		if ( (0 == InclWeb) ) then
		wPWebooE = 0.0 		else		wPWebooE = wPAssooE - wPEcrooE - wPMZooE - tPCFiJv * (oPZooE * uDptWE / (aPTotZo&
		&o + NearZero)) / uDptWE 		endif
! 	Total_food_web_flux_of_P_in_SRP_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbP4WE = 0.0 		else		wPWbP4WE = wPEcrooE + wPEooO4E + wPMooO4E +(tPEcriJv * ( 1.0 -uFivEcrH ) + tPEcr&
		&iAd * ( 1.0 - uFidEcrH )+ tPEiJPO4 * ( 1.0 - uFiJvEH ) + tPEiAPO4 * ( 1.0 - uFiA&
		&dEH ) + tPMiJPO4 * ( 1.0 - uFiJvMH ) + tPMiAPO4 * ( 1.0 - uFiAdMH )+ tPEcrisc * &
		&( 1.0 - uPicEcrH ) + tPEisPO4 * ( 1.0 - uPiscEH ) + tPMisPO4 * ( 1.0 - uPiscMH))&
		& / uDptWE 		endif
! 	Total_food_web_flux_of_P_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWebtWE = 0.0 		else		wPWebtWE = - wPCDtooE + wPEZoDtE + wPMZoDtE +(tPEFivDt * ( 1.0 - uFiJvEH ) + tPE&
		&FidDt * ( 1.0 - uFiAdEH )+ tPMFivDt * ( 1.0 - uFiJvMH ) + tPMFidDt * ( 1.0 - uFi&
		&AdMH )+ tPEPicDt * ( 1.0 - uPiscEH )+ tPMPicDt * ( 1.0 - uPiscMH )) / uDptWE 		endif
! 	Total_food_web_flux_of_P_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWebiWE = 0.0 		else		wPWebiWE = - wPCDiooE 		endif
! 	Total_food_web_flux_of_P_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbGnWE = 0.0 		else		wPWbGnWE = - wPCreooE 		endif
! 	Total_food_web_flux_of_P_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wPWbBeWE = 0.0 		else		wPWbBeWE = - wPCluooE 		endif
! 	Total_P_in_system
		if ( (0 == InclWeb) ) then
		tPWebotT = 0.0 		else		tPWebotT = tPMgriJv + tPMgriAd + tPMgrisc + tPMigBnt - tPMiJBot - tPMiABot- tPMi&
		&sBot - tPHrvish - tPHrvisc 		endif
! 	NH4_egestion_of_fish
		tNEiANH4 = fDiEgish * tNEFiAd
! 	Detrital_egestion_of_fish
		tNEFidDt = tNEFiAd - tNEiANH4
! 	Total_food_web_flux_of_N_in_nitrate_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbN3WH = 0.0 		else		wNWbN3WH = 0.0 		endif
! 	Total_food_web_flux_of_N_in_nitrate_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbN3WE = 0.0 		else		wNWbN3WE = 0.0 		endif
! 	Total_food_web_flux_of_N_in_Herbivorous_zooplankton
		if ( (0 == InclWeb .or. aInlSrat == 0) ) then
		wNWebooH = 0.0 		else		wNWebooH = wNAssooH - wNEcrooH - wNMZooH - tNCFiJv * (( aNTotZoo -oNZooE * uDptW&
		&E ) / (aNTotZoo + NearZero) ) / uDptWH 		endif
! 	Total_food_web_flux_of_N_in_ammonium_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbN4WH = 0.0 		else		wNWbN4WH = wNEcrooH + wNEooH4H + wNMooH4H +(tNEcriJv * uFivEcrH + tNEcriAd * uFi&
		&dEcrH + tNEiJNH4 * uFiJvEH+ tNEiANH4 * uFiAdEH + tNMiANH4 *uFiAdMH+ tNMiJNH4*uFi&
		&JvMH + tNEcrisc *uPicEcrH + tNEisNH4 *uPiscEH + tNMisNH4 *uPiscMH) / uDptWH 		endif
! 	Total_food_web_flux_of_N_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWebtWH = 0.0 		else		wNWebtWH = - wNCDtooH + wNEZoDtH + wNMZoDtH +(tNEFivDt * uFiJvEH +tNEFidDt * uFi&
		&AdEH+ tNMFivDt * uFiJvMH + tNMFidDt * uFiAdMH + tNEPicDt * uPiscEH + tNMPicDt * &
		&uPiscMH) / uDptWH 		endif
! 	Total_food_web_flux_of_N_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWebiWH = 0.0 		else		wNWebiWH = - wNCDiooH 		endif
! 	Total_food_web_flux_of_N_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbGnWH = 0.0 		else		wNWbGnWH = - wNCreooH 		endif
! 	Total_food_web_flux_of_N_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbBeWH = 0.0 		else		wNWbBeWH = - wNCluooH 		endif
! 	Total_food_web_flux_of_N_in_Herbivorous_zooplankton
		if ( (0 == InclWeb) ) then
		wNWebooE = 0.0 		else		wNWebooE = wNAssooE - wNEcrooE - wNMZooE - tNCFiJv * (oNZooE * uDptWE / (aNTotZo&
		&o + NearZero) ) / uDptWE 		endif
! 	Total_food_web_flux_of_N_in_ammonium_in_water_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbN4WE = 0.0 		else		wNWbN4WE = wNEcrooE + wNEooH4E + wNMooH4E +(tNEcriJv * ( 1.0 - uFivEcrH) + tNEcr&
		&iAd * ( 1.0 - uFidEcrH) + tNEiJNH4 * ( 1.0 - uFiJvEH ) + tNEiANH4 * ( 1.0 - uFiA&
		&dEH ) + tNMiANH4 * ( 1.0 -uFiAdMH ) + tNMiJNH4* ( 1.0 -uFiJvMH) + tNEcrisc * ( 1&
		&.0 -uPicEcrH ) + tNEisNH4 * ( 1.0 -uPiscEH ) + tNMisNH4 * ( 1.0 -uPiscMH)) / uDp&
		&tWE 		endif
! 	Total_food_web_flux_of_N_in_Detritus_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWebtWE = 0.0 		else		wNWebtWE = - wNCDtooE + wNEZoDtE + wNMZoDtE +(tNEFivDt * ( 1.0 - uFiJvEH ) +tNEF&
		&idDt * ( 1.0 - uFiAdEH ) + tNMFivDt * ( 1.0 - uFiJvMH ) + tNMFidDt * ( 1.0 - uFi&
		&AdMH ) + tNEPicDt * ( 1.0 - uPiscEH ) + tNMPicDt * ( 1.0 - uPiscMH )) / uDptWE 		endif
! 	Total_food_web_flux_of_N_in_Diatoms_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWebiWE = 0.0 		else		wNWebiWE = - wNCDiooE 		endif
! 	Total_food_web_flux_of_N_in_Greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbGnWE = 0.0 		else		wNWbGnWE = - wNCreooE 		endif
! 	Total_food_web_flux_of_N_in_Blue-greens_in_lake_water
		if ( (0 == InclWeb) ) then
		wNWbBeWE = 0.0 		else		wNWbBeWE = - wNCluooE 		endif
! 	Total_N_in_system
		if ( (0 == InclWeb) ) then
		tNWebotT = 0.0 		else		tNWebotT = tNMgriJv + tNMgriAd + tNMgrisc + tNMigBnt - tNMiJBot- tNMiABot - tNMi&
		&sBot - tNHrvish - tNHrvisc 		endif
! 	Total_food_web_flux_of_silica_in_lake_water_detritus
		if ( (0 == InclWeb) ) then
		wSiebtWE = 0.0 		else		wSiebtWE = wSiDiooE 		endif
! 	Average_selection_factor
		aPrefveE = (cPrefDi * oDDiWE + cPrefren * oDGrenWE + cPreflue * oDBlueWE + cPref&
		&De * oDDtWE) / oDOMWE
! 	Total_zooplankton_consumption(check)
		wDCZoo2E = aFiltE * aPrefveE * oDOMWE * oDZooE
! 	Specific_consumption_rate_of_zooplankton(daily_ration)
		aDCZoSpE = wDCZooE / oDZooE
! 	Specific_C_assimilation_of_zooplankton
		aDAsZSpE = wDAssooE / oDZooE
! 	Specific_DW_grazing(daily_grazing)
		aDGraSpE = wDCZooE / oDOMWE
! 	Specific_P_consumption_OR_daily_ration
		aPCZoSpE = wPCZooE / oPZooE
! 	Specific_P_grazing_OR_daily_grazing
		aPGraSpE = wPCZooE / oPOMWE
! 	Specific_N_consumption_OR_daily_ration
		aNCZoSpE = wNCZooE / oNZooE
! 	Specific_N_grazing_OR_daily_grazing
		aNGraSpE = wNCZooE / oNOMWE
! 	Total_settling
		tDSetotE = tDSetIME + tDSetDtE + tDStPytE
! 	Total_settling
		tPSetotE = tPSetIME + tPSetDtE + tPStPytE
! 	Total_settling
		tNSetotE = tNSetDtE + tNStPytE
! 	Total_resuspension
		tDRsuTot = tDRsuead + tDRsPTot
! 	Total_P_resuspension_flux
		tPRsuTot = tPRessDt + tPRsuAIM + tPRsuhyt + tPRsuPO4
! 	Total_N_resuspension_flux
		tNRsuTot = tNRessDt + tNRsuNH4 + tNRsuNO3 + tNRsuhyt
! 	Update_dredged_layer
		if ( (sTime == bTimered) ) then
		aDptSart = sDepthW 		else		aDptSart = aDptSart 		endif
! 	Rate_constant_of_deepening
		if ( ((sTime >= bTimered) .and. (sTime < bTimered + cLengred) .and. (aDptSart <=&
		& cDeptRef - cDepthS)) ) then
		akDreDpt = (log(cDeptRef / aDptSart)) / cLengred 		else		akDreDpt = 0.0 		endif
! 	Rate_constant_of_dredging(exponential_function)
		if ( ((sTime >= bTimered) .and. (sTime < bTimered + cLengred) .and. (aDptSart <=&
		& cDeptRef - cDepthS)) ) then
		akDred = (- log(1.0 - fEffDred)) / cLengred 		else		akDred = 0.0 		endif
! 	Rate_constant_of_dredging_for_zoobenthos
		if ( ((sTime >= bTimered) .and. (sTime < bTimered + cLengred) .and. (aDptSart <=&
		& cDeptRef - cDepthS)) ) then
		akDreBnt = (- log(1.0 - fEfreent)) / cLengred 		else		akDreBnt = 0.0 		endif
! 	Change_in_water_depth_due_to_dredging
		if ( (aInlSrat == 1) ) then
		vDredptW = 0.0 		else		vDredptW = akDreDpt * sDepthW 		endif
! 	Dredging_flux_of_DW_Detritus_in_lake_sediment
		tDDreDtS = akDred * sDDetS
! 	Dredging_flux_of_P_Detritus_in_lake_sediment
		tPDreDtS = akDred * sPDetS
! 	Dredging_flux_of_N_Detritus_in_lake_sediment
		tNDreDtS = akDred * sNDetS
! 	Dredging_flux_of_Si_Det_in_lake_sediment
		tSireDtS = akDred * sSiDetS
! 	Dredging_flux_of_P_absorbed_onto_inorganic_matter_in_lake_sediment
		tPDedIMS = akDred * sPAIMS
! 	Dredging_flux_of_DW_net_soil_in_lake_sediment
		tDDdNoil = -(tDDreDtS / cRhoOM) * bRholoil
! 	Dredging_flux_of_DW_net_inorganic_matter_in_lake_sediment
		tDDdNIMS = (1.0 - fDOrgoil) * tDDdNoil
! 	Dredging_flux_of_DW_net_humic_substances_in_lake_sediment
		tDDdNumS = fDOrgoil * tDDdNoil
! 	Dredging_flux_of_P_net_humic_substances_in_lake_sediment
		tPDdNumS = cPDSolOM * tDDdNumS
! 	Dredging_flux_of_N_net_humic_substances_in_lake_sediment
		tNDdNumS = cNDSolOM * tDDdNumS
! 	Dredging_flux_of_DW_Diat_on_lake_sediment
		tDDreDiS = akDred * sDDiatS
! 	Dredging_flux_of_P_Diat_on_lake_sediment
		tPDreDiS = akDred * sPDiatS
! 	Dredging_flux_of_N_Diat_on_lake_sediment
		tNDreDiS = akDred * sNDiatS
! 	Dredging_flux_of_DW_Gren_on_lake_sediment
		tDDedenS = akDred * sDGrenS
! 	Dredging_flux_of_P_Gren_on_lake_sediment
		tPDedenS = akDred * sPGrenS
! 	Dredging_flux_of_N_Gren_on_lake_sediment
		tNDedenS = akDred * sNGrenS
! 	Dredging_flux_of_DW_Blue_on_lake_sediment
		tDDedueS = akDred * sDBlueS
! 	Dredging_flux_of_P_Blue_on_lake_sediment
		tPDedueS = akDred * sPBlueS
! 	Dredging_flux_of_N_Blue_on_lake_sediment
		tNDedueS = akDred * sNBlueS
! 	Dredging_flux_of_DW_Phyt_on_lake_sediment
		tDDedytS = tDDreDiS+tDDedenS+tDDedueS
! 	Dredging_flux_of_P_Phyt_on_lake_sediment
		tPDedytS = tPDreDiS+tPDedenS+tPDedueS
! 	Dredging_flux_of_N_Phyt_on_lake_sediment
		tNDedytS = tNDreDiS+tNDedenS+tNDedueS
! 	Dredging_flux_of_DW_Bent_on_lake_sediment
		if ( (InclWeb == 1) ) then
		tDDreBnt = akDreBnt * sDBent 		else		tDDreBnt = 0.0 		endif
! 	Dredging_flux_of_P_Bent_on_lake_sediment
		if ( (InclWeb == 1) ) then
		tPDreBnt = akDreBnt * sPBent 		else		tPDreBnt = 0.0 		endif
! 	Dredging_flux_of_N_Bent_on_lake_sediment
		if ( (InclWeb == 1) ) then
		tNDreBnt = akDreBnt * sNBent 		else		tNDreBnt = 0.0 		endif
! 	Dredging_flux_of_DW_Veg_on_lake_sediment
		tDDreVeg = akDred * sDVeg
! 	Dredging_flux_of_P_Veg_on_lake_sediment
		tPDreVeg = akDred * sPVeg
! 	Dredging_flux_of_N_Veg_on_lake_sediment
		tNDreVeg = akDred * sNVeg
! 	Total_DW_dredging_flux
		tDDdNTot = tDDreDtS - tDDdNoil + tDDedytS + tDDreBnt + tDDreVeg
! 	Total_P_dredging_flux
		tPDdNTot = tPDreDtS - tPDdNumS + tPDedIMS + tPDedytS + tPDreBnt + tPDreVeg
! 	Total_N_dredging_flux
		tNDdNTot = tNDreDtS - tNDdNumS + tNDedytS + tNDreBnt + tNDreVeg
! 	Total_Si_dredging_flux
		tSireTot = tSireDtS + cSiDDi * tDDreDiS
! 	Increase_in_inorganic_matter_in_sediment
		tDIMS = tDAbiIMS
! 	Increase_in_sediment_humus_in_lake
		tDHumS = tDAioumS
! 	Increase_in_sediment_detritus_in_lake
		tDDtS = tDAbiDtS + tDPriDtS + tDWebDtS + tDBedtSE + tDBedtSH
! 	Turnover_depth_in_lake
		vDeltaS = (tDIMS / cRhoIM +(tDHumS + tDDtS) / cRhoOM)/(1.0 - bPorS)
! 	Lake_water_depth_change
		if ( (ContDpth == 1 ) ) then
		vDeltaW = 0.0 		else		vDeltaW = - vDeltaS 		endif
! 	Burial_flux_of_DW_in_inorganic_matter_in_lake
		if ( (InclBur == 0) ) then
		tDBurIM = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tDBurIM = ((tDHumS + tDDtS) +(cRhoOM / cRhoIM) * tDIMS) / ((sDHumS + sDDetS) / s&
		&DIMS + cRhoOM / cRhoIM) 		else		tDBurIM = ((tDHumS + tDDtS) +(cRhoOM / cRhoIM) * tDIMS) / (fDOrgoil /(1.0 - fDOr&
		&goil) + cRhoOM / cRhoIM) 		endif
! 	Burial_flux_of_DW_in_organic_matter_in_lake
		if ( (InclBur == 0) ) then
		tDBurOM = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tDBurOM = (sDHumS + sDDetS) / sDIMS * tDBurIM 		else		tDBurOM = fDOrgoil /(1.0 - fDOrgoil) * tDBurIM 		endif
! 	Burial_flux_of_DW_in_detritus_in_lake
		if ( (InclBur == 0) ) then
		tDBurDt = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tDBurDt = sDDetS /(sDHumS + sDDetS) * tDBurOM 		else		tDBurDt = 0.0 		endif
! 	Burial_flux_of_DW_in_humus_in_lake
		if ( (InclBur == 0) ) then
		tDBurHum = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tDBurHum = tDBurOM - tDBurDt 		else		tDBurHum = tDBurOM 		endif
! 	Total_DW_burial_flux_in_lake
		if ( (InclBur == 0) ) then
		tDBurTot = 0.0 		else if ((vDeltaS >= 0.0) ) then		tDBurTot = tDBurIM + tDBurOM 		else		tDBurTot = tDBurIM + tDBurOM 		endif
! 	Burial_flux_of_P_in_humus_in_lake
		if ( (InclBur == 0) ) then
		tPBurHum = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tPBurHum = rPDHumS * tDBurHum 		else		tPBurHum = cPDSolOM * tDBurHum 		endif
! 	Burial_flux_of_P_in_detritus_in_lake
		if ( (InclBur == 0) ) then
		tPBurDt = 0.0 		else if ((vDeltaS >= 0.0) ) then		tPBurDt = rPDDtS * tDBurDt 		else		tPBurDt = 0.0 		endif
! 	Burial_flux_of_P_absorbed_onto_inorganic_matter_in_lake
		if ( (InclBur == 0) ) then
		tPBurAIM = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tPBurAIM = sPAIMS / sDIMS * tDBurIM 		else		tPBurAIM = 0.0 		endif
! 	Burial_flux_of_dissolved_P_in_lake
		if ( (InclBur == 0) ) then
		tPBurPO4 = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tPBurPO4 = sPO4S *(vDeltaS / cDepthS) 		else		tPBurPO4 = cPO4Gr *(bPorS * vDeltaS) 		endif
! 	Total_P_burial_flux_in_lake
		if ( (InclBur == 0) ) then
		tPBurTot = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tPBurTot = tPBurDt + tPBurHum + tPBurAIM + tPBurPO4 		else		tPBurTot = tPBurHum + tPBurAIM + tPBurPO4 		endif
! 	Burial_flux_of_N_in_humus_in_lake
		if ( (InclBur == 0) ) then
		tNBurHum = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tNBurHum = rNDHumS * tDBurHum 		else		tNBurHum = cNDSolOM * tDBurHum 		endif
! 	Burial_flux_of_N_in_detritus_in_lake
		if ( (InclBur == 0) ) then
		tNBurDt = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tNBurDt = rNDDtS * tDBurDt 		else		tNBurDt = 0.0 		endif
! 	Burial_flux_of_dissolved_NH4_in_lake
		if ( (InclBur == 0) ) then
		tNBurNH4 = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tNBurNH4 = sNH4S *(vDeltaS / cDepthS) 		else		tNBurNH4 = cNH4Gr *(bPorS * vDeltaS) 		endif
! 	Burial_flux_of_dissolved_NO3_in_lake
		if ( (InclBur == 0) ) then
		tNBurNO3 = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tNBurNO3 = sNO3S *(vDeltaS / cDepthS) 		else		tNBurNO3 = cNO3Gr *(bPorS * vDeltaS) 		endif
! 	Total_N_burial_flux_in_lake
		if ( (InclBur == 0) ) then
		tNBurTot = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tNBurTot = tNBurDt + tNBurHum + tNBurNH4 + tNBurNO3 		else		tNBurTot = tNBurHum + tNBurNH4 + tNBurNO3 		endif
! 	Burial_flux_of_Si_in_detritus_in_lake
		if ( (InclBur == 0) ) then
		tSiBurDt = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tSiBurDt = rSiDDtS * tDBurDt 		else		tSiBurDt = 0.0 		endif
! 	Total_Si_burial_flux_in_lake
		if ( (InclBur == 0) ) then
		tSiBuTot = 0.0 		else if ( (vDeltaS >= 0.0) ) then		tSiBuTot = tSiBurDt 		else		tSiBuTot = 0.0 		endif
! 	Relative_water_depth_change_due_to_sediment_turnover_AND_dredging
		if ( ( aInlSrat == 1 ) ) then
		aReDeaWE = (vDeltaW ) / uDptWE 		else		aReDeaWE = (vDeltaW + vDredptW) / uDptWE 		endif
! 	Relative_water_depth_change_due_to_sediment_turnover_AND_dredging
		if ( ( aInlSrat == 0 ) ) then
		aReDeaWH = 0.0 		else		aReDeaWH = (vDredptW) / uDptWH 		endif
! 	Total_fish_biomass
		aPFish = sPFiJv + sPFiAd
! 	Total_fish_biomass
		aNFish = sNFiJv + sNFiAd
! 	Total_DW_in_system
		aDRelotT = uDptWH * aReDeaWH * ( oDDtWH + oDIMWH + oDDiWH + oDGrenWH + oDBlueWH &
		&+ oDZooH ) + fMarsh * sDepthWM * aReDeaWM * ( oDDtWM + oDIMWM + oDDiWM + oDGrenW&
		&M + oDBlueWM + oDZooM) + uDptWE * aReDeaWE * (oDDtWE + oDIMWE + oDDiWE + oDGrenW&
		&E + oDBlueWE + oDZooE )
! 	Total_N_in_system
		aNRelotT = uDptWH * aReDeaWH * ( oNH4WH + oNO3WH + oNDtWH + oNDiWH + oNGrenWH + &
		&oNBlueWH + oNZooH ) + fMarsh * sDepthWM * aReDeaWM * ( oNH4WM + oNO3WM + oNDtWM &
		&+ oNDiWM + oNGrenWM + oNBlueWM + oNZooM ) + uDptWE * aReDeaWE * ( oNH4WE + oNO3W&
		&E + oNDtWE + oNDiWE + oNGrenWE + oNBlueWE + oNZooE )
! 	Total_P_in_system
		aPRelotT = uDptWH * aReDeaWH * ( oPO4WH + oPAIMWH + oPDtWH + oPDiWH + oPGrenWH +&
		& oPBlueWH + oPZooH ) + fMarsh * sDepthWM * aReDeaWM * (oPO4WM + oPAIMWM + oPDtWM&
		& + oPDiWM + oPGrenWM + oPBlueWM + oPZooM ) + uDptWE * aReDeaWE * (oPO4WE + oPAIM&
		&WE + oPDtWE + oPDiWE + oPGrenWE + oPBlueWE + oPZooE )
! 	Total_Si_in_system
		aSielotT = uDptWH * aReDeaWH * ( oSiO2WH + oSiDtWH + cSiDDi*oDDiWH) + fMarsh * s&
		&DepthWM * aReDeaWM * ( oSiO2WM + oSiDtWM + cSiDDi*oDDiWM) + uDptWE * aReDeaWE * &
		&( oSiO2WE + oSiDtWE + cSiDDi*oDDiWE)
! 	Total_Si_in_system
		aO2elotT = uDptWH * aReDeaWH* oO2WH + fMarsh * sDepthWM * aReDeaWM * oO2WM + uDp&
		&tWE * aReDeaWE * oO2WE 
! 	Total_DW_in_system
		if ( (aInlSrat == 1) ) then
		aDTotTH = (oDIMWH+oDDtWH+oDDiWH+oDGrenWH+oDBlueWH+oDZooH) * uDptWH 		else		aDTotTH = 0.0 		endif
! 	Total_N_in_system
		if ( (aInlSrat == 1) ) then
		aNTotTH = (oNH4WH+oNO3WH+oNDtWH+oNDiWH+oNGrenWH+oNBlueWH +oNZooH)*uDptWH 		else		aNTotTH = 0.0 		endif
! 	Total_P_in_system
		if ( (aInlSrat == 1) ) then
		aPTotTH = (oPO4WH+oPDtWH+oPDiWH+oPGrenWH+oPBlueWH +oPZooH+oPAIMWH) * uDptWH 		else		aPTotTH = 0.0 		endif
! 	Total_Si_in_system
		if ( (aInlSrat == 1) ) then
		aSiTotTH = (oSiO2WH + oSiDtWH + cSiDDi*oDDiWH) *uDptWH 		else		aSiTotTH = 0.0 		endif
! 	Total_Si_in_system
		if ( (aInlSrat == 1) ) then
		aO2TotTH = (oO2WH ) *uDptWH 		else		aO2TotTH = 0.0 		endif
! 	Total_DW_in_system
		aDTotT = aDTotTH + (oDIMWE+oDDtWE+oDDiWE+oDGrenWE+oDBlueWE+oDZooE) * uDptWE +aDF&
		&ish+sDPisc+aDVeg +sDIMS+sDHumS+sDDetS+sDDiatS+sDGrenS+sDBlueS+sDBent + aDTotM
! 	Total_N_in_system
		aNTotT = aNTotTH+ (oNH4WE+oNO3WE+oNDtWE+oNDiWE+oNGrenWE+oNBlueWE +oNZooE)*uDptWE&
		& +aNFish+aNPisc+aNVeg +sNH4S+sNO3S+sNDetS+sNHumS+sNDiatS+sNGrenS+sNBlueS +sNBent&
		& + aNTotM
! 	Total_P_in_system
		aPTotT = aPTotTH + (oPO4WE+oPDtWE+oPDiWE+oPGrenWE+oPBlueWE +oPZooE+oPAIMWE) * uD&
		&ptWE +aPFish+aPPisc+aPVeg +sPO4S+sPDetS+sPHumS+sPDiatS+sPGrenS+sPBlueS +sPAIMS+s&
		&PBent + aPTotM
! 	Total_Si_in_system
		aSiTotT = aSiTotTH + (oSiO2WE + oSiDtWE + cSiDDi*oDDiWE) *uDptWE + sSiDetS + cSi&
		&DDi*sDDiatS + aSiTotM
! 	Total_Si_in_system
		aO2TotT = aO2TotTH + ( oO2WE) *uDptWE + aO2TotM
! 	Total_DW_flux_from_Vegetation_module
		if ( InclV == 1 ) then
		tDBedotT = tDMigVeg + tDPodegH+ tDPodegE - tDResVeg - tDManVeg - tDAsVgBi 		else		tDBedotT = 0.0 		endif
! 	Total_P_flux_from_Vegetation_module
		if ( InclV == 1 ) then
		tPBedotT = tPMigVeg - tPManVeg - tPAsVgBi 		else		tPBedotT = 0.0 		endif
! 	Total_N_flux_from_Vegetation_module
		if ( InclV == 1 ) then
		tNBedotT = tNMigVeg - tNManVeg - tNAsVgBi 		else		tNBedotT = 0.0 		endif
! 	DW_mass_balance_error
		aDError = aDTotT - sDExTotT
! 	N_mass_balance_error
		aNError = aNTotT - sNExTotT
! 	P_mass_balance_error
		aPError = aPTotT - sPExTotT
! 	Si_mass_balance_error
		aSiError = aSiTotT - sSiETotT
! 	O2_mass_balance_error
		aO2Error = aO2TotT - sO2ETotT
! 	Check
		if ( ( aVWin == 1 .and.  uVeHeght < min (cVHeW, sDepthW) ) ) then
		cChVegH = 1.0 		else if ( ( aVWin == 1 .and.  uVeHeght > min (cVHeW, sDepthW) ) ) then		cChVegH = 2.0 		else if ( ( aVWin == 0 .and.  uVeHeght < min (cVHeight, sDepthW) ) ) then		cChVegH =  3.0 		else if ( ( aVWin == 0 .and.  uVeHeght > min (cVHeight, sDepthW) ) ) then		cChVegH = 4.0 		else		cChVegH = 0.0 		endif
! 	Derivative_for_N_ammonium_in_water_in_lake_water
		dNH4WH = ( wNTan4WH + wNAio4WH + wNPim4WH + wNBdN4WH + wNWbN4WH + uNBckadH / uDp&
		&tWH - aReDeaWH * oNH4WH - wNExNH4H - wNExN4WH  - wNAdvH4W / uDptWH ) * MassH
! 	Derivative_for_N_nitrate_in_water_in_lake_water
		dNO3WH = ( wNTan3WH + wNAio3WH + wNPim3WH + wNBdN3WH + wNWbN3WH - aReDeaWH * oNO&
		&3WH - wNExNO3H - wNExN3WH  - wNAdvO3W / uDptWH ) * MassH
! 	Derivative_for_P_SRP_in_water_in_lake_water
		dPO4WH = ( wPTan4WH + wPAio4WH + wPPim4WH + wPBdP4WH + wPWbP4WH + uPBckadH / uDp&
		&tWH - aReDeaWH * oPO4WH - wPExPO4H - wPExP4WH  - wPAdvO4W / uDptWH ) * MassH
! 	Derivative_for_P_P-adsorbed_onto_IM_in_water_in_lake_water
		dPAIMWH = ( wPTanMWH + wPAioMWH - aReDeaWH * oPAIMWH - wPExAIMH - wPExAMWH  - wP&
		&AdvIMW / uDptWH ) * MassH
! 	Derivative_for_Si_dissolved_silica_in_water_in_lake_water
		dSiO2WH = ( wSianO2H + wSiio2WH + wSiim2WH - aReDeaWH * oSiO2WH - wSixSO2H - wSi&
		&xS2WH  - wSidvO2W / uDptWH ) * MassH
! 	Derivative_for_O2_oxygen_in_water_in_lake_water
		dO2WH = ( wO2TrnWH + wO2AboWH + wO2PrmWH - tO2PrmSH + tO2BedWH / uDptWH - aReDea&
		&WH* oO2WH - wO2ExH - wO2ExWH  - wO2AdvW / uDptWH ) * MassH
! 	Derivative_for_DW_Detritus_in_lake_water
		dDDetWH = ( wDTantWH + wDAiotWH + wDPimtWH + wDBedtWH + wDWebtWH - aReDeaWH * oD&
		&DtWH - wDExDtH - wDExDtWH  - wDAdvDtW / uDptWH ) * MassH
! 	Derivative_for_N_Detritus_in_lake_water
		dNDetWH = ( wNTantWH + wNAiotWH + wNPimtWH + wNBedtWH + wNWebtWH -aReDeaWH * oND&
		&tWH - wNExDtH - wNExDtWH  - wNAdvDtW / uDptWH ) * MassH
! 	Derivative_for_P_Detritus_in_lake_water
		dPDetWH = ( wPTantWH + wPAiotWH + wPPimtWH + wPBedtWH + wPWebtWH - aReDeaWH * oP&
		&DtWH - wPExDtH - wPExDtWH  - wPAdvDtW / uDptWH ) * MassH
! 	Derivative_for_Si_Detritus_in_lake_water
		dSiDetWH = ( wSiratWH + wSibitWH + wSiritWH + wSiebtWH - aReDeaWH * oSiDtWH - wS&
		&iExDtH - wSiExtWH  - wSiAdDtW / uDptWH ) * MassH
! 	Derivative_for_DW_inorg_matter_in_water_in_lake_water
		dDIMWH = ( wDTanMWH + wDAioMWH - aReDeaWH * oDIMWH - wDExIMH - wDExIMWH  - wDAdv&
		&IMW / uDptWH ) * MassH
! 	Derivative_for_DW_Diatoms_in_lake_water
		dDDiatWH = ( wDTraDiH + wDPimiWH + wDWebiWH - aReDeaWH * oDDiWH - wDExDiH - wDEx&
		&DiWH  - wDAdvDiW / uDptWH ) * MassH
! 	Derivative_for_N_Diatoms_in_lake_water
		dNDiatWH = ( wNTraDiH + wNPimiWH + wNWebiWH -aReDeaWH * oNDiWH - wNExDiH - wNExD&
		&iWH  - wNAdvDiW / uDptWH ) * MassH
! 	Derivative_for_P_Diatoms_in_lake_water
		dPDiatWH = ( wPTraDiH + wPPimiWH + wPWebiWH -aReDeaWH * oPDiWH - wPExDiH - wPExD&
		&iWH  - wPAdvDiW / uDptWH ) * MassH
! 	Derivative_for_DW_Greens_in_lake_water
		dDGrenWH = ( wDTanenH + wDPmGnWH + wDWbGnWH - aReDeaWH* oDGrenWH - wDExGenH - wD&
		&EGrnWH  - wDAvGenW / uDptWH ) * MassH
! 	Derivative_for_N_Greens_in_lake_water
		dNGrenWH = ( wNTanenH + wNPmGnWH + wNWbGnWH - aReDeaWH * oNGrenWH - wNExGenH - w&
		&NEGrnWH  - wNAvGenW / uDptWH ) * MassH
! 	Derivative_for_P_Greens_in_lake_water
		dPGrenWH = ( wPTanenH + wPPmGnWH + wPWbGnWH - aReDeaWH * oPGrenWH - wPExGenH - w&
		&PEGrnWH  - wPAvGenW / uDptWH ) * MassH
! 	Derivative_for_DW_Blue_greens_in_lake_water
		dDBlueWH = ( wDTanueH + wDPmBeWH + wDWbBeWH - aReDeaWH* oDBlueWH - wDExBueH - wD&
		&EBleWH  - wDAvBueW / uDptWH ) * MassH
! 	Derivative_for_N_Blue_greens_in_lake_water
		dNBlueWH = ( wNTanueH + wNPmBeWH + wNWbBeWH - aReDeaWH * oNBlueWH - wNExBueH - w&
		&NEBleWH  - wNAvBueW / uDptWH ) * MassH
! 	Derivative_for_P_Blue_greens_in_lake_water
		dPBlueWH = ( wPTanueH + wPPmBeWH + wPWbBeWH -aReDeaWH * oPBlueWH - wPExBueH - wP&
		&EBleWH  - wPAvBueW / uDptWH ) * MassH
! 	Derivative_for_DW_Zooplankton_in_lake_water
		dDZooH = ( wDTanooH + wDWebooH - aReDeaWH * oDZooH - wDExZooH - wDExZoWH  - wDAd&
		&vooW / uDptWH ) * MassH
! 	Derivative_for_N_Zooplankton_in_lake_water
		dNZooH = ( wNTanooH + wNWebooH -aReDeaWH * oNZooH - wNExZooH - wNExZoWH  - wNAdv&
		&ooW / uDptWH ) * MassH
! 	Derivative_for_P_Zooplankton_in_lake_water
		dPZooH = ( wPTanooH + wPWebooH - aReDeaWH * oPZooH - wPExZooH - wPExZoWH  - wPAd&
		&vooW / uDptWH ) * MassH
! 	Derivative_for_DW_Adult_whitefish_in_lake_water
		dDFiAd = tDWebiAd
! 	Derivative_for_DW_Juvenile_whitefish_in_lake_water
		dDFiJv = tDWebiJv
! 	Derivative_for_N_Adult_whitefish_in_lake_water
		dNFiAd = tNWebiAd
! 	Derivative_for_N_Juvenile_whitefish_in_lake_water
		dNFiJv = tNWebiJv
! 	Derivative_for_P_Adult_whitefish_in_lake_water
		dPFiAd = tPWebiAd
! 	Derivative_for_P_Juvenile_whitefish_in_lake_water
		dPFiJv = tPWebiJv
! 	Derivative_for_DW_predatory_fish_in_lake_water
		dDPisc = tDWebisc
! 	Derivative_for_N_pore_water_ammonium_in_lake_water
		dNH4S = tNAioH4S - tNBurNH4 + tNPimH4S + tNBedH4S + tNWebH4S
! 	Derivative_for_N_pore_water_nitrate_in_lake_water
		dNO3S = tNAioO3S - tNBurNO3 + tNPimO3S + tNBedO3S + tNWebO3S
! 	Derivative_for_P_pore_water_SRP_in_lake_water
		dPO4S = tPAioO4S - tPBurPO4 + tPPimO4S + tPBedO4S + tPWebO4S
! 	Derivative_for_P_P-adsorbed_onto_IM_in_sediment_in_lake_sediment
		dPAIMS = tPAioIMS - tPBurAIM - tPDedIMS
! 	Derivative_for_DW_Sediment_detritus_in_lake_sediment
		dDDetS = tDAbiDtS - tDBurDt + tDPriDtS + tDBedtSE + tDBedtSH + tDWebDtS - tDDreD&
		&tS
! 	Derivative_for_N_Sediment_detritus_N_in_lake_sediment
		dNDetS = tNAbiDtS - tNBurDt + tNPriDtS + tNBedDtS + tNWebDtS - tNDreDtS
! 	Derivative_for_P_Sediment_detritus_P_in_lake_sediment
		dPDetS = tPAbiDtS - tPBurDt + tPPriDtS + tPBedDtS + tPWebDtS - tPDreDtS
! 	Derivative_for_Si_Sediment_detritus_Si_in_lake_sediment
		dSiDetS = tSibiDtS - tSiBurDt + tSiriDtS + tSiWeDtS - tSireDtS
! 	Derivative_for_DW_humus_in_lake_sediment
		dDHumS = tDAioumS - tDBurHum - tDDdNumS
! 	Derivative_for_N_humus_in_lake_sediment
		dNHumS = tNAioumS - tNBurHum - tNDdNumS
! 	Derivative_for_P_humus_in_lake_sediment
		dPHumS = tPAioumS - tPBurHum - tPDdNumS
! 	Derivative_for_DW_inorg_matter_in_sediment_in_lake_sediment
		dDIMS = tDAbiIMS - tDBurIM - tDDdNIMS
! 	Derivative_for_DW_Sed_Diatoms_in_lake_sediment
		dDDiatS= tDPriDiS + tDWebDiS - tDDreDiS
! 	Derivative_for_N_Sediment_diatoms_in_lake_sediment
		dNDiatS= tNPriDiS + tNWebDiS - tNDreDiS
! 	Derivative_for_P_Sediment_diatoms_in_lake_sediment
		dPDiatS= tPPriDiS + tPWebDiS - tPDreDiS
! 	Derivative_for_DW_Sed_Greens_in_lake_sediment
		dDGrenS= tDPimenS + tDWbGenS - tDDedenS
! 	Derivative_for_N_Sediment_green_algae_in_lake_sediment
		dNGrenS= tNPimenS + tNWbGenS - tPDedenS
! 	Derivative_for_P_Sediment_green_algae_in_lake_sediment
		dPGrenS= tPPimenS + tPWbGenS - tPDedenS
! 	Derivative_for_DW_Sed_Blue-greens_in_lake_sediment
		dDBlueS= tDPimueS + tDWbBueS - tDDedueS
! 	Derivative_for_N_Sediment_blue-greens_in_lake_sediment
		dNBlueS= tNPimueS + tNWbBueS - tNDedueS
! 	Derivative_for_P_Sediment_blue-greens_in_lake_sediment
		dPBlueS=tPPimueS + tPWbBueS - tPDedueS
! 	Derivative_for_DW_Vegetation_in_lake_sediment
		dDVeg = tDBedVeg - tDDreVeg
! 	Derivative_for_N_Vegetation_in_lake_sediment
		dNVeg = tNBedVeg - tNDreVeg
! 	Derivative_for_P_Vegetation_in_lake_sediment
		dPVeg = tPBedVeg - tPDreVeg
! 	Derivative_for_vegetation_height_change
		if ( ( aVWin == 1 .and.  uVeHeght < min (cVHeW, sDepthW) ) ) then
		dVegHe =  min ( cVHeW - uVeHeght , ( min (sDepthW , max ( cVHeW , cVHeight) ) - &
		& min ( cVHeW , cVHeight , sDepthW) ) / cLengllo) 		else if ( ( aVWin == 1 .and.  uVeHeght > min (cVHeW, sDepthW) ) ) then		dVegHe = max (cVHeW - uVeHeght , ( min ( cVHeW , cVHeight , sDepthW) - min (sDep&
		&thW , max ( cVHeW , cVHeight) ) ) / cLengM) 		else if ( ( aVWin == 0 .and.  uVeHeght < min (cVHeight, sDepthW) ) ) then		dVegHe =  min ( cVHeight - uVeHeght , ( min (sDepthW , max ( cVHeW , cVHeight) )&
		& -  min ( cVHeW , cVHeight , sDepthW) ) / cLengllo) 		else if ( ( aVWin == 0 .and.  uVeHeght > min (cVHeight, sDepthW) ) ) then		dVegHe = max ( uVeHeght -  cVHeight, ( min ( cVHeW , cVHeight , sDepthW) - min (&
		&sDepthW , max ( cVHeW , cVHeight) ) ) / cLengM) 		else		dVegHe = 0.0 		endif
! 	Derivative_for_DW_Zoobenthos_in_lake_sediment
		dDBent = tDWebBnt - tDDreBnt
! 	Derivative_for_N_Zoobenthos_in_lake_sediment
		dNBent = tNWebBnt - tNDreBnt
! 	Derivative_for_P_Zoobenthos_in_lake_sediment
		dPBent = tPWebBnt - tPDreBnt
! 	Derivative_for_water_depth_change_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDepthWM = vTranptW + vDeltaWM 		else		dDepthWM = 0.0 		endif
! 	Derivative_for_N_NH4_in_water_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNH4WM = ( tNDifH4M/sDepthWM - wNNitrWM + wNMintWM - tNEvN4WM/sDepthWM - tNIfN4W&
		&M/sDepthWM + wNExN4ME + wNExN4MH - aReDeaWM * oNH4WM + wNANH4WM/sDepthWM) * Mass&
		&WM 		else		dNH4WM = 0.0 		endif
! 	Derivative_for_N_NO3_in_water_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNO3WM = ( tNDifO3M/sDepthWM + wNNitrWM - wNDentWM - tNEvN3WM/sDepthWM - tNIfN3W&
		&M/sDepthWM + wNExN3ME + wNExN3MH- aReDeaWM * oNO3WM + wNANO3WM/sDepthWM) * MassW&
		&M 		else		dNO3WM = 0.0 		endif
! 	Derivative_for_P_PO4_in_water_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPO4WM = ( - tPIfP4WM / sDepthWM + tPDifO4M / sDepthWM + wPMintWM - tPEvP4WM / s&
		&DepthWM - wPSrpMWM + wPExP4ME + wPExP4MH - aReDeaWM * oPO4WM + wPAPO4WM/sDepthWM&
		&) * MassWM 		else		dPO4WM = 0.0 		endif
! 	Derivative_for_P_P_adsorbed_onto_IM_in_water_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPAIMWM = ( - tPSetIMM / sDepthWM + wPSrpMWM + wPExAMME+ wPExAMMH - aReDeaWM * o&
		&PAIMWM + wPAAIMWM/sDepthWM) * MassWM 		else		dPAIMWM = 0.0 		endif
! 	Derivative_for_Si_sio2_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dSiO2WM = ( wSiintWM + tSiintSM / sDepthWM + wSixS2ME + wSixS2MH- aReDeaWM * oSi&
		&O2WM + wSiAO2WM/sDepthWM) * MassWM 		else		dSiO2WM = 0.0 		endif
! 	Derivative_for_O2_O2_in_water_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dO2WM = ( wO2AbioM + wO2ExME + wO2ExMH - aReDeaWM * oO2WM - tO2lohra / sDepthWM &
		&+ wO2AWM/sDepthWM) * MassWM 		else		dO2WM = 0.0 		endif
! 	Derivative_for_DW_Detritus_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDDetWM = ( tDMShhra/sDepthWM - tDSetDtM/sDepthWM - wDMintWM + wDExDtME + wDExDt&
		&MH- aReDeaWM * oDDtWM + wDADeWM/sDepthWM) * MassWM 		else		dDDetWM = 0.0 		endif
! 	Derivative_for_N_detritus_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNDetWM = ( tNMShhra / sDepthWM - tNSetDtM / sDepthWM - wNMintWM + wNExDtME + wN&
		&ExDtMH - aReDeaWM * oNDtWM + wNADeWM/sDepthWM) * MassWM 		else		dNDetWM = 0.0 		endif
! 	Derivative_for_P_detritus_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPDetWM = ( tPMShhra / sDepthWM - tPSetDtM / sDepthWM - wPMintWM + wPExDtME + wP&
		&ExDtMH - aReDeaWM * oPDtWM + wPADeWM/sDepthWM) * MassWM 		else		dPDetWM = 0.0 		endif
! 	Derivative_for_Si_detritus_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dSiDetWM = ( - tSiSeDtM / sDepthWM - wSiintWM + wSiExtME + wSiExtMH - aReDeaWM *&
		& oSiDtWM + wSiADeWM/sDepthWM) * MassWM 		else		dSiDetWM = 0.0 		endif
! 	Derivative_for_DW_Inorg_matter_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDIMWM = ( - tDSetIMM/sDepthWM + wDExIMME + wDExIMMH - aReDeaWM * oDIMWM + wDAIM&
		&WM/sDepthWM) * MassWM 		else		dDIMWM = 0.0 		endif
! 	Derivative_for_DW_diatoms_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDDiatWM = ( wDExDiME +wDExDiMH - tDSetDiM / sDepthWM - aReDeaWM * oDDiWM + wDAD&
		&iWM/sDepthWM) * MassWM 		else		dDDiatWM = 0.0 		endif
! 	Derivative_for_N_diatoms_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNDiatWM = ( wNExDiME + wNExDiMH - tNSetDiM / sDepthWM - aReDeaWM * oNDiWM + wNA&
		&DiWM/sDepthWM) * MassWM 		else		dNDiatWM = 0.0 		endif
! 	Derivative_for_P_diatoms_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPDiatWM = ( wPExDiME +wPExDiMH - tPSetDiM / sDepthWM - aReDeaWM * oPDiWM + wPAD&
		&iWM/sDepthWM) * MassWM 		else		dPDiatWM = 0.0 		endif
! 	Derivative_for_DW_greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDGrenWM = ( wDEGrnME + wDEGrnMH- tDStGenM / sDepthWM - aReDeaWM * oDGrenWM + wD&
		&AGWM/sDepthWM) * MassWM 		else		dDGrenWM = 0.0 		endif
! 	Derivative_for_N_greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNGrenWM = ( wNEGrnME + wNEGrnMH - tNStGenM / sDepthWM - aReDeaWM * oNGrenWM + w&
		&NAGWM/sDepthWM) * MassWM 		else		dNGrenWM = 0.0 		endif
! 	Derivative_for_P_greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPGrenWM = ( wPEGrnME + wPEGrnMH - tPStGenM / sDepthWM - aReDeaWM * oPGrenWM + w&
		&PAGWM/sDepthWM) * MassWM 		else		dPGrenWM = 0.0 		endif
! 	Derivative_for_DW_blue-greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDBlueWM = ( wDEBleME + wDEBleMH- tDStBueM / sDepthWM - aReDeaWM * oDBlueWM + wD&
		&ABWM/sDepthWM) * MassWM 		else		dDBlueWM = 0.0 		endif
! 	Derivative_for_N_blue-greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNBlueWM = ( wNEBleME + wNEBleMH - tNStBueM / sDepthWM - aReDeaWM * oNBlueWM + w&
		&NABWM/sDepthWM) * MassWM 		else		dNBlueWM = 0.0 		endif
! 	Derivative_for_P_blue-greens_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPBlueWM = ( wPEBleME + wPEBleMH - tPStBueM / sDepthWM - aReDeaWM * oPBlueWM + w&
		&PABWM/sDepthWM) * MassWM 		else		dPBlueWM = 0.0 		endif
! 	Derivative_for_DW_zooplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDZooM = ( wDExZoME + wDExZoMH - aReDeaWM * oDZooM + wDAZWM/sDepthWM) * MassWM 		else		dDZooM = 0.0 		endif
! 	Derivative_for_N_zooplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNZooM = ( wNExZoME + wNExZoMH - aReDeaWM * oNZooM + wNAZWM/sDepthWM) * MassWM 		else		dNZooM = 0.0 		endif
! 	Derivative_for_P_zooplankton_in_marsh_water
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPZooM = ( wPExZoME + wPExZoMH - aReDeaWM * oPZooM + wPAZWM/sDepthWM) * MassWM 		else		dPZooM = 0.0 		endif
! 	Derivative_for_N_NH4_in_water_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNH4SM = tNIfN4WM - tNIfN4SM +(1.0-fRefrDeS) * tNMintSM + tNMnHmSM - tNDifH4M - &
		&tNDroH4M - tNNitrSM - tNBurH4M - tNUH4raS + tNEvN4WM 		else		dNH4SM = 0.0 		endif
! 	Derivative_for_N_NO3_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNO3SM = tNIfN3WM - tNIfN3SM + tNNitrSM - tNDentSM - tNDifO3M - tNDroO3M - tNBur&
		&O3M - tNUO3raS + tNEvN3WM 		else		dNO3SM = 0.0 		endif
! 	Derivative_for_P_PO4_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPO4SM = tPIfP4WM - tPIfP4SM + tPEvP4WM +(1.0-fRefrDeS) * tPMintSM + tPMnHmSM - &
		&tPSrpMSM - tPDifO4M - tPDroO4M - tPCemO4M - tPUPhraS - tPBurO4M 		else		dPO4SM = 0.0 		endif
! 	Derivative_for_P_P_adsorbed_onto_IM_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPAIMSM = tPSetIMM - tPBurIMM + tPSrpMSM 		else		dPAIMSM = 0.0 		endif
! 	Derivative_for_DW_Detritus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDDetSM = tDMRthra + tDSetDtM - tDMintSM + tDStPytM - tDBurDtM 		else		dDDetSM = 0.0 		endif
! 	Derivative_for_N_detritus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNDetSM = tNMRthra + tNSetDtM - tNMintSM + tNStPytM - tNBurDtM 		else		dNDetSM = 0.0 		endif
! 	Derivative_for_P_detritus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPDetSM = tPMRthra + tPSetDtM - tPMintSM + tPStPytM - tPBurDtM 		else		dPDetSM = 0.0 		endif
! 	Derivative_for_P_detritus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dSiDetSM = tSiSeDtM - tSiintSM + cSiDDi * tDSetDiM - tSiBuDtM 		else		dSiDetSM = 0.0 		endif
! 	Derivative_for_DW_sediment_humus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDHumSM = fRefrDeS * tDMintSM - tDMnHmSM - tDBurumM 		else		dDHumSM = 0.0 		endif
! 	Derivative_for_N_sediment_humus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNHumSM = fRefrDeS * tNMintSM - tNMnHmSM - tNBurumM 		else		dNHumSM = 0.0 		endif
! 	Derivative_for_P_sediment_humus_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPHumSM = fRefrDeS * tPMintSM - tPMnHmSM - tPBurumM 		else		dPHumSM = 0.0 		endif
! 	Derivative_for_DW_Inorg_matter_in_sediment_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDIMSM = tDSetIMM - tDBurIMM 		else		dDIMSM = 0.0 		endif
! 	Derivative_for_DW_biomass_root_reed_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDRoPhra = tDPdRhra - tDRpRhra - tDMRthra - tDAllhra + tDRalhra 		else		dDRoPhra = 0.0 		endif
! 	Derivative_for_DW_biomass_shoot_reed_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dDShPhra = tDPdShra - tDRpShra - tDMShhra + tDAllhra - tDRalhra - tDMnShra 		else		dDShPhra = 0.0 		endif
! 	Derivative_for_N_N_in_root_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNRoPhra = tNURthra - tNMRthra - tNTanhra + tNRtrhra 		else		dNRoPhra = 0.0 		endif
! 	Derivative_for_N_N_in_shoot_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dNShPhra = tNUShhra - tNMShhra + tNTanhra - tNRtrhra - tNMnShra 		else		dNShPhra = 0.0 		endif
! 	Derivative_for_P_P_in_root_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPRoPhra = tPURthra - tPMRthra - tPTanhra + tPRtrhra 		else		dPRoPhra = 0.0 		endif
! 	Derivative_for_P_P_in_shoot_in_marsh_sediment
		if ( (InclMrsh == 1 .and. fMarsh > NearZero) ) then
		dPShPhra = tPUShhra - tPMShhra + tPTanhra - tPRtrhra - tPMnShra 		else		dPShPhra = 0.0 		endif
! 	Derivative_for_N_ammonium_in_water_in_lake_water
		dNH4WE = ( wNTan4WE + wNAio4WE + wNPim4WE + wNBdN4WE + wNWbN4WE + cNBckoad / uDp&
		&tWE - aReDeaWE * oNH4WE - wNExNH4E + wNExN4WE  + wNAdvH4W / uDptWE  - wNANH4WM/s&
		&DepthWM * afVlMshE) * MassE
! 	Derivative_for_N_nitrate_in_water_in_lake_water
		dNO3WE = ( wNTan3WE + wNAio3WE + wNPim3WE + wNBdN3WE + wNWbN3WE - aReDeaWE * oNO&
		&3WE - wNExNO3E + wNExN3WE  + wNAdvO3W / uDptWE  - wNANO3WM/sDepthWM * afVlMshE) &
		&* MassE
! 	Derivative_for_P_SRP_in_water_in_lake_water
		dPO4WE = ( wPTan4WE + wPAio4WE + wPPim4WE + wPBdP4WE + wPWbP4WE + cPBckoad / uDp&
		&tWE - aReDeaWE * oPO4WE - wPExPO4E + wPExP4WE  + wPAdvO4W / uDptWE  - wPAPO4WM/s&
		&DepthWM * afVlMshE) * MassE
! 	Derivative_for_P_P-adsorbed_onto_IM_in_water_in_lake_water
		dPAIMWE = ( wPTanMWE + wPAioMWE - aReDeaWE * oPAIMWE - wPExAIME + wPExAMWE  + wP&
		&AdvIMW / uDptWE  - wPAAIMWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_Si_dissolved_silica_in_water_in_lake_water
		dSiO2WE = ( wSianO2E + wSiio2WE + wSiim2WE - aReDeaWE * oSiO2WE - wSixSO2E + wSi&
		&xS2WE  + wSidvO2W / uDptWE  - wSiAO2WM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_O2_oxygen_in_water_in_lake_water
		dO2WE = ( wO2TrnWE + wO2AboWE + wO2PrmWE -tO2PrmSE + tO2BedWE / uDptWE - aReDeaW&
		&E * oO2WE - wO2ExE + wO2ExWE  + wO2AdvW / uDptWE  - wO2AWM/sDepthWM * afVlMshE) &
		&* MassE
! 	Derivative_for_DW_Detritus_in_lake_water
		dDDetWE = ( wDTantWE + wDAiotWE + wDPimtWE + wDBedtWE + wDWebtWE - aReDeaWE * oD&
		&DtWE - wDExDtE + wDExDtWE  + wDAdvDtW / uDptWE  - wDADeWM/sDepthWM * afVlMshE) *&
		& MassE
! 	Derivative_for_N_Detritus_in_lake_water
		dNDetWE = ( wNTantWE + wNAiotWE + wNPimtWE + wNBedtWE + wNWebtWE - aReDeaWE * oN&
		&DtWE - wNExDtE + wNExDtWE  + wNAdvDtW / uDptWE  - wNADeWM/sDepthWM * afVlMshE) *&
		& MassE
! 	Derivative_for_P_Detritus_in_lake_water
		dPDetWE = ( wPTantWE + wPAiotWE + wPPimtWE + wPBedtWE + wPWebtWE - aReDeaWE * oP&
		&DtWE - wPExDtE + wPExDtWE  + wPAdvDtW / uDptWE  - wPADeWM/sDepthWM * afVlMshE) *&
		& MassE
! 	Derivative_for_Si_Detritus_in_lake_water
		dSiDetWE = ( wSiratWE + wSibitWE + wSiritWE + wSiebtWE - aReDeaWE * oSiDtWE - wS&
		&iExDtE + wSiExtWE  + wSiAdDtW / uDptWE  - wSiADeWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_DW_inorg_matter_in_water_in_lake_water
		dDIMWE = ( wDTanMWE + wDAioMWE - aReDeaWE * oDIMWE - wDExIME + wDExIMWE  + wDAdv&
		&IMW / uDptWE  - wDAIMWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_DW_Diatoms_in_lake_water
		dDDiatWE = ( wDTraDiE + wDPimiWE + wDWebiWE - aReDeaWE * oDDiWE - wDExDiE + wDEx&
		&DiWE  + wDAdvDiW / uDptWE  - wDADiWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_N_Diatoms_in_lake_water
		dNDiatWE = ( wNTraDiE + wNPimiWE + wNWebiWE - aReDeaWE * oNDiWE - wNExDiE + wNEx&
		&DiWE  + wNAdvDiW / uDptWE  - wNADiWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_P_Diatoms_in_lake_water
		dPDiatWE = ( wPTraDiE + wPPimiWE + wPWebiWE - aReDeaWE * oPDiWE - wPExDiE + wPEx&
		&DiWE  + wPAdvDiW / uDptWE  - wPADiWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_DW_Greens_in_lake_water
		dDGrenWE = ( wDTanenE + wDPmGnWE + wDWbGnWE - aReDeaWE * oDGrenWE - wDExGenE + w&
		&DEGrnWE  + wDAvGenW / uDptWE  - wDAGWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_N_Greens_in_lake_water
		dNGrenWE = ( wNTanenE + wNPmGnWE + wNWbGnWE - aReDeaWE * oNGrenWE - wNExGenE + w&
		&NEGrnWE  + wNAvGenW / uDptWE  - wNAGWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_P_Greens_in_lake_water
		dPGrenWE = ( wPTanenE + wPPmGnWE + wPWbGnWE - aReDeaWE * oPGrenWE - wPExGenE + w&
		&PEGrnWE  + wPAvGenW / uDptWE  - wPAGWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_DW_Blue_greens_in_lake_water
		dDBlueWE = ( wDTanueE + wDPmBeWE + wDWbBeWE - aReDeaWE * oDBlueWE - wDExBueE + w&
		&DEBleWE  + wDAvBueW / uDptWE  - wDABWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_N_Blue_greens_in_lake_water
		dNBlueWE = ( wNTanueE + wNPmBeWE + wNWbBeWE - aReDeaWE * oNBlueWE - wNExBueE + w&
		&NEBleWE  + wNAvBueW / uDptWE  - wNABWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_P_Blue_greens_in_lake_water
		dPBlueWE = ( wPTanueE + wPPmBeWE + wPWbBeWE - aReDeaWE * oPBlueWE - wPExBueE + w&
		&PEBleWE  + wPAvBueW / uDptWE  - wPABWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_DW_Zooplankton_in_lake_water
		dDZooE =  ( wDTanooE + wDWebooE - aReDeaWE * oDZooE - wDExZooE + wDExZoWE  + wDA&
		&dvooW / uDptWE  - wDAZWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_N_Zooplankton_in_lake_water
		dNZooE =  ( wNTanooE + wNWebooE - aReDeaWE * oNZooE - wNExZooE + wNExZoWE  + wNA&
		&dvooW / uDptWE  - wNAZWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_P_Zooplankton_in_lake_water
		dPZooE =  ( wPTanooE + wPWebooE - aReDeaWE * oPZooE - wPExZooE + wPExZoWE  + wPA&
		&dvooW / uDptWE  - wPAZWM/sDepthWM * afVlMshE) * MassE
! 	Derivative_for_total_external_DW_flux_
		dDExTotT = uDLoad - wDOtfotE*uDptWE - wDOtfotH*uDptWH + wDTanooE * uDptWE + wDTa&
		&nooH * uDptWH - tDBurTot + tDAiootT + tDPimotT + tDBedotT + tDWebotT+ tDMrsotT -&
		& tDDdNTot - aDRelotT
! 	Derivative_for_total_external_N_flux_
		dNExTotT = uNLoad - wNOtfotE * uDptWE - wNOtfotH * uDptWH+ wNTanooH * uDptWH + w&
		&NTanooE * uDptWE + cNBckoad - tNBurTot + tNAiootT + tNPimotT + tNBedotT + tNWebo&
		&tT + tNMrsotT - tNDdNTot - aNRelotT
! 	Derivative_for_total_external_P_flux_
		dPExTotT = uPLoad - wPOtfotH * uDptWH - wPOtfotE * uDptWE + wPTanooH * uDptWH + &
		&wPTanooE * uDptWE + cPBckoad - tPBurTot + tPAiootT + tPPimotT + tPBedotT + tPWeb&
		&otT + tPMrsotT- tPDdNTot - aPRelotT
! 	Derivative_for_total_external_Si_flux_
		dSiETotT = uSiLoad - wSiilotH*uDptWH - wSiilotE*uDptWE + tSibiotT - tSiBuTot + t&
		&SiriotT + tSiarotT - tSireTot +tSiebotT - aSielotT
! 	Derivative_for_total_external_O2_flux_
		dO2ETotT = wO2TrnWH * uDptWH + wO2TrnWE * uDptWE + tO2BedWH + tO2BedWE + wO2PrmW&
		&H * uDptWH + wO2PrmWE * uDptWE + wO2AboWH * uDptWH + wO2AboWE * uDptWE + wO2Abio&
		&M* sDepthWM * fMarsh - tO2PrimS - tO2lohra * fMarsh - aO2elotT
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
dVegHe=dVegHe/sDepthW
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
dDRoPhra=dDRoPhra/sDepthW
dDShPhra=dDShPhra/sDepthW
dNRoPhra=dNRoPhra/sDepthW
dNShPhra=dNShPhra/sDepthW
dPRoPhra=dPRoPhra/sDepthW
dPShPhra=dPShPhra/sDepthW
dDExTotT=dDExTotT/sDepthW
dNExTotT=dNExTotT/sDepthW
dPExTotT=dPExTotT/sDepthW
dSiETotT=dSiETotT/sDepthW
dO2ETotT=dO2ETotT/sDepthW
if (counter1 == 0) then
open (unit=1,file="initrep.txt",action="write",status="replace")
write (1,*)"Type Id Name Set0 Set1 Set2 Set3 Rel0-1 Rel0-2 Rel0-3 Dif0-1 Dif0-2 Dif0-3"
write (1,*)" Parameter =IF(COUNTBLANK(A3)<1,ROW()-2,-1)_InitCalc_", InitCalc
write (1,*)" Parameter =IF(COUNTBLANK(A4)<1,ROW()-2,-1)_CalcMass_", CalcMass
write (1,*)" Parameter =IF(COUNTBLANK(A5)<1,ROW()-2,-1)_ConstDepth_", ContDpth
write (1,*)" Parameter =IF(COUNTBLANK(A6)<1,ROW()-2,-1)_InitDepth_", InitDpth
write (1,*)" Parameter =IF(COUNTBLANK(A7)<1,ROW()-2,-1)_ConstTrans_", ContTans
write (1,*)" Parameter =IF(COUNTBLANK(A8)<1,ROW()-2,-1)_InclTran_", InclTran
write (1,*)" Parameter =IF(COUNTBLANK(A9)<1,ROW()-2,-1)_InclVeg_", InclV
write (1,*)" Parameter =IF(COUNTBLANK(A10)<1,ROW()-2,-1)_InclBur_", InclBur
write (1,*)" Parameter =IF(COUNTBLANK(A11)<1,ROW()-2,-1)_InclWeb_", InclWeb
write (1,*)" Parameter =IF(COUNTBLANK(A12)<1,ROW()-2,-1)_InclMarsh_", InclMrsh
write (1,*)" Parameter =IF(COUNTBLANK(A13)<1,ROW()-2,-1)_InclSeason_", IncSeson
write (1,*)" Parameter =IF(COUNTBLANK(A14)<1,ROW()-2,-1)_InclStrat_", InclSrat
write (1,*)" Parameter =IF(COUNTBLANK(A15)<1,ROW()-2,-1)_calcMixDepth_", calixpth
write (1,*)" Parameter =IF(COUNTBLANK(A16)<1,ROW()-2,-1)_InclLat_", InclLat
write (1,*)" Parameter =IF(COUNTBLANK(A17)<1,ROW()-2,-1)_calcQEv_", calcQEv
write (1,*)" Parameter =IF(COUNTBLANK(A18)<1,ROW()-2,-1)_DayStart_", DayStart
write (1,*)" Parameter =IF(COUNTBLANK(A19)<1,ROW()-2,-1)_ReadTemp_", ReadTemp
write (1,*)" Parameter =IF(COUNTBLANK(A20)<1,ROW()-2,-1)_ReadStrat_", ReadSrat
write (1,*)" Parameter =IF(COUNTBLANK(A21)<1,ROW()-2,-1)_ReadLOut_", ReadLOut
write (1,*)" Parameter =IF(COUNTBLANK(A22)<1,ROW()-2,-1)_ReadVWind_", ReadVind
write (1,*)" Parameter =IF(COUNTBLANK(A23)<1,ROW()-2,-1)_ReadQIn_", ReadQIn
write (1,*)" Parameter =IF(COUNTBLANK(A24)<1,ROW()-2,-1)_ReadQOut_", ReadQOut
write (1,*)" Parameter =IF(COUNTBLANK(A25)<1,ROW()-2,-1)_ReadQEv_", ReadQEv
write (1,*)" Parameter =IF(COUNTBLANK(A26)<1,ROW()-2,-1)_ReadPLoad_", ReadPoad
write (1,*)" Parameter =IF(COUNTBLANK(A27)<1,ROW()-2,-1)_ReadNLoad_", ReadNoad
write (1,*)" Parameter =IF(COUNTBLANK(A28)<1,ROW()-2,-1)_ReadNutFrac_", ReaNurac
write (1,*)" Parameter =IF(COUNTBLANK(A29)<1,ROW()-2,-1)_ReadPLoadPhyt_", ReaPLadP
write (1,*)" Parameter =IF(COUNTBLANK(A30)<1,ROW()-2,-1)_ReadDLoadDet_", ReaDLdDe
write (1,*)" Parameter =IF(COUNTBLANK(A31)<1,ROW()-2,-1)_ReadVegShade_", ReaVSade
write (1,*)" Parameter =IF(COUNTBLANK(A32)<1,ROW()-2,-1)_UsePowerVeg_", UsePoerV
write (1,*)" Parameter =IF(COUNTBLANK(A33)<1,ROW()-2,-1)_UsePhotVeg_", UsePhotV
write (1,*)" Parameter =IF(COUNTBLANK(A34)<1,ROW()-2,-1)_UseLWinVeg_", UseLWinV
write (1,*)" Parameter =IF(COUNTBLANK(A35)<1,ROW()-2,-1)_UseVegHeight_", UseVHe
write (1,*)" Parameter =IF(COUNTBLANK(A36)<1,ROW()-2,-1)_ReadDLoadIM_", ReaDLdIM
write (1,*)" Parameter =IF(COUNTBLANK(A37)<1,ROW()-2,-1)_UseSeasonLoad_", Useasoad
write (1,*)" Parameter =IF(COUNTBLANK(A38)<1,ROW()-2,-1)_UsePulseLoad_", Uselsoad
write (1,*)" Parameter =IF(COUNTBLANK(A39)<1,ROW()-2,-1)_mTempEpi_", mTempE
write (1,*)" Parameter =IF(COUNTBLANK(A40)<1,ROW()-2,-1)_mTempHyp_", mTempH
write (1,*)" Parameter =IF(COUNTBLANK(A41)<1,ROW()-2,-1)_mStrat_", mStrat
write (1,*)" Parameter =IF(COUNTBLANK(A42)<1,ROW()-2,-1)_mLOut_", mLOut
write (1,*)" Parameter =IF(COUNTBLANK(A43)<1,ROW()-2,-1)_mVegShade_", mVShade
write (1,*)" Parameter =IF(COUNTBLANK(A44)<1,ROW()-2,-1)_mVWind_", mVWind
write (1,*)" Parameter =IF(COUNTBLANK(A45)<1,ROW()-2,-1)_mQIn_", mQIn
write (1,*)" Parameter =IF(COUNTBLANK(A46)<1,ROW()-2,-1)_mQOut_", mQOut
write (1,*)" Parameter =IF(COUNTBLANK(A47)<1,ROW()-2,-1)_mQEv_", mQEv
write (1,*)" Parameter =IF(COUNTBLANK(A48)<1,ROW()-2,-1)_mPLoad_", mPLoad
write (1,*)" Parameter =IF(COUNTBLANK(A49)<1,ROW()-2,-1)_mPLoadPO4_", mPLoaPO4
write (1,*)" Parameter =IF(COUNTBLANK(A50)<1,ROW()-2,-1)_mPLoadOrg_", mPLoaOrg
write (1,*)" Parameter =IF(COUNTBLANK(A51)<1,ROW()-2,-1)_mPLoadPhytTot_", mPLadTot
write (1,*)" Parameter =IF(COUNTBLANK(A52)<1,ROW()-2,-1)_mNLoad_", mNLoad
write (1,*)" Parameter =IF(COUNTBLANK(A53)<1,ROW()-2,-1)_mNLoadNH4_", mNLoaNH4
write (1,*)" Parameter =IF(COUNTBLANK(A54)<1,ROW()-2,-1)_mNLoadNO3_", mNLoaNO3
write (1,*)" Parameter =IF(COUNTBLANK(A55)<1,ROW()-2,-1)_mNLoadOrg_", mNLoaOrg
write (1,*)" Parameter =IF(COUNTBLANK(A56)<1,ROW()-2,-1)_mDLoadDet_", mDLoadDe
write (1,*)" Parameter =IF(COUNTBLANK(A57)<1,ROW()-2,-1)_mDLoadIM_", mDLoadIM
write (1,*)" Parameter =IF(COUNTBLANK(A58)<1,ROW()-2,-1)_StartTime_", StTime
write (1,*)" Parameter =IF(COUNTBLANK(A59)<1,ROW()-2,-1)_EndTime_", EndTime
write (1,*)" Parameter =IF(COUNTBLANK(A60)<1,ROW()-2,-1)_YearZero_", YearZero
write (1,*)" Parameter =IF(COUNTBLANK(A61)<1,ROW()-2,-1)_cDepthWInit0_", cDepthW0
write (1,*)" Parameter =IF(COUNTBLANK(A62)<1,ROW()-2,-1)_cMinDepthHypEpi_", cMnephHE
write (1,*)" Parameter =IF(COUNTBLANK(A63)<1,ROW()-2,-1)_cFetch_", cFetch
write (1,*)" Parameter =IF(COUNTBLANK(A64)<1,ROW()-2,-1)_fMarsh_", fMarsh
write (1,*)" Parameter =IF(COUNTBLANK(A65)<1,ROW()-2,-1)_fLutum_", fLutum
write (1,*)" Parameter =IF(COUNTBLANK(A66)<1,ROW()-2,-1)_kExchW_", kExchW
write (1,*)" Parameter =IF(COUNTBLANK(A67)<1,ROW()-2,-1)_kExchMaxW_", kExchMxW
write (1,*)" Parameter =IF(COUNTBLANK(A68)<1,ROW()-2,-1)_cDepthMix_", cDeptMix
write (1,*)" Parameter =IF(COUNTBLANK(A69)<1,ROW()-2,-1)_cAMix_", cAMix
write (1,*)" Parameter =IF(COUNTBLANK(A70)<1,ROW()-2,-1)_cBMix_", cBMix
write (1,*)" Parameter =IF(COUNTBLANK(A71)<1,ROW()-2,-1)_cfHyp_", cfH
write (1,*)" Parameter =IF(COUNTBLANK(A72)<1,ROW()-2,-1)_fFeDIM_", fFeDIM
write (1,*)" Parameter =IF(COUNTBLANK(A73)<1,ROW()-2,-1)_fAlDIM_", fAlDIM
write (1,*)" Parameter =IF(COUNTBLANK(A74)<1,ROW()-2,-1)_fVegShade_", fVShade
write (1,*)" Parameter =IF(COUNTBLANK(A75)<1,ROW()-2,-1)_cTmAveEpi_", cTmAveE
write (1,*)" Parameter =IF(COUNTBLANK(A76)<1,ROW()-2,-1)_cTmVarEpi_", cTmVarE
write (1,*)" Parameter =IF(COUNTBLANK(A77)<1,ROW()-2,-1)_cTmAveHyp_", cTmAveH
write (1,*)" Parameter =IF(COUNTBLANK(A78)<1,ROW()-2,-1)_cTmVarHyp_", cTmVarH
write (1,*)" Parameter =IF(COUNTBLANK(A79)<1,ROW()-2,-1)_cStratTm_", cStratTm
write (1,*)" Parameter =IF(COUNTBLANK(A80)<1,ROW()-2,-1)_cTimeLag_", cTimeLag
write (1,*)" Parameter =IF(COUNTBLANK(A81)<1,ROW()-2,-1)_cVWind_", cVWind
write (1,*)" Parameter =IF(COUNTBLANK(A82)<1,ROW()-2,-1)_cQInf_", cQInf
write (1,*)" Parameter =IF(COUNTBLANK(A83)<1,ROW()-2,-1)_cPBackLoad_", cPBckoad
write (1,*)" Parameter =IF(COUNTBLANK(A84)<1,ROW()-2,-1)_cNBackLoad_", cNBckoad
write (1,*)" Parameter =IF(COUNTBLANK(A85)<1,ROW()-2,-1)_cLDayAve_", cLDayAve
write (1,*)" Parameter =IF(COUNTBLANK(A86)<1,ROW()-2,-1)_cLDayVar_", cLDayVar
write (1,*)" Parameter =IF(COUNTBLANK(A87)<1,ROW()-2,-1)_cfDayAve_", cfDayAve
write (1,*)" Parameter =IF(COUNTBLANK(A88)<1,ROW()-2,-1)_cfDayVar_", cfDayVar
write (1,*)" Parameter =IF(COUNTBLANK(A89)<1,ROW()-2,-1)_fRefl_", fRefl
write (1,*)" Parameter =IF(COUNTBLANK(A90)<1,ROW()-2,-1)_cExtWat_", cExtWat
write (1,*)" Parameter =IF(COUNTBLANK(A91)<1,ROW()-2,-1)_cDredInterval_", cDrInval
write (1,*)" Parameter =IF(COUNTBLANK(A92)<1,ROW()-2,-1)_cDredStart_", cDrdSart
write (1,*)" Parameter =IF(COUNTBLANK(A93)<1,ROW()-2,-1)_cDepthRef_", cDeptRef
write (1,*)" Parameter =IF(COUNTBLANK(A94)<1,ROW()-2,-1)_cLengDred_", cLengred
write (1,*)" Parameter =IF(COUNTBLANK(A95)<1,ROW()-2,-1)_fEffDred_", fEffDred
write (1,*)" Parameter =IF(COUNTBLANK(A96)<1,ROW()-2,-1)_fEffDredBent_", fEfreent
write (1,*)" Parameter =IF(COUNTBLANK(A97)<1,ROW()-2,-1)_fPAR_", fPAR
write (1,*)" Parameter =IF(COUNTBLANK(A98)<1,ROW()-2,-1)_cLAlloVeg_", cLAlloV
write (1,*)" Parameter =IF(COUNTBLANK(A99)<1,ROW()-2,-1)_cVegInitLen_", cVIniLen
write (1,*)" Parameter =IF(COUNTBLANK(A100)<1,ROW()-2,-1)_cVegWinLen_", cVWinLen
write (1,*)" Parameter =IF(COUNTBLANK(A101)<1,ROW()-2,-1)_cExtSpDet_", cExtSpDe
write (1,*)" Parameter =IF(COUNTBLANK(A102)<1,ROW()-2,-1)_cExtSpIM_", cExtSpIM
write (1,*)" Parameter =IF(COUNTBLANK(A103)<1,ROW()-2,-1)_fDTotS0_", fDTotS0
write (1,*)" Parameter =IF(COUNTBLANK(A104)<1,ROW()-2,-1)_fDOrgS0_", fDOrgS0
write (1,*)" Parameter =IF(COUNTBLANK(A105)<1,ROW()-2,-1)_fDDetS0_", fDDeS0
write (1,*)" Parameter =IF(COUNTBLANK(A106)<1,ROW()-2,-1)_fSedPhyt0_", fSedP0
write (1,*)" Parameter =IF(COUNTBLANK(A107)<1,ROW()-2,-1)_fPInorgS0_", fPInogS0
write (1,*)" Parameter =IF(COUNTBLANK(A108)<1,ROW()-2,-1)_fPAdsS0_", fPAdsS0
write (1,*)" Parameter =IF(COUNTBLANK(A109)<1,ROW()-2,-1)_cPDDet0_", cPDDe0
write (1,*)" Parameter =IF(COUNTBLANK(A110)<1,ROW()-2,-1)_cNDDet0_", cNDDe0
write (1,*)" Parameter =IF(COUNTBLANK(A111)<1,ROW()-2,-1)_cSiDDet0_", cSiDDe0
write (1,*)" Parameter =IF(COUNTBLANK(A112)<1,ROW()-2,-1)_cPDHum0_", cPDHum0
write (1,*)" Parameter =IF(COUNTBLANK(A113)<1,ROW()-2,-1)_cNDHum0_", cNDHum0
write (1,*)" Parameter =IF(COUNTBLANK(A114)<1,ROW()-2,-1)_cPDPhyt0_", cPDP0
write (1,*)" Parameter =IF(COUNTBLANK(A115)<1,ROW()-2,-1)_cNDPhyt0_", cNDP0
write (1,*)" Parameter =IF(COUNTBLANK(A116)<1,ROW()-2,-1)_cPDDiat0_", cPDDi0
write (1,*)" Parameter =IF(COUNTBLANK(A117)<1,ROW()-2,-1)_cNDDiat0_", cNDDi0
write (1,*)" Parameter =IF(COUNTBLANK(A118)<1,ROW()-2,-1)_cPDGren0_", cPDGren0
write (1,*)" Parameter =IF(COUNTBLANK(A119)<1,ROW()-2,-1)_cNDGren0_", cNDGren0
write (1,*)" Parameter =IF(COUNTBLANK(A120)<1,ROW()-2,-1)_cPDBlue0_", cPDBlue0
write (1,*)" Parameter =IF(COUNTBLANK(A121)<1,ROW()-2,-1)_cNDBlue0_", cNDBlue0
write (1,*)" Parameter =IF(COUNTBLANK(A122)<1,ROW()-2,-1)_cPDVeg0_", cPDV0
write (1,*)" Parameter =IF(COUNTBLANK(A123)<1,ROW()-2,-1)_cNDVeg0_", cNDV0
write (1,*)" Parameter =IF(COUNTBLANK(A124)<1,ROW()-2,-1)_cSiDDiat_", cSiDDi
write (1,*)" Parameter =IF(COUNTBLANK(A125)<1,ROW()-2,-1)_cPDZooRef_", cPDZoRef
write (1,*)" Parameter =IF(COUNTBLANK(A126)<1,ROW()-2,-1)_cNDZooRef_", cNDZoRef
write (1,*)" Parameter =IF(COUNTBLANK(A127)<1,ROW()-2,-1)_cPDBentRef_", cPDenRef
write (1,*)" Parameter =IF(COUNTBLANK(A128)<1,ROW()-2,-1)_cNDBentRef_", cNDenRef
write (1,*)" Parameter =IF(COUNTBLANK(A129)<1,ROW()-2,-1)_cPDFishRef_", cPDisRef
write (1,*)" Parameter =IF(COUNTBLANK(A130)<1,ROW()-2,-1)_cNDFishRef_", cNDisRef
write (1,*)" Parameter =IF(COUNTBLANK(A131)<1,ROW()-2,-1)_cPDPisc_", cPDPisc
write (1,*)" Parameter =IF(COUNTBLANK(A132)<1,ROW()-2,-1)_cNDPisc_", cNDPisc
write (1,*)" Parameter =IF(COUNTBLANK(A133)<1,ROW()-2,-1)_cQIn_", cQIn
write (1,*)" Parameter =IF(COUNTBLANK(A134)<1,ROW()-2,-1)_cQInSum_", cQInSum
write (1,*)" Parameter =IF(COUNTBLANK(A135)<1,ROW()-2,-1)_cQInWin_", cQInWin
write (1,*)" Parameter =IF(COUNTBLANK(A136)<1,ROW()-2,-1)_cMaxWOut_", cMxWOut
write (1,*)" Parameter =IF(COUNTBLANK(A137)<1,ROW()-2,-1)_cDepthWMax_", cDeptWMx
write (1,*)" Parameter =IF(COUNTBLANK(A138)<1,ROW()-2,-1)_cQInExtraApril1_", cQIExpr1
write (1,*)" Parameter =IF(COUNTBLANK(A139)<1,ROW()-2,-1)_cQInExtraOct1_", cQIExct1
write (1,*)" Parameter =IF(COUNTBLANK(A140)<1,ROW()-2,-1)_cQOutExtraApril1_", cQOtEpr1
write (1,*)" Parameter =IF(COUNTBLANK(A141)<1,ROW()-2,-1)_cQOutExtraOct1_", cQOtEct1
write (1,*)" Parameter =IF(COUNTBLANK(A142)<1,ROW()-2,-1)_cQEvAve_", cQEvAve
write (1,*)" Parameter =IF(COUNTBLANK(A143)<1,ROW()-2,-1)_cQEvVar_", cQEvVar
write (1,*)" Parameter =IF(COUNTBLANK(A144)<1,ROW()-2,-1)_cPLoad_", cPLoad
write (1,*)" Parameter =IF(COUNTBLANK(A145)<1,ROW()-2,-1)_cPLoadSum_", cPLoaSum
write (1,*)" Parameter =IF(COUNTBLANK(A146)<1,ROW()-2,-1)_cPLoadWin_", cPLoaWin
write (1,*)" Parameter =IF(COUNTBLANK(A147)<1,ROW()-2,-1)_fPO4In_", fPO4In
write (1,*)" Parameter =IF(COUNTBLANK(A148)<1,ROW()-2,-1)_fPhytInWin_", fPInWin
write (1,*)" Parameter =IF(COUNTBLANK(A149)<1,ROW()-2,-1)_fPhytInSum_", fPInSum
write (1,*)" Parameter =IF(COUNTBLANK(A150)<1,ROW()-2,-1)_fDiatPhytIn_", fDiPIn
write (1,*)" Parameter =IF(COUNTBLANK(A151)<1,ROW()-2,-1)_fGrenPhytIn_", fGrenPIn
write (1,*)" Parameter =IF(COUNTBLANK(A152)<1,ROW()-2,-1)_fBluePhytIn_", fBluEn
write (1,*)" Parameter =IF(COUNTBLANK(A153)<1,ROW()-2,-1)_cNLoad_", cNLoad
write (1,*)" Parameter =IF(COUNTBLANK(A154)<1,ROW()-2,-1)_cNLoadSum_", cNLoaSum
write (1,*)" Parameter =IF(COUNTBLANK(A155)<1,ROW()-2,-1)_cNLoadWin_", cNLoaWin
write (1,*)" Parameter =IF(COUNTBLANK(A156)<1,ROW()-2,-1)_cNPLoadMeas_", cNPoaeas
write (1,*)" Parameter =IF(COUNTBLANK(A157)<1,ROW()-2,-1)_cNPPhytIn_", cNPPIn
write (1,*)" Parameter =IF(COUNTBLANK(A158)<1,ROW()-2,-1)_cNPDetIn_", cNPDeIn
write (1,*)" Parameter =IF(COUNTBLANK(A159)<1,ROW()-2,-1)_fNH4DissIn_", fNHDisIn
write (1,*)" Parameter =IF(COUNTBLANK(A160)<1,ROW()-2,-1)_cNDPhytIn_", cNDPIn
write (1,*)" Parameter =IF(COUNTBLANK(A161)<1,ROW()-2,-1)_cNDDetIn_", cNDDeIn
write (1,*)" Parameter =IF(COUNTBLANK(A162)<1,ROW()-2,-1)_cDIMIn_", cDIMn
write (1,*)" Parameter =IF(COUNTBLANK(A163)<1,ROW()-2,-1)_cO2In_", cO2In
write (1,*)" Parameter =IF(COUNTBLANK(A164)<1,ROW()-2,-1)_cSiO2In_", cSiO2In
write (1,*)" Parameter =IF(COUNTBLANK(A165)<1,ROW()-2,-1)_cSiDDetIn_", cSiDDeIn
write (1,*)" Parameter =IF(COUNTBLANK(A166)<1,ROW()-2,-1)_cDZooIn_", cDZooIn
write (1,*)" Parameter =IF(COUNTBLANK(A167)<1,ROW()-2,-1)_cDayApril1_", cDayApr1
write (1,*)" Parameter =IF(COUNTBLANK(A168)<1,ROW()-2,-1)_cDayOct1_", cDayOct1
write (1,*)" Parameter =IF(COUNTBLANK(A169)<1,ROW()-2,-1)_cLengChange_", cLegCnge
write (1,*)" Parameter =IF(COUNTBLANK(A170)<1,ROW()-2,-1)_cNLoadS_", cNLoadS
write (1,*)" Parameter =IF(COUNTBLANK(A171)<1,ROW()-2,-1)_fNH4LoadS_", fNH4LadS
write (1,*)" Parameter =IF(COUNTBLANK(A172)<1,ROW()-2,-1)_cDErosTot_", cDEroTot
write (1,*)" Parameter =IF(COUNTBLANK(A173)<1,ROW()-2,-1)_fSedErosIM_", fSeErsIM
write (1,*)" Parameter =IF(COUNTBLANK(A174)<1,ROW()-2,-1)_fHypErosIM_", fHErosIM
write (1,*)" Parameter =IF(COUNTBLANK(A175)<1,ROW()-2,-1)_fDOrgSoil_", fDOrgoil
write (1,*)" Parameter =IF(COUNTBLANK(A176)<1,ROW()-2,-1)_cPDSoilOM_", cPDSolOM
write (1,*)" Parameter =IF(COUNTBLANK(A177)<1,ROW()-2,-1)_cNDSoilOM_", cNDSolOM
write (1,*)" Parameter =IF(COUNTBLANK(A178)<1,ROW()-2,-1)_cPO4Ground_", cPO4Gr
write (1,*)" Parameter =IF(COUNTBLANK(A179)<1,ROW()-2,-1)_cNH4Ground_", cNH4Gr
write (1,*)" Parameter =IF(COUNTBLANK(A180)<1,ROW()-2,-1)_cNO3Ground_", cNO3Gr
write (1,*)" Parameter =IF(COUNTBLANK(A181)<1,ROW()-2,-1)_cDepthS_", cDepthS
write (1,*)" Parameter =IF(COUNTBLANK(A182)<1,ROW()-2,-1)_cCPerDW_", cCPerDW
write (1,*)" Parameter =IF(COUNTBLANK(A183)<1,ROW()-2,-1)_cRhoIM_", cRhoIM
write (1,*)" Parameter =IF(COUNTBLANK(A184)<1,ROW()-2,-1)_cRhoOM_", cRhoOM
write (1,*)" Parameter =IF(COUNTBLANK(A185)<1,ROW()-2,-1)_cTmRef_", cTmRef
write (1,*)" Parameter =IF(COUNTBLANK(A186)<1,ROW()-2,-1)_cAerRoot_", cAerR
write (1,*)" Parameter =IF(COUNTBLANK(A187)<1,ROW()-2,-1)_cAerLin_", cAerLin
write (1,*)" Parameter =IF(COUNTBLANK(A188)<1,ROW()-2,-1)_cAerSquare_", cAeSqare
write (1,*)" Parameter =IF(COUNTBLANK(A189)<1,ROW()-2,-1)_cThetaAer_", cThetAer
write (1,*)" Parameter =IF(COUNTBLANK(A190)<1,ROW()-2,-1)_kFloatAer_", kFloAer
write (1,*)" Parameter =IF(COUNTBLANK(A191)<1,ROW()-2,-1)_cVSetIM_", cVSetIM
write (1,*)" Parameter =IF(COUNTBLANK(A192)<1,ROW()-2,-1)_cVSetDet_", cVSetDe
write (1,*)" Parameter =IF(COUNTBLANK(A193)<1,ROW()-2,-1)_cThetaSet_", cThetSet
write (1,*)" Parameter =IF(COUNTBLANK(A194)<1,ROW()-2,-1)_cSuspMin_", cSuspMn
write (1,*)" Parameter =IF(COUNTBLANK(A195)<1,ROW()-2,-1)_cSuspMax_", cSuspMx
write (1,*)" Parameter =IF(COUNTBLANK(A196)<1,ROW()-2,-1)_cSuspSlope_", cSupSope
write (1,*)" Parameter =IF(COUNTBLANK(A197)<1,ROW()-2,-1)_hDepthSusp_", hDethusp
write (1,*)" Parameter =IF(COUNTBLANK(A198)<1,ROW()-2,-1)_cFetchRef_", cFetcRef
write (1,*)" Parameter =IF(COUNTBLANK(A199)<1,ROW()-2,-1)_fLutumRef_", fLutuRef
write (1,*)" Parameter =IF(COUNTBLANK(A200)<1,ROW()-2,-1)_cSuspRef_", cSuspRef
write (1,*)" Parameter =IF(COUNTBLANK(A201)<1,ROW()-2,-1)_kVegResus_", kVResus
write (1,*)" Parameter =IF(COUNTBLANK(A202)<1,ROW()-2,-1)_kTurbFish_", kTurbish
write (1,*)" Parameter =IF(COUNTBLANK(A203)<1,ROW()-2,-1)_kResusPhytMax_", kResuPMx
write (1,*)" Parameter =IF(COUNTBLANK(A204)<1,ROW()-2,-1)_cResusPhytExp_", cReusExp
write (1,*)" Parameter =IF(COUNTBLANK(A205)<1,ROW()-2,-1)_cThetaMinW_", cThetMnW
write (1,*)" Parameter =IF(COUNTBLANK(A206)<1,ROW()-2,-1)_kDMinDetW_", kDMnDeW
write (1,*)" Parameter =IF(COUNTBLANK(A207)<1,ROW()-2,-1)_hO2BOD_", hO2BOD
write (1,*)" Parameter =IF(COUNTBLANK(A208)<1,ROW()-2,-1)_O2PerNO3_", O2PerNO3
write (1,*)" Parameter =IF(COUNTBLANK(A209)<1,ROW()-2,-1)_cThetaMinS_", cThetMnS
write (1,*)" Parameter =IF(COUNTBLANK(A210)<1,ROW()-2,-1)_kDMinDetS_", kDMnDeS
write (1,*)" Parameter =IF(COUNTBLANK(A211)<1,ROW()-2,-1)_fRefrDetS_", fRefrDeS
write (1,*)" Parameter =IF(COUNTBLANK(A212)<1,ROW()-2,-1)_hNO3Denit_", hNO3Dnit
write (1,*)" Parameter =IF(COUNTBLANK(A213)<1,ROW()-2,-1)_NO3PerC_", NO3PerC
write (1,*)" Parameter =IF(COUNTBLANK(A214)<1,ROW()-2,-1)_kDMinHum_", kDMnHum
write (1,*)" Parameter =IF(COUNTBLANK(A215)<1,ROW()-2,-1)_kNitrW_", kNitrW
write (1,*)" Parameter =IF(COUNTBLANK(A216)<1,ROW()-2,-1)_kNitrS_", kNitrS
write (1,*)" Parameter =IF(COUNTBLANK(A217)<1,ROW()-2,-1)_cThetaNitr_", cThtaitr
write (1,*)" Parameter =IF(COUNTBLANK(A218)<1,ROW()-2,-1)_O2PerNH4_", O2PerNH4
write (1,*)" Parameter =IF(COUNTBLANK(A219)<1,ROW()-2,-1)_hO2Nitr_", hO2Nitr
write (1,*)" Parameter =IF(COUNTBLANK(A220)<1,ROW()-2,-1)_kPDifPO4_", kPDifPO4
write (1,*)" Parameter =IF(COUNTBLANK(A221)<1,ROW()-2,-1)_kNDifNO3_", kNDifNO3
write (1,*)" Parameter =IF(COUNTBLANK(A222)<1,ROW()-2,-1)_kNDifNH4_", kNDifNH4
write (1,*)" Parameter =IF(COUNTBLANK(A223)<1,ROW()-2,-1)_kO2Dif_", kO2Dif
write (1,*)" Parameter =IF(COUNTBLANK(A224)<1,ROW()-2,-1)_cThetaDif_", cThetDif
write (1,*)" Parameter =IF(COUNTBLANK(A225)<1,ROW()-2,-1)_fDepthDifS_", fDethifS
write (1,*)" Parameter =IF(COUNTBLANK(A226)<1,ROW()-2,-1)_cTurbDifNut_", cTubDNut
write (1,*)" Parameter =IF(COUNTBLANK(A227)<1,ROW()-2,-1)_cTurbDifO2_", cTubDfO2
write (1,*)" Parameter =IF(COUNTBLANK(A228)<1,ROW()-2,-1)_kPSorp_", kPSorp
write (1,*)" Parameter =IF(COUNTBLANK(A229)<1,ROW()-2,-1)_cRelPAdsD_", cRelPdsD
write (1,*)" Parameter =IF(COUNTBLANK(A230)<1,ROW()-2,-1)_cRelPAdsFe_", cRePAsFe
write (1,*)" Parameter =IF(COUNTBLANK(A231)<1,ROW()-2,-1)_cRelPAdsAl_", cRePAsAl
write (1,*)" Parameter =IF(COUNTBLANK(A232)<1,ROW()-2,-1)_cKPAdsOx_", cKPAdsOx
write (1,*)" Parameter =IF(COUNTBLANK(A233)<1,ROW()-2,-1)_fRedMax_", fRedMx
write (1,*)" Parameter =IF(COUNTBLANK(A234)<1,ROW()-2,-1)_coPO4Max_", coPO4Mx
write (1,*)" Parameter =IF(COUNTBLANK(A235)<1,ROW()-2,-1)_kPChemPO4_", kPChePO4
write (1,*)" Parameter =IF(COUNTBLANK(A236)<1,ROW()-2,-1)_cDayManVeg1_", cDayMnV1
write (1,*)" Parameter =IF(COUNTBLANK(A237)<1,ROW()-2,-1)_cDayManVeg2_", cDayMnV2
write (1,*)" Parameter =IF(COUNTBLANK(A238)<1,ROW()-2,-1)_fManVeg_", fManV
write (1,*)" Parameter =IF(COUNTBLANK(A239)<1,ROW()-2,-1)_cLengMan_", cLengMan
write (1,*)" Parameter =IF(COUNTBLANK(A240)<1,ROW()-2,-1)_cYearStartBirds_", cYetards
write (1,*)" Parameter =IF(COUNTBLANK(A241)<1,ROW()-2,-1)_cDayStartBirds_", cDaarrds
write (1,*)" Parameter =IF(COUNTBLANK(A242)<1,ROW()-2,-1)_cDayEndBirds_", cDandrds
write (1,*)" Parameter =IF(COUNTBLANK(A243)<1,ROW()-2,-1)_cBirdsPerha_", cBidsrha
write (1,*)" Parameter =IF(COUNTBLANK(A244)<1,ROW()-2,-1)_cDGrazPerBird_", cDGzPird
write (1,*)" Parameter =IF(COUNTBLANK(A245)<1,ROW()-2,-1)_hDVegBird_", hDVBird
write (1,*)" Parameter =IF(COUNTBLANK(A246)<1,ROW()-2,-1)_fDAssBird_", fDAssird
write (1,*)" Parameter =IF(COUNTBLANK(A247)<1,ROW()-2,-1)_fDissEgesBird_", fDiEgird
write (1,*)" Parameter =IF(COUNTBLANK(A248)<1,ROW()-2,-1)_fDissMortVeg_", fDissMV
write (1,*)" Parameter =IF(COUNTBLANK(A249)<1,ROW()-2,-1)_cLengAllo_", cLengllo
write (1,*)" Parameter =IF(COUNTBLANK(A250)<1,ROW()-2,-1)_cLengMort_", cLengM
write (1,*)" Parameter =IF(COUNTBLANK(A251)<1,ROW()-2,-1)_UseEmpUpt_", UseEmpU
write (1,*)" Parameter =IF(COUNTBLANK(A252)<1,ROW()-2,-1)_fSedUptVegMax_", fSedUVMx
write (1,*)" Parameter =IF(COUNTBLANK(A253)<1,ROW()-2,-1)_fSedUptVegCoef_", fSeUVoef
write (1,*)" Parameter =IF(COUNTBLANK(A254)<1,ROW()-2,-1)_fSedUptVegExp_", fSedUExp
write (1,*)" Parameter =IF(COUNTBLANK(A255)<1,ROW()-2,-1)_fRootVegSum_", fRVSum
write (1,*)" Parameter =IF(COUNTBLANK(A256)<1,ROW()-2,-1)_fRootVegWin_", fRVWin
write (1,*)" Parameter =IF(COUNTBLANK(A257)<1,ROW()-2,-1)_fFloatVeg_", fFloatV
write (1,*)" Parameter =IF(COUNTBLANK(A258)<1,ROW()-2,-1)_fEmergVeg_", fEmergV
write (1,*)" Parameter =IF(COUNTBLANK(A259)<1,ROW()-2,-1)_cVegHeight_", cVHeight
write (1,*)" Parameter =IF(COUNTBLANK(A260)<1,ROW()-2,-1)_cVegHeightWin_", cVHeW
write (1,*)" Parameter =IF(COUNTBLANK(A261)<1,ROW()-2,-1)_cVegMaxHeight_", cVMHe
write (1,*)" Parameter =IF(COUNTBLANK(A262)<1,ROW()-2,-1)_cDStemVeg_", cDStemV
write (1,*)" Parameter =IF(COUNTBLANK(A263)<1,ROW()-2,-1)_cDensVeg_", cDensV
write (1,*)" Parameter =IF(COUNTBLANK(A264)<1,ROW()-2,-1)_fDepth1Veg_", fDepth1V
write (1,*)" Parameter =IF(COUNTBLANK(A265)<1,ROW()-2,-1)_fDepth2Veg_", fDepth2V
write (1,*)" Parameter =IF(COUNTBLANK(A266)<1,ROW()-2,-1)_cDLayerVeg_", cDLayerV
write (1,*)" Parameter =IF(COUNTBLANK(A267)<1,ROW()-2,-1)_cCovSpVeg_", cCovSpV
write (1,*)" Parameter =IF(COUNTBLANK(A268)<1,ROW()-2,-1)_kMigrVeg_", kMigrV
write (1,*)" Parameter =IF(COUNTBLANK(A269)<1,ROW()-2,-1)_cDVegIn_", cDVIn
write (1,*)" Parameter =IF(COUNTBLANK(A270)<1,ROW()-2,-1)_cTmInitVeg_", cTMInitV
write (1,*)" Parameter =IF(COUNTBLANK(A271)<1,ROW()-2,-1)_cPhotInitVeg_", cPhInitV
write (1,*)" Parameter =IF(COUNTBLANK(A272)<1,ROW()-2,-1)_cDCarrVeg_", cDCarrV
write (1,*)" Parameter =IF(COUNTBLANK(A273)<1,ROW()-2,-1)_cMuMaxVeg_", cMuMxV
write (1,*)" Parameter =IF(COUNTBLANK(A274)<1,ROW()-2,-1)_cQ10ProdVeg_", cQ10PodV
write (1,*)" Parameter =IF(COUNTBLANK(A275)<1,ROW()-2,-1)_hLRefVeg_", hLRefV
write (1,*)" Parameter =IF(COUNTBLANK(A276)<1,ROW()-2,-1)_cExtSpVeg_", cExtSpV
write (1,*)" Parameter =IF(COUNTBLANK(A277)<1,ROW()-2,-1)_kDRespVeg_", kDRespV
write (1,*)" Parameter =IF(COUNTBLANK(A278)<1,ROW()-2,-1)_cQ10RespVeg_", cQ10RspV
write (1,*)" Parameter =IF(COUNTBLANK(A279)<1,ROW()-2,-1)_kMortVegSum_", kMVSum
write (1,*)" Parameter =IF(COUNTBLANK(A280)<1,ROW()-2,-1)_fWinVeg_", fWinV
write (1,*)" Parameter =IF(COUNTBLANK(A281)<1,ROW()-2,-1)_cDayWinVeg_", cDayWinV
write (1,*)" Parameter =IF(COUNTBLANK(A282)<1,ROW()-2,-1)_fDetWMortVeg_", fDeWMV
write (1,*)" Parameter =IF(COUNTBLANK(A283)<1,ROW()-2,-1)_cPrefVegBird_", cPrfVird
write (1,*)" Parameter =IF(COUNTBLANK(A284)<1,ROW()-2,-1)_cVPUptMaxVeg_", cVPUMxV
write (1,*)" Parameter =IF(COUNTBLANK(A285)<1,ROW()-2,-1)_cAffPUptVeg_", cAfPUV
write (1,*)" Parameter =IF(COUNTBLANK(A286)<1,ROW()-2,-1)_cPDVegMin_", cPDVMn
write (1,*)" Parameter =IF(COUNTBLANK(A287)<1,ROW()-2,-1)_cPDVegMax_", cPDVMx
write (1,*)" Parameter =IF(COUNTBLANK(A288)<1,ROW()-2,-1)_cVNUptMaxVeg_", cVNUMxV
write (1,*)" Parameter =IF(COUNTBLANK(A289)<1,ROW()-2,-1)_cAffNUptVeg_", cAfNUV
write (1,*)" Parameter =IF(COUNTBLANK(A290)<1,ROW()-2,-1)_cNDVegMin_", cNDVMn
write (1,*)" Parameter =IF(COUNTBLANK(A291)<1,ROW()-2,-1)_cNDVegMax_", cNDVMx
write (1,*)" Parameter =IF(COUNTBLANK(A292)<1,ROW()-2,-1)_cPACoefMin_", cPACofMn
write (1,*)" Parameter =IF(COUNTBLANK(A293)<1,ROW()-2,-1)_cPACoefMax_", cPACofMx
write (1,*)" Parameter =IF(COUNTBLANK(A294)<1,ROW()-2,-1)_hPACoef_", hPACoef
write (1,*)" Parameter =IF(COUNTBLANK(A295)<1,ROW()-2,-1)_cSecchiPlus_", cSechlus
write (1,*)" Parameter =IF(COUNTBLANK(A296)<1,ROW()-2,-1)_cEuph_", cEuph
write (1,*)" Parameter =IF(COUNTBLANK(A297)<1,ROW()-2,-1)_cCovSpPhyt_", cCovSpP
write (1,*)" Parameter =IF(COUNTBLANK(A298)<1,ROW()-2,-1)_cTmOptLoss_", cTmptoss
write (1,*)" Parameter =IF(COUNTBLANK(A299)<1,ROW()-2,-1)_cSigTmLoss_", cSiTmoss
write (1,*)" Parameter =IF(COUNTBLANK(A300)<1,ROW()-2,-1)_fDissMortPhyt_", fDissMP
write (1,*)" Parameter =IF(COUNTBLANK(A301)<1,ROW()-2,-1)_fDissLoss_", fDissoss
write (1,*)" Parameter =IF(COUNTBLANK(A302)<1,ROW()-2,-1)_cMuMaxDiat_", cMuMxDi
write (1,*)" Parameter =IF(COUNTBLANK(A303)<1,ROW()-2,-1)_cTmOptDiat_", cTmOptDi
write (1,*)" Parameter =IF(COUNTBLANK(A304)<1,ROW()-2,-1)_cSigTmDiat_", cSigTmDi
write (1,*)" Parameter =IF(COUNTBLANK(A305)<1,ROW()-2,-1)_cExtSpDiat_", cExtSpDi
write (1,*)" Parameter =IF(COUNTBLANK(A306)<1,ROW()-2,-1)_UseSteeleDiat_", UseteeDi
write (1,*)" Parameter =IF(COUNTBLANK(A307)<1,ROW()-2,-1)_cLOptRefDiat_", cLOtRfDi
write (1,*)" Parameter =IF(COUNTBLANK(A308)<1,ROW()-2,-1)_hLRefDiat_", hLRefDi
write (1,*)" Parameter =IF(COUNTBLANK(A309)<1,ROW()-2,-1)_cChDDiatMin_", cChDDiMn
write (1,*)" Parameter =IF(COUNTBLANK(A310)<1,ROW()-2,-1)_cChDDiatMax_", cChDDiMx
write (1,*)" Parameter =IF(COUNTBLANK(A311)<1,ROW()-2,-1)_kDRespDiat_", kDRespDi
write (1,*)" Parameter =IF(COUNTBLANK(A312)<1,ROW()-2,-1)_kLossDiat_", kLossDi
write (1,*)" Parameter =IF(COUNTBLANK(A313)<1,ROW()-2,-1)_kMortDiatW_", kMDiW
write (1,*)" Parameter =IF(COUNTBLANK(A314)<1,ROW()-2,-1)_kMortDiatS_", kMDiS
write (1,*)" Parameter =IF(COUNTBLANK(A315)<1,ROW()-2,-1)_cVSetDiat_", cVSetDi
write (1,*)" Parameter =IF(COUNTBLANK(A316)<1,ROW()-2,-1)_cVPUptMaxDiat_", cVPUMxDi
write (1,*)" Parameter =IF(COUNTBLANK(A317)<1,ROW()-2,-1)_cAffPUptDiat_", cAfPUDi
write (1,*)" Parameter =IF(COUNTBLANK(A318)<1,ROW()-2,-1)_cPDDiatMin_", cPDDiMn
write (1,*)" Parameter =IF(COUNTBLANK(A319)<1,ROW()-2,-1)_cPDDiatMax_", cPDDiMx
write (1,*)" Parameter =IF(COUNTBLANK(A320)<1,ROW()-2,-1)_cVNUptMaxDiat_", cVNUMxDi
write (1,*)" Parameter =IF(COUNTBLANK(A321)<1,ROW()-2,-1)_cAffNUptDiat_", cAfNUDi
write (1,*)" Parameter =IF(COUNTBLANK(A322)<1,ROW()-2,-1)_cNDDiatMin_", cNDDiMn
write (1,*)" Parameter =IF(COUNTBLANK(A323)<1,ROW()-2,-1)_cNDDiatMax_", cNDDiMx
write (1,*)" Parameter =IF(COUNTBLANK(A324)<1,ROW()-2,-1)_hSiAssDiat_", hSiAssDi
write (1,*)" Parameter =IF(COUNTBLANK(A325)<1,ROW()-2,-1)_cMuMaxGren_", cMuMxren
write (1,*)" Parameter =IF(COUNTBLANK(A326)<1,ROW()-2,-1)_cTmOptGren_", cTmptren
write (1,*)" Parameter =IF(COUNTBLANK(A327)<1,ROW()-2,-1)_cSigTmGren_", cSiTmren
write (1,*)" Parameter =IF(COUNTBLANK(A328)<1,ROW()-2,-1)_cExtSpGren_", cExSpren
write (1,*)" Parameter =IF(COUNTBLANK(A329)<1,ROW()-2,-1)_UseSteeleGren_", Useeeren
write (1,*)" Parameter =IF(COUNTBLANK(A330)<1,ROW()-2,-1)_hLRefGren_", hLRefren
write (1,*)" Parameter =IF(COUNTBLANK(A331)<1,ROW()-2,-1)_cLOptRefGren_", cLOReren
write (1,*)" Parameter =IF(COUNTBLANK(A332)<1,ROW()-2,-1)_cChDGrenMin_", cChGrnMn
write (1,*)" Parameter =IF(COUNTBLANK(A333)<1,ROW()-2,-1)_cChDGrenMax_", cChGrnMx
write (1,*)" Parameter =IF(COUNTBLANK(A334)<1,ROW()-2,-1)_kDRespGren_", kDRspren
write (1,*)" Parameter =IF(COUNTBLANK(A335)<1,ROW()-2,-1)_kLossGren_", kLossren
write (1,*)" Parameter =IF(COUNTBLANK(A336)<1,ROW()-2,-1)_kMortGrenW_", kMGrenW
write (1,*)" Parameter =IF(COUNTBLANK(A337)<1,ROW()-2,-1)_kMortGrenS_", kMGrenS
write (1,*)" Parameter =IF(COUNTBLANK(A338)<1,ROW()-2,-1)_cVSetGren_", cVSetren
write (1,*)" Parameter =IF(COUNTBLANK(A339)<1,ROW()-2,-1)_cVPUptMaxGren_", cVPMxren
write (1,*)" Parameter =IF(COUNTBLANK(A340)<1,ROW()-2,-1)_cAffPUptGren_", cAfPUren
write (1,*)" Parameter =IF(COUNTBLANK(A341)<1,ROW()-2,-1)_cPDGrenMin_", cPDGrnMn
write (1,*)" Parameter =IF(COUNTBLANK(A342)<1,ROW()-2,-1)_cPDGrenMax_", cPDGrnMx
write (1,*)" Parameter =IF(COUNTBLANK(A343)<1,ROW()-2,-1)_cVNUptMaxGren_", cVNMxren
write (1,*)" Parameter =IF(COUNTBLANK(A344)<1,ROW()-2,-1)_cAffNUptGren_", cAfNUren
write (1,*)" Parameter =IF(COUNTBLANK(A345)<1,ROW()-2,-1)_cNDGrenMin_", cNDGrnMn
write (1,*)" Parameter =IF(COUNTBLANK(A346)<1,ROW()-2,-1)_cNDGrenMax_", cNDGrnMx
write (1,*)" Parameter =IF(COUNTBLANK(A347)<1,ROW()-2,-1)_hSiAssGren_", hSissren
write (1,*)" Parameter =IF(COUNTBLANK(A348)<1,ROW()-2,-1)_cMuMaxBlue_", cMuMxlue
write (1,*)" Parameter =IF(COUNTBLANK(A349)<1,ROW()-2,-1)_cTmOptBlue_", cTmptlue
write (1,*)" Parameter =IF(COUNTBLANK(A350)<1,ROW()-2,-1)_cSigTmBlue_", cSiTmlue
write (1,*)" Parameter =IF(COUNTBLANK(A351)<1,ROW()-2,-1)_cExtSpBlue_", cExSplue
write (1,*)" Parameter =IF(COUNTBLANK(A352)<1,ROW()-2,-1)_UseSteeleBlue_", Useeelue
write (1,*)" Parameter =IF(COUNTBLANK(A353)<1,ROW()-2,-1)_cLOptRefBlue_", cLORelue
write (1,*)" Parameter =IF(COUNTBLANK(A354)<1,ROW()-2,-1)_hLRefBlue_", hLReflue
write (1,*)" Parameter =IF(COUNTBLANK(A355)<1,ROW()-2,-1)_cChDBlueMin_", cChBleMn
write (1,*)" Parameter =IF(COUNTBLANK(A356)<1,ROW()-2,-1)_cChDBlueMax_", cChBleMx
write (1,*)" Parameter =IF(COUNTBLANK(A357)<1,ROW()-2,-1)_cCyDBlueMin_", cCyBleMn
write (1,*)" Parameter =IF(COUNTBLANK(A358)<1,ROW()-2,-1)_cCyDBlueMax_", cCyBleMx
write (1,*)" Parameter =IF(COUNTBLANK(A359)<1,ROW()-2,-1)_kDRespBlue_", kDRsplue
write (1,*)" Parameter =IF(COUNTBLANK(A360)<1,ROW()-2,-1)_kLossBlue_", kLosslue
write (1,*)" Parameter =IF(COUNTBLANK(A361)<1,ROW()-2,-1)_kMortBlueW_", kMBlueW
write (1,*)" Parameter =IF(COUNTBLANK(A362)<1,ROW()-2,-1)_kMortBlueS_", kMBlueS
write (1,*)" Parameter =IF(COUNTBLANK(A363)<1,ROW()-2,-1)_cVSetBlue_", cVSetlue
write (1,*)" Parameter =IF(COUNTBLANK(A364)<1,ROW()-2,-1)_cVPUptMaxBlue_", cVPMxlue
write (1,*)" Parameter =IF(COUNTBLANK(A365)<1,ROW()-2,-1)_cAffPUptBlue_", cAfPUlue
write (1,*)" Parameter =IF(COUNTBLANK(A366)<1,ROW()-2,-1)_cPDBlueMin_", cPDBleMn
write (1,*)" Parameter =IF(COUNTBLANK(A367)<1,ROW()-2,-1)_cPDBlueMax_", cPDBleMx
write (1,*)" Parameter =IF(COUNTBLANK(A368)<1,ROW()-2,-1)_cVNUptMaxBlue_", cVNMxlue
write (1,*)" Parameter =IF(COUNTBLANK(A369)<1,ROW()-2,-1)_cAffNUptBlue_", cAfNUlue
write (1,*)" Parameter =IF(COUNTBLANK(A370)<1,ROW()-2,-1)_cNDBlueMin_", cNDBleMn
write (1,*)" Parameter =IF(COUNTBLANK(A371)<1,ROW()-2,-1)_cNDBlueMax_", cNDBleMx
write (1,*)" Parameter =IF(COUNTBLANK(A372)<1,ROW()-2,-1)_fFiJvMortHyp_", fFiJvMH
write (1,*)" Parameter =IF(COUNTBLANK(A373)<1,ROW()-2,-1)_fFiJvEgesHyp_", fFiJVesH
write (1,*)" Parameter =IF(COUNTBLANK(A374)<1,ROW()-2,-1)_fFiJvExcrHyp_", fFivEcrH
write (1,*)" Parameter =IF(COUNTBLANK(A375)<1,ROW()-2,-1)_fFiAdMortHyp_", fFiAdMH
write (1,*)" Parameter =IF(COUNTBLANK(A376)<1,ROW()-2,-1)_fFiAdEgesHyp_", fFidEesH
write (1,*)" Parameter =IF(COUNTBLANK(A377)<1,ROW()-2,-1)_fFiAdExcrHyp_", fFidEcrH
write (1,*)" Parameter =IF(COUNTBLANK(A378)<1,ROW()-2,-1)_fPiscMortHyp_", fPiscMH
write (1,*)" Parameter =IF(COUNTBLANK(A379)<1,ROW()-2,-1)_fPiscEgesHyp_", fPicEesH
write (1,*)" Parameter =IF(COUNTBLANK(A380)<1,ROW()-2,-1)_fPiscExcrHyp_", fPicEcrH
write (1,*)" Parameter =IF(COUNTBLANK(A381)<1,ROW()-2,-1)_hSiAssBlue_", hSisslue
write (1,*)" Parameter =IF(COUNTBLANK(A382)<1,ROW()-2,-1)_cDBentIn_", cDBentIn
write (1,*)" Parameter =IF(COUNTBLANK(A383)<1,ROW()-2,-1)_kMigrBent_", kMigrent
write (1,*)" Parameter =IF(COUNTBLANK(A384)<1,ROW()-2,-1)_kMigrFish_", kMigrish
write (1,*)" Parameter =IF(COUNTBLANK(A385)<1,ROW()-2,-1)_cDFiJvIn_", cDFiJvIn
write (1,*)" Parameter =IF(COUNTBLANK(A386)<1,ROW()-2,-1)_cDFiAdIn_", cDFiAdIn
write (1,*)" Parameter =IF(COUNTBLANK(A387)<1,ROW()-2,-1)_kHarvFishWin_", kHaFiWin
write (1,*)" Parameter =IF(COUNTBLANK(A388)<1,ROW()-2,-1)_kHarvFishSum_", kHaFiSum
write (1,*)" Parameter =IF(COUNTBLANK(A389)<1,ROW()-2,-1)_cDPiscIn_", cDPiscIn
write (1,*)" Parameter =IF(COUNTBLANK(A390)<1,ROW()-2,-1)_kMigrPisc_", kMigrisc
write (1,*)" Parameter =IF(COUNTBLANK(A391)<1,ROW()-2,-1)_kHarvPiscWin_", kHaPiWin
write (1,*)" Parameter =IF(COUNTBLANK(A392)<1,ROW()-2,-1)_kHarvPiscSum_", kHaPiSum
write (1,*)" Parameter =IF(COUNTBLANK(A393)<1,ROW()-2,-1)_cFiltMax_", cFiltMx
write (1,*)" Parameter =IF(COUNTBLANK(A394)<1,ROW()-2,-1)_hFilt_", hFilt
write (1,*)" Parameter =IF(COUNTBLANK(A395)<1,ROW()-2,-1)_cDCarrZoo_", cDCarZoo
write (1,*)" Parameter =IF(COUNTBLANK(A396)<1,ROW()-2,-1)_cPrefDiat_", cPrefDi
write (1,*)" Parameter =IF(COUNTBLANK(A397)<1,ROW()-2,-1)_cPrefGren_", cPrefren
write (1,*)" Parameter =IF(COUNTBLANK(A398)<1,ROW()-2,-1)_cPrefBlue_", cPreflue
write (1,*)" Parameter =IF(COUNTBLANK(A399)<1,ROW()-2,-1)_cPrefDet_", cPrefDe
write (1,*)" Parameter =IF(COUNTBLANK(A400)<1,ROW()-2,-1)_fDAssZoo_", fDAssZoo
write (1,*)" Parameter =IF(COUNTBLANK(A401)<1,ROW()-2,-1)_fDissEgesZoo_", fDiEgZoo
write (1,*)" Parameter =IF(COUNTBLANK(A402)<1,ROW()-2,-1)_kDRespZoo_", kDResZoo
write (1,*)" Parameter =IF(COUNTBLANK(A403)<1,ROW()-2,-1)_kMortZoo_", kMZoo
write (1,*)" Parameter =IF(COUNTBLANK(A404)<1,ROW()-2,-1)_fDissMortZoo_", fDissZoo
write (1,*)" Parameter =IF(COUNTBLANK(A405)<1,ROW()-2,-1)_cTmOptZoo_", cTmOpZoo
write (1,*)" Parameter =IF(COUNTBLANK(A406)<1,ROW()-2,-1)_cSigTmZoo_", cSigTZoo
write (1,*)" Parameter =IF(COUNTBLANK(A407)<1,ROW()-2,-1)_cDCarrBent_", cDCrrent
write (1,*)" Parameter =IF(COUNTBLANK(A408)<1,ROW()-2,-1)_kDAssBent_", kDAssent
write (1,*)" Parameter =IF(COUNTBLANK(A409)<1,ROW()-2,-1)_hDFoodBent_", hDFodent
write (1,*)" Parameter =IF(COUNTBLANK(A410)<1,ROW()-2,-1)_fDAssBent_", fDAssent
write (1,*)" Parameter =IF(COUNTBLANK(A411)<1,ROW()-2,-1)_fDissEgesBent_", fDiEgent
write (1,*)" Parameter =IF(COUNTBLANK(A412)<1,ROW()-2,-1)_kDRespBent_", kDRspent
write (1,*)" Parameter =IF(COUNTBLANK(A413)<1,ROW()-2,-1)_kMortBent_", kMBent
write (1,*)" Parameter =IF(COUNTBLANK(A414)<1,ROW()-2,-1)_fDissMortBent_", fDisMent
write (1,*)" Parameter =IF(COUNTBLANK(A415)<1,ROW()-2,-1)_cTmOptBent_", cTmptent
write (1,*)" Parameter =IF(COUNTBLANK(A416)<1,ROW()-2,-1)_cSigTmBent_", cSiTment
write (1,*)" Parameter =IF(COUNTBLANK(A417)<1,ROW()-2,-1)_fDBone_", fDBone
write (1,*)" Parameter =IF(COUNTBLANK(A418)<1,ROW()-2,-1)_fPBone_", fPBone
write (1,*)" Parameter =IF(COUNTBLANK(A419)<1,ROW()-2,-1)_cDCarrFish_", cDCrrish
write (1,*)" Parameter =IF(COUNTBLANK(A420)<1,ROW()-2,-1)_fDissEgesFish_", fDiEgish
write (1,*)" Parameter =IF(COUNTBLANK(A421)<1,ROW()-2,-1)_fDissMortFish_", fDisMish
write (1,*)" Parameter =IF(COUNTBLANK(A422)<1,ROW()-2,-1)_cTmOptFish_", cTmptish
write (1,*)" Parameter =IF(COUNTBLANK(A423)<1,ROW()-2,-1)_cSigTmFish_", cSiTmish
write (1,*)" Parameter =IF(COUNTBLANK(A424)<1,ROW()-2,-1)_cDayReprFish_", cDaepish
write (1,*)" Parameter =IF(COUNTBLANK(A425)<1,ROW()-2,-1)_fReprFish_", fReprish
write (1,*)" Parameter =IF(COUNTBLANK(A426)<1,ROW()-2,-1)_fAgeFish_", fAgeFish
write (1,*)" Parameter =IF(COUNTBLANK(A427)<1,ROW()-2,-1)_cRelVegFish_", cRelVish
write (1,*)" Parameter =IF(COUNTBLANK(A428)<1,ROW()-2,-1)_kDAssFiJv_", kDAssiJv
write (1,*)" Parameter =IF(COUNTBLANK(A429)<1,ROW()-2,-1)_hDZooFiJv_", hDZooiJv
write (1,*)" Parameter =IF(COUNTBLANK(A430)<1,ROW()-2,-1)_fDAssFiJv_", fDAssiJv
write (1,*)" Parameter =IF(COUNTBLANK(A431)<1,ROW()-2,-1)_kDRespFiJv_", kDRspiJv
write (1,*)" Parameter =IF(COUNTBLANK(A432)<1,ROW()-2,-1)_kMortFiJv_", kMFiJv
write (1,*)" Parameter =IF(COUNTBLANK(A433)<1,ROW()-2,-1)_kDAssFiAd_", kDAssiAd
write (1,*)" Parameter =IF(COUNTBLANK(A434)<1,ROW()-2,-1)_hDBentFiAd_", hDBntiAd
write (1,*)" Parameter =IF(COUNTBLANK(A435)<1,ROW()-2,-1)_fDAssFiAd_", fDAssiAd
write (1,*)" Parameter =IF(COUNTBLANK(A436)<1,ROW()-2,-1)_kDRespFiAd_", kDRspiAd
write (1,*)" Parameter =IF(COUNTBLANK(A437)<1,ROW()-2,-1)_kMortFiAd_", kMFiAd
write (1,*)" Parameter =IF(COUNTBLANK(A438)<1,ROW()-2,-1)_cDCarrPiscMax_", cDCrPcMx
write (1,*)" Parameter =IF(COUNTBLANK(A439)<1,ROW()-2,-1)_cDCarrPiscMin_", cDCrPcMn
write (1,*)" Parameter =IF(COUNTBLANK(A440)<1,ROW()-2,-1)_cDCarrPiscBare_", cDCPiare
write (1,*)" Parameter =IF(COUNTBLANK(A441)<1,ROW()-2,-1)_cDPhraMinPisc_", cDPaMisc
write (1,*)" Parameter =IF(COUNTBLANK(A442)<1,ROW()-2,-1)_cCovVegMin_", cCovVMn
write (1,*)" Parameter =IF(COUNTBLANK(A443)<1,ROW()-2,-1)_cTmOptVeg_", cTmOptV
write (1,*)" Parameter =IF(COUNTBLANK(A444)<1,ROW()-2,-1)_cSigTmVeg_", cSigTmV
write (1,*)" Parameter =IF(COUNTBLANK(A445)<1,ROW()-2,-1)_cRelPhraPisc_", cRehrisc
write (1,*)" Parameter =IF(COUNTBLANK(A446)<1,ROW()-2,-1)_cRelVegPisc_", cRelVisc
write (1,*)" Parameter =IF(COUNTBLANK(A447)<1,ROW()-2,-1)_kDAssPisc_", kDAssisc
write (1,*)" Parameter =IF(COUNTBLANK(A448)<1,ROW()-2,-1)_hDVegPisc_", hDVPisc
write (1,*)" Parameter =IF(COUNTBLANK(A449)<1,ROW()-2,-1)_hDFishPisc_", hDFshisc
write (1,*)" Parameter =IF(COUNTBLANK(A450)<1,ROW()-2,-1)_fDAssPisc_", fDAssisc
write (1,*)" Parameter =IF(COUNTBLANK(A451)<1,ROW()-2,-1)_fDissEgesPisc_", fDiEgisc
write (1,*)" Parameter =IF(COUNTBLANK(A452)<1,ROW()-2,-1)_kDRespPisc_", kDRspisc
write (1,*)" Parameter =IF(COUNTBLANK(A453)<1,ROW()-2,-1)_kMortPisc_", kMPisc
write (1,*)" Parameter =IF(COUNTBLANK(A454)<1,ROW()-2,-1)_fDissMortPisc_", fDisMisc
write (1,*)" Parameter =IF(COUNTBLANK(A455)<1,ROW()-2,-1)_cTmOptPisc_", cTmptisc
write (1,*)" Parameter =IF(COUNTBLANK(A456)<1,ROW()-2,-1)_cSigTmPisc_", cSiTmisc
write (1,*)" Parameter =IF(COUNTBLANK(A457)<1,ROW()-2,-1)_cDepthSM_", cDepthSM
write (1,*)" Parameter =IF(COUNTBLANK(A458)<1,ROW()-2,-1)_kExchMaxM_", kExchMxM
write (1,*)" Parameter =IF(COUNTBLANK(A459)<1,ROW()-2,-1)_hfMarsh_", hfMarsh
write (1,*)" Parameter =IF(COUNTBLANK(A460)<1,ROW()-2,-1)_fDTotSM0_", fDTotSM0
write (1,*)" Parameter =IF(COUNTBLANK(A461)<1,ROW()-2,-1)_fDOrgSM0_", fDOrgSM0
write (1,*)" Parameter =IF(COUNTBLANK(A462)<1,ROW()-2,-1)_fDDetSM0_", fDDeSM0
write (1,*)" Parameter =IF(COUNTBLANK(A463)<1,ROW()-2,-1)_fPInorgSM0_", fPIorSM0
write (1,*)" Parameter =IF(COUNTBLANK(A464)<1,ROW()-2,-1)_cPDPhra0_", cPDPhra0
write (1,*)" Parameter =IF(COUNTBLANK(A465)<1,ROW()-2,-1)_cNDPhra0_", cNDPhra0
write (1,*)" Parameter =IF(COUNTBLANK(A466)<1,ROW()-2,-1)_cDensStemPhra_", cDeSthra
write (1,*)" Parameter =IF(COUNTBLANK(A467)<1,ROW()-2,-1)_cTmInitPhra_", cTMithra
write (1,*)" Parameter =IF(COUNTBLANK(A468)<1,ROW()-2,-1)_fDAllPhra_", fDAllhra
write (1,*)" Parameter =IF(COUNTBLANK(A469)<1,ROW()-2,-1)_kDAllPhra_", kDAllhra
write (1,*)" Parameter =IF(COUNTBLANK(A470)<1,ROW()-2,-1)_cDStemPhra_", cDSemhra
write (1,*)" Parameter =IF(COUNTBLANK(A471)<1,ROW()-2,-1)_cQ10ProdPhra_", cQ1rohra
write (1,*)" Parameter =IF(COUNTBLANK(A472)<1,ROW()-2,-1)_cMuPhraMax_", cMuPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A473)<1,ROW()-2,-1)_cDShootPhraMax_", cDSPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A474)<1,ROW()-2,-1)_cCovSpPhra_", cCoSphra
write (1,*)" Parameter =IF(COUNTBLANK(A475)<1,ROW()-2,-1)_cPDPhraMin_", cPDPhaMn
write (1,*)" Parameter =IF(COUNTBLANK(A476)<1,ROW()-2,-1)_cPDPhraMax_", cPDPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A477)<1,ROW()-2,-1)_cNDPhraMin_", cNDPhaMn
write (1,*)" Parameter =IF(COUNTBLANK(A478)<1,ROW()-2,-1)_cNDPhraMax_", cNDPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A479)<1,ROW()-2,-1)_cAffNUptPhra_", cAfNUhra
write (1,*)" Parameter =IF(COUNTBLANK(A480)<1,ROW()-2,-1)_cAffPUptPhra_", cAfPUhra
write (1,*)" Parameter =IF(COUNTBLANK(A481)<1,ROW()-2,-1)_cVNUptPhraMax_", cVNPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A482)<1,ROW()-2,-1)_cVPUptPhraMax_", cVPPhaMx
write (1,*)" Parameter =IF(COUNTBLANK(A483)<1,ROW()-2,-1)_kDRespPhra_", kDRsphra
write (1,*)" Parameter =IF(COUNTBLANK(A484)<1,ROW()-2,-1)_cQ10RespPhra_", cQ1eshra
write (1,*)" Parameter =IF(COUNTBLANK(A485)<1,ROW()-2,-1)_fDayWin_", fDayWin
write (1,*)" Parameter =IF(COUNTBLANK(A486)<1,ROW()-2,-1)_fDRealPhra_", fDRalhra
write (1,*)" Parameter =IF(COUNTBLANK(A487)<1,ROW()-2,-1)_kDRealPhra_", kDRalhra
write (1,*)" Parameter =IF(COUNTBLANK(A488)<1,ROW()-2,-1)_kDMortShootPhra_", kDMShhra
write (1,*)" Parameter =IF(COUNTBLANK(A489)<1,ROW()-2,-1)_kDMortRootPhra_", kDMRPhra
write (1,*)" Parameter =IF(COUNTBLANK(A490)<1,ROW()-2,-1)_cDayWinPhra_", cDaWihra
write (1,*)" Parameter =IF(COUNTBLANK(A491)<1,ROW()-2,-1)_cDayManPhra_", cDaMahra
write (1,*)" Parameter =IF(COUNTBLANK(A492)<1,ROW()-2,-1)_fManPhra_", fManPhra
write (1,*)" Parameter =IF(COUNTBLANK(A493)<1,ROW()-2,-1)_kDManShootPhra_", kDMnShra
write (1,*)" Parameter =IF(COUNTBLANK(A494)<1,ROW()-2,-1)_DaysPerYear_", DayPeear
write (1,*)" Parameter =IF(COUNTBLANK(A495)<1,ROW()-2,-1)_TenDays_", TenDays
write (1,*)" Parameter =IF(COUNTBLANK(A496)<1,ROW()-2,-1)_HoursInDay_", HousIDay
write (1,*)" Parameter =IF(COUNTBLANK(A497)<1,ROW()-2,-1)_HoursPerDay_", HousPDay
write (1,*)" Parameter =IF(COUNTBLANK(A498)<1,ROW()-2,-1)_SecsPerDay_", SecPeDay
write (1,*)" Parameter =IF(COUNTBLANK(A499)<1,ROW()-2,-1)_mmPerm_", mmPerm
write (1,*)" Parameter =IF(COUNTBLANK(A500)<1,ROW()-2,-1)_m2Perha_", m2Perha
write (1,*)" Parameter =IF(COUNTBLANK(A501)<1,ROW()-2,-1)_mgPerg_", mgPerg
write (1,*)" Parameter =IF(COUNTBLANK(A502)<1,ROW()-2,-1)_gPerkg_", gPerkg
write (1,*)" Parameter =IF(COUNTBLANK(A503)<1,ROW()-2,-1)_gPerton_", gPerton
write (1,*)" Parameter =IF(COUNTBLANK(A504)<1,ROW()-2,-1)_PerDay_", PerDay
write (1,*)" Parameter =IF(COUNTBLANK(A505)<1,ROW()-2,-1)_PerCent_", PerCent
write (1,*)" Parameter =IF(COUNTBLANK(A506)<1,ROW()-2,-1)_NearZero_", NearZero
write (1,*)" Parameter =IF(COUNTBLANK(A507)<1,ROW()-2,-1)_molO2molC_", molO2olC
write (1,*)" Parameter =IF(COUNTBLANK(A508)<1,ROW()-2,-1)_molO2molN_", molO2olN
write (1,*)" Parameter =IF(COUNTBLANK(A509)<1,ROW()-2,-1)_molNmolC_", molNmolC
write (1,*)" Parameter =IF(COUNTBLANK(A510)<1,ROW()-2,-1)_cRhoWat_", cRhoWat
write (1,*)" Parameter =IF(COUNTBLANK(A511)<1,ROW()-2,-1)_cSolar_", cSolar
write (1,*)" Parameter =IF(COUNTBLANK(A512)<1,ROW()-2,-1)_cLAT_", cLAT
write (1,*)" Parameter =IF(COUNTBLANK(A513)<1,ROW()-2,-1)_cPhiR_", cPhiR
write (1,*)" Parameter =IF(COUNTBLANK(A514)<1,ROW()-2,-1)_cDayR_", cDayR
write (1,*)" Parameter =IF(COUNTBLANK(A515)<1,ROW()-2,-1)_fCloudH_", fCloudH
write (1,*)" Parameter =IF(COUNTBLANK(A516)<1,ROW()-2,-1)_fCloudM_", fCloudM
write (1,*)" Parameter =IF(COUNTBLANK(A517)<1,ROW()-2,-1)_fCloudL_", fCloudL
write (1,*)" Parameter =IF(COUNTBLANK(A518)<1,ROW()-2,-1)_cTrans_", cTrans
write (1,*)" Parameter =IF(COUNTBLANK(A519)<1,ROW()-2,-1)_Pi_", Pi
write (1,*)" Parameter =IF(COUNTBLANK(A520)<1,ROW()-2,-1)_cHeath_", cHeath
write (1,*)" Parameter =IF(COUNTBLANK(A521)<1,ROW()-2,-1)_cAlpha_", cAlpha
write (1,*)" Parameter =IF(COUNTBLANK(A522)<1,ROW()-2,-1)_cDepthMeasoChl_", cDeMeChl
write (1,*)" Parameter =IF(COUNTBLANK(A523)<1,ROW()-2,-1)_cPMax_", cPMx
write (1,*)" Parameter =IF(COUNTBLANK(A524)<1,ROW()-2,-1)_cDepthMetaMin_", cDehMaMn
write (1,*)" Parameter =IF(COUNTBLANK(A525)<1,ROW()-2,-1)_fMeta_", fMeta
write (1,*)" Parameter =IF(COUNTBLANK(A526)<1,ROW()-2,-1)_kBub_", kBub
write (1,*)" Parameter =IF(COUNTBLANK(A527)<1,ROW()-2,-1)_cVegHPar1_", cVegHP1
write (1,*)" Parameter =IF(COUNTBLANK(A528)<1,ROW()-2,-1)_cVegHPar2_", cVegHP2
write (1,*)" Parameter =IF(COUNTBLANK(A529)<1,ROW()-2,-1)_cVegL_", cVegL
write (1,*)" InitState  0 _cDepthW0_", sDepthW
write (1,*)" InitState  1 _cNH4WHyp0_", sNH4WH
write (1,*)" InitState  2 _cNO3WHyp0_", sNO3WH
write (1,*)" InitState  3 _cPO4WHyp0_", sPO4WH
write (1,*)" InitState  4 _cPAIMWHyp0_", sPAIMWH
write (1,*)" InitState  5 _cSiO2WHyp0_", sSiO2WH
write (1,*)" InitState  6 _cO2WHyp0_", sO2WH
write (1,*)" InitState  7 _cDDetWHyp0_", sDDetWH
write (1,*)" InitState  8 _cNDetWHyp0_", sNDetWH
write (1,*)" InitState  9 _cPDetWHyp0_", sPDetWH
write (1,*)" InitState  10 _cSiDetWHyp0_", sSiDetWH
write (1,*)" InitState  11 _cDIMWHyp0_", sDIMWH
write (1,*)" InitState  12 _cDDiatWHyp0_", sDDiatWH
write (1,*)" InitState  13 _cNDiatWHyp0_", sNDiatWH
write (1,*)" InitState  14 _cPDiatWHyp0_", sPDiatWH
write (1,*)" InitState  15 _cDGrenWHyp0_", sDGrenWH
write (1,*)" InitState  16 _cNGrenWHyp0_", sNGrenWH
write (1,*)" InitState  17 _cPGrenWHyp0_", sPGrenWH
write (1,*)" InitState  18 _cDBlueWHyp0_", sDBlueWH
write (1,*)" InitState  19 _cNBlueWHyp0_", sNBlueWH
write (1,*)" InitState  20 _cPBlueWHyp0_", sPBlueWH
write (1,*)" InitState  21 _cDZooHyp0_", sDZooH
write (1,*)" InitState  22 _cNZooHyp0_", sNZooH
write (1,*)" InitState  23 _cPZooHyp0_", sPZooH
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
write (1,*)" InitState  55 _cVegHeight0_", sVegHe
write (1,*)" InitState  56 _cDBent0_", sDBent
write (1,*)" InitState  57 _cNBent0_", sNBent
write (1,*)" InitState  58 _cPBent0_", sPBent
write (1,*)" InitState  59 _cDepthWM0_", sDepthWM
write (1,*)" InitState  60 _cNH4WM0_", sNH4WM
write (1,*)" InitState  61 _cNO3WM0_", sNO3WM
write (1,*)" InitState  62 _cPO4WM0_", sPO4WM
write (1,*)" InitState  63 _cPAIMWM0_", sPAIMWM
write (1,*)" InitState  64 _cSiO2WM0_", sSiO2WM
write (1,*)" InitState  65 _cO2WM0_", sO2WM
write (1,*)" InitState  66 _cDDetWM0_", sDDetWM
write (1,*)" InitState  67 _cNDetWM0_", sNDetWM
write (1,*)" InitState  68 _cPDetWM0_", sPDetWM
write (1,*)" InitState  69 _cSiDetWM0_", sSiDetWM
write (1,*)" InitState  70 _cDIMWM0_", sDIMWM
write (1,*)" InitState  71 _cDDiatWM0_", sDDiatWM
write (1,*)" InitState  72 _cNDiatWM0_", sNDiatWM
write (1,*)" InitState  73 _cPDiatWM0_", sPDiatWM
write (1,*)" InitState  74 _cDGrenWM0_", sDGrenWM
write (1,*)" InitState  75 _cNGrenWM0_", sNGrenWM
write (1,*)" InitState  76 _cPGrenWM0_", sPGrenWM
write (1,*)" InitState  77 _cDBlueWM0_", sDBlueWM
write (1,*)" InitState  78 _cNBlueWM0_", sNBlueWM
write (1,*)" InitState  79 _cPBlueWM0_", sPBlueWM
write (1,*)" InitState  80 _cDZooM0_", sDZooM
write (1,*)" InitState  81 _cNZooM0_", sNZooM
write (1,*)" InitState  82 _cPZooM0_", sPZooM
write (1,*)" InitState  83 _cNH4SM0_", sNH4SM
write (1,*)" InitState  84 _cNO3SM0_", sNO3SM
write (1,*)" InitState  85 _cPO4SM0_", sPO4SM
write (1,*)" InitState  86 _cPAIMSM0_", sPAIMSM
write (1,*)" InitState  87 _cDDetSM0_", sDDetSM
write (1,*)" InitState  88 _cNDetSM0_", sNDetSM
write (1,*)" InitState  89 _cPDetSM0_", sPDetSM
write (1,*)" InitState  90 _cSiDetSM0_", sSiDetSM
write (1,*)" InitState  91 _cDHumSM0_", sDHumSM
write (1,*)" InitState  92 _cNHumSM0_", sNHumSM
write (1,*)" InitState  93 _cPHumSM0_", sPHumSM
write (1,*)" InitState  94 _cDIMSM0_", sDIMSM
write (1,*)" InitState  95 _cDRootPhra0_", sDRoPhra
write (1,*)" InitState  96 _cDShootPhra0_", sDShPhra
write (1,*)" InitState  97 _cNRootPhra0_", sNRoPhra
write (1,*)" InitState  98 _cNShootPhra0_", sNShPhra
write (1,*)" InitState  99 _cPRootPhra0_", sPRoPhra
write (1,*)" InitState  100 _cPShootPhra0_", sPShPhra
write (1,*)" InitState  101 _cNH4WEpi0_", sNH4WE
write (1,*)" InitState  102 _cNO3WEpi0_", sNO3WE
write (1,*)" InitState  103 _cPO4WEpi0_", sPO4WE
write (1,*)" InitState  104 _cPAIMWEpi0_", sPAIMWE
write (1,*)" InitState  105 _cSiO2WEpi0_", sSiO2WE
write (1,*)" InitState  106 _cO2WEpi0_", sO2WE
write (1,*)" InitState  107 _cDDetWEpi0_", sDDetWE
write (1,*)" InitState  108 _cNDetWEpi0_", sNDetWE
write (1,*)" InitState  109 _cPDetWEpi0_", sPDetWE
write (1,*)" InitState  110 _cSiDetWEpi0_", sSiDetWE
write (1,*)" InitState  111 _cDIMWEpi0_", sDIMWE
write (1,*)" InitState  112 _cDDiatWEpi0_", sDDiatWE
write (1,*)" InitState  113 _cNDiatWEpi0_", sNDiatWE
write (1,*)" InitState  114 _cPDiatWEpi0_", sPDiatWE
write (1,*)" InitState  115 _cDGrenWEpi0_", sDGrenWE
write (1,*)" InitState  116 _cNGrenWEpi0_", sNGrenWE
write (1,*)" InitState  117 _cPGrenWEpi0_", sPGrenWE
write (1,*)" InitState  118 _cDBlueWEpi0_", sDBlueWE
write (1,*)" InitState  119 _cNBlueWEpi0_", sNBlueWE
write (1,*)" InitState  120 _cPBlueWEpi0_", sPBlueWE
write (1,*)" InitState  121 _cDZooEpi0_", sDZooE
write (1,*)" InitState  122 _cNZooEpi0_", sNZooE
write (1,*)" InitState  123 _cPZooEpi0_", sPZooE
write (1,*)" InitState  124 _cDExtTotT0_", sDExTotT
write (1,*)" InitState  125 _cNExtTotT0_", sNExTotT
write (1,*)" InitState  126 _cPExtTotT0_", sPExTotT
write (1,*)" InitState  127 _cSiExtTotT0_", sSiETotT
write (1,*)" InitState  128 _cO2ExtTotT0_", sO2ETotT
write (1,*)" InitState =IF(COUNTBLANK(A3)<1,ROW()-2,-1)_uDepthMix0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A4)<1,ROW()-2,-1)_aInclStrat0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A5)<1,ROW()-2,-1)_sDepthW0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A6)<1,ROW()-2,-1)_MassHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A7)<1,ROW()-2,-1)_sNH4WHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A8)<1,ROW()-2,-1)_sNO3WHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A9)<1,ROW()-2,-1)_sPO4WHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A10)<1,ROW()-2,-1)_sPAIMWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A11)<1,ROW()-2,-1)_sSiO2WHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A12)<1,ROW()-2,-1)_sO2WHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A13)<1,ROW()-2,-1)_sDDetWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A14)<1,ROW()-2,-1)_sNDetWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A15)<1,ROW()-2,-1)_sPDetWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A16)<1,ROW()-2,-1)_sSiDetWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A17)<1,ROW()-2,-1)_sDIMWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A18)<1,ROW()-2,-1)_sDDiatWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A19)<1,ROW()-2,-1)_sNDiatWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A20)<1,ROW()-2,-1)_sPDiatWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A21)<1,ROW()-2,-1)_sDGrenWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A22)<1,ROW()-2,-1)_sNGrenWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A23)<1,ROW()-2,-1)_sPGrenWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A24)<1,ROW()-2,-1)_sDBlueWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A25)<1,ROW()-2,-1)_sNBlueWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A26)<1,ROW()-2,-1)_sPBlueWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A27)<1,ROW()-2,-1)_sDZooHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A28)<1,ROW()-2,-1)_sNZooHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A29)<1,ROW()-2,-1)_sPZooHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A30)<1,ROW()-2,-1)_sDFiAd0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A31)<1,ROW()-2,-1)_sDFiJv0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A32)<1,ROW()-2,-1)_sNFiAd0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A33)<1,ROW()-2,-1)_sNFiJv0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A34)<1,ROW()-2,-1)_sPFiAd0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A35)<1,ROW()-2,-1)_sPFiJv0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A36)<1,ROW()-2,-1)_sDPisc0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A37)<1,ROW()-2,-1)_MassEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A38)<1,ROW()-2,-1)_sNH4WEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A39)<1,ROW()-2,-1)_sNO3WEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A40)<1,ROW()-2,-1)_sPO4WEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A41)<1,ROW()-2,-1)_sPAIMWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A42)<1,ROW()-2,-1)_sSiO2WEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A43)<1,ROW()-2,-1)_sO2WEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A44)<1,ROW()-2,-1)_sDDetWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A45)<1,ROW()-2,-1)_sNDetWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A46)<1,ROW()-2,-1)_sPDetWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A47)<1,ROW()-2,-1)_sSiDetWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A48)<1,ROW()-2,-1)_sDIMWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A49)<1,ROW()-2,-1)_sDDiatWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A50)<1,ROW()-2,-1)_sNDiatWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A51)<1,ROW()-2,-1)_sPDiatWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A52)<1,ROW()-2,-1)_sDGrenWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A53)<1,ROW()-2,-1)_sNGrenWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A54)<1,ROW()-2,-1)_sPGrenWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A55)<1,ROW()-2,-1)_sDBlueWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A56)<1,ROW()-2,-1)_sNBlueWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A57)<1,ROW()-2,-1)_sPBlueWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A58)<1,ROW()-2,-1)_sDZooEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A59)<1,ROW()-2,-1)_sNZooEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A60)<1,ROW()-2,-1)_sPZooEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A61)<1,ROW()-2,-1)_sNH4S0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A62)<1,ROW()-2,-1)_sNO3S0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A63)<1,ROW()-2,-1)_bRhoSolidS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A64)<1,ROW()-2,-1)_bPorS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A65)<1,ROW()-2,-1)_bRhoTotS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A66)<1,ROW()-2,-1)_bDTotS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A67)<1,ROW()-2,-1)_sPO4S0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A68)<1,ROW()-2,-1)_sPAIMS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A69)<1,ROW()-2,-1)_sDDetS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A70)<1,ROW()-2,-1)_sNDetS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A71)<1,ROW()-2,-1)_sPDetS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A72)<1,ROW()-2,-1)_sSiDetS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A73)<1,ROW()-2,-1)_sDHumS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A74)<1,ROW()-2,-1)_sNHumS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A75)<1,ROW()-2,-1)_sPHumS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A76)<1,ROW()-2,-1)_sDIMS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A77)<1,ROW()-2,-1)_sDDiatS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A78)<1,ROW()-2,-1)_sNDiatS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A79)<1,ROW()-2,-1)_sPDiatS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A80)<1,ROW()-2,-1)_sDGrenS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A81)<1,ROW()-2,-1)_sNGrenS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A82)<1,ROW()-2,-1)_sPGrenS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A83)<1,ROW()-2,-1)_sDBlueS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A84)<1,ROW()-2,-1)_sNBlueS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A85)<1,ROW()-2,-1)_sPBlueS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A86)<1,ROW()-2,-1)_sDVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A87)<1,ROW()-2,-1)_sNVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A88)<1,ROW()-2,-1)_sPVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A89)<1,ROW()-2,-1)_sDBent0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A90)<1,ROW()-2,-1)_sNBent0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A91)<1,ROW()-2,-1)_sPBent0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A92)<1,ROW()-2,-1)_sDepthWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A93)<1,ROW()-2,-1)_MassWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A94)<1,ROW()-2,-1)_sNH4WM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A95)<1,ROW()-2,-1)_sNO3WM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A96)<1,ROW()-2,-1)_sPO4WM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A97)<1,ROW()-2,-1)_sPAIMWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A98)<1,ROW()-2,-1)_sSiO2WM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A99)<1,ROW()-2,-1)_sO2WM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A100)<1,ROW()-2,-1)_sDDetWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A101)<1,ROW()-2,-1)_sNDetWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A102)<1,ROW()-2,-1)_sPDetWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A103)<1,ROW()-2,-1)_sSiDetWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A104)<1,ROW()-2,-1)_sDIMWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A105)<1,ROW()-2,-1)_sDDiatWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A106)<1,ROW()-2,-1)_sNDiatWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A107)<1,ROW()-2,-1)_sPDiatWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A108)<1,ROW()-2,-1)_sDGrenWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A109)<1,ROW()-2,-1)_sNGrenWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A110)<1,ROW()-2,-1)_sPGrenWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A111)<1,ROW()-2,-1)_sDBlueWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A112)<1,ROW()-2,-1)_sNBlueWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A113)<1,ROW()-2,-1)_sPBlueWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A114)<1,ROW()-2,-1)_sDZooM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A115)<1,ROW()-2,-1)_sNZooM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A116)<1,ROW()-2,-1)_sPZooM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A117)<1,ROW()-2,-1)_sNH4SM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A118)<1,ROW()-2,-1)_sNO3SM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A119)<1,ROW()-2,-1)_bRhoSolidSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A120)<1,ROW()-2,-1)_bPorSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A121)<1,ROW()-2,-1)_bRhoTotSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A122)<1,ROW()-2,-1)_bDTotSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A123)<1,ROW()-2,-1)_fPAdsSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A124)<1,ROW()-2,-1)_sPO4SM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A125)<1,ROW()-2,-1)_sPAIMSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A126)<1,ROW()-2,-1)_sDDetSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A127)<1,ROW()-2,-1)_sNDetSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A128)<1,ROW()-2,-1)_sPDetSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A129)<1,ROW()-2,-1)_sSiDetSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A130)<1,ROW()-2,-1)_sDHumSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A131)<1,ROW()-2,-1)_sNHumSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A132)<1,ROW()-2,-1)_sPHumSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A133)<1,ROW()-2,-1)_sDIMSM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A134)<1,ROW()-2,-1)_sDRootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A135)<1,ROW()-2,-1)_sDShootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A136)<1,ROW()-2,-1)_sNRootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A137)<1,ROW()-2,-1)_sNShootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A138)<1,ROW()-2,-1)_sPRootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A139)<1,ROW()-2,-1)_sPShootPhra0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A140)<1,ROW()-2,-1)_uDPhytWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A141)<1,ROW()-2,-1)_uDPhytWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A142)<1,ROW()-2,-1)_uPPhytWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A143)<1,ROW()-2,-1)_uPPhytWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A144)<1,ROW()-2,-1)_uNPhytWHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A145)<1,ROW()-2,-1)_uNPhytWEpi0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A146)<1,ROW()-2,-1)_uDPhytS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A147)<1,ROW()-2,-1)_uPPhytS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A148)<1,ROW()-2,-1)_uNPhytS0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A149)<1,ROW()-2,-1)_uDPhytWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A150)<1,ROW()-2,-1)_uPPhytWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A151)<1,ROW()-2,-1)_uNPhytWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A152)<1,ROW()-2,-1)_uSiDiatWM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A153)<1,ROW()-2,-1)_uDVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A154)<1,ROW()-2,-1)_uPVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A155)<1,ROW()-2,-1)_uNVeg0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A156)<1,ROW()-2,-1)_uDFish0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A157)<1,ROW()-2,-1)_uPFish0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A158)<1,ROW()-2,-1)_uNFish0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A159)<1,ROW()-2,-1)_uPPisc0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A160)<1,ROW()-2,-1)_uNPisc0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A161)<1,ROW()-2,-1)_uO2TotTHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A162)<1,ROW()-2,-1)_uDTotTHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A163)<1,ROW()-2,-1)_uPTotTHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A164)<1,ROW()-2,-1)_uNTotTHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A165)<1,ROW()-2,-1)_uSiTotTHyp0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A166)<1,ROW()-2,-1)_uDTotM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A167)<1,ROW()-2,-1)_uPTotM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A168)<1,ROW()-2,-1)_uNTotM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A169)<1,ROW()-2,-1)_uSiTotM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A170)<1,ROW()-2,-1)_uO2TotM0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A171)<1,ROW()-2,-1)_uO2TotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A172)<1,ROW()-2,-1)_uDTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A173)<1,ROW()-2,-1)_uPTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A174)<1,ROW()-2,-1)_uNTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A175)<1,ROW()-2,-1)_uSiTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A176)<1,ROW()-2,-1)_sDExtTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A177)<1,ROW()-2,-1)_sNExtTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A178)<1,ROW()-2,-1)_sPExtTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A179)<1,ROW()-2,-1)_sSiExtTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A180)<1,ROW()-2,-1)_sO2ExtTotT0_ 0"
write (1,*)" InitState =IF(COUNTBLANK(A181)<1,ROW()-2,-1)_sVegHeight0_ 0"
write (1,*)" state  0 _sDepthW_", sDepthW
write (1,*)" state  1 _sNH4WHyp_", sNH4WH
write (1,*)" state  2 _sNO3WHyp_", sNO3WH
write (1,*)" state  3 _sPO4WHyp_", sPO4WH
write (1,*)" state  4 _sPAIMWHyp_", sPAIMWH
write (1,*)" state  5 _sSiO2WHyp_", sSiO2WH
write (1,*)" state  6 _sO2WHyp_", sO2WH
write (1,*)" state  7 _sDDetWHyp_", sDDetWH
write (1,*)" state  8 _sNDetWHyp_", sNDetWH
write (1,*)" state  9 _sPDetWHyp_", sPDetWH
write (1,*)" state  10 _sSiDetWHyp_", sSiDetWH
write (1,*)" state  11 _sDIMWHyp_", sDIMWH
write (1,*)" state  12 _sDDiatWHyp_", sDDiatWH
write (1,*)" state  13 _sNDiatWHyp_", sNDiatWH
write (1,*)" state  14 _sPDiatWHyp_", sPDiatWH
write (1,*)" state  15 _sDGrenWHyp_", sDGrenWH
write (1,*)" state  16 _sNGrenWHyp_", sNGrenWH
write (1,*)" state  17 _sPGrenWHyp_", sPGrenWH
write (1,*)" state  18 _sDBlueWHyp_", sDBlueWH
write (1,*)" state  19 _sNBlueWHyp_", sNBlueWH
write (1,*)" state  20 _sPBlueWHyp_", sPBlueWH
write (1,*)" state  21 _sDZooHyp_", sDZooH
write (1,*)" state  22 _sNZooHyp_", sNZooH
write (1,*)" state  23 _sPZooHyp_", sPZooH
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
write (1,*)" state  55 _sVegHeight_", sVegHe
write (1,*)" state  56 _sDBent_", sDBent
write (1,*)" state  57 _sNBent_", sNBent
write (1,*)" state  58 _sPBent_", sPBent
write (1,*)" state  59 _sDepthWM_", sDepthWM
write (1,*)" state  60 _sNH4WM_", sNH4WM
write (1,*)" state  61 _sNO3WM_", sNO3WM
write (1,*)" state  62 _sPO4WM_", sPO4WM
write (1,*)" state  63 _sPAIMWM_", sPAIMWM
write (1,*)" state  64 _sSiO2WM_", sSiO2WM
write (1,*)" state  65 _sO2WM_", sO2WM
write (1,*)" state  66 _sDDetWM_", sDDetWM
write (1,*)" state  67 _sNDetWM_", sNDetWM
write (1,*)" state  68 _sPDetWM_", sPDetWM
write (1,*)" state  69 _sSiDetWM_", sSiDetWM
write (1,*)" state  70 _sDIMWM_", sDIMWM
write (1,*)" state  71 _sDDiatWM_", sDDiatWM
write (1,*)" state  72 _sNDiatWM_", sNDiatWM
write (1,*)" state  73 _sPDiatWM_", sPDiatWM
write (1,*)" state  74 _sDGrenWM_", sDGrenWM
write (1,*)" state  75 _sNGrenWM_", sNGrenWM
write (1,*)" state  76 _sPGrenWM_", sPGrenWM
write (1,*)" state  77 _sDBlueWM_", sDBlueWM
write (1,*)" state  78 _sNBlueWM_", sNBlueWM
write (1,*)" state  79 _sPBlueWM_", sPBlueWM
write (1,*)" state  80 _sDZooM_", sDZooM
write (1,*)" state  81 _sNZooM_", sNZooM
write (1,*)" state  82 _sPZooM_", sPZooM
write (1,*)" state  83 _sNH4SM_", sNH4SM
write (1,*)" state  84 _sNO3SM_", sNO3SM
write (1,*)" state  85 _sPO4SM_", sPO4SM
write (1,*)" state  86 _sPAIMSM_", sPAIMSM
write (1,*)" state  87 _sDDetSM_", sDDetSM
write (1,*)" state  88 _sNDetSM_", sNDetSM
write (1,*)" state  89 _sPDetSM_", sPDetSM
write (1,*)" state  90 _sSiDetSM_", sSiDetSM
write (1,*)" state  91 _sDHumSM_", sDHumSM
write (1,*)" state  92 _sNHumSM_", sNHumSM
write (1,*)" state  93 _sPHumSM_", sPHumSM
write (1,*)" state  94 _sDIMSM_", sDIMSM
write (1,*)" state  95 _sDRootPhra_", sDRoPhra
write (1,*)" state  96 _sDShootPhra_", sDShPhra
write (1,*)" state  97 _sNRootPhra_", sNRoPhra
write (1,*)" state  98 _sNShootPhra_", sNShPhra
write (1,*)" state  99 _sPRootPhra_", sPRoPhra
write (1,*)" state  100 _sPShootPhra_", sPShPhra
write (1,*)" state  101 _sNH4WEpi_", sNH4WE
write (1,*)" state  102 _sNO3WEpi_", sNO3WE
write (1,*)" state  103 _sPO4WEpi_", sPO4WE
write (1,*)" state  104 _sPAIMWEpi_", sPAIMWE
write (1,*)" state  105 _sSiO2WEpi_", sSiO2WE
write (1,*)" state  106 _sO2WEpi_", sO2WE
write (1,*)" state  107 _sDDetWEpi_", sDDetWE
write (1,*)" state  108 _sNDetWEpi_", sNDetWE
write (1,*)" state  109 _sPDetWEpi_", sPDetWE
write (1,*)" state  110 _sSiDetWEpi_", sSiDetWE
write (1,*)" state  111 _sDIMWEpi_", sDIMWE
write (1,*)" state  112 _sDDiatWEpi_", sDDiatWE
write (1,*)" state  113 _sNDiatWEpi_", sNDiatWE
write (1,*)" state  114 _sPDiatWEpi_", sPDiatWE
write (1,*)" state  115 _sDGrenWEpi_", sDGrenWE
write (1,*)" state  116 _sNGrenWEpi_", sNGrenWE
write (1,*)" state  117 _sPGrenWEpi_", sPGrenWE
write (1,*)" state  118 _sDBlueWEpi_", sDBlueWE
write (1,*)" state  119 _sNBlueWEpi_", sNBlueWE
write (1,*)" state  120 _sPBlueWEpi_", sPBlueWE
write (1,*)" state  121 _sDZooEpi_", sDZooE
write (1,*)" state  122 _sNZooEpi_", sNZooE
write (1,*)" state  123 _sPZooEpi_", sPZooE
write (1,*)" state  124 _sDExtTotT_", sDExTotT
write (1,*)" state  125 _sNExtTotT_", sNExTotT
write (1,*)" state  126 _sPExtTotT_", sPExTotT
write (1,*)" state  127 _sSiExtTotT_", sSiETotT
write (1,*)" state  128 _sO2ExtTotT_", sO2ETotT
write (1,*)" Auxiliary  0 _sTime_", sTime
write (1,*)" Auxiliary  1 _DaysInYear_", DaysInY
write (1,*)" Auxiliary  2 _aInclStrat_", aInlSrat
write (1,*)" Auxiliary  3 _uDepthMix_", uDptMix
write (1,*)" Auxiliary  4 _uDepthWEpi_", uDptWE
write (1,*)" Auxiliary  5 _uDepthWHyp_", uDptWH
write (1,*)" Auxiliary  6 _MassHyp_", MassH
write (1,*)" Auxiliary  7 _MassEpi_", MassE
write (1,*)" Auxiliary  8 _MassWM_", MassWM
write (1,*)" Auxiliary  9 _oNH4WHyp_", oNH4WH
write (1,*)" Auxiliary  10 _oNO3WHyp_", oNO3WH
write (1,*)" Auxiliary  11 _oPO4WHyp_", oPO4WH
write (1,*)" Auxiliary  12 _oPAIMWHyp_", oPAIMWH
write (1,*)" Auxiliary  13 _oSiO2WHyp_", oSiO2WH
write (1,*)" Auxiliary  14 _oO2WHyp_", oO2WH
write (1,*)" Auxiliary  15 _oDDetWHyp_", oDDtWH
write (1,*)" Auxiliary  16 _oNDetWHyp_", oNDtWH
write (1,*)" Auxiliary  17 _oPDetWHyp_", oPDtWH
write (1,*)" Auxiliary  18 _oSiDetWHyp_", oSiDtWH
write (1,*)" Auxiliary  19 _oDIMWHyp_", oDIMWH
write (1,*)" Auxiliary  20 _oDDiatWHyp_", oDDiWH
write (1,*)" Auxiliary  21 _oNDiatWHyp_", oNDiWH
write (1,*)" Auxiliary  22 _oPDiatWHyp_", oPDiWH
write (1,*)" Auxiliary  23 _oDGrenWHyp_", oDGrenWH
write (1,*)" Auxiliary  24 _oNGrenWHyp_", oNGrenWH
write (1,*)" Auxiliary  25 _oPGrenWHyp_", oPGrenWH
write (1,*)" Auxiliary  26 _oDBlueWHyp_", oDBlueWH
write (1,*)" Auxiliary  27 _oNBlueWHyp_", oNBlueWH
write (1,*)" Auxiliary  28 _oPBlueWHyp_", oPBlueWH
write (1,*)" Auxiliary  29 _oDZooHyp_", oDZooH
write (1,*)" Auxiliary  30 _oNZooHyp_", oNZooH
write (1,*)" Auxiliary  31 _oPZooHyp_", oPZooH
write (1,*)" Auxiliary  32 _oNH4WM_", oNH4WM
write (1,*)" Auxiliary  33 _oNO3WM_", oNO3WM
write (1,*)" Auxiliary  34 _oPO4WM_", oPO4WM
write (1,*)" Auxiliary  35 _oPAIMWM_", oPAIMWM
write (1,*)" Auxiliary  36 _oSiO2WM_", oSiO2WM
write (1,*)" Auxiliary  37 _oO2WM_", oO2WM
write (1,*)" Auxiliary  38 _oDDetWM_", oDDtWM
write (1,*)" Auxiliary  39 _oNDetWM_", oNDtWM
write (1,*)" Auxiliary  40 _oPDetWM_", oPDtWM
write (1,*)" Auxiliary  41 _oSiDetWM_", oSiDtWM
write (1,*)" Auxiliary  42 _oDIMWM_", oDIMWM
write (1,*)" Auxiliary  43 _oDDiatWM_", oDDiWM
write (1,*)" Auxiliary  44 _oNDiatWM_", oNDiWM
write (1,*)" Auxiliary  45 _oPDiatWM_", oPDiWM
write (1,*)" Auxiliary  46 _oDGrenWM_", oDGrenWM
write (1,*)" Auxiliary  47 _oNGrenWM_", oNGrenWM
write (1,*)" Auxiliary  48 _oPGrenWM_", oPGrenWM
write (1,*)" Auxiliary  49 _oDBlueWM_", oDBlueWM
write (1,*)" Auxiliary  50 _oNBlueWM_", oNBlueWM
write (1,*)" Auxiliary  51 _oPBlueWM_", oPBlueWM
write (1,*)" Auxiliary  52 _oDZooM_", oDZooM
write (1,*)" Auxiliary  53 _oNZooM_", oNZooM
write (1,*)" Auxiliary  54 _oPZooM_", oPZooM
write (1,*)" Auxiliary  55 _oNH4WEpi_", oNH4WE
write (1,*)" Auxiliary  56 _oNO3WEpi_", oNO3WE
write (1,*)" Auxiliary  57 _oPO4WEpi_", oPO4WE
write (1,*)" Auxiliary  58 _oPAIMWEpi_", oPAIMWE
write (1,*)" Auxiliary  59 _oSiO2WEpi_", oSiO2WE
write (1,*)" Auxiliary  60 _oO2WEpi_", oO2WE
write (1,*)" Auxiliary  61 _oDDetWEpi_", oDDtWE
write (1,*)" Auxiliary  62 _oNDetWEpi_", oNDtWE
write (1,*)" Auxiliary  63 _oPDetWEpi_", oPDtWE
write (1,*)" Auxiliary  64 _oSiDetWEpi_", oSiDtWE
write (1,*)" Auxiliary  65 _oDIMWEpi_", oDIMWE
write (1,*)" Auxiliary  66 _oDDiatWEpi_", oDDiWE
write (1,*)" Auxiliary  67 _oNDiatWEpi_", oNDiWE
write (1,*)" Auxiliary  68 _oPDiatWEpi_", oPDiWE
write (1,*)" Auxiliary  69 _oDGrenWEpi_", oDGrenWE
write (1,*)" Auxiliary  70 _oNGrenWEpi_", oNGrenWE
write (1,*)" Auxiliary  71 _oPGrenWEpi_", oPGrenWE
write (1,*)" Auxiliary  72 _oDBlueWEpi_", oDBlueWE
write (1,*)" Auxiliary  73 _oNBlueWEpi_", oNBlueWE
write (1,*)" Auxiliary  74 _oPBlueWEpi_", oPBlueWE
write (1,*)" Auxiliary  75 _oDZooEpi_", oDZooE
write (1,*)" Auxiliary  76 _oNZooEpi_", oNZooE
write (1,*)" Auxiliary  77 _oPZooEpi_", oPZooE
write (1,*)" Auxiliary  78 _TimeYears_", TimeYars
write (1,*)" Auxiliary  79 _Day_", Day
write (1,*)" Auxiliary  80 _Years_", Years
write (1,*)" Auxiliary  81 _uVegHeight_", uVeHeght
write (1,*)" Auxiliary  82 _aVegHeight_", aVeHeght
write (1,*)" Auxiliary  83 _uVegHeightHyp_", uVeHehtH
write (1,*)" Auxiliary  84 _uVegHeightEpi_", uVeHehtE
write (1,*)" Auxiliary  85 _aDepth2VegHyp_", aDpt2egH
write (1,*)" Auxiliary  86 _aDepth1VegHyp_", aDpt1egH
write (1,*)" Auxiliary  87 _aDepth2VegEpi_", aDpt2egE
write (1,*)" Auxiliary  88 _aDepth1VegEpi_", aDpt1egE
write (1,*)" Auxiliary  89 _uVegHeightLight_", uVegHL
write (1,*)" Auxiliary  90 _uVegHeightLightSum_", uVegHLS
write (1,*)" Auxiliary  91 _uVegHeightLightEpi_", uVeghhtE
write (1,*)" Auxiliary  92 _uVegHeightLightHyp_", uVeghhtH
write (1,*)" Auxiliary  93 _uDVegHyp_", uDVegH
write (1,*)" Auxiliary  94 _uDVegEpi_", uDVegE
write (1,*)" Auxiliary  95 _uVegShade_", uVegSade
write (1,*)" Auxiliary  96 _aExtPhytHyp_", aExtPytH
write (1,*)" Auxiliary  97 _aExtDetHyp_", aExtDtH
write (1,*)" Auxiliary  98 _aExtIMHyp_", aExtIMH
write (1,*)" Auxiliary  99 _aExtCoefOpenHyp_", aExCofOH
write (1,*)" Auxiliary  100 _aExtPhytEpi_", aExtPytE
write (1,*)" Auxiliary  101 _aExtDetEpi_", aExtDtE
write (1,*)" Auxiliary  102 _aExtIMEpi_", aExtIME
write (1,*)" Auxiliary  103 _aExtCoefOpenEpi_", aExCofOE
write (1,*)" Auxiliary  104 _aTmEpi_", aTmE
write (1,*)" Auxiliary  105 _aTmHyp_", aTmH
write (1,*)" Auxiliary  106 _uTmEpi_", uTmE
write (1,*)" Auxiliary  107 _uTmHyp_", uTmH
write (1,*)" Auxiliary  108 _uTmBot_", uTmBot
write (1,*)" Auxiliary  109 _uLAT_", uLAT
write (1,*)" Auxiliary  110 _uSolDecAng_", uSoDeAng
write (1,*)" Auxiliary  111 _uSunRise_", uSunRise
write (1,*)" Auxiliary  112 _uSunSet_", uSunSet
write (1,*)" Auxiliary  113 _uSunHours_", uSunHour
write (1,*)" Auxiliary  114 _uSunPath_", uSunPath
write (1,*)" Auxiliary  115 _uTrans_", uTrans
write (1,*)" Auxiliary  116 _ufDay_", ufDay
write (1,*)" Auxiliary  117 _uLDay_", uLDay
write (1,*)" Auxiliary  118 _uLOut_", uLOut
write (1,*)" Auxiliary  119 _uLPARSurf_", uLPARurf
write (1,*)" Auxiliary  120 _aLPARBotEpiOpen_", aLPRBtEO
write (1,*)" Auxiliary  121 _aLPARBotHypOpen_", aLPRBtHO
write (1,*)" Auxiliary  122 _aLPAR1VegEpi_", aLPR1egE
write (1,*)" Auxiliary  123 _aLPAR1VegHyp_", aLPR1egH
write (1,*)" Auxiliary  124 _aLPARVegLight_", aLPVeght
write (1,*)" Auxiliary  125 _uTmVegAve_", uTmVeAve
write (1,*)" Auxiliary  126 _uFunTmVeg_", uFunTVeg
write (1,*)" Auxiliary  127 _uFunTmProdVeg_", uFumPVeg
write (1,*)" Auxiliary  128 _uhLVeg_", uhLVeg
write (1,*)" Auxiliary  129 _aLPIVeg_", aLPIVeg
write (1,*)" Auxiliary  130 _aSpring_", aSpring
write (1,*)" Auxiliary  131 _aTimeWinVeg_", aTiWiVeg
write (1,*)" Auxiliary  132 _aTimeInitVeg_", aTiInVeg
write (1,*)" Auxiliary  133 _aVegSum_", aVSum
write (1,*)" Auxiliary  134 _aVegWin_", aVWin
write (1,*)" Auxiliary  135 _aDaysVegSum_", aDaysSuV
write (1,*)" Auxiliary  136 _aDaysVegWin_", aDaysWiV
write (1,*)" Auxiliary  137 _uDayWinVeg_", uDaWiVeg
write (1,*)" Auxiliary  138 _aDayInitVeg_", aDaInVeg
write (1,*)" Auxiliary  139 _bfRootVeg_", bfRtVeg
write (1,*)" Auxiliary  140 _bfShootVeg_", bfShVeg
write (1,*)" Auxiliary  141 _aDShootVeg_", aDShVeg
write (1,*)" Auxiliary  142 _aDEmergVeg_", aDEerVeg
write (1,*)" Auxiliary  143 _aDFloatVeg_", aDFoaVeg
write (1,*)" Auxiliary  144 _bfSubVeg_", bfSubVeg
write (1,*)" Auxiliary  145 _aDSubVeg_", aDSubVeg
write (1,*)" Auxiliary  146 _aExtVegHyp_", aExtVegH
write (1,*)" Auxiliary  147 _aExtCoefHyp_", aExtCefH
write (1,*)" Auxiliary  148 _aExtVegEpi_", aExtVegE
write (1,*)" Auxiliary  149 _aExtCoefEpi_", aExtCefE
write (1,*)" Auxiliary  150 _aLPARBotEpi_", aLPARotE
write (1,*)" Auxiliary  151 _uLPARSurfHyp_", uLPRSrfH
write (1,*)" Auxiliary  152 _aLPAR2VegEpi_", aLPR2egE
write (1,*)" Auxiliary  153 _aLPAR2VegHyp_", aLPR2egH
write (1,*)" Auxiliary  154 _uVWind_", uVWind
write (1,*)" Auxiliary  155 _aStrat_", aStrat
write (1,*)" Auxiliary  156 _uQInSeason_", uQISeson
write (1,*)" Auxiliary  157 _uQEvSinus_", uQEvSnus
write (1,*)" Auxiliary  158 _aQEv_", aQEv
write (1,*)" Auxiliary  159 _uQEv_", uQEv
write (1,*)" Auxiliary  160 _uQInExtra_", uQInEtra
write (1,*)" Auxiliary  161 _uQIn_", uQIn
write (1,*)" Auxiliary  162 _uQOutExtra_", uQOtEtra
write (1,*)" Auxiliary  163 _uQOutEpi_", uQOutE
write (1,*)" Auxiliary  164 _uQOutHyp_", uQOutH
write (1,*)" Auxiliary  165 _uQDilEpi_", uQDilE
write (1,*)" Auxiliary  166 _uQDilHyp_", uQDilH
write (1,*)" Auxiliary  167 _ukDilEpi_", ukDilE
write (1,*)" Auxiliary  168 _ukDilHyp_", ukDilH
write (1,*)" Auxiliary  169 _ukDilWatEpi_", ukDilatE
write (1,*)" Auxiliary  170 _ukDilWatHyp_", ukDilatH
write (1,*)" Auxiliary  171 _ukOutEpi_", ukOutE
write (1,*)" Auxiliary  172 _ukOutHyp_", ukOutH
write (1,*)" Auxiliary  173 _uTauWatEpi_", uTauWatE
write (1,*)" Auxiliary  174 _uTauWatHyp_", uTauWatH
write (1,*)" Auxiliary  175 _uTauWat_", uTauWat
write (1,*)" Auxiliary  176 _uTauSubstHyp_", uTaSustH
write (1,*)" Auxiliary  177 _uFiJvMortHyp_", uFiJvMH
write (1,*)" Auxiliary  178 _uFiJvEgesHyp_", uFiJvEH
write (1,*)" Auxiliary  179 _uFiJvExcrHyp_", uFivEcrH
write (1,*)" Auxiliary  180 _uFiAdMortHyp_", uFiAdMH
write (1,*)" Auxiliary  181 _uFiAdEgesHyp_", uFiAdEH
write (1,*)" Auxiliary  182 _uFiAdExcrHyp_", uFidEcrH
write (1,*)" Auxiliary  183 _uPiscMortHyp_", uPiscMH
write (1,*)" Auxiliary  184 _uPiscEgesHyp_", uPiscEH
write (1,*)" Auxiliary  185 _uPiscExcrHyp_", uPicEcrH
write (1,*)" Auxiliary  186 _vTranDepthW_", vTranptW
write (1,*)" Auxiliary  187 _vTranHypEpiW_", vTranHEW
write (1,*)" Auxiliary  188 _vDeltaWHyp_", vDeltaWH
write (1,*)" Auxiliary  189 _vDeltaWEpi_", vDeltaWE
write (1,*)" Auxiliary  190 _afVolHypEpi_", afVolHE
write (1,*)" Auxiliary  191 _wDAdvIMW_", wDAdvIMW
write (1,*)" Auxiliary  192 _wPAdvPO4W_", wPAdvO4W
write (1,*)" Auxiliary  193 _wPAdvAIMW_", wPAdvIMW
write (1,*)" Auxiliary  194 _wNAdvNH4W_", wNAdvH4W
write (1,*)" Auxiliary  195 _wNAdvNO3W_", wNAdvO3W
write (1,*)" Auxiliary  196 _wSiAdvSiO2W_", wSidvO2W
write (1,*)" Auxiliary  197 _wO2AdvW_", wO2AdvW
write (1,*)" Auxiliary  198 _wDAdvDetW_", wDAdvDtW
write (1,*)" Auxiliary  199 _wPAdvDetW_", wPAdvDtW
write (1,*)" Auxiliary  200 _wNAdvDetW_", wNAdvDtW
write (1,*)" Auxiliary  201 _wSiAdvDetW_", wSiAdDtW
write (1,*)" Auxiliary  202 _wDAdvDiatW_", wDAdvDiW
write (1,*)" Auxiliary  203 _wPAdvDiatW_", wPAdvDiW
write (1,*)" Auxiliary  204 _wNAdvDiatW_", wNAdvDiW
write (1,*)" Auxiliary  205 _wSiAdvDiatW_", wSiAdDiW
write (1,*)" Auxiliary  206 _wDAdvGrenW_", wDAvGenW
write (1,*)" Auxiliary  207 _wPAdvGrenW_", wPAvGenW
write (1,*)" Auxiliary  208 _wNAdvGrenW_", wNAvGenW
write (1,*)" Auxiliary  209 _wDAdvBlueW_", wDAvBueW
write (1,*)" Auxiliary  210 _wPAdvBlueW_", wPAvBueW
write (1,*)" Auxiliary  211 _wNAdvBlueW_", wNAvBueW
write (1,*)" Auxiliary  212 _wDAdvZooW_", wDAdvooW
write (1,*)" Auxiliary  213 _wPAdvZooW_", wPAdvooW
write (1,*)" Auxiliary  214 _wNAdvZooW_", wNAdvooW
write (1,*)" Auxiliary  215 _wDAdvIMWM_", wDAIMWM
write (1,*)" Auxiliary  216 _wPAdvPO4WM_", wPAPO4WM
write (1,*)" Auxiliary  217 _wPAdvAIMWM_", wPAAIMWM
write (1,*)" Auxiliary  218 _wNAdvNH4WM_", wNANH4WM
write (1,*)" Auxiliary  219 _wNAdvNO3WM_", wNANO3WM
write (1,*)" Auxiliary  220 _wSiAdvSiO2WM_", wSiAO2WM
write (1,*)" Auxiliary  221 _wO2AdvWM_", wO2AWM
write (1,*)" Auxiliary  222 _wDAdvDetWM_", wDADeWM
write (1,*)" Auxiliary  223 _wPAdvDetWM_", wPADeWM
write (1,*)" Auxiliary  224 _wNAdvDetWM_", wNADeWM
write (1,*)" Auxiliary  225 _wSiAdvDetWM_", wSiADeWM
write (1,*)" Auxiliary  226 _wDAdvDiatWM_", wDADiWM
write (1,*)" Auxiliary  227 _wPAdvDiatWM_", wPADiWM
write (1,*)" Auxiliary  228 _wNAdvDiatWM_", wNADiWM
write (1,*)" Auxiliary  229 _wSiAdvDiatWM_", wSiADiWM
write (1,*)" Auxiliary  230 _wDAdvGrenWM_", wDAGWM
write (1,*)" Auxiliary  231 _wPAdvGrenWM_", wPAGWM
write (1,*)" Auxiliary  232 _wNAdvGrenWM_", wNAGWM
write (1,*)" Auxiliary  233 _wDAdvBlueWM_", wDABWM
write (1,*)" Auxiliary  234 _wPAdvBlueWM_", wPABWM
write (1,*)" Auxiliary  235 _wNAdvBlueWM_", wNABWM
write (1,*)" Auxiliary  236 _wDAdvZooWM_", wDAZWM
write (1,*)" Auxiliary  237 _wPAdvZooWM_", wPAZWM
write (1,*)" Auxiliary  238 _wNAdvZooWM_", wNAZWM
write (1,*)" Auxiliary  239 _uExchMaxW_", uExMaxW
write (1,*)" Auxiliary  240 _akExchWHyp_", akExWH
write (1,*)" Auxiliary  241 _akExchWEpi_", akExWE
write (1,*)" Auxiliary  242 _wDExchIMWHyp_", wDExIMWH
write (1,*)" Auxiliary  243 _wPExchPO4WHyp_", wPExP4WH
write (1,*)" Auxiliary  244 _wPExchAIMWHyp_", wPExAMWH
write (1,*)" Auxiliary  245 _wNExchNH4WHyp_", wNExN4WH
write (1,*)" Auxiliary  246 _wNExchNO3WHyp_", wNExN3WH
write (1,*)" Auxiliary  247 _wSiExchSiO2WHyp_", wSixS2WH
write (1,*)" Auxiliary  248 _wO2ExchWHyp_", wO2ExWH
write (1,*)" Auxiliary  249 _wDExchDetWHyp_", wDExDtWH
write (1,*)" Auxiliary  250 _wPExchDetWHyp_", wPExDtWH
write (1,*)" Auxiliary  251 _wNExchDetWHyp_", wNExDtWH
write (1,*)" Auxiliary  252 _wSiExchDetWHyp_", wSiExtWH
write (1,*)" Auxiliary  253 _wDExchDiatWHyp_", wDExDiWH
write (1,*)" Auxiliary  254 _wPExchDiatWHyp_", wPExDiWH
write (1,*)" Auxiliary  255 _wNExchDiatWHyp_", wNExDiWH
write (1,*)" Auxiliary  256 _wSiExchDiatWHyp_", wSiExiWH
write (1,*)" Auxiliary  257 _wDExchGrenWHyp_", wDEGrnWH
write (1,*)" Auxiliary  258 _wPExchGrenWHyp_", wPEGrnWH
write (1,*)" Auxiliary  259 _wNExchGrenWHyp_", wNEGrnWH
write (1,*)" Auxiliary  260 _wDExchBlueWHyp_", wDEBleWH
write (1,*)" Auxiliary  261 _wPExchBlueWHyp_", wPEBleWH
write (1,*)" Auxiliary  262 _wNExchBlueWHyp_", wNEBleWH
write (1,*)" Auxiliary  263 _wDExchZooWHyp_", wDExZoWH
write (1,*)" Auxiliary  264 _wPExchZooWHyp_", wPExZoWH
write (1,*)" Auxiliary  265 _wNExchZooWHyp_", wNExZoWH
write (1,*)" Auxiliary  266 _wDExchIMWEpi_", wDExIMWE
write (1,*)" Auxiliary  267 _wPExchPO4WEpi_", wPExP4WE
write (1,*)" Auxiliary  268 _wPExchAIMWEpi_", wPExAMWE
write (1,*)" Auxiliary  269 _wNExchNH4WEpi_", wNExN4WE
write (1,*)" Auxiliary  270 _wNExchNO3WEpi_", wNExN3WE
write (1,*)" Auxiliary  271 _wSiExchSiO2WEpi_", wSixS2WE
write (1,*)" Auxiliary  272 _wO2ExchWEpi_", wO2ExWE
write (1,*)" Auxiliary  273 _wDExchDetWEpi_", wDExDtWE
write (1,*)" Auxiliary  274 _wPExchDetWEpi_", wPExDtWE
write (1,*)" Auxiliary  275 _wNExchDetWEpi_", wNExDtWE
write (1,*)" Auxiliary  276 _wSiExchDetWEpi_", wSiExtWE
write (1,*)" Auxiliary  277 _wDExchDiatWEpi_", wDExDiWE
write (1,*)" Auxiliary  278 _wPExchDiatWEpi_", wPExDiWE
write (1,*)" Auxiliary  279 _wNExchDiatWEpi_", wNExDiWE
write (1,*)" Auxiliary  280 _wSiExchDiatWEpi_", wSiExiWE
write (1,*)" Auxiliary  281 _wDExchGrenWEpi_", wDEGrnWE
write (1,*)" Auxiliary  282 _wPExchGrenWEpi_", wPEGrnWE
write (1,*)" Auxiliary  283 _wNExchGrenWEpi_", wNEGrnWE
write (1,*)" Auxiliary  284 _wDExchBlueWEpi_", wDEBleWE
write (1,*)" Auxiliary  285 _wPExchBlueWEpi_", wPEBleWE
write (1,*)" Auxiliary  286 _wNExchBlueWEpi_", wNEBleWE
write (1,*)" Auxiliary  287 _wDExchZooWEpi_", wDExZoWE
write (1,*)" Auxiliary  288 _wPExchZooWEpi_", wPExZoWE
write (1,*)" Auxiliary  289 _wNExchZooWEpi_", wNExZoWE
write (1,*)" Auxiliary  290 _akExchMHyp_", akExMH
write (1,*)" Auxiliary  291 _akExchLHyp_", akExLH
write (1,*)" Auxiliary  292 _oDPhytWHyp_", oDPhytWH
write (1,*)" Auxiliary  293 _oPPhytWHyp_", oPPhytWH
write (1,*)" Auxiliary  294 _oNPhytWHyp_", oNPhytWH
write (1,*)" Auxiliary  295 _aDPhytS_", aDPhytS
write (1,*)" Auxiliary  296 _aPPhytS_", aPPhytS
write (1,*)" Auxiliary  297 _aNPhytS_", aNPhytS
write (1,*)" Auxiliary  298 _oDPhytWEpi_", oDPhytWE
write (1,*)" Auxiliary  299 _oDOMWEpi_", oDOMWE
write (1,*)" Auxiliary  300 _oDOMWHyp_", oDOMWH
write (1,*)" Auxiliary  301 _oTDOMW_", oTDOMW
write (1,*)" Auxiliary  302 _oDSestWHyp_", oDSestWH
write (1,*)" Auxiliary  303 _oPOMWHyp_", oPOMWH
write (1,*)" Auxiliary  304 _oPSestWHyp_", oPSestWH
write (1,*)" Auxiliary  305 _oPInorgWHyp_", oPInogWH
write (1,*)" Auxiliary  306 _oPTotWHyp_", oPTotWH
write (1,*)" Auxiliary  307 _oNDissWEpi_", oNDissWE
write (1,*)" Auxiliary  308 _oNDissWHyp_", oNDissWH
write (1,*)" Auxiliary  309 _oNOMWHyp_", oNOMWH
write (1,*)" Auxiliary  310 _oNSestWHyp_", oNSestWH
write (1,*)" Auxiliary  311 _oNkjWHyp_", oNkjWH
write (1,*)" Auxiliary  312 _oNTotWHyp_", oNTotWH
write (1,*)" Auxiliary  313 _bPorS_", bPorS
write (1,*)" Auxiliary  314 _bPorCorS_", bPorCorS
write (1,*)" Auxiliary  315 _aDTotS_", aDTotS
write (1,*)" Auxiliary  316 _aRhoTotS_", aRhoTotS
write (1,*)" Auxiliary  317 _aRhoSolidS_", aRhSoidS
write (1,*)" Auxiliary  318 _afDTotS_", afDTotS
write (1,*)" Auxiliary  319 _afDOrgS_", afDOrgS
write (1,*)" Auxiliary  320 _afDetS_", afDtS
write (1,*)" Auxiliary  321 _afDetTotS_", afDtTotS
write (1,*)" Auxiliary  322 _aPInorgS_", aPInorgS
write (1,*)" Auxiliary  323 _aPTotAvailS_", aPTtAilS
write (1,*)" Auxiliary  324 _aPTotS_", aPTotS
write (1,*)" Auxiliary  325 _afPInorgS_", afPInrgS
write (1,*)" Auxiliary  326 _afPTotS_", afPTotS
write (1,*)" Auxiliary  327 _afPO4S_", afPO4S
write (1,*)" Auxiliary  328 _oPO4S_", oPO4S
write (1,*)" Auxiliary  329 _aNDissS_", aNDissS
write (1,*)" Auxiliary  330 _aNkjAvailS_", aNkAvilS
write (1,*)" Auxiliary  331 _aNkjS_", aNkjS
write (1,*)" Auxiliary  332 _aNTotAvailS_", aNTtAilS
write (1,*)" Auxiliary  333 _aNTotS_", aNTotS
write (1,*)" Auxiliary  334 _afNInorgS_", afNInrgS
write (1,*)" Auxiliary  335 _afNTotS_", afNTotS
write (1,*)" Auxiliary  336 _oNO3S_", oNO3S
write (1,*)" Auxiliary  337 _oNH4S_", oNH4S
write (1,*)" Auxiliary  338 _oNDissS_", oNDissS
write (1,*)" Auxiliary  339 _rPDIMWHyp_", rPDIMWH
write (1,*)" Auxiliary  340 _rPDIMS_", rPDIMS
write (1,*)" Auxiliary  341 _rPDDetWHyp_", rPDDtWH
write (1,*)" Auxiliary  342 _rNDDetWHyp_", rNDDtWH
write (1,*)" Auxiliary  343 _rSiDDetWHyp_", rSiDDtWH
write (1,*)" Auxiliary  344 _rPDHumS_", rPDHumS
write (1,*)" Auxiliary  345 _rNDHumS_", rNDHumS
write (1,*)" Auxiliary  346 _rPDDetS_", rPDDtS
write (1,*)" Auxiliary  347 _rNDDetS_", rNDDtS
write (1,*)" Auxiliary  348 _rSiDDetS_", rSiDDtS
write (1,*)" Auxiliary  349 _oDPhytWM_", oDPhytWM
write (1,*)" Auxiliary  350 _oPPhytWM_", oPPhytWM
write (1,*)" Auxiliary  351 _oNPhytWM_", oNPhytWM
write (1,*)" Auxiliary  352 _oSiDiatWM_", oSiDiWM
write (1,*)" Auxiliary  353 _oDOMWM_", oDOMWM
write (1,*)" Auxiliary  354 _oDSestWM_", oDSestWM
write (1,*)" Auxiliary  355 _oPOMWM_", oPOMWM
write (1,*)" Auxiliary  356 _oPSestWM_", oPSestWM
write (1,*)" Auxiliary  357 _oPInorgWM_", oPInogWM
write (1,*)" Auxiliary  358 _oPTotWM_", oPTotWM
write (1,*)" Auxiliary  359 _oNDissWM_", oNDissWM
write (1,*)" Auxiliary  360 _oNOMWM_", oNOMWM
write (1,*)" Auxiliary  361 _oNSestWM_", oNSestWM
write (1,*)" Auxiliary  362 _oNkjWM_", oNkjWM
write (1,*)" Auxiliary  363 _oNTotWM_", oNTotWM
write (1,*)" Auxiliary  364 _bPorSM_", bPorSM
write (1,*)" Auxiliary  365 _bPorCorSM_", bPorCrSM
write (1,*)" Auxiliary  366 _aDTotSM_", aDTotSM
write (1,*)" Auxiliary  367 _aRhoTotSM_", aRhoTtSM
write (1,*)" Auxiliary  368 _aRhoSolidSM_", aRhSodSM
write (1,*)" Auxiliary  369 _afDTotSM_", afDTotSM
write (1,*)" Auxiliary  370 _afDOrgSM_", afDOrgSM
write (1,*)" Auxiliary  371 _afDetSM_", afDtSM
write (1,*)" Auxiliary  372 _afDetTotSM_", afDtTtSM
write (1,*)" Auxiliary  373 _aPInorgSM_", aPInogSM
write (1,*)" Auxiliary  374 _aPTotAvailSM_", aPTAvlSM
write (1,*)" Auxiliary  375 _aPTotSM_", aPTotSM
write (1,*)" Auxiliary  376 _afPInorgSM_", afPnogSM
write (1,*)" Auxiliary  377 _afPTotSM_", afPTotSM
write (1,*)" Auxiliary  378 _afPO4SM_", afPO4SM
write (1,*)" Auxiliary  379 _oPO4SM_", oPO4SM
write (1,*)" Auxiliary  380 _aNDissSM_", aNDissSM
write (1,*)" Auxiliary  381 _aNkjAvailSM_", aNkAvlSM
write (1,*)" Auxiliary  382 _aNkjSM_", aNkjSM
write (1,*)" Auxiliary  383 _aNTotAvailSM_", aNTAvlSM
write (1,*)" Auxiliary  384 _aNTotSM_", aNTotSM
write (1,*)" Auxiliary  385 _afNInorgSM_", afNnogSM
write (1,*)" Auxiliary  386 _afNTotSM_", afNTotSM
write (1,*)" Auxiliary  387 _oNO3SM_", oNO3SM
write (1,*)" Auxiliary  388 _oNH4SM_", oNH4SM
write (1,*)" Auxiliary  389 _oNDissSM_", oNDissSM
write (1,*)" Auxiliary  390 _rPDIMWM_", rPDIMWM
write (1,*)" Auxiliary  391 _rPDIMSM_", rPDIMSM
write (1,*)" Auxiliary  392 _rPDDetWM_", rPDDtWM
write (1,*)" Auxiliary  393 _rNDDetWM_", rNDDtWM
write (1,*)" Auxiliary  394 _rSiDDetWM_", rSiDDtWM
write (1,*)" Auxiliary  395 _rPDHumSM_", rPDHumSM
write (1,*)" Auxiliary  396 _rNDHumSM_", rNDHumSM
write (1,*)" Auxiliary  397 _rPDDetSM_", rPDDtSM
write (1,*)" Auxiliary  398 _rNDDetSM_", rNDDtSM
write (1,*)" Auxiliary  399 _rSiDDetSM_", rSiDDtSM
write (1,*)" Auxiliary  400 _aDTotM_", aDTotM
write (1,*)" Auxiliary  401 _aPTotM_", aPTotM
write (1,*)" Auxiliary  402 _aNTotM_", aNTotM
write (1,*)" Auxiliary  403 _aSiTotM_", aSiTotM
write (1,*)" Auxiliary  404 _aO2TotM_", aO2TotM
write (1,*)" Auxiliary  405 _uPLoadSeason_", uPLdSson
write (1,*)" Auxiliary  406 _uPLoad_", uPLoad
write (1,*)" Auxiliary  407 _uPLoadPO4_", uPLoaPO4
write (1,*)" Auxiliary  408 _uPLoadOrg_", uPLoaOrg
write (1,*)" Auxiliary  409 _uPLoadPhytTot_", uPLdPTot
write (1,*)" Auxiliary  410 _uPLoadDet_", uPLoadDt
write (1,*)" Auxiliary  411 _uPLoadAIM_", uPLoaAIM
write (1,*)" Auxiliary  412 _uNLoadSeason_", uNLdSson
write (1,*)" Auxiliary  413 _uNLoadPhytTot_", uNLdPTot
write (1,*)" Auxiliary  414 _uNLoad_", uNLoad
write (1,*)" Auxiliary  415 _uNLoadDet_", uNLoadDt
write (1,*)" Auxiliary  416 _uNLoadOrg_", uNLoaOrg
write (1,*)" Auxiliary  417 _uNLoadDiss_", uNLadiss
write (1,*)" Auxiliary  418 _uNLoadNH4_", uNLoaNH4
write (1,*)" Auxiliary  419 _uNLoadNO3_", uNLoaNO3
write (1,*)" Auxiliary  420 _uNBackLoadHyp_", uNBckadH
write (1,*)" Auxiliary  421 _uPBackLoadHyp_", uPBckadH
write (1,*)" Auxiliary  422 _uNTotIn_", uNTotIn
write (1,*)" Auxiliary  423 _uDLoadDet_", uDLoadDt
write (1,*)" Auxiliary  424 _uDLoadPhytTot_", uDLdPTot
write (1,*)" Auxiliary  425 _uDLoadIM_", uDLoadIM
write (1,*)" Auxiliary  426 _uDLoad_", uDLoad
write (1,*)" Auxiliary  427 _uPTotIn_", UotIn
write (1,*)" Auxiliary  428 _uDLoadDiat_", uDLoadDi
write (1,*)" Auxiliary  429 _uPLoadDiat_", uPLoadDi
write (1,*)" Auxiliary  430 _uNLoadDiat_", uNLoadDi
write (1,*)" Auxiliary  431 _uDLoadGren_", uDLadren
write (1,*)" Auxiliary  432 _uPLoadGren_", uPLadren
write (1,*)" Auxiliary  433 _uNLoadGren_", uNLadren
write (1,*)" Auxiliary  434 _uDLoadBlue_", uDLadlue
write (1,*)" Auxiliary  435 _uPLoadBlue_", uPLadlue
write (1,*)" Auxiliary  436 _uNLoadBlue_", uNLadlue
write (1,*)" Auxiliary  437 _wDDilIMHyp_", wDDilIMH
write (1,*)" Auxiliary  438 _wDDilDetHyp_", wDDilDtH
write (1,*)" Auxiliary  439 _wDDilGrenHyp_", wDDlGenH
write (1,*)" Auxiliary  440 _wDDilBlueHyp_", wDDlBueH
write (1,*)" Auxiliary  441 _wDDilDiatHyp_", wDDilDiH
write (1,*)" Auxiliary  442 _wDDilZooHyp_", wDDilooH
write (1,*)" Auxiliary  443 _wDDilPhytHyp_", wDDlPytH
write (1,*)" Auxiliary  444 _wPDilZooHyp_", wPDilooH
write (1,*)" Auxiliary  445 _wNDilZooHyp_", wNDilooH
write (1,*)" Auxiliary  446 _wPDilPO4Hyp_", wPDilO4H
write (1,*)" Auxiliary  447 _wPDilDetHyp_", wPDilDtH
write (1,*)" Auxiliary  448 _wPDilAIMHyp_", wPDilIMH
write (1,*)" Auxiliary  449 _wNDilNH4Hyp_", wNDilH4H
write (1,*)" Auxiliary  450 _wNDilNO3Hyp_", wNDilO3H
write (1,*)" Auxiliary  451 _wNDilDetHyp_", wNDilDtH
write (1,*)" Auxiliary  452 _wO2InflowHyp_", wO2nfowH
write (1,*)" Auxiliary  453 _wO2OutflHyp_", wO2OuflH
write (1,*)" Auxiliary  454 _wPDilDiatHyp_", wPDilDiH
write (1,*)" Auxiliary  455 _wNDilDiatHyp_", wNDilDiH
write (1,*)" Auxiliary  456 _wPDilGrenHyp_", wPDlGenH
write (1,*)" Auxiliary  457 _wNDilGrenHyp_", wNDlGenH
write (1,*)" Auxiliary  458 _wPDilBlueHyp_", wPDlBueH
write (1,*)" Auxiliary  459 _wNDilBlueHyp_", wNDlBueH
write (1,*)" Auxiliary  460 _wPDilPhytHyp_", wPDlPytH
write (1,*)" Auxiliary  461 _wNDilPhytHyp_", wNDlPytH
write (1,*)" Auxiliary  462 _wDOutflTotHyp_", wDOtfotH
write (1,*)" Auxiliary  463 _wPOutflTotHyp_", wPOtfotH
write (1,*)" Auxiliary  464 _wNOutflTotHyp_", wNOtfotH
write (1,*)" Auxiliary  465 _wDTranDiatHyp_", wDTraDiH
write (1,*)" Auxiliary  466 _wPTranDiatHyp_", wPTraDiH
write (1,*)" Auxiliary  467 _wNTranDiatHyp_", wNTraDiH
write (1,*)" Auxiliary  468 _wDTranGrenHyp_", wDTanenH
write (1,*)" Auxiliary  469 _wPTranGrenHyp_", wPTanenH
write (1,*)" Auxiliary  470 _wNTranGrenHyp_", wNTanenH
write (1,*)" Auxiliary  471 _wDTranBlueHyp_", wDTanueH
write (1,*)" Auxiliary  472 _wPTranBlueHyp_", wPTanueH
write (1,*)" Auxiliary  473 _wNTranBlueHyp_", wNTanueH
write (1,*)" Auxiliary  474 _wDTranPhytHyp_", wDTanytH
write (1,*)" Auxiliary  475 _wPTranPhytHyp_", wPTanytH
write (1,*)" Auxiliary  476 _wNTranPhytHyp_", wNTanytH
write (1,*)" Auxiliary  477 _uSiLoadSiO2_", uSioaiO2
write (1,*)" Auxiliary  478 _uSiLoadDet_", uSiLodDt
write (1,*)" Auxiliary  479 _uSiLoadDiat_", uSiLodDi
write (1,*)" Auxiliary  480 _uSiLoad_", uSiLoad
write (1,*)" Auxiliary  481 _wSiDilSiO2Hyp_", wSiilO2H
write (1,*)" Auxiliary  482 _wSiDilDetHyp_", wSiDiDtH
write (1,*)" Auxiliary  483 _wSiDilDiatHyp_", wSiDiDiH
write (1,*)" Auxiliary  484 _wSiOutflTotHyp_", wSitfotH
write (1,*)" Auxiliary  485 _wSiTranSiO2Hyp_", wSianO2H
write (1,*)" Auxiliary  486 _wSiTranDetWHyp_", wSiratWH
write (1,*)" Auxiliary  487 _tSiTranTotTHyp_", tSiantTH
write (1,*)" Auxiliary  488 _wDTranZooHyp_", wDTanooH
write (1,*)" Auxiliary  489 _wPTranZooHyp_", wPTanooH
write (1,*)" Auxiliary  490 _wNTranZooHyp_", wNTanooH
write (1,*)" Auxiliary  491 _wDTranIMWHyp_", wDTanMWH
write (1,*)" Auxiliary  492 _wDTranDetWHyp_", wDTantWH
write (1,*)" Auxiliary  493 _wO2TranWHyp_", wO2TrnWH
write (1,*)" Auxiliary  494 _wPTranPO4WHyp_", wPTan4WH
write (1,*)" Auxiliary  495 _wPTranAIMWHyp_", wPTanMWH
write (1,*)" Auxiliary  496 _wPTranDetWHyp_", wPTantWH
write (1,*)" Auxiliary  497 _wNTranNH4WHyp_", wNTan4WH
write (1,*)" Auxiliary  498 _wNTranNO3WHyp_", wNTan3WH
write (1,*)" Auxiliary  499 _wNTranDetWHyp_", wNTantWH
write (1,*)" Auxiliary  500 _wDDilTotHyp_", wDDilotH
write (1,*)" Auxiliary  501 _wPDilTotHyp_", wPDilotH
write (1,*)" Auxiliary  502 _wNDilTotHyp_", wNDilotH
write (1,*)" Auxiliary  503 _wSiDilTotHyp_", wSiilotH
write (1,*)" Auxiliary  504 _tDTranTotTHyp_", tDTantTH
write (1,*)" Auxiliary  505 _tPTranTotTHyp_", tPTantTH
write (1,*)" Auxiliary  506 _tNTranTotTHyp_", tNTantTH
write (1,*)" Auxiliary  507 _wDExchIMMHyp_", wDExIMMH
write (1,*)" Auxiliary  508 _wPExchPO4MHyp_", wPExP4MH
write (1,*)" Auxiliary  509 _wPExchAIMMHyp_", wPExAMMH
write (1,*)" Auxiliary  510 _wNExchNH4MHyp_", wNExN4MH
write (1,*)" Auxiliary  511 _wNExchNO3MHyp_", wNExN3MH
write (1,*)" Auxiliary  512 _wSiExchSiO2MHyp_", wSixS2MH
write (1,*)" Auxiliary  513 _wO2ExchMHyp_", wO2ExMH
write (1,*)" Auxiliary  514 _wDExchDetMHyp_", wDExDtMH
write (1,*)" Auxiliary  515 _wPExchDetMHyp_", wPExDtMH
write (1,*)" Auxiliary  516 _wNExchDetMHyp_", wNExDtMH
write (1,*)" Auxiliary  517 _wSiExchDetMHyp_", wSiExtMH
write (1,*)" Auxiliary  518 _wDExchDiatMHyp_", wDExDiMH
write (1,*)" Auxiliary  519 _wPExchDiatMHyp_", wPExDiMH
write (1,*)" Auxiliary  520 _wNExchDiatMHyp_", wNExDiMH
write (1,*)" Auxiliary  521 _wSiExchDiatMHyp_", wSiExiMH
write (1,*)" Auxiliary  522 _wDExchGrenMHyp_", wDEGrnMH
write (1,*)" Auxiliary  523 _wPExchGrenMHyp_", wPEGrnMH
write (1,*)" Auxiliary  524 _wNExchGrenMHyp_", wNEGrnMH
write (1,*)" Auxiliary  525 _wDExchBlueMHyp_", wDEBleMH
write (1,*)" Auxiliary  526 _wPExchBlueMHyp_", wPEBleMH
write (1,*)" Auxiliary  527 _wNExchBlueMHyp_", wNEBleMH
write (1,*)" Auxiliary  528 _wDExchZooMHyp_", wDExZoMH
write (1,*)" Auxiliary  529 _wPExchZooMHyp_", wPExZoMH
write (1,*)" Auxiliary  530 _wNExchZooMHyp_", wNExZoMH
write (1,*)" Auxiliary  531 _wDExchIMHyp_", wDExIMH
write (1,*)" Auxiliary  532 _wPExchPO4Hyp_", wPExPO4H
write (1,*)" Auxiliary  533 _wPExchAIMHyp_", wPExAIMH
write (1,*)" Auxiliary  534 _wNExchNH4Hyp_", wNExNH4H
write (1,*)" Auxiliary  535 _wNExchNO3Hyp_", wNExNO3H
write (1,*)" Auxiliary  536 _wSiExchSiO2Hyp_", wSixSO2H
write (1,*)" Auxiliary  537 _wO2ExchHyp_", wO2ExH
write (1,*)" Auxiliary  538 _wDExchDetHyp_", wDExDtH
write (1,*)" Auxiliary  539 _wPExchDetHyp_", wPExDtH
write (1,*)" Auxiliary  540 _wNExchDetHyp_", wNExDtH
write (1,*)" Auxiliary  541 _wSiExchDetHyp_", wSiExDtH
write (1,*)" Auxiliary  542 _wDExchDiatHyp_", wDExDiH
write (1,*)" Auxiliary  543 _wPExchDiatHyp_", wPExDiH
write (1,*)" Auxiliary  544 _wNExchDiatHyp_", wNExDiH
write (1,*)" Auxiliary  545 _wSiExchDiatHyp_", wSiExDiH
write (1,*)" Auxiliary  546 _wDExchGrenHyp_", wDExGenH
write (1,*)" Auxiliary  547 _wPExchGrenHyp_", wPExGenH
write (1,*)" Auxiliary  548 _wNExchGrenHyp_", wNExGenH
write (1,*)" Auxiliary  549 _wDExchBlueHyp_", wDExBueH
write (1,*)" Auxiliary  550 _wPExchBlueHyp_", wPExBueH
write (1,*)" Auxiliary  551 _wNExchBlueHyp_", wNExBueH
write (1,*)" Auxiliary  552 _wDExchZooHyp_", wDExZooH
write (1,*)" Auxiliary  553 _wPExchZooHyp_", wPExZooH
write (1,*)" Auxiliary  554 _wNExchZooHyp_", wNExZooH
write (1,*)" Auxiliary  555 _tPInfPO4WHyp_", tPIfP4WH
write (1,*)" Auxiliary  556 _tNInfNH4WHyp_", tNIfN4WH
write (1,*)" Auxiliary  557 _tNInfNO3WHyp_", tNIfN3WH
write (1,*)" Auxiliary  558 _tPInfPO4S_", tPInfO4S
write (1,*)" Auxiliary  559 _tNInfNH4S_", tNInfH4S
write (1,*)" Auxiliary  560 _tNInfNO3S_", tNInfO3S
write (1,*)" Auxiliary  561 _tNH4LoadS_", tNH4LadS
write (1,*)" Auxiliary  562 _tNO3LoadS_", tNO3LadS
write (1,*)" Auxiliary  563 _uDErosIM_", uDErosIM
write (1,*)" Auxiliary  564 _uDErosIMS_", uDEroIMS
write (1,*)" Auxiliary  565 _uDErosIMW_", uDEroIMW
write (1,*)" Auxiliary  566 _uDErosOM_", uDErosOM
write (1,*)" Auxiliary  567 _uPErosOM_", uPErosOM
write (1,*)" Auxiliary  568 _uNErosOM_", uNErosOM
write (1,*)" Auxiliary  569 _uO2SatEpi_", uO2SatE
write (1,*)" Auxiliary  570 _uO2SatHyp_", uO2SatH
write (1,*)" Auxiliary  571 _kAer_", kAer
write (1,*)" Auxiliary  572 _uFunTmAerEpi_", uFuTmerE
write (1,*)" Auxiliary  573 _uFunTmAerHyp_", uFuTmerH
write (1,*)" Auxiliary  574 _aFunFloatAer_", aFuLeAer
write (1,*)" Auxiliary  575 _tO2BubHyp_", tO2BubH
write (1,*)" Auxiliary  576 _uFunTmFish_", uFuTmish
write (1,*)" Auxiliary  577 _tDTurbFish_", tDTrbish
write (1,*)" Auxiliary  578 _aFunVegResus_", aFuegsus
write (1,*)" Auxiliary  579 _aFunDimSusp_", aFuDiusp
write (1,*)" Auxiliary  580 _tDResusTauDead_", tDRsTead
write (1,*)" Auxiliary  581 _tDResusBareDead_", tDRsBead
write (1,*)" Auxiliary  582 _tDResusDead_", tDRsuead
write (1,*)" Auxiliary  583 _tDResusIM_", tDRessIM
write (1,*)" Auxiliary  584 _tDResusDet_", tDRessDt
write (1,*)" Auxiliary  585 _akResusPhytRef_", akRsPRef
write (1,*)" Auxiliary  586 _tDResusPhytTot_", tDRsPTot
write (1,*)" Auxiliary  587 _tPResusDet_", tPRessDt
write (1,*)" Auxiliary  588 _tPResusPO4_", tPRsuPO4
write (1,*)" Auxiliary  589 _tPResusAIM_", tPRsuAIM
write (1,*)" Auxiliary  590 _tNResusNO3_", tNRsuNO3
write (1,*)" Auxiliary  591 _tNResusNH4_", tNRsuNH4
write (1,*)" Auxiliary  592 _tNResusDet_", tNRessDt
write (1,*)" Auxiliary  593 _tSiResusDet_", tSiessDt
write (1,*)" Auxiliary  594 _aFunTauSetOMHyp_", aFuauOMH
write (1,*)" Auxiliary  595 _aFunTauSetIMHyp_", aFuauIMH
write (1,*)" Auxiliary  596 _uFunTmSetEpi_", uFuTmetE
write (1,*)" Auxiliary  597 _uFunTmSetHyp_", uFuTmetH
write (1,*)" Auxiliary  598 _uCorVSetIMHyp_", uCoVSIMH
write (1,*)" Auxiliary  599 _tDSetIMHyp_", tDSetIMH
write (1,*)" Auxiliary  600 _tPSetAIMHyp_", tPSetIMH
write (1,*)" Auxiliary  601 _uCorVSetDetHyp_", uCoVSDtH
write (1,*)" Auxiliary  602 _tDSetDetHyp_", tDSetDtH
write (1,*)" Auxiliary  603 _tPSetDetHyp_", tPSetDtH
write (1,*)" Auxiliary  604 _tNSetDetHyp_", tNSetDtH
write (1,*)" Auxiliary  605 _tSiSetDetHyp_", tSiSeDtH
write (1,*)" Auxiliary  606 _kPMinDetW_", kPMinDtW
write (1,*)" Auxiliary  607 _kNMinDetW_", kNMinDtW
write (1,*)" Auxiliary  608 _kSiMinDetW_", kSiMiDtW
write (1,*)" Auxiliary  609 _uFunTmMinWEpi_", uFuTmnWE
write (1,*)" Auxiliary  610 _uFunTmMinWHyp_", uFuTmnWH
write (1,*)" Auxiliary  611 _wDMinDetWHyp_", wDMintWH
write (1,*)" Auxiliary  612 _wPMinDetWHyp_", wPMintWH
write (1,*)" Auxiliary  613 _wNMinDetWHyp_", wNMintWH
write (1,*)" Auxiliary  614 _wSiMinDetWHyp_", wSiintWH
write (1,*)" Auxiliary  615 _aCorO2BODHyp_", aCoO2ODH
write (1,*)" Auxiliary  616 _wO2MinDetWHyp_", wO2intWH
write (1,*)" Auxiliary  617 _wDDenitWHyp_", wDDentWH
write (1,*)" Auxiliary  618 _wNDenitWHyp_", wNDentWH
write (1,*)" Auxiliary  619 _uFunTmNitrHyp_", uFuTmtrH
write (1,*)" Auxiliary  620 _uFunTmNitrEpi_", uFuTmtrE
write (1,*)" Auxiliary  621 _uFunTmNitrS_", uFuTmtrS
write (1,*)" Auxiliary  622 _aCorO2NitrWHyp_", aCo2NrWH
write (1,*)" Auxiliary  623 _wNNitrWHyp_", wNNitrWH
write (1,*)" Auxiliary  624 _wO2NitrWHyp_", wO2NirWH
write (1,*)" Auxiliary  625 _kPMinDetS_", kPMinDtS
write (1,*)" Auxiliary  626 _kNMinDetS_", kNMinDtS
write (1,*)" Auxiliary  627 _kSiMinDetS_", kSiMiDtS
write (1,*)" Auxiliary  628 _uFunTmMinS_", uFuTminS
write (1,*)" Auxiliary  629 _tDMinDetS_", tDMinDtS
write (1,*)" Auxiliary  630 _tPMinDetS_", tPMinDtS
write (1,*)" Auxiliary  631 _tNMinDetS_", tNMinDtS
write (1,*)" Auxiliary  632 _tSiMinDetS_", tSiMiDtS
write (1,*)" Auxiliary  633 _uFunTmDif_", uFunTDif
write (1,*)" Auxiliary  634 _akO2DifCor_", akODiCor
write (1,*)" Auxiliary  635 _tSOD_", tSOD
write (1,*)" Auxiliary  636 _aDepthDif_", aDptDif
write (1,*)" Auxiliary  637 _tPDifPO4Hyp_", tPDifO4H
write (1,*)" Auxiliary  638 _tNDifNO3Hyp_", tNDifO3H
write (1,*)" Auxiliary  639 _tNDifNH4Hyp_", tNDifH4H
write (1,*)" Auxiliary  640 _tO2DifHyp_", tO2DifH
write (1,*)" Auxiliary  641 _tPDifGroundPO4_", tPDroPO4
write (1,*)" Auxiliary  642 _tNDifGroundNO3_", tNDroNO3
write (1,*)" Auxiliary  643 _tNDifGroundNH4_", tNDroNH4
write (1,*)" Auxiliary  644 _aPAdsMaxWHyp_", aPAsMxWH
write (1,*)" Auxiliary  645 _aKPAdsWHyp_", aKPAdsWH
write (1,*)" Auxiliary  646 _aPIsoAdsWHyp_", aPIoAsWH
write (1,*)" Auxiliary  647 _aPEqIMWHyp_", aPEqIMWH
write (1,*)" Auxiliary  648 _wPSorpIMWHyp_", wPSrpMWH
write (1,*)" Auxiliary  649 _tPChemPO4_", tPChePO4
write (1,*)" Auxiliary  650 _tSiAbioTotT_", tSibiotT
write (1,*)" Auxiliary  651 _uQEvPhra_", uQEvPhra
write (1,*)" Auxiliary  652 _tPEvPO4WM_", tPEvP4WM
write (1,*)" Auxiliary  653 _tNEvNH4WM_", tNEvN4WM
write (1,*)" Auxiliary  654 _tNEvNO3WM_", tNEvN3WM
write (1,*)" Auxiliary  655 _tPInfPO4WM_", tPIfP4WM
write (1,*)" Auxiliary  656 _tNInfNH4WM_", tNIfN4WM
write (1,*)" Auxiliary  657 _tNInfNO3WM_", tNIfN3WM
write (1,*)" Auxiliary  658 _tPInfPO4SM_", tPIfP4SM
write (1,*)" Auxiliary  659 _tNInfNH4SM_", tNIfN4SM
write (1,*)" Auxiliary  660 _tNInfNO3SM_", tNIfN3SM
write (1,*)" Auxiliary  661 _tO2AerM_", tO2AerM
write (1,*)" Auxiliary  662 _tDSetIMM_", tDSetIMM
write (1,*)" Auxiliary  663 _tPSetAIMM_", tPSetIMM
write (1,*)" Auxiliary  664 _tDSetDetM_", tDSetDtM
write (1,*)" Auxiliary  665 _tPSetDetM_", tPSetDtM
write (1,*)" Auxiliary  666 _tNSetDetM_", tNSetDtM
write (1,*)" Auxiliary  667 _tSiSetDetM_", tSiSeDtM
write (1,*)" Auxiliary  668 _tDSetDiatM_", tDSetDiM
write (1,*)" Auxiliary  669 _tPSetDiatM_", tPSetDiM
write (1,*)" Auxiliary  670 _tNSetDiatM_", tNSetDiM
write (1,*)" Auxiliary  671 _tSiSetDiatM_", tSiSeDiM
write (1,*)" Auxiliary  672 _tDSetGrenM_", tDStGenM
write (1,*)" Auxiliary  673 _tPSetGrenM_", tPStGenM
write (1,*)" Auxiliary  674 _tNSetGrenM_", tNStGenM
write (1,*)" Auxiliary  675 _tDSetBlueM_", tDStBueM
write (1,*)" Auxiliary  676 _tPSetBlueM_", tPStBueM
write (1,*)" Auxiliary  677 _tNSetBlueM_", tNStBueM
write (1,*)" Auxiliary  678 _tDSetPhytM_", tDStPytM
write (1,*)" Auxiliary  679 _tPSetPhytM_", tPStPytM
write (1,*)" Auxiliary  680 _tNSetPhytM_", tNStPytM
write (1,*)" Auxiliary  681 _tDSetTotM_", tDSetotM
write (1,*)" Auxiliary  682 _wDMinDetWM_", wDMintWM
write (1,*)" Auxiliary  683 _wPMinDetWM_", wPMintWM
write (1,*)" Auxiliary  684 _wNMinDetWM_", wNMintWM
write (1,*)" Auxiliary  685 _wSiMinDetWM_", wSiintWM
write (1,*)" Auxiliary  686 _aCorO2BODM_", aCoO2ODM
write (1,*)" Auxiliary  687 _wO2MinDetWM_", wO2intWM
write (1,*)" Auxiliary  688 _wDDenitWM_", wDDentWM
write (1,*)" Auxiliary  689 _wNDenitWM_", wNDentWM
write (1,*)" Auxiliary  690 _aCorO2NitrWM_", aCo2NrWM
write (1,*)" Auxiliary  691 _wNNitrWM_", wNNitrWM
write (1,*)" Auxiliary  692 _wO2NitrWM_", wO2NirWM
write (1,*)" Auxiliary  693 _tDMinDetSM_", tDMintSM
write (1,*)" Auxiliary  694 _tPMinDetSM_", tPMintSM
write (1,*)" Auxiliary  695 _tNMinDetSM_", tNMintSM
write (1,*)" Auxiliary  696 _tSiMinDetSM_", tSiintSM
write (1,*)" Auxiliary  697 _akO2DifCorM_", akODiorM
write (1,*)" Auxiliary  698 _tSODM_", tSODM
write (1,*)" Auxiliary  699 _aDepthOxySedM_", aDpOxedM
write (1,*)" Auxiliary  700 _afOxySedM_", afOxyedM
write (1,*)" Auxiliary  701 _tDMinOxyDetSM_", tDMOxtSM
write (1,*)" Auxiliary  702 _tO2MinDetSM_", tO2intSM
write (1,*)" Auxiliary  703 _tDDenitSM_", tDDentSM
write (1,*)" Auxiliary  704 _tNDenitSM_", tNDentSM
write (1,*)" Auxiliary  705 _tNNitrSM_", tNNitrSM
write (1,*)" Auxiliary  706 _tO2NitrSM_", tO2NirSM
write (1,*)" Auxiliary  707 _tDMinHumSM_", tDMnHmSM
write (1,*)" Auxiliary  708 _tPMinHumSM_", tPMnHmSM
write (1,*)" Auxiliary  709 _tNMinHumSM_", tNMnHmSM
write (1,*)" Auxiliary  710 _aDepthDifM_", aDptDifM
write (1,*)" Auxiliary  711 _tPDifPO4M_", tPDifO4M
write (1,*)" Auxiliary  712 _tNDifNO3M_", tNDifO3M
write (1,*)" Auxiliary  713 _tNDifNH4M_", tNDifH4M
write (1,*)" Auxiliary  714 _tO2DifM_", tO2DifM
write (1,*)" Auxiliary  715 _tPDifGroundPO4M_", tPDroO4M
write (1,*)" Auxiliary  716 _tNDifGroundNO3M_", tNDroO3M
write (1,*)" Auxiliary  717 _tNDifGroundNH4M_", tNDroH4M
write (1,*)" Auxiliary  718 _aPAdsMaxWM_", aPAsMxWM
write (1,*)" Auxiliary  719 _aKPAdsWM_", aKPAdsWM
write (1,*)" Auxiliary  720 _aPIsoAdsWM_", aPIoAsWM
write (1,*)" Auxiliary  721 _aPEqIMWM_", aPEqIMWM
write (1,*)" Auxiliary  722 _wPSorpIMWM_", wPSrpMWM
write (1,*)" Auxiliary  723 _aPAdsMaxSM_", aPAsMxSM
write (1,*)" Auxiliary  724 _aKPAdsSM_", aKPAdsSM
write (1,*)" Auxiliary  725 _aPIsoAdsSM_", aPIoAsSM
write (1,*)" Auxiliary  726 _aPEqIMSM_", aPEqIMSM
write (1,*)" Auxiliary  727 _tPSorpIMSM_", tPSrpMSM
write (1,*)" Auxiliary  728 _tPChemPO4M_", tPCemO4M
write (1,*)" Auxiliary  729 _aDRootVeg_", aDRtVeg
write (1,*)" Auxiliary  730 _bfSubVegEpi_", bfSubegE
write (1,*)" Auxiliary  731 _bfSubVegHyp_", bfSubegH
write (1,*)" Auxiliary  732 _afCovSurfVeg_", afCSuVeg
write (1,*)" Auxiliary  733 _afCovEmergVeg_", afCEmVeg
write (1,*)" Auxiliary  734 _aCovVeg_", aCovVeg
write (1,*)" Auxiliary  735 _aDVeg_", aDVeg
write (1,*)" Auxiliary  736 _aPVeg_", aPVeg
write (1,*)" Auxiliary  737 _aNVeg_", aNVeg
write (1,*)" Auxiliary  738 _aLPARBotHyp_", aLPARotH
write (1,*)" Auxiliary  739 _rPDVeg_", rPDVeg
write (1,*)" Auxiliary  740 _rNDVeg_", rNDVeg
write (1,*)" Auxiliary  741 _tDMigrVeg_", tDMigVeg
write (1,*)" Auxiliary  742 _tPMigrVeg_", tPMigVeg
write (1,*)" Auxiliary  743 _tNMigrVeg_", tNMigVeg
write (1,*)" Auxiliary  744 _uFunTmRespVeg_", uFumRVeg
write (1,*)" Auxiliary  745 _afPUptVegS_", afPUVegS
write (1,*)" Auxiliary  746 _afNUptVegS_", afNUVegS
write (1,*)" Auxiliary  747 _aVPUptMaxCrVeg_", aVPaxVeg
write (1,*)" Auxiliary  748 _aVPUptVegWHyp_", aVPUVgWH
write (1,*)" Auxiliary  749 _aVPUptVegS_", aVPUVegS
write (1,*)" Auxiliary  750 _aVPUptVegWEpi_", aVPUVgWE
write (1,*)" Auxiliary  751 _tPUptVegWEpi_", tPUVegWE
write (1,*)" Auxiliary  752 _tPUptVegWHyp_", tPUVegWH
write (1,*)" Auxiliary  753 _tPUptVegS_", tPUVegS
write (1,*)" Auxiliary  754 _tPUptVegEpi_", tPUVegE
write (1,*)" Auxiliary  755 _tPUptVegHyp_", tPUVegH
write (1,*)" Auxiliary  756 _aVNUptMaxCrVeg_", aVNaxVeg
write (1,*)" Auxiliary  757 _ahNUptVeg_", ahNUVeg
write (1,*)" Auxiliary  758 _aVNUptVegWEpi_", aVNUVgWE
write (1,*)" Auxiliary  759 _aVNUptVegWHyp_", aVNUVgWH
write (1,*)" Auxiliary  760 _afNH4UptVegWEpi_", afN4UgWE
write (1,*)" Auxiliary  761 _afNH4UptVegWHyp_", afN4UgWH
write (1,*)" Auxiliary  762 _tNUptVegWEpi_", tNUVegWE
write (1,*)" Auxiliary  763 _tNUptVegWHyp_", tNUVegWH
write (1,*)" Auxiliary  764 _tNUptNH4VegWEpi_", tNUH4gWE
write (1,*)" Auxiliary  765 _tNUptNO3VegWEpi_", tNUO3gWE
write (1,*)" Auxiliary  766 _tNUptNH4VegWHyp_", tNUH4gWH
write (1,*)" Auxiliary  767 _tNUptNO3VegWHyp_", tNUO3gWH
write (1,*)" Auxiliary  768 _aVNUptVegS_", aVNUVegS
write (1,*)" Auxiliary  769 _tNUptVegS_", tNUVegS
write (1,*)" Auxiliary  770 _afNH4UptVegS_", afN4UegS
write (1,*)" Auxiliary  771 _tNUptNH4VegS_", tNUH4egS
write (1,*)" Auxiliary  772 _tNUptVegSEpi_", tNUVegSE
write (1,*)" Auxiliary  773 _tNUptNO3VegS_", tNUO3egS
write (1,*)" Auxiliary  774 _tNUptVeg_", tNUVeg
write (1,*)" Auxiliary  775 _aLLimShootVegEpi_", aLLmSegE
write (1,*)" Auxiliary  776 _aLLimShootVegHyp_", aLLmSegH
write (1,*)" Auxiliary  777 _aMuTmLVegHyp_", aMumLegH
write (1,*)" Auxiliary  778 _aPLimVeg_", aPLimVeg
write (1,*)" Auxiliary  779 _aNLimVeg_", aNLimVeg
write (1,*)" Auxiliary  780 _aNutLimVeg_", aNuLiVeg
write (1,*)" Auxiliary  781 _aMuVegHyp_", aMuVegH
write (1,*)" Auxiliary  782 _bkMortVeg_", bkMVeg
write (1,*)" Auxiliary  783 _akDIncrVegHyp_", akDncegH
write (1,*)" Auxiliary  784 _tDEnvVegHyp_", tDEnvegH
write (1,*)" Auxiliary  785 _tDEnvProdVegHyp_", tDEPregH
write (1,*)" Auxiliary  786 _tDProdVegHyp_", tDPodegH
write (1,*)" Auxiliary  787 _tDProdSubVegHyp_", tDPdSegH
write (1,*)" Auxiliary  788 _tDRespVeg_", tDResVeg
write (1,*)" Auxiliary  789 _tDEnvMortVegHyp_", tDEvMegH
write (1,*)" Auxiliary  790 _tDMortVegHyp_", tDMVegH
write (1,*)" Auxiliary  791 _tDMortVegWHyp_", tDMVegWH
write (1,*)" Auxiliary  792 _tDGrazVegBird_", tDGazgBi
write (1,*)" Auxiliary  793 _bkManVeg_", bkManVeg
write (1,*)" Auxiliary  794 _tDManVeg_", tDManVeg
write (1,*)" Auxiliary  795 _tPManVeg_", tPManVeg
write (1,*)" Auxiliary  796 _tNManVeg_", tNManVeg
write (1,*)" Auxiliary  797 _tO2ProdVegHyp_", tO2roegH
write (1,*)" Auxiliary  798 _tO2RespVegWHyp_", tO2spgWH
write (1,*)" Auxiliary  799 _aDepthOxySed_", aDpOxSed
write (1,*)" Auxiliary  800 _afOxySed_", afOxySed
write (1,*)" Auxiliary  801 _tO2RespVegS_", tO2esegS
write (1,*)" Auxiliary  802 _tO2ProdVegSHyp_", tO2odgSH
write (1,*)" Auxiliary  803 _tO2ProdVegWHyp_", tO2odgWH
write (1,*)" Auxiliary  804 _tO2UptNO3VegWHyp_", tO2O3gWH
write (1,*)" Auxiliary  805 _tO2UptNO3VegS_", tO2NOegS
write (1,*)" Auxiliary  806 _tPExcrVeg_", tPExcVeg
write (1,*)" Auxiliary  807 _tPExcrVegS_", tPEcregS
write (1,*)" Auxiliary  808 _tPExcrVegW_", tPEcregW
write (1,*)" Auxiliary  809 _tPMortVegHyp_", tPMVegH
write (1,*)" Auxiliary  810 _tPMortVegPO4Hyp_", tPMegO4H
write (1,*)" Auxiliary  811 _tPMortVegPO4SHyp_", tPMeg4SH
write (1,*)" Auxiliary  812 _tPMortVegPO4WHyp_", tPMeg4WH
write (1,*)" Auxiliary  813 _tPMortVegDetHyp_", tPMVeDtH
write (1,*)" Auxiliary  814 _tPMortVegDetWHyp_", tPMegtWH
write (1,*)" Auxiliary  815 _tPMortVegDetSHyp_", tPMegtSH
write (1,*)" Auxiliary  816 _tPGrazVegBird_", tPGazgBi
write (1,*)" Auxiliary  817 _tNExcrVeg_", tNExcVeg
write (1,*)" Auxiliary  818 _tNExcrVegS_", tNEcregS
write (1,*)" Auxiliary  819 _tNExcrVegW_", tNEcregW
write (1,*)" Auxiliary  820 _tNMortVegHyp_", tNMVegH
write (1,*)" Auxiliary  821 _tNMortVegNH4Hyp_", tNMegH4H
write (1,*)" Auxiliary  822 _tNMortVegNH4SHyp_", tNMeg4SH
write (1,*)" Auxiliary  823 _tNMortVegNH4WHyp_", tNMeg4WH
write (1,*)" Auxiliary  824 _tNMortVegDetHyp_", tNMVeDtH
write (1,*)" Auxiliary  825 _tNMortVegDetWHyp_", tNMegtWH
write (1,*)" Auxiliary  826 _tNMortVegDetSHyp_", tNMegtSH
write (1,*)" Auxiliary  827 _tNGrazVegBird_", tNGazgBi
write (1,*)" Auxiliary  828 _tDAssVegBird_", tDAsVgBi
write (1,*)" Auxiliary  829 _tDEgesBird_", tDEBi
write (1,*)" Auxiliary  830 _tPAssVegBird_", tPAsVgBi
write (1,*)" Auxiliary  831 _tPEgesBird_", tPEBi
write (1,*)" Auxiliary  832 _tPEgesBirdPO4_", tPEBiPO4
write (1,*)" Auxiliary  833 _tPEgesBirdDet_", tPEBiDt
write (1,*)" Auxiliary  834 _tNAssVegBird_", tNAsVgBi
write (1,*)" Auxiliary  835 _tNEgesBird_", tNEBi
write (1,*)" Auxiliary  836 _tNEgesBirdNH4_", tNEBiNH4
write (1,*)" Auxiliary  837 _tNEgesBirdDet_", tNEBiDt
write (1,*)" Auxiliary  838 _wDBedDetWHyp_", wDBedtWH
write (1,*)" Auxiliary  839 _aMuTmLVegEpi_", aMumLegE
write (1,*)" Auxiliary  840 _akDIncrVegEpi_", akDncegE
write (1,*)" Auxiliary  841 _tDEnvVegEpi_", tDEnvegE
write (1,*)" Auxiliary  842 _aMuVegEpi_", aMuVegE
write (1,*)" Auxiliary  843 _tDEnvProdVegEpi_", tDEPregE
write (1,*)" Auxiliary  844 _tDProdVegEpi_", tDPodegE
write (1,*)" Auxiliary  845 _tDEnvMortVegEpi_", tDEvMegE
write (1,*)" Auxiliary  846 _tDMortVegEpi_", tDMVegE
write (1,*)" Auxiliary  847 _tNMortVegEpi_", tNMVegE
write (1,*)" Auxiliary  848 _tNMortVegNH4Epi_", tNMegH4E
write (1,*)" Auxiliary  849 _tNMortVegNH4SEpi_", tNMeg4SE
write (1,*)" Auxiliary  850 _wPBedPO4WHyp_", wPBdP4WH
write (1,*)" Auxiliary  851 _wPBedDetWHyp_", wPBedtWH
write (1,*)" Auxiliary  852 _wNBedNH4WHyp_", wNBdN4WH
write (1,*)" Auxiliary  853 _wNBedNO3WHyp_", wNBdN3WH
write (1,*)" Auxiliary  854 _wNBedDetWHyp_", wNBedtWH
write (1,*)" Auxiliary  855 _tDMortVegSHyp_", tDMVegSH
write (1,*)" Auxiliary  856 _tDBedDetSHyp_", tDBedtSH
write (1,*)" Auxiliary  857 _tO2BedWHyp_", tO2BedWH
write (1,*)" Auxiliary  858 _UseLoss_", UseLoss
write (1,*)" Auxiliary  859 _uFunTmLossHyp_", uFuTmssH
write (1,*)" Auxiliary  860 _uFunTmLossEpi_", uFuTmssE
write (1,*)" Auxiliary  861 _rPDBlueWHyp_", rPDBleWH
write (1,*)" Auxiliary  862 _rNDBlueWHyp_", rNDBleWH
write (1,*)" Auxiliary  863 _rPDBlueS_", rPDBlueS
write (1,*)" Auxiliary  864 _rNDBlueS_", rNDBlueS
write (1,*)" Auxiliary  865 _uFunTmBlueHyp_", uFuTmueH
write (1,*)" Auxiliary  866 _uFunTmProdBlueHyp_", uFuPrueH
write (1,*)" Auxiliary  867 _uFunTmRespBlueHyp_", uFuReueH
write (1,*)" Auxiliary  868 _uFunTmBlueEpi_", uFuTmueE
write (1,*)" Auxiliary  869 _uFunTmProdBlueEpi_", uFuPrueE
write (1,*)" Auxiliary  870 _uFunTmRespBlueEpi_", uFuReueE
write (1,*)" Auxiliary  871 _uFunTmBlueS_", uFuTmueS
write (1,*)" Auxiliary  872 _uFunTmRespBlueS_", uFuReueS
write (1,*)" Auxiliary  873 _aVPUptMaxCrBlueHyp_", aVPxCueH
write (1,*)" Auxiliary  874 _aVPUptBlueHyp_", aVPUBueH
write (1,*)" Auxiliary  875 _wPUptBlueHyp_", wPUBlueH
write (1,*)" Auxiliary  876 _aVNUptMaxCrBlueHyp_", aVNxCueH
write (1,*)" Auxiliary  877 _ahNUptBlueHyp_", ahNUBueH
write (1,*)" Auxiliary  878 _aVNUptBlueHyp_", aVNUBueH
write (1,*)" Auxiliary  879 _wNUptBlueHyp_", wNUBlueH
write (1,*)" Auxiliary  880 _afNH4UptBlueHyp_", afN4UueH
write (1,*)" Auxiliary  881 _wNUptNH4BlueHyp_", wNUH4ueH
write (1,*)" Auxiliary  882 _wNUptNO3BlueHyp_", wNUO3ueH
write (1,*)" Auxiliary  883 _uMuMaxTmBlueHyp_", uMuxTueH
write (1,*)" Auxiliary  884 _uMuMaxTmBlueEpi_", uMuxTueE
write (1,*)" Auxiliary  885 _aPLimBlueHyp_", aPLmBueH
write (1,*)" Auxiliary  886 _aNLimBlueHyp_", aNLmBueH
write (1,*)" Auxiliary  887 _aSiLimBlueHyp_", aSiimueH
write (1,*)" Auxiliary  888 _aLLimBlueHyp_", aLLmBueH
write (1,*)" Auxiliary  889 _aMuTmLBlueHyp_", aMumLueH
write (1,*)" Auxiliary  890 _aNutLimBlueHyp_", aNuimueH
write (1,*)" Auxiliary  891 _aMuBlueHyp_", aMuBlueH
write (1,*)" Auxiliary  892 _wDAssBlueHyp_", wDAsBueH
write (1,*)" Auxiliary  893 _rChDBlueHyp_", rChDBueH
write (1,*)" Auxiliary  894 _oChlaBlueHyp_", oChlBlH
write (1,*)" Auxiliary  895 _aExtChBlueHyp_", aExChueH
write (1,*)" Auxiliary  896 _ukDRespTmBlueHyp_", ukDpTueH
write (1,*)" Auxiliary  897 _ukDRespTmBlueEpi_", ukDpTueE
write (1,*)" Auxiliary  898 _ukDRespTmBlueS_", ukDpTueS
write (1,*)" Auxiliary  899 _wDRespBlueWHyp_", wDRpBeWH
write (1,*)" Auxiliary  900 _ukLossTmBlueHyp_", ukLsTueH
write (1,*)" Auxiliary  901 _ukLossTmBlueEpi_", ukLsTueE
write (1,*)" Auxiliary  902 _wDLossBlueHyp_", wDLssueH
write (1,*)" Auxiliary  903 _wDMortBlueWHyp_", wDMBleWH
write (1,*)" Auxiliary  904 _uCorVSetBlueHyp_", uCoSeueH
write (1,*)" Auxiliary  905 _tDSetBlueHyp_", tDStBueH
write (1,*)" Auxiliary  906 _tDResusBlue_", tDRsulue
write (1,*)" Auxiliary  907 _tDRespBlueS_", tDRspueS
write (1,*)" Auxiliary  908 _tDMortBlueS_", tDMBlueS
write (1,*)" Auxiliary  909 _ukDDecBlueHyp_", ukDecueH
write (1,*)" Auxiliary  910 _wPExcrBlueWHyp_", wPErBeWH
write (1,*)" Auxiliary  911 _wPLossBlueHyp_", wPLssueH
write (1,*)" Auxiliary  912 _wPMortBlueWHyp_", wPMBleWH
write (1,*)" Auxiliary  913 _tPSetBlueHyp_", tPStBueH
write (1,*)" Auxiliary  914 _tPResusBlue_", tPRsulue
write (1,*)" Auxiliary  915 _tPExcrBlueS_", tPEcrueS
write (1,*)" Auxiliary  916 _tPMortBlueS_", tPMBlueS
write (1,*)" Auxiliary  917 _wNExcrBlueWHyp_", wNErBeWH
write (1,*)" Auxiliary  918 _wNLossBlueHyp_", wNLssueH
write (1,*)" Auxiliary  919 _wNMortBlueWHyp_", wNMBleWH
write (1,*)" Auxiliary  920 _tNSetBlueHyp_", tNStBueH
write (1,*)" Auxiliary  921 _tNResusBlue_", tNRsulue
write (1,*)" Auxiliary  922 _tNExcrBlueS_", tNEcrueS
write (1,*)" Auxiliary  923 _tNMortBlueS_", tNMBlueS
write (1,*)" Auxiliary  924 _aFunTauSetOMEpi_", aFuauOME
write (1,*)" Auxiliary  925 _aFunTauSetIMEpi_", aFuauIME
write (1,*)" Auxiliary  926 _uCorVSetBlueEpi_", uCoSeueE
write (1,*)" Auxiliary  927 _tDSetBlueEpi_", tDStBueE
write (1,*)" Auxiliary  928 _rNDBlueWEpi_", rNDBleWE
write (1,*)" Auxiliary  929 _tNSetBlueEpi_", tNStBueE
write (1,*)" Auxiliary  930 _rPDBlueWEpi_", rPDBleWE
write (1,*)" Auxiliary  931 _tPSetBlueEpi_", tPStBueE
write (1,*)" Auxiliary  932 _wDPrimBlueWHyp_", wDPmBeWH
write (1,*)" Auxiliary  933 _wPPrimBlueWHyp_", wPPmBeWH
write (1,*)" Auxiliary  934 _wNPrimBlueWHyp_", wNPmBeWH
write (1,*)" Auxiliary  935 _rPDGrenWHyp_", rPDGrnWH
write (1,*)" Auxiliary  936 _rNDGrenWHyp_", rNDGrnWH
write (1,*)" Auxiliary  937 _rPDGrenS_", rPDGrenS
write (1,*)" Auxiliary  938 _rNDGrenS_", rNDGrenS
write (1,*)" Auxiliary  939 _uFunTmGrenHyp_", uFuTmenH
write (1,*)" Auxiliary  940 _uFunTmGrenEpi_", uFuTmenE
write (1,*)" Auxiliary  941 _uFunTmGrenS_", uFuTmenS
write (1,*)" Auxiliary  942 _uFunTmProdGrenHyp_", uFuPrenH
write (1,*)" Auxiliary  943 _uFunTmRespGrenHyp_", uFuReenH
write (1,*)" Auxiliary  944 _uFunTmProdGrenEpi_", uFuPrenE
write (1,*)" Auxiliary  945 _uFunTmRespGrenEpi_", uFuReenE
write (1,*)" Auxiliary  946 _uFunTmRespGrenS_", uFuReenS
write (1,*)" Auxiliary  947 _aVPUptMaxCrGrenHyp_", aVPxCenH
write (1,*)" Auxiliary  948 _aVPUptGrenHyp_", aVPUGenH
write (1,*)" Auxiliary  949 _wPUptGrenHyp_", wPUGrenH
write (1,*)" Auxiliary  950 _aVNUptMaxCrGrenHyp_", aVNxCenH
write (1,*)" Auxiliary  951 _ahNUptGrenHyp_", ahNUGenH
write (1,*)" Auxiliary  952 _aVNUptGrenHyp_", aVNUGenH
write (1,*)" Auxiliary  953 _wNUptGrenHyp_", wNUGrenH
write (1,*)" Auxiliary  954 _afNH4UptGrenHyp_", afN4UenH
write (1,*)" Auxiliary  955 _wNUptNH4GrenHyp_", wNUH4enH
write (1,*)" Auxiliary  956 _wNUptNO3GrenHyp_", wNUO3enH
write (1,*)" Auxiliary  957 _uMuMaxTmGrenHyp_", uMuxTenH
write (1,*)" Auxiliary  958 _uMuMaxTmGrenEpi_", uMuxTenE
write (1,*)" Auxiliary  959 _aPLimGrenHyp_", aPLmGenH
write (1,*)" Auxiliary  960 _aNLimGrenHyp_", aNLmGenH
write (1,*)" Auxiliary  961 _aSiLimGrenHyp_", aSiimenH
write (1,*)" Auxiliary  962 _aLLimGrenHyp_", aLLmGenH
write (1,*)" Auxiliary  963 _aMuTmLGrenHyp_", aMumLenH
write (1,*)" Auxiliary  964 _aNutLimGrenHyp_", aNuimenH
write (1,*)" Auxiliary  965 _aMuGrenHyp_", aMuGrenH
write (1,*)" Auxiliary  966 _wDAssGrenHyp_", wDAsGenH
write (1,*)" Auxiliary  967 _rChDGrenHyp_", rChDGenH
write (1,*)" Auxiliary  968 _oChlaGrenHyp_", oChlGrH
write (1,*)" Auxiliary  969 _aExtChGrenHyp_", aExChenH
write (1,*)" Auxiliary  970 _ukDRespTmGrenHyp_", ukDpTenH
write (1,*)" Auxiliary  971 _ukDRespTmGrenEpi_", ukDpTenE
write (1,*)" Auxiliary  972 _ukDRespTmGrenS_", ukDpTenS
write (1,*)" Auxiliary  973 _wDRespGrenWHyp_", wDRpGnWH
write (1,*)" Auxiliary  974 _ukLossTmGrenHyp_", ukLsTenH
write (1,*)" Auxiliary  975 _ukLossTmGrenEpi_", ukLsTenE
write (1,*)" Auxiliary  976 _wDLossGrenHyp_", wDLssenH
write (1,*)" Auxiliary  977 _wDMortGrenWHyp_", wDMGrnWH
write (1,*)" Auxiliary  978 _uCorVSetGrenHyp_", uCoSeenH
write (1,*)" Auxiliary  979 _tDSetGrenHyp_", tDStGenH
write (1,*)" Auxiliary  980 _tDResusGren_", tDRsuren
write (1,*)" Auxiliary  981 _tDRespGrenS_", tDRspenS
write (1,*)" Auxiliary  982 _tDMortGrenS_", tDMGrenS
write (1,*)" Auxiliary  983 _ukDDecGrenHyp_", ukDecenH
write (1,*)" Auxiliary  984 _wPExcrGrenWHyp_", wPErGnWH
write (1,*)" Auxiliary  985 _wPLossGrenHyp_", wPLssenH
write (1,*)" Auxiliary  986 _wPMortGrenWHyp_", wPMGrnWH
write (1,*)" Auxiliary  987 _tPSetGrenHyp_", tPStGenH
write (1,*)" Auxiliary  988 _tPResusGren_", tPRsuren
write (1,*)" Auxiliary  989 _tPExcrGrenS_", tPEcrenS
write (1,*)" Auxiliary  990 _tPMortGrenS_", tPMGrenS
write (1,*)" Auxiliary  991 _wNExcrGrenWHyp_", wNErGnWH
write (1,*)" Auxiliary  992 _wNLossGrenHyp_", wNLssenH
write (1,*)" Auxiliary  993 _wNMortGrenWHyp_", wNMGrnWH
write (1,*)" Auxiliary  994 _tNSetGrenHyp_", tNStGenH
write (1,*)" Auxiliary  995 _tNResusGren_", tNRsuren
write (1,*)" Auxiliary  996 _tNExcrGrenS_", tNEcrenS
write (1,*)" Auxiliary  997 _tNMortGrenS_", tNMGrenS
write (1,*)" Auxiliary  998 _uCorVSetGrenEpi_", uCoSeenE
write (1,*)" Auxiliary  999 _tDSetGrenEpi_", tDStGenE
write (1,*)" Auxiliary  1000 _rNDGrenWEpi_", rNDGrnWE
write (1,*)" Auxiliary  1001 _tNSetGrenEpi_", tNStGenE
write (1,*)" Auxiliary  1002 _rPDGrenWEpi_", rPDGrnWE
write (1,*)" Auxiliary  1003 _tPSetGrenEpi_", tPStGenE
write (1,*)" Auxiliary  1004 _wDPrimGrenWHyp_", wDPmGnWH
write (1,*)" Auxiliary  1005 _wPPrimGrenWHyp_", wPPmGnWH
write (1,*)" Auxiliary  1006 _wNPrimGrenWHyp_", wNPmGnWH
write (1,*)" Auxiliary  1007 _rPDDiatWHyp_", rPDDiWH
write (1,*)" Auxiliary  1008 _rNDDiatWHyp_", rNDDiWH
write (1,*)" Auxiliary  1009 _rPDDiatS_", rPDDiS
write (1,*)" Auxiliary  1010 _uFunTmDiatHyp_", uFunTDiH
write (1,*)" Auxiliary  1011 _rNDDiatS_", rNDDiS
write (1,*)" Auxiliary  1012 _uFunTmDiatEpi_", uFunTDiE
write (1,*)" Auxiliary  1013 _uFunTmDiatS_", uFunTDiS
write (1,*)" Auxiliary  1014 _uFunTmProdDiatHyp_", uFumPDiH
write (1,*)" Auxiliary  1015 _uFunTmRespDiatHyp_", uFumRDiH
write (1,*)" Auxiliary  1016 _uFunTmProdDiatEpi_", uFumPDiE
write (1,*)" Auxiliary  1017 _uFunTmRespDiatEpi_", uFumRDiE
write (1,*)" Auxiliary  1018 _uFunTmRespDiatS_", uFumRDiS
write (1,*)" Auxiliary  1019 _aVPUptMaxCrDiatHyp_", aVPaxDiH
write (1,*)" Auxiliary  1020 _aVPUptDiatHyp_", aVPUDiH
write (1,*)" Auxiliary  1021 _wPUptDiatHyp_", wPUDiH
write (1,*)" Auxiliary  1022 _aVNUptMaxCrDiatHyp_", aVNaxDiH
write (1,*)" Auxiliary  1023 _ahNUptDiatHyp_", ahNUDiH
write (1,*)" Auxiliary  1024 _aVNUptDiatHyp_", aVNUDiH
write (1,*)" Auxiliary  1025 _wNUptDiatHyp_", wNUDiH
write (1,*)" Auxiliary  1026 _afNH4UptDiatHyp_", afNH4DiH
write (1,*)" Auxiliary  1027 _wNUptNH4DiatHyp_", wNUNHDiH
write (1,*)" Auxiliary  1028 _wNUptNO3DiatHyp_", wNUNODiH
write (1,*)" Auxiliary  1029 _uMuMaxTmDiatHyp_", uMuaxDiH
write (1,*)" Auxiliary  1030 _uMuMaxTmDiatEpi_", uMuaxDiE
write (1,*)" Auxiliary  1031 _aPLimDiatHyp_", aPLimDiH
write (1,*)" Auxiliary  1032 _aNLimDiatHyp_", aNLimDiH
write (1,*)" Auxiliary  1033 _aSiLimDiatHyp_", aSiLiDiH
write (1,*)" Auxiliary  1034 _aLLimDiatHyp_", aLLimDiH
write (1,*)" Auxiliary  1035 _aMuTmLDiatHyp_", aMuTmDiH
write (1,*)" Auxiliary  1036 _aNutLimDiatHyp_", aNuLiDiH
write (1,*)" Auxiliary  1037 _aMuDiatHyp_", aMuDiH
write (1,*)" Auxiliary  1038 _wDAssDiatHyp_", wDAssDiH
write (1,*)" Auxiliary  1039 _rChDDiatHyp_", rChDDiH
write (1,*)" Auxiliary  1040 _oChlaDiatHyp_", oChlDiH
write (1,*)" Auxiliary  1041 _aExtChDiatHyp_", aExtCDiH
write (1,*)" Auxiliary  1042 _ukDRespTmDiatHyp_", ukDspDiH
write (1,*)" Auxiliary  1043 _ukDRespTmDiatEpi_", ukDspDiE
write (1,*)" Auxiliary  1044 _ukDRespTmDiatS_", ukDspDiS
write (1,*)" Auxiliary  1045 _wDRespDiatWHyp_", wDRspiWH
write (1,*)" Auxiliary  1046 _ukLossTmDiatHyp_", ukLssDiH
write (1,*)" Auxiliary  1047 _ukLossTmDiatEpi_", ukLssDiE
write (1,*)" Auxiliary  1048 _wDLossDiatHyp_", wDLosDiH
write (1,*)" Auxiliary  1049 _wDMortDiatWHyp_", wDMDiWH
write (1,*)" Auxiliary  1050 _uCorVSetDiatHyp_", uCoVSDiH
write (1,*)" Auxiliary  1051 _tDSetDiatHyp_", tDSetDiH
write (1,*)" Auxiliary  1052 _tDResusDiat_", tDRessDi
write (1,*)" Auxiliary  1053 _tDRespDiatS_", tDResDiS
write (1,*)" Auxiliary  1054 _tDMortDiatS_", tDMDiS
write (1,*)" Auxiliary  1055 _ukDDecDiatHyp_", ukDDeDiH
write (1,*)" Auxiliary  1056 _wPExcrDiatWHyp_", wPEcriWH
write (1,*)" Auxiliary  1057 _wPLossDiatHyp_", wPLosDiH
write (1,*)" Auxiliary  1058 _wPMortDiatWHyp_", wPMDiWH
write (1,*)" Auxiliary  1059 _tPSetDiatHyp_", tPSetDiH
write (1,*)" Auxiliary  1060 _tPResusDiat_", tPRessDi
write (1,*)" Auxiliary  1061 _tPExcrDiatS_", tPExcDiS
write (1,*)" Auxiliary  1062 _tPMortDiatS_", tPMDiS
write (1,*)" Auxiliary  1063 _wNExcrDiatWHyp_", wNEcriWH
write (1,*)" Auxiliary  1064 _wNLossDiatHyp_", wNLosDiH
write (1,*)" Auxiliary  1065 _wNMortDiatWHyp_", wNMDiWH
write (1,*)" Auxiliary  1066 _tNSetDiatHyp_", tNSetDiH
write (1,*)" Auxiliary  1067 _tNResusDiat_", tNRessDi
write (1,*)" Auxiliary  1068 _tNExcrDiatS_", tNExcDiS
write (1,*)" Auxiliary  1069 _tNMortDiatS_", tNMDiS
write (1,*)" Auxiliary  1070 _uCorVSetDiatEpi_", uCoVSDiE
write (1,*)" Auxiliary  1071 _tDSetDiatEpi_", tDSetDiE
write (1,*)" Auxiliary  1072 _rPDDiatWEpi_", rPDDiWE
write (1,*)" Auxiliary  1073 _tPSetDiatEpi_", tPSetDiE
write (1,*)" Auxiliary  1074 _rNDDiatWEpi_", rNDDiWE
write (1,*)" Auxiliary  1075 _tNSetDiatEpi_", tNSetDiE
write (1,*)" Auxiliary  1076 _wDPrimDiatWHyp_", wDPimiWH
write (1,*)" Auxiliary  1077 _wPPrimDiatWHyp_", wPPimiWH
write (1,*)" Auxiliary  1078 _wNPrimDiatWHyp_", wNPimiWH
write (1,*)" Auxiliary  1079 _oChlaHyp_", oChlaH
write (1,*)" Auxiliary  1080 _wDAssPhytHyp_", wDAsPytH
write (1,*)" Auxiliary  1081 _wDRespPhytWHyp_", wDRpPtWH
write (1,*)" Auxiliary  1082 _wDMortPhytWHyp_", wDMPhtWH
write (1,*)" Auxiliary  1083 _tDSetPhytHyp_", tDStPytH
write (1,*)" Auxiliary  1084 _wDLossPhytHyp_", wDLssytH
write (1,*)" Auxiliary  1085 _wDPrimPhytWHyp_", wDPmPtWH
write (1,*)" Auxiliary  1086 _wPUptPhytHyp_", wPUPhytH
write (1,*)" Auxiliary  1087 _wPExcrPhytWHyp_", wPErPtWH
write (1,*)" Auxiliary  1088 _wPMortPhytWHyp_", wPMPhtWH
write (1,*)" Auxiliary  1089 _tPSetPhytHyp_", tPStPytH
write (1,*)" Auxiliary  1090 _tPResusPhyt_", tPRsuhyt
write (1,*)" Auxiliary  1091 _wPLossPhytHyp_", wPLssytH
write (1,*)" Auxiliary  1092 _wPPrimPhytWHyp_", wPPmPtWH
write (1,*)" Auxiliary  1093 _wNUptPhytHyp_", wNUPhytH
write (1,*)" Auxiliary  1094 _wNUptNH4PhytHyp_", wNUH4ytH
write (1,*)" Auxiliary  1095 _wNUptNO3PhytHyp_", wNUO3ytH
write (1,*)" Auxiliary  1096 _wNExcrPhytWHyp_", wNErPtWH
write (1,*)" Auxiliary  1097 _wNMortPhytWHyp_", wNMPhtWH
write (1,*)" Auxiliary  1098 _tNSetPhytHyp_", tNStPytH
write (1,*)" Auxiliary  1099 _tNResusPhyt_", tNRsuhyt
write (1,*)" Auxiliary  1100 _wNLossPhytHyp_", wNLssytH
write (1,*)" Auxiliary  1101 _wNPrimPhytWHyp_", wNPmPtWH
write (1,*)" Auxiliary  1102 _tDRespPhytS_", tDRspytS
write (1,*)" Auxiliary  1103 _tDMortPhytS_", tDMPhytS
write (1,*)" Auxiliary  1104 _tPExcrPhytS_", tPEcrytS
write (1,*)" Auxiliary  1105 _tPMortPhytS_", tPMPhytS
write (1,*)" Auxiliary  1106 _tNExcrPhytS_", tNEcrytS
write (1,*)" Auxiliary  1107 _tNMortPhytS_", tNMPhytS
write (1,*)" Auxiliary  1108 _wSiUptDiatHyp_", wSiUDiH
write (1,*)" Auxiliary  1109 _wSiExcrDiatWHyp_", wSixciWH
write (1,*)" Auxiliary  1110 _wSiLossDiatHyp_", wSiosDiH
write (1,*)" Auxiliary  1111 _wSiMortDiatWHyp_", wSiMDiWH
write (1,*)" Auxiliary  1112 _tSiSetDiatHyp_", tSiSeDiH
write (1,*)" Auxiliary  1113 _tSiResusDiat_", tSiessDi
write (1,*)" Auxiliary  1114 _rCyDBlueHyp_", rCyDBueH
write (1,*)" Auxiliary  1115 _oCyanHyp_", oCyanH
write (1,*)" Auxiliary  1116 _fDDiatHyp_", fDDiH
write (1,*)" Auxiliary  1117 _wDPrimDetWHyp_", wDPimtWH
write (1,*)" Auxiliary  1118 _wO2ProdPhytHyp_", wO2odytH
write (1,*)" Auxiliary  1119 _wO2RespPhytWHyp_", wO2sptWH
write (1,*)" Auxiliary  1120 _wO2UptNO3PhytHyp_", wO2O3ytH
write (1,*)" Auxiliary  1121 _wO2PrimWHyp_", wO2PrmWH
write (1,*)" Auxiliary  1122 _wPMortPhytPO4WHyp_", wPMyt4WH
write (1,*)" Auxiliary  1123 _wPMortPhytDetWHyp_", wPMhytWH
write (1,*)" Auxiliary  1124 _wPLossPhytPO4Hyp_", wPLPhO4H
write (1,*)" Auxiliary  1125 _wPLossPhytDetHyp_", wPLsPDtH
write (1,*)" Auxiliary  1126 _wPPrimPO4WHyp_", wPPim4WH
write (1,*)" Auxiliary  1127 _wPPrimDetWHyp_", wPPimtWH
write (1,*)" Auxiliary  1128 _tPMortPhytPO4S_", tPMhyO4S
write (1,*)" Auxiliary  1129 _tPMortPhytDetS_", tPMhyDtS
write (1,*)" Auxiliary  1130 _tPPrimPO4S_", tPPimO4S
write (1,*)" Auxiliary  1131 _tPPrimTotT_", tPPimotT
write (1,*)" Auxiliary  1132 _wNMortPhytNH4WHyp_", wNMyt4WH
write (1,*)" Auxiliary  1133 _wNMortPhytDetWHyp_", wNMhytWH
write (1,*)" Auxiliary  1134 _wNLossPhytNH4Hyp_", wNLPhH4H
write (1,*)" Auxiliary  1135 _wNLossPhytDetHyp_", wNLsPDtH
write (1,*)" Auxiliary  1136 _wNPrimNH4WHyp_", wNPim4WH
write (1,*)" Auxiliary  1137 _wNPrimNO3WHyp_", wNPim3WH
write (1,*)" Auxiliary  1138 _wNPrimDetWHyp_", wNPimtWH
write (1,*)" Auxiliary  1139 _tNMortPhytNH4S_", tNMhyH4S
write (1,*)" Auxiliary  1140 _tNMortPhytDetS_", tNMhyDtS
write (1,*)" Auxiliary  1141 _tNPrimNH4S_", tNPimH4S
write (1,*)" Auxiliary  1142 _tNPrimNO3S_", tNPimO3S
write (1,*)" Auxiliary  1143 _tNPrimTotT_", tNPimotT
write (1,*)" Auxiliary  1144 _tSiExcrDiatS_", tSixcDiS
write (1,*)" Auxiliary  1145 _tSiMortDiatS_", tSiMDiS
write (1,*)" Auxiliary  1146 _wSiPrimSiO2WHyp_", wSiim2WH
write (1,*)" Auxiliary  1147 _wSiPrimDetWHyp_", wSiritWH
write (1,*)" Auxiliary  1148 _tSiPrimTotT_", tSiriotT
write (1,*)" Auxiliary  1149 _aPACoefEpi_", aPACoefE
write (1,*)" Auxiliary  1150 _aPACoefHyp_", aPACoefH
write (1,*)" Auxiliary  1151 _bSecchiMaxEpi_", bSechaxE
write (1,*)" Auxiliary  1152 _aSecchiEpi_", aSecchiE
write (1,*)" Auxiliary  1153 _aTransparencyEpi_", aTrpacyE
write (1,*)" Auxiliary  1154 _bSecchiMaxHyp_", bSechaxH
write (1,*)" Auxiliary  1155 _aSecchiHyp_", aSecchiH
write (1,*)" Auxiliary  1156 _aSecchiT_", aSecchiT
write (1,*)" Auxiliary  1157 _aTransparencyHyp_", aTrpacyH
write (1,*)" Auxiliary  1158 _aTransparencyT_", aTrpacyT
write (1,*)" Auxiliary  1159 _aDepthEuphHyp_", aDptEphH
write (1,*)" Auxiliary  1160 _aRelDepthEuphHyp_", aReptphH
write (1,*)" Auxiliary  1161 _aChlaHHyp_", aChlaHH
write (1,*)" Auxiliary  1162 _aCovPhytWHyp_", aCoPhtWH
write (1,*)" Auxiliary  1163 _rExtChPhytHyp_", rExChytH
write (1,*)" Auxiliary  1164 _uFunTmZooHyp_", uFuTmooH
write (1,*)" Auxiliary  1165 _uFunTmZooEpi_", uFuTmooE
write (1,*)" Auxiliary  1166 _rPDZooHyp_", rPDZooH
write (1,*)" Auxiliary  1167 _rNDZooHyp_", rNDZooH
write (1,*)" Auxiliary  1168 _oDFoodZooHyp_", oDFodooH
write (1,*)" Auxiliary  1169 _aFiltHyp_", aFiltH
write (1,*)" Auxiliary  1170 _ukDAssTmZooHyp_", ukDsTooH
write (1,*)" Auxiliary  1171 _ukDAssTmZooEpi_", ukDsTooE
write (1,*)" Auxiliary  1172 _aDSatZooHyp_", aDSatooH
write (1,*)" Auxiliary  1173 _ukDRespTmZooHyp_", ukDspooH
write (1,*)" Auxiliary  1174 _ukDRespTmZooEpi_", ukDspooE
write (1,*)" Auxiliary  1175 _ukDIncrZooHyp_", ukDncooH
write (1,*)" Auxiliary  1176 _ukDIncrZooEpi_", ukDncooE
write (1,*)" Auxiliary  1177 _wDEnvZooHyp_", wDEnvooH
write (1,*)" Auxiliary  1178 _wDAssZooHyp_", wDAssooH
write (1,*)" Auxiliary  1179 _wDConsZooHyp_", wDCZooH
write (1,*)" Auxiliary  1180 _wDConsDetZooHyp_", wDCDtooH
write (1,*)" Auxiliary  1181 _wDConsDiatZooHyp_", wDCDiooH
write (1,*)" Auxiliary  1182 _wDConsGrenZooHyp_", wDCreooH
write (1,*)" Auxiliary  1183 _wDConsBlueZooHyp_", wDCluooH
write (1,*)" Auxiliary  1184 _wDConsPhytZooHyp_", wDChyooH
write (1,*)" Auxiliary  1185 _wDEgesZooHyp_", wDEZooH
write (1,*)" Auxiliary  1186 _aCorDRespZooHyp_", aCoReooH
write (1,*)" Auxiliary  1187 _wDRespZooHyp_", wDRspooH
write (1,*)" Auxiliary  1188 _wDMortZooHyp_", wDMZooH
write (1,*)" Auxiliary  1189 _oPFoodZooHyp_", oPFodooH
write (1,*)" Auxiliary  1190 _rPDFoodZooHyp_", rPDooooH
write (1,*)" Auxiliary  1191 _wPConsDiatZooHyp_", wPCDiooH
write (1,*)" Auxiliary  1192 _wPConsGrenZooHyp_", wPCreooH
write (1,*)" Auxiliary  1193 _wPConsBlueZooHyp_", wPCluooH
write (1,*)" Auxiliary  1194 _wPConsPhytZooHyp_", wPChyooH
write (1,*)" Auxiliary  1195 _wPConsDetZooHyp_", wPCDtooH
write (1,*)" Auxiliary  1196 _wPConsZooHyp_", wPCZooH
write (1,*)" Auxiliary  1197 _afPAssZooHyp_", afPssooH
write (1,*)" Auxiliary  1198 _wPAssZooHyp_", wPAssooH
write (1,*)" Auxiliary  1199 _wPEgesZooHyp_", wPEZooH
write (1,*)" Auxiliary  1200 _wPEgesZooPO4Hyp_", wPEooO4H
write (1,*)" Auxiliary  1201 _wPEgesZooDetHyp_", wPEZoDtH
write (1,*)" Auxiliary  1202 _akPExcrZooHyp_", akPxcooH
write (1,*)" Auxiliary  1203 _wPExcrZooHyp_", wPEcrooH
write (1,*)" Auxiliary  1204 _wPMortZooHyp_", wPMZooH
write (1,*)" Auxiliary  1205 _wPMortZooPO4Hyp_", wPMooO4H
write (1,*)" Auxiliary  1206 _wPMortZooDetHyp_", wPMZoDtH
write (1,*)" Auxiliary  1207 _oNFoodZooHyp_", oNFodooH
write (1,*)" Auxiliary  1208 _rNDFoodZooHyp_", rNDooooH
write (1,*)" Auxiliary  1209 _wNConsDiatZooHyp_", wNCDiooH
write (1,*)" Auxiliary  1210 _wNConsGrenZooHyp_", wNCreooH
write (1,*)" Auxiliary  1211 _wNConsBlueZooHyp_", wNCluooH
write (1,*)" Auxiliary  1212 _wNConsPhytZooHyp_", wNChyooH
write (1,*)" Auxiliary  1213 _wNConsDetZooHyp_", wNCDtooH
write (1,*)" Auxiliary  1214 _wNConsZooHyp_", wNCZooH
write (1,*)" Auxiliary  1215 _afNAssZooHyp_", afNssooH
write (1,*)" Auxiliary  1216 _wNAssZooHyp_", wNAssooH
write (1,*)" Auxiliary  1217 _wNEgesZooHyp_", wNEZooH
write (1,*)" Auxiliary  1218 _wNEgesZooNH4Hyp_", wNEooH4H
write (1,*)" Auxiliary  1219 _wNEgesZooDetHyp_", wNEZoDtH
write (1,*)" Auxiliary  1220 _kNExcrZooHyp_", kNEcrooH
write (1,*)" Auxiliary  1221 _wNExcrZooHyp_", wNEcrooH
write (1,*)" Auxiliary  1222 _wNMortZooHyp_", wNMZooH
write (1,*)" Auxiliary  1223 _wNMortZooNH4Hyp_", wNMooH4H
write (1,*)" Auxiliary  1224 _wNMortZooDetHyp_", wNMZoDtH
write (1,*)" Auxiliary  1225 _wSiConsDiatZooHyp_", wSiDiooH
write (1,*)" Auxiliary  1226 _uFunTmBent_", uFunTBnt
write (1,*)" Auxiliary  1227 _aDFoodBent_", aDFooBnt
write (1,*)" Auxiliary  1228 _rPDBent_", rPDBnt
write (1,*)" Auxiliary  1229 _rNDBent_", rNDBnt
write (1,*)" Auxiliary  1230 _tDMigrBent_", tDMigBnt
write (1,*)" Auxiliary  1231 _aDSatBent_", aDSatBnt
write (1,*)" Auxiliary  1232 _ukDIncrBent_", ukDncBnt
write (1,*)" Auxiliary  1233 _tDEnvBent_", tDEnvBnt
write (1,*)" Auxiliary  1234 _tDAssBent_", tDAssBnt
write (1,*)" Auxiliary  1235 _aDAssBentSp_", aDAsBtSp
write (1,*)" Auxiliary  1236 _tDConsBent_", tDCBnt
write (1,*)" Auxiliary  1237 _tDConsDetBent_", tDCDtBnt
write (1,*)" Auxiliary  1238 _tDConsDiatBent_", tDCDiBnt
write (1,*)" Auxiliary  1239 _tDConsGrenBent_", tDCreBnt
write (1,*)" Auxiliary  1240 _tDConsBlueBent_", tDCluBnt
write (1,*)" Auxiliary  1241 _tDConsPhytBent_", tDChyBnt
write (1,*)" Auxiliary  1242 _tDEgesBent_", tDEBnt
write (1,*)" Auxiliary  1243 _tDRespBent_", tDResBnt
write (1,*)" Auxiliary  1244 _tDMortBent_", tDMBnt
write (1,*)" Auxiliary  1245 _aPFoodBent_", aPFooBnt
write (1,*)" Auxiliary  1246 _rPDFoodBent_", rPDooBnt
write (1,*)" Auxiliary  1247 _tPConsDetBent_", tPCDtBnt
write (1,*)" Auxiliary  1248 _tPConsDiatBent_", tPCDiBnt
write (1,*)" Auxiliary  1249 _tPConsGrenBent_", tPCreBnt
write (1,*)" Auxiliary  1250 _tPConsBlueBent_", tPCluBnt
write (1,*)" Auxiliary  1251 _tPConsPhytBent_", tPChyBnt
write (1,*)" Auxiliary  1252 _tPConsBent_", tPCBnt
write (1,*)" Auxiliary  1253 _afPAssBent_", afPAsBnt
write (1,*)" Auxiliary  1254 _tPAssBent_", tPAssBnt
write (1,*)" Auxiliary  1255 _tPEgesBent_", tPEBnt
write (1,*)" Auxiliary  1256 _tPEgesBentPO4_", tPEBnPO4
write (1,*)" Auxiliary  1257 _tPEgesBentDet_", tPEBntDt
write (1,*)" Auxiliary  1258 _tPExcrBent_", tPExcBnt
write (1,*)" Auxiliary  1259 _tPMortBent_", tPMBnt
write (1,*)" Auxiliary  1260 _tPMortBentPO4_", tPMBnPO4
write (1,*)" Auxiliary  1261 _tPMortBentDet_", tPMBntDt
write (1,*)" Auxiliary  1262 _tPMigrBent_", tPMigBnt
write (1,*)" Auxiliary  1263 _aNFoodBent_", aNFooBnt
write (1,*)" Auxiliary  1264 _rNDFoodBent_", rNDooBnt
write (1,*)" Auxiliary  1265 _tNMigrBent_", tNMigBnt
write (1,*)" Auxiliary  1266 _tNConsDetBent_", tNCDtBnt
write (1,*)" Auxiliary  1267 _tNConsDiatBent_", tNCDiBnt
write (1,*)" Auxiliary  1268 _tNConsGrenBent_", tNCreBnt
write (1,*)" Auxiliary  1269 _tNConsBlueBent_", tNCluBnt
write (1,*)" Auxiliary  1270 _tNConsPhytBent_", tNChyBnt
write (1,*)" Auxiliary  1271 _tNConsBent_", tNCBnt
write (1,*)" Auxiliary  1272 _afNAssBent_", afNAsBnt
write (1,*)" Auxiliary  1273 _tNAssBent_", tNAssBnt
write (1,*)" Auxiliary  1274 _tNEgesBent_", tNEBnt
write (1,*)" Auxiliary  1275 _tNEgesBentNH4_", tNEBnNH4
write (1,*)" Auxiliary  1276 _tNEgesBentDet_", tNEBntDt
write (1,*)" Auxiliary  1277 _tNExcrBent_", tNExcBnt
write (1,*)" Auxiliary  1278 _tNMortBent_", tNMBnt
write (1,*)" Auxiliary  1279 _tNMortBentNH4_", tNMBnNH4
write (1,*)" Auxiliary  1280 _tNMortBentDet_", tNMBntDt
write (1,*)" Auxiliary  1281 _tSiConsDiatBent_", tSiCDBnt
write (1,*)" Auxiliary  1282 _aFunVegFish_", aFuVeish
write (1,*)" Auxiliary  1283 _ukDIncrFiJv_", ukDnciJv
write (1,*)" Auxiliary  1284 _aDSatFiAd_", aDSatiAd
write (1,*)" Auxiliary  1285 _ukDIncrFiAd_", ukDnciAd
write (1,*)" Auxiliary  1286 _ukHarvFish_", ukHrvish
write (1,*)" Auxiliary  1287 _afPAssFiAd_", afPssiAd
write (1,*)" Auxiliary  1288 _afNAssFiAd_", afNssiAd
write (1,*)" Auxiliary  1289 _uFunTmPisc_", uFuTmisc
write (1,*)" Auxiliary  1290 _aDCarrPisc_", aDCrrisc
write (1,*)" Auxiliary  1291 _aFunVegPisc_", aFuVeisc
write (1,*)" Auxiliary  1292 _akDIncrPisc_", akDncisc
write (1,*)" Auxiliary  1293 _ukHarvPisc_", ukHrvisc
write (1,*)" Auxiliary  1294 _tDWebDetS_", tDWebDtS
write (1,*)" Auxiliary  1295 _tDWebDiatS_", tDWebDiS
write (1,*)" Auxiliary  1296 _tDWebGrenS_", tDWbGenS
write (1,*)" Auxiliary  1297 _tDWebBlueS_", tDWbBueS
write (1,*)" Auxiliary  1298 _tDWebPhytS_", tDWbPytS
write (1,*)" Auxiliary  1299 _tPWebPO4S_", tPWebO4S
write (1,*)" Auxiliary  1300 _tPWebDetS_", tPWebDtS
write (1,*)" Auxiliary  1301 _tPWebDiatS_", tPWebDiS
write (1,*)" Auxiliary  1302 _tPWebGrenS_", tPWbGenS
write (1,*)" Auxiliary  1303 _tPWebBlueS_", tPWbBueS
write (1,*)" Auxiliary  1304 _tPWebPhytS_", tPWbPytS
write (1,*)" Auxiliary  1305 _tNWebNH4S_", tNWebH4S
write (1,*)" Auxiliary  1306 _tNWebNO3S_", tNWebO3S
write (1,*)" Auxiliary  1307 _tNWebDetS_", tNWebDtS
write (1,*)" Auxiliary  1308 _tNWebDiatS_", tNWebDiS
write (1,*)" Auxiliary  1309 _tNWebGrenS_", tNWbGenS
write (1,*)" Auxiliary  1310 _tNWebBlueS_", tNWbBueS
write (1,*)" Auxiliary  1311 _tNWebPhytS_", tNWbPytS
write (1,*)" Auxiliary  1312 _wSiWebSiO2W_", wSiebO2W
write (1,*)" Auxiliary  1313 _wSiWebDetWHyp_", wSiebtWH
write (1,*)" Auxiliary  1314 _tSiWebDetS_", tSiWeDtS
write (1,*)" Auxiliary  1315 _tSiWebTotT_", tSiebotT
write (1,*)" Auxiliary  1316 _aPrefAveHyp_", aPrefveH
write (1,*)" Auxiliary  1317 _wDConsZoo2Hyp_", wDCZoo2H
write (1,*)" Auxiliary  1318 _aDConsZooSpHyp_", aDCZoSpH
write (1,*)" Auxiliary  1319 _aDAssZooSpHyp_", aDAsZSpH
write (1,*)" Auxiliary  1320 _aDGrazSpHyp_", aDGraSpH
write (1,*)" Auxiliary  1321 _aPConsZooSpHyp_", aPCZoSpH
write (1,*)" Auxiliary  1322 _aPGrazSpHyp_", aPGraSpH
write (1,*)" Auxiliary  1323 _aNConsZooSpHyp_", aNCZoSpH
write (1,*)" Auxiliary  1324 _aNGrazSpHyp_", aNGraSpH
write (1,*)" Auxiliary  1325 _afDShootPhra_", afDShhra
write (1,*)" Auxiliary  1326 _rDSRPhra_", rDSRPhra
write (1,*)" Auxiliary  1327 _rPDShootPhra_", rPDShhra
write (1,*)" Auxiliary  1328 _rNDShootPhra_", rNDShhra
write (1,*)" Auxiliary  1329 _rPDRootPhra_", rPDRthra
write (1,*)" Auxiliary  1330 _rNDRootPhra_", rNDRthra
write (1,*)" Auxiliary  1331 _aLengShootPhra_", aLegShra
write (1,*)" Auxiliary  1332 _bDayInitPhra_", bDanihra
write (1,*)" Auxiliary  1333 _aDAllPhra_", aDAllhra
write (1,*)" Auxiliary  1334 _tDAllPhra_", tDAllhra
write (1,*)" Auxiliary  1335 _tNTransPhra_", tNTanhra
write (1,*)" Auxiliary  1336 _tPTransPhra_", tPTanhra
write (1,*)" Auxiliary  1337 _aVNUptPhraMaxCr_", aVNhrxCr
write (1,*)" Auxiliary  1338 _ahNUptPhraS_", ahNUPraS
write (1,*)" Auxiliary  1339 _aVNUptPhraS_", aVNUPraS
write (1,*)" Auxiliary  1340 _tNUptPhraS_", tNUPhraS
write (1,*)" Auxiliary  1341 _tNUptNH4PhraS_", tNUH4raS
write (1,*)" Auxiliary  1342 _tNUptNO3PhraS_", tNUO3raS
write (1,*)" Auxiliary  1343 _tNUptShootPhra_", tNUShhra
write (1,*)" Auxiliary  1344 _tNUptRootPhra_", tNURthra
write (1,*)" Auxiliary  1345 _aVPUptPhraMaxCr_", aVPhrxCr
write (1,*)" Auxiliary  1346 _ahPUptPhraS_", ahPUPraS
write (1,*)" Auxiliary  1347 _aVPUptPhraS_", aVPUPraS
write (1,*)" Auxiliary  1348 _tPUptPhraS_", tPUPhraS
write (1,*)" Auxiliary  1349 _tPUptShootPhra_", tPUShhra
write (1,*)" Auxiliary  1350 _tPUptRootPhra_", tPURthra
write (1,*)" Auxiliary  1351 _uFunTmProdPhra_", uFuPrhra
write (1,*)" Auxiliary  1352 _ukDRespTmPhra_", ukDsphra
write (1,*)" Auxiliary  1353 _aMuPhotPhra_", aMuhohra
write (1,*)" Auxiliary  1354 _aNLimProdPhra_", aNLPrhra
write (1,*)" Auxiliary  1355 _aPLimProdPhra_", aPLPrhra
write (1,*)" Auxiliary  1356 _aNutLimPhra_", aNuLihra
write (1,*)" Auxiliary  1357 _aMuPhra_", aMuPhra
write (1,*)" Auxiliary  1358 _akDIncrPhra_", akDnchra
write (1,*)" Auxiliary  1359 _tDDensPhra_", tDDnshra
write (1,*)" Auxiliary  1360 _tDDensProdPhra_", tDDPrhra
write (1,*)" Auxiliary  1361 _tDProdPhra_", tDPodhra
write (1,*)" Auxiliary  1362 _tDProdShootPhra_", tDPdShra
write (1,*)" Auxiliary  1363 _tDProdRootPhra_", tDPdRhra
write (1,*)" Auxiliary  1364 _tDRespShootPhra_", tDRpShra
write (1,*)" Auxiliary  1365 _tDRespRootPhra_", tDRpRhra
write (1,*)" Auxiliary  1366 _tO2RespRootPhra_", tO2sphra
write (1,*)" Auxiliary  1367 _tO2FlowPhra_", tO2lohra
write (1,*)" Auxiliary  1368 _bDayRealPhra_", bDaeahra
write (1,*)" Auxiliary  1369 _aDRealPhra_", aDRalhra
write (1,*)" Auxiliary  1370 _tDRealPhra_", tDRalhra
write (1,*)" Auxiliary  1371 _tNRetrPhra_", tNRtrhra
write (1,*)" Auxiliary  1372 _tPRetrPhra_", tPRtrhra
write (1,*)" Auxiliary  1373 _tDMortShootPhra_", tDMShhra
write (1,*)" Auxiliary  1374 _tNMortShootPhra_", tNMShhra
write (1,*)" Auxiliary  1375 _tPMortShootPhra_", tPMShhra
write (1,*)" Auxiliary  1376 _tDMortRootPhra_", tDMRthra
write (1,*)" Auxiliary  1377 _tNMortRootPhra_", tNMRthra
write (1,*)" Auxiliary  1378 _tPMortRootPhra_", tPMRthra
write (1,*)" Auxiliary  1379 _tDManShootPhra_", tDMnShra
write (1,*)" Auxiliary  1380 _tNManShootPhra_", tNMnShra
write (1,*)" Auxiliary  1381 _tPManShootPhra_", tPMnShra
write (1,*)" Auxiliary  1382 _tDIMSM_", tDIMSM
write (1,*)" Auxiliary  1383 _tDHumSM_", tDHumSM
write (1,*)" Auxiliary  1384 _tDDetSM_", tDDtSM
write (1,*)" Auxiliary  1385 _vDeltaSM_", vDeltaSM
write (1,*)" Auxiliary  1386 _tDBurIMM_", tDBurIMM
write (1,*)" Auxiliary  1387 _tDBurOMM_", tDBurOMM
write (1,*)" Auxiliary  1388 _tDBurDetM_", tDBurDtM
write (1,*)" Auxiliary  1389 _tDBurHumM_", tDBurumM
write (1,*)" Auxiliary  1390 _tDBurTotM_", tDBurotM
write (1,*)" Auxiliary  1391 _tPBurHumM_", tPBurumM
write (1,*)" Auxiliary  1392 _tPBurDetM_", tPBurDtM
write (1,*)" Auxiliary  1393 _tPBurAIMM_", tPBurIMM
write (1,*)" Auxiliary  1394 _tPBurPO4M_", tPBurO4M
write (1,*)" Auxiliary  1395 _tPBurTotM_", tPBurotM
write (1,*)" Auxiliary  1396 _tNBurHumM_", tNBurumM
write (1,*)" Auxiliary  1397 _tNBurDetM_", tNBurDtM
write (1,*)" Auxiliary  1398 _tNBurNH4M_", tNBurH4M
write (1,*)" Auxiliary  1399 _tNBurNO3M_", tNBurO3M
write (1,*)" Auxiliary  1400 _tNBurTotM_", tNBurotM
write (1,*)" Auxiliary  1401 _tSiBurDetM_", tSiBuDtM
write (1,*)" Auxiliary  1402 _tSiBurTotM_", tSiurotM
write (1,*)" Auxiliary  1403 _vDeltaWM_", vDeltaWM
write (1,*)" Auxiliary  1404 _aRelDeltaWM_", aReDeaWM
write (1,*)" Auxiliary  1405 _tDSetTotHyp_", tDSetotH
write (1,*)" Auxiliary  1406 _tPSetTotHyp_", tPSetotH
write (1,*)" Auxiliary  1407 _tNSetTotHyp_", tNSetotH
write (1,*)" Auxiliary  1408 _bTimeDred_", bTimered
write (1,*)" Auxiliary  1409 _bRhoSolidSoil_", bRholoil
write (1,*)" Auxiliary  1410 _tDMarsTotT_", tDMrsotT
write (1,*)" Auxiliary  1411 _tPMarsTotT_", tPMrsotT
write (1,*)" Auxiliary  1412 _tNMarsTotT_", tNMrsotT
write (1,*)" Auxiliary  1413 _tSiMarsTotT_", tSiarotT
write (1,*)" Auxiliary  1414 _uTauSubstEpi_", uTaSustE
write (1,*)" Auxiliary  1415 _akExchMEpi_", akExME
write (1,*)" Auxiliary  1416 _afVolMarshEpi_", afVlMshE
write (1,*)" Auxiliary  1417 _akExchLEpi_", akExLE
write (1,*)" Auxiliary  1418 _oPPhytWEpi_", oPPhytWE
write (1,*)" Auxiliary  1419 _oNPhytWEpi_", oNPhytWE
write (1,*)" Auxiliary  1420 _oDSestWEpi_", oDSestWE
write (1,*)" Auxiliary  1421 _oPOMWEpi_", oPOMWE
write (1,*)" Auxiliary  1422 _oPSestWEpi_", oPSestWE
write (1,*)" Auxiliary  1423 _oPInorgWEpi_", oPInogWE
write (1,*)" Auxiliary  1424 _oPTotWEpi_", oPTotWE
write (1,*)" Auxiliary  1425 _oNOMWEpi_", oNOMWE
write (1,*)" Auxiliary  1426 _oNSestWEpi_", oNSestWE
write (1,*)" Auxiliary  1427 _oNkjWEpi_", oNkjWE
write (1,*)" Auxiliary  1428 _oNTotWEpi_", oNTotWE
write (1,*)" Auxiliary  1429 _rPDIMWEpi_", rPDIMWE
write (1,*)" Auxiliary  1430 _rPDDetWEpi_", rPDDtWE
write (1,*)" Auxiliary  1431 _rNDDetWEpi_", rNDDtWE
write (1,*)" Auxiliary  1432 _rSiDDetWEpi_", rSiDDtWE
write (1,*)" Auxiliary  1433 _wDDilIMEpi_", wDDilIME
write (1,*)" Auxiliary  1434 _wDDilDetEpi_", wDDilDtE
write (1,*)" Auxiliary  1435 _wDDilDiatEpi_", wDDilDiE
write (1,*)" Auxiliary  1436 _wDDilGrenEpi_", wDDlGenE
write (1,*)" Auxiliary  1437 _wDDilBlueEpi_", wDDlBueE
write (1,*)" Auxiliary  1438 _wDDilPhytEpi_", wDDlPytE
write (1,*)" Auxiliary  1439 _wDDilZooEpi_", wDDilooE
write (1,*)" Auxiliary  1440 _wPDilZooEpi_", wPDilooE
write (1,*)" Auxiliary  1441 _wNDilZooEpi_", wNDilooE
write (1,*)" Auxiliary  1442 _wPDilPO4Epi_", wPDilO4E
write (1,*)" Auxiliary  1443 _wPDilDetEpi_", wPDilDtE
write (1,*)" Auxiliary  1444 _wPDilAIMEpi_", wPDilIME
write (1,*)" Auxiliary  1445 _wNDilNH4Epi_", wNDilH4E
write (1,*)" Auxiliary  1446 _wNDilNO3Epi_", wNDilO3E
write (1,*)" Auxiliary  1447 _wNDilDetEpi_", wNDilDtE
write (1,*)" Auxiliary  1448 _wO2InflowEpi_", wO2nfowE
write (1,*)" Auxiliary  1449 _wO2OutflEpi_", wO2OuflE
write (1,*)" Auxiliary  1450 _wPDilDiatEpi_", wPDilDiE
write (1,*)" Auxiliary  1451 _wNDilDiatEpi_", wNDilDiE
write (1,*)" Auxiliary  1452 _wPDilGrenEpi_", wPDlGenE
write (1,*)" Auxiliary  1453 _wNDilGrenEpi_", wNDlGenE
write (1,*)" Auxiliary  1454 _wPDilBlueEpi_", wPDlBueE
write (1,*)" Auxiliary  1455 _wNDilBlueEpi_", wNDlBueE
write (1,*)" Auxiliary  1456 _wPDilPhytEpi_", wPDlPytE
write (1,*)" Auxiliary  1457 _wNDilPhytEpi_", wNDlPytE
write (1,*)" Auxiliary  1458 _wDOutflTotEpi_", wDOtfotE
write (1,*)" Auxiliary  1459 _wPOutflTotEpi_", wPOtfotE
write (1,*)" Auxiliary  1460 _wNOutflTotEpi_", wNOtfotE
write (1,*)" Auxiliary  1461 _wDTranDiatEpi_", wDTraDiE
write (1,*)" Auxiliary  1462 _wPTranDiatEpi_", wPTraDiE
write (1,*)" Auxiliary  1463 _wNTranDiatEpi_", wNTraDiE
write (1,*)" Auxiliary  1464 _wDTranGrenEpi_", wDTanenE
write (1,*)" Auxiliary  1465 _wPTranGrenEpi_", wPTanenE
write (1,*)" Auxiliary  1466 _wNTranGrenEpi_", wNTanenE
write (1,*)" Auxiliary  1467 _wDTranBlueEpi_", wDTanueE
write (1,*)" Auxiliary  1468 _wPTranBlueEpi_", wPTanueE
write (1,*)" Auxiliary  1469 _wNTranBlueEpi_", wNTanueE
write (1,*)" Auxiliary  1470 _wDTranPhytEpi_", wDTanytE
write (1,*)" Auxiliary  1471 _wPTranPhytEpi_", wPTanytE
write (1,*)" Auxiliary  1472 _wNTranPhytEpi_", wNTanytE
write (1,*)" Auxiliary  1473 _wSiDilSiO2Epi_", wSiilO2E
write (1,*)" Auxiliary  1474 _wSiDilDetEpi_", wSiDiDtE
write (1,*)" Auxiliary  1475 _wSiDilDiatEpi_", wSiDiDiE
write (1,*)" Auxiliary  1476 _wSiOutflTotEpi_", wSitfotE
write (1,*)" Auxiliary  1477 _wSiTranSiO2Epi_", wSianO2E
write (1,*)" Auxiliary  1478 _wSiTranDetWEpi_", wSiratWE
write (1,*)" Auxiliary  1479 _tSiTranTotTEpi_", tSiantTE
write (1,*)" Auxiliary  1480 _wDTranZooEpi_", wDTanooE
write (1,*)" Auxiliary  1481 _wPTranZooEpi_", wPTanooE
write (1,*)" Auxiliary  1482 _wNTranZooEpi_", wNTanooE
write (1,*)" Auxiliary  1483 _wDTranIMWEpi_", wDTanMWE
write (1,*)" Auxiliary  1484 _wDTranDetWEpi_", wDTantWE
write (1,*)" Auxiliary  1485 _wO2TranWEpi_", wO2TrnWE
write (1,*)" Auxiliary  1486 _wPTranPO4WEpi_", wPTan4WE
write (1,*)" Auxiliary  1487 _wPTranAIMWEpi_", wPTanMWE
write (1,*)" Auxiliary  1488 _wPTranDetWEpi_", wPTantWE
write (1,*)" Auxiliary  1489 _wNTranNH4WEpi_", wNTan4WE
write (1,*)" Auxiliary  1490 _wNTranNO3WEpi_", wNTan3WE
write (1,*)" Auxiliary  1491 _wNTranDetWEpi_", wNTantWE
write (1,*)" Auxiliary  1492 _wDDilTotEpi_", wDDilotE
write (1,*)" Auxiliary  1493 _wPDilTotEpi_", wPDilotE
write (1,*)" Auxiliary  1494 _wNDilTotEpi_", wNDilotE
write (1,*)" Auxiliary  1495 _wSiDilTotEpi_", wSiilotE
write (1,*)" Auxiliary  1496 _tDTranTotTEpi_", tDTantTE
write (1,*)" Auxiliary  1497 _tPTranTotTEpi_", tPTantTE
write (1,*)" Auxiliary  1498 _tNTranTotTEpi_", tNTantTE
write (1,*)" Auxiliary  1499 _wDExchIMMEpi_", wDExIMME
write (1,*)" Auxiliary  1500 _wPExchPO4MEpi_", wPExP4ME
write (1,*)" Auxiliary  1501 _wPExchAIMMEpi_", wPExAMME
write (1,*)" Auxiliary  1502 _wNExchNH4MEpi_", wNExN4ME
write (1,*)" Auxiliary  1503 _wNExchNO3MEpi_", wNExN3ME
write (1,*)" Auxiliary  1504 _wSiExchSiO2MEpi_", wSixS2ME
write (1,*)" Auxiliary  1505 _wO2ExchMEpi_", wO2ExME
write (1,*)" Auxiliary  1506 _wDExchDetMEpi_", wDExDtME
write (1,*)" Auxiliary  1507 _wPExchDetMEpi_", wPExDtME
write (1,*)" Auxiliary  1508 _wNExchDetMEpi_", wNExDtME
write (1,*)" Auxiliary  1509 _wSiExchDetMEpi_", wSiExtME
write (1,*)" Auxiliary  1510 _wDExchDiatMEpi_", wDExDiME
write (1,*)" Auxiliary  1511 _wPExchDiatMEpi_", wPExDiME
write (1,*)" Auxiliary  1512 _wNExchDiatMEpi_", wNExDiME
write (1,*)" Auxiliary  1513 _wSiExchDiatMEpi_", wSiExiME
write (1,*)" Auxiliary  1514 _wDExchGrenMEpi_", wDEGrnME
write (1,*)" Auxiliary  1515 _wPExchGrenMEpi_", wPEGrnME
write (1,*)" Auxiliary  1516 _wNExchGrenMEpi_", wNEGrnME
write (1,*)" Auxiliary  1517 _wDExchBlueMEpi_", wDEBleME
write (1,*)" Auxiliary  1518 _wPExchBlueMEpi_", wPEBleME
write (1,*)" Auxiliary  1519 _wNExchBlueMEpi_", wNEBleME
write (1,*)" Auxiliary  1520 _wDExchZooMEpi_", wDExZoME
write (1,*)" Auxiliary  1521 _wPExchZooMEpi_", wPExZoME
write (1,*)" Auxiliary  1522 _wNExchZooMEpi_", wNExZoME
write (1,*)" Auxiliary  1523 _wDExchIMEpi_", wDExIME
write (1,*)" Auxiliary  1524 _wPExchPO4Epi_", wPExPO4E
write (1,*)" Auxiliary  1525 _wPExchAIMEpi_", wPExAIME
write (1,*)" Auxiliary  1526 _wNExchNH4Epi_", wNExNH4E
write (1,*)" Auxiliary  1527 _wNExchNO3Epi_", wNExNO3E
write (1,*)" Auxiliary  1528 _wSiExchSiO2Epi_", wSixSO2E
write (1,*)" Auxiliary  1529 _wO2ExchEpi_", wO2ExE
write (1,*)" Auxiliary  1530 _wDExchDetEpi_", wDExDtE
write (1,*)" Auxiliary  1531 _wPExchDetEpi_", wPExDtE
write (1,*)" Auxiliary  1532 _wNExchDetEpi_", wNExDtE
write (1,*)" Auxiliary  1533 _wSiExchDetEpi_", wSiExDtE
write (1,*)" Auxiliary  1534 _wDExchDiatEpi_", wDExDiE
write (1,*)" Auxiliary  1535 _wPExchDiatEpi_", wPExDiE
write (1,*)" Auxiliary  1536 _wNExchDiatEpi_", wNExDiE
write (1,*)" Auxiliary  1537 _wSiExchDiatEpi_", wSiExDiE
write (1,*)" Auxiliary  1538 _wDExchGrenEpi_", wDExGenE
write (1,*)" Auxiliary  1539 _wPExchGrenEpi_", wPExGenE
write (1,*)" Auxiliary  1540 _wNExchGrenEpi_", wNExGenE
write (1,*)" Auxiliary  1541 _wDExchBlueEpi_", wDExBueE
write (1,*)" Auxiliary  1542 _wPExchBlueEpi_", wPExBueE
write (1,*)" Auxiliary  1543 _wNExchBlueEpi_", wNExBueE
write (1,*)" Auxiliary  1544 _wDExchZooEpi_", wDExZooE
write (1,*)" Auxiliary  1545 _wPExchZooEpi_", wPExZooE
write (1,*)" Auxiliary  1546 _wNExchZooEpi_", wNExZooE
write (1,*)" Auxiliary  1547 _tPInfPO4WEpi_", tPIfP4WE
write (1,*)" Auxiliary  1548 _tNInfNH4WEpi_", tNIfN4WE
write (1,*)" Auxiliary  1549 _tNInfNO3WEpi_", tNIfN3WE
write (1,*)" Auxiliary  1550 _tO2AerEpi_", tO2AerE
write (1,*)" Auxiliary  1551 _tDTurbFishIM_", tDTbFhIM
write (1,*)" Auxiliary  1552 _uCorVSetIMEpi_", uCoVSIME
write (1,*)" Auxiliary  1553 _tDSetIMEpi_", tDSetIME
write (1,*)" Auxiliary  1554 _tPSetAIMEpi_", tPSetIME
write (1,*)" Auxiliary  1555 _uCorVSetDetEpi_", uCoVSDtE
write (1,*)" Auxiliary  1556 _tDSetDetEpi_", tDSetDtE
write (1,*)" Auxiliary  1557 _tPSetDetEpi_", tPSetDtE
write (1,*)" Auxiliary  1558 _tNSetDetEpi_", tNSetDtE
write (1,*)" Auxiliary  1559 _tSiSetDetEpi_", tSiSeDtE
write (1,*)" Auxiliary  1560 _wDMinDetWEpi_", wDMintWE
write (1,*)" Auxiliary  1561 _wPMinDetWEpi_", wPMintWE
write (1,*)" Auxiliary  1562 _wNMinDetWEpi_", wNMintWE
write (1,*)" Auxiliary  1563 _wSiMinDetWEpi_", wSiintWE
write (1,*)" Auxiliary  1564 _aCorO2BODEpi_", aCoO2ODE
write (1,*)" Auxiliary  1565 _wO2MinDetWEpi_", wO2intWE
write (1,*)" Auxiliary  1566 _wDDenitWEpi_", wDDentWE
write (1,*)" Auxiliary  1567 _wNDenitWEpi_", wNDentWE
write (1,*)" Auxiliary  1568 _aCorO2NitrWEpi_", aCo2NrWE
write (1,*)" Auxiliary  1569 _wNNitrWEpi_", wNNitrWE
write (1,*)" Auxiliary  1570 _wO2NitrWEpi_", wO2NirWE
write (1,*)" Auxiliary  1571 _tDMinOxyDetS_", tDMnODtS
write (1,*)" Auxiliary  1572 _tO2MinDetS_", tO2MiDtS
write (1,*)" Auxiliary  1573 _tDDenitS_", tDDenitS
write (1,*)" Auxiliary  1574 _tNDenitS_", tNDenitS
write (1,*)" Auxiliary  1575 _tNNitrS_", tNNitrS
write (1,*)" Auxiliary  1576 _tO2NitrS_", tO2NitrS
write (1,*)" Auxiliary  1577 _tDMinHumS_", tDMinumS
write (1,*)" Auxiliary  1578 _tPMinHumS_", tPMinumS
write (1,*)" Auxiliary  1579 _tNMinHumS_", tNMinumS
write (1,*)" Auxiliary  1580 _tPDifPO4Epi_", tPDifO4E
write (1,*)" Auxiliary  1581 _tNDifNO3Epi_", tNDifO3E
write (1,*)" Auxiliary  1582 _tNDifNH4Epi_", tNDifH4E
write (1,*)" Auxiliary  1583 _tO2DifEpi_", tO2DifE
write (1,*)" Auxiliary  1584 _aPAdsMaxWEpi_", aPAsMxWE
write (1,*)" Auxiliary  1585 _aKPAdsWEpi_", aKPAdsWE
write (1,*)" Auxiliary  1586 _aPIsoAdsWEpi_", aPIoAsWE
write (1,*)" Auxiliary  1587 _aPEqIMWEpi_", aPEqIMWE
write (1,*)" Auxiliary  1588 _wPSorpIMWEpi_", wPSrpMWE
write (1,*)" Auxiliary  1589 _aPAdsMaxS_", aPAdsaxS
write (1,*)" Auxiliary  1590 _aKPAdsS_", aKPAdsS
write (1,*)" Auxiliary  1591 _aPIsoAdsS_", aPIsodsS
write (1,*)" Auxiliary  1592 _aPEqIMS_", aPEqIMS
write (1,*)" Auxiliary  1593 _tPSorpIMS_", tPSorIMS
write (1,*)" Auxiliary  1594 _wO2AbioWHyp_", wO2AboWH
write (1,*)" Auxiliary  1595 _wPAbioDetWHyp_", wPAiotWH
write (1,*)" Auxiliary  1596 _wPAbioPO4WHyp_", wPAio4WH
write (1,*)" Auxiliary  1597 _wPAbioAIMWHyp_", wPAioMWH
write (1,*)" Auxiliary  1598 _tPAbioTotT_", tPAiootT
write (1,*)" Auxiliary  1599 _wNAbioNH4WHyp_", wNAio4WH
write (1,*)" Auxiliary  1600 _wNAbioNO3WHyp_", wNAio3WH
write (1,*)" Auxiliary  1601 _wNAbioDetWHyp_", wNAiotWH
write (1,*)" Auxiliary  1602 _wSiAbioSiO2WHyp_", wSiio2WH
write (1,*)" Auxiliary  1603 _wSiAbioDetWHyp_", wSibitWH
write (1,*)" Auxiliary  1604 _wDAbioDetWHyp_", wDAiotWH
write (1,*)" Auxiliary  1605 _wDAbioIMWHyp_", wDAioMWH
write (1,*)" Auxiliary  1606 _wDAbioIMWEpi_", wDAioMWE
write (1,*)" Auxiliary  1607 _wDAbioDetWEpi_", wDAiotWE
write (1,*)" Auxiliary  1608 _tDAbioIMS_", tDAbiIMS
write (1,*)" Auxiliary  1609 _tDAbioDetS_", tDAbiDtS
write (1,*)" Auxiliary  1610 _tDAbioHumS_", tDAioumS
write (1,*)" Auxiliary  1611 _tDAbioTotT_", tDAiootT
write (1,*)" Auxiliary  1612 _wO2AbioWEpi_", wO2AboWE
write (1,*)" Auxiliary  1613 _wO2AbioM_", wO2AbioM
write (1,*)" Auxiliary  1614 _wPAbioDetWEpi_", wPAiotWE
write (1,*)" Auxiliary  1615 _wPAbioPO4WEpi_", wPAio4WE
write (1,*)" Auxiliary  1616 _wPAbioAIMWEpi_", wPAioMWE
write (1,*)" Auxiliary  1617 _tPAbioDetS_", tPAbiDtS
write (1,*)" Auxiliary  1618 _tPAbioHumS_", tPAioumS
write (1,*)" Auxiliary  1619 _tPAbioPO4S_", tPAioO4S
write (1,*)" Auxiliary  1620 _tPAbioAIMS_", tPAioIMS
write (1,*)" Auxiliary  1621 _wNAbioNH4WEpi_", wNAio4WE
write (1,*)" Auxiliary  1622 _wNAbioNO3WEpi_", wNAio3WE
write (1,*)" Auxiliary  1623 _wNAbioDetWEpi_", wNAiotWE
write (1,*)" Auxiliary  1624 _tNAbioNH4S_", tNAioH4S
write (1,*)" Auxiliary  1625 _tNAbioNO3S_", tNAioO3S
write (1,*)" Auxiliary  1626 _tNAbioDetS_", tNAbiDtS
write (1,*)" Auxiliary  1627 _tNAbioHumS_", tNAioumS
write (1,*)" Auxiliary  1628 _tNAbioTotT_", tNAiootT
write (1,*)" Auxiliary  1629 _wSiAbioSiO2WEpi_", wSiio2WE
write (1,*)" Auxiliary  1630 _wSiAbioDetWEpi_", wSibitWE
write (1,*)" Auxiliary  1631 _tSiAbioDetS_", tSibiDtS
write (1,*)" Auxiliary  1632 _tDProdSubVegEpi_", tDPdSegE
write (1,*)" Auxiliary  1633 _tDMortVegWEpi_", tDMVegWE
write (1,*)" Auxiliary  1634 _tDMortVegSEpi_", tDMVegSE
write (1,*)" Auxiliary  1635 _tDBedVeg_", tDBedVeg
write (1,*)" Auxiliary  1636 _tO2ProdVegEpi_", tO2roegE
write (1,*)" Auxiliary  1637 _tO2RespVegWEpi_", tO2spgWE
write (1,*)" Auxiliary  1638 _tO2ProdVegSEpi_", tO2odgSE
write (1,*)" Auxiliary  1639 _tO2ProdVegWEpi_", tO2odgWE
write (1,*)" Auxiliary  1640 _tO2BedS_", tO2BedS
write (1,*)" Auxiliary  1641 _tO2UptNO3VegWEpi_", tO2O3gWE
write (1,*)" Auxiliary  1642 _tPMortVegEpi_", tPMVegE
write (1,*)" Auxiliary  1643 _tPMortVegPO4Epi_", tPMegO4E
write (1,*)" Auxiliary  1644 _tPMortVegPO4SEpi_", tPMeg4SE
write (1,*)" Auxiliary  1645 _tPMortVegPO4WEpi_", tPMeg4WE
write (1,*)" Auxiliary  1646 _tPMortVegDetEpi_", tPMVeDtE
write (1,*)" Auxiliary  1647 _tPMortVegDetWEpi_", tPMegtWE
write (1,*)" Auxiliary  1648 _tPMortVegDetSEpi_", tPMegtSE
write (1,*)" Auxiliary  1649 _tPBedVeg_", tPBedVeg
write (1,*)" Auxiliary  1650 _tNMortVegNH4WEpi_", tNMeg4WE
write (1,*)" Auxiliary  1651 _tNMortVegDetEpi_", tNMVeDtE
write (1,*)" Auxiliary  1652 _tNMortVegDetWEpi_", tNMegtWE
write (1,*)" Auxiliary  1653 _tNMortVegDetSEpi_", tNMegtSE
write (1,*)" Auxiliary  1654 _tNBedVeg_", tNBedVeg
write (1,*)" Auxiliary  1655 _wDBedDetWEpi_", wDBedtWE
write (1,*)" Auxiliary  1656 _tDBedDetSEpi_", tDBedtSE
write (1,*)" Auxiliary  1657 _wPBedPO4WEpi_", wPBdP4WE
write (1,*)" Auxiliary  1658 _wPBedDetWEpi_", wPBedtWE
write (1,*)" Auxiliary  1659 _tPBedPO4S_", tPBedO4S
write (1,*)" Auxiliary  1660 _tPBedDetS_", tPBedDtS
write (1,*)" Auxiliary  1661 _wNBedNH4WEpi_", wNBdN4WE
write (1,*)" Auxiliary  1662 _wNBedNO3WEpi_", wNBdN3WE
write (1,*)" Auxiliary  1663 _wNBedDetWEpi_", wNBedtWE
write (1,*)" Auxiliary  1664 _tNBedNH4S_", tNBedH4S
write (1,*)" Auxiliary  1665 _tNBedNO3S_", tNBedO3S
write (1,*)" Auxiliary  1666 _tNBedDetS_", tNBedDtS
write (1,*)" Auxiliary  1667 _tO2BedWEpi_", tO2BedWE
write (1,*)" Auxiliary  1668 _aVPUptMaxCrBlueEpi_", aVPxCueE
write (1,*)" Auxiliary  1669 _aVPUptBlueEpi_", aVPUBueE
write (1,*)" Auxiliary  1670 _wPUptBlueEpi_", wPUBlueE
write (1,*)" Auxiliary  1671 _aVNUptMaxCrBlueEpi_", aVNxCueE
write (1,*)" Auxiliary  1672 _ahNUptBlueEpi_", ahNUBueE
write (1,*)" Auxiliary  1673 _aVNUptBlueEpi_", aVNUBueE
write (1,*)" Auxiliary  1674 _wNUptBlueEpi_", wNUBlueE
write (1,*)" Auxiliary  1675 _afNH4UptBlueEpi_", afN4UueE
write (1,*)" Auxiliary  1676 _wNUptNH4BlueEpi_", wNUH4ueE
write (1,*)" Auxiliary  1677 _wNUptNO3BlueEpi_", wNUO3ueE
write (1,*)" Auxiliary  1678 _aPLimBlueEpi_", aPLmBueE
write (1,*)" Auxiliary  1679 _aNLimBlueEpi_", aNLmBueE
write (1,*)" Auxiliary  1680 _aSiLimBlueEpi_", aSiimueE
write (1,*)" Auxiliary  1681 _aLLimBlueEpi_", aLLmBueE
write (1,*)" Auxiliary  1682 _aMuTmLBlueEpi_", aMumLueE
write (1,*)" Auxiliary  1683 _aNutLimBlueEpi_", aNuimueE
write (1,*)" Auxiliary  1684 _aMuBlueEpi_", aMuBlueE
write (1,*)" Auxiliary  1685 _wDAssBlueEpi_", wDAsBueE
write (1,*)" Auxiliary  1686 _rChDBlueEpi_", rChDBueE
write (1,*)" Auxiliary  1687 _oChlaBlueEpi_", oChlBlE
write (1,*)" Auxiliary  1688 _aExtChBlueEpi_", aExChueE
write (1,*)" Auxiliary  1689 _wDRespBlueWEpi_", wDRpBeWE
write (1,*)" Auxiliary  1690 _wDLossBlueEpi_", wDLssueE
write (1,*)" Auxiliary  1691 _wDMortBlueWEpi_", wDMBleWE
write (1,*)" Auxiliary  1692 _ukDDecBlueEpi_", ukDecueE
write (1,*)" Auxiliary  1693 _wPExcrBlueWEpi_", wPErBeWE
write (1,*)" Auxiliary  1694 _wPLossBlueEpi_", wPLssueE
write (1,*)" Auxiliary  1695 _wPMortBlueWEpi_", wPMBleWE
write (1,*)" Auxiliary  1696 _wNExcrBlueWEpi_", wNErBeWE
write (1,*)" Auxiliary  1697 _wNLossBlueEpi_", wNLssueE
write (1,*)" Auxiliary  1698 _wNMortBlueWEpi_", wNMBleWE
write (1,*)" Auxiliary  1699 _wDPrimBlueWEpi_", wDPmBeWE
write (1,*)" Auxiliary  1700 _wPPrimBlueWEpi_", wPPmBeWE
write (1,*)" Auxiliary  1701 _wNPrimBlueWEpi_", wNPmBeWE
write (1,*)" Auxiliary  1702 _tDPrimBlueS_", tDPimueS
write (1,*)" Auxiliary  1703 _tPPrimBlueS_", tPPimueS
write (1,*)" Auxiliary  1704 _tNPrimBlueS_", tNPimueS
write (1,*)" Auxiliary  1705 _aVPUptMaxCrGrenEpi_", aVPxCenE
write (1,*)" Auxiliary  1706 _aVPUptGrenEpi_", aVPUGenE
write (1,*)" Auxiliary  1707 _wPUptGrenEpi_", wPUGrenE
write (1,*)" Auxiliary  1708 _aVNUptMaxCrGrenEpi_", aVNxCenE
write (1,*)" Auxiliary  1709 _ahNUptGrenEpi_", ahNUGenE
write (1,*)" Auxiliary  1710 _aVNUptGrenEpi_", aVNUGenE
write (1,*)" Auxiliary  1711 _wNUptGrenEpi_", wNUGrenE
write (1,*)" Auxiliary  1712 _afNH4UptGrenEpi_", afN4UenE
write (1,*)" Auxiliary  1713 _wNUptNH4GrenEpi_", wNUH4enE
write (1,*)" Auxiliary  1714 _wNUptNO3GrenEpi_", wNUO3enE
write (1,*)" Auxiliary  1715 _aPLimGrenEpi_", aPLmGenE
write (1,*)" Auxiliary  1716 _aNLimGrenEpi_", aNLmGenE
write (1,*)" Auxiliary  1717 _aSiLimGrenEpi_", aSiimenE
write (1,*)" Auxiliary  1718 _aLLimGrenEpi_", aLLmGenE
write (1,*)" Auxiliary  1719 _aMuTmLGrenEpi_", aMumLenE
write (1,*)" Auxiliary  1720 _aNutLimGrenEpi_", aNuimenE
write (1,*)" Auxiliary  1721 _aMuGrenEpi_", aMuGrenE
write (1,*)" Auxiliary  1722 _wDAssGrenEpi_", wDAsGenE
write (1,*)" Auxiliary  1723 _rChDGrenEpi_", rChDGenE
write (1,*)" Auxiliary  1724 _oChlaGrenEpi_", oChlGrE
write (1,*)" Auxiliary  1725 _aExtChGrenEpi_", aExChenE
write (1,*)" Auxiliary  1726 _wDRespGrenWEpi_", wDRpGnWE
write (1,*)" Auxiliary  1727 _wDLossGrenEpi_", wDLssenE
write (1,*)" Auxiliary  1728 _wDMortGrenWEpi_", wDMGrnWE
write (1,*)" Auxiliary  1729 _ukDDecGrenEpi_", ukDecenE
write (1,*)" Auxiliary  1730 _wPExcrGrenWEpi_", wPErGnWE
write (1,*)" Auxiliary  1731 _wPLossGrenEpi_", wPLssenE
write (1,*)" Auxiliary  1732 _wPMortGrenWEpi_", wPMGrnWE
write (1,*)" Auxiliary  1733 _wNExcrGrenWEpi_", wNErGnWE
write (1,*)" Auxiliary  1734 _wNLossGrenEpi_", wNLssenE
write (1,*)" Auxiliary  1735 _wNMortGrenWEpi_", wNMGrnWE
write (1,*)" Auxiliary  1736 _wDPrimGrenWEpi_", wDPmGnWE
write (1,*)" Auxiliary  1737 _wPPrimGrenWEpi_", wPPmGnWE
write (1,*)" Auxiliary  1738 _wNPrimGrenWEpi_", wNPmGnWE
write (1,*)" Auxiliary  1739 _tDPrimGrenS_", tDPimenS
write (1,*)" Auxiliary  1740 _tPPrimGrenS_", tPPimenS
write (1,*)" Auxiliary  1741 _tNPrimGrenS_", tNPimenS
write (1,*)" Auxiliary  1742 _aVPUptMaxCrDiatEpi_", aVPaxDiE
write (1,*)" Auxiliary  1743 _aVPUptDiatEpi_", aVPUDiE
write (1,*)" Auxiliary  1744 _wPUptDiatEpi_", wPUDiE
write (1,*)" Auxiliary  1745 _aVNUptMaxCrDiatEpi_", aVNaxDiE
write (1,*)" Auxiliary  1746 _ahNUptDiatEpi_", ahNUDiE
write (1,*)" Auxiliary  1747 _aVNUptDiatEpi_", aVNUDiE
write (1,*)" Auxiliary  1748 _wNUptDiatEpi_", wNUDiE
write (1,*)" Auxiliary  1749 _afNH4UptDiatEpi_", afNH4DiE
write (1,*)" Auxiliary  1750 _wNUptNH4DiatEpi_", wNUNHDiE
write (1,*)" Auxiliary  1751 _wNUptNO3DiatEpi_", wNUNODiE
write (1,*)" Auxiliary  1752 _aPLimDiatEpi_", aPLimDiE
write (1,*)" Auxiliary  1753 _aNLimDiatEpi_", aNLimDiE
write (1,*)" Auxiliary  1754 _aSiLimDiatEpi_", aSiLiDiE
write (1,*)" Auxiliary  1755 _aLLimDiatEpi_", aLLimDiE
write (1,*)" Auxiliary  1756 _aMuTmLDiatEpi_", aMuTmDiE
write (1,*)" Auxiliary  1757 _aNutLimDiatEpi_", aNuLiDiE
write (1,*)" Auxiliary  1758 _aMuDiatEpi_", aMuDiE
write (1,*)" Auxiliary  1759 _wDAssDiatEpi_", wDAssDiE
write (1,*)" Auxiliary  1760 _rChDDiatEpi_", rChDDiE
write (1,*)" Auxiliary  1761 _oChlaDiatEpi_", oChlDiE
write (1,*)" Auxiliary  1762 _aExtChDiatEpi_", aExtCDiE
write (1,*)" Auxiliary  1763 _wDRespDiatWEpi_", wDRspiWE
write (1,*)" Auxiliary  1764 _wDLossDiatEpi_", wDLosDiE
write (1,*)" Auxiliary  1765 _wDMortDiatWEpi_", wDMDiWE
write (1,*)" Auxiliary  1766 _ukDDecDiatEpi_", ukDDeDiE
write (1,*)" Auxiliary  1767 _wPExcrDiatWEpi_", wPEcriWE
write (1,*)" Auxiliary  1768 _wPLossDiatEpi_", wPLosDiE
write (1,*)" Auxiliary  1769 _wPMortDiatWEpi_", wPMDiWE
write (1,*)" Auxiliary  1770 _wNExcrDiatWEpi_", wNEcriWE
write (1,*)" Auxiliary  1771 _wNLossDiatEpi_", wNLosDiE
write (1,*)" Auxiliary  1772 _wNMortDiatWEpi_", wNMDiWE
write (1,*)" Auxiliary  1773 _wDPrimDiatWEpi_", wDPimiWE
write (1,*)" Auxiliary  1774 _wPPrimDiatWEpi_", wPPimiWE
write (1,*)" Auxiliary  1775 _wNPrimDiatWEpi_", wNPimiWE
write (1,*)" Auxiliary  1776 _tDPrimDiatS_", tDPriDiS
write (1,*)" Auxiliary  1777 _tPPrimDiatS_", tPPriDiS
write (1,*)" Auxiliary  1778 _tNPrimDiatS_", tNPriDiS
write (1,*)" Auxiliary  1779 _oChlaEpi_", oChlaE
write (1,*)" Auxiliary  1780 _oChla_", oChla
write (1,*)" Auxiliary  1781 _aLPIEpi_", aLPIE
write (1,*)" Auxiliary  1782 _aLMeasEpi_", aLMeasE
write (1,*)" Auxiliary  1783 _oChlaMeasEpi_", oChlMeE
write (1,*)" Auxiliary  1784 _wDAssPhytEpi_", wDAsPytE
write (1,*)" Auxiliary  1785 _wDRespPhytWEpi_", wDRpPtWE
write (1,*)" Auxiliary  1786 _wDMortPhytWEpi_", wDMPhtWE
write (1,*)" Auxiliary  1787 _tDSetPhytEpi_", tDStPytE
write (1,*)" Auxiliary  1788 _wDLossPhytEpi_", wDLssytE
write (1,*)" Auxiliary  1789 _wDPrimPhytWEpi_", wDPmPtWE
write (1,*)" Auxiliary  1790 _wPUptPhytEpi_", wPUPhytE
write (1,*)" Auxiliary  1791 _wPExcrPhytWEpi_", wPErPtWE
write (1,*)" Auxiliary  1792 _wPMortPhytWEpi_", wPMPhtWE
write (1,*)" Auxiliary  1793 _tPSetPhytEpi_", tPStPytE
write (1,*)" Auxiliary  1794 _wPLossPhytEpi_", wPLssytE
write (1,*)" Auxiliary  1795 _wPPrimPhytWEpi_", wPPmPtWE
write (1,*)" Auxiliary  1796 _wNUptPhytEpi_", wNUPhytE
write (1,*)" Auxiliary  1797 _wNUptNH4PhytEpi_", wNUH4ytE
write (1,*)" Auxiliary  1798 _wNUptNO3PhytEpi_", wNUO3ytE
write (1,*)" Auxiliary  1799 _wNExcrPhytWEpi_", wNErPtWE
write (1,*)" Auxiliary  1800 _wNMortPhytWEpi_", wNMPhtWE
write (1,*)" Auxiliary  1801 _tNSetPhytEpi_", tNStPytE
write (1,*)" Auxiliary  1802 _wNLossPhytEpi_", wNLssytE
write (1,*)" Auxiliary  1803 _wNPrimPhytWEpi_", wNPmPtWE
write (1,*)" Auxiliary  1804 _tDPrimPhytS_", tDPimytS
write (1,*)" Auxiliary  1805 _tPPrimPhytS_", tPPimytS
write (1,*)" Auxiliary  1806 _tNPrimPhytS_", tNPimytS
write (1,*)" Auxiliary  1807 _wSiUptDiatEpi_", wSiUDiE
write (1,*)" Auxiliary  1808 _wSiExcrDiatWEpi_", wSixciWE
write (1,*)" Auxiliary  1809 _wSiLossDiatEpi_", wSiosDiE
write (1,*)" Auxiliary  1810 _wSiMortDiatWEpi_", wSiMDiWE
write (1,*)" Auxiliary  1811 _tSiSetDiatEpi_", tSiSeDiE
write (1,*)" Auxiliary  1812 _wSiPrimDiatWHyp_", wSiriiWH
write (1,*)" Auxiliary  1813 _wSiPrimDiatWEpi_", wSiriiWE
write (1,*)" Auxiliary  1814 _rCyDBlueEpi_", rCyDBueE
write (1,*)" Auxiliary  1815 _oCyanEpi_", oCyanE
write (1,*)" Auxiliary  1816 _fDDiatEpi_", fDDiE
write (1,*)" Auxiliary  1817 _wDPrimDetWEpi_", wDPimtWE
write (1,*)" Auxiliary  1818 _tDPrimDetS_", tDPriDtS
write (1,*)" Auxiliary  1819 _tDPrimTotT_", tDPimotT
write (1,*)" Auxiliary  1820 _wO2ProdPhytEpi_", wO2odytE
write (1,*)" Auxiliary  1821 _wO2RespPhytWEpi_", wO2sptWE
write (1,*)" Auxiliary  1822 _wO2UptNO3PhytEpi_", wO2O3ytE
write (1,*)" Auxiliary  1823 _wO2PrimWEpi_", wO2PrmWE
write (1,*)" Auxiliary  1824 _tO2RespPhytS_", tO2spytS
write (1,*)" Auxiliary  1825 _tO2PrimS_", tO2PrimS
write (1,*)" Auxiliary  1826 _tO2PrimSHyp_", tO2PrmSH
write (1,*)" Auxiliary  1827 _tO2PrimSEpi_", tO2PrmSE
write (1,*)" Auxiliary  1828 _wPMortPhytPO4WEpi_", wPMyt4WE
write (1,*)" Auxiliary  1829 _wPMortPhytDetWEpi_", wPMhytWE
write (1,*)" Auxiliary  1830 _wPLossPhytPO4Epi_", wPLPhO4E
write (1,*)" Auxiliary  1831 _wPLossPhytDetEpi_", wPLsPDtE
write (1,*)" Auxiliary  1832 _wPPrimPO4WEpi_", wPPim4WE
write (1,*)" Auxiliary  1833 _wPPrimDetWEpi_", wPPimtWE
write (1,*)" Auxiliary  1834 _tPPrimDetS_", tPPriDtS
write (1,*)" Auxiliary  1835 _wNMortPhytNH4WEpi_", wNMyt4WE
write (1,*)" Auxiliary  1836 _wNMortPhytDetWEpi_", wNMhytWE
write (1,*)" Auxiliary  1837 _wNLossPhytNH4Epi_", wNLPhH4E
write (1,*)" Auxiliary  1838 _wNLossPhytDetEpi_", wNLsPDtE
write (1,*)" Auxiliary  1839 _wNPrimNH4WEpi_", wNPim4WE
write (1,*)" Auxiliary  1840 _wNPrimNO3WEpi_", wNPim3WE
write (1,*)" Auxiliary  1841 _wNPrimDetWEpi_", wNPimtWE
write (1,*)" Auxiliary  1842 _tNPrimDetS_", tNPriDtS
write (1,*)" Auxiliary  1843 _wSiPrimSiO2WEpi_", wSiim2WE
write (1,*)" Auxiliary  1844 _wSiPrimDetWEpi_", wSiritWE
write (1,*)" Auxiliary  1845 _tSiPrimDiatS_", tSiriDiS
write (1,*)" Auxiliary  1846 _tSiPrimDetS_", tSiriDtS
write (1,*)" Auxiliary  1847 _aDepthEuphEpi_", aDptEphE
write (1,*)" Auxiliary  1848 _aRelDepthEuphEpi_", aReptphE
write (1,*)" Auxiliary  1849 _aChlaHEpi_", aChlaHE
write (1,*)" Auxiliary  1850 _aCovPhytWEpi_", aCoPhtWE
write (1,*)" Auxiliary  1851 _rExtChPhytEpi_", rExChytE
write (1,*)" Auxiliary  1852 _rPDZooEpi_", rPDZooE
write (1,*)" Auxiliary  1853 _rNDZooEpi_", rNDZooE
write (1,*)" Auxiliary  1854 _oDFoodZooEpi_", oDFodooE
write (1,*)" Auxiliary  1855 _aFiltEpi_", aFiltE
write (1,*)" Auxiliary  1856 _aDSatZooEpi_", aDSatooE
write (1,*)" Auxiliary  1857 _wDEnvZooEpi_", wDEnvooE
write (1,*)" Auxiliary  1858 _wDAssZooEpi_", wDAssooE
write (1,*)" Auxiliary  1859 _wDConsZooEpi_", wDCZooE
write (1,*)" Auxiliary  1860 _wDConsDetZooEpi_", wDCDtooE
write (1,*)" Auxiliary  1861 _wDConsDiatZooEpi_", wDCDiooE
write (1,*)" Auxiliary  1862 _wDConsGrenZooEpi_", wDCreooE
write (1,*)" Auxiliary  1863 _wDConsBlueZooEpi_", wDCluooE
write (1,*)" Auxiliary  1864 _wDConsPhytZooEpi_", wDChyooE
write (1,*)" Auxiliary  1865 _wDEgesZooEpi_", wDEZooE
write (1,*)" Auxiliary  1866 _aCorDRespZooEpi_", aCoReooE
write (1,*)" Auxiliary  1867 _wDRespZooEpi_", wDRspooE
write (1,*)" Auxiliary  1868 _wDMortZooEpi_", wDMZooE
write (1,*)" Auxiliary  1869 _oPFoodZooEpi_", oPFodooE
write (1,*)" Auxiliary  1870 _rPDFoodZooEpi_", rPDooooE
write (1,*)" Auxiliary  1871 _wPConsDiatZooEpi_", wPCDiooE
write (1,*)" Auxiliary  1872 _wPConsGrenZooEpi_", wPCreooE
write (1,*)" Auxiliary  1873 _wPConsBlueZooEpi_", wPCluooE
write (1,*)" Auxiliary  1874 _wPConsPhytZooEpi_", wPChyooE
write (1,*)" Auxiliary  1875 _wPConsDetZooEpi_", wPCDtooE
write (1,*)" Auxiliary  1876 _wPConsZooEpi_", wPCZooE
write (1,*)" Auxiliary  1877 _afPAssZooEpi_", afPssooE
write (1,*)" Auxiliary  1878 _wPAssZooEpi_", wPAssooE
write (1,*)" Auxiliary  1879 _wPEgesZooEpi_", wPEZooE
write (1,*)" Auxiliary  1880 _wPEgesZooPO4Epi_", wPEooO4E
write (1,*)" Auxiliary  1881 _wPEgesZooDetEpi_", wPEZoDtE
write (1,*)" Auxiliary  1882 _akPExcrZooEpi_", akPxcooE
write (1,*)" Auxiliary  1883 _wPExcrZooEpi_", wPEcrooE
write (1,*)" Auxiliary  1884 _wPMortZooEpi_", wPMZooE
write (1,*)" Auxiliary  1885 _wPMortZooPO4Epi_", wPMooO4E
write (1,*)" Auxiliary  1886 _wPMortZooDetEpi_", wPMZoDtE
write (1,*)" Auxiliary  1887 _oNFoodZooEpi_", oNFodooE
write (1,*)" Auxiliary  1888 _rNDFoodZooEpi_", rNDooooE
write (1,*)" Auxiliary  1889 _wNConsDiatZooEpi_", wNCDiooE
write (1,*)" Auxiliary  1890 _wNConsGrenZooEpi_", wNCreooE
write (1,*)" Auxiliary  1891 _wNConsBlueZooEpi_", wNCluooE
write (1,*)" Auxiliary  1892 _wNConsPhytZooEpi_", wNChyooE
write (1,*)" Auxiliary  1893 _wNConsDetZooEpi_", wNCDtooE
write (1,*)" Auxiliary  1894 _wNConsZooEpi_", wNCZooE
write (1,*)" Auxiliary  1895 _afNAssZooEpi_", afNssooE
write (1,*)" Auxiliary  1896 _wNAssZooEpi_", wNAssooE
write (1,*)" Auxiliary  1897 _wNEgesZooEpi_", wNEZooE
write (1,*)" Auxiliary  1898 _wNEgesZooNH4Epi_", wNEooH4E
write (1,*)" Auxiliary  1899 _wNEgesZooDetEpi_", wNEZoDtE
write (1,*)" Auxiliary  1900 _kNExcrZooEpi_", kNEcrooE
write (1,*)" Auxiliary  1901 _wNExcrZooEpi_", wNEcrooE
write (1,*)" Auxiliary  1902 _wNMortZooEpi_", wNMZooE
write (1,*)" Auxiliary  1903 _wNMortZooNH4Epi_", wNMooH4E
write (1,*)" Auxiliary  1904 _wNMortZooDetEpi_", wNMZoDtE
write (1,*)" Auxiliary  1905 _wSiConsDiatZooEpi_", wSiDiooE
write (1,*)" Auxiliary  1906 _rPDFiJv_", rPDFiJv
write (1,*)" Auxiliary  1907 _rPDFiAd_", rPDFiAd
write (1,*)" Auxiliary  1908 _rNDFiJv_", rNDFiJv
write (1,*)" Auxiliary  1909 _rNDFiAd_", rNDFiAd
write (1,*)" Auxiliary  1910 _tDReprFish_", tDRprish
write (1,*)" Auxiliary  1911 _tDAgeFish_", tDAgeish
write (1,*)" Auxiliary  1912 _aDTotZoo_", aDTotZoo
write (1,*)" Auxiliary  1913 _aPTotZoo_", aPTotZoo
write (1,*)" Auxiliary  1914 _aNTotZoo_", aNTotZoo
write (1,*)" Auxiliary  1915 _aDSatFiJv_", aDSatiJv
write (1,*)" Auxiliary  1916 _tDEnvFiJv_", tDEnviJv
write (1,*)" Auxiliary  1917 _tDAssFiJv_", tDAssiJv
write (1,*)" Auxiliary  1918 _tDConsFiJv_", tDCFiJv
write (1,*)" Auxiliary  1919 _tDEgesFiJv_", tDEFiJv
write (1,*)" Auxiliary  1920 _tDRespFiJv_", tDRspiJv
write (1,*)" Auxiliary  1921 _tDMortFiJv_", tDMFiJv
write (1,*)" Auxiliary  1922 _tDMigrFiJv_", tDMgriJv
write (1,*)" Auxiliary  1923 _tDEnvFiAd_", tDEnviAd
write (1,*)" Auxiliary  1924 _tDAssFiAd_", tDAssiAd
write (1,*)" Auxiliary  1925 _tDConsFiAd_", tDCFiAd
write (1,*)" Auxiliary  1926 _tDEgesFiAd_", tDEFiAd
write (1,*)" Auxiliary  1927 _tDRespFiAd_", tDRspiAd
write (1,*)" Auxiliary  1928 _tDMortFiAd_", tDMFiAd
write (1,*)" Auxiliary  1929 _tDHarvFish_", tDHrvish
write (1,*)" Auxiliary  1930 _tDMigrFiAd_", tDMgriAd
write (1,*)" Auxiliary  1931 _tDMortFiJvBot_", tDMiJBot
write (1,*)" Auxiliary  1932 _tDMortFiJvDet_", tDMFivDt
write (1,*)" Auxiliary  1933 _tDMortFiAdBot_", tDMiABot
write (1,*)" Auxiliary  1934 _tDMortFiAdDet_", tDMFidDt
write (1,*)" Auxiliary  1935 _tPReprFish_", tPRprish
write (1,*)" Auxiliary  1936 _tPAgeFish_", tPAgeish
write (1,*)" Auxiliary  1937 _tPMigrFiJv_", tPMgriJv
write (1,*)" Auxiliary  1938 _tPConsFiJv_", tPCFiJv
write (1,*)" Auxiliary  1939 _afPAssFiJv_", afPssiJv
write (1,*)" Auxiliary  1940 _tPAssFiJv_", tPAssiJv
write (1,*)" Auxiliary  1941 _tPEgesFiJv_", tPEFiJv
write (1,*)" Auxiliary  1942 _tPExcrFiJv_", tPEcriJv
write (1,*)" Auxiliary  1943 _tPMortFiJv_", tPMFiJv
write (1,*)" Auxiliary  1944 _tPMigrFiAd_", tPMgriAd
write (1,*)" Auxiliary  1945 _tPConsFiAd_", tPCFiAd
write (1,*)" Auxiliary  1946 _tPAssFiAd_", tPAssiAd
write (1,*)" Auxiliary  1947 _tPEgesFiAd_", tPEFiAd
write (1,*)" Auxiliary  1948 _tPExcrFiAd_", tPEcriAd
write (1,*)" Auxiliary  1949 _tPMortFiAd_", tPMFiAd
write (1,*)" Auxiliary  1950 _tPHarvFish_", tPHrvish
write (1,*)" Auxiliary  1951 _tPMortFiJvBot_", tPMiJBot
write (1,*)" Auxiliary  1952 _tPMortFiJvPO4_", tPMiJPO4
write (1,*)" Auxiliary  1953 _tPMortFiJvDet_", tPMFivDt
write (1,*)" Auxiliary  1954 _tPMortFiAdBot_", tPMiABot
write (1,*)" Auxiliary  1955 _tPMortFiAdPO4_", tPMiAPO4
write (1,*)" Auxiliary  1956 _tPMortFiAdDet_", tPMFidDt
write (1,*)" Auxiliary  1957 _tPEgesFiJvPO4_", tPEiJPO4
write (1,*)" Auxiliary  1958 _tPEgesFiJvDet_", tPEFivDt
write (1,*)" Auxiliary  1959 _tPEgesFiAdPO4_", tPEiAPO4
write (1,*)" Auxiliary  1960 _tPEgesFiAdDet_", tPEFidDt
write (1,*)" Auxiliary  1961 _tNReprFish_", tNRprish
write (1,*)" Auxiliary  1962 _tNAgeFish_", tNAgeish
write (1,*)" Auxiliary  1963 _tNMigrFiJv_", tNMgriJv
write (1,*)" Auxiliary  1964 _tNConsFiJv_", tNCFiJv
write (1,*)" Auxiliary  1965 _afNAssFiJv_", afNssiJv
write (1,*)" Auxiliary  1966 _tNAssFiJv_", tNAssiJv
write (1,*)" Auxiliary  1967 _tNEgesFiJv_", tNEFiJv
write (1,*)" Auxiliary  1968 _tNExcrFiJv_", tNEcriJv
write (1,*)" Auxiliary  1969 _tNMortFiJv_", tNMFiJv
write (1,*)" Auxiliary  1970 _tNMigrFiAd_", tNMgriAd
write (1,*)" Auxiliary  1971 _tNConsFiAd_", tNCFiAd
write (1,*)" Auxiliary  1972 _tNAssFiAd_", tNAssiAd
write (1,*)" Auxiliary  1973 _tNEgesFiAd_", tNEFiAd
write (1,*)" Auxiliary  1974 _tNExcrFiAd_", tNEcriAd
write (1,*)" Auxiliary  1975 _tNMortFiAd_", tNMFiAd
write (1,*)" Auxiliary  1976 _tNHarvFish_", tNHrvish
write (1,*)" Auxiliary  1977 _tNMortFiAdBot_", tNMiABot
write (1,*)" Auxiliary  1978 _tNMortFiAdNH4_", tNMiANH4
write (1,*)" Auxiliary  1979 _tNMortFiAdDet_", tNMFidDt
write (1,*)" Auxiliary  1980 _tNMortFiJvBot_", tNMiJBot
write (1,*)" Auxiliary  1981 _tNMortFiJvNH4_", tNMiJNH4
write (1,*)" Auxiliary  1982 _tNMortFiJvDet_", tNMFivDt
write (1,*)" Auxiliary  1983 _tNEgesFiJvNH4_", tNEiJNH4
write (1,*)" Auxiliary  1984 _tNEgesFiJvDet_", tNEFivDt
write (1,*)" Auxiliary  1985 _tDMigrPisc_", tDMgrisc
write (1,*)" Auxiliary  1986 _aDFish_", aDFish
write (1,*)" Auxiliary  1987 _aDSatPisc_", aDSatisc
write (1,*)" Auxiliary  1988 _tDEnvPisc_", tDEnvisc
write (1,*)" Auxiliary  1989 _tDAssPisc_", tDAssisc
write (1,*)" Auxiliary  1990 _tDConsPisc_", tDCPisc
write (1,*)" Auxiliary  1991 _tDEgesPisc_", tDEPisc
write (1,*)" Auxiliary  1992 _tDConsFiJvPisc_", tDCiJisc
write (1,*)" Auxiliary  1993 _tDConsFiAdPisc_", tDCiAisc
write (1,*)" Auxiliary  1994 _tDMortPisc_", tDMPisc
write (1,*)" Auxiliary  1995 _tDMortPiscBot_", tDMisBot
write (1,*)" Auxiliary  1996 _tDMortPiscDet_", tDMPicDt
write (1,*)" Auxiliary  1997 _tDHarvPisc_", tDHrvisc
write (1,*)" Auxiliary  1998 _aPPisc_", aPPisc
write (1,*)" Auxiliary  1999 _tPConsFiJvPisc_", tPCiJisc
write (1,*)" Auxiliary  2000 _tPConsFiAdPisc_", tPCiAisc
write (1,*)" Auxiliary  2001 _tPConsPisc_", tPCPisc
write (1,*)" Auxiliary  2002 _rPDFoodPisc_", rPDooisc
write (1,*)" Auxiliary  2003 _aNPisc_", aNPisc
write (1,*)" Auxiliary  2004 _tNConsFiJvPisc_", tNCiJisc
write (1,*)" Auxiliary  2005 _tNConsFiAdPisc_", tNCiAisc
write (1,*)" Auxiliary  2006 _tNConsPisc_", tNCPisc
write (1,*)" Auxiliary  2007 _rNDFoodPisc_", rNDooisc
write (1,*)" Auxiliary  2008 _tDConsPiscNut_", tDCPiscN
write (1,*)" Auxiliary  2009 _tDEgesPiscNut_", tDEPiscN
write (1,*)" Auxiliary  2010 _tDConsFiJvPiscNut_", tDCFJPNu
write (1,*)" Auxiliary  2011 _tDConsFiAdPiscNut_", tDCFAPNu
write (1,*)" Auxiliary  2012 _tPConsFiJvPiscNut_", tPCFJPNu
write (1,*)" Auxiliary  2013 _tPConsFiAdPiscNut_", tPCFAPNu
write (1,*)" Auxiliary  2014 _tPConsPiscNut_", tPCPiscN
write (1,*)" Auxiliary  2015 _tNConsFiJvPiscNut_", tNCFJPNu
write (1,*)" Auxiliary  2016 _tNConsFiAdPiscNut_", tNCFAPNu
write (1,*)" Auxiliary  2017 _tNConsPiscNut_", tNCPiscN
write (1,*)" Auxiliary  2018 _afPAssPisc_", afPssisc
write (1,*)" Auxiliary  2019 _tPAssPisc_", tPAssisc
write (1,*)" Auxiliary  2020 _tPEgesPisc_", tPEPisc
write (1,*)" Auxiliary  2021 _tPEgesPiscPO4_", tPEisPO4
write (1,*)" Auxiliary  2022 _tPEgesPiscDet_", tPEPicDt
write (1,*)" Auxiliary  2023 _tDRespPisc_", tDRspisc
write (1,*)" Auxiliary  2024 _tPExcrPisc_", tPEcrisc
write (1,*)" Auxiliary  2025 _tPMortPisc_", tPMPisc
write (1,*)" Auxiliary  2026 _tPMortPiscBot_", tPMisBot
write (1,*)" Auxiliary  2027 _tPMortPiscPO4_", tPMisPO4
write (1,*)" Auxiliary  2028 _tPMortPiscDet_", tPMPicDt
write (1,*)" Auxiliary  2029 _tPMigrPisc_", tPMgrisc
write (1,*)" Auxiliary  2030 _tPHarvPisc_", tPHrvisc
write (1,*)" Auxiliary  2031 _afNAssPisc_", afNssisc
write (1,*)" Auxiliary  2032 _tNAssPisc_", tNAssisc
write (1,*)" Auxiliary  2033 _tNEgesPisc_", tNEPisc
write (1,*)" Auxiliary  2034 _tNEgesPiscNH4_", tNEisNH4
write (1,*)" Auxiliary  2035 _tNEgesPiscDet_", tNEPicDt
write (1,*)" Auxiliary  2036 _tNExcrPisc_", tNEcrisc
write (1,*)" Auxiliary  2037 _tNMortPisc_", tNMPisc
write (1,*)" Auxiliary  2038 _tNMortPiscBot_", tNMisBot
write (1,*)" Auxiliary  2039 _tNMortPiscNH4_", tNMisNH4
write (1,*)" Auxiliary  2040 _tNMortPiscDet_", tNMPicDt
write (1,*)" Auxiliary  2041 _tNMigrPisc_", tNMgrisc
write (1,*)" Auxiliary  2042 _tNHarvPisc_", tNHrvisc
write (1,*)" Auxiliary  2043 _tDWebBent_", tDWebBnt
write (1,*)" Auxiliary  2044 _tPWebBent_", tPWebBnt
write (1,*)" Auxiliary  2045 _tNWebBent_", tNWebBnt
write (1,*)" Auxiliary  2046 _tDWebFiJv_", tDWebiJv
write (1,*)" Auxiliary  2047 _tPWebFiJv_", tPWebiJv
write (1,*)" Auxiliary  2048 _tNWebFiJv_", tNWebiJv
write (1,*)" Auxiliary  2049 _tDWebFiAd_", tDWebiAd
write (1,*)" Auxiliary  2050 _tPWebFiAd_", tPWebiAd
write (1,*)" Auxiliary  2051 _tNWebFiAd_", tNWebiAd
write (1,*)" Auxiliary  2052 _tDWebPisc_", tDWebisc
write (1,*)" Auxiliary  2053 _wDWebZooHyp_", wDWebooH
write (1,*)" Auxiliary  2054 _wDWebDetWHyp_", wDWebtWH
write (1,*)" Auxiliary  2055 _wDWebDiatWHyp_", wDWebiWH
write (1,*)" Auxiliary  2056 _wDWebGrenWHyp_", wDWbGnWH
write (1,*)" Auxiliary  2057 _wDWebBlueWHyp_", wDWbBeWH
write (1,*)" Auxiliary  2058 _wDWebZooEpi_", wDWebooE
write (1,*)" Auxiliary  2059 _wDWebDetWEpi_", wDWebtWE
write (1,*)" Auxiliary  2060 _wDWebDiatWEpi_", wDWebiWE
write (1,*)" Auxiliary  2061 _wDWebGrenWEpi_", wDWbGnWE
write (1,*)" Auxiliary  2062 _wDWebBlueWEpi_", wDWbBeWE
write (1,*)" Auxiliary  2063 _tDWebTotT_", tDWebotT
write (1,*)" Auxiliary  2064 _wPWebZooHyp_", wPWebooH
write (1,*)" Auxiliary  2065 _wPWebPO4WHyp_", wPWbP4WH
write (1,*)" Auxiliary  2066 _wPWebDetWHyp_", wPWebtWH
write (1,*)" Auxiliary  2067 _wPWebDiatWHyp_", wPWebiWH
write (1,*)" Auxiliary  2068 _wPWebGrenWHyp_", wPWbGnWH
write (1,*)" Auxiliary  2069 _wPWebBlueWHyp_", wPWbBeWH
write (1,*)" Auxiliary  2070 _wPWebZooEpi_", wPWebooE
write (1,*)" Auxiliary  2071 _wPWebPO4WEpi_", wPWbP4WE
write (1,*)" Auxiliary  2072 _wPWebDetWEpi_", wPWebtWE
write (1,*)" Auxiliary  2073 _wPWebDiatWEpi_", wPWebiWE
write (1,*)" Auxiliary  2074 _wPWebGrenWEpi_", wPWbGnWE
write (1,*)" Auxiliary  2075 _wPWebBlueWEpi_", wPWbBeWE
write (1,*)" Auxiliary  2076 _tPWebTotT_", tPWebotT
write (1,*)" Auxiliary  2077 _tNEgesFiAdNH4_", tNEiANH4
write (1,*)" Auxiliary  2078 _tNEgesFiAdDet_", tNEFidDt
write (1,*)" Auxiliary  2079 _wNWebNO3WHyp_", wNWbN3WH
write (1,*)" Auxiliary  2080 _wNWebNO3WEpi_", wNWbN3WE
write (1,*)" Auxiliary  2081 _wNWebZooHyp_", wNWebooH
write (1,*)" Auxiliary  2082 _wNWebNH4WHyp_", wNWbN4WH
write (1,*)" Auxiliary  2083 _wNWebDetWHyp_", wNWebtWH
write (1,*)" Auxiliary  2084 _wNWebDiatWHyp_", wNWebiWH
write (1,*)" Auxiliary  2085 _wNWebGrenWHyp_", wNWbGnWH
write (1,*)" Auxiliary  2086 _wNWebBlueWHyp_", wNWbBeWH
write (1,*)" Auxiliary  2087 _wNWebZooEpi_", wNWebooE
write (1,*)" Auxiliary  2088 _wNWebNH4WEpi_", wNWbN4WE
write (1,*)" Auxiliary  2089 _wNWebDetWEpi_", wNWebtWE
write (1,*)" Auxiliary  2090 _wNWebDiatWEpi_", wNWebiWE
write (1,*)" Auxiliary  2091 _wNWebGrenWEpi_", wNWbGnWE
write (1,*)" Auxiliary  2092 _wNWebBlueWEpi_", wNWbBeWE
write (1,*)" Auxiliary  2093 _tNWebTotT_", tNWebotT
write (1,*)" Auxiliary  2094 _wSiWebDetWEpi_", wSiebtWE
write (1,*)" Auxiliary  2095 _aPrefAveEpi_", aPrefveE
write (1,*)" Auxiliary  2096 _wDConsZoo2Epi_", wDCZoo2E
write (1,*)" Auxiliary  2097 _aDConsZooSpEpi_", aDCZoSpE
write (1,*)" Auxiliary  2098 _aDAssZooSpEpi_", aDAsZSpE
write (1,*)" Auxiliary  2099 _aDGrazSpEpi_", aDGraSpE
write (1,*)" Auxiliary  2100 _aPConsZooSpEpi_", aPCZoSpE
write (1,*)" Auxiliary  2101 _aPGrazSpEpi_", aPGraSpE
write (1,*)" Auxiliary  2102 _aNConsZooSpEpi_", aNCZoSpE
write (1,*)" Auxiliary  2103 _aNGrazSpEpi_", aNGraSpE
write (1,*)" Auxiliary  2104 _tDSetTotEpi_", tDSetotE
write (1,*)" Auxiliary  2105 _tPSetTotEpi_", tPSetotE
write (1,*)" Auxiliary  2106 _tNSetTotEpi_", tNSetotE
write (1,*)" Auxiliary  2107 _tDResusTot_", tDRsuTot
write (1,*)" Auxiliary  2108 _tPResusTot_", tPRsuTot
write (1,*)" Auxiliary  2109 _tNResusTot_", tNRsuTot
write (1,*)" Auxiliary  2110 _aDepthStart_", aDptSart
write (1,*)" Auxiliary  2111 _akDredDepth_", akDreDpt
write (1,*)" Auxiliary  2112 _akDred_", akDred
write (1,*)" Auxiliary  2113 _akDredBent_", akDreBnt
write (1,*)" Auxiliary  2114 _vDredDepthW_", vDredptW
write (1,*)" Auxiliary  2115 _tDDredDetS_", tDDreDtS
write (1,*)" Auxiliary  2116 _tPDredDetS_", tPDreDtS
write (1,*)" Auxiliary  2117 _tNDredDetS_", tNDreDtS
write (1,*)" Auxiliary  2118 _tSiDredDetS_", tSireDtS
write (1,*)" Auxiliary  2119 _tPDredAIMS_", tPDedIMS
write (1,*)" Auxiliary  2120 _tDDredNetSoil_", tDDdNoil
write (1,*)" Auxiliary  2121 _tDDredNetIMS_", tDDdNIMS
write (1,*)" Auxiliary  2122 _tDDredNetHumS_", tDDdNumS
write (1,*)" Auxiliary  2123 _tPDredNetHumS_", tPDdNumS
write (1,*)" Auxiliary  2124 _tNDredNetHumS_", tNDdNumS
write (1,*)" Auxiliary  2125 _tDDredDiatS_", tDDreDiS
write (1,*)" Auxiliary  2126 _tPDredDiatS_", tPDreDiS
write (1,*)" Auxiliary  2127 _tNDredDiatS_", tNDreDiS
write (1,*)" Auxiliary  2128 _tDDredGrenS_", tDDedenS
write (1,*)" Auxiliary  2129 _tPDredGrenS_", tPDedenS
write (1,*)" Auxiliary  2130 _tNDredGrenS_", tNDedenS
write (1,*)" Auxiliary  2131 _tDDredBlueS_", tDDedueS
write (1,*)" Auxiliary  2132 _tPDredBlueS_", tPDedueS
write (1,*)" Auxiliary  2133 _tNDredBlueS_", tNDedueS
write (1,*)" Auxiliary  2134 _tDDredPhytS_", tDDedytS
write (1,*)" Auxiliary  2135 _tPDredPhytS_", tPDedytS
write (1,*)" Auxiliary  2136 _tNDredPhytS_", tNDedytS
write (1,*)" Auxiliary  2137 _tDDredBent_", tDDreBnt
write (1,*)" Auxiliary  2138 _tPDredBent_", tPDreBnt
write (1,*)" Auxiliary  2139 _tNDredBent_", tNDreBnt
write (1,*)" Auxiliary  2140 _tDDredVeg_", tDDreVeg
write (1,*)" Auxiliary  2141 _tPDredVeg_", tPDreVeg
write (1,*)" Auxiliary  2142 _tNDredVeg_", tNDreVeg
write (1,*)" Auxiliary  2143 _tDDredNetTot_", tDDdNTot
write (1,*)" Auxiliary  2144 _tPDredNetTot_", tPDdNTot
write (1,*)" Auxiliary  2145 _tNDredNetTot_", tNDdNTot
write (1,*)" Auxiliary  2146 _tSiDredTot_", tSireTot
write (1,*)" Auxiliary  2147 _tDIMS_", tDIMS
write (1,*)" Auxiliary  2148 _tDHumS_", tDHumS
write (1,*)" Auxiliary  2149 _tDDetS_", tDDtS
write (1,*)" Auxiliary  2150 _vDeltaS_", vDeltaS
write (1,*)" Auxiliary  2151 _vDeltaW_", vDeltaW
write (1,*)" Auxiliary  2152 _tDBurIM_", tDBurIM
write (1,*)" Auxiliary  2153 _tDBurOM_", tDBurOM
write (1,*)" Auxiliary  2154 _tDBurDet_", tDBurDt
write (1,*)" Auxiliary  2155 _tDBurHum_", tDBurHum
write (1,*)" Auxiliary  2156 _tDBurTot_", tDBurTot
write (1,*)" Auxiliary  2157 _tPBurHum_", tPBurHum
write (1,*)" Auxiliary  2158 _tPBurDet_", tPBurDt
write (1,*)" Auxiliary  2159 _tPBurAIM_", tPBurAIM
write (1,*)" Auxiliary  2160 _tPBurPO4_", tPBurPO4
write (1,*)" Auxiliary  2161 _tPBurTot_", tPBurTot
write (1,*)" Auxiliary  2162 _tNBurHum_", tNBurHum
write (1,*)" Auxiliary  2163 _tNBurDet_", tNBurDt
write (1,*)" Auxiliary  2164 _tNBurNH4_", tNBurNH4
write (1,*)" Auxiliary  2165 _tNBurNO3_", tNBurNO3
write (1,*)" Auxiliary  2166 _tNBurTot_", tNBurTot
write (1,*)" Auxiliary  2167 _tSiBurDet_", tSiBurDt
write (1,*)" Auxiliary  2168 _tSiBurTot_", tSiBuTot
write (1,*)" Auxiliary  2169 _aRelDeltaWEpi_", aReDeaWE
write (1,*)" Auxiliary  2170 _aRelDeltaWHyp_", aReDeaWH
write (1,*)" Auxiliary  2171 _aPFish_", aPFish
write (1,*)" Auxiliary  2172 _aNFish_", aNFish
write (1,*)" Auxiliary  2173 _aDRelTotT_", aDRelotT
write (1,*)" Auxiliary  2174 _aNRelTotT_", aNRelotT
write (1,*)" Auxiliary  2175 _aPRelTotT_", aPRelotT
write (1,*)" Auxiliary  2176 _aSiRelTotT_", aSielotT
write (1,*)" Auxiliary  2177 _aO2RelTotT_", aO2elotT
write (1,*)" Auxiliary  2178 _aDTotTHyp_", aDTotTH
write (1,*)" Auxiliary  2179 _aNTotTHyp_", aNTotTH
write (1,*)" Auxiliary  2180 _aPTotTHyp_", aPTotTH
write (1,*)" Auxiliary  2181 _aSiTotTHyp_", aSiTotTH
write (1,*)" Auxiliary  2182 _aO2TotTHyp_", aO2TotTH
write (1,*)" Auxiliary  2183 _aDTotT_", aDTotT
write (1,*)" Auxiliary  2184 _aNTotT_", aNTotT
write (1,*)" Auxiliary  2185 _aPTotT_", aPTotT
write (1,*)" Auxiliary  2186 _aSiTotT_", aSiTotT
write (1,*)" Auxiliary  2187 _aO2TotT_", aO2TotT
write (1,*)" Auxiliary  2188 _tDBedTotT_", tDBedotT
write (1,*)" Auxiliary  2189 _tPBedTotT_", tPBedotT
write (1,*)" Auxiliary  2190 _tNBedTotT_", tNBedotT
write (1,*)" Auxiliary  2191 _aDError_", aDError
write (1,*)" Auxiliary  2192 _aNError_", aNError
write (1,*)" Auxiliary  2193 _aPError_", aPError
write (1,*)" Auxiliary  2194 _aSiError_", aSiError
write (1,*)" Auxiliary  2195 _aO2Error_", aO2Error
write (1,*)" Auxiliary  2196 _cCheckVegHeight_", cChVegH
write (1,*)" Derivative  0 _dDepthW_ -999"
write (1,*)" Derivative  1 _dNH4WHyp_", dNH4WH
write (1,*)" Derivative  2 _dNO3WHyp_", dNO3WH
write (1,*)" Derivative  3 _dPO4WHyp_", dPO4WH
write (1,*)" Derivative  4 _dPAIMWHyp_", dPAIMWH
write (1,*)" Derivative  5 _dSiO2WHyp_", dSiO2WH
write (1,*)" Derivative  6 _dO2WHyp_", dO2WH
write (1,*)" Derivative  7 _dDDetWHyp_", dDDetWH
write (1,*)" Derivative  8 _dNDetWHyp_", dNDetWH
write (1,*)" Derivative  9 _dPDetWHyp_", dPDetWH
write (1,*)" Derivative  10 _dSiDetWHyp_", dSiDetWH
write (1,*)" Derivative  11 _dDIMWHyp_", dDIMWH
write (1,*)" Derivative  12 _dDDiatWHyp_", dDDiatWH
write (1,*)" Derivative  13 _dNDiatWHyp_", dNDiatWH
write (1,*)" Derivative  14 _dPDiatWHyp_", dPDiatWH
write (1,*)" Derivative  15 _dDGrenWHyp_", dDGrenWH
write (1,*)" Derivative  16 _dNGrenWHyp_", dNGrenWH
write (1,*)" Derivative  17 _dPGrenWHyp_", dPGrenWH
write (1,*)" Derivative  18 _dDBlueWHyp_", dDBlueWH
write (1,*)" Derivative  19 _dNBlueWHyp_", dNBlueWH
write (1,*)" Derivative  20 _dPBlueWHyp_", dPBlueWH
write (1,*)" Derivative  21 _dDZooHyp_", dDZooH
write (1,*)" Derivative  22 _dNZooHyp_", dNZooH
write (1,*)" Derivative  23 _dPZooHyp_", dPZooH
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
write (1,*)" Derivative  55 _dVegHeight_", dVegHe
write (1,*)" Derivative  56 _dDBent_", dDBent
write (1,*)" Derivative  57 _dNBent_", dNBent
write (1,*)" Derivative  58 _dPBent_", dPBent
write (1,*)" Derivative  59 _dDepthWM_", dDepthWM
write (1,*)" Derivative  60 _dNH4WM_", dNH4WM
write (1,*)" Derivative  61 _dNO3WM_", dNO3WM
write (1,*)" Derivative  62 _dPO4WM_", dPO4WM
write (1,*)" Derivative  63 _dPAIMWM_", dPAIMWM
write (1,*)" Derivative  64 _dSiO2WM_", dSiO2WM
write (1,*)" Derivative  65 _dO2WM_", dO2WM
write (1,*)" Derivative  66 _dDDetWM_", dDDetWM
write (1,*)" Derivative  67 _dNDetWM_", dNDetWM
write (1,*)" Derivative  68 _dPDetWM_", dPDetWM
write (1,*)" Derivative  69 _dSiDetWM_", dSiDetWM
write (1,*)" Derivative  70 _dDIMWM_", dDIMWM
write (1,*)" Derivative  71 _dDDiatWM_", dDDiatWM
write (1,*)" Derivative  72 _dNDiatWM_", dNDiatWM
write (1,*)" Derivative  73 _dPDiatWM_", dPDiatWM
write (1,*)" Derivative  74 _dDGrenWM_", dDGrenWM
write (1,*)" Derivative  75 _dNGrenWM_", dNGrenWM
write (1,*)" Derivative  76 _dPGrenWM_", dPGrenWM
write (1,*)" Derivative  77 _dDBlueWM_", dDBlueWM
write (1,*)" Derivative  78 _dNBlueWM_", dNBlueWM
write (1,*)" Derivative  79 _dPBlueWM_", dPBlueWM
write (1,*)" Derivative  80 _dDZooM_", dDZooM
write (1,*)" Derivative  81 _dNZooM_", dNZooM
write (1,*)" Derivative  82 _dPZooM_", dPZooM
write (1,*)" Derivative  83 _dNH4SM_", dNH4SM
write (1,*)" Derivative  84 _dNO3SM_", dNO3SM
write (1,*)" Derivative  85 _dPO4SM_", dPO4SM
write (1,*)" Derivative  86 _dPAIMSM_", dPAIMSM
write (1,*)" Derivative  87 _dDDetSM_", dDDetSM
write (1,*)" Derivative  88 _dNDetSM_", dNDetSM
write (1,*)" Derivative  89 _dPDetSM_", dPDetSM
write (1,*)" Derivative  90 _dSiDetSM_", dSiDetSM
write (1,*)" Derivative  91 _dDHumSM_", dDHumSM
write (1,*)" Derivative  92 _dNHumSM_", dNHumSM
write (1,*)" Derivative  93 _dPHumSM_", dPHumSM
write (1,*)" Derivative  94 _dDIMSM_", dDIMSM
write (1,*)" Derivative  95 _dDRootPhra_", dDRoPhra
write (1,*)" Derivative  96 _dDShootPhra_", dDShPhra
write (1,*)" Derivative  97 _dNRootPhra_", dNRoPhra
write (1,*)" Derivative  98 _dNShootPhra_", dNShPhra
write (1,*)" Derivative  99 _dPRootPhra_", dPRoPhra
write (1,*)" Derivative  100 _dPShootPhra_", dPShPhra
write (1,*)" Derivative  101 _dNH4WEpi_", dNH4WE
write (1,*)" Derivative  102 _dNO3WEpi_", dNO3WE
write (1,*)" Derivative  103 _dPO4WEpi_", dPO4WE
write (1,*)" Derivative  104 _dPAIMWEpi_", dPAIMWE
write (1,*)" Derivative  105 _dSiO2WEpi_", dSiO2WE
write (1,*)" Derivative  106 _dO2WEpi_", dO2WE
write (1,*)" Derivative  107 _dDDetWEpi_", dDDetWE
write (1,*)" Derivative  108 _dNDetWEpi_", dNDetWE
write (1,*)" Derivative  109 _dPDetWEpi_", dPDetWE
write (1,*)" Derivative  110 _dSiDetWEpi_", dSiDetWE
write (1,*)" Derivative  111 _dDIMWEpi_", dDIMWE
write (1,*)" Derivative  112 _dDDiatWEpi_", dDDiatWE
write (1,*)" Derivative  113 _dNDiatWEpi_", dNDiatWE
write (1,*)" Derivative  114 _dPDiatWEpi_", dPDiatWE
write (1,*)" Derivative  115 _dDGrenWEpi_", dDGrenWE
write (1,*)" Derivative  116 _dNGrenWEpi_", dNGrenWE
write (1,*)" Derivative  117 _dPGrenWEpi_", dPGrenWE
write (1,*)" Derivative  118 _dDBlueWEpi_", dDBlueWE
write (1,*)" Derivative  119 _dNBlueWEpi_", dNBlueWE
write (1,*)" Derivative  120 _dPBlueWEpi_", dPBlueWE
write (1,*)" Derivative  121 _dDZooEpi_", dDZooE
write (1,*)" Derivative  122 _dNZooEpi_", dNZooE
write (1,*)" Derivative  123 _dPZooEpi_", dPZooE
write (1,*)" Derivative  124 _dDExtTotT_", dDExTotT
write (1,*)" Derivative  125 _dNExtTotT_", dNExTotT
write (1,*)" Derivative  126 _dPExtTotT_", dPExTotT
write (1,*)" Derivative  127 _dSiExtTotT_", dSiETotT
write (1,*)" Derivative  128 _dO2ExtTotT_", dO2ETotT
     counter1 = 1
Close (1)
End If
!
if (counter2 == 0) then
open (unit=2,file="staterep.txt",action="write",status="replace")
write (2,*) "STEP ", &
&"sDepthW ", &
&"sNH4WH ", &
&"sNO3WH ", &
&"sPO4WH ", &
&"sPAIMWH ", &
&"sSiO2WH ", &
&"sO2WH ", &
&"sDDetWH ", &
&"sNDetWH ", &
&"sPDetWH ", &
&"sSiDetWH ", &
&"sDIMWH ", &
&"sDDiatWH ", &
&"sNDiatWH ", &
&"sPDiatWH ", &
&"sDGrenWH ", &
&"sNGrenWH ", &
&"sPGrenWH ", &
&"sDBlueWH ", &
&"sNBlueWH ", &
&"sPBlueWH ", &
&"sDZooH ", &
&"sNZooH ", &
&"sPZooH ", &
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
&"sVegHe ", &
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
&"sDRoPhra ", &
&"sDShPhra ", &
&"sNRoPhra ", &
&"sNShPhra ", &
&"sPRoPhra ", &
&"sPShPhra ", &
&"sNH4WE ", &
&"sNO3WE ", &
&"sPO4WE ", &
&"sPAIMWE ", &
&"sSiO2WE ", &
&"sO2WE ", &
&"sDDetWE ", &
&"sNDetWE ", &
&"sPDetWE ", &
&"sSiDetWE ", &
&"sDIMWE ", &
&"sDDiatWE ", &
&"sNDiatWE ", &
&"sPDiatWE ", &
&"sDGrenWE ", &
&"sNGrenWE ", &
&"sPGrenWE ", &
&"sDBlueWE ", &
&"sNBlueWE ", &
&"sPBlueWE ", &
&"sDZooE ", &
&"sNZooE ", &
&"sPZooE ", &
&"sDExTotT ", &
&"sNExTotT ", &
&"sPExTotT ", &
&"sSiETotT ", &
&"sO2ETotT "
     counter2 = 1
Close (2)
End If
!
if (sTime > counter3) then
OPEN (3, FILE = 'staterep.txt', ACCESS = 'APPEND')
write(3,*) sTime, &
&sDepthW , &
&sNH4WH , &
&sNO3WH , &
&sPO4WH , &
&sPAIMWH , &
&sSiO2WH , &
&sO2WH , &
&sDDetWH , &
&sNDetWH , &
&sPDetWH , &
&sSiDetWH , &
&sDIMWH , &
&sDDiatWH , &
&sNDiatWH , &
&sPDiatWH , &
&sDGrenWH , &
&sNGrenWH , &
&sPGrenWH , &
&sDBlueWH , &
&sNBlueWH , &
&sPBlueWH , &
&sDZooH , &
&sNZooH , &
&sPZooH , &
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
&sVegHe , &
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
&sDRoPhra , &
&sDShPhra , &
&sNRoPhra , &
&sNShPhra , &
&sPRoPhra , &
&sPShPhra , &
&sNH4WE , &
&sNO3WE , &
&sPO4WE , &
&sPAIMWE , &
&sSiO2WE , &
&sO2WE , &
&sDDetWE , &
&sNDetWE , &
&sPDetWE , &
&sSiDetWE , &
&sDIMWE , &
&sDDiatWE , &
&sNDiatWE , &
&sPDiatWE , &
&sDGrenWE , &
&sNGrenWE , &
&sPGrenWE , &
&sDBlueWE , &
&sNBlueWE , &
&sPBlueWE , &
&sDZooE , &
&sNZooE , &
&sPZooE , &
&sDExTotT , &
&sNExTotT , &
&sPExTotT , &
&sSiETotT , &
&sO2ETotT 
Close (3)
     counter3 = counter3 +  1 
End If
!
!
!     /* ==============================  */
!     /* integration calls               */
!     /* ==============================  */
      D0sNH4WH         	 =  dNH4WH
      D0sNO3WH         	 =  dNO3WH
      D0sPO4WH         	 =  dPO4WH
      D0sPAIMWH        	 =  dPAIMWH
      D0sSiO2WH        	 =  dSiO2WH
      D0sO2WH          	 =  dO2WH
      D0sDDetWH        	 =  dDDetWH
      D0sNDetWH        	 =  dNDetWH
      D0sPDetWH        	 =  dPDetWH
      D0sSiDetWH       	 =  dSiDetWH
      D0sDIMWH         	 =  dDIMWH
      D0sDDiatWH       	 =  dDDiatWH
      D0sNDiatWH       	 =  dNDiatWH
      D0sPDiatWH       	 =  dPDiatWH
      D0sDGrenWH       	 =  dDGrenWH
      D0sNGrenWH       	 =  dNGrenWH
      D0sPGrenWH       	 =  dPGrenWH
      D0sDBlueWH       	 =  dDBlueWH
      D0sNBlueWH       	 =  dNBlueWH
      D0sPBlueWH       	 =  dPBlueWH
      D0sDZooH         	 =  dDZooH
      D0sNZooH         	 =  dNZooH
      D0sPZooH         	 =  dPZooH
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
      D0sVegHe         	 =  dVegHe
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
      D0sDRoPhra       	 =  dDRoPhra
      D0sDShPhra       	 =  dDShPhra
      D0sNRoPhra       	 =  dNRoPhra
      D0sNShPhra       	 =  dNShPhra
      D0sPRoPhra       	 =  dPRoPhra
      D0sPShPhra       	 =  dPShPhra
      D0sNH4WE         	 =  dNH4WE
      D0sNO3WE         	 =  dNO3WE
      D0sPO4WE         	 =  dPO4WE
      D0sPAIMWE        	 =  dPAIMWE
      D0sSiO2WE        	 =  dSiO2WE
      D0sO2WE          	 =  dO2WE
      D0sDDetWE        	 =  dDDetWE
      D0sNDetWE        	 =  dNDetWE
      D0sPDetWE        	 =  dPDetWE
      D0sSiDetWE       	 =  dSiDetWE
      D0sDIMWE         	 =  dDIMWE
      D0sDDiatWE       	 =  dDDiatWE
      D0sNDiatWE       	 =  dNDiatWE
      D0sPDiatWE       	 =  dPDiatWE
      D0sDGrenWE       	 =  dDGrenWE
      D0sNGrenWE       	 =  dNGrenWE
      D0sPGrenWE       	 =  dPGrenWE
      D0sDBlueWE       	 =  dDBlueWE
      D0sNBlueWE       	 =  dNBlueWE
      D0sPBlueWE       	 =  dPBlueWE
      D0sDZooE         	 =  dDZooE
      D0sNZooE         	 =  dNZooE
      D0sPZooE         	 =  dPZooE
      D0sDExTotT       	 =  dDExTotT
      D0sNExTotT       	 =  dNExTotT
      D0sPExTotT       	 =  dPExTotT
      D0sSiETotT       	 =  dSiETotT
      D0sO2ETotT       	 =  dO2ETotT

!   *****     DUPROL code ends here    *****

         fl  ( ID0sNH4WH              	) = D0sNH4WH              
         fl  ( ID0sNO3WH              	) = D0sNO3WH              
         fl  ( ID0sPO4WH              	) = D0sPO4WH              
         fl  ( ID0sPAIMWH             	) = D0sPAIMWH             
         fl  ( ID0sSiO2WH             	) = D0sSiO2WH             
         fl  ( ID0sO2WH               	) = D0sO2WH               
         fl  ( ID0sDDetWH             	) = D0sDDetWH             
         fl  ( ID0sNDetWH             	) = D0sNDetWH             
         fl  ( ID0sPDetWH             	) = D0sPDetWH             
         fl  ( ID0sSiDetWH            	) = D0sSiDetWH            
         fl  ( ID0sDIMWH              	) = D0sDIMWH              
         fl  ( ID0sDDiatWH            	) = D0sDDiatWH            
         fl  ( ID0sNDiatWH            	) = D0sNDiatWH            
         fl  ( ID0sPDiatWH            	) = D0sPDiatWH            
         fl  ( ID0sDGrenWH            	) = D0sDGrenWH            
         fl  ( ID0sNGrenWH            	) = D0sNGrenWH            
         fl  ( ID0sPGrenWH            	) = D0sPGrenWH            
         fl  ( ID0sDBlueWH            	) = D0sDBlueWH            
         fl  ( ID0sNBlueWH            	) = D0sNBlueWH            
         fl  ( ID0sPBlueWH            	) = D0sPBlueWH            
         fl  ( ID0sDZooH              	) = D0sDZooH              
         fl  ( ID0sNZooH              	) = D0sNZooH              
         fl  ( ID0sPZooH              	) = D0sPZooH              
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
         fl  ( ID0sVegHe              	) = D0sVegHe              
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
         fl  ( ID0sDRoPhra            	) = D0sDRoPhra            
         fl  ( ID0sDShPhra            	) = D0sDShPhra            
         fl  ( ID0sNRoPhra            	) = D0sNRoPhra            
         fl  ( ID0sNShPhra            	) = D0sNShPhra            
         fl  ( ID0sPRoPhra            	) = D0sPRoPhra            
         fl  ( ID0sPShPhra            	) = D0sPShPhra            
         fl  ( ID0sNH4WE              	) = D0sNH4WE              
         fl  ( ID0sNO3WE              	) = D0sNO3WE              
         fl  ( ID0sPO4WE              	) = D0sPO4WE              
         fl  ( ID0sPAIMWE             	) = D0sPAIMWE             
         fl  ( ID0sSiO2WE             	) = D0sSiO2WE             
         fl  ( ID0sO2WE               	) = D0sO2WE               
         fl  ( ID0sDDetWE             	) = D0sDDetWE             
         fl  ( ID0sNDetWE             	) = D0sNDetWE             
         fl  ( ID0sPDetWE             	) = D0sPDetWE             
         fl  ( ID0sSiDetWE            	) = D0sSiDetWE            
         fl  ( ID0sDIMWE              	) = D0sDIMWE              
         fl  ( ID0sDDiatWE            	) = D0sDDiatWE            
         fl  ( ID0sNDiatWE            	) = D0sNDiatWE            
         fl  ( ID0sPDiatWE            	) = D0sPDiatWE            
         fl  ( ID0sDGrenWE            	) = D0sDGrenWE            
         fl  ( ID0sNGrenWE            	) = D0sNGrenWE            
         fl  ( ID0sPGrenWE            	) = D0sPGrenWE            
         fl  ( ID0sDBlueWE            	) = D0sDBlueWE            
         fl  ( ID0sNBlueWE            	) = D0sNBlueWE            
         fl  ( ID0sPBlueWE            	) = D0sPBlueWE            
         fl  ( ID0sDZooE              	) = D0sDZooE              
         fl  ( ID0sNZooE              	) = D0sNZooE              
         fl  ( ID0sPZooE              	) = D0sPZooE              
         fl  ( ID0sDExTotT            	) = D0sDExTotT            
         fl  ( ID0sNExTotT            	) = D0sNExTotT            
         fl  ( ID0sPExTotT            	) = D0sPExTotT            
         fl  ( ID0sSiETotT            	) = D0sSiETotT            
         fl  ( ID0sO2ETotT            	) = D0sO2ETotT            
         pmsa( ipnt( 658) ) = sDepthW
         pmsa( ipnt( 659) ) = sTime
         pmsa( ipnt( 660) ) = DaysInY
         pmsa( ipnt( 661) ) = aInlSrat
         pmsa( ipnt( 662) ) = uDptMix
         pmsa( ipnt( 663) ) = uDptWE
         pmsa( ipnt( 664) ) = uDptWH
         pmsa( ipnt( 665) ) = MassH
         pmsa( ipnt( 666) ) = MassE
         pmsa( ipnt( 667) ) = MassWM
         pmsa( ipnt( 668) ) = oNH4WH
         pmsa( ipnt( 669) ) = oNO3WH
         pmsa( ipnt( 670) ) = oPO4WH
         pmsa( ipnt( 671) ) = oPAIMWH
         pmsa( ipnt( 672) ) = oSiO2WH
         pmsa( ipnt( 673) ) = oO2WH
         pmsa( ipnt( 674) ) = oDDtWH
         pmsa( ipnt( 675) ) = oNDtWH
         pmsa( ipnt( 676) ) = oPDtWH
         pmsa( ipnt( 677) ) = oSiDtWH
         pmsa( ipnt( 678) ) = oDIMWH
         pmsa( ipnt( 679) ) = oDDiWH
         pmsa( ipnt( 680) ) = oNDiWH
         pmsa( ipnt( 681) ) = oPDiWH
         pmsa( ipnt( 682) ) = oDGrenWH
         pmsa( ipnt( 683) ) = oNGrenWH
         pmsa( ipnt( 684) ) = oPGrenWH
         pmsa( ipnt( 685) ) = oDBlueWH
         pmsa( ipnt( 686) ) = oNBlueWH
         pmsa( ipnt( 687) ) = oPBlueWH
         pmsa( ipnt( 688) ) = oDZooH
         pmsa( ipnt( 689) ) = oNZooH
         pmsa( ipnt( 690) ) = oPZooH
         pmsa( ipnt( 691) ) = oNH4WM
         pmsa( ipnt( 692) ) = oNO3WM
         pmsa( ipnt( 693) ) = oPO4WM
         pmsa( ipnt( 694) ) = oPAIMWM
         pmsa( ipnt( 695) ) = oSiO2WM
         pmsa( ipnt( 696) ) = oO2WM
         pmsa( ipnt( 697) ) = oDDtWM
         pmsa( ipnt( 698) ) = oNDtWM
         pmsa( ipnt( 699) ) = oPDtWM
         pmsa( ipnt( 700) ) = oSiDtWM
         pmsa( ipnt( 701) ) = oDIMWM
         pmsa( ipnt( 702) ) = oDDiWM
         pmsa( ipnt( 703) ) = oNDiWM
         pmsa( ipnt( 704) ) = oPDiWM
         pmsa( ipnt( 705) ) = oDGrenWM
         pmsa( ipnt( 706) ) = oNGrenWM
         pmsa( ipnt( 707) ) = oPGrenWM
         pmsa( ipnt( 708) ) = oDBlueWM
         pmsa( ipnt( 709) ) = oNBlueWM
         pmsa( ipnt( 710) ) = oPBlueWM
         pmsa( ipnt( 711) ) = oDZooM
         pmsa( ipnt( 712) ) = oNZooM
         pmsa( ipnt( 713) ) = oPZooM
         pmsa( ipnt( 714) ) = oNH4WE
         pmsa( ipnt( 715) ) = oNO3WE
         pmsa( ipnt( 716) ) = oPO4WE
         pmsa( ipnt( 717) ) = oPAIMWE
         pmsa( ipnt( 718) ) = oSiO2WE
         pmsa( ipnt( 719) ) = oO2WE
         pmsa( ipnt( 720) ) = oDDtWE
         pmsa( ipnt( 721) ) = oNDtWE
         pmsa( ipnt( 722) ) = oPDtWE
         pmsa( ipnt( 723) ) = oSiDtWE
         pmsa( ipnt( 724) ) = oDIMWE
         pmsa( ipnt( 725) ) = oDDiWE
         pmsa( ipnt( 726) ) = oNDiWE
         pmsa( ipnt( 727) ) = oPDiWE
         pmsa( ipnt( 728) ) = oDGrenWE
         pmsa( ipnt( 729) ) = oNGrenWE
         pmsa( ipnt( 730) ) = oPGrenWE
         pmsa( ipnt( 731) ) = oDBlueWE
         pmsa( ipnt( 732) ) = oNBlueWE
         pmsa( ipnt( 733) ) = oPBlueWE
         pmsa( ipnt( 734) ) = oDZooE
         pmsa( ipnt( 735) ) = oNZooE
         pmsa( ipnt( 736) ) = oPZooE
         pmsa( ipnt( 737) ) = TimeYars
         pmsa( ipnt( 738) ) = Day
         pmsa( ipnt( 739) ) = Years
         pmsa( ipnt( 740) ) = uVeHeght
         pmsa( ipnt( 741) ) = aVeHeght
         pmsa( ipnt( 742) ) = uVeHehtH
         pmsa( ipnt( 743) ) = uVeHehtE
         pmsa( ipnt( 744) ) = aDpt2egH
         pmsa( ipnt( 745) ) = aDpt1egH
         pmsa( ipnt( 746) ) = aDpt2egE
         pmsa( ipnt( 747) ) = aDpt1egE
         pmsa( ipnt( 748) ) = uVegHL
         pmsa( ipnt( 749) ) = uVegHLS
         pmsa( ipnt( 750) ) = uVeghhtE
         pmsa( ipnt( 751) ) = uVeghhtH
         pmsa( ipnt( 752) ) = uDVegH
         pmsa( ipnt( 753) ) = uDVegE
         pmsa( ipnt( 754) ) = uVegSade
         pmsa( ipnt( 755) ) = aExtPytH
         pmsa( ipnt( 756) ) = aExtDtH
         pmsa( ipnt( 757) ) = aExtIMH
         pmsa( ipnt( 758) ) = aExCofOH
         pmsa( ipnt( 759) ) = aExtPytE
         pmsa( ipnt( 760) ) = aExtDtE
         pmsa( ipnt( 761) ) = aExtIME
         pmsa( ipnt( 762) ) = aExCofOE
         pmsa( ipnt( 763) ) = aTmE
         pmsa( ipnt( 764) ) = aTmH
         pmsa( ipnt( 765) ) = uTmE
         pmsa( ipnt( 766) ) = uTmH
         pmsa( ipnt( 767) ) = uTmBot
         pmsa( ipnt( 768) ) = uLAT
         pmsa( ipnt( 769) ) = uSoDeAng
         pmsa( ipnt( 770) ) = uSunRise
         pmsa( ipnt( 771) ) = uSunSet
         pmsa( ipnt( 772) ) = uSunHour
         pmsa( ipnt( 773) ) = uSunPath
         pmsa( ipnt( 774) ) = uTrans
         pmsa( ipnt( 775) ) = ufDay
         pmsa( ipnt( 776) ) = uLDay
         pmsa( ipnt( 777) ) = uLOut
         pmsa( ipnt( 778) ) = uLPARurf
         pmsa( ipnt( 779) ) = aLPRBtEO
         pmsa( ipnt( 780) ) = aLPRBtHO
         pmsa( ipnt( 781) ) = aLPR1egE
         pmsa( ipnt( 782) ) = aLPR1egH
         pmsa( ipnt( 783) ) = aLPVeght
         pmsa( ipnt( 784) ) = uTmVeAve
         pmsa( ipnt( 785) ) = uFunTVeg
         pmsa( ipnt( 786) ) = uFumPVeg
         pmsa( ipnt( 787) ) = uhLVeg
         pmsa( ipnt( 788) ) = aLPIVeg
         pmsa( ipnt( 789) ) = aSpring
         pmsa( ipnt( 790) ) = aTiWiVeg
         pmsa( ipnt( 791) ) = aTiInVeg
         pmsa( ipnt( 792) ) = aVSum
         pmsa( ipnt( 793) ) = aVWin
         pmsa( ipnt( 794) ) = aDaysSuV
         pmsa( ipnt( 795) ) = aDaysWiV
         pmsa( ipnt( 796) ) = uDaWiVeg
         pmsa( ipnt( 797) ) = aDaInVeg
         pmsa( ipnt( 798) ) = bfRtVeg
         pmsa( ipnt( 799) ) = bfShVeg
         pmsa( ipnt( 800) ) = aDShVeg
         pmsa( ipnt( 801) ) = aDEerVeg
         pmsa( ipnt( 802) ) = aDFoaVeg
         pmsa( ipnt( 803) ) = bfSubVeg
         pmsa( ipnt( 804) ) = aDSubVeg
         pmsa( ipnt( 805) ) = aExtVegH
         pmsa( ipnt( 806) ) = aExtCefH
         pmsa( ipnt( 807) ) = aExtVegE
         pmsa( ipnt( 808) ) = aExtCefE
         pmsa( ipnt( 809) ) = aLPARotE
         pmsa( ipnt( 810) ) = uLPRSrfH
         pmsa( ipnt( 811) ) = aLPR2egE
         pmsa( ipnt( 812) ) = aLPR2egH
         pmsa( ipnt( 813) ) = uVWind
         pmsa( ipnt( 814) ) = aStrat
         pmsa( ipnt( 815) ) = uQISeson
         pmsa( ipnt( 816) ) = uQEvSnus
         pmsa( ipnt( 817) ) = aQEv
         pmsa( ipnt( 818) ) = uQEv
         pmsa( ipnt( 819) ) = uQInEtra
         pmsa( ipnt( 820) ) = uQIn
         pmsa( ipnt( 821) ) = uQOtEtra
         pmsa( ipnt( 822) ) = uQOutE
         pmsa( ipnt( 823) ) = uQOutH
         pmsa( ipnt( 824) ) = uQDilE
         pmsa( ipnt( 825) ) = uQDilH
         pmsa( ipnt( 826) ) = ukDilE
         pmsa( ipnt( 827) ) = ukDilH
         pmsa( ipnt( 828) ) = ukDilatE
         pmsa( ipnt( 829) ) = ukDilatH
         pmsa( ipnt( 830) ) = ukOutE
         pmsa( ipnt( 831) ) = ukOutH
         pmsa( ipnt( 832) ) = uTauWatE
         pmsa( ipnt( 833) ) = uTauWatH
         pmsa( ipnt( 834) ) = uTauWat
         pmsa( ipnt( 835) ) = uTaSustH
         pmsa( ipnt( 836) ) = uFiJvMH
         pmsa( ipnt( 837) ) = uFiJvEH
         pmsa( ipnt( 838) ) = uFivEcrH
         pmsa( ipnt( 839) ) = uFiAdMH
         pmsa( ipnt( 840) ) = uFiAdEH
         pmsa( ipnt( 841) ) = uFidEcrH
         pmsa( ipnt( 842) ) = uPiscMH
         pmsa( ipnt( 843) ) = uPiscEH
         pmsa( ipnt( 844) ) = uPicEcrH
         pmsa( ipnt( 845) ) = vTranptW
         pmsa( ipnt( 846) ) = vTranHEW
         pmsa( ipnt( 847) ) = vDeltaWH
         pmsa( ipnt( 848) ) = vDeltaWE
         pmsa( ipnt( 849) ) = afVolHE
         pmsa( ipnt( 850) ) = wDAdvIMW
         pmsa( ipnt( 851) ) = wPAdvO4W
         pmsa( ipnt( 852) ) = wPAdvIMW
         pmsa( ipnt( 853) ) = wNAdvH4W
         pmsa( ipnt( 854) ) = wNAdvO3W
         pmsa( ipnt( 855) ) = wSidvO2W
         pmsa( ipnt( 856) ) = wO2AdvW
         pmsa( ipnt( 857) ) = wDAdvDtW
         pmsa( ipnt( 858) ) = wPAdvDtW
         pmsa( ipnt( 859) ) = wNAdvDtW
         pmsa( ipnt( 860) ) = wSiAdDtW
         pmsa( ipnt( 861) ) = wDAdvDiW
         pmsa( ipnt( 862) ) = wPAdvDiW
         pmsa( ipnt( 863) ) = wNAdvDiW
         pmsa( ipnt( 864) ) = wSiAdDiW
         pmsa( ipnt( 865) ) = wDAvGenW
         pmsa( ipnt( 866) ) = wPAvGenW
         pmsa( ipnt( 867) ) = wNAvGenW
         pmsa( ipnt( 868) ) = wDAvBueW
         pmsa( ipnt( 869) ) = wPAvBueW
         pmsa( ipnt( 870) ) = wNAvBueW
         pmsa( ipnt( 871) ) = wDAdvooW
         pmsa( ipnt( 872) ) = wPAdvooW
         pmsa( ipnt( 873) ) = wNAdvooW
         pmsa( ipnt( 874) ) = wDAIMWM
         pmsa( ipnt( 875) ) = wPAPO4WM
         pmsa( ipnt( 876) ) = wPAAIMWM
         pmsa( ipnt( 877) ) = wNANH4WM
         pmsa( ipnt( 878) ) = wNANO3WM
         pmsa( ipnt( 879) ) = wSiAO2WM
         pmsa( ipnt( 880) ) = wO2AWM
         pmsa( ipnt( 881) ) = wDADeWM
         pmsa( ipnt( 882) ) = wPADeWM
         pmsa( ipnt( 883) ) = wNADeWM
         pmsa( ipnt( 884) ) = wSiADeWM
         pmsa( ipnt( 885) ) = wDADiWM
         pmsa( ipnt( 886) ) = wPADiWM
         pmsa( ipnt( 887) ) = wNADiWM
         pmsa( ipnt( 888) ) = wSiADiWM
         pmsa( ipnt( 889) ) = wDAGWM
         pmsa( ipnt( 890) ) = wPAGWM
         pmsa( ipnt( 891) ) = wNAGWM
         pmsa( ipnt( 892) ) = wDABWM
         pmsa( ipnt( 893) ) = wPABWM
         pmsa( ipnt( 894) ) = wNABWM
         pmsa( ipnt( 895) ) = wDAZWM
         pmsa( ipnt( 896) ) = wPAZWM
         pmsa( ipnt( 897) ) = wNAZWM
         pmsa( ipnt( 898) ) = uExMaxW
         pmsa( ipnt( 899) ) = akExWH
         pmsa( ipnt( 900) ) = akExWE
         pmsa( ipnt( 901) ) = wDExIMWH
         pmsa( ipnt( 902) ) = wPExP4WH
         pmsa( ipnt( 903) ) = wPExAMWH
         pmsa( ipnt( 904) ) = wNExN4WH
         pmsa( ipnt( 905) ) = wNExN3WH
         pmsa( ipnt( 906) ) = wSixS2WH
         pmsa( ipnt( 907) ) = wO2ExWH
         pmsa( ipnt( 908) ) = wDExDtWH
         pmsa( ipnt( 909) ) = wPExDtWH
         pmsa( ipnt( 910) ) = wNExDtWH
         pmsa( ipnt( 911) ) = wSiExtWH
         pmsa( ipnt( 912) ) = wDExDiWH
         pmsa( ipnt( 913) ) = wPExDiWH
         pmsa( ipnt( 914) ) = wNExDiWH
         pmsa( ipnt( 915) ) = wSiExiWH
         pmsa( ipnt( 916) ) = wDEGrnWH
         pmsa( ipnt( 917) ) = wPEGrnWH
         pmsa( ipnt( 918) ) = wNEGrnWH
         pmsa( ipnt( 919) ) = wDEBleWH
         pmsa( ipnt( 920) ) = wPEBleWH
         pmsa( ipnt( 921) ) = wNEBleWH
         pmsa( ipnt( 922) ) = wDExZoWH
         pmsa( ipnt( 923) ) = wPExZoWH
         pmsa( ipnt( 924) ) = wNExZoWH
         pmsa( ipnt( 925) ) = wDExIMWE
         pmsa( ipnt( 926) ) = wPExP4WE
         pmsa( ipnt( 927) ) = wPExAMWE
         pmsa( ipnt( 928) ) = wNExN4WE
         pmsa( ipnt( 929) ) = wNExN3WE
         pmsa( ipnt( 930) ) = wSixS2WE
         pmsa( ipnt( 931) ) = wO2ExWE
         pmsa( ipnt( 932) ) = wDExDtWE
         pmsa( ipnt( 933) ) = wPExDtWE
         pmsa( ipnt( 934) ) = wNExDtWE
         pmsa( ipnt( 935) ) = wSiExtWE
         pmsa( ipnt( 936) ) = wDExDiWE
         pmsa( ipnt( 937) ) = wPExDiWE
         pmsa( ipnt( 938) ) = wNExDiWE
         pmsa( ipnt( 939) ) = wSiExiWE
         pmsa( ipnt( 940) ) = wDEGrnWE
         pmsa( ipnt( 941) ) = wPEGrnWE
         pmsa( ipnt( 942) ) = wNEGrnWE
         pmsa( ipnt( 943) ) = wDEBleWE
         pmsa( ipnt( 944) ) = wPEBleWE
         pmsa( ipnt( 945) ) = wNEBleWE
         pmsa( ipnt( 946) ) = wDExZoWE
         pmsa( ipnt( 947) ) = wPExZoWE
         pmsa( ipnt( 948) ) = wNExZoWE
         pmsa( ipnt( 949) ) = akExMH
         pmsa( ipnt( 950) ) = akExLH
         pmsa( ipnt( 951) ) = oDPhytWH
         pmsa( ipnt( 952) ) = oPPhytWH
         pmsa( ipnt( 953) ) = oNPhytWH
         pmsa( ipnt( 954) ) = aDPhytS
         pmsa( ipnt( 955) ) = aPPhytS
         pmsa( ipnt( 956) ) = aNPhytS
         pmsa( ipnt( 957) ) = oDPhytWE
         pmsa( ipnt( 958) ) = oDOMWE
         pmsa( ipnt( 959) ) = oDOMWH
         pmsa( ipnt( 960) ) = oTDOMW
         pmsa( ipnt( 961) ) = oDSestWH
         pmsa( ipnt( 962) ) = oPOMWH
         pmsa( ipnt( 963) ) = oPSestWH
         pmsa( ipnt( 964) ) = oPInogWH
         pmsa( ipnt( 965) ) = oPTotWH
         pmsa( ipnt( 966) ) = oNDissWE
         pmsa( ipnt( 967) ) = oNDissWH
         pmsa( ipnt( 968) ) = oNOMWH
         pmsa( ipnt( 969) ) = oNSestWH
         pmsa( ipnt( 970) ) = oNkjWH
         pmsa( ipnt( 971) ) = oNTotWH
         pmsa( ipnt( 972) ) = bPorS
         pmsa( ipnt( 973) ) = bPorCorS
         pmsa( ipnt( 974) ) = aDTotS
         pmsa( ipnt( 975) ) = aRhoTotS
         pmsa( ipnt( 976) ) = aRhSoidS
         pmsa( ipnt( 977) ) = afDTotS
         pmsa( ipnt( 978) ) = afDOrgS
         pmsa( ipnt( 979) ) = afDtS
         pmsa( ipnt( 980) ) = afDtTotS
         pmsa( ipnt( 981) ) = aPInorgS
         pmsa( ipnt( 982) ) = aPTtAilS
         pmsa( ipnt( 983) ) = aPTotS
         pmsa( ipnt( 984) ) = afPInrgS
         pmsa( ipnt( 985) ) = afPTotS
         pmsa( ipnt( 986) ) = afPO4S
         pmsa( ipnt( 987) ) = oPO4S
         pmsa( ipnt( 988) ) = aNDissS
         pmsa( ipnt( 989) ) = aNkAvilS
         pmsa( ipnt( 990) ) = aNkjS
         pmsa( ipnt( 991) ) = aNTtAilS
         pmsa( ipnt( 992) ) = aNTotS
         pmsa( ipnt( 993) ) = afNInrgS
         pmsa( ipnt( 994) ) = afNTotS
         pmsa( ipnt( 995) ) = oNO3S
         pmsa( ipnt( 996) ) = oNH4S
         pmsa( ipnt( 997) ) = oNDissS
         pmsa( ipnt( 998) ) = rPDIMWH
         pmsa( ipnt( 999) ) = rPDIMS
         pmsa( ipnt( 1000) ) = rPDDtWH
         pmsa( ipnt( 1001) ) = rNDDtWH
         pmsa( ipnt( 1002) ) = rSiDDtWH
         pmsa( ipnt( 1003) ) = rPDHumS
         pmsa( ipnt( 1004) ) = rNDHumS
         pmsa( ipnt( 1005) ) = rPDDtS
         pmsa( ipnt( 1006) ) = rNDDtS
         pmsa( ipnt( 1007) ) = rSiDDtS
         pmsa( ipnt( 1008) ) = oDPhytWM
         pmsa( ipnt( 1009) ) = oPPhytWM
         pmsa( ipnt( 1010) ) = oNPhytWM
         pmsa( ipnt( 1011) ) = oSiDiWM
         pmsa( ipnt( 1012) ) = oDOMWM
         pmsa( ipnt( 1013) ) = oDSestWM
         pmsa( ipnt( 1014) ) = oPOMWM
         pmsa( ipnt( 1015) ) = oPSestWM
         pmsa( ipnt( 1016) ) = oPInogWM
         pmsa( ipnt( 1017) ) = oPTotWM
         pmsa( ipnt( 1018) ) = oNDissWM
         pmsa( ipnt( 1019) ) = oNOMWM
         pmsa( ipnt( 1020) ) = oNSestWM
         pmsa( ipnt( 1021) ) = oNkjWM
         pmsa( ipnt( 1022) ) = oNTotWM
         pmsa( ipnt( 1023) ) = bPorSM
         pmsa( ipnt( 1024) ) = bPorCrSM
         pmsa( ipnt( 1025) ) = aDTotSM
         pmsa( ipnt( 1026) ) = aRhoTtSM
         pmsa( ipnt( 1027) ) = aRhSodSM
         pmsa( ipnt( 1028) ) = afDTotSM
         pmsa( ipnt( 1029) ) = afDOrgSM
         pmsa( ipnt( 1030) ) = afDtSM
         pmsa( ipnt( 1031) ) = afDtTtSM
         pmsa( ipnt( 1032) ) = aPInogSM
         pmsa( ipnt( 1033) ) = aPTAvlSM
         pmsa( ipnt( 1034) ) = aPTotSM
         pmsa( ipnt( 1035) ) = afPnogSM
         pmsa( ipnt( 1036) ) = afPTotSM
         pmsa( ipnt( 1037) ) = afPO4SM
         pmsa( ipnt( 1038) ) = oPO4SM
         pmsa( ipnt( 1039) ) = aNDissSM
         pmsa( ipnt( 1040) ) = aNkAvlSM
         pmsa( ipnt( 1041) ) = aNkjSM
         pmsa( ipnt( 1042) ) = aNTAvlSM
         pmsa( ipnt( 1043) ) = aNTotSM
         pmsa( ipnt( 1044) ) = afNnogSM
         pmsa( ipnt( 1045) ) = afNTotSM
         pmsa( ipnt( 1046) ) = oNO3SM
         pmsa( ipnt( 1047) ) = oNH4SM
         pmsa( ipnt( 1048) ) = oNDissSM
         pmsa( ipnt( 1049) ) = rPDIMWM
         pmsa( ipnt( 1050) ) = rPDIMSM
         pmsa( ipnt( 1051) ) = rPDDtWM
         pmsa( ipnt( 1052) ) = rNDDtWM
         pmsa( ipnt( 1053) ) = rSiDDtWM
         pmsa( ipnt( 1054) ) = rPDHumSM
         pmsa( ipnt( 1055) ) = rNDHumSM
         pmsa( ipnt( 1056) ) = rPDDtSM
         pmsa( ipnt( 1057) ) = rNDDtSM
         pmsa( ipnt( 1058) ) = rSiDDtSM
         pmsa( ipnt( 1059) ) = aDTotM
         pmsa( ipnt( 1060) ) = aPTotM
         pmsa( ipnt( 1061) ) = aNTotM
         pmsa( ipnt( 1062) ) = aSiTotM
         pmsa( ipnt( 1063) ) = aO2TotM
         pmsa( ipnt( 1064) ) = uPLdSson
         pmsa( ipnt( 1065) ) = uPLoad
         pmsa( ipnt( 1066) ) = uPLoaPO4
         pmsa( ipnt( 1067) ) = uPLoaOrg
         pmsa( ipnt( 1068) ) = uPLdPTot
         pmsa( ipnt( 1069) ) = uPLoadDt
         pmsa( ipnt( 1070) ) = uPLoaAIM
         pmsa( ipnt( 1071) ) = uNLdSson
         pmsa( ipnt( 1072) ) = uNLdPTot
         pmsa( ipnt( 1073) ) = uNLoad
         pmsa( ipnt( 1074) ) = uNLoadDt
         pmsa( ipnt( 1075) ) = uNLoaOrg
         pmsa( ipnt( 1076) ) = uNLadiss
         pmsa( ipnt( 1077) ) = uNLoaNH4
         pmsa( ipnt( 1078) ) = uNLoaNO3
         pmsa( ipnt( 1079) ) = uNBckadH
         pmsa( ipnt( 1080) ) = uPBckadH
         pmsa( ipnt( 1081) ) = uNTotIn
         pmsa( ipnt( 1082) ) = uDLoadDt
         pmsa( ipnt( 1083) ) = uDLdPTot
         pmsa( ipnt( 1084) ) = uDLoadIM
         pmsa( ipnt( 1085) ) = uDLoad
         pmsa( ipnt( 1086) ) = UotIn
         pmsa( ipnt( 1087) ) = uDLoadDi
         pmsa( ipnt( 1088) ) = uPLoadDi
         pmsa( ipnt( 1089) ) = uNLoadDi
         pmsa( ipnt( 1090) ) = uDLadren
         pmsa( ipnt( 1091) ) = uPLadren
         pmsa( ipnt( 1092) ) = uNLadren
         pmsa( ipnt( 1093) ) = uDLadlue
         pmsa( ipnt( 1094) ) = uPLadlue
         pmsa( ipnt( 1095) ) = uNLadlue
         pmsa( ipnt( 1096) ) = wDDilIMH
         pmsa( ipnt( 1097) ) = wDDilDtH
         pmsa( ipnt( 1098) ) = wDDlGenH
         pmsa( ipnt( 1099) ) = wDDlBueH
         pmsa( ipnt( 1100) ) = wDDilDiH
         pmsa( ipnt( 1101) ) = wDDilooH
         pmsa( ipnt( 1102) ) = wDDlPytH
         pmsa( ipnt( 1103) ) = wPDilooH
         pmsa( ipnt( 1104) ) = wNDilooH
         pmsa( ipnt( 1105) ) = wPDilO4H
         pmsa( ipnt( 1106) ) = wPDilDtH
         pmsa( ipnt( 1107) ) = wPDilIMH
         pmsa( ipnt( 1108) ) = wNDilH4H
         pmsa( ipnt( 1109) ) = wNDilO3H
         pmsa( ipnt( 1110) ) = wNDilDtH
         pmsa( ipnt( 1111) ) = wO2nfowH
         pmsa( ipnt( 1112) ) = wO2OuflH
         pmsa( ipnt( 1113) ) = wPDilDiH
         pmsa( ipnt( 1114) ) = wNDilDiH
         pmsa( ipnt( 1115) ) = wPDlGenH
         pmsa( ipnt( 1116) ) = wNDlGenH
         pmsa( ipnt( 1117) ) = wPDlBueH
         pmsa( ipnt( 1118) ) = wNDlBueH
         pmsa( ipnt( 1119) ) = wPDlPytH
         pmsa( ipnt( 1120) ) = wNDlPytH
         pmsa( ipnt( 1121) ) = wDOtfotH
         pmsa( ipnt( 1122) ) = wPOtfotH
         pmsa( ipnt( 1123) ) = wNOtfotH
         pmsa( ipnt( 1124) ) = wDTraDiH
         pmsa( ipnt( 1125) ) = wPTraDiH
         pmsa( ipnt( 1126) ) = wNTraDiH
         pmsa( ipnt( 1127) ) = wDTanenH
         pmsa( ipnt( 1128) ) = wPTanenH
         pmsa( ipnt( 1129) ) = wNTanenH
         pmsa( ipnt( 1130) ) = wDTanueH
         pmsa( ipnt( 1131) ) = wPTanueH
         pmsa( ipnt( 1132) ) = wNTanueH
         pmsa( ipnt( 1133) ) = wDTanytH
         pmsa( ipnt( 1134) ) = wPTanytH
         pmsa( ipnt( 1135) ) = wNTanytH
         pmsa( ipnt( 1136) ) = uSioaiO2
         pmsa( ipnt( 1137) ) = uSiLodDt
         pmsa( ipnt( 1138) ) = uSiLodDi
         pmsa( ipnt( 1139) ) = uSiLoad
         pmsa( ipnt( 1140) ) = wSiilO2H
         pmsa( ipnt( 1141) ) = wSiDiDtH
         pmsa( ipnt( 1142) ) = wSiDiDiH
         pmsa( ipnt( 1143) ) = wSitfotH
         pmsa( ipnt( 1144) ) = wSianO2H
         pmsa( ipnt( 1145) ) = wSiratWH
         pmsa( ipnt( 1146) ) = tSiantTH
         pmsa( ipnt( 1147) ) = wDTanooH
         pmsa( ipnt( 1148) ) = wPTanooH
         pmsa( ipnt( 1149) ) = wNTanooH
         pmsa( ipnt( 1150) ) = wDTanMWH
         pmsa( ipnt( 1151) ) = wDTantWH
         pmsa( ipnt( 1152) ) = wO2TrnWH
         pmsa( ipnt( 1153) ) = wPTan4WH
         pmsa( ipnt( 1154) ) = wPTanMWH
         pmsa( ipnt( 1155) ) = wPTantWH
         pmsa( ipnt( 1156) ) = wNTan4WH
         pmsa( ipnt( 1157) ) = wNTan3WH
         pmsa( ipnt( 1158) ) = wNTantWH
         pmsa( ipnt( 1159) ) = wDDilotH
         pmsa( ipnt( 1160) ) = wPDilotH
         pmsa( ipnt( 1161) ) = wNDilotH
         pmsa( ipnt( 1162) ) = wSiilotH
         pmsa( ipnt( 1163) ) = tDTantTH
         pmsa( ipnt( 1164) ) = tPTantTH
         pmsa( ipnt( 1165) ) = tNTantTH
         pmsa( ipnt( 1166) ) = wDExIMMH
         pmsa( ipnt( 1167) ) = wPExP4MH
         pmsa( ipnt( 1168) ) = wPExAMMH
         pmsa( ipnt( 1169) ) = wNExN4MH
         pmsa( ipnt( 1170) ) = wNExN3MH
         pmsa( ipnt( 1171) ) = wSixS2MH
         pmsa( ipnt( 1172) ) = wO2ExMH
         pmsa( ipnt( 1173) ) = wDExDtMH
         pmsa( ipnt( 1174) ) = wPExDtMH
         pmsa( ipnt( 1175) ) = wNExDtMH
         pmsa( ipnt( 1176) ) = wSiExtMH
         pmsa( ipnt( 1177) ) = wDExDiMH
         pmsa( ipnt( 1178) ) = wPExDiMH
         pmsa( ipnt( 1179) ) = wNExDiMH
         pmsa( ipnt( 1180) ) = wSiExiMH
         pmsa( ipnt( 1181) ) = wDEGrnMH
         pmsa( ipnt( 1182) ) = wPEGrnMH
         pmsa( ipnt( 1183) ) = wNEGrnMH
         pmsa( ipnt( 1184) ) = wDEBleMH
         pmsa( ipnt( 1185) ) = wPEBleMH
         pmsa( ipnt( 1186) ) = wNEBleMH
         pmsa( ipnt( 1187) ) = wDExZoMH
         pmsa( ipnt( 1188) ) = wPExZoMH
         pmsa( ipnt( 1189) ) = wNExZoMH
         pmsa( ipnt( 1190) ) = wDExIMH
         pmsa( ipnt( 1191) ) = wPExPO4H
         pmsa( ipnt( 1192) ) = wPExAIMH
         pmsa( ipnt( 1193) ) = wNExNH4H
         pmsa( ipnt( 1194) ) = wNExNO3H
         pmsa( ipnt( 1195) ) = wSixSO2H
         pmsa( ipnt( 1196) ) = wO2ExH
         pmsa( ipnt( 1197) ) = wDExDtH
         pmsa( ipnt( 1198) ) = wPExDtH
         pmsa( ipnt( 1199) ) = wNExDtH
         pmsa( ipnt( 1200) ) = wSiExDtH
         pmsa( ipnt( 1201) ) = wDExDiH
         pmsa( ipnt( 1202) ) = wPExDiH
         pmsa( ipnt( 1203) ) = wNExDiH
         pmsa( ipnt( 1204) ) = wSiExDiH
         pmsa( ipnt( 1205) ) = wDExGenH
         pmsa( ipnt( 1206) ) = wPExGenH
         pmsa( ipnt( 1207) ) = wNExGenH
         pmsa( ipnt( 1208) ) = wDExBueH
         pmsa( ipnt( 1209) ) = wPExBueH
         pmsa( ipnt( 1210) ) = wNExBueH
         pmsa( ipnt( 1211) ) = wDExZooH
         pmsa( ipnt( 1212) ) = wPExZooH
         pmsa( ipnt( 1213) ) = wNExZooH
         pmsa( ipnt( 1214) ) = tPIfP4WH
         pmsa( ipnt( 1215) ) = tNIfN4WH
         pmsa( ipnt( 1216) ) = tNIfN3WH
         pmsa( ipnt( 1217) ) = tPInfO4S
         pmsa( ipnt( 1218) ) = tNInfH4S
         pmsa( ipnt( 1219) ) = tNInfO3S
         pmsa( ipnt( 1220) ) = tNH4LadS
         pmsa( ipnt( 1221) ) = tNO3LadS
         pmsa( ipnt( 1222) ) = uDErosIM
         pmsa( ipnt( 1223) ) = uDEroIMS
         pmsa( ipnt( 1224) ) = uDEroIMW
         pmsa( ipnt( 1225) ) = uDErosOM
         pmsa( ipnt( 1226) ) = uPErosOM
         pmsa( ipnt( 1227) ) = uNErosOM
         pmsa( ipnt( 1228) ) = uO2SatE
         pmsa( ipnt( 1229) ) = uO2SatH
         pmsa( ipnt( 1230) ) = kAer
         pmsa( ipnt( 1231) ) = uFuTmerE
         pmsa( ipnt( 1232) ) = uFuTmerH
         pmsa( ipnt( 1233) ) = aFuLeAer
         pmsa( ipnt( 1234) ) = tO2BubH
         pmsa( ipnt( 1235) ) = uFuTmish
         pmsa( ipnt( 1236) ) = tDTrbish
         pmsa( ipnt( 1237) ) = aFuegsus
         pmsa( ipnt( 1238) ) = aFuDiusp
         pmsa( ipnt( 1239) ) = tDRsTead
         pmsa( ipnt( 1240) ) = tDRsBead
         pmsa( ipnt( 1241) ) = tDRsuead
         pmsa( ipnt( 1242) ) = tDRessIM
         pmsa( ipnt( 1243) ) = tDRessDt
         pmsa( ipnt( 1244) ) = akRsPRef
         pmsa( ipnt( 1245) ) = tDRsPTot
         pmsa( ipnt( 1246) ) = tPRessDt
         pmsa( ipnt( 1247) ) = tPRsuPO4
         pmsa( ipnt( 1248) ) = tPRsuAIM
         pmsa( ipnt( 1249) ) = tNRsuNO3
         pmsa( ipnt( 1250) ) = tNRsuNH4
         pmsa( ipnt( 1251) ) = tNRessDt
         pmsa( ipnt( 1252) ) = tSiessDt
         pmsa( ipnt( 1253) ) = aFuauOMH
         pmsa( ipnt( 1254) ) = aFuauIMH
         pmsa( ipnt( 1255) ) = uFuTmetE
         pmsa( ipnt( 1256) ) = uFuTmetH
         pmsa( ipnt( 1257) ) = uCoVSIMH
         pmsa( ipnt( 1258) ) = tDSetIMH
         pmsa( ipnt( 1259) ) = tPSetIMH
         pmsa( ipnt( 1260) ) = uCoVSDtH
         pmsa( ipnt( 1261) ) = tDSetDtH
         pmsa( ipnt( 1262) ) = tPSetDtH
         pmsa( ipnt( 1263) ) = tNSetDtH
         pmsa( ipnt( 1264) ) = tSiSeDtH
         pmsa( ipnt( 1265) ) = kPMinDtW
         pmsa( ipnt( 1266) ) = kNMinDtW
         pmsa( ipnt( 1267) ) = kSiMiDtW
         pmsa( ipnt( 1268) ) = uFuTmnWE
         pmsa( ipnt( 1269) ) = uFuTmnWH
         pmsa( ipnt( 1270) ) = wDMintWH
         pmsa( ipnt( 1271) ) = wPMintWH
         pmsa( ipnt( 1272) ) = wNMintWH
         pmsa( ipnt( 1273) ) = wSiintWH
         pmsa( ipnt( 1274) ) = aCoO2ODH
         pmsa( ipnt( 1275) ) = wO2intWH
         pmsa( ipnt( 1276) ) = wDDentWH
         pmsa( ipnt( 1277) ) = wNDentWH
         pmsa( ipnt( 1278) ) = uFuTmtrH
         pmsa( ipnt( 1279) ) = uFuTmtrE
         pmsa( ipnt( 1280) ) = uFuTmtrS
         pmsa( ipnt( 1281) ) = aCo2NrWH
         pmsa( ipnt( 1282) ) = wNNitrWH
         pmsa( ipnt( 1283) ) = wO2NirWH
         pmsa( ipnt( 1284) ) = kPMinDtS
         pmsa( ipnt( 1285) ) = kNMinDtS
         pmsa( ipnt( 1286) ) = kSiMiDtS
         pmsa( ipnt( 1287) ) = uFuTminS
         pmsa( ipnt( 1288) ) = tDMinDtS
         pmsa( ipnt( 1289) ) = tPMinDtS
         pmsa( ipnt( 1290) ) = tNMinDtS
         pmsa( ipnt( 1291) ) = tSiMiDtS
         pmsa( ipnt( 1292) ) = uFunTDif
         pmsa( ipnt( 1293) ) = akODiCor
         pmsa( ipnt( 1294) ) = tSOD
         pmsa( ipnt( 1295) ) = aDptDif
         pmsa( ipnt( 1296) ) = tPDifO4H
         pmsa( ipnt( 1297) ) = tNDifO3H
         pmsa( ipnt( 1298) ) = tNDifH4H
         pmsa( ipnt( 1299) ) = tO2DifH
         pmsa( ipnt( 1300) ) = tPDroPO4
         pmsa( ipnt( 1301) ) = tNDroNO3
         pmsa( ipnt( 1302) ) = tNDroNH4
         pmsa( ipnt( 1303) ) = aPAsMxWH
         pmsa( ipnt( 1304) ) = aKPAdsWH
         pmsa( ipnt( 1305) ) = aPIoAsWH
         pmsa( ipnt( 1306) ) = aPEqIMWH
         pmsa( ipnt( 1307) ) = wPSrpMWH
         pmsa( ipnt( 1308) ) = tPChePO4
         pmsa( ipnt( 1309) ) = tSibiotT
         pmsa( ipnt( 1310) ) = uQEvPhra
         pmsa( ipnt( 1311) ) = tPEvP4WM
         pmsa( ipnt( 1312) ) = tNEvN4WM
         pmsa( ipnt( 1313) ) = tNEvN3WM
         pmsa( ipnt( 1314) ) = tPIfP4WM
         pmsa( ipnt( 1315) ) = tNIfN4WM
         pmsa( ipnt( 1316) ) = tNIfN3WM
         pmsa( ipnt( 1317) ) = tPIfP4SM
         pmsa( ipnt( 1318) ) = tNIfN4SM
         pmsa( ipnt( 1319) ) = tNIfN3SM
         pmsa( ipnt( 1320) ) = tO2AerM
         pmsa( ipnt( 1321) ) = tDSetIMM
         pmsa( ipnt( 1322) ) = tPSetIMM
         pmsa( ipnt( 1323) ) = tDSetDtM
         pmsa( ipnt( 1324) ) = tPSetDtM
         pmsa( ipnt( 1325) ) = tNSetDtM
         pmsa( ipnt( 1326) ) = tSiSeDtM
         pmsa( ipnt( 1327) ) = tDSetDiM
         pmsa( ipnt( 1328) ) = tPSetDiM
         pmsa( ipnt( 1329) ) = tNSetDiM
         pmsa( ipnt( 1330) ) = tSiSeDiM
         pmsa( ipnt( 1331) ) = tDStGenM
         pmsa( ipnt( 1332) ) = tPStGenM
         pmsa( ipnt( 1333) ) = tNStGenM
         pmsa( ipnt( 1334) ) = tDStBueM
         pmsa( ipnt( 1335) ) = tPStBueM
         pmsa( ipnt( 1336) ) = tNStBueM
         pmsa( ipnt( 1337) ) = tDStPytM
         pmsa( ipnt( 1338) ) = tPStPytM
         pmsa( ipnt( 1339) ) = tNStPytM
         pmsa( ipnt( 1340) ) = tDSetotM
         pmsa( ipnt( 1341) ) = wDMintWM
         pmsa( ipnt( 1342) ) = wPMintWM
         pmsa( ipnt( 1343) ) = wNMintWM
         pmsa( ipnt( 1344) ) = wSiintWM
         pmsa( ipnt( 1345) ) = aCoO2ODM
         pmsa( ipnt( 1346) ) = wO2intWM
         pmsa( ipnt( 1347) ) = wDDentWM
         pmsa( ipnt( 1348) ) = wNDentWM
         pmsa( ipnt( 1349) ) = aCo2NrWM
         pmsa( ipnt( 1350) ) = wNNitrWM
         pmsa( ipnt( 1351) ) = wO2NirWM
         pmsa( ipnt( 1352) ) = tDMintSM
         pmsa( ipnt( 1353) ) = tPMintSM
         pmsa( ipnt( 1354) ) = tNMintSM
         pmsa( ipnt( 1355) ) = tSiintSM
         pmsa( ipnt( 1356) ) = akODiorM
         pmsa( ipnt( 1357) ) = tSODM
         pmsa( ipnt( 1358) ) = aDpOxedM
         pmsa( ipnt( 1359) ) = afOxyedM
         pmsa( ipnt( 1360) ) = tDMOxtSM
         pmsa( ipnt( 1361) ) = tO2intSM
         pmsa( ipnt( 1362) ) = tDDentSM
         pmsa( ipnt( 1363) ) = tNDentSM
         pmsa( ipnt( 1364) ) = tNNitrSM
         pmsa( ipnt( 1365) ) = tO2NirSM
         pmsa( ipnt( 1366) ) = tDMnHmSM
         pmsa( ipnt( 1367) ) = tPMnHmSM
         pmsa( ipnt( 1368) ) = tNMnHmSM
         pmsa( ipnt( 1369) ) = aDptDifM
         pmsa( ipnt( 1370) ) = tPDifO4M
         pmsa( ipnt( 1371) ) = tNDifO3M
         pmsa( ipnt( 1372) ) = tNDifH4M
         pmsa( ipnt( 1373) ) = tO2DifM
         pmsa( ipnt( 1374) ) = tPDroO4M
         pmsa( ipnt( 1375) ) = tNDroO3M
         pmsa( ipnt( 1376) ) = tNDroH4M
         pmsa( ipnt( 1377) ) = aPAsMxWM
         pmsa( ipnt( 1378) ) = aKPAdsWM
         pmsa( ipnt( 1379) ) = aPIoAsWM
         pmsa( ipnt( 1380) ) = aPEqIMWM
         pmsa( ipnt( 1381) ) = wPSrpMWM
         pmsa( ipnt( 1382) ) = aPAsMxSM
         pmsa( ipnt( 1383) ) = aKPAdsSM
         pmsa( ipnt( 1384) ) = aPIoAsSM
         pmsa( ipnt( 1385) ) = aPEqIMSM
         pmsa( ipnt( 1386) ) = tPSrpMSM
         pmsa( ipnt( 1387) ) = tPCemO4M
         pmsa( ipnt( 1388) ) = aDRtVeg
         pmsa( ipnt( 1389) ) = bfSubegE
         pmsa( ipnt( 1390) ) = bfSubegH
         pmsa( ipnt( 1391) ) = afCSuVeg
         pmsa( ipnt( 1392) ) = afCEmVeg
         pmsa( ipnt( 1393) ) = aCovVeg
         pmsa( ipnt( 1394) ) = aDVeg
         pmsa( ipnt( 1395) ) = aPVeg
         pmsa( ipnt( 1396) ) = aNVeg
         pmsa( ipnt( 1397) ) = aLPARotH
         pmsa( ipnt( 1398) ) = rPDVeg
         pmsa( ipnt( 1399) ) = rNDVeg
         pmsa( ipnt( 1400) ) = tDMigVeg
         pmsa( ipnt( 1401) ) = tPMigVeg
         pmsa( ipnt( 1402) ) = tNMigVeg
         pmsa( ipnt( 1403) ) = uFumRVeg
         pmsa( ipnt( 1404) ) = afPUVegS
         pmsa( ipnt( 1405) ) = afNUVegS
         pmsa( ipnt( 1406) ) = aVPaxVeg
         pmsa( ipnt( 1407) ) = aVPUVgWH
         pmsa( ipnt( 1408) ) = aVPUVegS
         pmsa( ipnt( 1409) ) = aVPUVgWE
         pmsa( ipnt( 1410) ) = tPUVegWE
         pmsa( ipnt( 1411) ) = tPUVegWH
         pmsa( ipnt( 1412) ) = tPUVegS
         pmsa( ipnt( 1413) ) = tPUVegE
         pmsa( ipnt( 1414) ) = tPUVegH
         pmsa( ipnt( 1415) ) = aVNaxVeg
         pmsa( ipnt( 1416) ) = ahNUVeg
         pmsa( ipnt( 1417) ) = aVNUVgWE
         pmsa( ipnt( 1418) ) = aVNUVgWH
         pmsa( ipnt( 1419) ) = afN4UgWE
         pmsa( ipnt( 1420) ) = afN4UgWH
         pmsa( ipnt( 1421) ) = tNUVegWE
         pmsa( ipnt( 1422) ) = tNUVegWH
         pmsa( ipnt( 1423) ) = tNUH4gWE
         pmsa( ipnt( 1424) ) = tNUO3gWE
         pmsa( ipnt( 1425) ) = tNUH4gWH
         pmsa( ipnt( 1426) ) = tNUO3gWH
         pmsa( ipnt( 1427) ) = aVNUVegS
         pmsa( ipnt( 1428) ) = tNUVegS
         pmsa( ipnt( 1429) ) = afN4UegS
         pmsa( ipnt( 1430) ) = tNUH4egS
         pmsa( ipnt( 1431) ) = tNUVegSE
         pmsa( ipnt( 1432) ) = tNUO3egS
         pmsa( ipnt( 1433) ) = tNUVeg
         pmsa( ipnt( 1434) ) = aLLmSegE
         pmsa( ipnt( 1435) ) = aLLmSegH
         pmsa( ipnt( 1436) ) = aMumLegH
         pmsa( ipnt( 1437) ) = aPLimVeg
         pmsa( ipnt( 1438) ) = aNLimVeg
         pmsa( ipnt( 1439) ) = aNuLiVeg
         pmsa( ipnt( 1440) ) = aMuVegH
         pmsa( ipnt( 1441) ) = bkMVeg
         pmsa( ipnt( 1442) ) = akDncegH
         pmsa( ipnt( 1443) ) = tDEnvegH
         pmsa( ipnt( 1444) ) = tDEPregH
         pmsa( ipnt( 1445) ) = tDPodegH
         pmsa( ipnt( 1446) ) = tDPdSegH
         pmsa( ipnt( 1447) ) = tDResVeg
         pmsa( ipnt( 1448) ) = tDEvMegH
         pmsa( ipnt( 1449) ) = tDMVegH
         pmsa( ipnt( 1450) ) = tDMVegWH
         pmsa( ipnt( 1451) ) = tDGazgBi
         pmsa( ipnt( 1452) ) = bkManVeg
         pmsa( ipnt( 1453) ) = tDManVeg
         pmsa( ipnt( 1454) ) = tPManVeg
         pmsa( ipnt( 1455) ) = tNManVeg
         pmsa( ipnt( 1456) ) = tO2roegH
         pmsa( ipnt( 1457) ) = tO2spgWH
         pmsa( ipnt( 1458) ) = aDpOxSed
         pmsa( ipnt( 1459) ) = afOxySed
         pmsa( ipnt( 1460) ) = tO2esegS
         pmsa( ipnt( 1461) ) = tO2odgSH
         pmsa( ipnt( 1462) ) = tO2odgWH
         pmsa( ipnt( 1463) ) = tO2O3gWH
         pmsa( ipnt( 1464) ) = tO2NOegS
         pmsa( ipnt( 1465) ) = tPExcVeg
         pmsa( ipnt( 1466) ) = tPEcregS
         pmsa( ipnt( 1467) ) = tPEcregW
         pmsa( ipnt( 1468) ) = tPMVegH
         pmsa( ipnt( 1469) ) = tPMegO4H
         pmsa( ipnt( 1470) ) = tPMeg4SH
         pmsa( ipnt( 1471) ) = tPMeg4WH
         pmsa( ipnt( 1472) ) = tPMVeDtH
         pmsa( ipnt( 1473) ) = tPMegtWH
         pmsa( ipnt( 1474) ) = tPMegtSH
         pmsa( ipnt( 1475) ) = tPGazgBi
         pmsa( ipnt( 1476) ) = tNExcVeg
         pmsa( ipnt( 1477) ) = tNEcregS
         pmsa( ipnt( 1478) ) = tNEcregW
         pmsa( ipnt( 1479) ) = tNMVegH
         pmsa( ipnt( 1480) ) = tNMegH4H
         pmsa( ipnt( 1481) ) = tNMeg4SH
         pmsa( ipnt( 1482) ) = tNMeg4WH
         pmsa( ipnt( 1483) ) = tNMVeDtH
         pmsa( ipnt( 1484) ) = tNMegtWH
         pmsa( ipnt( 1485) ) = tNMegtSH
         pmsa( ipnt( 1486) ) = tNGazgBi
         pmsa( ipnt( 1487) ) = tDAsVgBi
         pmsa( ipnt( 1488) ) = tDEBi
         pmsa( ipnt( 1489) ) = tPAsVgBi
         pmsa( ipnt( 1490) ) = tPEBi
         pmsa( ipnt( 1491) ) = tPEBiPO4
         pmsa( ipnt( 1492) ) = tPEBiDt
         pmsa( ipnt( 1493) ) = tNAsVgBi
         pmsa( ipnt( 1494) ) = tNEBi
         pmsa( ipnt( 1495) ) = tNEBiNH4
         pmsa( ipnt( 1496) ) = tNEBiDt
         pmsa( ipnt( 1497) ) = wDBedtWH
         pmsa( ipnt( 1498) ) = aMumLegE
         pmsa( ipnt( 1499) ) = akDncegE
         pmsa( ipnt( 1500) ) = tDEnvegE
         pmsa( ipnt( 1501) ) = aMuVegE
         pmsa( ipnt( 1502) ) = tDEPregE
         pmsa( ipnt( 1503) ) = tDPodegE
         pmsa( ipnt( 1504) ) = tDEvMegE
         pmsa( ipnt( 1505) ) = tDMVegE
         pmsa( ipnt( 1506) ) = tNMVegE
         pmsa( ipnt( 1507) ) = tNMegH4E
         pmsa( ipnt( 1508) ) = tNMeg4SE
         pmsa( ipnt( 1509) ) = wPBdP4WH
         pmsa( ipnt( 1510) ) = wPBedtWH
         pmsa( ipnt( 1511) ) = wNBdN4WH
         pmsa( ipnt( 1512) ) = wNBdN3WH
         pmsa( ipnt( 1513) ) = wNBedtWH
         pmsa( ipnt( 1514) ) = tDMVegSH
         pmsa( ipnt( 1515) ) = tDBedtSH
         pmsa( ipnt( 1516) ) = tO2BedWH
         pmsa( ipnt( 1517) ) = UseLoss
         pmsa( ipnt( 1518) ) = uFuTmssH
         pmsa( ipnt( 1519) ) = uFuTmssE
         pmsa( ipnt( 1520) ) = rPDBleWH
         pmsa( ipnt( 1521) ) = rNDBleWH
         pmsa( ipnt( 1522) ) = rPDBlueS
         pmsa( ipnt( 1523) ) = rNDBlueS
         pmsa( ipnt( 1524) ) = uFuTmueH
         pmsa( ipnt( 1525) ) = uFuPrueH
         pmsa( ipnt( 1526) ) = uFuReueH
         pmsa( ipnt( 1527) ) = uFuTmueE
         pmsa( ipnt( 1528) ) = uFuPrueE
         pmsa( ipnt( 1529) ) = uFuReueE
         pmsa( ipnt( 1530) ) = uFuTmueS
         pmsa( ipnt( 1531) ) = uFuReueS
         pmsa( ipnt( 1532) ) = aVPxCueH
         pmsa( ipnt( 1533) ) = aVPUBueH
         pmsa( ipnt( 1534) ) = wPUBlueH
         pmsa( ipnt( 1535) ) = aVNxCueH
         pmsa( ipnt( 1536) ) = ahNUBueH
         pmsa( ipnt( 1537) ) = aVNUBueH
         pmsa( ipnt( 1538) ) = wNUBlueH
         pmsa( ipnt( 1539) ) = afN4UueH
         pmsa( ipnt( 1540) ) = wNUH4ueH
         pmsa( ipnt( 1541) ) = wNUO3ueH
         pmsa( ipnt( 1542) ) = uMuxTueH
         pmsa( ipnt( 1543) ) = uMuxTueE
         pmsa( ipnt( 1544) ) = aPLmBueH
         pmsa( ipnt( 1545) ) = aNLmBueH
         pmsa( ipnt( 1546) ) = aSiimueH
         pmsa( ipnt( 1547) ) = aLLmBueH
         pmsa( ipnt( 1548) ) = aMumLueH
         pmsa( ipnt( 1549) ) = aNuimueH
         pmsa( ipnt( 1550) ) = aMuBlueH
         pmsa( ipnt( 1551) ) = wDAsBueH
         pmsa( ipnt( 1552) ) = rChDBueH
         pmsa( ipnt( 1553) ) = oChlBlH
         pmsa( ipnt( 1554) ) = aExChueH
         pmsa( ipnt( 1555) ) = ukDpTueH
         pmsa( ipnt( 1556) ) = ukDpTueE
         pmsa( ipnt( 1557) ) = ukDpTueS
         pmsa( ipnt( 1558) ) = wDRpBeWH
         pmsa( ipnt( 1559) ) = ukLsTueH
         pmsa( ipnt( 1560) ) = ukLsTueE
         pmsa( ipnt( 1561) ) = wDLssueH
         pmsa( ipnt( 1562) ) = wDMBleWH
         pmsa( ipnt( 1563) ) = uCoSeueH
         pmsa( ipnt( 1564) ) = tDStBueH
         pmsa( ipnt( 1565) ) = tDRsulue
         pmsa( ipnt( 1566) ) = tDRspueS
         pmsa( ipnt( 1567) ) = tDMBlueS
         pmsa( ipnt( 1568) ) = ukDecueH
         pmsa( ipnt( 1569) ) = wPErBeWH
         pmsa( ipnt( 1570) ) = wPLssueH
         pmsa( ipnt( 1571) ) = wPMBleWH
         pmsa( ipnt( 1572) ) = tPStBueH
         pmsa( ipnt( 1573) ) = tPRsulue
         pmsa( ipnt( 1574) ) = tPEcrueS
         pmsa( ipnt( 1575) ) = tPMBlueS
         pmsa( ipnt( 1576) ) = wNErBeWH
         pmsa( ipnt( 1577) ) = wNLssueH
         pmsa( ipnt( 1578) ) = wNMBleWH
         pmsa( ipnt( 1579) ) = tNStBueH
         pmsa( ipnt( 1580) ) = tNRsulue
         pmsa( ipnt( 1581) ) = tNEcrueS
         pmsa( ipnt( 1582) ) = tNMBlueS
         pmsa( ipnt( 1583) ) = aFuauOME
         pmsa( ipnt( 1584) ) = aFuauIME
         pmsa( ipnt( 1585) ) = uCoSeueE
         pmsa( ipnt( 1586) ) = tDStBueE
         pmsa( ipnt( 1587) ) = rNDBleWE
         pmsa( ipnt( 1588) ) = tNStBueE
         pmsa( ipnt( 1589) ) = rPDBleWE
         pmsa( ipnt( 1590) ) = tPStBueE
         pmsa( ipnt( 1591) ) = wDPmBeWH
         pmsa( ipnt( 1592) ) = wPPmBeWH
         pmsa( ipnt( 1593) ) = wNPmBeWH
         pmsa( ipnt( 1594) ) = rPDGrnWH
         pmsa( ipnt( 1595) ) = rNDGrnWH
         pmsa( ipnt( 1596) ) = rPDGrenS
         pmsa( ipnt( 1597) ) = rNDGrenS
         pmsa( ipnt( 1598) ) = uFuTmenH
         pmsa( ipnt( 1599) ) = uFuTmenE
         pmsa( ipnt( 1600) ) = uFuTmenS
         pmsa( ipnt( 1601) ) = uFuPrenH
         pmsa( ipnt( 1602) ) = uFuReenH
         pmsa( ipnt( 1603) ) = uFuPrenE
         pmsa( ipnt( 1604) ) = uFuReenE
         pmsa( ipnt( 1605) ) = uFuReenS
         pmsa( ipnt( 1606) ) = aVPxCenH
         pmsa( ipnt( 1607) ) = aVPUGenH
         pmsa( ipnt( 1608) ) = wPUGrenH
         pmsa( ipnt( 1609) ) = aVNxCenH
         pmsa( ipnt( 1610) ) = ahNUGenH
         pmsa( ipnt( 1611) ) = aVNUGenH
         pmsa( ipnt( 1612) ) = wNUGrenH
         pmsa( ipnt( 1613) ) = afN4UenH
         pmsa( ipnt( 1614) ) = wNUH4enH
         pmsa( ipnt( 1615) ) = wNUO3enH
         pmsa( ipnt( 1616) ) = uMuxTenH
         pmsa( ipnt( 1617) ) = uMuxTenE
         pmsa( ipnt( 1618) ) = aPLmGenH
         pmsa( ipnt( 1619) ) = aNLmGenH
         pmsa( ipnt( 1620) ) = aSiimenH
         pmsa( ipnt( 1621) ) = aLLmGenH
         pmsa( ipnt( 1622) ) = aMumLenH
         pmsa( ipnt( 1623) ) = aNuimenH
         pmsa( ipnt( 1624) ) = aMuGrenH
         pmsa( ipnt( 1625) ) = wDAsGenH
         pmsa( ipnt( 1626) ) = rChDGenH
         pmsa( ipnt( 1627) ) = oChlGrH
         pmsa( ipnt( 1628) ) = aExChenH
         pmsa( ipnt( 1629) ) = ukDpTenH
         pmsa( ipnt( 1630) ) = ukDpTenE
         pmsa( ipnt( 1631) ) = ukDpTenS
         pmsa( ipnt( 1632) ) = wDRpGnWH
         pmsa( ipnt( 1633) ) = ukLsTenH
         pmsa( ipnt( 1634) ) = ukLsTenE
         pmsa( ipnt( 1635) ) = wDLssenH
         pmsa( ipnt( 1636) ) = wDMGrnWH
         pmsa( ipnt( 1637) ) = uCoSeenH
         pmsa( ipnt( 1638) ) = tDStGenH
         pmsa( ipnt( 1639) ) = tDRsuren
         pmsa( ipnt( 1640) ) = tDRspenS
         pmsa( ipnt( 1641) ) = tDMGrenS
         pmsa( ipnt( 1642) ) = ukDecenH
         pmsa( ipnt( 1643) ) = wPErGnWH
         pmsa( ipnt( 1644) ) = wPLssenH
         pmsa( ipnt( 1645) ) = wPMGrnWH
         pmsa( ipnt( 1646) ) = tPStGenH
         pmsa( ipnt( 1647) ) = tPRsuren
         pmsa( ipnt( 1648) ) = tPEcrenS
         pmsa( ipnt( 1649) ) = tPMGrenS
         pmsa( ipnt( 1650) ) = wNErGnWH
         pmsa( ipnt( 1651) ) = wNLssenH
         pmsa( ipnt( 1652) ) = wNMGrnWH
         pmsa( ipnt( 1653) ) = tNStGenH
         pmsa( ipnt( 1654) ) = tNRsuren
         pmsa( ipnt( 1655) ) = tNEcrenS
         pmsa( ipnt( 1656) ) = tNMGrenS
         pmsa( ipnt( 1657) ) = uCoSeenE
         pmsa( ipnt( 1658) ) = tDStGenE
         pmsa( ipnt( 1659) ) = rNDGrnWE
         pmsa( ipnt( 1660) ) = tNStGenE
         pmsa( ipnt( 1661) ) = rPDGrnWE
         pmsa( ipnt( 1662) ) = tPStGenE
         pmsa( ipnt( 1663) ) = wDPmGnWH
         pmsa( ipnt( 1664) ) = wPPmGnWH
         pmsa( ipnt( 1665) ) = wNPmGnWH
         pmsa( ipnt( 1666) ) = rPDDiWH
         pmsa( ipnt( 1667) ) = rNDDiWH
         pmsa( ipnt( 1668) ) = rPDDiS
         pmsa( ipnt( 1669) ) = uFunTDiH
         pmsa( ipnt( 1670) ) = rNDDiS
         pmsa( ipnt( 1671) ) = uFunTDiE
         pmsa( ipnt( 1672) ) = uFunTDiS
         pmsa( ipnt( 1673) ) = uFumPDiH
         pmsa( ipnt( 1674) ) = uFumRDiH
         pmsa( ipnt( 1675) ) = uFumPDiE
         pmsa( ipnt( 1676) ) = uFumRDiE
         pmsa( ipnt( 1677) ) = uFumRDiS
         pmsa( ipnt( 1678) ) = aVPaxDiH
         pmsa( ipnt( 1679) ) = aVPUDiH
         pmsa( ipnt( 1680) ) = wPUDiH
         pmsa( ipnt( 1681) ) = aVNaxDiH
         pmsa( ipnt( 1682) ) = ahNUDiH
         pmsa( ipnt( 1683) ) = aVNUDiH
         pmsa( ipnt( 1684) ) = wNUDiH
         pmsa( ipnt( 1685) ) = afNH4DiH
         pmsa( ipnt( 1686) ) = wNUNHDiH
         pmsa( ipnt( 1687) ) = wNUNODiH
         pmsa( ipnt( 1688) ) = uMuaxDiH
         pmsa( ipnt( 1689) ) = uMuaxDiE
         pmsa( ipnt( 1690) ) = aPLimDiH
         pmsa( ipnt( 1691) ) = aNLimDiH
         pmsa( ipnt( 1692) ) = aSiLiDiH
         pmsa( ipnt( 1693) ) = aLLimDiH
         pmsa( ipnt( 1694) ) = aMuTmDiH
         pmsa( ipnt( 1695) ) = aNuLiDiH
         pmsa( ipnt( 1696) ) = aMuDiH
         pmsa( ipnt( 1697) ) = wDAssDiH
         pmsa( ipnt( 1698) ) = rChDDiH
         pmsa( ipnt( 1699) ) = oChlDiH
         pmsa( ipnt( 1700) ) = aExtCDiH
         pmsa( ipnt( 1701) ) = ukDspDiH
         pmsa( ipnt( 1702) ) = ukDspDiE
         pmsa( ipnt( 1703) ) = ukDspDiS
         pmsa( ipnt( 1704) ) = wDRspiWH
         pmsa( ipnt( 1705) ) = ukLssDiH
         pmsa( ipnt( 1706) ) = ukLssDiE
         pmsa( ipnt( 1707) ) = wDLosDiH
         pmsa( ipnt( 1708) ) = wDMDiWH
         pmsa( ipnt( 1709) ) = uCoVSDiH
         pmsa( ipnt( 1710) ) = tDSetDiH
         pmsa( ipnt( 1711) ) = tDRessDi
         pmsa( ipnt( 1712) ) = tDResDiS
         pmsa( ipnt( 1713) ) = tDMDiS
         pmsa( ipnt( 1714) ) = ukDDeDiH
         pmsa( ipnt( 1715) ) = wPEcriWH
         pmsa( ipnt( 1716) ) = wPLosDiH
         pmsa( ipnt( 1717) ) = wPMDiWH
         pmsa( ipnt( 1718) ) = tPSetDiH
         pmsa( ipnt( 1719) ) = tPRessDi
         pmsa( ipnt( 1720) ) = tPExcDiS
         pmsa( ipnt( 1721) ) = tPMDiS
         pmsa( ipnt( 1722) ) = wNEcriWH
         pmsa( ipnt( 1723) ) = wNLosDiH
         pmsa( ipnt( 1724) ) = wNMDiWH
         pmsa( ipnt( 1725) ) = tNSetDiH
         pmsa( ipnt( 1726) ) = tNRessDi
         pmsa( ipnt( 1727) ) = tNExcDiS
         pmsa( ipnt( 1728) ) = tNMDiS
         pmsa( ipnt( 1729) ) = uCoVSDiE
         pmsa( ipnt( 1730) ) = tDSetDiE
         pmsa( ipnt( 1731) ) = rPDDiWE
         pmsa( ipnt( 1732) ) = tPSetDiE
         pmsa( ipnt( 1733) ) = rNDDiWE
         pmsa( ipnt( 1734) ) = tNSetDiE
         pmsa( ipnt( 1735) ) = wDPimiWH
         pmsa( ipnt( 1736) ) = wPPimiWH
         pmsa( ipnt( 1737) ) = wNPimiWH
         pmsa( ipnt( 1738) ) = oChlaH
         pmsa( ipnt( 1739) ) = wDAsPytH
         pmsa( ipnt( 1740) ) = wDRpPtWH
         pmsa( ipnt( 1741) ) = wDMPhtWH
         pmsa( ipnt( 1742) ) = tDStPytH
         pmsa( ipnt( 1743) ) = wDLssytH
         pmsa( ipnt( 1744) ) = wDPmPtWH
         pmsa( ipnt( 1745) ) = wPUPhytH
         pmsa( ipnt( 1746) ) = wPErPtWH
         pmsa( ipnt( 1747) ) = wPMPhtWH
         pmsa( ipnt( 1748) ) = tPStPytH
         pmsa( ipnt( 1749) ) = tPRsuhyt
         pmsa( ipnt( 1750) ) = wPLssytH
         pmsa( ipnt( 1751) ) = wPPmPtWH
         pmsa( ipnt( 1752) ) = wNUPhytH
         pmsa( ipnt( 1753) ) = wNUH4ytH
         pmsa( ipnt( 1754) ) = wNUO3ytH
         pmsa( ipnt( 1755) ) = wNErPtWH
         pmsa( ipnt( 1756) ) = wNMPhtWH
         pmsa( ipnt( 1757) ) = tNStPytH
         pmsa( ipnt( 1758) ) = tNRsuhyt
         pmsa( ipnt( 1759) ) = wNLssytH
         pmsa( ipnt( 1760) ) = wNPmPtWH
         pmsa( ipnt( 1761) ) = tDRspytS
         pmsa( ipnt( 1762) ) = tDMPhytS
         pmsa( ipnt( 1763) ) = tPEcrytS
         pmsa( ipnt( 1764) ) = tPMPhytS
         pmsa( ipnt( 1765) ) = tNEcrytS
         pmsa( ipnt( 1766) ) = tNMPhytS
         pmsa( ipnt( 1767) ) = wSiUDiH
         pmsa( ipnt( 1768) ) = wSixciWH
         pmsa( ipnt( 1769) ) = wSiosDiH
         pmsa( ipnt( 1770) ) = wSiMDiWH
         pmsa( ipnt( 1771) ) = tSiSeDiH
         pmsa( ipnt( 1772) ) = tSiessDi
         pmsa( ipnt( 1773) ) = rCyDBueH
         pmsa( ipnt( 1774) ) = oCyanH
         pmsa( ipnt( 1775) ) = fDDiH
         pmsa( ipnt( 1776) ) = wDPimtWH
         pmsa( ipnt( 1777) ) = wO2odytH
         pmsa( ipnt( 1778) ) = wO2sptWH
         pmsa( ipnt( 1779) ) = wO2O3ytH
         pmsa( ipnt( 1780) ) = wO2PrmWH
         pmsa( ipnt( 1781) ) = wPMyt4WH
         pmsa( ipnt( 1782) ) = wPMhytWH
         pmsa( ipnt( 1783) ) = wPLPhO4H
         pmsa( ipnt( 1784) ) = wPLsPDtH
         pmsa( ipnt( 1785) ) = wPPim4WH
         pmsa( ipnt( 1786) ) = wPPimtWH
         pmsa( ipnt( 1787) ) = tPMhyO4S
         pmsa( ipnt( 1788) ) = tPMhyDtS
         pmsa( ipnt( 1789) ) = tPPimO4S
         pmsa( ipnt( 1790) ) = tPPimotT
         pmsa( ipnt( 1791) ) = wNMyt4WH
         pmsa( ipnt( 1792) ) = wNMhytWH
         pmsa( ipnt( 1793) ) = wNLPhH4H
         pmsa( ipnt( 1794) ) = wNLsPDtH
         pmsa( ipnt( 1795) ) = wNPim4WH
         pmsa( ipnt( 1796) ) = wNPim3WH
         pmsa( ipnt( 1797) ) = wNPimtWH
         pmsa( ipnt( 1798) ) = tNMhyH4S
         pmsa( ipnt( 1799) ) = tNMhyDtS
         pmsa( ipnt( 1800) ) = tNPimH4S
         pmsa( ipnt( 1801) ) = tNPimO3S
         pmsa( ipnt( 1802) ) = tNPimotT
         pmsa( ipnt( 1803) ) = tSixcDiS
         pmsa( ipnt( 1804) ) = tSiMDiS
         pmsa( ipnt( 1805) ) = wSiim2WH
         pmsa( ipnt( 1806) ) = wSiritWH
         pmsa( ipnt( 1807) ) = tSiriotT
         pmsa( ipnt( 1808) ) = aPACoefE
         pmsa( ipnt( 1809) ) = aPACoefH
         pmsa( ipnt( 1810) ) = bSechaxE
         pmsa( ipnt( 1811) ) = aSecchiE
         pmsa( ipnt( 1812) ) = aTrpacyE
         pmsa( ipnt( 1813) ) = bSechaxH
         pmsa( ipnt( 1814) ) = aSecchiH
         pmsa( ipnt( 1815) ) = aSecchiT
         pmsa( ipnt( 1816) ) = aTrpacyH
         pmsa( ipnt( 1817) ) = aTrpacyT
         pmsa( ipnt( 1818) ) = aDptEphH
         pmsa( ipnt( 1819) ) = aReptphH
         pmsa( ipnt( 1820) ) = aChlaHH
         pmsa( ipnt( 1821) ) = aCoPhtWH
         pmsa( ipnt( 1822) ) = rExChytH
         pmsa( ipnt( 1823) ) = uFuTmooH
         pmsa( ipnt( 1824) ) = uFuTmooE
         pmsa( ipnt( 1825) ) = rPDZooH
         pmsa( ipnt( 1826) ) = rNDZooH
         pmsa( ipnt( 1827) ) = oDFodooH
         pmsa( ipnt( 1828) ) = aFiltH
         pmsa( ipnt( 1829) ) = ukDsTooH
         pmsa( ipnt( 1830) ) = ukDsTooE
         pmsa( ipnt( 1831) ) = aDSatooH
         pmsa( ipnt( 1832) ) = ukDspooH
         pmsa( ipnt( 1833) ) = ukDspooE
         pmsa( ipnt( 1834) ) = ukDncooH
         pmsa( ipnt( 1835) ) = ukDncooE
         pmsa( ipnt( 1836) ) = wDEnvooH
         pmsa( ipnt( 1837) ) = wDAssooH
         pmsa( ipnt( 1838) ) = wDCZooH
         pmsa( ipnt( 1839) ) = wDCDtooH
         pmsa( ipnt( 1840) ) = wDCDiooH
         pmsa( ipnt( 1841) ) = wDCreooH
         pmsa( ipnt( 1842) ) = wDCluooH
         pmsa( ipnt( 1843) ) = wDChyooH
         pmsa( ipnt( 1844) ) = wDEZooH
         pmsa( ipnt( 1845) ) = aCoReooH
         pmsa( ipnt( 1846) ) = wDRspooH
         pmsa( ipnt( 1847) ) = wDMZooH
         pmsa( ipnt( 1848) ) = oPFodooH
         pmsa( ipnt( 1849) ) = rPDooooH
         pmsa( ipnt( 1850) ) = wPCDiooH
         pmsa( ipnt( 1851) ) = wPCreooH
         pmsa( ipnt( 1852) ) = wPCluooH
         pmsa( ipnt( 1853) ) = wPChyooH
         pmsa( ipnt( 1854) ) = wPCDtooH
         pmsa( ipnt( 1855) ) = wPCZooH
         pmsa( ipnt( 1856) ) = afPssooH
         pmsa( ipnt( 1857) ) = wPAssooH
         pmsa( ipnt( 1858) ) = wPEZooH
         pmsa( ipnt( 1859) ) = wPEooO4H
         pmsa( ipnt( 1860) ) = wPEZoDtH
         pmsa( ipnt( 1861) ) = akPxcooH
         pmsa( ipnt( 1862) ) = wPEcrooH
         pmsa( ipnt( 1863) ) = wPMZooH
         pmsa( ipnt( 1864) ) = wPMooO4H
         pmsa( ipnt( 1865) ) = wPMZoDtH
         pmsa( ipnt( 1866) ) = oNFodooH
         pmsa( ipnt( 1867) ) = rNDooooH
         pmsa( ipnt( 1868) ) = wNCDiooH
         pmsa( ipnt( 1869) ) = wNCreooH
         pmsa( ipnt( 1870) ) = wNCluooH
         pmsa( ipnt( 1871) ) = wNChyooH
         pmsa( ipnt( 1872) ) = wNCDtooH
         pmsa( ipnt( 1873) ) = wNCZooH
         pmsa( ipnt( 1874) ) = afNssooH
         pmsa( ipnt( 1875) ) = wNAssooH
         pmsa( ipnt( 1876) ) = wNEZooH
         pmsa( ipnt( 1877) ) = wNEooH4H
         pmsa( ipnt( 1878) ) = wNEZoDtH
         pmsa( ipnt( 1879) ) = kNEcrooH
         pmsa( ipnt( 1880) ) = wNEcrooH
         pmsa( ipnt( 1881) ) = wNMZooH
         pmsa( ipnt( 1882) ) = wNMooH4H
         pmsa( ipnt( 1883) ) = wNMZoDtH
         pmsa( ipnt( 1884) ) = wSiDiooH
         pmsa( ipnt( 1885) ) = uFunTBnt
         pmsa( ipnt( 1886) ) = aDFooBnt
         pmsa( ipnt( 1887) ) = rPDBnt
         pmsa( ipnt( 1888) ) = rNDBnt
         pmsa( ipnt( 1889) ) = tDMigBnt
         pmsa( ipnt( 1890) ) = aDSatBnt
         pmsa( ipnt( 1891) ) = ukDncBnt
         pmsa( ipnt( 1892) ) = tDEnvBnt
         pmsa( ipnt( 1893) ) = tDAssBnt
         pmsa( ipnt( 1894) ) = aDAsBtSp
         pmsa( ipnt( 1895) ) = tDCBnt
         pmsa( ipnt( 1896) ) = tDCDtBnt
         pmsa( ipnt( 1897) ) = tDCDiBnt
         pmsa( ipnt( 1898) ) = tDCreBnt
         pmsa( ipnt( 1899) ) = tDCluBnt
         pmsa( ipnt( 1900) ) = tDChyBnt
         pmsa( ipnt( 1901) ) = tDEBnt
         pmsa( ipnt( 1902) ) = tDResBnt
         pmsa( ipnt( 1903) ) = tDMBnt
         pmsa( ipnt( 1904) ) = aPFooBnt
         pmsa( ipnt( 1905) ) = rPDooBnt
         pmsa( ipnt( 1906) ) = tPCDtBnt
         pmsa( ipnt( 1907) ) = tPCDiBnt
         pmsa( ipnt( 1908) ) = tPCreBnt
         pmsa( ipnt( 1909) ) = tPCluBnt
         pmsa( ipnt( 1910) ) = tPChyBnt
         pmsa( ipnt( 1911) ) = tPCBnt
         pmsa( ipnt( 1912) ) = afPAsBnt
         pmsa( ipnt( 1913) ) = tPAssBnt
         pmsa( ipnt( 1914) ) = tPEBnt
         pmsa( ipnt( 1915) ) = tPEBnPO4
         pmsa( ipnt( 1916) ) = tPEBntDt
         pmsa( ipnt( 1917) ) = tPExcBnt
         pmsa( ipnt( 1918) ) = tPMBnt
         pmsa( ipnt( 1919) ) = tPMBnPO4
         pmsa( ipnt( 1920) ) = tPMBntDt
         pmsa( ipnt( 1921) ) = tPMigBnt
         pmsa( ipnt( 1922) ) = aNFooBnt
         pmsa( ipnt( 1923) ) = rNDooBnt
         pmsa( ipnt( 1924) ) = tNMigBnt
         pmsa( ipnt( 1925) ) = tNCDtBnt
         pmsa( ipnt( 1926) ) = tNCDiBnt
         pmsa( ipnt( 1927) ) = tNCreBnt
         pmsa( ipnt( 1928) ) = tNCluBnt
         pmsa( ipnt( 1929) ) = tNChyBnt
         pmsa( ipnt( 1930) ) = tNCBnt
         pmsa( ipnt( 1931) ) = afNAsBnt
         pmsa( ipnt( 1932) ) = tNAssBnt
         pmsa( ipnt( 1933) ) = tNEBnt
         pmsa( ipnt( 1934) ) = tNEBnNH4
         pmsa( ipnt( 1935) ) = tNEBntDt
         pmsa( ipnt( 1936) ) = tNExcBnt
         pmsa( ipnt( 1937) ) = tNMBnt
         pmsa( ipnt( 1938) ) = tNMBnNH4
         pmsa( ipnt( 1939) ) = tNMBntDt
         pmsa( ipnt( 1940) ) = tSiCDBnt
         pmsa( ipnt( 1941) ) = aFuVeish
         pmsa( ipnt( 1942) ) = ukDnciJv
         pmsa( ipnt( 1943) ) = aDSatiAd
         pmsa( ipnt( 1944) ) = ukDnciAd
         pmsa( ipnt( 1945) ) = ukHrvish
         pmsa( ipnt( 1946) ) = afPssiAd
         pmsa( ipnt( 1947) ) = afNssiAd
         pmsa( ipnt( 1948) ) = uFuTmisc
         pmsa( ipnt( 1949) ) = aDCrrisc
         pmsa( ipnt( 1950) ) = aFuVeisc
         pmsa( ipnt( 1951) ) = akDncisc
         pmsa( ipnt( 1952) ) = ukHrvisc
         pmsa( ipnt( 1953) ) = tDWebDtS
         pmsa( ipnt( 1954) ) = tDWebDiS
         pmsa( ipnt( 1955) ) = tDWbGenS
         pmsa( ipnt( 1956) ) = tDWbBueS
         pmsa( ipnt( 1957) ) = tDWbPytS
         pmsa( ipnt( 1958) ) = tPWebO4S
         pmsa( ipnt( 1959) ) = tPWebDtS
         pmsa( ipnt( 1960) ) = tPWebDiS
         pmsa( ipnt( 1961) ) = tPWbGenS
         pmsa( ipnt( 1962) ) = tPWbBueS
         pmsa( ipnt( 1963) ) = tPWbPytS
         pmsa( ipnt( 1964) ) = tNWebH4S
         pmsa( ipnt( 1965) ) = tNWebO3S
         pmsa( ipnt( 1966) ) = tNWebDtS
         pmsa( ipnt( 1967) ) = tNWebDiS
         pmsa( ipnt( 1968) ) = tNWbGenS
         pmsa( ipnt( 1969) ) = tNWbBueS
         pmsa( ipnt( 1970) ) = tNWbPytS
         pmsa( ipnt( 1971) ) = wSiebO2W
         pmsa( ipnt( 1972) ) = wSiebtWH
         pmsa( ipnt( 1973) ) = tSiWeDtS
         pmsa( ipnt( 1974) ) = tSiebotT
         pmsa( ipnt( 1975) ) = aPrefveH
         pmsa( ipnt( 1976) ) = wDCZoo2H
         pmsa( ipnt( 1977) ) = aDCZoSpH
         pmsa( ipnt( 1978) ) = aDAsZSpH
         pmsa( ipnt( 1979) ) = aDGraSpH
         pmsa( ipnt( 1980) ) = aPCZoSpH
         pmsa( ipnt( 1981) ) = aPGraSpH
         pmsa( ipnt( 1982) ) = aNCZoSpH
         pmsa( ipnt( 1983) ) = aNGraSpH
         pmsa( ipnt( 1984) ) = afDShhra
         pmsa( ipnt( 1985) ) = rDSRPhra
         pmsa( ipnt( 1986) ) = rPDShhra
         pmsa( ipnt( 1987) ) = rNDShhra
         pmsa( ipnt( 1988) ) = rPDRthra
         pmsa( ipnt( 1989) ) = rNDRthra
         pmsa( ipnt( 1990) ) = aLegShra
         pmsa( ipnt( 1991) ) = bDanihra
         pmsa( ipnt( 1992) ) = aDAllhra
         pmsa( ipnt( 1993) ) = tDAllhra
         pmsa( ipnt( 1994) ) = tNTanhra
         pmsa( ipnt( 1995) ) = tPTanhra
         pmsa( ipnt( 1996) ) = aVNhrxCr
         pmsa( ipnt( 1997) ) = ahNUPraS
         pmsa( ipnt( 1998) ) = aVNUPraS
         pmsa( ipnt( 1999) ) = tNUPhraS
         pmsa( ipnt( 2000) ) = tNUH4raS
         pmsa( ipnt( 2001) ) = tNUO3raS
         pmsa( ipnt( 2002) ) = tNUShhra
         pmsa( ipnt( 2003) ) = tNURthra
         pmsa( ipnt( 2004) ) = aVPhrxCr
         pmsa( ipnt( 2005) ) = ahPUPraS
         pmsa( ipnt( 2006) ) = aVPUPraS
         pmsa( ipnt( 2007) ) = tPUPhraS
         pmsa( ipnt( 2008) ) = tPUShhra
         pmsa( ipnt( 2009) ) = tPURthra
         pmsa( ipnt( 2010) ) = uFuPrhra
         pmsa( ipnt( 2011) ) = ukDsphra
         pmsa( ipnt( 2012) ) = aMuhohra
         pmsa( ipnt( 2013) ) = aNLPrhra
         pmsa( ipnt( 2014) ) = aPLPrhra
         pmsa( ipnt( 2015) ) = aNuLihra
         pmsa( ipnt( 2016) ) = aMuPhra
         pmsa( ipnt( 2017) ) = akDnchra
         pmsa( ipnt( 2018) ) = tDDnshra
         pmsa( ipnt( 2019) ) = tDDPrhra
         pmsa( ipnt( 2020) ) = tDPodhra
         pmsa( ipnt( 2021) ) = tDPdShra
         pmsa( ipnt( 2022) ) = tDPdRhra
         pmsa( ipnt( 2023) ) = tDRpShra
         pmsa( ipnt( 2024) ) = tDRpRhra
         pmsa( ipnt( 2025) ) = tO2sphra
         pmsa( ipnt( 2026) ) = tO2lohra
         pmsa( ipnt( 2027) ) = bDaeahra
         pmsa( ipnt( 2028) ) = aDRalhra
         pmsa( ipnt( 2029) ) = tDRalhra
         pmsa( ipnt( 2030) ) = tNRtrhra
         pmsa( ipnt( 2031) ) = tPRtrhra
         pmsa( ipnt( 2032) ) = tDMShhra
         pmsa( ipnt( 2033) ) = tNMShhra
         pmsa( ipnt( 2034) ) = tPMShhra
         pmsa( ipnt( 2035) ) = tDMRthra
         pmsa( ipnt( 2036) ) = tNMRthra
         pmsa( ipnt( 2037) ) = tPMRthra
         pmsa( ipnt( 2038) ) = tDMnShra
         pmsa( ipnt( 2039) ) = tNMnShra
         pmsa( ipnt( 2040) ) = tPMnShra
         pmsa( ipnt( 2041) ) = tDIMSM
         pmsa( ipnt( 2042) ) = tDHumSM
         pmsa( ipnt( 2043) ) = tDDtSM
         pmsa( ipnt( 2044) ) = vDeltaSM
         pmsa( ipnt( 2045) ) = tDBurIMM
         pmsa( ipnt( 2046) ) = tDBurOMM
         pmsa( ipnt( 2047) ) = tDBurDtM
         pmsa( ipnt( 2048) ) = tDBurumM
         pmsa( ipnt( 2049) ) = tDBurotM
         pmsa( ipnt( 2050) ) = tPBurumM
         pmsa( ipnt( 2051) ) = tPBurDtM
         pmsa( ipnt( 2052) ) = tPBurIMM
         pmsa( ipnt( 2053) ) = tPBurO4M
         pmsa( ipnt( 2054) ) = tPBurotM
         pmsa( ipnt( 2055) ) = tNBurumM
         pmsa( ipnt( 2056) ) = tNBurDtM
         pmsa( ipnt( 2057) ) = tNBurH4M
         pmsa( ipnt( 2058) ) = tNBurO3M
         pmsa( ipnt( 2059) ) = tNBurotM
         pmsa( ipnt( 2060) ) = tSiBuDtM
         pmsa( ipnt( 2061) ) = tSiurotM
         pmsa( ipnt( 2062) ) = vDeltaWM
         pmsa( ipnt( 2063) ) = aReDeaWM
         pmsa( ipnt( 2064) ) = tDSetotH
         pmsa( ipnt( 2065) ) = tPSetotH
         pmsa( ipnt( 2066) ) = tNSetotH
         pmsa( ipnt( 2067) ) = bTimered
         pmsa( ipnt( 2068) ) = bRholoil
         pmsa( ipnt( 2069) ) = tDMrsotT
         pmsa( ipnt( 2070) ) = tPMrsotT
         pmsa( ipnt( 2071) ) = tNMrsotT
         pmsa( ipnt( 2072) ) = tSiarotT
         pmsa( ipnt( 2073) ) = uTaSustE
         pmsa( ipnt( 2074) ) = akExME
         pmsa( ipnt( 2075) ) = afVlMshE
         pmsa( ipnt( 2076) ) = akExLE
         pmsa( ipnt( 2077) ) = oPPhytWE
         pmsa( ipnt( 2078) ) = oNPhytWE
         pmsa( ipnt( 2079) ) = oDSestWE
         pmsa( ipnt( 2080) ) = oPOMWE
         pmsa( ipnt( 2081) ) = oPSestWE
         pmsa( ipnt( 2082) ) = oPInogWE
         pmsa( ipnt( 2083) ) = oPTotWE
         pmsa( ipnt( 2084) ) = oNOMWE
         pmsa( ipnt( 2085) ) = oNSestWE
         pmsa( ipnt( 2086) ) = oNkjWE
         pmsa( ipnt( 2087) ) = oNTotWE
         pmsa( ipnt( 2088) ) = rPDIMWE
         pmsa( ipnt( 2089) ) = rPDDtWE
         pmsa( ipnt( 2090) ) = rNDDtWE
         pmsa( ipnt( 2091) ) = rSiDDtWE
         pmsa( ipnt( 2092) ) = wDDilIME
         pmsa( ipnt( 2093) ) = wDDilDtE
         pmsa( ipnt( 2094) ) = wDDilDiE
         pmsa( ipnt( 2095) ) = wDDlGenE
         pmsa( ipnt( 2096) ) = wDDlBueE
         pmsa( ipnt( 2097) ) = wDDlPytE
         pmsa( ipnt( 2098) ) = wDDilooE
         pmsa( ipnt( 2099) ) = wPDilooE
         pmsa( ipnt( 2100) ) = wNDilooE
         pmsa( ipnt( 2101) ) = wPDilO4E
         pmsa( ipnt( 2102) ) = wPDilDtE
         pmsa( ipnt( 2103) ) = wPDilIME
         pmsa( ipnt( 2104) ) = wNDilH4E
         pmsa( ipnt( 2105) ) = wNDilO3E
         pmsa( ipnt( 2106) ) = wNDilDtE
         pmsa( ipnt( 2107) ) = wO2nfowE
         pmsa( ipnt( 2108) ) = wO2OuflE
         pmsa( ipnt( 2109) ) = wPDilDiE
         pmsa( ipnt( 2110) ) = wNDilDiE
         pmsa( ipnt( 2111) ) = wPDlGenE
         pmsa( ipnt( 2112) ) = wNDlGenE
         pmsa( ipnt( 2113) ) = wPDlBueE
         pmsa( ipnt( 2114) ) = wNDlBueE
         pmsa( ipnt( 2115) ) = wPDlPytE
         pmsa( ipnt( 2116) ) = wNDlPytE
         pmsa( ipnt( 2117) ) = wDOtfotE
         pmsa( ipnt( 2118) ) = wPOtfotE
         pmsa( ipnt( 2119) ) = wNOtfotE
         pmsa( ipnt( 2120) ) = wDTraDiE
         pmsa( ipnt( 2121) ) = wPTraDiE
         pmsa( ipnt( 2122) ) = wNTraDiE
         pmsa( ipnt( 2123) ) = wDTanenE
         pmsa( ipnt( 2124) ) = wPTanenE
         pmsa( ipnt( 2125) ) = wNTanenE
         pmsa( ipnt( 2126) ) = wDTanueE
         pmsa( ipnt( 2127) ) = wPTanueE
         pmsa( ipnt( 2128) ) = wNTanueE
         pmsa( ipnt( 2129) ) = wDTanytE
         pmsa( ipnt( 2130) ) = wPTanytE
         pmsa( ipnt( 2131) ) = wNTanytE
         pmsa( ipnt( 2132) ) = wSiilO2E
         pmsa( ipnt( 2133) ) = wSiDiDtE
         pmsa( ipnt( 2134) ) = wSiDiDiE
         pmsa( ipnt( 2135) ) = wSitfotE
         pmsa( ipnt( 2136) ) = wSianO2E
         pmsa( ipnt( 2137) ) = wSiratWE
         pmsa( ipnt( 2138) ) = tSiantTE
         pmsa( ipnt( 2139) ) = wDTanooE
         pmsa( ipnt( 2140) ) = wPTanooE
         pmsa( ipnt( 2141) ) = wNTanooE
         pmsa( ipnt( 2142) ) = wDTanMWE
         pmsa( ipnt( 2143) ) = wDTantWE
         pmsa( ipnt( 2144) ) = wO2TrnWE
         pmsa( ipnt( 2145) ) = wPTan4WE
         pmsa( ipnt( 2146) ) = wPTanMWE
         pmsa( ipnt( 2147) ) = wPTantWE
         pmsa( ipnt( 2148) ) = wNTan4WE
         pmsa( ipnt( 2149) ) = wNTan3WE
         pmsa( ipnt( 2150) ) = wNTantWE
         pmsa( ipnt( 2151) ) = wDDilotE
         pmsa( ipnt( 2152) ) = wPDilotE
         pmsa( ipnt( 2153) ) = wNDilotE
         pmsa( ipnt( 2154) ) = wSiilotE
         pmsa( ipnt( 2155) ) = tDTantTE
         pmsa( ipnt( 2156) ) = tPTantTE
         pmsa( ipnt( 2157) ) = tNTantTE
         pmsa( ipnt( 2158) ) = wDExIMME
         pmsa( ipnt( 2159) ) = wPExP4ME
         pmsa( ipnt( 2160) ) = wPExAMME
         pmsa( ipnt( 2161) ) = wNExN4ME
         pmsa( ipnt( 2162) ) = wNExN3ME
         pmsa( ipnt( 2163) ) = wSixS2ME
         pmsa( ipnt( 2164) ) = wO2ExME
         pmsa( ipnt( 2165) ) = wDExDtME
         pmsa( ipnt( 2166) ) = wPExDtME
         pmsa( ipnt( 2167) ) = wNExDtME
         pmsa( ipnt( 2168) ) = wSiExtME
         pmsa( ipnt( 2169) ) = wDExDiME
         pmsa( ipnt( 2170) ) = wPExDiME
         pmsa( ipnt( 2171) ) = wNExDiME
         pmsa( ipnt( 2172) ) = wSiExiME
         pmsa( ipnt( 2173) ) = wDEGrnME
         pmsa( ipnt( 2174) ) = wPEGrnME
         pmsa( ipnt( 2175) ) = wNEGrnME
         pmsa( ipnt( 2176) ) = wDEBleME
         pmsa( ipnt( 2177) ) = wPEBleME
         pmsa( ipnt( 2178) ) = wNEBleME
         pmsa( ipnt( 2179) ) = wDExZoME
         pmsa( ipnt( 2180) ) = wPExZoME
         pmsa( ipnt( 2181) ) = wNExZoME
         pmsa( ipnt( 2182) ) = wDExIME
         pmsa( ipnt( 2183) ) = wPExPO4E
         pmsa( ipnt( 2184) ) = wPExAIME
         pmsa( ipnt( 2185) ) = wNExNH4E
         pmsa( ipnt( 2186) ) = wNExNO3E
         pmsa( ipnt( 2187) ) = wSixSO2E
         pmsa( ipnt( 2188) ) = wO2ExE
         pmsa( ipnt( 2189) ) = wDExDtE
         pmsa( ipnt( 2190) ) = wPExDtE
         pmsa( ipnt( 2191) ) = wNExDtE
         pmsa( ipnt( 2192) ) = wSiExDtE
         pmsa( ipnt( 2193) ) = wDExDiE
         pmsa( ipnt( 2194) ) = wPExDiE
         pmsa( ipnt( 2195) ) = wNExDiE
         pmsa( ipnt( 2196) ) = wSiExDiE
         pmsa( ipnt( 2197) ) = wDExGenE
         pmsa( ipnt( 2198) ) = wPExGenE
         pmsa( ipnt( 2199) ) = wNExGenE
         pmsa( ipnt( 2200) ) = wDExBueE
         pmsa( ipnt( 2201) ) = wPExBueE
         pmsa( ipnt( 2202) ) = wNExBueE
         pmsa( ipnt( 2203) ) = wDExZooE
         pmsa( ipnt( 2204) ) = wPExZooE
         pmsa( ipnt( 2205) ) = wNExZooE
         pmsa( ipnt( 2206) ) = tPIfP4WE
         pmsa( ipnt( 2207) ) = tNIfN4WE
         pmsa( ipnt( 2208) ) = tNIfN3WE
         pmsa( ipnt( 2209) ) = tO2AerE
         pmsa( ipnt( 2210) ) = tDTbFhIM
         pmsa( ipnt( 2211) ) = uCoVSIME
         pmsa( ipnt( 2212) ) = tDSetIME
         pmsa( ipnt( 2213) ) = tPSetIME
         pmsa( ipnt( 2214) ) = uCoVSDtE
         pmsa( ipnt( 2215) ) = tDSetDtE
         pmsa( ipnt( 2216) ) = tPSetDtE
         pmsa( ipnt( 2217) ) = tNSetDtE
         pmsa( ipnt( 2218) ) = tSiSeDtE
         pmsa( ipnt( 2219) ) = wDMintWE
         pmsa( ipnt( 2220) ) = wPMintWE
         pmsa( ipnt( 2221) ) = wNMintWE
         pmsa( ipnt( 2222) ) = wSiintWE
         pmsa( ipnt( 2223) ) = aCoO2ODE
         pmsa( ipnt( 2224) ) = wO2intWE
         pmsa( ipnt( 2225) ) = wDDentWE
         pmsa( ipnt( 2226) ) = wNDentWE
         pmsa( ipnt( 2227) ) = aCo2NrWE
         pmsa( ipnt( 2228) ) = wNNitrWE
         pmsa( ipnt( 2229) ) = wO2NirWE
         pmsa( ipnt( 2230) ) = tDMnODtS
         pmsa( ipnt( 2231) ) = tO2MiDtS
         pmsa( ipnt( 2232) ) = tDDenitS
         pmsa( ipnt( 2233) ) = tNDenitS
         pmsa( ipnt( 2234) ) = tNNitrS
         pmsa( ipnt( 2235) ) = tO2NitrS
         pmsa( ipnt( 2236) ) = tDMinumS
         pmsa( ipnt( 2237) ) = tPMinumS
         pmsa( ipnt( 2238) ) = tNMinumS
         pmsa( ipnt( 2239) ) = tPDifO4E
         pmsa( ipnt( 2240) ) = tNDifO3E
         pmsa( ipnt( 2241) ) = tNDifH4E
         pmsa( ipnt( 2242) ) = tO2DifE
         pmsa( ipnt( 2243) ) = aPAsMxWE
         pmsa( ipnt( 2244) ) = aKPAdsWE
         pmsa( ipnt( 2245) ) = aPIoAsWE
         pmsa( ipnt( 2246) ) = aPEqIMWE
         pmsa( ipnt( 2247) ) = wPSrpMWE
         pmsa( ipnt( 2248) ) = aPAdsaxS
         pmsa( ipnt( 2249) ) = aKPAdsS
         pmsa( ipnt( 2250) ) = aPIsodsS
         pmsa( ipnt( 2251) ) = aPEqIMS
         pmsa( ipnt( 2252) ) = tPSorIMS
         pmsa( ipnt( 2253) ) = wO2AboWH
         pmsa( ipnt( 2254) ) = wPAiotWH
         pmsa( ipnt( 2255) ) = wPAio4WH
         pmsa( ipnt( 2256) ) = wPAioMWH
         pmsa( ipnt( 2257) ) = tPAiootT
         pmsa( ipnt( 2258) ) = wNAio4WH
         pmsa( ipnt( 2259) ) = wNAio3WH
         pmsa( ipnt( 2260) ) = wNAiotWH
         pmsa( ipnt( 2261) ) = wSiio2WH
         pmsa( ipnt( 2262) ) = wSibitWH
         pmsa( ipnt( 2263) ) = wDAiotWH
         pmsa( ipnt( 2264) ) = wDAioMWH
         pmsa( ipnt( 2265) ) = wDAioMWE
         pmsa( ipnt( 2266) ) = wDAiotWE
         pmsa( ipnt( 2267) ) = tDAbiIMS
         pmsa( ipnt( 2268) ) = tDAbiDtS
         pmsa( ipnt( 2269) ) = tDAioumS
         pmsa( ipnt( 2270) ) = tDAiootT
         pmsa( ipnt( 2271) ) = wO2AboWE
         pmsa( ipnt( 2272) ) = wO2AbioM
         pmsa( ipnt( 2273) ) = wPAiotWE
         pmsa( ipnt( 2274) ) = wPAio4WE
         pmsa( ipnt( 2275) ) = wPAioMWE
         pmsa( ipnt( 2276) ) = tPAbiDtS
         pmsa( ipnt( 2277) ) = tPAioumS
         pmsa( ipnt( 2278) ) = tPAioO4S
         pmsa( ipnt( 2279) ) = tPAioIMS
         pmsa( ipnt( 2280) ) = wNAio4WE
         pmsa( ipnt( 2281) ) = wNAio3WE
         pmsa( ipnt( 2282) ) = wNAiotWE
         pmsa( ipnt( 2283) ) = tNAioH4S
         pmsa( ipnt( 2284) ) = tNAioO3S
         pmsa( ipnt( 2285) ) = tNAbiDtS
         pmsa( ipnt( 2286) ) = tNAioumS
         pmsa( ipnt( 2287) ) = tNAiootT
         pmsa( ipnt( 2288) ) = wSiio2WE
         pmsa( ipnt( 2289) ) = wSibitWE
         pmsa( ipnt( 2290) ) = tSibiDtS
         pmsa( ipnt( 2291) ) = tDPdSegE
         pmsa( ipnt( 2292) ) = tDMVegWE
         pmsa( ipnt( 2293) ) = tDMVegSE
         pmsa( ipnt( 2294) ) = tDBedVeg
         pmsa( ipnt( 2295) ) = tO2roegE
         pmsa( ipnt( 2296) ) = tO2spgWE
         pmsa( ipnt( 2297) ) = tO2odgSE
         pmsa( ipnt( 2298) ) = tO2odgWE
         pmsa( ipnt( 2299) ) = tO2BedS
         pmsa( ipnt( 2300) ) = tO2O3gWE
         pmsa( ipnt( 2301) ) = tPMVegE
         pmsa( ipnt( 2302) ) = tPMegO4E
         pmsa( ipnt( 2303) ) = tPMeg4SE
         pmsa( ipnt( 2304) ) = tPMeg4WE
         pmsa( ipnt( 2305) ) = tPMVeDtE
         pmsa( ipnt( 2306) ) = tPMegtWE
         pmsa( ipnt( 2307) ) = tPMegtSE
         pmsa( ipnt( 2308) ) = tPBedVeg
         pmsa( ipnt( 2309) ) = tNMeg4WE
         pmsa( ipnt( 2310) ) = tNMVeDtE
         pmsa( ipnt( 2311) ) = tNMegtWE
         pmsa( ipnt( 2312) ) = tNMegtSE
         pmsa( ipnt( 2313) ) = tNBedVeg
         pmsa( ipnt( 2314) ) = wDBedtWE
         pmsa( ipnt( 2315) ) = tDBedtSE
         pmsa( ipnt( 2316) ) = wPBdP4WE
         pmsa( ipnt( 2317) ) = wPBedtWE
         pmsa( ipnt( 2318) ) = tPBedO4S
         pmsa( ipnt( 2319) ) = tPBedDtS
         pmsa( ipnt( 2320) ) = wNBdN4WE
         pmsa( ipnt( 2321) ) = wNBdN3WE
         pmsa( ipnt( 2322) ) = wNBedtWE
         pmsa( ipnt( 2323) ) = tNBedH4S
         pmsa( ipnt( 2324) ) = tNBedO3S
         pmsa( ipnt( 2325) ) = tNBedDtS
         pmsa( ipnt( 2326) ) = tO2BedWE
         pmsa( ipnt( 2327) ) = aVPxCueE
         pmsa( ipnt( 2328) ) = aVPUBueE
         pmsa( ipnt( 2329) ) = wPUBlueE
         pmsa( ipnt( 2330) ) = aVNxCueE
         pmsa( ipnt( 2331) ) = ahNUBueE
         pmsa( ipnt( 2332) ) = aVNUBueE
         pmsa( ipnt( 2333) ) = wNUBlueE
         pmsa( ipnt( 2334) ) = afN4UueE
         pmsa( ipnt( 2335) ) = wNUH4ueE
         pmsa( ipnt( 2336) ) = wNUO3ueE
         pmsa( ipnt( 2337) ) = aPLmBueE
         pmsa( ipnt( 2338) ) = aNLmBueE
         pmsa( ipnt( 2339) ) = aSiimueE
         pmsa( ipnt( 2340) ) = aLLmBueE
         pmsa( ipnt( 2341) ) = aMumLueE
         pmsa( ipnt( 2342) ) = aNuimueE
         pmsa( ipnt( 2343) ) = aMuBlueE
         pmsa( ipnt( 2344) ) = wDAsBueE
         pmsa( ipnt( 2345) ) = rChDBueE
         pmsa( ipnt( 2346) ) = oChlBlE
         pmsa( ipnt( 2347) ) = aExChueE
         pmsa( ipnt( 2348) ) = wDRpBeWE
         pmsa( ipnt( 2349) ) = wDLssueE
         pmsa( ipnt( 2350) ) = wDMBleWE
         pmsa( ipnt( 2351) ) = ukDecueE
         pmsa( ipnt( 2352) ) = wPErBeWE
         pmsa( ipnt( 2353) ) = wPLssueE
         pmsa( ipnt( 2354) ) = wPMBleWE
         pmsa( ipnt( 2355) ) = wNErBeWE
         pmsa( ipnt( 2356) ) = wNLssueE
         pmsa( ipnt( 2357) ) = wNMBleWE
         pmsa( ipnt( 2358) ) = wDPmBeWE
         pmsa( ipnt( 2359) ) = wPPmBeWE
         pmsa( ipnt( 2360) ) = wNPmBeWE
         pmsa( ipnt( 2361) ) = tDPimueS
         pmsa( ipnt( 2362) ) = tPPimueS
         pmsa( ipnt( 2363) ) = tNPimueS
         pmsa( ipnt( 2364) ) = aVPxCenE
         pmsa( ipnt( 2365) ) = aVPUGenE
         pmsa( ipnt( 2366) ) = wPUGrenE
         pmsa( ipnt( 2367) ) = aVNxCenE
         pmsa( ipnt( 2368) ) = ahNUGenE
         pmsa( ipnt( 2369) ) = aVNUGenE
         pmsa( ipnt( 2370) ) = wNUGrenE
         pmsa( ipnt( 2371) ) = afN4UenE
         pmsa( ipnt( 2372) ) = wNUH4enE
         pmsa( ipnt( 2373) ) = wNUO3enE
         pmsa( ipnt( 2374) ) = aPLmGenE
         pmsa( ipnt( 2375) ) = aNLmGenE
         pmsa( ipnt( 2376) ) = aSiimenE
         pmsa( ipnt( 2377) ) = aLLmGenE
         pmsa( ipnt( 2378) ) = aMumLenE
         pmsa( ipnt( 2379) ) = aNuimenE
         pmsa( ipnt( 2380) ) = aMuGrenE
         pmsa( ipnt( 2381) ) = wDAsGenE
         pmsa( ipnt( 2382) ) = rChDGenE
         pmsa( ipnt( 2383) ) = oChlGrE
         pmsa( ipnt( 2384) ) = aExChenE
         pmsa( ipnt( 2385) ) = wDRpGnWE
         pmsa( ipnt( 2386) ) = wDLssenE
         pmsa( ipnt( 2387) ) = wDMGrnWE
         pmsa( ipnt( 2388) ) = ukDecenE
         pmsa( ipnt( 2389) ) = wPErGnWE
         pmsa( ipnt( 2390) ) = wPLssenE
         pmsa( ipnt( 2391) ) = wPMGrnWE
         pmsa( ipnt( 2392) ) = wNErGnWE
         pmsa( ipnt( 2393) ) = wNLssenE
         pmsa( ipnt( 2394) ) = wNMGrnWE
         pmsa( ipnt( 2395) ) = wDPmGnWE
         pmsa( ipnt( 2396) ) = wPPmGnWE
         pmsa( ipnt( 2397) ) = wNPmGnWE
         pmsa( ipnt( 2398) ) = tDPimenS
         pmsa( ipnt( 2399) ) = tPPimenS
         pmsa( ipnt( 2400) ) = tNPimenS
         pmsa( ipnt( 2401) ) = aVPaxDiE
         pmsa( ipnt( 2402) ) = aVPUDiE
         pmsa( ipnt( 2403) ) = wPUDiE
         pmsa( ipnt( 2404) ) = aVNaxDiE
         pmsa( ipnt( 2405) ) = ahNUDiE
         pmsa( ipnt( 2406) ) = aVNUDiE
         pmsa( ipnt( 2407) ) = wNUDiE
         pmsa( ipnt( 2408) ) = afNH4DiE
         pmsa( ipnt( 2409) ) = wNUNHDiE
         pmsa( ipnt( 2410) ) = wNUNODiE
         pmsa( ipnt( 2411) ) = aPLimDiE
         pmsa( ipnt( 2412) ) = aNLimDiE
         pmsa( ipnt( 2413) ) = aSiLiDiE
         pmsa( ipnt( 2414) ) = aLLimDiE
         pmsa( ipnt( 2415) ) = aMuTmDiE
         pmsa( ipnt( 2416) ) = aNuLiDiE
         pmsa( ipnt( 2417) ) = aMuDiE
         pmsa( ipnt( 2418) ) = wDAssDiE
         pmsa( ipnt( 2419) ) = rChDDiE
         pmsa( ipnt( 2420) ) = oChlDiE
         pmsa( ipnt( 2421) ) = aExtCDiE
         pmsa( ipnt( 2422) ) = wDRspiWE
         pmsa( ipnt( 2423) ) = wDLosDiE
         pmsa( ipnt( 2424) ) = wDMDiWE
         pmsa( ipnt( 2425) ) = ukDDeDiE
         pmsa( ipnt( 2426) ) = wPEcriWE
         pmsa( ipnt( 2427) ) = wPLosDiE
         pmsa( ipnt( 2428) ) = wPMDiWE
         pmsa( ipnt( 2429) ) = wNEcriWE
         pmsa( ipnt( 2430) ) = wNLosDiE
         pmsa( ipnt( 2431) ) = wNMDiWE
         pmsa( ipnt( 2432) ) = wDPimiWE
         pmsa( ipnt( 2433) ) = wPPimiWE
         pmsa( ipnt( 2434) ) = wNPimiWE
         pmsa( ipnt( 2435) ) = tDPriDiS
         pmsa( ipnt( 2436) ) = tPPriDiS
         pmsa( ipnt( 2437) ) = tNPriDiS
         pmsa( ipnt( 2438) ) = oChlaE
         pmsa( ipnt( 2439) ) = oChla
         pmsa( ipnt( 2440) ) = aLPIE
         pmsa( ipnt( 2441) ) = aLMeasE
         pmsa( ipnt( 2442) ) = oChlMeE
         pmsa( ipnt( 2443) ) = wDAsPytE
         pmsa( ipnt( 2444) ) = wDRpPtWE
         pmsa( ipnt( 2445) ) = wDMPhtWE
         pmsa( ipnt( 2446) ) = tDStPytE
         pmsa( ipnt( 2447) ) = wDLssytE
         pmsa( ipnt( 2448) ) = wDPmPtWE
         pmsa( ipnt( 2449) ) = wPUPhytE
         pmsa( ipnt( 2450) ) = wPErPtWE
         pmsa( ipnt( 2451) ) = wPMPhtWE
         pmsa( ipnt( 2452) ) = tPStPytE
         pmsa( ipnt( 2453) ) = wPLssytE
         pmsa( ipnt( 2454) ) = wPPmPtWE
         pmsa( ipnt( 2455) ) = wNUPhytE
         pmsa( ipnt( 2456) ) = wNUH4ytE
         pmsa( ipnt( 2457) ) = wNUO3ytE
         pmsa( ipnt( 2458) ) = wNErPtWE
         pmsa( ipnt( 2459) ) = wNMPhtWE
         pmsa( ipnt( 2460) ) = tNStPytE
         pmsa( ipnt( 2461) ) = wNLssytE
         pmsa( ipnt( 2462) ) = wNPmPtWE
         pmsa( ipnt( 2463) ) = tDPimytS
         pmsa( ipnt( 2464) ) = tPPimytS
         pmsa( ipnt( 2465) ) = tNPimytS
         pmsa( ipnt( 2466) ) = wSiUDiE
         pmsa( ipnt( 2467) ) = wSixciWE
         pmsa( ipnt( 2468) ) = wSiosDiE
         pmsa( ipnt( 2469) ) = wSiMDiWE
         pmsa( ipnt( 2470) ) = tSiSeDiE
         pmsa( ipnt( 2471) ) = wSiriiWH
         pmsa( ipnt( 2472) ) = wSiriiWE
         pmsa( ipnt( 2473) ) = rCyDBueE
         pmsa( ipnt( 2474) ) = oCyanE
         pmsa( ipnt( 2475) ) = fDDiE
         pmsa( ipnt( 2476) ) = wDPimtWE
         pmsa( ipnt( 2477) ) = tDPriDtS
         pmsa( ipnt( 2478) ) = tDPimotT
         pmsa( ipnt( 2479) ) = wO2odytE
         pmsa( ipnt( 2480) ) = wO2sptWE
         pmsa( ipnt( 2481) ) = wO2O3ytE
         pmsa( ipnt( 2482) ) = wO2PrmWE
         pmsa( ipnt( 2483) ) = tO2spytS
         pmsa( ipnt( 2484) ) = tO2PrimS
         pmsa( ipnt( 2485) ) = tO2PrmSH
         pmsa( ipnt( 2486) ) = tO2PrmSE
         pmsa( ipnt( 2487) ) = wPMyt4WE
         pmsa( ipnt( 2488) ) = wPMhytWE
         pmsa( ipnt( 2489) ) = wPLPhO4E
         pmsa( ipnt( 2490) ) = wPLsPDtE
         pmsa( ipnt( 2491) ) = wPPim4WE
         pmsa( ipnt( 2492) ) = wPPimtWE
         pmsa( ipnt( 2493) ) = tPPriDtS
         pmsa( ipnt( 2494) ) = wNMyt4WE
         pmsa( ipnt( 2495) ) = wNMhytWE
         pmsa( ipnt( 2496) ) = wNLPhH4E
         pmsa( ipnt( 2497) ) = wNLsPDtE
         pmsa( ipnt( 2498) ) = wNPim4WE
         pmsa( ipnt( 2499) ) = wNPim3WE
         pmsa( ipnt( 2500) ) = wNPimtWE
         pmsa( ipnt( 2501) ) = tNPriDtS
         pmsa( ipnt( 2502) ) = wSiim2WE
         pmsa( ipnt( 2503) ) = wSiritWE
         pmsa( ipnt( 2504) ) = tSiriDiS
         pmsa( ipnt( 2505) ) = tSiriDtS
         pmsa( ipnt( 2506) ) = aDptEphE
         pmsa( ipnt( 2507) ) = aReptphE
         pmsa( ipnt( 2508) ) = aChlaHE
         pmsa( ipnt( 2509) ) = aCoPhtWE
         pmsa( ipnt( 2510) ) = rExChytE
         pmsa( ipnt( 2511) ) = rPDZooE
         pmsa( ipnt( 2512) ) = rNDZooE
         pmsa( ipnt( 2513) ) = oDFodooE
         pmsa( ipnt( 2514) ) = aFiltE
         pmsa( ipnt( 2515) ) = aDSatooE
         pmsa( ipnt( 2516) ) = wDEnvooE
         pmsa( ipnt( 2517) ) = wDAssooE
         pmsa( ipnt( 2518) ) = wDCZooE
         pmsa( ipnt( 2519) ) = wDCDtooE
         pmsa( ipnt( 2520) ) = wDCDiooE
         pmsa( ipnt( 2521) ) = wDCreooE
         pmsa( ipnt( 2522) ) = wDCluooE
         pmsa( ipnt( 2523) ) = wDChyooE
         pmsa( ipnt( 2524) ) = wDEZooE
         pmsa( ipnt( 2525) ) = aCoReooE
         pmsa( ipnt( 2526) ) = wDRspooE
         pmsa( ipnt( 2527) ) = wDMZooE
         pmsa( ipnt( 2528) ) = oPFodooE
         pmsa( ipnt( 2529) ) = rPDooooE
         pmsa( ipnt( 2530) ) = wPCDiooE
         pmsa( ipnt( 2531) ) = wPCreooE
         pmsa( ipnt( 2532) ) = wPCluooE
         pmsa( ipnt( 2533) ) = wPChyooE
         pmsa( ipnt( 2534) ) = wPCDtooE
         pmsa( ipnt( 2535) ) = wPCZooE
         pmsa( ipnt( 2536) ) = afPssooE
         pmsa( ipnt( 2537) ) = wPAssooE
         pmsa( ipnt( 2538) ) = wPEZooE
         pmsa( ipnt( 2539) ) = wPEooO4E
         pmsa( ipnt( 2540) ) = wPEZoDtE
         pmsa( ipnt( 2541) ) = akPxcooE
         pmsa( ipnt( 2542) ) = wPEcrooE
         pmsa( ipnt( 2543) ) = wPMZooE
         pmsa( ipnt( 2544) ) = wPMooO4E
         pmsa( ipnt( 2545) ) = wPMZoDtE
         pmsa( ipnt( 2546) ) = oNFodooE
         pmsa( ipnt( 2547) ) = rNDooooE
         pmsa( ipnt( 2548) ) = wNCDiooE
         pmsa( ipnt( 2549) ) = wNCreooE
         pmsa( ipnt( 2550) ) = wNCluooE
         pmsa( ipnt( 2551) ) = wNChyooE
         pmsa( ipnt( 2552) ) = wNCDtooE
         pmsa( ipnt( 2553) ) = wNCZooE
         pmsa( ipnt( 2554) ) = afNssooE
         pmsa( ipnt( 2555) ) = wNAssooE
         pmsa( ipnt( 2556) ) = wNEZooE
         pmsa( ipnt( 2557) ) = wNEooH4E
         pmsa( ipnt( 2558) ) = wNEZoDtE
         pmsa( ipnt( 2559) ) = kNEcrooE
         pmsa( ipnt( 2560) ) = wNEcrooE
         pmsa( ipnt( 2561) ) = wNMZooE
         pmsa( ipnt( 2562) ) = wNMooH4E
         pmsa( ipnt( 2563) ) = wNMZoDtE
         pmsa( ipnt( 2564) ) = wSiDiooE
         pmsa( ipnt( 2565) ) = rPDFiJv
         pmsa( ipnt( 2566) ) = rPDFiAd
         pmsa( ipnt( 2567) ) = rNDFiJv
         pmsa( ipnt( 2568) ) = rNDFiAd
         pmsa( ipnt( 2569) ) = tDRprish
         pmsa( ipnt( 2570) ) = tDAgeish
         pmsa( ipnt( 2571) ) = aDTotZoo
         pmsa( ipnt( 2572) ) = aPTotZoo
         pmsa( ipnt( 2573) ) = aNTotZoo
         pmsa( ipnt( 2574) ) = aDSatiJv
         pmsa( ipnt( 2575) ) = tDEnviJv
         pmsa( ipnt( 2576) ) = tDAssiJv
         pmsa( ipnt( 2577) ) = tDCFiJv
         pmsa( ipnt( 2578) ) = tDEFiJv
         pmsa( ipnt( 2579) ) = tDRspiJv
         pmsa( ipnt( 2580) ) = tDMFiJv
         pmsa( ipnt( 2581) ) = tDMgriJv
         pmsa( ipnt( 2582) ) = tDEnviAd
         pmsa( ipnt( 2583) ) = tDAssiAd
         pmsa( ipnt( 2584) ) = tDCFiAd
         pmsa( ipnt( 2585) ) = tDEFiAd
         pmsa( ipnt( 2586) ) = tDRspiAd
         pmsa( ipnt( 2587) ) = tDMFiAd
         pmsa( ipnt( 2588) ) = tDHrvish
         pmsa( ipnt( 2589) ) = tDMgriAd
         pmsa( ipnt( 2590) ) = tDMiJBot
         pmsa( ipnt( 2591) ) = tDMFivDt
         pmsa( ipnt( 2592) ) = tDMiABot
         pmsa( ipnt( 2593) ) = tDMFidDt
         pmsa( ipnt( 2594) ) = tPRprish
         pmsa( ipnt( 2595) ) = tPAgeish
         pmsa( ipnt( 2596) ) = tPMgriJv
         pmsa( ipnt( 2597) ) = tPCFiJv
         pmsa( ipnt( 2598) ) = afPssiJv
         pmsa( ipnt( 2599) ) = tPAssiJv
         pmsa( ipnt( 2600) ) = tPEFiJv
         pmsa( ipnt( 2601) ) = tPEcriJv
         pmsa( ipnt( 2602) ) = tPMFiJv
         pmsa( ipnt( 2603) ) = tPMgriAd
         pmsa( ipnt( 2604) ) = tPCFiAd
         pmsa( ipnt( 2605) ) = tPAssiAd
         pmsa( ipnt( 2606) ) = tPEFiAd
         pmsa( ipnt( 2607) ) = tPEcriAd
         pmsa( ipnt( 2608) ) = tPMFiAd
         pmsa( ipnt( 2609) ) = tPHrvish
         pmsa( ipnt( 2610) ) = tPMiJBot
         pmsa( ipnt( 2611) ) = tPMiJPO4
         pmsa( ipnt( 2612) ) = tPMFivDt
         pmsa( ipnt( 2613) ) = tPMiABot
         pmsa( ipnt( 2614) ) = tPMiAPO4
         pmsa( ipnt( 2615) ) = tPMFidDt
         pmsa( ipnt( 2616) ) = tPEiJPO4
         pmsa( ipnt( 2617) ) = tPEFivDt
         pmsa( ipnt( 2618) ) = tPEiAPO4
         pmsa( ipnt( 2619) ) = tPEFidDt
         pmsa( ipnt( 2620) ) = tNRprish
         pmsa( ipnt( 2621) ) = tNAgeish
         pmsa( ipnt( 2622) ) = tNMgriJv
         pmsa( ipnt( 2623) ) = tNCFiJv
         pmsa( ipnt( 2624) ) = afNssiJv
         pmsa( ipnt( 2625) ) = tNAssiJv
         pmsa( ipnt( 2626) ) = tNEFiJv
         pmsa( ipnt( 2627) ) = tNEcriJv
         pmsa( ipnt( 2628) ) = tNMFiJv
         pmsa( ipnt( 2629) ) = tNMgriAd
         pmsa( ipnt( 2630) ) = tNCFiAd
         pmsa( ipnt( 2631) ) = tNAssiAd
         pmsa( ipnt( 2632) ) = tNEFiAd
         pmsa( ipnt( 2633) ) = tNEcriAd
         pmsa( ipnt( 2634) ) = tNMFiAd
         pmsa( ipnt( 2635) ) = tNHrvish
         pmsa( ipnt( 2636) ) = tNMiABot
         pmsa( ipnt( 2637) ) = tNMiANH4
         pmsa( ipnt( 2638) ) = tNMFidDt
         pmsa( ipnt( 2639) ) = tNMiJBot
         pmsa( ipnt( 2640) ) = tNMiJNH4
         pmsa( ipnt( 2641) ) = tNMFivDt
         pmsa( ipnt( 2642) ) = tNEiJNH4
         pmsa( ipnt( 2643) ) = tNEFivDt
         pmsa( ipnt( 2644) ) = tDMgrisc
         pmsa( ipnt( 2645) ) = aDFish
         pmsa( ipnt( 2646) ) = aDSatisc
         pmsa( ipnt( 2647) ) = tDEnvisc
         pmsa( ipnt( 2648) ) = tDAssisc
         pmsa( ipnt( 2649) ) = tDCPisc
         pmsa( ipnt( 2650) ) = tDEPisc
         pmsa( ipnt( 2651) ) = tDCiJisc
         pmsa( ipnt( 2652) ) = tDCiAisc
         pmsa( ipnt( 2653) ) = tDMPisc
         pmsa( ipnt( 2654) ) = tDMisBot
         pmsa( ipnt( 2655) ) = tDMPicDt
         pmsa( ipnt( 2656) ) = tDHrvisc
         pmsa( ipnt( 2657) ) = aPPisc
         pmsa( ipnt( 2658) ) = tPCiJisc
         pmsa( ipnt( 2659) ) = tPCiAisc
         pmsa( ipnt( 2660) ) = tPCPisc
         pmsa( ipnt( 2661) ) = rPDooisc
         pmsa( ipnt( 2662) ) = aNPisc
         pmsa( ipnt( 2663) ) = tNCiJisc
         pmsa( ipnt( 2664) ) = tNCiAisc
         pmsa( ipnt( 2665) ) = tNCPisc
         pmsa( ipnt( 2666) ) = rNDooisc
         pmsa( ipnt( 2667) ) = tDCPiscN
         pmsa( ipnt( 2668) ) = tDEPiscN
         pmsa( ipnt( 2669) ) = tDCFJPNu
         pmsa( ipnt( 2670) ) = tDCFAPNu
         pmsa( ipnt( 2671) ) = tPCFJPNu
         pmsa( ipnt( 2672) ) = tPCFAPNu
         pmsa( ipnt( 2673) ) = tPCPiscN
         pmsa( ipnt( 2674) ) = tNCFJPNu
         pmsa( ipnt( 2675) ) = tNCFAPNu
         pmsa( ipnt( 2676) ) = tNCPiscN
         pmsa( ipnt( 2677) ) = afPssisc
         pmsa( ipnt( 2678) ) = tPAssisc
         pmsa( ipnt( 2679) ) = tPEPisc
         pmsa( ipnt( 2680) ) = tPEisPO4
         pmsa( ipnt( 2681) ) = tPEPicDt
         pmsa( ipnt( 2682) ) = tDRspisc
         pmsa( ipnt( 2683) ) = tPEcrisc
         pmsa( ipnt( 2684) ) = tPMPisc
         pmsa( ipnt( 2685) ) = tPMisBot
         pmsa( ipnt( 2686) ) = tPMisPO4
         pmsa( ipnt( 2687) ) = tPMPicDt
         pmsa( ipnt( 2688) ) = tPMgrisc
         pmsa( ipnt( 2689) ) = tPHrvisc
         pmsa( ipnt( 2690) ) = afNssisc
         pmsa( ipnt( 2691) ) = tNAssisc
         pmsa( ipnt( 2692) ) = tNEPisc
         pmsa( ipnt( 2693) ) = tNEisNH4
         pmsa( ipnt( 2694) ) = tNEPicDt
         pmsa( ipnt( 2695) ) = tNEcrisc
         pmsa( ipnt( 2696) ) = tNMPisc
         pmsa( ipnt( 2697) ) = tNMisBot
         pmsa( ipnt( 2698) ) = tNMisNH4
         pmsa( ipnt( 2699) ) = tNMPicDt
         pmsa( ipnt( 2700) ) = tNMgrisc
         pmsa( ipnt( 2701) ) = tNHrvisc
         pmsa( ipnt( 2702) ) = tDWebBnt
         pmsa( ipnt( 2703) ) = tPWebBnt
         pmsa( ipnt( 2704) ) = tNWebBnt
         pmsa( ipnt( 2705) ) = tDWebiJv
         pmsa( ipnt( 2706) ) = tPWebiJv
         pmsa( ipnt( 2707) ) = tNWebiJv
         pmsa( ipnt( 2708) ) = tDWebiAd
         pmsa( ipnt( 2709) ) = tPWebiAd
         pmsa( ipnt( 2710) ) = tNWebiAd
         pmsa( ipnt( 2711) ) = tDWebisc
         pmsa( ipnt( 2712) ) = wDWebooH
         pmsa( ipnt( 2713) ) = wDWebtWH
         pmsa( ipnt( 2714) ) = wDWebiWH
         pmsa( ipnt( 2715) ) = wDWbGnWH
         pmsa( ipnt( 2716) ) = wDWbBeWH
         pmsa( ipnt( 2717) ) = wDWebooE
         pmsa( ipnt( 2718) ) = wDWebtWE
         pmsa( ipnt( 2719) ) = wDWebiWE
         pmsa( ipnt( 2720) ) = wDWbGnWE
         pmsa( ipnt( 2721) ) = wDWbBeWE
         pmsa( ipnt( 2722) ) = tDWebotT
         pmsa( ipnt( 2723) ) = wPWebooH
         pmsa( ipnt( 2724) ) = wPWbP4WH
         pmsa( ipnt( 2725) ) = wPWebtWH
         pmsa( ipnt( 2726) ) = wPWebiWH
         pmsa( ipnt( 2727) ) = wPWbGnWH
         pmsa( ipnt( 2728) ) = wPWbBeWH
         pmsa( ipnt( 2729) ) = wPWebooE
         pmsa( ipnt( 2730) ) = wPWbP4WE
         pmsa( ipnt( 2731) ) = wPWebtWE
         pmsa( ipnt( 2732) ) = wPWebiWE
         pmsa( ipnt( 2733) ) = wPWbGnWE
         pmsa( ipnt( 2734) ) = wPWbBeWE
         pmsa( ipnt( 2735) ) = tPWebotT
         pmsa( ipnt( 2736) ) = tNEiANH4
         pmsa( ipnt( 2737) ) = tNEFidDt
         pmsa( ipnt( 2738) ) = wNWbN3WH
         pmsa( ipnt( 2739) ) = wNWbN3WE
         pmsa( ipnt( 2740) ) = wNWebooH
         pmsa( ipnt( 2741) ) = wNWbN4WH
         pmsa( ipnt( 2742) ) = wNWebtWH
         pmsa( ipnt( 2743) ) = wNWebiWH
         pmsa( ipnt( 2744) ) = wNWbGnWH
         pmsa( ipnt( 2745) ) = wNWbBeWH
         pmsa( ipnt( 2746) ) = wNWebooE
         pmsa( ipnt( 2747) ) = wNWbN4WE
         pmsa( ipnt( 2748) ) = wNWebtWE
         pmsa( ipnt( 2749) ) = wNWebiWE
         pmsa( ipnt( 2750) ) = wNWbGnWE
         pmsa( ipnt( 2751) ) = wNWbBeWE
         pmsa( ipnt( 2752) ) = tNWebotT
         pmsa( ipnt( 2753) ) = wSiebtWE
         pmsa( ipnt( 2754) ) = aPrefveE
         pmsa( ipnt( 2755) ) = wDCZoo2E
         pmsa( ipnt( 2756) ) = aDCZoSpE
         pmsa( ipnt( 2757) ) = aDAsZSpE
         pmsa( ipnt( 2758) ) = aDGraSpE
         pmsa( ipnt( 2759) ) = aPCZoSpE
         pmsa( ipnt( 2760) ) = aPGraSpE
         pmsa( ipnt( 2761) ) = aNCZoSpE
         pmsa( ipnt( 2762) ) = aNGraSpE
         pmsa( ipnt( 2763) ) = tDSetotE
         pmsa( ipnt( 2764) ) = tPSetotE
         pmsa( ipnt( 2765) ) = tNSetotE
         pmsa( ipnt( 2766) ) = tDRsuTot
         pmsa( ipnt( 2767) ) = tPRsuTot
         pmsa( ipnt( 2768) ) = tNRsuTot
         pmsa( ipnt( 2769) ) = aDptSart
         pmsa( ipnt( 2770) ) = akDreDpt
         pmsa( ipnt( 2771) ) = akDred
         pmsa( ipnt( 2772) ) = akDreBnt
         pmsa( ipnt( 2773) ) = vDredptW
         pmsa( ipnt( 2774) ) = tDDreDtS
         pmsa( ipnt( 2775) ) = tPDreDtS
         pmsa( ipnt( 2776) ) = tNDreDtS
         pmsa( ipnt( 2777) ) = tSireDtS
         pmsa( ipnt( 2778) ) = tPDedIMS
         pmsa( ipnt( 2779) ) = tDDdNoil
         pmsa( ipnt( 2780) ) = tDDdNIMS
         pmsa( ipnt( 2781) ) = tDDdNumS
         pmsa( ipnt( 2782) ) = tPDdNumS
         pmsa( ipnt( 2783) ) = tNDdNumS
         pmsa( ipnt( 2784) ) = tDDreDiS
         pmsa( ipnt( 2785) ) = tPDreDiS
         pmsa( ipnt( 2786) ) = tNDreDiS
         pmsa( ipnt( 2787) ) = tDDedenS
         pmsa( ipnt( 2788) ) = tPDedenS
         pmsa( ipnt( 2789) ) = tNDedenS
         pmsa( ipnt( 2790) ) = tDDedueS
         pmsa( ipnt( 2791) ) = tPDedueS
         pmsa( ipnt( 2792) ) = tNDedueS
         pmsa( ipnt( 2793) ) = tDDedytS
         pmsa( ipnt( 2794) ) = tPDedytS
         pmsa( ipnt( 2795) ) = tNDedytS
         pmsa( ipnt( 2796) ) = tDDreBnt
         pmsa( ipnt( 2797) ) = tPDreBnt
         pmsa( ipnt( 2798) ) = tNDreBnt
         pmsa( ipnt( 2799) ) = tDDreVeg
         pmsa( ipnt( 2800) ) = tPDreVeg
         pmsa( ipnt( 2801) ) = tNDreVeg
         pmsa( ipnt( 2802) ) = tDDdNTot
         pmsa( ipnt( 2803) ) = tPDdNTot
         pmsa( ipnt( 2804) ) = tNDdNTot
         pmsa( ipnt( 2805) ) = tSireTot
         pmsa( ipnt( 2806) ) = tDIMS
         pmsa( ipnt( 2807) ) = tDHumS
         pmsa( ipnt( 2808) ) = tDDtS
         pmsa( ipnt( 2809) ) = vDeltaS
         pmsa( ipnt( 2810) ) = vDeltaW
         pmsa( ipnt( 2811) ) = tDBurIM
         pmsa( ipnt( 2812) ) = tDBurOM
         pmsa( ipnt( 2813) ) = tDBurDt
         pmsa( ipnt( 2814) ) = tDBurHum
         pmsa( ipnt( 2815) ) = tDBurTot
         pmsa( ipnt( 2816) ) = tPBurHum
         pmsa( ipnt( 2817) ) = tPBurDt
         pmsa( ipnt( 2818) ) = tPBurAIM
         pmsa( ipnt( 2819) ) = tPBurPO4
         pmsa( ipnt( 2820) ) = tPBurTot
         pmsa( ipnt( 2821) ) = tNBurHum
         pmsa( ipnt( 2822) ) = tNBurDt
         pmsa( ipnt( 2823) ) = tNBurNH4
         pmsa( ipnt( 2824) ) = tNBurNO3
         pmsa( ipnt( 2825) ) = tNBurTot
         pmsa( ipnt( 2826) ) = tSiBurDt
         pmsa( ipnt( 2827) ) = tSiBuTot
         pmsa( ipnt( 2828) ) = aReDeaWE
         pmsa( ipnt( 2829) ) = aReDeaWH
         pmsa( ipnt( 2830) ) = aPFish
         pmsa( ipnt( 2831) ) = aNFish
         pmsa( ipnt( 2832) ) = aDRelotT
         pmsa( ipnt( 2833) ) = aNRelotT
         pmsa( ipnt( 2834) ) = aPRelotT
         pmsa( ipnt( 2835) ) = aSielotT
         pmsa( ipnt( 2836) ) = aO2elotT
         pmsa( ipnt( 2837) ) = aDTotTH
         pmsa( ipnt( 2838) ) = aNTotTH
         pmsa( ipnt( 2839) ) = aPTotTH
         pmsa( ipnt( 2840) ) = aSiTotTH
         pmsa( ipnt( 2841) ) = aO2TotTH
         pmsa( ipnt( 2842) ) = aDTotT
         pmsa( ipnt( 2843) ) = aNTotT
         pmsa( ipnt( 2844) ) = aPTotT
         pmsa( ipnt( 2845) ) = aSiTotT
         pmsa( ipnt( 2846) ) = aO2TotT
         pmsa( ipnt( 2847) ) = tDBedotT
         pmsa( ipnt( 2848) ) = tPBedotT
         pmsa( ipnt( 2849) ) = tNBedotT
         pmsa( ipnt( 2850) ) = aDError
         pmsa( ipnt( 2851) ) = aNError
         pmsa( ipnt( 2852) ) = aPError
         pmsa( ipnt( 2853) ) = aSiError
         pmsa( ipnt( 2854) ) = aO2Error
         pmsa( ipnt( 2855) ) = cChVegH
         pmsa( ipnt( 2856) ) = dNH4WH
         pmsa( ipnt( 2857) ) = dNO3WH
         pmsa( ipnt( 2858) ) = dPO4WH
         pmsa( ipnt( 2859) ) = dPAIMWH
         pmsa( ipnt( 2860) ) = dSiO2WH
         pmsa( ipnt( 2861) ) = dO2WH
         pmsa( ipnt( 2862) ) = dDDetWH
         pmsa( ipnt( 2863) ) = dNDetWH
         pmsa( ipnt( 2864) ) = dPDetWH
         pmsa( ipnt( 2865) ) = dSiDetWH
         pmsa( ipnt( 2866) ) = dDIMWH
         pmsa( ipnt( 2867) ) = dDDiatWH
         pmsa( ipnt( 2868) ) = dNDiatWH
         pmsa( ipnt( 2869) ) = dPDiatWH
         pmsa( ipnt( 2870) ) = dDGrenWH
         pmsa( ipnt( 2871) ) = dNGrenWH
         pmsa( ipnt( 2872) ) = dPGrenWH
         pmsa( ipnt( 2873) ) = dDBlueWH
         pmsa( ipnt( 2874) ) = dNBlueWH
         pmsa( ipnt( 2875) ) = dPBlueWH
         pmsa( ipnt( 2876) ) = dDZooH
         pmsa( ipnt( 2877) ) = dNZooH
         pmsa( ipnt( 2878) ) = dPZooH
         pmsa( ipnt( 2879) ) = dDFiAd
         pmsa( ipnt( 2880) ) = dDFiJv
         pmsa( ipnt( 2881) ) = dNFiAd
         pmsa( ipnt( 2882) ) = dNFiJv
         pmsa( ipnt( 2883) ) = dPFiAd
         pmsa( ipnt( 2884) ) = dPFiJv
         pmsa( ipnt( 2885) ) = dDPisc
         pmsa( ipnt( 2886) ) = dNH4S
         pmsa( ipnt( 2887) ) = dNO3S
         pmsa( ipnt( 2888) ) = dPO4S
         pmsa( ipnt( 2889) ) = dPAIMS
         pmsa( ipnt( 2890) ) = dDDetS
         pmsa( ipnt( 2891) ) = dNDetS
         pmsa( ipnt( 2892) ) = dPDetS
         pmsa( ipnt( 2893) ) = dSiDetS
         pmsa( ipnt( 2894) ) = dDHumS
         pmsa( ipnt( 2895) ) = dNHumS
         pmsa( ipnt( 2896) ) = dPHumS
         pmsa( ipnt( 2897) ) = dDIMS
         pmsa( ipnt( 2898) ) = dDDiatS
         pmsa( ipnt( 2899) ) = dNDiatS
         pmsa( ipnt( 2900) ) = dPDiatS
         pmsa( ipnt( 2901) ) = dDGrenS
         pmsa( ipnt( 2902) ) = dNGrenS
         pmsa( ipnt( 2903) ) = dPGrenS
         pmsa( ipnt( 2904) ) = dDBlueS
         pmsa( ipnt( 2905) ) = dNBlueS
         pmsa( ipnt( 2906) ) = dPBlueS
         pmsa( ipnt( 2907) ) = dDVeg
         pmsa( ipnt( 2908) ) = dNVeg
         pmsa( ipnt( 2909) ) = dPVeg
         pmsa( ipnt( 2910) ) = dVegHe
         pmsa( ipnt( 2911) ) = dDBent
         pmsa( ipnt( 2912) ) = dNBent
         pmsa( ipnt( 2913) ) = dPBent
         pmsa( ipnt( 2914) ) = dDepthWM
         pmsa( ipnt( 2915) ) = dNH4WM
         pmsa( ipnt( 2916) ) = dNO3WM
         pmsa( ipnt( 2917) ) = dPO4WM
         pmsa( ipnt( 2918) ) = dPAIMWM
         pmsa( ipnt( 2919) ) = dSiO2WM
         pmsa( ipnt( 2920) ) = dO2WM
         pmsa( ipnt( 2921) ) = dDDetWM
         pmsa( ipnt( 2922) ) = dNDetWM
         pmsa( ipnt( 2923) ) = dPDetWM
         pmsa( ipnt( 2924) ) = dSiDetWM
         pmsa( ipnt( 2925) ) = dDIMWM
         pmsa( ipnt( 2926) ) = dDDiatWM
         pmsa( ipnt( 2927) ) = dNDiatWM
         pmsa( ipnt( 2928) ) = dPDiatWM
         pmsa( ipnt( 2929) ) = dDGrenWM
         pmsa( ipnt( 2930) ) = dNGrenWM
         pmsa( ipnt( 2931) ) = dPGrenWM
         pmsa( ipnt( 2932) ) = dDBlueWM
         pmsa( ipnt( 2933) ) = dNBlueWM
         pmsa( ipnt( 2934) ) = dPBlueWM
         pmsa( ipnt( 2935) ) = dDZooM
         pmsa( ipnt( 2936) ) = dNZooM
         pmsa( ipnt( 2937) ) = dPZooM
         pmsa( ipnt( 2938) ) = dNH4SM
         pmsa( ipnt( 2939) ) = dNO3SM
         pmsa( ipnt( 2940) ) = dPO4SM
         pmsa( ipnt( 2941) ) = dPAIMSM
         pmsa( ipnt( 2942) ) = dDDetSM
         pmsa( ipnt( 2943) ) = dNDetSM
         pmsa( ipnt( 2944) ) = dPDetSM
         pmsa( ipnt( 2945) ) = dSiDetSM
         pmsa( ipnt( 2946) ) = dDHumSM
         pmsa( ipnt( 2947) ) = dNHumSM
         pmsa( ipnt( 2948) ) = dPHumSM
         pmsa( ipnt( 2949) ) = dDIMSM
         pmsa( ipnt( 2950) ) = dDRoPhra
         pmsa( ipnt( 2951) ) = dDShPhra
         pmsa( ipnt( 2952) ) = dNRoPhra
         pmsa( ipnt( 2953) ) = dNShPhra
         pmsa( ipnt( 2954) ) = dPRoPhra
         pmsa( ipnt( 2955) ) = dPShPhra
         pmsa( ipnt( 2956) ) = dNH4WE
         pmsa( ipnt( 2957) ) = dNO3WE
         pmsa( ipnt( 2958) ) = dPO4WE
         pmsa( ipnt( 2959) ) = dPAIMWE
         pmsa( ipnt( 2960) ) = dSiO2WE
         pmsa( ipnt( 2961) ) = dO2WE
         pmsa( ipnt( 2962) ) = dDDetWE
         pmsa( ipnt( 2963) ) = dNDetWE
         pmsa( ipnt( 2964) ) = dPDetWE
         pmsa( ipnt( 2965) ) = dSiDetWE
         pmsa( ipnt( 2966) ) = dDIMWE
         pmsa( ipnt( 2967) ) = dDDiatWE
         pmsa( ipnt( 2968) ) = dNDiatWE
         pmsa( ipnt( 2969) ) = dPDiatWE
         pmsa( ipnt( 2970) ) = dDGrenWE
         pmsa( ipnt( 2971) ) = dNGrenWE
         pmsa( ipnt( 2972) ) = dPGrenWE
         pmsa( ipnt( 2973) ) = dDBlueWE
         pmsa( ipnt( 2974) ) = dNBlueWE
         pmsa( ipnt( 2975) ) = dPBlueWE
         pmsa( ipnt( 2976) ) = dDZooE
         pmsa( ipnt( 2977) ) = dNZooE
         pmsa( ipnt( 2978) ) = dPZooE
         pmsa( ipnt( 2979) ) = dDExTotT
         pmsa( ipnt( 2980) ) = dNExTotT
         pmsa( ipnt( 2981) ) = dPExTotT
         pmsa( ipnt( 2982) ) = dSiETotT
         pmsa( ipnt( 2983) ) = dO2ETotT
         ID0sNH4WH               = ID0sNH4WH                + noflux
         ID0sNO3WH               = ID0sNO3WH                + noflux
         ID0sPO4WH               = ID0sPO4WH                + noflux
         ID0sPAIMWH              = ID0sPAIMWH               + noflux
         ID0sSiO2WH              = ID0sSiO2WH               + noflux
         ID0sO2WH                = ID0sO2WH                 + noflux
         ID0sDDetWH              = ID0sDDetWH               + noflux
         ID0sNDetWH              = ID0sNDetWH               + noflux
         ID0sPDetWH              = ID0sPDetWH               + noflux
         ID0sSiDetWH             = ID0sSiDetWH              + noflux
         ID0sDIMWH               = ID0sDIMWH                + noflux
         ID0sDDiatWH             = ID0sDDiatWH              + noflux
         ID0sNDiatWH             = ID0sNDiatWH              + noflux
         ID0sPDiatWH             = ID0sPDiatWH              + noflux
         ID0sDGrenWH             = ID0sDGrenWH              + noflux
         ID0sNGrenWH             = ID0sNGrenWH              + noflux
         ID0sPGrenWH             = ID0sPGrenWH              + noflux
         ID0sDBlueWH             = ID0sDBlueWH              + noflux
         ID0sNBlueWH             = ID0sNBlueWH              + noflux
         ID0sPBlueWH             = ID0sPBlueWH              + noflux
         ID0sDZooH               = ID0sDZooH                + noflux
         ID0sNZooH               = ID0sNZooH                + noflux
         ID0sPZooH               = ID0sPZooH                + noflux
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
         ID0sVegHe               = ID0sVegHe                + noflux
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
         ID0sDRoPhra             = ID0sDRoPhra              + noflux
         ID0sDShPhra             = ID0sDShPhra              + noflux
         ID0sNRoPhra             = ID0sNRoPhra              + noflux
         ID0sNShPhra             = ID0sNShPhra              + noflux
         ID0sPRoPhra             = ID0sPRoPhra              + noflux
         ID0sPShPhra             = ID0sPShPhra              + noflux
         ID0sNH4WE               = ID0sNH4WE                + noflux
         ID0sNO3WE               = ID0sNO3WE                + noflux
         ID0sPO4WE               = ID0sPO4WE                + noflux
         ID0sPAIMWE              = ID0sPAIMWE               + noflux
         ID0sSiO2WE              = ID0sSiO2WE               + noflux
         ID0sO2WE                = ID0sO2WE                 + noflux
         ID0sDDetWE              = ID0sDDetWE               + noflux
         ID0sNDetWE              = ID0sNDetWE               + noflux
         ID0sPDetWE              = ID0sPDetWE               + noflux
         ID0sSiDetWE             = ID0sSiDetWE              + noflux
         ID0sDIMWE               = ID0sDIMWE                + noflux
         ID0sDDiatWE             = ID0sDDiatWE              + noflux
         ID0sNDiatWE             = ID0sNDiatWE              + noflux
         ID0sPDiatWE             = ID0sPDiatWE              + noflux
         ID0sDGrenWE             = ID0sDGrenWE              + noflux
         ID0sNGrenWE             = ID0sNGrenWE              + noflux
         ID0sPGrenWE             = ID0sPGrenWE              + noflux
         ID0sDBlueWE             = ID0sDBlueWE              + noflux
         ID0sNBlueWE             = ID0sNBlueWE              + noflux
         ID0sPBlueWE             = ID0sPBlueWE              + noflux
         ID0sDZooE               = ID0sDZooE                + noflux
         ID0sNZooE               = ID0sNZooE                + noflux
         ID0sPZooE               = ID0sPZooE                + noflux
         ID0sDExTotT             = ID0sDExTotT              + noflux
         ID0sNExTotT             = ID0sNExTotT              + noflux
         ID0sPExTotT             = ID0sPExTotT              + noflux
         ID0sSiETotT             = ID0sSiETotT              + noflux
         ID0sO2ETotT             = ID0sO2ETotT              + noflux

         ipnt        = ipnt        + increm 
   !   
    9000 continue  
   !   
         return    
         end subroutine    
