# Mesocosm
All data associated with mesocosm experiment examining effects of OM and temp on GHG emissions from simulated isolated pools of non-perennial rivers

This repository includes:
- an R script used to calculaate the CO2 and CH4 fluxes (Mesocosm_GHGdat.R)
- a .csv output of the CO2 and CH4 fluxes (CO2.CH4.fluxes.csv)
- an R script to calculate the leaf litter mass loss (Mesocosm_leaf_litter_mass_loss.R)
- a .csv output of the leaf litter mass loss (leaf_mass_loss.csv)
- a .csv of the daily average iButton data logger water temperature (degrees C) in each mesocosm (Daily_ave_ibutton_temps_mesocosm.csv)
- a .csv of ancillary data collected at each GHG measurement time, including the GHG measurement start and end times, headspace height, dissolved oxygen, water temperature, etc. (Mesocosm_GHG_data_sheet_2022.csv)
- a .csv of the treatment codes from each unique column (leaf_mass_loss.csv)
- an R script used to analyze and visualize the data (Data_analysis_mesocosm.R)

For any questions, contact: teresa.silverthorn@gmail.com

Data dictionary

Column ID: ID for each mesocosm unit in the experiment

ID_unique: unique ID for each mesocosm at each sampling time point

CO2_C_mg_m2_h: CO2 flux in mg/m2/h

CH4_C_mg_m2_h: CH4 flux in mg/m2/h

Date: date of the sampling

start_time: start time of the GHG measurement

end_time: end time of the GHG measurement

Headspace_height_cm: height from the water surface to the top of the mesocosm in cm

DO_mg_L: dissolved oxygen concentration in mg/L

DO_.: dissolved oxygen concentration in %

Water_temp_C : water temperature from point measurements

Note: notes

volume_L: volume of the headspace air of the mesocosm

area_m2: area of exposed sediment of the mesocosm

ibutton_ID: unique ID for the ibutton datalogger used to continuously measure temperature in the mesocosms

leaf_treatment: leaf treatment type (low, medium, high, or control)

sediment_g: mass of sediments from intermittent river in g in the mesocosm at the start of the experiment

dry_input_leaves_g: mass of the dry Alnus leaves put in to the mesocosms at the start of the experiment

dry_output_leaves_g: oven dry mass of the Alnus leaves after the end of the experiment

ceramic_pot_g_leaves: mass of the ceramic container used to measure the ash free dry mass of the leaves remaining

ceramic_pot.burned_leaves_g: mass of the certamic container plus the burned leaves

leaf_OM: percentage of organic matter in the leaf sample 

incubation_water_mL: amount of incubation water from the stream added to the mesocosm in mL 

groundwater_ml: amount of groundwater added to the mesocosm in mL



