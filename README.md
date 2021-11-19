# SWMM_bioretention

This repository contains code and data necessary to reproduce the analysis including in Lammers et al. (Effecs of design and licmate on bioretention effectiveness for watershed-scale hydrologic benefits - submitted to Journal of Sustainable Water in the Built Environment). Please see this manuscript and supplemental material for details. The specific files included in this repository are described below.

## SWMM Files
Baseline input files for the Storm Water Management Model (SWMM) are included. "SHC_NEW_event_noflood.inp" is used in the design storm modeling and "SHC_NEW_snow.inp" is used for the continuous simulations. Differences between the two are minor and mainly consistent of differences in control parameters (e.g., simulation period, etc.) and the incorporation of temperature and wind climate data for the continuous simulations. The baseline SWMM model for Shayler Crossing was graciously provided by Drs. Joong Gwang Lee and Christopher Nietch.

## Data
This folder contains climate and precipitation necessary for both the design storm and continuous simulations for each of the nine cities included in this analysis. "City_design_stormsv2.csv" contains the NRCS 24-hour design storm temporal patterns specific to each city. "CITYNAME_IDF.csv" contains the precipitation depths for different duration and reccurrence intervals for each city (mostly from NOAA's Atlas 14 web server). "Daily station Temp.Rdata" and "Hourly Station Precip.Rdata" include observed climate data from each city for use in the continuous SWMM simulations. These data were obtained using the "NOAA Rainfall_Redacted.R" script.

## R Scripts
This folder contains R scripts used in this analysis - including scripts for creating SWMM input files, running the models, and analyzing results.

"City 24-hr storms.R" - This script runs SWMM models for each city for 24-hour design storms of various recurrence intervals. These results are then analyzed using various included functions.

"Create SWMM Climate Files.R" - This script converts observed hourly precipitation and daily temperature and wind for each city into input files that can be read by SWMM.

"NOAA Rainfall_redacted.R" - This script obtains continuous climate data for each city from NOAA COOP stations.

"Run SWMM Function.R" - This script has two functions for creating SWMM input files for all continuous and design storm simulations.

"SWMM_Cities_Analysis.R" - This script runs continuous SWMM models for each city and analyzes the results.

"Storm event analysis.R" - This script performs a storm event analysis on the hourly rainfall data for each city. This includes separating rainfall data into discrete storm events and calculating metrics for each storm (e.g., total rainfall, mean intensity). These data are used to analyze the performance of bioretention areas for all continuous SWMM simulations.

"City Continuous Event Plots.Rmd" - This R Markdown document analyzes results and produces plots for all continuous SWMM simulations. This was used to produce many plots in the supplemental material. Commented sections can also be used to reproduce figures from the main body of the manuscript.

"City Design Storm Plots.Rmd" - This R Markdown document analyzes results and produces plots for all design storm SWMM simulations. This code was used to produce supplmental figures and some figures from the main body of the manuscript.


These input data and code should be sufficient to replicate the modeling analysis presented in Lammers et al. We do not include all model files and results here because of the sheer size and number of these files. Please direct any questions or concerns to Rod Lammers (rodlammers@gmail.com).
