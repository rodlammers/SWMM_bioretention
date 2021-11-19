# SWMM_bioretention

This repository contains code and data necessary to reproduce the analysis including in Lammers et al. (Effecs of design and licmate on bioretention effectiveness for watershed-scale hydrologic benefits - submitted to Journal of Sustainable Water in the Built Environment). Please see this manuscript and supplemental material for details. The specific files included in this repository are described below.

## SWMM Files
Baseline input files for the Storm Water Management Model (SWMM) are included. "SHC_NEW_event_noflood.inp" is used in the design storm modeling and "SHC_NEW_snow.inp" is used for the continuous simulations. Differences between the two are minor and mainly consistent of differences in control parameters (e.g., simulation period, etc.) and the incorporation of temperature and wind climate data for the continuous simulations. The baseline SWMM model for Shayler Crossing was graciously provided by Drs. Joong Gwang Lee and Christopher Nietch.

## Data
This folder contains climate and precipitation necessary for both the design storm and continuous simulations for each of the nine cities included in this analysis. "City_design_stormsv2.csv" contains the NRCS 24-hour design storm temporal patterns specific to each city. "CITYNAME_IDF.csv" contains the precipitation depths for different duration and reccurrence intervals for each city (mostly from NOAA's Atlas 14 web server).

