
# 
# SWMM_sims <- function(ratio, dir_name, inp_file, rain_file, rain_name){
#   #Load the required functions
#   library(swmmr)
#   library(dplyr)
#   
#   print(paste("Ratio = ", ratio, "\n"))
#   
#   #This is the slightly modified SWMM model from EPA (I basically changed the start and end dates manually)
#   inp_file <- file.path(dir_name, inp_file)
#   
#   #Read in the input file
#   inputs <- read_inp(inp_file)
#   
#   #Output name
#   out_name <- paste0("SHC_", substr(basename(rain_file), 1, nchar(basename(rain_file)) - 4))
#   
#   #Take subcatchment information from the input file, combine it with the lid_usage information and calculate areas
#   #of pervious and impervious area in each sub-basin. This is important for determining how big our rain gardens
#   #will be
#   subs <- inputs$subcatchments %>%
#     left_join(inputs$lid_usage, by = c("Name" = "Subcatchment")) %>% #combine with lid_usage information
#     #Remove sub-basins which have no (or very small) impervious area or the area is already treated
#     filter(FromImp < 90, !is.na(FromImp)) %>%
#     mutate(BPA = round(Area.y / 43560, 4),
#            imp_area = round((Area.x - BPA) * Perc_Imperv / 100, 4), #adjusted impervious area minus BPA
#            ICIA = round(FromImp / 100 * imp_area, 4), #Calculate some additional variables
#            ratio_imp = round(Area.y / 43560 / ICIA * 100, 4),
#            SPA = round(Area.x - imp_area - Area.y / 43560, 4), #standalone pervious area (Total - impervious - BPA)
#            DCIA = round(imp_area - ICIA, 4), #DCIA is the main one we are interested in
#            max_ratio = SPA / DCIA) %>%
#     filter(DCIA > 0) #only keep sub-basins where DCIA is greater than zero (the other ones don't need rain gardens)
#   
#   #Create a folder for the LID report files
#   lid_folder <- file.path(dir_name, paste0(out_name, "_", 100*ratio, "_LID"))
#   if (ratio != 0){
#     dir.create(lid_folder)
#   }
#   
#   #Determine whether pervious area drains to impervious for each catchment
#   subs2 <- left_join(subs, inputs$subareas, by = c("Name" = "Subcatchment"))
#   pervious <- subs2$RouteTo == "IMPERVIOUS"
#   
#   bio_area <- subs$DCIA * ratio * 43560
#   #bio_area <- if_else(pervious, (subs$SPA * subs$Perc_Imperv / 100 + subs$DCIA) * ratio * 43560, subs$DCIA * ratio * 43560)
#   #from_perv <- if_else(pervious, subs$Perc_Imperv, 0)
#   
#   #Create a data.frame for all the rain gardens we are adding
#   rain_gardens <- data.frame(Subcatchment = subs$Name, #sub-catchment names from filtered data above
#                              LID_Process = "BioRet1", #This is the name of the LID practice in the model (probably will need to change)
#                              Number = 1, #Only 1 rain garden per sub-basin
#                              Area = bio_area, #Calculate area of rain garden based on drainage area ratio, convert to ft2. If pervious area drains to impervious, need to accout for this in DAR also.
#                              Width = round(sqrt(bio_area / 2), 3), #Calculate width assuming a fixed aspect ratio (2Lx1W)
#                              InitSat = 0, #Fixed initial saturation (%)
#                              FromImp = 100 - subs$FromImp, #This is the rest of the impervious area that hasn't already been captured by the vegetated swales in the baseline model
#                              ToPerv = 0,
#                              RptFile = paste0("\"", lid_folder, "/", subs$Name, ".txt\""),
#                              DrainTo = "* 0")
#   
#   #Rename one of the columns
#   colnames(rain_gardens)[2] <- "LID Process"
# 
#   #Create a new SWMM inputs list
#   inputs_new <- inputs
#   
#   #Append the rain garden data frame to the lid_usage data frame
#   inputs_new$lid_usage <- rbind(inputs_new$lid_usage, rain_gardens)
#   
#   #Set all initial storage to zero
#   inputs_new$lid_usage$InitSat = 0
#   
#   #Adjust percent impervious for sub-basins with rain gardens to account for rain garden area
#   subs_new <- inputs$subcatchments %>%
#     left_join(subs, by = "Name") %>%
#     mutate(new_perc_imp = case_when(!is.na(Area.y) ~ (DCIA + ICIA) / (Area - BPA - DCIA * ratio) * 100,
#                                     TRUE ~ Perc_Imperv.x))
#   
#   inputs_new$subcatchments$Perc_Imperv <- subs_new$new_perc_imp
#   
#   #Set groundwater depth parameter to "*"
#   inputs_new$groundwater$Egwt <- "*"
#   
#   #Chnage A1
#   inputs_new$groundwater$A1 <- 10
#   inputs_new$groundwater$B1 <- 2
#   
#   #Change outflow points
#   inputs_new$subareas$RouteTo <- "OUTLET"
#   inputs_new$subareas$PctRouted <- NA
#   
#   #Add name of rainfall file
#   inputs_new$raingages$Interval <- "0:06"
#   rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
#   inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", rain_name, " IN") #This name will change too ("Less_1month")
#   
#   #Allow ponding
#   inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
#   
#   #Deepen trapezoidal channels to prevent flooding
#   inputs_new$xsections <- inputs_new$xsections %>%
#     mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
#                              TRUE ~ Geom1))
#   
#   #Add transect data back in
#   fileName <- file.path(dir_name, "Transect.txt")
#   transect <- readLines(fileName)
#   inputs_new$transects <- transect
#   
#   #reorder the list of inputs
#   inputs_new <- inputs_new[c(1:18, 26, 19:25)]
#   class(inputs_new) <- "inp"
#   
#   if (ratio != 0){
#     #write file - this file name will change
#     filenm <- file.path(dir_name, paste0(out_name, "_", 100*ratio, ".inp"))
#     write_inp(inputs_new, filenm)
#     
#     ############################
#     #Run SWMM with the filenm from above
#     swmm_files <- run_swmm(inp = filenm)
#     
#     # #Get the report file
#     # rpt <- read_rpt(x = swmm_files$rpt)
#     # 
#     # # #Get the output discharge at the outfall
#     # # out <- read_out(file = swmm_files$out, iType = 1, object_name = "Outfall",
#     # #                 vIndex = 4)
#     # # 
#     # # #Plot the values
#     # # zoo::plot.zoo(out[[1]]$total_inflow, main = "Test", ylab = "Q [cfs]", las = 1, xlab = "Time")
#     # # grid()
#     
#   }else{
# 
#     ######################3
#     #Run baseline
#     inputs_new <- inputs
#     inputs_new$raingages$Interval <- "0:06"
#     #rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
#     inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", rain_name, " IN") #This name will change too ("Less_1month")
#     
#     inputs_new$transects <- transect
#     
#     #Set all initial storage to zero
#     inputs_new$lid_usage$InitSat = 0
#     
#     #Set groundwater depth parameter to "*"
#     inputs_new$groundwater$Egwt <- "*"
#     
#     #Chnage A1
#     inputs_new$groundwater$A1 <- 10
#     inputs_new$groundwater$B1 <- 2
#     
#     #Change outflow points
#     inputs_new$subareas$RouteTo <- "OUTLET"
#     inputs_new$subareas$PctRouted <- NA
#     
#     #Allow ponding
#     inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
#     
#     #Deepen trapezoidal channels to prevent flooding
#     inputs_new$xsections <- inputs_new$xsections %>%
#       mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
#                                TRUE ~ Geom1))
#     
#     #reorder
#     inputs_new <- inputs_new[c(1:18, 26, 19:25)]
#     class(inputs_new) <- "inp"
#     
#     #write file
#     filenm <- file.path(dir_name, paste0(out_name, "_baseline.inp"))
#     write_inp(inputs_new, filenm)
#     
#     swmm_files <- run_swmm(inp = filenm)
#     #rpt <- read_rpt(x = swmm_files$rpt)
#   }
# 
#   
#   return(swmm_files)
# 
# }

# SWMM_event_forest <- function(dir_name, inp_file, rain_file, rain_name){
#   
#   library(swmmr)
#   library(dplyr)
#   
#   #This is the slightly modified SWMM model from EPA (I basically changed the start and end dates manually)
#   inp_file <- file.path(dir_name, inp_file)
#   
#   #Read in the input file
#   inputs <- read_inp(inp_file)
#   
#   #Output name
#   out_name <- paste0("SHC_", substr(basename(rain_file), 1, nchar(basename(rain_file)) - 4))
#   
#   inputs_new <- inputs
#   inputs_new$raingages$Interval <- "0:06"
#   rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
#   inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", rain_name, " IN") #This name will change too ("Less_1month")
#   
#   #Adjust subcatchment properties
#   inputs_new$subcatchments <- inputs_new$subcatchments %>%
#     mutate(Width = sqrt(Area * 43560 / 5)) #Assume 4W:1L aspect ratio?
#   
#   inputs_new$subareas <- inputs_new$subareas %>%
#     mutate(`N-Perv` = 0.6,
#            `S-Perv` = 0.4)
#   
#   inputs_new$infiltration$Ksat <- inputs_new$infiltration$Ksat * 2 #Double hydraulic conductivity
#   
#   fileName <- file.path(dir_name, "Transect_forest.txt")
#   transect <- readLines(fileName)
#   inputs_new$transects <- transect
#   
#   #reorder
#   inputs_new <- inputs_new[c(1:12, 20, 13:19)]
#   class(inputs_new) <- "inp"
#   
#   #write file
#   filenm <- file.path(dir_name, paste0(out_name, "_forest.inp"))
#   write_inp(inputs_new, filenm)
#   
#   swmm_files <- run_swmm(inp = filenm)
#   
#   return(swmm_files)
# 
# }

############################################################
#Modified function to run event-scale city analysis
SWMM_event <- function(ratio, dir_name, inp_file, rain_file, rain_name, soils, city){
  #Load the required functions
  library(swmmr)
  library(dplyr)
  
  print(paste(city, soils, rain_name, ratio * 100))
  #print(paste("Ratio = ", ratio))
  
  #Read in the input file
  inputs <- read_inp(inp_file)
  
  #Output name
  out_name <- paste0(city, "_", soils, "_", substr(basename(rain_file), 1, nchar(basename(rain_file)) - 4))
  
  #Take subcatchment information from the input file, combine it with the lid_usage information and calculate areas
  #of pervious and impervious area in each sub-basin. This is important for determining how big our rain gardens
  #will be
  subs <- inputs$subcatchments %>%
    left_join(inputs$lid_usage, by = c("Name" = "Subcatchment")) %>% #combine with lid_usage information
    #Remove sub-basins which have no (or very small) impervious area or the area is already treated
    filter(FromImp < 90, !is.na(FromImp)) %>%
    mutate(BPA = round(Area.y / 43560, 4),
           imp_area = round((Area.x - BPA) * Perc_Imperv / 100, 4), #adjusted impervious area minus BPA
           ICIA = round(FromImp / 100 * imp_area, 4), #Calculate some additional variables
           ratio_imp = round(Area.y / 43560 / ICIA * 100, 4),
           SPA = round(Area.x - imp_area - Area.y / 43560, 4), #standalone pervious area (Total - impervious - BPA)
           DCIA = round(imp_area - ICIA, 4), #DCIA is the main one we are interested in
           max_ratio = SPA / DCIA) %>%
    filter(DCIA > 0) #only keep sub-basins where DCIA is greater than zero (the other ones don't need rain gardens)
  
  #Create a folder for the LID report files
  # lid_folder <- file.path(dir_name, paste0(out_name, "_", 100*ratio, "_LID"))
  # if (ratio != 0){
  #   dir.create(lid_folder)
  # }
  
  #Determine whether pervious area drains to impervious for each catchment
  subs2 <- left_join(subs, inputs$subareas, by = c("Name" = "Subcatchment"))
  pervious <- subs2$RouteTo == "IMPERVIOUS"
  
  bio_area <- subs$DCIA * ratio * 43560
  #bio_area <- if_else(pervious, (subs$SPA * subs$Perc_Imperv / 100 + subs$DCIA) * ratio * 43560, subs$DCIA * ratio * 43560)
  #from_perv <- if_else(pervious, subs$Perc_Imperv, 0)
  
  #Create a data.frame for all the rain gardens we are adding
  rain_gardens <- data.frame(Subcatchment = subs$Name, #sub-catchment names from filtered data above
                             LID_Process = "BioRet1", #This is the name of the LID practice in the model (probably will need to change)
                             Number = 1, #Only 1 rain garden per sub-basin
                             Area = bio_area, #Calculate area of rain garden based on drainage area ratio, convert to ft2. If pervious area drains to impervious, need to accout for this in DAR also.
                             Width = round(sqrt(bio_area / 2), 3), #Calculate width assuming a fixed aspect ratio (2Lx1W)
                             InitSat = 0, #Fixed initial saturation (%)
                             FromImp = 100 - subs$FromImp, #This is the rest of the impervious area that hasn't already been captured by the vegetated swales in the baseline model
                             ToPerv = 0,
                             RptFile = "*", #paste0("\"", lid_folder, "/", subs$Name, ".txt\""),
                             DrainTo = "* 0")
  
  #Rename one of the columns
  colnames(rain_gardens)[2] <- "LID Process"
  
  #Create a new SWMM inputs list
  inputs_new <- inputs
  
  #Append the rain garden data frame to the lid_usage data frame
  inputs_new$lid_usage <- rbind(inputs_new$lid_usage, rain_gardens)
  
  #Set all initial storage to zero
  inputs_new$lid_usage$InitSat = 0
  
  #Adjust percent impervious for sub-basins with rain gardens to account for rain garden area
  subs_new <- inputs$subcatchments %>%
    left_join(subs, by = "Name") %>%
    mutate(new_perc_imp = case_when(!is.na(Area.y) ~ (DCIA + ICIA) / (Area - BPA - DCIA * ratio) * 100,
                                    TRUE ~ Perc_Imperv.x))
  
  inputs_new$subcatchments$Perc_Imperv <- subs_new$new_perc_imp
  
  #Set groundwater depth parameter to "*"
  inputs_new$groundwater$Egwt <- "*"
  
  #Chnage A1
  inputs_new$groundwater$A1 <- 10
  inputs_new$groundwater$B1 <- 2
  
  #Change outflow points
  inputs_new$subareas$RouteTo <- "OUTLET"
  inputs_new$subareas$PctRouted <- NA
  
  #Add name of rainfall file
  inputs_new$raingages$Interval <- "0:06"
  rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
  inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", rain_name, " IN") #This name will change too ("Less_1month")
  
  #Allow ponding
  inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
  
  #Deepen trapezoidal channels to prevent flooding
  inputs_new$xsections <- inputs_new$xsections %>%
    mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
                             TRUE ~ Geom1))
  
  #Adjust bioretention parameters
  soil_parms <- list("low" = c(0.464, 0.31, 0.187, 0.04, 8.27, 10),
                     "med" = c(0.463, 0.232, 0.116, 0.13, 3.5, 10),
                     "high" = c(0.437, 0.105, 0.047, 1.18, 2.4, 10))
  soil_parms <- lapply(soil_parms, function(x){
    names(x) <- c("Phi", "FC", "WP", "K", "Psi", "Slope")
    return(x)
  })
  
  inputs_new$lid_controls$Par2[3] <- soil_parms[[soils]]['Phi']
  inputs_new$lid_controls$Par3[3] <- soil_parms[[soils]]['FC']
  inputs_new$lid_controls$Par4[3] <- soil_parms[[soils]]['WP']
  inputs_new$lid_controls$Par5[3] <- soil_parms[[soils]]['K']
  inputs_new$lid_controls$Par6[3] <- soil_parms[[soils]]['Slope']
  inputs_new$lid_controls$Par7[3] <- soil_parms[[soils]]['Psi']
  
  #Add transect data back in
  fileName <- file.path(dirname(inp_file), "Transect.txt")
  transect <- readLines(fileName)
  inputs_new$transects <- transect
  
  #reorder the list of inputs
  inputs_new <- inputs_new[c(1:18, 26, 19:25)]
  class(inputs_new) <- "inp"
  
  if (ratio != 0){
    #write file - this file name will change
    filenm <- file.path(dir_name, paste0(out_name, "_", 100*ratio, ".inp"))
    write_inp(inputs_new, filenm)
    
    ############################
    #Run SWMM with the filenm from above
    swmm_files <- run_swmm(inp = filenm)
    
    # #Get the report file
    # rpt <- read_rpt(x = swmm_files$rpt)
    # 
    # # #Get the output discharge at the outfall
    # # out <- read_out(file = swmm_files$out, iType = 1, object_name = "Outfall",
    # #                 vIndex = 4)
    # # 
    # # #Plot the values
    # # zoo::plot.zoo(out[[1]]$total_inflow, main = "Test", ylab = "Q [cfs]", las = 1, xlab = "Time")
    # # grid()
    
  }else{
    
    ######################3
    #Run baseline
    inputs_new <- inputs
    inputs_new$raingages$Interval <- "0:06"
    #rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
    inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", rain_name, " IN") #This name will change too ("Less_1month")
    
    inputs_new$transects <- transect
    
    #Set all initial storage to zero
    inputs_new$lid_usage$InitSat = 0
    
    #Set groundwater depth parameter to "*"
    inputs_new$groundwater$Egwt <- "*"
    
    #Chnage A1
    inputs_new$groundwater$A1 <- 10
    inputs_new$groundwater$B1 <- 2
    
    #Change outflow points
    inputs_new$subareas$RouteTo <- "OUTLET"
    inputs_new$subareas$PctRouted <- NA
    
    #Allow ponding
    inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
    
    #Deepen trapezoidal channels to prevent flooding
    inputs_new$xsections <- inputs_new$xsections %>%
      mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
                               TRUE ~ Geom1))
    
    #reorder
    inputs_new <- inputs_new[c(1:18, 26, 19:25)]
    class(inputs_new) <- "inp"
    
    #write file
    filenm <- file.path(dir_name, paste0(out_name, "_baseline.inp"))
    write_inp(inputs_new, filenm)
    
    swmm_files <- run_swmm(inp = filenm)
    #rpt <- read_rpt(x = swmm_files$rpt)
  }
  
  
  return(swmm_files)
  
}

#################################################################################
################################################################################
#Function to run continuous cities analysis



SWMM_city <- function(ratio, city, rain_name, dir_name, soils, inp_file){
  #Load the required functions
  library(swmmr)
  library(dplyr)
  
  print(paste("Ratio = ", ratio))
  print(paste("City = ", city))
  print(paste("Soil = ", soils))
  print(paste("Rain =", rain_name))
  
  #This is the slightly modified SWMM model from EPA with snow activated
  inp_file <- file.path(dir_name, inp_file)
  
  #Read in the input file
  inputs <- read_inp(inp_file)
  
  #Output name
  out_name <- paste0(city, "_", soils, "_", rain_name)
  
  #Take subcatchment information from the input file, combine it with the lid_usage information and calculate areas
  #of pervious and impervious area in each sub-basin. This is important for determining how big our rain gardens
  #will be
  subs <- inputs$subcatchments %>%
    left_join(inputs$lid_usage, by = c("Name" = "Subcatchment")) %>% #combine with lid_usage information
    #Remove sub-basins which have no (or very small) impervious area or the area is already treated
    filter(FromImp < 90, !is.na(FromImp)) %>%
    mutate(BPA = round(Area.y / 43560, 4),
           imp_area = round((Area.x - BPA) * Perc_Imperv / 100, 4), #adjusted impervious area minus BPA
           ICIA = round(FromImp / 100 * imp_area, 4), #Calculate some additional variables
           ratio_imp = round(Area.y / 43560 / ICIA * 100, 4),
           SPA = round(Area.x - imp_area - Area.y / 43560, 4), #standalone pervious area (Total - impervious - BPA)
           DCIA = round(imp_area - ICIA, 4), #DCIA is the main one we are interested in
           max_ratio = SPA / DCIA) %>%
    filter(DCIA > 0) #only keep sub-basins where DCIA is greater than zero (the other ones don't need rain gardens)
  
  #Create a folder for the LID report files
  lid_folder <- file.path(dir_name, city, paste0(out_name, "_", 100*ratio, "_LID"))
  if (ratio != 0){
    dir.create(lid_folder)
  }
  
  #Determine whether pervious area drains to impervious for each catchment
  subs2 <- left_join(subs, inputs$subareas, by = c("Name" = "Subcatchment"))
  pervious <- subs2$RouteTo == "IMPERVIOUS"
  
  bio_area <- subs$DCIA * ratio * 43560
  #bio_area <- if_else(pervious, (subs$SPA * subs$Perc_Imperv / 100 + subs$DCIA) * ratio * 43560, subs$DCIA * ratio * 43560)
  #from_perv <- if_else(pervious, subs$Perc_Imperv, 0)
  
  #Create a data.frame for all the rain gardens we are adding
  rain_gardens <- data.frame(Subcatchment = subs$Name, #sub-catchment names from filtered data above
                             LID_Process = "BioRet1", #This is the name of the LID practice in the model (probably will need to change)
                             Number = 1, #Only 1 rain garden per sub-basin
                             Area = bio_area, #Calculate area of rain garden based on drainage area ratio, convert to ft2. If pervious area drains to impervious, need to accout for this in DAR also.
                             Width = round(sqrt(bio_area / 2), 3), #Calculate width assuming a fixed aspect ratio (2Lx1W)
                             InitSat = 0, #Fixed initial saturation (%)
                             FromImp = 100 - subs$FromImp, #This is the rest of the impervious area that hasn't already been captured by the vegetated swales in the baseline model
                             ToPerv = 0,
                             RptFile = paste0("\"", lid_folder, "/", subs$Name, ".txt\""),
                             DrainTo = "* 0")
  
  #Rename one of the columns
  colnames(rain_gardens)[2] <- "LID Process"
  
  #Create a new SWMM inputs list
  inputs_new <- inputs
  
  #Append the rain garden data frame to the lid_usage data frame
  inputs_new$lid_usage <- rbind(inputs_new$lid_usage, rain_gardens)
  
  #Adjust percent impervious for sub-basins with rain gardens to account for rain garden area
  subs_new <- inputs$subcatchments %>%
    left_join(subs, by = "Name") %>%
    mutate(new_perc_imp = case_when(!is.na(Area.y) ~ (DCIA + ICIA) / (Area - BPA - DCIA * ratio) * 100,
                                    TRUE ~ Perc_Imperv.x))
  
  inputs_new$subcatchments$Perc_Imperv <- subs_new$new_perc_imp
  
  #Set all initial storage to zero
  inputs_new$lid_usage$InitSat = 0
  
  #Adjust bioretention parameters
  soil_parms <- list("low" = c(0.464, 0.31, 0.187, 0.04, 8.27, 10),
                     "med" = c(0.463, 0.232, 0.116, 0.13, 3.5, 10),
                     "high" = c(0.437, 0.105, 0.047, 1.18, 2.4, 10))
  soil_parms <- lapply(soil_parms, function(x){
    names(x) <- c("Phi", "FC", "WP", "K", "Psi", "Slope")
    return(x)
  })
  
  inputs_new$lid_controls$Par2[3] <- soil_parms[[soils]]['Phi']
  inputs_new$lid_controls$Par3[3] <- soil_parms[[soils]]['FC']
  inputs_new$lid_controls$Par4[3] <- soil_parms[[soils]]['WP']
  inputs_new$lid_controls$Par5[3] <- soil_parms[[soils]]['K']
  inputs_new$lid_controls$Par6[3] <- soil_parms[[soils]]['Slope']
  inputs_new$lid_controls$Par7[3] <- soil_parms[[soils]]['Psi']
  
  #Set groundwater depth parameter to "*"
  inputs_new$groundwater$Egwt <- "*"
  
  #Chnage A1
  inputs_new$groundwater$A1 <- 10
  inputs_new$groundwater$B1 <- 2
  
  #Change outflow points
  inputs_new$subareas$RouteTo <- "OUTLET"
  inputs_new$subareas$PctRouted <- NA
  
  #Allow ponding
  inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
  
  #Deepen trapezoidal channels to prevent flooding
  inputs_new$xsections <- inputs_new$xsections %>%
    mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
                             TRUE ~ Geom1))
  
  #Add name of rainfall file
  inputs_new$raingages$Interval <- "1:00"
  rain_file <- paste0(city, "/", city, "_PPT_", rain_name, ".txt")
  rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
  inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", city, " IN") #This name will change too ("Less_1month")
  
  #Add temperature/climate file
  inputs_new$temperature$Values[inputs_new$temperature$`Data Element` == "FILE"] <- paste0("\"", file.path(dir_name, paste0(city, "/", city, "_Climate_", rain_name, ".txt")), "\"")
  
  #Change start/end dates
  rain <- read.table(rain_file, header = FALSE, sep = " ")
  year <- min(rain$V2)
  start_date <- paste0("01/01/", year)
  end_date <- paste0("12/31/", year)
  inputs_new$options$Value[inputs_new$options$Option == "START_DATE"] <- start_date
  inputs_new$options$Value[inputs_new$options$Option == "END_DATE"] <- end_date
  inputs_new$options$Value[inputs_new$options$Option == "REPORT_START_DATE"] <- start_date
  
  #Add snowpack
  inputs_new$subcatchments$Snowpack <- "Snow_all"
  
  #Add transect data back in
  fileName <- file.path(dir_name, "Transect.txt")
  transect <- readLines(fileName)
  inputs_new$transects <- transect
  
  #reorder the list of inputs
  inputs_new <- inputs_new[c(1:20, 28, 21:27)]
  class(inputs_new) <- "inp"
  
  if (ratio != 0){
    #write file - this file name will change
    filenm <- file.path(dir_name, paste0(city, "/", out_name, "_", 100*ratio, ".inp"))
    write_inp(inputs_new, filenm)
    
    ############################
    #Run SWMM with the filenm from above
    swmm_files <- run_swmm(inp = filenm)
    
    # #Get the report file
    # rpt <- read_rpt(x = swmm_files$rpt)
    # 
    # # #Get the output discharge at the outfall
    # # out <- read_out(file = swmm_files$out, iType = 1, object_name = "Outfall",
    # #                 vIndex = 4)
    # # 
    # # #Plot the values
    # # zoo::plot.zoo(out[[1]]$total_inflow, main = "Test", ylab = "Q [cfs]", las = 1, xlab = "Time")
    # # grid()
    
  }else{
    
    ######################3
    #Run baseline
    inputs_new <- inputs
    #Set all initial storage to zero
    inputs_new$lid_usage$InitSat = 0
    
    #Set groundwater depth parameter to "*"
    inputs_new$groundwater$Egwt <- "*"
    
    #Chnage A1
    inputs_new$groundwater$A1 <- 10
    inputs_new$groundwater$B1 <- 2
    
    #Change outflow points
    inputs_new$subareas$RouteTo <- "OUTLET"
    inputs_new$subareas$PctRouted <- NA
    
    #Add name of rainfall file
    inputs_new$raingages$Interval <- "1:00"
    rain_file <- paste0(city, "/", city, "_PPT_", rain_name, ".txt")
    rain_file <- file.path(dir_name, rain_file) #This file name will change depending on the storm you want to model
    inputs_new$raingages$Source <- paste0("FILE \"", rain_file, "\" ", city, " IN") #This name will change too ("Less_1month")
    
    #Add temperature/climate file
    inputs_new$temperature$Values[inputs_new$temperature$`Data Element` == "FILE"] <- paste0("\"", file.path(dir_name, paste0(city, "/", city, "_Climate_", rain_name, ".txt")), "\"")
    
    #Change start/end dates
    rain <- read.table(rain_file, header = FALSE, sep = " ")
    year <- min(rain$V2)
    start_date <- paste0("01/01/", year)
    end_date <- paste0("12/31/", year)
    inputs_new$options$Value[inputs_new$options$Option == "START_DATE"] <- start_date
    inputs_new$options$Value[inputs_new$options$Option == "END_DATE"] <- end_date
    inputs_new$options$Value[inputs_new$options$Option == "REPORT_START_DATE"] <- start_date
    
    #Add snowpack
    inputs_new$subcatchments$Snowpack <- "Snow_all"
    
    #Allow ponding
    inputs_new$junctions$Aponded <- 100 #100 ft2 ponded area
    
    #Deepen trapezoidal channels to prevent flooding
    inputs_new$xsections <- inputs_new$xsections %>%
      mutate(Geom1 = case_when(Shape == "TRAPEZOIDAL" ~ as.character(as.numeric(Geom1) * 3),
                               TRUE ~ Geom1))
    
    #Add transect data back in
    fileName <- file.path(dir_name, "Transect.txt")
    transect <- readLines(fileName)
    inputs_new$transects <- transect
    
    #reorder the list of inputs
    inputs_new <- inputs_new[c(1:20, 28, 21:27)]
    class(inputs_new) <- "inp"
    
    #write file
    filenm <- file.path(dir_name, city, paste0(out_name, "_baseline.inp"))
    write_inp(inputs_new, filenm)
    
    swmm_files <- run_swmm(inp = filenm)
    #rpt <- read_rpt(x = swmm_files$rpt)
  }
  
  return(swmm_files)
  
}

