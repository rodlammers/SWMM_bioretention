#SWMM Plots
library(swmmr)
library(dplyr)
library(pbapply)
#Analyze results
get_results <- function(swmm_files, location = "Outfall"){
  
  # #Get the output discharge at the outfall
  out <- read_out(file = swmm_files, iType = 1, object_name = location,
                  vIndex = 4)
  
  return(out)
  
}

#Plot Results
plot_results <- function(results, ymax = NULL){
  
  if(is.null(ymax)){ymax = max(results[[1]][[1]])}
  
  zoo::plot.zoo(results[[1]][[1]], ylab = "Q [cfs]", las = 1, xlab = "Time", lwd = 2, ylim = c(0, ymax))
  grid()
  
}

#Caculate summary stats from hydrographs
calc_stats <- function(results, soils, rains, ratio, threshold){
  data <- results[[1]][[1]]
  
  stats <- data.frame(vol = sum(data) * 0.9917, #runoff volume, ac-in / yr
                      vol_high = sum(data[data > threshold]), #runoff volume above high flow threshold, ac-in / yr
                      HPC = sum(table(cumsum(data < threshold)) > 1), #number of discrete peaks above flow threshold
                      HPD = sum(data > threshold) / 4, #duration of flow above threshold (in hours)
                      no_flow = sum(data < 0.5) / 4, #duration with essentially zero flow (<0.5 cfs) in hours
                      soils = soils,
                      rains = rains,
                      ratio = ratio
  )
  
  return(stats)
}

#Water Balance

#Water Balance
water_balance <- function(city, soil, rain, ratio){
  filenm <- paste0("Sims/Continuous/", city, "/", city, "_", soil, "_", rain, "_", ratio, ".rpt")
  report <- read_rpt(filenm)
  data <- report$runoff_quantity_continuity$Depth
  names(data) <- report$runoff_quantity_continuity$Component
  
  gw_flow <- report$groundwater_continuity$Depth[report$groundwater_continuity$Component == "Groundwater Flow"]
  
  output <- data.frame(Runoff = data["Surface Runoff"],
                       GW_flow = gw_flow,
                       LID = data["LID Drainage"],
                       Infil = data["Infiltration Loss"] - gw_flow,
                       Evap = data["Evaporation Loss"],
                       Precip = data["Total Precipitation"],
                       city = city,
                       soil = soil,
                       rain = rain,
                       ratio = ratio)
  
  return(output)
  
}



subcatchment_results_continuous <- function(ratio, rain, soil, city, dir_name){
  filenm <- paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, ".rpt")
  
  rpt <- read_rpt(filenm)
  rpt <- fix_report(rpt)
  
  input <- read_inp(stringr::str_replace(filenm, "rpt", "inp"))
  baseline_rpt <- fix_report(read_rpt(stringr::str_replace(filenm, paste0(ratio * 100, ".rpt"), "baseline.rpt")))
  
  comb <- left_join(rpt$subcatchment_runoff_summary, input$subcatchments, by = c("Subcatchment" = "Name")) %>%
    left_join(baseline_rpt$subcatchment_runoff_summary, by = "Subcatchment") %>%
    mutate(vol_red = ifelse(Total_Runoff_Depth.y > 0, (Total_Runoff_Depth.y - Total_Runoff_Depth.x) / Total_Runoff_Depth.y * 100, NA),
           peak_red = ifelse(Total_Peak_Runoff.y > 0, (Total_Peak_Runoff.y - Total_Peak_Runoff.x) / Total_Peak_Runoff.y * 100, NA))
  
  summary <- data.frame(cum_vol_red = sum(comb$vol_red * comb$Area, na.rm = TRUE) / sum(comb$Area),
                        cum_peak_red = sum(comb$peak_red * comb$Area, na.rm = TRUE) / sum(comb$Area),
                        ratio = paste0(ratio * 100, "%"),
                        rain = rain,
                        soil = soil,
                        city = city)
  
  #summary <- data.frame(cum_vol_red = Total_Runoff_Depth.y 
  
  return(summary)
}

peak_events <- function(city, rain, soil, ratio, hourly_data, threshold, location = "Outfall", 
                        dir_name){
  
  #Do event analysis
  index <- which(names(hourly_data) == gsub("_", " ", city))
  events <- event_separation(data = hourly_data[[index]], city = city)
  
  #Get events for year of analysis
  event_summary <- events[[1]]
  years <- unique(event_summary$year)
  analysis_year <- case_when(rain == "Dry" ~ years[1],
                             rain == "Norm" ~ years[2],
                             rain == "Wet" ~ years[3])
  
  event_summary <- filter(event_summary, year == analysis_year)
  
  #Get output
  out_file <- paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, ".out")
  out <- read_out(file = out_file, iType = 1, object_name = location,
                  vIndex = 4)
  
  #baseline output
  baseline_file <- paste0(dir_name, "/", city, "/", city, "_high_", rain, "_baseline.out")
  baseline_out <- read_out(file = baseline_file, iType = 1, object_name = location,
                           vIndex = 4)
  
  #combine outputs
  event_summary <- mutate(event_summary,
                          peak_out = NA,
                          peak_baseline = NA,
                          vol_out = NA,
                          vol_baseline = NA)
  for (i in 1:nrow(event_summary)){
    start <- event_summary$start[i]
    end <- event_summary$end[i]
    index <- paste0(start, "/", end)
    flow_out <- out[[1]][[1]][index]
    flow_baseline <- baseline_out[[1]][[1]][index]
    
    event_summary$peak_out[i] <- max(flow_out)
    event_summary$peak_baseline[i] <- max(flow_baseline)
    event_summary$vol_out[i] <- sum(flow_out)
    event_summary$vol_baseline[i] <- sum(flow_baseline)
  }
  
  event_summary <- mutate(event_summary,
                          peak_red = (peak_baseline - peak_out) / peak_baseline * 100,
                          vol_red = (vol_baseline - vol_out) / vol_baseline * 100,
                          city = city,
                          soil = soil,
                          ratio = ratio,
                          rain = rain) %>%
    filter(peak_baseline > threshold)
  
  return(event_summary)
  
}

clean_LID_reports <- function(ratio, city, rain, soil, dir_name){
  header <- c("Date", "Elapsed_Time", 
              "Total_Inflow", "Total_Evap", "Surface_Infil", 
              "Pavement_Perc", "Soil_Perc", "Storage_Exfil", 
              "Surface_Runoff", "Drain_Outflow", "Surface_Level", 
              "Pavement_Level", "Soil_Moisture", "Storage_Level")
  
  #Read in all files
  files <- list.files(paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, "_LID/"), pattern = ".txt", full.names = TRUE)
  subs <- sapply(files, function(x){
    substr(x, nchar(x) - 9, nchar(x) - 4)
  })
  
  data <- lapply(files, function(x, header){
    data.table::fread(x, header = FALSE, col.names = header, skip = 9) %>%
      mutate(Date = lubridate::mdy_hms(Date),
             event = NA)
  }, header = header)
  
  #Add names of sub-basins
  names(data) <- subs
  
  #write to Rds object
  saveRDS(data, file = paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, "_LID.Rds"))
  
  #Delete folder
  unlink(x = paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, "_LID"), recursive = TRUE)

}

#Run SWMM
source("R/Run SWMM Function.R")
dir <- "C:/Users/lamme1r/Documents/SWMM_bioretention/Sims/City_Analysis"
city <- "Seattle"
rains <- c("Dry", "Norm", "Wet")
soils <- c("low", "med", "high")
ratio <- 0.1

#Run SWMM
for (i in 1:length(rains)){
  for (j in 1:length(soils)){
    SWMM_city(ratio = ratio, city = city, rain_name = rains[i], dir_name = dir, inp_file = "SHC_NEW_snow.inp", soils = soils[j])
  }
}

# #Run SWMM for baseline
for (i in 1:length(rains)){
  SWMM_city(ratio = 0, city = city, rain_name = rains[i], dir_name = dir, inp_file = "SHC_NEW_snow.inp", soils = "high")
}

#Read in LID reports and save as R object
for (i in 1:length(rains)){
  for (j in 1:length(soils)){
    clean_LID_reports(ratio = 0.1, city = city, rain = rains[i], soil = soils[j], dir_name = dir)
    gc()
  }
}

#Get hydrographs
files <- lapply(rains, function(x, dir){
  y <- list.files(path = dir, pattern = ".out", full.names = TRUE)
  y <- Filter(function(z) grepl(x, z), y)
  #reorder
  y <- y[c(2, 1, 4, 3)]
}, dir = file.path(dir, city))

results <- lapply(files, function(x){
  lapply(x, get_results, location = "10yr_Detention")
})

# soil_type <- c("High", "Med", "Low")
# for (i in 1:length(results)){
#   par(mfrow = c(4, 1), mar = c(4, 4, 2, 0.5))
#   ymax <- max(unlist(results[[i]]))
#   for (j in 1:length(results[[i]])){
#     plot_results(results[[i]][[j]], ymax = ymax)
#     
#     title(main = paste(substr(files[[i]][[j]], nchar(files[[i]][[j]]) - 9, nchar(files[[i]][[j]]) - 7), "-", soil_type[j]))
#   }
#   
# }

#Calculate runoff volumes and peaks above threshold
#get daily mean flow for baseline to get flow threshold
mean_flow <- mean(sapply(results, function(x){mean(x[[1]][[1]][[1]][x[[1]][[1]][[1]] > 0])}))
# mean_flow <- sapply(results, function(x){
#   ep <- xts::endpoints(x[[1]]$Outfall$total_inflow, on = "days")
#   daily_mean <- xts::period.apply(x[[1]]$Outfall$total_inflow, INDEX = ep, FUN = mean)
#   
#   return(daily_mean)
#   })
soils2 <- c("Baseline", rev(soils))
ratios <- c("Baseline", rep("0.1", 3))
stats <- list()

for(i in 1:length(rains)){
  stats[[i]] <- list()
  for(j in 1:4){
    stats[[i]][[j]] <- calc_stats(results[[i]][[j]], soils = soils2[j], rains = rains[i], ratio = ratios[j], threshold = 2 * mean_flow)
  }
}

stats_combined <- do.call("rbind", do.call("rbind", stats))

#Summary plot showing performance across different soil types and years
percentages <- stats_combined %>%
  group_by(rains) %>%
  mutate(vol = (first(vol) - vol) / first(vol) * 100,
            vol_high = (first(vol_high) - vol_high) / first(vol_high) * 100,
            no_flow = no_flow / 24, #convert to days
            HPC = HPC,
            HPD = HPD)

source("R/Storm event analysis.R")
events_peak <- list()
for (i in 1:length(rains)){
  events_peak[[i]] <- list()
  for (j in 1:length(soils)){
    events_peak[[i]][[j]] <- peak_events(city = city, rain = rains[i], soil = soils[j], ratio = 0.1,
                                         hourly_data = hourly_data, threshold = 2 * mean_flow)
  }
}

events_peak_comb <- do.call("rbind", do.call("rbind", events_peak)) %>%
  mutate(rain = factor(rain, levels = c("Dry", "Norm", "Wet"))) %>%
  mutate(soil = factor(soil, levels = c("low", "med", "high")))


source("~/WORK/R Functions/Plot Functions.R")
png(paste0("Sims/City_Analysis/", city, "/", city, "_Summary_Plot_basin.png"), type = "cairo", units = "in",
    height = 5, width = 5, res = 500)
colors <- RColorBrewer::brewer.pal(n = 3, "PuBu")
colors <- c("gray20", rev(colors))
palette(cRamp_legend(3, "PuBu", alpha = 0.7))
par(mfcol = c(2, 2), mar = c(3, 4, 1, 0.5), oma = c(1, 0, 3, 0))
plot(peak_red ~ vol, events_peak_comb, pch = 21, bg = soil, las = 1, xlab = "Storm Depth [in]", ylab = "% Peak Flow Reduction")
barplot(vol_high ~ soils + rains, percentages, beside = TRUE, ylab = "% Reduction Runoff Volume Above Threshold", xlab = "", las = 1, col = colors)
barplot(HPC ~ soils + rains, percentages, beside = TRUE, ylab = "High Pulse Count", xlab = "", las = 1, col = colors)
barplot(HPD ~ soils + rains, percentages, beside = TRUE, ylab = "High Pulse Duration (hr)", xlab = "", las = 1, col = colors)
mtext("Year", side = 1, line = 0, outer = TRUE)
mtext(stringr::str_replace(city, "_", " "), side = 3, line = 2, outer = TRUE, font = 2)
legend("topleft", legend = c("No GI", "High", "Med", "Low"), fill = colors, horiz = TRUE, bty = "n", title = "GI Soil Infiltration",
       inset = c(-1, -1.8), xpd = NA)
dev.off()
#barplot(no_flow ~ soils + rains, percentages, beside = TRUE)




source("R/Storm event analysis.R")
GI_event_analysis <- function(city, rain, soil, ratio, hourly_data, dir_name = "C:/Users/lamme1r/SWMM_bioretention/Sims/City_Analysis"){
  
  #Check if .csv file with results already exists, if it does then just read it in, otherwise, create it
  output_file  <- paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, "_LID.csv")
  if (file.exists(output_file)){
    lid_ts <- read.csv(output_file)
    print("Reading saved results from file...")
  }else{
    print("Writing results to file...")
    #Do event analysis
    index <- which(names(hourly_data) == gsub("_", " ", city))
    events <- event_separation(data = hourly_data[[index]], city = city)
    
    #Get events for year of analysis
    event_summary <- events[[1]]
    years <- unique(event_summary$year)
    analysis_year <- case_when(rain == "Dry" ~ years[1],
                               rain == "Norm" ~ years[2],
                               rain == "Wet" ~ years[3])
    
    event_summary <- filter(event_summary, year == analysis_year)
    
    #Figure out soil porosity based on soil type
    porosity <- case_when(soil == "low" ~ 0.464,
                          soil == "med" ~ 0.463,
                          soil == "high" ~ 0.437)
    
    #Get GI performance results - Rds file
    file <- paste0(dir_name, "/", city, "/", city, "_", soil, "_", rain, "_", ratio * 100, "_LID.Rds")
    data <- readRDS(file)
      
    lid_ts <- purrr::map2(data, names(data), function(x, y, event_summary, porosity){
      #x$event[which(x$Date %in% event_summary$start)] <- event_summary$event
      x <- select(x, -event)
      x <- left_join(x, select(event_summary, event, start), by = c("Date" = "start"))
      x$event[1] <- ifelse(is.na(x$event[1]), 1, x$event[1])
      x$event <- zoo::na.locf(x$event)
      
      data_summary <- x %>%
        group_by(event) %>%
        summarize(inflow = sum(Total_Inflow) * 1 / 60, #inflow in inches
                  evap = sum(Total_Evap) * 1 / 60, #evaporation in inches
                  infil = sum(Surface_Infil) * 1 / 60, #infiltration in inches
                  exfil = sum(Storage_Exfil) * 1 / 60, #exfiltration (into surrounding soils) in inches
                  runoff = sum(Surface_Runoff) * 1 / 60, #surface runoff in inches
                  drain_outflow = sum(Drain_Outflow) * 1 / 60, #Drain outflow in inches
                  outflow = runoff + drain_outflow, #combination of drain outflow and surface runoff in inches
                  outflow_dur = sum(outflow > 0) / 60, #Duration of outflow, in hours
                  max_inflow_rate = max(Total_Inflow), #maximum inflow rate (in/hr)
                  max_inflow_dur = sum(Total_Inflow > Surface_Infil) / 60, #duration above max inflow of rain garden soil (in hours)
                  outflow_high_intensity = sum(Surface_Runoff[Total_Inflow > Surface_Infil & Soil_Moisture < porosity & Storage_Level < 12]) * 1 / 60, #surface outflow caused by too high of rainfall intensity (in inches)
                  max_exfiltration = max(Storage_Exfil), #maximum storage exfiltration rate in in/hr
                  soil_moist_initial = first(Soil_Moisture) / porosity, #inital soil moisture (% saturated)
                  soil_moist_final = last(Soil_Moisture) / porosity, #final soil moisture (% saturated)
                  storage_initial = first(Storage_Level) / 12, #initial storage (% saturated)
                  storage_final = last(Storage_Level) / 12)  %>% #final storage (% saturate)
        left_join(event_summary, by = "event") %>%
        mutate(sub = y)
      
    }, event_summary, porosity)
    
    lid_ts <- do.call("rbind", lid_ts)
    
    write.csv(lid_ts, output_file, row.names = FALSE)
  }
  
  return(lid_ts)
}

#Analyze all simulations to get 1) amount of surface runoff from high intensity inflow and 2) fraction of outflow
#that can be attributed to "back-to-back" events
rains <- c("Wet", "Norm", "Dry")
soils <- c("high", "med", "low")
ratios <- 0.1

results_summary <- list()
for (j in 1:length(rains)){
  results_summary[[j]] <- list()
  for (k in 1:length(soils)){
    results_summary[[j]][[k]] <- list()
    for (m in 1:length(ratios)){
      output <- GI_event_analysis(city = city, rain = rains[j], soil = soils[k], ratio = ratios[m], hourly_data = hourly_data)
      gc()
    
      #Get sub-basin areas
      inp_file <- read_inp(paste0(dir, "/", city, "/", city, "_", soils[k], "_", rains[j], "_", ratios[m] * 100, ".inp"))
      bioret <- inp_file$lid_usage %>%
        filter(`LID Process` == "BioRet1")
      subbasins <- inp_file$subcatchments
      
      by_event <- left_join(output, bioret, by = c("sub" = "Subcatchment")) %>%
        group_by(event) %>%
        summarize(tot_outflow = sum(outflow / 12 * Area), #total outflow in ft^3
                  sat_outflow = sum(outflow[soil_moist_initial >= 0.9] / 12 * Area[soil_moist_initial >= 0.9]), #total outflow where initial soil moisture is high
                  int_outflow = sum(outflow[soil_moist_initial < 0.9] / 12 * Area[soil_moist_initial < 0.9]), #total outflow where initial soil moisture is lower
                  tot_inflow = sum(inflow / 12 * Area), #total inflow in ft^3
                  p_depth = mean(vol), #storm depth in inches
                  storm_delta = mean(delta), #time (hours) since previous rainfall event
                  high_intensity_outflow = sum(outflow_high_intensity), #sum of high intensity outflow
                  month = first(lubridate::month(start)), #month of storm event
                  season = case_when(month %in% c(12, 1, 2) ~ "Winter",
                                     month %in% 3:5 ~ "Spring",
                                     month %in% 6:8 ~ "Summer",
                                     month %in% 9:11 ~ "Fall") #Seasons (meteorolgical)
        )
      
      season_results <- by_event %>%
        group_by(season) %>%
        summarize(sat_outflow_sum = sum(sat_outflow),
                  intense_outflow_sum = sum(high_intensity_outflow))
      
      results_summary[[j]][[k]][[m]] <- data.frame(surface_runoff = sum(output$runoff),
                                    drain = sum(output$drain_outflow),
                                    tot_runoff = sum(output$outflow),
                                    high_intensity_runoff = sum(output$outflow_high_intensity),
                                    tot_outflow_ft3 = sum(by_event$tot_outflow),
                                    sat_outflow_ft3 = sum(by_event$sat_outflow),
                                    n_intense_outflow = sum(by_event$high_intensity_outflow > 0),
                                    n_sat_outflow = sum(by_event$sat_outflow > 0),
                                    n_events = nrow(by_event),
                                    city = city,
                                    rain = rains[j],
                                    soil = soils[k],
                                    ratio = ratios[m]) %>%
                            mutate(frac_high_intensity = high_intensity_runoff / tot_runoff,
                                   frac_intense_events = n_intense_outflow / n_events,
                                   frac_sat = sat_outflow_ft3 / tot_outflow_ft3,
                                   frac_sat_events = n_sat_outflow / n_events) %>%
        cbind(matrix(unlist(season_results[,2]), ncol = 4, nrow = 1)) %>%
        cbind(matrix(unlist(season_results[,3]), ncol = 4, nrow = 1))
      
      colnames(results_summary[[j]][[k]][[m]])[18:25] <- paste0(c("Fall", "Spring", "Summer", "Winter"), "_", c(rep("sat", 4), rep("intense", 4)))
    }
    
  }
}

results_df <- do.call("rbind", do.call("rbind", do.call("rbind", results_summary)))

#Plot of intensity and saturation
png(paste0("Sims/City_Analysis/", city, "/", city, "_GI_Summary.png"), type = "cairo", units = "in",
    height = 5, width = 5, res = 500)
colors <- rev(RColorBrewer::brewer.pal(n = 3, "PuBu"))
par(mfcol = c(2, 2), mar = c(3, 4, 1, 0.5), oma = c(1, 0, 3, 0))
barplot(I(frac_high_intensity * 100) ~ soil + rain, results_df, beside = TRUE, ylab = "% Outflow from High Intensity", xlab = "", las = 1, col = colors, ylim = c(0, 100))
barplot(I(frac_intense_events * 100) ~ soil + rain, results_df, beside = TRUE, ylab = "% Events with High Intensity Outflow", xlab = "", las = 1, col = colors, ylim = c(0, 100))
barplot(I(frac_sat * 100) ~ soil + rain, results_df, beside = TRUE, ylab = "% Outflow from Back-to-Back Events", xlab = "", las = 1, col = colors, ylim = c(0, 100))
barplot(I(frac_sat_events * 100) ~ soil + rain, results_df, beside = TRUE, ylab = "% Back-to-Back Events", xlab = "", las = 1, col = colors, ylim = c(0, 100))
mtext("Year", side = 1, line = 0, outer = TRUE)
mtext(stringr::str_replace(city, "_", " "), side = 3, line = 2, outer = TRUE, font = 2)
legend("topleft", legend = c("High", "Med", "Low"), fill = colors, horiz = TRUE, bty = "n", title = "GI Soil Infiltration",
       inset = c(-1, -1.8), xpd = NA)
dev.off()

##################################################3
#Water balance
# city <- "New_Orleans"
# soils <- c("low", "med", "high")
# rains <- c("Wet", "Norm", "Dry")

wb <- list()
wb_baseline <- list()
count <- 1
for (i in 1:length(rains)){
  wb_baseline[[i]] <- water_balance(city, "high", rains[i], ratio = "baseline")
  for (j in 1:length(soils)){
    wb[[count]] <- water_balance(city, soils[j], rains[i], ratio = "10")
    count <- count + 1
  }
}

wb_baseline <- do.call("rbind", wb_baseline)
wb_baseline$soil <- "baseline"
wb_combined <- do.call("rbind", wb) %>%
  rbind(wb_baseline) %>%
  mutate(soil = factor(soil, levels = c("baseline", "high", "med", "low")))
wb_combined[is.na(wb_combined)] <- 0

png(paste0("Sims/City_Analysis/", city, "/", city, "_Water_Balance.png"), type = "cairo", units = "in",
    height = 4, width = 6.5, res = 500)
colors <- RColorBrewer::brewer.pal(5, "Paired")[c(1:2, 5, 3:4)]
par(mfrow = c(1, 3), mar = c(2, 3, 2, 0.5), oma = c(3, 1.5, 1.5, 0))
ymax <- max(wb_combined$Precip)
for (i in 3:1){
  subset <- filter(wb_combined, rain == rains[i]) %>%
    arrange(soil)
  barplot(t(as.matrix(subset[, 1:5])), col = colors, las = 1, names.arg = c("No GI", "High", "Med", "Low"),
          main = rains[i], ylim = c(0, ymax))
}
mtext(side = 3, stringr::str_replace(city, "_", " "), outer = TRUE, line = 0, font = 2)
mtext(side = 2, "Depth [in]", outer = TRUE, line = 0)

legend("bottomleft", legend = c("Runoff", "GW Flow", "LID Drain", "Infilt.", "Evapor."), fill = colors, horiz = TRUE,
       xpd = NA, inset = c(-2, -0.2), bty = "n")

#f.horlegend(pos = "bottomleft", legend = c("Runoff", "GW Flow", "Infil", "Evap", "LID Drain"))
dev.off()
#######################################################################
#SUMMARY PLOTS

# comb_results2 <- do.call("rbind", comb_results) %>%
#   mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
#                                         "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))
# 
# write.csv(comb_results2, "~/WORK/SWMM_Rain_Gardens/City_Analysis/City_comb_results_basin.csv", row.names = FALSE)
comb_results2 <- read.csv("Results/City_comb_results_basin.csv") %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))
soils <- c("low", "med", "high")

colors <- RColorBrewer::brewer.pal(8, "Set1")
colors <- R.utils::insert(colors, ats = c(3, 4), values = adjustcolor(colors[c(2, 3)], alpha.f = 0.6))
colors <- colors[-8]
colors <- adjustcolor(colors, alpha.f = 0.8)
palette(colors)
# png("Results/Cities_Continuous_Summary_basin.png", type = "cairo", units = "in",
#     height = 8.5, width = 6.5, res = 500)
pdf("Results/Figure_5.pdf", height = 8.5, width = 6.5, pointsize = 12)
par(mfcol = c(3, 3), mar = c(4, 3, 1, 0.5), oma = c(5, 1, 2, 1.5), mgp = c(2, 0.8, 0))
for (i in 1:length(soils)){
  subset <- filter(comb_results2, soil == soils[i])
  
  plot(HPC_red ~ I(precip * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 85),
       cex = 1.5,
       main = "HPC", xlim = c(0, 203.2), mgp = c(2, 0.5, 0))
  axis(side = 1, at = seq(0, 80, 20) * 2.54, labels = seq(0, 80, 20), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(-0.05, -0.05, paste0("(", letters[i], ")"))
  add_label(0.98, 1.19, "[in]")
  add_label(0.98, 1.07, "[cm]")
  if (i == 1){
    mtext("High Pulse Count % Red.", side = 2, line = 2.7, cex = 0.8)
    mtext("Low Infiltration", side = 3, line = 1.2, font = 2)
  }else if (i == 2){
    mtext("Med Infiltration", side = 3, line = 1.2, font = 2)
  }else {
    mtext("High Infiltration", side = 3, line = 1.2, font = 2)
  }
  
  plot(HPD_red ~ I(precip * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = range(comb_results2$HPD_red), cex = 1.5,
       main = "HPD", xlim = c(0, 203.2), mgp = c(2, 0.5, 0))
  axis(side = 1, at = seq(0, 80, 20) * 2.54, labels = seq(0, 80, 20), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(0.98, 1.19, "[in]")
  add_label(0.98, 1.07, "[cm]")
  add_label(-0.05, -0.05, paste0("(", letters[i + 3], ")"))
  abline(h = 0, lwd = 2)
  if (i == 1){
    mtext("High Pulse Duration % Red.", side = 2, line = 2.7, cex = 0.8)
  }
  
  plot(vol_red ~ I(precip * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0,85), cex = 1.5,
       main = "Surface Runoff", xlim = c(0, 203.2), mgp = c(2, 0.5, 0))
  axis(side = 1, at = seq(0, 80, 20) * 2.54, labels = seq(0, 80, 20), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(0.98, 1.19, "[in]")
  add_label(0.98, 1.07, "[cm]")
  add_label(-0.05, -0.05, paste0("(", letters[i + 6], ")"))
  if (i == 1){
    mtext("Surface Runoff % Red.", side = 2, line = 2.7, cex = 0.8)
  }
  
}

mtext("Total Annual Precipitation", side = 1, outer = TRUE, line = -0.2)
legend("bottomleft", legend = c("New Orleans", "Atlanta", "Seattle", "Des Moines", "Tampa", "Columbus", "Elko", "Los Angeles", "Colorado Springs"),
       pch = 21, pt.bg = colors, bty = "n", ncol = 3, inset = c(-1.8, -0.65), xpd = NA, pt.cex = 1.5)
dev.off()

#Plot of sub-basin vs. watershed results for continuous sims
# event_results_df <- do.call("rbind", event_results_all) %>%
#   mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
#                                         "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))
#
# write.csv(event_results_df, "~/WORK/SWMM_Rain_Gardens/City_Analysis/City_event_results_basin.csv", row.names = FALSE)
event_results_df <- read.csv("Results/City_event_results_basin.csv") %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))


source("~/WORK/R Functions/Plot Functions.R")

high_data <- filter(event_results_df, soil == "high")
city_nms <- c("New\nOrleans", "Atlanta", "Seattle", "Des\nMoines", "Tampa", "Columbus", "Elko", "Los\nAngeles", "Colorado\nSprings")
n_events <- group_by(high_data, city) %>%
  summarize(n_events = n(),
            .groups = "keep")

#png("Results/Continous_SubvWatershed_basin.png", type = "cairo", units = "in", width = 6.5, height = 5, res = 500)
pdf("Results/Figure_7.pdf", height = 5, width = 6.5, pointsize = 12)
par(mfrow = c(2, 1), mar = c(3, 4.5, 1.5, 0.5), mgp = c(2.3, 0.7, 0))
rodplot(I(sub_peak - peak_red) ~ city, high_data, col = colors, las = 1, main = "Peak Flow", ylab = "Subbasins - Watershed\nPeak % Reduction",
        xlab = "", xaxt = "n", ylim = c(-175, 175))
abline(h = 0, lwd = 2)
axis(side = 1, at = 1:9, labels = city_nms, cex.axis = 0.8, padj = 1, mgp = c(2, -0.1, 0), gap.axis = 0.1)
text("Watershed Effects > Subbasins", x = 10, y = -165, pos = 2, cex = 0.8)
text("Subasin Effects > Watershed", x = 10, y = 165, pos = 2, cex = 0.8)
add_label(-0.02, -0.05, "(a)")

rodplot(I(sub_vol - vol_red) ~ city, high_data, col = colors, las = 1, main = "Runoff Volume", ylab = "Subbasins - Watershed\nVolume % Reduction",
        xlab = "", xaxt = "n", ylim = c(-175, 175))
abline(h = 0, lwd = 2)
axis(side = 1, at = 1:9, labels = city_nms, cex.axis = 0.8, padj = 1, mgp = c(2, -0.1, 0), gap.axis = 0.1)
axis(side = 1, at = 1:9, labels = n_events$n_events, tick = FALSE, line = 1.4, cex.axis = 0.8)
text("# storms =", x = -1, y = -315, pos = 4, cex = 0.8, xpd = NA)
text("Watershed Effects > Subbasins", x = 10, y = -165, pos = 2, cex = 0.8)
text("Subasin Effects > Watershed", x = 10, y = 165, pos = 2, cex = 0.8)
add_label(-0.02, -0.05, "(b)")
dev.off()

#GI Plot
#Get precip
precip_metrics <- list()
for (i in 1:length(cities)){
  precip_metrics[[i]] <- list()
  city <- cities[i]
  index <- which(names(hourly_data) == gsub("_", " ", city))
  events <- event_separation(data = hourly_data[[index]], city = city)
  
  #Get events for year of analysis
  event_summary <- events[[1]]
  years <- unique(event_summary$year)
  
  for (j in 1:length(rains)){
    rain <- rains[j]
    analysis_year <- case_when(rain == "Dry" ~ years[1],
                               rain == "Norm" ~ years[2],
                               rain == "Wet" ~ years[3])
    
    event_summary2 <- filter(event_summary, year == analysis_year)
    events_data <- filter(events[[2]], year == analysis_year)
    
    precip_metrics[[i]][[j]] <- data.frame(delta_mean = mean(event_summary2$delta, na.rm = TRUE),
                                           delta_med = median(event_summary2$delta, na.rm = TRUE),
                                           dur_mean = mean(event_summary2$dur, na.rm = TRUE),
                                           dur_med = median(event_summary2$dur, na.rm = TRUE),
                                           int_mean = mean(events_data$value, na.rm = TRUE),
                                           int_med = median(events_data$value, na.rm = TRUE),
                                           tot_precip = sum(event_summary2$vol, na.rm = TRUE),
                                           rain = rain,
                                           city = city)
  }
}

precip_metrics_df <- do.call("rbind", do.call("rbind", precip_metrics))

results_all_df <- do.call("rbind", results_all) %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs"))) %>%
  left_join(precip_metrics_df, by = c("city", "rain"))

int_season_df <- do.call("rbind", int_season_all) %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))

sat_season_df <- do.call("rbind", sat_season_all) %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))

write.csv(do.call("rbind", results_all), "~/WORK/SWMM_Rain_Gardens/City_analysis/City_GI_results.csv", row.names = FALSE)



colors <- RColorBrewer::brewer.pal(8, "Set1")
colors <- R.utils::insert(colors, ats = c(3, 4), values = adjustcolor(colors[c(2, 3)], alpha.f = 0.6))
colors <- colors[-8]
colors <- adjustcolor(colors, alpha.f = 0.8)
palette(colors)
soils <- c("low", "med", "high")
png("~/WORK/SWMM_Rain_Gardens/City_Analysis/Cities_GI_Summary2.png", type = "cairo", units = "in",
    height = 8.5, width = 6.5, res = 500)
par(mfcol = c(3, 3), mar = c(4.5, 2, 1.5, 1.5), oma = c(3, 1, 2, 1.7), mgp = c(2, 0.5, 0))
for (i in 1:length(soils)){
  subset <- filter(results_all_df, soil == soils[i])
  sub_int <- filter(int_season_df, soil == soils[i])
  sub_sat <- filter(sat_season_df, soil == soils[i])
  
  # plot(I(tot_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "Fraction Outflow")
  plot(I(tot_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "Total Outflow")
  axis(side = 1, at = seq(0.04, 0.16, 0.02) * 2.54, labels = seq(0.04, 0.16, 0.02), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(1.01, 1.19, "[in/hr]")
  add_label(1.01, 1.07, "[cm/hr]")
  
  add_label(-0.05, -0.05, paste0("(", letters[i], ")"))
  if (i == 1){
    mtext("Total Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
    mtext("Low Infiltration", side = 3, line = 1.5, font = 2)
  }else if (i == 2){
    mtext("Med Infiltration", side = 3, line = 1.5, font = 2)
    mtext("Mean Precip Intensity", side = 1, line = 3.3, cex = 0.8)
  }else {
    mtext("High Infiltration", side = 3, line = 1.5, font = 2)
  }
  
  # plot(I(frac_high_intensity * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "High Intensity")
  plot(I(high_intensity_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "High Intensity")
  axis(side = 1, at = seq(0.04, 0.16, 0.02) * 2.54, labels = seq(0.04, 0.16, 0.02), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(1.01, 1.19, "[in/hr]")
  add_label(1.01, 1.07, "[cm/hr]")
  
  add_label(-0.05, -0.05, paste0("(", letters[i + 3], ")"))
  if (i == 1){
    mtext("High Intensity Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
    #mtext("Low Infiltration", side = 3, line = 1.5, font = 2)
  }else if (i == 2){
    # mtext("Med Infiltration", side = 3, line = 1.5, font = 2)
    mtext("Mean Precip Intensity", side = 1, line = 3.3, cex = 0.8)
  }else {
    #mtext("High Infiltration", side = 3, line = 1.5, font = 2)
  }
  
  
  # plot(I(frac_sat * 100) ~ delta_mean, subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "Back-to-Back", xaxt= "n")
  plot(I(sat_outflow_ft3 / 95.5 / tot_inflow * 100) ~ delta_mean, subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "Back-to-Back")
  axis(side = 1, gap.axis = 0.5)
  add_label(-0.05, -0.05, paste0("(", letters[i + 6], ")"))
  if (i == 1){
    mtext("Back-to-Back Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
  }else if (i ==2){
    mtext("Mean Time Between Events [hr]", side = 1, line = 2, cex = 0.8)
  }
  
  
}

legend("bottomleft", legend = c("New Orleans", "Atlanta", "Seattle", "Des Moines", "Tampa", "Columbus", "Elko", "Los Angeles", "Colorado Springs"),
       pch = 21, pt.bg = colors, bty = "n", ncol = 3, inset = c(-1.8, -0.5), xpd = NA, pt.cex = 1.5)
dev.off()

#GI Plot
#Get precip
precip_metrics <- list()
for (i in 1:length(cities)){
  precip_metrics[[i]] <- list()
  city <- cities[i]
  index <- which(names(hourly_data) == gsub("_", " ", city))
  events <- event_separation(data = hourly_data[[index]], city = city)
  
  #Get events for year of analysis
  event_summary <- events[[1]]
  years <- unique(event_summary$year)
  
  for (j in 1:length(rains)){
    rain <- rains[j]
    analysis_year <- case_when(rain == "Dry" ~ years[1],
                               rain == "Norm" ~ years[2],
                               rain == "Wet" ~ years[3])
    
    event_summary2 <- filter(event_summary, year == analysis_year)
    events_data <- filter(events[[2]], year == analysis_year)
    
    precip_metrics[[i]][[j]] <- data.frame(delta_mean = mean(event_summary2$delta, na.rm = TRUE),
                                           delta_med = median(event_summary2$delta, na.rm = TRUE),
                                           dur_mean = mean(event_summary2$dur, na.rm = TRUE),
                                           dur_med = median(event_summary2$dur, na.rm = TRUE),
                                           int_mean = mean(events_data$value, na.rm = TRUE),
                                           int_med = median(events_data$value, na.rm = TRUE),
                                           tot_precip = sum(event_summary2$vol, na.rm = TRUE),
                                           rain = rain,
                                           city = city)
  }
}

precip_metrics_df <- do.call("rbind", do.call("rbind", precip_metrics))

results_all_df <- do.call("rbind", results_all) %>%
  mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
                                        "Columbus", "Elko", "Los_Angeles", "Colorado_Springs"))) %>%
  left_join(precip_metrics_df, by = c("city", "rain"))

# int_season_df <- do.call("rbind", int_season_all) %>%
#     mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
#                                         "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))
# 
# sat_season_df <- do.call("rbind", sat_season_all) %>%
#     mutate(city = factor(city, levels = c("New_Orleans", "Atlanta", "Seattle", "Des_Moines", "Tampa",
#                                         "Columbus", "Elko", "Los_Angeles", "Colorado_Springs")))

write.csv(results_all_df, "Results/City_GI_results.csv", row.names = FALSE)

colors <- RColorBrewer::brewer.pal(8, "Set1")
colors <- R.utils::insert(colors, ats = c(3, 4), values = adjustcolor(colors[c(2, 3)], alpha.f = 0.6))
colors <- colors[-8]
colors <- adjustcolor(colors, alpha.f = 0.8)
palette(colors)
soils <- c("low", "med", "high")
# png("Results/Cities_GI_Summary.png", type = "cairo", units = "in",
#     height = 8.5, width = 6.5, res = 500)
pdf("Results/Figure_6.pdf", height = 8.5, width = 6.5, pointsize = 12)
par(mfcol = c(3, 3), mar = c(4.5, 2, 1.5, 1.5), oma = c(3, 1, 2, 1.7), mgp = c(2, 0.5, 0))
for (i in 1:length(soils)){
  subset <- filter(results_all_df, soil == soils[i])
  # sub_int <- filter(int_season_df, soil == soils[i])
  # sub_sat <- filter(sat_season_df, soil == soils[i])
  
  # plot(I(tot_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "Fraction Outflow")
  plot(I(tot_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "Total Outflow")
  axis(side = 1, at = seq(0.04, 0.16, 0.02) * 2.54, labels = seq(0.04, 0.16, 0.02), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(1.01, 1.19, "[in/hr]")
  add_label(1.01, 1.07, "[cm/hr]")
  
  add_label(-0.05, -0.05, paste0("(", letters[i], ")"))
  if (i == 1){
    mtext("Total Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
    mtext("Low Infiltration", side = 3, line = 1.5, font = 2)
  }else if (i == 2){
    mtext("Med Infiltration", side = 3, line = 1.5, font = 2)
    mtext("Mean Precip Intensity", side = 1, line = 3.3, cex = 0.8)
  }else {
    mtext("High Infiltration", side = 3, line = 1.5, font = 2)
  }
  
  # plot(I(frac_high_intensity * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "High Intensity")
  plot(I(high_intensity_runoff / tot_inflow * 100) ~ I(int_mean * 2.54), subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "High Intensity")
  axis(side = 1, at = seq(0.04, 0.16, 0.02) * 2.54, labels = seq(0.04, 0.16, 0.02), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(1.01, 1.19, "[in/hr]")
  add_label(1.01, 1.07, "[cm/hr]")
  
  add_label(-0.05, -0.05, paste0("(", letters[i + 3], ")"))
  if (i == 1){
    mtext("High Intensity Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
    #mtext("Low Infiltration", side = 3, line = 1.5, font = 2)
  }else if (i == 2){
    # mtext("Med Infiltration", side = 3, line = 1.5, font = 2)
    mtext("Mean Precip Intensity", side = 1, line = 3.3, cex = 0.8)
  }else {
    #mtext("High Infiltration", side = 3, line = 1.5, font = 2)
  }
  
  
  # plot(I(frac_sat * 100) ~ delta_mean, subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
  #      ylim = c(0, 100), cex = 1.5,
  #      main = "Back-to-Back", xaxt= "n")
  plot(I(sat_outflow_ft3 / 95.5 / tot_inflow * 100) ~ delta_mean, subset, pch = 21, bg = subset$city, las = 1, xlab = "", ylab = "",
       ylim = c(0, 100), cex = 1.5,
       main = "Back-to-Back")
  axis(side = 1, gap.axis = 0.5)
  add_label(-0.05, -0.05, paste0("(", letters[i + 6], ")"))
  if (i == 1){
    mtext("Back-to-Back Outflow / Inflow [%]", side = 2, line = 1.8, cex = 0.8)
  }else if (i ==2){
    mtext("Mean Time Between Events [hr]", side = 1, line = 2, cex = 0.8)
  }
  
  
}

legend("bottomleft", legend = c("New Orleans", "Atlanta", "Seattle", "Des Moines", "Tampa", "Columbus", "Elko", "Los Angeles", "Colorado Springs"),
       pch = 21, pt.bg = colors, bty = "n", ncol = 3, inset = c(-1.8, -0.5), xpd = NA, pt.cex = 1.5)
dev.off()
