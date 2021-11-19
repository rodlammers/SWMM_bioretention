#Create NRCS design storms by city for various return periods
library(dplyr)
library(swmmr)

cities <- c("New_Orleans",
            "Atlanta",
            "Seattle",
            "Des_Moines",
            "Tampa",
            "Elko",
            "Los_Angeles",
            "Columbus",
            "Colorado_Springs")

#Get 1-month, 6-month, 2-yr, 10-yr, 50-yr, 100-yr for each city
storm_vals <- list()

par(mfrow = c(3, 3), mar = c(4, 4, 2, 0.5))
for (i in 1:length(cities)){
  #Get IDF Curve for city
  IDF <- read.csv(paste0("~/WORK/SWMM_Rain_Gardens/City_Analysis/", cities[i], "_IDF.csv"))
  
  rains <- filter(IDF, Hour == 24) %>%
    select(-Hour)
  
  rains <- rains[1:7]
  
  years <- as.numeric(substr(colnames(rains), 2, nchar(colnames(rains))))
  ##Extrapolating to predict more frequent rain events - not sure this is very accurate
  # plot(years, rains[1,], log = "xy", xlim = c(0.1, 100), ylim = c(min(as.numeric(rains[1,])) / 3, max(as.numeric(rains[1,]))),
  #      main = cities[i], ylab = "Rain depth [in]", xlab = "RI")
  # y <- log10(as.numeric(rains[1,]))
  # x <- log10(years)
  # fit <- lm(y ~ x)
  # predicted <- predict(fit, newdata = data.frame("x" = log10(c(0.5, 1/12))))
  # 
  # abline(fit)
  # points(c(0.5, 1/12), 10^predicted, col = "red")
  # legend("bottomright", legend = paste(c("1-yr =", "6-mo =", "1-mo ="), round(c(rains[1,1], 10^predicted), 2)), pch = NA, bty = "n")
  # 
  # if (cities[i] == "Seattle"){
  #   storm_vals[[i]] <- unlist(c(10^predicted[2], rains[1, c(1, 2, 4, 6, 7)]))
  # }else{
  #   storm_vals[[i]] <- unlist(c(rev(10^predicted), rains[1, c(2, 4, 6, 7)]))
  # }
  
  ##Just select frequent rain events as 1/2 and 1/4 of 2-yr event
  storm_vals[[i]] <- unlist(c(rains[1, 2] * c(0.25, 0.5), rains[1, c(2, 4, 6, 7)]))
  
  barplot(storm_vals[[i]], las = 1, main = cities[i], ylab = "Rain depth [in]")
  box()
}  

#Get storm temporal distributions
design_storms <- read.csv("~/WORK/SWMM_Rain_Gardens/City_Analysis/City_design_storms_v2.csv")

design_out <- list()
par(mfrow = c(3, 3), mar = c(4, 4, 2, 0.5))
for (i in 1:length(cities)){
  x <- design_storms[,1]
  y <- unlist(select(design_storms, cities[i]))
  
  plot(x, y, main = cities[i])
  
  x_out <- seq(0, 24, 0.1)
  y_out <- approx(x, y, xout = x_out)$y
  
  design_out[[i]] <- data.frame(time = x_out, cum_prob = y_out / 100)
  
  lines(x_out, y_out, col = "red")
}


#Create hyetographs
rain <- c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr")
hyeto <- list()
for (i in 1:length(cities)){
  hyeto[[i]] <- list()
  for (j in 1:length(rain)){
    hyeto[[i]][[j]] <- design_out[[i]] %>%
        mutate(cum_p = cum_prob * storm_vals[[i]][j],
               "Rainfall_in/hr" = c(0, diff(cum_p)) / 0.1,
               Rain_Hour = c(rep(seq(0, 23, 1), each = 10), 24),
               Rain_Minute = c(rep(seq(0, 54, by = 6), 24), 0),
               Rain_Station = rep(rain[j], n()),
               Rain_Year = rep(2000, n()),
               Rain_Month = rep(1, n()),
               Rain_Day = rep(1, n())) %>%
        select(Rain_Station, Rain_Year, Rain_Month, Rain_Day, Rain_Hour, Rain_Minute, "Rainfall_in/hr")
  }
}

#Create files
for (i in 1:length(cities)){
  for (j in 1:length(rain)){
    write.table(hyeto[[i]][[j]], paste0("~/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storms_NRCS/", cities[i], "/Design_Storms/Rainfall_", rain[j], ".dat"), sep = "\t", row.names = FALSE, 
                col.names = TRUE, quote = FALSE)
  }
}

###################################
#Run analyses - for all cities, all ratios, all rain events, 3 soil types - also plot results

plot_hydrographs <- function(results){
  
  zoo::plot.zoo(results[[1]][[1]][[1]], ylab = "Q [cfs]", las = 1, xlab = "Time", lwd = 2, 
                ylim = c(0, max(results[[1]][[1]][[1]])))
  grid()
  
  colors <- RColorBrewer::brewer.pal(n = length(results), "Greens")
  
  lty <- c(rep(1, 5), 2)
  for (i  in 2:length(results)){
    lines(zoo::as.zoo(results[[i]][[1]][[1]]), col = colors[i], lwd = 2, lty = lty[i - 1])
  }
  
  legend("topright", legend = c("1%", "5%", "10%", "15%", "20%"), col = colors[2:length(colors)], lwd = 2, lty = lty)
  
  #print(max(results[[length(results)]]$Outfall$total_inflow))
}

get_outflow <- function(swmm_files, location = "Outfall"){
  
  # #Get the output discharge at the outfall
  out <- read_out(file = swmm_files, iType = 1, object_name = location,
                  vIndex = 4)
  
  return(out)
  
}

get_report <- function(swmm_files){
  rpt <- read_rpt(swmm_files)
  
  return(rpt[1:4])
}

summarize_results <- function(results, report){
  peaks <- sapply(results, function(x){max(x[[1]][[1]])})
  time <- sapply(results, function(x){which.max(x[[1]][[1]])})
  #vol <- sapply(results, function(x){sum(x$Outfall$total_inflow) * 5 * 60 / 0.94 * 2.59 / 5280 ^ 2 * 12})
  # vol <- sapply(report, function(x){
  #   y <- x$flow_routing_continuity %>%
  #     filter(Component == "External Outflow")
  #   return(y$Volume_b) #depth of runoff in 10^6 gal
  # })
  vol <- sapply(results, function(x){sum(x[[1]][[1]])})
  
  out <- data.frame(peaks,
                    time,
                    vol,
                    treatment = c("Baseline", "1%", "5%", "10%", "15%", "20%"))
  
  return(out)
}

#Fix subcatchment report
fix_report <- function(y){
  nc <- ncol(y$subcatchment_runoff_summary)
  vals <- apply(y$subcatchment_runoff_summary[,nc], 1, strsplit, "\\s+")
  vals <- t(apply(do.call("rbind", do.call("rbind", vals)), 1, as.numeric))
  #drop column
  y$subcatchment_runoff_summary <- y$subcatchment_runoff_summary[,-nc]
  #Bind
  y$subcatchment_runoff_summary <- cbind(y$subcatchment_runoff_summary, vals)
  #Change names
  colnames(y$subcatchment_runoff_summary) <- c("Subcatchment",
                                               "Total_Precip",
                                               "Total_Runon",
                                               "Total_Evap",
                                               "Total_Infil",
                                               "Imperv_Runoff",
                                               "Perv_Runoff",
                                               "Total_Runoff_Depth",
                                               "Total_Runoff_Volume",
                                               "Total_Peak_Runoff",
                                               "Runoff_Coeff")
  
  return(y)
}

subcatchment_results <- function(ratio, rain, soil, city, dir_name = "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis"){
  filenm <- paste0(dir_name, "/", city, "/", "Design_Storms/", city, "_", soil, "_Rainfall_", rain, "_", ratio * 100, ".rpt")
  
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

subcatchment_results2 <- function(ratio, rain, soil, city, dir_name = "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis"){
  filenm <- paste0(dir_name, "/", city, "/", "Design_Storms/", city, "_", soil, "_Rainfall_", rain, "_", ratio * 100, ".rpt")
  
  rpt <- read_rpt(filenm)
  rpt <- fix_report(rpt)
  
  outnm <- stringr::str_replace(filenm, ".rpt", ".out")
  outputs <- lapply(rpt$subcatchment_runoff_summary$Subcatchment, function(x, outnm){
    output <- read_out(outnm, iType = 0, object_name = x, vIndex = 4)
    
    results <- data.frame(peak = max(output[[1]][[1]]),
                          vol = sum(output[[1]][[1]]),
                          sub = x) %>%
      mutate(peak_time = ifelse(peak == 0, NA, which(output[[1]][[1]] == peak)),
             centroid = ifelse(peak == 0, NA, sum(output[[1]][[1]] * 1:length(output[[1]][[1]])) / sum(output[[1]][[1]])))
    
    return(results)
  }, outnm = outnm)
  
  outputs <- do.call("rbind", outputs)
  
  test <- left_join(outputs, rpt$subcatchment_runoff_summary, by = c("sub" = "Subcatchment"))
  
  input <- read_inp(stringr::str_replace(filenm, "rpt", "inp"))
  
  baseline_outnm <- stringr::str_replace(filenm, paste0(ratio * 100, ".rpt"), "baseline.out")
  baseline_outputs <- lapply(rpt$subcatchment_runoff_summary$Subcatchment, function(x, outnm){
    output <- read_out(outnm, iType = 0, object_name = x, vIndex = 4)
    
    results <- data.frame(peak = max(output[[1]][[1]]),
                          vol = sum(output[[1]][[1]]),
                          sub = x) %>%
      mutate(peak_time = ifelse(peak == 0, NA, which(output[[1]][[1]] == peak)),
             centroid = ifelse(peak == 0, NA, sum(output[[1]][[1]] * 1:length(output[[1]][[1]])) / sum(output[[1]][[1]])))
    
    return(results)
  }, outnm = baseline_outnm)
  baseline_outputs <- do.call("rbind", baseline_outputs)
  
  comb <- left_join(outputs, input$subcatchments, by = c("sub" = "Name")) %>%
    left_join(baseline_outputs, by = "sub") %>%
    mutate(vol_red = ifelse(vol.y > 0, (vol.y - vol.x) / vol.y * 100, NA),
           peak_red = ifelse(peak.y > 0, (peak.y - peak.x) / peak.y * 100, NA),
           peak_diff = (peak_time.y - peak_time.x) / 4, #peak difference in hours (time steps are 15 min)
           centroid_diff = (centroid.y - centroid.x) / 4) #centroid difference in hours (time steps are 15 min)
  
  summary <- data.frame(cum_vol_red = sum(comb$vol_red * comb$Area, na.rm = TRUE) / sum(comb$Area),
                        cum_peak_red = sum(comb$peak_red * comb$Area, na.rm = TRUE) / sum(comb$Area),
                        peak_diff = mean(comb$peak_diff, na.rm = TRUE),
                        centroid_diff = mean(comb$centroid_diff, na.rm = TRUE),
                        ratio = paste0(ratio * 100, "%"),
                        rain = rain,
                        soil = soil,
                        city = city)
  
  #summary <- data.frame(cum_vol_red = Total_Runoff_Depth.y 
  
  return(summary)
}

source("~/WORK/SWMM_Rain_Gardens/Run SWMM Function.R")

ratio <- c(0, 0.01, 0.05, 0.1, 0.15, 0.2)
rains <- c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr")
soils <- c("low", "med", "high")
inp_file <- "~/WORK/SWMM_Rain_Gardens/City_Analysis/SHC_NEW_event_noflood.inp"

# results <- SWMM_event(ratio = ratio, dir_name = dir_name, inp_file = "~/WORK/SWMM_Rain_Gardens/City_Analysis/SHC_NEW_event.inp", 
#                       soils = "med", city = city, rain_file = rain_file, rain_name = rain_name)

#RUN SWMM FOR EACH CITY, STORM EVENT, SOIL, and BMP SCENARIO
for (i in 1:length(cities)){
#for (i in 2){
  city <- cities[i]
  dir_name <- paste0("C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storms_NRCS/", city, "/Design_Storms")
  
  for (j in 1:length(soils)){
    
    results <- list()
    for (k in 1:length(rains)){
      rain_file <- paste0("Rainfall_", rains[k], ".dat")
      rain_name <- rains[k]
      
      for (m in 1:length(ratio)){
        SWMM_event(ratio[m], dir_name = dir_name, inp_file = inp_file, soils = soils[j], city = city, rain_file = rain_file, rain_name = rain_name)
      }
      
      files <- lapply(ratio, function(x){list.files(dir_name, pattern = paste0(rain_name, "_", x * 100, ".out"), full.names = TRUE)})
      files[[1]] <- list.files(dir_name, pattern = paste0(rain_name, "_baseline.out"), full.names = TRUE)
      
      #Get only files with the correct soil type
      files <- lapply(files, function(x, soil_type){grep(soil_type, x, value = TRUE)}, soil_type = soils[j])
      
      results[[k]] <- lapply(files, get_outflow)
    }
    
    #Plot hydrographs
    # png(paste0(dir_name, "/Hydrographs_", soils[j], ".png"), type = "cairo", units = "in",
    #     height = 5, width = 6.5, res = 500)
    # par(mfrow = c(2, 3), mar = c(4, 4, 2, 0.5))
    # for (k in 1:length(results)){
    #   plot_hydrographs(results[[k]])
    #   title(main = rains[k])
    # }
    # dev.off()
  }
}



#Plot all hydrographs - also summarize by city
library(swmmr)
source("~/WORK/R Functions/Plot Functions.R")
rains <- c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr")
soils <- c("low", "med", "high")
ratio <- c(0, 0.01, 0.05, 0.1, 0.15, 0.2)

all_cities_summary <- list()
all_cities_sub <- list()
all_cities_sub2 <- list()
for (i in 1:length(cities)){
#for (i in 2){
  all_cities_summary[[i]] <- list()
  all_cities_sub[[i]] <- list()
  all_cities_sub2[[i]] <- list()
  city <- cities[i]
  #dir_name <- paste0("C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/", city, "/Design_Storms")
  dir_name <- paste0("C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storms_NRCS/", city, "/Design_Storms")
  
  results <- list()
  report <- list()
  summary <- list()
  sub_results <- list()
  sub_results2 <- list()
  for (j in 1:length(soils)){
    results[[j]] <- list()
    report[[j]] <- list()
    summary[[j]] <- list()
    sub_results[[j]] <- list()
    sub_results2[[j]] <- list()
    
    for (k in 1:length(rains)){
      files <- lapply(ratio, function(x){list.files(dir_name, pattern = paste0(rains[k], "_", x * 100, ".out"), full.names = TRUE)})
      files[[1]] <- list.files(dir_name, pattern = paste0(rains[k], "_baseline.out"), full.names = TRUE)
      
      #Get only files with the correct soil type
      files <- lapply(files, function(x, soil_type){grep(soil_type, x, value = TRUE)}, soil_type = soils[j])
      
      results[[j]][[k]] <- lapply(files, get_outflow, location = "10yr_Detention")
      
      files <- lapply(ratio, function(x){list.files(dir_name, pattern = paste0(rains[k], "_", x * 100, ".rpt"), full.names = TRUE)})
      files[[1]] <- list.files(dir_name, pattern = paste0(rains[k], "_baseline.rpt"), full.names = TRUE)
      
      #Get only files with the correct soil type
      files <- lapply(files, function(x, soil_type){grep(soil_type, x, value = TRUE)}, soil_type = soils[j])
      
      report[[j]][[k]] <- lapply(files, get_report)
      
      summary[[j]][[k]] <- summarize_results(results[[j]][[k]], report[[j]][[k]])
      
      #Get subcatchment-averaged performance and compare to watershed
      sub_results[[j]][[k]] <- do.call("rbind", lapply(ratio[2:6], subcatchment_results, rain = rains[k], soil = soils[j], city = cities[i],
                                       dir_name = "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storms_NRCS"))
      sub_results2[[j]][[k]] <- do.call("rbind", lapply(ratio[2:6], subcatchment_results2, rain = rains[k], soil = soils[j], city = cities[i],
                                                        dir_name = "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storms_NRCS"))
    }
    
    #Plot hydrographs
    png(paste0(dir_name, "/", cities[i], "_Hydrographs_basin_", soils[j], ".png"), type = "cairo", units = "in",
        height = 5, width = 6.5, res = 500)
    par(mfrow = c(2, 3), mar = c(4, 4, 2, 0.5), oma = c(0, 0, 2, 0))
    for (k in 1:length(results[[j]])){
      plot_hydrographs(results[[j]][[k]])
      title(main = paste0(rains[k], " (", storm_vals[[i]][k], " in)"))
    }
    mtext(paste(cities[[i]], "-", soils[j], "soil"), side = 3, outer = TRUE, line = 0.5, font = 2)
    dev.off()
    
    #Plot summary figures
    treat_names = c("Baseline", "1%", "5%", "10%", "15%", "20%")
    summary[[j]] <- purrr::map2(summary[[j]], rains, function(x, y){
      x$Event <- y
      return(x)
    })
    summary_all <- do.call("rbind", summary[[j]]) %>%
      mutate(treatment = factor(treatment, levels = treat_names),
             Event = factor(Event, levels = rains))
    summary_all <- summary_all %>%
      group_by(Event) %>%
      mutate(peak_red = (peaks[1] - peaks) / peaks[1] * 100,
             time_diff = (time - time[1]) * 5,
             vol_red = (vol[1] - vol) / vol[1] * 100) %>%
      filter(treatment != "Baseline")
    
    colors <- RColorBrewer::brewer.pal(n = length(treat_names) - 1, "Greens")
    colors2 <- adjustcolor(colors, alpha.f = 0.7)
    png(paste0(dir_name, "/", cities[i], "_Performance_basin_", soils[j], ".png"), type = "cairo", units = "in",
        height = 8, width = 4, res = 500)
    par(mfrow = c(3, 1), mar = c(3, 4, 2, 0.5), oma = c(0, 0, 2, 0))
    plot(peak_red ~ as.numeric(Event), summary_all, pch = 21, bg = colors2, cex = 1.3,
         las = 1, ylab = "% Peak Flow Reduction", xlab = "", xaxt = "n", main = "Peak Flow")
    axis(side = 1, at = 1:6, labels = rains)
    treat <- unique(summary_all$treatment)
    for(k in 1:length(treat)){
      sub <- filter(summary_all, treatment == treat[k])
      lines(peak_red ~ as.numeric(Event), sub, col = colors2[k])
    }
    abline(h = 0)

    plot(time_diff ~ as.numeric(Event), summary_all, type = "p", pch = 21, bg = colors2, cex = 1.3,
         las = 1, ylab = "Peak Flow Delay [min]", xlab = "", xaxt = "n", main = "Peak Timing")
    axis(side = 1, at = 1:6, labels = rains)
    for(k in 1:length(treat)){
      sub <- filter(summary_all, treatment == treat[k])
      lines(time_diff ~ as.numeric(Event), sub, col = colors2[k])
    }
    abline(h = 0)

    plot(vol_red ~ as.numeric(Event), summary_all, type = "p", pch = 21, bg = colors2, cex = 1.3,
         las = 1, ylab = "% Volume Reduction", xlab = "", xaxt = "n", main = "Runoff Volume")
    axis(side = 1, at = 1:6, labels = rains)
    for(k in 1:length(treat)){
      sub <- filter(summary_all, treatment == treat[k])
      lines(vol_red ~ as.numeric(Event), sub, col = colors2[k])
    }
    abline(h = 0)
    legend("topright", legend = c("1%", "5%", "10%", "15%", "20%"), col = colors2, lwd = 2, bty = "n")

    mtext(paste(cities[[i]], "-", soils[j], "soil"), side = 3, outer = TRUE, line = 0.5, font = 2)
    dev.off()
    
    all_cities_summary[[i]][[j]] <- summary_all %>%
      mutate(City = cities[i],
             Soil = soils[j])
    
    ###########################
    #Sub results
    sub_results_comb <- do.call("rbind", sub_results[[j]]) %>%
      left_join(summary_all, by = c("ratio" = "treatment", "rain" = "Event")) %>%
      arrange(desc(rain))

    all_cities_sub[[i]][[j]] <- sub_results_comb %>%
      left_join(data.frame(storm_val = storm_vals[[i]],
                           rain = factor(c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr"),
                                         levels = c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr"))))

    all_cities_sub2[[i]][[j]] <- do.call("rbind", sub_results2[[j]]) %>%
      left_join(summary_all, by = c("ratio" = "treatment", "rain" = "Event")) %>%
      arrange(desc(rain)) %>%
      left_join(data.frame(storm_val = storm_vals[[i]],
                           rain = factor(c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr"),
                                         levels = c("1_month", "6_month", "2yr", "10yr", "50yr", "100yr"))))

    png(paste0(dir_name, "/", cities[i], "_WatershedScaling_basin_", soils[j], ".png"), type = "cairo", units = "in",
        height = 6.5, width = 4.5, res = 500)
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 0.5), oma = c(0, 0, 2, 0), mgp = c(2, 0.8, 0))
    colors <- adjustcolor(RColorBrewer::brewer.pal(5, "Greens"), alpha.f = 0.7)
    plot(cum_peak_red ~ peak_red, all_cities_sub2[[i]][[j]], pch = 21, bg = colors, cex = as.numeric(sub_results_comb$rain) / 2,
         xlab = "Watershed % Peak Reduction", ylab = "Subcatchment % Peak Reduction", las = 1,
         xlim = range(cum_peak_red, peak_red), ylim = range(cum_peak_red, peak_red), main = "Peak Flow")
    abline(a = 0, b = 1, lwd = 2)
    legend("bottomright", legend = c("1%", "5%", "10%", "15%", "20%"), pt.bg = colors, pch = 21, bty = "n")

    plot(cum_vol_red ~ vol_red, all_cities_sub2[[i]][[j]], pch = 21, bg = colors, cex = as.numeric(sub_results_comb$rain) / 2,
         xlab = "Watershed % Volume Reduction", ylab = "Subcatchment % Volume Reduction", las = 1,
         xlim = range(cum_vol_red, vol_red), ylim = range(cum_vol_red, vol_red), main = "Runoff Volume")
    abline(a = 0, b = 1, lwd = 2)
    legend("bottomright", legend = c("1mo", "6mo", "2yr", "10yr", "50yr", "100yr"), pch = 21, pt.bg = colors[5], pt.cex = 1:6 / 2,
           bty = "n")

    mtext(paste(cities[[i]], "-", soils[j], "soil"), side = 3, outer = TRUE, line = 0.5, font = 2)
    dev.off()
  }

}

all_cities_df <- do.call("rbind", do.call("rbind", all_cities_summary))
storm_vals2 <- list()
for (i in 1:length(storm_vals)){
  storm_vals2[[i]] <- data.frame(Rain = storm_vals[[i]],
                                 City = cities[i],
                                 Event = factor(rains, levels = rains))
}
storm_vals_df <- do.call("rbind", storm_vals2)

all_cities_df <- left_join(all_cities_df, storm_vals_df, by = c("City", "Event"))

write.csv(all_cities_df, "~/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storm_Performance_Results.csv", row.names = FALSE)

png("~/WORK/SWMM_Rain_Gardens/City_Analysis/Performance_Plots_all_NRCS_basin.png", type = "cairo",
    units = "in", height = 9, width = 6.5, res = 500)
par(mfcol = c(4, 3), mar = c(4.5, 2.5, 1.5, 0.5), oma = c(3, 0.8, 2, 1.4), mgp = c(2.5, 0.8, 0))

colors2 <- adjustcolor(RColorBrewer::brewer.pal(5, "Greens"), alpha.f = 0.7)
for (i in 1:length(soils)){
  soils_sub <- filter(all_cities_df, Soil == soils[i])
  colors_rain <- cRamp(log10(soils_sub$Rain), "BuPu", alpha = 0.7)

  plot(peak_red ~ I(Rain * 2.54), soils_sub, type = "p", pch = 21, bg = colors2, cex = 1.3,
       las = 1, ylab = "", xlab = "", main = "Peak Flow", ylim = range(all_cities_df$peak_red), xaxt = "n")
  add_label(-0.05, -0.05, paste0("(", letters[i], ")"))
  axis(side = 1, mgp = c(2, 0.5, 0))
  axis(side = 1, at = seq(0, 14, 2) * 2.54, labels = seq(0, 14, 2), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(0.98, 1.28, "[in]")
  add_label(0.98, 1.11, "[cm]")
  mtext(paste(Hmisc::capitalize(soils[i]), "Infiltration"), side = 3, line = 2, font = 2)
  if (i == 1){
    mtext("% Peak Flow Reduction", side = 2, line = 2.2, cex = 0.7)
  }else if (i == 3){
    #legend("topright", legend = c("1%", "5%", "10%", "15%", "20%"), pt.bg = colors2, pch = 21, cex = 1.2, bty = "n")
  }else if (i == 2){
    mtext("Storm Depth", side = 1, line = 3.2, cex = 0.7)
  }
  
  plot(peak_red ~ as.numeric(treatment), soils_sub, type = "n", cex = 1.3,
       las = 1, ylab = "", xlab = "", main = "Peak Flow", ylim = range(all_cities_df$peak_red),
       xaxt = "n")
  add_label(-0.05, -0.05, paste0("(", letters[i + 3], ")"))
  for(j in 1:(nrow(soils_sub) / 5)){
    lines(peak_red ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], col = colors_rain[5 * j])
    points(peak_red ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], 
           bg = colors_rain[(5 * (j  - 1) + 1):(5 * j)], pch = 21, cex = 1.3)
  }
  
  axis(side = 1, at = 2:6, labels = c("1%", "5%", "10%", "15%", "20%"))
  if (i == 1){
    mtext("% Peak Flow Reduction", side = 2, line = 2.2, cex = 0.7)
  }else if(i == 3){
    #legend("topleft", legend = round(seq(min(soils_sub$Rain), max(soils_sub$Rain), length.out = 5), 1), 
    #       pt.bg = cRamp_legend(5, "BuPu", alpha = 0.7), pch = 21, cex = 1.2, bty = "n")
  } else if (i == 2){
    mtext("Drainage Area Ratio", side = 1, line = 2.5, cex = 0.7)
  }
  
  # plot(time_diff ~ Rain, soils_sub, type = "p", pch = 21, bg = colors2, cex = 1.3,
  #      las = 1, ylab = "", xlab = "", main = "Peak Timing", ylim = range(all_cities_df$time_diff))
  # if (i == 1){
  #   mtext("Peak Flow Delay [min]", side = 2, line = 2.5, cex = 0.7)
  # }
  
  plot(vol_red ~ I(Rain * 2.54), soils_sub, type = "p", pch = 21, bg = colors2, cex = 1.3,
       las = 1, ylab = "", xlab = "", main = "Runoff Volume", xpd = NA, ylim = range(all_cities_df$vol_red), xaxt = "n")
  add_label(-0.05, -0.05, paste0("(", letters[i + 6], ")"))
  axis(side = 1, mgp = c(2, 0.5, 0))
  axis(side = 1, at = seq(0, 14, 2) * 2.54, labels = seq(0, 14, 2), line = 1.7, mgp = c(2, 0.5, 0))
  add_label(0.98, 1.28, "[in]")
  add_label(0.98, 1.11, "[cm]")
  if (i == 1){
    mtext("% Volume Reduction", side = 2, line = 2.2, cex = 0.7)
  } else if (i == 3){
    #legend("topright", legend = c("1%", "5%", "10%", "15%", "20%"), pt.bg = colors2, pch = 21, cex = 1.2, bty = "n")
  } else if (i == 2){
    mtext("Storm Depth", side = 1, line = 3.2, cex = 0.7)
  }
  
  plot(vol_red ~ as.numeric(treatment), soils_sub, type = "n", cex = 1.3,
       las = 1, ylab = "", xlab = "", main = "Runoff Volume", ylim = range(all_cities_df$vol_red),
       xaxt = "n")
  add_label(-0.05, -0.05, paste0("(", letters[i + 9], ")"))
  for(j in 1:(nrow(soils_sub) / 5)){
    lines(vol_red ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], col = colors_rain[5 * j])
    points(vol_red ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], 
           bg = colors_rain[(5 * (j  - 1) + 1):(5 * j)], pch = 21, cex = 1.3)
  }
  axis(side = 1, at = 2:6, labels = c("1%", "5%", "10%", "15%", "20%"))
  if (i == 1){
    mtext("% Volume Reduction", side = 2, line = 2.2, cex = 0.7)
  }else if(i == 3){
    #legend("topleft", legend = round(seq(min(soils_sub$Rain), max(soils_sub$Rain), length.out = 5), 1), 
    #       pt.bg = cRamp_legend(5, "BuPu", alpha = 0.7), pch = 21, cex = 1.2, bty = "n")
  }else if (i == 2){
    mtext("Drainage Area Ratio", side = 1, line = 2.5, cex = 0.7)
  }
  

}

legend("bottomleft", legend = c("1%", "5%", "10%", "15%", "20%"), pt.bg = colors2, pch = 21, cex = 1.2, bty = "n",
       title = "Drainage Area Ratio", horiz = TRUE, inset = c(-2.5, -0.7), xpd = NA)
legend("bottomleft", legend = round(10 ^ seq(log10(min(soils_sub$Rain)), log10(max(soils_sub$Rain)), length.out = 5) * 2.54, 1), 
        pt.bg = cRamp_legend(5, "BuPu", alpha = 0.7), pch = 21, cex = 1.2, bty = "n", title = "Storm Depth [cm]",
       horiz = TRUE, inset = c(-0.5, -0.7), xpd = NA)

#mtext("Storm Depth [in]", side = 1, outer = TRUE, line = 0, cex = 0.7)
dev.off()


#boxplot summaries
# source("~/WORK/R Functions/Plot Functions.R")
# 
# png("~/WORK/SWMM_Rain_Gardens/City_Analysis/Performance_boxplots.png", type = "cairo",
#     units = "in", height = 4.5, width = 6.5, res = 500)
# par(mfcol = c(2, 3), mar = c(2.5, 2.5, 2, 0.5), oma = c(1, 1, 2, 0), mgp = c(2.5, 1, 0))
# for (i in 1:length(soils)){
#   soils_sub <- filter(all_cities_df, Soil == soils[i]) %>%
#     mutate(treatment = droplevels(treatment))
#   
#   
#   rodplot(peak_red ~ treatment, soils_sub, col = colors2, las = 1, ylim = range(all_cities_df$peak_red), xlab = "", ylab = "",
#           main = "Peak Flow")
#   mtext(paste(Hmisc::capitalize(soils[i]), "Infiltration Capacity"), side = 3, line = 2, font = 2)
#   if (i == 1){
#     mtext("% Peak Flow Reduction", side = 2, line = 2.5, cex = 0.7)
#   }
#   
#   # rodplot(time_diff ~ treatment, soils_sub, col = colors2, las = 1, ylim = range(all_cities_df$time_diff), xlab = "", ylab = "",
#   #         main = "Peak Timing")
#   # if (i == 1){
#   #   mtext("Peak Flow Delay [min]", side = 2, line = 2.5, cex = 0.7)
#   # }
#   
#   rodplot(vol_red ~ treatment, soils_sub, col = colors2, las = 1, ylim = range(all_cities_df$vol_red), xlab = "", ylab = "",
#           main = "Runoff Volume")
#   if (i == 1){
#     mtext("% Volume Reduction", side = 2, line = 2.5, cex = 0.7)
#   }
#   
# }
# mtext("Loading Ratio", side = 1, outer = TRUE, line = 0, cex = 0.7)
# dev.off()

#Subbasin summary results
# all_subs_df <- do.call("rbind", do.call("rbind", all_cities_sub))
# 
# colors <- adjustcolor(RColorBrewer::brewer.pal(5, "Greens"))
# 
# 
# png("~/WORK/SWMM_Rain_Gardens/City_Analysis/All_Cities_Subbasin_Results.png", type = "cairo", units = "in",
#     height = 5, width = 5, res = 500)
# par(mfrow = c(2, 1), mar = c(2.5, 4, 1.5, 0.5), mgp = c(2.1, 0.8, 0), oma = c(1, 0, 0, 0))
# plot(I(cum_peak_red - peak_red) ~ storm_val, all_subs_df, pch = 21, bg = colors, ylab = "Subbasins - Watershed\nPeak % Reduction",
#      xlab = "", las = 1, main = "Peak Flow", ylim = c(-50, 50))
# add_label(-0.05, -0.05, "(a)")
# abline(h = 0, lwd = 2)
# text("Watershed Effects > Subbasins", x = 14, y = -15, pos = 2)
# text("Subasin Effects > Watershed", x = 14, y = 15, pos = 2)
# 
# plot(I(cum_vol_red - vol_red) ~ storm_val, all_subs_df, pch = 21, bg = colors, ylab = "Subbasins - Watershed\nVolume % Reduction",
#      xlab = "Rainfall Depth [in]", las = 1, main = "Runoff Volume", xpd = NA, ylim = c(-50, 50))
# add_label(-0.05, -0.05, "(b)")
# abline(h = 0, lwd = 2)
# text("Watershed Effects > Subbasins", x = 14, y = -15, pos = 2)
# text("Subasin Effects > Watershed", x = 14, y = 15, pos = 2)
# 
# legend("bottomleft", legend = c("1%", "5%", "10%", "15%", "20%"), pch = 21, pt.bg = colors, bty = "n", ncol = 2)
# dev.off()

######################################
#Different computing - uses hydrograph of runoff to be more consistent with continuous simulation results. Very little difference
#between this method and using the summary report numbers
all_subs_df2 <- do.call("rbind", do.call("rbind", all_cities_sub2))

write.csv(all_subs_df2, "~/WORK/SWMM_Rain_Gardens/City_Analysis/Design_Storm_Subbasin_Scaling_Results.csv", row.names = FALSE)

colors <- adjustcolor(RColorBrewer::brewer.pal(5, "Greens"))


png("~/WORK/SWMM_Rain_Gardens/City_Analysis/All_Cities_Subbasin_Results_NRCS_basin.png", type = "cairo", units = "in",
    height = 6, width = 5.5, res = 500)
par(mfrow = c(2, 1), mar = c(4, 4, 1.5, 2), mgp = c(2.1, 0.8, 0), oma = c(1, 0, 0, 0))
plot(I(cum_peak_red - peak_red) ~ I(storm_val * 2.54), all_subs_df2, pch = 21, bg = colors, ylab = "Subbasins - Watershed\nPeak % Reduction",
     xlab = "", las = 1, main = "Peak Flow", ylim = c(-50, 50), xaxt = "n")
add_label(-0.05, -0.05, "(a)")
axis(side = 1, mgp = c(2, 0.5, 0))
axis(side = 1, at = seq(0, 14, 2) * 2.54, labels = seq(0, 14, 2), line = 1.7, mgp = c(2, 0.5, 0))
add_label(0.98, 1.31, "[in]")
add_label(0.98, 1.12, "[cm]")

abline(h = 0, lwd = 2)
text("Watershed Effects > Subbasins", x = 14 * 2.54, y = -15, pos = 2)
text("Subasin Effects > Watershed", x = 14 * 2.54, y = 15, pos = 2)

plot(I(cum_vol_red - vol_red) ~ I(storm_val * 2.54), all_subs_df2, pch = 21, bg = colors, ylab = "Subbasins - Watershed\nVolume % Reduction",
     xlab = "", las = 1, main = "Runoff Volume", xpd = NA, ylim = c(-50, 50), xaxt = "n")
add_label(-0.05, -0.05, "(b)")
axis(side = 1, mgp = c(2, 0.5, 0))
axis(side = 1, at = seq(0, 14, 2) * 2.54, labels = seq(0, 14, 2), line = 1.7, mgp = c(2, 0.5, 0))
add_label(0.98, 1.31, "[in]")
add_label(0.98, 1.12, "[cm]")
abline(h = 0, lwd = 2)
text("Watershed Effects > Subbasins", x = 14 * 2.54, y = -15, pos = 2)
text("Subasin Effects > Watershed", x = 14 * 2.54, y = 15, pos = 2)
mtext(side = 1, "Storm Depth", line = 3.5)

legend("bottomleft", legend = c("1%", "5%", "10%", "15%", "20%"), pch = 21, pt.bg = colors, bty = "n", ncol = 2)
dev.off()

######################################

#Comparing % reduction in peak flow vs volume
source("~/WORK/R Functions/Plot Functions.R")
png("~/WORK/SWMM_Rain_Gardens/City_Analysis/Peak_v_vol_results_NRCS_basin.png", type = "cairo", units = "in",
    height = 4, width = 6.5, res = 500)
par(mfrow = c(1,3), mar = c(4, 3, 2, 0.5), oma = c(3, 1, 0, 0))
max_y <- max(all_cities_df$peak_red - all_cities_df$vol_red)
for (i in 1:length(soils)){
  soils_sub <- filter(all_cities_df, Soil == soils[i])
  colors_rain <- cRamp(log10(soils_sub$Rain), "BuPu", alpha = 0.7)
  
  plot(I(peak_red - vol_red) ~ as.numeric(treatment), soils_sub, type = "n", cex = 1.3,
       las = 1, ylab = "", xlab = "", ylim = c(-max_y, max_y), xaxt = "n",
       main = paste(Hmisc::capitalize(soils[i]), "Infiltration")) 
  add_label(-0.05, -0.02, paste0("(", letters[i], ")"))
  for(j in 1:(nrow(soils_sub) / 5)){
    lines(I(peak_red - vol_red) ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], col = colors_rain[5 * j])
    points(I(peak_red - vol_red) ~ as.numeric(treatment), soils_sub[(5 * (j  - 1) + 1):(5 * j),], 
           bg = colors_rain[(5 * (j  - 1) + 1):(5 * j)], pch = 21, cex = 1.3)
  }
  
  text("Peak Red. > Vol Red.", x = 6.2, y = 45, pos = 2)
  text("Vol Red. > Peak Red.", x = 6.2, y = -45, pos = 2)
  
  abline(h = 0, lwd = 2)
  axis(side = 1, at = 2:6, labels = c("1%", "5%", "10%", "15%", "20%"))
}
mtext(side = 2, text = "Peak Flow - Volume Reduction [%]", outer = TRUE, line = -0.5, cex = 0.8)
mtext(side = 1, text = "Drainage Area Ratio", outer = TRUE, line = -1.5, cex = 0.8)
legend("bottomleft", legend = round(10 ^ seq(log10(min(soils_sub$Rain)), log10(max(soils_sub$Rain)), length.out = 5) * 2.54, 1), 
       pt.bg = cRamp_legend(5, "BuPu", alpha = 0.7), pch = 21, cex = 1.2, bty = "n", title = "Storm Depth [cm]",
       horiz = TRUE, inset = c(-1.65, -0.35), xpd = NA)
dev.off()
#############################################

dir <- "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/City_Analysis/Atlanta/Design_Storms/Atlanta_"
soil <- "high"
rain <- "2yr"
size <- "10"

files <- paste0(dir, soil, "_Rainfall_", rain, "_", size, ".rpt")

rpt <- read_rpt(files)


rpt <- fix_report(rpt)

#Get input
inp_file <- stringr::str_replace(files, "rpt", "inp")
inputs <- read_inp(inp_file)

lid_inp <- left_join(inputs$lid_usage, inputs$subcatchments, by = c("Subcatchment" = "Name")) %>%
  filter(`LID Process` == "BioRet1") %>%
  mutate(Area_treated = Area.x / 43560 * as.numeric(size),
           Area_frac = Area_treated / Area.y)

comb <- left_join(rpt$lid_performance_summary, rpt$subcatchment_runoff_summary, by = "Subcatchment") %>%
  filter(`LID Control` == "BioRet1") %>%
  left_join(lid_inp, by = "Subcatchment") %>%
  mutate(Tot_Outflow = Surface_Outflow + Drain_Outflow,
         RC_bioret = Tot_Outflow / Total_Inflow,)

plot(RC_bioret ~ Runoff_Coeff, comb)
