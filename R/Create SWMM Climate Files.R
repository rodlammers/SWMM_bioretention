library(dplyr)

#Create swmm climate inputs
load(file = "Data/Hourly Station Precip.Rdata")
load("Data/Daily station Temp.Rdata")

#Custom SWMM Climate files
cities <- c("New_Orleans",
            "Atlanta",
            "Seattle",
            "Des_Moines",
            "Tampa",
            "Elko",
            "Los_Angeles",
            "Columbus",
            "Colorado_Springs")


yrs <- c("Dry", "Norm", "Wet")
for (i in 1:length(daily)){
  for (j in 1:3){
    filenm <- paste0("Sims/Continuous/", cities[i], "/", cities[i], "_Climate_", yrs[j], ".txt")
    indices <- 365 * (j - 1) + 1:365
    data <- daily[[i]][indices, ] %>%
      ungroup() %>%
      mutate(city = cities[i],
             day = sprintf("%02d", lubridate::day(date)),
             month = sprintf("%02d", month),
             evap = NA,
             Avg_Wind = round(Avg_Wind, 1)) %>%
      select(city, year, month, day, Max_temp, Min_temp, evap, Avg_Wind)
    
    write.table(data, filenm, col.names = FALSE, row.names = FALSE, sep = " ", quote = FALSE)
  }
}

#Custom SWMM Rainfall files
for (i in 1:length(hourly_data)){
  years <- unique(hourly_data[[i]]$year)
  for (j in 1:3){
    filenm <- paste0("Sims/Continuous/", cities[i], "/", cities[i], "_PPT_", yrs[j], ".txt")
    data <- hourly_data[[i]] %>%
      ungroup() %>%
      filter(year == years[j]) %>%
      filter(value > 0) %>% #Remove missing data and zeroes (often Trace observations)
      mutate(city = cities[i],
             day = sprintf("%02d", lubridate::day(date)),
             month = sprintf("%02d", lubridate::month(date)),
             hour = sprintf("%02d", lubridate::hour(date)),
             min = sprintf("%02d", lubridate::minute(date))) %>%
      select(city, year, month, day, hour, min, value)
    
    write.table(data, filenm, col.names = FALSE, row.names = FALSE, sep = " ", quote = FALSE)
  }
}
