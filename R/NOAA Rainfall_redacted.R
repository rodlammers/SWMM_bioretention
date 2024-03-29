library(rnoaa)
library(dplyr)
source("~/WORK/R Functions/Plot Functions.R")
options(noaakey = "") #Need to obtain and set personalized key to access NOAA API

#Selected weather stations
stations <- c("COOP:166660", #NOLA Airport
              "COOP:090451", #ATL Airport
              "COOP:457473", #Seattle Airport
              "COOP:132203", #Des Moines Airport
              "COOP:088788", #Tampa Airport
              "COOP:262573", #Elko, NV
              "COOP:045114", #Los Angeles Airport
              "COOP:331786", #Columbus Airport
              "COOP:051778") #colorado Springs

names(stations) <- c("New Orleans",
                     "Atlanta",
                     "Seattle",
                     "Des Moines",
                     "Tampa",
                     "Elko",
                     "Los Angeles",
                     "Columbus",
                     "Colorado Springs")


#Function to get precip data for weather stations
get_ppt_data <- function(station, name, choose_years = NULL, time_zone = NULL, output = "hourly"){
  
    
  #Split years into thirds to make sure we get all the data - there seems to be
  #a limit to the amount of data that can be retrieved at one time
    if(!is.null(choose_years)){
      years <- choose_years
    }else{
      years <- 1981:2010
    }
    ppt_yrs <- list()
    count <- 1
    for (i in 1:length(years)){
      for (j in 1:3){
        startdate <- case_when(j == 1 ~ paste0(years[i], "-01-01"),
                                j == 2 ~ paste0(years[i], "-05-01"),
                                j == 3 ~ paste0(years[i], "-09-01"))
        enddate <- case_when(j == 1 ~ paste0(years[i], "-04-30"),
                                j == 2 ~ paste0(years[i], "-08-31"),
                                j == 3 ~ paste0(years[i], "-12-31"))
        
        data <- ncdc(stationid = unname(station), datasetid = "PRECIP_HLY", limit = 1000,
                             startdate = startdate, 
                             enddate = enddate, add_units = TRUE)
        
        if (nrow(data$data) == 1000){
          print(paste("WARNING: MAX DATA POINTS EXCEEDED:", years[i], name))
        }
        
        if (nrow(data$data) != 0){
          ppt_yrs[[count]] <- data$data %>%
            mutate(date = gsub("T", " ", date),
                   date = as.POSIXct(date)) %>%
            mutate(value = if_else(value > 50000, -100L, value))
          
          count <- count + 1
        }
        Sys.sleep(0.5) #pause for half a second to prevent too many calls to NOAA API
      }
    }
    
  
  all_data <- do.call("rbind", ppt_yrs)
    
  if (output == "yearly"){
    summary <- all_data %>%
      mutate(year = lubridate::year(date),
             value = value / 100) %>% #Convert from 100ths of an inch to inches
      group_by(year) %>%
      summarize(tot_ppt = sum(value[value > 0]),
                p_missing = sum(value < 0) / n()) %>% #Find percent missing and drop years w/ >5%
      filter(p_missing < 0.05)
    
    #print(summary)
    colors <- cRamp(c(summary$p_missing, 1), "Reds")[1:nrow(summary)]
    plot(tot_ppt ~ year, summary, ylab = "Annual PPT [in]", main = name, pch = 21, bg = colors)
    abline(h = median(summary$tot_ppt))
    abline(h = quantile(summary$tot_ppt, probs = c(0.1, 0.9)), lty = 2)
    
    return(summary)
    
  }else if(output == "hourly"){
    data <- all_data %>%
      mutate(year = lubridate::year(date),
             value = value / 100) %>% #Convert to inches
      filter(year %in% choose_years)
    
    #Fix time zone issues
    lubridate::tz(data$date) <- time_zone[1]
    data$date <- lubridate::with_tz(data$date, "UTC")
    data$date <- data$date - as.numeric(time_zone[2]) * 3600
    # data %>%
    #   group_by(year) %>%
    #   plot(value)
    return(data)
  }

}


#Get and print yearly data
# yearly_data <- Map(get_ppt_data, stations, names(stations), output = "yearly")
# 
# save(yearly_data, file = "Data/Yearly Station Precip.Rdata")

#Select low-med-high precipitation years for each city - selected based on
#pots of yearly data

ppt_years <- list(c(1999, 2002, 2009), #New Orleans
              c(1999, 1993, 1994), #Atlanta
              c(2000, 2001, 1990), #Seattle
              c(2005, 1996, 1990), #Des Moines
              c(1999, 2008, 2004), #Tampa
              c(2002, 2009, 2005), #Elko
              c(2002, 2000, 1993), #Los Angeles
              c(1991, 1993, 1995), #Columbus
              c(2005, 2009, 2004)) #Colorado Springs

time_zones <- list(c("America/Chicago", 6), #New Orleans
                   c("America/New_York", 5), #Atlanta
                   c("America/Los_Angeles", 8), #Seattle
                   c("America/Chicago", 6), #Des Moines
                   c("America/New_York", 5), #Tampa
                   c("America/Los_Angeles", 8), #Elko
                   c("America/Los_Angeles", 8), #Los Angeles
                   c("America/New_York", 5), #Columbus
                   c("America/Denver", 7)) #Colorado Springs

#Get hourly data
hourly_data <- Map(get_ppt_data, stations, name = names(stations[1]), choose_years = ppt_years, time_zone = time_zones, output = "hourly")

#NOTE: THE DATES IN 'HOURLY_DATA' WILL BE LISTED AS BEING IN UTC. IN FACT, THEY ARE IN LOCAL TIME, BUT STRIPPED OF ANY DAYLIGHT SAVINGS TIME ADJUSTMENT
#EG: 05/01 AT 11:00:00 UTC IN ATLANTA IS REALLY 05/01 AT 11:00:00 EST (ACTUAL TIME IS 12:00:00 EDT)

#Plot by year
for (i in 1:length(hourly_data)){
  par(mfrow = c(3, 1))
  ymax <- max(hourly_data[[i]]$value)
  years <- ppt_years[[i]]
  name <- names(hourly_data)[i]
  for (j in 1:length(years)){
    data <- filter(hourly_data[[i]], year == years[j])
    plot(value ~ date, data, ylab = "PPT [in/hr]", main = paste(name, "-", data$year[1]), type = "h", ylim = c(0, ymax))
  }
}

save(list = "hourly_data", file = "Data/Hourly Station Precip.Rdata")

#Get temperature data
temp_stations <- c("72231012916", #New Orleans,
                   "72219013874", #Atlanta
                   "72793024233", #Seattle
                   "72546014933", #Des Moines
                   "72211012842", #Tampa
                   "72582524121", #Elko
                   "72295023174", #Los Angeles
                   "72428014821", #Columbus
                   "72466093037") #Colorado Springs
                  
get_daily_data <- function(station, years){
  daily <- list()
  for (i in 1:length(years)){

      daily[[i]] <- lcd(station, year = years[i]) %>%
        mutate(date = gsub("T", " ", date),
               date = as.POSIXct(date),
               year = lubridate::year(date),
               day = lubridate::day(date),
               month = lubridate::month(date)) %>%
        group_by(month, day) %>%
        summarize(station = first(station),
                  date = first(date),
                  latitude = first(latitude),
                  longitude = first(longitude),
                  elevation = first(elevation),
                  name = first(name),
                  dailyminimumdrybulbtemperature = min(as.numeric(hourlydrybulbtemperature), na.rm = TRUE),
                  dailymaximumdrybulbtemperature = max(as.numeric(hourlydrybulbtemperature), na.rm = TRUE),
                  dailyaveragewindspeed = mean(as.numeric(hourlywindspeed), na.rm = TRUE),
                  year = first(year))
   # }
  }
  
  daily_all <- do.call("rbind", daily) %>%
    select(station:name, "Min_temp" = dailyminimumdrybulbtemperature,
           "Max_temp" = dailymaximumdrybulbtemperature,
           "Avg_Wind" = dailyaveragewindspeed,
           year)
  
  return(daily_all)
}

daily <- Map(get_daily_data, temp_stations, ppt_years)

#Plot temp data by year
for (i in 1:length(daily)){
  par(mfrow = c(3, 1))
  ymax <- max(daily[[i]]$Max_temp)
  ymin <- min(daily[[i]]$Min_temp)
  years <- ppt_years[[i]]
  for (j in 1:length(years)){
    data <- filter(daily[[i]], year == years[j])
    plot(Max_temp ~ date, data, ylab = "Temp [F]", main = paste(data$name[1], "-", data$year[1]), type = "p", ylim = c(ymin, ymax),
         pch = 16, col = "red")
    points(Min_temp ~ date, data, pch = 16, col = "blue")
  }
}

#Plot wind speed
for (i in 1:length(daily)){
  par(mfrow = c(3, 1))
  ymax <- max(daily[[i]]$Avg_Wind)
  years <- ppt_years[[i]]
  for (j in 1:length(years)){
    data <- filter(daily[[i]], year == years[j])
    plot(Avg_Wind ~ date, data, ylab = "Temp [F]", main = paste(data$name[1], "-", data$year[1]), type = "p", ylim = c(0, ymax),
         pch = 16, col = "green")
  }
}


save(daily, file = "~Data/Daily station Temp.Rdata")
