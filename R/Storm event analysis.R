#Separate precipitation series into events
library(dplyr)
library(zoo)
load(file = "../Data/Hourly Station Precip.Rdata") #hourly precipitation data in inches

event_separation <- function(data, city){
  #Get IDF Curve for city
  IDF <- read.csv(paste0("../Data/", city, "_IDF.csv"))
  
  #Interpolate to get all durations
  IDF_all <- as.data.frame(apply(IDF[,2:ncol(IDF)], 2, function(d, h){
    h_pred <- 1:240
    d_pred <- approx(h, d, xout = h_pred)$y
    
    return(d_pred)
  }, h = IDF[,1])) %>%
    mutate(Dur = 1:240) %>%
    tidyr::gather("RI", "Depth", -Dur) %>%
    mutate(RI = as.numeric(substr(RI, 2, nchar(RI))))
  
  #Do by year
  years <- unique(data$year)
  event_summary <- list()
  events_all <- list()
  for (i in 1:length(years)){
    events <- data %>%
      filter(year == years[i]) %>%
      filter(value > 0) %>% #remove trace and missing data
      mutate(diff = c(7, diff(date)))
    
    event_starts <- which(events$diff >= 6)
    event_num <- 1:length(event_starts)
    
    events$event <- NA
    events$event[event_starts] <- event_num
    events$event <- na.locf(events$event)
    
    event_summary[[i]] <- group_by(events, event) %>%
      summarize(vol = sum(value),
              dur = difftime(max(date), min(date), units = "hours") + 1,
              avg_int = vol / as.numeric(dur),
              max_int = max(vol),
              start = min(date),
              end = max(date),
              delta = max(diff)) %>% #delta is hours since previous storm
      mutate(year = lubridate::year(start),
             RI = NA)
    event_summary[[i]]$delta[1] <- NA #Set first event delta to NA since it is the first event (i.e. there is no preceeding event)
    
    #Add return periods
    for (j in 1:nrow(event_summary[[i]])){
      idf <- filter(IDF_all, Dur == event_summary[[i]]$dur[j])
      event_summary[[i]]$RI[j] <- approx(idf$Depth, idf$RI, xout = event_summary[[i]]$vol[j])$y
    }
    
    events_all[[i]] <- events
  }
  
  event_summary_all <- do.call("rbind", event_summary)
  events_all <- do.call("rbind", events_all)
  
  return(list(event_summary_all, events_all))
  
}

