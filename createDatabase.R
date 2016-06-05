library(dplyr)
library(tidyr)
library(lunar)
library(lubridate)
library(rgbif)

trips <- read.csv("2015_trip_data.csv", sep = ",")
stations <- read.csv("2015_station_data.csv", sep = ",")
weather <- read.csv("2015_weather_data.csv", sep = ",")

trips$starttime <- as.POSIXct(trips$starttime, format = "%m/%d/%Y %H:%M")
trips$stoptime <- as.POSIXct(trips$stoptime, format = "%m/%d/%Y %H:%M")

# Converts station and weather date related info to POSIXct
stations$online <- as.POSIXct(stations$online, format = "%m/%d/%Y")
weather$Date <- as.POSIXct(weather$Date, format = "%m/%d/%Y")

# Adds a column of days of the week and a column of Weekend or Weekday
trips$days <- weekdays(trips$starttime)

for (i in 1:length(trips$days)) {
        if (trips$days[i] == "Saturday" | trips$days[i] == "Sunday") {
                trips$dayType[i] = "Weekend"
        } else  trips$dayType[i] = "Weekday"
}

### What is the effect of the phase of the moon on number of trips taken?
trips$Moon <- lunar.phase(trips$starttime, name = TRUE)

# Adds elevation data to stations
apikey <- getOption("elevationAPI")
elevation <- stations
names(elevation) <- c("id", "name", "terminal", "decimalLatitude", "decimalLongitude", 
                      "dockcount", "opened")
elevation <- elevation(elevation, elevation$decimalLatitude, elevation$decimalLongitude, key = apikey)
stations$elevation <- elevation$elevation

stations$region <- sub("^([[:alpha:]]*).*", "\\1", stations$terminal)

# Match up from and to stations with lat, long and elevation in main dataframe

database <- trips %>% 
            left_join(stations, by = c("from_station_id" = "terminal")) %>% 
            select(trip_id, starttime, stoptime, bikeid, tripduration, from_station_name, 
                to_station_name, from_station_id, to_station_id, usertype, gender, birthyear, 
                days, dayType, Moon, lat_from = lat, long_from = long, elevation_from = elevation, region_from = region) %>%
            left_join(stations, by = c("to_station_id" = "terminal")) %>% 
            select(trip_id, starttime, stoptime, bikeid, tripduration, from_station_name, 
                to_station_name, from_station_id, to_station_id, usertype, gender, birthyear, 
                days, dayType, Moon, lat_from, long_from, elevation_from, region_from,
                lat_to = lat, long_to = long, elevation_to = elevation, region_to = region) %>% 
            mutate(elevation_diff = elevation_to - elevation_from)

weather_small <- weather %>% mutate(day = round_date(Date, unit = "day")) %>%
                select(day, Max_Temperature_F, Mean_Temperature_F, 
                                    Min_TemperatureF, Max_Wind_Speed_MPH, Mean_Wind_Speed_MPH, 
                                    Max_Gust_Speed_MPH, Precipitation_In, Events) 

database <- database %>% mutate(Date = round_date(starttime, unit = "day")) %>% left_join(weather_small, by = c("Date" = "day"))

saveRDS(database, "data.rds")
        