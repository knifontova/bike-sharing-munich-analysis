library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)
library(kableExtra)

#loading the data
#outside the boundaries of Munich


#setting working directory
setwd("C:/Users/marsi/Desktop/Szkoła/Munchen I/Big Data")

#Munich city extent
munich = st_read("districts_munich.shp")

#Grid load
grid = st_read("./Grid/GRID_REALLY_FINAL_VERSION.shp")

#temperature data
temperature = read.table("./hourly_data/temperature.txt",sep=";",header=T)
temperature$MESS_DATUM = ymd_h(temperature$MESS_DATUM)

#precipitation
precipitation = read.table("./hourly_data/precipitation.txt",sep=";",header=T)
precipitation$MESS_DATUM = ymd_h(precipitation$MESS_DATUM)

#wind
wind = read.table("./hourly_data/wind.txt",sep=";",header=T)
wind$MESS_DATUM = ymd_h(wind$MESS_DATUM)

#loading the data
data_2016 = read.csv('MVG_Rad_Fahrten_2016.csv', TRUE, sep = ";", dec = "," )
data_2017 = read.csv('MVG_Rad_Fahrten_2017.csv', TRUE, sep = ";", dec = "," )
data_2018 = read.csv('MVG_Rad_Fahrten_2018.csv', TRUE, sep = ";", dec = "," )
data_2019 = read.csv('MVG_Rad_Fahrten_2019.csv', TRUE, sep = ";", dec = "," )
data_2020 = read.csv('MVG_Rad_Fahrten_2020.csv', TRUE, sep = ";", dec = "," )
data_2021 = read.csv('MVG_Rad_Fahrten_2021.csv', TRUE, sep = ";", dec = "," )
data_2022 = read.csv('MVG_Rad_Fahrten_2022.csv', TRUE, sep = ";", dec = "," )

data_segmentation = function(data, grid, munich, temperature, precipitation, wind, desired_months){
  
  #spatial aspect. filtering by the start point within Munich
  data1 = st_as_sf(data, coords = c("STARTLON","STARTLAT"), crs = 4326)
  data1 = st_filter(data1, munich)
  
  #adding landuse info to start points
  landuse = select(grid,c(2,3,6))
  data1 = st_join(data1, landuse, join = st_intersects)
  colnames(data1)[colnames(data1) == 'type_1'] <- 'landuse_start'
  colnames(data1)[colnames(data1) == 'buildPOP'] <- 'buildPOP_start'
  
  #spatial aspect. filtering by the end point within Munich
  data2 = st_as_sf(data, coords = c("ENDLON","ENDLAT"), crs = 4326)
  data2 = st_filter(data2,munich)
  
  #adding landuse info to end points
  data2 = st_join(data2, landuse, join = st_intersects)
  colnames(data2)[colnames(data2) == 'type_1'] <- 'landuse_end'
  colnames(data2)[colnames(data2) == 'buildPOP'] <- 'buildPOP_end'
  
  
  #filtering off values and some tricks to futher process the data
  data = filter(data, Row %in% data1$Row)
  data = filter(data, Row %in% data2$Row)
  
  
  #adding landuse info to the final dataset
  data = inner_join(data, select(data1,c(1,10,11)), by = "Row")
  data = inner_join(data, select(data2,c(1,10,11)), by = "Row")
  
  data = select(data, !c(14,17))
  
  #duration of a trip
  data$Duration_minutes = as.numeric((ymd_hm(data$ENDTIME) - ymd_hm(data$STARTTIME))/60)
  
  
  
  
  #Duration filtering. Only trips with non-negative duration and up to 1 day are taken into account
  data = dplyr::filter(data, Duration_minutes > 0 & Duration_minutes <= 1440)
  
  #Some trips start and end exactly in the same coordinates. Cleaning these rows
  data = dplyr::filter(data, (STARTLON != ENDLON) & (STARTLAT != ENDLAT))
  
  
  #start and end date - It's just for day date without hour, you can activate it if you wish
  data$Date = as.Date(ymd_hm(data$STARTTIME))
  
  #time selection - I think it's unecessary, but leave it in case you want to use
  data$StartTime = format(ymd_hm(data$STARTTIME),"%H:%M")
  data$EndTime = format(ymd_hm(data$ENDTIME),"%H:%M")
  
  #month day, day of the month
  data$month_day = day(data$Date)
  
  #month selection
  data$Month = month(data$Date)
  
  #filter by desired months
  data = filter(data,Month %in% desired_months)
  
  #year info
  data$Year = year(data$Date)
  
  #week starts from Monday, week days will be numbers from 1 to 7
  data$Week_Day = wday(data$Date, FALSE, FALSE, 1)
  
  #travel start hour
  
  data$hour = hour(hm(data$StartTime))
  
  
  #weekday morning rush
  data$if_week_morning_rush = 1
  data$if_week_morning_rush [data$Week_Day == 6 | data$Week_Day == 7] = 0
  data$if_week_morning_rush [data$hour < 6 | data$hour > 9] = 0
  
  #weekday afternoon rush
  data$if_week_afternoon_rush = 1
  data$if_week_afternoon_rush [data$Week_Day == 6 | data$Week_Day == 7] = 0
  data$if_week_afternoon_rush [data$hour < 16 | data$hour > 18] = 0
  
  #weekday midday
  data$if_week_midday = 1
  data$if_week_midday [data$Week_Day == 6 | data$Week_Day == 7] = 0
  data$if_week_midday [data$hour <= 9 | data$hour >= 16] = 0
  
  #weekday evening
  data$if_week_evening = 1
  data$if_week_evening [data$Week_Day == 6 | data$Week_Day == 7] = 0
  data$if_week_evening [data$hour <= 18 | data$hour >= 22] = 0
  
  #weekday night
  data$if_week_night = 1
  data$if_week_night [data$Week_Day == 6 | data$Week_Day == 7] = 0
  data$if_week_night [data$hour %in% c(6:21)] = 0
  
  
  
  
  
  
  #weekend morning rush
  data$if_weekend_morning_rush = 0
  data$if_weekend_morning_rush [data$Week_Day == 6 | data$Week_Day == 7] = 1
  data$if_weekend_morning_rush [data$hour < 6 | data$hour > 9] = 0
  
  #weekend afternoon rush
  data$if_weekend_afternoon_rush = 0
  data$if_weekend_afternoon_rush [data$Week_Day == 6 | data$Week_Day == 7] = 1
  data$if_weekend_afternoon_rush [data$hour < 16 | data$hour > 18] = 0
  
  #weekend midday
  data$if_weekend_midday = 0
  data$if_weekend_midday [data$Week_Day == 6 | data$Week_Day == 7] = 1
  data$if_weekend_midday [data$hour <= 9 | data$hour >= 16] = 0
  
  #weekend evening
  data$if_weekend_evening = 0
  data$if_weekend_evening [data$Week_Day == 6 | data$Week_Day == 7] = 1
  data$if_weekend_evening [data$hour <= 18 | data$hour >= 22] = 0
  
  #weekend night
  data$if_weekend_night = 0
  data$if_weekend_night [data$Week_Day == 6 | data$Week_Day == 7] = 1
  data$if_weekend_night [data$hour %in% c(6:21)] = 0
  
  
  
  
  #hard lockdowns first
  
  lockdown = interval(ymd('2020-03-22'),ymd('2020-05-04'))
  
  data$first_lockdown = 0
  data$first_lockdown [data$Date %within% lockdown] = 1
  
  data$before_lockdown = 0
  data$before_lockdown [data$Date < ymd('2020-03-22')] = 1
  
  data$after_lockdown = 0
  data$after_lockdown [data$Date > ymd('2020-05-04')] = 1
  
  
  #temperature
  temperature$Date = as.Date(ymd_hms(temperature$MESS_DATUM))
  temperature = filter(temperature, year(MESS_DATUM) %in% data$Year)
  temperature$hour = hour(temperature$MESS_DATUM)
  
  #renaming columns
  colnames(temperature)[colnames(temperature) == 'TT_TU'] <- 'Air_Temperature'
  
  #formatting the table
  temperature = select(temperature, Date, hour, Air_Temperature)
  
  #if temperature data is extremely weird, then my own idea
  #if temperature is lower than -30 or higher than 45
  #(its mostly -999 then)
  #then, you find the next reliable temperature and set the -999 as it
  for(i in c(1:length(temperature$Air_Temperature))){
    if (temperature$Air_Temperature[i] < -30 | temperature$Air_Temperature[i] > 45){
      j = i
      
      controler = TRUE
      while(controler == TRUE){
        if(temperature$Air_Temperature[j] < -30 | temperature$Air_Temperature[j] > 45){
          j = j+1
        }else{
          temperature$Air_Temperature[i] = temperature$Air_Temperature[j]
          
          controler = FALSE
        }
      }
    }
  }
  
  
  #joining the data - temperature
  data = left_join(data, temperature, by = c("Date","hour"))
  
  
  #precipitation data cleaning
  precipitation$Date = as.Date(ymd_hms(precipitation$MESS_DATUM))
  precipitation = filter(precipitation, year(MESS_DATUM) %in% data$Year)
  precipitation$hour = hour(precipitation$MESS_DATUM)
  
  #renaming columns
  colnames(precipitation)[colnames(precipitation) == 'R1'] <- 'Precipitation_mm'
  
  #formatting the table
  precipitation = select(precipitation, Date, hour, Precipitation_mm)
  
  for(i in c(1:length(precipitation$Precipitation_mm))){
    if (precipitation$Precipitation_mm[i] < 0){
      j = i
      
      controler = TRUE
      while(controler == TRUE){
        if(precipitation$Precipitation_mm[j] < 0){
          j = j+1
        }else{
          precipitation$Precipitation_mm[i] = precipitation$Precipitation_mm[j]
          
          controler = FALSE
        }
      }
    }
  }
  
  #joining the data - precipitation
  data = left_join(data, precipitation, by = c("Date","hour"))
  
  #wind data
  wind$Date = as.Date(ymd_hms(wind$MESS_DATUM))
  wind = filter(wind, year(MESS_DATUM) %in% data$Year)
  wind$hour = hour(wind$MESS_DATUM)
  
  #renaming columns
  colnames(wind)[colnames(wind) == 'F'] <- 'Wind_velocity_ms'
  
  #formatting the table
  wind = select(wind, Date, hour, Wind_velocity_ms)
  
  #wind data is extremely weird, then my own idea
  #wind velocity is lower than -30 or higher than 45
  #(its mostly -999 then)
  #then, you find the next reliable temperature and set the -999 as it
  for(i in c(1:length(wind$Wind_velocity_ms))){
    if (wind$Wind_velocity_ms[i] < 0){
      j = i
      
      controler = TRUE
      while(controler == TRUE){
        if(wind$Wind_velocity_ms[j] < 0){
          j = j+1
        }else{
          wind$Wind_velocity_ms[i] = wind$Wind_velocity_ms[j]
          
          controler = FALSE
        }
      }
    }
  }
  
  #joining the data - wind
  data = left_join(data, wind, by = c("Date","hour"))
  
  
  return(data)
}


#months in numbers (like 1 for january etc)
#Input data cropped to Munich extent and with grid cells ids!
results_2016 = data_segmentation(data_2016, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2016, "data_2016.csv")

results_2017 = data_segmentation(data_2017, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2017, "data_2017.csv")

results_2018 = data_segmentation(data_2018, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2018, "data_2018.csv")

results_2019 = data_segmentation(data_2019, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2019, "data_2019.csv")

results_2020 = data_segmentation(data_2020, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2020, "data_2020.csv")

results_2021 = data_segmentation(data_2021, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2021, "data_2021.csv")

results_2022 = data_segmentation(data_2022, grid,munich, temperature, precipitation, wind,c(1:12))
#write.csv(results_2022, "data_2022.csv")

data_every_alot = rbind(results_2016,results_2017,results_2018,
                        results_2019,results_2020,results_2021,results_2022)


#data every selection of useful columns
data_every_use = select(data_every_alot, c(1,4:7,12:17,21,22,24:40))

#adding distances. Those had to be calculated separately in Python
#took way too much time in R
distances = read.csv('data_everything_wDist.csv', TRUE, sep = ";", dec = ".")


#that is the final dataset
data_every_use = cbind(data_every_use, select(distances,12))

#land use to 0 and 1

###RESIDENTIAL
unique(data_every_use$landuse_start)

data_every_use$residential_start = 0
data_every_use$residential_start[data_every_use$landuse_start == 'residential'] = 1

data_every_use$residential_end = 0
data_every_use$residential_end[data_every_use$landuse_end == 'residential'] = 1

###mixed-use
data_every_use$mixed_start = 0
data_every_use$mixed_start[data_every_use$landuse_start == 'mixed-use'] = 1

data_every_use$mixed_end = 0
data_every_use$mixed_end[data_every_use$landuse_end == 'mixed-use'] = 1

###recreational
data_every_use$recreational_start = 0
data_every_use$recreational_start[data_every_use$landuse_start == 'recreational'] = 1

data_every_use$recreational_end = 0
data_every_use$recreational_end[data_every_use$landuse_end == 'recreational'] = 1

###university
data_every_use$university_start = 0
data_every_use$university_start[data_every_use$landuse_start == 'university'] = 1

data_every_use$university_end = 0
data_every_use$university_end[data_every_use$landuse_end == 'university'] = 1

###commercial
data_every_use$commercial_start = 0
data_every_use$commercial_start[data_every_use$landuse_start == 'commercial'] = 1

data_every_use$commercial_end = 0
data_every_use$commercial_end[data_every_use$landuse_end == 'commercial'] = 1

###industrial
data_every_use$industrial_start = 0
data_every_use$industrial_start[data_every_use$landuse_start == 'industrial'] = 1

data_every_use$industrial_end = 0
data_every_use$industrial_end[data_every_use$landuse_end == 'industrial'] = 1

###farmland
data_every_use$farmland_start = 0
data_every_use$farmland_start[data_every_use$landuse_start == 'farmland'] = 1

data_every_use$farmland_end = 0
data_every_use$farmland_end[data_every_use$landuse_end == 'farmland'] = 1

#final export data

data_every_use_export = select(data_every_use, -c(6,8))