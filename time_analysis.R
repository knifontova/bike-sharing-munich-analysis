library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)
library(kableExtra)

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



#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))



#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))


#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))


#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))


#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))


#factors for month abbreviations
trip_counts$Month = month.abb[trip_counts$Month]
trip_counts$Month <- factor(trip_counts$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#factor for pandemic months
trip_counts = filter(trip_counts, Year %in% 2019:2021)
trip_counts$Year <- factor(trip_counts$Year, levels = c(2019:2021))

#Analysis for specific months, comparison
ggplot(data=trip_counts, aes(x=Month, y=counts, group=Year, fill = Year)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(title = "Trip counts with MVG over years and months",
       y = "Trip Count per year", x = "Month") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold"))


#charts, times of days, years and trip distances
ggplot(data=data_bytimes_years, aes(x=Year, y=distan, group=group_times, fill = group_times)) + 
  geom_bar(position="stack", stat='identity') +
  labs(title = "Median trip distances with MVG over years and times of days",
       y = "Distance [km]", fill = "Time of the day") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold")) + scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("brown","#D66900", "#E8B03F", "#FCED0A", "#FFFFED",
                               "black","blue","#1a84b8","#1aa4b8", "cyan"))


#charts, times of days, years and trip duration
ggplot(data=data_bytimes_years, aes(x=Year, y=dur, group=group_times, fill = group_times)) + 
  geom_bar(position="stack", stat='identity') +
  labs(title = "Median trip durations with MVG over years and times of days",
       y = "Duration [min]", fill = "Time of the day") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        legend.title = element_text(size=24),
        legend.text = element_text(size=20),
        title = element_text(size=27,face="bold")) + scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("brown","#D66900", "#E8B03F", "#FCED0A", "#FFFFED",
                               "black","blue","#1a84b8","#1aa4b8", "cyan"))


#some tables for trips between specific landuse types

#making a string to display
data_every_use$from_to = paste(data_every_use$landuse_start, "-",data_every_use$landuse_end)

travels_year_month = group_by(data_every_use, from_to,Year)

travels_year_month = summarise(travels_year_month,
                               counts = length(Row))

travels_year_month = arrange(travels_year_month, desc(counts))

travels_year_month = filter(travels_year_month, Year %in% 2019:2021)


filter(travels_year_month, Year == 2019) %>%
  kbl(caption = "Trips between specific landuse types in 2019") %>%
  kable_minimal()


filter(travels_year_month, Year == 2020) %>%
  kbl(caption = "Trips between specific landuse types in 2020") %>%
  kable_minimal()


filter(travels_year_month, Year == 2021) %>%
  kbl(caption = "Trips between specific landuse types in 2021") %>%
  kable_minimal()