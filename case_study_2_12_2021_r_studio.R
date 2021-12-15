#==========================================================================
# STEP 1: Installation of required packages & change of directory for ease
#==========================================================================
```{r}
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("geosphere")
install.packages("gridExtra")
install.packages("ggmap")
install.packages("scales")
install.packages("janitor")
install.packages("rmarkdown")
```


library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(geosphere) #This package implements functions that compute various aspects of distance, direction, area, etc.for geographic (geodetic) coordinates.
library(gridExtra) #Provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables.
library(ggmap) #
library(scales) #use of the scales package is to customise to control the appearance of axis and legend labels
library(janitor) #The janitor package is a R package that has simple functions for examining and cleaning dirty data. It was built with beginning and intermediate R users in mind and is optimised for user-friendliness.
library(rmarkdown)
library(CGPfunctions)


getwd() #displays your working directory
setwd("C:/Users/Darko/Desktop/coursera/Cyclistic_Case_Study/divvy_tripdata_original") #sets your working directory to simplify calls to data
getwd()



#==========================================================================
# STEP 2: Import data into R
#==========================================================================
# read_csv() imports data from .csv files


nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")



#====================================================
# STEP 3: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)
colnames(jun_2021)
colnames(jul_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)



##
##Combine 12 data frames into one (1) data frame
##

bike_rides <- rbind(nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021,oct_2021)




#
dim(bike_rides)# Let's check the size of a dataframe
#


bike_rides <- janitor::remove_empty(bike_rides, which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides, which = c("rows"))


dim(bike_rides)


str(bike_rides) #Determine the data types of a data frame's columns

#==========================================================================
# Check unique output values generated
#==========================================================================
table(bike_rides$rideable_type)
table(bike_rides$member_casual)

## Convert Data/Time stamp to Date/Time
bike_rides$Ymd <- as.Date(bike_rides$started_at)
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$ride_length <- difftime(bike_rides$ended_at, bike_rides$started_at)
#==========================================================================
# Analysis
#==========================================================================
bike_rides %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length))
##
bike_rides_v2 <- bike_rides[!(bike_rides$ride_length<0),]
bike_rides_v2 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length))
glimpse(bike_rides_v2)
write.csv(bike_rides_v2,file="bike_rides_member_casual_no_rides.csv", row.names = FALSE)
##### Total member rides
```{r}
glimpse(bike_rides)
trips <- bike_rides %>% select(member_casual) %>% group_by(member_casual) %>% count()
trips <- rename(trips, no_of_rides = n)
glimpse(trips)
```
###Average ride duration  weekdays with plot
all_trips_v2 <- bike_rides[!(bike_rides$ride_length<0),]
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title= "Average Ride Duration",
                                       y="Seconds",x="Weekday")
##No of rides
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = comma) + labs(title= "Number of rides by user type",
                                      y="Number of rides",x="Weekday")
##Top 20 stations Casual
bike_rides %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual != 'member') %>%
  arrange(-number_of_ride) %>%
  head(n=20) %>%
  select(-member_casual)
##Top 20 stations Members
bike_rides %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual != 'casual') %>%
  arrange(-number_of_ride) %>%
  head(n=20) %>%
  select(-member_casual)

## Export

write.csv(bike_rides,file="bike_rides_tableau.csv", row.names = FALSE)


