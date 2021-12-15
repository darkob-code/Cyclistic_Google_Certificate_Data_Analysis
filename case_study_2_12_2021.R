#==========================================================================
# STEP 1: Installation of required packages & change of directory for ease
#==========================================================================
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library("geosphere") #This package implements functions that compute various aspects of distance, direction, area, etc.
for geographic (geodetic) coordinates.
library("gridExtra") #Provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables.
library("ggmap") #
library("scales") #use of the scales package is to customise to control the appearance of axis and legend labels
library("janitor") #The janitor package is a R package that has simple functions for examining and cleaning dirty data. It was built with beginning and intermediate R users in mind and is optimised for user-friendliness.
getwd() #displays your working directory
setwd("C:/Users/Darko/Desktop/coursera/Cyclistic_Case_Study/divvy_tripdata_original") #sets your working directory to simplify calls to data
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
#
dim(bike_rides)
str(bike_rides) #Determine the data types of a data frame's columns
## Convert Data/Time stamp to Date/Time
bike_rides$start_time <- lubridate::hms(bike_rides$started_at)
bike_rides$end_time <- lubridate::hms(bike_rides$ended_at)

## Create hour field
bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

## Create date field
bike_rides$start_date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

glimpse(bike_rides)
head(bike_rides, n=10)
ls(bike_rides)

# Visualization
bike_rides %>% count(start_hour, sort = T) %>%
   ggplot() + geom_line(aes(x=start_hour, y=n)) + 
   scale_y_continuous(labels = comma) +
   labs(title = "count of Bike Ride by Hour: Previous 12 Months", x= "Start Hour of Rides", y="Count of Rides")


# TEst
##### Rides by day hour
```{r}

member_rides_by_hour <- bike_rides %>% 
  filter(member_casual == "member") %>% 
  select(start_hour) %>% 
  group_by(start_hour) %>% 
  count()
member_rides_by_hour <- rename(member_rides_by_hour, no_of_member_rides = n)
glimpse(member_rides_by_hour)
casual_rides_by_hour <- bike_rides %>% 
  filter(member_casual == "casual") %>% 
  select(start_hour) %>% 
  group_by(start_hour) %>% 
  count()
casual_rides_by_hour <- rename(casual_rides_by_hour, no_of_casual_rides = n)
glimpse(casual_rides_by_hour)
rides_by_hour <- left_join(member_rides_by_hour, casual_rides_by_hour, by = "start_hour")
rides_by_hour <- rides_by_hour %>% arrange(start_hour)
rides_by_hour <- mutate(rides_by_hour, start_hour = as_string(start_hour))
glimpse(rides_by_hour)


##

bike_rides %>% count(start_station_name, sort = TRUE) %>%
  top_n(20) %>%  ggplot() + geom_col(aes(x=start_station_name, y=n) +
                                       coord_flip() + labs(title= "Top 20 Start Stations by Ride Count",
                                                           y="Station Name",x="Count of Rides")
                                     
##
bike_rides %>%
group_by(start_station_name, member_casual) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual != 'member') %>%
  arrange(-number_of_ride) %>%
  head(n=20) %>%
  select(-member_casual)
                                                                 
                                     
                                     
## Export
write.csv(rides_by_hour,file="rides_by_hour.csv", row.names = FALSE)
```


