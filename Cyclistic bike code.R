## Install required packages

library("tidyverse")            # Helps with data wrangling
library("lubridate")            # Helps wrangle date functions and attributes
library("ggplot2")              # Helps visualize data

getwd()                         # gets working directory
setwd("C:/Users/Admin/Desktop/Data Analytics Projects/Google Capstone Project/Cyclistic Bike/Cyclistic Bike Dataset/Working dataset")

## Upload csv files containing the dataset

m01_2022<-read_csv("202201-divvy-tripdata.csv")
m02_2022<-read_csv("202202-divvy-tripdata.csv")
m03_2022<-read_csv("202203-divvy-tripdata.csv")
m04_2022<-read_csv("202204-divvy-tripdata.csv")
m05_2022<-read_csv("202205-divvy-tripdata.csv")
m06_2022<-read_csv("202206-divvy-tripdata.csv")

# Comparing column names to make sure they are consistent
# Column names were found to be consistent
colnames(m01_2022)
colnames(m02_2022)
colnames(m03_2022)
colnames(m04_2022)
colnames(m05_2022)
colnames(m06_2022)


# Renaming columns as the guide lines require us to 
(m01_2022<-rename(m01_2022
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,from_station_name = start_station_name
                  ,from_station_id = start_station_id
                  ,to_station_name = end_station_name
                  ,to_station_id = end_station_id
                  ,user_type = member_casual))

(m02_2022<-rename(m02_2022
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,from_station_name = start_station_name
                  ,from_station_id = start_station_id
                  ,to_station_name = end_station_name
                  ,to_station_id = end_station_id
                  ,user_type = member_casual))

(m03_2022<-rename(m03_2022
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,from_station_name = start_station_name
                  ,from_station_id = start_station_id
                  ,to_station_name = end_station_name
                  ,to_station_id = end_station_id
                  ,user_type = member_casual))

(m04_2022<-rename(m04_2022
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,from_station_name = start_station_name
                  ,from_station_id = start_station_id
                  ,to_station_name = end_station_name
                  ,to_station_id = end_station_id
                  ,user_type = member_casual))

(m05_2022<-rename(m05_2022
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,from_station_name = start_station_name
                  ,from_station_id = start_station_id
                  ,to_station_name = end_station_name
                  ,to_station_id = end_station_id
                  ,user_type = member_casual))

(m06_2022 <- rename(m06_2022
                    ,trip_id = ride_id
                    ,bikeid = rideable_type
                    ,start_time = started_at
                    ,end_time = ended_at
                    ,from_station_name = start_station_name
                    ,from_station_id = start_station_id
                    ,to_station_name = end_station_name
                    ,to_station_id = end_station_id
                    ,user_type = member_casual))

## Inspecting dataframes for incongruencies
# No incongruencies were found at this point
str(m01_2022)
str(m02_2022)
str(m03_2022)
str(m04_2022)
str(m05_2022)
str(m06_2022)


# Stacking individual dataframes into one dataframe
all_trips <- bind_rows(m01_2022,m02_2022,m03_2022,m04_2022,m05_2022,m06_2022)

# Removing columns not necessary for the analysis
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng,))

View(all_trips)

# Inspecting the new table created

colnames(all_trips)  # list column names
nrow(all_trips)      # How many rows are in dataframe
dim(all_trips)       # Dimensions of the dataframe
head(all_trips)      # Allows to see the first 6 rows of dataframe
str(all_trips)       # See list of colums and data types
summary(all_trips)   # Statistical summary of data

table(all_trips$bikeid)

## Adding columns that list the date, month, day, and year of each ride

all_trips$date <- as.Date(all_trips$start_time, "%m/%d/%y")
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Adding ride_length_sec(ride length in seconds) to all trips
#Changing format of 'start_time' and 'end_time' to POS

all_trips$start_time <- as_datetime(all_trips$start_time, format = "%m/%d/%Y %H:%M")
all_trips$end_time <- as_datetime(all_trips$end_time, format = "%m/%d/%Y %H:%M")


all_trips$ride_length_sec <- as.difftime(all_trips$end_time - all_trips$start_time, units = "secs" )


str(all_trips)


all_trips = na.omit(all_trips)

all_trips_v2 <- all_trips[!(all_trips$ride_length_sec<=0),]


View(all_trips_v2)


# Descriptive Analysis

mean(all_trips_v2$ride_length_sec)
median(all_trips_v2$ride_length_sec)
max(all_trips_v2$ride_length_sec)
min(all_trips_v2$ride_length_sec)

all_trips_v2 %>%
  summarize(mean=mean(ride_length_sec), median=median(ride_length_sec),
            max=max(ride_length_sec), min=min(ride_length_sec))
  
aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type,FUN = mean)
aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type,FUN = median)
aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type,FUN = max)
aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type,FUN = min)

aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type + all_trips_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday

all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%   #creates weekday field using wday()
  group_by(user_type, weekday) %>%                       #groups by usertype and weekday
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
            arrange(user_type, weekday)	
  
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge")

all_trips_v2 %>%
  group_by(user_type, bikeid) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
  arrange(user_type, bikeid)  %>% 
  ggplot(aes(x = bikeid, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge")









#Exporting summary file for further analysis
counts <- aggregate(all_trips_v2$ride_length_sec~all_trips_v2$user_type
                    + all_trips_v2$day_of_week, FUN = mean)

write.csv(counts, 'C:\\Users\\Admin\\Desktop\\Data Analytics Projects\\Marked.csv')

View(counts)
  
  
  