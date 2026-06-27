#install.packages("tidyverse")
#install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)

files <- list.files(
  path = "C:/Users/HP/Desktop/capstone project",
  pattern = "*.csv",
  full.names = TRUE
)
cyclistic <- bind_rows(lapply(files, read_csv))
dim(cyclistic)
colSums(is.na(cyclistic))
cyclistic$started_at <- as.POSIXct(cyclistic$started_at)
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at)

cyclistic$ride_length_min <-
  as.numeric(
    difftime(
      cyclistic$ended_at,
      cyclistic$started_at,
      units="mins"
    )
  )
summary(cyclistic$ride_length_min)
sum(cyclistic$ride_length_min < 0,na.rm=TRUE)
sum(cyclistic$ride_length_min == 0,na.rm=TRUE)

cyclistic <- cyclistic %>%
  filter(ride_length_min > 0)
dim(cyclistic)
cyclistic$year <- year(cyclistic$started_at)
cyclistic$month <- month(cyclistic$started_at,label=TRUE)
cyclistic$day_of_week <- weekdays(cyclistic$started_at)
cyclistic$day_of_week <- factor(cyclistic$day_of_week,
                                levels = c("Monday",
                                           "Tuesday",
                                           "Wednesday",
                                           "Thursday",
                                           "Friday",
                                           "Saturday",
                                           "Sunday"))
cyclistic$hour <- hour(cyclistic$started_at)
dim(cyclistic)

cyclistic %>%
  count(year,member_casual)

cyclistic <- cyclistic %>%
  filter(year %in% c(2025, 2026))


cyclistic <- read_csv("cyclistic_2025_2026.csv")


dim(cyclistic)
#writing the cleaned file
#write.csv(cyclistic,"cyclistic_2025_2026.csv",row.names=FALSE)
#too large file 1.6 GB

#top ten busiest stations
top_stations <- cyclistic %>%
  filter(!is.na(start_station_name)) %>%
  count(start_station_name,sort = TRUE) %>%
  slice_head(n=10)
write.csv(top_stations,"top_stations.csv", row.names = FALSE)

#top ten busiest station for casuals
top_casual_stations <- cyclistic %>%
  filter(
    member_casual == "casual",
    !is.na(start_station_name)
  ) %>%
  count(start_station_name, sort = TRUE) %>%
  slice_head(n = 10)

write.csv(top_casual_stations,"top_casual_stations.csv",row.names=FALSE)
# top ten busiest casual stations chart

ggplot(top_casual_stations,aes(x=reorder(start_station_name,n),y=n))+
  geom_col()+
  coord_flip() +
  labs(
    title="Top 10 Stations Used by Casual Riders",
       x="Station",
       y="Number of rides"
       )
# top ten busiest stations for members
top_member_stations <- cyclistic %>%
  filter(
    member_casual=="member",
    !is.na(start_station_name)) %>%
  count(start_station_name,sort=TRUE) %>%
  slice_head(n=10)
write.csv(top_member_stations,"top_member_stations.csv",row.names=FALSE)
  
#ride_count
ride_counts <- cyclistic %>%
  count(member_casual)
write.csv(ride_counts, "ride_counts.csv", row.names = FALSE)
#ride_counts bar_graph
ggplot(ride_counts,aes(x=member_casual,y=n,fill=member_casual))+
  geom_col()+
  labs(title="Number of Rides by Rider Type",x="Rider Type",y="Number of Rides")


#Average Ride Length
avg_ride <- cyclistic %>%
  group_by(member_casual) %>%
  summarise(avg_ride=mean(ride_length_min,na.rm=TRUE),.groups = "drop")
write.csv(avg_ride, "avg_ride.csv", row.names = FALSE)
#plot of avg_ride 
ggplot(avg_ride,aes(x=member_casual,y=avg_ride,fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title="Average Ride Length",
       x="Rider Type",
       y="Average Ride in Minutes")

#Rides by Day of Week(Week_day_usage)
weekly_usage <- cyclistic %>%
  count(day_of_week, member_casual)
write.csv(weekly_usage,"weekly_usage.csv",row.names=FALSE)
#plot for Rides By day of week
ggplot(weekly_usage,aes(x=day_of_week,y=n,fill=member_casual))+
  geom_col(position="dodge")+
  labs(title="Ride by Day of Week",x="Day",y="Number of Rides")


#Rideable Type Preference
bike_type <- cyclistic %>%
  count(member_casual, rideable_type)
write.csv(bike_type, "bike_type.csv", row.names = FALSE)
#plot
ggplot(bike_type,aes(x=rideable_type,y=n,fill=member_casual))+
  geom_col(position ="dodge")+
  labs(title="Bike Type Preference",x="Bike Type",y="Number Of Rides")


#Rides by Month(Monthly_usage)
monthly_usage_2025 <- cyclistic %>%
  filter(year==2025) %>%
  count(month,member_casual)
write.csv(monthly_usage_2025,"monthly_usage_2025.csv",row.names=FALSE)
#Monthly Ride Trends
ggplot(monthly_usage_2025,aes(x=month,y=n,colour=member_casual,group=member_casual))+
  geom_line()+
  geom_point()+
  labs(title="Monthly Ride Trends_2025",x="Months",y="Number of Rides")

#Hourly Usage
hourly_usage <- cyclistic %>%
  count(hour, member_casual,sort=TRUE)
hourly_usage$hour_label <- factor( 
  sprintf("%02d:00", hourly_usage$hour), 
  levels = sprintf("%02d:00", 0:23), 
  labels = c
  ( "12 AM","1 AM","2 AM","3 AM","4 AM","5 AM", "6 AM","7 AM",
    "8 AM","9 AM","10 AM","11 AM", "12 PM","1 PM","2 PM",
    "3 PM","4 PM","5 PM", "6 PM","7 PM","8 PM","9 PM","10 PM","11 PM" ))

write.csv(hourly_usage ,"hourly_usage.csv",row.names=FALSE)

# Hourly Ride Trends
ggplot(hourly_usage,aes(x=hour_label,y=n,colour=member_casual,group=member_casual))+
  geom_line()+
  geom_point()+
  labs(title="Hourly Ride Usage",x="Time of Day",y="Number of Rides")+
  scale_x_discrete(
    breaks = c(
      "12 AM","2 AM","4 AM","6 AM","8 AM","10 AM",
      "12 PM","2 PM","4 PM","6 PM","8 PM","10 PM"
    ))+
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1
  ))



