#loading the libraries
library(tidyverse)
library(dplyr)
library(readr)
ds<-read.csv("ds_dropped.csv", header = T) 

#freq of rideable type according to member and casual riders
ggplot(ds)+geom_bar(mapping=aes(x=rideable_type, fill=member_casual))+
  facet_wrap(~member_casual)

#freq of day of week according to member and casual with rideable bike types
ds$day_of_week<-factor(ds$day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday","Friday","Saturday","Sunday"))
ggplot(ds)+geom_bar(mapping=aes(x=day_of_week, fill=rideable_type))+
  facet_wrap(~member_casual)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#duration vs distance line graph for member and casual
ggplot(filter(ds, ds$duration_in_min<100 ))+
  geom_histogram(mapping=aes(x=duration_in_min))+
  facet_wrap(~member_casual)

#distance traveled in meters
ggplot(filter(ds, ds$distance_traveled<10000 ))+
  geom_density(mapping=aes(x=distance_traveled))+
  facet_wrap(~member_casual)


#filtering only members to a new dataset
filtered_member<-filter(ds, member_casual=="member")

#summary for member ds after dropping NA values, we can observe mean, median
summary(drop_na(select(filtered_member, c('day_of_week', 'distance_traveled', 'duration_in_min'))))

#sorts the dataset according to most popular start and end station names
head(count(filtered_member, start_station_name, sort=T), n=10)
head(count(filtered_member, end_station_name, sort=T), n=10)


#concat start stn name with end stn name and observe the top 5 routes
filtered_member$routes<-paste(filtered_member$start_station_name,"---",filtered_member$end_station_name)
head(count(filtered_member, routes, sort=T), n=10)



#reapeating same above fnc to casual ds aswell

filtered_casual<-filter(ds, member_casual=="casual")
summary(drop_na(select(filtered_casual, c('day_of_week', 'distance_traveled', 'duration_in_min'))))

head(count(filtered_casual, start_station_name, sort=T), n=10)
head(count(filtered_casual, end_station_name, sort=T), n=10)


filtered_casual$routes<-paste(filtered_casual$start_station_name,"---",filtered_casual$end_station_name)
head(count(filtered_casual, routes, sort=T), n=10)


library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
df<-data.frame('User_type'=c("Member", "Casual"),
               "Amount"=c("2,352,923 (57.9%)","1,710,107 (42.1%)"),
               "Avg_and_median_trip_duration"=c("15.26 min - 11.07 min","41.83 min - 20.20 min"),
               "Avg_and_median_trip_distance"=c("2.25 km - 1.69 km", "2.19 km - 1.66 km"), 
               "Busiest_day"=c("Saturday","Saturday"),
               "Preffered_bike_type"=c("docked bike","docked bike"),
               "Most_occured_route"=c("Ellis Ave & 60th St-Ellis Ave & 55th St (1,409)",
                                      "Streeter Dr & Grand Ave-Streeter Dr & Grand Ave (8,230)"))
formattable(df, 
            align =c("l","c","c","c","c", "c", "r"), 
            list("User_type" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")) ))





#analyzing  missing_data values which represents NA values
missing_data<-drop_na(filter(ds, start_station_name=="missing_data"))
summary(select(missing_data, c('day_of_week', 'distance_traveled', 'duration_in_min')))


head(count(missing_data, member_casual, rideable_type, sort=T), n=10)
dim(filter(ds, rideable_type=='electric_bike'))


