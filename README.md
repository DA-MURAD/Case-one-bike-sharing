# Case-one-bike-sharing
 capstone case study of google data analytics using R
---
title: "Cyclistic bike-share v1"
author: "Murad Farhan"
date: '2022-06-08'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction 

In this case study, I will perform data analysis for a fictional bike-share called Cyclistic.In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and
returned to any other station in the system anytime.Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. The director of marketing and my manage that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers,The director of marketing believes there is a very good chance to convert casual riders into members.


## Scenario

I work as a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago.The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, my team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members.


## Phase One: Ask
* Identifying The Kesy stakeholders 
  * Primary stakeholder - the director of marketing (my manager)
  * Secondary stakeholders - the marketing analytics team and the executive team
  
* Business Task 
  * identify how casual riders and annual members use Cyclistic bikes differently ?

* Deliverable
  * Figures to understant the differences in bike usage among casual and member users in     order to recommend how to encourage casual riders to become members.

## Phase Two: Prepare 
* I will be using the public dataset located [here](https://divvy-tripdata.s3.amazonaws.com/index.html).The data has been made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement) 
This is public data that you can use to explore how different customer types are
using Cyclistic bikes. But note that data-privacy issues prohibit you from using riders’ personally identifiable information. This means that you won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the Cyclistic service area or if they have purchased multiple single passes.

* The dataset follows the ROCCC Analysis as described below:

  * Reliable - yes, not biased

  * Original - yes, can locate the original public data

  * Comprehensive - yes, not missing important information

  * Current - yes, updated monthly

  * Cited - yes

SO to prepare the data I have downloaded the last 12 months (May 2021 - Apr 2022) which is the required datasets to answer our questions. we save it appropriate in a file and I renamed them to be easly to read and uderstant. I used this (Month_Year_Name of the dateset) file converting name style. 


## Phase Three: Process
I tried processing this data in Excel but accordin to the Excel limitation of data processing, I use Rstudio Desktop  for storage reasons, Rstudio Desktop was a better fit.

# 3.1 Reading The Pckages into R 

```{r}
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(dplyr)
library(tibble)
```
# 3.2 Uploading Divvy datasets (csv files) here

Previous 12 months (from May, 2021 to Apr, 2022) of Cyclistic trip

```{r}
May_2021_divvy_tripdata <- read_csv("data/May 2021-divvy-tripdata.csv")
Jun_2021_divvy_tripdata <- read_csv("data/Jun 2021-divvy-tripdata.csv")
Jul_2021_divvy_tripdata <- read_csv("data/Jul 2021-divvy-tripdata.csv")
Aug_2021_divvy_tripdata <- read_csv("data/Aug 2021-divvy-tripdata.csv")
sep_2021_divvy_tripdata <- read_csv("data/Sep 2021-divvy-tripdata.csv")
Oct_2021_divvy_tripdata <- read_csv("data/Oct 2021-divvy-tripdata.csv")
Nov_2021_divvy_tripdata <- read_csv("data/Nov 2021-divvy-tripdata.csv")
Dec_2021_divvy_tripdata <- read_csv("data/Dec 2021-divvy-tripdata.csv")
Jan_2022_divvy_tripdata <- read_csv("data/Jan 2022-divvy-tripdata.csv")
Feb_2022_divvy_tripdata <- read_csv("data/Feb 2022-divvy-tripdata.csv")
Mar_2022_divvy_tripdata <- read_csv("data/Mar 2022-divvy-tripdata.csv")
Apr_2022_divvy_tripdata <- read_csv("data/Apr 2022-divvy-tripdata.csv")
```

# 3.3 WRANGLE DATA AND COMBINE INTO A SINGLE FILE
* Display each column name and check for consistency

in order to combine the data sets into one single dataframe it's prefered to 
display each column name , type and check for consistency


```{r}
str(May_2021_divvy_tripdata)
str(Jun_2021_divvy_tripdata)
str(Jul_2021_divvy_tripdata)
str(Aug_2021_divvy_tripdata)
str(sep_2021_divvy_tripdata)
str(Oct_2021_divvy_tripdata)
str(Nov_2021_divvy_tripdata)
str(Dec_2021_divvy_tripdata)
str(Jan_2022_divvy_tripdata)
str(Feb_2022_divvy_tripdata)
str(Mar_2022_divvy_tripdata)
```


all data sets having the same data types for each column

* combine data frames into one big data frame called (Cyclistic-trips)

```{r}
Cyclistic_trips <- rbind(May_2021_divvy_tripdata,Jun_2021_divvy_tripdata,                                 Jul_2021_divvy_tripdata,Aug_2021_divvy_tripdata,
                         sep_2021_divvy_tripdata,Oct_2021_divvy_tripdata,
                         Nov_2021_divvy_tripdata,Dec_2021_divvy_tripdata,
                         Jan_2022_divvy_tripdata,Feb_2022_divvy_tripdata,
                         Mar_2022_divvy_tripdata)
dim(Cyclistic_trips) # to check how many rows and column 
```



# 3.4 remove empty records and unneeded cloumn 
```{r}
Cyclistic_trips <- janitor:: remove_empty(Cyclistic_trips, which = c("cols"))
Cyclistic_trips <- janitor:: remove_empty(Cyclistic_trips, which = c("rows"))
Cyclistic_trips <- Cyclistic_trips %>%  select(-c(start_lat, start_lng, end_lat, end_lng))
dim(Cyclistic_trips) # to see if the column removed
```


# 3.5  adding new coloumn

* I added three coloumn (Ymd refers to the date , Start_hour for the started at hour and ended_hour refers to the end time of the trips by hour )

* adding a column to measure the duration of trips (ended_at - started_at)

```{r}
Cyclistic_trips$Ymd <- as.Date(Cyclistic_trips$started_at)
Cyclistic_trips$Wday <- lubridate::wday(Cyclistic_trips$Ymd,label = TRUE)
Cyclistic_trips$day <- lubridate::wday(Cyclistic_trips$Ymd,label = FALSE)
Cyclistic_trips$month <- lubridate::month(Cyclistic_trips$Ymd,label = TRUE)
Cyclistic_trips$year <- lubridate::year(Cyclistic_trips$Ymd)
Cyclistic_trips$start_hour <- lubridate::hour(Cyclistic_trips$started_at)
Cyclistic_trips$ended_hour <- lubridate::hour(Cyclistic_trips$ended_at)
Cyclistic_trips$trips_duration <- difftime(Cyclistic_trips$ended_at,Cyclistic_trips$started_at)
Cyclistic_trips$trips_duration <- as.numeric(as.character(Cyclistic_trips$trips_duration))
glimpse(Cyclistic_trips)##
```


#3.6 creating a clean dataframe to start analysis 

because of the value in trips_duration is in seconds We want to concentrate on trip duration that are greater than zero. We will therefore, create a new dataset specifying trip durations (trips_duration) greater than 0

```{r}
Cyclistic_trips_v1<- Cyclistic_trips[!(Cyclistic_trips$trips_duration <= 0),]
```

## Phase four: Analyizing 

* conduct descriptive analysis on trips_duration (all figures in minutes)
```{r}
#We can get mean, median, max and minimum ride_length by using the summary() function as below:
  
summary(Cyclistic_trips_v1$trips_duration)
```

* for further statistics comparison of trips_duration  grouping by the type of rider i.e( member or casual)

```{r}
aggregate(Cyclistic_trips_v1$trips_duration ~ Cyclistic_trips_v1$member_casual, FUN = mean)
aggregate(Cyclistic_trips_v1$trips_duration ~ Cyclistic_trips_v1$member_casual, FUN = median)
aggregate(Cyclistic_trips_v1$trips_duration ~ Cyclistic_trips_v1$member_casual, FUN = max)
aggregate(Cyclistic_trips_v1$trips_duration ~ Cyclistic_trips_v1$member_casual, FUN = min)
```


* We can further analyze the data by finding out the average ride length for each type of rider (casual or member) by the day of the week.

```{r}
Cyclistic_trips_v1$Wday <- lubridate::wday(Cyclistic_trips_v1$Ymd,label = TRUE)
#average ride time by each day for members vs casual
aggregate(Cyclistic_trips_v1$trips_duration ~ Cyclistic_trips_v1$member_casual + Cyclistic_trips_v1$Wday, FUN = mean)
```


* We can also summarize the trips_duration data by type of rider and weekday while at the same time calculate the number of rides and the average duration

```{r}
Cyclistic_trips_v1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using   wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()  #calculates the number of rides  
  ,average_duration = mean(trips_duration)) %>% # calculates the average duration
  arrange(member_casual, weekday)  # sorts          
  
```

*  Comparing general bike type preference between members and casual riders


```{r}
Cyclistic_trips_v1 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')
```


* for more analysis to define which top 10 stations for each rider type (casual or memeber ) we will separate the data frame by rider type and combine the start and end stations


```{r}
# Combine start and end stations
# Removing entries with no station name
# Separate the data frame by rider type
all_stations <- bind_rows(data.frame("stations" = Cyclistic_trips_v1$start_station_name, 
                                     "member_casual" = Cyclistic_trips_v1$member_casual),
                          data.frame("stations" = Cyclistic_trips_v1$end_station_name,
                                     "member_casual" = Cyclistic_trips_v1$member_casual))
all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]
# Get the top 10 popular stations all, members, and casual riders
top_10_station <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)
top_10_station_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)
top_10_station_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count))
```


## Phase four: Visualizing 

```{r}
Cyclistic_trips_v1%>% 
  group_by(member_casual, Wday) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = Wday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders")
```


```{r}
Cyclistic_trips_v1 %>% 
  group_by(member_casual, Wday) %>% 
  summarise(average_duration = mean(trips_duration), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = Wday, y = average_duration, fill = member_casual)) +
  geom_col(position = "nudge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders")
```


```{r}
Cyclistic_trips_v1 %>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders")
```


```{r}
Cyclistic_trips_v1 %>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'member') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: member Riders")
```


```{r}
ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = '#56B4E9') +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal()
```



```{r}
options(repr.plot.width = 14, repr.plot.height = 10)
Cyclistic_trips_v1 %>% 
  group_by(month, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Month", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Month") +
  theme(axis.text.x = element_text(angle = 90))
```

## Phase five:sharing the figures with the stakeholders

* Casual riders ride more during the weekend.
* Both causal and members prefer classic bikes while docked bikes not being used by casual.
* Casual riders who rides for a long duration.
* most trips rides in summer times ( july - August ) for both casual and member

## recomendations 

*  Offering incentives or offers to the most rides members and announcing them by placing banners in the locations of the most frequently used stations by casual riders to attract them 

* Offering discounts on holidays or the most frequently used July-August months is (summer offers)
