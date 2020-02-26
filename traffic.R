library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)

#file location and name:
filename = "/home/courtney/R/STAT 570/Classification Data_Boeckman Road_2010.xlsx"

#load raw data:
traffic_raw = read_excel(filename)

#pivot the data so that the totals for westbound and eastbound traffic are in separate columns:
traffic = dcast(traffic_raw, Datetime ~ Direction, fun.aggregate = sum, value.var = 'Total')

#add columns to sum the east and westbound traffic totals:
traffic$total = traffic$Eastbound + traffic$Westbound

#add column for day of week:
traffic$day_of_week = wday(traffic$Datetime, label = TRUE)

# add column for day of year:  
traffic$day = yday(traffic$Datetime)

# add column for hour of day: 
traffic$hour = hour(traffic$Datetime)

# add column to differentiate weekends from weekdays:  
traffic$day_type = ifelse((traffic$day_of_week == "Sun") |(traffic$day_of_week == "Sat"), "Weekend", "Weekday")


###PLOTS###

#vehicles per hour over course of 1-week study
circ_hist_daily = ggplot(traffic, aes(x = hour, y = total)) +
  coord_polar(theta = "x", start = -.13) +
  geom_bar(stat = "summary", fun.y = "sum", fill = "maroon4", width = .9) +
  geom_hline(yintercept = seq(0, 80, by = 10), color = "grey80", size = 0.3) +
  scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
  labs(x = "Hour", y = "Number of Vehicles", title = "Average Number of Vehicles per 15 minutes") +
  theme_bw() +
  facet_wrap(day_of_week ~.)
circ_hist_daily

#plot hourly averages separately by day_type (weekend vs. weekday)
circ_hist_day_type = ggplot(traffic, aes(x = hour, y = total)) +
  coord_polar(theta = "x", start = -.13) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "maroon4", width = .9) +
  geom_hline(yintercept = seq(0, 80, by = 10), color = "grey80", size = 0.3) +
  scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
  labs(x = "Hour", y = "Number of Vehicles", title = "Average Number of Vehicles per 15 minutes") +
  theme_bw() +
  facet_wrap(day_type ~.)
circ_hist_day_type

#regular histogram; plot average traffic by day_type (weekend vs. weekday)
hist_day_type = ggplot(traffic) +
  geom_bar(mapping = aes(x = hour, y = total), stat = "summary", fun.y = "mean") +
  facet_wrap(day_type ~.) + 
  geom_hline(yintercept = 15, color = "red") + # 1 vehicle per minute on average
  geom_hline(yintercept = 7.5, color = "blue") + # 1 vehicle every 2 minutes on average
  geom_hline(yintercept = 3, color = "green") + # 1 vehicle every 5 minutes on average
  labs(x = "Hour", y = "Number of Vehicles", title = "Average Number of Vehicles per 15 minutes") 
hist_day_type

