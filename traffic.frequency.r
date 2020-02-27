#Library
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(purrr)
library(tidyverse)


#Import Dataset
d.eb <- data.frame(read_excel("trafficdata.xlsx", sheet = "eb"))
d.wb <- data.frame(read_excel("trafficdata.xlsx", sheet = "wb"))

d.mean.e <- d.eb[c(9:11)]     #For mean aggregates
d.day.e <- d.eb[c(2:8)]       #For each individual day

d.mean.w <- d.wb[c(7)]     #For mean aggregates
d.day.w <- d.wb[c(2:6)]    #For each individual day

dm.m.e <- d.mean.e %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

dm.d.e <- d.day.e %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

dm.m.w <- d.mean.w %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

dm.d.w <- d.day.w %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

d.m.e <- data.frame(hour = rep(0:23, each = 1), rbind(dm.m.e))
d.d.e <- data.frame(hour = rep(0:23, each = 1), rbind(dm.d.e))
d.m.w <- data.frame(hour = rep(0:23, each = 1), rbind(dm.m.w))
d.d.w <- data.frame(hour = rep(0:23, each = 1), rbind(dm.d.w))


#Generate Plots
p.m.e <- ggplot(d.m.e, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Aggregate Mean Frequency For Eastbound Traffic") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.d.e <- ggplot(d.d.e, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Daily Frequency for Eastbound Traffic") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.m.w <- ggplot(d.m.w, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Aggregate Mean Frequency for Westbound Traffic") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.d.w <- ggplot(d.d.w, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Daily Frequency for Westbound") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")


p.m.e
p.d.e
p.m.w
p.d.w


#Overlap
d.tot <- data.frame(read_excel("trafficdata.xlsx", sheet = "tot"))

p.tot <- ggplot(d.tot, aes(fill=as.factor(h), y=wb, x=as.factor(h))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Mean Weekday Frequency of Eastbound and Westbound Traffic") +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.tot
