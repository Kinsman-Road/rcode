#Library
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(purrr)
library(tidyverse)


#Import Dataset
d.eb <- data.frame(read_excel("trafficdata.xlsx", sheet = eb))
d.wb <- data.frame(read_excel("trafficdata.xlsx", sheet = wb))

d.mean <- d.eb[c(9:11)]     #For mean aggregates
d.day <- d.eb[c(2:8)]      #For each individual day

w.mean <- d.wb[c()]  
w.day <- d.wb[c()]  

dm.m <- d.mean %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

dm.d <- d.day %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

d.m <- data.frame(hour = rep(0:23, each = 1), rbind(dm.m))
d.d <- data.frame(hour = rep(0:23, each = 1), rbind(dm.d))

d.m
d.d


#Generate Plots
p.m <- ggplot(d.m, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Aggregate Mean Frequency") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.d <- ggplot(d.d, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Daily Frequency") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.m
p.d