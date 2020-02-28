#Library
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(purrr)
library(tidyverse)


#Import Dataset
d.e <- data.frame(read_excel("trafficdata.xlsx"))

d.mean <- d.e[c(8:10)]     #For mean aggregates
d.day <- d.e[c(1:7)]       #For each individual day

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


#Generate Plots
p.mean <- ggplot(d.m, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Aggregate Mean Frequency for Boeckman Traffic") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.day <- ggplot(d.d, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Daily Frequency for Boeckman Traffic") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.mean
p.day

#Overlap
d.tot <- data.frame(read_excel("trafficdata.xlsx", sheet = "tot"))

m.tot <- d.tot %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

df.tot <- data.frame(hour = rep(0:23, each = 1), rbind(d.tot))

p.tot <- ggplot(df.tot, aes(fill=as.factor(hour), y=tot, x=as.factor(hour))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Mean Weekday Frequency of Eastbound and Westbound Traffic") +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("Hour") +
  ylab("Count")

p.tot
