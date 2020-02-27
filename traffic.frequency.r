#Library
library(read_xl)
library(ggplot2)
library(hrbrthemes)
library(viridis)


#Import Dataset
data <- data.frame(read_excel("trafficdata.xlsx"))

d.mean <- data[c(9:11)]     #For mean aggregates
d.day <- data[c(2:8)]      #For each individual day

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
