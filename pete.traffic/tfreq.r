#Initialization
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(purrr)
library(tidyverse)


#::: If needed, load fonts from extrafont into windows. This takes ~5 minutes
#library(extrafont)
#font_import()
#loadfonts(device = "win")


#Import Dataset
d.e <- data.frame(read_excel("pete.traffic/tdata.xlsx", sheet = "tot"))
d.deer <- data.frame(read_excel("pete.traffic/tdata.xlsx", sheet = "deer"))

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

# Use Factor() to change the order
d.m$text <- factor(d.m$text, levels = c("Weekday","Weekend","Overall"))
d.d$text <- factor(d.d$text, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


# Generate Plots
par(mfrow=c(2,4))

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

p.deer <- ggplot(d.deer, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Deer Frequency for Post-Construction Kinsman") +
  facet_wrap(~text) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

p.mean
p.day
p.deer

#Overlap
d.deer <- d.e[10]

m.deer <- d.deer %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

df.tot <- data.frame(hour = rep(0:23, each = 1), rbind(m.deer))

p.tot <- ggplot(df.tot, aes(fill=as.factor(hour), y=value, x=as.factor(hour))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Mean Weekly Frequency Against Deer") +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("Hour") +
  ylab("Count")

p.tot
