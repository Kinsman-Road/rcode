#::: Links :::
#
#


#::: Todo :::  
# Goal is to create regression for hours of prediction for every single species
# Regression for prediction for species dimensions given traffic,e tc
# Use PCA for variable selection as well as for dimensionality reduction
# R2 + AIC + Mcp

#::: Pulling Drive Files :::
install.packages("googledrive")
library(googledrive)
drive.fine(n=30)

drive_download("Pre")
drive_download("Post")

pre <- read_excel("Pre.xlsx")
post <- read_excel("Post.xlsx")


#::: Preparing datasets as data frames :::
pre <- data.frame(pre)
post <- data.frame(post)

pre.n <- scale(pre[1:7])      #create dataframes with only numerical columns from pre. Be sure to choose [i:a]
post.n <- scale(post[1:7])    #create dataframes with only numerical columns from post. Be sure to choose [i:a]


#::: Initial Regression :::
#::: Pre + Post to compare potential changes in behavior :::
#Regression to predict what hour key species will appear based off of numerical variables
#Numerical Variables - Count, Boeckman Traffic Data

#Deer
par(mrow=c(5,2))
deer.count <- lm([hour] ~ individuals)
deer.count <- lm([hour] ~ solar)
deer.count <- lm([hour] ~ tmin)
deer.count <- lm([hour] ~ tmax)
deer.count <- lm([hour] ~ pcip)
deer.count <- lm([hour] ~ traffic)
deer.count <- lm([hour] ~ weather)
deer.count <- lm([hour] ~ sunlight)
deer.count <- lm([hour] ~ wind)
deer.count <- lm([hour] ~ [individuals:wind)

#Raccoon
par(mrow=c(5,2))
rac.count <- lm([hour] ~ individuals)
rac.count <- lm([hour] ~ solar)
rac.count <- lm([hour] ~ tmin)
rac.count <- lm([hour] ~ tmax)
rac.count <- lm([hour] ~ pcip)
rac.count <- lm([hour] ~ traffic)
rac.count <- lm([hour] ~ weather)
rac.count <- lm([hour] ~ sunlight)
rac.count <- lm([hour] ~ wind)
rac.count <- lm([hour] ~ [individuals:wind)

#Coyote
par(mrow=c(5,2))
coy.count <- lm([hour] ~ individuals)
coy.count <- lm([hour] ~ solar)
coy.count <- lm([hour] ~ tmin)
coy.count <- lm([hour] ~ tmax)
coy.count <- lm([hour] ~ pcip)
coy.count <- lm([hour] ~ traffic)
coy.count <- lm([hour] ~ weather)
coy.count <- lm([hour] ~ sunlight)
coy.count <- lm([hour] ~ wind)
coy.count <- lm([hour] ~ [individuals:wind)

#Rabbit
par(mrow=c(5,2))
rab.count <- lm([hour] ~ individuals)
rab.count <- lm([hour] ~ solar)
rab.count <- lm([hour] ~ tmin)
rab.count <- lm([hour] ~ tmax)
rab.count <- lm([hour] ~ pcip)
rab.count <- lm([hour] ~ traffic)
rab.count <- lm([hour] ~ weather)
rab.count <- lm([hour] ~ sunlight)
rab.count <- lm([hour] ~ wind)
rab.count <- lm([hour] ~ [individuals:wind)

