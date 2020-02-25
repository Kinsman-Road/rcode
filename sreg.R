#::: Links :::
#
#


#::: Todo :::  
# Goal is to create regression for hours of prediction for every single species
# Regression for prediction for species dimensions given traffic,e tc
#
#



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
par(mrow=c(4,2))
deer.count <- lm([timeday] ~ count)
deer.count <- lm([timeday] ~ solar)
deer.count <- lm([timeday] ~ camera)
deer.count <- lm([timeday] ~ tmin)
deer.count <- lm([timeday] ~ tmax)
deer.count <- lm([timeday] ~ pcip)
deer.count <- lm([timeday] ~ traffic)
deer.count <- lm([timeday] ~ count+solar+camera+tmin+tmax+pcip+traffic)

#Raccoon
par(mrow=c(4,2))
deer.count <- lm([timeday] ~ count)
deer.count <- lm([timeday] ~ solar)
deer.count <- lm([timeday] ~ camera)
deer.count <- lm([timeday] ~ tmin)
deer.count <- lm([timeday] ~ tmax)
deer.count <- lm([timeday] ~ pcip)
deer.count <- lm([timeday] ~ traffic)
deer.count <- lm([timeday] ~ count+solar+camera+tmin+tmax+pcip+traffic)

#Coyote
par(mrow=c(4,2))
deer.count <- lm([timeday] ~ count)
deer.count <- lm([timeday] ~ solar)
deer.count <- lm([timeday] ~ camera)
deer.count <- lm([timeday] ~ tmin)
deer.count <- lm([timeday] ~ tmax)
deer.count <- lm([timeday] ~ pcip)
deer.count <- lm([timeday] ~ traffic)
deer.count <- lm([timeday] ~ count+solar+camera+tmin+tmax+pcip+traffic)

#Rabbit
par(mrow=c(4,2))
deer.count <- lm([timeday] ~ count)
deer.count <- lm([timeday] ~ solar)
deer.count <- lm([timeday] ~ camera)
deer.count <- lm([timeday] ~ tmin)
deer.count <- lm([timeday] ~ tmax)
deer.count <- lm([timeday] ~ pcip)
deer.count <- lm([timeday] ~ traffic)
deer.count <- lm([timeday] ~ count+solar+camera+tmin+tmax+pcip+traffic)

