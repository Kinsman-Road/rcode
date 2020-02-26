#::: Links :::
#
#


#::: Notes :::  
# Goal is to create regression for count prediction per hour, per species
# Key Species: Deer, Raccoons, Coyotes, Rabbit
# Primary Objection: Simple Linear Regression
# Separate evalution for pre and post
# Use PCA used for variable selection
# R2 + AIC + Mcp for model evalulation
# Potential to use PCR for model and kernel PCR for clustering

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

pre.n <- scale(pre[1:7])      #create dataframes with only numerical columns from pre. Be sure to choose [i:a]. Includes traffic
post.n <- scale(post[1:7])    #create dataframes with only numerical columns from post. Be sure to choose [i:a]. Includes traffic


#::: Initial Regression :::
#::: Pre + Post to compare potential changes in behavior :::
#Regression to predict what hour key species will appear based off of numerical variables
#Numerical Variables - Count, Boeckman Traffic Data

#:::::  Deer  :::::

#Pre-Construction
pre.deer.month <- lm(count ~ month, data=subset(pre,species="Deer"))
pre.deer.hour <- lm(count ~ hour, data=subset(pre,species="Deer"))
pre.deer.pcip <- lm(count ~ pcip, data=subset(pre,species="Deer"))
pre.deer.tmax <- lm(count ~ tmax, data=subset(pre,species="Deer"))
pre.deer.tmin <- lm(count ~ tmin, data=subset(pre,species="Deer"))
pre.deer.traffic <- lm(count ~ traffic, data=subset(pre,species="Deer"))
pre.deer.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Deer"))

#Post-Construction
post.deer.month <- lm(count ~ month, data=subset(pre,species="Deer"))
post.deer.hour <- lm(count ~ hour, data=subset(pre,species="Deer"))
post.deer.pcip <- lm(count ~ pcip, data=subset(pre,species="Deer"))
post.deer.tmax <- lm(count ~ tmax, data=subset(pre,species="Deer"))
post.deer.tmin <- lm(count ~ tmin, data=subset(pre,species="Deer"))
post.deer.traffic <- lm(count ~ traffic, data=subset(pre,species="Deer"))
post.deer.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Deer"))

par(mrow=c(5,2))
plot(pre.deer.lm)
plot(post.deer.lm)



#:::::  Raccoon  :::::

#Pre-Construction
pre.racc.month <- lm(count ~ month, data=subset(pre,species="Raccoon"))
pre.racc.hour <- lm(count ~ hour, data=subset(pre,species="Raccoon"))
pre.racc.pcip <- lm(count ~ pcip, data=subset(pre,species="Raccoon"))
pre.racc.tmax <- lm(count ~ tmax, data=subset(pre,species="Raccoon"))
pre.racc.tmin <- lm(count ~ tmin, data=subset(pre,species="Raccoon"))
pre.racc.traffic <- lm(count ~ traffic, data=subset(pre,species="Raccoon"))
pre.racc.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Raccoon"))

#Post-Construction
post.racc.month <- lm(count ~ month, data=subset(pre,species="Raccoon"))
post.racc.hour <- lm(count ~ hour, data=subset(pre,species="Raccoon"))
post.racc.pcip <- lm(count ~ pcip, data=subset(pre,species="Raccoon"))
post.racc.tmax <- lm(count ~ tmax, data=subset(pre,species="Raccoon"))
post.racc.tmin <- lm(count ~ tmin, data=subset(pre,species="Raccoon"))
post.racc.traffic <- lm(count ~ traffic, data=subset(pre,species="Raccoon"))
post.racc.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Raccoon"))

par(mrow=c(5,2))
plot(pre.racc.lm)
plot(post.racc.lm)



#:::::  Coyote  :::::

#Pre-Construction
pre.coyo.month <- lm(count ~ month, data=subset(pre,species="Coyote"))
pre.coyo.hour <- lm(count ~ hour, data=subset(pre,species="Coyote"))
pre.coyo.pcip <- lm(count ~ pcip, data=subset(pre,species="Coyote"))
pre.coyo.tmax <- lm(count ~ tmax, data=subset(pre,species="Coyote"))
pre.coyo.tmin <- lm(count ~ tmin, data=subset(pre,species="Coyote"))
pre.coyo.traffic <- lm(count ~ traffic, data=subset(pre,species="Coyote"))
pre.coyo.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Coyote"))

#Post-Construction
post.coyo.month <- lm(count ~ month, data=subset(pre,species="Coyote"))
post.coyo.hour <- lm(count ~ hour, data=subset(pre,species="Coyote"))
post.coyo.pcip <- lm(count ~ pcip, data=subset(pre,species="Coyote"))
post.coyo.tmax <- lm(count ~ tmax, data=subset(pre,species="Coyote"))
post.coyo.tmin <- lm(count ~ tmin, data=subset(pre,species="Coyote"))
post.coyo.traffic <- lm(count ~ traffic, data=subset(pre,species="Coyote"))
post.coyo.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Coyote"))

par(mrow=c(5,2))
plot(pre.coyo.lm)
plot(post.coyo.lm)



#:::::  Rabbit  :::::

#Pre-Construction
pre.rabb.month <- lm(count ~ month, data=subset(pre,species="Rabbit"))
pre.rabb.hour <- lm(count ~ hour, data=subset(pre,species="Rabbit"))
pre.rabb.pcip <- lm(count ~ pcip, data=subset(pre,species="Rabbit"))
pre.rabb.tmax <- lm(count ~ tmax, data=subset(pre,species="Rabbit"))
pre.rabb.tmin <- lm(count ~ tmin, data=subset(pre,species="Rabbit"))
pre.rabb.traffic <- lm(count ~ traffic, data=subset(pre,species="Rabbit"))
pre.rabb.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Rabbit"))

#Post-Construction
post.rabb.month <- lm(count ~ month, data=subset(pre,species="Rabbit"))
post.rabb.hour <- lm(count ~ hour, data=subset(pre,species="Rabbit"))
post.rabb.pcip <- lm(count ~ pcip, data=subset(pre,species="Rabbit"))
post.rabb.tmax <- lm(count ~ tmax, data=subset(pre,species="Rabbit"))
post.rabb.tmin <- lm(count ~ tmin, data=subset(pre,species="Rabbit"))
post.rabb.traffic <- lm(count ~ traffic, data=subset(pre,species="Rabbit"))
post.rabb.lm <- lm(count ~ month + hour + pcip + tmax + tmin+ traffic, data=subset(pre,species="Rabbit"))

par(mrow=c(5,2))
plot(pre.rabb.lm)
plot(post.rabb.lm)

