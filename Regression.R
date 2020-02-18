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

