#Resources
#https://www.datacamp.com/community/tutorials/pca-analysis-r
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table

#:::::  If Pulling From Drive Files  :::::
#Note: If pulling from Github, you can input the code from the next section: "Import"
install.packages("googledrive")
library(googledrive)
drive.fine(n=30)

drive_download("Pre")
drive_download("Post")

#:::::  Import  :::::
pre <- read_excel("pca.xlsx", sheet="pre")
post <- read_excel("pca.xlsx", sheet="post")


#:::::  Preparing datasets as data frames  :::::
pre <- data.frame(pre)
post <- data.frame(post)

pre.n <- pre[1:7]      #create dataframes with only numerical columns from pre
post.n <- post[1:7]    #create dataframes with only numerical columns from post



#:::::PCA:::::
library(factoextra)
library(FactoMineR)

pca.pre <- prcomp(pre.n, scale = TRUE)   #singular value pca
pca.post <- prcomp(post.n, scale = TRUE)   #singular value pca

pre.eig <- get_eigenvalue(pca.pre)
post.eig <- get_eigenvalue(pca.post)



#:::::PCA Coordinates:::::
pre.vcf <- function(pre.load, comp.sdev){pre.load*comp.sdev}
pre.load <- pre.pca$rotation
pre.sdev <- pre.pca$sdev
pre.vcoord <- t(apply(pre.load, 1, pre.vcf, pre.sdev ))
pre.vc <- head(pre.vcoord[,1:8])

post.vcf <- function(post.load, comp.sdev){post.load*comp.sdev}
post.load <- post.pca$rotation
post.sdev <- post.pca$sdev
post.vcoord <- t(apply(post.load, 1, post.vcf, post.sdev))
post.vc <- head(post.vcoord[,1:8])



#:::::PCA cos2:::::
pre.cos2 <- pre.vcoord^2
post.cos2 <- post.vcoord^2



#:::::PCA Contributions:::::
pre.cc2 <- apply(pre.cos2, 2, sum)
contrib <- function(pre.cos2, comp.cos2){pre.cos2*100/comp.cos2}
pre.varc <- t(apply(pre.cos2, 1, contrib, comp.cos2))
pre.vcontrib <- head(pre.varc[,1:8])

post.cc2 <- apply(post.cos2, 2, sum)
contrib <- function(post.cos2, comp.cos2){post.cos2*100/comp.cos2}
post.varc <- t(apply(post.cos2, 1, contrib, comp.cos2))
post.vcontrib <- head(post.varc[,1:8])



#:::::Creating a scree plot:::::
pre.scree <- fviz_eig(pca.pre)
post.scree <- fviz_eig(pca.post)



#:::::Creating contribution plot for individual observations:::::
pre.ind <- fviz_pca_ind(pca.pre,
                        col.ind = "cos2",   #maybe "contribution?"
                        gradient.cols = c(),   #default R colors
                        repel = TRUE,
                        label = "none",
                        main = "Pre-Construction Individual Plots")

post.ind <- fviz_pca_ind(pca.post,
                        col.ind = "cos2",
                        gradient.cols = c(),   #default R colors
                        repel = TRUE,
                        label = "none",
                        main = "Post-Construction Individual Plots")



#:::::Creating contribution plot for variable contributions:::::
pre.var <- fviz_pca_var(pca.pre,
                        col.var = "co2",  #maybe "contribution?"
                        gradient.cols = c(),   #default R colors
                        repel = TRUE,
                        label = "none",
                        main = "Pre-Construction Variable Contribution")

post.var <- fviz_pca_var(pca.post,
                         col.var = "co2",  #maybe "contribution?"
                         gradient.cols = c(),   #default R colors
                         repel = TRUE,
                         label = "none",
                         main = "Post-Construction Variable Contribution")



#:::::Creating a biplot(combination of ind + var plots):::::
pre.bp <- fviz_pca_biplot(pca.pre,
                         col.var = "#E7B800",
                         col.var = "#FC4E07",
                         label = "none",
                         main = "Pre-Construction Biplot")
                       

post.bp <- fviz_pca_biplot(pca.post,
                           col.var = "#E7B800",
                           col.var = "#FC4E07",
                           label = "none",
                           main = "Post-Construction Biplot")
                 
                           
                                     
#:::::Creating an individual PCA plot with ellipses for categories:::::
#(1) First define categories as factors

#--(1a) Pre categories
pre.g.species <- as.factor(pre$species[1:470])
pre.g.solar <- as.factor(pre$solar[1:470])
pre.g.cat <- as.factor(pre$category[1:470])
pre.g.cam <- as.factor(pre$camera[1:470])
pre.g.traffic <- as.factor(pre$traffic[1:470])
pre.g.dnc <- as.factor(pre$dnc[1:470])

#--(1b) Post categories
post.g.species <- as.factor(post$species[1:655])
post.g.solar <- as.factor(post$solar[1:655])
post.g.cat <- as.factor(post$category[1:655])
post.g.cam <- as.factor(post$camera[1:655])
post.g.traffic <- as.factor(post$traffic[1:655])
post.g.dnc <- as.factor(post$dnc[1:655])

#(2) Produce ellipses PCA graphs for every factor   

#--(2a) Pre-Construction Ellipses PCA categories
pre.species <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Pre-Construction: Species Groupings")

pre.solar <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.solar,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Pre-Construction: Daylight Preference")

pre.cat <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cat,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Pre-Construction: Mammalian Groupings")

pre.cam <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cam,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Pre-Construction: Camera Preference")

pre.traffic <- fviz_pca_ind(pca.pre,
                           col.ind = pre.g.traffic,
                           palette = c( ),
                           addEllipses = TRUE,
                           ellipse.type = "confidence",
                           legend.title = "Groups",
                           repel = TRUE,
                           label = "none",
                           main = "Pre-Construction: SUMMER Traffic Preference")

pre.dnc <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.dnc,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Pre-Construction: D/N/C Category")

#--(2b) Post-Construction Ellipses PCA categories
post.species <- fviz_pca_ind(pca.post,
                            col.ind = post.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Post-Construction: Species Groupings")

post.solar <- fviz_pca_ind(pca.post,
                          col.ind = post.g.solar,
                          palette = c( ),
                          addEllipses = TRUE,
                          ellipse.type = "confidence",
                          legend.title = "Groups",
                          repel = TRUE,
                          label = "none",
                          main = "Post-Construction: Daylight preference")

post.cat <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cat,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE,
                        label = "none",
                        main = "Post-Construction: Mammalian Groupings")

post.cam <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cam,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE,
                        label = "none",
                        main = "Post-Construction: Camera Preference")

post.traffic <- fviz_pca_ind(pca.post,
                            col.ind = post.g.traffic,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            main = "Post-Construction: SUMMER Traffic Preference")

post.dnc <- fviz_pca_ind(pca.post,
                             col.ind = post.g.dnc,
                             palette = c( ),
                             addEllipses = TRUE,
                             ellipse.type = "confidence",
                             legend.title = "Groups",
                             repel = TRUE,
                             label = "none",
                             main = "Post-Construction: D/N/C Category")

