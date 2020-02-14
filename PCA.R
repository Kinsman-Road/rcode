#Pulling Drive Files
install.packages("googledrive")
library(googledrive)
drive.fine(n=30)

drive_download("Pre")
drive_download("Post")

pre <- read_excel("Pre.xlsx")
post <- read_excel("Post.xlsx")



#Preparing datasets as data frames
pre <- data.frame(pre)
post <- data.frame(post)

pre.n <- pre[1:7]      #create numerical only dataframes
post.n <- post[1:7]    #create numerical only dataframes



#PCA
library(factoextra)
library(FactoMineR)

pca.pre <- prcomp(pre.n, scale = TRUE)   #singular value pca
pca.post <- prcomp(post.n, scale = TRUE)   #singular value pca



#Creating a scree plot
pre.scree <- fviz_eig(pca.pre)
post.scree <- fviz_eig(pca.post)



#Creating contribution plot for individual observations
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



#Creating contribution plot for variable contributions
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



#Creating a biplot(combination of ind + var plots)
pre.bp <- fviz_pca_biplot(pca.pre,
                         col.var = "#E7B800",
                         col.var = "#FC4E07",
                         label = "none",
                         main = "Pre-Construction Biplot"
                       

post.bp <- fviz_pca_biplot(pca.post,
                           col.var = "#E7B800",
                           col.var = "#FC4E07",
                           label = "none",
                           main = "Post-Construction Biplot"
                 
                           
                                     
#Creating an individual PCA plot with ellipses for categories
#(1) First define categories as factors

#--(1a) Pre categories
pre.g.species <- as.factor(pre$species[1:470])
pre.g.solar <- as.factor(pre$solar[1:470])
pre.g.cat <- as.factor(pre$cat[1:470])
pre.g.cam <- as.factor(pre$cam[1:470])
pre.g.daynight <- as.factor(pre$daynight[1:470])
pre.g.season <- as.factor(pre$season[1:470])

#--(1b) Post categories
post.g.species <- as.factor(post$species[1:655])
post.g.solar <- as.factor(post$solar[1:655])
post.g.cat <- as.factor(post$cat[1:655])
post.g.cam <- as.factor(post$cam[1:655])
post.g.daynight <- as.factor(post$daynight[1:655])
post.g.season <- as.factor(post$season[1:655])

#(2) Produce ellipses PCA graphs for every factor   

#--(2a) Pre-Construction Ellipses PCA categories
pre.species <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.species,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Species Groupings")

pre.solar <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.solar,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Daylight Preference")

pre.cat <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cat,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Mammalian Groupings")

pre.cam <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cam,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Camera Preference")

pre.daynight <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.daynight,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Day/Night Preference")

pre.season <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.season,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "Pre-Construction: Seasonal Preference")

#--(2b) Post-Construction Ellipses PCA categories
post.species <- fviz_pca_ind(pca.post,
                            col.ind = post.g.species,
                            palette = c( ),
                            addEllipses = TRUE
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE
                            label = "none",
                            main = "post-Construction: Species Groupings")

post.solar <- fviz_pca_ind(pca.post,
                          col.ind = post.g.solar,
                          palette = c( ),
                          addEllipses = TRUE
                          ellipse.type = "confidence",
                          legend.title = "Groups",
                          repel = TRUE
                          label = "none",
                          main = "post-Construction: Daylight postference")

post.cat <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cat,
                        palette = c( ),
                        addEllipses = TRUE
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE
                        label = "none",
                        main = "post-Construction: Mammalian Groupings")

post.cam <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cam,
                        palette = c( ),
                        addEllipses = TRUE
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE
                        label = "none",
                        main = "post-Construction: Camera postference")

post.daynight <- fviz_pca_ind(pca.post,
                             col.ind = post.g.daynight,
                             palette = c( ),
                             addEllipses = TRUE
                             ellipse.type = "confidence",
                             legend.title = "Groups",
                             repel = TRUE
                             label = "none",
                             main = "post-Construction: Day/Night postference")

post.season <- fviz_pca_ind(pca.post,
                           col.ind = post.g.season,
                           palette = c( ),
                           addEllipses = TRUE
                           ellipse.type = "confidence",
                           legend.title = "Groups",
                           repel = TRUE
                           label = "none",
                           main = "post-Construction: Seasonal postference")





























