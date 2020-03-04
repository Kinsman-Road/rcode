#Resources
#https://www.datacamp.com/community/tutorials/pca-analysis-r
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table



#::::: [CONDENSED PCA RUN WITH SELECT SPEICES] :::::



#:::::  Import  :::::
library(readxl)
pre <- read_excel("PCA/pca.xlsx", sheet = "pca.pre.c")
post <- read_excel("PCA/pca.xlsx", sheet = "pca.post.c")


#:::::  Preparing datasets as data frames  :::::
pre <- data.frame(pre)
post <- data.frame(post)

pre.n <- pre[1:7]      #create dataframes with only numerical columns from pre
post.n <- post[1:7]    #create dataframes with only numerical columns from post



#:::::PCA:::::
library(factoextra)
library(FactoMineR)

pca.pre <- prcomp(pre.n, scale = TRUE)   #singular value pca method - not spectral decomposition
pca.post <- prcomp(post.n, scale = TRUE)   #singular value pca method - not spectral decomposition

pre.eig <- get_eigenvalue(pca.pre)
post.eig <- get_eigenvalue(pca.post)



#::::: PCA Coordinates :::::
#These are what is driving the direction of the plots below

#Pre-Construction PCA Coordinates
pre.vcf <- function(pre.load, comp.sdev){pre.load*comp.sdev}
pre.load <- pca.pre$rotation
pre.sdev <- pca.pre$sdev
pre.vcoord <- t(apply(pre.load, 1, pre.vcf, pre.sdev ))
pre.vc <- head(pre.vcoord[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

#Post-Construction PCA Coordinates
post.vcf <- function(post.load, comp.sdev){post.load*comp.sdev}
post.load <- pca.post$rotation
post.sdev <- pca.post$sdev
post.vcoord <- t(apply(post.load, 1, post.vcf, post.sdev))
post.vc <- head(post.vcoord[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

pre.vc       #table of pre pca coords
post.vc      #table of post pca coords


#:::::PCA cos2:::::
pre.cos2 <- pre.vcoord^2
post.cos2 <- post.vcoord^2

pre.cos2     #table of contribution to each dimension
post.cos2    #table of contribution to each dimension



#:::::PCA Contributions to Each Given Component:::::
pre.cc2 <- apply(pre.cos2, 2, sum)
contrib <- function(pre.cos2, pre.cc2){pre.cos2*100/pre.cc2}
pre.varc <- t(apply(pre.cos2, 1, contrib, pre.cc2))
pre.vcontrib <- head(pre.varc[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

post.cc2 <- apply(post.cos2, 2, sum)
contrib <- function(post.cos2, post.cc2){post.cos2*100/post.cc2}
post.varc <- t(apply(post.cos2, 1, contrib, post.cc2))
post.vcontrib <- head(post.varc[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

pre.vcontrib
post.vcontrib


#:::::Creating a scree plot:::::
pre.scree <- fviz_eig(pca.pre)
post.scree <- fviz_eig(pca.post)

pre.scree
post.scree

#:::::Creating contribution plot for individual observations:::::
pre.ind <- fviz_pca_ind(pca.pre,
                        col.ind = "cos2",   #maybe "contribution?"
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        label = "none",
                        title = "Pre-Construction Individual Plots")

post.ind <- fviz_pca_ind(pca.post,
                        col.ind = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        label = "none",
                        title = "Post-Construction Individual Plots")

pre.ind
post.ind

#:::::Creating contribution plot for variable contributions:::::
pre.var <- fviz_pca_var(pca.pre,
                        col.var = "cos2",  #maybe "contribution?"
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        title = "Pre-Construction Variable Contribution")

post.var <- fviz_pca_var(pca.post,
                         col.var = "cos2",  #maybe "contribution?"
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                         repel = TRUE,
                         title = "Post-Construction Variable Contribution")

pre.var
post.var

#:::::Creating a biplot(combination of ind + var plots):::::
pre.bp <- fviz_pca_biplot(pca.pre,
                         col.ind = "#fa995c",
                         col.var = "#2f2091",
                         label = "var",
                         repel = TRUE,
                         title = "Pre-Construction Biplot") 
#+ geom_text(label=pre$species, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)
                       

post.bp <- fviz_pca_biplot(pca.post,
                           col.ind = "#fa995c",
                           col.var = "#2f2091",
                           label = "var",
                           repel = TRUE,
                           title = "Post-Construction Biplot")
#+ geom_text(label=pre$species, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)

pre.bp
post.bp

                           
                                     
#:::::Creating an individual PCA plot with ellipses for categories:::::
#(1) First define categories as factors

#--(1a) Pre categories
pre.g.species <- as.factor(pre$species[1:351])
pre.g.solar <- as.factor(pre$solar[1:351])
pre.g.cat <- as.factor(pre$category[1:351])
pre.g.cam <- as.factor(pre$camera[1:351])
pre.g.traffic <- as.factor(pre$traffic[1:351])
pre.g.dnc <- as.factor(pre$dnc[1:351])


#--(1b) Post categories
post.g.species <- as.factor(post$species[1:221])
post.g.solar <- as.factor(post$solar[1:221])
post.g.cat <- as.factor(post$category[1:221])
post.g.cam <- as.factor(post$camera[1:221])
post.g.traffic <- as.factor(post$traffic[1:221])
post.g.dnc <- as.factor(post$dnc[1:221])

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
                            title = "Pre-Construction: Species Groupings") + 
                   geom_text(
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)

pre.solar <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.solar,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            title = "Pre-Construction: Daylight Preference") + 
                   geom_text(
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)

pre.cat <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cat,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            title = "Pre-Construction: Mammalian Groupings") + 
                   geom_text(
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)


pre.cam <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cam,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            title = "Pre-Construction: Camera Preference") + 
                   geom_text(
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)


pre.traffic <- fviz_pca_ind(pca.pre,
                           col.ind = pre.g.traffic,
                           palette = c( ),
                           addEllipses = TRUE,
                           ellipse.type = "confidence",
                           legend.title = "Groups",
                           repel = TRUE,
                           label = "none",
                           title = "Pre-Construction: SUMMER Traffic Preference") + 
                  geom_text(
                           label=pre$species, 
                           nudge_x = 0.25, nudge_y = 0.25,
                           check_overlap = T)


pre.dnc <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.dnc,
                            palette = c(""),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "groups",
                            repel = TRUE,
                            label = "none",
                            title = "Pre-Construction: D/N/C Category") + 
                   geom_text(
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)

#--(2b) Post-Construction Ellipses PCA categories
post.species <- fviz_pca_ind(pca.post,
                            col.ind = post.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            title = "Post-Construction: Species Groupings") + 
                   geom_text(
                            label=post$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)

post.solar <- fviz_pca_ind(pca.post,
                          col.ind = post.g.solar,
                          palette = c( ),
                          addEllipses = TRUE,
                          ellipse.type = "confidence",
                          legend.title = "Groups",
                          repel = TRUE,
                          label = "none",
                          title = "Post-Construction: Daylight preference") + 
                 geom_text(
                          label=post$species, 
                          nudge_x = 0.25, nudge_y = 0.25,
                          check_overlap = T)

post.cat <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cat,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE,
                        label = "none",
                        title = "Post-Construction: Mammalian Groupings") + 
               geom_text(
                        label=post$species, 
                        nudge_x = 0.25, nudge_y = 0.25,
                        check_overlap = T)

post.cam <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cam,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups",
                        repel = TRUE,
                        label = "none",
                        title = "Post-Construction: Camera Preference") + 
               geom_text(
                        label=post$species, 
                        nudge_x = 0.25, nudge_y = 0.25,
                        check_overlap = T)

post.traffic <- fviz_pca_ind(pca.post,
                            col.ind = post.g.traffic,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups",
                            repel = TRUE,
                            label = "none",
                            title = "Post-Construction: SUMMER Traffic Preference") + 
                   geom_text(
                            label=post$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T)

post.dnc <- fviz_pca_ind(pca.post,
                             col.ind = post.g.dnc,
                             palette = c( ),
                             addEllipses = TRUE,
                             ellipse.type = "confidence",
                             legend.title = "Groups",
                             repel = TRUE,
                             label = "none",
                             title = "Post-Construction: D/N/C Category") + 
                    geom_text(
                             label=post$species, 
                             nudge_x = 0.25, nudge_y = 0.25,
                             check_overlap = T)

#Generate Plots
pre.scree
pre.ind
pre.var
pre.bp
pre.species
pre.solar
pre.cat
pre.cam
pre.traffic
pre.dnc


post.scree
post.ind
post.var
post.bp
post.species
post.solar
post.cat
post.cam
post.traffic
post.dnc
