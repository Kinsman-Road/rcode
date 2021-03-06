#Resources
#https://www.datacamp.com/community/tutorials/pca-analysis-r
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table

#::::: [CONDENSED PCA RUN WITH SELECT SPEICES] :::::


# ::: to-do :::
# redo R code appendix
# review conclusions and paper for submission

# Import data set from github
library(readxl)
pre <- read_excel("PCA/pca.xlsx", sheet = "pca.pre.c")
post <- read_excel("PCA/pca.xlsx", sheet = "pca.post.c")

# Prepare dataframes
pre <- data.frame(pre)
post <- data.frame(post)

pre.n <- pre[1:7]      #create dataframes with only numerical columns from pre
post.n <- post[1:7]    #create dataframes with only numerical columns from post


# Performing principal components analysis
library(factoextra)
library(FactoMineR)

pca.pre <- prcomp(pre.n, scale = TRUE)   #singular value pca method - not spectral decomposition
pca.post <- prcomp(post.n, scale = TRUE)   #singular value pca method - not spectral decomposition

pre.eig <- get_eigenvalue(pca.pre)
post.eig <- get_eigenvalue(pca.post)



# Calculate PCA coordinates
# These are what is driving the direction of the plots below

# Pre-Construction PCA Coordinates
pre.vcf <- function(pre.load, comp.sdev){pre.load*comp.sdev}
pre.load <- pca.pre$rotation
pre.sdev <- pca.pre$sdev
pre.vcoord <- t(apply(pre.load, 1, pre.vcf, pre.sdev ))
pre.vc <- head(pre.vcoord[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

# Post-Construction PCA Coordinates
post.vcf <- function(post.load, comp.sdev){post.load*comp.sdev}
post.load <- pca.post$rotation
post.sdev <- pca.post$sdev
post.vcoord <- t(apply(post.load, 1, post.vcf, post.sdev))
post.vc <- head(post.vcoord[,1:7])   #1:8 just refers to the number of dimensions/eigenvectors to choose

pre.vc       #table of pre pca coords
post.vc      #table of post pca coords


# PCA Qualtity of Representation
pre.cos2 <- pre.vcoord^2
post.cos2 <- post.vcoord^2

pre.cos2     #table of contribution to each dimension
post.cos2    #table of contribution to each dimension



# PCA Contributions to Each Given Component
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


# Creating a scree plot to determine principal dimensions of interest
pre.scree <- fviz_eig(pca.pre,
                      subtitle = "Pre-Construction Scree Plot")
post.scree <- fviz_eig(pca.post,
                       subtitle = "Post-Construction Scree Plot")

pre.scree
post.scree

# Creating contribution plot for individual observations
pre.ind <- fviz_pca_ind(pca.pre,
                        col.ind = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        label = "none",
                        title = "Pre-Construction Individual Plot")

post.ind <- fviz_pca_ind(pca.post,
                        col.ind = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        label = "none",
                        title = "Post-Construction Individual Plot")

pre.ind
post.ind

# Creating contribution plot for variable contributions
pre.var <- fviz_pca_var(pca.pre,
                        col.var = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                        repel = TRUE,
                        title = "Pre-Construction Variable Plot")

post.var <- fviz_pca_var(pca.post,
                         col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),   #default R colors
                         repel = TRUE,
                         title = "Post-Construction Variable Plot")

pre.var
post.var

# Creating a biplot(combination of ind + var plots)
pre.bp <- fviz_pca_biplot(pca.pre,
                         col.ind = "coral",
                         col.var = "#2f2091",
                         label = "var",
                         repel = TRUE,
                         title = "Pre-Construction Biplot")
                       

post.bp <- fviz_pca_biplot(pca.post,
                           col.ind = "cadetblue3",
                           col.var = "#2f2091",
                           label = "var",
                           repel = TRUE,
                           title = "Post-Construction Biplot")

pre.bp
post.bp

# Creating 3D Observation Plots 
library(pca3d)


pre3d.species <- pca3d(pca.pre, group=pre$species)
pre3d.mcat <- pca3d(pca.pre, group=pre$category)
pre3d.dnc <- pca3d(pca.pre, group=pre$dnc)
pre3d.traffic <- pca3d(pca.pre, group=pre$traffic)

post3d.species <- pca3d(pca.post, group=post$species)
post3d.mcat <- pca3d(pca.post, group=post$category)
post3d.dnc <- pca3d(pca.post, group=post$dnc)
post3d.traffic <- pca3d(pca.post, group=post$traffic)
                                     
# Creating an individual PCA plot with ellipses for categories
# (1) First define categories as factors

# (1a) Pre categories
pre.g.species <- as.factor(pre$species[1:349])
pre.g.solar <- as.factor(pre$solar[1:349])
pre.g.cat <- as.factor(pre$category[1:349])
pre.g.cam <- as.factor(pre$camera[1:349])
pre.g.traffic <- as.factor(pre$traffic[1:349])
pre.g.dnc <- as.factor(pre$dnc[1:349])


# (1b) Post categories
post.g.species <- as.factor(post$species[1:222])
post.g.solar <- as.factor(post$solar[1:222])
post.g.cat <- as.factor(post$category[1:222])
post.g.cam <- as.factor(post$camera[1:222])
post.g.traffic <- as.factor(post$traffic[1:222])
post.g.dnc <- as.factor(post$dnc[1:222])

# (2) Produce ellipses PCA graphs for every factor   

# (2a) Pre-Construction Ellipses PCA categories
pre.species <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.5a: Pre-Construction Species Groupings") + 
                   geom_text(aes(fontface=3, color = factor(pre$species)),
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

pre.solar <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.solar,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
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
                            legend.title = "Mammalian Category",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.6a: Pre-Construction Mammalian Groupings") + 
                   geom_text(aes(fontface=3, color = pre$category),
                            label = pre$species,
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

pre.cam <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.cam,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.8a: Pre-Construction Camera Preference") + 
                   geom_text(aes(fontface=3, color = factor(pre$camera)),
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

pre.traffic <- fviz_pca_ind(pca.pre,
                           col.ind = pre.g.traffic,
                           palette = c( ),
                           addEllipses = TRUE,
                           ellipse.type = "confidence",
                           legend.title = "Groups:",
                           repel = TRUE,
                           label = "none",
                           title = "Figure C.9a: Pre-Construction Traffic Preference") + 
                  geom_text(aes(fontface=3, color = factor(pre$traffic)),
                           label=pre$species, 
                           nudge_x = 0.25, nudge_y = 0.25,
                           check_overlap = T,
                           size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))


pre.dnc <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.dnc,
                            palette = c(""),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.7a: Pre-Construction Diel Activity") + 
                   geom_text(aes(fontface=3, color = factor(pre$dnc)),
                            label=pre$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

# (2b) Post-Construction Ellipses PCA categories
post.species <- fviz_pca_ind(pca.post,
                            col.ind = post.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.5b: Post-Construction Species Groupings") + 
                   geom_text(
                            label=post$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

post.solar <- fviz_pca_ind(pca.post,
                          col.ind = post.g.solar,
                          palette = c( ),
                          addEllipses = TRUE,
                          ellipse.type = "confidence",
                          legend.title = "Groups:",
                          repel = TRUE,
                          label = "none",
                          title = "Post-Construction: Daylight preference") + 
                 geom_text(
                          label=post$species, 
                          nudge_x = 0.25, nudge_y = 0.25,
                          check_overlap = T) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

post.cat <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cat,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups:",
                        repel = TRUE,
                        label = "none",
                        title = "Figure C.6b: Post-Construction Mammalian Groupings") + 
               geom_text(aes(fontface=3, color = factor(post$category)),
                        label=post$species, 
                        nudge_x = 0.25, nudge_y = 0.25,
                        check_overlap = T,
                        size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

post.cam <- fviz_pca_ind(pca.post,
                        col.ind = post.g.cam,
                        palette = c( ),
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        legend.title = "Groups:",
                        repel = TRUE,
                        label = "none",
                        title = "Figure C.8b: Post-Construction Camera Preference") + 
               geom_text(aes(fontface=3, color = factor(post$camera)),
                        label=post$species, 
                        nudge_x = 0.25, nudge_y = 0.25,
                        check_overlap = T,
                        size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

post.traffic <- fviz_pca_ind(pca.post,
                            col.ind = post.g.traffic,
                            palette = c(),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.9b: Post-Construction Traffic Preference") + 
                   geom_text(aes(fontface=3, color = factor(post$traffic)),
                            label=post$species, 
                            nudge_x = 0.25, nudge_y = 0.25,
                            check_overlap = T,
                            size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

post.dnc <- fviz_pca_ind(pca.post,
                             col.ind = post.g.dnc,
                             palette = c( ),
                             addEllipses = TRUE,
                             ellipse.type = "confidence",
                             legend.title = "Groups:",
                             repel = TRUE,
                             label = "none",
                             title = "Figure C.7b: Post-Construction Diel Activity") + 
                    geom_text(aes(fontface=3, color = factor(post$dnc)),
                             label=post$species, 
                             nudge_x = 0.25, nudge_y = 0.25,
                             check_overlap = T,
                             size = 2.5) + theme(plot.title = element_text(size=12), title = element_text(face = "italic"))

# Generate All Plots
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

# Generate Select Species
pre.species
pre.cat
pre.dnc
pre.cam
pre.traffic
post.species
post.cat
post.dnc
post.cam
post.traffic

# Presentation Select Species
pre.scree + theme(legend.position="bottom")
post.scree + theme(legend.position="bottom")

pre.var + theme(legend.position="bottom")
post.var + theme(legend.position="bottom")

pre.bp + theme(legend.position="bottom")
post.bp + theme(legend.position="bottom")

pre.species + theme(legend.position="bottom")
post.species + theme(legend.position="bottom")

pre.cat + theme(legend.position="bottom")
post.cat + theme(legend.position="bottom")

pre.dnc + theme(legend.position="bottom")
post.dnc + theme(legend.position="bottom")

pre.cam + theme(legend.position="bottom")
post.cam + theme(legend.position="bottom")

post.traffic + theme(legend.position="bottom")
