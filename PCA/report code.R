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
library(ggpubr)
library(gridExtra)

pca.pre <- prcomp(pre.n, scale = TRUE)   #singular value pca method - not spectral decomposition
pca.post <- prcomp(post.n, scale = TRUE)   #singular value pca method - not spectral decomposition


# (2) Produce ellipses PCA graphs for every factor   
pre.species <- fviz_pca_ind(pca.pre,
                            col.ind = pre.g.species,
                            palette = c( ),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Groups:",
                            repel = TRUE,
                            label = "none",
                            title = "Figure C.5a: Pre-Construction Species Groupings")

post.species <- fviz_pca_ind(pca.post,
                             col.ind = post.g.species,
                             palette = c( ),
                             addEllipses = TRUE,
                             ellipse.type = "confidence",
                             legend.title = "Groups:",
                             repel = TRUE,
                             label = "none",
                             title = "Figure C.5b: Post-Construction Species Groupings")

# Presentation Select Species
pre.species + theme(legend.position="bottom")
post.species + theme(legend.position="bottom")

ggarrange(pre.species + theme(legend.position="bottom"), post.species + theme(legend.position="bottom"),
          ncol = 2, nrow = 1)
