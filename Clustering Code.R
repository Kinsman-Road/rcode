library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)

#-----Data Prep-----test
pre.scale <- scale(pre.1)
post.scale <- scale(post.1)

pre.d <- dist(pre.scale, method = "euclidean")
post.d <- dist(post.scale, method = "euclidean")

#-----Hclust: Complete-----
pre.hc1 <- hclust(pre.d, method = "complete")
post.hc1 <- hclust(post.d, method = "complete")

#-----Agnes: Complete-----
pre.hc2 <- agnes(pre.scale, method="complete")
post.hc2 <- agnes(post.scale, method="complete")

#agglomerative coefficient
pre.hc2$ac
post.hc2$ac

#-----Agnes: Ward-----
pre.hc3 <- agnes(pre.scale, method = "ward")
post.hc3 <- agnes(post.scale, method = "ward")

pre.pltree <- pltree(pre.hc3, cex=0.6, hang=-1, main="Pre-Construction: Dendrogram")
post.pltree <- pltree(post.hc3, cex=0.6, hang=-1, main="Post-Construction: Dendrogram")

pre.hc3.dend <- rect.hclust(pre.hc3, k=4, border = 2:5)
post.hc3.dend <- rect.hclust(post.hc3, k=4, border = 2:5)

#-----Divisive Hierarchical Clustering (DIANA)-----
pre.hc4 <- diana(pre.scale)
post.hc4 <- diana(post.scale)

pre.hc4$dc
post.hc4$dc

pre.hc4.diana <- pltree(pre.hc4, cex=0.6, hang=-1, main="Pre DIANA")
pre.hc4.dend <- rect.hclust(pre.hc4, k=4, border = 2:5)

post.hc4.diana <- pltree(post.hc4, cex=0.6, hang=-1, main="Post DIANA")
post.hc4.dend <- rect.hclust(post.hc4, k=4, border = 2:5)

pre.subgroup4 <- cutree(pre.hc4, k=4)
post.subgroup4 <- cutree(post.hc4, k=4)

pre.hc4.dend
post.hc4.dend

pre.cluster4 <- fviz_cluster(list(data=pre.scale, cluster=pre.subgroup4), main="Pre-Construction: DIANA Groups", labels="none")
post.cluster4 <- fviz_cluster(list(data=post.scale, cluster=post.subgroup4), main="Post-Construction: DIANA Groups", labels="none")

pre.cluster4
post.cluster4

#-----Ward's Method-----
pre.hc5 <- hclust(pre.scale, method = "ward.D2")
post.hc5 <- hclust(post.scale, method = "ward.D2")

pre.subgroup <- cutree(pre.hc3, k=4)
post.subgroup <- cutree(post.hc3, k=4)

#Will give us how many members are in each cluster
table(pre.subgroup)
table(post.subgroup)

#Plotting dendrogram with borders around clusters
pre.hc5.dend <- plot(pre.hc5, cex=0.6)
post.hc5.dend <- plot(post.hc5, cex=0.6)

pre.hc5.dend2 <- rect.hclust(pre.hc5, k=4, border = 2:5)
post.hc5.dend2 <- rect.hclust(post.hc5, k=4, border = 2:5)

#-----fviz-----
#First do ward's method and define subgroups
pre.hc5 <- hclust(pre.scale, method = "ward.D2")
post.hc5 <- hclust(post.scale, method = "ward.D2")

pre.subgroup <- cutree(pre.hc5, k=4)
post.subgroup <- cutree(post.hc5, k=4)

#Now visualize
pre.cluster <- fviz_cluster(list(data=pre.scale, cluster=pre.subgroup), main="Pre-Construction: Cluster Plot", labels="none")
post.cluster <- fviz_cluster(list(data=post.scale, cluster=post.subgroup), main="Post.Construction: Cluster Plot", labels="none")
#With labels
pre.lcluster <- fviz_cluster(list(data=pre.scale, cluster=pre.subgroup), main="Pre-Construction: Cluster Plot", repel=TRUE)
post.lcluster <- fviz_cluster(list(data=post.scale, cluster=post.subgroup), main="Post.Construction: Cluster Plot", repel=TRUE)

