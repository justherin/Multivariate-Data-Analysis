#Scatter plot
pca <- prcomp(X, scale = TRUE)
summary(pca)
pca_var <- pca$sdev^2
pve <- pca_var/sum(pca_var)
plot(pve, xlab = "PCA", ylab = "Proportion of Variance Explained", ylim = c(0,0.5))
plot(cumsum(pve), xlab = "PCA",
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1))
abline(h = 0.8, col = "red")

#PCs as pca$x
S<-var(scale(X))
egn_S <- eigen(S)
PCs <- pca$x
apply(PCs, 2, var)
egn_S$values

#Plotting PCs
plot(PCs[, 1], PCs[, 2], xlab = "PC1 (28%)", ylab = "PC2 (18%)")
plot(PCs[, 1], PCs[, 3], xlab = "PC1 (28%)", ylab = "PC3 (14%)")
mn_1 <- which.min(PCs[, 1])
mx_1 <- which.max(PCs[, 1])
mn_1
mx_1

#Trees

dd <- as.dist(matrix(c(0, 3, 2, 5, 3, 0, 4, 1, 2, 4, 0, 7, 5, 1, 7, 0), nrow = 4))
par(mfrow = c(1, 3))
plot(hc_sng <- hclust(dd, method = "single"))
plot(hc_com <- hclust(dd, method = "complete"))
plot(hc_avg <- hclust(dd, method = "average"))

hc_sng$merge
hc_sng$height

#kmeans
out_kmeans <- kmeans(X, centers = cbind(cntrd_0[[1]], cntrd_0[[2]]),
                     algorithm = "MacQueen")

#To create primary key
row.names((data)) <- paste(data[,1],"",row.names(data))


## Cluster membership.
rect.hclust(hc_com,k=2)
cluster_membership <- cutree(hc_com, k = 2)

##plot the cut on the dendrogram that produces these four clusters
plot(hc_complete, labels = labs, cex = 0.5)
abline(h=139, col = "red")

## K-Means clustering, with K = 4.
set.seed(2)
out_kmeans <- kmeans(X, centers = 4, nstart = 20)
clusters_kmeans <- out_kmeans$cluster
table(clusters_kmeans, cluster_membership)


#PCA with colr coded by k-means
set.seed(2)
km<- kmeans(dist(nc),scale=TRUE,center=TRUE)
pc4<-prcomp(dist(nc),scale=TRUE,center=TRUE)
ntab<- data.frame(cbind(data.frame(pc4$x),data.farme(km$cluster)))
plot(ntab[,1],ntab[,2],col=ntab[,65])