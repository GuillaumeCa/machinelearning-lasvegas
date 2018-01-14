lasvegas = read.table("LasVegas.csv", sep=";", header = T)
attach(lasvegas)

summary(lasvegas)

# K-means clustering

set.seed(1)
data = data.frame(Score, Nr..rooms, Nr..hotel.reviews, Helpful.votes, Member.years)

wss = (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# with 4 clusters
fit = kmeans(data, 4)

# get cluster means
aggregate(data,by=list(fit$cluster),FUN=mean)

# append cluster assignment
data = data.frame(data, fit$cluster)

library(cluster) 
clusplot(data, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Hierarchical clustering

data.dist = dist(data, method = "euclidean") # distance matrix

data.fit = hclust(data.dist, method="ward.D") 

plot(data.fit) # display dendogram
data.groups = cutree(data.fit, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters 
rect.hclust(data.fit, k=4, border="red")

# PCA
data = data.frame(Score, Nr..reviews, Nr..hotel.reviews, Helpful.votes, Member.years)
pr.out = prcomp(data, scale=TRUE)

names(pr.out)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.sqrt = pr.out$sdev^2
pve = pr.sqrt / sum(pr.sqrt)

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type="s")
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="s")
