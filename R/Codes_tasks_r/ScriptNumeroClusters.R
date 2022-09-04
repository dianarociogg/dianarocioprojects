library(factoextra)
library(NbClust)
library(cluster)

setwd("E:/OTROS/CENSO/DATOS/")
Proy<-read.csv(file="ProyeccionesDANEWEB2.csv", header=TRUE, sep=";",colClasses=c("DPMP"="character"),stringsAsFactors=F)
str(Proy)
Pro2<-Proy[,c(4,6:8)]
str(Pro2)
df<-scale(Pro2[,c(2:4)])
str(df)

### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(df[,2:3], kmeans, method = "wss") # 4 clusters
fviz_nbclust(df[,2:3], kmeans, method = "silhouette") # 2 clusters
fviz_nbclust(df[,2:3], kmeans, method = "gap_stat") # 3 clusters

fviz_nbclust(df[,2:3], cluster::pam, method = "wss") # 6 clusters
fviz_nbclust(df[,2:3], cluster::pam, method = "silhouette") # 2 clusters
fviz_nbclust(df[,2:3], cluster::pam, method = "gap_stat") # 3 clusters

fviz_nbclust(df[,2:3], cluster::clara, method = "wss") # 6 clusters
fviz_nbclust(df[,2:3], cluster::clara, method = "silhouette") # 2 clusters
fviz_nbclust(df[,2:3], cluster::clara, method = "gap_stat") # 3 clusters

fviz_nbclust(df[,2:3], hcut, method = "wss") # 6 clusters
fviz_nbclust(df[,2:3], hcut, method = "silhouette") # 2 clusters
fviz_nbclust(df[,2:3], hcut, method = "gap_stat") # 3 clusters




# PAM clustering

pam.res <- pam(df,5)
pam.res$cluster