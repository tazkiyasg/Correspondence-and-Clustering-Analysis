library(readxl)
DataAnmul <- read_excel("D:/anmul/DataAnmul.xlsx")
View(DataAnmul)

install.packages("car")
install.packages("magrittr")
install.packages("knitr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("clValid")
install.packages("factoextra")
install.packages("tidyverse")

library(car)
library(magrittr)
library(knitr)
library(ggplot2)
library(cluster)
library(clValid)
library(factoextra)
library(tidyverse)

summary(DataAnmul)

Datastand <- scale(DataAnmul[,2:5])
Datastand 

n <- dim(Datastand)[1]
Data <- data.matrix(Datastand,1:4)
rownames(Data)=c(1:nrow(Data))  

jarak <- dist(Datastand, method = "euclidean")
jarak

#Single Linkage
hiers <- hclust(dist(Data), method = "single")
#korelasi cophenetic
d1 <- dist(Data)
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
cors <- cor(d1,d2)
cors

#Average Linkage
hierave <- hclust(dist(Data), method = "ave")
#korelasi cophenetic
d1 <- dist(Data)
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
corave <- cor(d1,d2)
corave

#Complete Linkage
hiercomp <- hclust(dist(Data), method = "complete")
#korelasi cophenetic
d1 <- dist(Data)
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
corcomp <- cor(d1,d2)
corcomp

#Centorid Linkage
hiercen <- hclust(dist(Data), method = "centroid")
#korelasi cophenetic
d1 <- dist(Data)
hc <- hclust(d1, "centroid")
d2 <- cophenetic(hc)
corcen <- cor(d1,d2)
corcen

#Ward
hierward <- hclust(dist(Data), method = "ward.D")
#korelasi cophenetic
d1 <- dist(Data)
hc <- hclust(d1,"ward.D")
d2 <- cophenetic(hc)
corward <- cor(d1,d2)
corward

library(clValid)
library(ggplot2)
inval <- clValid(Data, 2:5, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
inval

library(cluster)
library(factoextra)
library(tidyverse)
library(car)
hirave <- hclust(dist(scale(DataAnmul[,2:5])), method = "average")
hirave
plot(hirave, labels(DataAnmul$Nama), hang = 1, col = "blue", main = "Cluster Dendogram", sub = " ", xlab = "Responden", ylab = "Jarak")

anggotaave <- data.frame(id = DataAnmul$Nama, cutree(hirave, k = 5))
anggotaave

clus_hier <- eclust(Datastand, FUNcluster = "hclust", k = 5, hc_method = "average", graph = TRUE)
fviz_dend(clus_hier, rect = TRUE, show_labels = TRUE, cex = 0.5)

idclus = clus_hier$cluster
idclus

aggregate(DataAnmul,list(idclus),mean)
