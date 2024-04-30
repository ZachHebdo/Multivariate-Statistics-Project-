#install.packages("cluster")
#install.packages("factoextra")
#install.packages("clValid")
#install.packages("clusterSim")
#install.packages("vcd")
#install.packages("multcomp")
#install.packages("NbClust")
library(vcd)
library(factoextra)
library(ggplot2)
library(dbscan)
library(fpc)
library(cluster)

#library(GDAtools)
#library(clValid)  
#library(clusterSim)
#library(kableExtra)
#library(multcomp)

library(cluster)
library(dplyr)
library(tidyverse)
library(knitr)
library(FactoMineR)
library(factoextra)
library(questionr)
library(ade4)
library(ggrepel)
library(scatterplot3d)
library(NbClust)


df <- read.csv("preprocessedData.csv", sep=",")

df$N_claims_history <- as.factor(df$N_claims_history)
df$N_claims_year <- as.factor(df$N_claims_year)
df$Policies_in_force <- as.factor(df$Policies_in_force)
df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies<- as.factor(df$Policies)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$Type_risk <- as.factor(df$Type_risk)

df <- subset(df, select = -Policies_in_force)
df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))

# Clustering PCA  #################################### 

scores_pca <- scores_robust[,1:3]

df_quant <- df %>% select(where(is.numeric))
#df_qual <- df %>% select(where(~ !is.numeric(.)))

within=NULL
for(i in 1:10) within[i]=sum(kmeans(scores_pca,centers=i)$withinss)
plot(1:10, within, type="b",xlab = "clusters",ylab = "intra_group_inertia")
abline(v = 4, col = "blue", lty = 2)
#Nombre de clusters retenu : 4

#Graphique du premier plan factoriel et de l'appartenance aux clusters
clust2=kmeans(scores_pca, 4) #k-means avec 5 clusters
fviz_cluster(clust2, scores_pca[,c(1,2)], geom = "point")
fviz_cluster(clust2, scores_pca[,c(1,3)], geom = "point")
fviz_cluster(clust2, scores_pca[,c(2,3)], geom = "point")

scatterplot3d(scores_pca, y=NULL, z=NULL, pch = 20, color = clust2$cluster)

scores <- as.data.frame(cbind(scores_pca, Cost_claims_year = scale(df_quant$Cost_claims_year), Premium = scale(df_quant$Premium)))
boxplot(scores$Cost_claims_year~clust2$cluster, main='Boxplot of Cost of claims in the year for each cluster')
boxplot(scores$Premium~clust2$cluster, main='Boxplot of Premium for each cluster')


anova(lm(scores$Cost_claims_year~clust2$cluster))
anova(lm(scores$Premium~clust2$cluster))

kruskal.test(scores$Cost_claims_year~clust2$cluster, data = as.data.frame(scores))
kruskal.test(scores$Premium~clust2$cluster, data = as.data.frame(scores))

### USING MASS and princomp. Virtually no difference. 
library(MASS) # Don´t  load before.
mcd <- cov.mcd(df_quant)
pca_robust = princomp(df_quant, cor = TRUE, covmat = mcd)

# Last plot:
fviz_pca_biplot(pca_robust, label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_pca_biplot(pca_robust, axes = c(1, 3), label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))

# scatterplot3d(pca_robust$scores[,1:3], y=NULL, z=NULL, pch = 20, color = clust2$cluster)

