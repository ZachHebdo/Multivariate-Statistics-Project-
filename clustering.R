install.packages("cluster")
install.packages("factoextra")
install.packages("clValid")
install.packages("clusterSim")
install.packages("vcd")
install.packages("multcomp")
install.packages("NbClust")
library(vcd)
library(factoextra)
library(ggplot2)
library(dbscan)
library(fpc)
library(cluster)
library(clValid)  
library(clusterSim)
library(cluster)
library(dplyr)
library(GDAtools)
library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(questionr)
library(ade4)
library(ggrepel)
library(scatterplot3d)
library(NbClust)
library(GDAtools)
library(cluster)
library(multcomp)

df <- read.csv("C:/Users/denis/Downloads/preprocessedData.csv", sep=",")

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




HCA variables qualitatives###############################################################"
# Calcul de la matrice de dissimilarité avec la distance de Chi-square
chi_square_dist  <- daisy(df_qual, metric = "gower")
# Application de la classification hiérarchique
hc_qual <- hclust(as.dist(chi_square_dist), method = "ward.D2")
# Visualiser le dendrogramme
plot(hc_qual)
rect.hclust(hc_qual, k=5, border="red")
legend("topright", legend = "Dendrogramme HCA ascendant",
       bty = "n", cex = 0.8)


# Créer une liste pour stocker les indices silhouette
silhouette_values <- numeric(0)
silhouette <- numeric(0)
sill <- numeric(0)
# Nombre de coupures à tester
max_k <- 100
avg_sil <- vector()
# Boucle à travers différentes valeurs de coupure
for (k in 1:max_k) {
  # Découper le dendrogramme pour obtenir les clusters
  clusters_qual <- cutree(hc_qual, k = k)
  
  # Calculer l'indice silhouette pour les clusters obtenus
  silhouette <- silhouette(clusters_qual, chi_square_dist)
  
  # Calculer l'indice silhouette moyen
  avg_silhouette <- mean(silhouette[, "sil_width"])
  sill[k] <- avg_silhouette
  # Stocker l'indice silhouette moyen
  silhouette_values <- c(silhouette_values, avg_silhouette)
  avg_sil[k] <- avg_silhouette
}



plot(hc_qual)
rect.hclust(hc_qual, k=6, border="red")



#Création des groupes
groups=cutree(hc_qual, k=6) 
df_qual[groups==1,] 
df_qual[groups==2,] 
df_qual[groups==3,]
df_qual[groups==4,]
df_qual[groups==5,]
summary(df_qual[groups==6,])




df_nombre_individus <- data.frame(Groupe = as.integer(names(nombre_individus_par_groupe)), Nombre_Individus = as.integer(nombre_individus_par_groupe))

print(df_nombre_individus)
# Résumé du nombre d'individus par groupe
summary_groups <- summary(groups)

barplot(summary_groups, main = "Répartition des individus par groupe", xlab = "Groupes", ylab = "Nombre d'individus")


summary(df_qual[groups==6,])
length(df_qual[groups==1,])
for(variable in names(df_qual)){
  boxplot(df_qual[[variable]] ~ groups, main = paste( variable, 'by cluster'),ylab =variable  )

chi_results_df <- data.frame(Variable = character(),
                             Test_statistic = numeric(),
                             P_value = numeric(),
                             stringsAsFactors = FALSE)
qualitative_vars <- names(df_qual)[sapply(df_qual, is.factor)]
###CHI 2
for (i in seq_along(qualitative_vars)) {
  var <- qualitative_vars[i] 
  contingency_table <- table(df_qual[[var]], groups)
  expected_table <- chi_test$expected
  conditions_satisfaites <- "Oui"
  if (sum(contingency_table) < 40) {
    conditions_satisfaites <- "Non (Échantillon trop petit)"
  }
  if (any(expected_table < 5)) {
    conditions_satisfaites <- "Non (Fréquence attendue < 5)"
  }
  
  chi_test <- chisq.test(contingency_table)
  
  chi_results_df <- rbind(chi_results_df, data.frame(Variable = var,
                                                     Test_statistic = chi_test$statistic,
                                                     P_value = chi_test$p.value))
}

print(chi_results_df)


K-MEANS qualitatif  ##################################################################################


within=NULL
for(i in 1:11)
  within[i]=sum(kmeans(chi_square_dist, centers = i)$withinss)
plot(1:11, within, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Intra-group Inertia",
     main = "Cluster Selection")
points(3, within[3], col = "red", pch = 16)
     main = "Cluster Selection"

# Calculer la distance chisquare
gower_dist <- daisy(df_qual, metric = "gower")

# algorithme K-means
kmeans_clusters <- kmeans(chi_square_dist, centers = 3)

print(kmeans_clusters)

for (variable in names(df_qual)) {
  boxplot(df_qual[[variable]] ~ kmeans_clusters$cluster,
          col = kmeans_clusters$cluster,
          main = paste("Boxplot of", variable, "with clusters"),
          xlab = "Cluster",
          ylab = variable)
  
  legend("topright",
         legend = unique(kmeans_clusters$cluster),
         col = unique(kmeans_clusters$cluster),
         pch = 19,
         title = "Cluster")
}

nombre_par_groupe <- table(kmeans_clusters$cluster)

tableau_groupes <- data.frame(Cluster = names(nombre_par_groupe),
                              Nombre_individus = as.numeric(nombre_par_groupe))

print(tableau_groupes)

cluster_labels <- kmeans_clusters$cluster

df_qual$cluster <- factor(cluster_labels)

clustered_df <- cbind(df_qual, cluster = factor(cluster_labels))

# Appliquer la fonction aggregate pour calculer les statistiques descriptives par cluster
summary_by_cluster <- aggregate(. ~ cluster, data = clustered_df, FUN = summary)

print(summary_by_cluster)




HCA variables quantitatives sur toute la DB#################################################

d <- dist(scale(df_quant), method = "euclidean")

clust <- hclust(d, method = "ward.D2")

silhouette_values <- numeric(0)
silhouette <- numeric(0)
silhouette_index <- numeric(0)
# Nombre de coupures à tester
max_k <- 100

# Boucle à travers différentes valeurs de coupure
for (k in 2:max_k) {
  # Découper le dendrogramme pour obtenir les clusters
  clusters <- cutree(clust, k = k)
  
  # Calculer l'indice silhouette pour les clusters obtenus
  silhouette <- silhouette(clusters, d)
  
  # Calculer l'indice silhouette moyen
  avg_silhouette <- mean(silhouette[, "sil_width"])
  silhouette_index[k] <- (avg_silhouette)
  # Stocker l'indice silhouette moyen
  silhouette_values <- c(silhouette_values, avg_silhouette)
}
plot(silhouette_index,xlab = "nb_clusters")
# Trouver le nombre optimal de coupures qui maximise l'indice silhouette moyen
optimal_k <- which.max(silhouette_values) + 1

# Afficher le nombre optimal de coupures
print(paste("Nombre optimal de coupures (selon l'indice silhouette) :", optimal_k))
# Tracer le graphique de l'indice silhouette moyen en fonction du nombre de coupures
plot(2:max_k, silhouette_values, type = "b", 
     main = "Indice silhouette en fonction du nombre de coupures", 
     xlab = "Nombre de coupures", ylab = "Indice silhouette moyen")

# Ajouter une ligne verticale pour indiquer le nombre optimal de coupures
abline(v = optimal_k, col = "red", lty = 2)


#clustering

d_quant <- dist(scale(df_quant), method="euclidean")
hc_quant <- hclust(d_quant, method="ward.D2")
clusters_quant <- cutree(hc_quant, h = optimal_k)
plot(hc_quant,cex = 0.6, hang = -1)
rect.hclust(hc_quant, k=optimal_k, border="red")

#Création des groupes
groups_quant=cutree(hc_quant, k=2) 
df_quant[groups_quant==1,] 
df_quant[groups_quant==2,] 


effectif_groupes <- table(groups_quant)
print(effectif_groupes)



for(variable in names(df_quant)){
  boxplot(df_quant[[variable]] ~ groups_quant, main = paste('Boxplot', variable, 'for each cluster'))
}

#Test Anova pour les moyennes de la variable Meat dans les différents clusters

# Initialiser une liste pour stocker les résultats des ANOVA
anova_results <- list()

# Boucle à travers chaque variable de df_quant
for (variable in names(df_quant)) {
  # Construire la formule pour la régression linéaire
  formula <- as.formula(paste(variable, "~ groups_quant"))
  
  # Effectuer ANova
  anova_result <- anova(lm(formula, data = df_quant))
  

  anova_results[[variable]] <- anova_result
}

print(anova_results)

#Test de Student
  
# liste pour stocker les résultats des tests t
t_test_results <- list()

num_groups <- max(groups_quant)
diff_matrix <- matrix("", nrow = length(names(df_quant)), ncol = num_groups,
                      dimnames = list(names(df_quant), paste0("G", 1:num_groups)))

for (variable in names(df_quant)) {
  # Initialiser une matrice pour stocker les résultats des tests t pour chaque paire de groupes
  t_test_matrix <- matrix("", nrow = num_groups, ncol = num_groups)
  
  # Effectuer un test t de Student pour chaque paire de groupes
  for (i in 1:num_groups) {
    for (j in 1:num_groups) {
      # Éviter de comparer un groupe avec lui-même
      if (i != j) {
        # Sélectionner les données correspondant aux groupes i et j
        select <- (groups_quant == i | groups_quant == j)
        
        # Effectuer le test t de Student
        test <- t.test(df_quant[select, variable] ~ groups_quant[select])
        
        # Stocker le résultat du test t dans la matrice
        if (test$p.value < 0.05) {
          t_test_matrix[i, j] <- "1"  # Différence significative
          diff_matrix[variable, j] <- "1"
        } else {
          t_test_matrix[i, j] <- "0"  # Pas de différence significative
        }
      } else {
        # Laisser la valeur sur la diagonale comme NA
        t_test_matrix[i, j] <- ""
      }
    }
  }
  
  # Stocker la matrice des résultats des tests t pour la variable dans la liste
  t_test_results[[variable]] <- t_test_matrix
}

print(diff_matrix)
print(t_test_results)


#K-means variable quantitatif sur toute la db ####################################################

within=NULL
for(i in 1:11)
  within[i]=sum(kmeans(scale(df_quant),centers=i)$withinss)
plot(1:11, within, type="b")

clust2=kmeans(df_quant, centers=3)
plot(scale(df_quant), col=clust2$cluster, pch=19, cex=2)
abline(h=0, v=0)
legend("topright", legend = unique(clust2$cluster), col = unique(clust2$cluster), pch = 19, title = "Cluster")

Clustering ACM###################################################################################

  
scores_mca <- mca3$ind$coord[,1:3]  #Voir table MCA
df <- read.csv("preprocessedData.csv")

df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies<- as.factor(df$Policies)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$Type_risk <- as.factor(df$Type_risk)


# In case we need it in the future:
preprocessed_df <- df

#df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))



scores_mca <- scores_mca[,2:4]

df2 <- df_qual

d=dist(scores_mca, method="euclidean") #matrice des distances euclidiennes
clust_mca=hclust(d, method="ward.D2")

# Trouver le nombre de cluster optimal
silhouette_values <- numeric(0)
silhouette <- numeric(0)
silhouette_index <- numeric(0)
# Nombre de coupures à tester
max_k <- 20

for (k in 2:max_k) {
  # Découper le dendrogramme pour obtenir les clusters
  clusters_mca <- cutree(clust_mca, k = k)
  
  # Calculer l'indice silhouette pour les clusters obtenus
  silhouette <- silhouette(clusters_mca, d)
  
  # Calculer l'indice silhouette moyen
  avg_silhouette <- mean(silhouette[, "sil_width"])
  silhouette_index[k] <- (avg_silhouette)
  # Stocker l'indice silhouette moyen
  silhouette_values <- c(silhouette_values, avg_silhouette)
}
plot(silhouette_index,xlab = "nb_clusters")
# Trouver le nombre optimal de coupures qui maximise l'indice silhouette moyen
optimal_k <- which.max(silhouette_values) + 1

print(paste("Nombre optimal de coupures (selon l'indice silhouette) :", optimal_k))
plot(2:max_k, silhouette_values, type = "b", 
     main = "silhouette index and number of cluster", 
     xlab = "Clusters", ylab = "Index")
abline(v = optimal_k, col = "red", lty = 2)



scores_mca <- cbind(scores_mca, Cost_claims_year = scale(preprocessed_df$Cost_claims_year), Premium = scale(preprocessed_df$Premium))
scores_mca <- scores_mca[,c(1,2,3,6,7)]
groups=cutree(clust_mca, k=optimal_k) 
 summary(scores[groups==1,]) 
# scores[groups==2,]

anova_cost_mca <- anova(lm(as.data.frame(scores_mca)$Cost_claims_year~groups))

bonferroni_test <- pairwise.t.test(scores_mca$Cost_claims_year, groups, p.adjust.method = "bonferroni")
print(bonferroni_test)


anova_premium_mca <- anova(lm(as.data.frame(scores_mca)$Premium~groups))

bonferroni_test <- pairwise.t.test(scores_mca$Premium, groups, p.adjust.method = "bonferroni")
print(bonferroni_test)


#select=(groups==1|groups==2) #opérateur logique "ou"
#test=t.test(as.data.frame(scores)$Cost_claims_year[select]~groups[select])
#test

#test=t.test(as.data.frame(scores)$Premium[select]~groups[select])
#test

scores_mca <- scores_mca[1:3]
within=NULL
for(i in 1:10) within[i]=sum(kmeans(scores_mca,centers=i)$withinss)
plot(1:10, within, type="b")
abline(v = 4, col = "blue", lty = 2)
#Nombre de clusters retenu : 4

#Graphique  de l'appartenance aux clusters
clust2=kmeans(scores_mca, 4) #k-means avec 5 clusters
fviz_cluster(clust2, scores_mca[,c(1,2)], geom = "point")
fviz_cluster(clust2, scores_mca[,c(1,3)], geom = "point")
fviz_cluster(clust2, scores_mca[,c(2,3)], geom = "point")

scatterplot3d(scores, y=NULL, z=NULL, pch = 20, color = clust2$cluster)

scores <- as.data.frame(cbind(scores_mca, Cost_claims_year = scale(preprocessed_df$Cost_claims_year), Premium = scale(preprocessed_df$Premium)))
boxplot(scores$Cost_claims_year~clust2$cluster, main='Boxplot of Cost of claims in the year for each cluster')
boxplot(scores$Premium~clust2$cluster, main='Boxplot of Premium for each cluster')


anova(lm(scores$Cost_claims_year~clust2$cluster))
anova(lm(scores$Premium~clust2$cluster))

# Since we saw that Premium was a very variable heavily affected by outliers, we decided to do a Kruskal Wallis test. 
# This concludes that the clusters do differ in terms of Premium, while they do not differ for the Cost_claims_year (when accounting for outliers).
kruskal.test(scores$Cost_claims_year~clust2$cluster, data = as.data.frame(scores))
kruskal.test(scores$Premium~clust2$cluster, data = as.data.frame(scores))

# Last plot:
fviz_mca_biplot(scores_mca, label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_mca_biplot(scores_mca, axes = c(1, 3), label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))

       
       Clustering PCA  #################################### 

scores_pca <- pca$scores
df <- read.csv("C:/Users/denis/Downloads/preprocessedData (1).csv")

df_quant <- df %>% select(where(is.numeric))
#df_qual <- df %>% select(where(~ !is.numeric(.)))
scores_pca <- scores_pca[,2:4]
df2 <- df_quant
       
# liste pour stocker les indices silhouette
silhouette_values <- numeric(0)
silhouette <- numeric(0)
silhouette_index <- numeric(0)
# Nombre de coupures à tester
max_k <- 20

# 
for (k in 2:max_k) {
  # Découper le dendrogramme pour obtenir les clusters
  clusters_pca <- cutree(clust_pca, k = k)
    silhouette <- silhouette(clusters_pca, d)
    avg_silhouette <- mean(silhouette[, "sil_width"])
  silhouette_index[k] <- (avg_silhouette)
  silhouette_values <- c(silhouette_values, avg_silhouette)
}
plot(silhouette_index,xlab = "nb_clusters")
# Trouver le nombre optimal de coupures qui maximise l'indice silhouette moyen
optimal_k <- which.max(silhouette_values) + 1

print(paste("Nombre optimal de coupures (selon l'indice silhouette) :", optimal_k))
# graphique de l'indice silhouette moyen en fonction du nombre de coupures
plot(2:max_k, silhouette_values, type = "b", 
     main = "silhouette index and number of cluster", 
     xlab = "Clusters", ylab = "Index")
abline(v = optimal_k, col = "red", lty = 2)

plot(clust_pca)
rect.hclust(clust_pca, k= 6, border="red")


scores_pca <- cbind(scores_pca, Cost_claims_year = scale(preprocessed_df$Cost_claims_year), Premium = scale(preprocessed_df$Premium))
groups=cutree(clust_pca, k=optimal_k) 
 summary(scores[groups==6,])

anova_cost_pca <- anova(lm(as.data.frame(scores_pca)$Cost_claims_year~groups))
bonferroni_test <- pairwise.t.test(scores_pca$Cost_claims_year, groups, p.adjust.method = "bonferroni")
print(bonferroni_test)

anova_premium_pca <- anova(lm(as.data.frame(scores_pca)$Premium~groups))
bonferroni_test <- pairwise.t.test(scores_pca$Premium, groups, p.adjust.method = "bonferroni")
print(bonferroni_test)

scores_pca <- scores_pca[1:3]
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

scores <- as.data.frame(cbind(scores_pca, Cost_claims_year = scale(preprocessed_df$Cost_claims_year), Premium = scale(preprocessed_df$Premium)))
boxplot(scores$Cost_claims_year~clust2$cluster, main='Boxplot of Cost of claims in the year for each cluster')
boxplot(scores$Premium~clust2$cluster, main='Boxplot of Premium for each cluster')


anova(lm(scores$Cost_claims_year~clust2$cluster))
anova(lm(scores$Premium~clust2$cluster))

kruskal.test(scores$Cost_claims_year~clust2$cluster, data = as.data.frame(scores))
kruskal.test(scores$Premium~clust2$cluster, data = as.data.frame(scores))

# Last plot:
fviz_pca_biplot(scores_pca, label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_pca_biplot(scores_pca, axes = c(1, 3), label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
       

