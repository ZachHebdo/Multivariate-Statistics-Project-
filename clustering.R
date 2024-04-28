install.packages("cluster")
install.packages("factoextra")
install.packages("clValid")
install.packages("clusterSim")
install.packages("vcd")
library(vcd)
library(factoextra)
library(ggplot2)
library(dbscan)
library(fpc)
library(cluster)
library(clValid)  
library(clusterSim)

df <- read.csv("preprocessedData.csv")
View(df)

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

##HCA variables qualitatives
# Calcul de la matrice de dissimilarité avec la distance de Chi-square
chi_square_dist  <- daisy(df_qual, metric = "gower")
# Application de la classification hiérarchique
hc_qual <- hclust(as.dist(chi_square_dist), method = "ward.D2")
# Visualiser le dendrogramme
plot(hc_qual)
rect.hclust(hc_qual, k=5, border="red")

#Création des groupes
groups=cutree(hc_qual, k=5) 
df_qual[groups==1,] 
df_qual[groups==2,] 
df_qual[groups==3,]
df_qual[groups==4,]
df_qual[groups==5,]

#Faire les test chi carrés pour les différentes variables dans les différents groupes
# Liste des noms des variables qualitatives dans votre dataframe df_qual
qualitative_vars <- names(df_qual)[sapply(df_qual, is.factor)]

# Initialisation d'une liste pour stocker les résultats des tests
chi_results <- list()

# Boucle à travers chaque variable qualitative
for (var in qualitative_vars) {
  # Création du tableau de contingence
  contingency_table <- table(df_qual[[var]], groups)
  
  # Test du chi-deux d'indépendance
  chi_test <- chisq.test(contingency_table)
  
  # Stockage des résultats dans la liste
  chi_results[[var]] <- chi_test
}

# Affichage des résultats pour chaque variable
for (var in qualitative_vars) {
  cat("Résultats pour la variable", var, ":\n")
  print(chi_results[[var]])
  cat("\n")
}

###########  Kmeans varariables qualitative sur toute la db

within=NULL
 for(i in 1:11)
   within[i]=sum(kmeans(gower_dist, centers = i)$withinss)
plot(1:11, within, type="b")

# Calculer la distance de Gower
gower_dist <- daisy(df_qual, metric = "gower")

# Appliquer l'algorithme K-means
kmeans_clusters <- kmeans(gower_dist, centers = 5)

# Afficher les résultats
print(kmeans_clusters)

for(variable in names(df_qual)) {
  plot(df_qual[[variable]], col = kmeans_clusters$cluster, pch = 19,        main = paste("Nuage de points de", variable, "avec clusters"))
  legend("topright", legend = unique(kmeans_clusters$cluster), col = unique(kmeans_clusters$cluster), pch = 19, title = "Cluster")
  }

# Description de chaque cluster
cluster_labels <- kmeans_clusters$cluster

df_qual$cluster <- factor(cluster_labels)

# Créer un nouveau dataframe avec les étiquettes de cluster
clustered_df <- cbind(df_qual, cluster = factor(cluster_labels))

# Appliquer la fonction aggregate pour calculer les statistiques descriptives par cluster
summary_by_cluster <- aggregate(. ~ cluster, data = clustered_df, FUN = summary)

# Afficher les statistiques descriptives pour chaque variable dans chaque cluster
print(summary_by_cluster)


HCA variables quantitatives sur toute la DB#################################################
 #Trouver le nombre de coupures idéal

d <- dist(df_quant, method = "euclidean")
clust <- hclust(d, method = "ward.D2")
silhouette_values <- numeric(0)
max_k <- 20

for (k in 2:max_k) {
  clusters <- cutree(clust, k = k)
  
  # Calculer l'indice silhouette pour les clusters obtenus
  silhouette <- silhouette(clusters, d)
  
  avg_silhouette <- mean(silhouette[, "sil_width"])
  
  silhouette_values <- c(silhouette_values, avg_silhouette)
}

# Trouver le nombre optimal de coupures qui maximise l'indice silhouette moyen
optimal_k <- which.max(silhouette_values) + 1

# Afficher le nombre optimal de coupures
print(paste("Nombre optimal de coupures (selon l'indice silhouette) :", optimal_k))

# Graphiques
plot(2:max_k, silhouette_values, type = "b", 
     main = "Indice silhouette en fonction du nombre de coupures", 
     xlab = "Nombre de coupures", ylab = "Indice silhouette moyen")
abline(v = optimal_k, col = "red", lty = 2)

#Realiser le HCA avec le cutree optimal
d <- dist(df_quant, method="euclidean")
clust <- hclust(d, method="ward.D2")
clusters <- cutree(clust, h = optimal_k)
plot(clust)
rect.hclust(clust, k=optimal_k, border="red")

#Création des groupes
groups_quant=cutree(hc_quant, k=optimal_k) 
df_quant[groups_quant==1,] 
df_quant[groups_quant==2,] 

#Boxplot analysis
for(variable in names(df_quant)){
  boxplot(df_quant[[variable]] ~ groups_quant, main = paste('Boxplot of', variable, 'for each cluster'))
}
#Test Anova pour les moyennes de la variable Meat dans les différents clusters

# Initialiser une liste pour stocker les résultats des ANOVA
anova_results <- list()

# Boucle à travers chaque variable de df_quant
for (variable in names(df_quant)) {
  # Construire la formule pour la régression linéaire
  formula <- as.formula(paste(variable, "~ groups_quant"))
  
  # Effectuer l'ANOVA
  anova_result <- anova(lm(formula, data = df_quant))
  
  # Stocker le résultat de l'ANOVA dans la liste
  anova_results[[variable]] <- anova_result
}

# Afficher les résultats
print(anova_results)

#Test de Student

t_test_results <- list()
num_groups <- max(groups_quant)

#  résultats des tests t pour chaque paire de groupes
diff_matrix <- matrix("", nrow = length(names(df_quant)), ncol = num_groups,
                      dimnames = list(names(df_quant), paste0("G", 1:num_groups)))

# Boucle chaque variable
for (variable in names(df_quant)) {
  # Matrice résultats des tests t pour chaque paire de groupes
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
          t_test_matrix[i, j] <- "1"  # interprétation : ifférence significative
          diff_matrix[variable, j] <- "1"
        } else {
          t_test_matrix[i, j] <- "0"  # interprétationpas de différence significative
        }
      } else {
        # Laisser la valeur sur la diagonale comme NA pcq test avec lui même
        t_test_matrix[i, j] <- ""
      }
    }
  }
  
#  matrice des résultats des tests t pour la variable dans la liste
  t_test_results[[variable]] <- t_test_matrix
}

# matrice des différences
print(diff_matrix)

K means variable quantitative sur toute la DB##################################################

within=NULL
for(i in 1:11)
  within[i]=sum(kmeans(df_quant,centers=i)$withinss)
plot(1:11, within, type="b")

clust2=kmeans(df_quant, centers=5)
plot(df_quant, col=clust2$cluster, pch=19, cex=2)
abline(h=0, v=0)
legend("topright", legend = unique(clust2$cluster), col = unique(clust2$cluster), pch = 19, title = "Cluster")

####################################Methode algorithmique densité (density based spatial clustering of application with noise) 

df <-  # Définition de la base de donnée (ACP,ACM) avec comme variables les composantes et le score
qplot(df$x,df$y,shape = as.factor())

  
dbscan::kNNdistplot(df,k=5)
  
# exemple de fonctionnement de la fonction
k <- dbscan(df,eps=.15,minPts=5)
print(k)
table(df,k$cluster)


# Définir la plage de valeurs pour epsilon
eps_range <- seq(0.05, 0.5, by = 0.05)

# Initialiser un vecteur pour stocker le nombre de clusters pour chaque valeur de epsilon
num_clusters <- numeric(length(eps_range))

# Exécuter DBSCAN pour chaque valeur de epsilon et stocker le nombre de clusters
for (i in seq_along(eps_range)) {
  k <- dbscan(df, eps = eps_range[i], MinPts = 5)
  num_clusters[i] <- length(unique(k$cluster[k$cluster != 0]))
}

# Tracer le nombre de clusters en fonction de epsilon
plot(eps_range, num_clusters, type = "b", 
     main = "Nombre de clusters en fonction de epsilon",
     xlab = "Epsilon (eps)", ylab = "Nombre de clusters")


#  Nuage de points coloré par cluster
ggplot(data = df, aes(x = x, y = y, color = factor(k$cluster))) +
  geom_point() +
  labs(title = "Clusters identifiés par DBSCAN",
       x = "Variable x", y = "Variable y") +
  theme_minimal()

#  Visualisation des distances kNN
dbscan::kNNdistplot(df, k = 5)

#  Analyse des bruits

ggplot(data = df, aes(x = x, y = y)) +
  geom_point(data = subset(df, k$cluster == -1), color = "red") +
  geom_point(data = subset(df, k$cluster != -1), color = "blue") +
  labs(title = "Bruit identifié par DBSCAN",
       x = "Variable x", y = "Variable y") +
  theme_minimal()

#K-means (voir TP)###############################################################################################################################################################

within=NULL
for(i in 1:11)
within[i]=sum(kmeans(df,centers=i)$withinss)
plot(1:11, within, type="b")

clust2=kmeans(df, centers=5)
plot(df, col=clust2$cluster, pch=19, cex=2)
abline(h=0, v=0)


#Rajouter des evaluation des clusters du style anova et student

#Methode HCA####################################################################################################################################################################################################



#On va tester HCA avec les methode de regroupement single et ward sur la distance euclidienne et selon les indexe dunn et silouhette

linkage_methods <- c("single", "ward.D2")
results <- list()

silhouette_scores <- numeric(length = 10)
dunn_index <- numeric(length = 10)

for (method in linkage_methods) {
  distance_matrix <- dist(df)
  for (k in 2:10) {
    hc <- hclust(distance_matrix, method = method)
    clusters <- cutree(hc, k = k)
    
    # Calculer le coefficient de silhouette
    silhouette_obj <- silhouette(clusters, distance_matrix)
    silhouette_scores[k] <- mean(silhouette_obj[, "sil_width"])
    
    # Calculer l'indice de Dunn
    dunn_index[k] <- dunn(distance_matrix, clusters)
  }
  
  results[[method]] <- list(
    silhouette = silhouette_scores,
    dunn = dunn_index
  )
}



par(mfrow = c(2, length(linkage_methods)))
for (i in 1:length(linkage_methods)) {
  method <- linkage_methods[i]
  plot(2:10, results[[method]]$silhouette[2:10], type = "b", 
       main = paste("Coefficient de silhouette (", method, ")"), 
       xlab = "Nombre de clusters", ylab = "Score de silhouette")
  plot(2:10, results[[method]]$dunn[2:10], type = "b", 
       main = paste("Indice de Dunn (", method, ")"), 
       xlab = "Nombre de clusters", ylab = "Indice de Dunn")
}


# Trouver le nombre de clusters avec le meilleur coefficient de silhouette
best_silhouette <- sapply(results, function(method) max(method$silhouette))
best_silhouette_method <- names(results)[which.max(best_silhouette)]
best_silhouette_clusters <- which.max(results[[best_silhouette_method]]$silhouette)

# Trouver le nombre de clusters avec le meilleur indice de Dunn
best_dunn <- sapply(results, function(method) max(method$dunn))
best_dunn_method <- names(results)[which.max(best_dunn)]
best_dunn_clusters <- which.max(results[[best_dunn_method]]$dunn)

# Afficher les résultats
cat("Meilleur nombre de clusters selon le coefficient de silhouette:", best_silhouette_clusters,
    "avec la méthode", best_silhouette_method, "\n")
cat("Meilleur nombre de clusters selon l'indice de Dunn:", best_dunn_clusters,
    "avec la méthode", best_dunn_method, "\n")


#Vérification méthode optimale
sim <- cluster.Sim(df,p =7, minClusterNo = 2, maxClusterNo = 10, icq = "S")


# Créer les dendrogrammes avec les clusters optimaux
par(mfrow = c(1, length(linkage_methods)))
for (method in linkage_methods) {
  # Créer le dendrogramme
  hc <- hclust(distance_matrix, method = method)
  plot(hc, main = paste("Dendrogramme (", method, ")"), cex = 0.8, hang = -1)
  
  # Ajouter le rectangle autour du cluster optimal
  rect.hclust(hc, k = best_silhouette_clusters, border = "red")
}


#On va faire pareil pour des metriques adaptées aux HCA, le R^2 semi partiel et le pseudo t^2

# Initialiser une liste pour stocker les résultats SPRSQ
sprsq_values <- numeric(0)

# Effectuer le clustering ascendant pour différents nombres de classes (de 2 à 10)
for (k in 2:10) {
  # Effectuer le clustering ascendant
  hc <- hclust(dist(df), method = "single")
  
  # Couper le dendrogramme pour obtenir le nombre de classes spécifié
  clusters <- cutree(hc, k = k)
  
  # Calculer l'indice SPRSQ
  sprsq <- sum(hc$height[cutree(hc, k = k)]) / length(df)
  
  # Ajouter l'indice SPRSQ à la liste
  sprsq_values <- c(sprsq_values, sprsq)
}



# Initialiser une liste pour stocker les résultats Pseudo-T2
pst2_values <- numeric(0)


# Effectuer le clustering ascendant pour différents nombres de classes (de 2 à 10)
for (k in 2:10) {
  # Effectuer le clustering ascendant
  hc <- hclust(dist(df), method = "single")
  
  # Couper le dendrogramme pour obtenir le nombre de classes spécifié
  clusters <- cutree(hc, k = k)
  
  # Calculer l'indice Pseudo-T2
  mahalanobis_dist <- mahalanobis(df, colMeans(df), cov(df))
  pst2 <- sum(mahalanobis_dist * (clusters - 1)^2)
  
  # Ajouter l'indice Pseudo-T2 à la liste
  pst2_values <- c(pst2_values, pst2)
}

# Tracer les graphiques
par(mfrow = c(1, 2))

# Graphique pour l'indice SPRSQ
plot(2:10, sprsq_values, type = "b", main = "Indice SPRSQ en fonction du nombre de clusters", 
     xlab = "Nombre de clusters", ylab = "Valeur de l'indice SPRSQ")

# Graphique pour l'indice Pseudo-T2
plot(2:10, pst2_values, type = "b", main = "Indice Pseudo-T2 en fonction du nombre de clusters", 
     xlab = "Nombre de clusters", ylab = "Valeur de l'indice Pseudo-T2")


#idem pour l'indice Calinski-Harabasz

# Créer une liste pour stocker les valeurs de l'indice de Calinski-Harabasz
calinski_values <- numeric(0)

# Effectuer le clustering ascendant pour différents nombres de classes (de 2 à 10)
for (k in 2:10) {
  # Effectuer le clustering ascendant
  hc <- hclust(dist(df), method = "ward.D2")
  
  # Couper le dendrogramme pour obtenir le nombre de classes spécifié
  clusters <- cutree(hc, k = k)
  
  # Calculer l'indice de Calinski-Harabasz
  calinski <- calinhara(df, clusters)
  
  # Ajouter l'indice de Calinski-Harabasz à la liste
  calinski_values <- c(calinski_values, calinski)
}

# Tracer le graphique
plot(2:10, calinski_values, type = "b", 
     main = "Indice de Calinski-Harabasz en fonction du nombre de clusters", 
     xlab = "Nombre de clusters", ylab = "Valeur de l'indice Calinski-Harabasz")


# rajouer des anova pour tous ces clusters

#clustersim permet d'evaluer beaucoup de methodes d'un coup et d'optimiser les approches. Je le pose là mais je le supprimerai certainement, il prend trop de temps de calcul.

sim <- cluster.Sim(df,p =7, minClusterNo = 2, maxClusterNo = 10, icq = "S") # le S est la methode d'aggregation single, on peut evidemment mettre ward ou une autre 
print(sim)
#Clustering spectral 
set.seed(123)
k <- 4

# Matrice de similarité
similarity_matrix <- exp(-dist(df)^2)

# Decompo spectrale
vp <- eigen(similarity_matrix)

# Extraction du top k des vp
k_vp <- vp$vectors[, 1:k]

# K means sur les vp
cluster_assignments <- kmeans(k_vp, centers = k)$cluster

# Graphique 
plot(df, col = clusters, pch = 19, 
     main = "Spectral Clustering avec k-means")

Topologie de kohonen simple mais on peut sortir le mot "reseau de neurones" aux solvaysiens#########################################################################################
# Normalisation des données
scaled_df <- scale(df)

# Création du réseau de Kohonen
set.seed(123)  # Pour la reproductibilité
kohonen_network <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# Entrainement du réseau
kohonen_model <- som(scaled_df, grid = kohonen_network, rlen = 10000) # j'ai mis 10 000 mais je n'ai pas le temps de l'optimiser

# Affichage des neurones
plot(kohonen_model)
som_cluster <- cutree(hclust(dist(df)), k = 5)
# Ajouter des étiquettes aux neurones
add.cluster.boundaries(kohonen_model, som_cluster)

plot(kohonen_model, type="mapping", pchs=20, main="Kohonen Map avec les Clusters")


# Ajouter une constante pour rendre les valeurs positives
cluster_assignments_positive <- cluster_assignments + abs(min(cluster_assignments))

# Visualiser les résultats sur un nuage de points avec des couleurs
plot(df, col = cluster_assignments_positive, pch = 20, main = "Nuage de points avec les clusters")
legend("topright", col = unique(cluster_assignments_positive), pch = 20)
