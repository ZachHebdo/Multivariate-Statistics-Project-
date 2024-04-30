#PCA analysis
library(dplyr)
library(robust)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(ggfortify)
library(grid)
library(NbClust)

#function to visualize the correlation circle 
plot_pca_circle = function(x, y) {
    # Create a data frame from the vectors
    dftest = data.frame(PCx = x, PCy = y, Labels = colnames(df_quant))
    
    # Determine the names for the PC axes dynamically based on input
    x_name = deparse(substitute(x))
    y_name = deparse(substitute(y))
    
    # Create the ggplot object
    p = ggplot(dftest, aes(x = PCx, y = PCy)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_point() +
        geom_text_repel(aes(label = Labels)) +
        annotate("path", x = cos(seq(0, 2 * pi, length.out = 100)), 
                 y = sin(seq(0, 2 * pi, length.out = 100)), colour = "black") +
        coord_fixed(ratio = 1) +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(x = x_name, y = y_name) +
        theme_minimal()
    
    # Return the plot
    return(p)
}

#fonction to visualize the biplot 
plot_biplot <- function(comp1, comp2, df_scores, df_loadings, scale_factor = 12) {
    # Convert column indices to names if numeric indices are provided
    if (is.numeric(comp1)) comp1 <- names(df_scores)[comp1]
    if (is.numeric(comp2)) comp2 <- names(df_scores)[comp2]
    
    ggplot(df_scores, aes_string(x = comp1, y = comp2, color = "typerisk")) +
        geom_point(alpha = 0.7) +  # Add points
        theme_minimal() +
        geom_segment(data = df_loadings, aes_string(x = "0", y = "0", 
                                                    xend = paste(comp1, "* scale_factor"), 
                                                    yend = paste(comp2, "* scale_factor")), 
                     arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") +
        labs(title = "Basic PCA Biplot", x = comp1, y = comp2) +
        geom_text_repel(data = df_loadings, aes_string(x = paste(comp1, "* scale_factor"), 
                                                       y = paste(comp2, "* scale_factor"), label = "Label"), 
                        color = "black", size = 3)
}

#function to create graphs to see contribution to each component 
plot_pca_contribution <- function(loadings, dimension, palette = "Blues") {
    # Calculate contributions for the specified dimension
    contributions <- (loadings[, dimension]^2 / sum(loadings[, dimension]^2)) * 100
    
    # Create a data frame for the contributions
    df_contribution <- data.frame(
        Variables = loadings$Label,
        Contributions = contributions
    )
    
    # Order the data frame by contributions in descending order to get the descending order bar 
    df_contribution <- df_contribution[order(-df_contribution$Contributions),]
    
    # Create the plot
    p <- ggplot(df_contribution, aes(x = reorder(Variables, -Contributions), y = Contributions, fill = Variables)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste("Contribution of columns to Dim-", dimension, sep=""),
             y = "Contributions (%)", x ="Variables") +
        geom_hline(yintercept = mean(df_contribution$Contributions), linetype="dashed", color = "red") +
        scale_fill_brewer(palette = palette, direction = -1) # Optional color scale
    
    return(p)
}

df = read.csv("preprocessedData.csv")

#df$N_claims_history <- as.factor(df$N_claims_history)
#df$N_claims_year <- as.factor(df$N_claims_year)
#df$N_doors<- as.factor(df$N_doors)
df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies<- as.factor(df$Policies)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$Type_risk <- as.factor(df$Type_risk)
df$N_claims <- as.factor(df$N_claims)
df$Licence <- as.factor(df$Licence)
df$Age <- as.factor(df$Age)

df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))

#descriptive analysis 
summary(df_quant)
boxplot(df_quant)
cov(df_quant)

# check if needed to scale 
apply(df_quant,2,mean) # we have to scale 

#standardizing it 
# data_st = as.data.frame(scale(db_quant))

#Calculate PCA using corr matrix 
corr_matrix = cor(df_quant)
print(corr_matrix)
PCA = eigen(corr_matrix)
PCA$values 
PCA$vectors

#plotting and looks like we should keep around 4 dimensions 
plot(PCA$values, type='line')

#double checking with pca function 
pca = princomp(df_quant, cor=TRUE)
attributes(pca)
biplot(pca)
eigenvectors=pca$loadings
eigenvalues=pca$sdev^2.
pca$scores[,1:2]
#have to do this to have same direction of graph 
PCA$vectors[,3] = -1 * PCA$vectors[,3]
PCA$vectors[,2] = -1 * PCA$vectors[,2]
PCA$vectors[,1] = -1 * PCA$vectors[,1]

#normal way to calculate scores 
scores = scale(df_quant) %*% PCA$vectors 
plot(scores[,1:2])


#calculating percentage of inertie explicated 
inertie_perc=100*PCA$values/sum(PCA$values)
cumsum(inertie_perc)

#calculating percentage of inertie explicated 
perc_explained = PCA$values / sum(PCA$values) * 100

#dataframe for ggplot 
pca_df = data.frame(
  Dimension = 1:length(PCA$values),
  VarianceExplained = perc_explained
)

# Now create the screeplot 
ggplot(pca_df, aes(x = Dimension, y = VarianceExplained)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = sprintf("%.1f%%", VarianceExplained)), vjust = -0.3, size = 3.5) +
  geom_line(aes(y = perc_explained), group = 1, colour = "black") +
  geom_point(aes(y = perc_explained), colour = "black") +
  theme_minimal() +
  labs(x = "Dimensions", y = "Percentage of Explained Variance", 
       title = "Scree Plot for the non-robust PCA") +
  scale_x_continuous(breaks = 1:length(PCA$values)) # Ensure all dimension labels are shown


#calculating correlations for the first 4 components 
cor1 = sqrt(PCA$values[1])*PCA$vectors[,1]
cor2 = sqrt(PCA$values[2])*PCA$vectors[,2]
cor3 = sqrt(PCA$values[3])*PCA$vectors[,3]
cor4 = sqrt(PCA$values[4])*PCA$vectors[,4]

#graph of interpretation 
plot_pca_circle(cor1, cor2)
plot_pca_circle(cor2, cor3)
plot_pca_circle(cor3, cor4)

scores = as.data.frame(scores)
colnames(scores) = paste("PC", 1:7, sep = "")
typerisk = df$Type_risk
scores$Typerisk = typerisk
ndoors = df$N_doors
scores$ndoors = ndoors

loadings = as.data.frame(PCA$vectors) 
loadings$variable = colnames(df_quant)
colnames(loadings) = paste("PC", 1:7, sep = "")
colnames(loadings)[8] = "Label"

#biplots for the different non-robust pca components 
plot_biplot("PC1", "PC2", scores, loadings)
plot_biplot("PC1", "PC3", scores, loadings)
plot_biplot("PC2", "PC3", scores, loadings)

#plotting the contributions to each dimension of each variable 
plot_pca_contribution(loadings, 1)
plot_pca_contribution(loadings, 2)
plot_pca_contribution(loadings, 3)


##            ROBUST PCA 
##
# Apply robust PCA on your dataset (If there are less than 50000 observations and less than 20 variables then the MCD is used.)
robust_cov =  covRob(df_quant, corr = TRUE, estim = 'weighted')
PCA_robust = eigen(robust_cov$cov)
PCA_robust$values

#plotting and looks like we should keep around 3 or 4 dimensions 
plot(PCA_robust$values, type='l')

#calculating percentage of inertie explicated 
perc_explained_robust = PCA_robust$values / sum(PCA_robust$values) * 100

#dataframe for ggplot 
pca_df_robust = data.frame(
  Dimension = 1:length(PCA_robust$values),
  VarianceExplained = perc_explained_robust)

# Now create the screeplot 
ggplot(pca_df_robust, aes(x = Dimension, y = VarianceExplained)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  geom_text(aes(label = sprintf("%.1f%%", VarianceExplained)), vjust = -0.3, size = 3.5) +
  geom_line(aes(y = perc_explained_robust), group = 1, colour = "black") +
  geom_point(aes(y = perc_explained_robust), colour = "black") +
  theme_minimal() +
  labs(x = "Dimensions", y = "Percentage of Explained Variance", 
       title = "Scree Plot for the Robust PCA") +
  scale_x_continuous(breaks = 1:length(PCA_robust$values)) # Ensure all dimension labels are shown

#calculating correlations for the first 4 components 
cor_rob1 = sqrt(PCA_robust$values[1])*PCA_robust$vectors[,1]
cor_rob2 = sqrt(PCA_robust$values[2])*PCA_robust$vectors[,2]
cor_rob3 = sqrt(PCA_robust$values[3])*PCA_robust$vectors[,3]

# Plot correlation circle 
plot_pca_circle(cor_rob1, cor_rob2)
plot_pca_circle(cor_rob2, cor_rob3)
plot_pca_circle(cor_rob1, cor_rob3)

#calculating scores 
PCA_robust$vectors[,1] = -1 * PCA_robust$vectors[,1]
PCA_robust$vectors[,2] = -1 * PCA_robust$vectors[,2]

scores_robust = scale(df_quant) %*% PCA_robust$vectors 

### IDEA: Keep three principal components, to have a decent amount of variance explained. 

scores_robust = as.data.frame(scores_robust)
colnames(scores_robust) = paste("PC_robust", 1:7, sep = "")
typerisk = df$Type_risk
scores_robust$Typerisk = typerisk

loadings_robust = as.data.frame(PCA_robust$vectors) 
loadings_robust$variable = colnames(df_quant)
colnames(loadings_robust) = paste("PC_robust", 1:7, sep = "")
colnames(loadings_robust)[8] = "Label"

#plotting the biplot of the robust PCA
plot_biplot("PC_robust1", "PC_robust2", scores_robust, loadings_robust)
plot_biplot("PC_robust1", "PC_robust3", scores_robust, loadings_robust)
plot_biplot("PC_robust2", "PC_robust3", scores_robust, loadings_robust)

#plotting the contributions to each dimension of each variable 
plot_pca_contribution(loadings_robust, 1)
plot_pca_contribution(loadings_robust, 2)
plot_pca_contribution(loadings_robust, 3)

# Clustering PCA  #################################### 

scores_pca <- scores_robust[,1:3]

df_quant <- df %>% select(where(is.numeric))
#df_qual <- df %>% select(where(~ !is.numeric(.)))

# FOR FINDING THE OPTIMAL NUMBER OF CLUSTERS (SLOW):
# nb <- NbClust(scores_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

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
fviz_pca_biplot(pca_robust, label = "var", repel = TRUE, alpha.ind = 0.5, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_pca_biplot(pca_robust, axes = c(1, 3), label = "var", repel = TRUE, alpha.ind = 0.5, col.var = "grey52", habillage = as.factor(clust2$cluster))

# scatterplot3d(pca_robust$scores[,1:3], y=NULL, z=NULL, pch = 20, color = clust2$cluster)

