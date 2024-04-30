library(dplyr)
library(ggplot2)
library(tidyverse)
library(dummy)
library(FactoMineR)
library(factoextra)
library(questionr)
library(ade4)
library(ggrepel)
library(scatterplot3d)
library(NbClust)

# Function to place multiple plots next to eachother:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

df = read.csv("preprocessedData.csv")

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

# In case we need it in the future:
preprocessed_df <- df

#df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))
glimpse(df_qual)

###############################################################################
# Chi-square test of independence to each combination of variables: 

P_value <- data.frame(row.names = names(df_qual))
Chi_statistic <- data.frame(row.names = names(df_qual))
for (i in 1:dim(df_qual)[2]) {
    for (j in i:dim(df_qual)[2]) {
        v <- chisq.test(df_qual[,i], df_qual[,j])
        Chi_statistic[i, j] <- round(v$statistic, 3)
        P_value[i, j] <- round(v$p.value, 3)
    }
}
colnames(P_value) <- names(df_qual)
colnames(Chi_statistic) <- names(df_qual)
P_value <- P_value %>% replace(is.na(.), " ")
Chi_statistic <- Chi_statistic %>% replace(is.na(.), " ")
P_value <- P_value %>% replace(.<0.05, " ")
P_value
Chi_statistic

# We fail to reject independence for 23 out of 66 possible combinations between the 12 variables.   
################################################################################
# Counting the variables
countVar <- function(var) {
    df_qual %>% count({{var}})
}

lapply(df, countVar)

df <- df_qual

### Starting MCA
data=dummy(df, int=TRUE)
data

# Number of modalities (28)
K <- length(names(data))
# Number of variables (12)
P <- dim(df)[2] 

cutoff = 100/P

#Table des profils lignes
row_profiles=data/rowSums(data)
glimpse(row_profiles)

G_l=colSums(data)/sum(data) #centre de gravité
G_l

#Table des profils colonnes
column_profiles=as.data.frame(t(t(data)/colSums(data)))
glimpse(column_profiles)

n=nrow(df)
G_c=1/n
G_c #centre de gravité
# equal to:
rowSums(data)/sum(data)

# MCA 
mca1=MCA(df, ncp = 16, graph = FALSE)

mca2=MCA(df, ncp = 5, graph = FALSE, method = "Burt")
burt = acm.burt(df, df)

# Verifying that the Burt table is equal to XtX.
all(t(as.matrix(data)) %*% as.matrix(data) == burt)

eigenBurt = get_eigenvalue(mca2)
eigenCDT = get_eigenvalue(mca1)

# On vérifie que les valeurs propres en utilisent la table de Burt sont les carrées des valeurs propres de la table disjonctive complète.
all(near(eigenBurt[,1], eigenCDT[,1]^2))

# On vérifie qu'on doit garder 4 dimensions, considerant le cutoff de 1/P.
fviz_screeplot(mca1, ncp = 16, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red") + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 15))

p1 <- fviz_mca_var(mca1, geom = c("point", "text"), axes = c(2, 1), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 15))

p2 <- fviz_mca_var(mca1, geom = c("point", "text"), axes = c(1, 3), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 15))
p1
p2
multiplot(p1, p2, cols=1)

mca1$var$coord[,1:3] #Coordonnées des modalités sur les axes principaux
mca1$var$contrib[,1:3] #Contribution des modalités à la construction des axes principaux
mca1$var$cos2[,1:3] #Qualité de représentation des modalités sur les axes principaux
mca1$var$eta2[,1:3] # Correlation au carré entre les VARIABLES et les axes principaux 

v0 <- fviz_screeplot(mca1, ncp = 16, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red") + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20))
v1 <- fviz_contrib(mca1, choice = "var", axes = 1, top = 5)+ 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 1") + coord_fixed(ratio = 1/2) 
v2 <- fviz_contrib(mca1, choice ="var", axes = 2, top = 5)+ 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 2") + coord_fixed(ratio = 2/5) 
v3 <- fviz_contrib(mca1, choice ="var", axes = 3, top = 5)+ 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 3") + coord_fixed(ratio = 7/16) 
v0
v1
v2
v3

multiplot(v1, v2, v3, cols=3) 

c0 <- fviz_cos2(mca1, choice = "var", axes = 1, top = 5) + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 1") + ylab("Cos2") + coord_fixed(ratio = 4/1)
c1 <- fviz_cos2(mca1, choice ="var", axes = 2, top = 5)+ 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 2") + ylab("Cos2") + coord_fixed(ratio = 7/1)
c2 <- fviz_cos2(mca1, choice ="var", axes = 3, top = 5) + 
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 20)) + ggtitle("Dim. 3") + ylab("Cos2") + coord_fixed(ratio = 7/1)
c0
c1
c2
multiplot(c0, c1, c2, cols=3)
###############################################################################
# Correlation circle (only positive values, since #eta2 is the correlation squared)

fviz_mca_var(mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca1, choice = "mca.cor", axes = c(1, 3),
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# Distance between modalities
#------------------------------------
# Table Disjonctive Complete
CDT = acm.disjonctif(df)

npl <- colSums(CDT)         # same as: apply(CDT, 2, sum)

Mod_dist <- function(mod1, mod2) {
    (n*sum((CDT[,mod1]/npl[mod1] - CDT[,mod2]/npl[mod2])^2))^0.5
}
dist_df <- data.frame(row.names = (names(CDT)))
for (i in 1:dim(CDT)[2]) {
    for (j in i:dim(CDT)[2]) {
        dist_df[names(CDT)[i], names(CDT)[j]] <- round(Mod_dist(i, j), 2)
    }
}
dist_df <- dist_df %>% replace(is.na(.), " ")
dist_df

Dist_CP_Gc <- n/npl - 1
Dist_CP_Gc

################################################################################
# CLUSTERING SUR LES SCORES DU MCA
df2 <- df_qual

mca3=MCA(df2, ncp = 5, graph = FALSE)

fviz_mca_var(mca3, geom = c("point", "text"), axes = c(1, 2), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

scores <- mca3$ind$coord[,1:3]
d=dist(scores, method="euclidean") #matrice des distances euclidiennes
clust=hclust(d, method="ward.D2") #clustering hiérarchique ascendant basé sur le type de lien simple
plot(clust) #dendogramme
rect.hclust(clust, k=2, border="red") #supposons que l'on veuille k=2 clusters

scores <- cbind(scores, Cost_claims_year = preprocessed_df$Cost_claims_year, Premium = preprocessed_df$Premium)

groups=cutree(clust, k=2) #appartenance de chaque ménage français à l'un des trois clusters
# scores[groups==1,] 
# scores[groups==2,]

# anova(lm(as.data.frame(scores)$Cost_claims_year~groups))
# anova(lm(as.data.frame(scores)$Premium~groups))

select=(groups==1|groups==2) #opérateur logique "ou"
test=t.test(as.data.frame(scores)$Cost_claims_year[select]~groups[select])
test

test=t.test(as.data.frame(scores)$Premium[select]~groups[select])
test

# Uses 30 different indices and choses the optimal number of clusters based on which cluster is picked most by the indices.
# Majority rule leads to the choice of 4 clusters (Uncomment to run - SLOW): 
#### nb <- NbClust(scores, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

## For KMeans, we keep the amount of clusters for which the intragroup variance begins to stabilise (variance within each group becomes stable): 
scores <- mca3$ind$coord[,1:3]
within=NULL
for(i in 1:10) within[i]=sum(kmeans(scores,centers=i)$withinss)
plot(1:10, within, type="b")
abline(v = 4, col = "blue", lty = 2)
#Nombre de clusters retenu : 4

#Graphique du premier plan factoriel et de l'appartenance aux clusters
clust2=kmeans(scores, 4) #k-means avec 5 clusters
fviz_cluster(clust2, scores[,c(1,2)], geom = "point")
fviz_cluster(clust2, scores[,c(1,3)], geom = "point")
fviz_cluster(clust2, scores[,c(2,3)], geom = "point")

scatterplot3d(scores, y=NULL, z=NULL, pch = 20, color = clust2$cluster)

scores <- as.data.frame(cbind(scores, Cost_claims_year = preprocessed_df$Cost_claims_year, Premium = preprocessed_df$Premium))

boxplot(scores$Cost_claims_year~clust2$cluster, main='Boxplot of Cost of claims in the year for each cluster')
boxplot(scores$Premium~clust2$cluster, main='Boxplot of Premium for each cluster')

anova(lm(scores$Cost_claims_year~clust2$cluster))
anova(lm(scores$Premium~clust2$cluster))

select=(clust2$cluster==1|clust2$cluster==2) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

select=(clust2$cluster==1|clust2$cluster==3) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

select=(clust2$cluster==1|clust2$cluster==4) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

select=(clust2$cluster==2|clust2$cluster==3) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

select=(clust2$cluster==2|clust2$cluster==4) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

select=(clust2$cluster==3|clust2$cluster==4) #opérateur logique "ou"
test=wilcox.test(as.data.frame(scores)$Cost_claims_year[select]~clust2$cluster[select])
test
test=wilcox.test(as.data.frame(scores)$Premium[select]~clust2$cluster[select])
test

# Since we saw that Premium was a very variable heavily affected by outliers, we decided to do a Kruskal Wallis test. 
# This concludes that the clusters do differ in terms of Premium, while they do not differ for the Cost_claims_year (when accounting for outliers).
kruskal.test(scores$Cost_claims_year~clust2$cluster, data = as.data.frame(scores))
kruskal.test(scores$Premium~clust2$cluster, data = as.data.frame(scores))

################################################################################
# USING THE LOG INSTEAD (NO SIGNIFICANT DIFFERENCE TO TEST RESULTS):
boxplot(log(scores$Cost_claims_year)~clust2$cluster, main='Boxplot of Cost of claims in the year for each cluster')
boxplot(log(scores$Premium)~clust2$cluster, main='Boxplot of Premium for each cluster')

anova(lm(log(scores$Cost_claims_year)~clust2$cluster))
anova(lm(log(scores$Premium)~clust2$cluster))

# Since we saw that Premium was a very variable heavily affected by outliers, we decided to do a Kruskal Wallis test. 
# This concludes that the clusters do differ in terms of Premium, while they do not differ for the Cost_claims_year (when accounting for outliers).
kruskal.test(log(scores$Cost_claims_year)~clust2$cluster, data = as.data.frame(scores))
kruskal.test(log(scores$Premium)~clust2$cluster, data = as.data.frame(scores))
################################################################################
# Last plots:
fviz_mca_biplot(mca3, label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_mca_biplot(mca3, axes = c(1, 3), label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))
fviz_mca_biplot(mca3, axes = c(2, 3), label = "var", repel = TRUE, alpha.ind = 0.15, col.var = "grey52", habillage = as.factor(clust2$cluster))

# #function to visualize the correlation circle 
# corr_circle = function(x, y) {
#     # Create a data frame from the vectors
#     dftest = data.frame(PCx = x, PCy = y, Labels = colnames(df_qual))
#     
#     # Determine the names for the PC axes dynamically based on input
#     x_name = deparse(substitute(x))
#     y_name = deparse(substitute(y))
#     
#     # Create the ggplot object
#     p = ggplot(dftest, aes(x = PCx, y = PCy)) +
#         geom_hline(yintercept = 0) +
#         geom_vline(xintercept = 0) +
#         geom_point() +
#         geom_text_repel(aes(label = Labels)) +
#         annotate("path", x = cos(seq(0, 2 * pi, length.out = 100)), 
#                  y = sin(seq(0, 2 * pi, length.out = 100)), colour = "black") +
#         coord_fixed(ratio = 1) +
#         xlim(-1, 1) +
#         ylim(-1, 1) +
#         labs(x = x_name, y = y_name) +
#         theme_minimal()
#     
#     # Return the plot
#     return(p)
# }

# cor <- sqrt(mca1$var$eta2)
# corr_circle(cor[,1], cor[,2])

df$cluster <- clust2$cluster

# Now, count categories of Diesel within each cluster
category_counts <- df %>%
    group_by(cluster, Diesel) %>%
    summarise(Count = n(), .groups = 'drop')

# Display the counts
print(category_counts)

category_counts <- df %>%
    group_by(cluster, newClient) %>%
    summarise(Count = n(), .groups = 'drop')

# Display the counts
print(category_counts)

category_counts <- df %>%
    group_by(cluster, Type_risk) %>%
    summarise(Count = n(), .groups = 'drop')

# Display the counts
print(category_counts)

category_counts <- df %>%
    group_by(cluster, Second_driver) %>%
    summarise(Count = n(), .groups = 'drop')

# Display the counts
print(category_counts)

category_counts <- df %>%
    group_by(cluster, Age) %>%
    summarise(Count = n(), .groups = 'drop')

# Display the counts
print(category_counts)


################################################################################
#  Calculating eigenvalues manually (Just for checking)
element <- function(df, i, j) {
    t <- (df[i, j] - npl[j]/n)/sqrt(P*npl[j])
    
    return(t)
}

T <- matrix(nrow = dim(CDT)[1], ncol = dim(CDT)[2])
for (i in 1:dim(CDT)[1]) {
    for (j in 1:dim(CDT)[2]) {
        T[i, j] <- element(CDT, i, j)        
    }
}

V <- t(T) %*% T
W <- T %*% t(T)

eigenV <- eigen(V)
# eigenW <- eigen(W)

round(eigenV$values/sum(eigenV$values), 3)
all(near(eigenV$vectors %*% diag(eigenV$values) %*% t(eigenV$vectors), V))

# Verifying that the eigenvalues are correctly calculated for all K-P relevant dimensions. Thus, T is correctly calculated as well.
all(near(eigenV$values[1:(K-P)], mca1$eig[1:(K-P),1]))

#################################################################################
### Inspect if there's any interesting relation between the qualitative variables and the Cost_claims_year variable: 
df3 <- preprocessed_df %>% select(where(~ !is.numeric(.)) | "Cost_claims_year")
df3 <- df3 %>% mutate(Cost_claims_year = quant.cut(Cost_claims_year, 4))

K2 <- length(names(dummy(df3))) # Number of modalities (28)
P2 <- dim(df3)[2] # Number of variables (12)

cutoff = 100/P2
mca_extra=MCA(df2, ncp = 5, graph = FALSE)

fviz_screeplot(mca_extra, ncp = 20, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red")

fviz_mca_var(mca_extra, geom = c("point", "text"), axes = c(1, 2), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Nothing particularly interesting. The contribution of the modalities of Cost_claims_year variable is very low in the first 2 dimensions.

