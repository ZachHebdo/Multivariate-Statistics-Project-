library(dplyr)
library(robust)
library(tidyverse)
library(dummy)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)

#library(MASS)
#detach("package:MASS")

df = read.csv("preprocessedData.csv")

df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies_in_force<- as.factor(df$Policies_in_force)
#df$N_doors<- as.factor(df$N_doors)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
#df$N_claims_year <- as.factor(df$N_claims_year)
df$Type_risk <- as.factor(df$Type_risk)
#df$N_claims_history <- as.factor(df$N_claims_history)
df$N_claims <- as.factor(df$N_claims)
df$Licence_time <- as.factor(df$Licence_time)
df$Age <- as.factor(df$Age)

#df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))
glimpse(df_qual)

countVar <- function(var) {
    df_qual %>% count({{var}})
}

lapply(df, countVar)

df <- df_qual

### Starting MCA
data=dummy(df, int=TRUE)
data
col <- names(data)

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
#mca=CA(data, graph = FALSE)  # MCA appliquée à la table disjonctive complète

mca2=MCA(df, graph = FALSE)  # MCA appliquée à la table disjonctive complète
plot.MCA(mca2, invisible=c("ind","quali.sup"), cex=0.7)
summary(mca2)

#mca2 = MCA(df, ncp = 9, method = "Burt", graph = FALSE) # Using the Burt Table, since we have a lot of observations
mca2$var$cos2
mca2$var$contrib
mca2$var$coord
mca2$var$coord

# Si on veut garder l'observations avec le nouveaux valeurs.
mca2$ind$coord

cutoff = 100/length(col)
get_eigenvalue(mca2)
fviz_screeplot(mca2, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red")

fviz_mca_var(mca2, geom = c("point", "text"), shape.var = 19, alpha = 1)
    
#Valeurs propres
mca2$eig[,1]
barplot(mca2$eig[,1])

#Pourcentage d'inertie expliquée par les axes principaux
mca$eig[,2:3]

#Vecteurs propres
mca$svd$U #pour les profils lignes
mca$svd$V #pour les profils colonnes


#Coordonnées des modalités sur les axes principaux
mca$row$coord #pour les profils lignes
mca$col$coord #pour les profils colonnes

#Contribution des modalités à la construction des axes principaux
mca$row$contrib #pour les profils lignes
mca$col$contrib #pour les profils colonnes

# So second dimension is mainly composed of Motorcycle (vs vans) and gasoline (vs diesel)

#Qualité de représentation des modalités sur les axes principaux
mca$row$cos2 #pour les profils lignes
mca$col$cos2 #pour les profils colonnes

#Calcul de n_11 pour l'exemple sur les profils colonnes
d=df
T=dummy(d, int=TRUE)
colSums(T)
n_11=colSums(T)[1] # What is she doing here? 
n_11


fviz_contrib(mca2, choice = "var", axes = 1)
fviz_contrib(mca2, choice ="var", axes = 2)
fviz_contrib(mca2, choice ="var", axes = 3)
fviz_contrib(mca2, choice ="var", axes = 4)
fviz_contrib(mca2, choice ="var", axes = 5)
