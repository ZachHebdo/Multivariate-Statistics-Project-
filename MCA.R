library(dplyr)
library(robust)
library(tidyverse)
library(dummy)
library(FactoMineR)

#library(MASS)
#detach("package:MASS")

df = read.csv("preprocessedData.csv")

df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies_in_force<- as.factor(df$Policies_in_force)
df$N_doors<- as.factor(df$N_doors)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$N_claims_year <- as.factor(df$N_claims_year)
df$Type_risk <- as.factor(df$Type_risk)
df$N_claims_history <- as.factor(df$N_claims_history)

#df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))

countVar <- function(var) {
    df_qual %>% count({{var}})
}

lapply(df, countVar)

df <- df_qual

### Starting MCA
data=dummy(df, int=TRUE)
data

#Table des profils lignes
row_profiles=data/rowSums(data)
glimpse(row_profiles)

G_l=colSums(data)/sum(data) #centre de gravité
G_l

#Table des profils colonnes
column_profiles=t(t(data)/colSums(data))
column_profiles

n=nrow(df)
G_c=1/n
G_c #centre de gravité
# equal to:
rowSums(data)/sum(data)

#Application de l'analyse des correspondances multiples
mca=CA(data) #ACOBI appliquée à la table disjonctive complète
summary(mca)
plot(mca, cex=1.4)

#rownames(data)<-df$Type_risk #pour afficher les races de chiens
#mca=CA(data)

#Valeurs propres
mca$eig[,1]
barplot(mca$eig[,1])

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

#Qualité de représentation des modalités sur les axes principaux
mca$row$cos2 #pour les profils lignes
mca$col$cos2 #pour les profils colonnes


#Calcul de n_11 pour l'exemple sur les profils colonnes
d=df
T=dummy(d, int=TRUE)
colSums(T)
n_11=colSums(T)[1] # What is she doing here? 
n_11

################################################################################
# Removing all motorcycle observations, and focusing on other data/variables
df2 <- df %>%
    filter(Type_risk !=  "motorbikes") 

df2 <- droplevels(df2)
table(droplevels(df2)$N_doors)

data=dummy(df2, int=TRUE)
glimpse(data)

#Table des profils lignes
row_profiles=data/rowSums(data)
glimpse(row_profiles)

G_l=colSums(data)/sum(data) #centre de gravité
G_l

#Table des profils colonnes
column_profiles=t(t(data)/colSums(data))
column_profiles

n=nrow(df)
G_c=1/n
G_c #centre de gravité
# equal to:
rowSums(data)/sum(data)

#Application de l'analyse des correspondances multiples
mca=CA(data) #ACOBI appliquée à la table disjonctive complète
summary(mca)
plot(mca, cex=1.4)

#rownames(data)<-df$Type_risk #pour afficher les races de chiens
#mca=CA(data)

#Valeurs propres
mca$eig[,1]
barplot(mca$eig[,1])

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

#Qualité de représentation des modalités sur les axes principaux
mca$row$cos2 #pour les profils lignes
mca$col$cos2 #pour les profils colonnes



