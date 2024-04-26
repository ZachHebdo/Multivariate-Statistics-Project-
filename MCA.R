library(dplyr)
library(robust)
library(tidyverse)
library(dummy)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(questionr)

#library(MASS)
#detach("package:MASS")

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
K <- length(names(data)) # Number of modalities (28)
P <- dim(df)[2] # Number of variables (12)
#cutoff = 100/K
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
mca2=MCA(df, ncp = 5, graph = FALSE)
mca2=MCA(df, ncp = 5, graph = FALSE, method = "Burt")
burt = acm.burt(df, df)
afc_burt2 = CA(burt, graph = FALSE)
# Attraction/Repulsion -> Coord? 

fviz_ca_col(afc_burt2, geom = c("point", "text"), axes = c(1, 2), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_screeplot(afc_burt2, ncp = 10, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red")

get_mca_var(mca2)

get_eigenvalue(mca2)

get_mca_var(mca2)$coord
get_mca_var(mca2)$cos2
get_mca_var(mca2)$contrib

cutoff = 100/K
cutoff = 100/P

fviz_screeplot(mca2, ncp = 8, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red")

fviz_mca_var(mca2, geom = c("point", "text"), axes = c(1, 2), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_mca_var(mca2, geom = c("point", "text"), axes = c(1, 3), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_mca_var(mca2, geom = c("point", "text"), axes = c(1, 4), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Coordonnées des modalités sur les axes principaux
mca2$var$coord[,1:4]
mca2$var$eta2[,1:4]
mca2$var$contrib[,1:4] #Contribution des modalités à la construction des axes principaux
mca2$var$cos2[,1:4] #Qualité de représentation des modalités sur les axes principaux
mca2$var$v.test[,1:4] 

# First axis:
round(mca2$var$contrib[,1], 2)
round(mca2$var$contrib[,1]/sum(mca2$var$contrib[,1]), 2)

summary(mca2)

# Si on veut garder l'observations avec les nouveaux valeurs.
qual_quant <- mca2$ind$coord

# So second dimension is mainly composed of Motorcycle (vs vans) and gasoline (vs diesel), and young adult young driver
fviz_contrib(mca2, choice = "var", axes = 1)
fviz_contrib(mca2, choice ="var", axes = 2)
fviz_contrib(mca2, choice ="var", axes = 3)
fviz_contrib(mca2, choice ="var", axes = 4)

fviz_contrib(mca2, choice ="ind", axes = 1, top = 30)
fviz_contrib(mca2, choice ="ind", axes = 2, top = 30)

fviz_cos2(mca2, choice = "var", axes = 1)
fviz_cos2(mca2, choice = "var", axes = 2)
fviz_cos2(mca2, choice = "var", axes = 3)
fviz_cos2(mca2, choice = "var", axes = 4)


# Distance between modalities
#------------------------------------
# Calcul des effectif de chaque modalit´e pour le calcul des distances
library(ade4)

tdc = acm.disjonctif(df)

npl = NULL
for (i in 1: dim(tdc)[2]){
    npl[i] = sum(tdc[,i])
}
# Calcul des distances entre modalit´es
dist = matrix(nrow = dim(tdc)[2], ncol = dim(tdc)[2])
for (i in 1: dim(tdc)[2]){
    for (j in 1:dim(tdc)[2]){
        a = tdc[,i]/npl[i]
        b = tdc[,j]/npl[j]
        c = (a-b)^2
        dist[i,j] = sqrt(dim(df)[1]*sum(c))
    }
}
distAff = round(dist,3) # Affichage ´epur´e des distances

### Inspect if there's any interesting relation between the qualitative variables and the Cost_claims_year variable: 
df2 <- df %>% select(where(~ !is.numeric(.)) | "Cost_claims_year")
df2 <- df2 %>% mutate(Cost_claims_year = quant.cut(Cost_claims_year, 4))

K <- length(names(dummy(df2))) # Number of modalities (28)
P <- dim(df2)[2] # Number of variables (12)

cutoff = 100/P
mca2=MCA(df2, ncp = 5, graph = FALSE, method = "Burt")

fviz_screeplot(mca2, ncp = 20, addlabels = TRUE, ylim = c(0, 30)) + 
    geom_hline(yintercept=cutoff, linetype=2, color="red")

fviz_mca_var(mca2, geom = c("point", "text"), axes = c(1, 2), repel = TRUE, shape.var = 19, alpha = 0.7, 
             col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


# Nothing particularly interesting. The contribution of all the modalities of Cost_claims_year is very low to the first 2 dimensions.
