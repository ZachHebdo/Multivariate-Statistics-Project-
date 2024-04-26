library(dplyr)
library(robust)
library(tidyverse)
library(dummy)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(questionr)
library(ade4)

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
mca1=MCA(df, ncp = 5, graph = FALSE)
mca2=MCA(df, ncp = 5, graph = FALSE, method = "Burt")
burt = acm.burt(df, df)

eigenBurt = get_eigenvalue(mca2)
eigenCDT = get_eigenvalue(mca1)

# On vérifie que les valeurs propres en utilisent la table de Burt sont les carrées des valeurs propres de la table disjonctive complète.
all(near(eigenBurt[,1], eigenCDT[,1]^2))

# On vérifie qu'on doit garder 4 dimensions, considerant le cutoff de 1/P.
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
# Table Disjonctive Complete
CDT = acm.disjonctif(df)

npl <- colSums(CDT)         # same as: apply(CDT, 2, sum)

dist <- function(mod1, mod2) {
    (n*sum((CDT[,mod1]/npl[mod1] - CDT[,mod2]/npl[mod2])^2))^0.5
}
dist_df <- data.frame(row.names = (names(CDT)))
for (i in 1:dim(CDT)[2]) {
    for (j in i:dim(CDT)[2]) {
        dist_df[names(CDT)[i], names(CDT)[j]] <- round(dist(i, j), 2)
    }
}
dist_df <- dist_df %>% replace(is.na(.), " ")
dist_df

Dist_CP_Gc <- n/npl - 1
Dist_CP_Gc

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
