#PCA analysis
library(dplyr)
library(robust)
library(tidyverse)

# library(MASS)

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

#calculating 
inertie_perc=100*PCA$values/sum(PCA$values)
cumsum(inertie_perc)

#cor1=pca$sdev[1]*eigenvectors[,1]
#cor2=pca$sdev[2]*eigenvectors[,2]

#plot(cor1, cor2, xlim=c(-1,1), ylim=c(-1,1))
#abline(h=0, v=0)
#symbols(0, 0, circles=1, inches=F, add=T)
#identify(cor1, cor2, labels=colnames(db_quant))

# Apply robust PCA on your dataset (If there are less than 50000 observations and less than 20 variables then the MCD is used.)
robust_cov <- covRob(df_quant, corr = TRUE, estim = 'weighted')
PCA_robust <- eigen(robust_cov$cov)
PCA_robust$vectors
PCA_robust$values

plot(PCA_robust$values, type='l')

inertie_perc_robuste=100*PCA_robust$values/sum(PCA_robust$values)
cumsum(inertie_perc_robuste)
plot(cumsum(inertie_perc_robuste), type='l')

### To discuss: Keep only three principal components, to favour interpretability of the dataset.  

### USING MASS and princomp. Virtually no difference. 
# library(MASS)
# mcd <- cov.mcd(df_quant)
# pca_robust = princomp(df_quant, cor = TRUE, covmat = mcd)
# inertie_perc_robuste2 = 100*pca_robust$sdev^2/sum(pca_robust$sdev^2)
# cumsum(inertie_perc_robuste2)
# detach("package:MASS")

