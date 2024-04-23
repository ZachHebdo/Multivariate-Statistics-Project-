#PCA analysis
library(dplyr)

database = open.csv("preprocessedData.csv")
database$newClient<-as.factor(database$newClient)
database$Broker <- as.factor(database$Broker)
database$Lapse <- as.factor(database$Lapse)
database$Policies_in_force<- as.factor(database$Policies_in_force)
database$N_doors<- as.factor(database$N_doors)
database$Urban <- as.factor(database$Urban)
database$Diesel <- as.factor(database$Diesel)
database$Payment <- as.factor(database$Payment)
database$Second_driver <- as.factor(database$Second_driver)
database$N_claims_year <- as.factor(database$N_claims_year)



db_quant <- database %>% select(where(is.numeric))
db_qual <- database %>% select(where(~ !is.numeric(.)))

dbqual
#descriptive analysis 
summary(db_quant)
boxplot(db_quant)
cov(db_quant)

# check if needed to scale 
apply(db_quant,2,mean) # we have to scale 

#standardizing it 
data_st = as.data.frame(scale(db_quant))

#Calculate PCA using corr matrix 
corr_matrix = cor(db_quant)
print(corr_matrix)
PCA = eigen(corr_matrix)
PCA$values 
PCA$vectors

#plotting and looks like we should keep around 4 dimensions 
plot(PCA$values, type='line')

#double checking with pca function 
pca = princomp(data_st, corr=TRUE)
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



library(robust)

# Apply robust PCA on your dataset
robust_cov <- covRob(db_quant, corr = TRUE, estim = 'weighted')
PCA_robust <- eigen(robust_cov$cov)
PCA_robust$vectors
PCA_robust$values

plot(PCA_robust$values, type='l')


inertie_perc_robuste=100*PCA_robust$values/sum(PCA_robust$values)
cumsum(inertie_perc_robuste)






