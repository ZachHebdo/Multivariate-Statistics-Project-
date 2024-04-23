#PCA analysis
quant_var = database[c("Cost_claims_year","Power","Cylinder_capacity", 
                       "Value_vehicle","Length", "Weight")]


#descriptive analysis 
summary(quant_var)
boxplot(quant_var)
cov(quant_var)

# check if needed to scale 
apply(quant_var,2,mean) # we have to scale 

#standardizing it 
data_st = as.data.frame(scale(quant_var))

corr_matrix = cor(quant_var)
print(corr_matrix)
PCA = eigen(corr_matrix)
PCA$values 
PCA$vectors

plot(PCA$values, type='line')


pca = princomp(data_st, corr=TRUE)
attributes(pca)



inertie_perc=100*PCA$values/sum(PCA$values)
cumsum(inertie_perc)






cor1=pca$sdev[1]*eigenvectors[,1]
cor2=pca$sdev[2]*eigenvectors[,2]


plot(cor1, cor2, xlim=c(-1,1), ylim=c(-1,1))
abline(h=0, v=0)
symbols(0, 0, circles=1, inches=F, add=T)
identify(cor1, cor2, labels=colnames(quant_var))



library(robust)

# Apply robust PCA on your dataset
robust_cov <- covRob(quant_var, corr = TRUE, estim = 'mcd')
PCA_robust <- eigen(robust_cov$cov)
PCA_robust$vectors
PCA_robust$values

plot(eigenvalues_robust$values, type='l')


inertie_perc_robuste=100*PCA_robust$values/sum(PCA_robust$values)
cumsum(inertie_perc_robuste)






