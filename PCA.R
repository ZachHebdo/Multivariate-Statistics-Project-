#PCA analysis
quant_var = database[c("Cost_claims_year","Power","Cylinder_capacity", 
                       "Value_vehicle","Length", "Weight")]


#descriptive analysis 
summary(quant_var)
boxplot(quant_var)
cov(quant_var)

apply(quant_var,2,mean)

data_st = as.data.frame(scale(quant_var))
apply(data_st, 2, mean)
apply(data_st, 2, var)

pca = princomp(data_st, corr=TRUE)
attributes(pca)

eigenvalues=pca$sdev^2
inertie_perc=100*eigenvalues/sum(eigenvalues)
cumsum(inertie_perc)

plot(eigenvalues, type='line')

eigenvectors=pca$loadings

cor1=pca$sdev[1]*eigenvectors[,1]
cor2=pca$sdev[2]*eigenvectors[,2]


plot(cor1, cor2, xlim=c(-1,1), ylim=c(-1,1))
abline(h=0, v=0)
symbols(0, 0, circles=1, inches=F, add=T)
identify(cor1, cor2, labels=colnames(quant_var))



library(robustbase)

# Apply robust PCA on your dataset
robust_cov <- covMcd(quant_var)
eigenvalues_robust <- eigen(robust_cov$cov)
loadings <- eigen$vectors
scores <- scale(quant_var, center = robust_cov$center, scale = FALSE) %*% loadings

# The eigenvalues and vectors are used to find the principal components
pca_result <- list(
  eigenvalues = eigenvalues$values,
  eigenvectors = eigenvalues$vectors,
  scores = scores
)