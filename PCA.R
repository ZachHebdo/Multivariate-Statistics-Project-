#PCA analysis
library(dplyr)
library(robust)
library(tidyverse)
library(FactoMineR)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(ggfortify)


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
biplot(pca)
eigenvectors=pca$loadings
eigenvalues=pca$sdev^2.
pca$scores

#normal way to calculate scores 
scores = scale(df_quant) %*% PCA$vectors 
plot(scores[,1:2])


#calculating percentage of inertie explicated 
inertie_perc=100*PCA$values/sum(PCA$values)
cumsum(inertie_perc)

#calculating correlations for the first 4 components 
cor1 = sqrt(PCA$values[1])*PCA$vectors[,1]
cor2 = sqrt(PCA$values[2])*PCA$vectors[,2]
cor3 = sqrt(PCA$values[3])*PCA$vectors[,3]
cor4 = sqrt(PCA$values[4])*PCA$vectors[,4]

#graph of interpretation 
plot(cor1, cor2, xlim=c(-1,1), ylim=c(-1,1), xlab="PC1", ylab="PC2")
abline(h=0, v=0)
symbols(0, 0, circles=1, inches=FALSE, add=TRUE)
text(cor1, cor2, labels=colnames(df_quant), cex=0.7)

# Plot for PC3 and PC4
plot(cor3, cor4, xlim=c(-1,1), ylim=c(-1,1), xlab="PC3", ylab="PC4")
abline(h=0, v=0)
symbols(0, 0, circles=1, inches=FALSE, add=TRUE)
text(cor3, cor4, labels=colnames(df_quant), cex=0.7)


# Apply robust PCA on your dataset (If there are less than 50000 observations and less than 20 variables then the MCD is used.)
robust_cov <- covRob(df_quant, corr = TRUE, estim = 'weighted')
PCA_robust <- eigen(robust_cov$cov)
PCA_robust$vectors
PCA_robust$values

#plotting and looks like we should keep around 3 or 4 dimensions 
plot(PCA_robust$values, type='l')

#calculating percentage of inertie explicated 
inertie_perc_robuste=100*PCA_robust$values/sum(PCA_robust$values)
cumsum(inertie_perc_robuste)

#calculating correlations for the first 4 components 
cor_rob1 = sqrt(PCA_robust$values[1])*PCA_robust$vectors[,1]
cor_rob2 = sqrt(PCA_robust$values[2])*PCA_robust$vectors[,2]
cor_rob3 = sqrt(PCA_robust$values[3])*PCA_robust$vectors[,3]

#plotting circle again 
plot(cor_rob1, cor_rob2, xlim=c(-1,1), ylim=c(-1,1), xlab="PC1", ylab="PC2")
abline(h=0, v=0)
symbols(0, 0, circles=1, inches=FALSE, add=TRUE)
text(cor1, cor2, labels=colnames(df_quant), cex=0.7, pos=1, offset=0.1)

#calculating scores 
scores_robust = scale(df_quant) %*% PCA_robust$vectors 


#plotting the biplot 
autoplot(pca, data= df, colour="Type_risk",loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### IDEA: Keep only three principal components, to favour interpretability of the dataset. 







#visualization of the circle with a function, have to work on this 
plot_pca_correlation <- function(cor1, cor2, dataset) {
  # Create a data frame for ggplot
  pca_data <- data.frame(
    PC1 = cor1,
    PC2 = cor2,
    Variable = colnames(dataset)
  )
  
  # Define the limits for the plot
  limits <- max(abs(c(cor1, cor2))) * 1.1  # Slightly increase limits to ensure space for text
  
  # Create the plot
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, label = Variable)) +
    geom_hline(yintercept = 0, color = "black") +  # Solid line for x-axis
    geom_vline(xintercept = 0, color = "black") +  # Solid line for y-axis
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "black", linetype = "solid") +  # Solid circle
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), 
              color = "black", fill = NA, linetype = "solid") +  # Rectangle around the circle
    geom_point(color = "blue") +  # Add points
    geom_text_repel() +  # Add labels
    xlim(-limits, limits) +
    ylim(-limits, limits) +
    coord_fixed(ratio = 1) +  # Ensure the aspect ratio is fixed to make the circle round
    theme_classic() +
    theme(axis.title = element_text(size = 12), 
          axis.text = element_text(size = 10)) +
    labs(title = "PCA Correlation Circle", 
         x = "PC1", 
         y = "PC2")
  
  return(p)
}
plot_pca_correlation(cor_rob1,cor_rob2,df_quant)
