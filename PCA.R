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
## make sure to run the function for plotting the circle at the end of the code 


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
pca$scores[,1:2]
#have to do this to have same direction of graph 
PCA$vectors = -1 * PCA$vectors

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
plot_pca_circle(cor1, cor2)
plot_pca_circle(cor2, cor3)
plot_pca_circle(cor3, cor4)



# Apply robust PCA on your dataset (If there are less than 50000 observations and less than 20 variables then the MCD is used.)
robust_cov <- covRob(df_quant, corr = TRUE, estim = 'weighted')
PCA_robust <- eigen(robust_cov$cov)
PCA_robust$vectors = -1* PCA_robust$vectors
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
text(cor1, cor2, labels=colnames(df_quant), cex=0.7, offset=0.1)


# Plot
plot_pca_circle(cor_rob1, cor_rob2)
plot_pca_circle(cor_rob2, cor_rob3)
plot_pca_circle(cor_rob1, cor_rob3)

#calculating scores 
scores_robust = scale(df_quant) %*% PCA_robust$vectors 


#plotting the biplot 
autoplot(pca, data= df, colour="Type_risk",loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


### IDEA: Keep only three principal components, to favour interpretability of the dataset. 
scores = as.data.frame(scores)
colnames(scores) = paste("PC", 1:7, sep = "")
typerisk = df$Type_risk
scores$Typerisk = typerisk

loadings = as.data.frame(PCA$vectors) 
loadings$variable = colnames(df_quant)
colnames(loadings) = paste("PC", 1:7, sep = "")
colnames(loadings)[8] = "Label"

#biplots for the different non-robust pca components 

scale_factor = 12
ggplot(scores, aes(x = PC2, y = PC3,color=typerisk)) +
  geom_point(alpha =0.7) +  # Add points
  theme_minimal() +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC2 * scale_factor, yend = PC3 * scale_factor), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "blue") +
  labs(title = "Basic PCA Biplot", x = "PC1", y = "PC2") +
  geom_text(data = loadings, aes(x = PC2 * scale_factor, y = PC3 * scale_factor, label = Label), 
          hjust = 0, vjust = 0, color = "blue", size = 3, nudge_x = 0.02, nudge_y = 0.02) 

ggplot(scores, aes(x = PC1, y = PC2,color=typerisk)) +
  geom_point(alpha =0.7) +  # Add points
  theme_minimal() +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "blue") +
  labs(title = "Basic PCA Biplot", x = "PC1", y = "PC2") +
  geom_text(data = loadings, aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Label), 
            hjust = 0, vjust = 0, color = "blue", size = 3, nudge_x = 0.02, nudge_y = 0.02) 




scores_robust = as.data.frame(scores_robust)
colnames(scores_robust) = paste("PC", 1:7, sep = "")
typerisk = df$Type_risk
scores_robust$Typerisk = typerisk

loadings_robust = as.data.frame(PCA_robust$vectors) 
loadings_robust$variable = colnames(df_quant)
colnames(loadings_robust) = paste("PC", 1:7, sep = "")
colnames(loadings_robust)[8] = "Label"

#biplots for the robust princicipal components 
ggplot(scores_robust, aes(x = PC1, y = PC2,color=typerisk)) +
  geom_point(alpha =0.7) +  # Add points
  theme_minimal() +
  geom_segment(data = loadings_robust, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "blue") +
  labs(title = "Basic PCA Biplot", x = "PC1", y = "PC2") +
  geom_text(data = loadings_robust, aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Label), 
            hjust = 0, vjust = 0, color = "blue", size = 3, nudge_x = 1.03, nudge_y = 0.03) 



#function to visualize the correlation circle 
plot_pca_circle <- function(x, y) {
  # Create a data frame from the vectors
  dftest <- data.frame(PCx = x, PCy = y, Labels = colnames(df_quant))
  
  # Determine the names for the PC axes dynamically based on input
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Create the ggplot object
  p <- ggplot(dftest, aes(x = PCx, y = PCy)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_point() +
    geom_text_repel(aes(label = Labels)) +
    annotate("path", x = cos(seq(0, 2 * pi, length.out = 100)), 
             y = sin(seq(0, 2 * pi, length.out = 100)), colour = "black") +
    coord_fixed(ratio = 1) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    labs(x = x_name, y = y_name) +
    theme_minimal()
  
  # Return the plot
  return(p)
}



