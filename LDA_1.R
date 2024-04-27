install.packages("klaR")
install.packages("psych")
install.packages("ggord")
install.packages("devtools")

library(caret)
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
set.seed(123)
#sepration training set et validation set
ind <- createDataPartition(y = df_quant$target_variable, p = 0.8, list = FALSE)
# Créer l'ensemble de validation et d'entrainement
training <- df_quant[ind, ]
testing <- df_quant[-ind, ]

linear <- lda(Cost_claims_year~., training)
linear
par(mar=c(1,1,1,1))
p <- predict(linear, training) #predire si oui ou non on est dans un class
ldahist(data = p$x[,1], g = training$Cost_claims_year)# graphique
ldahist(data = p$x[,2], g = training$Cost_claims_year) # grapique
gg_ordiplot(linear, training$Cost_claims_year)
partimat(Cost_claims_year~., data = training, method = "lda")

