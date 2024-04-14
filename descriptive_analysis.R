#analyse descriptive de notre portefeuille 
library(ggplot2)
library(scales)

#frequence de sinistre
ggplot(db_final, aes(x=N_claims_year))+
  geom_bar()+
  geom_label(stat='count', 
             aes(label =  percent(prop.table(after_stat(count)), 
                                  accuracy = 0.01)),
             vjust = 0.5)+
  scale_x_continuous(name = "Number of Claims")+
  scale_y_continuous(name = "Number of Polices", 
                     labels = label_number())+
  ggtitle("Proportion of policies by number of claims")



#premium
ggplot(db_final, aes(x = Premium)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(x = "Nom de Variable", y = "Fréquence") +
  ggtitle("Titre du Graphique")
#premium with h= 10 we can see a concentration of obervations around 250 euros 

#type de fuel

ggplot(db_final, aes(x = Type_fuel)) + 
  geom_bar(fill = "blue") +
  labs(x = "Nom de Variable", y = "Compte") +
  ggtitle("Nom de Variable")


ggplot(db_final, aes(x = Type_fuel, y = Premium)) + 
  geom_boxplot() +
  labs(x = "Nom de Variable Catégorielle", y = "Nom de Variable Numérique") +
  ggtitle("Relation entre  et ")


ggplot(db_final, aes(x = N_doors, y = N_claims_year)) + 
  geom_boxplot() +
  labs(x = "Nom de Variable Catégorielle", y = "Nom de Variable Numérique") +
  ggtitle("Relation entre  et ")

ggplot(db_final, aes(x = N_doors)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Nom de Variable", y = "Fréquence") +
  ggtitle("claims by n_doors")




# Calculer le tableau de contingence
tableau <- table(db_final$N_doors, db_final$N_claims_year)
ggplot(data = db_final, aes(x = factor(N_doors), fill = factor(N_claims_year))) +
  geom_bar(position = "fill") +
  labs(x = "nb_doors", y = "Proportion", fill = "nb claims") +
  theme_minimal() +
  coord_flip()
# Afficher le tableau
print(tableau)


ggplot(db_final, aes(x = Type_fuel, y = Premium)) + 
  geom_boxplot() +
  labs(x = "Nom de Variable Catégorielle", y = "Nom de Variable Numérique") +
  ggtitle("Relation entre  et ")
  
  dfquanti <- data %>% select(where(is.numeric))
sumtable <- as.data.frame(c(0,0,0,0,0,0,0))
row.names(sumtable) <- c('Min', 'Q1', 'M?diane','Moyenne','Q3','Max',"NA's")

for (i in 1:ncol(dfquanti)){
  x <- as.vector(dfquanti[,i])
  nas <- dfquanti %>% filter(is.na(x)) %>% nrow()
  min <- min(x,na.rm = TRUE)
  Q1 <- unname(quantile(x,.25,na.rm = TRUE))
  Median <- unname(quantile(x,.5,na.rm = TRUE))
  Mean <- unname(colMeans(x,na.rm = TRUE))
  Q3 <- unname(quantile(x,.75,na.rm = TRUE))
  max <- max(x,na.rm = TRUE)
  nas <- dfquanti %>% filter(is.na(x)) %>% nrow()
  sumtable <- sumtable %>% cbind(c(min,Q1,Median,Mean,Q3,max,nas))
}
colnames(sumtable) <- c("0",colnames(dfquanti))
sumtable <- sumtable %>% select(-"0")
