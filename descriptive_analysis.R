#analyse descriptive de notre portefeuille 
library(ggplot2)
library(scales)
database = read_csv("db_final.csv")


#frequence de sinistre
ggplot(database, aes(x=N_claims_year))+
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
# Création des classes de catégories de primes
database$Categorie <- cut(database$Premium, breaks = c(seq(0, 600, by = 150), Inf), include.lowest = TRUE)

# Compter le nombre de polices dans chaque catégorie
nombre_polices <- table(database$Categorie)

# Afficher le nombre de polices dans chaque catégorie
print(nombre_polices)

ggplot(database, aes(x = Categorie)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Catégorie de Prime", y = "Nombre de Polices") +
  ggtitle("Répartition des Polices d'Assurance par Catégorie de Prime")


#type de fuel

ggplot(database, aes(x = Diesel)) + 
  geom_bar(fill = "blue") +
  labs(x = "Fuel type", y = "Compte") +
  ggtitle("Fuel type")




ggplot(database, aes(x = N_doors, y = N_claims_year)) + 
  geom_boxplot() +
  labs(x = "Nom de Variable Catégorielle", y = "Nom de Variable Numérique") +
  ggtitle("Relation entre  et ")

ggplot(database, aes(x = N_doors)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Nom de Variable", y = "Fréquence") +
  ggtitle("claims by n_doors")




# Calculer le tableau de contingence
tableau <- table(database$N_doors, database$N_claims_year)
ggplot(data = database, aes(x = factor(N_doors), fill = factor(N_claims_year))) +
  geom_bar(position = "fill") +
  labs(x = "nb_doors", y = "Proportion", fill = "nb claims") +
  theme_minimal() +
  coord_flip()
# Afficher le tableau
print(tableau)



  
dfquanti <- database %>% select(where(is.numeric))
sumtable <- as.data.frame(c(0,0,0,0,0,0,0))
row.names(sumtable) <- c('Min', 'Q1', 'M?diane','Moyenne','Q3','Max',"NA's")

for (i in 1:ncol(dfquanti)){
  x <- dfquanti[,i]
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
