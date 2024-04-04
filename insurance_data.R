#library(openxlsx)

library(tidyverse)

setwd("~/Studies/ULB - Masters/Multivariate Statistical Analysis/Project")
#df <- read.xlsx('TF_VEHICLES_COLLISION_2022.xlsx')

df <- read.csv('Motor vehicle insurance data.csv', sep = ";")
df <- df %>% mutate(Year_Renewal = gsub("^.*/", "", Date_last_renewal)) %>% group_by(Year_Renewal) %>% group_nest() 
df <- df %>% add_column(distinct_ID = c(n_distinct(df$data[[1]][1]), n_distinct(df$data[[2]][1]), n_distinct(df$data[[3]][1]), n_distinct(df$data[[4]][1])))
df
lapply(df$data, head, 3)

# Chosing the data for contracts renewed in 2018, the most recent year in the dataset, and selecting the relevant variables: 
df4 <- df$data[[4]] %>% mutate(Start_year = as.integer(gsub("^.*/", "", Date_start_contract))) %>%
    mutate(Birth_year = as.integer(gsub("^.*/", "", Date_birth))) %>%
    mutate(Licence_year = as.integer(gsub("^.*/", "", Date_driving_licence))) %>%
    select(c("Start_year", "Birth_year", "Licence_year", 
                                 "Distribution_channel", "Seniority" , "Policies_in_force", 
                                 "Lapse", "Payment", "Premium", "Cost_claims_year", 
                                 "N_claims_year", "N_claims_history", "R_Claims_history", 
                                 "Type_risk", "Area", "Second_driver", "Year_matriculation", 
                                 "Power", "Cylinder_capacity", "Value_vehicle", "N_doors", 
                                 "Type_fuel", "Length", "Weight"))


glimpse(df4)

na_count <-sapply(df4, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)


#analyse descriptive de notre portefeuille 

#frequence de sinistre
ggplot(dataset, aes(x=N_claims_year))+
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
