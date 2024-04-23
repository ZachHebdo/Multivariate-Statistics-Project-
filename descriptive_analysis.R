library(ggplot2)
library(scales)
library(dplyr)
database <- read.csv("preprocessedData.csv")
str(database)
# 1. Univariate analysis

#frequency of number of claims/year
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

#Type risks
ggplot(database, aes(x = Type_risk)) + 
  geom_bar(fill = "blue") +
  labs(x = "Type risk", y = "Amount of people") +
  ggtitle("Histogram of type risk associated with the policy")

#Premium class
ggplot(database, aes(x = Premium_class)) + 
  geom_bar(fill = "blue") +
  labs(x = "Premium class", y = "Amount of people") +
  ggtitle("Histogram of premium class")

#type of fuel
ggplot(database, aes(x = Diesel)) + 
  geom_bar(fill = "blue") +
  labs(x = "Fuel type", y = "Amount of people") +
  ggtitle("Histogram of fuel type")

#Contingency table (ACM non?)
table(database$Premium_class, database$age)
tableau <- table(database$N_doors, database$N_claims_year)
ggplot(data = database, aes(x = factor(N_doors), fill = factor(N_claims_year)))+
  geom_bar(position = "fill") +
  labs(x = "nb_doors", y = "Proportion", fill = "nb claims") +
  theme_minimal() +
  coord_flip()
# Afficher le tableau
print(tableau)

# 2. Outliers detection
selected_variables <- database[, c("Cost_claims_year", "Power", 
                                   "Cylinder_capacity","Value_vehicle",
                                   "Length","Weight")]
summary(selected_variables)

boxplot(selected_variables)

boxplot(selected_variables$Cost_claims_year) 
boxplot(selected_variables$Cylinder_capacity)
boxplot(selected_variables$Value_vehicle)
boxplot(selected_variables$Length)
boxplot(selected_variables$Power)
boxplot(selected_variables$Weight)
