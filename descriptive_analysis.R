install.packages("GGally")
install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(scales)
library(dplyr)
library(GGally)

database <- read.csv("preprocessedData.csv")
str(database)
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
database$Type_risk <- as.factor(database$Type_risk)
database$N_claims_history <- as.factor(database$N_claims_history)

database_quant <- database %>% select(where(is.numeric))
database_qual <- database %>% select(where(~ !is.numeric(.)))

# 1. Univariate analysis

#comparison of different estimators of mean 
mean(database$Premium)
median(database$Premium)
mean(database$Premium,trim=0.1)
winsor=function(x, p=c(0.05,0.95)){
  xq=quantile(x, probs=p)
  x[x < xq[1]]=xq[1]
  x[x > xq[2]]=xq[2]
  return(mean(x))}
winsor(database$Premium)

#Number of claims per year
ggplot(database, aes(x = N_claims_year)) + 
  geom_bar(fill = "blue") +
  labs(x = "Number of claims per year", y = "Amount of people") +
  ggtitle("Bar plot of number of claims per year")

#Type risks
ggplot(database, aes(x = Type_risk)) + 
  geom_bar(fill = "blue") +
  labs(x = "Type risk", y = "Amount of people") +
  ggtitle("Bar plot of type risk associated with the policy")

# Pie chart function
plot_pie_chart <- function(data, variable_name, title = NULL) {
  if (!(variable_name %in% colnames(data))) {
    stop("The specified variable is not in the dataframe.")
  }
  var_count <- as.data.frame(table(data[[variable_name]]))
  colnames(var_count) <- c("category", "count")
  var_count$fraction = var_count$count / sum(var_count$count)
  var_count$ymax = cumsum(var_count$fraction)
  var_count$ymin = c(0, head(var_count$ymax, -1))
  ggplot(var_count, aes(ymin = ymin, ymax = ymax, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    coord_polar(theta = "y") +
    theme_void() +
    ggtitle(title) +
    labs(fill = variable_name)
}
plot_pie_chart(database, "Type_risk", "Pie chart for Type Risk")

#frequency of premium
ggplot(database, aes(x = Premium)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +  
  labs(x = "Premium", y = "Frequency") +  
  ggtitle("Histogram of Premium")

# Type of fuel
ggplot(database, aes(x = Diesel)) + 
  geom_bar(fill = "blue") +
  labs(x = "Fuel type", y = "Amount of people") +
  ggtitle("Bar plot of fuel type")

# Relation between the type of fuel and premium
ggplot(database, aes(x = Diesel, y = Premium)) + 
  geom_boxplot() +
  labs(x = "Type of fuel", y = "Premium") +
  ggtitle("Box Plot of Premium by the type of fuel")

# Type of client
ggplot(database, aes(x = newClient)) + 
  geom_bar(fill = "blue") +
  labs(x = "type of client", y = "Amount of people") +
  ggtitle("Bar plot of type of client")

#total number of policies held by the insured
ggplot(database, aes(x = Policies_in_force)) + 
  geom_bar(fill = "blue") +
  labs(x = "Total number of policies", y = "Amount of people") +
  ggtitle("Bar plot of total number of policies")

#number of policies that the customer has cancelled or has been cancelled
ggplot(database, aes(x = Lapse)) + 
  geom_bar(fill = "blue") +
  labs(x = "Number of policies cancelled", y = "Amount of people") +
  ggtitle("Bar plot of number of policies cancelled")

#type of payment
ggplot(database, aes(x = Payment)) + 
  geom_bar(fill = "blue") +
  labs(x = "Type of payment", y = "Amount of people") +
  ggtitle("Bar plot of type of payment")

#area
ggplot(database, aes(x = Urban)) + 
  geom_bar(fill = "blue") +
  labs(x = "Area", y = "Amount of people") +
  ggtitle("Bar plot of area")

#Number of doors
ggplot(database, aes(x = N_doors)) + 
  geom_bar(fill = "blue") +
  labs(x = "Number of doors", y = "Amount of people") +
  ggtitle("Bar plot of number of doors")

#frequency of claims
ggplot(database, aes(x=N_claims_year))+
  geom_bar()+
  geom_label(stat='count', 
             aes(label =  percent(prop.table(after_stat(count)), 
                                  accuracy = 0.01)),vjust = 0.5)+
  ggtitle("Proportion of policies by number of claims")

# Box plot
ggplot(database, aes(x = Policies_in_force, y = Premium)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(x = "Total number of policies", y = "Premium") +
  ggtitle("Box Plot of Premium by Total number of policies")

# Contigency table
tableau <- table(database$Type_risk, database$N_claims_year)
ggplot(data = database, aes(x = factor(Type_risk), fill = factor(N_claims_year))) +
  geom_bar(position = "fill") +
  labs(x = "Type risk", y = "Proportion", fill = "nb claims") +
  theme_minimal() +
  coord_flip()
tableau

summary(database_quant)
cor(database_quant)
ggpairs(database_quant)
