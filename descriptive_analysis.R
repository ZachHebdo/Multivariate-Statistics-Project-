#install.packages("robust")
library(ggplot2)
library(scales)
library(dplyr)
library(robust)

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

#frequency of premium
ggplot(database, aes(x = Premium)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +  
  labs(x = "Premium", y = "Frequency") +  
  ggtitle("Histogram of Premium")

#type of fuel
ggplot(database, aes(x = Diesel)) + 
  geom_bar(fill = "blue") +
  labs(x = "Fuel type", y = "Amount of people") +
  ggtitle("Bar plot of fuel type")

#type of client
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

# 2. Outliers detection

# 2.1 Univariate detection
summary(database_quant)
boxplot(database_quant)

boxplot(database_quant$Premium, main="Box plot of premium") 
boxplot(database_quant$Cost_claims_year, main="Box plot of cost of claims/year") 
boxplot(database_quant$Power, main="Box plot of power")
boxplot(database_quant$Cylinder_capacity, main="Box plot of cylinder capacity")
boxplot(database_quant$Value_vehicle, main="Box plot of value of vehicle on 2019")
boxplot(database_quant$Length, main="Box plot of length")
boxplot(database_quant$Weight, main="Box plot of weight")
# => outliers presence

#3sigma-rule (robust)
b1=median(database$Premium)-3*mad(database$Premium)
b2=median(database$Premium)+3*mad(database$Premium)
plot(database$Premium, ylab='Premium')
abline(h=b2, col='red')

b1=median(database$Power)-3*mad(database$Power)
b2=median(database$Power)+3*mad(database$Power)
plot(database$Power, ylab='Power')
abline(h=b2, col='red')

# 2.2 Multivariate detection
apply(database_quant, 2, mean) #mean vector of quantitative variables
cov(database_quant) #covariance matrix

#Stahel-Donoho estimators
SD=covRob(database_quant, estim="donostah")
SD

#MCD
MCD=covRob(database_quant, estim="mcd")
MCD
