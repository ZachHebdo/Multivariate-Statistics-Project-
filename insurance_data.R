#library(openxlsx)

library(tidyverse)
library(dplyr)

#setwd("~/Studies/ULB - Masters/Multivariate Statistical Analysis/Project")
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

df4$Distribution_channel <- ifelse(df4$Distribution_channel == "00/01/1900", "0", df4$Distribution_channel) 

na_count <-sapply(df4, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#impute missing Value getting rid of the NA values through random forest 
library(mice)
df4$Type_fuel <- as.factor(df4$Type_fuel)
md.pattern(df4)
imputedData <- mice(df4, method = "rf", m = 1, maxit = 5)
complet<-complete(imputedData)
md.pattern(complet)

stripplot(complet, pch = 1, cex = 1.2)
densityplot(imputedData)


stripplot(imputedData, "Type_fuel", pch = 20, cex = 1.2)
stripplot(imputedData, "Length", pch = 20, cex = 1.2)


# Utilisation de xyplot pour visualiser les imputations

write.csv(complet, "C:/Users/jeand/Desktop/travail perso unif/cours maths et eco/db_final.csv", row.names = FALSE)
library(readr)
db_final <- read_csv("C:/Users/jeand/Desktop/dataset multivarie/db_final.csv")
View(db_final)
db_final = read_csv("db_final.csv")


# this part is to change the columns that have numbers while they are categorical
# into purely categorical columns which will make it easier to run categorization

db_final <- db_final %>%
  mutate(Type_risk = case_when(
    Type_risk == 1 ~ "motorbikes",
    Type_risk == 2 ~ "vans",
    Type_risk == 3 ~ "passenger cars",
    Type_risk == 4 ~ "agricultural vehicles",
    TRUE ~ as.character(Type_risk)  # Pour gérer les éventuels cas non couverts
  ))


db_final <- db_final %>%
  mutate(Area = case_when(
    Area == 0 ~ "rural",
    Area == 1 ~ "urban",
    TRUE ~ as.character(Area)  # Pour gérer les éventuels cas non couverts
  ))



db_final <- db_final %>%
  mutate(Second_driver= case_when(
    Second_driver == 0 ~ "no",
    Second_driver == 1 ~ "yes",
    TRUE ~ as.character(Area)  # Pour gérer les éventuels cas non couverts
  ))


# to get age instead of birthdate, easier to categorise 
db_final$age<- 2018- as.numeric(db_final$Birth_year) 

# creating classes per age 
db_final$age <- case_when(db_final$age>15 & db_final$age<=25 ~"15-25",
                          db_final$age>25 & db_final$age<=35~ "25-35",
                          db_final$age>35 & db_final$age<=45~"35-45",
                          db_final$age>45 & db_final$age<=55~"45-55",
                          db_final$age>55 & db_final$age<=65~"55-65",
                          db_final$age>65 & db_final$age<=75~"65-75",
                          db_final$age>75 & db_final$age<=80~"75-85",
                          db_final$age>80 ~"80+")

# to get licence age, added +1, because new drivers would have value of 0 
db_final$Licence_time<- 2018- as.numeric(db_final$Licence_year)+ 1 

#creation of classes for agence nd brokers instead of 0 and 1 
db_final <- db_final %>%
  mutate(Distribution_channel= case_when(
    Distribution_channel == 0 ~ "agency",
    Distribution_channel == 1 ~ "brokers",
    TRUE ~ as.character(Distribution_channel)
  ))

#creation of class for payment 
db_final <- db_final %>%
  mutate(Payment= case_when(
    Payment == 0 ~ "annual",
    Payment == 1 ~ "semester",
    TRUE ~ as.character(Area)  # Pour g?rer les ?ventuels cas non couverts
  ))