###INSTALLATION PACKAGES ; remove # if needed
#install.packages("mice")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("openxlsx")

library(openxlsx)
library(mice)
library(tidyverse)
library(dplyr)
library(readr)

Motor_vehicle_insurance_data <- read_delim("Motor_vehicle_insurance_data.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Motor_vehicle_insurance_data)
df<-Motor_vehicle_insurance_data

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

df4$Type_fuel <- as.factor(df4$Type_fuel)
md.pattern(df4)
imputedData <- mice(df4, method = "rf", m = 1, maxit = 5)
complet<-complete(imputedData)
md.pattern(complet)

densityplot(imputedData)

stripplot(imputedData, "Type_fuel", pch = 20, cex = 1.2)
stripplot(imputedData, "Length", pch = 20, cex = 1.2)
db_final<-complet

# Utilisation de xyplot pour visualiser les imputations

write.csv(complet, "db_final.csv", row.names = FALSE)
db_final <- read_csv("db_final.csv")
View(db_final)

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


# to get age instead of birthdate, easier to categorise 
db_final$Premium<-as.numeric(db_final$Premium)
# creating classes per age 
db_final$Premium_class <- case_when(db_final$Premium>0 & db_final$Premium<=150 ~"0-150",
                          db_final$Premium>150 & db_final$Premium<=300~ "150-300",
                          db_final$Premium>300 & db_final$Premium<=450~"300-450",
                          db_final$Premium>450 & db_final$Premium<=600~"450-600",
                          db_final$Premium>600 ~"600+")
db_final$Premium<-as.factor(db_final$Premium)
# to get licence age, added +1, because new drivers would have value of 0 

db_final$age<- 2018- as.numeric(db_final$Birth_year) 
db_final$age <- case_when(db_final$age>15 & db_final$age<=30 ~"young adult",
                          db_final$age>30 & db_final$age<=50 ~ "adult",
                          db_final$age>50 & db_final$age<=70 ~ "senior",
                          db_final$age>70 ~"70+",
                          )
db_final$age<-as.factor(db_final$age)

db_final$Licence_time<- 2018- as.numeric(db_final$Licence_year)+ 1 
db_final$Licence_time <- case_when(db_final$Licence_time>1 & db_final$Licence_time<=10 ~"young driver",
                                   db_final$Licence_time>10 & db_final$Licence_time<=35 ~ "experienced driver",
                                   db_final$Licence_time>35 ~"senior driver",
)
db_final$age<-as.factor(db_final$age)

db_final <- db_final %>%
  mutate(Policies_in_force_category = ifelse(Policies_in_force > 6, "6+", as.character(Policies_in_force)))



db_final$age<-as.factor(db_final$age)


db_final <- db_final %>%
  mutate(Seniority = ifelse(as.numeric(Seniority) > 1, "1+", as.character(Seniority)))


db_final <- db_final %>%
  mutate(Lapse = ifelse(as.numeric(Lapse) > 1, "1+", as.character(Lapse)))


db_final$Seniority<-as.factor(db_final$Seniority)

#creation of classes for agence nd brokers instead of 0 and 1 

#creation of class for payment 

db_final$Categorie <- cut(db_final$Premium, breaks = c(seq(0, 600, by = 150), Inf), include.lowest = TRUE)


db_sinistralité <- db_final %>%filter(N_claims_year > 0) 

count <- db_sinistralité %>%
  count(Seniority)


db_sinistralité <- subset(db_sinistralité, select = -Birth_year)
db_sinistralité <- subset(db_sinistralité, select = -Licence_year)
db_sinistralité <- subset(db_sinistralité, select = -Start_year)
db_sinistralité <- subset(db_sinistralité, select = -Year_matriculation)


db_sinistralité$Lapse <- as.factor(db_sinistralité$Lapse)
db_sinistralité$Seniority <- as.factor(db_sinistralité$Seniority)
db_sinistralité$Policies_in_force<- as.factor(db_sinistralité$Policies_in_force)
db_sinistralité$N_doors<- as.factor(db_sinistralité$N_doors)







