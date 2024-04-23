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

################################################################################
#FIRST PART: Load the raw data, filter the data relative to the most recent year, and input missing values.
Motor_vehicle_insurance_data <- read_delim("Motor_vehicle_insurance_data.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(Motor_vehicle_insurance_data)
df<-Motor_vehicle_insurance_data

# FILTER ONLY THE OBSERVATIONS REFERING TO THE LAST YEAR => df4
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

# Correct a mistake in the database, where 0s were converted into "00/01/1900", due to marking the variable column for a subset of a database as type date.
df4$Distribution_channel <- ifelse(df4$Distribution_channel == "00/01/1900", "0", df4$Distribution_channel) 

na_count <-sapply(df4, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#Using MICE package to impute missing values, via random forest (rf) algorithms. 
df4$Type_fuel <- as.factor(df4$Type_fuel)
md.pattern(df4)
imputedData <- mice(df4, method = "rf", m = 1, maxit = 5)
complet<-complete(imputedData)
md.pattern(complet)

# Evaluate the quality of the inputations.
densityplot(imputedData)

stripplot(imputedData, "Type_fuel", pch = 20, cex = 1.2)
stripplot(imputedData, "Length", pch = 20, cex = 1.2)
################################################################################
# SECOND PART: Preparing the data for analysis.
# This part is to change the columns that have numbers while they are categorical
# into purely categorical columns which will make it easier to run categorization

db_final<-complet

db_final <- db_final %>%
    mutate(Type_risk = case_when(
        Type_risk == 1 ~ "motorbikes",
        Type_risk == 2 ~ "vans",
        Type_risk == 3 ~ "passenger cars",
        Type_risk == 4 ~ "agricultural vehicles",
        TRUE ~ as.character(Type_risk)  # Pour gérer les éventuels cas non couverts
    ))

# Categorizing number of policies in force
db_final <- db_final %>%
    mutate(Policies_in_force = ifelse(Policies_in_force > 5, "6+", as.character(Policies_in_force)))

# Categorizing Seniority
db_final <- db_final %>%
    mutate(Seniority = ifelse(as.numeric(Seniority) > 1, "0", as.character(Seniority))) 
names(db_final)[names(db_final) == "Seniority"] <- "newClient"

# Categorizing Lapse
db_final <- db_final %>%
    mutate(Lapse = ifelse(as.numeric(Lapse) > 1, 1, as.character(Lapse)))

# Premium remains quantitative.
db_final$Premium = as.numeric(db_final$Premium)

db_final <-  db_final %>% mutate(N_claims_history = case_when(
    N_claims_history == 1 ~ "1",
    N_claims_history > 1 & N_claims_history < 5 ~ "2 - 4",
    N_claims_history > 4 ~ "5+",
    TRUE ~ as.character(N_claims_history)))  # Pour gérer les éventuels cas non couverts

db_final$N_claims_history <- as.factor(db_final$N_claims_history)

################################################################################
# Here we are interested in only the observations which have had at least one claim in the current year.
db_sinis <- db_final %>%filter(as.numeric(N_claims_year) > 0) 

# Suppose you want to rename "oldName" to "newName"
names(db_sinis)[names(db_sinis) == "Distribution_channel"] <- "Broker"
names(db_sinis)[names(db_sinis) == "Area"] <- "Urban"

# Convert 'P' to 0 and 'D' to 1
db_sinis$Type_fuel <- ifelse(db_sinis$Type_fuel == "P", 0, 1)
names(db_sinis)[names(db_sinis) == "Type_fuel"] <- "Diesel"

# REMOVING THESE VARIABLES, AS THEY ARE NOT INTERESTING FOR OUR ANALYSIS
db_sinis <- subset(db_sinis, select = -Birth_year)
db_sinis <- subset(db_sinis, select = -Licence_year)
db_sinis <- subset(db_sinis, select = -Start_year)
db_sinis <- subset(db_sinis, select = -Year_matriculation)
db_sinis <- subset(db_sinis, select = -R_Claims_history)

db_sinis <- db_sinis %>%
    mutate(N_claims_year = ifelse(as.numeric(N_claims_year) > 3, "4+", as.character(N_claims_year)))

db_sinis$newClient<-as.factor(db_sinis$newClient)
db_sinis$Broker <- as.factor(db_sinis$Broker)
db_sinis$Lapse <- as.factor(db_sinis$Lapse)
db_sinis$Policies_in_force<- as.factor(db_sinis$Policies_in_force)
db_sinis$N_doors<- as.factor(db_sinis$N_doors)
db_sinis$Urban <- as.factor(db_sinis$Urban)
db_sinis$Diesel <- as.factor(db_sinis$Diesel)
db_sinis$Payment <- as.factor(db_sinis$Payment)
db_sinis$Second_driver <- as.factor(db_sinis$Second_driver)
db_sinis$N_claims_year <- as.factor(db_sinis$N_claims_year)
db_sinis$Type_risk <- as.factor(db_sinis$Type_risk)
db_sinis$N_claims_history <- as.factor(db_sinis$N_claims_history)

db_quant <- db_sinis %>% select(where(is.numeric))
db_qual <- db_sinis %>% select(where(~ !is.numeric(.)))

#glimpse(db_quant)
#glimpse(db_qual)

countVar <- function(var) {
    db_sinis %>% count({{var}})
}

lapply(db_qual, countVar)

# Organize the dataset into quantitative variables appearing first, qualitative later
db_sinis <- db_sinis %>% select(c(colnames(db_quant), colnames(db_qual)))

view(db_sinis)
write.csv(db_sinis, "preprocessedData.csv", row.names = FALSE)
