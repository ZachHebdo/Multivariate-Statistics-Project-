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

inputed_df<-complet

inputed_df <- inputed_df %>%
    mutate(Type_risk = case_when(
        Type_risk == 1 ~ "moto",  # originally 'motorbikes'
        Type_risk == 2 ~ "vans", 
        Type_risk == 3 ~ "cars",  # originally 'passanger cars'
        Type_risk == 4 ~ "agricultural vehicles",
        TRUE ~ as.character(Type_risk)  # Pour gérer les éventuels cas non couverts
    ))

# Categorizing number of policies in force
inputed_df <- inputed_df %>%
    mutate(Policies_in_force = ifelse(Policies_in_force > 1, "Many", as.character(Policies_in_force)))
names(inputed_df)[names(inputed_df) == "Policies_in_force"] <- "Policies"

# Transforming the Birth Year variable in Age variable.
inputed_df$Age<- 2018 - as.numeric(inputed_df$Birth_year) 
inputed_df$Age <- case_when(inputed_df$Age<30 ~"young adult",
                            inputed_df$Age>=30 & inputed_df$Age<70 ~ "adult",
                            inputed_df$Age>=70 ~ "senior")

# To get Licence time, added +1, because new drivers would have value of 0. 
inputed_df$Licence<- 2018- as.numeric(inputed_df$Licence_year)+ 1 
inputed_df$Licence<- case_when(inputed_df$Licence>=1 & inputed_df$Licence<=10 ~"young driver",
                                     inputed_df$Licence>10 & inputed_df$Licence<=35 ~ "experienced driver",
                                     inputed_df$Licence>35 ~"senior driver")
                            
# Categorizing Seniority
inputed_df <- inputed_df %>%
    mutate(Seniority = ifelse(as.numeric(Seniority) > 1, "0", as.character(Seniority))) 
names(inputed_df)[names(inputed_df) == "Seniority"] <- "newClient"

# Categorizing Lapse
inputed_df <- inputed_df %>%
    mutate(Lapse = ifelse(as.numeric(Lapse) > 1, 1, as.character(Lapse)))

# Premium remains quantitative.
inputed_df$Premium = as.numeric(inputed_df$Premium)

inputed_df <- inputed_df %>% mutate(N_claims = case_when(
    as.numeric(N_claims_history) == 1 ~ "1",
    as.numeric(N_claims_history) > 1 & as.numeric(N_claims_year) == 1 ~ "2",
    as.numeric(N_claims_year) > 1 ~ "3"))


inputed_df <- inputed_df %>% mutate(N_claims_history = ifelse(as.numeric(N_claims_history) > 1, "many", as.character(N_claims_history))) 

inputed_df$N_claims_history <- as.factor(inputed_df$N_claims_history)

################################################################################
## VARIABLE SELECTION and TREATMENT (separating into quantitative and qualitative)

# df <- inputed_df 

#### If we take the entire set of observations, the number of 0s in cost_claims_year is so large, that the robust covariance matrix becomes singular.
#### We could direct our questions to the group of people who had claims in that year, so that we only look at non-zero, and thus invertible matrices.

# Here we are interested in only the observations which have had at least one claim in the current year.
df <- inputed_df %>%filter(as.numeric(N_claims_year) > 0) 

# Suppose you want to rename "oldName" to "newName"
names(df)[names(df) == "Distribution_channel"] <- "Broker"
names(df)[names(df) == "Area"] <- "Urban"

# Convert 'P' to 0 and 'D' to 1
df$Type_fuel <- ifelse(df$Type_fuel == "P", 0, 1)
names(df)[names(df) == "Type_fuel"] <- "Diesel"

# REMOVING THESE VARIABLES, AS THEY ARE NOT INTERESTING FOR OUR ANALYSIS
df <- subset(df, select = -Birth_year)
df <- subset(df, select = -Licence_year)
df <- subset(df, select = -Start_year)
df <- subset(df, select = -Year_matriculation)
df <- subset(df, select = -R_Claims_history)
df <- subset(df, select = -N_doors)
df <- subset(df, select = -N_claims_history)
df <- subset(df, select = -N_claims_year)
# df <- df %>%
#     mutate(N_claims_year = ifelse(as.numeric(N_claims_year) > 3, "4+", as.character(N_claims_year)))

#df$N_claims_history <- as.factor(df$N_claims_history)
#df$N_claims_year <- as.factor(df$N_claims_year)
#df$N_doors<- as.factor(df$N_doors)
df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies<- as.factor(df$Policies)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$Type_risk <- as.factor(df$Type_risk)
df$N_claims <- as.factor(df$N_claims)
df$Licence <- as.factor(df$Licence)
df$Age <- as.factor(df$Age)

df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))

#glimpse(df_quant)
#glimpse(df_qual)

countVar <- function(var) {
    df %>% count({{var}})
}

lapply(df_qual, countVar)

# Organize the dataset into quantitative variables appearing first, qualitative later
df <- df %>% select(c(colnames(df_quant), colnames(df_qual)))

view(df)
write.csv(df, "preprocessedData.csv", row.names = FALSE)
