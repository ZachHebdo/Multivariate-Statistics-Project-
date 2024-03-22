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

