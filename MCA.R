library(dplyr)
library(robust)
library(tidyverse)
#library(MASS)
#detach("package:MASS")

df = read.csv("preprocessedData.csv")

df$newClient<-as.factor(df$newClient)
df$Broker <- as.factor(df$Broker)
df$Lapse <- as.factor(df$Lapse)
df$Policies_in_force<- as.factor(df$Policies_in_force)
df$N_doors<- as.factor(df$N_doors)
df$Urban <- as.factor(df$Urban)
df$Diesel <- as.factor(df$Diesel)
df$Payment <- as.factor(df$Payment)
df$Second_driver <- as.factor(df$Second_driver)
df$N_claims_year <- as.factor(df$N_claims_year)
df$Type_risk <- as.factor(df$Type_risk)
df$N_claims_history <- as.factor(df$N_claims_history)

df_quant <- df %>% select(where(is.numeric))
df_qual <- df %>% select(where(~ !is.numeric(.)))

countVar <- function(var) {
    df_qual %>% count({{var}})
}

lapply(df_qual, countVar)

