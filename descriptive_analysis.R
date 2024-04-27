#install.packages("GGally")
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(GGally)
library(xtable)

database <- read.csv("preprocessedData.csv")
str(database)

database_quant <- database %>%
  select(Premium, Cost_claims_year, Power, Cylinder_capacity, Value_vehicle, 
         Length, Weight)

database_qual <- database %>%
  select(Broker, newClient, Policies, Lapse, Payment, Type_risk, Urban, Diesel,
         Age, Licence, N_claims, Second_driver)

#### FUNCTION TO RUN BEFORE THE DESCRIPTIVE ANALYSIS ##########################
# Function to create table
create_summary_table <- function(data, variable_name) {
  if (!(variable_name %in% colnames(data))) {
    stop("Variable not found in the dataframe")
  }
  summary_table <- data %>%
    count(.data[[variable_name]]) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    rename(
      Category = variable_name,
      Number = n,
      Percentage = percentage
    )
  return(summary_table)
}
################################################################################

# 1. Univariate analysis

# Summary of quantitative variables
summary(database_quant)

# Histogram of premium
ggplot(database, aes(x = Premium)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +  
  labs(x = "Premium", y = "Frequency") +  
  ggtitle("Histogram of Premium")

# Histogram of total cost of claims
require(gridExtra)
dp <- ggplot(database, aes(x = Cost_claims_year)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +  
  labs(x = "Total cost of claims", y = "Frequency") +  
  ggtitle("Histogram of Cost of claims")
min <- min(database$Cost_claims_year)
max <- 1000
dp1 <- dp + scale_x_continuous(limits=c(min,max))
grid.arrange(dp,dp1,ncol=2)

ggplot(database, aes(x = Cost_claims_year)) +
  geom_histogram(aes(y = ..density..), binwidth = 20, fill = "lightblue", color = "blue") +
  geom_density(kernel = "gaussian", alpha = 0.5,col = "red", fill = NA) +
  labs(x = "Total Cost of Claims", y = "Density") +
  ggtitle("Histogram and Density Plot (Zoomed)") +
  scale_x_continuous(limits = c(min, max)) 

# Type risks
risk_summary <- create_summary_table(database, "Type_risk")
risk_summary

### Pie chart function with percentages
plot_pie_chart <- function(data, variable_name, title = NULL) {
  if (!(variable_name %in% colnames(data))) {
    stop("The specified variable is not in the dataframe.")
  }
  var_count <- as.data.frame(table(data[[variable_name]]))
  colnames(var_count) <- c("category", "count")
  var_count$fraction <- var_count$count / sum(var_count$count)
  var_count$ymax <- cumsum(var_count$fraction)
  var_count$ymin <- c(0, head(var_count$ymax, -1))
  var_count$ytext <- (var_count$ymin + var_count$ymax) / 2
  ggplot(var_count, aes(ymin = ymin, ymax = ymax, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() + 
    coord_polar(theta = "y") +
    theme_void() +
    ggtitle(title) +
    labs(fill = variable_name) +
    scale_fill_brewer(palette = "Greens") + #replace the color of your choice, I used "Blues" and "Greens"
    geom_text(aes(x = 3.5, y = ytext, label = scales::percent(fraction)), size = 5)
}
plot_pie_chart(database, "Type_risk", "Pie Chart for Type Risk")

# Type of fuel
plot_pie_chart(database, "Diesel", "Pie Chart for fuel type")

# Frequency modalities of categorical variables table
create_summary_table(database, "Broker")
create_summary_table(database, "newClient")
create_summary_table(database, "Policies")
create_summary_table(database, "Lapse")
create_summary_table(database, "Payment")
create_summary_table(database, "Urban")
create_summary_table(database, "Age")
create_summary_table(database, "Licence")
create_summary_table(database, "N_claims")
create_summary_table(database, "Second_driver")

#frequency of claims
ggplot(database, aes(x=N_claims))+
  geom_bar()+
  geom_label(stat='count', 
             aes(label =  percent(prop.table(after_stat(count)), 
                                  accuracy = 0.01)),vjust = 0.5)+
  ggtitle("Proportion of policies by number of claims")


# 2. Bivariate analysis

# Box plot
ggplot(database, aes(x = Policies_in_force, y = Premium)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(x = "Total number of policies", y = "Premium") +
  ggtitle("Box Plot of Premium by Total number of policies")

# Relation between the type of fuel and premium
ggplot(database, aes(x = Diesel, y = Premium)) + 
  geom_boxplot() +
  labs(x = "Type of fuel", y = "Premium") +
  ggtitle("Box Plot of Premium by the type of fuel")

# Contigency table
tableau <- table(database$Type_risk, database$N_claims)
ggplot(data = database, aes(x = factor(Type_risk), fill = factor(N_claims))) +
  geom_bar(position = "fill") +
  labs(x = "Type risk", y = "Proportion", fill = "nb claims") +
  theme_minimal() +
  coord_flip()
tableau

cor(database_quant)
ggpairs(database_quant)

# Summary table
dfquanti <- database %>%
  select(Premium, Cost_claims_year,Power,Cylinder_capacity,Value_vehicle,Length,
         Weight)
sumtable <- as.data.frame(c(0,0,0,0,0,0))
row.names(sumtable) <- c('Min', 'Q1', 'Median','Mean','Q3','Max')

for (i in 1:ncol(dfquanti)){
  x <- as.vector(dfquanti[,i])
  min <- min(x)
  Q1 <- unname(quantile(x,.25))
  Median <- unname(quantile(x,.5))
  Mean <- mean(x)
  Q3 <- unname(quantile(x,.75))
  max <- max(x)
  sumtable <- sumtable %>% cbind(c(min,Q1,Median,Mean,Q3,max))
}
colnames(sumtable) <- c("0",colnames(dfquanti))
sumtable <- sumtable %>% select(-"0")
latex_table <- xtable(sumtable, caption = "Summary Statistics")
print(latex_table, type = "latex", include.rownames = TRUE, hline.after = 
        c(-1, 0, nrow(sumtable)))