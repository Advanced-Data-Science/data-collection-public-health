# Ainsley Williams
# STAT/CS 3870
# T4 Exploratory Data Analysis - 1.1 Summary Statistics

########## PRE-DATA SET-UP ##########
# Read packages
pacman::p_load(readxl, tidyverse)

# Loading in dataset and making adjustments necessary for multivariate analysis
FDA <- read_excel("~/Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/T4 Exploratory Data Analysis/FDAcleaned copy.xlsx")
FDA <- FDA |> 
  select(-event_classification) |> 
  mutate(product_classification_numeric = as.numeric(factor(product_classification, 
                                                            levels = c("Class I", "Class II", "Class III")))) |> 
  na.omit()
FDA$recall_year <- as.numeric(substr(FDA$recall_date, 7, 10)) # extract recall year
FDA$is_high_risk <- ifelse(FDA$product_classification_numeric == 1, 1, 0)

########## SUMMARY STATISTICS ##########
### Product Class Frequency
library(scales)
ggplot(FDA, mapping = aes(x = product_classification)) +
  geom_bar(fill = "aquamarine4") +
  labs(title = "Frequency of FDA Recalls by Product Class",
       x = "Product Classification",
       y = "Frequency") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()





