# Ainsley Williams
# STAT/CS 3870
# T4 Exploratory Data Analysis - 1.4 Multivariate Analysis

##### QUESTIONS & PURPOSE #####
# One IV (class), several DVs (state, product_type)

########## PRE-DATA SET-UP ##########
# Read packages
pacman::p_load(readxl, tidyverse, Rfast, heplots, GGally, MVN, car, rstatix)
library(nnet)

# Loading in dataset and making adjustments necessary for multivariate analysis
FDA <- read_excel("~/Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/T4 Exploratory Data Analysis/FDAcleaned copy.xlsx")
FDA <- FDA |> 
  select(-event_classification) |> 
  mutate(product_classification_numeric = as.numeric(factor(product_classification, 
                                                          levels = c("Class I", "Class II", "Class III")))) |> 
  na.omit()
FDA$recall_year <- as.numeric(substr(FDA$recall_date, 7, 10)) # extract recall year
FDA$is_high_risk <- ifelse(FDA$product_classification_numeric == 1, 1, 0)

########## MULTIPLE LOGISTIC REGRESSION ##########
### Question: Can a product recall be predicted as high-risk using its recall year, status, and product type?
FDA_glm <- FDA |> 
  mutate(
    # Convert status to numeric (0, 1, 2)
    status_numeric = case_when(
      status == "Ongoing" ~ 1,
      status == "Terminated" ~ 2,
      status == "Completed" ~ 0,  # or whatever your third level is
      TRUE ~ NA_real_
    ),
    
    # Convert product_type to numeric (1-6)
    product_type_numeric = case_when(
      product_type == "Devices" ~ 1,
      product_type == "Drugs" ~ 2,
      product_type == "Food/Cosmetics" ~ 3,
      product_type == "Tobacco" ~ 4,
      product_type == "Veterinary" ~ 5,
      TRUE ~ NA_real_
    )
  )

### Full model:
FDA_multi_glm <- glm(is_high_risk ~ recall_year + status_numeric + product_type_numeric,
                     data = FDA_glm,
                     family = binomial(link = "logit"))
summary(FDA_multi_glm)

### McFadden Pseudo R2:
# Create a reduced model.
nullmod = glm(is_high_risk ~ 1, family=binomial, data = FDA_glm)
logLik(nullmod)
logLik(FDA_multi_glm)
1-logLik(FDA_multi_glm)/logLik(nullmod)
as.numeric( 1-logLik(FDA_multi_glm)/logLik(nullmod))
# Compute value
1 - (logLik(FDA_multi_glm)/logLik(nullmod)) # Pseudo R2 value: 0.1548215
# Not the strongest model, but if threshold is 0.2-0.4 then this isn't terrible.

### Interpretation:
# PASTE full logistic equation in document (format)
# For every 1 year increase, the odds ratio for high risk recall products is increased by exp(0.020869), controlling for status and product type.
# For every change in status (Ongoing, Terminated, Complete),the odds ratio for high risk recall products is increased by exp(-0.150999), controlling for product type and recall year.
# Between each product type, the odds ratio for high risk recall products is increased by exp(0.800709), controlling for recall year and product type.

### Plots:
library(ggplot2)
library(dplyr)
model_data <- FDA_multi_glm$model  # This gets the exact data used in fitting

# Add the outcome variable name back if needed
model_data$is_high_risk <- model_data[, 1]  # First column is always the outcome

# Add predictions
model_data$predicted_prob <- predict(FDA_multi_glm, type = "response")

# Now plot
ggplot(model_data, aes(x = predicted_prob, y = is_high_risk)) +
  geom_point(alpha = 0.5, position = position_jitter(height = 0.01)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              color = "skyblue", 
              size = 1.5,
              se = TRUE) +
  labs(title = "Risk Factor of Product Recalls, Predicted by Year, Status, and Product Type",
       x = "Predicted Probability of High Risk",
       y = "Actual Risk") +
  theme_minimal()


