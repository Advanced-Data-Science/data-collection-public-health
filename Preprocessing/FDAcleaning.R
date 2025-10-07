# STAT 3870 Preprocessing
# FDA Data
# Ainsley Williams

library(dplyr)
library(writexl)

##### Raw data #####
FDA <- read.csv("Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/Project/Preprocessing/FDA.csv",
                na.strings = c("", "NA"))
summary(FDA)
ncol(FDA) # 13 variables beginning
nrow(FDA) # 78734 observations
sum(is.na(FDA)) # 4400 missing values in FEI.Number
colSums(is.na(FDA)) # Mostly by state

##### Cleaning the data #####
# Remove rows with missing values
FDA1 <- na.omit(FDA) # NO MISSING VALUES!

# Rename variables
FDA2 <- FDA1 |> 
  select(-Recalling.Firm.Country, -Recalling.Firm.City) |> 
  rename(product_classification = Product.Classification,
         firm = Recalling.Firm.Name,
         product_type = Product.Type,
        status = Status,
        state = Recalling.Firm.State,
        recall_date = Center.Classification.Date,
        product_id = Product.ID,
        event_classification = Event.Classification,
        event_id = Event.ID,
        center = Center) |> 
  arrange(recall_date)
# 11 variables

##### Convert to Excel file #####
write_xlsx(FDA2, "Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/Project/Preprocessing/FDAcleaned.xlsx")




