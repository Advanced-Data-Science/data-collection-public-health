# STAT 3870 Preprocessing
# FDA Data
# Ainsley Williams

library(dplyr)
library(writexl)
library(readxl)

##### Raw data #####
FDA <- read.csv("Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Project/Preprocessing/FDA.csv",
                na.strings = c("-", "NA"))
summary(FDA)
ncol(FDA) # 17 variables beginning
nrow(FDA) # 98333 observations
sum(is.na(FDA)) # 4768 missing values in FEI.Number
colSums(is.na(FDA)) # Mostly by state and reason for recall

##### Cleaning the data #####
# Remove rows with missing values
FDA1 <- na.omit(FDA) # NO MISSING VALUES!

# Rename variables
FDAcleaned <- FDA1 |> 
  select(-Recalling.Firm.Country, -Recalling.Firm.City, -Recall.Details) |> # remove unecessary variables
  rename(product_classification = Product.Classification, # rename to make data easier to read
         firm = Recalling.Firm.Name,
         product_type = Product.Type,
        status = Status,
        state = Recalling.Firm.State,
        recall_date = Center.Classification.Date,
        product_id = Product.ID,
        event_classification = Event.Classification,
        event_id = Event.ID,
        center = Center,
        distribution = Distribution.Pattern,
        reason = Reason.for.Recall,
        product_description = Product.Description)

# Going to filter by fiscal year by hand - too difficult in R as data is a chr variable

##### Convert to Excel file #####
write_xlsx(FDAcleaned, "Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Project/Preprocessing/FDAcleaned.xlsx")




