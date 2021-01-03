#Prepare V - Dem dataset to be in the dashboard
library(haven)
# Set the working directory
setwd("C:/Users/escan/Dropbox/Horizontal/Dashboard/Shiny/Dataset")


## Importing stata dataset
Origin_data <- read_dta("CPD_V-Party_R_v1.dta")

## Saving dataset
saveRDS(Origin_data, file = "C:/Users/escan/OneDrive/Bureau/Github/Dashboard_V_Dem/Dashboard2/data/data_dashboard.rds")