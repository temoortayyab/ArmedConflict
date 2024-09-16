#Reading in file
library(here)
here()
rawdat1 <- read.csv(here("Original", "disaster.csv"), header = TRUE)

##filtering
library(dplyr)
data1 <- rawdat1 %>%
  filter(Year >= 2000 & Year <= 2019, 
         Disaster.Type %in% c("Earthquake", "Drought"))

#Subsetting
subset_data <- data1 %>%
  select(Year, ISO, Disaster.Type)

#Dummy variables
subset_data <- subset_data %>%
  mutate(
    Drought = as.integer(Disaster.Type == "Drought"),
    Earthquake = as.integer(Disaster.Type == "Earthquake")
  )


data3 <- subset_data %>%
  group_by(ISO, Year) %>%
  summarize(
    Drought = max(Drought),        # If any entry is 1, result will be 1
    Earthquake = max(Earthquake),  # If any entry is 1, result will be 1
    .groups = 'drop'               # Drop the grouping structure
  )
