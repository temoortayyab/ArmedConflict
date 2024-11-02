#sourcing previously created scritps to obtain datasets from them to merge
library(here)
source(here("R", "code_disaster.R"))
source(here("R", "conflict_Week3_code.R"))
source(here("R", "mortality_week3_code.R"))

covariates <- read.csv(here("data", "Original", "covariates.csv"), header = TRUE)

#renaming year to Year
covariates <- covariates %>%
  rename(Year = year)

#putting all data frames into list
alllist <- list(new_combined, conflict1, data4)

#merging all data frames in list
alllist |> reduce(full_join, by = c('ISO', 'Year')) -> finaldata0

finaldata <- covariates |>
  left_join(finaldata0, by = c('ISO', 'Year'))

#checking where values are NA
na_counts <- sapply(finaldata, function(x) sum(is.na(x)))

# Display columns with NA values
na_columns <- na_counts[na_counts > 0]
print("Columns with NA values:")
print(na_columns)

# need to fill in NAs with 0's for conflict, drought, earthquake
finaldata <- finaldata |>
  mutate(conflict = replace_na(conflict, 0),
         Drought = replace_na(Drought, 0),
         Earthquake = replace_na(Earthquake, 0))

#counting if there are 20 rows for each country
iso_counts <- finaldata %>%
  group_by(ISO) %>%
  summarise(count = n(), .groups = 'drop')

#checking if there are any countries with less or more than 20 rows
countries_other_than_20 <- iso_counts %>%
  filter(count != 20)
print(countries_other_than_20) #None

write.csv(finaldata, file = here("data", "analytical", "finaldata.csv"), row.names = FALSE)

#Classifying Level and Level 2 variables
str(finaldata)

# Define level-1 and level-2 variables
level_1_vars <- c("Year", "gdp1000", "popdens", "agedep", "male_edu", "temp", "rainfall1000", "MarMor", "InfMort", "NeonatalMort", "Under5Mort", "total_deaths", "conflict", "Drought", "Earthquake")  # Level-1 variables
level_2_vars <- c("country_name", "ISO", "region", "OECD", "OECD2023", "urban")  # Level-2 variables

# Create a data frame for level-1 and level-2 variables
level_1_data <- finaldata[level_1_vars]
level_2_data <- finaldata[level_2_vars]

# Check the created data frames
print(head(level_1_data))
print(head(level_2_data))

#creating a list of level 1 and level 2
#list of variables by level 1 or level 2
variable_categories <- list(
  level_1 = level_1_vars,
  level_2 = level_2_vars
)

# Print the variable categories
print(variable_categories)
