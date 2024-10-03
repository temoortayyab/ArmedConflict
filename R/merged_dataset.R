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