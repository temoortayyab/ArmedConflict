#libraries
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(countrycode)

#Reading in data
maternal_raw <- read.csv(here("data","Original", "maternalmortality.csv"), header = TRUE)
infant_raw <- read.csv(here("data","Original", "infantmortality.csv"), header = TRUE)
neonatal_raw <- read.csv(here("data","Original", "neonatalmortality.csv"), header = TRUE)
under5_raw <- read.csv(here("data","Original", "under5mortality.csv"), header = TRUE)


# Define the data cleaning function
data_cleaning_fx <- function(data, values_col_name) {
  # Data manipulation
  # Subsetting data
  data1 <- data %>% select(Country.Name, matches("^X20(0|1)[0-9]$"))
  
  # Removing 'X' from the beginning of variables
  colnames(data1) <- sub("^X", "", colnames(data1))
  
  # Wide to long format change
  data1_long <- data1 %>%
    pivot_longer(
      cols = starts_with('20'),
      names_to = 'Year',
      values_to = values_col_name
    )
  
  return(data1_long)
}

#Applying functions to different datasets
maternal_mortality_cleaned <- data_cleaning_fx(maternal_raw, "MarMor")
infant_mortality_cleaned <- data_cleaning_fx(infant_raw, "InfMort")  
neonatal_mortality_cleaned <- data_cleaning_fx(neonatal_raw, "NeonatalMort")  
under5_mortality_cleaned <- data_cleaning_fx(under5_raw, "Under5Mort")

#quality checks
combined <- list(maternal_mortality_cleaned, infant_mortality_cleaned, neonatal_mortality_cleaned, under5_mortality_cleaned)
lapply(combined, FUN = summary)

#creating a dataframe and then subsetting
#basically reduce fx applies to list. reduce it to something else. and you are doing it by
#full_join. reduces lists into dataframe. 
combineddata <- reduce(combined, full_join, by = c("Country.Name", "Year"))

new_combined <- combineddata %>%
  select(Country.Name, Year, MarMor, InfMort, NeonatalMort, Under5Mort)

#adding built in country codes that R has and then removing Country.Name
new_combined$ISO <- countrycode(new_combined$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")

#warnings since applying country codes to regions which show up as NA but not important 
#for this assignment

#dropping country 
new_combined <- new_combined %>%
  select(-Country.Name)

#Year is a character variable. Convert to numeric
new_combined <- new_combined %>%
  mutate(Year = as.numeric(Year))


write.csv(new_combined, here("data", "allmortality.csv"), row.names = FALSE)
