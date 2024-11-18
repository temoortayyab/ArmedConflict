#loading libraries
library(here)
library(dplyr)
library(boot)

#loading data
final <- read.csv(here("data", "analytical", "finaldata.csv"))

#Setting seed
set.seed(2024)

#Creating a function for calculating bootstrap CIs based on difference in median
bootstrap_ci <- function(data, response_var, group_var, R = 1000) {
  # Define the bootstrapping function
  getmeddiff <- function(data, indices) {
    sample_data <- data[indices, ]
    group_meds <- tapply(sample_data[[response_var]], sample_data[[group_var]], 
                         FUN = function(x) median(x, na.rm = TRUE))
    meddiff <- group_meds[2] - group_meds[1]
    return(meddiff)
  }
  
  # Perform bootstrapping
  bootout <- boot(data, statistic = getmeddiff, strata = data[[group_var]], R = R)
  
  # Compute bootstrap confidence intervals
  ci <- boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
  
  # Return the results
  list(
    observed_diff = bootout$t0,
    ci = ci
  )
}


#Specifying each variable and bootstrapping, and obtaining CIs
# Subset data for 2017
data2017 <- final |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(if_all(c(InfMort, NeonatalMort, Under5Mort), ~ !is.na(.)))


# List of variables to analyze
variables <- c("InfMort", "NeonatalMort", "Under5Mort")

# Apply the function to each variable and store results in a list
results <- lapply(variables, function(var) {
  bootstrap_ci(data = data2017, response_var = var, group_var = "conflict", R = 1000)
})

# Access results for each variable
names(results) <- variables

# Example: Print results 
results


