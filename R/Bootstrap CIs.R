#loading libraries
library(here)
library(dplyr)
library(boot)

#loading data
final <- read.csv(here("data", "analytical", "finaldata.csv"))


#Creating a function for calculating bootstrap CIs based on difference in median
bootstrap_ci <- function(data, response_var, group_var, R = 1000, probs = c(0.025, 0.975)) {
  # Ensure the response_var and group_var are interpreted as strings
  response <- data[[response_var]]
  group <- data[[group_var]]
  
  # Define the bootstrapping statistic function
  getmeddiff <- function(data, indices) {
    sample_data <- data[indices, ]
    group_meds <- tapply(sample_data[[response_var]], sample_data[[group_var]], FUN = function(x) median(x, na.rm = TRUE))
    meddiff <- group_meds[2] - group_meds[1] # Ensure group levels are in the correct order
    return(meddiff)
  }
  
  # Perform bootstrapping
  bootout <- boot(data, statistic = getmeddiff, strata = group, R = R)
  
  # Calculate bootstrap standard error
  se <- sd(bootout$t)
  
  # Calculate percentile confidence intervals
  ci <- quantile(bootout$t, probs = probs)
  
  # Return the results
  list(
    observed_diff = bootout$t0,
    bootstrap_se = se,
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
results$InfMort
results$NeonatalMort
results$Under5Mort


