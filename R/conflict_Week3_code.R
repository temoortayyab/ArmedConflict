conflict <- read.csv(here("data", "Original", "conflictdata.csv"), header = TRUE)

#creating a binary variable based on 25 deaths as a threshold. But since multiple conflicts
#per year for some countries, have to add

conflict_data <- conflict %>%
  group_by(ISO, year) %>%
  # Create a new variable that codes 1 if deaths are 25 or more, otherwise 0
  mutate(conflict = ifelse(sum(best, na.rm = TRUE) >= 25, 1, 0)) %>%
  ungroup()

#collapsing the observations by ISO and year 
conflict1 <- conflict_data %>%
  group_by(ISO, year) %>%
  summarize(
    total_deaths = sum(best, na.rm = TRUE),       # Total deaths
    conflict = max(conflict, na.rm = TRUE),  # Take the maximum indicator value
    .groups = 'drop'  # Ungroup after summarizing
  )

#adding 1 to each year as there is a lag in the analysis paper
conflict1 <- conflict1 %>%
  mutate(year = year + 1)

#renaming year to Year
conflict1 <- conflict1 %>%
  rename(Year = year)

write.csv(conflict1, here("data", "conflict.csv"), row.names = FALSE)
