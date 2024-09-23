#sourcing previously created scritps to obtain datasets from them to merge
source(here("R", "code_disaster.R"))
source(here("R", "conflict_Week3_code.R"))
source(here("R", "mortality_week3_code.R"))

#merging datasets based on year and ISO
merged_data <- data4 %>% #disaster data
  full_join(new_combined, by = c("ISO", "Year")) %>% #mortality data
  full_join(conflict1, by = c("ISO", "Year")) #conflict data

#counting if there are 20 rows for each country
iso_counts <- merged_data %>%
  group_by(ISO) %>%
  summarise(count = n(), .groups = 'drop')

countries_other_than_20 <- iso_counts %>%
  filter(count != 20)
print(countries_other_than_20) #there are some

write.csv(merged_data, here("data", "merged_data.csv"), row.names = FALSE)
