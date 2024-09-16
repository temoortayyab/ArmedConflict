#setting git username and email address if you don't have one
#install.packages("usethis")
#library(usethis) 

#usethis::use_git_config(user.name = "My Name", user.email = "myemail@email.com")

# to confirm, generate a git situation-report, your user name and email should appear under Git config (global)
#usethis::git_sitrep()


#push existing projects to github <- slide 21
#after this, use version control when get updates

#Reading in file
library(here)
here()
rawdat <- read.csv(here("Original", "maternalmortality.csv"), header = TRUE)

####Data manipulation

#subsetting data
library(dplyr)
data1 <- rawdat %>% select(Country.Name, (matches("^X20(0|1)[0-9]$")))

# Removing 'X' from the beginning of variables
colnames(data1) <- sub("^X", "", colnames(data1))


#wide to long format change
library(tidyr)
data1_long <- data1 %>%
  pivot_longer(
    cols = c(starts_with('20')),   # Include Country.Name and year columns starting with 20
    names_to = 'Year',
    values_to = 'MatMor'
  )

write.csv(data1_long, here("data", "processeddata.csv"), row.names = FALSE)

#pushing to github
library(usethis) 
use_git()


usethis::use_git_config(user.name = "temoortayyab", user.email = "temoortayyab@gmail.com")
gitcreds::gitcreds_set()

use_github()




# to confirm, generate a git situation-report, your user name and email should appear under Git config (global)
usethis::git_sitrep()
