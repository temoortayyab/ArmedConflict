#libraries
library(here)
library(mice)
library(dplyr)
library(texreg)

#load data
finaldata <- read.csv(here("data", "analytical", "finaldata.csv"), header = TRUE)

#Examining missingness
missing_summary_df <- data.frame(
  Variable = names(finaldata),
  Missing_Count = sapply(finaldata, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(finaldata, function(x) mean(is.na(x))) * 100
)

# Filter for variables with any missing values
missing_summary_df <- missing_summary_df[missing_summary_df$Missing_Count > 0, ]

# Display the summary
print(missing_summary_df)
#A lot of missigness in the MarMor variable

#logging gdp
finaldata$logGDP <- log(finaldata$gdp1000)

##Multiple Imputation
midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO, -gdp1000)

#initialize a mice object, not imputing yet
mice0  <- mice(midata, seed = 100, m = 10, maxit = 0, print = F)

#Meth and pred
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "MarMor", "InfMort", "NeonatalMort", "Under5Mort", "logGDP", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "MarMor", "InfMort", "NeonatalMort", "Under5Mort", "logGDP", "popdens"), "ISOnum"] <- -2

#MI with 10 imputations
mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

#Checking for convergence
plot(mice.multi.out) #fails

#Running regression models on each imputed dataset
#with() here will run lm() on each imputed dataset from mice.multi.out
#note cannot use the update.formula thing here with multiple imputations here unlike the loggdp doc (did not do multiple imputations here)

matmormod <- with(mice.multi.out, lm(MarMor ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                                      agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                                      + as.factor(ISOnum) + as.factor(Year)))

un5mormod <- with(mice.multi.out, lm(Under5Mort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                            agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                            as.factor(Year)))

infmormod <- with(mice.multi.out, lm(InfMort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       as.factor(Year)))

neomormod <- with(mice.multi.out, lm(NeonatalMort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       as.factor(Year)))

#pooling results for each mortality
matmormod_im <- pool(matmormod) #Maternal mortality
un5mormod_im <- pool(un5mormod) #Under 5 mortality
infmormod_im <- pool(infmormod) #Infant mortality
neomormod_im <- pool(matmormod) #Neonatal mortality

#Complete case results
matmormod_CC <- lm(MarMor ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       + as.numeric(as.factor(ISO)) + as.numeric(as.factor(ISO)) + as.factor(Year), data = finaldata)

un5mormod_CC <- lm(Under5Mort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       as.numeric(as.factor(ISO)) + as.factor(Year), data = finaldata)

infmormod_CC <- lm(InfMort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       as.numeric(as.factor(ISO)) + as.factor(Year), data = finaldata)

neomormod_CC <- lm(NeonatalMort ~ -1 + conflict + logGDP + OECD + popdens + urban +
                                       agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
                                       as.numeric(as.factor(ISO)) + as.factor(Year), data=finaldata)


#Table outputs
tosave <- list(matmormod_im, un5mormod_im, infmormod_im, neomormod_im, 
               matmormod_CC, un5mormod_CC, infmormod_CC, neomormod_CC)

keepvars <- list("conflict" = "armed conflict",
                 "logGDP" = "log(GDP)",
                 "OECD" = "OECD",
                 "popdens" = "population density",
                 "urban" = "urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "male education",
                 "temp" = "temperature",
                 "ISO" = "ISO",
                 "Year" = "Year")

screenreg(list(matmormod_im, matmormod_CC, un5mormod_im, un5mormod_CC, infmormod_im, infmormod_CC, neomormod_im, neomormod_CC), 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Mat CC", "Mat MI", "Un5 CC", "Un5 MI", "Inf CC", "Inf MI", "Neo CC", "Neo MI"))