##### The R script for my R project called "Lockdowns vs Liberty project"
# load the library or libraries needed
library(tidyverse)


# load in the first overall covid dataset from Our World in Data
getwd()
#setwd("C:/UsersSpencer/OneDrive/Documents/Analytics Projects/Covid Policies, Liberty, & Human Flourishing project/Our World in Data datasets")
owid_new <- read_csv("owid covid data new (as of 7-5-2023).csv")
class(owid_new)
str(owid_new)
sapply(owid_new, function(column) length(unique(column)))
length(unique(owid_new$location))
str(owid_new$stringency_index)

owid_old_12_4_2020 <- read_csv("owid covid data (as of 12-4-2020).csv")
str(owid_old_12_4_2020)

owid_old_3_7_2023 <- read_csv("owid covid data old (as in March 7th, 2023).csv")
str(owid_old_3_7_2023)
head(owid_old_3_7_2023, n = 3)





# load in the excess mortality dataset
excess_deaths <- read_csv("excess mortality.csv")
str(excess_deaths)
length(unique(excess_deaths$location))



# load in the death rate (per million) dataset from OWID
tdpm_data <- read_csv("total deaths per million.csv")
class(tdpm_data)
head(tdpm_data$Zimbabwe)



# load in the Oxford COVID-19 Government Response Tracker (OxCGRT) datasets
Ox_Gov_Tracker <- read_csv("OxCGRT_USA_differentiated_withnotes_2020.csv")



# load in the human freedom index
Cato <- read_csv("human-freedom-index-2022.csv")








##### Regressions Analysis of lockdown severity on various
##### covid health outcomes like the death rate & excess mortality
# Start out by running a simple linear regression of
# total deaths per million on Oxford's lockdown stringency_index 
lockdown_stringency <- owid_old_3_7_2023$stringency_index
stringency_SLR <- lm(formula = total_deaths_per_million ~ lockdown_stringency, 
                     data = owid_old_3_7_2023)
coef(stringency_SLR)
summary(stringency_SLR)
plot(stringency_SLR)
plot(stringency_SLR, total_deaths_per_million)



# create the initial multiple linear regression model and assign it to an object
death_rate_model1 <- lm(formula = total_deaths_per_million ~ stringency_index + median_age + aged_70_older +  cardiovasc_death_rate + diabetes_prevalence + life_expectancy + female_smokers + male_smokers, 
             data = owid_old_3_7_2023)
coef(death_rate_model1)
summary(death_rate_model1)
plot(death_rate_model1)


excess_mortality_model1 <- lm(formula = excess_mortality_cumulative_per_million ~ stringency_index + median_age + aged_70_older + cardiovasc_death_rate + diabetes_prevalence + life_expectancy + female_smokers + male_smokers, 
                        data = owid_old_3_7_2023)
coef(excess_mortality_model1)
summary(excess_mortality_model1)
plot(excess_mortality_model1)



# Create another multiple regression model with a different set of control 
# variables chosen, this time, ones which are not directly about health but still
# could play a role in the death rate, and assign that model to an object.
# For this model, back to total deaths per million as the dependent variable, 
# remove diabetes prevalence, median age, populous over 70, cardiovascular death rate
# diabetes prevalence, life expectancy, and prevalence of smoking from the list 
# of explanatory variables included and add gdp per capita, rate of extreme poverty,
# population density, and the prevalence of hand washing facilities as their replacements.
death_rate_model2 <- lm(formula = total_deaths_per_million ~  stringency_index + population_density + handwashing_facilities + extreme_poverty + gdp_per_capita,
            data = owid_old_3_7_2023)
coef(death_rate_model2)
summary(death_rate_model2)
plot(death_rate_model2)






