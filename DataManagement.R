###Patrick Wolff & Gaelin Kingston
### ECON310: Environmental Resource Economics Final Paper
### Dataset Compilation

# Installing and Loading Packages: readr, tidyverse, stargazer

# install.packages("readr")
# install.packages("tidyverse")
#install.packages("stargazer")
#install.packages("fixest")

library(readr)
library(tidyverse)
library(stargazer)
library(descr)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(varhandle) #To change variable types https://cran.r-project.org/web/packages/varhandle/varhandle.pdf
library(fixest) #For FE OLS

## Importing Massachusetts Datasets from 2009-2019

#2009 survey does not include data on PAYT or SMART programs
mass_msw_2009<-read.csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2009.csv")

#2010 survey does not include data on PAYT or SMART programs
mass_msw_2010 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2010.csv")

mass_msw_2011 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2011.csv")

mass_msw_2012 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2012.csv")

mass_msw_2013 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2013.csv")

mass_msw_2014 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2014.csv")

mass_msw_2015 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2015.csv")

mass_msw_2016 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2016.csv")

mass_msw_2017 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2017.csv")

mass_msw_2018 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2018.csv")

mass_msw_2019 = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/msw_mass_survey_2019.csv")


# Cleaning sets and dropping empty columns from bad excel import

# First drop-Empty Columns

new2011 = mass_msw_2011[, 1:65]

new2012 = mass_msw_2012[, 1:71]

new2013 = mass_msw_2013[, 1:79]

new2014 = mass_msw_2014[, 1:84]

new2015 = mass_msw_2015[1:352, 1:86]

new2016 = mass_msw_2016[1:352, 1:93]

new2017 = mass_msw_2017[1:352, 1:99]

new2018 = mass_msw_2018[1:352, 1:99]

new2019 = mass_msw_2019[, 1:99]

# Adding year columns to each

new2011$year = 2011

new2012$year = 2012

new2013$year = 2013

new2014$year = 2014

new2015$year = 2015

new2016$year = 2016

new2017$year = 2017

new2018$year = 2018

new2019$year = 2019

# Subsetting to important variables for the simple sets and renaming where necessary for clean join
# renaming: PAYT/SMART columns -> "PAYT", NA's recoded as 0, 'Yes' recoded as 1

#2011

simple_2011 = new2011 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, PAYT, `Trash Service Type`, year)

simple_2011$PAYT = ifelse(simple_2011$PAYT == "Yes", 1, 0)
simple_2011$PAYT[is.na(simple_2011$PAYT)] = 0

colnames(simple_2011) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2012

simple_2012 = new2012 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, PAYT, `Trash Service Type`, year)

simple_2012$PAYT = ifelse(simple_2012$PAYT == "Yes", 1, 0)
simple_2012$PAYT[is.na(simple_2012$PAYT)] = 0

colnames(simple_2012) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2013

simple_2013 = new2013 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, PAYT, `Trash Service Type`, year)

simple_2013$PAYT = ifelse(simple_2013$PAYT == "Yes", 1, 0)
simple_2013$PAYT[is.na(simple_2013$PAYT)] = 0

colnames(simple_2013) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2014
simple_2014 = new2014 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, PAYT, `Trash Service Type`, year)

simple_2014$PAYT = ifelse(simple_2014$PAYT == "Yes", 1, 0)
simple_2014$PAYT[is.na(simple_2014$PAYT)] = 0

colnames(simple_2014) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2015
simple_2015 = new2015 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, PAYT, `Trash Service Type`, year)

simple_2015$PAYT = ifelse(simple_2015$PAYT == "Yes", 1, 0)
simple_2015$PAYT[is.na(simple_2015$PAYT)] = 0

colnames(simple_2015) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2016
simple_2016 = new2016 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `PAYT/ SMART`, `Trash Service Type`, year)

simple_2016$`PAYT/ SMART` = ifelse(simple_2016$`PAYT/ SMART` == "Yes", 1, 0)
simple_2016$`PAYT/ SMART`[is.na(simple_2016$`PAYT/ SMART`)] = 0

colnames(simple_2016) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2017
simple_2017 = new2017 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `PAYT/ SMART`, `Trash Service Type`, year)

simple_2017$`PAYT/ SMART` = ifelse(simple_2017$`PAYT/ SMART` == "Yes", 1, 0)
simple_2017$`PAYT/ SMART`[is.na(simple_2017$`PAYT/ SMART`)] = 0

colnames(simple_2017) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2018
simple_2018 = new2018 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `PAYT/ SMART`, `Trash Service Type`, year)

simple_2018$`PAYT/ SMART` = ifelse(simple_2018$`PAYT/ SMART` == "Yes", 1, 0)
simple_2018$`PAYT/ SMART`[is.na(simple_2018$`PAYT/ SMART`)] = 0

colnames(simple_2018) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


#2019
simple_2019 = new2019 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `PAYT/ SMART`, `Trash Service Type`, year)

simple_2019$`PAYT/ SMART` = ifelse(simple_2019$`PAYT/ SMART` == "Yes", 1, 0)
simple_2019$`PAYT/ SMART`[is.na(simple_2019$`PAYT/ SMART`)] = 0

colnames(simple_2019) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year")


# Joining Sets

simple_2011_2019 = rbind(simple_2011,
                         simple_2012,
                         simple_2013,
                         simple_2014,
                         simple_2015,
                         simple_2016,
                         simple_2017,
                         simple_2018,
                         simple_2019)

#normalizing text case for municipality names -- lowercase

simple_2011_2019$municipality = tolower(simple_2011_2019$municipality)

#dropping incomplete cases (n = 3168 with NA's)

complete_data_2011_2019 = na.omit(simple_2011_2019) #dropped rows with any remaining NA's, n = 2490

#dropping observations with 0 reported waste tonnage

complete_data_2011_2019 = complete_data_2011_2019[complete_data_2011_2019$trash_tonnage != 0, ] # n = 2347


#checking distribution of complete cases by year

complete_data_2011_2019 %>% 
  dplyr::group_by(year) %>%  #Added "dplyy::" because it would not run for me without this (Gaelin)
  dplyr::summarize(n = n())

#most observations in a year: 2018, n = 284
#least observations in a year: 2013, n = 219

# converting categorical variables to factor type for summary purposes

complete_data_2011_2019$service_type = as.factor(complete_data_2011_2019$service_type)
complete_data_2011_2019$PAYT = as.factor(complete_data_2011_2019$PAYT)


# messy output

summary(complete_data_2011_2019)  

# clean table of means (at least for numerical vars)

stargazer(as.data.frame(complete_data_2011_2019), type = "latex", out = "tab_of_means.txt")

#income dataset, includes population figures as well
income = read_csv("https://raw.githubusercontent.com/GaelinKingston/ECON-310-Project-/main/Data/income.csv")

# joining original set with socioeconomic controls and cleaning out identifiers

data_with_controls = left_join(complete_data_2011_2019, income, by = c("municipality" = "Municipality", "year" = "Cherry Sheet FY"))

data_with_controls = na.omit(data_with_controls %>% select(municipality, trash_tonnage, num_households, PAYT, service_type, year, Population, `DOR Income`, `DOR Income Per Capita`, EQV, `EQV Per Capita`))

# Renaming Odd Variables (DOR)

colnames(data_with_controls) = c("municipality", "trash_tonnage", "num_households", "PAYT", "service_type", "year", "population", "income", "income_pc", "EQV", "EQV_pc" )

#  scatterplot 

scatter_set = data_with_controls %>% 
   group_by(municipality) %>% 
   mutate(avg_pop = mean(population), avg_tonnage = mean(trash_tonnage)) 
   
scatter_set %>% 
   ggplot(aes(x = avg_pop, y = avg_tonnage)) + geom_point()

#  dropping boston

data_with_controls = data_with_controls[data_with_controls$municipality != "boston",]

#  feols without boston

no_boston_fe = feols(trash_tonnage~PAYT + PAYT*service_type + population + income_pc | factor(municipality) + factor(year), data = yankee_set, se = "hetero")

summary(no_boston_fe)

#   Decluttering environment after combinations (add anything to this list that was temporary in the script)

rm(mass_msw_2009,
   mass_msw_2010,
   mass_msw_2011,
   mass_msw_2012,
   mass_msw_2013,
   mass_msw_2014,
   mass_msw_2015,
   mass_msw_2016,
   mass_msw_2017, 
   mass_msw_2018, 
   mass_msw_2019,
   new2011,
   new2012,
   new2013,
   new2014,
   new2015,
   new2016,
   new2017,
   new2018,
   new2019,
   simple_2011,
   simple_2012,
   simple_2013,
   simple_2014,
   simple_2015,
   simple_2016,
   simple_2017,
   simple_2018,
   simple_2019,
   complete_data_2011_2019,
   income)

# making the data panel format: adding year columns indicating whether or not a municipality switched to PAYT in that year

# ^^^ still needs work for panel, code below generates a column indicating the change in PAYT from year to year. 0 is no change, 1 is a change from no to yes, -1 is a change from yes to no

data_with_controls$PAYT = as.numeric(as.character(data_with_controls$PAYT))
#
data_with_controls = data_with_controls %>%
    group_by(municipality) %>%
    mutate(PAYT.change = PAYT - lag(PAYT))
 
# data_with_controls$PAYT = as.factor(data_with_controls$PAYT)

# as of now, 2011 is all NA and there are some with missing entries where changes can't be detected, a start

# checking for balanced panel (municipalities that have observations for every year in the dataset)

data_with_controls %>% 
   group_by(municipality) %>% 
   count() -> balanced_panel

balanced_panel %>% 
   filter(n >= 9)

data_with_controls %>% 
   filter(PAYT.change == 1) -> to_payt

to_payt %>% 
   count(municipality)

data_with_controls %>% 
   group_by(year) %>% 
   dplyr::summarize(avg_trash = mean(trash_tonnage))



### SLR and MLR ###

slr_1 = lm(trash_tonnage ~ PAYT, data = data_with_controls)

summary(slr_1)

mlr_1 = lm(trash_tonnage ~ PAYT + income_pc , data = data_with_controls)

summary(mlr_1)

mlr_2 = lm(trash_tonnage ~ PAYT + income_pc + population , data = data_with_controls)

summary(mlr_2)

mlr_3 = lm(trash_tonnage ~ PAYT + PAYT*service_type + income_pc + population , data = data_with_controls)

summary(mlr_3)

ln_slr_1 = lm(log(trash_tonnage) ~ PAYT, data = data_with_controls)

summary(ln_slr_1)

ln_mlr_1 = lm(log(trash_tonnage) ~ PAYT + income_pc , data = data_with_controls)

summary(ln_mlr_1)

ln_mlr_2 = lm(log(trash_tonnage) ~ PAYT + income_pc + population , data = data_with_controls)

summary(ln_mlr_2)

ln_mlr_3 = lm(log(trash_tonnage) ~ PAYT + PAYT*service_type + income_pc + population , data = data_with_controls)

summary(ln_mlr_3)


stargazer(slr_1, ln_slr_1, type = "latex", out = "regression_output_first")

stargazer(mlr_1, mlr_2, mlr_3, type = "latex")

stargazer(ln_mlr_1, ln_mlr_2, ln_mlr_3, type = "latex")

# Diff in diff should have an xt and a Wi

## Question for consultants: what would cause a municipality to switch from PAYT to not PAYT?
## Move from slr->diff in diff->fixed effects/event history, talk about the progression towards a more unbiased model




### Code for fixed effect OLS regression


#Change service_type from a factor to a character
data_with_controls$service_type <- as.character(data_with_controls$service_type)

#Changing "service_type"response values to 1 if "curbside" or "Both" and 0 if "dropoff"
data_with_controls$service_type[data_with_controls$service_type == "Drop-off"] <- 0
data_with_controls$service_type[data_with_controls$service_type == "Curbside" | data_with_controls$service_type == "Both"] <- 1

# Dropping observations with "none" for service_type
data_with_controls = data_with_controls[data_with_controls$service_type != "None", ]

#Change service_type to numeric from character
data_with_controls$service_type <- as.numeric(data_with_controls$service_type)

freq(data_with_controls$service_type)

#FE OLS Regression code without controls
fe_reg1 <- feols(trash_tonnage~PAYT + PAYT*service_type | factor(municipality) + factor(year), data=data_with_controls, se = "cluster") #last element provides heteroscedisity assesment

summary(fe_reg1)

#FE OLS Regression code with controls
fe_reg2 <- feols(trash_tonnage~PAYT + PAYT*service_type + population + income_pc | factor(municipality) + factor(year), data=data_with_controls, se = "cluster") #last element provides heteroscedisity assesment

summary(fe_reg2)

# FE OLS w/ logs without controls

log_fe_reg1 <- feols(log(trash_tonnage)~PAYT + PAYT*service_type | factor(municipality) + factor(year), data=data_with_controls, se = "cluster") #last element provides heteroscedisity assesment

summary(log_fe_reg1)

# FE OLS w/ logs with controls

log_fe_reg2 <- feols(log(trash_tonnage)~PAYT + PAYT*service_type + population + income_pc | factor(municipality) + factor(year), data=data_with_controls, se = "cluster") #last element provides heteroscedisity assesment

summary(log_fe_reg2)

# for final regression table

etable(fe_reg1, fe_reg2, log_fe_reg1, log_fe_reg2, tex = T)

# Question for MA DEEP consultants: what other parallel trends could potentially be affecting this relationship? What could defeat this parallel assumption?



### Code for Event History Model

#  discussion section: more data could provide grounds to revisit the investigation of the effectiveness of this policy through an event history analysis

# create two figures: one is a line (just the data)
#  the other is a regression
#   year (current) - payt(first year)


data_with_controls %>% 
   ggplot(aes(x = PAYT)) + geom_histogram(stat = "count") + facet_wrap(data_with_controls$year)

first_last_entry.df <- data_with_controls %>%
   group_by(municipality) %>%
   filter(PAYT.change == 1) %>%
   mutate(payt_entry_first = min(year, na.rm = T)) %>%
   select(municipality, payt_entry_first) %>%
   distinct()

event_hist_data = data_with_controls %>% 
   left_join(first_last_entry.df, by = c("municipality" = "municipality"))

event_hist_data$years_to_PAYT = event_hist_data$year - event_hist_data$payt_entry_first


event_hist_agg = event_hist_data %>%
   ungroup() %>% 
   group_by(years_to_PAYT) %>% 
   summarize(avg_trash = mean(trash_tonnage))

#DISCUSSION SECTION

event_hist_data %>%
   ungroup() %>% 
   group_by(years_to_PAYT) %>% 
   dplyr::summarise(n = n_distinct(municipality))

p5 <- ggplot(event_hist_agg, aes(x=years_to_PAYT, y=avg_trash)) +
   geom_line(color = "#e34a33") +
   geom_vline(xintercept = 0, linetype = "solid", color = "grey30")

p5
















