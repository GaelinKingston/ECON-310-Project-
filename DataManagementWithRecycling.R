### Patrick Wolff & Gaelin Kingston
### ECON310: Environmental Resource Economics Final Paper
### Dataset Compilation

# Installing and Loading Packages: readr, tidyverse, stargazer

# install.packages("readr")
# install.packages("tidyverse")
# install.packages("stargazer")

library(readr)
library(tidyverse)
library(stargazer)

# Importing Massachusetts Datasets from 2009-2019

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
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, PAYT, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2011$PAYT = ifelse(simple_2011$PAYT == "Yes", 1, 0)
simple_2011$PAYT[is.na(simple_2011$PAYT)] = 0

colnames(simple_2011) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2012

simple_2012 = new2012 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, PAYT, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2012$PAYT = ifelse(simple_2012$PAYT == "Yes", 1, 0)
simple_2012$PAYT[is.na(simple_2012$PAYT)] = 0

colnames(simple_2012) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2013

simple_2013 = new2013 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, PAYT, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2013$PAYT = ifelse(simple_2013$PAYT == "Yes", 1, 0)
simple_2013$PAYT[is.na(simple_2013$PAYT)] = 0

colnames(simple_2013) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2014
simple_2014 = new2014 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, PAYT, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2014$PAYT = ifelse(simple_2014$PAYT == "Yes", 1, 0)
simple_2014$PAYT[is.na(simple_2014$PAYT)] = 0

colnames(simple_2014) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2015
simple_2015 = new2015 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, PAYT, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2015$PAYT = ifelse(simple_2015$PAYT == "Yes", 1, 0)
simple_2015$PAYT[is.na(simple_2015$PAYT)] = 0

colnames(simple_2015) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2016
simple_2016 = new2016 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, `PAYT/ SMART`, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2016$`PAYT/ SMART` = ifelse(simple_2016$`PAYT/ SMART` == "Yes", 1, 0)
simple_2016$`PAYT/ SMART`[is.na(simple_2016$`PAYT/ SMART`)] = 0

colnames(simple_2016) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2017
simple_2017 = new2017 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, `PAYT/ SMART`, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2017$`PAYT/ SMART` = ifelse(simple_2017$`PAYT/ SMART` == "Yes", 1, 0)
simple_2017$`PAYT/ SMART`[is.na(simple_2017$`PAYT/ SMART`)] = 0

colnames(simple_2017) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2018
simple_2018 = new2018 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, `PAYT/ SMART`, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2018$`PAYT/ SMART` = ifelse(simple_2018$`PAYT/ SMART` == "Yes", 1, 0)
simple_2018$`PAYT/ SMART`[is.na(simple_2018$`PAYT/ SMART`)] = 0

colnames(simple_2018) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


#2019
simple_2019 = new2019 %>% 
  select(`Municipality Name`, `Trash Disposal Tonnage`, `Total Number of Households`, `Households Served by Municipal Trash Program`, `PAYT/ SMART`, `Trash Service Type`, `Households Served by Municipal Recycling Program`,`Tons Single Stream Recyclables`, year)

simple_2019$`PAYT/ SMART` = ifelse(simple_2019$`PAYT/ SMART` == "Yes", 1, 0)
simple_2019$`PAYT/ SMART`[is.na(simple_2019$`PAYT/ SMART`)] = 0

colnames(simple_2019) = c("municipality", "trash_tonnage", "total_households", "num_municipal_trash_program_households", "PAYT", "trash_service_type", "num_municipal_recycling_program_households", "ss_recycling_tonnage", "year")


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

complete_data_2011_2019 = na.omit(simple_2011_2019) #dropped rows with any remaining NA's, n = 1303

#dropping observations with 0 reported waste or recycling tonnage or 0 served households for recycling or trash

complete_data_2011_2019 = complete_data_2011_2019[complete_data_2011_2019$trash_tonnage > 0, ] # n = 1274

complete_data_2011_2019 = complete_data_2011_2019[complete_data_2011_2019$ss_recycling_tonnage > 0, ] # n = 1140

complete_data_2011_2019 = complete_data_2011_2019[complete_data_2011_2019$num_municipal_trash_program_households > 0, ] # n = 1135

complete_data_2011_2019 = complete_data_2011_2019[complete_data_2011_2019$num_municipal_recycling_program_households > 0, ] # n = 1134


#checking distribution of complete cases by year

complete_data_2011_2019 %>% 
  group_by(year) %>% 
  summarize(n = n())

#most observations in a year: 2018 & 2019, n = 149
#least observations in a year: 2011, n = 89

# converting categorical variables to factor type for summary purposes

complete_data_2011_2019$service_type = as.factor(complete_data_2011_2019$trash_service_type)
complete_data_2011_2019$year = as.factor(complete_data_2011_2019$year)
complete_data_2011_2019$PAYT = as.factor(complete_data_2011_2019$PAYT)

# messy output

summary(complete_data_2011_2019) 

# number of municipalities with and without UBP implemented from 2011-2019

table(complete_data_2011_2019$PAYT, complete_data_2011_2019$year)

table(complete_data_2011_2019$year)

# clean table of means (at least for numerical vars)

stargazer(as.data.frame(complete_data_2011_2019), type = "latex", out = "tab_of_means.txt")
