# Prep the TB HMIS data to work with
# Create a date variable and clean facilities
# NOTE: MERGE META DATA 
# 4/7/2019
#-----------------------------------

rm(list=ls()) # clear memory
library(reshape2)
library(ggplot2)
library(foreign)
library(tidyr)
library(stats4)
library(caret)
library(dplyr)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(lubridate)
##-disable scientific notation
options(scipen=999)

setwd("C:/Users/Jaffer/Desktop/PCE/TB/Data/Latest")
tb <- read.csv("tb_04_07_2019.csv", stringsAsFactors = FALSE)
tb = data.table(tb)

# delete unecessary variables
tb[ ,c("organisationunitcode", "organisationunitdescription", "perioddescription", "periodid"):=NULL]

#-------------------------------
# rename the variables
names(tb)

# confirm you have renamed the variables correctly
setnames(tb, c("organisationunitid", "organisationunitname", 
               "periodname", "periodcode", "bacteriologically.confirmed..ptb..p.bc...cases.",
               "lost..all.tb.cases.", "cured..all.tb.cases."),
              c("org_unit_id", "facility", "period", "period_code", 
               "ptb_bact_conf_cases", "tb_cases_lost_all_perc", "tb_cases_cured_all_perc"))

#-------------------------------
# create a date variable

# grab the year from quarterly data 
tb[ ,year:=unlist(lapply(strsplit(period_code, "Q"), "[", 1))]

# grab the month from quarterly data
tb[grep("Q1", period_code), month:='01']
tb[grep("Q2", period_code), month:='04']
tb[grep("Q3", period_code), month:='07']
tb[grep("Q4", period_code), month:='10']

# visualize reporting completeness
tb[ , date:=paste0(year, '-', month, '-01')]
tb[ , c("year", "month", "period"):=NULL]
tb[ , date:=ymd(date)]

#-------------------------------
# create a variable for facility level 

# drop out the white spaces and put facility in lower case
tb[ ,facility1:=tolower(facility)]
tb[ ,facility1:=gsub(" ", "", facility1)]

# search for variable type - remember the order is important

# strange types of facilities - do first so they are overwritten
tb[grep("-nr", facility1), facility_level:='NR']
tb[grep("clinic", facility1), facility_level:='Clinic']
tb[grep("medicalcent", facility1), facility_level:='Medical Centre']
tb[grep("maternity", facility1), facility_level:='Maternity Home']
tb[grep("drugshop", facility1), facility_level:='Drug Shop']

tb[grep("hospital", facility1), facility_level:='Hospital']
tb[grep("militaryhospital", facility1), facility_level:='Military Hospital']
tb[grep("hcii", facility1), facility_level:='HC II']
tb[grep("hciii", facility1), facility_level:='HC III']
tb[grep("iii", facility1), facility_level:='HC III']
tb[grep("hciv", facility1), facility_level:='HC IV']

# typos and strange facility types set to other
tb[is.na(facility_level), facility_level:='Other']

# drop out the extra variable used to create facility level
tb[ ,facility1:=NULL]

#-------------------------------
# merge in the names of the districts
# use jaffer's code to match the districts to the shape file

#-------------------------------
# save the prepped data to the working directory

saveRDS(tb, "tb_hmis_prepped.rds")

#-------------------------------