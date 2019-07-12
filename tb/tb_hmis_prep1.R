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


#-------------------------------
# import the files 

# set the working directory 
setwd("C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_data/")

# add an argument for level (district or facility)
level = 'facility'

# list the name of the directory
if (level=='facility') dir = "C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_data/facility/"
if (level=='district') dir = "C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_data/district/"

# list the files in the data set
files = list.files(dir)

#-------------------------------
# import the files that are facility level counts

# loop through each file and rbind them together
setwd(dir)
i = 1

for (f in files) {

# import the csv
tb = read.csv(f, stringsAsFactors = FALSE)
tb = data.table(tb)

# delete unecessary variables
tb[ , c("organisationunitcode", "organisationunitdescription", "perioddescription", "periodid"):=NULL]

#-------------------------------
# rename the variables

# confirm you have renamed the variables correctly
setnames(tb, c("organisationunitid", "organisationunitname", 
               "periodname", "periodcode"),
              c("org_unit_id", "facility", "period", "period_code"))

#-------------------------------
# add a variable for the file name

tb[ ,data_set:=as.character(f)]

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
# shape the data long

idVars = c('data_set', 'org_unit_id', 'facility', 'facility_level', 'period_code', 'date')
tb = melt(tb, id.vars=idVars)

#-------------------------------
# merge in the names of the districts
# use jaffer's code to match the districts to the shape file

#-------------------------------
# rbind the files together
 if (i==1) dt = tb
 if(1 < i) dt = rbind(dt, tb)
 
  # print the progress
   print(paste0("Added in the file:", f))
   print(paste0("File number ", i, " of ", length(files)))
   print("Ignore the warning!")
   # increase the index
    i = i+1
  } 
#--------------------------------------------------------
# look at the final file
str(dt)

#--------------------------------------------------------
# import the meta data - the districts for each health facility

# import
distdict="C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_data/"
dist = data.table(read.csv(paste0(distdict,"district_hfacility_names.csv"), stringsAsFactors = FALSE))
dist[ ,c("organisationunitcode", "organisationunitdescription", "periodid",
         "periodname", "periodcode", "perioddescription", "hsdp.tb.treatment.success.rate"):=NULL]

# rename the variables
setnames(dist, c("org_unit_id", 'org_unit'))

# get the region and district names
dist[ , region:=unlist(lapply(strsplit(org_unit, "/"), "[", 3))]
dist[ , district:=unlist(lapply(strsplit(org_unit, "/"), "[", 4))]

# eliminate the word region and district from the names
dist[ , region:=trimws(gsub("Region", "", region))]
dist[ , district:=trimws(gsub("District", "", district))]

# drop out org_unit
dist[ ,org_unit:=NULL]
#--------------------------------------------------------
# merge in the districts and regions

dt = merge(dt, dist, by='org_unit_id', all.x=T)

#--------------------------------------------------------
# format the district level data

if (level=='district') {
  dt[ , c('facility', 'facility_level'):=NULL]
}

#--------------------------------------------------------
# output directory
outDir = "C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_prepped/"

# save the prepped data to the working directory
if(level=='facility') saveRDS(dt, paste0(outDir, "tb_hmis_facility_counts_prepped.rds"))
if(level=='district') saveRDS(dt, paste0(outDir, "tb_hmis_district_ratios_prepped.rds"))
#-------------------------------




