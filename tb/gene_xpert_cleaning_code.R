# ----------------------------------------------
# GENEXPERT DATA ANALYSIS
# Cleaning code for GeneXpert data 
# Imports data as weekly Excel spreadshets
#
# Caitlin O'Brien-Carelli
#
# 4/30/2019

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(readxl) # this library loads the data 
library(stringi) # this creates a data from the file name
library(Hmisc)
library(tools)
# --------------------

#------------------------------------------------
# to code in the same code, we need to set file pathways for each user
# change the username to your username on your computer
# for example: user = 'copio'

# user = 'copio'

user = 'ccarelli'

# -----------------------------------------------
# place all the tb files in a single folder in order to import the folder contents

# change to the folder on your computer where all of the TB data are saved
# change to the folder on your computer where all of the TB data are saved
inDir = paste0("C:/Users/", user,   "/Documents/tb_raw_data/")

# create a folder for outputs, including figures and cleaned data sets as RDS files
outDir = paste0("C:/Users/", user, "/Documents/tb_prepped/")

# create a vector of the file name
files = list.files(inDir)

# print f to the console to see the files
files

# ----------------------
# read in the data 

# set the index
# the index keeps track of which iteration of the loop the code is on
i = 1

# loop to clean and append the data together
for (f in files) {

# prints the names of the sheets in the excel file 
excel_sheets(paste0(inDir, f))

# import the data and make it into a data table 
# ignore the warnings! they just say that the first line has a date in it
# for now, we only want the first sheet (sheet = 1)
# the argument 'skip = 1' allows us to skip the first line of the excel sheet, which is blank
dt = data.table(read_excel(paste0(inDir, f), sheet = 1, skip = 1))

# look at the first six lines of data 
# it's a mess! 
head(dt)

# ----------------------
# format the columns to appear correctly

# drop out the extra column added by excel
dt[ , X__1:=NULL]


# list the names for the columns that you want to appear
new_column_names = c("genexpert_site", "district", "region", "impl_partner", "reported", 
                     "total_samples", "tb_positive", "rif_resist", "rif_indet", "total_errors",
                     "children_under_14", "retreatments", "percent_reporting_region", "status")

# reset the column (variable) names         
setnames(dt, new_column_names)

# check that the column names correspond to the correct columns
# the values in row 1 that are not missing should be the same as the variable names
# View(dt) # run this line to examine the data 

# delete the first row, since it contains column names and not values
dt = dt[-1, ]

# ----------------------
# subset to only the indivual values, not the aggregate tables at the bottom
# we will use the tabular data later, but first just the counts from the facilities

# this code finds the areas where 3 columns are false 
dt[ ,index:=1:nrow(dt)]
drop = dt[ ,lapply(.SD, is.na), by=index, .SDcols=2:4]
drop = drop[district==T & region==T & impl_partner==T, min(index)]
dt = dt[index < drop]
dt[ ,index:=NULL]

# ----------------------
# clean the data so that it can be analyzed

# add a variable for the number of genexpert machines per site 
dt[grep('machines', genexpert_site), machines:=genexpert_site]
dt[ , machines:=unlist(lapply(strsplit(dt$machines, "-"), "[", 2))]
dt[!is.na(machines), machines:=gsub(")", "", machines)]
dt[ ,machines:=as.numeric(machines)]
dt[is.na(machines), machines:=1] # warning is ok - it thinks the values are words not numbers


# ----------------------
# create a date variable using the file name

dt[ , day:=stri_extract_first_regex(f, "[0-9]+") ]
dt[ , month:=unlist(lapply(strsplit(f, "\\."), "[", 3))]
dt[ , year:=unlist(lapply(strsplit(f, "\\."), "[", 4))]
dt[ , date:=as.Date(paste0(month,'-', day, '-', year), format="%m-%d-%Y")]
dt[ ,c('month', 'day', 'year'):=NULL]

# ----------------------
# change reported to a logical rather than a 0/1 split
# this code tests the statement "reported==1" and returns T/F
dt[ ,reported:=reported==1]

# ---------------------------------------
# fix region and implementing partner names with distinct capitalization
# formatting for figures and tables 

# -------------------------
# geographic areas 

# format names of regions
dt[region!='KCCA',region:=capitalize(tolower(region))]
dt[region=='Fortportal', region:='Fort Portal']
dt[district=='Kayunga', region:='Central'] # one facility in Kayunga is missing a region 

# format the names of districts 
# drop the words district and hospital, fix capitalization, trim blanks
dt[ , district:=gsub('District', "", district)]
dt[ , district:=gsub('Hospital', "", district)]
dt[ , district:=toTitleCase(tolower(district))]
dt[ , district:=str_trim(district, side="both")]

# -------------------------
# format the names of implementing partners
dt[ ,impl_partner:=gsub("DEFEAT", "Defeat", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[impl_partner=="Defeat TB/ HIWA (World Vision)", impl_partner:="Defeat TB/HIWA (World Vision)" ]
dt[impl_partner=="RHSP/HIWA (World vision)", impl_partner:="RHSP/HIWA (World Vision)"]
dt[impl_partner=='BAYLOR', impl_partner:='Baylor']
dt[impl_partner=="CDC SOROTI PROJECT", impl_partner:='CDC Soroti Project']
dt[impl_partner=='No IP', impl_partner:='No partner']

# -------------------------
# format facility names and add facility level 

# drop out the (machines-1) designation from the facility names
dt[ , genexpert_site:=unlist(lapply(strsplit(genexpert_site, "\\("), "[", 1))]
dt[ , genexpert_site:=trimws(genexpert_site)] 

# some of the hospital names are abbreviated or in all caps
dt[!grep("Hospital", genexpert_site) , genexpert_site:=gsub("Hosp", "Hospital", genexpert_site)]
dt[ , genexpert_site:=gsub("HOSPITAL", "Hospital", genexpert_site)]

# fix the names of sites that contain typos
dt[grep("Dzaipi", genexpert_site), genexpert_site:='Dzaipi HC III']
dt[genexpert_site=='Bugono HICV', level:='HC IV']
dt[genexpert_site=='Mbarara RRH', level:='Hospital']
dt[genexpert_site=='Yumbe GH', level:='Hospital']

# use facility names to determine the facility level
dt[grep("Hospital", genexpert_site), level:='Hospital']
dt[grep("II", genexpert_site), level:='HC II']
dt[grep("III", genexpert_site), level:='HC III']
dt[grep("IV", genexpert_site), level:='HC IV']


# print the percentage of facilities for which level cannot be determined
total_sites = dt[ ,length(unique(genexpert_site))]
print(paste0(dt[is.na(level), .(round(length(unique(genexpert_site))/total_sites, 1))],
             '% of total sites are missing the facility level.'))

# -----------------------------------------------------------
# convert character types

dt = dt[ , lapply(.SD, as.numeric), 
         by=c("genexpert_site", "level", "status", "district", "region", "impl_partner",
                                    "reported", "date"), .SDcols=c(6:12, 15)]

# --------------------
# append the current excel sheet to the full data 
# we are adding each cleaned sheet to one, single data set
# when the loop is complete, the data set will be called 'full_data'

if(i==1) full_data = dt
if(i>1) full_data = rbind(full_data, dt)
i = i+1

# --------------------
}

# --------------------------------------------------
# examine the full data set! yay!

# look at the structure of the data 
str(full_data)

# look at the first six lines of the full data
head(full_data)

# ----------------------
# save the prepped file - it is ready for analysis! 

saveRDS(full_data, paste0(outDir, 'clean_genexpert_data.rds'))

# ----------------------




