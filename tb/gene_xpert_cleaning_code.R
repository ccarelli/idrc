# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/25/2019

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
# change the username to your username to code
# for example: user = 'copio'

user = 'ccarelli'

# -----------------------------------------------
# place all the tb files in a single folder in order to import the folder contents

# change to the folder on your computer where all of the TB data are saved
if (user=='ccarelli') inDir = "C:/Users/ccarelli/Documents/tb/"

# create a folder for outputs, including figures and cleaned data sets as RDS files
if (user=='ccarelli') outDir = "C:/Users/ccarelli/Documents/tb_outputs/"

# create a vector of the file name
f = list.files(inDir)

# ----------------------
# read in the data 

# to print the sheets in the excel file 
excel_sheets(paste0(inDir, 'GenXp 1st.7th.4.2019.xls'))

# import the data and make it a data table 
# ignore the warnings! they just say that the first line has a date in it
dt = data.table(read_excel(paste0(inDir, 'GenXp 1st.7th.4.2019.xls'), sheet = 1, skip = 1))

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
# View(dt)

# delete the first row, since it contains column names and not values
dt = dt[-1, ]

# ----------------------
# subset to only the indivual values, not the aggregate tables at the bottom
# we will use these data, but first just the counts

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
dt[is.na(machines), machines:=1] # warning is ok - it thinks the values are words
dt[ ,machines:=as.numeric(machines)] # machines act as a string 

# ----------------------
# create a date variable using the file name

dt[ , day:=stri_extract_first_regex(f, "[0-9]+") ]
dt[ , month:=unlist(lapply(strsplit(f, "\\."), "[", 3))]
dt[ , year:=unlist(lapply(strsplit(f, "\\."), "[", 4))]
dt[ , date:=as.Date(paste0(month,'-', day, '-', year), format="%m-%d-%Y")]
dt[ ,c('month', 'day', 'year'):=NULL]

# ----------------------
# change reported to a logical
dt[ ,reported:=reported==1]

# ----------------------
# fix region and implementing partner names with distinct capitalization

# format names of regions
dt[region!='KCCA',region:=capitalize(tolower(region))]
dt[region=='Fortportal', region:='Fort Portal']
dt[district=='Kayunga', region:='Central'] # one facility in Kayunga is missing a region 
   
# format the names of implementing partners
dt[ ,impl_partner:=gsub("DEFEAT", "Defeat", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ impl_partner=="DEFEAT TB/ HIWA (World Vision)", impl_partner:="Defeat TB/HIWA (World Vision)" ]
dt[ impl_partner=="RHSP/HIWA (World vision)", impl_partner:="RHSP/HIWA (World Vision)"]
dt[impl_partner=='BAYLOR', impl_partner:='Baylor']
dt[impl_partner=="CDC SOROTI PROJECT", impl_partner:='CDC Soroti Project']
dt[impl_partner=='No IP', impl_partner:='No partner']

#----------------------
# format facility names and add facility level 

# drop out the (machines-1) designation from the facility names
dt[ , genexpert_site:=unlist(lapply(strsplit(genexpert_site, "\\("), "[", 1))]
dt[ , genexpert_site:=trimws(genexpert_site)]

# some of the hospital names are abbreviated or in all caps
dt[ , genexpert_site:=gsub("Hosp", "Hospital", genexpert_site)]
dt[ , genexpert_site:=gsub("HOSPITAL", "Hospital", genexpert_site)]

# fix the name of this site 
dt[genexpert_site=='Dzaipi     H/C 111 ', genexpert_site:='Dzaipi HC III']

# use facility names to determine the facility level
dt[grep("Hospital", genexpert_site), level:='Hospital']
dt[grep("II", genexpert_site), level:='HC II']
dt[grep("III", genexpert_site), level:='HC III']
dt[grep("IV", genexpert_site), level:='HC IV']

# fix the facilities with typos
dt[genexpert_site=='Bugono HICV', level:='HC IV']
dt[genexpert_site=='Mbarara RRH', level:='Hospital']
dt[genexpert_site=='Yumbe GH', level:='Hospital']

# print the percentage of facilities for which level cannot be determined
total_sites = dt[ ,length(unique(genexpert_site))]
print(paste0(dt[is.na(level), .(round(length(unique(genexpert_site))/total_sites, 1))],
             '% of total sites are missing the facility level.'))

# ----------------------
# convert character types

dt = dt[ , lapply(.SD, as.numeric), 
         by=c("genexpert_site", "level", "status", "district", "region", "impl_partner",
                                    "reported", "date"), .SDcols=c(6:12, 15)]

# ----------------------
# save the clean file 

saveRDS(dt, paste0(outDir, 'clean_genexpert_data.rds'))

# ----------------------




