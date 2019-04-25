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
new_column_names = c("genexpert_site", "district", "region", "intl_partner", "reported", 
                     "total_samples", "tb_positive", "rif_resist", "rif_indet", "total_errors",
                     "children_under_14", "retreatments", "percent_reporting_region", "status")

# reset the column (variable) names         
setnames(dt, new_column_names)

# check that the column names correspond to the correct columns
# the values in row 1 that are not missing should be the same as the variable names
View(dt)

# delete the first row, since it contains column names and not values
dt = dt[-1, ]

# ----------------------
# subset to only the indivual values, not the aggregate tables at the bottom
# we will use these data, but first just the counts

# this code finds the areas where 3 columns are false 
dt[ ,index:=1:nrow(dt)]
drop = dt[ ,lapply(.SD, is.na), by=index, .SDcols=2:4]
drop = drop[district==T & region==T & intl_partner==T, min(index)]
dt = dt[index < drop]
dt[ ,index:=NULL]

# ----------------------
# clean the data so that it can be analyzed

# add a variable for the number of genexpert machines per site 
dt[grep('machines', genexpert_site), machines:=genexpert_site]
dt[ , machines:=unlist(lapply(strsplit(dt$machines, "-"), "[", 2))]
dt[!is.na(machines), machines:=gsub(")", "", machines)]
dt[is.na(machines), machines:=1] # warning is ok - it thinks the values are words
dt[ ,machines:=as.numeric(machines)] # machines act as a string 

# ----------------------
# create a date variable using the file name

dt[ , day:=stri_extract_first_regex(f, "[0-9]+") ]
dt[ , month:=unlist(lapply(strsplit(f, "\\."), "[", 3))]
dt[ ,year:=unlist(lapply(strsplit(f, "\\."), "[", 4))]
dt[ ,date:=as.Date(paste0(month,'-', day, '-', year), format="%m-%d-%Y")]
dt[ ,c('month', 'day', 'year'):=NULL]

# ----------------------







