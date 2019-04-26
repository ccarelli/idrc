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
View(dt)

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

dt[region!='KCCA',region:=capitalize(tolower(region))]

# format the names of implementing partners
dt[(impl_partner!="IDI" & impl_partner!="MUWRP" & impl_partner!="RHSP" & impl_partner!="URC/DHAPP" & impl_partner!="RTI"),
   impl_partner:=titleCase(tolower(impl_partner))]
dt[ , impl_partner:=gsub("tb", "TB", impl_partner)]
dt[ , impl_partner:=gsub("idi", "IDI", impl_partner)]
dt[ , impl_partner:=gsub("cdc", "CDC", impl_partner)]
dt[ , impl_partner:=gsub("cdc", "CDC", impl_partner)]

# ----------------------
# convert character types

dt = dt[ , lapply(.SD, as.numeric), 
         by=c("genexpert_site", "district", "region", "impl_partner",
                                    "reported", "date"), .SDcols=6:12]

# ----------------------


# ---------------------------------------------
# exploratory graphs

# facilities reporting by region 
rep = dt[ ,.(reported=sum(reported), sites=length(unique(genexpert_site))), by=region]

reporting_rate = rep[ ,.(round(100*sum(reported)/sum(sites), 1))]

bar_colors = c('GeneXpert Sites'='#9ecae1', 'Reported'='#fc9272')

ggplot(rep, aes(x=region, y=sites, fill='GeneXpert Sites')) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=rep$reported, fill='Reported'), stat="identity") + 
    scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
    labs(x='Region', y='GeneXpert Sites', subtitle=paste0('Reporting rate: ', reporting_rate, '%')) +
theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

rep[ ,rate:=round(100*reported/sites, 1)]
rep[ ,all:=rate==100]

ggplot(rep, aes(x=region, y=rate, color=all, size=sites)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(title="Percent of facilities reporting (%)", y="% reporting", x="Region", color="100% reported", 
       size="Number of facilities") +
  theme(text = element_text(size=14), axis.text.x=element_text(size=12, angle=90)) 

# -------------------------------------
# percent of tests that are positive 
tb = dt[ , lapply(.SD, sum, na.rm=T), by=region, .SDcols=7:13]
tb[ ,percent_pos:=round(100*tb_positive/total_samples, 1)]

# plot of percent TB positive
ggplot(tb, aes(x=region, y=percent_pos, color='red')) +
  geom_point(size=4, alpha=0.5) +
  scale_fill_manual(values=all, aesthetics="fill") +
  theme_bw() +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  labs(x='Region', y='Percent TB positive (%)', color=" ",
       title="Percent of GeneXpert tests that were TB positive, April 1 - 7, 2019") +
  guides(fill=FALSE, color=FALSE)

# -------------------------------------
# tests performed by implementer

part = dt[ , lapply(.SD, sum, na.rm=T), by=impl_partner, .SDcols=7:13]
part[ ,percent_pos:=round(100*tb_positive/total_samples, 1)]

total = sum(part$total_samples)

ggplot(part, aes(x=impl_partner, y=total_samples, fill='Total samples')) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x='Implementing Partner', y='Total Samples', subtitle=paste0('Total samples: ', total), 
   fill=" ") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))


total_pr[ , ]

ggplot(part, aes(x=impl_partner, y=percent_pos, fill='Percent positive')) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x='Implementing Partner', y='% positive', subtitle=paste0('Total percent positive: ', total_or), 
       fill=" ") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))






# -------------------------------------












