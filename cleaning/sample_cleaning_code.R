# Sample cleaning code for PNLS data 
# 1/31/19
# ----------------------------------------------

# --------------------
# Set up R
# load the packages you need to analyze these data
# if a package does not appear, remember install.packages('library_name')

rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(RColorBrewer) # package for choosing unique colors!
# --------------------

#---------------------------------------
# load the testing data

# set the directory to where you have stored your data 
dt = readRDS("C:/Users/ccarelli/Documents/snis_pnls_vcts.rds")

# make sure the data is a data table object so you can do data tables fun things!
dt = data.table(dt)

# because of the data lag, subset to before August 2019
dt = dt[date < '2018-09-01']

#-------------------------------------
# check out the data

# take a look
head(dt)

# what types of variables are there
str(dt)

# what are the range of dates
dt[ ,range(date)]

# delete stock_category - it is only for supply chain data 
dt[ , stock_category:=NULL]

#-------------------------------------
# we want to create a single variable for "VIH+" that includes all key populations
# that way, we can get the total HIV+ persons and stratify by subpop
# subset to the elements that include only 'VIH+'
# for example, "Clients VIH+ informés des résultats CDIP"

# print out the elements that contain the word "VIH+"
dt[grep("VIH+", element), unique(element)]

# print out the elements that contain the word "VIH+" by sub-population
# check that the sub-populations listed in the element are all reflected in the subpop variable
dt[grep("VIH+", element), unique(element), by=subpop]

# we only want to capture the elements that JUST say 'VIH+'
# we do not want to capture other variables, like "X VIH+informés des résultats"
dt[grep("VIH+", element), variable:='HIV+']
dt[grep('informés', element), variable:=NA]   
dt[grep('testés', element), variable:=NA] 

# see what happens when we sum by this variable:
dt[variable=='HIV+', .(value=sum(value, na.rm=T)), by=subpop]

# now we know how many people were diagnosed HIV+!

# create a ggplot of just these data, stratified by subpopulation
vih = dt[variable=='HIV+', .(value=sum(value, na.rm=T)), by=.(date, variable, subpop)]

# choose a name for the title of the graph
name = vih[ , unique(variable)]

# plot 
ggplot(vih, aes(x=date, y=value, color=subpop)) +
  geom_point(alpha=0.7) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=brewer.pal(12, 'RdYlBu')) +
  labs(x='Date', y='Count', title=name, color='Sub-population') 

# drop out csw clients because it is messing up the scale!
ggplot(vih[subpop!='csw_client'], aes(x=date, y=value, color=subpop)) +
  geom_point(alpha=0.7) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=brewer.pal(12, 'RdYlBu')) +
  labs(x='Date', y='Count', title=name, color='Sub-population') 
  
# let's try one where all of the populations are on a different scale
ggplot(vih[subpop!='csw_client'], aes(x=date, y=value)) +
  geom_point(alpha=0.7) +
  geom_line() +
  theme_bw() +
  facet_wrap(~subpop, scales='free_y') +
  labs(x='Date', y='Count', title=name)

#-----------------------------------------
# continue the data preparation process by aggregating another variable:

# how many people were counselled and tested?
dt[grep('conseillés et testés', element), variable:='Counseled and tested']
dt[variable=='Counseled and tested', unique(element), by=subpop]

# and so forth...

# and so many more variables to prep!


