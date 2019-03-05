# Cleaning malaria data in R
#
# 8/15/18
# Malaria sample cleaning and analysis code
# Create maps and graphs of the malaria data
# Merge the years
#---------------------------------------

#--------------------------------------
# Set up R
# clear your workspace and load R packages

rm(list=ls()) # clears the global enviroment
library(data.table)
library(ggplot2)
library(stringr) # allows you to split words to create new variables
library(plyr)  # useful for cleaning and data prep
library(RColorBrewer)
library(raster)
library(rgdal)
library(tibble)
library(dplyr)
library(maptools)


#---------------------------------------
# set a local directory to use throughout

# you will need to set this to your home directory
# (where you saved the data)

# to get the file pathway, click shift and right click, then 'copy as path'
# my directory:
dir <- "C:/Users/ccarelli/Documents/malaria_sample/"

# load data from the directory you created (paste 0 pastes the file name onto the directory)
# create separate data tables
# by leaving these uploaded, you can explore different data sets for errors
# import all years for which you have data before determing where 'cutoff' is

rdt2015 <- read.csv(paste0(dir, '/malaria_2015.csv'))
rdt2016 <- read.csv(paste0(dir, '/malaria_2016.csv'))
rdt2017 <- read.csv(paste0(dir, '/malaria_2017.csv'))
rdt2018 <- read.csv(paste0(dir, '/malaria_2018.csv'))

#-----------------------------------------------------
# merge the data sets

rdt1 <- merge(rdt2015, rdt2016, by='District')
rdt2 <- merge(rdt1, rdt2017, by='District')
rdt <- merge(rdt2, rdt2018, by='District')

# check and explore the merge
str(rdt) # the number of variables should correspond to the total of each dt

# convert the data to a data table
rdt <- data.table(rdt)

# convert district to a character, rather than a factor


# typically, we only use lower case variable names
# change the name using the setnames() function
setnames(rdt, 'District', 'district')

# check the class of the District variable
# it should be a character
str(rdt)

# print a unique list of districts to determine which are included
rdt[ ,unique(district)]

# how many districts are included?
rdt[ ,length(unique(district))]

#----------------------------------
# before we can explore the data, we need it to be in a rational format
# melt the dates so that they are in a single column
# this allows us to create a date variable!!

# melt to place the dates in a single column
rdt <- melt(rdt, id.vars='district')

# did it work? View the data (same as browse in Stata)
View(rdt)

# we need a date variable to analyze effectively
# rename the date column 'date'
setnames(rdt, 'variable', 'date')

#---------------------------------------------------------------
# create a year variable

# for 2015
rdt[date=='jan.15', year:=2015]
rdt[date=='feb.15', year:=2015]
rdt[date=='mar.15', year:=2015]
rdt[date=='apr.15', year:=2015]
rdt[date=='may.15', year:=2015]
rdt[date=='jun.15', year:=2015]
rdt[date=='jul.15', year:=2015]
rdt[date=='aug.15', year:=2015]
rdt[date=='sep.15', year:=2015]
rdt[date=='oct.15', year:=2015]
rdt[date=='nov.15', year:=2015]
rdt[date=='dec.15', year:=2015]

# 2016
rdt[date=='jan.16', year:=2016]
rdt[date=='feb.16', year:=2016]
rdt[date=='mar.16', year:=2016]
rdt[date=='apr.16', year:=2016]
rdt[date=='may.16', year:=2016]
rdt[date=='jun.16', year:=2016]
rdt[date=='jul.16', year:=2016]
rdt[date=='aug.16', year:=2016]
rdt[date=='sep.16', year:=2016]
rdt[date=='oct.16', year:=2016]
rdt[date=='nov.16', year:=2016]
rdt[date=='dec.16', year:=2016]

#2017
rdt[date=='jan.17', year:=2017]
rdt[date=='feb.17', year:=2017]
rdt[date=='mar.17', year:=2017]
rdt[date=='apr.17', year:=2017]
rdt[date=='may.17', year:=2017]
rdt[date=='jun.17', year:=2017]
rdt[date=='jul.17', year:=2017]
rdt[date=='aug.17', year:=2017]
rdt[date=='sep.17', year:=2017]
rdt[date=='oct.17', year:=2017]
rdt[date=='nov.17', year:=2017]
rdt[date=='dec.17', year:=2017]

# 2018
rdt[date=='jan.18', year:=2018]
rdt[date=='feb.18', year:=2018]
rdt[date=='mar.18', year:=2018]
rdt[date=='apr.18', year:=2018]
rdt[date=='may.18', year:=2018]
rdt[date=='jun.18', year:=2018]
rdt[date=='jul.18', year:=2018]
rdt[date=='aug.18', year:=2018]

#----------------------------------------------------
# month 

# for 2015
rdt[date=='jan.15', month:='01']
rdt[date=='feb.15', month:='02']
rdt[date=='mar.15', month:='03']
rdt[date=='apr.15', month:='04']
rdt[date=='may.15', month:='05']
rdt[date=='jun.15', month:='06']
rdt[date=='jul.15', month:='07']
rdt[date=='aug.15', month:='08']
rdt[date=='sep.15', month:='09']
rdt[date=='oct.15', month:='10']
rdt[date=='nov.15', month:='11']
rdt[date=='dec.15', month:='12']

# for 2016
rdt[date=='jan.16', month:='01']
rdt[date=='feb.16', month:='02']
rdt[date=='mar.16', month:='03']
rdt[date=='apr.16', month:='04']
rdt[date=='may.16', month:='05']
rdt[date=='jun.16', month:='06']
rdt[date=='jul.16', month:='07']
rdt[date=='aug.16', month:='08']
rdt[date=='sep.16', month:='09']
rdt[date=='oct.16', month:='10']
rdt[date=='nov.16', month:='11']
rdt[date=='dec.16', month:='12']

# for 2017
rdt[date=='jan.17', month:='01']
rdt[date=='feb.17', month:='02']
rdt[date=='mar.17', month:='03']
rdt[date=='apr.17', month:='04']
rdt[date=='may.17', month:='05']
rdt[date=='jun.17', month:='06']
rdt[date=='jul.17', month:='07']
rdt[date=='aug.17', month:='08']
rdt[date=='sep.17', month:='09']
rdt[date=='oct.17', month:='10']
rdt[date=='nov.17', month:='11']
rdt[date=='dec.17', month:='12']

# for 2018
rdt[date=='jan.18', month:='01']
rdt[date=='feb.18', month:='02']
rdt[date=='mar.18', month:='03']
rdt[date=='apr.18', month:='04']
rdt[date=='may.18', month:='05']
rdt[date=='jun.18', month:='06']
rdt[date=='jul.18', month:='07']
rdt[date=='aug.18', month:='08']

#-------------------------------------------------
# create a date variable

# use month and year to create a date variable
rdt[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# you can then delete month and year and just use date
# I usually keep them in for subsetting
#--------------------------------------------------
# you can also rename the variable 'value' if you have more than one
# for example, if you have both rdts performed and cases:
# setnames(rdt, 'value', 'rdts_performed')

#-------------------------------------------------
# explore the data set

# confirm the data frame is also a data table
class(rdt)

# browse the data set
View(rdt)

# get the names of the variables in the data set
names(rdt)

# see what types of variables are in the data set
str(rdt)

#-------------------------------------------------
# explore the data for outliers
# create a spaghetti plot
# do any of the districts have outliers

ggplot(rdt, aes(x=date, y=value, color=district, group=district)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='RDTs performed', x='date')

# we need to remove the outliers to interpret the data 
# we can identify the outlier using the map, and a few manual functions
# there appears to be a single outlier in each year

# these outliers are the highest values in a single year
rdt[year==2015, max(value)]
rdt[year==2016, max(value)]
rdt[year==2017, max(value)]
rdt[year==2018, max(value)]

# now that we've identified the outlier, let's print it
rdt[value==10000000] # replace with the maximum value


# let's replace it with a missing value:
rdt[value==10000000, value:=NA]

# you can also replace it more specifically
# using the unique combination of district and date:
rdt[district=='Gulu' & date=='2015-02-01', value:=NA]
rdt[district=='Gulu' & date=='2016-02-01', value:=NA]
rdt[district=='Gulu' & date=='2017-02-01', value:=NA]
rdt[district=='Gulu' & date=='2018-02-01', value:=NA]

# now let's visualize again - does the spaghetti plot show an outlier?
ggplot(rdt, aes(x=date, y=value, color=district, group=district)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='RDTs performed', x='date')

# there are some higher values, but they are not clearly data errors

#--------------------------------------------------------
# let's make some time trend graphs

# create a new data table with national totals by date
# use na.rm=T to make sure missing data is dropped out
rdt_total <- rdt[ , .(rdts_performed=sum(value, na.rm=T)), by=date]

ggplot(rdt, aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='RDTs performed', x='date')

#------------------------------------------------------










