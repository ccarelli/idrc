# PNLT Preparation Code
# We have already cleaned the raw data 
# 8/20/2019
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(RColorBrewer)
library(gridExtra)
library(grid)
# --------------------

# --------------------
# set working directories

# the paste function pastes things together without spaces, paste0 uses spaces
paste('put these', "together")
paste0('put these', "together")

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data locally
dt = read.csv(paste0(dir, 'PNLT_PREPPED_2017_2018_TBHIV.csv'), stringsAsFactors = FALSE)
dt = data.table(dt)
# --------------------
# view the structure of the cleaned data 

# view the first six lines of the file
head(dt, n=2)

# view the structure of the file
str(dt)

# change the date variable to a "date type"
dt[ ,date:=as.Date(date, format="%Y-%m-%d")]

# convert trimestre to just a quarter variable
dt[ ,trimestre:=gsub("T1 2018", "T1", trimestre)]
dt[ ,trimestre:=gsub("T2 2018", "T2", trimestre)]
dt[ ,trimestre:=gsub("T3 2018", "T3", trimestre)]
dt[ ,trimestre:=gsub("T4 2018", "T4", trimestre)]

dt[ ,trimestre:=gsub("T1 2017", "T1", trimestre)]
dt[ ,trimestre:=gsub("T2 2017", "T2", trimestre)]
dt[ ,trimestre:=gsub("T3 2017", "T3", trimestre)]
dt[ ,trimestre:=gsub("T4 2017", "T4", trimestre)]

# make it into a number 
dt[ ,trimestre:=as.numeric(trimws(gsub("T", "", trimestre)))]

# trimws() removes spaces from the front and end
class(dt$trimestre) # data frames
dt[ ,class(trimestre)] # data tables

# format the dps names so they look beautiful 
dt[ ,dps:=gsub("_", " ", dps)]
dt[ , dps:=toTitleCase(dps)]

















