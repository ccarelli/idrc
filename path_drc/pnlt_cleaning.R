# PNLT Cleaning Code
# 8/20/2019
# Constant K
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(RColorBrewer)

# --------------------

# --------------------
# set working directories

# local directory
#dir = "C:/Users/ccarelli/Documents/pnls_data/"
dir<-"C:/Users/ckingongo/Documents/Atelier TB 2019/Data tb/"

# read in the data locally
dt <- read.csv(paste0(dir, 'PNLT_PREPPED_2017_2018_TBHIV.csv'),stringsAsFactors = FALSE)
dt<-data.table(dt)

# --------------------

head(dt)

str(dt)

dt[ , date:=gsub("/", "-", date)]
dt[,date:=as.Date(date, format="%d-%m-%Y")]
dt[ , trimestre:=gsub("T1 2018", "T1", trimestre)]
dt[ , trimestre:=gsub("T2 2018", "T2", trimestre)]
dt[ , trimestre:=gsub("T3 2018", "T3", trimestre)]
dt[ , trimestre:=gsub("T4 2018", "T4", trimestre)]
dt[ , trimestre:=gsub("T1 2017", "T1", trimestre)]
dt[ , trimestre:=gsub("T2 2017", "T2", trimestre)]
dt[ , trimestre:=gsub("T3 2017", "T3", trimestre)]
dt[ , trimestre:=gsub("T4 2017", "T4", trimestre)]
dt[,trimestre:=as.numeric(gsub("T","",trimestre))]


