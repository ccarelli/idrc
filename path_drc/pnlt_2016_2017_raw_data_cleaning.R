# PNLT Cleaning code - 2016 and 2017
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
library(openxlsx)
library(gridExtra)
library(grid)
library(tools)
# --------------------

# --------------------
# set working directories

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data locally
# the excel spreadsheet has been converted to a csv
dt = read.table(paste0(dir, "tbl_2016_2017.csv"), as.is = !stringsAsFactors, nrows = -20)


dt = data.table
dt = read.table(paste0(dir, "tbl_2016_2017.csv"), as.is = !stringsAsFactors, nrows = -20)

# --------------------
# format the columns

dt = head(dt, n = 20)

