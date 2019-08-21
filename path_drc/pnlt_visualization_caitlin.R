# PNLT Visualization code - initial graphs
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
library(tools)
# --------------------

# --------------------
# set working directories


# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data locally
dt = readRDS(paste0(dir, "tb_hiv_ready_for_analysis.rds"))
dt = data.table(dt)

# --------------------
# check the structure of the data - are there any problems?
# hint: there is one problem!

dt[ ,population_totale:=as.numeric(population_totale)]

# ----------------------------------
# create a new data table summed to the national level

# view the names of the variables
names(dt)

# shape the data long so it is easier to work with
dt_long = melt(dt, id.vars=c('year', 'date', 'dps', 'trimestre', 'zs', 'csdt'))

# sum the values to the national level
nt = dt_long[ ,.(value=sum(value, na.rm=T)), by=.(date, variable)]

# ----------------------------------
# put all the variables on the same graph


ggplot(nt, aes(x=date, y=value, color=variable)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggplot(nt, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


# ----------------------------------














