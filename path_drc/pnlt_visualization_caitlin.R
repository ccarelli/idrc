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


# # shape the data long so it is easier to work with
dt_long = melt(dt, id.vars=c('year', 'date', 'dps', 'trimestre', 'zs', 'csdt'))

# sum the values to the national level
nt = dt_long[ ,.(value=sum(value, na.rm=T)), by=.(date, variable)]

# ----------------------------------
# put all the variables on the same graph

# all the variables on one graph
ggplot(nt, aes(x=date, y=value, color=variable)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

# all the variables on one page, many graphs 
ggplot(nt, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

# ----------------------------------
# calculate national level proportions

# sum the data to the national level.
hiv_tests = dt[ ,.(total_de_cas=sum(total_de_cas, na.rm = T), 
       total_testes_vih=sum(tb_teste_vih, na.rm=T)), by=c('date') ]

hiv_tests[ ,percent:=100*(total_testes_vih/total_de_cas)]

# make a graph of the percentage of total tb cases tested for hiv
ggplot(hiv_tests,aes(x=date,y=percent))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="Percentage of total TB cases tested for HIV", 
       x="Date", y="Percent (%)", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# make a graph of the total tb cases
ggplot(hiv_tests,aes(x=date,y=total_de_cas))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="Total TB cases", 
       x="Date", y="Count", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# make a graph of the total tb cases
ggplot(hiv_tests,aes(x=date,y=total_testes_vih))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="total_testes_vih", 
       x="Date", y="Count", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# ----------------------------------
# complicated visualization from caitlin








