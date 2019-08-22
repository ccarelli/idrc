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
dir = "C:/Users/ckingongo/Documents/Atelier TB 2019/Data tb/"

# read in the data locally
dt = readRDS(paste0(dir, "tb_vih_analysis.rds"))
dt = data.table(dt) 
dt[,population_totale:=as.numeric(population_totale)]
# --------------------
# fix the very strange date problem
# date refered to the first day in the quarter

dt[ ,month:=unlist(lapply(str_split(date, "-"), "[", 3))]
dt[ ,year:=as.character(year)]
dt[ ,date:=paste0(month, "-01-", year)]
dt[ ,date:=as.Date(date, format="%m-%d-%Y")]

#
#tbale large to long
#=======================
#dt_long<-dt%>%gather("population_totale","population_couverte","total_de_cas_incident","total_de_cas","tb_teste_vih","tb_vih_positif","tb_vih_positif_tarv","pvvih_sous_inh",key=element,value=value)

#dt_long=data.table(dt_long)
#head(dt_long)

# Group by national level
#===========================


# # shape the data long so it is easier to work with
dt_long = melt(dt, id.vars=c('year', 'date', 'dps', 'trimestre', 'zs', 'csdt'))

# sum the values to the national level
nt = dt_long[ ,.(value=sum(value, na.rm=T)), by=.(date, variable)]

# ----------------------------------
# put all the variables on the same graph

# all the variables on one graph
p1<-ggplot(nt, aes(x=date, y=value, color=variable)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

# all the variables on one page, many graphs 
p2<-ggplot(nt, aes(x=date, y=value, color=variable)) +
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
hiv_tests[ ,percent:=round(percent, 1)] # round the percentages to 1 digit

# make a graph of the percentage of total tb cases tested for hiv
p3<-ggplot(hiv_tests,aes(x=date,y=percent))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="Percentage of total TB cases tested for HIV", 
       x="Date", y="Percent (%)", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# make a graph of the total tb cases
p4<-ggplot(hiv_tests,aes(x=date,y=total_de_cas))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="Total TB cases", 
       x="Date", y="Count", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# make a graph of the total hiv tests performed
p5<-ggplot(hiv_tests,aes(x=date,y=total_testes_vih))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="Total HIV tests performed on TB cases", 
       x="Date", y="Count", caption="Source: PNLT; data are quarterly")+
  theme(text=element_text(size=18))

# ----------------------------------
# complicated visualization from caitlin

# shape the national level data long
hiv_long = melt(hiv_tests, id.vars='date')
hiv_long[variable!='percent', identifier:='TB cases and HIV tests performed']
hiv_long[variable=='percent', identifier:='Percent tested (%)']

hiv_long$variable = factor(hiv_long$variable, c('total_de_cas', 'total_testes_vih', 'percent'),
                           c('Total TB cases', 'Total HIV tests on TB pts.', 'Percent tested'))


p6<-ggplot(hiv_long, aes(x=date, y=value, color=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(~identifier, scales='free_y')+
  theme_bw() +
  labs(color="", y="", x="Date")+
  theme(text=element_text(size=20))

# calculate statistics
hiv_tests[order(date)]





