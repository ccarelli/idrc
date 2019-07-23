#
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
library(tools)
library(lubridate)
##-disable scientific notation
options(scipen=999)

inDir="/Malaria_raw_data/"

setwd(inDir)

outDir="/Malaria_prepped_data/"

outputs="/Malaria_outputs/"
#---------------------------------------------------------------
mal<-read.csv("mal_wep_district_quarterly.csv")
mal<-data.table(mal)

mal[,c("organisationunitid" ,"organisationunitcode" ,"organisationunitdescription",
       "perioddescription","periodcode","periodname"):=NULL]

new_col<-c("district","qtr","Suspected_malaria","Microscopy_positive_cases","RDT_positive_cases","Microscopy_tested_cases",
           "RDT_tested_cases","Microscopy_negative_treated","Microscopy_positive_treated","RDT_positive_treated",
           "Not_tested_treated","RDT_negative_treated")

setnames(mal,new_col)

#Tested for malaria = Malaria tests - WEP Microscopy Tested Cases + Malaria tests - WEP RDT Tested Cases 
mal$Suspected_cases_tested<-mal$Microscopy_tested_cases +  mal$RDT_tested_cases

#.	Malaria positive cases = Malaria tests - WEP Microscopy Positive Cases + Malaria tests - WEP RDT Positve Cases
mal$Cases_confirmed_positive<- mal$Microscopy_positive_cases + mal$RDT_positive_cases

#.	Negative for malaria = [ Malaria tests - WEP Microscopy Tested Cases + Malaria tests - WEP RDT Tested Cases] - 
#[ Malaria tests - WEP Microscopy Positive Cases + Malaria tests - WEP RDT Positve Cases]

mal$negative_for_malaria<-(mal$Microscopy_tested_cases + mal$RDT_tested_cases) - (mal$Microscopy_positive_cases + mal$RDT_positive_cases)

#.	Total treated for malaria  = Malaria treated - WEP Microscopy Negative Cases Treated + 
#   malaria treated- WEP Microscopy Positive Cases Treated + Malaria treated - WEP RDT Positive Cases Treated + Malaria treated - WEP Not Tested Cases Treated + Malaria treated - WEP RDT Negative Cases Treated

mal$total_treated_malaria<-mal$Microscopy_negative_treated + mal$Microscopy_positive_treated + mal$RDT_positive_treated + mal$Not_tested_treated + mal$RDT_negative_treated
mal$positive_treated<-(mal$RDT_positive_treated + mal$Microscopy_positive_treated)
mal$negative_treated<-(mal$Microscopy_negative_treated + mal$RDT_negative_treated)
#---------------------------------------------------------------
#replace suspected_malaria as suspected cases tested whenever the suspected cases tested are greater then suspected malaria
mal[Suspected_cases_tested>Suspected_malaria,Suspected_malaria:=Suspected_cases_tested]

#
mal_long<-melt(mal,id.vars = c("district","qtr"))
mal_long<-data.table(mal_long)
mal_long$qtr<-as.character(mal_long$qtr)
# grab the year from quarterly data 
mal_long[ ,year:=unlist(lapply(strsplit(qtr, "Q"), "[", 1))]

# grab the month from quarterly data
mal_long[grep("Q1", qtr), month:='01']
mal_long[grep("Q2", qtr), month:='04']
mal_long[grep("Q3", qtr), month:='07']
mal_long[grep("Q4", qtr), month:='10']

# visualize reporting completeness
mal_long[ , date:=paste0(year, '-', month, '-01')]
mal_long[ , c("year", "month", "qtr"):=NULL]
mal_long[ , date:=ymd(date)]

mal_long_overall<-mal_long[,.(Tot_cases=sum(value,na.rm=T)),by=c("variable","date")]

#--------------------------------------------------------------------------------------
propn<-mal_long_overall[variable=='Suspected_cases_tested' | variable=='Suspected_malaria']
propn<-propn[,.(date,variable,Tot_cases)]
propn<-dcast(propn,date~variable)
propn<-data.table(propn)
propn<-propn[,.(prop=100*(Suspected_cases_tested/Suspected_malaria)),by="date"]

#------------------------------------------------------------------------------------
#status treated
propn_status<-mal_long_overall[variable=='positive_treated' | variable=='negative_treated'| variable=='Not_tested_treated'| variable=='total_treated_malaria']

propn_status<-dcast(propn_status,date~variable)

propn_status$prop_positive_treated<-100*(propn_status$positive_treated /propn_status$total_treated_malaria)
propn_status$prop_negative_treated<-100*(propn_status$negative_treated/propn_status$total_treated_malaria)
propn_status$prop_not_tested_treated<-100*(propn_status$Not_tested_treated/propn_status$total_treated_malaria)

propn_status<-melt(propn_status,id.var="date")
propn_status<-propn_status[variable=='prop_positive_treated' | variable=='prop_negative_treated' | variable=='prop_not_tested_treated']


pdf(paste0(outputs,"Malaria_updated_figures.pdf"))

ggplot(mal_long_overall[variable=='Suspected_cases_tested' | variable=='Cases_confirmed_positive'],
       aes(x=date, y=Tot_cases,color=variable))+
  geom_point()+
  geom_line()+
  theme_bw() +
  
  labs(title='Number tested and confirmed', subtitle = '2000-2019',
       x='Date', y='Count') +
  theme(legend.position='bottom')
 

mal_long_overall[,year:=year(date)]

mal_long_overall<-mal_long_overall[year>='2013' & year<='2019']

ggplot(mal_long_overall[variable=='Suspected_cases_tested' | variable=='Cases_confirmed_positive'],
       aes(x=date, y=Tot_cases,color=variable))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number tested and confirmed', subtitle = '2013-2019',
       x='Date', y='Count') +
  theme(legend.position='bottom')

ggplot(propn,
       aes(x=date, y=prop))+
  geom_point()+
  geom_line()+
  theme_bw() +
    labs(title='Proportion of suspected cases tested', subtitle = '2000-2019',
       x='Date', y='Percent') 

ggplot(propn[date>='2013-01-01'],
       aes(x=date, y=prop))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")+
  theme_bw() +
  labs(title='Proportion of suspected cases tested', subtitle = '2013-2019',
       x='Date', y='Percent') 

ggplot(propn_status[date>='2015-07-01'],
       aes(x=date, y=value,color=variable))+
  geom_point()+
  geom_line()+
  theme_bw() +
  
  labs(title='Proportion of cases tested by testing status', subtitle = 'July 2015-2019',
       x='Date', y='Percent') +
  theme(legend.position='bottom')

dev.off()
